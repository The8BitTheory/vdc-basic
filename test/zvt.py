#!/usr/bin/env python3
"""VRAM-Vergleich ohne Monitor fuer Z64K (run_rxv.py / run_draw.py mit --emu z64k).

Prinzip (Begruendung in test/README.md, Abschnitt Z64K): Z64K hat keinen Monitor, der Haltepunkte setzt und den VRAM
sichert. Darum liegt ein kleines 6502-Programm (test/z64k/zvt.s, bei $2F00) im Rechner, das die Aufgaben der
beiden Haltepunkte $1BFE/$1BFF selbst erledigt: PREP laedt das Grundmuster ueber $D600/$D601 in den VRAM und
schreibt Parameter und Strom des naechsten Vektors in den RAM, CHECK bildet die CRC-16 (CCITT) ueber alle 65536
VRAM-Bytes und vergleicht sie mit dem in Python vorausberechneten Wert (dazu optional einzelne RAM-Bytes, z. B.
$1C06/$1C08 bei RXV). Alle Vektoren eines Laufs stehen in einer Datei (Blob) in Bank 1 und werden mit einem
einzigen Z64K-Start geprueft; das Ergebnis ist der Exitcode ($D7FF: 0 = alles gleich, 10 = Abweichung) und die
Datei RES auf dem Diskimage (Nummern und Ist-CRC der abweichenden Vektoren). Bei Abweichungen laeuft ein
zweiter, kurzer Start nur mit den ersten abweichenden Vektoren und schreibt deren VRAM als Dateien auf das
Image (Diagnose: erste falsche Adresse, Anzahl falscher Bytes wie bei VICE).
"""
import binascii, collections, os, random, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import z64k  # noqa: E402

ENTRY = {'init': 0x2F00, 'prep': 0x2F03, 'check': 0x2F06, 'fin': 0x2F09, 'finerr': 0x2F0C}
RES_ADDR = 0x3A00
EXIT_FAIL, EXIT_BASIC = 10, 2

Rec = collections.namedtuple('Rec', 'name params src stream crc checks')   # checks: [(addr, value)]
Fail = collections.namedtuple('Fail', 'idx flags crc addr actual')


def tables():
    r = random.Random(0x7A64)
    t1 = bytes(r.randrange(256) for _ in range(256))
    t2 = bytes(r.randrange(256) for _ in range(256))
    return t1, t2


def pattern():
    """Grundmuster des Z64K-Laufs: VRAM[256 p + y] = T1[y] xor T2[p] (6502: 16 Takte je Byte statt Zufallsdatei)."""
    t1, t2 = tables()
    return b''.join(bytes(a ^ c for a in t1) for c in t2)


def crc16(data):
    return binascii.crc_hqx(bytes(data), 0xFFFF)     # CCITT, MSB zuerst, Start $FFFF, wie zvt.s


def crc_tables():
    lo, hi = [], []
    for i in range(256):
        c = i << 8
        for _ in range(8):
            c = ((c << 1) ^ 0x1021) & 0xFFFF if c & 0x8000 else (c << 1) & 0xFFFF
        lo.append(c & 255)
        hi.append(c >> 8)
    return bytes(lo), bytes(hi)


def assemble(work):
    """zvt.s -> work/zvt.prg (mit Ladeadresse $2F00)."""
    t1, t2 = tables()
    lo, hi = crc_tables()
    with open(os.path.join(work, 'zvt_tables.inc'), 'w') as f:
        for name, tb in (('crctl', lo), ('crcth', hi), ('t1', t1), ('t2', t2)):
            f.write(name + ':\n')
            for k in range(0, 256, 16):
                f.write('        .byte ' + ','.join(str(x) for x in tb[k:k + 16]) + '\n')
    src = os.path.join(HERE, 'z64k')
    obj = os.path.join(work, 'zvt.o')
    prg = os.path.join(work, 'zvt.prg')
    subprocess.run(['ca65', '-I', work, '-o', obj, os.path.join(src, 'zvt.s')], check=True)
    subprocess.run(['ld65', '-C', os.path.join(src, 'zvt.cfg'), '-o', prg, obj], check=True)
    return prg


def make_blob(recs, pdst, flagad, diag=0, maxdiag=0):
    b = bytearray([0x00, 0x10])                       # Ladeadresse $1000 (Bank 1)
    b += len(recs).to_bytes(2, 'little') + pdst.to_bytes(2, 'little') + flagad.to_bytes(2, 'little')
    b += bytes([diag, maxdiag])
    for r in recs:
        b += bytes([len(r.params)]) + bytes(r.params)
        b += len(r.stream).to_bytes(2, 'little') + r.src.to_bytes(2, 'little') + bytes(r.stream)
        b += r.crc.to_bytes(2, 'little') + bytes([len(r.checks)])
        for a, v in r.checks:
            b += a.to_bytes(2, 'little') + bytes([v])
    if len(b) > 0xFF00 - 0x1000:
        raise ValueError(f'Blob zu gross: {len(b)} Byte (Bank 1 ab $1000)')
    return bytes(b)


def driver(text, fast=False):
    """VICE-Treiber (Haltepunkte $1BFE/$1BFF) -> Z64K-Treiber (SYS auf zvt.s). Aenderungen am Treiber:
    SYS INIT in Zeile 10, SYS PREP/CHECK statt der Haltepunkte, SYS FIN statt POKE $D7FF, TRAP -> FINERR."""
    t = text.replace('sys dec("1bfe")', f'sys {ENTRY["prep"]}').replace('sys dec("1bff")', f'sys {ENTRY["check"]}')
    n = t.count('poke dec("d7ff"),0')
    if n != 1:
        raise ValueError('Treiber hat nicht genau einen Ausgang POKE $D7FF,0')
    t = t.replace('poke dec("d7ff"),0', f'sys {ENTRY["fin"]}')
    lines = t.splitlines()
    first = lines[0]
    num, rest = first.split(' ', 1)
    lines[0] = f'{num} sys {ENTRY["init"]}:{rest}'
    lines.insert(0, f'5 trap 95')
    lines.append(f'95 poke {RES_ADDR + 4},er:sys {ENTRY["finerr"]}')
    return '\n'.join(lines) + '\n'


BOOT = '''10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:BANK 15:POKE DEC("D7FF"),4:END
25 BLOAD"ZVT",B0,U8:BLOAD"BLOB",B1,U8:IF DS THEN PRINT DS$:BANK 15:POKE DEC("D7FF"),4:END
30 SYS 7168
40 IF PEEK(DEC("1C03"))<>DEC("30") THEN PRINT"WRONG VDC-BASIC":BANK 15:POKE DEC("D7FF"),199:END
50 RUN"T"
'''


def sh(cmd, **kw):
    r = subprocess.run(cmd, **kw)
    if r.returncode:
        sys.exit(f'Fehler ({r.returncode}): {" ".join(str(c) for c in cmd)}')
    return r


def build_disk(work, bin_, drv, blob, compile_):
    """Diskimage: BOOT, VDCBASIC3, ZVT, BLOB und T (Treiber, interpretiert oder mit Blitz! kompiliert)."""
    sys.path.insert(0, HERE)
    import vicemon as vm
    open(os.path.join(work, 'boot.bas'), 'w').write(BOOT)
    open(os.path.join(work, 'drv.bas'), 'w').write(drv)
    open(os.path.join(work, 'blob.prg'), 'wb').write(blob)
    tok = [sys.executable, os.path.join(HERE, 'bastok.py')]
    boot = os.path.join(work, 'boot.prg')
    sh(tok + [os.path.join(work, 'boot.bas'), boot])
    t = os.path.join(work, 't.prg')
    if compile_:
        node, blitzc = vm.find_node(), vm.find_blitzc()
        if not node or not blitzc:
            print('SKIP (blitzc not found)')
            sys.exit(3)
        sh([sys.executable, os.path.join(HERE, 'blitzprep.py'), os.path.join(work, 'drv.bas'), os.path.join(work, 'tb.bas')])
        sh(tok + [os.path.join(work, 'tb.bas'), os.path.join(work, 'tb.prg')])
        sh([node, blitzc, os.path.join(work, 'tb.prg'), os.path.join(work, 'blitz'), '--name', 'T'],
           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        c = os.path.join(work, 'blitz', 'c.prg')
        if open(c, 'rb').read(2) != b'\x01\x40':
            sys.exit('Kompilat liegt nicht bei $4001')
        shutil.copy(c, t)
    else:
        sh(tok + ['--start', '0x4001', os.path.join(work, 'drv.bas'), t])
    d64 = os.path.join(work, 't.d64')
    if os.path.exists(d64):
        os.remove(d64)
    sh(['c1541', '-format', 'z64k,01', 'd64', d64, '-write', boot, 'boot', '-write', bin_, 'vdcbasic3',
        '-write', assemble(work), 'zvt', '-write', os.path.join(work, 'blob.prg'), 'blob',
        '-write', t, 't'], stdout=subprocess.DEVNULL)
    return d64, boot


def read_file(image, name):
    out = tempfile.mktemp(prefix='zvt-')
    try:
        r = subprocess.run(['c1541', '-attach', image, '-read', name, out], capture_output=True)
        if r.returncode or not os.path.exists(out):
            return None
        return open(out, 'rb').read()
    finally:
        if os.path.exists(out):
            os.remove(out)


def parse_res(data):
    """RES: 2 Byte Ladeadresse, nfail, ndone, berr, 3 frei, 8-Byte-Eintraege."""
    d = data[2:]
    nfail, ndone, berr = d[0] | d[1] << 8, d[2] | d[3] << 8, d[4]
    fails = []
    for k in range(min(nfail, 128)):
        e = d[8 + 8 * k:16 + 8 * k]
        fails.append(Fail(e[0] | e[1] << 8, e[2], e[3] | e[4] << 8, e[5] | e[6] << 8, e[7]))
    return nfail, ndone, berr, fails


Outcome = collections.namedtuple('Outcome', 'rc secs nfail ndone berr fails image work')


def session(recs, drv_text, pdst, flagad, bin_, compile_, timeout, diag=0, keep_work=False, label=''):
    """Ein Z64K-Start fuer alle Vektoren. Liefert Outcome (image/work nur mit keep_work, sonst geloescht)."""
    work = tempfile.mkdtemp(prefix='zvt-')
    ok = False
    try:
        blob = make_blob(recs, pdst, flagad, 1 if diag else 0, diag)
        d64, boot = build_disk(work, bin_, driver(drv_text), blob, compile_)
        # Wacheter: -limitcycles grosszuegig (rund 5 Mio. Zyklen je Vektor), Gesamt-Timeout in Sekunden
        limit = 200_000_000 + len(recs) * 12_000_000 + (diag * 400_000_000)
        saved = os.path.join(work, 'saved.d64')
        r = z64k.run(boot, d64, limit=limit, timeout=timeout, save=saved, log=os.path.join(work, 'z64k.log'))
        nfail = ndone = berr = None
        fails = []
        if r.image:
            data = read_file(r.image, 'res')
            if data:
                nfail, ndone, berr, fails = parse_res(data)
        out = Outcome(r.rc, r.secs, nfail, ndone, berr, fails, r.image, work)
        ok = keep_work
        return out
    finally:
        if not ok:
            shutil.rmtree(work, ignore_errors=True)


def read_dump(image, idx):
    # Kleinbuchstaben: c1541 macht daraus PETSCII $41-$5A, das schreibt zvt.s ("D", Hexziffern A-F)
    parts = [read_file(image, f'd{idx:04x}{k}') for k in range(4)]
    if any(p is None for p in parts):
        return None
    return b''.join(p[2:] for p in parts)


def first_diff(a, b):
    for i in range(len(a)):
        if a[i] != b[i]:
            return i
    return None


def verify(recs, exp_vram, drv_text, pdst, flagad, bin_, compile_, timeout, diag=3, jobs=None):
    """Alle Vektoren pruefen, bei Abweichungen die ersten `diag` per VRAM-Dump erklaeren.

    recs[0] ist der Selbsttest (Grundmuster, keine Befehle). Die uebrigen Vektoren werden auf `jobs` parallele
    Z64K-Starts verteilt (je ein eigenes Xvfb, je ein Selbsttest vorweg); jobs=None: min(3, Kerne - 1).
    exp_vram(i) -> erwarteter VRAM von recs[i]. Rueckgabe: (anzahl_ok ohne Selbsttest, abweichende Namen,
    Fehlertexte, letzter Outcome)."""
    import concurrent.futures
    jobs = jobs or max(1, min(3, (os.cpu_count() or 2) - 1))
    body = list(range(1, len(recs)))
    jobs = max(1, min(jobs, len(body) // 20 or 1))
    chunks = [body[k::jobs] for k in range(jobs)]          # verteilt: Gruppen gleichmaessig auf alle Starts
    errors, fails_g = [], []                               # fails_g: (globaler Index, Fail)
    secs, rcs, ndone_total = [], [], 0

    def one(ch):
        sub = [recs[0]] + [recs[i] for i in ch]
        return ch, session(sub, drv_text, pdst, flagad, bin_, compile_, timeout)

    with concurrent.futures.ThreadPoolExecutor(jobs) as ex:
        results = list(ex.map(one, chunks))
    for ch, out in results:
        sub = [0] + ch
        secs.append(out.secs)
        rcs.append(out.rc)
        tag = f' (Start {chunks.index(ch) + 1})' if jobs > 1 else ''
        if out.rc == z64k.EXIT_MISSING:
            sys.exit('Z64K nicht verfuegbar')
        if out.rc == z64k.EXIT_TIMEOUT:
            errors.append(f'Waechter-Timeout ({timeout} s){tag}')
        elif out.rc == 1:
            errors.append(f'-limitcycles erreicht (Waechter){tag}')
        elif out.rc == EXIT_BASIC:
            k = out.ndone - 1 if out.ndone else None
            nm = recs[sub[k]].name if k is not None and k < len(sub) else '-'
            errors.append(f'BASIC-Fehler {out.berr} (Vektor {nm}){tag}')
        elif out.rc not in (0, EXIT_FAIL):
            errors.append(f'unerwarteter Exitcode {out.rc}{tag} (4 = Laden vom Image, 199 = VDC-BASIC nicht installiert)')
        if out.nfail is None:
            if out.rc in (0, EXIT_FAIL):
                errors.append(f'keine Ergebnisdatei RES auf dem Image{tag}')
            continue
        ndone_total += max(0, out.ndone - 1)
        if out.ndone != len(sub) and out.rc in (0, EXIT_FAIL):
            errors.append(f'nur {out.ndone} von {len(sub)} Vektoren gelaufen{tag}')
        for f in out.fails:
            if f.idx == 0:
                errors.append(f'Selbsttest (Grundmuster ohne Befehle) weicht ab{tag}: Harness oder VDC-Schreib/Lese-Pfad')
            else:
                fails_g.append((sub[f.idx], f))
        if out.nfail > len(out.fails):
            errors.append(f'{out.nfail - len(out.fails)} weitere Abweichungen nicht aufgelistet{tag}')
    fails_g.sort(key=lambda t: t[0])
    print(f'Z64K: {len(recs) - 1} Vektoren in {jobs} Start(s), je {", ".join(f"{x:.0f}" for x in secs)} s, '
          f'Exitcodes {rcs}', flush=True)
    dumps = {}
    sel = fails_g[:diag] if diag else []
    if sel:
        sub = [recs[g] for g, _ in sel]
        o2 = session(sub, drv_text, pdst, flagad, bin_, False, timeout, diag=len(sub), keep_work=True)
        try:
            if o2.image:
                for k, (g, _) in enumerate(sel):
                    dumps[g] = read_dump(o2.image, k)
        finally:
            shutil.rmtree(o2.work, ignore_errors=True)
    bad = []
    for g, f in fails_g:
        r = recs[g]
        bad.append(r.name)
        line = f'FAIL  {r.name}  CRC ist ${f.crc:04X}, soll ${r.crc:04X}'
        msg = []
        if f.flags & 2:
            msg.append(f'RAM ${f.addr:04X} ist ${f.actual:02X}')
        got = dumps.get(g)
        if f.flags & 1 and got:
            exp = exp_vram(g)
            a = first_diff(got, exp)
            if a is None:
                msg.append('VRAM-Dump gleich (Abweichung nur beim ersten Lauf, nicht reproduzierbar)')
            else:
                n = sum(1 for k in range(65536) if got[k] != exp[k])
                msg.append(f'VRAM ${a:04X}: ist ${got[a]:02X}, soll ${exp[a]:02X} ({n} Byte falsch)')
        elif f.flags & 1 and g in dumps:
            msg.append('VRAM-Dump des Diagnoselaufs fehlt')
        print(line + ('\n      ' + '; '.join(msg) if msg else ''), flush=True)
    return ndone_total - len(fails_g), bad, errors, results[-1][1]
