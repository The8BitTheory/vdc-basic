#!/usr/bin/env python3
"""VRAM-Vergleich ohne Monitor fuer Z64K (run_rxv.py / run_draw.py mit --emu z64k).

Prinzip (Begruendung in test/README.md, Abschnitt Z64K): Z64K hat keinen Monitor, der Haltepunkte setzt und den VRAM
sichert. Darum liegt ein kleines 6502-Programm (test/z64k/zvt.s, bei $2F00) im Rechner, das die Aufgaben der
beiden Haltepunkte $1BFE/$1BFF selbst erledigt. Der VRAM wird nur einmal je Start ganz mit dem Grundmuster geladen.
Jeder Fall hat eine kurze Liste von VRAM-Bereichen (hoechstens 16, aus der Menge der Bytes, die er laut Modell
aendert, plus Rand; Python berechnet sie), und nur diese Bereiche werden angefasst:
  PREP   Vorpruefung (CRC der Bereiche muss das Grundmuster sein, faengt Schreibzugriffe frueherer Faelle ab, die das
         Wiederherstellen sonst loeschen wuerde), Parameter und Strom des naechsten Vektors in den RAM schreiben;
  CHECK  CRC-16 (CCITT) ueber die Bereiche gegen den in Python vorausberechneten Wert (dazu optional einzelne
         RAM-Bytes, z. B. $1C06/$1C08 bei RXV), danach die Bereiche wieder mit dem Grundmuster laden. An jedem
         N-ten Vektor (Pruefpunkt) und am Ende dazu eine CRC ueber alle 64 KB (muss wieder das Grundmuster sein).
Schreibzugriffe ausserhalb des Fallbereichs zeigen also die Vorpruefung eines spaeteren Falls oder der naechste
Pruefpunkt. Dann wiederholt Python nur das Fenster seit dem letzten guten Pruefpunkt mit einer Gesamt-CRC nach
jedem Vektor und benennt den schuldigen Vektor (mit VRAM-Dump). Alle Vektoren eines Laufs stehen in einer Datei
(Blob) in Bank 1 und werden mit einem einzigen Z64K-Start geprueft; das Ergebnis ist der Exitcode ($D7FF: 0 = alles
gleich, 10 = Abweichung) und die Datei RES auf dem Diskimage (Nummern, Flags und Ist-CRC der abweichenden Vektoren).
Bei Abweichungen laeuft ein zweiter, kurzer Start nur mit den ersten abweichenden Vektoren und schreibt deren VRAM
als Dateien auf das Image (Diagnose: erste falsche Adresse, Anzahl falscher Bytes wie bei VICE).
Testhaken: ZVT_INJECT="wrong:NAME" (ein Byte im Fallbereich verfaelschen) oder "stray:NAME" (ein Byte ausserhalb
verfaelschen), jeweils im CHECK des Vektors NAME; beides muss als FAIL gemeldet werden.
"""
import binascii, collections, math, os, random, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import z64k  # noqa: E402

BUILD = os.path.join(ROOT, 'build')      # Temporaeres liegt in build/, nicht in /tmp
FULL_EVERY = 64                          # Gesamt-CRC alle N Vektoren (Pruefpunkt)
MAXRANGES = 16

ENTRY = {'init': 0x2F00, 'prep': 0x2F03, 'check': 0x2F06, 'fin': 0x2F09, 'finerr': 0x2F0C}
RES_ADDR = 0x3A00
EXIT_FAIL, EXIT_BASIC = 10, 2

# ranges: [(start, ende einschliesslich)] des VRAM, den der Fall beschreiben darf (merge_ranges); checks: [(addr, value)]
Rec = collections.namedtuple('Rec', 'name params src stream ranges checks')
Item = collections.namedtuple('Item', 'name params src stream ranges checks crc precrc')
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


def tmpdir(prefix):
    os.makedirs(BUILD, exist_ok=True)
    return tempfile.mkdtemp(prefix=prefix, dir=BUILD)


def merge_ranges(runs, margin=32, gap=64, maxn=MAXRANGES):
    """runs: [(start, ende einschliesslich)] -> verbreitert um margin, Naechste (Abstand <= gap) verschmolzen, dann die
    mit dem kleinsten Abstand, bis hoechstens maxn uebrig sind."""
    rs = sorted((max(0, a - margin), min(65535, b + margin)) for a, b in runs)
    out = []
    for a, b in rs:
        if out and a <= out[-1][1] + gap + 1:
            out[-1] = (out[-1][0], max(out[-1][1], b))
        else:
            out.append((a, b))
    while len(out) > maxn:
        k = min(range(len(out) - 1), key=lambda i: out[i + 1][0] - out[i][1])
        out[k:k + 2] = [(out[k][0], out[k + 1][1])]
    return out


def runs_from_extent(start, length):
    """Bereich ab start mit length Bytes, Umlauf $FFFF -> $0000 als zwei Laeufe."""
    if length <= 0:
        return []
    if length >= 65536:
        return [(0, 65535)]
    end = start + length - 1
    return [(start, end)] if end < 65536 else [(start, 65535), (0, end - 65536)]


def range_bytes(vram, ranges):
    return b''.join(bytes(vram[a:b + 1]) for a, b in ranges)


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


def make_blob(items, pdst, flagad, diag=0, maxdiag=0, fulls=(), fullcrc=0, inj=(0xFFFF, 0)):
    """fulls: Positionen (Index in items) mit Gesamt-CRC nach dem Vektor; diag: Maske (1 = Dump bei Abweichung im
    Bereich, 2 = bei Abweichung der Gesamt-CRC); inj: (Index, VRAM-Adresse) des Testhakens."""
    b = bytearray([0x00, 0x10])                       # Ladeadresse $1000 (Bank 1)
    b += len(items).to_bytes(2, 'little') + pdst.to_bytes(2, 'little') + flagad.to_bytes(2, 'little')
    b += bytes([diag, maxdiag]) + fullcrc.to_bytes(2, 'little') + inj[0].to_bytes(2, 'little') + inj[1].to_bytes(2, 'little')
    for i, r in enumerate(items):
        b += bytes([len(r.params)]) + bytes(r.params)
        b += len(r.stream).to_bytes(2, 'little') + r.src.to_bytes(2, 'little') + bytes(r.stream)
        b += r.crc.to_bytes(2, 'little') + r.precrc.to_bytes(2, 'little') + bytes([1 if i in fulls else 0, len(r.ranges)])
        for x, y in r.ranges:
            b += x.to_bytes(2, 'little') + y.to_bytes(2, 'little')
        b += bytes([len(r.checks)])
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
    out = os.path.join(tmpdir('zvt-rd-'), 'f')
    try:
        r = subprocess.run(['c1541', '-attach', image, '-read', name, out], capture_output=True)
        if r.returncode or not os.path.exists(out):
            return None
        return open(out, 'rb').read()
    finally:
        shutil.rmtree(os.path.dirname(out), ignore_errors=True)


def parse_res(data):
    """RES: 2 Byte Ladeadresse, nfail, ndone, berr, 3 frei, 8-Byte-Eintraege."""
    d = data[2:]
    nfail, ndone, berr = d[0] | d[1] << 8, d[2] | d[3] << 8, d[4]
    fails = []
    for k in range(min(nfail, 128)):
        e = d[8 + 8 * k:16 + 8 * k]
        fails.append(Fail(e[0] | e[1] << 8, e[2], e[3] | e[4] << 8, e[5] | e[6] << 8, e[7]))
    return nfail, ndone, berr, fails


def estimate_cycles(items, nfull, ndiag):
    """Grobe Obergrenze der emulierten Zyklen eines Starts (Waechter, keine Messung)."""
    n = 4_000_000 + nfull * 3_500_000 + ndiag * 400_000_000
    for r in items:
        n += 600_000 + 160 * sum(b - a + 1 for a, b in r.ranges) + 40 * len(r.stream)
    return n


def inject_spec(items):
    """ZVT_INJECT="wrong:NAME" oder "stray:NAME" -> (Index in items, VRAM-Adresse) oder None."""
    spec = os.environ.get('ZVT_INJECT')
    if not spec:
        return None
    kind, _, name = spec.partition(':')
    for i, r in enumerate(items):
        if r.name == name:
            if kind == 'wrong':
                if not r.ranges:
                    sys.exit(f'ZVT_INJECT: {name} hat keinen Bereich')
                return i, r.ranges[0][0]
            if kind == 'stray':
                for a in (0xFF00, 0xF7F0, 0x7000, 0x3C00, 0x1230, 0xC0DE):
                    if all(not (x - 512 <= a <= y + 512) for x, y in r.ranges):
                        return i, a
            sys.exit(f'ZVT_INJECT: unbekannte Art {kind!r} (wrong|stray)')
    return None


Outcome = collections.namedtuple('Outcome', 'rc secs nfail ndone berr fails image work')


def session(items, drv_text, pdst, flagad, bin_, compile_, timeout, fullcrc, fulls, diag=0, diagmask=1,
            keep_work=False):
    """Ein Z64K-Start fuer alle Vektoren (items). fulls: Positionen mit Gesamt-CRC. Liefert Outcome (image/work nur
    mit keep_work, sonst geloescht). Wiederholt einmal bei rc 125 (Absturz des Java-Starters bei parallelen Starts)
    oder rc 124 (Waechter-Timeout, z. B. Start blieb vor der Emulation stehen)."""
    work = tmpdir('zvt-')
    ok = False
    try:
        inj = inject_spec(items) or (0xFFFF, 0)
        blob = make_blob(items, pdst, flagad, diagmask if diag else 0, diag, fulls, fullcrc, inj)
        d64, boot = build_disk(work, bin_, driver(drv_text), blob, compile_)
        # Waechter: -limitcycles gegen Endlosschleifen im 6502, Wallclock-Timeout gegen haengenden Java-Starter
        limit = estimate_cycles(items, len(fulls), diag) * 2 + 100_000_000
        guard = min(timeout, 120 + limit // 1_000_000)
        saved = os.path.join(work, 'saved.d64')
        for attempt in (1, 2):
            r = z64k.run(boot, d64, limit=limit, timeout=guard, save=saved, log=os.path.join(work, 'z64k.log'))
            if r.rc in (z64k.EXIT_MISSING, z64k.EXIT_TIMEOUT) and attempt == 1 and z64k.available() is None:
                print(f'Z64K-Start rc {r.rc}, einmalige Wiederholung', flush=True)
                continue
            break
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


def verify(recs, exp_vram, drv_text, pdst, flagad, bin_, compile_, timeout, diag=3, jobs=None, full_every=None):
    """Alle Vektoren pruefen, bei Abweichungen die ersten `diag` per VRAM-Dump erklaeren.

    recs[0] ist der Selbsttest (Grundmuster, keine Befehle, ohne Bereiche). Die uebrigen Vektoren werden auf `jobs`
    parallele Z64K-Starts verteilt (je ein eigenes Xvfb, je ein Selbsttest vorweg); jobs=None: min(3, Kerne - 1).
    exp_vram(i) -> erwarteter VRAM von recs[i] (nur in recs[i].ranges muss er stimmen). Rueckgabe: (anzahl_ok ohne
    Selbsttest, abweichende Namen, Fehlertexte, letzter Outcome)."""
    import concurrent.futures
    full_every = full_every or FULL_EVERY
    jobs = jobs or max(1, min(3, (os.cpu_count() or 2) - 1))
    base = pattern()
    fullcrc = crc16(base)
    items = []
    for i, r in enumerate(recs):
        exp = exp_vram(i)
        items.append(Item(*r, crc16(range_bytes(exp, r.ranges)), crc16(range_bytes(base, r.ranges))))
    body = list(range(1, len(items)))
    jobs = max(1, min(jobs, len(body) // 20 or 1))
    chunks = [body[k::jobs] for k in range(jobs)]          # verteilt: Gruppen gleichmaessig auf alle Starts
    errors, fails_g, strays = [], [], []                   # fails_g: (globaler Index, Fail); strays: (sub, Position, Fail)
    secs, rcs, ndone_total = [], [], 0

    def checkpoints(n):
        return {p for p in range(n) if p % full_every == 0} | {n - 1}

    def one(ch):
        sub = [items[0]] + [items[i] for i in ch]
        return ch, session(sub, drv_text, pdst, flagad, bin_, compile_, timeout, fullcrc, checkpoints(len(sub)))

    with concurrent.futures.ThreadPoolExecutor(jobs) as ex:
        results = list(ex.map(one, chunks))
    for ch, out in results:
        sub = [0] + ch
        secs.append(out.secs)
        rcs.append(out.rc)
        tag = f' (Start {chunks.index(ch) + 1})' if jobs > 1 else ''
        if out.rc == z64k.EXIT_MISSING:
            sys.exit('Z64K nicht verfuegbar oder Java-Starter abgestuerzt (rc 125, auch nach Wiederholung)')
        if out.rc == z64k.EXIT_TIMEOUT:
            errors.append(f'Waechter-Timeout{tag}')
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
                continue
            if f.flags & 3:
                fails_g.append((sub[f.idx], f))
            if f.flags & 12:
                strays.append((sub, f.idx, f))
        if out.nfail > len(out.fails):
            errors.append(f'{out.nfail - len(out.fails)} weitere Abweichungen nicht aufgelistet{tag}')
    fails_g.sort(key=lambda t: t[0])
    print(f'Z64K: {len(recs) - 1} Vektoren in {jobs} Start(s), je {", ".join(f"{x:.0f}" for x in secs)} s, '
          f'Exitcodes {rcs}', flush=True)
    dumps = {}
    sel = fails_g[:diag] if diag else []
    if sel:
        sub = [items[g] for g, _ in sel]
        o2 = session(sub, drv_text, pdst, flagad, bin_, False, timeout, fullcrc, {len(sub) - 1}, diag=len(sub),
                     diagmask=1, keep_work=True)
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
        line = f'FAIL  {r.name}  CRC ist ${f.crc:04X}, soll ${items[g].crc:04X}'
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
    # Schreibzugriffe ausserhalb der Fallbereiche: je Start nur das erste Auftreten eingrenzen
    seen = set()
    for sub, k, f in sorted(strays, key=lambda t: (t[0][1:2], t[1])):
        if tuple(sub) in seen:
            continue
        seen.add(tuple(sub))
        cps = sorted(checkpoints(len(sub)))
        c = max(p for p in cps if p < k)
        window = list(range(c + 1, k + 1)) if f.flags & 4 else list(range(c + 1, k))
        what = 'Gesamt-CRC' if f.flags & 4 else 'Vorpruefung'
        names = [recs[sub[p]].name for p in window]
        if not window:
            errors.append(f'Schreibzugriff ausserhalb der Fallbereiche ({what} bei Vektor {recs[sub[k]].name}) '
                          f'nicht eingegrenzt')
            continue
        wi = [items[0]] + [items[sub[p]] for p in window]
        o3 = session(wi, drv_text, pdst, flagad, bin_, False, timeout, fullcrc, set(range(len(wi))), diag=1,
                     diagmask=2, keep_work=True)
        try:
            hit = [e for e in o3.fails if e.idx > 0 and e.flags & 4]
            if not hit:
                errors.append(f'Schreibzugriff ausserhalb der Fallbereiche ({what} bei Vektor {recs[sub[k]].name}) '
                              f'im Fenster {names[0]} .. {names[-1]} nicht reproduzierbar')
                continue
            g = sub[window[hit[0].idx - 1]]
            bad.append(recs[g].name)
            msg = f'FAIL  {recs[g].name}  schreibt ausserhalb seines Bereichs (erkannt durch {what} bei {recs[sub[k]].name})'
            got = read_dump(o3.image, hit[0].idx) if o3.image else None
            if got:
                a = first_diff(got, base)
                if a is not None:
                    n = sum(1 for q in range(65536) if got[q] != base[q])
                    msg += f'\n      VRAM ${a:04X}: ist ${got[a]:02X}, soll ${base[a]:02X} ({n} Byte falsch)'
            print(msg, flush=True)
        finally:
            shutil.rmtree(o3.work, ignore_errors=True)
    return ndone_total - len(set(bad)), bad, errors, results[-1][1]
