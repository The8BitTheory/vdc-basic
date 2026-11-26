#!/usr/bin/env python3
"""P2b: RXV in VICE gegen vrl.decode prüfen (docs/v3/P2b-rxv.md, 7.1/7.2).

Aufruf:
  test/rxv/run_rxv.py [-c] [--port N] [--only REGEX] [--bin vdcbasic3.bin] [--sym build/vdcbasic3.sym]
                      [--cycles test/rxv/cycles.md] [--hw test/rxv-hw.bas] [--list]

- vrl.py: aus tools/ im Repository (kanonisch); DBE_TOOLS (Verzeichnis) hat Vorrang, wenn gesetzt.
  Fehlt es -> Meldung, Exitcode 2.
- Ein VICE-Lauf für alle Vektoren. Treiber rxvt.bas (interpretiert, mit -c als Blitz!-Kompilat) ruft
  je Vektor SYS $1BFE (Haltepunkt: Harness schreibt Grundmuster ins VRAM, Strom ins RAM, src/dst/T in
  den Parameterblock $1BF0), dann RXV, dann SYS $1BFF (Haltepunkt: Harness sichert den ganzen VRAM
  und vergleicht mit vrl.decode, dazu $1C06/$1C08).
- Monitor über test/vicemon.py (Port DBE_VICE_PORT, bind-Test, nur eigener Prozess), ereignisgesteuert.
- Stoppuhr zwischen den Haltepunkten rxv_execute und complex_instruction_shared_exit (Information).
- --hw schreibt das Hardware-Listing test/rxv-hw.bas (Sollwerte als Prüfsummen), --cycles die Tabelle.
Exitcode: 0 alle gleich, 1 Abweichung oder Fehler, 2 Aufruf/Umgebung.
"""
import argparse, collections, os, random, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
TEST = os.path.dirname(HERE)
ROOT = os.path.dirname(TEST)
sys.path.insert(0, TEST)
import vicemon as vm  # noqa: E402
import zvt  # noqa: E402

_tools = os.environ.get('DBE_TOOLS') or os.path.join(ROOT, 'tools')
if not os.path.exists(os.path.join(_tools, 'vrl.py')):
    print(f'vrl.py nicht gefunden in {_tools} (DBE_TOOLS setzen)', file=sys.stderr)
    sys.exit(2)
sys.path.insert(0, _tools)
import vrl  # noqa: E402

PARAM = 0x1BF0          # Parameterblock: src lo/hi, dst lo/hi, T, Ende-Flag, R6, RGD(28)
BP_PREP, BP_CHECK = 0x1BFE, 0x1BFF
T_DEFAULT = 14
SRC = 0x8000

Vec = collections.namedtuple('Vec', 'name stream src dst T group r6')

DRIVER = '''10 {fast}b=dec("1bf0"):poke dec("1bfe"),96:poke dec("1bff"),96
20 q=rgd(28):poke b+7,q:rgo 28,16
30 sys dec("1bfe"):if peek(b+5) then 90
40 s=peek(b)+256*peek(b+1):d=peek(b+2)+256*peek(b+3):poke dec("1c04"),peek(b+4):rgw 6,peek(b+6)
50 rxv s,d:sys dec("1bff"):goto 30
90 rgw 6,25:bank 15:poke dec("d7ff"),0
'''

BOOT = '''10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:END
30 SYS 7168
40 IF PEEK(DEC("1C03"))<>DEC("30") THEN PRINT"WRONG VDC-BASIC":END
50 RUN"T"
'''


def h(s):
    return bytes.fromhex(s.replace(' ', ''))


def base_pattern():
    r = random.Random(7)
    return bytes(r.randrange(256) for _ in range(65536))


def rand_sequence(r, n, alpha, skip_pct):
    vals = [r.randrange(256) for _ in range(alpha)]
    seq = []
    while len(seq) < n:
        x = r.random()
        if x < 0.02:                                # langer Lauf eingestreut
            seq += [r.choice(vals)] * r.randrange(128, 1500)
        elif x < 0.25:                              # kurzer Lauf
            seq += [r.choice(vals)] * r.randrange(2, 40)
        elif r.randrange(100) < skip_pct:
            seq += [vrl.SKIP] * r.randrange(1, 30)
        else:
            seq.append(r.choice(vals))
    return seq[:n]


def vectors():
    V = []

    def add(name, stream, group, src=SRC, dst=0x1234, T=T_DEFAULT, r6=25):
        V.append(Vec(name, bytes(stream), src, dst, T, group, r6))

    v = 0xA5
    # Gruppe B
    add('empty', h('00'), 'B')
    add('lit1', h('01 05 00'), 'B')
    add('lit127', bytes([0x7F]) + bytes(range(1, 128)) + b'\0', 'B')
    s = vrl.encode(list(range(128)))
    assert s == bytes([0x7F]) + bytes(range(127)) + bytes([1, 127, 0]), s.hex()
    add('lit128', s, 'B')
    add('zero-data', h('02 00 00 80 00 00'), 'B')
    add('foreign-1', h('80 07 80 07 00'), 'B')
    add('foreign-2', h('FF 01 00 07 00'), 'B')
    add('foreign-3', h('FE 05 00 00'), 'B')
    s = vrl.encode_rect([1, 2, 3, 4], 2, 2, 80)
    assert s == h('02 01 02 FE 4E 00 02 03 04 00'), s.hex()
    add('rect', s, 'B')
    add('rect-mask', vrl.encode_rect([1, 2, 3, 4], 2, 2, 10, mask=[1, 0, 0, 1]), 'B')
    # Gruppe R: jeder Vektor mit allen T
    R = [('run2', bytes([0x80, v, 0])), ('run3', bytes([0x81, v, 0])),
         ('runT-1', bytes([0x8B, v, 0])), ('runT', bytes([0x8C, v, 0])), ('runT+1', bytes([0x8D, v, 0])),
         ('run127', bytes([0xFD, v, 0])), ('lrun1', bytes([0xFF, 1, 0, v, 0])),
         ('lrun128', bytes([0xFF, 0x80, 0, v, 0])), ('lrun255', bytes([0xFF, 0xFF, 0, v, 0])),
         ('lrun256', bytes([0xFF, 0, 1, v, 0])), ('lrun257', bytes([0xFF, 1, 1, v, 0])),
         ('lrun511', bytes([0xFF, 0xFF, 1, v, 0])), ('lrun512', bytes([0xFF, 0, 2, v, 0])),
         ('lrun513', bytes([0xFF, 1, 2, v, 0])), ('lrun65535', bytes([0xFF, 0xFF, 0xFF, v, 0])),
         ('lrun65536', bytes([0xFF, 0, 0, v, 0])),
         ('fill-fill', h('FF 00 01 07 FF 00 01 09 00')), ('fill-lit', h('FF 00 02 07 01 09 00')),
         ('fill-run', h('FF 00 02 07 81 09 00'))]
    for T in (0, 1, 2, 3, 14, 127, 128, 255):
        for name, s in R:
            add(f'{name}/T{T}', s, 'R', T=T)
    # Gruppe S
    add('skip-first', h('FE 01 00 01 05 00'), 'S')
    add('skip0', h('FE 00 00 01 05 00'), 'S')
    add('skip-max', h('FE FE FF 01 01 00'), 'S', dst=0x0010)
    add('skip-end', h('01 05 FE 05 00 00'), 'S')
    add('fill-skip', h('FF 00 01 07 FE 10 00 01 09 00'), 'S')
    add('lit-skip', h('03 01 02 03 FE 02 00 01 04 00'), 'S')
    add('skip-skip', h('FE 03 00 FE 04 00 01 05 00'), 'S')
    # Gruppe P
    lit127 = bytes([0x7F]) + bytes((i * 7 + 3) & 255 for i in range(127))
    add('page-cross-lit', lit127 + b'\0', 'P', src=0x80FE)
    s = bytes([126]) + bytes(range(126)) + lit127 + lit127 + bytes([0x90, 0x33, 0])
    add('norm-edge', s, 'P', src=0x8000)
    r = random.Random(11)
    s = vrl.encode(rand_sequence(r, 600, 5, 10))
    add('below-4000', s, 'P', src=0x1300)
    s = vrl.encode(rand_sequence(r, 900, 5, 10))
    add('below-d000', s, 'P', src=0xD000 - len(s))
    s = vrl.encode([r.randrange(256) for _ in range(3000)] + rand_sequence(r, 3000, 2, 10)
                   + [r.randrange(256) for _ in range(1500)])
    assert len(s) > 4096
    add('long-stream', s, 'P', src=0x8000)
    # Gruppe Z
    add('wrap', bytes([32]) + bytes(range(100, 132)) + b'\0', 'Z', dst=0xFFF0)
    add('dst0', h('03 0A 0B 0C 85 0D FE 03 00 01 0E 00'), 'Z', dst=0x0000)
    add('dstFFFF', h('03 0A 0B 0C 85 0D FE 03 00 01 0E 00'), 'Z', dst=0xFFFF)
    # Gruppe X: Zufall (rand-0..4 kurz, damit rxv-hw.bas klein bleibt)
    r = random.Random(3)
    for i in range(60):
        n = r.randrange(0, 401) if i < 5 else r.randrange(0, 3001)
        alpha = r.choice((2, 5, 256))
        skip = r.choice((0, 10, 50))
        s = vrl.encode(rand_sequence(r, n, alpha, skip))
        add(f'rand-{i}', s, 'X', dst=r.randrange(65536), T=r.choice((0, 2, 14, 255)))
    data = open(os.path.join(ROOT, 'docs', 'listings', 'data', 'dungeon'), 'rb').read()
    pic = data[2:] if len(data) == 8002 else data[-8000:]
    add('dungeon', vrl.encode(pic), 'X', dst=0x0000)
    # Gruppe C: Zeitmessung (Pfade, T = 14) mit gesperrtem IRQ, je 8-mal: ausgewertet wird das Minimum (Rasterlage).
    # c-: normale Anzeige (R6 = 25); cb-: R6 = 0, keine sichtbaren Zeilen (VICE: kein Busy im Bild)
    for pre, r6 in (('c-', 25), ('cb-', 0)):
        for k in range(8):
            add(f'{pre}empty#{k}', h('00'), 'C', r6=r6)
            for n in (1, 64, 127):
                add(f'{pre}lit{n}#{k}', bytes([n]) + bytes(range(n)) + b'\0', 'C', r6=r6)
            for L in (2, 13):
                add(f'{pre}cpu{L}#{k}', bytes([0x7E + L, 0x5A, 0]), 'C', r6=r6)
            for L in (14, 127):
                add(f'{pre}fill{L}#{k}', bytes([0x7E + L, 0x5A, 0]), 'C', r6=r6)
            add(f'{pre}lrun300#{k}', h('FF 2C 01 5A 00'), 'C', r6=r6)
            add(f'{pre}skip#{k}', h('FE 05 00 00'), 'C', r6=r6)
    return V


def expected(vec, base):
    toks = vrl.decode_tokens(vec.stream)
    u = sum(t.length for t in toks)
    d = vec.dst
    if u <= 65536:
        # vrl.decode auf den ab dst rotierten VRAM: Umlauf $FFFF -> $0000 ohne eigene Dekodierung
        rot = base[d:] + base[:d]
        out = bytes(vrl.decode(vec.stream, rot)[:65536])
        exp = out[65536 - d:] + out[:65536 - d] if d else out
    else:
        # nur für Ströme, die der Encoder nie erzeugt (z. B. Skip 65536 vor Daten): vrl.decode lehnt
        # U > 65536 ab, RXV rechnet modulo 65536
        out = bytearray(base)
        pos = d
        for t in toks:
            if t.kind in ('lit', 'run', 'lrun'):
                data = t.value if t.kind == 'lit' else bytes((t.value,)) * t.length
                for k, b in enumerate(data):
                    out[(pos + k) & 0xFFFF] = b
            pos += t.length
        exp = bytes(out)
    end_ptr = (vec.src + toks[-1].offset + 1) & 0xFFFF
    return exp, end_ptr, (d + u) & 0xFFFF


def sh(cmd, **kw):
    r = subprocess.run(cmd, **kw)
    if r.returncode:
        sys.exit(f'Fehler ({r.returncode}): {" ".join(cmd)}')
    return r


def build_disk(work, bin_, compile_, fast):
    open(os.path.join(work, 'boot.bas'), 'w').write(BOOT)
    open(os.path.join(work, 'rxvt.bas'), 'w').write(DRIVER.format(fast='fast:' if fast else ''))
    tok = [sys.executable, os.path.join(TEST, 'bastok.py')]
    boot = os.path.join(work, 'boot.prg')
    sh(tok + [os.path.join(work, 'boot.bas'), boot])
    t = os.path.join(work, 't.prg')
    if compile_:
        node, blitzc = vm.find_node(), vm.find_blitzc()
        if not node or not blitzc:
            print('SKIP (blitzc not found)')
            sys.exit(3)
        sh([sys.executable, os.path.join(TEST, 'blitzprep.py'), os.path.join(work, 'rxvt.bas'),
            os.path.join(work, 'tb.bas')])
        sh(tok + [os.path.join(work, 'tb.bas'), os.path.join(work, 'tb.prg')])
        sh([node, blitzc, os.path.join(work, 'tb.prg'), os.path.join(work, 'blitz'), '--name', 'T'],
           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        c = os.path.join(work, 'blitz', 'c.prg')
        if open(c, 'rb').read(2) != b'\x01\x40':
            sys.exit('Kompilat liegt nicht bei $4001')
        shutil.copy(c, t)
        print(f'Kompilat: $4001-${0x4001 + os.path.getsize(c) - 3:04X}')
    else:
        sh(tok + ['--start', '0x4001', os.path.join(work, 'rxvt.bas'), t])
    d64 = os.path.join(work, 't.d64')
    sh(['c1541', '-format', 'rxv,01', 'd64', d64, '-write', boot, 'boot', '-write', bin_, 'vdcbasic3',
        '-write', t, 't'], stdout=subprocess.DEVNULL)
    return d64, boot


def flags(m):
    t = m.cmd('r')
    return int(re.findall(r'\.;[0-9a-f]{4}(?: [0-9a-f]{2}){6} ([01]{8})', t)[-1], 2)


def first_diff(a, b):
    for i in range(len(a)):
        if a[i] != b[i]:
            return i
    return None


def run(vecs, args):
    sym = vm.symbols(args.sym)
    work = tempfile.mkdtemp(prefix='rxv-')
    try:
        d64, boot = build_disk(work, args.bin, args.compile, args.fast)
        base = base_pattern()
        basef = os.path.join(work, 'base.bin')
        open(basef, 'wb').write(base)
        vramf = os.path.join(work, 'vram.bin')
        streamf = os.path.join(work, 's.bin')
        res = {'fail': [], 'ok': 0, 'cycles': {}, 'reg28': None, 'errors': []}
        state = {'i': 0, 'started': False}
        lo_src = min(v.src for v in vecs)
        hi_src = max(v.src + len(v.stream) for v in vecs)
        print(f'{len(vecs)} Vektoren, Ströme ${lo_src:04X}-${hi_src - 1:04X}', flush=True)

        def script(m):
            ev = m.mem(0x300, 2)
            err_at = ev[0] | ev[1] << 8
            m.bp(BP_PREP)
            m.bp(BP_CHECK)
            m.bp(err_at)
            m.bp(sym['rxv_execute'])
            m.bp(sym['complex_instruction_shared_exit'])
            cur = None
            while True:
                try:
                    pc, out = m.go()
                except vm.Exited:
                    return
                if pc == err_at:
                    res['errors'].append(f'BASIC-Fehler {m.reg_x()} (Vektor {cur.name if cur else "-"})')
                    m.s.sendall(b'quit\n')
                    return
                if pc == sym['rxv_execute']:
                    if cur is not None and cur.group == 'C':
                        # Zeitmessung ohne IRQ: I-Flag setzen, am Ende alten Zustand zurück
                        state['fl'] = flags(m)
                        m.cmd(f'r fl = ${state["fl"] | 4:02x}')
                    m.cmd('stopwatch reset')
                    continue
                if pc == sym['complex_instruction_shared_exit']:
                    if cur is not None:
                        t = m.cmd('stopwatch')
                        res['cycles'][cur.name] = int(re.search(r'Stopwatch:\s*(\d+)', t).group(1))
                        if state.get('fl') is not None:
                            m.cmd(f'r fl = ${(flags(m) & ~4) | (state["fl"] & 4):02x}')
                            state['fl'] = None
                    continue
                if pc == BP_PREP:
                    if res['reg28'] is None:
                        m.cmd('bank ram00')
                        res['reg28'] = m.mem(PARAM + 7)[0]
                    if state['i'] >= len(vecs):
                        m.cmd('bank ram00')
                        m.cmd(f'> {PARAM + 5:04x} 01')
                        cur = None
                        continue
                    cur = vecs[state['i']]
                    open(streamf, 'wb').write(cur.stream)
                    m.cmd('bank vdc')
                    t = m.cmd(f'bload "{basef}" 0 0000')
                    if 'Loading' not in t:
                        raise RuntimeError('bload VRAM: ' + t)
                    m.cmd('bank ram00')
                    t = m.cmd(f'bload "{streamf}" 0 {cur.src:04x}')
                    if 'Loading' not in t:
                        raise RuntimeError('bload RAM: ' + t)
                    p = [cur.src & 255, cur.src >> 8, cur.dst & 255, cur.dst >> 8, cur.T, 0, cur.r6]
                    m.cmd(f'> {PARAM:04x} ' + ' '.join(f'{x:02x}' for x in p))
                    continue
                if pc == BP_CHECK:
                    m.cmd('bank vdc')
                    if os.path.exists(vramf):
                        os.remove(vramf)
                    m.cmd(f'bsave "{vramf}" 0 0000 ffff')
                    m.cmd('bank ram00')
                    hdr = m.mem(0x1C06, 4)
                    got = open(vramf, 'rb').read()
                    exp, e_end, e_vram = expected(cur, base)
                    g_end, g_vram = hdr[0] | hdr[1] << 8, hdr[2] | hdr[3] << 8
                    msg = []
                    if got != exp:
                        a = first_diff(got, exp)
                        n = sum(1 for k in range(65536) if got[k] != exp[k])
                        msg.append(f'VRAM ${a:04X}: ist ${got[a]:02X}, soll ${exp[a]:02X} ({n} Byte falsch)')
                    if g_end != e_end:
                        msg.append(f'$1C06 ist ${g_end:04X}, soll ${e_end:04X}')
                    if g_vram != e_vram:
                        msg.append(f'$1C08 ist ${g_vram:04X}, soll ${e_vram:04X}')
                    cyc = res['cycles'].get(cur.name)
                    line = f'{"FAIL" if msg else "ok  "}  {cur.name:<18} src ${cur.src:04X} dst ${cur.dst:04X} ' \
                           f'T {cur.T:<3} {len(cur.stream):5} B  {cyc if cyc is not None else "-":>7} Zyklen'
                    if msg:
                        res['fail'].append(cur.name)
                        print(line + '\n      ' + '; '.join(msg), flush=True)
                    else:
                        res['ok'] += 1
                        if args.verbose:
                            print(line, flush=True)
                    state['i'] += 1
                    continue
                raise RuntimeError(f'unerwarteter Halt bei ${pc:04X}')

        cmd = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
               '+drive8truedrive', '-trapdevice8', '-8', d64, '-autostartprgmode', '1', '-autostart', boot,
               '-limitcycles', '4000000000']
        _, rc = vm.run_vice(cmd, args.port, args.timeout, script)
        res['rc'] = rc
        return res
    finally:
        shutil.rmtree(work, ignore_errors=True)


def run_z64k(vecs, args):
    """Z64K statt VICE: ohne Monitor. Grundmuster, Vergleich und Strom-Ladung macht zvt.s (test/zvt.py);
    ein Start fuer alle Vektoren ausser der Zeitgruppe C (Zyklen messen nur der VICE-Monitor)."""
    vecs = [v for v in vecs if v.group != 'C']
    base = zvt.pattern()
    exps = {}

    def exp_of(v):
        if v.name not in exps:
            exps[v.name] = expected(v, base)
        return exps[v.name]

    selftest = Vec('selftest', h('00'), SRC, 0, T_DEFAULT, 'B', 25)
    recs = []
    for v in [selftest] + vecs:
        exp, e_end, e_vram = exp_of(v)
        params = bytes([v.src & 255, v.src >> 8, v.dst & 255, v.dst >> 8, v.T, 0, v.r6])
        checks = [(0x1C06, e_end & 255), (0x1C07, e_end >> 8), (0x1C08, e_vram & 255), (0x1C09, e_vram >> 8)]
        # Bereich: dst .. dst + U - 1 (Umlauf moeglich), U = Summe der Token-Laengen; selftest schreibt nichts
        u = 0 if v is selftest else sum(t.length for t in vrl.decode_tokens(v.stream))
        recs.append(zvt.Rec(v.name, params, v.src, v.stream, zvt.merge_ranges(zvt.runs_from_extent(v.dst, u)), checks))
    allv = [selftest] + vecs
    print(f'{len(vecs)} Vektoren (+ Selbsttest), Ströme ${min(v.src for v in vecs):04X}-'
          f'${max(v.src + len(v.stream) for v in vecs) - 1:04X}', flush=True)
    ok, bad, errors, out = zvt.verify(recs, lambda i: exp_of(allv[i])[0], DRIVER.format(fast=''), PARAM, PARAM + 5,
                                      args.bin, args.compile, args.timeout, diag=0 if args.no_diag else 3, jobs=args.jobs)
    return ok, bad, errors, out


def checksum(data):
    return sum(data) & 0xFFFF


def write_hw(path, vecs):
    """Hardware-Listing: Ströme per DATA, Grund 90 per VMF, Prüfsumme über den Bereich dst..dst+U-1."""
    want = ['fill-skip', 'lit-skip', 'lrun257/T14', 'rand-0', 'rand-1', 'rand-2', 'rand-3', 'rand-4']
    byname = {v.name: v for v in vecs}
    lines = ['# Hardware check (docs/v3/P2b-rxv.md 3.3, 3.4, 7.4): RXV vectors with a skip directly after a literal',
             '# and after a block fill, R30 = 0, a 65536-byte fill (screen saved and restored), random streams.',
             '# Generated by test/rxv/run_rxv.py --hw. Runs on a real C128 (result as text) and in VICE (exit code).',
             '1 trap 9000',
             '2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995',
             '10 a=dec("1300"):e=0:k=0:bank 0',
             '20 read n$:if n$="end" then 300',
             '30 read t,d,u,c,ln:k=k+1:for i=0 to ln-1:read b:poke a+i,b:next',
             '40 vmf d,90,u+1:poke dec("1c04"),t',
             '50 rxv a,d',
             '60 s=0:if u=0 then 80',
             '65 for i=0 to u-1:q=vmd(d+i):s=s+q:if s>65535 then s=s-65536',
             '70 next',
             '80 q=vmd(d+u):if s<>c or q<>90 then print n$;": fail":e=k',
             '90 if peek(dec("1c06"))+256*peek(dec("1c07"))<>a+ln then print n$;": $1c06 fail":e=k',
             '100 if peek(dec("1c08"))+256*peek(dec("1c09"))<>d+u then print n$;": $1c08 fail":e=k',
             '110 goto 20',
             '300 rem 65536 bytes of 165: save the screen (16 kib), fill, check samples, restore',
             '310 vtr 0,dec("8000"),16384:poke a,255:poke a+1,0:poke a+2,0:poke a+3,165:poke a+4,0',
             '320 rxv a,1000',
             '330 f=0:for i=0 to 65535 step 251:q=vmd(i):if q<>165 then f=1',
             '340 next',
             '350 q=peek(dec("1c08"))+256*peek(dec("1c09")):rtv dec("8000"),0,16384',
             '360 if f or q<>1000 then print "lrun65536: fail":e=99',
             '370 r=e:if e=0 then print "rxv-hw: all ok"',
             '380 goto 8995']
    ln = 1000
    for nm in want:
        v = byname[nm]
        exp_u = sum(t.length for t in vrl.decode_tokens(v.stream))
        dst = 0x1000
        assert exp_u + 1 <= 0x1000, nm
        out = vrl.decode(v.stream, bytes([90]) * exp_u)
        lines.append(f'{ln} data "{nm.split("/")[0]}",{v.T},{dst},{exp_u},{checksum(out[:exp_u])},{len(v.stream)}')
        ln += 1
        bs = list(v.stream)
        for k in range(0, len(bs), 20):
            lines.append(f'{ln} data ' + ','.join(str(b) for b in bs[k:k + 20]))
            ln += 1
    lines.append(f'{ln} data "end"')
    lines += ['8995 bank 15:poke dec("d7ff"),r:end', '9000 r=200+er:goto 8995']
    open(path, 'w').write('\n'.join(lines) + '\n')
    print(f'{path}: {len(want)} Vektoren + lrun65536, {len(lines)} Zeilen')


def write_cycles(path, res, mode, fast, append):
    f = 2 if fast else 1        # Stoppuhr zählt Systemtakte (1 MHz); bei FAST laufen je Takt 2 CPU-Zyklen
    c = {}
    for k, val in res['cycles'].items():
        b = k.split('#')[0]
        c[b] = min(c.get(b, val), val)
    c = {k: v * f for k, v in c.items()}

    def table(pre):
        def g(n):
            return c[pre + n]
        lit = (g('lit127') - g('lit1')) / 126
        cpu = (g('cpu13') - g('cpu2')) / 11
        e = g('empty')
        return [
            ('Aufruf ohne Token (`00`): Setup + Ende', e, 'empty'),
            ('Literal je Byte', lit, '(lit127 − lit1) / 126'),
            ('Literal-Token fest', g('lit1') - e - lit, 'lit1 − empty − 1 Byte'),
            ('Lauf L < T (CPU) je Byte', cpu, '(L13 − L2) / 11, T = 14'),
            ('Lauf L < T (CPU) fest', g('cpu2') - e - 2 * cpu, 'L2 − empty − 2 Byte'),
            ('Lauf L ≥ T (Block fill), L = 14', g('fill14') - e, 'fill14 − empty, inkl. Warten am Ende'),
            ('Lauf L ≥ T (Block fill), L = 127', g('fill127') - e, 'fill127 − empty, inkl. Warten am Ende'),
            ('Langer Lauf L = 300 (Block fill, 2 Seiten)', g('lrun300') - e, 'lrun300 − empty, inkl. Warten am Ende'),
            ('Skip', g('skip') - e, 'skip − empty'),
        ]
    out = [] if append else [
        '# RXV: Zyklen in VICE (vorläufig)', '',
        'Erzeugt von `test/rxv/run_rxv.py --cycles F` (1 MHz) und `--fast --cycles-append F` (2 MHz). Stoppuhr des',
        'VICE-Monitors von `rxv_execute` bis `complex_instruction_shared_exit`, IRQ für die Messung gesperrt (I-Flag',
        'über den Monitor), je Vektor das Minimum aus 8 Läufen (die Rasterlage streut). Die Stoppuhr zählt',
        'Systemtakte zu 1 MHz: ohne `FAST` 1 CPU-Zyklus je Takt, mit `FAST` 2; die Tabellen zeigen **CPU-Zyklen**.',
        'Der CPU-Pfad (Vergleich mit dem Listing, docs/v3/P2b-rxv.md 6) steht in „1 MHz, R6 = 0“: dort wartet die',
        'CPU nicht auf den VDC. Bei 2 MHz begrenzt in VICE der VDC.', '',
        '**Vorläufig:** VICE modelliert den VDC-Zugriff vereinfacht (Busy-Zeit je Reg-31-Zugriff im sichtbaren',
        'Bereich); die Werte taugen nur zum Vergleich der Pfade untereinander. Block-fill-Zeilen enthalten das',
        'Warten auf das Ende des Fills beim Lesen von R18/R19 am Ende. Werte auf echter Hardware stehen aus.']
    for pre, title in (('cb-', 'ohne sichtbare Zeilen (R6 = 0)'), ('c-', 'normale Anzeige (R6 = 25)')):
        out += ['', f'## {"2 MHz" if fast else "1 MHz"}, {title}, {mode}', '', '| Pfad | Zyklen | Herleitung |',
                '|---|---|---|']
        for name, val, how in table(pre):
            out.append(f'| {name} | {val:.1f} | {how} |')
    big = [k for k in ('lrun65536/T14', 'dungeon', 'long-stream') if k in c]
    if big:
        out += ['', f'Ganze Ströme ({"2 MHz" if fast else "1 MHz"}, IRQ an, ein Lauf): ' +
                ', '.join(f'`{k}` {c[k]:.0f}' for k in big)]
    open(path, 'a' if append else 'w').write('\n'.join(out) + '\n')
    print(f'{path} geschrieben')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('-c', '--compile', action='store_true', help='Treiber mit Blitz! 128 kompilieren')
    ap.add_argument('--emu', choices=('vice', 'z64k'), default='vice',
                    help='z64k: ohne Monitor, VRAM-Pruefsumme im 6502 (test/zvt.py), ohne Zeitgruppe C')
    ap.add_argument('--jobs', type=int, help='z64k: parallele Z64K-Starts (Standard: min(3, Kerne - 1))')
    ap.add_argument('--no-diag', action='store_true', help='z64k: keinen zweiten Lauf mit VRAM-Dump bei Abweichungen')
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=900)
    ap.add_argument('--only', help='Regex auf Vektornamen')
    ap.add_argument('--bin', default=os.environ.get('VDCBASIC', os.path.join(ROOT, 'vdcbasic3.bin')))
    ap.add_argument('--sym', default=os.path.join(ROOT, 'build', 'vdcbasic3.sym'))
    ap.add_argument('--cycles', help='Zyklentabelle schreiben (Markdown)')
    ap.add_argument('--cycles-append', help='Zyklentabelle an eine Datei anhängen')
    ap.add_argument('--hw', help='Hardware-Listing schreiben und beenden')
    ap.add_argument('--list', action='store_true')
    ap.add_argument('--fast', action='store_true', help='Treiber schaltet mit FAST auf 2 MHz')
    ap.add_argument('-v', '--verbose', action='store_true')
    args = ap.parse_args()
    vecs = vectors()
    if args.only:
        vecs = [v for v in vecs if re.search(args.only, v.name)]
    for v in vecs:
        if not (v.src >= 0x8000 and v.src + len(v.stream) <= 0xD000 or v.src == 0x1300):
            sys.exit(f'{v.name}: Strom liegt außerhalb der freien Bereiche')
    if args.list:
        for v in vecs:
            print(f'{v.name:<18} {v.group} src ${v.src:04X} dst ${v.dst:04X} T {v.T:<3} {len(v.stream)} B')
        return 0
    if args.hw:
        write_hw(args.hw, vectors())
        return 0
    if args.emu == 'z64k':
        if args.cycles or args.cycles_append or args.fast:
            sys.exit('--cycles/--fast gibt es nur mit VICE (Stoppuhr des Monitors)')
        ok, bad, errors, out = run_z64k(vecs, args)
        for e in errors:
            print(e)
        n = len([v for v in vecs if v.group != 'C'])
        print(f'{ok}/{n} gleich, {len(bad)} Abweichungen, Z64K-Exit {out.rc}')
        return 0 if not bad and not errors and out.rc == 0 else 1
    args.port = vm.choose_port(args.port)
    res = run(vecs, args)
    for e in res['errors']:
        print(e)
    n = res['ok'] + len(res['fail'])
    print(f'RGD(28) beim Start: ${res["reg28"]:02X}' if res['reg28'] is not None else 'RGD(28): -')
    print(f'{res["ok"]}/{len(vecs)} gleich, {len(res["fail"])} Abweichungen, VICE-Exit {res["rc"]}')
    path = args.cycles or args.cycles_append
    if path and n == len(vecs):
        write_cycles(path, res, 'Kompilat' if args.compile else 'interpretiert', args.fast, bool(args.cycles_append))
    ok = not res['fail'] and not res['errors'] and n == len(vecs) and res['rc'] == 0
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
