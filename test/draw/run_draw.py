#!/usr/bin/env python3
"""P2c: VDM/VDL/VDR/VDB in VICE gegen das Referenzmodell prüfen (docs/v3/P2c-draw.md, 6.1 und 11.3/11.4).

Aufruf:
  test/draw/run_draw.py [-c] [--quick] [--group S1,L,...] [--only REGEX] [--port N] [--cycles test/draw/cycles.md]
                        [--bin vdcbasic3.bin] [--sym build/vdcbasic3.sym] [--list] [-v]

- Fälle aus test/ref/gen_vectors.build() (Gruppen S1 S0 V L R B G Z; --quick = gen_vectors.is_quick),
  dazu mit --cycles die Zeitmessgruppe C (Pfade aus Abschnitt 7, je 8-mal, ausgewertet wird das Minimum).
- Ein VICE-Lauf für alle Fälle. Treiber drawt.bas (interpretiert, mit -c als Blitz!-Kompilat) ruft je Fall
  SYS $1BFE (Haltepunkt: Harness lädt das Grundmuster random.Random(13) in den ganzen VRAM und schreibt T,
  R6 und die Befehle in den Parameterblock $1B00), führt die Befehle aus (ON K GOSUB), dann SYS $1BFF
  (Haltepunkt: Harness sichert den ganzen VRAM und vergleicht alle 65536 Byte mit dem Modell).
- Stoppuhr je Zeichenbefehl zwischen den Haltepunkten draw_begin (nach Parsen und Prüfen) und draw_end.
- Monitor über test/vicemon.py (Port DBE_VICE_PORT, bind-Test, nur eigener Prozess), ereignisgesteuert.
Exitcode: 0 alle gleich, 1 Abweichung oder Fehler, 2 Aufruf/Umgebung, 3 blitzc fehlt (-c).
"""
import argparse, hashlib, os, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
TEST = os.path.dirname(HERE)
ROOT = os.path.dirname(TEST)
sys.path.insert(0, TEST)
sys.path.insert(0, os.path.join(TEST, 'ref'))
import vicemon as vm  # noqa: E402
import zvt  # noqa: E402
import gen_vectors  # noqa: E402
from vdcdraw import Vdc  # noqa: E402

PARAM = 0x1B00          # Ende-Flag, RGD(28), T, R6, Anzahl, dann je Befehl Kennung + 5 Wörter
BP_PREP, BP_CHECK = 0x1BFE, 0x1BFF
KIND = {'VDM': 1, 'VDL': 2, 'VDR': 3, 'VDB': 4}

DRIVER = '''10 {fast}b=dec("1b00"):poke dec("1bfe"),96:poke dec("1bff"),96
20 q=rgd(28):poke b+1,q:rgo 28,16
30 sys dec("1bfe"):if peek(b) then 90
40 poke dec("1c04"),peek(b+2):rgw 6,peek(b+3):n=peek(b+4):a=b+5
50 if n=0 then 80
60 k=peek(a):x=peek(a+1)+256*peek(a+2):y=peek(a+3)+256*peek(a+4):w=peek(a+5)+256*peek(a+6)
65 h=peek(a+7)+256*peek(a+8):c=peek(a+9)+256*peek(a+10)
70 on k gosub 100,200,300,400:a=a+11:n=n-1:if n then 60
80 sys dec("1bff"):goto 30
90 rgw 6,25:bank 15:poke dec("d7ff"),0
100 vdm x,y,w:return
200 vdl x,y,w,h,c:return
300 vdr x,y,w,h,c:return
400 vdb x,y,w,h,c:return
'''

BOOT = '''10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:END
30 SYS 7168
40 IF PEEK(DEC("1C03"))<>DEC("30") THEN PRINT"WRONG VDC-BASIC":END
50 RUN"T"
'''

# Zeitmessung (Abschnitt 7): Name, Befehle, T. Je 8-mal mit R6 = 0 (cb-) und R6 = 25 (c-).
M0, M1 = ['VDM', 0, 0, 80], ['VDM', 1, 0, 80]
TIMING = [
    ('pt-m0', [M0, ['VDL', 21, 10, 21, 10, 1]], 14),
    ('pt-m1', [M1, ['VDL', 21, 10, 21, 10, 7]], 14),
    ('pt-y200', [M0, ['VDL', 21, 200, 21, 200, 1]], 14),
    ('v1', [M0, ['VDL', 21, 10, 21, 10, 1]], 14),
    ('v101', [M0, ['VDL', 21, 10, 21, 110, 1]], 14),
    ('d1', [M0, ['VDL', 21, 10, 22, 11, 1]], 14),
    ('d101', [M0, ['VDL', 21, 10, 121, 110, 1]], 14),
    ('h2px', [M0, ['VDL', 21, 10, 22, 10, 1]], 14),
    ('edges-m0', [M0, ['VDB', 3, 10, 10, 1, 1]], 14),
    ('edges-m1', [M1, ['VDB', 1, 10, 4, 1, 7]], 14),
    ('mid2-m1', [M1, ['VDB', 0, 10, 4, 1, 7]], 14),
    ('mid12-m1', [M1, ['VDB', 0, 10, 24, 1, 7]], 14),
    ('mid14-m1', [M1, ['VDB', 0, 10, 28, 1, 7]], 14),
    ('mid70-m1', [M1, ['VDB', 0, 10, 140, 1, 7]], 14),
    ('b70x1', [M1, ['VDB', 0, 0, 140, 1, 4]], 14),
    ('b70x96', [M1, ['VDB', 0, 0, 140, 96, 4]], 14),
    ('x-m0-1', [M0, ['VDB', 3, 10, 21, 1, 2]], 14),
    ('x-m0-9', [M0, ['VDB', 3, 10, 85, 1, 2]], 14),
    ('e2-m0-h1', [M0, ['VDB', 3, 10, 10, 1, 1]], 14),
    ('e2-m0-h11', [M0, ['VDB', 3, 10, 10, 11, 1]], 14),
    ('e2-m1-h1', [M1, ['VDB', 1, 10, 4, 1, 7]], 14),
    ('e2-m1-h11', [M1, ['VDB', 1, 10, 4, 11, 7]], 14),
    ('cpu10-h1', [M1, ['VDB', 0, 10, 20, 1, 7]], 14),
    ('cpu10-h11', [M1, ['VDB', 0, 10, 20, 11, 7]], 14),
    ('r10x10', [M0, ['VDR', 3, 10, 10, 10, 1]], 14),
    ('l300x1', [M0, ['VDL', 3, 5, 303, 6, 1]], 14),
    ('l300x0', [M0, ['VDL', 3, 5, 303, 5, 1]], 14),
]


def sh(cmd, **kw):
    r = subprocess.run(cmd, **kw)
    if r.returncode:
        sys.exit(f'Fehler ({r.returncode}): {" ".join(cmd)}')
    return r


def build_disk(work, bin_, compile_, fast):
    open(os.path.join(work, 'boot.bas'), 'w').write(BOOT)
    open(os.path.join(work, 'drawt.bas'), 'w').write(DRIVER.format(fast='fast:' if fast else ''))
    tok = [sys.executable, os.path.join(TEST, 'bastok.py')]
    boot = os.path.join(work, 'boot.prg')
    sh(tok + [os.path.join(work, 'boot.bas'), boot])
    t = os.path.join(work, 't.prg')
    if compile_:
        node, blitzc = vm.find_node(), vm.find_blitzc()
        if not node or not blitzc:
            print('SKIP (blitzc not found)')
            sys.exit(3)
        sh([sys.executable, os.path.join(TEST, 'blitzprep.py'), os.path.join(work, 'drawt.bas'),
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
        sh(tok + ['--start', '0x4001', os.path.join(work, 'drawt.bas'), t])
    d64 = os.path.join(work, 't.d64')
    sh(['c1541', '-format', 'draw,01', 'd64', d64, '-write', boot, 'boot', '-write', bin_, 'vdcbasic3',
        '-write', t, 't'], stdout=subprocess.DEVNULL)
    return d64


def cases(args):
    groups = set(args.group.split(',')) if args.group else None
    data = gen_vectors.build(groups - {'C'} if groups else None, args.quick)
    out = []
    for c in data['cases']:
        out.append(dict(name=c['name'], group=c['group'], T=c['T'], cmds=c['cmds'], r6=25,
                        sha1=c['sha1'], diff=c['diff']))
    if groups and groups - {'C'} == set():
        out = []
    if args.cycles or (groups and 'C' in groups):
        base = gen_vectors.base_pattern()
        for pre, r6 in (('cb-', 0), ('c-', 25)):
            for k in range(8):
                for name, cmds, T in TIMING:
                    v = Vdc(base)
                    for cmd in cmds:
                        v.run(cmd)
                    out.append(dict(name=f'{pre}{name}#{k}', group='C', T=T, cmds=cmds, r6=r6,
                                    sha1=hashlib.sha1(bytes(v.ram)).hexdigest(),
                                    diff=gen_vectors.diff_runs(base, v.ram)))
    if args.only:
        out = [c for c in out if re.search(args.only, c['name'])]
    return out


def expected(c, base):
    b = bytearray(base)
    for a, hx in c['diff']:
        d = bytes.fromhex(hx)
        b[a:a + len(d)] = d
    return bytes(b)


def param_block(c):
    p = [0, 0, c['T'], c['r6'], len(c['cmds'])]
    for cmd in c['cmds']:
        vals = list(cmd[1:]) + [0] * (6 - len(cmd))
        p.append(KIND[cmd[0]])
        for v in vals:
            p += [v & 255, v >> 8]
    return p


def flags(m):
    t = m.cmd('r')
    return int(re.findall(r'\.;[0-9a-f]{4}(?: [0-9a-f]{2}){6} ([01]{8})', t)[-1], 2)


def run(cs, args):
    sym = vm.symbols(args.sym)
    work = tempfile.mkdtemp(prefix='draw-')
    try:
        d64 = build_disk(work, args.bin, args.compile, args.fast)
        base = gen_vectors.base_pattern()
        basef = os.path.join(work, 'base.bin')
        open(basef, 'wb').write(base)
        vramf = os.path.join(work, 'vram.bin')
        res = {'fail': [], 'ok': 0, 'cycles': {}, 'errors': []}
        state = {'i': 0, 'fl': None}
        print(f'{len(cs)} Fälle', flush=True)

        def script(m):
            ev = m.mem(0x300, 2)
            err_at = ev[0] | ev[1] << 8
            for a in (BP_PREP, BP_CHECK, err_at, sym['draw_begin'], sym['draw_end']):
                m.bp(a)
            cur = None
            while True:
                try:
                    pc, out = m.go()
                except vm.Exited:
                    return
                if pc == err_at:
                    res['errors'].append(f'BASIC-Fehler {m.reg_x()} (Fall {cur["name"] if cur else "-"})')
                    m.s.sendall(b'quit\n')
                    return
                if pc == sym['draw_begin']:
                    if cur is not None and cur['group'] == 'C':
                        state['fl'] = flags(m)
                        m.cmd(f'r fl = ${state["fl"] | 4:02x}')
                    m.cmd('stopwatch reset')
                    continue
                if pc == sym['draw_end']:
                    if cur is not None:
                        t = m.cmd('stopwatch')
                        res['cycles'].setdefault(cur['name'], []).append(
                            int(re.search(r'Stopwatch:\s*(\d+)', t).group(1)))
                        if state['fl'] is not None:
                            m.cmd(f'r fl = ${(flags(m) & ~4) | (state["fl"] & 4):02x}')
                            state['fl'] = None
                    continue
                if pc == BP_PREP:
                    m.cmd('bank ram00')
                    if state['i'] >= len(cs):
                        m.cmd(f'> {PARAM:04x} 01')
                        cur = None
                        continue
                    cur = cs[state['i']]
                    m.cmd('bank vdc')
                    t = m.cmd(f'bload "{basef}" 0 0000')
                    if 'Loading' not in t:
                        raise RuntimeError('bload VRAM: ' + t)
                    m.cmd('bank ram00')
                    p = param_block(cur)
                    m.cmd(f'> {PARAM + 2:04x} ' + ' '.join(f'{x:02x}' for x in p[2:]))
                    continue
                if pc == BP_CHECK:
                    m.cmd('bank vdc')
                    if os.path.exists(vramf):
                        os.remove(vramf)
                    m.cmd(f'bsave "{vramf}" 0 0000 ffff')
                    m.cmd('bank ram00')
                    got = open(vramf, 'rb').read()
                    cyc = res['cycles'].get(cur['name'], [])
                    ok = hashlib.sha1(got).hexdigest() == cur['sha1']
                    line = f'{"ok  " if ok else "FAIL"}  {cur["name"]:<26} T {cur["T"]:<3} ' \
                           f'{"+".join(str(x) for x in cyc) or "-":>12} Takte'
                    if not ok:
                        exp = expected(cur, base)
                        bad = [k for k in range(65536) if got[k] != exp[k]]
                        a = bad[0]
                        res['fail'].append(cur['name'])
                        print(line + f'\n      {cur["cmds"]}\n      VRAM ${a:04X}: ist ${got[a]:02X}, soll ${exp[a]:02X}'
                              f' ({len(bad)} Byte falsch, letztes ${bad[-1]:04X})', flush=True)
                    else:
                        res['ok'] += 1
                        if args.verbose:
                            print(line, flush=True)
                    state['i'] += 1
                    if state['i'] % 100 == 0:
                        print(f'  {state["i"]}/{len(cs)}', flush=True)
                    continue
                raise RuntimeError(f'unerwarteter Halt bei ${pc:04X}')

        cmd = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
               '+drive8truedrive', '-trapdevice8', '-8', d64, '-autostartprgmode', '1', '-autostart',
               os.path.join(work, 'boot.prg'), '-limitcycles', '40000000000']
        _, rc = vm.run_vice(cmd, args.port, args.timeout, script)
        res['rc'] = rc
        return res
    finally:
        shutil.rmtree(work, ignore_errors=True)


def run_z64k(cs, args):
    """Z64K statt VICE: ohne Monitor. Grundmuster (zvt.pattern), Vergleich (CRC-16 ueber 64 KB) und Parameter-
    Ladung macht zvt.s (test/zvt.py); ein Start fuer alle Faelle; je Fall werden nur die vom Modell
    geaenderten Bereiche (mit Rand) wiederhergestellt und per CRC geprueft, der ganze VRAM nur an Pruefpunkten. Die Faelle wurden schon gegen dieses Grundmuster
    berechnet (cases() mit ersetztem gen_vectors.base_pattern); Zeitgruppe C gibt es nur mit VICE."""
    cs = [c for c in cs if c['group'] != 'C']
    base = zvt.pattern()
    selftest = dict(name='selftest', group='-', T=14, cmds=[], r6=25, sha1='', diff=[])
    allc = [selftest] + cs

    def exp_of(i):                          # nicht zwischenspeichern: 1500 mal 64 KB
        return expected(allc[i], base)

    def ranges_of(c):                       # Bereich = geaenderte Laeufe laut Modell, mit Rand
        return zvt.merge_ranges([(a, a + len(hx) // 2 - 1) for a, hx in c['diff']])

    recs = [zvt.Rec(c['name'], bytes(param_block(c)[2:]), 0, b'', ranges_of(c), []) for c in allc]
    ok, bad, errors, out = zvt.verify(recs, exp_of, DRIVER.format(fast=''), PARAM + 2, PARAM,
                                      args.bin, args.compile, args.timeout, diag=0 if args.no_diag else 3, jobs=args.jobs)
    return ok, bad, errors, out, len(cs)


TARGETS = {   # Abschnitt 7
    'draw_addr': 250, 'pix_rmw': 130, 'span2': 330, 'vrun': 140, 'diag': 160, 'vdb_row': 60,
}


def write_cycles(path, res, mode, fast):
    f = 2 if fast else 1
    c = {}
    for k, vals in res['cycles'].items():
        if not k.startswith('c'):
            continue
        b = k.split('#')[0]
        c[b] = min(c.get(b, sum(vals)), sum(vals)) * 1
    c = {k: v * f for k, v in c.items()}

    def table(pre):
        def g(n):
            return c[pre + n]
        return [
            ('Punkt Bitmap (`VDL` x,y,x,y; draw_addr + pix_rmw + Ablauf)', g('pt-m0'), '—', 'pt-m0, y = 10'),
            ('Punkt chunky', g('pt-m1'), '—', 'pt-m1'),
            ('Punkt bei y = 200 (längere Multiplikation)', g('pt-y200'), '—', 'pt-y200'),
            ('senkrechter Lauf je Pixel', (g('v101') - g('v1')) / 100, '≤ 140', '(v101 − v1) / 100'),
            ('schräge Linie 45° je Pixel', (g('d101') - g('d1')) / 100, '≤ 160', '(d101 − d1) / 100'),
            ('`VDB` mit einer Zeile, zwei Ränder, m = 0, Bitmap (ganzer Befehl)', g('edges-m0'), '—', 'VDB 3,y,10,1'),
            ('Spanne mit zwei Rändern, m = 0, Bitmap, je Zeile', (g('e2-m0-h11') - g('e2-m0-h1')) / 10, '≤ 330',
             '(h11 − h1) / 10, VDB 3,y,10,h'),
            ('Spanne mit zwei Rändern, m = 0, chunky, je Zeile', (g('e2-m1-h11') - g('e2-m1-h1')) / 10, '≤ 330',
             '(h11 − h1) / 10, VDB 1,y,4,h'),
            ('`VDB` volle Bytes je Zeile, m = 10 < T (CPU)', (g('cpu10-h11') - g('cpu10-h1')) / 10, '≤ 60 + 150',
             '(h11 − h1) / 10, VDB 0,y,20,h chunky'),
            ('Mittelteil unter T je Byte (chunky)', (g('mid12-m1') - g('mid2-m1')) / 10, '15', '(m12 − m2) / 10'),
            ('Mittelteil m = 14 (Block fill, T = 14)', g('mid14-m1'), '—', 'VDB 0,y,28,1'),
            ('Mittelteil m = 70 (Block fill)', g('mid70-m1'), '—', 'VDB 0,y,140,1'),
            ('`VDB` volle Bytes je Zeile (70 Byte, Block fill)', (g('b70x96') - g('b70x1')) / 95, '≤ 60 + Fill',
             '(b70x96 − b70x1) / 95'),
            ('`VDB 0,0,140,96,c` chunky gesamt', g('b70x96'), '≤ 96 × (60 + Fill)', 'b70x96'),
            ('XOR-Mittelteil je Byte (Bitmap, c = 2)', (g('x-m0-9') - g('x-m0-1')) / 8, '—', '(m9 − m1) / 8'),
            ('`VDR 3,y,10,10,1` gesamt', g('r10x10'), '—', 'r10x10'),
            ('flache Linie dy = 1, dx = 300 (2 Spannen), je Pixel Fehlerterm', (g('l300x1') - g('l300x0')) / 300,
             '≈ 15', '(l300x1 − l300x0) / 300, Näherung'),
        ]
    out = ['# VDM/VDL/VDR/VDB: Zyklen in VICE (vorläufig)', '',
           'Erzeugt von `test/draw/run_draw.py --cycles F`. Stoppuhr des VICE-Monitors von `draw_begin` (nach Parsen',
           'und Prüfen) bis `draw_end`, IRQ für die Messung gesperrt (I-Flag über den Monitor), je Fall das Minimum',
           f'aus 8 Läufen. {"2 MHz (FAST)" if fast else "1 MHz"}, Treiber {mode}. Die Werte sind **CPU-Zyklen**.',
           '', '**Vorläufig:** VICE modelliert den VDC-Zugriff vereinfacht (Busy-Zeit je Reg-31-Zugriff im sichtbaren',
           'Bereich, 43 Takte statt 4 im Rand); Werte auf echter Hardware stehen aus. Ziele aus docs/v3/P2c-draw.md,',
           'Abschnitt 7; Abweichungen sind dort in Abschnitt 12 begründet.']
    for pre, title in (('cb-', 'ohne sichtbare Zeilen (R6 = 0): CPU-Pfad'), ('c-', 'normale Anzeige (R6 = 25)')):
        out += ['', f'## {title}', '', '| Pfad | Zyklen | Ziel (7) | Herleitung |', '|---|---|---|---|']
        for name, val, tgt, how in table(pre):
            out.append(f'| {name} | {val:.1f} | {tgt} | {how} |')
    open(path, 'w').write('\n'.join(out) + '\n')
    print(f'{path} geschrieben')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('-c', '--compile', action='store_true', help='Treiber mit Blitz! 128 kompilieren')
    ap.add_argument('--emu', choices=('vice', 'z64k'), default='vice',
                    help='z64k: ohne Monitor, VRAM-Pruefsumme im 6502 (test/zvt.py), ohne Zeitgruppe C')
    ap.add_argument('--jobs', type=int, help='z64k: parallele Z64K-Starts (Standard: min(3, Kerne - 1))')
    ap.add_argument('--no-diag', action='store_true', help='z64k: keinen zweiten Lauf mit VRAM-Dump bei Abweichungen')
    ap.add_argument('--quick', action='store_true', help='Teilmenge (gen_vectors.is_quick)')
    ap.add_argument('--group', help='kommagetrennt: S1,S0,V,L,R,B,G,Z,C')
    ap.add_argument('--only', help='Regex auf Fallnamen')
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=3600)
    ap.add_argument('--bin', default=os.environ.get('VDCBASIC', os.path.join(ROOT, 'vdcbasic3.bin')))
    ap.add_argument('--sym', default=os.path.join(ROOT, 'build', 'vdcbasic3.sym'))
    ap.add_argument('--cycles', help='Zeitmessgruppe C dazunehmen und Zyklentabelle schreiben (Markdown)')
    ap.add_argument('--fast', action='store_true', help='Treiber schaltet mit FAST auf 2 MHz')
    ap.add_argument('--list', action='store_true')
    ap.add_argument('-v', '--verbose', action='store_true')
    args = ap.parse_args()
    if args.emu == 'z64k':
        if args.cycles or args.fast:
            sys.exit('--cycles/--fast gibt es nur mit VICE (Stoppuhr des Monitors)')
        gen_vectors.base_pattern = zvt.pattern      # Sollwerte (sha1, diff) gegen das Z64K-Grundmuster rechnen
    cs = cases(args)
    if args.list:
        for c in cs:
            print(f'{c["name"]:<28} T {c["T"]:<3} {c["cmds"]}')
        return 0
    if args.emu == 'z64k':
        ok, bad, errors, out, n = run_z64k(cs, args)
        for e in errors:
            print(e)
        print(f'{ok}/{n} gleich, {len(bad)} Abweichungen, Z64K-Exit {out.rc}')
        return 0 if not bad and not errors and out.rc == 0 else 1
    args.port = vm.choose_port(args.port)
    res = run(cs, args)
    for e in res['errors']:
        print(e)
    n = res['ok'] + len(res['fail'])
    print(f'{res["ok"]}/{len(cs)} gleich, {len(res["fail"])} Abweichungen, VICE-Exit {res["rc"]}')
    if args.cycles and n == len(cs) and not res['fail']:
        write_cycles(args.cycles, res, 'Kompilat' if args.compile else 'interpretiert', args.fast)
    ok = not res['fail'] and not res['errors'] and n == len(cs) and res['rc'] == 0
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
