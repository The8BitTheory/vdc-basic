#!/usr/bin/env python3
"""Experiment E1 (docs/v3/P2a-relocate.md, 5.2): Blitz!-Runtime Image 2 mit VDC-BASIC bei $1C00.

Aufruf: test/e1_blitz.py [--bin vdcbasic3.bin --sym build/vdcbasic3.sym] [--port N] [--timeout S]

Ohne --bin wird src/vdcbasic.asm nach build/e1/ gebaut (ein v2h-Stand mit "* = $1300" wird dabei
nach $1C00 verlegt, so lief E1 urspruenglich). Jedes Testprogramm laeuft
interpretiert und als Kompilat (Image 2) ueber den Boot-Lader. Nach der Installation (Haltepunkt
auf dem RTS am Ende von install) wird ein Watchpoint 'store $1300-$3FFF' gesetzt, der nur bei PC
ausserhalb des VDC-BASIC-Codes ausloest (E1a). Ergebnis: Exitcode der Debug-Cartridge je Lauf. Exitcode des Skripts 0, wenn alles wie in
Abschnitt 9 der Spec ausfaellt (inkl. Treffer bei der Gegenprobe und beim Image-1-Kompilat).
Ereignisgesteuert, keine festen Wartezeiten; das Timeout ist nur ein Waechter.
"""
import argparse, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import vicemon as vm

ROOT = vm.ROOT
OUT = os.path.join(ROOT, 'build', 'e1')

# Name -> (Listing, erwarteter Exitcode). 63999 GRAPHIC 0 erzwingt Image 2 (E1e).
PROGS = {
    'e1b-assign': ('''10 a=1
20 ::a=7
30 if a<>7 then 8000
40 bank 15:poke dec("d7ff"),0:end
8000 bank 15:poke dec("d7ff"),10+a:end
63999 graphic 0
''', 0),
    'e1b-vmd': ('''10 a=1
20 ::vmw 2000,42
30 ::a=vmd(2000)+5
40 if a<>47 then 8000
50 bank 15:poke dec("d7ff"),0:end
8000 bank 15:poke dec("d7ff"),100+(a and 127):end
63999 graphic 0
''', 0),
    'e1c-func': ('''10 ::vmw 2000,42
20 a=vmd(2000)
30 if a<>42 then 8000
40 bank 15:poke dec("d7ff"),0:end
8000 bank 15:poke dec("d7ff"),1:end
63999 graphic 0
''', None),
    'e1d-trap': ('''10 trap 9000
20 ::vmw 70000,1
30 bank 15:poke dec("d7ff"),1:end
9000 if er<>14 then bank 15:poke dec("d7ff"),20+(er and 127):end
9010 bank 15:poke dec("d7ff"),0:end
63999 graphic 0
''', 0),
    # Gegenprobe zu E1a: POKE aus BASIC bzw. Runtime (PC im ROM/in der Runtime) muss einen Treffer liefern
    'e1a-control': ('''10 bank 0:poke dec("1300"),1
20 bank 15:poke dec("d7ff"),0:end
63999 graphic 0
''', 0),
    'e1e-nographic': ('''10 a=1
20 ::a=7
30 if a<>7 then 8000
40 bank 15:poke dec("d7ff"),0:end
8000 bank 15:poke dec("d7ff"),10+a:end
''', None),
}

BOOT = '''10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:END
30 SYS 7168
50 RUN"T"
'''


def sh(cmd, **kw):
    r = subprocess.run(cmd, **kw)
    if r.returncode:
        sys.exit(f'Fehler ({r.returncode}): {" ".join(cmd)}')
    return r


def build_e1():
    os.makedirs(OUT, exist_ok=True)
    src = open(os.path.join(ROOT, 'src', 'vdcbasic.asm')).read()
    src = re.sub(r'^(\s*)\* = \$1300', r'\1* = $1C00', src, flags=re.M)
    src = re.sub(r'!to "[^"]*", cbm', '!to "build/e1/vdcbasic-e1.bin", cbm', src)
    asm = os.path.join(OUT, 'vdcbasic-e1.asm')
    open(asm, 'w').write(src)
    env = dict(os.environ, ACME=os.environ.get('ACME', os.path.expanduser('~/.local/share/acme/ACME_Lib')))
    sh(['acme', '-l', os.path.join(OUT, 'vdcbasic-e1.sym'), asm], cwd=ROOT, env=env, stdout=subprocess.DEVNULL)
    return os.path.join(OUT, 'vdcbasic-e1.bin'), os.path.join(OUT, 'vdcbasic-e1.sym')


def tok(bas, prg, start=None):
    sh(['python3', os.path.join(HERE, 'bastok.py')] + (['--start', start] if start else []) + [bas, prg])


def run_one(name, prog, bin_, sym, port, tmo):
    """Liefert dict mit Exitcode, E1a-Treffern und BASIC-Fehlern."""
    s = vm.symbols(sym)
    install_end = s['vectors'] - 1          # RTS am Ende von install
    code_end = os.path.getsize(bin_) - 2 + 0x1C00 - 1
    d64 = os.path.join(OUT, name + '.d64')
    if os.path.exists(d64):
        os.remove(d64)
    boot_bas = os.path.join(OUT, 'boot.bas')
    open(boot_bas, 'w').write(BOOT)
    boot_prg = os.path.join(OUT, 'boot.prg')
    tok(boot_bas, boot_prg)
    sh(['c1541', '-format', 'e1,01', 'd64', d64, '-write', boot_prg, 'boot', '-write', bin_, 'vdcbasic3',
        '-write', prog, 't'], stdout=subprocess.DEVNULL)
    args = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
            '+drive8truedrive', '-trapdevice8', '-8', d64, '-autostartprgmode', '1', '-autostart', boot_prg,
            '-limitcycles', '300000000']
    info = {'hits': [], 'errors': [], 'installed': False}

    def script(m):
        m.bp(install_end)
        ev = m.mem(0x300, 2)
        err_at = ev[0] | ev[1] << 8
        m.bp(err_at)
        while True:
            try:
                pc, out = m.go()
            except vm.Exited:
                return
            if pc == install_end and not info['installed']:
                info['installed'] = True
                # VICE 3.10: ohne Klammern wertet die Bedingung '||' nicht wie erwartet aus
                t = m.cmd(f'watch store 1300 3fff if (pc < $1c00) || (pc > ${code_end + 3:04x})')
                if 'WATCH' not in t:
                    raise RuntimeError('watch nicht gesetzt: ' + t)
            elif pc == err_at:
                x = m.reg_x()
                lo, hi = m.mem(0x3B, 2)
                info['errors'].append((x, lo | hi << 8))
            else:
                # Watchpoint: Zeile 'WATCH: n  C:$xxxx ... ' bzw. Prompt mit PC
                a_ = re.search(r'Stop on store ([0-9a-fA-F]{4})', out)
                i_ = re.search(r'\.C:([0-9a-fA-F]{4})', out)
                info['hits'].append(f'${a_.group(1) if a_ else "?"} von PC ${i_.group(1) if i_ else format(pc, "04x")}')
                if len(info['hits']) > 50:
                    m.cmd('delete')
                    m.bp(err_at)

    _, rc = vm.run_vice(args, port, tmo, script)
    info['rc'] = rc
    return info


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--bin')
    ap.add_argument('--sym')
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=120)
    ap.add_argument('--only')
    a = ap.parse_args()
    bin_, sym = (a.bin, a.sym) if a.bin else build_e1()
    port = vm.choose_port(a.port)
    node, blitzc = vm.find_node(), vm.find_blitzc()
    if not node or not blitzc:
        print('SKIP (blitzc not found)')
        sys.exit(3)
    os.makedirs(OUT, exist_ok=True)
    bad = 0
    for name, (text, want) in PROGS.items():
        if a.only and a.only not in name:
            continue
        bas = os.path.join(OUT, name + '.bas')
        open(bas, 'w').write(text)
        # interpretiert
        prg = os.path.join(OUT, name + '-i.prg')
        tok(bas, prg, '0x4001')
        r = run_one(name + '-i', prg, bin_, sym, port, a.timeout)
        print(f'{name:16s} interpretiert: exit {r["rc"]}  E1a-Treffer {r["hits"]}  BASIC-Fehler {r["errors"]}', flush=True)
        # Treffer erwartet: Gegenprobe immer; ohne GRAPHIC (Image 1) beim Kompilat
        ctl = 'control' in name
        if want is not None and r['rc'] != want or bool(r['hits']) != ctl:
            bad = 1
        # kompiliert
        src = os.path.join(OUT, name + '-src.prg')
        tok(bas, src)
        cdir = os.path.join(OUT, name + '-blitz')
        shutil.rmtree(cdir, ignore_errors=True)
        c = subprocess.run([node, blitzc, src, cdir, '--name', 'T', '--runtime', vm.blitz_runtime()], capture_output=True, text=True)
        scr = open(os.path.join(cdir, 'screen.txt')).read() if os.path.exists(os.path.join(cdir, 'screen.txt')) else ''
        msg = ' | '.join(l.strip() for l in scr.splitlines() if l.strip() and re.search(r'error|\?|extension', l, re.I))
        cprg = os.path.join(cdir, 'c.prg')
        if not os.path.exists(cprg) or c.returncode:
            print(f'{name:16s} kompiliert:    Compiler rc {c.returncode}: {c.stdout.strip()}  Bildschirm: {msg}', flush=True)
            if want is not None:
                bad = 1
            continue
        la = open(cprg, 'rb').read(2)
        la = la[0] | la[1] << 8
        r = run_one(name + '-c', cprg, bin_, sym, port, a.timeout)
        print(f'{name:16s} kompiliert:    Ladeadresse ${la:04x}  exit {r["rc"]}  E1a-Treffer {r["hits"]}  '
              f'BASIC-Fehler {r["errors"]}  ({c.stdout.strip()}; {msg})', flush=True)
        if want is not None and (r['rc'] != want or la != 0x4001) or bool(r['hits']) != (ctl or 'nographic' in name):
            bad = 1
    sys.exit(bad)


if __name__ == '__main__':
    main()
