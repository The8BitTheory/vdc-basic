#!/usr/bin/env python3
"""Pflichttest boot-sector (docs/v3/P2a-relocate.md, 3.3 und 6.5).

Aufruf: test/boot_sector.py [--port N] [--timeout S]

Disk: BOOT (boot/boot.bas, Zeile 50 = RUN"T"), VDCBASIC3, T (test/smoke.bas fuer $4001),
Bootsektor aus tools/mkboot.py. Zwei Laeufe:
  1. ohne -autostart, echte Laufwerksemulation: der KERNAL liest den Bootsektor beim Reset;
  2. -autostart disk.d64: VICE laedt den ersten Directory-Eintrag (BOOT).
Je Lauf: Haltepunkt $1C00 (Installation erreicht) und BASIC-Fehlervektor; Erfolg = Exitcode 0
der Debug-Cartridge aus T. Ereignisgesteuert, das Timeout ist nur ein Waechter.
"""
import argparse, os, re, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import vicemon as vm

ROOT = vm.ROOT


def sh(cmd):
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL)


def make_disk(d):
    boot_bas = os.path.join(d, 'boot.bas')
    src = open(os.path.join(ROOT, 'boot', 'boot.bas')).read()
    open(boot_bas, 'w').write(re.sub(r'^50 .*$', '50 RUN"T"', src, flags=re.M))
    sh(['python3', os.path.join(HERE, 'bastok.py'), boot_bas, os.path.join(d, 'boot.prg')])
    sh(['python3', os.path.join(HERE, 'bastok.py'), '--start', '0x4001',
        os.path.join(HERE, 'smoke.bas'), os.path.join(d, 't.prg')])
    d64 = os.path.join(d, 'boot.d64')
    sh(['c1541', '-format', 'vdc-basic,v3', 'd64', d64, '-write', os.path.join(d, 'boot.prg'), 'boot',
        '-write', os.environ.get('VDCBASIC', os.path.join(ROOT, 'vdcbasic3.bin')), 'vdcbasic3',
        '-write', os.path.join(d, 't.prg'), 't'])
    sh(['python3', os.path.join(ROOT, 'tools', 'mkboot.py'), os.path.join(d, 'bootsec.bin'), '--disk', d64])
    return d64


def run(label, args, port, tmo):
    st = {'install': False, 'errors': []}

    def script(m):
        m.bp(0x1C00)
        # beim Start ohne -autostart haelt der Monitor vor der KERNAL-Initialisierung: ($0300) ist dann
        # noch nicht gesetzt, daher der Standardwert $4D3F (BASIC-Fehlerroutine)
        err_at = 0x4D3F
        m.bp(err_at)
        while True:
            try:
                pc, _ = m.go()
            except vm.Exited:
                return
            if pc == 0x1C00:
                st['install'] = True
            elif pc == err_at:
                lo, hi = m.mem(0x3B, 2)
                st['errors'].append((m.reg_x(), lo | hi << 8))

    _, rc = vm.run_vice(args, port, tmo, script)
    ok = rc == 0 and st['install'] and not st['errors']
    print(f'{"PASS" if ok else "FAIL"}  {label}: $1C00 erreicht {st["install"]}, BASIC-Fehler {st["errors"]}, exit {rc}',
          flush=True)
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=180)
    a = ap.parse_args()
    port = vm.choose_port(a.port)
    with tempfile.TemporaryDirectory() as d:
        d64 = make_disk(d)
        common = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
                  '-limitcycles', '400000000']
        ok = run('Bootsektor', common + ['-drive8truedrive', '+trapdevice8', '-8', d64], port, a.timeout)
        ok &= run('-autostart disk.d64', common + ['+drive8truedrive', '-trapdevice8', '-autostart', d64],
                  port, a.timeout)
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
