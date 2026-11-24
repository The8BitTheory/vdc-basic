#!/usr/bin/env python3
"""LOAD ,8 / LOAD ,8,1 matrix for Blitz!128 programs, measured in VICE x128.

For each case a fresh x128 boots with t.d64 in drive 8. After every step (initial state, LOAD, RUN) the key buffer
types POKE 2816,n. A watchpoint on $0B00 stops the machine at that point (no fixed waits). The remote monitor then
reads the BASIC start ($2D/$2E), the end of the program text ($1210/$1211), the load end ($AE/$AF) and the first bytes
at $1C01 and $4001 (bank ram), plus the 40-column screen.

usage:  ./run.py              all eight cases
        ./run.py b_81 g_a_81  only the cases named
Needs x128 (VICE) in PATH. A case that hangs ends when the cycle limit (the only timeout) is reached.
"""
import os, re, socket, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
GRAPHIC = 'graphic1:graphic0'            # reserves the hi-res area: BASIC start moves to $4001
CASES = [('a_8', '', 'load"a",8'), ('a_81', '', 'load"a",8,1'),
         ('b_8', '', 'load"b",8'), ('b_81', '', 'load"b",8,1'),
         ('g_a_8', GRAPHIC, 'load"a",8'), ('g_a_81', GRAPHIC, 'load"a",8,1'),
         ('g_b_8', GRAPHIC, 'load"b",8'), ('g_b_81', GRAPHIC, 'load"b",8,1')]
CYCLE_LIMIT = '200000000'                # guard against hangs only
PROMPT = re.compile(rb'\(C:\$[0-9a-f]{4}\) $')
MEM = re.compile(r'>C:([0-9a-f]{4})((?:  ?[0-9a-f]{2})+)')


class Hang(Exception):
    pass


def screencode(c):
    c &= 0x7f
    if c == 0: return '@'
    if 1 <= c <= 26: return chr(c + 64)
    return chr(c) if 32 <= c < 64 else '.'


def run_case(name, pre, load, port):
    mon_file = os.path.join(HERE, f'.{name}.mon')
    with open(mon_file, 'w') as f:
        f.write('watch store 0b00\n')
    keybuf = (pre + '\n' if pre else '') + 'poke2816,1\n'
    vice = subprocess.Popen(['x128', '-default', '-warp', '-sounddev', 'dummy', '-8', os.path.join(HERE, 't.d64'),
                             '-moncommands', mon_file, '-remotemonitor',
                             '-remotemonitoraddress', f'ip4://127.0.0.1:{port}',
                             '-keybuf', keybuf, '-limitcycles', CYCLE_LIMIT],
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        for _ in range(200):             # only waits for VICE to open the port
            try:
                sock = socket.create_connection(('127.0.0.1', port))
                break
            except OSError:
                time.sleep(0.05)
        else:
            raise RuntimeError('remote monitor not reachable')
        buf = b''

        def until_prompt():
            nonlocal buf
            while not PROMPT.search(buf):
                data = sock.recv(65536)
                if not data:             # VICE quit: cycle limit reached
                    raise Hang()
                buf += data
            out, buf = buf.decode('latin1'), b''
            return out

        def mon(cmd):
            sock.sendall((cmd + '\n').encode())
            return until_prompt()

        def wait_sentinel(v):
            until_prompt()
            for _ in range(50):
                mon('bank ram')
                if re.search(r'>C:0b00\s+%02x' % v, mon('m 0b00 0b00')):
                    return
                sock.sendall(b'x\n')
                until_prompt()
            raise Hang()

        res = {}

        def snapshot(tag):
            mon('bank ram')
            out = ''.join(mon(f'm {a}') for a in ('002d 002e', '1210 1211', '00ae 00af', '1c01 1c08', '4001 4008'))
            res[tag] = ' | '.join(m.group(1) + m.group(2) for m in MEM.finditer(out))

        def screen():
            mon('bank ram')
            out = mon('m 0400 07e7')
            data = bytes(int(x, 16) for m in MEM.finditer(out) for x in m.group(2).split())
            lines = (''.join(screencode(c) for c in data[i:i + 40]).rstrip() for i in range(0, len(data), 40))
            return [l for l in lines if l]

        steps = [('before', None, 1), ('after_load', load, 2), ('after_run', 'run', 3)]
        try:
            for tag, cmd, v in steps:
                if cmd:
                    mon(f'keybuf {cmd}\\npoke2816,{v}\\n')   # keybuf takes the text as is, no quotes
                    sock.sendall(b'x\n')
                wait_sentinel(v)
                snapshot(tag)
            scr = screen()
        except Hang:
            res[tag] = 'HANG (no READY until the cycle limit)'
            scr = []
        try:
            mon('quit')
        except Hang:
            pass
        return res, scr
    finally:
        vice.kill()
        os.remove(mon_file)


def main():
    only = sys.argv[1:]
    print('columns: $2D/$2E BASIC start | $1210 end of text | $AE load end | $1C01.. | $4001..')
    for i, (name, pre, load) in enumerate(CASES):
        if only and name not in only:
            continue
        res, scr = run_case(name, pre, load, 6600 + i)
        print(f'\n== {name}: {pre + ":" if pre else ""}{load}')
        for tag, val in res.items():
            print(f'  {tag:11} {val}')
        for line in scr[4:]:             # skip the boot message
            print('  |' + line)


if __name__ == '__main__':
    main()
