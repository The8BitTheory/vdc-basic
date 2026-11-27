#!/usr/bin/env python3
"""Profile a vertrot listing: time between SYS markers 7160..7167 (each an RTS, see the listing).

usage: prof.py [-c] [--frames N] prog.bas

Every marker is a breakpoint; the stopwatch time from one marker to the next is added to the segment
"a->b". Markers 7166/7167 are those of run.py; a profiling copy adds  SYS 7160 ... SYS 7165  where wanted
(and pokes 96 into them).
"""
import argparse, collections, os, re, shutil, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'test'))
sys.path.insert(0, HERE)
import vicemon as vm  # noqa: E402
import run            # noqa: E402

ap = argparse.ArgumentParser()
ap.add_argument('-c', dest='compile', action='store_true')
ap.add_argument('--frames', type=int, default=20)
ap.add_argument('prog')
args = ap.parse_args()
work = tempfile.mkdtemp(prefix='vprof-')
seg = collections.defaultdict(int)
try:
    disk = run.build(work, args.prog, args.compile, vm.blitz_runtime())

    def script(m):
        for a in range(7160, 7168):
            m.bp(a)
        last, frames = None, 0
        while True:
            try:
                pc, _ = m.go()
            except vm.Exited:
                return
            t = int(re.search(r'Stopwatch:\s*(\d+)', m.cmd('stopwatch')).group(1))
            m.cmd('stopwatch reset')
            if pc == 7166:
                frames += 1
                if frames > args.frames:
                    m.s.sendall(b'quit\n')
                    return
            if last is not None and frames >= 1:
                seg[f'{last}->{pc}'] += t
            last = pc

    cmd = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB', '-drive8type', '1581',
           '+drive8truedrive', '-trapdevice8', '-8', disk, '-autostartprgmode', '1',
           '-autostart', os.path.join(work, 'boot.prg'), '-limitcycles', '40000000000']
    vm.run_vice(cmd, vm.choose_port(None), 900, script)
finally:
    shutil.rmtree(work, ignore_errors=True)
tot = sum(seg.values())
for k, v in sorted(seg.items(), key=lambda kv: int(kv[0].split('->')[0]) * 10000 + int(kv[0].split('->')[1])):
    print(f'{k:12s} {v / args.frames / run.CLOCK * 1000:8.1f} ms/frame  {100 * v / tot:5.1f} %')
print(f'{"total":12s} {tot / args.frames / run.CLOCK * 1000:8.1f} ms/frame')
