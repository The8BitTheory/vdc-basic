#!/usr/bin/env python3
"""Build and measure the vertrot port in x128 (headless, warp, VICE remote monitor).

usage: run.py [-c] [--runtime improved|original] [--prog portrend.bas] [--shots DIR] [--frames N]

-c         compile with Blitz! 128 (blitzc, runtime image 2 at $4001) instead of running interpreted
--shots    save every frame as DIR/<prog>-<mode>-NN.png (from VRAM, doubled like the VDC shows it)
--frames   stop after N frames (the camera path in the listing decides otherwise)

Disk (D81, 1581): BOOT (boot/boot.bas, line 50 = RUN"VERTROT"), VDCBASIC3, VERTROT.
The program calls SYS 7166 before and SYS 7167 after drawing a frame (into the hidden page). The script
breaks there: at 7166 the time since the last 7166 (whole frame: drawing, page flip with SYN, keys) and a
stopwatch reset (CPU clock, 985248 Hz PAL); at 7167 the drawing time, the picture (8000 bytes of the
page) and the comparison with ref.py. A BASIC error stops at the error vector ($0300).
Nothing waits for a fixed time; --timeout is the guard.
"""
import argparse, os, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
sys.path.insert(0, os.path.join(ROOT, 'test'))
sys.path.insert(0, HERE)
import vicemon as vm  # noqa: E402
import ref            # noqa: E402

CLOCK = 985248


def build(work, prog, compile_, runtime):
    tok = os.path.join(ROOT, 'test', 'bastok.py')
    boot = os.path.join(work, 'boot.bas')
    with open(os.path.join(ROOT, 'boot', 'boot.bas')) as f, open(boot, 'w') as o:
        o.write(re.sub(r'(?m)^50 .*$', '50 RUN"VERTROT"', f.read()))
    subprocess.run(['python3', tok, boot, os.path.join(work, 'boot.prg')], check=True)
    prg = os.path.join(work, 'vertrot.prg')
    if compile_:
        node, blitzc = vm.find_node(), vm.find_blitzc()
        if not node or not blitzc:
            sys.exit('blitzc or node not found')
        src = os.path.join(work, 'src.prg')
        subprocess.run(['python3', tok, prog, src], check=True)
        out = os.path.join(work, 'blitz')
        r = subprocess.run([node, blitzc, src, out, '--name', 'VERTROT', '--runtime', runtime],
                           capture_output=True, text=True)
        scr = os.path.join(out, 'screen.txt')
        txt = open(scr).read() if os.path.exists(scr) else ''
        if r.returncode or 'errors: 0' not in txt:
            sys.exit(f'blitzc failed ({r.returncode}):\n{r.stdout}{r.stderr}{txt}')
        shutil.copy(os.path.join(out, 'c.prg'), prg)
        if open(prg, 'rb').read(2) != b'\x01\x40':
            sys.exit('compiled program does not load at $4001 (runtime image 2)')
        print(f'compiled: {os.path.getsize(prg)} bytes, runtime {runtime}', flush=True)
    else:
        subprocess.run(['python3', tok, '--start', '0x4001', prog, prg], check=True)
    disk = os.path.join(work, 'vertrot.d81')
    subprocess.run(['c1541', '-format', 'vertrot,01', 'd81', disk,
                    '-write', os.path.join(work, 'boot.prg'), 'boot',
                    '-write', os.path.join(ROOT, 'vdcbasic3.bin'), 'vdcbasic3',
                    '-write', prg, 'vertrot'], check=True, stdout=subprocess.DEVNULL)
    if 'VTCLIP' in open(prog).read():
        subprocess.run(['cl65', '-t', 'none', '-C', os.path.join(HERE, 'vtclip.cfg'), '-o',
                        os.path.join(work, 'vtclip.prg'), os.path.join(HERE, 'vtclip.s')],
                       check=True)
        if os.path.exists(os.path.join(HERE, 'vtclip.o')):
            os.remove(os.path.join(HERE, 'vtclip.o'))
        subprocess.run(['c1541', disk, '-write', os.path.join(work, 'vtclip.prg'), 'vtclip'],
                       check=True, stdout=subprocess.DEVNULL)
    return disk


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('-c', dest='compile', action='store_true')
    ap.add_argument('--runtime', default=vm.blitz_runtime())
    ap.add_argument('--prog', default=os.path.join(HERE, 'vertrot.bas'))
    ap.add_argument('--shots')
    ap.add_argument('--frames', type=int, default=0)
    ap.add_argument('-v', dest='verbose', action='store_true')
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=3600)
    args = ap.parse_args()
    port = vm.choose_port(args.port)
    mode = 'blitz' if args.compile else 'interp'
    name = os.path.splitext(os.path.basename(args.prog))[0]
    expect = [(dp, ref.page_pixels(vd.ram, dp)) for _, dp, vd in ref.frames(args.prog)]
    work = tempfile.mkdtemp(prefix='vertrot-')
    try:
        disk = build(work, args.prog, args.compile, args.runtime)
        if args.shots:
            os.makedirs(args.shots, exist_ok=True)
        vf = os.path.join(work, 'pic.bin')
        flips = []    # (frame, attribute address shown) where the wrong page is on display
        res = []      # (drawing cycles, whole frame cycles or None, differing pixels)

        def script(m):
            ev = m.mem(0x300, 2)
            err_at = ev[0] | ev[1] << 8
            for a in (7166, 7167, err_at):
                m.bp(a)
            started = False
            j0 = None
            while True:
                try:
                    pc, out = m.go()
                except vm.Exited:
                    return
                if pc == err_at:
                    ln = m.mem(0x3b, 2)
                    print(f'BASIC error {m.reg_x()} in line {ln[0] | ln[1] << 8}', flush=True)
                    m.s.sendall(b'quit\n')
                    return
                if pc == 7166:
                    if started:
                        res[-1][1] = int(re.search(r'Stopwatch:\s*(\d+)', m.cmd('stopwatch')).group(1))
                        # the page drawn last must be on display now (R20/R21, set by SYN)
                        regs = re.search(r'\n10:([ 0-9a-f]*)', m.cmd('io d600')).group(1).split()
                        shown = int(regs[4], 16) << 8 | int(regs[5], 16)
                        if shown != expect[len(res) - 1][0]:
                            flips.append((len(res) - 1, shown))
                        if args.frames and len(res) >= args.frames:
                            m.s.sendall(b'quit\n')
                            return
                    started = True
                    m.cmd('stopwatch reset')
                    j0 = m.mem(0xa0, 3)
                    continue
                if pc == 7167:
                    t = int(re.search(r'Stopwatch:\s*(\d+)', m.cmd('stopwatch')).group(1))
                    j1 = m.mem(0xa0, 3)
                    jt = (j1[0] << 16 | j1[1] << 8 | j1[2]) - (j0[0] << 16 | j0[1] << 8 | j0[2])
                    i = len(res)
                    dp, exp = expect[i] if i < len(expect) else (16000, None)
                    m.cmd('bank vdc')
                    if os.path.exists(vf):
                        os.remove(vf)
                    m.cmd(f'bsave "{vf}" 0 {dp:04x} {dp + 7999:04x}')
                    m.cmd('bank ram00')
                    pix = \
                        [[(b >> s) & 15 for b in row for s in (4, 0)]
                         for row in (open(vf, 'rb').read()[y * 80:y * 80 + 80] for y in range(100))]
                    diff = sum(pix[y][x] != exp[y][x] for y in range(100) for x in range(160)) if exp else -1
                    res.append([t, None, diff])
                    if args.verbose:
                        print(f'frame {i:2d}: drawing {t / CLOCK * 1000:7.1f} ms ({t} cycles, {jt} jiffies), {diff} pixels differ', flush=True)
                    if args.shots:
                        ref.save_png(pix, os.path.join(args.shots, f'{name}-{mode}-{i:02d}.png'))
                    continue
                raise RuntimeError(f'unexpected stop at ${pc:04X}')

        cmd = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
               '-drive8type', '1581', '+drive8truedrive', '-trapdevice8', '-8', disk, '-autostartprgmode', '1',
               '-autostart', os.path.join(work, 'boot.prg'), '-limitcycles', '40000000000']
        _, rc = vm.run_vice(cmd, port, args.timeout, script)
        if res:
            ts = [r[0] for r in res]
            fs = [r[1] for r in res if r[1]]
            bad = [i for i, r in enumerate(res) if r[2]]
            print(f'{name} {mode}: {len(res)} frames; drawing mean {sum(ts) / len(ts) / CLOCK * 1000:.1f} ms '
                  f'(min {min(ts) / CLOCK * 1000:.1f}, max {max(ts) / CLOCK * 1000:.1f}); whole frame mean '
                  f'{sum(fs) / len(fs) / CLOCK * 1000:.1f} ms = {len(fs) * CLOCK / sum(fs):.2f} fps')
            print(f'frames differing from ref.py: {len(bad)} {bad[:10]} '
                  f'({sum(res[i][2] for i in bad)} pixels); VICE exit {rc}')
            print(f'frames shown from a wrong address: {len(flips)} {flips[:5]}')
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == '__main__':
    main()
