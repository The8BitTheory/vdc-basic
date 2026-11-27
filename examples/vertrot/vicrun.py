#!/usr/bin/env python3
"""Build and measure the VIC-II versions of vertrot in x128 (headless, warp, VICE remote monitor).

usage: vicrun.py [-c] [--prog vic.bas|vicasm.s] [--shots DIR] [--frames N] [-v]

BASIC listing: disk (D81) with VBOOT (sets the start of BASIC to $4001 and runs VIC), VIC (tokenised
for $4001, or compiled with Blitz! with -c) and VICSHOW. Machine code program (.s): VICASM alone.
The program calls SYS 7166 (JSR $1BFE) before and SYS 7167 (JSR $1BFF) after drawing a frame. The
script breaks there: at 7166 the time since the last 7166 (whole frame: drawing, page flip, keys),
at 7167 the drawing time; with --shots the frame just drawn as a picture (multicolour 160 x 200 from
the bitmap in memory and the colours of the cells, doubled in width).
Nothing waits for a fixed time; --timeout is the guard.
"""
import argparse, os, re, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
sys.path.insert(0, os.path.join(ROOT, 'test'))
import vicemon as vm  # noqa: E402

CLOCK = 985248
# VIC-II colours (approximately the "pepto" palette)
PAL = [(0, 0, 0), (255, 255, 255), (104, 55, 43), (112, 164, 178), (111, 61, 134), (88, 141, 67),
       (53, 40, 121), (184, 199, 111), (111, 79, 37), (67, 57, 0), (154, 103, 89), (68, 68, 68),
       (108, 108, 108), (154, 210, 132), (108, 94, 181), (149, 149, 149)]
VBOOT = '10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"VBOOT"\n20 RUN"VIC"\n'


def cl65(src, out):
    base = os.path.splitext(src)[0]
    subprocess.run(['cl65', '-t', 'none', '-C', base + '.cfg', '-o', out, src], check=True)
    if os.path.exists(base + '.o'):
        os.remove(base + '.o')


def build(work, prog, compile_, runtime):
    tok = os.path.join(ROOT, 'test', 'bastok.py')
    disk = os.path.join(work, 'vic.d81')
    start = os.path.join(work, 'start.prg')
    if prog.endswith('.s'):
        cl65(prog, start)
        subprocess.run(['c1541', '-format', 'vic,01', 'd81', disk, '-write', start, 'vicasm'],
                       check=True, stdout=subprocess.DEVNULL)
        return disk, start
    open(os.path.join(work, 'vboot.bas'), 'w').write(VBOOT)
    subprocess.run(['python3', tok, os.path.join(work, 'vboot.bas'), start], check=True)
    prg = os.path.join(work, 'vic.prg')
    if compile_:
        node, blitzc = vm.find_node(), vm.find_blitzc()
        src = os.path.join(work, 'src.prg')
        subprocess.run(['python3', tok, prog, src], check=True)
        out = os.path.join(work, 'blitz')
        r = subprocess.run([node, blitzc, src, out, '--name', 'VIC', '--runtime', runtime],
                           capture_output=True, text=True)
        scr = os.path.join(out, 'screen.txt')
        txt = open(scr).read() if os.path.exists(scr) else ''
        if r.returncode or 'errors: 0' not in txt:
            sys.exit(f'blitzc failed ({r.returncode}):\n{r.stdout}{r.stderr}{txt}')
        shutil.copy(os.path.join(out, 'c.prg'), prg)
        if open(prg, 'rb').read(2) != b'\x01\x40':
            sys.exit('compiled program does not load at $4001 (runtime image 2)')
        end = 0x4001 + os.path.getsize(prg) - 2
        print(f'compiled: {os.path.getsize(prg)} bytes, ends at ${end:04X}, runtime {runtime}', flush=True)
        if end >= 0x8C00:
            sys.exit('the compiled program reaches the screen memory of the second page ($8C00)')
    else:
        subprocess.run(['python3', tok, '--start', '0x4001', prog, prg], check=True)
    show = os.path.join(work, 'vicshow.prg')
    cl65(os.path.join(HERE, 'vicshow.s'), show)
    subprocess.run(['c1541', '-format', 'vic,01', 'd81', disk, '-write', start, 'vboot', '-write', prg, 'vic',
                    '-write', show, 'vicshow'], check=True, stdout=subprocess.DEVNULL)
    return disk, start


def render(bitmap, screen, colram, bg, fn):
    from PIL import Image
    img = Image.new('RGB', (160, 200))
    px = img.load()
    for cy in range(25):
        for cx in range(40):
            s, c = screen[cy * 40 + cx], colram[cy * 40 + cx] & 15
            cols = [PAL[bg], PAL[s >> 4], PAL[s & 15], PAL[c]]
            for r in range(8):
                b = bitmap[cy * 320 + cx * 8 + r]
                for k in range(4):
                    px[cx * 4 + k, cy * 8 + r] = cols[(b >> (6 - 2 * k)) & 3]
    img.resize((640, 400), Image.NEAREST).save(fn)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('-c', dest='compile', action='store_true')
    ap.add_argument('--runtime', default=vm.blitz_runtime())
    ap.add_argument('--prog', default=os.path.join(HERE, 'vic.bas'))
    ap.add_argument('--shots')
    ap.add_argument('--frames', type=int, default=0)
    ap.add_argument('-v', dest='verbose', action='store_true')
    ap.add_argument('--port', type=int)
    ap.add_argument('--timeout', type=int, default=3600)
    args = ap.parse_args()
    port = vm.choose_port(args.port)
    asm = args.prog.endswith('.s')
    mode = 'asm' if asm else 'blitz' if args.compile else 'interp'
    name = os.path.splitext(os.path.basename(args.prog))[0]
    work = tempfile.mkdtemp(prefix='vic-')
    try:
        disk, start = build(work, args.prog, args.compile, args.runtime)
        if args.shots:
            os.makedirs(args.shots, exist_ok=True)
        res = []      # [drawing cycles, whole frame cycles]
        bf = os.path.join(work, 'bm.bin')

        def script(m):
            ev = m.mem(0x300, 2)
            err_at = ev[0] | ev[1] << 8
            for a in (7166, 7167) + (() if asm else (err_at,)):
                m.bp(a)
            started = False
            while True:
                try:
                    pc, _ = m.go()
                except vm.Exited:
                    return
                if not asm and pc == err_at:
                    ln = m.mem(0x3b, 2)
                    print(f'BASIC error {m.reg_x()} in line {ln[0] | ln[1] << 8}', flush=True)
                    m.s.sendall(b'quit\n')
                    return
                if pc == 7166:
                    if started:
                        res[-1][1] = int(re.search(r'Stopwatch:\s*(\d+)', m.cmd('stopwatch')).group(1))
                        if args.frames and len(res) >= args.frames:
                            m.s.sendall(b'quit\n')
                            return
                    started = True
                    m.cmd('stopwatch reset')
                    continue
                if pc == 7167:
                    t = int(re.search(r'Stopwatch:\s*(\d+)', m.cmd('stopwatch')).group(1))
                    i = len(res)
                    res.append([t, None])
                    if args.verbose:
                        print(f'frame {i:2d}: drawing {t / CLOCK * 1000:7.1f} ms', flush=True)
                    if args.shots:
                        # the bitmap drawn into: $2000 (BASIC) or, for vicasm, the hidden page in $0FFE/$0FFF
                        m.cmd('bank ram00')
                        base = 0x2000
                        if asm:
                            p = m.mem(0x0FFE, 2)
                            base = p[0] | p[1] << 8
                        if os.path.exists(bf):
                            os.remove(bf)
                        m.cmd(f'bsave "{bf}" 0 {base:04x} {base + 7999:04x}')
                        bitmap = open(bf, 'rb').read()
                        screen = m.mem(0x8C00, 1000)
                        m.cmd('bank io')
                        colram = m.mem(0xD800, 1000)
                        bg = m.mem(0xD021, 1)[0] & 15
                        m.cmd('bank ram00')
                        render(bitmap, screen, colram, bg, os.path.join(args.shots, f'{name}-{mode}-{i:02d}.png'))
                    continue
                raise RuntimeError(f'unexpected stop at ${pc:04X}')

        cmd = ['-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-drive8type', '1581', '+drive8truedrive',
               '-trapdevice8', '-8', disk, '-autostartprgmode', '1', '-autostart', start,
               '-limitcycles', '40000000000']
        _, rc = vm.run_vice(cmd, port, args.timeout, script)
        if res:
            ts = [r[0] for r in res]
            fs = [r[1] for r in res if r[1]]
            print(f'{name} {mode}: {len(res)} frames; drawing mean {sum(ts) / len(ts) / CLOCK * 1000:.1f} ms '
                  f'(min {min(ts) / CLOCK * 1000:.1f}, max {max(ts) / CLOCK * 1000:.1f}); whole frame mean '
                  f'{sum(fs) / len(fs) / CLOCK * 1000:.1f} ms = {len(fs) * CLOCK / sum(fs):.2f} fps; VICE exit {rc}')
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == '__main__':
    main()
