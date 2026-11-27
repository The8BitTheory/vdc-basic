#!/usr/bin/env python3
"""Fixed point model of vtfix.s / vicfix.s: the same frames as the floating point versions, computed
with the integer arithmetic of the machine code, bit for bit. It is the reference for the fixed point
programs (run.py / vicrun.py compare against it) and shows how far fixed point moves the picture.

usage: fixref.py vdc [outdir]     # differences to ref.py, frames as pictures
       fixref.py tab > fixtab.inc   # the angle table of the machine code

Formats (signed 32 bit): geometry (positions, rotation, intersection) 16.16; projected coordinates and
everything the clipping works on 24.8 (the near plane of the original gives coordinates up to ~200000).
One operation does all multiplying and dividing: muldiv(a, b, c) = a*b/c with a 64 bit product,
truncated towards zero; a product is muldiv(a, b, ONE), a quotient muldiv(a, ONE, b).
SIN/COS of the angle k*0.1 (the only angles the path reaches) come from a table, rounded to 16.16.
Pixels: OX + floor(L + 0.5) in 24.8, lines with the Bresenham of VDL (test/ref/vdcdraw.py).
"""
import math, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import ref  # noqa: E402

ONE = 1 << 16          # 1.0 in 16.16
F8 = 1 << 8            # 1.0 in 24.8
KMIN, KMAX = -32, 31   # angle table: k*0.1 for k in KMIN..KMAX


def wrap(v):
    v &= 0xFFFFFFFF
    return v - (1 << 32) if v & 0x80000000 else v


def muldiv(a, b, c):
    neg = (a < 0) ^ (b < 0) ^ (c < 0)
    q = (abs(a) * abs(b)) // abs(c)
    return wrap(-q if neg else q)


def fmul(a, b):
    return muldiv(a, b, ONE)


def fdiv(a, b):
    return muldiv(a, ONE, b)


def fx(v):
    """A constant in 16.16, rounded."""
    return int(round(v * ONE))


SIN = {k: fx(math.sin(k * 0.1)) for k in range(KMIN, KMAX + 1)}
COS = {k: fx(math.cos(k * 0.1)) for k in range(KMIN, KMAX + 1)}
K0001 = fx(0.0001)     # 7


def asr(v, n):
    return v >> n      # Python >> on negative numbers = arithmetic shift (floor)


class Geo:
    """View layout: vdc = 160 x 100 (y halved), vic = 160 x 200 (y as in the original)."""
    def __init__(self, chip):
        self.vic = chip == 'vic'
        self.oy = 40 if self.vic else 20
        self.vh = 109 if self.vic else 54

    def y(self, v8):
        """A y value in 24.8 as the listing scales it: halved on the VDC."""
        return v8 if self.vic else asr(v8, 1)


def clip_draw(lines, la, lb, lc, ld, ox, oy, vw, vh, col):
    """Cohen-Sutherland in 24.8 (vw, vh whole numbers), then OX+floor(L+.5); appends the pixel line."""
    W, H = vw * F8, vh * F8

    def code(u, v):
        c = 1 if u < 0 else 2 if u > W else 0
        return c + (4 if v < 0 else 8 if v > H else 0)
    ca, cb = code(la, lb), code(lc, ld)
    while True:
        if ca | cb == 0:
            lines.append((ox + asr(la + 128, 8), oy + asr(lb + 128, 8), ox + asr(lc + 128, 8),
                          oy + asr(ld + 128, 8), col))
            return
        if ca & cb:
            return
        c = ca or cb
        if c & 12:
            k = H if c & 8 else 0
            u, v = la + muldiv(lc - la, k - lb, ld - lb), k
        else:
            k = W if c & 2 else 0
            u, v = k, lb + muldiv(ld - lb, k - la, lc - la)
        if c == ca:
            la, lb = u, v
            ca = code(la, lb)
        else:
            lc, ld = u, v
            cb = code(lc, ld)


def intersect(e1, f1, e2, f2, e3, f3, e4, f4):
    ix = fmul(e1, f2) - fmul(f1, e2)
    iy = fmul(e3, f4) - fmul(f3, e4)
    dt = fmul(e1 - e2, f3 - f4) - fmul(f1 - f2, e3 - e4)
    ix = fdiv(fmul(ix, e3 - e4) - fmul(e1 - e2, iy), dt)
    iy = fdiv(fmul(ix, f3 - f4) - fmul(f1 - f2, iy), dt)      # with the new ix, as in the original
    return ix, iy


def frames(chip):
    """Yields (frame, lines): the lines of each frame after the template (walls/borders/player there)."""
    g = Geo(chip)
    path = ref.path(os.path.join(HERE, 'vertrot.bas'))
    px, py, k = 50 * ONE, 50 * ONE, 0
    c1 = (1, 15, 13, 13, 12) if not g.vic else (2, 3, 1, 1, 2)   # colours: dir, point, wall2, v3 top/bottom, sides
    for fr in range(len(path) + 1):
        sn, cs = SIN[k], COS[k]
        lines = []
        # view 1: OX=2, LA=PX/2, LB=PY/2 (VIC: PY), LC=(CS*5+PX)/2, LD=(SN*5+PY)/2 (VIC: SN*5+PY)
        to8 = lambda v: asr(v, 8)          # 16.16 -> 24.8
        la, lb = to8(asr(px, 1)), g.y(to8(py))
        lc = to8(asr(cs * 5 + px, 1))
        ld = g.y(to8(sn * 5 + py))
        clip_draw(lines, la, lb, lc, ld, 2, g.oy, 49, g.vh, c1[0])
        clip_draw(lines, la, lb, la, lb, 2, g.oy, 49, g.vh, c1[1])
        # view 2
        t1, u1, t2, u2 = 70 * ONE - px, 20 * ONE - py, 70 * ONE - px, 70 * ONE - py
        z1 = fmul(t1, cs) + fmul(u1, sn)
        z2 = fmul(t2, cs) + fmul(u2, sn)
        t1, t2 = fmul(t1, sn) - fmul(u1, cs), fmul(t2, sn) - fmul(u2, cs)
        la, lb = to8(asr(50 * ONE - t1, 1)), g.y(to8(50 * ONE - z1))
        lc, ld = to8(asr(50 * ONE - t2, 1)), g.y(to8(50 * ONE - z2))
        n0 = len(lines)
        clip_draw(lines, la, lb, lc, ld, 54, g.oy, 49, g.vh, c1[2])
        lines.append(('player?', la, lb, lc, ld))   # the template player again if the wall may cover it
        # view 3
        if z1 > 0 or z2 > 0:
            if z1 <= 0 or z2 <= 0:
                i1, j1 = intersect(t1, z1, t2, z2, -K0001, K0001, -20 * ONE, 5 * ONE)
                i2, j2 = intersect(t1, z1, t2, z2, K0001, K0001, 20 * ONE, 5 * ONE)
                if z1 <= 0:
                    t1, z1 = (i1, j1) if j1 > 0 else (i2, j2)
                if z2 <= 0:
                    t2, z2 = (i1, j1) if j1 > 0 else (i2, j2)
            x1, ya, yb = muldiv(-(t1 << 4), F8, z1), muldiv(-50 * ONE, F8, z1), muldiv(50 * ONE, F8, z1)
            x2, yc, yd = muldiv(-(t2 << 4), F8, z2), muldiv(-50 * ONE, F8, z2), muldiv(50 * ONE, F8, z2)
            h = lambda v: asr(50 * F8 + v, 1)
            hy = lambda v: g.y(50 * F8 + v)
            for (a, b, c, d, col) in ((x1, ya, x2, yc, c1[3]), (x1, yb, x2, yd, c1[3]),
                                      (x1, ya, x1, yb, c1[4]), (x2, yc, x2, yd, c1[4])):
                clip_draw(lines, h(a), hy(b), h(c), hy(d), 107, g.oy, 50, g.vh, col)
        yield fr, lines
        if fr == len(path):
            return
        st = path[fr]
        if st == 'U':
            px, py = px + cs, py + sn
        elif st == 'D':
            px, py = px - cs, py - sn
        elif st == 'L':
            k -= 1
        elif st == 'R':
            k += 1
        elif st == 'A':
            px, py = px + sn, py - cs
        elif st == 'E':
            px, py = px - sn, py + cs


def covered(la, lb, lc, ld, vic):
    """492 in 24.8 (view coordinates): False if the wall line lies away from the player."""
    y0, y1 = (44, 51) if vic else (22, 26)
    return not (la >= 26 * F8 and lc >= 26 * F8 or la <= 24 * F8 and lc <= 24 * F8 or
                lb >= y1 * F8 and ld >= y1 * F8 or lb <= y0 * F8 and ld <= y0 * F8)


def vdc_pages():
    """VDC: yields (frame, page address, vdc) like ref.frames, drawn from the fixed point lines."""
    sys.path.insert(0, os.path.join(HERE, '..', '..', 'test', 'ref'))
    import vdcdraw
    vd = vdcdraw.Vdc()
    dp = 24000
    for fr, lines in frames('vdc'):
        vd.vdm(1, dp, 80)
        vd.vdb(2, 20, 50, 55, 0); vd.vdr(1, 19, 52, 57, 2)
        vd.vdl(37, 30, 37, 55, 13)
        vd.vdb(54, 20, 50, 55, 0); vd.vdr(53, 19, 52, 57, 4)
        vd.vdl(79, 43, 79, 45, 1); vd.vdl(79, 45, 79, 45, 15)
        vd.vdb(107, 20, 51, 55, 0); vd.vdr(106, 19, 53, 57, 6)
        for ln in lines:
            if ln[0] == 'player?':
                if covered(*ln[1:], False):
                    vd.vdl(79, 43, 79, 45, 1); vd.vdl(79, 45, 79, 45, 15)
                continue
            vd.vdl(*ln)
        yield fr, dp, vd
        dp = 40000 - dp


def table():
    """fixtab.inc for vtfix.s / vicfix.s: SIN and COS of k*0.1, k = KMIN..KMAX, 16.16."""
    out = ['; generated by fixref.py tab: SIN and COS of k*0.1 for k = %d..%d in 16.16 (index k-(%d))' % (KMIN, KMAX, KMIN)]
    for name, t in (('sintab', SIN), ('costab', COS)):
        vals = [t[k] & 0xFFFFFFFF for k in range(KMIN, KMAX + 1)]
        out.append(name + ':')
        for i in range(0, len(vals), 4):
            out.append('        .dword  ' + ', '.join('$%08X' % v for v in vals[i:i + 4]))
    return '\n'.join(out) + '\n'


if __name__ == '__main__':
    chip = sys.argv[1] if len(sys.argv) > 1 else 'vdc'
    if chip == 'tab':
        sys.stdout.write(table())
        sys.exit(0)
    out = sys.argv[2] if len(sys.argv) > 2 else None
    if chip == 'vdc':
        exp = {fr: ref.page_pixels(vd.ram, dp) for fr, dp, vd in ref.frames(os.path.join(HERE, 'vertrot.bas'))}
        tot, bad = 0, []
        for fr, dp, vd in vdc_pages():
            pix = ref.page_pixels(vd.ram, dp)
            d = sum(pix[y][x] != exp[fr][y][x] for y in range(100) for x in range(160))
            tot += d
            if d:
                bad.append((fr, d))
            if out:
                os.makedirs(out, exist_ok=True)
                ref.save_png(pix, os.path.join(out, f'fix-{fr:02d}.png'))
        print(f'fixed point vs floating point (ref.py): {len(bad)} of 91 frames differ, {tot} pixels; {bad[:12]}')
