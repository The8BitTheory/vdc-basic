#!/usr/bin/env python3
"""Reference for the vertrot port: the same frames as vertrot.bas, drawn with the reference model of
VDC-BASIC's drawing commands (test/ref/vdcdraw.py). Mirrors the listing statement by statement
(clipping, rounding, page flip), so the picture of each frame can be compared byte by byte.

usage: ref.py vertrot.bas [outdir]      # outdir/ref-NN.png, doubled in height like the VDC shows it
"""
import math, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'test', 'ref'))
import vdcdraw  # noqa: E402

PALETTE = [(0, 0, 0), (85, 85, 85), (0, 0, 170), (85, 85, 255), (0, 170, 0), (85, 255, 85),
           (0, 170, 170), (85, 255, 255), (170, 0, 0), (255, 85, 85), (170, 0, 170), (255, 85, 255),
           (170, 85, 0), (255, 255, 85), (170, 170, 170), (255, 255, 255)]


def path(bas):
    for line in open(bas, encoding='latin-1'):
        m = re.search(r'DATA\s*"([A-Z]+)"', line)
        if m:
            return m.group(1)


def rnd(v):
    return math.floor(v + 0.5)


class View:
    def __init__(self, vdc, ox, oy, vw, vh):
        self.v, self.ox, self.oy, self.vw, self.vh = vdc, ox, oy, vw, vh

    def code(self, u, v):
        c = 1 if u < 0 else 2 if u > self.vw else 0
        return c + (4 if v < 0 else 8 if v > self.vh else 0)

    def line(self, la, lb, lc, ld, k):
        ca, cb = self.code(la, lb), self.code(lc, ld)
        while True:
            if ca | cb == 0:
                self.v.vdl(self.ox + rnd(la), self.oy + rnd(lb), self.ox + rnd(lc), self.oy + rnd(ld), k)
                return
            if ca & cb:
                return
            c = ca or cb
            if c & 8:
                u, v = la + (lc - la) * (self.vh - lb) / (ld - lb), self.vh
            elif c & 4:
                u, v = la + (lc - la) * (0 - lb) / (ld - lb), 0
            elif c & 2:
                u, v = self.vw, lb + (ld - lb) * (self.vw - la) / (lc - la)
            else:
                u, v = 0, lb + (ld - lb) * (0 - la) / (lc - la)
            if c == ca:
                la, lb = u, v; ca = self.code(u, v)
            else:
                lc, ld = u, v; cb = self.code(u, v)


def intersect(x1, y1, x2, y2, x3, y3, x4, y4):
    x = x1 * y2 - y1 * x2
    y = x3 * y4 - y3 * x4
    det = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    x = (x * (x3 - x4) - (x1 - x2) * y) / det
    y = (x * (y3 - y4) - (y1 - y2) * y) / det   # uses the new x, as in the original
    return x, y


def frames(bas):
    """Yields (frame, page address, vdc) after each frame is drawn."""
    vd = vdcdraw.Vdc()
    a1, b1, a2, b2 = 70, 20, 70, 70
    px, py, ag = 50.0, 50.0, 0.0
    dp = 24000
    w = path(bas)
    fr = 0
    while True:
        vd.vdm(1, dp, 80)
        vd.vdb(2, 20, 50, 55, 0); vd.vdr(1, 19, 52, 57, 2)
        m = View(vd, 2, 20, 49, 54)
        m.line(a1 / 2, b1 / 2, a2 / 2, b2 / 2, 13)
        m.line(px / 2, py / 2, (math.cos(ag) * 5 + px) / 2, (math.sin(ag) * 5 + py) / 2, 1)
        m.line(px / 2, py / 2, px / 2, py / 2, 15)
        vd.vdb(54, 20, 50, 55, 0); vd.vdr(53, 19, 52, 57, 4)
        m = View(vd, 54, 20, 49, 54)
        t1, u1, t2, u2 = a1 - px, b1 - py, a2 - px, b2 - py
        s, c = math.sin(ag), math.cos(ag)
        z1, z2 = t1 * c + u1 * s, t2 * c + u2 * s
        t1, t2 = t1 * s - u1 * c, t2 * s - u2 * c
        m.line((50 - t1) / 2, (50 - z1) / 2, (50 - t2) / 2, (50 - z2) / 2, 13)
        m.line(25, 25, 25, 22.5, 1)
        m.line(25, 25, 25, 25, 15)
        vd.vdb(107, 20, 51, 55, 0); vd.vdr(106, 19, 53, 57, 6)
        m = View(vd, 107, 20, 50, 54)
        if z1 > 0 or z2 > 0:
            i1, j1 = intersect(t1, z1, t2, z2, -.0001, .0001, -20, 5)
            i2, j2 = intersect(t1, z1, t2, z2, .0001, .0001, 20, 5)
            if z1 <= 0:
                t1, z1 = (i1, j1) if j1 > 0 else (i2, j2)
            if z2 <= 0:
                t2, z2 = (i1, j1) if j1 > 0 else (i2, j2)
            x1, ya, yb = -t1 * 16 / z1, -50 / z1, 50 / z1
            x2, yc, yd = -t2 * 16 / z2, -50 / z2, 50 / z2
            m.line((50 + x1) / 2, (50 + ya) / 2, (50 + x2) / 2, (50 + yc) / 2, 13)
            m.line((50 + x1) / 2, (50 + yb) / 2, (50 + x2) / 2, (50 + yd) / 2, 13)
            m.line((50 + x1) / 2, (50 + ya) / 2, (50 + x1) / 2, (50 + yb) / 2, 12)
            m.line((50 + x2) / 2, (50 + yc) / 2, (50 + x2) / 2, (50 + yd) / 2, 12)
        yield fr, dp, vd
        dp = 40000 - dp
        fr += 1
        if fr > len(w):
            return
        k = w[fr - 1]
        if k == 'U': px += math.cos(ag); py += math.sin(ag)
        elif k == 'D': px -= math.cos(ag); py -= math.sin(ag)
        elif k == 'L': ag -= .1
        elif k == 'R': ag += .1
        elif k == 'A': px += math.sin(ag); py -= math.cos(ag)
        elif k == 'E': px -= math.sin(ag); py += math.cos(ag)


def page_pixels(ram, dp):
    return [[(ram[dp + y * 80 + x // 2] >> (0 if x & 1 else 4)) & 15 for x in range(160)] for y in range(100)]


def save_png(pix, fn):
    from PIL import Image
    img = Image.new('RGB', (160, 100))
    img.putdata([PALETTE[c] for row in pix for c in row])
    img.resize((640, 400), Image.NEAREST).save(fn)


if __name__ == '__main__':
    out = sys.argv[2] if len(sys.argv) > 2 else None
    if out:
        os.makedirs(out, exist_ok=True)
    n = 0
    for fr, dp, vd in frames(sys.argv[1]):
        n += 1
        if out:
            save_png(page_pixels(vd.ram, dp), os.path.join(out, f'ref-{fr:02d}.png'))
    print(f'{n} frames')
