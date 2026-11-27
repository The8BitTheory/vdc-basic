#!/usr/bin/env python3
"""Animation benchmark: what does one animation step cost on the VDC, by source and method.

Background: dungeonboringengine/docs/vma-animation-notes.md. Spec: test/bench/anim/README.md.

Measures, on a C128 with 64 KB VDC (and in VICE/Z64K for checking), in the chunky mode of Crawl
(2 MHz, R36 = 0, VIC blanked, interrupts off, CIA 2 timer in microseconds):

  opaque patches   portrait parts, torch, gate (distance 1 and 2), two-wing gate, whole viewport
                   vmc  : source packed in VRAM, VCL type 2/3 (block copy, VDC-BASIC v3)
                   cpu  : source raw in RAM, CPU loop per row (bit $d600 / sta $d601), best case of a
                          possible "RTV rectangle" entry
                   rxv14/rxv2 : source as VRL stream in RAM, row ends as skips, VCL type 8 with T = 14/2
  transparent      grille gate, goblin: vms (VRAM data + run list, VCL type 12), cpu (one row entry per
                   opaque run), rxv14/rxv2 (skips for transparent bytes)
  layered (R2)     background layer + goblin behind + gate or grille layer, one VCL list
  preload          assets RAM -> VRAM in one piece: rtv (VDC-BASIC RTV), cpu, rxv14/rxv2

Every case runs K times at different positions in the frame; afterwards a Fletcher-16 over the target
rectangle is compared with a Python model of the VRAM.

  anim.py build [--out DIR]               program, data, loader, disk (anim.d81)
  anim.py vice  [--out DIR]               build, run in x128 (warp), evaluate (results are provisional)
  anim.py z64k  [--out DIR]               the same in Z64K
  anim.py eval  ANIMRES.prg [--source c128] [--out DIR]   evaluate a result file
"""
import argparse, datetime, json, os, random, shutil, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, '..', '..', '..'))
sys.path.insert(0, os.path.join(REPO, 'tools'))
sys.path.insert(0, os.path.join(REPO, 'test'))
import vrl  # noqa: E402

VERSION = 1
K = 12                          # samples per case
STRIDE = 80
SCREEN, BGBUF = 0x0000, 0x2600  # 80 x 120 bytes each (chunky attributes, as Crawl)
POOL, POOL_END = 0x4C00, 0x7000
SCRATCH, SCRATCH_END = 0x7000, 0xB400
TRAMP = 0x1300
RES, RES_END = 0x1400, 0x1C00
CODE, TABLES, CODE_END = 0x4800, 0x5400, 0x6000
D1, D1_END = 0x6000, 0xD000
D2, D2_END = 0xE000, 0xFF00
T_DEFAULT = 14
MODE = [(0, 127), (4, 159), (6, 120), (7, 133), (9, 1), (12, 0xB4), (13, 0xFF), (20, 0), (21, 0), (26, 0xFF), (36, 0)]
SAVE_REGS = [r for r, _ in MODE] + [24, 25, 28]
GATE = (25, 12)                 # byte column, row of the gate rectangle (distance 1) on screen
SEED = 1979

KIND_NONE, KIND_VCL, KIND_RECTS, KIND_RTV = 0, 1, 2, 3


# ------------------------------------------------------------------ content (palette indices 0-15)
def pack(px_rows):
    out = []
    for row in px_rows:
        assert len(row) % 2 == 0
        out += [(row[i] << 4) | row[i + 1] for i in range(0, len(row), 2)]
    return out


def bricks(w, h, rng):
    rows = []
    for y in range(h):
        row = []
        for x in range(w * 2):
            if y % 5 == 4:
                c = 12
            elif (x + (8 if (y // 5) % 2 else 0)) % 16 == 15:
                c = 12
            else:
                c = 2 if ((x + (8 if (y // 5) % 2 else 0)) // 16 + y // 5) % 3 else 8
                if rng.random() < 0.08:
                    c = 10
            row.append(c)
        rows.append(row)
    return pack(rows)


def behind(w, h, rng):          # dark corridor seen through the gate
    rows = []
    for y in range(h):
        row = [0] * (w * 2)
        for x in range(w * 2):
            if y > h * 2 // 3 and rng.random() < 0.15:
                row[x] = 11
            elif rng.random() < 0.02:
                row[x] = 11
        rows.append(row)
    return pack(rows)


def wood_gate(w, h, rng):
    rows = []
    for y in range(h):
        row = []
        for x in range(w * 2):
            if y % 28 in (6, 7, 8):
                c = 11 if (x % 12) else 15          # iron band with rivets
            elif x % 10 == 9:
                c = 0                               # gap between planks
            else:
                c = 9 if (x // 10) % 2 else 8
                if rng.random() < 0.15:
                    c = 0 if rng.random() < 0.3 else (9 if c == 8 else 8)
            row.append(c)
        rows.append(row)
    return pack(rows)


def grille(w, h):               # bytes: bars opaque, rest transparent (byte granular)
    data, mask = [], []
    for y in range(h):
        for x in range(w):
            bar = (x % 4 == 1) or (y % 16 in (3, 4))
            data.append(0xFF if (y % 16 in (3, 4)) else 0xF7)
            mask.append(bar)
    return data, mask


def noisy(w, h, rng, pal):
    return pack([[rng.choice(pal) for _ in range(w * 2)] for _ in range(h)])


def goblin(w, h, rng):
    data, mask = [], []
    for y in range(h):
        for x in range(w):
            dx, dy = (x + 0.5 - w / 2) / (w / 2), (y + 0.5 - h / 2) / (h / 2)
            inside = dx * dx + dy * dy <= 1.0 or (y > h * 0.8 and abs(dx) < 0.7)
            mask.append(inside)
            a, b = rng.choice((5, 13, 5, 0, 13)), rng.choice((5, 13, 13, 0))
            data.append((a << 4) | b)
    return data, mask


def opaque_bytes(data, mask):
    return [d for d, m in zip(data, mask) if m]


# ------------------------------------------------------------------ VRAM model
class Vram:
    def __init__(self):
        self.m = bytearray(65536)

    def vmc(self, src, dst, count, reps, tstep, sstep=0):
        s = src
        for r in range(reps):
            row = src + r * sstep if sstep else s
            for i in range(count):
                self.m[(dst + r * tstep + i) & 0xFFFF] = self.m[(row + i) & 0xFFFF]
            s = row + count
        return self

    def vmf(self, dst, val, count, reps, step):
        for r in range(reps):
            for i in range(count):
                self.m[(dst + r * step + i) & 0xFFFF] = val

    def rxv(self, stream, dst):
        u = vrl.unpacked_length(stream)
        out = vrl.decode(stream, base=self.m[dst:dst + u])
        self.m[dst:dst + len(out)] = out

    def vms(self, src, dst, lst, ram):
        n = lst[1] | lst[2] << 8
        p, rd = 3, src
        for _ in range(n):
            t = lst[p]
            if t == 1:
                off, ln = lst[p + 1] | lst[p + 2] << 8, lst[p + 3]
                for i in range(ln):
                    self.m[(dst + off + i) & 0xFFFF] = self.m[(rd + i) & 0xFFFF]
                rd += ln
                p += 4
            else:
                raise ValueError('model: VMS type %d' % t)

    def rects(self, entries, ram):
        for src, dst, w, h, st in entries:
            for r in range(h):
                for i in range(w):
                    self.m[(dst + r * st + i) & 0xFFFF] = ram[src + r * w + i]

    def rect(self, va, w, h, st):
        return [self.m[(va + r * st + i) & 0xFFFF] for r in range(h) for i in range(w)]


def fletcher(data):
    s1 = s2 = 0
    for b in data:
        s1 = (s1 + b) & 255
        s2 = (s2 + s1) & 255
    return s1, s2


# ------------------------------------------------------------------ memory allocation
class Ram:
    """Data in bank 0, two segments (D1 below the I/O area, D2 above)."""
    def __init__(self):
        self.seg = [[D1, D1_END, bytearray()], [D2, D2_END, bytearray()]]

    def put(self, data):
        data = bytes(data)
        for s in self.seg:
            if s[0] + len(s[2]) + len(data) <= s[1]:
                a = s[0] + len(s[2])
                s[2] += data
                return a
        raise SystemExit('RAM data does not fit (%d bytes more)' % len(data))

    def image(self):
        m = bytearray(65536)
        for base, _, b in self.seg:
            m[base:base + len(b)] = b
        return m


class Pool:
    def __init__(self, lo, hi):
        self.p, self.hi = lo, hi

    def take(self, n):
        a = self.p
        self.p += n
        if self.p > self.hi:
            raise SystemExit('VRAM pool full')
        return a


def w16(*ws):
    out = []
    for w in ws:
        out += [w & 0xFF, (w >> 8) & 0xFF]
    return out


def vcl_vmc2(src, dst, count, reps, tstep):
    return [2] + w16(src, dst, count, reps, tstep)


def vcl_vmc3(src, dst, count, reps, tstep, sstep):
    return [3] + w16(src, dst, count, reps, tstep, sstep)


def vcl_vmf5(dst, val, count, reps, step):
    return [5] + w16(dst, val, count, reps, step)


def vcl_rxv(src, dst):
    return [8] + w16(src, dst)


def vcl_vms(src, dst, lst):
    return [12] + w16(src, dst, lst)


def vms_list(w, h, mask, rows=None):
    """Type 1 runs (byte-granular transparency); rows: only the first n rows (truncation)."""
    ents, n = [], 0
    for y in range(h if rows is None else rows):
        x = 0
        while x < w:
            if mask[y * w + x]:
                x0 = x
                while x < w and mask[y * w + x] and x - x0 < 255:
                    x += 1
                ents += [1] + w16(y * STRIDE + x0) + [x - x0]
                n += 1
            else:
                x += 1
    return [0] + w16(n) + ents


def run_entries(w, h, mask, src_ram, dst):
    """CPU variant of a transparent sprite: one rectangle entry (1 row) per opaque run."""
    ents, rd = [], src_ram
    for y in range(h):
        x = 0
        while x < w:
            if mask[y * w + x]:
                x0 = x
                while x < w and mask[y * w + x]:
                    x += 1
                ents.append((rd, dst + y * STRIDE + x0, x - x0, 1, x - x0))
                rd += x - x0
            else:
                x += 1
    return ents


def rect_bytes(ents):
    out = []
    for src, dst, w, h, st in ents:
        out += w16(src, dst) + [w, h, st]
    return out + [0, 0, 0, 0, 0]


def lin_shape(n):
    for h in range(1, n + 1):
        if n % h == 0 and n // h <= 255:
            return n // h, h
    raise ValueError(n)


def pad16(data):
    return list(data) + [0] * (-len(data) % 16)


# ------------------------------------------------------------------ cases
def make_cases():
    rng = random.Random(SEED)
    ram, pool, vr = Ram(), Pool(POOL, POOL_END), Vram()
    init = []
    cases = []

    def case(name, uc, var, kind, op, vrect, pre=None, T=T_DEFAULT, info=None):
        cases.append(dict(name=name, uc=uc, var=var, kind=kind, op=op, vrect=vrect, pre=pre, T=T,
                          info=info or {}))

    def asset(data):            # packed in VRAM (pool), uploaded at init by RXV from a linear stream
        a = pool.take(len(data))
        s = vrl.encode(data)
        sa = ram.put(s)
        init.append(vcl_rxv(sa, a))
        return a, sa, s

    # background: screen and background buffer
    bg = bricks(80, 120, rng)
    bg_s = vrl.encode(bg)
    bg_sa = ram.put(bg_s)
    init.append(vcl_rxv(bg_sa, SCREEN))
    init.append(vcl_rxv(bg_sa, BGBUF))

    def restore(va, w, h):      # pre: background buffer -> screen rectangle
        return vcl_vmc3(BGBUF + va, SCREEN + va, w, h, STRIDE, STRIDE)

    def clear(va, w, h):        # pre for opaque cases: something different from the result
        return vcl_vmf5(SCREEN + va, 0x5A, w, h, STRIDE)

    # baseline
    case('none', 'base', 'none', KIND_NONE, None, None)
    case('vcl-empty', 'base', 'vcl', KIND_VCL, [0], None)
    case('vcl-vmc1', 'base', 'vcl', KIND_VCL, vcl_vmc2(BGBUF, SCREEN + 79, 1, 1, STRIDE) + [0], None)
    case('vcl-rxv0', 'base', 'vcl', KIND_VCL, vcl_rxv(ram.put([0]), SCREEN) + [0], None)

    # opaque patches: (name, list of (data, w, h, screen offset))
    gx, gy = GATE
    beh1 = behind(30, 64, rng)
    gate1 = wood_gate(30, 64, rng)
    g1c = beh1[:24 * 30] + gate1[:40 * 30]                       # baked mid phase: sunk by 24 rows
    beh2 = behind(20, 42, rng)
    gate2 = wood_gate(20, 42, rng)
    g2c = beh2[:16 * 20] + gate2[:26 * 20]
    eyes = noisy(8, 4, rng, (0, 1, 9, 15, 10))
    mouth = noisy(6, 3, rng, (2, 9, 0, 10))
    spark = noisy(2, 4, rng, (1, 15, 7))
    torch = noisy(4, 12, rng, (7, 8, 2, 1, 10))
    px, py = 2, 20                                               # portrait position on screen
    view = [bg[(r) * STRIDE + 5 + i] for r in range(96) for i in range(70)]
    patches = [
        ('eyes', [(eyes, 8, 4, (py + 10) * STRIDE + px + 4)]),
        ('portrait3', [(eyes, 8, 4, (py + 10) * STRIDE + px + 4), (mouth, 6, 3, (py + 20) * STRIDE + px + 5),
                       (spark, 2, 4, (py + 30) * STRIDE + px + 14)]),
        ('torch', [(torch, 4, 12, 30 * STRIDE + 12)]),
        ('gate2', [(g2c, 20, 42, (gy + 8) * STRIDE + gx + 5)]),
        ('gate1', [(g1c, 30, 64, gy * STRIDE + gx)]),
    ]
    vram_of = {}
    for name, parts in patches:
        ents_vmc, ents_cpu = [], []
        for data, w, h, off in parts:
            key = id(data)
            if key not in vram_of:
                vram_of[key] = (asset(data)[0], ram.put(data))
            va, ra = vram_of[key]
            ents_vmc += vcl_vmc2(va, SCREEN + off, w, h, STRIDE)
            ents_cpu.append((ra, SCREEN + off, w, h, STRIDE))
        # bounding rectangle: verification, pre, and the one RXV stream (mask = union of the parts)
        x0 = min(off % STRIDE for _, w, h, off in parts)
        y0 = min(off // STRIDE for _, w, h, off in parts)
        x1 = max(off % STRIDE + w for _, w, h, off in parts)
        y1 = max(off // STRIDE + h for _, w, h, off in parts)
        bw, bh, bva = x1 - x0, y1 - y0, y0 * STRIDE + x0
        bdata, bmask = [0] * (bw * bh), [False] * (bw * bh)
        for data, w, h, off in parts:
            ox, oy = off % STRIDE - x0, off // STRIDE - y0
            for r in range(h):
                for i in range(w):
                    bdata[(oy + r) * bw + ox + i] = data[r * w + i]
                    bmask[(oy + r) * bw + ox + i] = True
        full = all(bmask)
        st = vrl.encode_rect(bdata, bw, bh, STRIDE, None if full else bmask)
        sa = ram.put(st)
        nbytes = sum(w * h for _, w, h, _ in parts)
        vrect = (SCREEN + bva, bw, bh, STRIDE)
        pre = clear(bva, bw, bh)
        info = dict(bytes=nbytes, rects=len(parts), rows=sum(h for _, w, h, _ in parts), stream=len(st))
        case(name + '/vmc', name, 'vmc', KIND_VCL, ents_vmc + [0], vrect, pre, info=info)
        case(name + '/cpu', name, 'cpu', KIND_RECTS, ram.put(rect_bytes(ents_cpu)), vrect, pre, info=info)
        for T in (14, 2):
            case('%s/rxv%d' % (name, T), name, 'rxv%d' % T, KIND_VCL, vcl_rxv(sa, SCREEN + bva) + [0], vrect, pre,
                 T=T, info=info)

    # two-wing gate, mid phase: three columns of 10 bytes cut out of the baked frame (VCL type 3)
    g1va, g1ra = vram_of[id(g1c)]
    goff = gy * STRIDE + gx
    vrect = (SCREEN + goff, 30, 64, STRIDE)
    ents = []
    cpu = []
    for c in range(3):
        ents += vcl_vmc3(g1va + 10 * c, SCREEN + goff + 10 * c, 10, 64, STRIDE, 30)
    wing_raw = [g1c[r * 30 + 10 * c + i] for c in range(3) for r in range(64) for i in range(10)]
    wra = ram.put(wing_raw)
    for c in range(3):
        cpu.append((wra + c * 640, SCREEN + goff + 10 * c, 10, 64, STRIDE))
    info = dict(bytes=1920, rects=3, rows=192)
    case('wings1/vmc', 'wings1', 'vmc', KIND_VCL, ents + [0], vrect, clear(goff, 30, 64), info=info)
    case('wings1/cpu', 'wings1', 'cpu', KIND_RECTS, ram.put(rect_bytes(cpu)), vrect, clear(goff, 30, 64), info=info)

    # whole viewport from the off-screen buffer (70 x 96 at column 5)
    vrect = (SCREEN + 5, 70, 96, STRIDE)
    vs = vrl.encode_rect(view, 70, 96, STRIDE)
    vsa = ram.put(vs)
    info = dict(bytes=6720, rects=1, rows=96, stream=len(vs))
    case('view/vmc', 'view', 'vmc', KIND_VCL, vcl_vmc3(BGBUF + 5, SCREEN + 5, 70, 96, STRIDE, STRIDE) + [0], vrect,
         clear(5, 70, 96), info=info)
    for T in (14, 2):
        case('view/rxv%d' % T, 'view', 'rxv%d' % T, KIND_VCL, vcl_rxv(vsa, SCREEN + 5) + [0], vrect, clear(5, 70, 96),
             T=T, info=info)

    # transparent sprites
    gr_d, gr_m = grille(30, 64)
    gb_d, gb_m = goblin(16, 40, rng)
    sprites = {}
    for name, (d, m, w, h, off) in (('grille1', (gr_d, gr_m, 30, 64, goff)),
                                    ('goblin', (gb_d, gb_m, 16, 40, (gy + 14) * STRIDE + gx + 7))):
        ob = opaque_bytes(d, m)
        va = asset(ob)[0]
        ra = ram.put(ob)
        lst = vms_list(w, h, m)
        la = ram.put(lst)
        sprites[name] = (va, la, w, h, m, ob, ra)
        vrect = (SCREEN + off, w, h, STRIDE)
        pre = restore(off, w, h)
        st = vrl.encode_rect(d, w, h, STRIDE, m)
        sa = ram.put(st)
        runs = run_entries(w, h, m, ra, SCREEN + off)
        info = dict(bytes=len(ob), rects=len(runs), rows=h, stream=len(st), vms_list=len(lst))
        case(name + '/vms', name, 'vms', KIND_VCL, vcl_vms(va, SCREEN + off, la) + [0], vrect, pre, info=info)
        case(name + '/cpu', name, 'cpu', KIND_RECTS, ram.put(rect_bytes(runs)), vrect, pre, info=info)
        for T in (14, 2):
            case('%s/rxv%d' % (name, T), name, 'rxv%d' % T, KIND_VCL, vcl_rxv(sa, SCREEN + off) + [0], vrect, pre,
                 T=T, info=info)

    # layered (R2): behind layer, goblin behind the gate, then the gate (opaque rows or grille) sunk by 24 rows
    b1va = asset(beh1)[0]
    gt1va = asset(gate1)[0]
    gva, gla = sprites['goblin'][0], sprites['goblin'][1]
    grva = sprites['grille1'][0]
    grl_cut = ram.put(vms_list(30, 64, gr_m, rows=40))
    gob_off = (gy + 14) * STRIDE + gx + 7
    vrect = (SCREEN + goff, 30, 64, STRIDE)
    base = vcl_vmc2(b1va, SCREEN + goff, 30, 64, STRIDE) + vcl_vms(gva, SCREEN + gob_off, gla)
    case('r2gate/vcl', 'r2gate', 'layers', KIND_VCL, base + vcl_vmc2(gt1va, SCREEN + goff + 24 * STRIDE, 30, 40, STRIDE)
         + [0], vrect, clear(goff, 30, 64), info=dict(bytes=1920 + len(sprites['goblin'][5]) + 1200, rects=3))
    case('r2grille/vcl', 'r2grille', 'layers', KIND_VCL, base + vcl_vms(grva, SCREEN + goff + 24 * STRIDE, grl_cut)
         + [0], vrect, clear(goff, 30, 64), info=dict(rects=3))

    # preload RAM -> VRAM (one piece) into the scratch area
    # RTV and the CPU loop do not depend on the content: raw copies only where RAM already holds them
    gob = sprites['goblin'][5]
    gob_n = len(gob) - len(gob) % 8
    pre_sets = [('pl-gate1', g1c, ('rtv', 'cpu', 'rxv14', 'rxv2'), vram_of[id(g1c)][1]),
                ('pl-behind', beh1, ('rxv2',), None),
                ('pl-goblin', gob[:gob_n], ('rtv', 'rxv2'), sprites['goblin'][6]),
                ('pl-portrait', eyes + mouth, ('rtv', 'rxv2'), None),
                ('pl-bg', bg, ('rxv2',), None)]
    for name, data, vars_, ra in pre_sets:
        n = len(data)
        w, h = lin_shape(n)
        vrect = (SCRATCH, w, h, w)
        st = bg_s if name == 'pl-bg' else vrl.encode(data)
        if ra is None and any(v in ('rtv', 'cpu') for v in vars_):
            ra = ram.put(data)
        sa = bg_sa if name == 'pl-bg' else ram.put(st)
        info = dict(bytes=n, stream=len(st))
        for v in vars_:
            if v == 'rtv':
                case(name + '/rtv', name, 'rtv', KIND_RTV, w16(ra, SCRATCH, n), vrect, info=info)
            elif v == 'cpu':
                case(name + '/cpu', name, 'cpu', KIND_RECTS, ram.put(rect_bytes([(ra, SCRATCH, w, h, w)])), vrect,
                     info=info)
            else:
                T = int(v[3:])
                case('%s/%s' % (name, v), name, v, KIND_VCL, vcl_rxv(sa, SCRATCH) + [0], vrect, T=T, info=info)

    assert SCRATCH + max(len(d) for _, d, _, _ in pre_sets) <= SCRATCH_END
    init_list = sum(init, []) + [0]

    # expected results with the VRAM model
    rimg = ram.image()
    for e in init:
        vr.rxv(bytes(rimg[e[1] | e[2] << 8:]), e[3] | e[4] << 8) if e[0] == 8 else None
    for c in cases:
        for lst in ([c['pre'] + [0]] if c['pre'] else []) + ([c['op']] if c['kind'] == KIND_VCL else []):
            run_vcl(vr, lst, rimg)
        if c['kind'] == KIND_RECTS:
            ents, p = [], c['op']
            while True:
                src, dst = rimg[p] | rimg[p + 1] << 8, rimg[p + 2] | rimg[p + 3] << 8
                w, h, st = rimg[p + 4], rimg[p + 5], rimg[p + 6]
                if w == 0:
                    break
                ents.append((src, dst, w, h, st))
                p += 7
            vr.rects(ents, rimg)
        elif c['kind'] == KIND_RTV:
            src, dst, n = (c['op'][0] | c['op'][1] << 8, c['op'][2] | c['op'][3] << 8, c['op'][4] | c['op'][5] << 8)
            vr.m[dst:dst + n] = rimg[src:src + n]
        c['expect'] = fletcher(vr.rect(*c['vrect'])) if c['vrect'] else None
    return cases, ram, init_list, pool


def run_vcl(vr, lst, rimg):
    lst, p = list(lst) + [0] * 14, 0
    while lst[p]:
        t = lst[p]
        a = [lst[p + 1 + 2 * i] | lst[p + 2 + 2 * i] << 8 for i in range(6)]
        if t == 2:
            vr.vmc(a[0], a[1], a[2], a[3] or 256, sx(a[4]))
            p += 11
        elif t == 3:
            vr.vmc(a[0], a[1], a[2], a[3] or 256, sx(a[4]), sx(a[5]))
            p += 13
        elif t == 5:
            vr.vmf(a[0], a[1] & 255, a[2], a[3] or 256, sx(a[4]))
            p += 11
        elif t == 8:
            vr.rxv(bytes(rimg[a[0]:]), a[1])
            p += 5
        elif t == 12:
            vr.vms(a[0], a[1], bytes(rimg[a[2]:a[2] + 4096]), rimg)
            p += 7
        else:
            raise ValueError('model: VCL type %d' % t)


def sx(v):
    return v - 65536 if v >= 32768 else v


# ------------------------------------------------------------------ assembler
SYMS = ('vcl', 'rtv_vtr_swp_shared_setup', 'ram_to_vram', 'arg1', 'arg2', 'arg3')

TRAMP_SRC = r'''
; animation benchmark, entry below $4000 - generated by test/bench/anim/anim.py, do not edit
!cpu 6502
!src "syms.a"
* = $%(TRAMP)04x
    php                     ; BANK 15: SYS %(TRAMP)d
    sei
    lda $ff00
    pha
    lda #$3e                ; RAM 0 everywhere, I/O on
    sta $ff00
    jsr $%(CODE)04x
    pla
    sta $ff00
    plp
    rts
rtv_call                    ; arg1 = RAM, arg2 = VRAM, arg3 = count (as RTV)
    ldy arg2
    lda arg2 + 1
    jsr rtv_vtr_swp_shared_setup
    jsr ram_to_vram         ; leaves $ff00 = 0 (ROMs): we are below $4000
    lda #$3e
    sta $ff00
    rts
'''

MAIN_SRC = r'''
; animation benchmark - generated by test/bench/anim/anim.py, do not edit
!cpu 6502
!src "syms.a"
cid = $50                   ; case record (12 bytes, order as in the table)
kind = $51
tval = $52
pre = $53
op = $55
va = $57
vw = $59
vh = $5a
vs = $5b
rp = $5c
k = $5e
s1 = $5f
s2 = $60
src = $61
dst = $63
w = $65
h = $66
st = $67
cp = $68
rl = $6a
faddr = $6c
fval = $6e
pages = $6f
NZP = $20
K = %(K)d
RES = $%(RES)04x
VCL3 = vcl + 3              ; behind the parameter parsing: Y = low, A = high byte of the list
RTV_CALL = rtv_call

* = $%(CODE)04x
main
    ldx #NZP - 1
-   lda $50, x
    sta zsave, x
    dex
    bpl -
    lda $d011
    sta sv_d011
    and #$6f                ; VIC blanked, raster bit 8 cleared
    sta $d011
    lda $d030
    sta sv_d030
    ora #1                  ; 2 MHz
    sta $d030
    lda #$7f
    sta $dd0d
    lda $1c04
    sta sv_T
    ldy #NSAVE - 1
-   ldx save_regs, y
    jsr vdc_r
    sta mode_save, y
    dey
    bpl -
    ldy #NMODE - 1
-   ldx save_regs, y
    lda mode_vals, y
    jsr vdc_w
    dey
    bpl -
    ldx #25
    jsr vdc_r
    ora #$80                ; bitmap
    jsr vdc_w
    ldx #28
    jsr vdc_r
    ora #$18                ; 64 KB
    jsr vdc_w
    ldx #24
    jsr vdc_r
    and #$7f                ; fill
    jsr vdc_w
    lda #$00                ; bitmap $b400-$ffff = $0f
    sta faddr
    lda #$b4
    sta faddr + 1
    lda #$0f
    sta fval
    lda #76
    sta pages
    jsr fill
    lda #T_DEFAULT
    sta $1c04
    ldy #<init_list
    lda #>init_list
    jsr VCL3
    ldy #15
-   lda header, y
    sta RES, y
    dey
    bpl -
    lda #<(RES + 16)
    sta rp
    lda #>(RES + 16)
    sta rp + 1
    lda #<cases
    sta cp
    lda #>cases
    sta cp + 1
case_loop
    ldy #0
    lda (cp), y
    cmp #$ff
    bne +
    jmp all_done
+   ldy #11
-   lda (cp), y
    sta cid, y
    dey
    bpl -
    clc
    lda cp
    adc #12
    sta cp
    bcc +
    inc cp + 1
+   ldy #0
    lda cid
    sta (rp), y
    lda #1
    jsr rp_add
    lda tval
    sta $1c04
    lda #0
    sta k
sample
    lda pre + 1
    beq +
    ldy pre
    jsr VCL3
+   ldx #31                 ; VDC idle (block operation of the pre list finished)
    stx $d600
-   bit $d600
    bpl -
    jsr vdc_sync
    lda #$ff
    sta $dd04
    sta $dd05
    sta $dd06
    sta $dd07
    lda #$51
    sta $dd0f
    lda #$11
    sta $dd0e
    jsr do_op
    lda #0
    sta $dd0e
    sta $dd0f
    ldy #0
    lda $dd04
    eor #$ff
    sta (rp), y
    iny
    lda $dd05
    eor #$ff
    sta (rp), y
    iny
    lda $dd06
    eor #$ff
    sta (rp), y
    lda #3
    jsr rp_add
    inc k
    lda k
    cmp #K
    bne sample
    jsr verify
    jmp case_loop

all_done
    lda rp
    sta RES + 8
    lda rp + 1
    sta RES + 9
    lda sv_T
    sta $1c04
    ldy #NSAVE - 1
-   ldx save_regs, y
    lda mode_save, y
    jsr vdc_w
    dey
    bpl -
    lda sv_d030
    sta $d030
    lda sv_d011
    and #$7f
    sta $d011
    ldx #NZP - 1
-   lda zsave, x
    sta $50, x
    dex
    bpl -
    rts

do_op
    lda kind
    bne +
    rts
+   cmp #1
    bne +
    ldy op
    lda op + 1
    jmp VCL3
+   cmp #2
    bne +
    jmp rects
+   ldy #5                  ; RTV: src, dst, count -> arg1, arg2, arg3
-   lda (op), y
    sta arg1, y
    dey
    bpl -
    jmp RTV_CALL

rects                       ; entries src, dst, w, h, stride; w = 0 ends
    lda op
    sta rl
    lda op + 1
    sta rl + 1
rnext
    ldy #0
    lda (rl), y
    sta src
    iny
    lda (rl), y
    sta src + 1
    iny
    lda (rl), y
    sta dst
    iny
    lda (rl), y
    sta dst + 1
    iny
    lda (rl), y
    beq rdone
    sta w
    iny
    lda (rl), y
    sta h
    iny
    lda (rl), y
    sta st
    clc
    lda rl
    adc #7
    sta rl
    bcc rrow
    inc rl + 1
rrow
    ldx #18
    stx $d600
-   bit $d600
    bpl -
    lda dst + 1
    sta $d601
    inx
    stx $d600
-   bit $d600
    bpl -
    lda dst
    sta $d601
    ldx #31
    stx $d600
    ldy #0
-   lda (src), y
--  bit $d600
    bpl --
    sta $d601
    iny
    cpy w
    bne -
    clc
    lda src
    adc w
    sta src
    bcc +
    inc src + 1
+   clc
    lda dst
    adc st
    sta dst
    bcc +
    inc dst + 1
+   dec h
    bne rrow
    jmp rnext
rdone
    rts

verify                      ; Fletcher-16 (mod 256) over the rectangle va, vw x vh, stride vs
    lda va
    sta dst
    lda va + 1
    sta dst + 1
    lda #0
    sta s1
    sta s2
    lda vh
    beq vdone
    sta h
vrow
    ldx #18
    lda dst + 1
    jsr vdc_w
    inx
    lda dst
    jsr vdc_w
    ldx #31
    stx $d600
    ldy vw
-   bit $d600
    bpl -
    lda $d601
    clc
    adc s1
    sta s1
    clc
    adc s2
    sta s2
    dey
    bne -
    clc
    lda dst
    adc vs
    sta dst
    bcc +
    inc dst + 1
+   dec h
    bne vrow
vdone
    ldy #0
    lda s1
    sta (rp), y
    iny
    lda s2
    sta (rp), y
    lda #2
    jmp rp_add

vdc_sync                    ; status bit 5 (vertical blank) rises, then falls; then delay by sample
-   lda $d600
    and #$20
    beq -
-   lda $d600
    and #$20
    bne -
    ldx k
    ldy dly_tab, x
    beq ++
--  ldx #0
-   dex
    bne -
    dey
    bne --
++  rts

rp_add
    clc
    adc rp
    sta rp
    bcc +
    inc rp + 1
+   rts

vdc_w                       ; R(X) = A
    stx $d600
-   bit $d600
    bpl -
    sta $d601
    rts
vdc_r                       ; A = R(X)
    stx $d600
-   bit $d600
    bpl -
    lda $d601
    rts

fill                        ; faddr, fval, pages (256 bytes each), R24 in fill mode
    ldx #18
    lda faddr + 1
    jsr vdc_w
    inx
    lda faddr
    jsr vdc_w
    ldx #31
    lda fval
    jsr vdc_w
    ldx #30
    lda #255
    jsr vdc_w
    ldy pages
    dey
    beq +
-   lda #0
    jsr vdc_w
    dey
    bne -
+   rts

T_DEFAULT = %(T_DEFAULT)d
NSAVE = %(NSAVE)d
NMODE = %(NMODE)d
save_regs !byte %(SAVE_REGS)s
mode_vals !byte %(MODE_VALS)s
dly_tab !byte %(DLY)s
header !text "ANIM"
    !byte %(VERSION)d, K, %(NCASES)d, 0, 0, 0, 0, 0, 0, 0, 0, 0
sv_d011 !byte 0
sv_d030 !byte 0
sv_T !byte 0
mode_save !fill NSAVE, 0
zsave !fill NZP, 0
code_end
!if code_end > $%(TABLES)04x { !error "code too large" }

* = $%(TABLES)04x
%(TABLES_SRC)s
tables_end
!if tables_end > $%(CODE_END)04x { !error "tables too large" }
'''


def syms_from(symfile):
    vals = {}
    for line in open(symfile):
        p = line.split()
        if len(p) >= 3 and p[1] == '=' and p[0] in SYMS:
            vals[p[0]] = int(p[2].lstrip('$'), 16)
    missing = [s for s in SYMS if s not in vals]
    if missing:
        raise SystemExit('symbols missing: %s' % missing)
    return ''.join('%s = $%04x\n' % (k, v) for k, v in vals.items())


def gen_tables(cases, init_list):
    """Case table (12 bytes each) + VCL lists and RTV parameter blocks (all in bank 0 below $6000)."""
    lines, blobs = [], []

    def blob(data):
        lab = 'b%d' % len(blobs)
        blobs.append((lab, data))
        return lab

    lines.append('cases')
    for i, c in enumerate(cases):
        pre = blob(c['pre'] + [0]) if c['pre'] else '0'
        if c['kind'] in (KIND_VCL, KIND_RTV):
            op = blob(c['op'])
        elif c['kind'] == KIND_RECTS:
            op = '$%04x' % c['op']
        else:
            op = '0'
        va, vw, vh, vs = c['vrect'] or (0, 0, 0, 0)
        lines.append('    !byte %d, %d, %d, <%s, >%s, <%s, >%s, <$%04x, >$%04x, %d, %d, %d' % (
            i, c['kind'], c['T'], pre, pre, op, op, va, va, vw, vh, vs))
    lines.append('    !byte $ff')
    lines.append('init_list')
    lines.append('    !byte ' + ', '.join(map(str, init_list)))
    for lab, data in blobs:
        for j in range(0, len(data), 24):
            lines.append(('%s ' % lab if j == 0 else '    ') + '!byte ' + ', '.join(map(str, data[j:j + 24])))
    return '\n'.join(lines)


LOADER = '''10 TRAP 90
20 BLOAD"ANIMT",B0,P({TRAMP}):BLOAD"ANIMC",B0,P({CODE}):BLOAD"ANIMD1",B0,P({D1}){D2LOAD}
30 PRINT"MEASURING, ABOUT 1 MIN":BANK15:SYS{TRAMP}
40 BANK0:E=PEEK({RES8})+256*PEEK({RES9}):BANK15
50 RST 3:PRINT CHR$(147);
60 BSAVE"@ANIMRES",B0,P({RES}) TO P(E):PRINT"ANIMRES WRITTEN, FERTIG"{EXIT}
70 END
90 {ERR}
'''


def build(out):
    os.makedirs(out, exist_ok=True)
    cases, ram, init_list, pool = make_cases()
    nres = 16 + sum(1 + 3 * K + 2 for _ in cases)
    if RES + nres > RES_END:
        raise SystemExit('result block too large: %d bytes' % nres)
    env = dict(ACME=os.path.expanduser('~/.local/share/acme/ACME_Lib'), **os.environ)
    subprocess.run(['acme', '-f', 'cbm', '-o', os.path.join(out, 'v3.bin'), '-l', os.path.join(out, 'v3.sym'),
                    os.path.join(REPO, 'src', 'vdcbasic.asm')], check=True, env=env, cwd=REPO,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if open(os.path.join(out, 'v3.bin'), 'rb').read() != open(os.path.join(REPO, 'vdcbasic3.bin'), 'rb').read():
        raise SystemExit('src/vdcbasic.asm does not assemble to vdcbasic3.bin')
    open(os.path.join(out, 'syms.a'), 'w').write(syms_from(os.path.join(out, 'v3.sym')))
    dly = [round(k * 31 / K) for k in range(K)]
    open(os.path.join(out, 'tramp.a'), 'w').write(TRAMP_SRC % dict(TRAMP=TRAMP, CODE=CODE))
    open(os.path.join(out, 'main.a'), 'w').write(MAIN_SRC % dict(
        K=K, RES=RES, TRAMP=TRAMP, CODE=CODE, TABLES=TABLES, CODE_END=CODE_END, T_DEFAULT=T_DEFAULT,
        NSAVE=len(SAVE_REGS), NMODE=len(MODE), SAVE_REGS=', '.join(map(str, SAVE_REGS)),
        MODE_VALS=', '.join(str(v) for _, v in MODE), DLY=', '.join(map(str, dly)), VERSION=VERSION,
        NCASES=len(cases), TABLES_SRC=gen_tables(cases, init_list)))
    for src, prg in (('tramp.a', 'animt.prg'), ('main.a', 'animc.prg')):
        subprocess.run(['acme', '-f', 'cbm', '-o', prg, '-l', prg[:-4] + '.sym', src], check=True, cwd=out)
        if src == 'tramp.a':
            rc = [l.split()[2] for l in open(os.path.join(out, 'animt.sym')) if l.split()[:1] == ['rtv_call']]
            open(os.path.join(out, 'syms.a'), 'a').write('rtv_call = %s\n' % rc[0])
    files = [('animt', 'animt.prg'), ('animc', 'animc.prg')]
    for i, (base, _, data) in enumerate(ram.seg):
        if data:
            open(os.path.join(out, 'animd%d.prg' % (i + 1)), 'wb').write(bytes([base & 255, base >> 8]) + bytes(data))
            files.append(('animd%d' % (i + 1), 'animd%d.prg' % (i + 1)))
    tok = [sys.executable, os.path.join(REPO, 'test', 'bastok.py')]
    boot = open(os.path.join(REPO, 'boot', 'boot.bas')).read()
    boot = '\n'.join(('50 RUN"ANIM"' if l.startswith('50 ') else l) for l in boot.splitlines()) + '\n'
    open(os.path.join(out, 'boot.bas'), 'w').write(boot)
    subprocess.run(tok + [os.path.join(out, 'boot.bas'), os.path.join(out, 'boot.prg')], check=True)
    d2 = ':BLOAD"ANIMD2",B0,P(%d)' % D2 if ram.seg[1][2] else ''
    for name, ex, er in (('anim', '', 'PRINT"ERROR";ER;"IN";EL'),
                         ('anim-emu', ':POKE DEC("D7FF"),0', 'BANK15:POKE DEC("D7FF"),2')):
        txt = LOADER.format(TRAMP=TRAMP, CODE=CODE, D1=D1, D2LOAD=d2, RES=RES, RES8=RES + 8, RES9=RES + 9,
                            EXIT=ex, ERR=er)
        open(os.path.join(out, name + '.bas'), 'w').write(txt)
        subprocess.run(tok + ['--start', '0x4001', os.path.join(out, name + '.bas'), os.path.join(out, name + '.prg')],
                       check=True)
    # emulators: d64 (a 1581 in Z64K keeps the last sectors in its track cache when the run ends at $D7FF)
    for img, loader in (('anim.d81', 'anim.prg'), ('anim-emu.d64', 'anim-emu.prg')):
        p = os.path.join(out, img)
        if os.path.exists(p):
            os.remove(p)
        cmd = ['c1541', '-format', 'anim bench,an', img[-3:], p, '-write', os.path.join(out, 'boot.prg'), 'boot',
               '-write', os.path.join(REPO, 'vdcbasic3.bin'), 'vdcbasic3', '-write', os.path.join(out, loader), 'anim']
        for n, f in files:
            cmd += ['-write', os.path.join(out, f), n]
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL)
    meta = [{k: v for k, v in c.items() if k not in ('op', 'pre')} for c in cases]
    json.dump(dict(version=VERSION, K=K, cases=meta, ram=[len(s[2]) for s in ram.seg], pool_used=pool.p - POOL,
                   result_bytes=nres), open(os.path.join(out, 'cases.json'), 'w'), indent=1)
    return os.path.join(out, 'anim.d81')


# ------------------------------------------------------------------ evaluation
def parse(data, meta):
    if data[:2] == bytes([RES & 255, RES >> 8]):
        data = data[2:]
    if data[:4] != b'ANIM':
        raise SystemExit('not an ANIMRES block')
    ver, k, n = data[4], data[5], data[6]
    p, recs = 16, []
    for _ in range(n):
        cid = data[p]
        samples = [data[p + 1 + 3 * i] | data[p + 2 + 3 * i] << 8 | data[p + 3 + 3 * i] << 16 for i in range(k)]
        chk = (data[p + 1 + 3 * k], data[p + 2 + 3 * k])
        recs.append((cid, samples, chk))
        p += 1 + 3 * k + 2
    return ver, k, recs


def evaluate(path, source, out):
    meta = json.load(open(os.path.join(out, 'cases.json')))
    ver, k, recs = parse(open(path, 'rb').read(), meta)
    cases = meta['cases']
    base = sorted(recs[0][1])[len(recs[0][1]) // 2]                    # case 'none': timer and call overhead
    rows, bad = [], []
    for cid, s, chk in recs:
        c = cases[cid]
        ok = c['expect'] is None or tuple(c['expect']) == chk
        if not ok:
            bad.append(c['name'])
        t = sorted(x - base for x in s)
        rows.append(dict(name=c['name'], uc=c['uc'], var=c['var'], info=c['info'], ok=ok,
                         min=t[0], med=t[len(t) // 2], mean=sum(t) / len(t), max=t[-1]))
    today = datetime.date.today().strftime('%Y%m%d')
    res = dict(source=source, date=today, version=ver, K=k, baseline_us=base, provisional=source != 'c128',
               cases=rows, failed=bad)
    rdir = os.path.join(HERE, 'results')
    os.makedirs(rdir, exist_ok=True)
    stem = os.path.join(rdir, '%s-%s' % (source, today))
    json.dump(res, open(stem + '.json', 'w'), indent=1)
    open(stem + '.md', 'w').write(report(res))
    return stem, bad


def report(res):
    L = ['# Animation benchmark (%s, %s)' % (res['source'], res['date']), '',
         'VDC-BASIC v3 (`vdcbasic3.bin`), chunky mode as Crawl, 2 MHz, R36 = 0, VIC blanked. '
         'Times in µs (CIA), timer/call overhead (%d µs) subtracted; K = %d positions in the frame.' % (
             res['baseline_us'], res['K']), '']
    if res['provisional']:
        L += ['**Emulator: provisional, only real hardware counts.**', '']
    if res['failed']:
        L += ['**Wrong result (checksum): %s**' % ', '.join(res['failed']), '']
    L += ['| case | bytes | stream | min | median | mean | max | µs/byte (mean) | ok |', '|---|---|---|---|---|---|---|---|---|']
    for r in res['cases']:
        b = r['info'].get('bytes')
        L.append('| %s | %s | %s | %d | %d | %.0f | %d | %s | %s |' % (
            r['name'], b if b else '', r['info'].get('stream', ''), r['min'], r['med'], r['mean'], r['max'],
            '%.2f' % (r['mean'] / b) if b else '', 'ok' if r['ok'] else '**FAIL**'))
    return '\n'.join(L) + '\n'


# ------------------------------------------------------------------ emulators
def read_res(img, out):
    res = os.path.join(out, 'ANIMRES.prg')
    if os.path.exists(res):
        os.remove(res)
    subprocess.run(['c1541', '-attach', img, '-read', 'animres', res], check=True, stdout=subprocess.DEVNULL,
                   stderr=subprocess.DEVNULL)
    return res


def run_vice(out, timeout):
    build(out)
    img = os.path.join(out, 'run-vice.d64')
    shutil.copy(os.path.join(out, 'anim-emu.d64'), img)
    boot = os.path.join(out, 'boot.prg')
    with open(os.path.join(out, 'vice.log'), 'w') as log:
        r = subprocess.run(['x128', '-default', '-warp', '-sounddev', 'dummy', '-debugcart', '-80col', '-VDC64KB',
                            '+drive8truedrive', '-trapdevice8', '-8', img, '-autostartprgmode', '1', '-autostart', boot,
                            '-limitcycles', '2000000000'], stdout=log, stderr=subprocess.STDOUT, timeout=timeout)
    if r.returncode != 0:
        raise SystemExit('x128 exit code %d (see %s/vice.log)' % (r.returncode, out))
    return evaluate(read_res(img, out), 'vice3.10', out)


def run_z64k(out, timeout):
    import z64k
    build(out)
    img = os.path.join(out, 'run-z64k.d64')
    r = z64k.run(os.path.join(out, 'boot.prg'), d64=os.path.join(out, 'anim-emu.d64'), limit=4_000_000_000,
                 timeout=timeout, save=img, log=os.path.join(out, 'z64k.log'))
    if r.rc != 0:
        raise SystemExit('Z64K exit code %d (see %s/z64k.log)' % (r.rc, out))
    return evaluate(read_res(r.image, out), 'z64k', out)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest='cmd', required=True)
    for n in ('build', 'vice', 'z64k'):
        p = sub.add_parser(n)
        p.add_argument('--out', default=os.path.join(REPO, 'build', 'anim'))
        p.add_argument('--timeout', type=int, default=900)
    p = sub.add_parser('eval')
    p.add_argument('res')
    p.add_argument('--source', default='c128')
    p.add_argument('--out', default=os.path.join(REPO, 'build', 'anim'))
    a = ap.parse_args()
    if a.cmd == 'build':
        print(build(a.out))
        print(json.dumps(json.load(open(os.path.join(a.out, 'cases.json')))['ram']), 'bytes RAM data')
    elif a.cmd in ('vice', 'z64k'):
        stem, bad = (run_vice if a.cmd == 'vice' else run_z64k)(a.out, a.timeout)
        print(open(stem + '.md').read())
        sys.exit(1 if bad else 0)
    else:
        stem, bad = evaluate(a.res, a.source, a.out)
        print(open(stem + '.md').read())
        sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
