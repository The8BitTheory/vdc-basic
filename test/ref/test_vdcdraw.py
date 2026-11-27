"""Selbsttests des Referenzmodells: python3 -m unittest discover -s test/ref"""
import hashlib
import itertools
import json
import os
import random
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vdcdraw import Vdc, VdcError, line_pixels, rect_pixels  # noqa: E402
import gen_vectors  # noqa: E402


def nz(v):
    """Alle Bytes <> 0 als {adresse: wert}."""
    return {i: b for i, b in enumerate(v.ram) if b}


def mk(mode, base=0, stride=80, vram=None):
    v = Vdc(vram)
    v.vdm(mode, base, stride)
    return v


class Hand(unittest.TestCase):
    def test_initial_state(self):
        v = Vdc()
        self.assertEqual((v.mode, v.base, v.stride), (0, 0, 80))
        self.assertEqual(len(v.ram), 65536)

    def test_point_m0(self):
        v = mk(0); v.vdl(3, 0, 3, 0, 1)
        self.assertEqual(nz(v), {0: 0x10})
        v = mk(0); v.vdl(15, 2, 15, 2, 1)
        self.assertEqual(nz(v), {160 + 1: 0x01})

    def test_point_m1(self):
        v = mk(1); v.vdl(2, 0, 2, 0, 0xA)
        self.assertEqual(nz(v), {1: 0xA0})
        v = mk(1); v.vdl(3, 0, 3, 0, 0xA)
        self.assertEqual(nz(v), {1: 0x0A})

    def test_hline_vline_m0(self):
        v = mk(0); v.vdl(0, 0, 9, 0, 1)
        self.assertEqual(nz(v), {0: 0xFF, 1: 0xC0})
        v = mk(0); v.vdl(2, 0, 2, 2, 1)
        self.assertEqual(nz(v), {0: 0x20, 80: 0x20, 160: 0x20})

    def test_hline_vline_m1(self):
        v = mk(1); v.vdl(1, 0, 4, 0, 0xF)
        self.assertEqual(nz(v), {0: 0x0F, 1: 0xFF, 2: 0xF0})
        v = mk(1); v.vdl(3, 0, 3, 1, 0x3)
        self.assertEqual(nz(v), {1: 0x03, 81: 0x03})

    def test_diag(self):
        v = mk(0); v.vdl(0, 0, 3, 3, 1)
        self.assertEqual(nz(v), {0: 0x80, 80: 0x40, 160: 0x20, 240: 0x10})
        v = mk(1); v.vdl(0, 0, 3, 3, 0xC)
        self.assertEqual(nz(v), {0: 0xC0, 80: 0x0C, 161: 0xC0, 241: 0x0C})

    def test_line_0_0_4_1(self):
        exp = [(0, 0), (1, 0), (2, 0), (3, 1), (4, 1)]
        self.assertEqual(line_pixels(0, 0, 4, 1), exp)
        v = mk(0); v.vdl(0, 0, 4, 1, 1)
        self.assertEqual(nz(v), {0: 0xE0, 80: 0x18})
        v = mk(1); v.vdl(0, 0, 4, 1, 0xC)
        self.assertEqual(nz(v), {0: 0xCC, 1: 0xC0, 81: 0x0C, 82: 0xC0})

    def test_vdr(self):
        v = mk(0); v.vdr(1, 1, 3, 3, 1)
        self.assertEqual(nz(v), {80: 0x70, 160: 0x50, 240: 0x70})
        v = mk(1); v.vdr(1, 1, 3, 3, 5)
        self.assertEqual(nz(v), {80: 0x05, 81: 0x55, 160: 0x05, 161: 0x05, 240: 0x05, 241: 0x55})

    def test_vdb_odd_edges(self):
        v = mk(1); v.vdb(1, 0, 4, 1, 0xF)
        self.assertEqual(nz(v), {0: 0x0F, 1: 0xFF, 2: 0xF0})
        v = mk(0); v.vdb(3, 0, 7, 1, 1)
        self.assertEqual(nz(v), {0: 0x1F, 1: 0xC0})
        v = mk(0); v.vdb(8, 0, 16, 2, 1)       # byte-aligned
        self.assertEqual(nz(v), {1: 0xFF, 2: 0xFF, 81: 0xFF, 82: 0xFF})

    def test_clear_and_xor_m0(self):
        v = mk(0, vram=b"\xAA" * 65536)
        v.vdb(0, 0, 8, 1, 0); self.assertEqual(v.ram[0], 0)
        v.vdb(0, 1, 8, 1, 1); self.assertEqual(v.ram[80], 0xFF)
        v.vdb(0, 2, 8, 1, 2); self.assertEqual(v.ram[160], 0x55)

    def test_chunky_keeps_other_nibble(self):
        v = mk(1, vram=b"\x5A" * 65536)
        v.vdl(0, 0, 0, 0, 3); self.assertEqual(v.ram[0], 0x3A)
        v.vdl(1, 0, 1, 0, 3); self.assertEqual(v.ram[0], 0x33)


class Lines(unittest.TestCase):
    def test_grid(self):
        n = 9
        for x1, y1, x2, y2 in itertools.product(range(n), repeat=4):
            p = line_pixels(x1, y1, x2, y2)
            dx, dy = abs(x2 - x1), abs(y2 - y1)
            self.assertEqual(len(p), max(dx, dy) + 1)
            self.assertEqual(len(set(p)), len(p))
            self.assertIn((x1, y1), p)
            self.assertIn((x2, y2), p)
            for a, b in zip(p, p[1:]):
                self.assertLessEqual(max(abs(a[0] - b[0]), abs(a[1] - b[1])), 1)
            self.assertEqual(set(p), set(line_pixels(x2, y2, x1, y1)))

    def test_symmetry_in_vram(self):
        for mode in (0, 1):
            a, b = mk(mode), mk(mode)
            a.vdl(5, 70, 300, 3, 1); b.vdl(300, 3, 5, 70, 1)
            self.assertEqual(a.ram, b.ram)

    def test_max_coord(self):
        p = line_pixels(0, 0, 16383, 16383)
        self.assertEqual(len(p), 16384)


class Xor(unittest.TestCase):
    def test_twice_is_identity(self):
        r = random.Random(5)
        img = bytes(r.getrandbits(8) for _ in range(65536))
        cmds = [("vdl", (3, 4, 250, 90, 2)), ("vdl", (200, 5, 10, 150, 2)),
                ("vdr", (7, 9, 40, 30, 2)), ("vdr", (9, 9, 1, 1, 2)), ("vdr", (9, 9, 2, 2, 2)),
                ("vdb", (13, 17, 61, 23, 2)), ("vdb", (0, 0, 640, 3, 2))]
        for name, args in cmds:
            v = mk(0, vram=img)
            getattr(v, name)(*args)
            self.assertNotEqual(bytes(v.ram), img)
            getattr(v, name)(*args)
            self.assertEqual(bytes(v.ram), img, name)

    def test_each_pixel_once(self):
        # XOR einmal ueber ein Nullbild: jedes Pixel genau 1 => Anzahl gesetzter Bits = Pixelzahl
        for (w, h) in ((2, 2), (5, 3), (7, 7), (1, 6), (6, 1), (3, 2)):
            v = mk(0); v.vdr(3, 3, w, h, 2)
            bits = sum(bin(b).count("1") for b in v.ram)
            self.assertEqual(bits, len(rect_pixels(3, 3, w, h)))
        for x1, y1, x2, y2 in ((0, 0, 100, 37), (50, 50, 50, 90), (9, 9, 9, 9)):
            v = mk(0); v.vdl(x1, y1, x2, y2, 2)
            self.assertEqual(sum(bin(b).count("1") for b in v.ram), max(abs(x2 - x1), abs(y2 - y1)) + 1)


class Rect(unittest.TestCase):
    def test_count(self):
        for w in range(2, 12):
            for h in range(2, 12):
                p = rect_pixels(4, 6, w, h)
                self.assertEqual(len(p), 2 * w + 2 * h - 4)
                self.assertEqual(len(set(p)), len(p))

    def test_degenerate(self):
        self.assertEqual(set(rect_pixels(2, 3, 1, 5)), set(line_pixels(2, 3, 2, 7)))
        self.assertEqual(set(rect_pixels(2, 3, 5, 1)), set(line_pixels(2, 3, 6, 3)))
        self.assertEqual(rect_pixels(2, 3, 0, 5), [])
        self.assertEqual(rect_pixels(2, 3, 5, 0), [])
        self.assertEqual(rect_pixels(2, 3, 1, 1), [(2, 3)])

    def test_vdb_matches_pixel_set(self):
        for mode in (0, 1):
            v = mk(mode)
            v.vdb(5, 3, 9, 4, 1)
            for y in range(0, 10):
                for x in range(0, 20):
                    inside = 5 <= x < 14 and 3 <= y < 7
                    self.assertEqual(v.pixel(x, y), 1 if inside else 0, (mode, x, y))

    def test_zero_size_no_effect(self):
        v = mk(0); v.vdb(3, 3, 0, 5, 1); v.vdr(3, 3, 5, 0, 1)
        self.assertEqual(nz(v), {})


class Errors(unittest.TestCase):
    def check(self, v, fn, args, code):
        before = bytes(v.ram); st = (v.mode, v.base, v.stride)
        with self.assertRaises(VdcError) as cm:
            getattr(v, fn)(*args)
        self.assertEqual(cm.exception.code, code, (fn, args))
        self.assertEqual(bytes(v.ram), before)
        self.assertEqual((v.mode, v.base, v.stride), st)

    def test_all(self):
        r = random.Random(1)
        img = bytes(r.getrandbits(8) for _ in range(65536))
        v0, v1 = mk(0, vram=img), mk(1, vram=img)
        for v in (v0, v1):
            self.check(v, "vdm", (2, 0, 80), 14)
            self.check(v, "vdm", (-1, 0, 80), 14)
            self.check(v, "vdm", (0, 65536, 80), 14)
            self.check(v, "vdm", (0, -1, 80), 14)
            self.check(v, "vdm", (0, 0, 0), 14)
            self.check(v, "vdm", (0, 0, 65536), 14)
            self.check(v, "vdm", (0, 0, None), 11)
            self.check(v, "vdl", (0, 0, 16384, 0, 1), 14)
            self.check(v, "vdl", (0, 16384, 0, 0, 1), 14)
            self.check(v, "vdl", (-1, 0, 0, 0, 1), 14)
            self.check(v, "vdl", (0, 0, 5, 5, None), 11)
            self.check(v, "vdl", (0, 0, 5, None, 1), 11)
            self.check(v, "vdr", (65535, 0, 2, 1, 1), 14)
            self.check(v, "vdb", (0, 65535, 1, 2, 1), 14)
            self.check(v, "vdb", (0, 0, 65536, 1, 1), 14)
            self.check(v, "vdb", (-1, 0, 1, 1, 1), 14)
            self.check(v, "vdb", (0, 0, 1, 1, None), 11)
        for fn, a in (("vdl", (0, 0, 9, 9)), ("vdr", (0, 0, 9, 9)), ("vdb", (0, 0, 9, 9))):
            self.check(v0, fn, a + (3,), 14)
            self.check(v0, fn, a + (-1,), 14)
            self.check(v1, fn, a + (16,), 14)
            self.check(v1, fn, a + (-1,), 14)
        # Grenzfaelle gueltig
        v0.vdl(16383, 16383, 0, 0, 1)
        v1.vdb(65535, 65535, 1, 1, 15)
        v1.vdb(1, 0, 65535, 1, 1)

    def test_order_left_to_right(self):
        # wie getadr: 0..65535 sofort beim Parsen, befehlsspezifische Bereiche erst danach
        v = mk(0)
        self.check(v, "vdl", (70000, 0, None, 0, 1), 14)
        self.check(v, "vdl", (20000, 0, None, 0, 1), 11)
        self.check(v, "vdm", (2, 0, None), 11)
        self.check(v, "vdm", (2, 70000, 80), 14)
        self.check(v, "vdb", (0, 0, 9, 9, None), 11)
        self.check(v, "vdb", (65535, 0, 9, 9, None), 11)

    def test_non_int_is_test_bug(self):
        with self.assertRaises(TypeError):
            mk(0).vdl(1.5, 0, 3, 3, 1)

    def test_error_before_any_write(self):
        v = mk(0)
        with self.assertRaises(VdcError):
            v.vdb(0, 0, 100, 100, 7)
        self.assertEqual(nz(v), {})


class Wrap(unittest.TestCase):
    def test_base_wrap(self):
        v = mk(0, base=0xFFF0, stride=80)
        v.vdl(0, 0, 200, 0, 1)   # Bytes 0xFFF0 .. 0xFFF0+25
        self.assertEqual(v.ram[0xFFF0], 0xFF)
        self.assertEqual(v.ram[0xFFFF], 0xFF)
        self.assertEqual(v.ram[0x0000], 0xFF)
        self.assertEqual(v.ram[0x0008], 0xFF)    # x=200 -> byte 25 -> 0xFFF0+25 = 0x0009
        self.assertEqual(v.ram[0x0009], 0x80)   # x=200 = letztes Pixel

    def test_row_overflow_in_next_line(self):
        v = mk(1, stride=80)
        v.vdl(160, 0, 160, 0, 7)       # x=160 -> byte 80 = Zeile 1, Byte 0
        self.assertEqual(nz(v), {80: 0x70})

    def test_y_stride_wrap(self):
        v = mk(0, base=0, stride=300)
        v.vdl(0, 300, 0, 300, 1)       # 300*300 = 90000 mod 65536 = 24464
        self.assertEqual(nz(v), {24464: 0x80})


def full_bytes(mode, x, w):
    """Anzahl voller Mittelbytes einer Spanne x..x+w-1 (3.1)."""
    ppb = 8 if mode == 0 else 2
    first = -(-x // ppb)              # erstes volles Byte
    last = (x + w) // ppb             # erstes Byte nach dem letzten vollen
    return max(0, last - first)


class Vectors(unittest.TestCase):
    def test_mid_counts_match_names(self):
        for c in gen_vectors.gen_cases():
            if c["group"] in ("S0", "S1") and "-mid" in c["name"]:
                m = int(c["name"].split("-mid")[1].split("-")[0])
                _, x, _, w, _, _ = c["cmds"][1]
                self.assertEqual(full_bytes(0 if c["group"] == "S0" else 1, x, w), m, c["name"])


    def test_generator_consistent(self):
        data = gen_vectors.build({"S1", "R", "G"})
        self.assertGreater(len(data["cases"]), 100)
        init = gen_vectors.base_pattern()
        self.assertEqual(hashlib.sha1(init).hexdigest(), data["init"]["sha1"])
        json.dumps(data)
        for c in data["cases"]:
            ram = bytearray(init)
            for addr, hx in c["diff"]:
                ram[addr:addr + len(hx) // 2] = bytes.fromhex(hx)
            self.assertEqual(hashlib.sha1(bytes(ram)).hexdigest(), c["sha1"], c["name"])
            v = Vdc(init)
            for cmd in c["cmds"]:
                v.run(cmd)
            self.assertEqual(bytes(v.ram), bytes(ram), c["name"])

    def test_deterministic_and_all_groups_valid(self):
        a = gen_vectors.build()
        b = gen_vectors.build()
        self.assertEqual(json.dumps(a), json.dumps(b))
        self.assertEqual({c["group"] for c in a["cases"]}, {"S1", "S0", "V", "L", "R", "B", "G", "Z"})
        for c in a["cases"]:
            self.assertEqual(c["cmds"][0][0], "VDM", c["name"])   # Zustand nie aus dem Vorfall
        names = [c["name"] for c in a["cases"]]
        self.assertEqual(len(names), len(set(names)))


if __name__ == "__main__":
    unittest.main()
