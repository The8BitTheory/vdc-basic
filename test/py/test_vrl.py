"""Tests für tools/vrl.py (Spec docs/specs/P1-vrl.md, Abschnitt 9)."""
import functools
import json
import os
import random
import subprocess
import sys
import tempfile
import unittest

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(ROOT, "tools"))
import vrl  # noqa: E402

VRL_PY = os.path.join(ROOT, "tools", "vrl.py")
N = None


def wr(path, data):
    with open(path, "wb") as f:
        f.write(data)


def rd(path):
    with open(path, "rb") as f:
        return f.read()


def hx(s):
    return bytes.fromhex(s)


class Vectors(unittest.TestCase):
    def test_encode(self):
        r128 = " ".join("%02X" % i for i in range(127))
        cases = [
            ([], "00"), ([5], "01 05 00"), ([7, 7], "80 07 00"), ([7] * 3, "81 07 00"),
            ([7] * 127, "FD 07 00"), ([7] * 128, "FF 80 00 07 00"),
            ([7] * 65535, "FF FF FF 07 00"),
            ([7] * 128 + [1, 2], "FD 07 03 07 01 02 00"),
            ([1, 2, 3], "03 01 02 03 00"), ([1, 1, 1, 2], "81 01 01 02 00"),
            ([1, 1, 2, 2, 3], "05 01 01 02 02 03 00"),
            (list(range(128)), "7F " + r128 + " 01 7F 00"),
            ([N, 5], "FE 01 00 01 05 00"), ([5, N], "01 05 00"), ([N] * 3, "00"),
            ([5, N, N, 5], "01 05 FE 02 00 01 05 00"),
            ([N] * 65534 + [1], "FE FE FF 01 01 00"),
            ([N] * 70000, "00"),
            ([9, 9, N, 9, 9], "80 09 FE 01 00 80 09 00"),
            ([1, 1, 1, 2, 3, 3, 3], "81 01 01 02 81 03 00"),
        ]
        for data, exp in cases:
            with self.subTest(data=str(data[:6]) + "…%d" % len(data)):
                self.assertEqual(vrl.encode(data), hx(exp))
        self.assertEqual(vrl.encode(b""), b"\x00")
        self.assertEqual(vrl.encode(bytearray(b"\x05")), hx("01 05 00"))

    def test_errors(self):
        for data in ([7] * 65536, [N] * 65535 + [1], [256], [-1]):
            with self.assertRaises(ValueError):
                vrl.encode(data)

    def test_rect(self):
        self.assertEqual(vrl.encode_rect([1, 2, 3, 4], 2, 2, 80), hx("02 01 02 FE 4E 00 02 03 04 00"))
        self.assertEqual(vrl.encode_rect([5] * 8, 4, 2, 4), hx("86 05 00"))
        self.assertEqual(vrl.encode_rect([5] * 4, 4, 1, 4, mask=[1, 0, 0, 1]), hx("01 05 FE 02 00 01 05 00"))
        self.assertEqual(vrl.encode_rect([1, 2, 3, 4], 2, 2, 10, mask=[1, 0, 0, 1]), hx("01 01 FE 0A 00 01 04 00"))
        self.assertEqual(vrl.encode_rect([1, 2], 2, 1, 80, mask=[0, 0]), b"\x00")
        with self.assertRaises(ValueError):
            vrl.encode_rect([1, 2], 2, 1, 1)
        with self.assertRaises(ValueError):
            vrl.encode_rect([1, 2], 2, 0, 2)


def expected_image(x, base):
    img = bytearray(base)
    for i, v in enumerate(x):
        if v is not None:
            img[i] = v
    return img


def brute(x):
    """Unabhängige erschöpfende Suche: minimale Stromlänge ohne End-Byte."""
    x = list(x)
    while x and x[-1] is None:
        x.pop()

    @functools.lru_cache(None)
    def f(i):
        if i == len(x):
            return 0
        best = 10 ** 9
        if x[i] is None:
            k = i
            while k < len(x) and x[k] is None:
                k += 1
            return 3 + f(k)
        j = i
        while j < len(x) and x[j] is not None:
            j += 1
        for n in range(1, j - i + 1):
            best = min(best, 1 + n + f(i + n))
        for L in range(2, j - i + 1):
            if len(set(x[i:i + L])) == 1:
                best = min(best, (2 if L <= 127 else 4) + f(i + L))
        return best
    return f(0)


class Properties(unittest.TestCase):
    def test_roundtrip(self):
        rng = random.Random(1)
        for _ in range(2000):
            n = rng.randint(0, 600)
            alpha = rng.choice([2, 5, 256])
            skipfrac = rng.choice([0, 0.1, 0.5])
            x = []
            while len(x) < n:
                if rng.random() < 0.03:
                    v = None if rng.random() < skipfrac else rng.randrange(alpha)
                    x += [v] * rng.randint(2, 300)
                else:
                    x.append(None if rng.random() < skipfrac else rng.randrange(alpha))
            x = x[:n]
            s = vrl.encode(x)
            u = len(x)
            while u and x[u - 1] is None:
                u -= 1
            base = bytes(rng.randrange(256) for _ in range(u + 5))
            self.assertEqual(vrl.decode(s, base), expected_image(x[:u], base))
            self.assertEqual(vrl.unpacked_length(s), u)

    def test_optimal(self):
        rng = random.Random(2)
        for _ in range(300):
            x = [rng.choice([0, 1, 2, None]) for _ in range(rng.randint(0, 12))]
            self.assertEqual(len(vrl.encode(x)) - 1, brute(x), x)

    def test_determinism(self):
        x = [random.Random(3).choice([1, 2, None]) for _ in range(500)]
        self.assertEqual(vrl.encode(x), vrl.encode(x))

    def test_limits(self):
        for n in (127, 128):
            self.assertEqual(len(vrl.encode(list(range(n)))) - 1, n + (1 if n == 127 else 2))
        for L in (127, 128, 129, 255, 256, 257, 65534, 65535):
            x = [7] * L
            s = vrl.encode(x)
            self.assertEqual(len(s), 3 if L <= 127 else 5)
            self.assertEqual(vrl.decode(s), bytearray(x))
        for k in (65533, 65534):
            x = [N] * k + [1]
            s = vrl.encode(x)
            self.assertEqual(vrl.decode(s), bytearray(k) + b"\x01")
            self.assertEqual(vrl.unpacked_length(s), k + 1)
        with self.assertRaises(ValueError):
            vrl.encode([7] * 65536)

    def test_decoder_errors(self):
        for s in (b"", b"\x03\x01", b"\x81", b"\xFE\x01", b"\xFF\x00\x00", b"\x01\x05"):
            with self.subTest(s=s):
                with self.assertRaises(ValueError):
                    vrl.decode(s)
        with self.assertRaises(ValueError):          # Ziel über 65536
            vrl.decode(hx("FE 00 00 FE 01 00 00"))

    def test_decoder_foreign(self):
        self.assertEqual(vrl.decode(hx("FF 01 00 07 00")), bytearray([7]))
        self.assertEqual(vrl.decode(hx("80 07 80 07 00")), bytearray([7] * 4))
        self.assertEqual(vrl.decode(hx("FE 05 00 00")), bytearray(5))
        self.assertEqual(vrl.unpacked_length(hx("FE 05 00 00")), 5)
        self.assertEqual(len(vrl.decode(hx("FF 00 00 07 00"))), 65536)
        t = vrl.decode_tokens(hx("01 05 81 07 FE 02 00 FF 03 00 09 00"))
        self.assertEqual([x.kind for x in t], ["lit", "run", "skip", "lrun", "end"])
        self.assertEqual(t[0], vrl.Token("lit", 1, b"\x05", 0))
        self.assertEqual([x.offset for x in t], [0, 2, 4, 7, 11])

    def test_file(self):
        s = vrl.encode([1, 2, 3, N, 4])
        f = vrl.pack_file(s, 0x3000)
        self.assertEqual(vrl.unpack_file(f), (0x3000, 1, 5, s))
        self.assertEqual(f[:7], hx("00 30 56 52 01 05 00"))
        with self.assertRaises(ValueError):
            vrl.unpack_file(f[:2] + b"XX" + f[4:])
        with self.assertRaises(ValueError):
            vrl.unpack_file(f[:4] + b"\x02" + f[5:])
        with self.assertRaises(ValueError):
            vrl.unpack_file(f[:5] + b"\x06" + f[6:])


STAT_X = [7] * 300 + [1, 2, 3] + [None] * 10 + [4]


class Stats(unittest.TestCase):
    def test_stats(self):
        s = vrl.encode(STAT_X)
        self.assertEqual(s, hx("FF 2C 01 07 03 01 02 03 FE 0A 00 01 04 00"))
        st = vrl.stats(s, T=15)
        self.assertEqual(st["tokens"], {"lrun": 1, "lit": 2, "skip": 1, "run": 0})
        self.assertEqual((st["written"], st["skipped"], st["unpacked"]), (304, 10, 314))
        self.assertEqual(st["cycles"], 4 * 30 + (200 + 300 + 30) + 3 * 22 + 80 + 22)
        self.assertEqual(st["cycles"], 818)
        self.assertEqual(st["bytes_by_kind"], {"lit": 4, "run_cpu": 0, "run_fill": 300})
        self.assertEqual(st["run_hist"]["> 256"], 1)
        self.assertEqual(st["stream_bytes"], len(s))
        self.assertAlmostEqual(st["ratio"], len(s) / 304)

    def test_threshold(self):
        # 200 + L < 10 + 15 L  <=>  L >= 14 (die Spec nennt 15; Rechenfehler dort)
        self.assertEqual(vrl.threshold({}), 14)
        self.assertEqual(vrl.stats(vrl.encode([5] * 14))["bytes_by_kind"]["run_fill"], 14)
        self.assertEqual(vrl.stats(vrl.encode([5] * 13))["bytes_by_kind"]["run_cpu"], 13)
        self.assertEqual(vrl.threshold({"fill_byte": 15}), 65537)
        self.assertEqual(vrl.threshold({"fill_byte": 20}), 65537)

    def test_load_bench(self):
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, "b.json")
            with open(p, "w") as f:
                json.dump({"engine": {"tok": 40, "T": 9, "unbekannt": 1}}, f)
            b = vrl.load_bench(p)
            self.assertEqual(b["tok"], 40)
            self.assertEqual(b["T"], 9)
            self.assertEqual(b["cpu_byte"], vrl.PLACEHOLDER["cpu_byte"])
            self.assertNotIn("unbekannt", b)
            self.assertEqual(vrl.stats(vrl.encode([5] * 12), params=b)["T"], 9)


class Cli(unittest.TestCase):
    def run_cli(self, *args):
        return subprocess.run([sys.executable, "-I", VRL_PY, *args], capture_output=True)

    def test_roundtrip(self):
        rng = random.Random(5)
        raw = bytes(rng.choice([0, 0, 0, 1, 2]) for _ in range(3000))
        with tempfile.TemporaryDirectory() as d:
            a, b, c = (os.path.join(d, n) for n in ("a.bin", "a.vrl", "c.bin"))
            with open(a, "wb") as f:
                f.write(raw)
            self.assertEqual(self.run_cli("encode", a, b, "--load", "$3000").returncode, 0)
            with open(b, "rb") as f:
                self.assertEqual(f.read()[:4], hx("00 30 56 52"))
            self.assertEqual(self.run_cli("decode", b, c).returncode, 0)
            with open(c, "rb") as f:
                self.assertEqual(f.read(), raw)
            # raw + in-prg + out-prg
            with open(a, "wb") as f:
                f.write(b"\x00\x40" + raw)
            self.assertEqual(self.run_cli("encode", a, b, "--raw", "--in-prg").returncode, 0)
            self.assertEqual(self.run_cli("decode", b, c, "--raw", "--out-prg", "0x4000").returncode, 0)
            with open(c, "rb") as f:
                self.assertEqual(f.read(), b"\x00\x40" + raw)
            r = self.run_cli("stats", b, "--raw", "--json")
            self.assertEqual(r.returncode, 0)
            self.assertEqual(json.loads(r.stdout)["unpacked"], len(raw))
            self.assertEqual(self.run_cli("stats", b, "--raw").returncode, 0)

    def test_rect_and_base(self):
        with tempfile.TemporaryDirectory() as d:
            a, m, o, base, out = (os.path.join(d, n) for n in ("a", "m", "o", "base", "out"))
            wr(a, bytes([1, 2, 3, 4]))
            wr(m, bytes([1, 0, 0, 1]))
            wr(base, bytes([9] * 12))
            r = self.run_cli("rect", a, o, "--width", "2", "--height", "2", "--stride", "10",
                             "--mask", m, "--raw")
            self.assertEqual(r.returncode, 0)
            self.assertEqual(rd(o), hx("01 01 FE 0A 00 01 04 00"))
            self.assertEqual(self.run_cli("decode", o, out, "--raw", "--base", base).returncode, 0)
            self.assertEqual(rd(out), bytes([1] + [9] * 10 + [4]))

    def test_format_error(self):
        with tempfile.TemporaryDirectory() as d:
            a = os.path.join(d, "bad")
            wr(a, b"\x03\x01")
            r = self.run_cli("decode", a, os.path.join(d, "o"), "--raw")
            self.assertEqual(r.returncode, 1)
            self.assertNotIn(b"Traceback", r.stderr)
            self.assertTrue(r.stderr)
            r = self.run_cli("decode", a, os.path.join(d, "o"))
            self.assertEqual(r.returncode, 1)
            self.assertNotIn(b"Traceback", r.stderr)
            self.assertEqual(self.run_cli("encode").returncode, 2)


if __name__ == "__main__":
    unittest.main()
