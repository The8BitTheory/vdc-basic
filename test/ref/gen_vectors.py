#!/usr/bin/env python3
"""Testvektoren-Generator fuer die 6502-Implementierung (P2c, Abschnitt 6).

Aufruf:  python3 test/ref/gen_vectors.py [-o vectors.json] [--group S1,L,...] [--quick] [--list]

Der Harness importiert build() direkt; die JSON-Datei ist optional (nicht einchecken).

Ausgabeformat (JSON, UTF-8):
{
  "format": "vdcdraw-vectors-1",
  "init":   {"seed": 13, "size": 65536, "sha1": "<sha1 des Grundmusters>"},
             # Grundmuster: r = random.Random(13); bytes(r.getrandbits(8) for _ in range(65536))
             # (siehe base_pattern()); der Harness laedt es vor jedem Fall ins VRAM.
  "cases": [
    {"name": "S1-x1-w5-c7-T14",
     "group": "S1",
     "T": 14,                              # Wert fuer POKE $1C04
     "cmds": [["VDM",1,0,80], ["VDB",1,3,5,1,7]],   # in dieser Reihenfolge ausfuehren
     "diff": [[addr, "hexbytes"], ...],    # Soll-Bytes aller gegenueber dem Grundmuster
                                           # geaenderten Bereiche (zusammenhaengende Laeufe)
     "sha1": "<sha1 ueber alle 64 KB nach den Befehlen>"}
  ]
}
Pruefen: VRAM = Grundmuster, diff anwenden => muss dem VRAM-Dump nach den Befehlen entsprechen
(sha1 als Kurzpruefung, diff fuer die Fehlerausgabe "erste Adresse, Ist/Soll").
"""
import argparse
import hashlib
import json
import math
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vdcdraw import Vdc  # noqa: E402

SEED = 13


def base_pattern(seed=SEED):
    r = random.Random(seed)
    return bytes(r.getrandbits(8) for _ in range(65536))


def diff_runs(a, b):
    """Zusammenhaengende Laeufe, in denen b von a abweicht: [[addr, hex], ...]."""
    out, i = [], 0
    while i < 65536:
        if a[i] != b[i]:
            j = i
            while j < 65536 and a[j] != b[j]:
                j += 1
            out.append([i, bytes(b[i:j]).hex()])
            i = j
        else:
            i += 1
    return out


def case(group, name, T, cmds):
    return {"group": group, "name": "%s-%s" % (group, name), "T": T, "cmds": cmds}


def gen_cases():
    cs = []
    m0, m1 = ["VDM", 0, 0, 80], ["VDM", 1, 0, 80]
    # S1 Spannen chunky
    for x in range(4):
        for w in range(1, 13):
            for c in (0, 7, 15):
                cs.append(case("S1", "x%d-w%d-c%d" % (x, w, c), 14, [m1, ["VDB", x, 2, w, 1, c]]))
    for T in (0, 2, 14, 255):
        for m in (T - 1, T, T + 1):
            if m < 0:
                continue
            for x in (0, 1):
                # genau m volle Mittelbytes: x=0 ohne Raender, x=1 mit beiden Rand-Nibbles
                w = 2 * m if x == 0 else 2 * m + 2
                if w == 0:
                    continue
                cs.append(case("S1", "mid%d-x%d-T%d" % (m, x, T), T, [m1, ["VDB", x, 5, w, 1, 9]]))
    cs.append(case("S1", "full", 14, [m1, ["VDB", 0, 7, 160, 1, 5]]))
    # S0 Spannen Bitmap
    for x in range(8):
        for w in range(1, 25):
            for c in (0, 1, 2):
                cs.append(case("S0", "x%d-w%d-c%d" % (x, w, c), 14, [m0, ["VDB", x, 2, w, 1, c]]))
    for T in (0, 2, 14, 255):
        for m in (T - 1, T, T + 1):
            if m < 0:
                continue
            for x in (0, 3):
                for c in (0, 1, 2):
                    # genau m volle Mittelbytes: x=0 ohne Raender, x=3 mit Rand links (5 Pixel) und rechts (3 Pixel)
                    w = 8 * m if x == 0 else 8 * m + 8
                    if w == 0:
                        continue
                    cs.append(case("S0", "mid%d-x%d-c%d-T%d" % (m, x, c, T), T, [m0, ["VDB", x, 5, w, 1, c]]))
    for c in (0, 1, 2):
        cs.append(case("S0", "full-c%d" % c, 14, [m0, ["VDB", 0, 7, 640, 1, c]]))
    # V senkrecht
    for mode, mc in ((0, m0), (1, m1)):
        cols = range(8) if mode == 0 else range(4)
        cc = (0, 1, 2) if mode == 0 else (3, 12)
        for x in cols:
            for n in (1, 2, 100):
                for c in cc:
                    for rev in (False, True):
                        y0, y1 = 10, 10 + n - 1
                        a = (x, y1, x, y0) if rev else (x, y0, x, y1)
                        cs.append(case("V", "m%d-x%d-n%d-c%d-%s" % (mode, x, n, c, "r" if rev else "f"),
                                       14, [mc, ["VDL", a[0], a[1], a[2], a[3], c]]))
    # L Linien
    for mode, mc, cx, cy, cc in ((0, m0, 320, 100, 1), (1, m1, 80, 50, 6)):
        for k in range(32):
            ang = 2 * math.pi * k / 32
            x2 = cx + round(37 * math.cos(ang))
            y2 = cy + round(37 * math.sin(ang))
            cs.append(case("L", "m%d-circ%d" % (mode, k), 14, [mc, ["VDL", cx, cy, x2, y2, cc]]))
            cs.append(case("L", "m%d-circ%d-rev" % (mode, k), 14, [mc, ["VDL", x2, y2, cx, cy, cc]]))
        special = [(10, 10, 40, 40), (10, 10, 41, 40), (10, 10, 39, 40), (10, 10, 40, 41),
                   (3, 5, 303, 6), (3, 5, 4, 155), (7, 7, 7, 7)]
        for i, (a, b, c_, d) in enumerate(special):
            cs.append(case("L", "m%d-sp%d" % (mode, i), 14, [mc, ["VDL", a, b, c_, d, cc]]))
            cs.append(case("L", "m%d-sp%d-rev" % (mode, i), 14, [mc, ["VDL", c_, d, a, b, cc]]))
        if mode == 0:
            cs.append(case("L", "m0-xor2", 14, [mc, ["VDL", 5, 5, 200, 60, 2], ["VDL", 5, 5, 200, 60, 2]]))
    # R VDR
    for mode, mc, c in ((0, m0, 1), (1, m1, 10)):
        for w in (0, 1, 2, 3, 10):
            for h in (0, 1, 2, 3, 10):
                for x in (4, 5):
                    cs.append(case("R", "m%d-w%d-h%d-x%d" % (mode, w, h, x), 14, [mc, ["VDR", x, 3, w, h, c]]))
    for w, h in ((2, 2), (3, 3), (10, 5)):
        cs.append(case("R", "xor-w%d-h%d" % (w, h), 14, [m0, ["VDR", 9, 3, w, h, 2]]))
    # B VDB
    for mode, mc, c in ((0, m0, 1), (1, m1, 11)):
        for x in range(4):
            for w in (1, 2, 5, 8, 17):
                cs.append(case("B", "m%d-x%d-w%d" % (mode, x, w), 14, [mc, ["VDB", x, 4, w, 6, c]]))
    for T in (0, 14, 255):
        cs.append(case("B", "chunky-70x96-T%d" % T, T, [m1, ["VDB", 10, 20, 70, 96, 4]]))
        cs.append(case("B", "bitmap-640x200-T%d" % T, T, [m0, ["VDM", 0, 0, 80], ["VDB", 0, 0, 640, 200, 2]]))
    cs.append(case("B", "w0", 14, [m0, ["VDB", 3, 3, 0, 5, 1]]))
    cs.append(case("B", "h0", 14, [m0, ["VDB", 3, 3, 5, 0, 1]]))
    # G Geometrie
    for stride in (160, 37, 255, 300):
        for mode in (0, 1):
            cs.append(case("G", "stride%d-m%d" % (stride, mode), 14,
                           [["VDM", mode, 100, stride], ["VDL", 2, 2, 60, 30, 1 if mode == 0 else 9],
                            ["VDR", 5, 5, 20, 9, 1 if mode == 0 else 3], ["VDB", 31, 12, 11, 4, 2 if mode == 0 else 12]]))
    cs.append(case("G", "base-odd", 14, [["VDM", 1, 4321, 80], ["VDB", 3, 3, 9, 5, 8]]))
    cs.append(case("G", "wrap-m0", 14, [["VDM", 0, 0xFFC0, 80], ["VDB", 0, 0, 100, 3, 1], ["VDL", 0, 0, 50, 40, 2]]))
    cs.append(case("G", "wrap-m1", 14, [["VDM", 1, 0xFFC0, 80], ["VDB", 1, 0, 80, 3, 5], ["VDL", 0, 0, 40, 30, 7]]))
    cs.append(case("G", "x-beyond-row", 14, [["VDM", 1, 0, 80], ["VDL", 150, 4, 200, 10, 7], ["VDB", 190, 6, 20, 3, 3]]))
    # Z Zufall
    r = random.Random(11)
    for i in range(200):
        mode = r.randint(0, 1)
        cmds = [["VDM", mode, r.choice((0, 0, 1000, 0xFFC0, 4321)), r.choice((80, 80, 160, 37, 255))]]
        cname = r.choice(("VDL", "VDR", "VDB"))
        big = r.random() < 0.25
        lim = 300 if mode == 0 else 150
        c = r.randint(0, 2) if mode == 0 else r.randint(0, 15)
        if cname == "VDL":
            cmds.append(["VDL", r.randint(0, lim), r.randint(0, 120 if not big else 300),
                         r.randint(0, lim), r.randint(0, 120 if not big else 300), c])
        else:
            w = r.randint(0, 30 if not big else 200)
            h = r.randint(0, 20 if not big else 80)
            cmds.append([cname, r.randint(0, lim), r.randint(0, 100), w, h, c])
        cs.append(case("Z", "%03d" % i, r.choice((0, 2, 14, 255)), cmds))
    return cs


def is_quick(cs):
    """Teilmenge fuer schnelle Laeufe (6.3): S0-Raster nur x in {0,3,7}, Z nur 000-049."""
    n = cs["name"]
    if cs["group"] == "S0" and n.startswith("S0-x"):
        return n[4] in "037"
    if cs["group"] == "Z":
        return int(n[2:]) < 50
    return True


def build(groups=None, quick=False):
    init = base_pattern()
    out = {"format": "vdcdraw-vectors-1",
           "init": {"seed": SEED, "size": 65536, "sha1": hashlib.sha1(init).hexdigest()},
           "cases": []}
    for cs in gen_cases():
        if groups and cs["group"] not in groups:
            continue
        if quick and not is_quick(cs):
            continue
        v = Vdc(init)
        for cmd in cs["cmds"]:
            v.run(cmd)
        cs["diff"] = diff_runs(init, v.ram)
        cs["sha1"] = hashlib.sha1(bytes(v.ram)).hexdigest()
        out["cases"].append(cs)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-o", "--output", default="-")
    ap.add_argument("--group", help="kommagetrennt: S1,S0,V,L,R,B,G,Z")
    ap.add_argument("--list", action="store_true")
    ap.add_argument("--quick", action="store_true", help="Teilmenge (6.3)")
    a = ap.parse_args()
    groups = set(a.group.split(",")) if a.group else None
    data = build(groups, a.quick)
    if a.list:
        for c in data["cases"]:
            print(c["name"])
        return
    txt = json.dumps(data, separators=(",", ":"))
    if a.output == "-":
        print(txt)
    else:
        with open(a.output, "w") as f:
            f.write(txt)


if __name__ == "__main__":
    main()
