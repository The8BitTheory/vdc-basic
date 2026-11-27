#!/usr/bin/env python3
"""Build test/hw/vdc-hw-tests.d81: all test programs that still have to run on a real C128.

usage: test/hw/mkhwdisk.py [OUT.d81]

Contents (see test/hw/README.md):
  MENU          menu, starts the others with RUN
  LOADER-HW     bench (docs/v3/P0-bench.md 11), needs VDCBASIC2H and VDCBENCH, writes BENCHRES
  LOADER, W     wait experiment (docs/v3/P2g-wait-experiment.md 6), writes RES0..RES5
  BOOT-R30      VDC-BASIC v3 + vdc-reg30-zero (docs/v3/P2b-rxv.md 3.3)
  BOOT-RXV      VDC-BASIC v3 + rxv-hw (docs/v3/P2b-rxv.md 3.4)

Sources: build/bench/bench.d64 (made by test/bench/run_bench.py --short) and
test/bench/waits/waits-hw.d64 (checked in); vdcbasic3.bin, boot/boot.bas, test/*.bas from the repo.
"""
import os, re, subprocess, sys, tempfile

ROOT = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(ROOT, "test", "hw", "vdc-hw-tests.d81")
P = lambda *a: os.path.join(ROOT, *a)
BENCH_D64 = P("build", "bench", "bench.d64")
WAITS_D64 = P("test", "bench", "waits", "waits-hw.d64")

MENU = """10 SCNCLR:PRINT"VDC-BASIC HARDWARE TESTS (C128, 80 COLUMNS, 64K VDC)"
20 PRINT:PRINT" 1  BENCH (LOADER-HW), ABOUT 1 MIN, WRITES BENCHRES"
30 PRINT" 2  WAIT EXPERIMENT (LOADER), ABOUT 10 MIN, WRITES RES0-RES5"
40 PRINT" 3  REG 30 = 0 FILLS 256 BYTES (VDC-REG30-ZERO)"
50 PRINT" 4  RXV VECTORS, R18/R19 AFTER FILL (RXV-HW)"
60 PRINT:PRINT"1-4 (OTHER KEY = QUIT)";
70 GETKEY K$:K=ASC(K$)-48:IF K<1 OR K>4 THEN PRINT:END
80 ON K GOTO 100,110,120,130
100 RUN"LOADER-HW"
110 RUN"LOADER"
120 RUN"BOOT-R30"
130 RUN"BOOT-RXV"
"""


def run(cmd):
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def main():
    if not os.path.exists(BENCH_D64):
        sys.exit("missing %s (run test/bench/run_bench.py --short first)" % BENCH_D64)
    with tempfile.TemporaryDirectory() as d:
        w = lambda n: os.path.join(d, n)
        for src, names in ((BENCH_D64, ("vdcbasic2h", "vdcbench", "loader-hw")), (WAITS_D64, ("loader", "w"))):
            for n in names:
                run(["c1541", "-attach", src, "-read", n, w(n + ".prg")])
        open(w("menu.bas"), "w").write(MENU)
        run([sys.executable, P("test", "bastok.py"), w("menu.bas"), w("menu.prg")])
        boot = open(P("boot", "boot.bas")).read()
        files = [("menu", w("menu.prg"))]
        for tag, prog in (("r30", "vdc-reg30-zero"), ("rxv", "rxv-hw")):
            b = re.sub(r'(?m)^10 (.*)RUN"BOOT"', r'10 \1RUN"BOOT-%s"' % tag.upper(), boot)
            b = re.sub(r"(?m)^50 .*", '50 RUN"%s"' % tag.upper(), b)
            open(w("boot-%s.bas" % tag), "w").write(b)
            run([sys.executable, P("test", "bastok.py"), w("boot-%s.bas" % tag), w("boot-%s.prg" % tag)])
            run([sys.executable, P("test", "bastok.py"), "--start", "0x4001", P("test", prog + ".bas"), w(tag + ".prg")])
            files += [("boot-" + tag, w("boot-%s.prg" % tag)), (tag, w(tag + ".prg"))]
        files += [("loader-hw", w("loader-hw.prg")), ("vdcbasic2h", w("vdcbasic2h.prg")), ("vdcbench", w("vdcbench.prg")),
                  ("loader", w("loader.prg")), ("w", w("w.prg")), ("vdcbasic3", P("vdcbasic3.bin"))]
        if os.path.exists(OUT):
            os.remove(OUT)
        cmd = ["c1541", "-format", "vdc hw tests,hw", "d81", OUT]
        for n, f in files:
            cmd += ["-write", f, n]
        run(cmd)
    print(OUT)


main()
