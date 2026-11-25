#!/usr/bin/env python3
"""Make a C128 boot sector (track 1, sector 0) that runs BOOT.

usage: mkboot.py OUT.bin [--name BOOT] [--message "VDC-BASIC V3"] [--disk DISK.d64]

Layout (C128 KERNAL, BOOT_CALL): "CBM", load address of further blocks (0), bank (0),
number of further blocks (0), message + 0, file name + 0 (empty: nothing is loaded),
then code, entered at $0B00 + offset:  LDX #<(cmd-1) : LDY #>(cmd-1) : JMP $AFA5  with
cmd = RUN"BOOT" + 0 (plain text) in the sector buffer. $AFA5 jumps to $4DCD, which sets
TXTPTR ($3D/$3E) to X/Y, tokenises the text there ($430A) and executes it; CHRGET increments
TXTPTR before reading, hence cmd-1. (Checked in VICE 3.10, test/boot_sector.py.)

--disk: write the sector into DISK.d64 with c1541 (after all files are on the disk); stops
if sector 1/0 is not empty (c1541 uses track 1 only when the disk is nearly full).
"""
import argparse, os, subprocess, sys, tempfile

ap = argparse.ArgumentParser()
ap.add_argument("out")
ap.add_argument("--name", default="BOOT")
ap.add_argument("--message", default="VDC-BASIC V3")
ap.add_argument("--disk")
a = ap.parse_args()

pet = lambda s: s.upper().encode("ascii")
sec = bytearray(b"CBM" + bytes([0, 0, 0, 0]))
sec += pet(a.message) + b"\0" + b"\0"     # message, empty file name
code_at = len(sec)
cmd_at = code_at + 7
cmd = 0x0B00 + cmd_at - 1
sec += bytes([0xA2, cmd & 255, 0xA0, cmd >> 8, 0x4C, 0xA5, 0xAF])
sec += pet('RUN"%s"' % a.name) + b"\0"
if len(sec) > 256:
    sys.exit("boot sector too long")
sec += bytes(256 - len(sec))
open(a.out, "wb").write(sec)

if a.disk:
    with tempfile.TemporaryDirectory() as d:
        old = os.path.join(d, "old.bin")
        subprocess.run(["c1541", a.disk, "-bread", old, "1", "0"], check=True, stdout=subprocess.DEVNULL)
        if any(open(old, "rb").read()):
            sys.exit(f"{a.disk}: track 1 sector 0 is not empty")
    subprocess.run(["c1541", a.disk, "-bwrite", a.out, "1", "0"], check=True, stdout=subprocess.DEVNULL)
