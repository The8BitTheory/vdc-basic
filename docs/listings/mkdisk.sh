#!/bin/sh
# Build a disk image with BOOT, VDC-BASIC v3, all listings of the guide and their data files.
# The listings are tokenised for BASIC at $4001: RUN"BOOT" first, then RUN"NAME".
# usage: docs/listings/mkdisk.sh [output.d64]      (needs c1541 from VICE and Python 3)
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=${1:-vdcbasic-guide.d64}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
c1541 -format "vdc-basic guide,v3" d64 "$OUT" >/dev/null || exit 1
# BOOT first, so that -autostart disk.d64 finds it; then VDC-BASIC itself
python3 "$HERE/../../test/bastok.py" "$HERE/../../boot/boot.bas" "$WORK/boot" || exit 1
cp "$HERE/../../vdcbasic3.bin" "$WORK/vdcbasic3"
c1541 "$OUT" -write "$WORK/boot" boot -write "$WORK/vdcbasic3" vdcbasic3 >/dev/null || exit 1
for f in "$HERE"/*.bas; do
    n=$(basename "$f" .bas)
    python3 "$HERE/../../test/bastok.py" --start 0x4001 "$f" "$WORK/$n" || exit 1
    c1541 "$OUT" -write "$WORK/$n" "$n" >/dev/null || exit 1
done
for f in "$HERE"/data/*; do
    c1541 "$OUT" -write "$f" "$(basename "$f")" >/dev/null || exit 1
done
c1541 "$OUT" -list
