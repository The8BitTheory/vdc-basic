#!/bin/sh
# Build a disk image with VDC-BASIC, all listings of the guide and their data files.
# usage: docs/listings/mkdisk.sh [output.d64]      (needs c1541 from VICE and Python 3)
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=${1:-vdcbasic-guide.d64}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
c1541 -format "vdc-basic guide,2h" d64 "$OUT" >/dev/null || exit 1
cp "$HERE/../../vdcbasic2h.bin" "$WORK/vdcbasic2h"
c1541 "$OUT" -write "$WORK/vdcbasic2h" vdcbasic2h >/dev/null || exit 1
for f in "$HERE"/*.bas; do
    n=$(basename "$f" .bas)
    python3 "$HERE/../../test/bastok.py" "$f" "$WORK/$n" || exit 1
    c1541 "$OUT" -write "$WORK/$n" "$n" >/dev/null || exit 1
done
for f in "$HERE"/data/*; do
    c1541 "$OUT" -write "$f" "$(basename "$f")" >/dev/null || exit 1
done
c1541 "$OUT" -list
