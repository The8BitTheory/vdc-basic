#!/bin/sh
# Build vertrot.d81: BOOT (starts VERTROT3C), VDCBASIC3, VTCLIP, the listings tokenised for $4001
# (VERTROT, VERTROT2, VERTROT3) and compiled with Blitz! improved runtime (VERTROTC, VERTROT2C, VERTROT3C).
# The .prg files are written next to the sources as well.
set -e
H=$(cd "$(dirname "$0")" && pwd); R=$H/../..; W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
NODE=${NODE:-node}; BLITZC=${BLITZC:-$HOME/git/blitz-compiler/blitz128/blitzc/js/cli.js}
sed 's/^50 .*/50 RUN"VERTROT3C"/' "$R/boot/boot.bas" > "$W/boot.bas"
python3 "$R/test/bastok.py" "$W/boot.bas" "$H/boot.prg"
cl65 -t none -C "$H/vtclip.cfg" -o "$H/vtclip.prg" "$H/vtclip.s"; rm -f "$H/vtclip.o"
set -- -write "$H/boot.prg" boot -write "$R/vdcbasic3.bin" vdcbasic3 -write "$H/vtclip.prg" vtclip
for p in vertrot vertrot2 vertrot3; do
    python3 "$R/test/bastok.py" --start 0x4001 "$H/$p.bas" "$H/$p.prg"
    python3 "$R/test/bastok.py" "$H/$p.bas" "$W/src.prg"
    "$NODE" "$BLITZC" "$W/src.prg" "$W/b" --name "${p}c" --runtime improved >/dev/null
    grep -q "errors: 0" "$W/b/screen.txt"; cp "$W/b/c.prg" "$H/${p}c.prg"; rm -rf "$W/b"
    set -- "$@" -write "$H/$p.prg" "$p" -write "$H/${p}c.prg" "${p}c"
done
rm -f "$H/vertrot.d81"
c1541 -format "vertrot,01" d81 "$H/vertrot.d81" "$@" -dir
