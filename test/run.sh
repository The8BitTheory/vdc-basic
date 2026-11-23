#!/bin/sh
# Run BASIC test listings in x128 (headless, warp) and report their exit codes.
# usage: test/run.sh [-s shot.png] file.bas ...
# A listing ends itself with  BANK 15:POKE DEC("D7FF"),code  (0 = pass).
# The cycle limit is only a guard against hangs (exit code 1).
HERE=$(cd "$(dirname "$0")" && pwd)
BIN=${VDCBASIC:-$HERE/../vdcbasic2g.bin}
SHOT=
if [ "$1" = "-s" ]; then SHOT=$2; shift 2; fi
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cp "$BIN" "$WORK/vdcbasic2g"
c1541 -format "test,01" d64 "$WORK/t.d64" -write "$WORK/vdcbasic2g" vdcbasic2g >/dev/null || exit 2
fail=0
for f in "$@"; do
    python3 "$HERE/bastok.py" "$f" "$WORK/t.prg" || exit 2
    x128 -default -warp -sounddev dummy -debugcart -80col -VDC64KB \
        +drive8truedrive -trapdevice8 -8 "$WORK/t.d64" -autostartprgmode 1 -autostart "$WORK/t.prg" \
        -limitcycles ${LIMIT:-200000000} ${SHOT:+-exitscreenshot "$SHOT"} >"$WORK/log.txt" 2>&1
    rc=$?
    if [ $rc -eq 0 ]; then echo "PASS  $f"; else echo "FAIL  $f (exit $rc)"; fail=1; fi
done
exit $fail
