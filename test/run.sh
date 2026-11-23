#!/bin/sh
# Run BASIC listings in x128 (headless, warp) and report their exit codes.
#
# usage: test/run.sh [-l] [-d dir] file.bas ...
#   -l      book listing: the file has no test code of its own. It is wrapped so that
#           reaching its end reports 0 and a BASIC error n reports 200 + n.
#   -d dir  save a screenshot of the 80-column screen at exit as dir/<name>.png
#
# A test listing ends itself with  BANK 15:POKE DEC("D7FF"),code  (0 = pass).
# Nothing here waits for a fixed time; the cycle limit is only a guard against hangs.
HERE=$(cd "$(dirname "$0")" && pwd)
BIN=${VDCBASIC:-$HERE/../vdcbasic2h.bin}
WRAP= ; SHOTDIR=
while getopts ld: o; do
    case $o in l) WRAP=1 ;; d) SHOTDIR=$OPTARG ;; *) exit 2 ;; esac
done
shift $((OPTIND - 1))
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cp "$BIN" "$WORK/vdcbasic2h"
c1541 -format "test,01" d64 "$WORK/t.d64" -write "$WORK/vdcbasic2h" vdcbasic2h >/dev/null || exit 2
fail=0
for f in "$@"; do
    src=$f
    if [ -n "$WRAP" ]; then
        src=$WORK/wrapped.bas
        {
            echo '0 trap 63998'
            cat "$f"
            echo '63996 syn:syn'
            echo '63997 bank 15:poke dec("d7ff"),0:end'
            echo '63998 bank 15:poke dec("d7ff"),200+er'
        } >"$src"
    fi
    python3 "$HERE/bastok.py" "$src" "$WORK/t.prg" || exit 2
    shot=
    [ -n "$SHOTDIR" ] && shot="$SHOTDIR/$(basename "$f" .bas).png"
    x128 -default -warp -sounddev dummy -debugcart -80col -VDC64KB \
        +drive8truedrive -trapdevice8 -8 "$WORK/t.d64" -autostartprgmode 1 -autostart "$WORK/t.prg" \
        -limitcycles ${LIMIT:-200000000} ${shot:+-exitscreenshot "$shot"} >"$WORK/log.txt" 2>&1
    rc=$?
    if [ $rc -eq 0 ]; then echo "PASS  $f"; else echo "FAIL  $f (exit $rc)"; fail=1; fi
done
exit $fail
