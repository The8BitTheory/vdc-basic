#!/bin/sh
# Run BASIC listings in x128 (headless, warp) and report their exit codes.
#
# usage: test/run.sh [-z] [-l | -c] [-d dir] file.bas ...
#
# Default: VDC-BASIC v3 (vdcbasic3.bin, $1C00). The disk holds BOOT (boot/boot.bas with line 50
# replaced by  50 RUN"T" ), VDCBASIC3 and the test program T (tokenised for $4001). The emulator
# autostarts BOOT, which moves the BASIC text to $4001, installs VDC-BASIC and runs T.
#   -c      compile T with Blitz! 128 (runtime image 2, $4001) first: test/blitzprep.py, then
#           blitzc (BLITZC, default ~/git/blitz-compiler/blitz128/blitzc/js/cli.js; node from NODE,
#           PATH or ~/.nvm). Without node or cli.js: "SKIP (blitzc not found)", exit code 3.
#   -l      book listing: the file has no test code of its own. It is wrapped so that
#           reaching its end reports 0 and a BASIC error n reports 200 + n.
#           A listing that waits for a key does so with  GETKEY K$ ; in the test the
#           first such statement ends the run instead, so the screenshot shows the
#           picture the reader sees while the program waits.
#           If a file NAME.keys exists next to NAME.bas, its characters are fed to
#           the GETKEY statements one by one, and the run ends when they are used up.
#           The files in docs/listings/data are on the disk.
#   -z      run in Z64K instead of VICE (test/z64k.py: own Xvfb, java, Z64K.jar in ~/.local/share/z64k or
#           Z64K_JAR). Same disk, same exit-code rule ($D7FF). Needs no VICE monitor; -d is ignored (Z64K
#           has no VDC screenshot). Z64K starts take about 8 s each (Java); ZTIMEOUT (default 600 s) is
#           the guard for one run. See test/README.md, section Z64K.
#   -d dir  save a screenshot of the 80-column screen at exit as dir/<name>.png,
#           doubled in height because VDC pixels are twice as high as wide (needs Pillow)
#
# A test listing ends itself with  BANK 15:POKE DEC("D7FF"),code  (0 = pass).
# Nothing here waits for a fixed time; the cycle limit is only a guard against hangs.
HERE=$(cd "$(dirname "$0")" && pwd)
WRAP= ; SHOTDIR= ; COMPILE= ; Z64K=
while getopts zlcd: o; do
    case $o in z) Z64K=1 ;; l) WRAP=1 ;; c) COMPILE=1 ;; d) SHOTDIR=$OPTARG ;; *) exit 2 ;; esac
done
shift $((OPTIND - 1))
[ -n "$WRAP" ] && [ -n "$COMPILE" ] && { echo "-l and -c exclude each other" >&2; exit 2; }
if [ -n "$COMPILE" ]; then
    BLITZC=${BLITZC:-$HOME/git/blitz-compiler/blitz128/blitzc/js/cli.js}
    if [ -z "$NODE" ]; then
        NODE=$(command -v node || ls -d "$HOME"/.nvm/versions/node/*/bin/node 2>/dev/null | sort -V | tail -1)
    fi
    if [ -z "$NODE" ] || [ ! -x "$NODE" ] || [ ! -f "$BLITZC" ]; then
        echo "SKIP (blitzc not found)"; exit 3
    fi
fi
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
BIN=${VDCBASIC:-$HERE/../vdcbasic3.bin}
cp "$BIN" "$WORK/vdcbasic3"
sed 's/^50 .*/50 RUN"T"/' "$HERE/../boot/boot.bas" >"$WORK/boot.bas"
python3 "$HERE/bastok.py" "$WORK/boot.bas" "$WORK/boot.prg" || exit 2
# BOOT first: -autostart disk.d64 would load the first file
c1541 -format "test,01" d64 "$WORK/base.d64" -write "$WORK/boot.prg" boot \
    -write "$WORK/vdcbasic3" vdcbasic3 >/dev/null || exit 2
if [ -n "$WRAP" ]; then
    for d in "$HERE"/../docs/listings/data/*; do
        [ -f "$d" ] && { c1541 "$WORK/base.d64" -write "$d" "$(basename "$d")" >/dev/null || exit 2; }
    done
fi
if [ -n "$Z64K" ]; then
    SHOTDIR=
    python3 "$HERE/z64k.py" --check || { echo "SKIP (z64k not available)"; exit 3; }
fi
fail=0
for f in "$@"; do
    src=$f
    if [ -n "$WRAP" ]; then
        src=$WORK/wrapped.bas
        {
            echo '0 trap 63998'
            keys=${f%.bas}.keys
            if [ -f "$keys" ]; then
                sed 's/GETKEY K\$/GOSUB 63990/' "$f"
                echo "63990 KQ=KQ+1:K\$=MID\$(\"$(cat "$keys")\",KQ,1):IF K\$<>\"\" THEN RETURN"
                echo '63991 SYN:SYN:BANK 15:POKE DEC("D7FF"),0'
            else
                sed 's/GETKEY K\$/SYN:SYN:BANK 15:POKE DEC("D7FF"),0/' "$f"
            fi
            echo '63996 syn:syn'
            echo '63997 bank 15:poke dec("d7ff"),0:end'
            echo '63998 bank 15:poke dec("d7ff"),200+er'
        } >"$src"
    fi
    cp "$WORK/base.d64" "$WORK/t.d64"
    start=$WORK/t.prg
    if [ -n "$COMPILE" ]; then
        rm -rf "$WORK/blitz"
        if ! python3 "$HERE/blitzprep.py" "$f" "$WORK/tb.bas"; then
            echo "FAIL  $f (blitzprep)"; fail=1; continue
        fi
        # source for the compiler at $1C01, like all references of blitzc
        python3 "$HERE/bastok.py" "$WORK/tb.bas" "$WORK/tb.prg" || exit 2
        "$NODE" "$BLITZC" "$WORK/tb.prg" "$WORK/blitz" --name T >"$WORK/blitzc.txt" 2>&1
        crc=$?
        if [ $crc -ne 0 ] || [ ! -f "$WORK/blitz/c.prg" ]; then
            echo "FAIL  $f (blitzc exit $crc)"; grep -i 'error' "$WORK/blitz/screen.txt" "$WORK/blitzc.txt" 2>/dev/null | sed "s/^/      /"
            fail=1; continue
        fi
        if [ "$(od -An -tx1 -N2 "$WORK/blitz/c.prg" | tr -d ' ')" != "0140" ]; then
            echo "FAIL  $f (not image 2)"; fail=1; continue
        fi
        c1541 "$WORK/t.d64" -write "$WORK/blitz/c.prg" t >/dev/null || exit 2
        start=$WORK/boot.prg
    else
        python3 "$HERE/bastok.py" --start 0x4001 "$src" "$WORK/t.prg" || exit 2
        c1541 "$WORK/t.d64" -write "$WORK/t.prg" t >/dev/null || exit 2
        start=$WORK/boot.prg
    fi
    shot=
    [ -n "$SHOTDIR" ] && shot="$SHOTDIR/$(basename "$f" .bas).png"
    if [ -n "$Z64K" ]; then
        python3 "$HERE/z64k.py" --prg "$start" --d64 "$WORK/t.d64" --limit ${LIMIT:-200000000} \
            --timeout ${ZTIMEOUT:-600} --log "$WORK/log.txt" >"$WORK/zout.txt" 2>&1
        rc=$?
        cat "$WORK/zout.txt" >>"$WORK/log.txt"
    else
    try=0
    while :; do
        x128 -default -warp -sounddev dummy -debugcart -80col -VDC64KB \
            +drive8truedrive -trapdevice8 -8 "$WORK/t.d64" -autostartprgmode 1 -autostart "$start" \
            -limitcycles ${LIMIT:-200000000} $X128OPTS ${shot:+-exitscreenshot "$shot"} >"$WORK/log.txt" 2>&1
        rc=$?
        # 143 = the emulator was killed from outside (e.g. another test run on this machine): try again
        try=$((try + 1))
        [ $rc -eq 143 ] && [ $try -lt 4 ] || break
    done
    fi
    # the VDC's pixels are twice as high as wide: double the height of the screenshot
    [ -n "$shot" ] && [ -f "$shot" ] && python3 -c 'import sys; from PIL import Image; i=Image.open(sys.argv[1]); i.resize((i.width, i.height*2), Image.NEAREST).save(sys.argv[1], optimize=True)' "$shot"
    if [ $rc -eq 0 ]; then echo "PASS  $f"; else echo "FAIL  $f (exit $rc)"; tail -4 "$WORK/log.txt" | sed "s/^/      /"; fail=1; fi
done
exit $fail
