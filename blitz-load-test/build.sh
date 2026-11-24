#!/bin/sh
# Builds the test disk t.d64 with two Blitz!128 compiles:
#   a = a.bas (no graphics)       -> C/ file with load address $1C01
#   b = b.bas (GRAPHIC1,1:GRAPHIC0) -> C/ file with load address $4001
# Needs: petcat and c1541 (VICE), node, and the Blitz!128 JS compiler from
#   https://github.com/the8bittheory/blitz-compiler   (path in BLITZC, default ~/git/blitz-compiler/blitz128/blitzc)
# t.d64 is checked in, so this is only needed to rebuild it.
set -e
cd "$(dirname "$0")"
BLITZC=${BLITZC:-$HOME/git/blitz-compiler/blitz128/blitzc}
OUT=$(mktemp -d)
trap 'rm -rf "$OUT"' EXIT
for p in a b; do
    petcat -w70 -o "$OUT/$p.prg" "$p.bas"
    node "$BLITZC/js/cli.js" "$OUT/$p.prg" "$OUT/$p/" | tail -1
done
c1541 -format t,01 d64 t.d64 -write "$OUT/a/c.prg" a -write "$OUT/b/c.prg" b >/dev/null
for p in a b; do printf '%s: load address $%s\n' "$p" "$(head -c2 "$OUT/$p/c.prg" | xxd -p | sed 's/\(..\)\(..\)/\2\1/')"; done
