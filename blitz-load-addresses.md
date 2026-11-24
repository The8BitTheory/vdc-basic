# Blitz!128 programs: load address, `LOAD ,8` vs. `LOAD ,8,1`

Programs compiled with Blitz! for the C128 come in two variants with different load addresses. This note
explains why, and what `LOAD "x",8` and `LOAD "x",8,1` do with each of them. All values below were measured in VICE
(x128 3.10) with two real Blitz compiles. Program A has no graphics commands (header `$1C01`, 10,960 bytes).
Program B contains `GRAPHIC1,1:GRAPHIC0` (header `$4001`, 11,369 bytes).

## Why some compiled programs start at `$4001`

- On the C128, `GRAPHIC 1-4` reserves the 9 KB hi-res area (`$1C00-$3FFF`) and moves the start of BASIC from
  `$1C01` to `$4001`.
- The BASIC interpreter can do that at run time: it moves the program text up and relinks the lines, because
  BASIC text can be moved.
- A compiled program cannot be moved that way. The runtime code, dispatch tables, headers and the jump targets in
  the bytecode are all at absolute addresses. So Blitz looks for GRAPHIC commands at compile time. If it finds one,
  it writes runtime image 2 (linked for `$4001`, header at `$6C41`) into the `C/` file instead of image 1
  (`$1C01-$46AF`). Image 2 is not just image 1 moved up: it has about 400 bytes of extra code.
- So the load address in the file is the address the program is linked for.
- The directive `REM ** NG` (no graphic array) keeps the program at `$1C01` despite GRAPHIC commands and does not
  reserve the hi-res area. The manual says you only need it for `GRAPHIC 5` or for very long programs (more than
  220 blocks).

## The self-moving loader stub

Both variants start with the same BASIC line:

```
POKE6,PEEK(981):BANK0:SYS PEEK(46)*256+47 : <HOME/ESC-@/CR> "BLITZ!"
```

`SYS PEEK(46)*256+47` jumps to the loader relative to the current BASIC start (`$2E` is the high byte of the BASIC
start). The loader compares that high byte with the address the program is linked for (`$1C` or `$40`). If they
differ, it copies the whole program page by page to the right place and sets the BASIC start to match. After that
it jumps into the runtime.

## What happens on LOAD and RUN

"Graphics off" means BASIC starts at `$1C01` (the normal state). "Graphics reserved" means BASIC starts at `$4001`
(after `GRAPHIC 1-4`).

| | **header `$1C01`** (A) | **header `$4001`** (B) |
|---|---|---|
| **`LOAD ,8`, graphics off** | loaded to `$1C01-$46CE`<br>BASIC start: `$1C01`<br>RUN: runs, nothing is copied | loaded to `$1C01-$4867`<br>BASIC start: `$1C01`<br>RUN: the loader copies the program up to `$4001`, sets the BASIC start to `$4001`, then runs it |
| **`LOAD ,8`, graphics reserved** | loaded to `$4001-$6ACE`<br>BASIC start: `$4001`<br>RUN: the loader copies the program down to `$1C01`, sets the BASIC start to `$1C01`, then runs it. The graphics area is overwritten after that. | loaded to `$4001-$6C67`<br>BASIC start: `$4001`<br>RUN: runs, nothing is copied |
| **`LOAD ,8,1`, graphics off** | same as `,8` (the header matches the BASIC start) | loaded to `$4001-$6C67`<br>BASIC start stays `$1C01`<br>RUN: runs the empty program at `$1C01`, READY at once, no error |
| **`LOAD ,8,1`, graphics reserved** | loaded to `$1C01-$46CE`, into the graphics area and past it<br>BASIC start stays `$4001`<br>**the machine hangs right after the READY that follows LOAD** | same as `,8` (the header matches the BASIC start) |

## More details

- **`,8` loads to the current BASIC start (`$2D/$2E`), and `,8,1` loads to the address in the header.** LOAD itself
  never changes the BASIC start. Only the Blitz loader does that, when the program is RUN.
- **In every case, LOAD sets the end of the program (`$1210/$1211`) to the end of the load (`$AE/$AF`).** It does so
  even with `,8,1` when that end does not fit the BASIC start. In "B, `,8,1`, graphics off", BASIC therefore treats
  `$1C01-$6C67` as program text.
- **After LOAD, BASIC relinks the lines from the BASIC start.** With `,8` and graphics reserved, the link of the stub
  line changes from `2D 1C` to `2D 40`, so the stub runs at either address.
- **The hang in "A, `,8,1`, graphics reserved":** after READY the machine takes no more input. I did not trace the
  cause. The likely reason is that A's runtime code now sits at `$4001` (`20 78 21 ...`), and relinking from `$4001`
  reads that code as line links.
- **Result:** `,8` works in all four cases, because the loader in the stub fixes the position itself. `,8,1` only
  works when the header address happens to equal the current BASIC start.
- **Detecting a compiled program at run time:** a check for the text "BLITZ!" in the stub, for example
  `PEEK($1C25+L)`, only works for the `$1C01` variant. A `$4001` program is at `$4001` once it runs, so the text is
  at `$4026`. Use `PEEK(46)*256+37+L` to cover both variants.

## Test setup

Everything is in [`blitz-load-test/`](blitz-load-test/):

| file | purpose |
|---|---|
| `a.bas`, `b.bas` | test programs (A prints `A-OK`; B runs `GRAPHIC1,1:GRAPHIC0` and prints `B-OK`) |
| `t.d64` | test disk with the compiled programs `a` (load address `$1C01`) and `b` (`$4001`) |
| `build.sh` | rebuilds `t.d64`. Needs `petcat`/`c1541` (VICE), `node` and the Blitz!128 JS compiler from github.com/the8bittheory/blitz-compiler (`blitz128/blitzc`, path in `BLITZC`) |
| `run.py` | runs the eight cases in VICE x128 and prints the pointers, memory and screen per step |
| `result.txt` | output of `run.py` for the checked-in `t.d64` (the source of the table above) |

```sh
cd blitz-load-test
./run.py              # all eight cases, about 25 s
./run.py g_a_81       # one case: g_ = GRAPHIC1:GRAPHIC0 first, a/b = program, 8/81 = LOAD ,8 / ,8,1
./build.sh            # only to rebuild t.d64
```

How `run.py` works: each case boots a fresh x128 (`-default -warp`) with `t.d64` in drive 8. After every step
(initial state, LOAD, RUN), the key buffer types a `POKE 2816,n`. A `watch store 0b00` (set with `-moncommands`) stops
the machine at that point, so there are no fixed waits. Over the remote monitor, the script then reads `$2D/$2E`,
`$1210/$1211`, `$AE/$AF`, `$1C01..` and `$4001..` in bank `ram`, plus the 40-column screen. The next command goes in
with the monitor command `keybuf`, which takes the text as is (no quotes around it). The only timeout is the cycle
limit (200 million cycles). A case that reaches it is reported as `HANG`.

Each compile comes out a few bytes different in length, so after `build.sh` the end addresses can differ slightly
from the table.
