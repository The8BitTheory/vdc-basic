# Tests

Each `.bas` file is a BASIC 7.0 listing that checks one behaviour, mostly by reading VRAM back with `VMD`, and reports the result as an exit code through the VICE debug cartridge (`POKE $D7FF`; 0 = pass, 199 = VDC-BASIC v3 not installed, 200 + n = BASIC error n).

```
test/run.sh test/*.bas                           # behaviour tests, interpreted
test/run.sh -c test/*.bas                        # the same tests compiled with Blitz! 128 (runtime image 2)
test/run.sh -l -d docs/img docs/listings/*.bas   # the listings of the guide, with screenshots
test/boot_sector.py                              # BOOT via the C128 boot sector and via -autostart disk.d64
test/e1_blitz.py                                 # experiment E1 (docs/v3/P2a-relocate.md, 5.2 and 9)
```

Needs `x128` and `c1541` from VICE and Python 3. `bastok.py` tokenises the listings using the keyword table in `BASIC V7.0 VDC.txt` (`--start 0x4001` for programs that run with v3). The runs are headless and in warp mode; the cycle limit is only a guard against hangs.

**v3 runs** (default and `-c`): the disk holds `BOOT` (`boot/boot.bas` with line 50 = `RUN"T"`), `VDCBASIC3` and the test program `T`. The emulator autostarts `BOOT`, which moves the BASIC text to `$4001`, installs VDC-BASIC at `$1C00` and runs `T`. Line 2 of every test checks the version byte at `$1C03`. Lists that must lie below `$4000` use `$1300`, lists above `$4000` use `$A000`.

**`-c`**: `blitzprep.py` writes `::` in front of every VDC-BASIC instruction, moves `VMD(…)`/`RGD(…)` calls in front of their statement into helper variables `Q0`…`Q9` (`::Q0=VMD(…)`), and adds `63999 GRAPHIC 0` (never executed; makes the compiler use runtime image 2 at `$4001`). Then blitzc compiles it (`BLITZC`, default `~/git/blitz-compiler/blitz128/blitzc/js/cli.js`; node from `NODE`, `PATH` or `~/.nvm`). A compiled program that does not load at `$4001` fails. Without node or blitzc: `SKIP (blitzc not found)`, exit code 3. `-l` and `-c` exclude each other. Rules for test listings that should run in both modes: no VDC-BASIC instruction directly after `THEN`/`ELSE` (unless the listing writes `THEN::` itself), no `VMD`/`RGD` in the `THEN` part of a statement, no variables `Q0`…`Q9`.

Known limit of compiled programs: after an error inside an extension has been caught by `TRAP`, later extensions no longer work correctly (Blitz! runtime, independent of VDC-BASIC). `chapter-claims.bas` therefore checks the syntax error from `SYNC=1` last.

**`-l`**: until the guide is moved to v3, its listings run with v2h (`vdcbasic2h.bin` at `$1300`, BASIC at `$1C01`, no `BOOT`), as before.

**Monitor scripts** (`boot_sector.py`, `e1_blitz.py`, module `vicemon.py`): VICE remote monitor on `127.0.0.1`, port from `--port`, else `DBE_VICE_PORT`, else 6631 (range 6631–6649, checked with a bind test). They stop only their own emulator process and wait for breakpoints, never for a fixed time.

To rebuild the binary: `ACME=<path to ACME_Lib> acme -l build/vdcbasic3.sym src/vdcbasic.asm` in the repository root.
