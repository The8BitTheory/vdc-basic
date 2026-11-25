# Tests

Each `.bas` file is a BASIC 7.0 listing that checks one behaviour, mostly by reading VRAM back with `VMD`, and reports the result as an exit code through the VICE debug cartridge (`POKE $D7FF`; 0 = pass, 199 = VDC-BASIC v3 not installed, 200 + n = BASIC error n).

```
test/run.sh test/*.bas                           # behaviour tests, interpreted
test/run.sh -c test/*.bas                        # the same tests compiled with Blitz! 128 (runtime image 2)
test/run.sh -l -d docs/img docs/listings/*.bas   # the listings of the guide, with screenshots
test/boot_sector.py                              # BOOT via the C128 boot sector and via -autostart disk.d64
test/e1_blitz.py                                 # experiment E1 (docs/v3/P2a-relocate.md, 5.2 and 9)
test/rxv/run_rxv.py [-c] [--fast]                # RXV against vrl.decode, whole VRAM (docs/v3/P2b-rxv.md 7.1)
test/draw/run_draw.py [-c] [--quick]             # VDM/VDL/VDR/VDB against the reference model, whole VRAM (docs/v3/P2c-draw.md 6)
test/run.sh -z [-c] test/*.bas                  # the same tests in Z64K instead of VICE (see Z64K below)
test/rxv/run_rxv.py --emu z64k [-c]              # RXV in Z64K, no monitor (see Z64K below)
test/draw/run_draw.py --emu z64k --quick [-c]    # drawing commands in Z64K
python3 -m unittest discover -s test/ref         # self tests of the reference model test/ref/vdcdraw.py
```

Needs `x128` and `c1541` from VICE and Python 3. `bastok.py` tokenises the listings using the keyword table in `BASIC V7.0 VDC.txt` (`--start 0x4001` for programs that run with v3). The runs are headless and in warp mode; the cycle limit is only a guard against hangs.

**v3 runs** (default and `-c`): the disk holds `BOOT` (`boot/boot.bas` with line 50 = `RUN"T"`), `VDCBASIC3` and the test program `T`. The emulator autostarts `BOOT`, which moves the BASIC text to `$4001`, installs VDC-BASIC at `$1C00` and runs `T`. Line 2 of every test checks the version byte at `$1C03`. Lists that must lie below `$4000` use `$1300`, lists above `$4000` use `$A000`.

**`-c`**: `blitzprep.py` writes `::` in front of every VDC-BASIC instruction, moves `VMD(…)`/`RGD(…)` calls in front of their statement into helper variables `Q0`…`Q9` (`::Q0=VMD(…)`), and adds `63999 GRAPHIC 0` (never executed; makes the compiler use runtime image 2 at `$4001`). Then blitzc compiles it (`BLITZC`, default `~/git/blitz-compiler/blitz128/blitzc/js/cli.js`; node from `NODE`, `PATH` or `~/.nvm`). A compiled program that does not load at `$4001` fails. Without node or blitzc: `SKIP (blitzc not found)`, exit code 3. `-l` and `-c` exclude each other. Rules for test listings that should run in both modes: no VDC-BASIC instruction directly after `THEN`/`ELSE` (unless the listing writes `THEN::` itself), no `VMD`/`RGD` in the `THEN` part of a statement, no variables `Q0`…`Q9`.

Known limit of compiled programs: after an error inside an extension has been caught by `TRAP`, later extensions no longer work correctly (Blitz! runtime, independent of VDC-BASIC). `chapter-claims.bas` therefore checks the syntax error from `SYNC=1` last.

**`-l`**: until the guide is moved to v3, its listings run with v2h (`vdcbasic2h.bin` at `$1300`, BASIC at `$1C01`, no `BOOT`), as before.

**Monitor scripts** (`boot_sector.py`, `e1_blitz.py`, module `vicemon.py`): VICE remote monitor on `127.0.0.1`, port from `--port`, else `DBE_VICE_PORT`, else 6631 (range 6631–6649, checked with a bind test). They stop only their own emulator process and wait for breakpoints, never for a fixed time.

To rebuild the binary: `ACME=<path to ACME_Lib> acme -l build/vdcbasic3.sym src/vdcbasic.asm` in the repository root.

**RXV** (`docs/v3/P2b-rxv.md`): `test/rxv/run_rxv.py` checks `RXV` against `vrl.decode` from `tools/vrl.py` (the canonical copy in this repository; `DBE_TOOLS=<dir>` takes `vrl.py` from another directory, exit code 2 if it is missing). One VICE run for all vectors (groups B, R with T 0–255, S, P, Z, X and timing group C): the driver `rxvt.bas` (interpreted, with `-c` compiled with Blitz!) calls `SYS $1BFE`, `RXV`, `SYS $1BFF` for each vector. At the breakpoint `$1BFE` the script loads a fixed random pattern into the whole VRAM (`bank vdc`, `bload`), the stream into RAM and src/dst/T into the parameter block at `$1BF0`; at `$1BFF` it saves the VRAM (`bsave`) and compares all 65536 bytes and `$1C06`/`$1C08` with the expectation. Options: `--only REGEX`, `-v` (one line per vector), `--fast` (driver runs with `FAST`), `--bin`/`--sym` (other build, e.g. `RXV_TRACK_DST = 1`), `--cycles FILE` / `--cycles-append FILE` (timing table, `test/rxv/cycles.md`), `--hw test/rxv-hw.bas` (writes the hardware listing with checksums). Port rule as for the other monitor scripts (`DBE_VICE_PORT`, 6631–6649, bind test, only its own emulator process).

The listings `rxv-*.bas`, `vcl-rxv.bas`, `vms-source-skip.bas`, `vcl-vms-source-skip.bas` run in the normal test runs (both modes). The error checks of `RXV` are one per file (`rxv-errors*.bas`) because of the Blitz! limit above. `vdc-reg30-zero.bas` and `rxv-hw.bas` are also meant for a real C128: they print their result and need no debug cartridge.

**Drawing** (`docs/v3/P2c-draw.md`): `test/ref/vdcdraw.py` is the Python reference model of `VDM`/`VDL`/`VDR`/`VDB` (standard library only); it defines the pixels of a line and every result byte. `test/ref/test_vdcdraw.py` checks the model itself, `test/ref/gen_vectors.py` makes the test cases (`build()`, groups S1 S0 V L R B G Z, `--quick` subset). `test/draw/run_draw.py` imports `build()` directly (no JSON file) and runs all cases in one VICE session: the driver `drawt.bas` (interpreted, with `-c` compiled with Blitz!) calls `SYS $1BFE`, the commands of the case (read from the parameter block at `$1B00`, executed with `ON K GOSUB`) and `SYS $1BFF`. At `$1BFE` the script loads the pattern `random.Random(13)` into the whole VRAM and writes T, R6 and the commands into the block; at `$1BFF` it saves the VRAM and compares all 65536 bytes with the model. Options: `--quick` (1010 of 1520 cases, the default for development; the full set for acceptance), `--group`, `--only REGEX`, `-v`, `--bin`/`--sym`, `--cycles FILE` (adds the timing group C, measured between the breakpoints `draw_begin` and `draw_end` with interrupts off, and writes `test/draw/cycles.md`). Port rule as for the other monitor scripts. Full set interpreted ≈ 30 s, `-c --quick` about the same.

`draw-basic.bas` is a small check of each drawing command with `VMD`. The error checks are one per file (`draw-errors*.bas`, Blitz! limit above); after the caught error they read the VRAM directly through `$D600`/`$D601`, because no extension works any more in the compiled program. `vcl-vms-bank1.bas` and `vcl-vms-bank1-invalid.bas` check `VCL` type 9 (`docs/v3/P2d-vms-bank1.md`): the same lists in bank 1 (type 9) and bank 0 (type 12), decoy lists in bank 0 at the bank 1 addresses, a header across a page boundary, a list of 403 bytes, source skip, `$D506` restored, an invalid VMS command. `vcl-rxv-twice.bas` (two `VCL` type 8 entries in a row) and `vcl-rxv-vms.bas` (types 12, 8, 12, 8: register 24 bit 7 switches between block copy and block fill) close the gaps from the review of P2b.

## Z64K (second emulator)

Z64K (Java, GUI) models the VDC more exactly than VICE. It is a second backend for the behaviour tests, RXV, the drawing commands and the benchmark; VICE stays the default and is unchanged.

**Needs** `java` 21 (with `jdk.compiler`, the runner is started from source), `Xvfb`, and `Z64K.jar` in `~/.local/share/z64k/` (or `Z64K_JAR`). `test/z64k.py` is the shared module (also used by `bench/run_bench.py`): it starts its own Xvfb (display number from `-displayfd`, never a fixed one), runs `java -cp Z64K.jar test/z64k/Z64KRun.java c128 -vdc64kb -warp -debugcart -limitcycles N -8 disk.d64 -autoload prog.prg`, and ends only its own Java and Xvfb processes by PID. The end of the run is the event: a write to `$D7FF` ends Z64K with that exit code; `-limitcycles` (exit code 1) and the total timeout (exit code 124) are only guards. Z64K never writes disk images back itself, so `Z64KRun.java` saves drive 8 in a shutdown hook (`Z64K_SAVE`). A start takes about 8 s (Java), and Z64K in warp mode runs only at about 1.7 MHz of emulated CPU, so the cycle counts below matter. `test/z64k.py --prg X --d64 Y [--save OUT]` runs one program from the command line.

**Behaviour tests**: `test/run.sh -z [-c] files...` (same disk, same exit-code rule as with VICE; `-d` screenshots are ignored, Z64K has no VDC screenshot; `ZTIMEOUT`, default 600 s, is the guard per file). `-l` works too. About 8 s per file, so all 45 files take about 6 to 7 minutes per mode.

**VRAM comparison without a monitor** (`run_rxv.py --emu z64k`, `run_draw.py --emu z64k`): Z64K has no monitor with breakpoints and `bsave`, so a small 6502 program (`test/z64k/zvt.s`, loaded at `$2F00`, behind VDC-BASIC) does the work of the two breakpoints `$1BFE`/`$1BFF`. The driver calls `SYS` on it instead (`test/zvt.py` rewrites the driver):
- `PREP` writes the base pattern into the whole VRAM through `$D600`/`$D601` (`VRAM[256 p + y] = T1[y] xor T2[p]`, two random 256-byte tables; the VICE runs keep `random.Random`), copies the parameters and the stream of the next vector from a data file in RAM bank 1 (loaded by BOOT, at `$1000`, above the BASIC variables) to their addresses.
- `CHECK` reads all 65536 bytes of VRAM and builds a CRC-16 (CCITT, table driven, about 40 cycles per byte), compares it with the value Python precomputed, and also compares single RAM bytes (RXV: `$1C06`..`$1C09`).
- `FIN` writes the result file `RES` (number and actual CRC of each deviating vector) to the disk and ends Z64K with `$D7FF` = 0 (all equal) or 10. A BASIC error ends with 2 (`TRAP`, error number and vector are in `RES`).
Why a CRC: it needs no space for a second 64 KB copy, is cheap enough for the 6502 (a byte-wise compare against an expectation would need that copy), and a CRC-16 misses a random deviation with a probability of 1/65536. The first record of every start is a self test (base pattern, no command), which shows a broken harness or VDC read/write path separately. Many vectors per start: all of them go into one start per job; the vectors are spread over `--jobs` parallel starts (default min(3, cores - 1)). On a deviation, a second short start repeats the first three deviating vectors with `diag`: the 6502 copies the VRAM in four 16 KB parts to `$8000` and saves them as files `Dnnnnk` on the image (`SAVE` through KERNAL, bank 0), Python reads them with `c1541` and prints the first wrong address and the number of wrong bytes like the VICE run (`--no-diag` skips it). The timing groups `C` are only measured with VICE (stopwatch of the monitor); `--cycles` and `--fast` are rejected with `--emu z64k`.

**Differences to VICE**
- Each data access to `$D601` must wait for bit 7 of `$D600` (ready) in Z64K. Without the wait Z64K loses VRAM writes and returns wrong reads, VICE accepts back-to-back accesses. The helper therefore waits for every byte.
- Z64K starts with R28 = `$2x` (bit 4 = 0); the drivers set 64 KB with `RGO 28,16`, as with VICE.
- The BASIC variables of the C128 live in bank 1 from `$0400`; the data file for the helper therefore starts at `$1000`.
- Z64K runs only about 1.7 MHz of emulated CPU in warp mode. Measured with three parallel starts (4 cores): `run.sh -z` 45 files interpreted 6.7 min (compiled about the same), `run_rxv.py --emu z64k` 238 vectors 5 min, `run_draw.py --emu z64k --quick` 1010 cases 21 min (about 3.5 s per vector and start). The cycle tables stay VICE only.
- The RAM at power-on is not zero in Z64K. `rxv-config.bas` read one stream byte too few (the end token `0` at `$1307` was never poked) and only passed in VICE because that byte happened to be 0; fixed (loop to 7).

**SKIP list**: none of the `test/*.bas` files needs the VICE monitor (they all report through `$D7FF`), so `run.sh -z` has no SKIP list for them. What is skipped with `--emu z64k`: the timing group `C` of `run_rxv.py`/`run_draw.py` (needs the stopwatch of the VICE monitor), the `--cycles`/`--hw`-style outputs, `boot_sector.py` and `e1_blitz.py` (VICE remote monitor and boot sector), and the screenshots of `run.sh -d`.
