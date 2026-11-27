# vdcbench: measurement program for VDC timings

Specification: `docs/v3/P0-bench.md`. Measures CPU streaming via reg 31, block fill/copy, `VMC`/`VMS`
(VDC-BASIC v2h) and reg 36, and determines the threshold T (block fill versus CPU streaming).

## Usage

```sh
test/bench/run_bench.py [--emu vice|z64k] [--short] [--no-r36] [--skip-build] [--port N] [--out DIR] [--timeout S]
test/bench/run_bench.py --from-file BENCHRES.prg --source c128     # evaluate a hardware result
```

- `--short`: short run (configuration A only, L from M), `--no-r36`: skip the reg 36 test.
- VICE monitor port: `--port`, otherwise `DBE_VICE_PORT`, otherwise 6631; if it is in use, the next free port up to 6649
  is taken (without `--port`); with an explicit `--port` it aborts with exit code 2.
- Results: `results/<source>-<YYYYMMDD>.json` and `.md` (VICE: preliminary).

## Z64K as a second backend (`--emu z64k`)

Z64K (Java, GUI, more accurate VDC than VICE) runs under its own Xvfb (display number via `-displayfd`,
only this Xvfb and Java are terminated by PID). The hardware path runs: `loader-hw` (unchanged),
so `run_bench.py` generates `build/bench/loader-z64k.prg` = `loader-hw.bas` + `POKE 8196,<mode>`
(for `--short`/`--no-r36`) + `POKE $D7FF,0` after the `BSAVE` (only this loader; `loader-hw.bas` does not access
`$D7FF`).

Invocation of Z64K (verified): `java -cp Z64K.jar test/z64k/Z64KRun.java c128 -vdc64kb -warp -debugcart
-limitcycles N -8 bench.d64 -autoload loader-z64k.prg`. `-debugcart`: writing to `$D7FF` terminates Z64K immediately
with that value as the exit code; `-limitcycles N`: exit code 1; `-exitscreenshot FILE` (VIC-II image) is possible.
Z64K **never** writes disk images back (`BSAVE` ends up only in memory, only the GUI dialog saves);
`Z64KRun.java` therefore starts Z64K in the same process and saves the image of drive 8 in a
shutdown hook (reflection on `config.ConfigManager`, `CBMConfig.saveDisk`). Afterwards `run_bench.py` reads
`BENCHRES` from the image via `c1541` and evaluates as with `--from-file` (`results/z64k-<YYYYMMDD>.json/.md`,
`"source": "z64k"`, with a comparison table against the newest `vice3.10-*.json`). The event source is the
process end (exit code 0); `--timeout` (default 600 s per run) and `-limitcycles` are only guards. A full run (one run) takes about 85 s, a short run about 70 s. The loader contains `TRAP`: a BASIC error terminates Z64K
immediately with exit code 2 (event-driven instead of waiting for the guard). Previously the reg 36 test hung in Z64K
(over 1 h): `vdcbench` wrote the read bit 7 of `$D011` (raster line bit 8) back as
raster compare bit 8 when blanking the VIC; depending on timing, no raster IRQ came afterwards and the jiffy clock stood still. Since
bit 7 is cleared, the full run goes through (the bug also affects real hardware). `--moncommands` has no effect in Z64K. Additionally required: `Xvfb`, `java` (21,
with `jdk.compiler` for the source launch), Z64K.jar in `~/.local/share/z64k/` (or `Z64K_JAR`).
`--skip-build` uses an existing `build/bench` (without rebuilding and without the `cmp` test).

## Dependencies

`acme`, `git`, `ca65`/`ld65` (cc65), `c1541`, `x128` (VICE 3.10, remote monitor), Python 3 (standard library only).
Intermediate files are in `build/bench/` (do not check in).

## Files

`vdcbench.s`/`vdcbench.cfg` measurement program; `loader.bas` (VICE), `loader-hw.bas` (real hardware, `BSAVE`
of the results); `run_bench.py` harness; `../z64k.py` and `../z64k/Z64KRun.java` (shared Z64K module, start with image saving); `evaluate.py` evaluation.

## Real hardware (C128 with 64 KB VDC)

1. Run `test/bench/run_bench.py --short` once (builds `build/bench/bench.d64` with
   `VDCBASIC2H`, `VDCBENCH`, `LOADER-HW`), and get the image onto a real disk or SD2IEC/Ultimate.
2. On the C128 in 80-column mode: `LOAD"LOADER-HW",8` and `RUN`. The screen stays black/
   unsteady (VIC blanked, VDC in bitmap mode); duration about 1 min (the reg 36 test with F = 500 alone
   takes 6 x 10 s). At the end `FERTIG` appears; the loader has written `BENCHRES` (16 KB, $4000-$7FFF) to the disk.
3. Fetch `BENCHRES` to the PC, e.g. `c1541 -attach bench.d64 -read benchres BENCHRES.prg`.
4. `test/bench/run_bench.py --from-file BENCHRES.prg --source c128` writes
   `results/c128-<YYYYMMDD>.json` and `.md` (`"provisional": false`).

Implementation notes: In cases 8-10 `$FB` counts the repetitions (instead of `Y`); the control test
sets `xchk_a`/`xchk_b` inside the CIA window (CIA value minus 15 cycles of framing against stopwatch);
`vdc_sync` starts every case at the start of the display; `cpu_byte` comes only from N <= 16 (spec 10.2).
`--timeout` (default 120 s) applies to the whole run.

Note: What is measured is VDC-BASIC v2h (`$1300`, checked-in `vdcbasic2h.bin`). For this, `run_bench.py` builds the source from
the last commit that changed `vdcbasic2h.bin` (`git show`, into `build/bench/v2h/`), independent of `src/`.
