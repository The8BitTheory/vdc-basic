# P0: VDC timing benchmark (block fill, CPU streaming, VMC/VMS, reg 36)

As of: 2026-11-25 (adjusted after review of the implementation, see section 13). Detailed specification for step P0 from `dungeonboringengine/docs/plan-0.2.md`. Implementation: Sonnet (benchmark program, harness, evaluation), Haiku (VICE runs). Review: Opus.

## 1. Purpose and result

The benchmark measures the cost of the VDC accesses used by `RXV` (VRL decoder, P2b) and the engine (P6):

| Part | Measured quantity |
|---|---|
| (a) CPU streaming via reg 31 | Time per byte (constant value and literal from RAM), cost of a new address |
| (b) Block fill | Time per run length L = 1 … 256 including setup (reg 24 RMW, first byte, reg 30) and wait until done; plus the CPU time up to the write of reg 30 ("issue") |
| (c) Block copy | like (b), with source address reg 32/33 |
| (d) `VMC` / `VMS` of VDC-BASIC v2h | Time per byte, per row, per command; `VMS` type 1 and type 8 |
| (e) Reg 36 | Data retention in VRAM outside the display at reg 36 = 0 … 5 |

**Main result:** the threshold **T** = smallest run length from which block fill (until done) is faster than CPU streaming, for all lengths ≥ T. Also the cost parameters for the cycle model of `tools/vrl.py` (P1, section 8.3).

**All results from VICE are provisional** (section 2). First real-hardware run: 2026-11-27, `test/bench/results/c128-20261127.{json,md}` (threshold T = 2 for R36 = 0, 6 for R36 = 5; the plausibility warnings (see the plausibility check below) appear because the expected values come from the VICE model). The benchmark program therefore also runs unchanged on real hardware (section 11); only those values count as final.

## 2. VDC timing in VICE 3.10 (from the source code)

`~/.local/src/svn-mirror-3.10.0/vice/src/vdc/vdc-mem.c` models the busy time of the VDC only roughly (`vdc_status_clear_clock`, unit `maincpu_clk`):

| Access | VICE model |
|---|---|
| Write/read reg 31, write reg 18/19 | busy 43 clocks in the visible area (`display_enable`), otherwise 4 |
| Block fill | executed immediately, busy `L × 66 / 100` clocks |
| Block copy | executed immediately, busy `L × 120 / 100` clocks |
| Reg 36 (refresh) | no effect ("unsupported"), VRAM never loses data |
| other registers | no busy |

Consequences:

- In VICE, T will turn out very small (fill almost free, CPU byte in the display about 50 µs, in the blanking limited by the loop). On real hardware both depend on the VDC memory bandwidth and on the display. **T from VICE must not be hard-coded into `RXV`** (see `review-notes.md`: T as a patchable byte).
- **Clarified:** `maincpu_clk` counts in 1 MHz clocks even in 2 MHz mode (`vicii/vicii-clock-stretch.c`, `vicii_clock_add`: half steps in `fastmode`). The busy times above are therefore µs: 43 µs = 86 CPU cycles at 2 MHz. The cross-check (section 5) confirms this (stopwatch/CIA = 0.9999 even at 2 MHz). Measurement continues to use the CIA timer in µs; the CPU cycles are derived from it.
- `display_enable` in VICE is vertical only: character rows 1 … R6 of R4 + 1 (here 120 of 160, 75 % of the time). A byte via reg 31 costs 43 µs busy in the display plus polling loop and DRAM refresh of the VIC (measured ≈ 49.6 µs = 99 cycles at 2 MHz, 57 cycles at 1 MHz), in the blanking only the loop. A window that extends beyond the display mixes both; hence `vdc_sync` (section 6) and `cpu_byte` only from short windows (section 10.2).
- The reg 36 test in VICE can only check the harness (expected: 0 errors).
- Every JSON output from VICE carries `"provisional": true` with this reason.

**Plausibility check (mandatory, warning in the output):** If the result deviates significantly from the VICE model, the benchmark program is more likely wrong. Expectation in VICE: `fill_done(256) − fill_done(2)` about 254 × 0.66 clocks; `copy_done(256) − copy_done(1)` about 255 × 1.2 clocks; `cpu_byte` (display) between 43 × MHz and 62 × MHz CPU cycles (only when `provisional`).

## 3. Files (all under `test/bench/` in the vdc-basic repo)

| File | Content |
|---|---|
| `vdcbench.s` | Benchmark program, ca65 (`ca65 -t none`), loads at `$2000` |
| `vdcbench.cfg` | ld65 configuration: `LOADADDR` (2 bytes `$2000`), `CODE`+`RODATA`+`DATA` from `$2000` to `$2FFF`, `BSS` `$3000–$37FF` (VMS lists, variables) |
| `loader.bas` | BASIC loader for VICE (text, tokenized with `test/bastok.py`) |
| `loader-hw.bas` | Loader for real hardware: like `loader.bas`, then `BSAVE` of the results |
| `run_bench.py` | Build, disk, VICE run via the remote monitor, evaluation |
| `evaluate.py` | evaluation only: result block (`.prg`) → JSON + Markdown; imported by `run_bench.py` |
| `results/` | Results: `<source>-<YYYYMMDD>.json` and `.md`, `<source>` = `vice3.10` or `c128` (hardware) |
| `README.md` | Quick guide (invocation, dependencies) |

Intermediate build files go to `build/bench/` (do not check in; add to `.gitignore`).

### 3.1 Build

```sh
ACME=$HOME/.local/share/acme/ACME_Lib acme -f cbm -o build/bench/vdcbasic2h.bin \
     -l build/bench/vdcbasic2h.sym src/vdcbasic.asm
cmp build/bench/vdcbasic2h.bin vdcbasic2h.bin        # must be identical, otherwise abort
```

`run_bench.py` converts exactly these symbols from the ACME symbol list (lines `\tname\t= $xxxx\t; …`) into `build/bench/vdcbasic_syms.inc` (ca65: `name = $xxxx`): `remember_mem_conf`, `saved_mem_conf`, `vmc_execute`, `vms_execute`. If one is missing → abort.

```sh
ca65 -t none -I build/bench -o build/bench/vdcbench.o test/bench/vdcbench.s
ld65 -C test/bench/vdcbench.cfg -Ln build/bench/vdcbench.lbl -m build/bench/vdcbench.map \
     -o build/bench/vdcbench.prg build/bench/vdcbench.o
```

Disk (like `test/run.sh`): `c1541 -format "bench,01" d64 build/bench/bench.d64 -write build/bench/vdcbasic2h.bin vdcbasic2h -write build/bench/vdcbench.prg vdcbench`; loader with `python3 test/bastok.py test/bench/loader.bas build/bench/loader.prg`.

## 4. Flow and memory

| Area (bank 0) | Content |
|---|---|
| `$1300–$1BFF` | VDC-BASIC v2h (`BLOAD`, `SYS 4864`) |
| `$1C01–` | BASIC loader (a few lines, no variables) |
| `$2000–$2FFF` | Benchmark program |
| `$3000–$37FF` | VMS lists, variables of the benchmark program |
| `$4000–$7FFF` | Result block (RAM 0 under the BASIC ROM; written with `$FF00 = $3E`) |

Header of the benchmark program at `$2000`:

| Address | Content |
|---|---|
| `$2000` | `JMP main` |
| `$2003` | Version of the benchmark program (1) |
| `$2004` | Mode, set by the harness before the start: bit 0 = reg 36 wait time from the harness (1) or via the jiffy clock (0, real hardware); bit 1 = skip the reg 36 test; bit 2 = short run (only configuration 2 MHz/reg 36 = 0, L only from the subset) |
| `$2005` | `r36_flag` (harness sets 1 = wait time over) |
| `$2006–$2007` | `r36_frames`: frames of the current wait time |

`loader.bas`:

```
10 BLOAD"VDCBASIC2H",B0,P(4864):SYS4864
20 BLOAD"VDCBENCH",B0,P(8192)
30 BANK15:SYS8192
40 BANK15:POKE DEC("D7FF"),0
```

`loader-hw.bas` replaces line 40 with `40 BSAVE"@BENCHRES",B0,P(16384) TO P(32768):PRINT"FERTIG"`.

`main`:

1. Save state: `$FF00`, `$D011`, `$D030`, CIA2 ICR mask (set to `$7F` = all CIA2 NMIs off), VDC registers 12/13, 20/21, 24, 25, 26, 28, 36.
2. Blank the VIC (`$D011` bit 4 = 0) so that no badlines steal cycles.
3. Set the VDC configuration (section 4.1).
4. Write the result block header.
5. For each configuration (section 6.2): `SEI`, `$FF00 = $3E`, set clock (`$D030` bit 0), run measurement cases 0–10, restore `$FF00`, `CLI`.
6. Reg 36 test (section 9), except mode bit 1.
7. Write the final record (`$FF`), restore state, label **`bench_done`**, `RTS`.

### 4.1 VDC configuration (like Crawl, `crawl.bas` 10065–10135)

All register changes except those named here stay at the Kernal state. Reg 24, 25, 28 only via read-modify-write.

| Reg | Value | Meaning |
|---|---|---|
| 0 | 127 | horizontal total |
| 4 | 159 | vertical total |
| 6 | 120 | displayed rows |
| 7 | 133 | vertical sync position |
| 9 | 1 | 2 raster lines per row |
| 25 | bit 7 = 1 | bitmap mode (RMW) |
| 28 | bit 4 = 1 | 64 KB RAM (RMW) |
| 26 | `$FF` | colors |
| 12/13 | `$B4FF` | bitmap (like `VR(0)` in Crawl) |
| 20/21 | `$0000` | attributes (`AP`) |
| 36 | 0 or 5 per configuration | refresh |

VRAM areas: display attributes `$0000–$257F`, bitmap `$B4FF–$FFFF`; **destination measurement area** `$5000–$6FFF`, **source measurement area** (copy, VMC, VMS) `$7000–$8FFF`, **reg 36 test area** `$9000–$AFFF`. Before case 0 the bitmap is filled with `$0F` (block fill, like Crawl) so that the display matches chunky mode.

## 5. Time measurement (CIA 2, µs)

CIA 2 timer A counts φ2 (1 MHz, also in 2 MHz mode), timer B counts the underflows of timer A. This yields 32 bits in µs; 24 bits are stored.

```
t_start:                  ; X, Y are preserved
    lda #$00
    sta $DD0E             ; timer A stop
    sta $DD0F             ; timer B stop
    lda #$FF
    sta $DD04
    sta $DD05
    sta $DD06
    sta $DD07
    lda #%01010001        ; timer B: counts underflows of A, force load, start
    sta $DD0F
    lda #%00010001        ; timer A: φ2, continuous, force load, start
    sta $DD0E
    rts
t_stop:                   ; -> t_lo/t_mid/t_hi = $FFFFFFFF - counter value (24 bits)
    lda #$00
    sta $DD0E             ; A stop (B then stops counting)
    ... read TB hi/lo, TA hi/lo, invert (EOR #$FF), store 24 bits
    rts
```

- The measurement spans `jsr t_start` to `jsr t_stop`. Case 0 (empty) measures exactly this frame with the same loop structure; `evaluate.py` subtracts it.
- Each measurement repeats the operation `reps` times in a loop **inside** the time window (exception: "issue" cases, 6.1). The loop is the same everywhere: `ldy #reps` … `dey / bne` (Y remains free for the case: cases that need Y save it in a zero-page cell `$FB`). Implementation of cases 8–10: the VDC-BASIC routines modify Y; there `$FB` itself counts the repetitions (`dec $FB / bne`) instead of `dey`.
- Zero page: only `$FA–$FF` and the VDC-BASIC arguments `$83–$8E` (only cases 8–10).
- The time in CPU cycles is µs × 1 (1 MHz) or µs × 2 (2 MHz). This is a conversion, not a second measurement.

**Cross-check (once, VICE only):** For case 1 with N = 256 the harness additionally sets breakpoints on the labels `xchk_a` and `xchk_b` directly before `t_start` and after `t_stop` and reads the monitor stopwatch there (`r`, last column, like `test/vice_drive.py` in the engine repo). The difference in µs must match the CIA value within ±20 (the stopwatch also counts µs at 2 MHz, section 2); the ratio is logged as well. Implementation: `xchk_a`/`xchk_b` lie inside the CIA window (after `jsr t_start` or before `jmp t_stop`); `evaluate.py` subtracts 15 CPU cycles of frame from the CIA value (`FRAME_CYCLES`, in µs: 15 / MHz).

## 6. Measurement cases

Macros (ca65):

```
.macro vwait
:   bit $D600
    bpl :-
.endmacro
.macro vsel r             ; select register
    ldx #r
    stx $D600
.endmacro
.macro vput               ; A into the selected register
    vwait
    sta $D601
.endmacro
```

**`vdc_sync` (before each case, outside the window):** wait until status bit 5 (`$D600`, VBLANK) is set, then until it falls, i.e. until the start of the display. This way each measurement starts in the same raster phase; without sync, runs in VICE scattered by up to 504 µs. Consequence: windows up to about 15 ms (case 1/2 with N ≤ 16, all fill/copy single values) lie entirely in the display and measure its cost; longer windows (case 1/2 with N ≥ 32) average over display and blanking. On real hardware bit 5 is also VBLANK.

Before each case: set the destination address (reg 18/19) to the start of the destination measurement area; for copy/VMC/VMS set the source to the source measurement area; then `vwait` (VDC ready). All outside the time window.

| ID | Name | Parameter p | Sequence per repetition (in the time window) | reps |
|---|---|---|---|---|
| 0 | `empty` | – | nothing (loop only) | 16 |
| 1 | `stream_const` | N | `vsel 31` once before the window, `lda #$55`; then N times `vwait / sta $D601` (loop `ldx #N … dex / bne`, N = 256 as `ldx #0`) | 16 |
| 2 | `stream_lit` | N | like 1, but `lda src,x` from a RAM buffer (`ldx #0 … inx / cpx #N / bne`, N = 256 as `cpx #0`) | 16 |
| 3 | `new_addr` | – | write reg 18 and 19 (`vsel 18 / lda #hi / vput / vsel 19 / lda #lo / vput`), `vsel 31`, one byte `vput` | 16 |
| 4 | `fill_done` | L | [variant rmw: `vsel 24 / vwait / lda $D601 / and #$7F / vput`] · `vsel 31 / lda #$AA / vput` (first byte) · for L ≥ 2: `vsel 30 / lda #L−1 / vput` · `vwait` (done) | 16 |
| 5 | `fill_issue` | L | like 4 without the final `vwait`; measurement per single operation (6.1) | 16 |
| 6 | `copy_done` | L | [rmw: `vsel 24 / vwait / lda $D601 / ora #$80 / vput`] · source: `vsel 32 / lda #hi / vput / vsel 33 / lda #lo / vput` · `vsel 30 / lda #L&255 / vput` · `vwait` | 16 |
| 7 | `copy_issue` | L | like 6 without the last `vwait`; single operation (6.1) | 16 |
| 8 | `vmc` | w, h | `jsr remember_mem_conf`; `arg1` = source, `arg2` = destination, `arg3` = w, `arg4` = h, `arg5` = 80, `arg6` = 0; `jsr vmc_execute` | 4 |
| 9 | `vms1` | w, h | list at `$3000`: alpha 0, count h, h × type 1 (offset r × 80, length w); `jsr remember_mem_conf`; `arg1` = source, `arg2` = destination, `arg3` = `$3000`; `jsr vms_execute` | 4 |
| 10 | `vms8` | n | list at `$3000`: alpha 0, count n, n × type 8 (offset 0 … n−1); otherwise like 9 | 4 |

Notes:

- In 4 and 6 the destination address is **not** set again: after a block operation reg 18/19 points past the end, so the destination continues (as in `RXV`). 16 × 256 bytes = 4 KB fit in the measurement area (8 KB).
- Variant "norm" of 4: set reg 24 bit 7 = 0 once before the window. Variant "norm" of 6: set bit 7 = 1 beforehand.
- Reg 30 = 0 means 256 on the VDC. Fill L = 256 therefore writes reg 30 = 255, copy L = 256 reg 30 = 0.
- Cases 8–10: The routines run with `$FF00 = $3E` (set by `remember_mem_conf`) and restore the remembered value at the end; that is then also `$3E`. `vms_execute` needs `saved_mem_conf` (set by `remember_mem_conf`).
- The destination addresses of 8–10 start again at the beginning of the measurement area for each repetition (part of the setup cost, which cancels out in the evaluation via the slope).

### 6.1 Single operations ("issue")

For 5 and 7, per repetition: `jsr t_start`, operation, `jsr t_stop`, add the time to a 24-bit sum, then `vwait` outside the window. The sum is stored; case 0 is additionally measured in the same form for this purpose (record with variant bit 2 = "single"). The records of 5 and 7 also carry bit 2 (marker "sum of single operations"); `evaluate.py` subtracts the single-operation empty value (already a sum over reps) from them.

### 6.2 Parameters and configurations

| Configuration | Clock | Reg 36 | Parameters |
|---|---|---|---|
| A (engine) | 2 MHz | 0 | Case 4, 6: L = 1 … 256 (all), both variants; case 5, 7: L ∈ M |
| B | 2 MHz | 5 | L ∈ M |
| C | 1 MHz | 0 | L ∈ M |
| D | 1 MHz | 5 | L ∈ M |

M = {1, 2, 3, 4, 5, 6, 8, 10, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256}. Case 1, 2: N ∈ {1, 2, 4, 8, 16, 32, 64, 128, 256}. Case 8, 9: w ∈ {1, 2, 4, 8, 16, 32, 64} × h ∈ {1, 16}. Case 10: n ∈ {1, 8, 32, 128}. In the short run (mode bit 2) only configuration A with L ∈ M.

## 7. Result block (bank 0, from `$4000`)

| Offset | Content |
|---|---|
| 0–1 | `"B" "R"` |
| 2 | Version 1 |
| 3 | Version of the benchmark program |
| 4–5 | Number of records |
| 6–7 | reserved 0 |
| 8 … | Records of 8 bytes, end = record with case `$FF` |

Record:

| Byte | Content |
|---|---|
| 0 | Case ID (0–11, `$FF` = end) |
| 1 | Configuration: bit 0 = 2 MHz, bit 1 = variant rmw, bit 2 = single operation (6.1), bit 4–7 = reg 36 value |
| 2–3 | Parameter (L, N, n; for 8/9: low = w, high = h; for 11: frames) |
| 4 | reps (for 11: reg 36 value) |
| 5–7 | Time in µs, 24 bits, raw (including frame); for 11: number of erroneous bytes |

Space requirement about 1,700 records = 13.6 KB (< 16 KB). The benchmark program checks the limit `$7FF8` and otherwise aborts with record `$FE`.

## 8. Harness `run_bench.py`

Invocation: `test/bench/run_bench.py [--short] [--no-r36] [--port N] [--out test/bench/results] [--from-file BENCHRES.prg --source c128]`

**Monitor port (mandatory, applies to all VICE tests of this project):**

- Port from `--port`, otherwise from the environment variable `DBE_VICE_PORT`, otherwise **6631** (not 6510, which is the VICE default and is used by other sessions). Concurrently running tests of this project use 6631–6649, each test/agent its own port.
- Before starting, check whether the port is free (`socket.bind(("127.0.0.1", port))` and close immediately). If occupied: take the next free port in the range 6631–6649 and report that; if none is free, or `--port` was given explicitly and is occupied, abort with a clear message (exit code 2).
- VICE is given the port explicitly: `-remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>`. No `-binarymonitor`.
- Terminate only the process started by the harness itself (`Popen` object: `terminate()`, after timeout `kill()`, in the `finally`). No `pkill`/`killall` on `x128`/`x64sc`.
- The monitor connects only to exactly this port; if no VICE with a prompt answers there, abort (do not attach to a foreign VICE).

Flow (event-driven, no fixed wait times; timeout only as a guard over the whole run, not per receive; default 120 s, `--timeout S`; full run in VICE about 12 s):

1. Build (3.1), disk, loader.
2. Start VICE:
   ```
   x128 -default -warp -sounddev dummy -80col -VDC64KB -debugcart \
        +drive8truedrive -trapdevice8 -8 build/bench/bench.d64 \
        -autostartprgmode 1 -autostart build/bench/loader.prg \
        -remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>
   ```
   `<port>` according to the port rule above.
3. Connect the monitor (take over the `Monitor` class from `dungeonboringengine/test/vice_drive.py`: connection setup with retry only until the port is open, prompt counting). Breakpoints from `build/bench/vdcbench.lbl` (`al C:xxxx .name`): `main`, `bench_done`, `r36_wait_enter`, `xchk_a`, `xchk_b`; plus the BASIC error vector (`($0300)`, like `vice_drive.py`).
4. At `main`: write the mode to `$2004` (`> 2004 xx`), continue.
5. At `xchk_a`/`xchk_b`: read the stopwatch (5, cross-check), continue.
6. At `r36_wait_enter`: read `r36_frames`; enable a breakpoint on the IRQ routine from `($0314)`; `x` until F hits are counted; disable the breakpoint; `> 2005 01`; continue. (During this time the program waits in a loop for `r36_flag`, with `CLI`, `$FF00 = $00`.)
7. At `bench_done`: `bank ram`, `save "build/bench/benchres.prg" 0 4000 7fff`, then `quit`.
8. On BASIC error: print error number and line, exit code 1.
9. Call `evaluate.py`.

`--from-file`: evaluation only of a `BENCHRES` saved on real hardware (PRG, load address `$4000`).

## 9. Reg 36 test (case 11)

For r = 0 … 5 and F ∈ {1, 50, 500}:

1. `SEI`, `$FF00 = $3E`: fill the test area `$9000–$AFFF` (8 KB) via reg 31 with an 8-bit LFSR (Galois, polynomial `$B8`: `lsr; bcc +; eor #$B8`; start value `r × 16 + (F AND 15) + 1`, never 0).
2. Reg 36 = r (write directly, no bit needs to be preserved).
3. `r36_frames` = F, `r36_flag` = 0, `$FF00 = $00`, `CLI`. Label `r36_wait_enter`, then wait:
   - Mode bit 0 = 1: until `r36_flag` ≠ 0 (the harness counts the IRQs, 8.6).
   - Mode bit 0 = 0: until the jiffy clock (`$A2`) has changed F times.
4. `SEI`, `$FF00 = $3E`, reg 36 = 5, read the test area back (reg 31), compare with the same LFSR, count errors.
5. Record: case 11, configuration bit 4–7 = r, parameter = F, reps = r, time field = error count.

Clock: 2 MHz. Expectation VICE: 0 errors everywhere (reg 36 is not emulated).

## 10. Evaluation `evaluate.py`

### 10.1 Raw values

For each record: `us = raw − empty` (matching case 0 record of the same configuration; for bit 2 the single-operation empty value × reps), then `us_per_op = us / reps`, `cycles_per_op = us_per_op × MHz`.

### 10.2 Derived quantities per configuration (CPU cycles)

| Key | Computation |
|---|---|
| `cpu_byte` | Slope (linear regression) of case 1 over **N ≤ 16** (window entirely in the display, see `vdc_sync`) |
| `cpu_run_fixed` | Intercept of the same regression (≈ 0 or slightly negative: the first byte per window does not wait because `vwait` ran beforehand) |
| `cpu_byte_avg` | Case 1, N = 256, per byte (average over several frames of display + blanking) |
| `cpu_lit_byte_avg` | the same for case 2 |
| `cpu_lit_byte` | Slope of case 2 over N ≤ 16 |
| `new_addr` | Case 3 per operation − `cpu_byte` |
| `fill_fixed`, `fill_byte` | Regression `fill_done(L)` = fixed + byte × L over L ≥ 2, variant norm |
| `fill_fixed_rmw` | the same, variant rmw |
| `fill_issue_fixed`, `fill_issue_byte` | Regression over case 5 |
| `copy_fixed`, `copy_byte`, `copy_fixed_rmw` | like fill, case 6 (L ≥ 1) |
| `vmc_byte`, `vmc_row`, `vmc_call` | Model `t = call + h × (row + w × byte)`, least squares over case 8 |
| `vms1_byte`, `vms1_cmd`, `vms1_call` | the same model over case 9 (per command = per row) |
| `vms8_byte`, `vms8_call` | Regression over case 10 |
| `T`, `T_rmw` | see 10.3 |

### 10.3 Threshold T

`t_cpu(L) = cpu_run_fixed + cpu_byte × L` (CPU run), `t_fill(L)` = measured `fill_done(L)` (configuration A: every value; otherwise linearly interpolated between the values from M). **T = smallest L ≥ 2 for which `t_fill(L') < t_cpu(L')` holds for all L' from L to 256**; if there is none, T = 257 ("block fill never pays off"). `T_rmw` likewise with variant rmw. In addition `T_issue` with `fill_issue` instead of `fill_done` (applies when `RXV` already reads the next token during the fill; information only).

**Interpretation (Z64K measurement 2026-11-25):** T is determined from the measured `fill_done(L)`, not from `fill_fixed + fill_byte × L`. This is important because `fill_done` is not linear outside VICE: in Z64K (A) 447 cycles at L = 2, about 495 up to L = 12, then steep up to L = 96, afterwards almost flat (the later ones of the 16 repetitions fall into the blanking, where the VDC is faster). The regression over L = 2 … 256 therefore yields `fill_fixed` = 908 and `fill_byte` = 5.7; this is not a fixed-cost value and overestimates short fills by a factor of two (over L = 2 … 16 about 443 + 5.2 × L fits). Busy wait time is included on both sides: `t_cpu` waits for status bit 7 before each byte (hence `cpu_byte` ≈ 123 cycles, about one VDC line per byte), `fill_done` waits until the end of the fill. For small L both windows lie in the display after `vdc_sync` and are comparable: L = 3 CPU 441 against fill 448, L = 4 CPU 564 against fill 464, so T = 4 (narrow: a second run gave T = 3, the other values deviate by less than 2 %). With `cpu_byte_avg` (display and blanking averaged, 55.5 cycles) instead of `cpu_byte` the result would be T = 9; that is an upper bound because `fill_done` for small L is measured only in the display. For the cycle model (`tools/vrl.py`) outside VICE, therefore do not use `fill_fixed`/`fill_byte`, but `fill_done(L)` from `records` (for L ≤ 16 the line fit over this range). The same applies to `copy_fixed`/`copy_byte` (Z64K, A: `copy_done` 244 cycles up to L = 4, 495 at 8, 1904 at 64, 2315 at 256).

### 10.4 Output

`results/<source>-<YYYYMMDD>.json`:

```json
{
  "meta": {"source": "vice3.10", "date": "2026-11-25", "bench_version": 1,
           "vdcbasic": "v2h", "provisional": true,
           "provisional_reason": "VICE 3.10: VDC busy model rough (43/4 clocks per reg 31 access, fill 0.66, copy 1.2 clocks per byte), reg 36 without effect",
           "vdc_config": {"r0": 127, "r4": 159, "r6": 120, "r7": 133, "r9": 1, "bitmap": true, "ram64k": true},
           "warnings": []},
  "records": [{"case": "fill_done", "variant": "norm", "clock_mhz": 2, "r36": 0, "param": 17,
               "reps": 16, "us_total": 123, "us_per_op": 7.69, "cycles_per_op": 15.4}],
  "derived": {"A": {"cpu_byte": 0.0, "...": 0.0, "T": 0}, "B": {}, "C": {}, "D": {}},
  "engine": {"config": "A", "cycles_clock_mhz": 2, "cpu_byte": 0.0, "cpu_byte_avg": 0.0, "cpu_lit_byte": 0.0, "cpu_run_fixed": 0.0, "new_addr": 0.0,
             "fill_fixed": 0.0, "fill_byte": 0.0, "copy_fixed": 0.0, "copy_byte": 0.0, "T": 0},
  "r36": [{"r36": 0, "frames": 1, "errors": 0}]
}
```

`engine` is the interface to `tools/vrl.py --bench` (P1): keys and unit (CPU cycles at 2 MHz) are fixed.

`results/<…>.md` (Markdown, for the documentation): header with source, date and the note **"provisional (VICE)"**; tables:

1. Cost parameters per configuration (rows = keys from 10.2, columns A–D, cycles; µs in parentheses).
2. CPU versus block fill for L ∈ M in configuration A: `t_cpu`, `t_fill` norm, `t_fill` rmw, `t_issue`, faster (`CPU`/`Fill`).
3. T, T_rmw, T_issue per configuration.
4. `VMC` / `VMS` per byte, per row/command, per call.
5. Reg 36: r × F → errors.
6. Warnings (plausibility, section 2; cross-check 5).

## 11. Real hardware

`run_bench.py` also puts `loader-hw` (tokenized) on `build/bench/bench.d64`; on the C128 (80 columns, 64 KB VDC) `LOAD"LOADER-HW",8`, `RUN`. Mode `$2004` stays 0 (jiffy clock), the loader saves `BENCHRES` (65 blocks) to the same disk and reports `FERTIG`. Verified in VICE with mode 0 (`BSAVE` writes `BENCHRES`, `--from-file` evaluates it). Step-by-step instructions: `test/bench/README.md`. Evaluation: `run_bench.py --from-file BENCHRES.prg --source c128`. The JSON output then carries `"provisional": false`. On real hardware the reg 36 test is meaningful; F = 500 (10 s on PAL, 50 jiffies/s) shows whether data outside the display is lost without refresh.

## 12. Acceptance

| Check | Expectation |
|---|---|
| `run_bench.py` | runs through without prompts, exit code 0, produces `.json` and `.md` |
| Runtime | short run < 1 min, full run < 10 min (warp) |
| Port | default 6631 or `DBE_VICE_PORT`; occupied port is detected; only the own VICE process is terminated |
| no fixed wait times | no `sleep` in the harness except for retrying the connection setup (like `vice_drive.py`) |
| Repeatability | two runs in VICE: T identical; values in `engine` ±2 % or ±2 cycles; each `us_total` ±max(100 µs, 0.1 %). (±1 µs is not achievable in VICE: even with `vdc_sync` the phase relative to VIC refresh and VDC line remains open; control run: 77 % ±5 µs, max. 57 µs for windows < 15 ms, max. 97 µs = 0.08 % at N = 256) |
| Plausibility | Warnings per section 2 are empty or justified |
| Cross-check | CIA against monitor stopwatch ±20 µs (1 MHz) |
| Reg 36 in VICE | 0 errors |
| Repo | only `test/bench/*`, `docs/v3/P0-bench.md`, `.gitignore`; `vdcbasic2h.bin` unchanged |

## 13. Review of the implementation (2026-11-25)

- `cpu_byte` was initially the regression over all N (57 cycles at 2 MHz): a mixed value, because windows from N = 32 on extend into the blanking after `vdc_sync`. Now N ≤ 16 (99 cycles = 49.6 µs, matches 43 µs busy) plus `cpu_byte_avg` (N = 256, 58 cycles). `new_addr` accordingly 193 instead of 234. T stays 2 in VICE.
- Harness: guard timeout applies to the whole run (previously 600 s per `recv`, so a hang could go unnoticed for up to 10 min); abort if the own VICE ends before the connection is established; port check with `SO_REUSEADDR` like VICE (otherwise the port counts as occupied for 60 s after a run because of TIME_WAIT).
- Deviations of the implementation (incorporated in the sections above): `vdc_sync`, `$FB` as counter in cases 8–10, cross-check inside the window with 15 cycles of frame, bit 2 also for case 5/7, repeatability relaxed for VICE.
