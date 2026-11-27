# P2f – Row loop of VMC/VMF without JSR

As of: 2026-11-26. Occasion: `dungeonboringengine/docs/specs/P6g-vcl-analysis.md` (proposals V3/V5): in the Crawl view the costs depend on the number of operations; a VMC row cost 187 cycles (with source step 281), a VMF row 237.

## 1. What

`vmc_execute`, `block_copy_target` (entry point from VMS) and `vmf_execute` share one row loop without JSR/RTS:

- Register number directly to `$D600` (`STX`), before **every** access to `$D601` the ready bit (`BIT $D600 / BPL`), then `STA $D601`.
- Target stays in `arg2` (zero page), step `arg5`; as before, the addition happens only after `dec arg4` (last row: no step, the zero page at the end is the same as before).
- Source step (`arg6`) is checked **once per call**: at the end the loop jumps either to `.vmc_dst_step` (only R18/R19) or to `.vmc_src_step` (`arg1 += arg6`, R32/R33, then R18/R19); the low byte of the jump back is modified by the code itself (both labels lie in one page, `!if` checks that). With `arg6` = 0 the VDC keeps counting the source itself, R32/R33 are not set again (unchanged).
- R30: low byte directly if it is not 0; if the high byte is not 0, the existing page path follows (`vdc_do_YY_pages`, new label in the macro `addcode_vdc_do_YYAA_cycles`, code unchanged). Thus for every length (0, < 256, ≥ 256) exactly the write sequence of `vdc_do_YYAA_cycles` applies.
- VMF: the same loop with a fill step (R31 = value) after R18/R19. A self-modified instruction before the fill step is `JMP` (VMC, skips it) or `BIT abs` (VMF, 4 clocks, falls through). So that target and step are in the same cells, VMF swaps `arg1` and `arg2` before the loop and back again afterwards; the zero page at the end is as before.
- R24 (bit 7 copy/fill) and the head of VMF (counter − 1, also with Y from the 6-parameter call) are unchanged.
- `complex_instruction_shared_exit` now lies directly behind the loop (the exit is a `BEQ`), `remember_mem_conf` writes `saved_mem_conf` (the same address as `.cr` previously).

Order of the register accesses per row as before: (R32, R33), R18, R19, (R31), R30. The address calculation for the next row runs after R30, while the VDC copies/fills. Experiment: doing the calculation between R19 and R30 (VICE keeps the VDC busy for 43 clocks after R18/R19) was about the same in VICE, but significantly slower in Z64K (VMC 8 bytes 1 MHz 171 instead of 150 clocks per row), rejected.

## 2. Why

Per row, five to seven JSR/RTS pairs are eliminated (`AY_to_vdc_regs_18_19` with two inner calls, `vdc_do_YYAA_cycles`, for VMF `A_to_vram_XXYY`) and the check of `arg6` per row. All callers benefit: BASIC `VMC`/`VMF`, `VCL` type 1–5, `VMS` type 1/2 (`block_copy_target`), `VMP`/`VMB`.

## 3. Size

`vdcbasic3.bin` 4771 → 4842 bytes (+71), end `$2EA0` → `$2EE9` (below `zvt` `$2F00` and `$4000`). Header unchanged: `$1C00` `JMP $1C10`, `$1C03` = `$30`.

## 4. Cycles per row

Measurement program (`build/p2f/h/timing.py`, `stub.a`, not in the repository): a stub at `$1B10` calls `vmc_execute` / `vmf_execute` / `block_copy_target` with the I flag set, CIA 2 timer A measures (clocks at 1 MHz, µs at 2 MHz); per case 1 and 201 rows, 5 (VICE) or 9 (Z64K) repetitions each, value = (mean 201 − mean 1) / 200. VMC/VMS: length 8 or 40, target step 80; "source" = source step 80; VMF: length 8/40. VICE is deterministic, Z64K varies from run to run by up to ±10 % (position relative to the frame build-up).

| Case | VICE 1 MHz before → after | VICE 2 MHz (µs) | Z64K 1 MHz | Z64K 2 MHz (µs) |
|---|---|---|---|---|
| VMC, 8 bytes | 180 → 136 (-25 %) | 112 → 84 (-25 %) | 207 → 144 (-30 %) | 120 → 80 (-33 %) |
| VMC, 8 bytes, source step | 274 → 200 (-27 %) | 152 → 122 (-20 %) | 279 → 214 (-23 %) | 162 → 125 (-23 %) |
| VMC, 40 bytes | 181 → 149 (-18 %) | 123 → 114 (-7 %) | 251 → 187 (-26 %) | 157 → 155 (-1 %) |
| VMF, 8 bytes | 230 → 173 (-25 %) | 156 → 110 (-30 %) | 241 → 181 (-25 %) | 142 → 99 (-30 %) |
| VMF, 40 bytes | 231 → 174 (-25 %) | 146 → 112 (-23 %) | 268 → 192 (-28 %) | 164 → 126 (-23 %) |
| `block_copy_target` (VMS type 2), 8 bytes | 183 → 137 (-25 %) | 113 → 83 (-27 %) | 209 → 144 (-31 %) | 118 → 82 (-30 %) |

Pure CPU time of the new loop (counted): VMC 98 clocks per row, with source step +58, VMF +20 (fill step); the rest is waiting for the ready bit. With 40 bytes per row, in Z64K at 2 MHz the VDC determines the time (hardly any gain). The before values come from the same measurement program with the P2e state (not to be re-measured); for comparison P6g (VICE, measured in the game): VMC row 187, with source step 281, VMF 237.

## 5. Tests

- Differential test old versus new (`build/p2f/h/diff.py`, VICE monitor): 1000 vectors (`vmc_execute`, `vmf_execute` with Y = arg3 and with a foreign Y, `block_copy_target`), length 0–2047, rows 1–256 (0), steps and source steps also negative (`$FFB0`, `$FFFF`, `$FF00`), `$0100`, random 16-bit values, `arg4` high byte set. Compared: the whole VRAM (random pattern beforehand), `arg1`–`arg6` afterwards, R18/R19/R24/R30/R32/R33. Result: 1000/1000 identical.
- VICE: `test/run.sh test/*.bas` 49/49 interpreted, `-c` 49/49 compiled; `test/rxv/run_rxv.py` 398/398; `test/draw/run_draw.py` (full set) 1520/1520.
- Z64K: `test/run.sh -z` with all 29 listings that use VMC/VMF/VCL/VMS/VMP/VMB, interpreted 29/29 and `-z -c` 29/29. 1 and 10 runs, respectively, had to be repeated: the Java launcher aborted with rc 125 (with three to four parallel starts), or a start stalled before the emulation (8 s CPU, only JIT threads). The repeat was green each time.
- Z64K `test/rxv/run_rxv.py --emu z64k`: 238/238. In the first run one of the three starts reached `-limitcycles` (guard) at 159/238 identical vectors and 0 deviations. The repeat gave 238/238 (RXV does not use the VMC/VMF loop).
- `run_draw.py --emu z64k` was aborted: VDM/VDL/VDR/VDB call neither `vmc_execute` nor the VMF loop, and their helpers are unchanged in the code (only the label `vdc_do_YY_pages` was added in `vdclib.a`). For the drawing commands only the position has shifted, which the full VICE run covers (1520/1520).

## 6. Effect on Crawl 128 1.1

Only `libs/vdcbasic3.prg` replaced (same format, load address `$1C00`). `test/vice_crawl.py` (pixel test against 1.0f, interpreted and compiled): **PASS**. Times (VICE: clocks, two identical runs; Z64K: T9 in jiffies, two identical runs each):

| Configuration | CPU | 1.0f | 1.1 P2e (jump) | **1.1 P2f** | vs. P2e | vs. 1.0f |
|---|---|---|---|---|---|---|
| VICE, compiled | 2 MHz | 3 385 841 | 3 353 392 | **3 104 710** (3.15 s) | −7.4 % | −8.3 % |
| VICE, compiled | 1 MHz | 6 318 623 | 6 337 353 | **5 870 128** (5.96 s) | −7.4 % | −7.1 % |
| VICE, interpreted | 2 MHz | 21 550 022 | 21 908 130 | **21 652 569** (21.98 s) | −1.2 % | +0.5 % |
| VICE, interpreted | 1 MHz | 43 727 387 | 44 549 257 | **44 078 865** (44.74 s) | −1.1 % | +0.8 % |
| Z64K, compiled | 2 MHz | 260 | 256 | **230** (3.82 s) | −10.2 % | −11.5 % |
| Z64K, compiled | 1 MHz | 463 | 459 | **429** (7.13 s) | −6.5 % | −7.3 % |

The measured values are stored permanently in the game repository in `test/timing-results.json`. Release images `crawl128-110.d81`/`.d64` rebuilt.
