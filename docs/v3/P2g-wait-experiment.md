# P2g – Experiment: fewer VDC ready waits (Z64K)

As of: 2026-11-26. Request from the user: check whether the ready waits in the VMC row loop (P2f) can be reduced, e.g. "only wait every 2nd to 8th row", plus reg 36 (refresh cycles per row) set to 0 or 1. **Experiment only:** `src/vdcbasic.asm` is unchanged, nothing is committed; the user makes the decision.

## 1. Summary

- **In Z64K a row needs exactly two waits:** before the **first access after R30** (R18 or R32; waits for the end of the copy of the previous row) and **before R30** (waits for the read-ahead triggered by writing R19). The waits before R19 (and with source step before R33 and R18) are superfluous: all 936 runs without them are byte-identical.
- **"Only wait every N-th row" does not work in Z64K.** With N = 2, 4, 8 and with no wait at all, 2 of 1 080 runs were correct, at every length (4–40 bytes), at 1 and 2 MHz and at every reg 36. Reason: in the engine's chunky mode the VDC has only about **16 free memory cycles per character row** (2 raster lines, 126 µs) during the display. Even 1 byte of copy then takes up to 127 µs, while the CPU has only 16 µs (2 MHz) or 33 µs (1 MHz) from R30 to the next R18. The VDC is finished in time only in the blanking, and the loop cannot check that more cheaply than with the wait itself.
- **Savings without the superfluous waits** (P2f loop otherwise the same, 2 MHz, reg 36 = 0 like Crawl): −5 … −17 % per row at 4–24 bytes, at 40 bytes −1 %. With source step −5 … −10 %. At 1 MHz −2 … −6 % and −4 … −10 % respectively. **Crawl frame (≈ 435 rows): ≈ 4–5 ms at 2 MHz, ≈ 3 ms at 1 MHz**, so at most ≈ 5 % and 2 % respectively of the Crawl run.
- **The bigger lever is the loop itself** (side finding): a non-unrolled loop only for lengths 1–255 without a fill switch, with the address beforehand in A/Y (`R-ACt`, 79 instead of 98 clocks) saves −19 … −35 % per row at 2 MHz at 4–24 bytes (with source step −8 … −19 %). That would be **≈ 12 ms per Crawl frame** (2 MHz) or ≈ 8 ms (1 MHz). Unrolling brings hardly anything more.
- **Reg 36 in Z64K:** the value only counts the refresh cycles per raster line (0 = none). Z64K does **not** model data loss. 0 and 1 are equally fast (±2 %). 5 is clearly slower at 2 MHz and 8–24 bytes (P2f loop up to +52 %). **Hardware risk:** with 0 (Crawl since 1.0) the VDC refreshes only via the display. In the vertical blanking there is no refresh at all for about 5 ms. Reg 36 = 1 costs nothing in Z64K compared with 0 and is safer. This has to be clarified by a test on real hardware (section 6).

## 2. What Z64K does on access

Source: `Z64K.jar`, class `mos/videochip/vdc/MOS8563` (bytecode read with `jdk.internal.classfile` from Java 21, tool in `build/p2g/jar/`). The VDC has one memory cycle per character clock (0.5 µs), and each cycle is exactly one of: fetch graphics, fetch attribute, refresh, block command/CPU access ("BUSY") or free. Status bit 7 is `!busy`.

| Access | Z64K | if still busy |
|---|---|---|
| write `$D600` (select register) | immediately | without consequences |
| R18 (address high) | immediately, **no** busy | changes the target of a running copy → wrong bytes |
| R19 (address low) | immediately; when idle busy, reads ahead the byte at the address (1 cycle) | changes the target of a running copy, no read-ahead |
| R32/R33 (source) | immediately, no busy | changes the source of a running copy |
| R30 (counter) | busy, starts copy/fill | **only the counter is set, no start** → row is missing (or the running copy gets longer) |
| write R31 | busy: write, advance, read ahead | only the data latch changes, **no write** |
| read R31 | returns the read-ahead value, then advances and reads ahead | old value, no advance |

Costs in memory cycles: copy 2 per byte + 3 (end, read-ahead), fill 1 per byte + 3. In the blanking every cycle is free: copy ≈ 1 µs/byte, fill ≈ 0.5 µs/byte (measured minima). In the display of chunky mode (R9 = 1, bitmap + attributes) the VDC fetches 2 × 80 graphics and 80 attribute bytes per character row. Of 256 character clocks, ≈ 16 − 2 × R36 remain free, once per character row (126 µs). Measured, these fit into such a window: copy of 6 bytes (R36 = 0), 4 bytes (R36 = 1) and 1 byte (R36 = 5).

Reg 36: at the end of the horizontal display, `R36 & 15` refresh cycles are inserted (0 = none). The VRAM is an `int[]` without decay. R36 has no influence on the display or on other accesses. Z64K can therefore not show data loss from missing refresh.

## 3. Method

- Program `test/bench/waits/waits.py` (generates `waits.a`, ACME). It runs standalone without VDC-BASIC and is thus also the hardware test program. Code is at `$2000`, results at `$6000` (bank 0, `BSAVE "RESn"`).
- Screen like Crawl: R0 127, R4 159, R6 120, R7 133, R9 1, R25 |= `$80`, R28 |= 24, R26 `$FF`, bitmap `$B4FF` = `$0F`, attributes from `$0000`. The VIC is blanked (no badlines even at 1 MHz), interrupts are off, measurement uses CIA 2 (timer A + B cascaded, µs). At the end the program restores the saved VDC registers.
- **Loop cases:** copy of L bytes per row from `$8000` (LFSR pattern) to `$0000` (row distance 80, visible attribute area). 8 and 128 rows are measured; time per row = (t128 − t8) / 120. Each measurement starts at the start of the display (status bit 5) with three delays (0, ⅓, ⅔ frame). After each of the 3 repetitions a Fletcher-16 over `$0000–$27FF` is checked against the expected value computed in Python (target cleared beforehand). For `A`, `C` and `none` with L = 8 and 40, rows 0–15 are additionally read out.
- **Duration cases:** time from the register access to the ready bit (polled every 7 clocks), 200 samples with a random pause (0–1 275 clocks) after idle. The samples predominate in the blanking, because they finish quickly there. Median and share are therefore too favorable; the maximum shows the display.
- **Variants** (wait = `bit $d600 / bpl`, 6 clocks when ready; "A" = first access of the row, "B" = remaining address registers, "C" = before R30):

| Variant | Structure | CPU clocks/row |
|---|---|---|
| `p2f` | copy of the P2f loop (not unrolled, waits A, B, C) | 98 |
| `p2f-AC` | the same without wait before R19 | 92 |
| `p2fS` / `p2fS-AC` | P2f with source step (R32/R33 per row) / without waits before R33, R18, R19 | 156 / 138 |
| `R-ACt` / `RS-ACt` | not unrolled, only L 1–255, without fill switch; address beforehand in A/Y, register selected beforehand; waits A, C (dec/bne) | 79 / 125 |
| `full`, `AC`, `A`, `C`, `none` | 8-fold unrolled, target: all waits / A+C / A only / C only / none | 78 / 72 / 66 / 66 / 60 |
| `AC/N`, `all/N` (N = 2, 4, 8) | unrolled; A only every N-th row (C every row) or A, B, C only every N-th row | 62–69 |
| `S-…`, `ACt`, `S-ACt` | the same with source step; `…t` like `R-ACt`, but unrolled | |

- Configurations: 1 and 2 MHz × reg 36 = 5 (Kernal), 1, 0; L ∈ {4, 8, 12, 16, 24, 40}. Each configuration is a separate Z64K run (own Xvfb, 25–150 s). Repeated cases from different runs deviate by up to ±4 %, within a run (3 positions) hardly at all.

## 4. Results

### 4.1 Which accesses need a wait? (question 1)

| Access in the row | Wait needed? | Evidence |
|---|---|---|
| R18 (without source) or R32 (with source), first access after R30 | **yes**: the copy of the previous row must be finished | `C` (without): 0/108 correct; excerpt: rows "wrong" (target changed in the middle of the copy) |
| R33, R18 (with source), R19 | **no** (R18, R32, R33 do not make busy) | `AC`, `S-AC`, `p2f-AC`, `p2fS-AC`, `ACt`, `S-ACt`, `R-ACt`, `RS-ACt`: 936/936 correct; duration R18: 0 µs |
| R30 | **yes**: the read-ahead after R19 must be finished (median 28–38 µs, max 116–126 µs) | `A` (without): 0/108; excerpt: 13–16 of 16 rows "empty" (start lost) |
| R31 (VMF, after R19) and R30 after R31 | yes (both make busy) | from the code; duration R31: like R19 |
| `$D600` (select register) | no | P2f already selects without a wait now |

The hypothesis holds: after R30 the copy runs; only the next access has to wait for its end, the address registers afterwards do not. R30, however, has to wait again, namely for the read-ahead that R19 triggers.

### 4.2 Duration until ready (question 2)

Median / max in µs; in parentheses the share of samples that were finished after ≤ 16 µs (2 MHz) or ≤ 33 µs (1 MHz). That is the CPU time from R30 to the next R18 without a wait. The share is too favorable, see section 3.

| Access | 2 MHz R36=0 | 2 MHz R36=1 | 2 MHz R36=5 | 1 MHz R36=0 | 1 MHz R36=1 | 1 MHz R36=5 |
|---|---|---|---|---|---|---|
| R19 (read-ahead) | 38 / 116 (39 %) | 34 / 120 (40 %) | 35 / 124 (40 %) | 35 / 119 (49 %) | 35 / 119 (50 %) | 28 / 126 (51 %) |
| write R31 | 38 / 120 (39 %) | 42 / 123 (35 %) | 42 / 125 (34 %) | 28 / 119 (53 %) | 28 / 119 (54 %) | 35 / 126 (48 %) |
| copy 1 | 46 / 120 (36 %) | 46 / 120 (35 %) | 51 / 127 (31 %) | 35 / 119 (49 %) | 35 / 119 (49 %) | 35 / 126 (46 %) |
| copy 4 | 50 / 124 (33 %) | 50 / 127 (32 %) | 158 / 255 (36 %) | 35 / 126 (47 %) | 42 / 126 (44 %) | 161 / 252 (28 %) |
| copy 6 | 50 / 127 (32 %) | 161 / 244 (30 %) | 283 / 379 (34 %) | 35 / 126 (44 %) | 154 / 245 (29 %) | 280 / 378 (29 %) |
| copy 8 | 148 / 244 (35 %) | 158 / 251 (34 %) | 412 / 505 (33 %) | 154 / 245 (28 %) | 154 / 245 (28 %) | 406 / 504 (29 %) |
| copy 16 | 284 / 372 (20 %) | 288 / 376 (18 %) | 652 / 756 (15 %) | 280 / 371 (31 %) | 287 / 371 (30 %) | 658 / 756 (34 %) |
| copy 40 | 636 / 752 (0 %) | 651 / 756 (0 %) | – / 1 766 (0 %) | 651 / 749 (0 %) | 658 / 756 (0 %) | 1 547 / 1 764 (0 %) |
| copy 64 | 864 / 1 130 (0 %) | 850 / 1 256 (0 %) | – / 2 775 (0 %) | 1 022 / 1 127 (0 %) | 1 148 / 1 253 (0 %) | – / 2 772 (0 %) |
| fill 40 | 294 / 379 (0 %) | 410 / 497 (0 %) | 884 / 1 008 (0 %) | 287 / 378 (30 %) | 406 / 497 (29 %) | 896 / 1 008 (37 %) |

("–": median in the blanking, not meaningful.) Minimum in the blanking: copy L ≈ L µs, fill ≈ L/2 µs. **At no length is the VDC guaranteed to be finished at the next access**, not even at 1 byte: if the start in the display lies behind the free window, it takes until the next window (up to 127 µs). The maxima are multiples of ≈ 126 µs (one character row).

### 4.3 Fewer waits in the loop (question 3)

Correct (Fletcher equal) over all 6 configurations × 6 lengths × 3 repetitions:

| Variants | correct |
|---|---|
| `p2f`, `full`, `S-full`, `p2fS` (all waits) | 756 / 756 |
| without B: `AC`, `S-AC`, `p2f-AC`, `p2fS-AC`, `ACt`, `S-ACt`, `R-ACt`, `RS-ACt` | 936 / 936 |
| `A` (without C), `C` (without A) | 0 / 216 |
| `AC/2`, `AC/4`, `AC/8`, `S-AC/2…8` | 2 / 648 |
| `all/2`, `all/4`, `all/8`, `none`, `S-none` | 0 / 540 |

Time per row (µs; at 1 MHz = clocks), only the correct variants. `p2f…` and `R-…` come from the same runs (mean of two runs where available):

**2 MHz, reg 36 = 0 (Crawl):**

| Variant | L=4 | L=8 | L=12 | L=16 | L=24 | L=40 |
|---|---|---|---|---|---|---|
| `p2f` (today) | 111 | 115 | 131 | 139 | 164 | 216 |
| `p2f-AC` | 105 (−5 %) | 108 (−6 %) | 118 (−10 %) | 121 (−13 %) | 137 (−17 %) | 214 (−1 %) |
| `R-ACt` | 90 (−19 %) | 91 (−21 %) | 92 (−30 %) | 93 (−33 %) | 106 (−35 %) | 211 (−2 %) |
| `full` → `AC` (unrolled) | 89 → 86 | 90 → 87 | 90 → 84 | 98 → 85 | 136 → 120 | 213 → 212 |
| `p2fS` (today) | 169 | 180 | 211 | 214 | 228 | 234 |
| `p2fS-AC` | 153 (−9 %) | 162 (−10 %) | 198 (−6 %) | 205 (−4 %) | 223 (−3 %) | 225 (−4 %) |
| `RS-ACt` | 139 (−18 %) | 147 (−19 %) | 177 (−16 %) | 186 (−13 %) | 209 (−8 %) | 221 (−5 %) |

**1 MHz, reg 36 = 0:**

| Variant | L=4 | L=8 | L=12 | L=16 | L=24 | L=40 |
|---|---|---|---|---|---|---|
| `p2f` | 183 | 224 | 226 | 237 | 239 | 248 |
| `p2f-AC` | 177 (−3 %) | 218 (−3 %) | 220 (−3 %) | 233 (−2 %) | 235 (−2 %) | 233 (−6 %) |
| `R-ACt` | 162 (−11 %) | 205 (−9 %) | 207 (−8 %) | 225 (−5 %) | 226 (−5 %) | 224 (−10 %) |
| `p2fS` | 219 | 268 | 271 | 316 | 361 | 388 |
| `p2fS-AC` | 210 (−4 %) | 255 (−5 %) | 256 (−5 %) | 288 (−9 %) | 330 (−9 %) | 377 (−3 %) |
| `RS-ACt` | 203 (−7 %) | 242 (−10 %) | 244 (−10 %) | 263 (−17 %) | 298 (−17 %) | 355 (−9 %) |

Why the savings are larger than the 6 clocks per wait: the copies run almost only in the blanking (≈ 25 % of the frame, where all cycles are free). The less CPU time a row needs, the more rows the loop manages there, and the fewer fall into the display (≈ 6 bytes of copy per 126 µs). At 40 bytes the VDC determines the time, and the CPU side gains nothing. All tables (all variants, reg 36 = 1 and 5) are in `test/bench/waits/results/z64k-20261126/report.md`.

### 4.4 Reg 36 (question 4)

| | Z64K |
|---|---|
| Model | `R36 & 15` refresh cycles per raster line, 0 = none; no data decay |
| Display, stability | The display does not depend on R36 (only the refresh counter reads it). All Z64K runs (21 sets and the set `hw`) ran through without hangs. The VDC picture itself cannot be checked (`-exitscreenshot` shows only the VIC). |
| 0 versus 1 | equally fast (loops ±2 %); free cycles per character row 16 versus 14 |
| 0 versus 5 (Kernal) | `p2f` with 5: 2 MHz L 4–24 slower by 17–52 % (174 versus 115 µs at L = 8), L = 40 faster by 8 %; 1 MHz L 4–40 slower by 4–38 %. Unrolled variants with 40 bytes are partly faster with 5. |
| only during the list? | In Z64K, R36 affects only the speed of the VDC commands. Lowering R36 only around VCL/VMC (2 register accesses, ≈ 30 clocks) would bring the same savings. |

**Hardware risk (cannot be checked in Z64K):** the 64 KB VDCs use DRAM, which has to be refreshed every few ms. With R36 = 0 this happens only through the read accesses of the display. In the vertical blanking (80 of 320 raster lines, ≈ 5 ms) there is then no refresh, and areas outside the display (asset pool, fonts) depend on the accesses of the display. With R36 = 1 there are 320 refresh cycles per frame instead of 0. That is less than the Kernal value (1 600), but in Z64K just as fast as 0. Lowering R36 only during the list shortens the time without refresh, but does not eliminate it (VCL runs ≈ 0.1–0.3 s per frame). For data retention there is already the reg 36 test in the P0 bench (`test/bench/loader-hw.bas`, case 11: 8 KB outside the display, 1/50/500 frames, R36 = 0–5). It should run on real hardware before R36 = 0 stays.

### 4.5 Conclusion (question 5)

Basis for the Crawl frame: P6g (`dungeonboringengine/docs/specs/P6g-vcl-analysis.md` 4.1, mean per frame) with 279 VMC rows without source step (mostly wall cores, Ø ≈ 10 bytes), 106 rows with source step (Ø ≈ 10–22 bytes) and 53 VMF rows. Crawl run (`dungeoncrawlerjam2025/test/timing-results.json`, 1.1 P2f): Z64K compiled 230 jiffies (3.82 s) at 2 MHz and 429 (7.13 s) at 1 MHz, 39 keys. The savings per row come from 4.3 (L ≈ 8–12). VMF is not measured; the same saving as without source step is assumed.

| Change | per Crawl frame 2 MHz | 1 MHz | Share of the Crawl run (upper estimate: 39 full frames) |
|---|---|---|---|
| omit waits before R19 (or R33/R18/R19) (`p2f-AC`, `p2fS-AC`) | ≈ 4–5 ms | ≈ 3 ms | 2 MHz ≈ 5 %, 1 MHz ≈ 2 % |
| additionally fast loop for L < 256 without fill switch (`R-ACt`, `RS-ACt`) | ≈ 12 ms | ≈ 8 ms | 2 MHz ≈ 12 %, 1 MHz ≈ 4 % |
| wait only every N-th row | – | – | wrong in Z64K, not usable |

- **Appears safe (in Z64K):** omit the waits before R19, R33 and R18 in the source step and keep the two remaining ones. For VMF only the wait before R19 is dropped; R31 and R30 need theirs. This is a small change to `.vmc_src_set`/`.vmc_dst_step` (replace `+vdc_sta` → `sta vdc_data`, code gets shorter).
- **More worthwhile** is a dedicated fast path for the most common form (VMC, length 1–255, without fill step) like `R-ACt` (≈ 40 bytes). For this, space has to be checked: `vdcbasic3.bin` ends at `$2EE9`.
- **Not** usable: waits only every N-th row, so also unrolling alone for that. Reg 36 = 0 instead of 1 brings nothing in Z64K.
- **To be checked on real hardware:** (1) Are the variants without B byte-identical there as well? How the real 8563 reacts to write accesses to R18/R19/R32/R33 while busy is not documented; Z64K is a model, VICE does not check it at all. (2) Do `A`, `C` and `AC/N` fail there in the same way, i.e. does the bandwidth picture of the display hold? (3) Free cycles per character row in chunky mode (duration table). (4) Data retention at R36 = 0/1 (P0 bench case 11).

## 5. Failure patterns (excerpts rows 0–15, L = 8/40)

| Variant | Picture |
|---|---|
| `A` (without wait before R30) | 13–16 rows "empty": R30 hit the read-ahead after R19, the copy did not start |
| `C` (without wait before R18) | 6–16 rows "wrong", partly correct: R18/R19 moved the target of a running copy |
| `none` | 9–15 empty, 1–7 wrong |

## 6. Real hardware: test program

`test/bench/waits/waits.py build --set hw` generates `build/p2g/hw/waits.d64` with `LOADER` and `W`. The set `hw` comprises 14 variants (`p2f`, `p2f-AC`, `R-ACt`, `p2fS`, `p2fS-AC`, `RS-ACt`, `full`, `AC`, `ACt`, `A`, `C`, `AC/2`, `AC/8`, `S-AC`) × 6 lengths and all duration cases. The whole thing runs in 6 configurations: 2 MHz and 1 MHz with R36 = 0, 1, 5.

1. Image onto disk/SD2IEC/Ultimate, on the C128 (64 KB VDC) `LOAD"LOADER",8` and `RUN`. During the run the 40-column screen is black and the VDC shows patterns. For each configuration the loader writes `RES0` … `RES5` (≈ 15 KB each) and reports `RESn OK`, at the end `FERTIG`. In Z64K one configuration takes ≈ 2 min in warp mode; on real hardware an estimated 1–2 min, together about 10 min.
2. Fetch the files, e.g. `c1541 -attach waits.d64 -read res0 RES0.prg` etc.
3. Evaluate: `test/bench/waits/waits.py eval RES0.prg … RES5.prg --json hw.json` (tables as above, `F` = Fletcher wrong).

Evaluation of the hardware runs: if `p2f` (all waits) is wrong, it is not due to the waits. Then data retention has to be checked: the source `$8000–$93FF` lies outside the display, especially at R36 = 0/1. In Z64K the set `hw` ran through completely once (loader, 6 × `BSAVE`, rc 0). Result as above: all variants with A and C were byte-identical (1 080/1 080), `A` and `C` 0/108, `AC/2` 1/108, `AC/8` 0/108. Storage: `test/bench/waits/results/z64k-20261126/hw/` (`res0–5.prg`, `report.md`); this is the Z64K reference for the hardware comparison.

## 7. Files, raw data

- `test/bench/waits/waits.py`: generator, Z64K run (`z64k --sets …`), evaluation (`eval`). Sets: `z<MHz>r<R36>` (all variants + duration, version 1 of the duration measurement without random position), `tz…`/`rz…` (additions `…t`, `p2f-AC`, `p2fS…`, `R-…`), `dz…` (duration only, random position, 200 samples), `hw`.
- Raw data (result blocks `$6000…`, version 2 with all duration samples): `test/bench/waits/results/z64k-20261126/*.prg`. Overall report: `…/report.md` (`waits.py eval` over all files; duration only from `dz*`).
- Intermediate files: `build/p2g/` (do not check in). The Z64K bytecode listing (`build/p2g/jar/MOS8563.yaml`) is also there.
- Comparison values P2f (text mode, Kernal R36): `docs/v3/P2f-vmc-loop.md` 4. The values here are higher because chunky mode needs many more memory cycles for the display.
