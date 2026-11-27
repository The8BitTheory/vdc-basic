# P2c: `VDM`, `VDL`, `VDR`, `VDB` – line, rectangle, box in bitmap (1 bpp) and chunky (4 bpp)

As of: 2026-11-25. Detailed specification for step P2c from `dungeonboringengine/docs/plan-0.2.md` (section 7.3–7.5). Prerequisite: P2a (head, tokens), P2b (threshold T at `$1C04`, run routine). Implementation: Opus (assembler), Sonnet (reference model, tests). Review: Opus.

## 1. Commands

| Command | Token | Syntax | Effect |
|---|---|---|---|
| `VDM` | `$FE42` | `VDM mode,base,stride` | set the drawing surface (stays until the next `VDM`) |
| `VDL` | `$FE3E` | `VDL x1,y1,x2,y2,c` | line, both endpoints included |
| `VDR` | `$FE3F` | `VDR x,y,w,h,c` | rectangle, outline only (w × h pixels outside) |
| `VDB` | `$FE40` | `VDB x,y,w,h,c` | rectangle, filled |

The tokens `$FE3E–$FE40` have been in the table with a syntax stub since P2b (P2a, 2.2); P2c replaces the stubs. Entries in `BASIC V7.0 VDC.txt`: `VDL;fe3e;`, `VDR;fe3f;`, `VDB;fe40;`, `VDM;fe42;`.

### 1.1 `VDM mode,base,stride`

| Parameter | Range | Meaning |
|---|---|---|
| `mode` | 0, 1 | 0 = bitmap 1 bpp, 1 = chunky 4 bpp |
| `base` | 0–65535 | VRAM address of the byte with pixel (0,0) |
| `stride` | 1–65535 | bytes per pixel row |

Other values: `?ILLEGAL QUANTITY ERROR` (14). State after installation: mode 0, base 0, stride 80. The state is in the data part (5 bytes: mode, base, stride), not in the head.

### 1.2 Pixels and addresses

Byte address of a pixel: `base + y · stride + (x >> 3)` (mode 0) or `base + y · stride + (x >> 1)` (mode 1), everything modulo 65536.

| Mode | Pixel in the byte |
|---|---|
| 0 | bit `7 − (x AND 7)`: bit 7 = left pixel, 1 = foreground |
| 1 | x even → **high nibble** (bits 7–4), x odd → low nibble |

**Rationale for the nibble order (mode 1):**

- Chunky means: bitmap fixed at `$0F`, pixels in attribute RAM (plan 2, Crawl, guide chapter 5). The left 4 pixels of a byte have bits = 0 in the bitmap byte `$0F`, the right ones bits = 1.
- In bitmap mode with attributes the VDC colors 0 bits with the high nibble and 1 bits with the low nibble of the attribute. Evidence: VICE `src/vdc/vdc-draw.c`, `draw_std_bitmap`: table index `(attr & 0xF0) + ((attr & 0x0F) << 8)` versus `regs[26] << 4` in the monochrome case; reg 26 has foreground in bits 7–4 and background in 3–0, so the attribute low nibble ends up in the foreground slot (1 bits), the high nibble in the background slot (0 bits).
- From this: left pixel = high nibble. This agrees with the guide ("The upper four bits are the left pixel", chapter 5, tested with screenshots), with `dungeonboringengine/tools/crawlsprites.py` (`c % 2 == 0` → `>> 4`) and with `VMS` type 8 ("lower nybble … rightmost pixel").
- This holds only for bitmap byte `$0F` and reg 24 bit 6 = 0 (no reverse). `VDM` does not check this: mode 1 only fixes the mapping x ↔ nibble.

### 1.3 Color and drawing mode `c`

| Mode | `c` | Effect |
|---|---|---|
| 0 | 0 | clear pixel (bit = 0) |
| 0 | 1 | set pixel (bit = 1) |
| 0 | 2 | invert pixel (XOR) |
| 1 | 0–15 | nibble = c |

Other values: error 14. No XOR in mode 1 (rarely useful with colors, would rule out the fast path for middle parts).

**Every pixel is processed exactly once** – also at the corners of `VDR` and for lines. Thus doing the same thing twice with c = 2 gives the original image again.

### 1.4 Geometry and limits (no clipping)

- There is **no clipping**, as with all VDC-BASIC commands. An x beyond the row width lands in the next row, addresses wrap from `$FFFF` to `$0000`. Rationale: speed and size; the engine only draws into its viewport. Stated in the reference.
- `VDL`: all four coordinates 0–16383, otherwise error 14 (the error term then fits into 16 bits with sign, 4.3). This amply covers bitmap 640 × 800 and chunky 160 × 400.
- `VDR`/`VDB`: x, y, w, h 0–65535 with `x + w ≤ 65536` and `y + h ≤ 65536`, otherwise error 14. w = 0 or h = 0: draw nothing, no error.
- Missing parameters: error 11.

## 2. Memory, configuration, IRQ

- The drawing commands read no CPU RAM except their own state; they run in the configuration of `execute_instruction` (`$FF00 = 0`, I/O on, `io_on`), without `remember_mem_conf`. The code is below `$4000` (P2a).
- Zero page: `arg1`–`arg6` (`$83–$8E`) for parameters and intermediate values, `$0A–$14` (`vcl_command_id`, `multi1`, `multi2`) for multiplication and error term. None of this has to be preserved beyond the command. Data part: `VDM` state (5 bytes), buffer `draw_buf` (64 bytes) for XOR middle parts.
- IRQ: like `RXV` (P2b, 3.6): I flag unchanged, no interrupt routine may use the VDC.
- Reg 24 bit 7 = 0 (block fill) via read-modify-write, only if a middle part is written via block fill.

## 3. Basic operations (assembler)

| Routine | Input | Effect |
|---|---|---|
| `draw_addr` | x, y | byte address per 1.2: `y · stride` (16 × 16 → 16 bits, shift-and-add loop over the bits of y, at most 14 rounds), plus `base`, plus `x >> 3` or `x >> 1`; also bit mask or nibble side |
| `vdc_seek` | address | write reg 18/19 (`AY_to_vdc_regs_18_19`) |
| `pix_rmw` | address, mask | `vdc_seek`; read reg 31 (address +1); modify byte; `vdc_seek`; write reg 31. Modify: mode 0 `ORA m` / `AND ~m` / `EOR m`; mode 1 `(b AND keep) ORA (cc AND m)` with `cc = c · 17` |
| `vdc_run` | value, length L ≥ 1 (16 bits), reg 31 selected, address set | L < T (`$1C04`): CPU loop via reg 31; otherwise first byte via reg 31, rest with `vdc_do_YYAA_cycles` (block fill). Same decision as `RXV`; `RXV` may keep its own inline version, `vdc_run` is for the drawing commands |
| `xor_run` | length m ≥ 1, address set | in chunks of ≤ 64 bytes: remember address, read m bytes via reg 31 into `draw_buf`, address back, write bytes with `EOR #$FF` |

VDC read: after writing reg 18/19, reg 31 returns the byte at the address and increments the address (like `vram_AAYY_to_A` in v2h).

### 3.1 Horizontal span `span(x0, x1, row)` (x0 ≤ x1)

Mode 1 (chunky), bytes `b0 = x0 >> 1`, `b1 = x1 >> 1`:

| Case | Flow |
|---|---|
| `b0 = b1`, x0 = x1 | one nibble: `pix_rmw` |
| `b0 = b1`, x0 even, x1 = x0 + 1 | one full byte: `vdc_seek`, reg 31 = cc |
| `b0 < b1` | `vdc_seek(b0)`; x0 odd → left edge: read reg 31, `vdc_seek(b0)`, replace low nibble, write; middle part `m = (number of full bytes)` via `vdc_run(cc, m)` without a new address; x1 even → right edge at the now current address `b1`: read, `vdc_seek(b1)`, replace high nibble, write |

Mode 0 (bitmap), `b0 = x0 >> 3`, `b1 = x1 >> 3`, left mask `lm = $FF >> (x0 AND 7)`, right mask `rm = ($FF << (7 − (x1 AND 7))) AND $FF`:

| Case | Flow |
|---|---|
| `b0 = b1` | one byte with `lm AND rm`: `pix_rmw` |
| `b0 < b1` | left edge only if `lm ≠ $FF` (otherwise the byte belongs to the middle part); middle part: c = 1 → `vdc_run($FF, m)`, c = 0 → `vdc_run($00, m)`, c = 2 → `xor_run(m)`; right edge only if `rm ≠ $FF` |

Addresses: at most three `vdc_seek` per span (start, after reading the left edge, after reading the right edge). m = 0 is allowed (two edges next to each other).

### 3.2 Vertical run `vrun(x, y0, y1)` (y0 ≤ y1)

Address and mask once with `draw_addr`, then per row `pix_rmw` and address + stride (16 bits). A vertical run always touches only one bit or nibble per byte; a block fill does not help here. The plan ("increment = stride") is thereby implemented: the address is not recomputed, only advanced.

### 3.3 `VDB` (filled)

Compute edges, masks and m once from x and w, then for each of the h rows: `vdc_seek(row address)`, edges and middle part as in 3.1, row address + stride. For full bytes without edges (mode 1 with x and w even; mode 0 with x and w divisible by 8, c ≠ 2) this is one `vdc_seek` and one `vdc_run` per row – the same effort as `VMF` with repetitions, without the BASIC parser per row.

### 3.4 `VDR` (outline)

In this order, every pixel exactly once:

1. top row: `span(x, x+w−1, y)`
2. if h ≥ 2: bottom row `span(x, x+w−1, y+h−1)`
3. if h ≥ 3: left column `vrun(x, y+1, y+h−2)`
4. if h ≥ 3 and w ≥ 2: right column `vrun(x+w−1, y+1, y+h−2)`

Pixel count for w, h ≥ 2: `2w + 2h − 4`.

### 3.5 `VDL` (line, Bresenham with run output)

**Binding pixel set** (the reference model, 5, is the definition):

```
dx = |x2 − x1|, dy = |y2 − y1|
if dx ≥ dy:                            # horizontal dominates
    if x1 > x2: swap endpoints
    sy = +1 if y2 > y1, else −1
    e = 2·dy − dx; y = y1
    for x = x1 … x2:
        plot(x, y)
        if e > 0: y += sy; e −= 2·dx
        e += 2·dy
else:                                  # vertical dominates, roles of x and y swapped
    if y1 > y2: swap endpoints
    sx = +1 if x2 > x1, else −1
    e = 2·dx − dy; x = x1
    for y = y1 … y2:
        plot(x, y)
        if e > 0: x += sx; e −= 2·dy
        e += 2·dx
```

Properties: `max(dx, dy) + 1` pixels, 8-connected, both endpoints included, each pixel once. Because of the swapping the result does not depend on the order of the endpoints. With coordinates ≤ 16383, |e| ≤ 32766 (16 bits with sign).

**Output in runs (run slicing):** The assembler computes the error term as above per pixel (a few CPU cycles), but does not write per pixel, rather per run:

- horizontal dominates: pixels with the same y form a horizontal span → `span(x_start, x, y)` at the row change and at the end. dy = 0 is a single span with a block fill middle part.
- vertical dominates: pixels with the same x form a vertical run → `vrun(x, y_start, y)`. dx = 0 is a single run.
- The row address is advanced by ±stride, not multiplied anew.

A true run slicing with division (run lengths without a step per pixel) is allowed as a later optimization if the tests (same pixel set) stay green. The effort per pixel for the error term (≈ 15 cycles) is small compared with a VDC access.

## 4. Errors

| Case | Error |
|---|---|
| parameter missing, comma missing, too many parameters | 11 `?SYNTAX ERROR` |
| `mode` ≠ 0/1, `stride` = 0 | 14 |
| `c` outside 1.3 for the current mode | 14 |
| `VDL` coordinate > 16383 | 14 |
| `x + w > 65536` or `y + h > 65536` | 14 |

Checks happen before the first VRAM access; on an error the VRAM stays unchanged. Order with several causes: 11.2.

## 5. Python reference model `test/ref/vdcdraw.py`

Standard library only. The pixel set and all byte results are defined here; assembler and docs follow it.

```python
class VdcError(Exception):          # .code = BASIC error number (11, 14)
class Vdc:
    def __init__(self, vram=None)     # vram: bytes(65536) or None (zeros)
    ram: bytearray                    # 65536 bytes
    mode, base, stride                # state as after installation: 0, 0, 80
    def vdm(self, mode, base, stride)
    def vdl(self, x1, y1, x2, y2, c)
    def vdr(self, x, y, w, h, c)
    def vdb(self, x, y, w, h, c)
def line_pixels(x1, y1, x2, y2) -> list[tuple[int, int]]   # 3.5, in drawing order
def rect_pixels(x, y, w, h) -> list[tuple[int, int]]       # 3.4
```

- All commands check as in 4 and raise `VdcError` before any change.
- Set pixels via a single function `_plot(x, y, c)` per 1.2/1.3; addresses modulo 65536.
- The model does not know T: the result does not depend on T.

Self-tests `test/ref/test_vdcdraw.py` (unittest, `python3 -m unittest discover -s test/ref`):

| Check | Content |
|---|---|
| small images by hand | per mode: point, horizontal and vertical line, 45°, line (0,0)–(4,1) = pixels (0,0),(1,0),(2,0),(3,1),(4,1) (recomputed per 3.5), `VDR 1,1,3,3`, `VDB` with odd edges; expected bytes as literals in the test |
| lines | for all pairs in a 9 × 9 grid: pixel count `max(dx,dy)+1`, 8-connected, endpoints included, no duplicates, swapped endpoints = same set |
| XOR | `vdl`/`vdr`/`vdb` with c = 2 twice = original image (random image) |
| outline | `len(rect_pixels)` = `2w+2h−4` for w, h ≥ 2; w = 1 or h = 1 = a line |
| errors | all cases from 4 raise with the right code and leave `ram` unchanged |
| wrap | `base = $FFF0`: pixels land at `$0000+` |

## 6. Tests assembler against model

### 6.1 Harness `test/draw/run_draw.py`

Structured like `test/rxv/run_rxv.py` (P2b, 7.1), shared parts in `test/vramtest.py` (base pattern, breakpoints `$1BFE` prepare / `$1BFF` check, VRAM dump via `bank vdc`) and `test/vicemon.py` (port rule P2a 6.4):

1. Cases from `test/draw/cases.py`; per case a sequence of commands (`VDM`, `VDL`, `VDR`, `VDB`) and a T.
2. Driver `drawt.bas` (generated): per case `SYS DEC("1BFE"):POKE DEC("1C04"),T`, the commands from `DATA` (command id + parameters, executed via `ON K GOSUB`), `SYS DEC("1BFF")`.
3. Preparation: base pattern `random.Random(13)` into the VRAM; check: VRAM dump against `Vdc(base pattern)` with the same commands, bytewise over all 64 KB. Deviation: case, first address, actual/expected, exit code 1.
4. Cycles per case from the monitor stopwatch between breakpoints on `draw_begin` (after parsing) and `draw_end`; output `test/draw/cycles.md`, comparison with 7 (warning, not an error).

The driver runs interpreted; additionally once as a compiled program (P2a, 5.4), the result must be the same.

### 6.2 Cases

| Group | Cases |
|---|---|
| S1 spans chunky | `VDB x,y,w,1,c` for x = 0…3, w = 1…12, c ∈ {0, 7, 15}; middle part m = T−1, T, T+1 for T ∈ {0, 2, 14, 255} (x even and odd); w = 160 (full row) |
| S0 spans bitmap | x = 0…7, w = 1…24, c ∈ {0, 1, 2}; m around T as S1; w = 640 |
| V vertical | `VDL x,y0,x,y1,c` both modes, x with all bit positions or both nibbles, length 1, 2, 100, endpoints in both orders |
| L lines | 32 endpoints on a circle (radius 37) around (80, 50) or (320, 100): all octants; dx = dy, dx = dy ± 1; dy = 1, dx = 300 (bitmap, long spans); dx = 1, dy = 150; single point; every line also with swapped endpoints; c = 2 twice |
| R `VDR` | w, h ∈ {0, 1, 2, 3, 10} all combinations, both modes; c = 2 (corners once) |
| B `VDB` | odd/even edges; 70 × 96 chunky (middle part via fill); 640 × 200 bitmap; w = 0, h = 0 |
| G geometry | stride 160, 37, 255, 300; odd base; base `$FFC0` with wrap; x beyond the row (x = 200 at stride 80, chunky) |
| Z random | 200 commands from `random.Random(11)`: mode, command, position, size (small and large), c, T |

Error cases run in the normal run (`test/run.sh`, interpreted and `-c`):

| File | Content |
|---|---|
| `draw-errors.bas` | all cases from 4 via `TRAP`, per case check `ER` and check via `VMD` at one location that nothing was drawn |
| `draw-basic.bas` | one small case per command with `VMD` check (smoke test without monitor) |

## 7. Cycle targets (CPU cycles at 2 MHz, without BASIC parser)

| Path | Target | Remark |
|---|---|---|
| `draw_addr` (multiplication, mask) | ≤ 250 | once per command or per `VDR` side |
| `pix_rmw` | ≤ 130 | 2 × `vdc_seek` + read + write |
| span, fixed part with two edges | ≤ 330 | plus middle part: 15 per byte below T, above that block fill |
| vertical run | ≤ 140 per pixel | `pix_rmw` + 16-bit addition |
| diagonal line (≈ 45°) | ≤ 160 per pixel | error term + `pix_rmw` |
| `VDB` full bytes | ≤ 60 per row + middle part | `vdc_seek` + `vdc_run` |
| `VDB 0,0,140,96,c` chunky | ≤ 96 × (60 + fill of 70 bytes) | clear the engine's viewport |

Comparison: one pixel in BASIC with `VMA` + `VMO` costs two commands with parser, about 10 000 cycles. Values from VICE are only to be read relatively because of the coarse VDC model (P0, section 2).

## 8. Size

Estimate per part (bytes):

| Part | Bytes |
|---|---|
| parameters (5 values, shared for `VDL`/`VDR`/`VDB`) + range checks | 90 |
| `VDM` + state | 50 |
| `draw_addr` (multiplication, masks) | 70 |
| `pix_rmw` (both modes, set/clear/XOR) | 70 |
| `vdc_run`, `xor_run` + buffer 64 | 60 + 64 |
| span (both modes) | 220 |
| vertical run | 60 |
| `VDL` (both directions, 16-bit error term, run output) | 260 |
| `VDR`, `VDB` | 140 |
| **Sum P2c** | **≈ 1 100** |

Total v3: v2h 2 294 + head 16 + stubs/tables ≈ 20 + `RXV`/VCL 8 ≈ 400 (P2b) + P2c ≈ 1 100 → **≈ 3.8 KB** of 9 216 bytes (`$1C00–$3FFF`). Reserve ≈ 5.4 KB. The overall check from P2a aborts the assembly if the estimate tips over.

## 9. Docs

| File | Change |
|---|---|
| `docs/reference/commands.md` | new section "Drawing": `VDM`, `VDL`, `VDR`, `VDB` with pixel/nibble mapping (1.2, with brief rationale), color/XOR, no clipping, limits, errors; overview; memory (state, buffer) |
| `BASIC V7.0 VDC.txt` | four tokens |
| `README.md` | command list, token table, "Known limits" (no clipping, no XOR in mode 1), delete section "Unfinished parts" |
| `test/README.md` | reference model, `run_draw.py` |
| Guide | chapter on it with P8 (bitmap chapters 4/10 and chunky chapter 5 can refer to `VDL`/`VDB`) |

## 10. Acceptance

| Check | Expectation |
|---|---|
| `test/ref/test_vdcdraw.py` | green |
| `test/draw/run_draw.py` | all cases 6.2 byte-identical with the model over 64 KB interpreted, as a compiled program with `--quick` (11.4) |
| T independence | S0/S1/B the same with all T |
| `test/run.sh test/*.bas` and `-c` | all PASS (incl. `draw-errors.bas`, `draw-basic.bas`) |
| Cycles | `cycles.md` generated, deviations from 7 explained |
| Size | P2c ≤ 1 400 bytes, overall end ≤ `$3FFF` |
| Docs | 9 complete |

## 11. Decisions from the reference model

As of 2026-11-25, review of the model `test/ref/` (Opus). Binding; sections 1–6 apply where nothing else is stated here.

### 11.1 Pixel set and output

- `line_pixels` is the classic Bresenham (Wikipedia `plotLineLow/High`): step only at `e > 0`, on a tie (e = 0) the minor axis stays. After the swap the major axis always runs upward; therefore the set is independent of the order of the endpoints. The run output (3.5) computes the same error term per pixel and only summarizes, so it yields the same set. A later run slicing with division must hit this set exactly (mind the rounding on a tie).
- `rect_pixels` order: top, bottom, left, right (as 3.4). The order is irrelevant for the result because every pixel occurs exactly once (checked via XOR bit counting).
- `VDB` in the model row by row, pixel by pixel; the assembler may choose any order.
- Nibble/bit mapping as 1.2 (bit 7 or high nibble = left pixel), in the model `_plot` and `pixel`.

### 11.2 Parameters and errors

The 6502 version reads every parameter with the ROM routine `getadr` (`b_parse_uint16` `$8812` or `b_parse_comma_uint16` `$880F`, core `$8815`). Looked up in `basichi-318019-04.bin`: negative sign or exponent ≥ `$91` (value ≥ 65536) → `$7D28` (`?ILLEGAL QUANTITY`, 14); otherwise `qint`, fractional digits are truncated. Missing comma: `b_skip_comma` `$795C` → `?SYNTAX ERROR` (11). Numbers 11 and 14 match BASIC 7.0.

Binding from this (model `_args`, then `_num`):

1. Parameters **from left to right**; per parameter: missing → 11, outside 0–65535 → 14. The first error wins.
2. Only when all parameters are read: command-specific ranges (`mode` 0/1, `stride` ≥ 1, `c` per mode, `VDL` ≤ 16383, `x + w ≤ 65536`, `y + h ≤ 65536`) → 14.
3. Examples: `VDL 70000,0` → 14; `VDL 20000,0` (rest missing) → 11; `VDM 2,0` → 11.
4. w, h 0–65535 (w = 65536 is already 14 through step 1). `base` 0–65535, `stride` 1–65535.
5. Non-integer values: the ROM truncates. The model accepts only `int` and otherwise raises `TypeError` (error in the test, not a BASIC error); there are no test cases with fractions. Strings (`?TYPE MISMATCH`, 22) and too many parameters (11) are checked only by `draw-errors.bas`, not by the model.
6. A failed `VDM` leaves the state unchanged.

### 11.3 Test vectors

- Generator `test/ref/gen_vectors.py`; format `vdcdraw-vectors-1` (head of the file). Base pattern `random.Random(13)`, per case `diff` (changed runs) and `sha1` over 64 KB. The S1 middle part widths and the circle rounding (`round`) are fixed as in the generator.
- S0/S1 "mid" cases have **exactly m** full middle bytes (x = 0 without edges, x = 1 or 3 with both edges; checked in the test). The first draft was off by 1 or 2 and would never have hit m = T − 1.
- Every case begins with `VDM`; the state never depends on the previous case.
- The harness imports `gen_vectors.build()` directly (≈ 9 s) and compares the VRAM dump with the model; the JSON file (≈ 600 KB) is not checked in.

### 11.4 Runtime in VICE (recommendation for 6.1)

1 520 cases (S0 645, Z 200, V 192, S1 166, L 157, R 103, B 48, G 12). Per case: load base pattern (`load`, 64 KB, bank `vdc`), commands interpreted, `save` 64 KB, comparison in Python. Estimated 0.2–0.4 s per case in warp → full set 5–10 min per pass, interpreted plus compiled double that.

- `run_draw.py` supports `--group` and `--quick`. `--quick` (`gen_vectors.is_quick`: S0 grid only x ∈ {0, 3, 7}, Z only 000–049) has 1 010 cases and is the default for development runs; the full set for acceptance (10) and only interpreted, the compiled program runs with `--quick`.
- All cases in **one** VICE session (breakpoints `$1BFE`/`$1BFF`, event-driven, timeout only as a guard). No restart per case.
- Comparison over the full 64 KB remains mandatory (finds writes next to the target). `diff` serves only for the error message.


## 12. Findings from the implementation (6502)

As of 2026-11-25, implementation `src/vdcbasic.asm` (zone `draw`), harness `test/draw/run_draw.py`. Result: all 1 520 cases byte-identical with the model over 64 KB (interpreted), `-c --quick` 1 010/1 010, `test/run.sh` both modes green.

### 12.1 Size (deviation from 8 and 10)

The rows below date from before the optimizations; their net effect is in the last row.

| Part | Bytes |
|---|---|
| `VDM` + error exits | 53 |
| `VDL` (distinction, swap, error term, run output, fast paths dy = 0 / dx = 0) | 626 |
| `VDR` | 182 |
| `VDB` | 54 |
| shared: parameters, checks, `.setup`, x range, span, `pix_rmw`, `run`/`xor_run`, multiplication, masks | 938 |
| data: `VDM` state 5, working bytes 38, `draw_buf` 64 | 107 |
| optimizations 1–4 (net), new data `draw_vh`, `draw_majx`, `draw_xs` | +102 |
| **Sum P2c** | **≈ 2 064** |

The optimizations in detail: (1) `pix_rmw` sets reg 18 only when the high byte has changed (`draw_vh`), after reading only resets reg 19 (not at low byte $FF); reg 19 always last: +58 bytes. (2) shared pixel loop `.ppx` for y-major, vertical runs (also VDR columns) and x-major with 2dy ≥ dx; end of `pix_rmw` self-modifying (rts/clc), replaces `.yl` and `.vcol`: +74 bytes. (3) right edge without reg 18 check (`.pix_rmw_19`): ±0 bytes. (4) wrap path via `.seek` and `x+w−1` / `y+h−1` as subroutine `.last`: −30 bytes.

Target ≤ 1 400 missed; vdcbasic3.bin 4 711 bytes, end of v3 ≈ $2E67, 4 505 bytes reserve up to $3FFF (assertion stays). Reasons: every VDC wait loop is inline (`+vdc_sta`/`+vdc_lda`/`+vdc_sty`, 8 bytes, 14 times), `pix_rmw` sets the address twice inline (2 × 29 bytes) instead of via subroutine, and there are dedicated fast paths (line dy = 0 as one span, dx = 0 as vertical run, run within a byte without span calculation). Shorter would mainly be at the expense of `pix_rmw` (seek as subroutine: −60 bytes, +24 cycles per pixel); this was not done. Only the wrap path now uses a `.seek` subroutine (optimization 4).

### 12.2 Cycles (deviation from 7)

Values from `test/draw/cycles.md` (VICE, 1 MHz, R6 = 0, preliminary; authoritative):

| Path | Target | measured | Reason |
|---|---|---|---|
| vertical run per pixel | ≤ 140 | 144 | `pix_rmw` (2 seeks, read, write) plus 16-bit addition of stride and counter; shared pixel loop (optimization 2) and reg 18 check (1) brought −25 (169 → 144), target narrowly missed |
| diagonal line 45° per pixel | ≤ 160 | 196 | one run per pixel: `pix_rmw` + run end + row change ±stride + x step + 16-bit error term; optimizations brought −119 (315 → 196). Target not reachable without a bigger rework (dedicated row loop for VDB) |
| span with two edges per row (`VDB`), bitmap | ≤ 330 | 375 | two `pix_rmw`, plus row address, edge/middle part decision; −37 (412 → 375). Target not reachable without a bigger rework (dedicated row loop for VDB) |
| span with two edges per row (`VDB`), chunky | ≤ 330 | 446 | like bitmap; −32 (478 → 446). Target not reachable without a bigger rework |
| middle part below T per byte | 15 | 15 | as planned |
| `VDB` full bytes per row | ≤ 60 + middle part | 386 at m = 10 (CPU, of which 150 middle part), 309.5 at m = 70 (block fill) | seek, two 16-bit additions for start/end of the row, decisions in `span_draw`, first byte + counter register for block fill |
| `VDR 3,y,10,10,1` total | — | 4 255 (before 4 537) | ten rows with two edges and span each |
| flat line, error term per pixel | ≈ 15 (3.5) | 74 | 16-bit error term, 16-bit counter and x step (bit index, carry into the address) per pixel |

Open, without changing the pixel set: run slicing with division (3.5) for flat lines, pull `.xadv`/`.endrun` into the loop (45°). For 45° and span no target is reachable without a bigger rework (dedicated row loop for VDB). Numbers from real hardware are pending.

### 12.3 Right edge without reg 18 check

3.1 reads the right edge at the address that is current after the middle part. The implementation only resets reg 19 before it (`.pix_rmw_19`, without checking reg 18; ±0 bytes). Hardware assumption: writing reg 19 alone sets the address correctly if reg 18 is right. VICE always reads directly at the address and would not show an error here; to be checked on real hardware. The left edge runs as in 3.1: after writing back, the address is at the first middle byte.

### 12.4 Zero page, parameters, measurement points

- `$0A`–`$14` only after parsing: the ROM's expression evaluator uses this area (`$0D` VALTYP etc.); the loop index when reading the five parameters therefore lies in the data part (`draw_tmp`).
- In addition to 2: `$26`–`$2A` (working bytes of `VCL`/`VMS`, free during a drawing command) for `draw_d2`, `draw_d2m`, `draw_sp`, and `arg5` (`c`) after the check as `draw_sa`. Stated in the reference under "Memory used".
- Too many parameters is checked by the command itself (`chrgot` after the last value) before the first VRAM access; BASIC would report the syntax error only after drawing.
- `draw_begin` (after parsing and checking, before the geometry) and `draw_end` are global labels with `RTS` for the stopwatch; every drawing command calls `draw_begin` once and ends with `JMP draw_end` (12 or 3 cycles).
- Reg 24 bit 7 is cleared only at the first middle part via block fill (`draw_r24`), as 2 demands.

### 12.5 Tests

- `draw-errors.bas` is split into 13 files `draw-errors*.bas` (one error per file, Blitz! limit as with `rxv-errors*.bas`). The check "nothing drawn" reads the VRAM after the error directly via `$D600`/`$D601`, because in the compiled program no extension works afterwards. "A failed `VDM` leaves the state unchanged" (11.2, 6.) cannot be checked in the compiled program for the same reason; the code saves the state only after all checks.
- `test/vramtest.py` (shared parts with `run_rxv.py`, 6.1) does not exist yet; `run_draw.py` is built standalone following the pattern of `run_rxv.py`. The commands are not in the driver as `DATA` (1 520 cases do not fit into BASIC memory), but are written per case into the parameter block `$1B00` and read via `PEEK`; T and R6 likewise.
- Runtime: full set interpreted ≈ 30 s, `-c --quick` similar (instead of 5–10 min per 11.4): `bload`/`bsave` via the monitor are fast.
- Timing group C (`--cycles`, 27 paths × 8 × R6 0/25) is not part of `gen_vectors`, but is also checked against the model.
- Z64K version (`run_draw.py --emu z64k`, `test/z64k/zvt.s`, `test/README.md`): per case only the areas changed by the model (with a margin of 32 bytes, at most 16 areas) are checked by CRC and restored instead of all 64 KB; writes next to them are caught by the pre-check of the next case and a total CRC at every 64th case, the culprit case is named by repeating the window with a total CRC per case. Full set (1 520 cases): about 30 min (estimated from 3.5 s per case and start) → 3 min 25 s, green. The comparison over the full 64 KB per case remains in the VICE version (mandatory above).
- P2b review: `vcl-rxv-twice.bas` (two type 8 entries in a row, both with block fill) and `vcl-rxv-vms.bas` (type 12, 8, 12, 8: change of reg 24 bit 7) added, both modes green.
