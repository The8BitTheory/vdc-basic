# P2b: `RXV src,dst` – unpack VRL from bank 0 into VRAM (+ VCL type 8)

As of: 2026-11-25. Detailed specification for step P2b from `dungeonboringengine/docs/plan-0.2.md` (section 7.2). Format: `dungeonboringengine/docs/specs/P1-vrl.md` (VRL v1), reference `dungeonboringengine/tools/vrl.py`. Prerequisite: P2a (header at `$1C00`). Implementation and review: Opus.

## 1. Command

```
RXV src,dst
```

| Parameter | Range | Meaning |
|---|---|---|
| `src` | 0–65535 | Address of the first token in **bank 0**, independent of `BANK` (like `RTV`, `VMS`, `VCL`) |
| `dst` | 0–65535 | VRAM address of the first destination byte |

- Decision on review-notes D.7: `src` points to the **stream**, not to the file header. For a VRL file: `BLOAD"BILD.VRL",B0,P(BE):RXV BE+5,ZIEL` (header `"VR"`, version, U = 5 bytes). Embedded streams (DTS, P5) go without a header. There is no second form; the header is not checked.
- Token `$FE $41` (P2a, 2.2). Entry in `BASIC V7.0 VDC.txt`: `RXV;fe41;`.
- After the command the header (P2a, 2.1) contains: `$1C06/$1C07` = address after the end token (for chaining streams), `$1C08/$1C09` = VRAM address after the last written or skipped byte. Both low/high.

## 2. Memory and registers

| Name | Location | Content |
|---|---|---|
| `rxv_ptr` | `arg1` (`$83/$84`) | Stream pointer (base), advanced after every ≥ 128 bytes read |
| Y | register | Offset in the stream relative to `rxv_ptr`, always < 128 at the start of a token |
| `arg2` (`$85/$86`) | | `dst` at the start, afterwards scratch cell |
| `rxv_cnt` | `arg3` (`$87/$88`) | Counts (L − 1 for block fill, skip length) |
| `rxv_len` | `arg4` (`$89/$8A`) | Length of a long run |
| `rxv_y` | `arg5` (`$8B`) | Save of Y around `vdc_do_YYAA_cycles` and skip |
| `rxv_ttok` | variable in the data part | Token threshold for short runs (3.2) |
| `RXV_T` | **`$1C04`** | Threshold T (5) |

Memory configuration: the whole command runs with `$FF00 = $3E` (RAM 0 + I/O, like `VMS`), at the end the remembered configuration is written back (`complex_instruction_shared_exit`). Consequence: the stream must **not** lie in `$D000–$DFFF` (the CPU sees I/O there) and not in `$FF00–$FF04`. No check for this (cost); stated in the reference.

VDC registers:

| Reg | Use |
|---|---|
| 24 | once per call bit 7 = 0 (block **fill**), read-modify-write, all other bits remain (bit 6 = reverse) |
| 18/19 | Destination address: write once at the start; on skip **read**, add n, write; at the end read (for `$1C08`) |
| 31 | Data: literals, CPU runs, first byte of a block fill. Stays selected between tokens (`$D600 = 31`) |
| 30 | Block fill: number of further bytes |

## 3. Algorithm

### 3.1 Flow

```
rxv:                                   ; from BASIC
    jsr b_parse_uint16       -> arg1   ; src
    jsr b_parse_comma_uint16 -> arg2   ; dst
    jsr remember_mem_conf               ; $FF00 = $3E, old configuration remembered
rxv_execute:                            ; entry for VCL: arg1 = src, arg2 = dst, $FF00 = $3E
    Reg 24: A = Reg24 AND $7F, write back
    rxv_ttok = min(RXV_T + $7E, $FE)    ; 16-bit safe: on carry $FE
    Reg 18/19 = arg2 (AY_to_vdc_regs_18_19: A = high, Y = low)
    $D600 = 31
    Y = 0
next:
    if Y >= $80: rxv_ptr += Y; Y = 0    ; cpy #$80 / bcc tok
tok:
    b = (rxv_ptr),Y; Y++                 ; lda (arg1),y / iny / tax
    b = $00          -> end
    b = $01..$7F     -> lit (X = n = b)
    b = $80..$FD     -> run
    b = $FE          -> skip
    b = $FF          -> lrun

lit:                                     ; X = n, 1..127
    repeat X times: A = (rxv_ptr),Y; Y++; wait until VDC ready; $D601 = A
    -> next

run:                                     ; X = b, L = b - $7E (2..127)
    A = (rxv_ptr),Y; Y++                 ; value v
    if b < rxv_ttok:                     ; L < T  -> CPU
        X = b - $7E
        -> cpu_run
    else:
        rxv_cnt = b - $7F (= L - 1), high = 0
        -> fill

lrun:                                    ; $FF lo hi v
    rxv_len = word; A = v; Y += 3
    if rxv_len.hi = 0 and rxv_len.lo != 0 and rxv_len.lo < RXV_T:
        X = rxv_len.lo; -> cpu_run
    rxv_cnt = rxv_len - 1 (16 bit; L = 0 means 65536 -> rxv_cnt = $FFFF)
    -> fill

cpu_run:                                 ; A = v, X = L >= 1
    repeat X times: wait until VDC ready; $D601 = A
    -> next

fill:                                    ; A = v, rxv_cnt = L - 1, reg 31 selected
    wait until VDC ready; $D601 = A      ; first byte (sets reg 31 = v for the fill)
    rxv_y = Y
    A = rxv_cnt.lo; Y = rxv_cnt.hi
    jsr vdc_do_YYAA_cycles               ; selects reg 30; writes lo (if != 0), then hi times 0 (= 256)
    $D600 = 31; Y = rxv_y
    -> next                              ; do NOT wait for the end: the VDC fills while the CPU reads the next token

skip:                                    ; $FE lo hi
    n = word; Y += 2; rxv_y = Y
    hi = reg 18 (vdc_reg_X_to_A waits for ready -> a running fill is done)
    lo = reg 19
    (hi:lo) += n (mod 65536; n = 0 means 65536 = unchanged, follows by itself)
    Reg 18/19 = (hi:lo)
    $D600 = 31; Y = rxv_y
    -> next

end:
    $1C06/$1C07 = rxv_ptr + Y            ; after the $00
    $1C09 = reg 18, $1C08 = reg 19       ; waits for ready
    jmp complex_instruction_shared_exit
```

"Wait until VDC ready" is `bit $D600 / bpl *-3` (macro `+vdc_sta`). Every data operation waits by itself; as a result a block fill overlaps with reading the next token ("issue", P0 case 5).

### 3.2 Threshold in the token comparison

Short run: L = b − `$7E`, so L < T holds exactly if b < T + `$7E`. `rxv_ttok` is computed once per call; the comparison per token is then a `cpx rxv_ttok`. For T ≥ 128, `rxv_ttok` = `$FE` (all short runs via CPU). T = 0 or 1: all runs via block fill (a run with L = 1 then writes only the first byte, without reg 30).

Long run: via CPU exactly if L ≤ 255 and L < T. Runs ≥ 256 always go via block fill.

### 3.3 Reg 30 = 0

`vdc_do_YYAA_cycles` (from `vdclib.a`, also used by `VMF`/`VMC`) writes reg 30 = 0 for whole pages and relies on the VDC then filling 256 bytes.

- VICE 3.10: confirmed (`vdc-mem.c`, `vdc_perform_fillcopy`: `blklen = regs[30] ? regs[30] : 256`).
- Real hardware: described this way in the literature, `VMF` with more than 256 bytes has worked this way since 2013. **To be verified** on a C128 with the test `vdc-reg30-zero.bas` (7.4); the listing also prints the result as text and runs without a debug cartridge.
- If the hardware deviates, this affects `VMF`/`VMC` equally; remedy then centrally in `vdc_do_YYAA_cycles` (chunks of 255), not in `RXV`.

### 3.4 Skip via reading reg 18/19

The decoder does not track the destination address in software (saves about 20 cycles per token). On skip it reads the current address from reg 18/19. After reg 31 write accesses and after a block fill, the address after the last byte is there (VICE: `vdc-mem.c`, both paths update `regs[18/19]`).

**To be verified on real hardware:** readability of reg 18/19 after block fill and after reg 31 writes. Test: `rxv-hw.bas` (7.4, vectors with skip directly after literal and directly after fill). Fallback if the hardware deviates: build switch `RXV_TRACK_DST = 1` – the decoder adds the length of each literal/run to `arg2` and on skip writes `arg2 + n` (16 bit) without reading; `$1C08` then comes from `arg2`. Both variants must pass the same tests.

### 3.5 Limits and error behavior

| Case | Behavior |
|---|---|
| `src`/`dst` missing, comma missing, third parameter | `?SYNTAX ERROR` (error 11) |
| Value outside 0–65535 | `?ILLEGAL QUANTITY ERROR` (14) |
| Stream without `$00`, invalid lengths at the end of memory | not checked: the decoder keeps reading (pointer wraps from `$FFFF` to `$0000`), result undefined |
| `src` points to the file header instead of the stream | not detected (`"V"` = `$56` is read as 86 literals) → reference warns |
| Stream in `$D000–$DFFF` or `$FF00–$FF04` | reads I/O or MMU → undefined |
| Destination wraps past `$FFFF` | VRAM address wraps to `$0000` (VDC behavior); on a 16 KB VDC the memory mirrors |
| STOP key | not polled |
| `T` (`$1C04`) | any value 0–255 allowed (3.2); the result in VRAM does **not** depend on T, only the time |

### 3.6 IRQ

`RXV` does not change the I flag (like `RTV`, `VMC`). Interrupts stay on during unpacking; the Kernal IRQ routine also runs with `$FF00 = $3E` (entry in RAM at `$FF05`). Prerequisite as for all VDC-BASIC commands: no interrupt routine accesses the VDC (it would switch `$D600`, after which the data bytes would go into a wrong register). Already stated in the reference ("Conventions"), add `RXV` there.

## 4. VCL type 8 = `RXV`

| Byte | Content |
|---|---|
| 0 | 8 |
| 1–2 | `src` (bank 0) |
| 3–4 | `dst` (VRAM) |

Length 5 bytes. Implementation in `.vcl_next_command`: `vcl_parameter_bytes[8]` = 3 (count − 1); in the branch `.vcl_check_vms` before the check for 12: `cmp #8 / beq .vcl_rxv`; `.vcl_rxv: jsr rxv_execute : jmp .vcl_next_command`. `rxv_execute` uses `arg1`–`arg5` (cleared by VCL per entry) and does not touch `arg_address`, `offset_1`, `vcl_saved_cr`. The memory configuration is already `$3E` in VCL (`remember_mem_conf` per entry), `complex_instruction_shared_exit` restores exactly this – as for types 1–5.

Previously type 8 was reserved for "VMO with 2 parameters" (comment in the source, `vcl-format.md`). Type 8 becomes `RXV`; 9–11 remain reserved. Adjust comment, `vcl-format.md` and README ("Known limits").

Cost: a VCL entry costs about 270 CPU cycles up to the call (reading 5 bytes ≈ 150, clearing arguments ≈ 90, branching ≈ 30) compared to several thousand cycles for parsing `RXV S,D` in BASIC. For many small streams (tiles, partial images) therefore use VCL.

## 5. Threshold T (`$1C04`)

- Documented, patchable byte (review-notes D.8): `POKE DEC("1C04"),T` in BASIC (`BANK 15` suffices, the cell lies below `$4000`), likewise in a compilate. Affects `RXV` and the middle parts of `VDL`/`VDR`/`VDB` (P2c).
- Default value in the binary: **14** (placeholder from the cost model of P1, `vrl.py threshold()` with the placeholders). Replace with T from the P0 measurement on **real hardware** (`test/bench/results/c128-*.json`, `engine.T`); T from VICE is not transferable (P0, section 2). In the source as `RXV_T_DEFAULT = 14 ; vorläufig, siehe docs/v3/P2b-rxv.md 5`.
- No fake register via `RGW`: `RGW` writes VDC registers; a pseudo register 253 would be special handling in the fast path. `POKE` suffices.

## 6. Cycles (CPU cycles, VDC immediately ready)

Recounted from the listing (`src/vdcbasic.asm`, zone `rxv`, `RXV_TRACK_DST = 0`) (state of implementation) and compared with VICE. Column "VICE": `test/rxv/cycles.md`, 1 MHz, R6 = 0 (no visible rows, the CPU does not wait for the VDC), IRQ disabled, minimum of 8 runs – **provisional**, see below.

| Path | Listing fixed | Listing per byte | VICE | Remark |
|---|---|---|---|---|
| Read token + branch (`tok`) | 18 (literal) / 19 (otherwise) | – | – | `cpy #$80 / bcc`, `lda (zp),y / iny / tax / beq / bmi`; advancing `rxv_ptr` 19 (+ 4 on carry) per ≥ 128 stream bytes |
| Literal | 20 (incl. `tok`) | 22 (+1 on page change in the stream) | 18.9 fixed, 23.1 per byte | `lda (zp),y / iny / bit / bpl / sta / dex / bne` |
| Run L < T via CPU | 42 (incl. `tok`) | 15 | 42.0 fixed, 15.0 per byte | `cpx #$fe / cpx #T+$7e` (self-modified) / compute L / read value |
| Run L ≥ T via block fill (short run) | 118 (incl. `tok`) up to reg 30 | VDC | 119 (L = 14, incl. waiting at the end) | afterwards the VDC works alone; the CPU waits only at the next data access |
| long run (`$FF`), L ≥ 256 | 136 (incl. `tok`) | 15 per further full page + wait time | 269 (L = 300) | the CPU waits per page for the previous fill |
| Skip | 186 (incl. `tok`) | – | 186 | 2 register reads, 16-bit addition, 2 register writes, `$D600 = 31` |
| End | 118 (incl. `tok`, `complex_instruction_shared_exit`) | – | – | pointer into the header, read R18/R19 |
| Call setup (`rxv_execute`) | 147 | – | – | without BASIC parser; setup + end measured 251 (stopwatch without the 12 cycles of the exit) |
| VCL entry type 8 up to `rxv_execute` | ≈ 270 | – | – | estimated (4) |

For `vrl.py stats` (P1, 8.2) this yields: `tok` 19, `cpu_lit_byte` 22, `cpu_byte` 15, `cpu_run_fixed` 23 (42 − `tok`), `new_addr` 167 (186 − `tok`), `fill_fixed` 99 + VDC startup, `fill_byte` and `fill_chunk` from P0. Taking these values into the P0 JSON (`engine`) or as placeholders into `vrl.py` is part of the acceptance (separate small commit in the engine repo, after consultation) – **open**.

In VICE the VDC access is modeled in simplified form; cycle measurements in VICE are therefore meaningful only relatively (paths among each other). At 2 MHz (`--fast`) the VDC limits in VICE: literal 23.2 and CPU run 23.8–25.3 CPU cycles per byte instead of 22 or 15 (`cycles.md`). Measurement on real hardware is pending.

## 7. Tests

### 7.1 Harness `test/rxv/run_rxv.py`

- Imports `vrl.py` from `$DBE_TOOLS` (default `~/git/dungeonboringengine/tools`); if missing → abort with message, exit code 2.
- Monitor via `test/vicemon.py` (P2a, 6.4: port 6631–6649, bind test, `-remotemonitoraddress`, only own PID).
- **Step 0 (once, result in 9):** verify that the VICE monitor reads and writes VRAM with `bank vdc` (`m`, `>`, `save`, `load`; source `c128mem.c`: bank `vdc` with `vdc_ram_read`/`vdc_ram_store` present). Fallback: copy VRAM content via `VTR` into RAM 0 and save from there.

Flow (one VICE run for all vectors):

1. Generate vectors (7.2): per vector stream, `src`, `dst`, T. All streams into one file `RXVDATA` (load address `$8000`, streams up to `$CFFF`), special placements (7.2, group P) via monitor directly into RAM.
2. Driver program `rxvt.bas` (generated by the harness, interpreted; compilate additionally via the `-c` path from P2a 5.4):
   ```
   10 POKE DEC("1BFE"),96:POKE DEC("1BFF"),96:BLOAD"RXVDATA",B0,P(32768)
   20 READ S,D,T:IF S<0 THEN 90
   30 SYS DEC("1BFE"):POKE DEC("1C04"),T:RXV S,D:SYS DEC("1BFF"):GOTO 20
   90 BANK 15:POKE DEC("D7FF"),0
   100 DATA …,-1,0,0
   ```
   No `RGO 28,16` needed if `-VDC64KB` is set and the Kernal loads reg 28 appropriately; the harness checks `RGD(28) AND 16` once at the beginning.
3. Breakpoint `$1BFE` (preparation): the harness writes the base pattern (64 KB, `random.Random(7)`, fixed) into VRAM with `bank vdc` + `load`.
4. Breakpoint `$1BFF` (check): `save` VRAM `0000–ffff`, `m 1c06 1c09` (bank RAM). Expectation: base pattern, on top of it `vrl.decode(stream)` written from `dst`, addresses **modulo 65536** (wraparound); `$1C06` = `src + len(stream)`; `$1C08` = `(dst + sum of all literal, run and skip lengths) mod 65536` (from `vrl.decode_tokens`, skips at the end count too). Any deviation: vector name, first deviating address, actual/expected, exit code 1.
5. BASIC error vector `($0300)` as breakpoint → report error.

Output: one line per vector, at the end the count; additionally the time per vector from the monitor stopwatch between breakpoints at `rxv_execute` and `complex_instruction_shared_exit` (information only, table in `test/rxv/cycles.md`).

### 7.2 Test vectors

Stream in hex without file header; "v" = fixed value per vector (e.g. `$A5`). The base pattern is random, comparison is byte by byte over the whole VRAM.

**Group B – basic forms** (T = default)

| Name | Stream | Checks |
|---|---|---|
| `empty` | `00` | nothing written; `$1C06` = src + 1; `$1C08` = dst |
| `lit1` | `01 05 00` | |
| `lit127` | `7F` + 127 bytes + `00` | longest literal |
| `lit128` | `list(range(128))` encoded (`7F … 01 7F 00`) | two literal tokens |
| `zero-data` | `02 00 00 80 00 00` | value 0 in literal and run |
| `foreign` | `80 07 80 07 00`, `FF 01 00 07 00`, `FE 05 00 00` | sequences the encoder never produces |
| `rect` | `encode_rect([1,2,3,4],2,2,80)` = `02 01 02 FE 4E 00 02 03 04 00` | row jump via skip |
| `rect-mask` | `encode_rect([1,2,3,4],2,2,10,mask=[1,0,0,1])` | skip over mask + stride |

**Group R – runs** (each vector with T ∈ {0, 1, 2, 3, 14, 127, 128, 255}; the VRAM result must be the same for all T)

| Name | Stream | Checks |
|---|---|---|
| `run2`, `run3` | `80 v 00`, `81 v 00` | shortest runs |
| `runT-1`, `runT`, `runT+1` | short run with L = 13, 14, 15 (token `$8B`, `$8C`, `$8D`) | boundary at T = 14 |
| `run127` | `FD v 00` | longest short run, `rxv_ttok` cap |
| `lrun1` | `FF 01 00 v 00` | L = 1 via fill path without reg 30 (at T ≤ 1) |
| `lrun128`, `lrun255` | `FF 80 00 v 00`, `FF FF 00 v 00` | long runs < 256: `lrun128` at T = 255 via CPU, `lrun255` always via fill (255 < T never satisfied) |
| `lrun256`, `lrun257` | `FF 00 01 v 00`, `FF 01 01 v 00` | reg 30 = 255 or 0 (one full page) |
| `lrun511`, `lrun512`, `lrun513` | `FF FF 01 …`, `FF 00 02 …`, `FF 01 02 …` | partial page + pages |
| `lrun65535` | `FF FF FF v 00` | 255 pages + 254 |
| `lrun65536` | `FF 00 00 v 00` | whole VRAM = v, `$1C08` = dst |
| `fill-fill` | `FF 00 01 07 FF 00 01 09 00` | fill directly after fill (value in reg 31 changes) |
| `fill-lit` | `FF 00 02 07 01 09 00` | literal waits for running fill |
| `fill-run` | `FF 00 02 07 81 09 00` | CPU run after fill |

**Group S – skips**

| Name | Stream | Checks |
|---|---|---|
| `skip-first` | `FE 01 00 01 05 00` | leading skip |
| `skip0` | `FE 00 00 01 05 00` | n = 0 = 65536 → address unchanged |
| `skip-max` | `FE FE FF 01 01 00` with dst = `$0010` | skip 65534 → wraparound to `$000E` |
| `skip-end` | `01 05 FE 05 00 00` | skip before end: nothing written, `$1C08` = dst + 6 |
| `fill-skip` | `FF 00 01 07 FE 10 00 01 09 00` | skip reads 18/19 **during** a fill (waiting) |
| `lit-skip` | `03 01 02 03 FE 02 00 01 04 00` | skip after reg 31 write |
| `skip-skip` | `FE 03 00 FE 04 00 01 05 00` | two skips in a row |

**Group P – placement in RAM**

| Name | src | Checks |
|---|---|---|
| `page-cross-lit` | `$80FE`, literal 127 | stream across a page boundary |
| `norm-edge` | `$8000`: literal 126, literal 127, literal 127, run, `00` | Y reaches 127 → 255 before advancing (3.1 `next`) |
| `below-4000` | `$1300` | stream below `$4000` |
| `below-d000` | stream ends at `$CFFF` | last page before I/O |
| `long-stream` | `$8000`, > 4 KB stream | many advance steps |

**Group Z – destination**

| Name | dst | Checks |
|---|---|---|
| `wrap` | `$FFF0`, literal 32 | wraparound `$FFFF` → `$0000` |
| `dst0`, `dstFFFF` | `$0000`, `$FFFF` | edges |

**Group X – random and real data**

- `rand-*`: 60 sequences from `random.Random(3)`, length 0…3000, alphabet {2, 5, 256} values, `SKIP` share {0, 10, 50} %, long runs interspersed; `vrl.encode`, dst random; T random from {0, 2, 14, 255}.
- `dungeon`: `docs/listings/data/dungeon` (8000-byte image) with `vrl.encode` → dst `$0000`, T default.
- `wall`: a wall image from P3/P5, as soon as available (otherwise omitted).

### 7.3 Tests in the normal run (`test/run.sh`, interpreted and `-c`)

| File | Content |
|---|---|
| `rxv-basic.bas` | small stream via `POKE` at `$A000`, `RXV`, check with `VMD`; `$1C06`/`$1C08` via `PEEK` |
| `rxv-errors.bas` | `RXV` without parameters → `ER` = 11; `RXV 70000,0` → 14; `RXV 1,2,3` → 11 (via `TRAP`) |
| `vcl-rxv.bas` | VCL list: type 8 (`RXV`), then type 6 (`VMW`), then end; checks both effects (list pointer intact after `RXV`) and that `PEEK(DEC("FF00"))` after `VCL` again equals the value before |
| `rxv-config.bas` | `RXV` called from `BANK 0` and `BANK 15`: same effect, `$FF00` afterwards as before; reg 24 bit 6 is preserved |

### 7.4 Hardware listings (run without harness on a C128, output on screen, in VICE additionally with debug cartridge)

| File | Checks |
|---|---|
| `test/vdc-reg30-zero.bas` | Reg 30 = 0 fills 256 bytes: `RGA 24,127`, set address, `RGW 31,v`, `RGW 30,0`; `VMD(a+256)` = v, `VMD(a+257)` unchanged |
| `test/rxv-hw.bas` | the vectors `fill-skip`, `lit-skip`, `lrun257`, `lrun65536` (with screen restore) and `rand-0…4`, expected values as checksum (16 bit, generated by the harness) in the listing |

Z64K version (`run_rxv.py --emu z64k`, 6502 test program `test/z64k/zvt.s`, description in `test/README.md`): per vector only the range `dst .. dst + U - 1` (with margin, wraparound) is checked via CRC and then restored; write accesses outside are caught by the pre-check of the next case and a CRC over all 64 KB at every 64th vector (narrowing down the guilty vector by repeating the window). 238 vectors: 5 min → 1 min 18 s, green. The comparison over 64 KB per vector remains in the VICE version.

## 8. Documentation

| File | Change |
|---|---|
| `docs/reference/commands.md` | new section "Unpacking" with `RXV` (syntax, bank 0, stream instead of header, memory prohibitions, `$1C04`/`$1C06`/`$1C08`, wraparound, no checking); overview table; "Memory used by VDC-BASIC" extended by the header and `arg1`–`arg5` |
| `docs/reference/vrl-format.md` (new) | Token table and file frame VRL v1 (from P1, 2 and 3), reference to the tool |
| `docs/reference/vcl-format.md` | Type 8 = `RXV`, type 9–11 reserved |
| `BASIC V7.0 VDC.txt` | `RXV;fe41;` |
| `README.md` | number of commands, token table, header, version history v3 |
| `test/README.md` | `test/rxv/run_rxv.py`, `DBE_TOOLS`, port rule |

A guide chapter on `RXV` follows with P8.

## 9. Acceptance

| Check | Expectation |
|---|---|
| `test/rxv/run_rxv.py` | all vectors 7.2 byte-identical with `vrl.decode` over the whole VRAM, pointers `$1C06`/`$1C08` correct, exit code 0 |
| T independence | group R identical for all T |
| `test/run.sh test/*.bas` and `-c` | all PASS, incl. 7.3 |
| Size | `RXV` + VCL type 8 ≤ 400 bytes (symbol list) |
| Cycles | table 6 recounted from the listing, `cycles.md` from VICE |
| Documentation | 8 complete |
| Findings | step 0 (monitor bank `vdc`), 3.3/3.4 on hardware noted as open or done |

## 10. Findings

State of implementation 2026-11-25 (VICE 3.10, `x128 -VDC64KB`).

- **Step 0 (monitor bank `vdc`):** done. After `bank vdc`, `m`, `>`, `bload "datei" 0 0000` and `bsave "datei" 0 0000 ffff` read and write the VRAM (64 KB, without load address). The harness uses `bload`/`bsave` instead of `load`/`save` (no two-byte header). RAM 0 via `bank ram00`. The fallback via `VTR` is not needed.
- **R28 at start:** `RGD(28)` = `$2F` (bit 4 = 0) even with `-VDC64KB`; the Kernal does not enable 64 KB addressing. The driver therefore sets `RGO 28,16` (after reading).
- **3.3 Reg 30 = 0 (hardware):** open. Passed in VICE (`test/vdc-reg30-zero.bas`, group R `lrun256`…`lrun65536`). Run `vdc-reg30-zero.bas` on a C128.
- **3.4 Reg 18/19 readable after fill and after reg 31 write (hardware):** open. Passed in VICE (`fill-skip`, `lit-skip`, all random vectors with skips). Run `test/rxv-hw.bas` on a C128. The fallback `RXV_TRACK_DST = 1` is implemented (+28 bytes) and passes the same tests (`run_rxv.py --bin … --sym …`, `VDCBASIC=… test/run.sh test/rxv*.bas test/vcl-rxv.bas`).
- **Result:** `run_rxv.py` 398/398 vectors byte-identical over the whole VRAM, `$1C06`/`$1C08` correct, interpreted and compiled (`-c`); group R identical for all eight T. Control with a deliberately wrong binary: deviations are reported.
- **Size:** `RXV` 267 bytes (`rxv` up to `zzz`), VCL type 8 10 bytes, token and pointers 5 bytes: 282 bytes ≤ 400. Plus stub and table entries `VDL`/`VDR`/`VDB` 20 bytes, `VMS` type 3 (11) 34 bytes. Code ends at `$2654`.

Deviations from this specification:

- `rxv_ttok` is not a variable but the operand of a self-modified `cpx #` (2 instead of 4 cycles per run token). `rxv_len` (`arg4`) is dropped: the long run holds lo in X and hi directly in `rxv_cnt + 1`. The short run via block fill computes `rxv_cnt` = b − `$7F` directly.
- `vrl.py` comes from `tools/` of this repository (canonical copy, commit `4fd03ff`); `DBE_TOOLS` takes precedence if set.
- Harness: the streams do not arrive via a file `RXVDATA`, but per vector via monitor (`bload` at breakpoint `$1BFE`) at their address; the driver reads src/dst/T from a parameter block at `$1BF0` instead of from `DATA`. This way any number of and arbitrarily long streams (group X, `long-stream`) fit without a space problem. For U > 65536 (stream `skip0`: skip 65536 before data), which `vrl.decode` rejects, the harness computes modulo 65536 itself.
- Additionally group C (timing, 2 × 8 × 10 vectors, with R6 = 25 and R6 = 0) and `--fast`.
- `rand-0`…`rand-4` have at most 400 elements (otherwise up to 3000), so that `rxv-hw.bas` stays small.
- `rxv-errors.bas` is split across four files (`rxv-errors.bas`, `-range`, `-extra`, `-one`): in the compilate, extensions no longer work after an error caught with `TRAP` (README, P2a 9).
- `BASIC V7.0 VDC.txt` gets `VDL;fe3e;`, `VDR;fe3f;`, `VDB;fe40;` already alongside `RXV;fe41;` (P2a 2.3: "with P2b/P2c"), because the VDC-BASIC tokenizer now knows them (stub with `?SYNTAX ERROR`, test `vd-stub.bas`).
- `test/rxv/cycles.md` contains 1 MHz and 2 MHz, each with R6 = 0 and R6 = 25.

## 11. VMS list command 3: source skip

User decision (plan 0.2, section 9): sprite clipping at the viewport edge. The engine shortens VMS lists when copying into the buffer; for the omitted entries the read position in the sprite data must nevertheless advance.

| Byte | Content |
|---|---|
| 0 | 3 |
| 1–2 | n (word, low/high) |

Length 3 bytes. Effect: read position (`vms_block_source`) += n (16 bit, wraparound), then R32/R33 = new read position; nothing is written. n = 0 changes nothing. Type 3 was free (existing: 1 copy run, 2 rectangle, 8 half byte); 4–7 and 9+ remain free.

Notes:

- Type 2 does not track the read position (intended only as a sole entry); type 3 therefore only to be mixed with type 1 and 8.
- Cost: about 100 CPU cycles (read list, 16-bit addition, write R32/R33) plus the loop per entry; replaces any number of omitted entries.
- Implementation: `.vms_source_skip` in `src/vdcbasic.asm` (34 bytes incl. branch), jump to the common end of type 8 (`.vms_set_source`).
- Tests: `test/vms-source-skip.bas` (with type 1 and 8, n = 0, n > 255), `test/vcl-vms-source-skip.bas` (inside VCL type 12, list above `$4000`), `test/rxv-examples.bas` (example from the reference).
- Documentation: `docs/reference/vms-format.md` (table, section "Clipping: type 3" with example), README (version history).
