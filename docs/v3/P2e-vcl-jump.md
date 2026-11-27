# P2e – VCL entry type 10: continue at address

## 1. Purpose

Dungeon Boring Engine 0.2 builds its VCL list downward from the end of the buffer (it walks through the view from front to back, but the list has to paint from back to front). The start of the list therefore moves with every frame, and the BASIC program has to read it for `VCL` (a `PEEK` calculation, expensive in BASIC). With a jump entry the engine writes a reference to the real start at a fixed location (the start of the buffer); the game always calls `VCL` with the same address. In general: a fixed entry address for a list that moves, and chaining of several lists.

## 2. Format

| Type | Bytes | Parameter | Effect |
|---|---|---|---|
| 10 | 3 | address (word, low byte first) | the next entry is read from *address* (bank 0) |

No BASIC equivalent. Nothing is drawn, the memory state (`$FF00`) stays as with every entry. The same rules apply to the target address as to the start address of `VCL` (bank 0, not `$D000–$DFFF`). A jump back into the list itself never ends (not checked). Type 11 and everything above 13 remain unknown (`INVALID`).

## 3. Implementation

`src/vdcbasic.asm`, dispatcher in `.vcl_next_command`: after the `< 13` check, `CMP #10` compares; for 10 the code reads the two bytes after the type byte (`offset_1` is 0 there, Y = 0) and sets `arg_address` to them, then continues with `.vcl_next_command` (which adds `offset_1` = 0). The code sits in the dispatcher itself because the short jumps to `.vcl_syn` cannot tolerate any more distance. Entry 10 of the `vcl_parameter_bytes` table stays unused (0).

Size: `vdcbasic3.bin` 4753 → 4771 bytes (+18, code only), end `$2EA0` (previously `$2E8E`; the Z64K helper `zvt` at `$2F00` stays free, the `!if * > $4000` check is unchanged). Header unchanged: `$1C00` `JMP $1C10`, `$1C03` = `$30`.

## 4. Cycles

VICE (`x128 -80col -VDC64KB`, 1 MHz), monitor stopwatch between two `SYS` markers with the I flag set: `VCL` over 200 chained jumps (3 bytes each, each to the next, then end) versus `VCL` on an empty list, three times: 212–217 cycles per jump. This is essentially the bookkeeping every entry has (`remember_mem_conf`, clear parameters, read type); the jump itself accounts for about 30 cycles of that. Measurement program: `build/p2e/` (not in the repository).

## 5. Tests

| Test | Checks |
|---|---|
| `test/vcl-jump.bas` | Chain `$A000` → `$A100` → `$1300` (also below `$4000`); the bytes after a jump are not read; jump as the first entry, jump to a jump, end byte at the target; list of 300 bytes before the jump (handling of `offset_1` across the page boundary). Control experiment: with the old `vdcbasic3.bin` the test ends with 10 (`INVALID`). |

Result: see section 6.

## 6. Result

Implemented as in 3, +18 bytes, end `$2EA0`, header unchanged. Tests: VICE `test/run.sh test/*.bas` 49/49 interpreted and `-c` 49/49 compiled (Blitz!); Z64K `test/run.sh -z test/vcl-*.bas` 14/14 interpreted and `-z -c` with `vcl-jump.bas`, `vcl-with-vms.bas`, `vcl-vms-bank1.bas` 3/3 (two runs aborted on the first pass with rc 125, a crash of the Java launcher with three parallel starts; the repeat was green). Users: dungeonboringengine P6j (fixed list address, `BUFSET` X bit 0) and Crawl 128 1.1.
