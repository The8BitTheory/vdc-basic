# P2d: VCL type 9 – VMS lists from bank 1

As of: 2026-11-25. Occasion: proposal from the engine (`dungeonboringengine/docs/specs/P6-engine.md`, section 6a): today the engine copies every VMS list from its sprite sets (bank 1) into the buffer in bank 0, about 16 cycles per byte, together ~5–30k cycles per frame. A VCL entry that reads the list directly in bank 1 saves this copy for all unclipped lists. Prerequisite: P2a (VDC-BASIC at `$1C00–$3FFF`), P2b (VMS type 3).

## 1. Entry

| Type | Bytes | Parameters | Effect |
|---|---|---|---|
| 9 | 7 | source, target, list (each a word, low/high) | like type 12 (`VMS source,target,list`), but *list* is an address in **RAM bank 1** |

- List format unchanged ([`vms-format.md`](../reference/vms-format.md)): alpha, count (word), commands 1, 2, 3, 8. Any length, also across page boundaries.
- Location of the list: entirely in bank 1 **`$4000–$CFFF`** or **`$E000–$FEFF`**. Not checked (cost); outside, the result is undefined (below `$4000` the entry reads bank 0, in `$D000–$DFFF` I/O, from `$FF00` on the MMU).
- Unknown VMS command: as with type 12 `INVALID VMS COMMAND`, the VCL list continues with the next entry.
- Interrupts stay on.
- Only via `VCL`, no BASIC form (like type 7).

## 2. Decision: 16 KB common RAM during the entry

### 2.1 The problem of the proposal

The proposal reads the list with `$FF00 = $7E`. But the code of VDC-BASIC is in bank 0 at `$1C00–$2E8E`. With the Kernal default of `$D506` (1 KB common at the bottom, `$0000–$03FF`) the CPU fetches the next instruction from bank 1 after `STA $FF00` – where BASIC variables live: crash. Even the 8 KB of the engine (`$D506` = …`$06`, only during an engine call) do not cover `$2000–$2E8E`.

### 2.2 Chosen approach

For the duration of a type 9 entry, `VCL` switches the common RAM to **16 KB at the bottom** (`$D506` = (old AND `$F0`) OR `$07`). Then bank 0 `$0000–$3FFF` – that is, all of VDC-BASIC with code, variables, zero page and stack – is visible also in configuration `$7E` (RAM bank 1, I/O). The VMS routine runs unchanged with `$FF00 = $7E` instead of `$3E` and reads the list as before with `LDA (arg_address),Y`. Afterwards: `$FF00` back to bank 0, only then `$D506` back (the other way round the code would vanish).

Flow in `.vcl_check_vms` (type 12 and 9 together):

```
push arg_address, offset_1
vcl_saved_rcr = $D506                  ; $FF00 = $3E
$FF00 = vcl_saved_cr; remember_mem_conf ; for k_primm in the error path of VMS, afterwards $FF00 = $3E
A = $3E
if type 9: $D506 = (vcl_saved_rcr AND $F0) OR $07; A = $7E
jsr vms_execute_a                      ; A = configuration for reading the list
remember_mem_conf                      ; $FF00 = $3E (bank 0, I/O)
$D506 = vcl_saved_rcr
pull
```

`vms_execute_a` (new entry point before `vms_execute`, which now only prepends `LDA #$3E`) writes A to `$FF00` and to `saved_mem_conf`. Reason: `vmc_execute` ends with `$FF00 = saved_mem_conf`; commands 1 and 2 previously called `remember_mem_conf` per command (sets `$3E`), which would have switched to bank 0 in the middle of the list. The two calls are dropped, `saved_mem_conf` is set once per list. Side effect: type 12 and `VMS` from BASIC become 26 cycles faster per command 1/2.

### 2.3 Why this is safe

- **Code and data below `$4000`**: VDC-BASIC ends at `$2E8E`; the overall check at the end of the file (`!if * > $4000`, P2a) secures this for type 9 as well (comment added there).
- **Zero page and stack** are always in bank 0 anyway (MMU default `$D507–$D50A`).
- **Interrupts**: an IRQ/NMI at `$FF00 = $7E` takes the vector from bank 1 `$FFFA–$FFFF` and runs through the Kernal interrupt switch in bank 1 (`$FF05` ff.), which sets `$FF00 = 0`. The Kernal copies the switch (`$FF05–$FF44`) and the vectors into RAM bank 1 at reset, independent of the engine (review: from pure BASIC after `BOOT` byte for byte identical to the Kernal ROM; type 9 with 403-byte lists at `$9000` and `$F000`, 300 passes with `MOVSPR` and `PLAY` in the IRQ, green). A list must therefore not overwrite these bytes (limit `$FEFF`). The same switch is already used by the Kernal `FETCH` (`$02A2`, without `SEI`) and by the engine during its calls. The Kernal/BASIC IRQ works in bank 0; common RAM changes nothing there. The test with the 403-byte list takes ~31 000 cycles, so at least one IRQ falls into the window; it runs green.
- **Bank 1 `$0000–$3FFF`** (BASIC variables from `$0400`) is hidden during the entry; during this time nobody reads it (BASIC sits in `VCL`, the IRQ routines do not access bank 1). The engine makes the same assumption with 8 KB.
- **Error path**: `INVALID VMS COMMAND` prints with `k_primm` in the caller's configuration (bank 0, Kernal); the common RAM does not interfere there. Afterwards `VCL` restores `$D506` as usual (test `vcl-vms-bank1-invalid.bas`).
- `$D506` bits 4–7 (VIC bank among others) remain unchanged.

### 2.4 Rejected approaches (cycles)

Today: the engine copies ~16 cycles per list byte (bank 1 → bank 0), VDC-BASIC then reads with `LDA (zp),Y` (5 cycles).

| Approach | Extra cost per list byte compared with type 12 | compared with today (copy + type 12) | Finding |
|---|---|---|---|
| Kernal `FETCH` `$02A2` (like `VMP`) | `LDX #$7F` 2 + `JSR` 6 + body 4+4+2+5+4 + `RTS` 6 = 33 instead of 5 → **+28** | +12 per byte | slower than today, rejected |
| own read stub in always-common RAM | like `FETCH` without `LDX`, ≈ +25 | +9 | no room: `$0000–$03FF` is occupied (Blitz! `$0202–$02A1`, Kernal `$02A2–$03FF`); `$0B00`/`$0E00` are common only with 8 KB, which applies only during engine calls |
| copy blockwise into a bank 0 buffer (from common code) | ≈ +13–16 (copy loop) | ±0 | only a relocation of today's copy, plus buffer and code; rejected |
| **16 KB common** (chosen) | **0** (same instructions) | **−16** | one-time +15 cycles per entry |

## 3. Memory

| Name | Location | Contents |
|---|---|---|
| `vcl_saved_rcr` | data part (`$2E10`) | `$D506` of the caller during a type 9/12 entry |
| `vcl_parameter_bytes[9]` | data part | 5 (6 parameter bytes, like type 12) |

Code size: `vdcbasic3.bin` 4711 → 4753 bytes (+42: code +41, data +1), end `$2E8E` (Z64K helper `zvt` at `$2F00` stays free).

## 4. Cycles (VICE, preliminary)

Measured with the stopwatch of the VICE monitor, IRQ off during the measurement, from the type check in `.vcl_check_vms` to the end of the restoration (that is, including VMS). Lists: n commands of type 1 with length 1 (4 bytes per command), `x128 -80col -VDC64KB`, 1 MHz.

| Case | n = 0 | n = 100 (403 bytes) | per command | per list byte |
|---|---|---|---|---|
| Type 9 (bank 1), new | 392 | 30 634 | 302 | 75.6 |
| Type 12 (bank 0), new | 336–379 | 30 573–30 966 | 302–306 | 75.5–76.6 |
| Type 12, before P2d | 276–319 | 33 430–33 651 | 332 | 83.1 |

- Per list byte, type 9 and type 12 are the same: they are the same instructions. The scatter (a few hundred cycles per run) comes from the wait times for the VDC (block copy, register accesses), not from reading the list.
- Fixed extra cost of type 9 compared with type 12: counted 15 cycles (`LDA`/`AND`/`ORA`/`STA $D506`, `LDA #$7E`, one branch not taken). Both types now pay ≈ 42 cycles per entry for saving/restoring `$D506` and an additional `remember_mem_conf`; in return 26 cycles per VMS command 1/2 are saved.
- Savings for the engine: the copy (~16 cycles per list byte, ~5–30k per frame) is dropped for every list that is output verbatim; also less buffer space (VMSLEN).

## 5. Tests

| File | Checks |
|---|---|
| `test/vcl-vms-bank1.bas` | In a VCL list, type 9 and type 12 alternate, each with the same list in bank 1 and bank 0 respectively. List A (commands 1, 3, 8, source skip 256) with head at `$80FC` (page boundary in the middle of the first command); list B: 100 commands of type 1, 403 bytes (> 256). In bank 0, **different** valid lists (decoys) lie at the bank 1 addresses: if type 9 accidentally reads bank 0, the result changes. Check: fixed expected values, type 9 versus type 12 result byte for byte, `$D506` unchanged afterwards. |
| `test/vcl-vms-bank1-invalid.bas` | Type 9 with an unknown VMS command in bank 1: message, `$D506` unchanged, the following VCL entry (type 6) runs. |

Result (2026-11-25): `test/run.sh test/*.bas` 47/47 green, `test/run.sh -c test/*.bas` 47/47 green; Z64K see 6. The old state (before P2d) fails `vcl-vms-bank1.bas` with code 10 (type 9 = `INVALID`).

## 6. Z64K

`test/run.sh -z test/vcl-*.bas test/vms-*.bas` (interpreted): 17/17 green. `test/run.sh -z -c` with `vcl-vms-bank1.bas`, `vcl-vms-bank1-invalid.bas`, `vcl-with-vms.bas`, `vcl-vms-source-skip.bas`, `vms-long-list.bas`: 5/5 green. Z64K emulates MMU and `$D506` independently of VICE; both agree.

## 7. What the engine has to adapt

- For every **unclipped** VMS list write a type 9 entry (`9, src, dst, list`) with the address of the list in the sprite set (bank 1) instead of the copy into the buffer and type 12. Clipped lists (`clip_image`, P5b 6.2) the engine continues to build in the buffer and outputs as type 12.
- Location: list entirely in bank 1 `$4000–$CFFF` or `$E000–$FEFF` – **not `$2000`** as in the proposal (below `$4000` bank 0 is visible during the entry). The default `$7000–$8FFF` for S2 fits. The check when setting the sprite sets (`SPRITESET`) accordingly.
- The list must be in the sprite set the way VDC-BASIC reads it: alpha, count, commands, contiguous.
- `VCL` itself stays in bank 0 (buffer). Change nothing in the stub: type 9 sets `$D506` itself and restores the caller's value.
- For the size calculation (P6 5.10): type 9 costs 7 bytes of VCL instead of 7 bytes of VCL + list length in the buffer.

## 8. Result (2026-11-25)

Implemented in `src/vdcbasic.asm` (`.vcl_check_vms`, `vms_execute_a`, `vcl_saved_rcr`). Size +42 bytes, end `$2E8E`. VICE: `run.sh` 47/47 interpreted and compiled; Z64K: 17/17 interpreted, 5/5 compiled (selection). Cycles per list byte type 9 = type 12 (≈ 76 for commands of type 1 with length 1, VDC-dominated); type 9 saves the engine the copy (~16 cycles per byte).
