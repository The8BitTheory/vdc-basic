# Command reference

VDC-BASIC v2h: 23 instructions and 2 functions (v3: 24, plus the reserved VDL/VDR/VDB/VDM tokens), grouped by what they work on. Parts marked **(v3)** need VDC-BASIC v3 (`vdcbasic3.bin` at `$1C00`, see the [README](../../README.md)): `RXV`, `VCL` entry type 8 and `VMS` entry type 3.

Everything stated here was either checked by a listing in `test/` or read directly from `src/vdcbasic.asm`.

## Conventions

| Term | Meaning |
|---|---|
| *reg* | VDC register number, 0–37 |
| *vaddr* | Address in VDC RAM (VRAM), 0–65535 |
| *addr* | Address in CPU RAM, bank 0, 0–65535. The `BANK` setting is ignored. |
| *byte* | 0–255 |
| *count* | 0–65535 |
| *reps* | Number of repetitions, 0–255. **0 means 256.** |
| *step* | Signed increment, −32768 to 32767 |
| [ … ] | Optional |

All numeric parameters are ordinary BASIC expressions. A value outside its range gives `?ILLEGAL QUANTITY ERROR`.

No VDC-BASIC command changes the interrupt flag (except `SYN` with pairs, which writes its registers with interrupts off). An interrupt routine of your own must not access the VDC while a command runs: it would select another register in `$D600`, and the next data byte of the command, for example of `RXV` or `RTV`, would go into that register.

### 16 KiB and 64 KiB

With 16 KiB VRAM, addresses 16384–65535 mirror the lower 16 KiB.

A VDC with 64 KiB also starts out addressing only 16 KiB. To reach all of it, set bit 4 of register 28:

```
RGO 28,16
```

Do this before you put anything into VRAM. The switch changes how the chip addresses its memory, so on real hardware the existing content, including the character set, no longer appears where it was; reload the character set with `RST 2` if you stay in text mode. `RST 1` clears the bit again. `RGD(255)` reports the installed size whatever the bit says.

## Loading

```
BLOAD "VDCBASIC2H",B0 : SYS DEC("1300")
```

The extension occupies `$1300`–`$1BF6` and prints `VDC BASIC V2H INSTALLED`. It stays active until reset. After installation the new keywords are recognised when a line is entered, so they can no longer be part of a variable name (`ATTRIB`, `DISPLAY`, `SYNC` will not work).

## Overview

| Command | Purpose |
|---|---|
| [`RGW`](#rgw) [`RGA`](#rga-rgo) [`RGO`](#rga-rgo) [`RGD()`](#rgd) | Write, AND, OR, read a VDC register |
| [`VMW`](#vmw) [`VMA`](#vma-vmo) [`VMO`](#vma-vmo) [`VMD()`](#vmd) | Write, AND, OR, read one VRAM byte |
| [`VMF`](#vmf) | Fill VRAM: a run of bytes or a rectangle |
| [`VMC`](#vmc) | Copy inside VRAM: a run of bytes or a rectangle (the blit) |
| [`RTV`](#rtv) [`VTR`](#vtr) [`SWP`](#swp) | CPU RAM → VRAM, VRAM → CPU RAM, exchange |
| [`RXV`](#rxv) | Unpack a run-length coded picture (VRL stream) from CPU RAM into VRAM (v3) |
| [`VCC`](#vcc) | Copy 8-byte character patterns into the VDC character set |
| [`DISP`](#disp-attr-crsr) [`ATTR`](#disp-attr-crsr) [`CRSR`](#disp-attr-crsr) | Set display start, attribute start, cursor address |
| [`SYN`](#syn) | Wait for the vertical blank, optionally then write registers |
| [`RST`](#rst) | Restore the default registers and/or character set |
| [`VCS`](#vcs) [`VMP`](#vmp) [`VMB`](#vmb) | Set up a bitmap font, print a string, print bytes from memory |
| [`VMS`](#vms) | Draw a sprite with transparent parts from a run list |
| [`VCL`](#vcl) | Execute a list of copy, fill, write and sprite commands |

---

## Registers

### RGW

```
RGW reg, byte
```

Writes *byte* to VDC register *reg*.

```
RGW 26,DEC("F0") : REM white on black
```

### RGA, RGO

```
RGA reg, byte
RGO reg, byte
```

Read the register, combine it with *byte* by AND (`RGA`) or OR (`RGO`), write it back. Use them to clear or set single bits.

```
RGO 25,128 : REM bitmap mode on
RGA 25,127 : REM bitmap mode off
```

### RGD

```
RGD(reg)
```

Function. Returns the content of VDC register *reg*. Two numbers are not real registers:

| *reg* | Returns |
|---|---|
| 254 | VDC version: 0 = 8563 R7A, 1 = 8563 R8/R9, 2 = 8568 |
| 255 | VRAM size: 0 = 16 KiB, 1 = 64 KiB. The test writes to VRAM `$BFFF` and restores it. |

---

## Single VRAM bytes

### VMW

```
VMW vaddr, byte
```

Writes *byte* to VRAM address *vaddr*.

### VMA, VMO

```
VMA vaddr, byte
VMO vaddr, byte
```

Read the VRAM byte, combine it with *byte* by AND (`VMA`) or OR (`VMO`), write it back.

```
VMA A,DEC("F0") : VMO A,C : REM replace the right pixel of a 160x100 byte by colour C
```

### VMD

```
VMD(vaddr)
```

Function. Returns the byte at VRAM address *vaddr*.

---

## Fill and copy

Both commands use the block operations built into the VDC. The chip does the work; BASIC only sets it up.

### VMF

```
VMF vaddr, byte, count
VMF vaddr, byte, count, reps, step
```

Fills *count* bytes starting at *vaddr* with *byte*.

With *reps* and *step*, the fill is repeated *reps* times and *vaddr* grows by *step* after each run. With *step* equal to the bytes per screen row this fills a rectangle *count* bytes wide and *reps* rows high.

- *count* 0 fills 65536 bytes, the whole VRAM.
- *step* may be negative.
- *reps* and *step* must be given together.

```
VMF 0,32,2000              : REM clear the text screen
VMF A+10*80+20,255,8,16,80 : REM solid box, 8 bytes wide, 16 rows high
```

### VMC

```
VMC source, target, count
VMC source, target, count, reps, targetstep
VMC source, target, count, reps, targetstep, sourcestep
```

*source* and *target* are VRAM addresses. Copies *count* bytes from *source* to *target*.

With *reps* and *targetstep*, the copy is repeated *reps* times and *target* grows by *targetstep* after each run. This is the blit: *count* is the width in bytes, *reps* the height in rows, *targetstep* the bytes per screen row.

Where the source continues depends on *sourcestep*:

| *sourcestep* | Source of the next run |
|---|---|
| omitted or 0 | Directly behind the previous run. The picture is stored packed, row after row, without gaps. |
| any other value | Start of the previous run plus *sourcestep*. Use the row length of the source to cut a rectangle out of a larger picture. |

- *count* 0 copies nothing.
- Both steps may be negative. A negative *targetstep* draws bottom-up, which flips the picture vertically.
- A source cannot be held still across repetitions, because 0 means "continue".
- The copy runs byte by byte towards higher addresses. If the target lies inside the source range above its start, bytes that were already overwritten are copied again. `VMC A,A+1,N` therefore smears the byte at A over N+1 bytes. Overlap with the target below the source is safe.
- *reps* and *targetstep* must be given together. The character between *reps* and *targetstep*, and before *sourcestep*, is skipped without being checked; write a comma.

```
VMC 16000,0,8000               : REM copy a whole 160x100 page
VMC SH,A+Y*80+X,4,16,80        : REM blit a packed 4x16 byte tile
VMC PIC+Y*80+X,A,10,20,80,80   : REM cut 10x20 bytes out of a full-width picture
VMC SH,A+15*80,4,16,-80        : REM the same tile upside down
```

---

## CPU RAM and VRAM

These three run a loop in machine code, one byte at a time. They are much slower than `VMC` but still far faster than BASIC. They always use RAM bank 0 and see the RAM under the ROMs and under the I/O area.

In all three, *count* 0 means 65536 bytes.

### RTV

```
RTV addr, vaddr, count
```

"RAM to VRAM". Copies *count* bytes from CPU RAM at *addr* to VRAM at *vaddr*.

```
BLOAD "PICTURE",B0,P(DEC("4000")) : RTV DEC("4000"),0,8000
```

### VTR

```
VTR vaddr, addr, count
```

"VRAM to RAM". Copies *count* bytes from VRAM at *vaddr* to CPU RAM at *addr*.

### SWP

```
SWP vaddr, addr, count
```

Exchanges *count* bytes between VRAM at *vaddr* and CPU RAM at *addr*.

### VCC

```
VCC addr, vaddr, chars
```

Copies character patterns of 8 bytes each from CPU RAM into a VDC character set. The VDC reserves 16 bytes per character; `VCC` writes 8 and skips 8, the skipped bytes keep their content. *chars* is 1–255, 0 means 256.

```
VCC DEC("4000"),DEC("2000"),0 : REM replace all 256 characters of the first set
```

---

## Unpacking

### RXV

```
RXV src, dst
```

**(v3)** "RAM, unpack, to VRAM". Unpacks the VRL stream at *src* in CPU RAM bank 0 into VRAM from *dst* on. A VRL stream is a sequence of tokens: literal bytes, runs of one value, and skips that move on in VRAM without writing, ended by a zero byte. See [VRL format](vrl-format.md); `tools/vrl.py` makes such streams from pictures.

- *src* points at the first token of the **stream**, not at the file header. A VRL file starts with a 5-byte header (`"VR"`, version, unpacked length), so after `BLOAD"PIC.VRL",B0,P(A)` the call is `RXV A+5,dst`. Streams embedded in other data have no header. The header is not checked: `RXV` on the header reads `"V"` (86) as a literal of 86 bytes.
- The `BANK` setting is ignored. The stream must not lie in `$D000`–`$DFFF` (the command reads with I/O mapped in) or in `$FF00`–`$FF04`. Everywhere else in bank 0, also under the BASIC ROM and across `$4000`, it is fine.
- Nothing is checked: a stream without its end token makes `RXV` read on (past `$FFFF` to `$0000`) with undefined results. The STOP key is not polled.
- VRAM addresses wrap from 65535 to 0. With 16 KiB VRAM the memory mirrors as described above.
- Runs shorter than the threshold *T* are written by the CPU, longer runs by the VDC's block fill, which works on its own while the CPU reads the next token. *T* is the byte at `$1C04` (default 14, provisional until measured on real hardware) and can be changed with `POKE DEC("1C04"),T`; any value 0–255 is allowed and changes only the speed, never the result. *T* = 0 or 1: all runs by block fill; *T* ≥ 128: all short runs (up to 127 bytes) by the CPU. Runs of 256 bytes or more always use the block fill.
- `RXV` switches register 24 to block fill (bit 7 = 0) and keeps its other bits.
- Afterwards `$1C06`/`$1C07` hold the address behind the end token (to unpack the next stream that follows directly) and `$1C08`/`$1C09` the VRAM address behind the last byte written or skipped, both low byte first.

```
10 BLOAD"DUNGEON.VRL",B0,P(DEC("8000"))
20 RXV DEC("8000")+5,0
30 A=PEEK(DEC("1C06"))+256*PEEK(DEC("1C07")) : REM where a following stream would start
40 RXV A,8000
```

In a [VCL](#vcl) list, entry type 8 does the same without the time BASIC needs to evaluate the statement, which matters for many small streams (tiles, parts of a picture).

---

## Display control

### DISP, ATTR, CRSR

```
DISP vaddr
ATTR vaddr
CRSR vaddr
```

Set a 16-bit VDC address register pair in one statement.

| Command | Registers | Meaning |
|---|---|---|
| `DISP` | 12/13 | Start of the displayed characters or bitmap |
| `ATTR` | 20/21 | Start of the displayed attributes |
| `CRSR` | 14/15 | Cursor position |

The screen editor of the C128 knows nothing about these changes; `PRINT` keeps writing to the default locations.

### SYN

```
SYN
SYN reg, byte [, reg, byte] ...
```

Without parameters: waits until the VDC enters the vertical blank.

With pairs: evaluates all parameters first, waits for the vertical blank, then writes the registers in the given order with interrupts off. Use it for changes that must take effect together.

```
SYN 12,P/256, 13,P AND 255, 20,Q/256, 21,Q AND 255
```

Both forms can also stand in a [`VCL`](#vcl) list as entry type 13; see [VCL list format](vcl-format.md).

### RST

```
RST flags
```

| Bit | Value | Effect |
|---|---|---|
| 0 | 1 | Sets all VDC registers to their power-on values (the Kernal table, PAL or NTSC as detected, plus register 37) |
| 1 | 2 | Copies the ROM character set into VRAM at `$2000` |

`RST 3` does both and brings back a normal text screen after bitmap experiments. The screen content itself is not restored; print `CHR$(147)` afterwards.

---

## Text in bitmap modes

A font for these commands is a sheet of small pictures in VRAM. Each character (glyph) is *width* bytes wide and *height* rows high and is stored packed: row 0, row 1, … The glyphs follow each other without gaps. The first glyph belongs to character code 32 (space), the next to 33, and so on.

### VCS

```
VCS vaddr, width, height [, rowbytes]
```

Declares the font: *vaddr* is the VRAM address of the first glyph, *width* its width in bytes, *height* its height in rows. *width* × *height* must not exceed 255.

*rowbytes* is the distance in bytes from one screen row to the next. If omitted, it is read from VDC register 1 at this moment (80 on a standard screen). Give 160 to print on every second row.

The settings stay in force until the next `VCS`. `VMP` and `VMB` give undefined results before the first `VCS`.

### VMP

```
VMP vaddr, string
```

Prints *string* with the font declared by `VCS`. *vaddr* is the VRAM address of the top left byte of the first character; each further character is placed *width* bytes to the right. *string* is any string expression. An empty string prints nothing.

- There is no line wrap and no clipping.
- Character codes below 32 are not printable and produce garbage.
- Each character is one blit, so the background under the text is replaced, not mixed.

```
VCS F,1,8 : VMP A+10*80+5,"SCORE"+STR$(S)
```

### VMB

```
VMB vaddr, addr, length
```

Like `VMP`, but the text is *length* bytes (0–255) at *addr* in CPU RAM bank 0. Useful for fixed texts loaded from disk and for programs that keep text outside BASIC strings. Length 0 prints nothing.

---

## Sprites and lists

### VMS

```
VMS source, target, addr
```

Draws a picture with transparent parts. *source* is the VRAM address of the packed sprite data, *target* the VRAM address of the top left corner on screen, *addr* the address of a run list in CPU RAM bank 0.

The list says which stretches of the sprite data go where. Bytes not mentioned are skipped on screen, which is what makes them transparent. See [VMS list format](vms-format.md).

- The list may be anywhere in bank 0 except `$D000`–`$DFFF`, and may have any length.
- An unknown entry type prints `INVALID VMS COMMAND` and ends the command.
- Target offsets in the list assume the row length the list was made for.

### VCL

```
VCL addr
```

Executes a command list stored at *addr* in CPU RAM bank 0. Each entry is one type byte followed by 16-bit parameters and stands for one `VMC`, `VMF`, `VMW`, `VMS`, `RXV` (v3) or `SYN`. A zero byte ends the list. See [VCL list format](vcl-format.md).

A list saves the time BASIC needs to read and evaluate each statement, which is most of the time of a short `VMC`.

- The list may be anywhere in bank 0 except `$D000`–`$DFFF`, and may have any length.
- An unknown type prints `INVALID` and ends the command.

---

## Memory used by VDC-BASIC

For programs that add their own machine code.

| Location | Use |
|---|---|
| `$1300`–`$1BF6` | Code and variables (v2h) |
| `$1C00`–`$1C0F` | (v3) Header: `$1C00` `JMP install`, `$1C03` version, `$1C04` threshold T of `RXV`, `$1C06`/`$1C07` and `$1C08`/`$1C09` results of `RXV`, rest reserved |
| `$1C10`–`$3FFF` | (v3) Code and variables (currently up to `$2654`) |
| `$02FC`, `$030C`–`$0311` | BASIC vectors taken over (function call, tokenise, list, execute) |
| `$0A`–`$0E`, `$26`–`$2A` | Work bytes of `VCL`, `VMS`, `VMP` |
| `$3F`/`$40`, `$77`/`$78` | List and target pointers |
| `$83`–`$8E` | Parameters. BASIC uses these for its 40-column graphics commands. `RXV` uses `$83`–`$8B` (stream pointer, VRAM address, counters). |
| `$16`/`$17`, `$24`/`$25` | As BASIC uses them for `POKE` and strings |
