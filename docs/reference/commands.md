# Command reference

VDC-BASIC v3: 28 instructions and 2 functions, grouped by what they work on. Parts marked **(v3)** are new since v2h (23 instructions and 2 functions): `RXV`, `VDM`, `VDL`, `VDR`, `VDB`, `VCL` entry types 8, 9 and 10, `VMS` entry type 3, and the faster row loop of `VMC`, `VMF` and `VMS` (same results, 20-30 % fewer cycles per row).

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
RUN "BOOT"
```

The boot loader `boot/boot.bas` (on disk as `BOOT`, first file on the disk) runs twice. The first pass sets the start of BASIC text (`$2D/$2E`) to `$4001` and loads itself there with `RUN"BOOT"`; the second pass loads `VDCBASIC3` to `$1C00` (`BLOAD"VDCBASIC3",B0,U8`), installs it with `SYS 7168`, checks the version byte at `$1C03` and ends with `NEW`. Replace its last line with `50 RUN"PROGRAM"` to start a program afterwards. A program can check that this has happened: `IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END`.

The extension occupies `$1C00`–`$2EE9` (4842 bytes; everything up to `$3FFF` is reserved), BASIC programs start at `$4001`, and the installation prints `VDC BASIC V3 INSTALLED`. It stays active until reset. After installation the new keywords are recognised when a line is entered, so they can no longer be part of a variable name (`ATTRIB`, `DISPLAY`, `SYNC` will not work).

**`GRAPHIC 1` to `GRAPHIC 4` must not be used**: the VIC bitmap (`$2000`–`$3FFF`) and its colour memory (`$1C00`–`$1FFF`) overwrite VDC-BASIC. `GRAPHIC 0` and `GRAPHIC 5` are fine, `GRAPHIC CLR` is not. `tools/mkboot.py` writes a C128 boot sector that runs `BOOT` at power-on. Programs compiled with Blitz! must use runtime image 2 (load address `$4001`); see the README.

## Overview

| Command | Purpose |
|---|---|
| [`RGW`](#rgw) [`RGA`](#rga-rgo) [`RGO`](#rga-rgo) [`RGD()`](#rgd) | Write, AND, OR, read a VDC register |
| [`VMW`](#vmw) [`VMA`](#vma-vmo) [`VMO`](#vma-vmo) [`VMD()`](#vmd) | Write, AND, OR, read one VRAM byte |
| [`VMF`](#vmf) | Fill VRAM: a run of bytes or a rectangle |
| [`VMC`](#vmc) | Copy inside VRAM: a run of bytes or a rectangle (the blit) |
| [`RTV`](#rtv) [`VTR`](#vtr) [`SWP`](#swp) | CPU RAM → VRAM, VRAM → CPU RAM, exchange |
| [`RXV`](#rxv) | Unpack a run-length coded picture (VRL stream) from CPU RAM into VRAM (v3) |
| [`VDM`](#vdm) [`VDL`](#vdl) [`VDR`](#vdr-vdb) [`VDB`](#vdr-vdb) | Choose the drawing area; draw a line, a rectangle outline, a filled box in a bitmap or chunky picture (v3) |
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
BLOAD "PICTURE",B0,P(DEC("8000")) : RTV DEC("8000"),0,8000
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
VCC DEC("8000"),DEC("2000"),0 : REM replace all 256 characters of the first set
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
- Runs shorter than the threshold *T* are written by the CPU, longer runs by the VDC's block fill, which works on its own while the CPU reads the next token. *T* is the byte at `$1C04` (default 2, the value measured on a real C128 at 2 MHz with register 36 = 0) and can be changed with `POKE DEC("1C04"),T`; any value 0–255 is allowed and changes only the speed, never the result. *T* = 0 or 1: all runs by block fill; *T* ≥ 128: all short runs (up to 127 bytes) by the CPU. Runs of 256 bytes or more always use the block fill.
- `RXV` switches register 24 to block fill (bit 7 = 0) and keeps its other bits.
- Afterwards `$1C06`/`$1C07` hold the address behind the end token (to unpack the next stream that follows directly) and `$1C08`/`$1C09` the VRAM address behind the last byte written or skipped, both low byte first.

```
10 BLOAD"DUNGEON.VRL",B0,P(DEC("8000"))
20 RXV DEC("8000")+5,0
30 A=PEEK(DEC("1C06"))+256*PEEK(DEC("1C07")) : REM where a following stream would start
40 RXV A,8000
```

In a [VCL](#vcl) list, entry type 8 does the same without the time BASIC needs to evaluate the statement, which matters for many small streams (tiles, parts of a picture).

## Drawing

**(v3)** Lines, rectangle outlines and filled boxes in a picture in VRAM. `VDM` says where the picture is and how its pixels are stored; `VDL`, `VDR` and `VDB` draw into it. The commands work on VRAM bytes only and do not care what the VDC displays: the picture may be the visible bitmap, a hidden page or a buffer that is copied later.

| Mode | Pixels | Byte of pixel (x, y) | Pixel in the byte |
|---|---|---|---|
| 0, bitmap | 1 bit, 8 per byte | *base* + *y* × *stride* + *x* / 8 | bit 7 − (*x* AND 7): bit 7 is the left pixel |
| 1, chunky | 4 bits, 2 per byte | *base* + *y* × *stride* + *x* / 2 | even *x*: upper four bits, odd *x*: lower four bits |

Divisions round down; addresses are taken modulo 65536. The nibble order of mode 1 matches chunky mode as the guide sets it up ([chapter 5](../05-sixteen-colours.md)): bitmap bytes all `$0F`, the pixels in the attribute bytes. The VDC paints 0 bits with the upper four bits of the attribute and 1 bits with the lower four, so the left four dots of a character cell show the upper nibble. The same order is used by `VMS` entry type 8. `VDM` does not check the VDC registers; with reverse video (register 24 bit 6) the halves swap on screen.

**Colour *c*:**

| Mode | *c* | Effect |
|---|---|---|
| 0 | 0 | clear the pixels |
| 0 | 1 | set the pixels |
| 0 | 2 | invert the pixels (XOR) |
| 1 | 0–15 | pixels get colour *c* |

Every pixel of a figure is changed exactly once, corners of `VDR` and the pixels of a line included. Drawing the same figure twice with *c* = 2 therefore restores the picture. There is no XOR in mode 1.

**No clipping.** Nothing is checked against the size of the picture, as with all VDC-BASIC commands. An *x* beyond the end of a row continues in the next row, and addresses wrap from 65535 to 0. Keep figures inside the picture.

**Speed.** Pixels that lie in one row are written as a span: the partly covered bytes at both ends by reading and writing them back, the full bytes in between in one go. A middle part of at least *T* bytes (the threshold at `$1C04`, see [`RXV`](#rxv)) is written by the VDC's block fill, shorter ones by the CPU; *T* changes the speed, never the result. A vertical run costs one read and one write per pixel. A filled box of whole bytes (`VDB` with *x* and *w* multiples of 8 in mode 0 or of 2 in mode 1, *c* ≠ 2) is one block fill per row.

### VDM

```
VDM mode, base, stride
```

Sets the drawing area for the following `VDL`, `VDR` and `VDB`: *mode* 0 (bitmap) or 1 (chunky), *base* the VRAM address of pixel (0, 0), 0–65535, *stride* the bytes per pixel row, 1–65535. The setting stays until the next `VDM`. After `SYS 7168` it is `VDM 0,0,80` (a 640-pixel bitmap at VRAM address 0).

- *mode* other than 0 or 1, or *stride* 0: `?ILLEGAL QUANTITY ERROR`; the old setting stays.
- *stride* is usually the row length of the screen (register 1, 80) or a multiple of it for a picture that leaves rows out.

```
VDM 0,0,80        : REM 640 x 200 bitmap at VRAM 0
VDM 1,0,80        : REM chunky: 160 pixels per row, attributes at VRAM 0
```

### VDL

```
VDL x1, y1, x2, y2, c
```

Draws a line from (*x1*, *y1*) to (*x2*, *y2*), both end points included. All four coordinates 0–16383.

- The pixels are those of Bresenham's algorithm: max(|*x2* − *x1*|, |*y2* − *y1*|) + 1 pixels, each touching the next at a side or a corner. When the line runs exactly between two pixels it stays on the row (or column) it is on. Swapping the end points gives the same pixels.
- A line from a point to itself is one pixel.
- Horizontal stretches of a line are written as spans (see above), so a flat line is much faster per pixel than a steep one.

```
VDL 0,0,639,199,1 : REM diagonal across the bitmap
VDL 10,5,10,5,2   : REM invert one pixel
```

### VDR, VDB

```
VDR x, y, w, h, c
VDB x, y, w, h, c
```

`VDR` draws the outline of a rectangle, `VDB` fills it. (*x*, *y*) is the top left pixel, *w* × *h* the outer size in pixels. Values 0–65535 with *x* + *w* ≤ 65536 and *y* + *h* ≤ 65536.

- *w* = 0 or *h* = 0 draws nothing (no error).
- `VDR` with *w* or *h* 1 or 2 has no inside: *h* = 1 is a horizontal line, *w* = 1 a vertical one. Otherwise it changes 2*w* + 2*h* − 4 pixels.

```
VDB 0,0,640,200,0      : REM clear a 640 x 200 bitmap
VDM 1,0,80:VDB 10,20,70,96,4 : REM chunky box, colour 4
VDR 5,5,20,10,2        : REM inverted frame, twice restores
```

### Errors of the drawing commands

All parameters are read from left to right; the first error ends the command. Nothing is drawn when there is an error.

| Case | Error |
|---|---|
| a parameter or comma missing, too many parameters | `?SYNTAX ERROR` |
| a value below 0 or above 65535 (found while reading, so before a missing later parameter) | `?ILLEGAL QUANTITY ERROR` |
| *mode* not 0/1, *stride* 0, *c* not allowed in the current mode, a `VDL` coordinate above 16383, *x* + *w* or *y* + *h* above 65536 | `?ILLEGAL QUANTITY ERROR` |
| a string | `?TYPE MISMATCH ERROR` |

Fractions are cut off, as everywhere in VDC-BASIC.

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
- **(v3)** Entry type 3 skips *n* bytes of sprite data without drawing; with it a program can cut a sprite at an edge (see [VMS list format](vms-format.md#clipping-type-3-v3)).
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
- **(v3)** Entry type 9 runs a `VMS` whose run list lies in RAM bank 1 (`$4000`–`$CFFF` or `$E000`–`$FEFF`); the `VCL` list itself stays in bank 0.
- **(v3)** Entry type 10 continues the list at another address in bank 0 (3 bytes: 10, address).

---

## Memory used by VDC-BASIC

For programs that add their own machine code.

| Location | Use |
|---|---|
| `$1300`–`$1BFF` | (v3) Free for the program: lists that must lie below `$4000` |
| `$1C00`–`$1C0F` | (v3) Header: `$1C00` `JMP install`, `$1C03` version, `$1C04` threshold T of `RXV`, `$1C06`/`$1C07` and `$1C08`/`$1C09` results of `RXV`, rest reserved |
| `$1C10`–`$3FFF` | (v3) Code and variables (currently up to `$2EE9`), including the state of `VDM` (5 bytes) and a 64-byte buffer for inverted spans |
| `$02FC`, `$030C`–`$0311` | BASIC vectors taken over (function call, tokenise, list, execute) |
| `$0A`–`$14`, `$26`–`$2A` | Work bytes of `VCL`, `VMS`, `VMP`, `VDL`/`VDR`/`VDB` (v3) |
| `$3F`/`$40`, `$77`/`$78` | List and target pointers |
| `$83`–`$8E` | Parameters. BASIC uses these for its 40-column graphics commands. `RXV` uses `$83`–`$8B` (stream pointer, VRAM address, counters), the drawing commands `$83`–`$8E` (parameters, then work bytes). |
| `$16`/`$17`, `$24`/`$25` | As BASIC uses them for `POKE` and strings |
