# VDC-BASIC Programmer's Guide: draft outline

Status: draft for discussion; chapter texts are not written yet. The decisions table below is agreed.
Basis: branch `v2g`, `src/vdcbasic.asm` (commit ec049bd).

## 1. What this guide is

A book for BASIC 7.0 programmers on the C128 who want to use the 80-column chip (VDC 8563/8568) for bitmap work: loading pictures, blitting, page flipping, scrolling, soft sprites with transparency, text in bitmap mode.

It has two halves:

- **Part I–IV, the guided tour.** Chapters build on each other. Each one starts from a thing you want on screen, explains the piece of VDC behaviour that makes it possible, and ends with a listing that runs. Later chapters reuse the listings of earlier ones.
- **Part V, the reference.** Every command and function, one page each, in a fixed layout. Tables for list formats, tokens and registers.

The guiding idea, stated early and repeated: VDC-BASIC does not draw lines and circles. It moves memory. The VDC has its own RAM (up to 64 KiB) that the CPU cannot see, and a built-in block copy and block fill that run far faster than any BASIC loop. Almost every technique in the book is "arrange VRAM cleverly, then let the chip copy".

### Decisions

| Topic | Decision |
|---|---|
| Reader | Knows BASIC 7.0 well. Has never programmed the VDC. Does not need assembly. |
| Hardware | Every example that needs 64 KiB VRAM says so at its top. |
| Binary | `vdcbasic2g.bin`, load address `$1300`, built from `src/vdcbasic.asm` with ACME and checked in. |
| Scope | The guide describes what the v2g code does, including its limits. |
| Main mode | 160×100 with 16 colours is the mode for anything animated. 640×200 is used for static pictures and for text on a single-colour background, where the attribute RAM rarely needs touching. |
| Format | Markdown, one file per chapter under `docs/`, listings also as separate `.bas` files under `docs/listings/`. |
| Listings | Every listing is run in VICE (`x128`, 80-column output) before it goes in; screenshots come from those runs. The tests are event driven (breakpoints through the remote monitor), never fixed waits. |
| Running example | One project grows through the book and ends as a top-down room: a floor drawn from tiles, a figure with transparent pixels that walks around under keyboard control, things to pick up, a status line in a bitmap font. No view construction or other game maths, so the listing stays about VRAM. (Proposal, replaces the first-person dungeon.) |
| Language | English first, structured so a German translation can mirror it file by file. |

## 2. Proposed file layout

```
docs/
  README.md                  entry page, table of contents, how to read
  01-first-contact.md
  02-two-memories.md
  ...
  14-the-room.md
  reference/
    commands.md              all instructions and functions
    vcl-format.md            command list byte layout
    vms-format.md            sprite list byte layout
    tokens.md                token values, editor/toolchain setup
    vdc-registers.md         the registers this guide uses
    history.md               versions v2c … v2g, what changed
  listings/                  .bas sources of all listings
  img/                       screenshots and diagrams
```

## 3. The guided tour

Each chapter entry lists: the goal, what is explained, the commands introduced, and the listing it ends with.

### Part I: Meeting the chip

**1. First contact**
Goal: VDC-BASIC is loaded and one command visibly changes the 80-column screen.
- What VDC-BASIC is, where it comes from (Rosenhahn 1993, Mac Bacon 2013, the v2c–v2g extensions).
- Loading and starting: `BLOAD`, `SYS`, the install message, the two load addresses.
- The memory configuration it expects (`BANK`, where your own data can live).
- How to write programs with the new keywords: on the machine, and on a PC (RetroDevStudio/C64 Studio token file, building a disk image).
- First effect: change a colour or the cursor with one `RGW`.
- Commands: `RGW`, `RGD`.
- Listing: load, install, report chip revision and VRAM size (`RGD(254)`, `RGD(255)`).

**2. Two memories**
Goal: understand that there are two separate address spaces, and read and write the second one.
- CPU RAM versus VRAM. Why `POKE` cannot reach the screen. The two-register door (`$D600/$D601`) in one paragraph, then never again.
- The default VRAM map in text mode: characters, attributes, character set.
- Register access with bit operations.
- Commands: `VMW`, `VMD`, `VMA`, `VMO`, `RGA`, `RGO`.
- Listing: write characters and colours straight into screen and attribute memory; a tiny "VRAM monitor" that dumps a region.

**3. The chip does the work**
Goal: fill and copy large areas in one statement and see the speed difference.
- Block fill and block copy as built-in VDC operations.
- Timing comparison: a BASIC loop with `VMW` against one `VMF`.
- Commands: `VMF`, `VMC` (three-parameter forms only).
- Listing: clear the screen, duplicate a line, scroll the text screen by one row with a single `VMC`.

### Part II: A bitmap to draw on

**4. Switching to bitmap mode**
Goal: a clean 640×200 bitmap with attributes, and a safe way back to the text screen.
- The registers that matter: 25 (bitmap bit, attribute bit), 28 (RAM type), 12/13 and 20/21.
- Planning a VRAM map: where the bitmap goes, where the attributes go, what is left over as off-screen space. This map is the central diagram of the book and comes back in every later chapter.
- Getting back: `RST`.
- Commands: `DISP`, `ATTR`, `CRSR`, `RST`.
- Listing: enter bitmap mode, clear bitmap and attributes, wait for a key, restore.

**5. Sixteen colours per pixel: the chunky mode**
Goal: the 160×100 mode with 16 freely placed colours that the rest of the book uses.
- The trick: fill the bitmap with a fixed pattern (`%00001111`), shorten character cells to two scan lines, and let the attribute bytes carry the picture. One attribute byte is two pixels, one nibble each.
- The register recipe line by line (R0, R4, R6, R7, R9, R25, R26, R28, R36), PAL and NTSC notes.
- Addressing a pixel: `address = base + y*80 + x/2`, left or right nibble.
- Why this mode suits the VDC's block copy: one byte is a clean pair of pixels, so byte-wise copying never needs masks.
- Listing: set the mode, plot pixels with `VMW`/`VMA`/`VMO`, draw a colour bar test picture with `VMF`.

**6. Getting pictures into VRAM**
Goal: load a picture from disk and show it.
- Disk → CPU RAM (`BLOAD`) → VRAM. Where to put the buffer in bank 0, and how big it can be.
- The other direction: saving a screen. Exchanging a buffer and a screen region.
- Converting artwork: the BMP converter in `tools/`, the file layout it writes.
- Custom text-mode character sets as a side trip.
- Commands: `RTV`, `VTR`, `SWP`, `VCC`.
- Listing: picture viewer; screen saver/loader pair.

### Part III: Moving things

**7. The blit**
Goal: copy a rectangle from an off-screen sheet to any place on screen with one statement.
- From "copy n bytes" to "copy n bytes, h times, stepping the target by one screen row": the repetition and target-increment parameters of `VMC`.
- How the source advances: contiguous by default, so sheets can be stored packed; the optional source increment for cutting a rectangle out of a full-width picture.
- Rectangle fills with the same parameters on `VMF`.
- Negative increments: copying bottom-up, vertical flipping.
- Overlap rules: what happens when source and target overlap.
- Commands: `VMC` (5- and 6-parameter forms), `VMF` (5-parameter form).
- Listing: a tile sheet in off-screen VRAM, a map drawn from tiles; filled boxes.

**8. Two screens, no flicker**
Goal: build the next frame where nobody sees it, then show it at once.
- Double buffering with two attribute areas and `ATTR` (or two bitmaps and `DISP`).
- Timing the switch to the vertical blank: plain `SYN`, and `SYN` with register/value pairs for changes that must happen together.
- Cost in VRAM, and how the chunky mode makes it cheap (8000 bytes per page).
- Commands: `SYN`.
- Listing: bouncing block without and with page flipping.

**9. Scrolling and wide worlds**
Goal: scroll a picture that is larger than the screen.
- Coarse scrolling by moving the display start (`DISP`/`ATTR`) instead of moving data.
- Fine scrolling registers (24, 25) in brief.
- Virtual screens wider than 80 bytes and what that means for the row increment in blits.
- Listing: horizontal pan across a wide picture (based on `examples/horzdemo`).

**10. Words on a bitmap**
Goal: print text in bitmap or chunky mode with your own font.
- A font is just a sheet of small blits. Layout of the glyphs in VRAM, character code to glyph number.
- Setting up once, printing many times; the optional screen-width parameter for unusual row lengths and double-spaced output.
- Strings from BASIC variables versus text stored at a fixed address.
- Fonts with odd sizes (based on `examples/9x9pixelchars`).
- Commands: `VCS`, `VMP`, `VMB`.
- Listing: status line and a text box over a picture.

**11. Sprites with holes**
Goal: a figure with transparent pixels over a background.
- Why a plain blit draws a box around the figure.
- The idea behind `VMS`: at conversion time, cut the sprite into runs of solid bytes; at run time, copy only those runs. Bytes that are half transparent are merged with the background nibble by nibble, using an alpha colour.
- The list format, read through one small hand-made example.
- Producing lists from artwork with the tools in `tools/`.
- Moving a sprite: redraw from a clean background page (ties back to chapter 8), or save and restore the area under it.
- Commands: `VMS`.
- Listing: sprite walking over the picture from chapter 6, keyboard controlled.

**12. Scripts for the chip: command lists**
Goal: draw a complete scene with a single BASIC statement.
- The real cost in a BASIC program is the interpreter parsing each statement, not the VDC. A command list stores the parameters as bytes, so one `VCL` replaces dozens of statements.
- Building a list: by `POKE`, from `DATA`, or loaded from disk. The byte layout for copy, fill, write and sprite entries.
- Lists as data: one list per wall piece, per screen, per animation frame.
- Commands: `VCL`.
- Listing: the tile map from chapter 7 drawn by one list, with timing.

### Part IV: Putting it together

**13. Speed, memory and other limits**
- `FAST`, and what it does and does not speed up.
- Compiling VDC-BASIC programs.
- A VRAM budget worksheet: pages, sheets, fonts, sprite data.
- 16 KiB machines: what fits.
- Parameter ranges and wrap-around pitfalls collected in one place.

**14. The room**
The capstone. A top-down room built from everything before:
- VRAM plan (two pages, tile sheet, font, figure, clean background copy).
- Floor and walls from tiles, drawn by one command list.
- The figure as `VMS` sprite, moved with the keyboard, redrawn from the clean background.
- Objects that disappear when picked up (a tile blit) and a counter in the status line (`VMP`).
- Page flip per frame.
- Complete listing, discussed section by section.
- Closing remark: the same building blocks carry a first-person view; pointer to the dungeon crawler jam game as a larger example.

## 4. The reference (Part V)

### 4.1 Command pages

Fixed layout per command: syntax, parameters with ranges, what it does, notes and limits, short example, "since version", and a pointer to the tour chapter.

| Group | Entries |
|---|---|
| Registers | `RGW`, `RGA`, `RGO`, `RGD()` including pseudo registers 254 and 255 |
| Single VRAM bytes | `VMW`, `VMA`, `VMO`, `VMD()` |
| Fill and copy | `VMF`, `VMC` |
| CPU RAM ↔ VRAM | `RTV`, `VTR`, `SWP`, `VCC` |
| Display control | `DISP`, `ATTR`, `CRSR`, `SYN`, `RST` |
| Text | `VCS`, `VMP`, `VMB` |
| Sprites and lists | `VMS`, `VCL` |

### 4.2 Tables and appendices

- `VCL` entry types with exact byte layout (types 1–7 and 12, terminator 0).
- `VMS` list layout: alpha byte, 16-bit count, entry types 1, 2 and 8.
- Token values and keyword abbreviations; editor setup.
- VDC registers used in this guide, with the values of the standard modes.
- VRAM maps: text mode default, 640×200 bitmap, chunky mode with two pages.
- File formats written by the tools.
- Messages (`invalid`, `invalid vms command`) and what causes them.
- Version history v2c–v2g.
- Zero-page and memory locations VDC-BASIC uses, for readers who mix in their own machine code.

## 5. Points found in the source that the reference must state precisely

These came up while reading `vdcbasic.asm` and differ from, or are missing in, `README.txt`. Each needs a decision or a test before it is written down.

1. `VMF` accepts repetitions and a target increment like `VMC`. The README does not mention it.
2. `VMC`/`VMF`: if repetitions are given, the target increment is required. The separator before the increments is skipped without being checked for a comma.
3. Repetitions are one byte; 0 behaves as 256.
4. `VMC` source increment 0 or omitted means "source runs on contiguously". A source cannot be held still across repetitions.
5. `VCL` type 7 (`VMW` with repetitions and increment) is marked "reserved" in the comment but there is code for it. Types 8–11 (`VMO`/`VMA`) and `$FF` (sleep) are listed in the comment and not implemented. The bank parameter in the comment does not exist; lists are read from RAM bank 0.
6. `VCL` type 6/7 take the value as a 16-bit field of which only the low byte is used.
7. `VMS` entry type 2 copies a rectangle to the target address using the current R1 as row length; it takes no offset of its own. Needs a test to confirm intended use.
8. `VMP` maps a character to glyph number `code − 32`, so codes below 32 are not printable, and it reads the string from bank 1; `VMB` reads from bank 0.
9. `VCS` stores width × height in one byte, so a glyph is limited to 255 bytes.
10. `VCC` always copies 8 bytes per character and skips 8.
11. `DISP`, `ATTR`, `CRSR` accept 0–65535 only (open TODO in the source).
12. `VCL` never advances its list pointer and indexes with one byte, so a list is limited to 256 bytes including the terminator. (`VMS` lists had the same limit until ec049bd.)
13. `VMS` reads the entry type bytes, and all bytes of type 8 entries, with the BASIC ROM mapped in. From reading the code, a list at `$4000` or above should fail. To be confirmed by test.
14. `VMS` entry type 2 does not set the high byte of the row increment or the source increment; both are left over from an earlier `VMC`.
15. `VMB` with length 0 prints 256 characters. `VMP` with an empty string prints nothing.
16. `VCL` type 7 turns an increment of 0 into 1.
17. `RTV`, `VTR`, `SWP` with count 0 transfer 65536 bytes; `VCC` with count 0 copies 256 characters.
18. The source assembles to `$1300` and writes `vdcbasic2g.bin`. The binaries `vdcbasic.bin`, `vdcbasic2c.bin` and `vdcbasicac6.bin` are older than v2g, and the tools load `VDCBASIC2F.1300`. v2g is 2184 bytes (`$1300`–`$1B87`), which leaves 121 bytes below the BASIC start at `$1C01` and no longer fits at `$0AC6` without running into `$1300`.

## 6. Proposed order of work

1. Agree on this outline and the open questions.
2. Build a small test harness: tokenise a `.bas` listing with the VDC-BASIC keywords, put it on a disk image with the v2g binary, run it in `x128`, take a screenshot.
3. Write the reference first. It forces every item in section 5 to be settled by test.
4. Write the tour chapter by chapter, each with tested listings.
5. Review pass for reading flow, then the German translation.
