# VDC-BASIC Programmer's Guide

VDC-BASIC extends BASIC 7.0 on the Commodore 128 with commands for the 80-column chip. It is built around moving blocks of video memory (filling, copying, blitting, page flipping, sprites with transparency) with a few commands for unpacking pictures and for drawing lines and boxes (v3), and leaves circles and polygons to others.

This is the first complete draft, in English. It describes VDC-BASIC **v3** (`vdcbasic3.bin` at `$1C00`, BASIC programs from `$4001`, loaded by `BOOT`); the earlier v2h is no longer covered.

| Part | State |
|---|---|
| [Outline](OUTLINE.md) | Structure and contents of the whole guide |
| [Command reference](reference/commands.md) | Complete for v3 |
| [VCL list format](reference/vcl-format.md) | Complete for v3 (entry types 8, 9, 10) |
| [VMS list format](reference/vms-format.md) | Complete for v3 (source skip, type 3) |
| [VRL format](reference/vrl-format.md) | Complete (the streams of `RXV`) |
| [1. First contact](01-first-contact.md) | Written (loading with `BOOT`) |
| [2. Two memories](02-two-memories.md) | Written |
| [3. The chip does the work](03-the-chip-does-the-work.md) | Written |
| [4. A canvas](04-a-canvas.md) | Written (with `VDM`, `VDL`, `VDR`, `VDB` in bitmap mode) |
| [5. Sixteen colours per pixel](05-sixteen-colours.md) | Written (drawing in chunky mode) |
| [6. Getting pictures into VRAM](06-getting-pictures-in.md) | Written (with `RXV`) |
| [7. The blit](07-the-blit.md) | Written |
| [8. Two screens, no flicker](08-two-screens.md) | Written |
| [9. Scrolling and wide worlds](09-scrolling.md) | Written |
| [10. Words on a bitmap](10-words-on-a-bitmap.md) | Written |
| [11. Sprites with holes](11-sprites-with-holes.md) | Written (with clipping by source skip) |
| [12. Scripts for the chip](12-scripts-for-the-chip.md) | Written (entry types 8, 9, 10) |
| [13. Speed, memory and other limits](13-speed-and-limits.md) | Written |
| [14. The room](14-the-room.md) | Written |
| [Converter](tools/README.md) (`tools/vdc-converter.html`) | Pictures, tile sheets, fonts and sprites. The conversion code is tested through the listings; the page itself has not been tried in a browser yet. |

The listings of the chapters are in [`listings/`](listings/). They are copied into the text by `listings/sync.py` and run by `test/run.sh -l -d docs/img docs/listings/*.bas`, which also takes the screenshots. The examples in the reference and statements made in the text are checked by the listings in [`test/`](../test/README.md).

To try the listings, build a disk image with `listings/mkdisk.sh`. It contains `BOOT`, VDC-BASIC, all listings as programs (tokenised for BASIC at `$4001`) and the data files they load. In VICE: `x128 -80col -VDC64KB -8 vdcbasic-guide.d64`, then `RUN "BOOT"` (once after every reset), then `RUN "07-TILES"`.

The pictures in `art/` were made for the Dungeon Crawler Jam 2025 game by the maintainer of VDC-BASIC; `art/dungeon.png` and `art/tiles.png` were assembled from them for this guide.
