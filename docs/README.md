# VDC-BASIC Programmer's Guide

VDC-BASIC extends BASIC 7.0 on the Commodore 128 with commands for the 80-column chip. It is built around moving blocks of video memory (filling, copying, blitting, page flipping, sprites with transparency) and leaves lines and circles to others.

This guide is being written. What exists so far:

| Part | State |
|---|---|
| [Outline](OUTLINE.md) | Structure and contents of the whole guide |
| [Command reference](reference/commands.md) | Complete for v2h |
| [VCL list format](reference/vcl-format.md) | Complete |
| [VMS list format](reference/vms-format.md) | Complete |
| [1. First contact](01-first-contact.md) | Written |
| [2. Two memories](02-two-memories.md) | Written |
| [3. The chip does the work](03-the-chip-does-the-work.md) | Written |
| [4. A canvas](04-a-canvas.md) | Written |
| [5. Sixteen colours per pixel](05-sixteen-colours.md) | Written |
| [6. Getting pictures into VRAM](06-getting-pictures-in.md) | Written |
| Chapters 7–14 | Not written yet |
| [Converter](tools/README.md) (`tools/vdc-converter.html`) | Pictures, tile sheets, fonts and sprites. The conversion code is tested through the listings; the page itself has not been tried in a browser yet. |

The listings of the chapters are in [`listings/`](listings/). They are copied into the text by `listings/sync.py` and run by `test/run.sh -l -d docs/img docs/listings/*.bas`, which also takes the screenshots. The examples in the reference and statements made in the text are checked by the listings in [`test/`](../test/README.md).
