# VMS list format

A run list for [`VMS`](commands.md#vms) describes how the packed sprite data in VRAM is spread over the screen. It lies in CPU RAM bank 0.

## Header

| Offset | Size | Content |
|---|---|---|
| 0 | 1 | Alpha colour, 0–15: the colour that counts as transparent in type 8 entries |
| 1 | 2 | Number of entries, low byte first |
| 3 | | Entries |

## Entries

`VMS` keeps a read position in the sprite data. It starts at the *source* parameter and moves forward as entries use up bytes.

| Type | Bytes | Parameters | Effect |
|---|---|---|---|
| 1 | 4 | offset (word), length (byte) | Copies *length* bytes from the read position to *target* + *offset*. The read position moves on by *length*. |
| 8 | 3 | offset (word) | Takes one byte from the read position and mixes it with the screen byte at *target* + *offset*, one half each (see below). The read position moves on by 1. |
| 2 | 5 | count (word), reps (word) | Copies a rectangle without transparency: *count* bytes per row, *reps* rows, to *target*, row step taken from VDC register 1. Meant as the only entry of a list. |
| 3 | 3 | n (word) | **(v3)** Source skip: the read position moves on by *n* bytes, nothing is written. n = 0 does nothing. |

*offset* is added to the *target* parameter of `VMS`. For a sprite made for a screen with 80 bytes per row, the offset of row *y*, byte *x* is *y* × 80 + *x*.

Any other type prints `INVALID VMS COMMAND` and ends the command.

## Clipping: type 3 (v3)

Type 3 lets a program cut a sprite at the edge of a viewport without changing the sprite data. The data of the part that lies outside is passed over: the read position moves on by *n* bytes, the screen is not touched. A shortened list therefore keeps only the entries for the visible part and replaces each run of dropped entries by one type 3 entry with the sum of their data bytes (type 1: *length*, type 8: 1).

The read position is kept in bytes from *source* on and wraps from 65535 to 0. Type 2 does not move it; use type 3 only together with types 1 and 8.

Example: the sprite below with its first row cut off. The dropped entries 1–3 used 3 data bytes:

| Entry | Bytes | Meaning |
|---|---|---|
| 1 | `3, 3,0` | pass over `$51 $11 $15` |
| 2 | `1, 0,0, 2` | copy 2 bytes (`$22 $22`) to offset 0 |

Complete list: `5, 2,0, 3,3,0, 1,0,0,2` (with *target* one row lower than before).

## Half-transparent bytes: type 8

In the 160×100 mode one byte holds two pixels: the left one in the upper four bits, the right one in the lower four. At the edge of a figure a byte often has one solid and one transparent pixel. Type 8 handles these bytes:

| Sprite byte | Result on screen |
|---|---|
| Right pixel equals the alpha colour | Left pixel from the sprite, right pixel from the screen |
| Otherwise | Right pixel from the sprite, left pixel from the screen |

The left pixel is not compared with the alpha colour. A converter must use type 8 only for bytes with exactly one transparent pixel, type 1 for solid bytes, and no entry at all for fully transparent bytes.

Type 8 reads the screen byte and writes it back through the CPU, so it is slower than a type 1 run.

Fully transparent bytes need not be stored in the sprite data at all, because the read position advances only through entries.

## Example

A sprite of 3 bytes × 2 rows, alpha colour 5. `.` marks a transparent pixel (colour 5), digits are colours.

| Row | Pixels | Bytes |
|---|---|---|
| 0 | `. 1 1 1 1 .` | `$51 $11 $15` |
| 1 | `2 2 2 2 . .` | `$22 $22` (third byte dropped) |

Sprite data in VRAM: `$51 $11 $15 $22 $22`

| Entry | Bytes | Meaning |
|---|---|---|
| 1 | `8, 0,0` | mix `$51` at offset 0: right pixel kept from sprite |
| 2 | `1, 1,0, 1` | copy 1 byte (`$11`) to offset 1 |
| 3 | `8, 2,0` | mix `$15` at offset 2: left pixel kept from sprite |
| 4 | `1, 80,0, 2` | copy 2 bytes (`$22 $22`) to offset 80 |

Complete list: `5, 4,0, 8,0,0, 1,1,0,1, 8,2,0, 1,80,0,2`
