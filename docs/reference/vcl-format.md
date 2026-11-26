# VCL list format

A command list for [`VCL`](commands.md#vcl) is a sequence of entries in CPU RAM bank 0, ended by a zero byte.

Each entry starts with a type byte. All parameters are 16-bit words, low byte first, also where the BASIC command takes a byte. Steps are signed (65456 = −80).

| Type | Bytes | Parameters | Same as |
|---|---|---|---|
| 0 | 1 | none | end of list |
| 1 | 7 | source, target, count | `VMC source,target,count` |
| 2 | 11 | source, target, count, reps, targetstep | `VMC` with 5 parameters |
| 3 | 13 | source, target, count, reps, targetstep, sourcestep | `VMC` with 6 parameters |
| 4 | 7 | vaddr, byte, count | `VMF vaddr,byte,count` |
| 5 | 11 | vaddr, byte, count, reps, step | `VMF` with 5 parameters |
| 6 | 5 | vaddr, byte | `VMW vaddr,byte` |
| 7 | 9 | vaddr, byte, reps, step | `VMW` repeated *reps* times, *vaddr* growing by *step* |
| 8 | 5 | src, dst | `RXV src,dst` (v3) |
| 9 | 7 | source, target, list address in **bank 1** | `VMS source,target,addr` with the list in RAM bank 1 (v3) |
| 10 | 3 | address | no BASIC counterpart: the list continues at *address* in bank 0 (v3) |
| 12 | 7 | source, target, list address | `VMS source,target,addr` |
| 13 | 3 + 4·n | n, then n pairs reg, value | `SYN reg,value,...` (n = 0: plain `SYN`) |

Notes:

- *reps*: only the low byte counts. In types 2, 3 and 5, 0 means 256; in type 7, 0 means 1.
- *byte*: only the low byte counts.
- Type 7 has no BASIC counterpart. It draws dotted or vertical lines of single bytes.
- *n* (type 13): the low byte is the number of pairs, 0 to 255, so an entry can be longer than 256 bytes. *reg* and *value*: only the low byte counts. With n = 0 the entry only waits, like plain `SYN`. With n > 0 it waits until the end of the text window (start of the vertical border area), then writes all registers one after the other with interrupts disabled. A page flip through R12/R13 therefore happens in one piece in the border.
- High byte of *n* (type 13): 0 waits like `SYN`, for the *start* of the border; if the entry is reached while the border is already showing, that is almost a whole frame. Any other value only waits if the text window is being drawn, and continues at once inside the border. That halves the average wait, about 10 ms → 4 ms in PAL, but two such entries can pass in the same frame, and an entry reached in the last moments of the border may finish its writes after the next frame has begun. For a page flip, choose page addresses whose low bytes are equal, so that only one register changes.
- Type 8 (v3): *src* is the address of a [VRL stream](vrl-format.md) in bank 0, *dst* the VRAM address. The entry costs about 270 CPU cycles before the unpacking starts, a `RXV` statement in BASIC several thousand; use lists for many small streams.
- Type 9 (v3): like type 12, but the [VMS list](vms-format.md) is read from RAM bank 1. It must lie completely in `$4000`–`$CFFF` or `$E000`–`$FEFF` of bank 1 (not checked). While the entry runs, the lowest 16 KB of RAM are common to both banks (`$D506`, restored afterwards), so that VDC-BASIC stays visible; interrupts stay enabled. Bank 1 also holds BASIC's variables and strings, so a BASIC program must keep the list's area free itself. Reading the list costs the same as with type 12, the entry about 15 cycles more. Meant for programs that keep sprite lists in bank 1 and would otherwise copy them to bank 0 first.
- Type 10 (v3): the next entry is read from *address* (bank 0, same rules as the start address of `VCL`). Nothing is drawn; the entry costs about 215 CPU cycles, like the bookkeeping of any other entry. A program can keep a fixed start address while the rest of the list moves, or chain several lists. A list that jumps back into itself never ends.
- Type 11 and all types above 13 are not implemented. They print `INVALID` and end the list. (Type 8 was once planned for `VMO`; since v3 it is `RXV`.)

## Example

Fill a 160×100 page at VRAM 0 with colour 0, then blit a packed 4×16 tile from VRAM 20000 to row 10, byte 20.

| Bytes | Meaning |
|---|---|
| `4, 0,0, 0,0, 64,31` | type 4: `VMF 0,0,8000` |
| `2, 32,78, 52,3, 4,0, 16,0, 80,0` | type 2: `VMC 20000,820,4,16,80` |
| `0` | end |

```
10 A=DEC("4000"):BANK 0
20 FOR I=0 TO 18:READ B:POKE A+I,B:NEXT
30 DATA 4, 0,0, 0,0, 64,31
40 DATA 2, 32,78, 52,3, 4,0, 16,0, 80,0
50 DATA 0
60 VCL A
```

Flip to the page at VRAM 16384 (`$4000`, bytes `$40,$00` for R12 and R13) right after the wait:

| Bytes | Meaning |
|---|---|
| `13, 2,0, 12,0, 64,0, 13,0, 0,0` | type 13: `SYN 12,64,13,0` (3 + 4·2 = 11 bytes) |
| `0` | end |

The same with `13, 2,1, …` does not wait if the border has already begun.
