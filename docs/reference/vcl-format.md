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
| 12 | 7 | source, target, list address | `VMS source,target,addr` |

Notes:

- *reps*: only the low byte counts. In types 2, 3 and 5, 0 means 256; in type 7, 0 means 1.
- *byte*: only the low byte counts.
- Type 7 has no BASIC counterpart. It draws dotted or vertical lines of single bytes.
- Types 8–11 and all types above 12 are not implemented. They print `INVALID` and end the list.

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
