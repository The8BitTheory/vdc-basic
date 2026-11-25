# VRL format

A VRL stream ("VDC run length", version 1) is what [`RXV`](commands.md#rxv) unpacks into VRAM (v3). It lies in CPU RAM bank 0. The tool `tools/vrl.py` encodes, decodes and analyses such streams (`vrl.py encode`, `rect`, `decode`, `stats`); it is the reference for the format.

## Tokens

A stream is a sequence of tokens. The first byte of a token says what it is. Words are low byte first.

| First byte b | Followed by | Meaning | Bytes |
|---|---|---|---|
| `$00` | – | End of the stream | 1 |
| `$01`–`$7F` | b bytes | Literal: write the b bytes (1–127) | 1 + b |
| `$80`–`$FD` | value | Run: write *value* L = b − 126 times (2–127) | 2 |
| `$FE` | lo, hi | Skip: move the VRAM address on by n = hi·256 + lo bytes without writing; n = 0 means 65536 | 3 |
| `$FF` | lo, hi, value | Long run: write *value* L = hi·256 + lo times; L = 0 means 65536 | 4 |

The VRAM address starts at the *dst* parameter of `RXV` and moves on by one for every byte written or skipped. Skipped bytes keep their content, which is how a stream draws a rectangle with a row length (stride) or a picture with transparent parts.

A decoder must accept every valid token sequence, also those the encoder never produces: a long run with L = 1, two runs of the same value in a row, a skip directly before the end, a skip of 65536 bytes.

`RXV` writes literals with the CPU, runs shorter than the threshold *T* (`$1C04`) with the CPU and longer runs with the VDC's block fill (first byte by the CPU, then register 30). A skip reads the current address from registers 18/19 and writes it back increased by n. The result in VRAM does not depend on *T*.

## File

A VRL file is a PRG file:

| Offset | Size | Content |
|---|---|---|
| 0 | 2 | Load address (default `$2000`) |
| 2 | 2 | `"VR"` (`$56 $52`) |
| 4 | 1 | Version, 1 |
| 5 | 2 | Unpacked length U, 0–65535 |
| 7 | | Stream, up to and including the `$00` |

U is the number of VRAM bytes from the start to behind the last byte written, the sum of all literal, run and skip lengths. The encoder does not write a skip at the end of a stream.

After `BLOAD"PIC.VRL",B0,P(A)` the header lies at *A* (the load address is not loaded into memory) and the stream at *A* + 5:

```
BLOAD"PIC.VRL",B0,P(DEC("8000")) : RXV DEC("8000")+5,0
```

`RXV` does not read the header. Streams without a header (raw streams, `vrl.py --raw`) can be embedded in other data.

## Example

A 2 × 2 rectangle with the bytes 1, 2 / 3, 4 at a row length of 80 (`vrl.py rect --width 2 --height 2 --stride 80`):

| Bytes | Meaning |
|---|---|
| `02 01 02` | literal: 1, 2 |
| `FE 4E 00` | skip 78 bytes to the next row |
| `02 03 04` | literal: 3, 4 |
| `00` | end |

```
10 BANK 0:A=DEC("A000")
20 FOR I=0 TO 9:READ B:POKE A+I,B:NEXT
30 DATA 2,1,2, 254,78,0, 2,3,4, 0
40 RXV A,1000
```
