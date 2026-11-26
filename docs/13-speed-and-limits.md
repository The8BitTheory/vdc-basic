# 13. Speed, memory and other limits

Before the last chapter builds something complete, this one collects what the earlier chapters learned about the three things that are always short on an 8-bit machine: time, memory, and patience with mistakes that produce no error message.

## Where the time goes

All times in this book were measured in the emulator VICE, on a PAL machine in its normal `SLOW` mode. They have not been compared with a real VDC, whose block operations may take longer than the emulator's. Read them as proportions. A jiffy is 1/60 second.

| What | Jiffies |
|---|---|
| 2000 single `VMW` in a loop | 915 |
| `VMF` over the same 2000 bytes | 1 |
| Copying a whole page of 8000 bytes with `VMC` | under 2 |
| Blit of a 73 × 32 byte picture, one `VMC` | under 2 |
| The same with one `VMC` per row from a BASIC loop | 65 |
| 60 tile blits, one statement each | 131 |
| The same 60 blits as a command list | 14 |
| A 16 × 32 byte sprite with `VMS` | under 3 |
| `RTV` or `VTR` of 8000 bytes | 26 |

The pattern is hard to miss. One statement costs about two jiffies, nearly whatever it does: `VMC SH+T*128,P+R*16*80+C*8,8,16,80` is mostly BASIC reading text and doing floating point arithmetic. What the chip then does is, by comparison, free.

From this follow the rules of thumb:

1. **Count statements, not bytes.** Copying 8000 bytes costs about as much as copying 8. Wiping a whole page is cheaper than working out which part of it needs wiping.
2. **Let one statement do more.** Repetitions and steps instead of loops. A list instead of many statements.
3. **Calculate early.** An address that is worked out inside a loop is worked out every time. Put it in a variable before the loop, or in an array: a table `RA(Y)=P+Y*80` for all rows turns a multiplication into a lookup.
4. **Cross the border once.** `RTV`, `VTR` and `SWP` are the slow road. Load everything at the start; afterwards stay inside VRAM.
5. **Draw unseen.** What is built on a hidden page may take as long as it takes.

### What one row costs

Version 3 made the loop that `VMC`, `VMF`, `VCL` and `VMS` run for every row of a blit about a quarter faster; the results are the same. These are processor cycles per row at 1 MHz (`SLOW`), measured in VICE with a timer, before and after:

| One row of | Before | Now |
|---|---|---|
| `VMC`, 8 bytes | 180 | 136 |
| `VMC`, 8 bytes, with source step | 274 | 200 |
| `VMC`, 40 bytes | 181 | 149 |
| `VMF`, 8 bytes | 230 | 173 |
| `VMS` entry of type 2, 8 bytes | 183 | 137 |

A row is about a hundred cycles of CPU work. The rest of the time the CPU waits until the VDC says it is ready: the program polls a status bit before it touches a VDC register again. So the gain is real for short rows and small for long ones, where the chip's own copying time dominates. A tile blit of 16 rows costs around 2200 cycles, about two milliseconds; the statement around it still costs two jiffies, and that is why a list is the faster way to many blits.

These figures come from the emulator. In another emulator that models the VDC more closely (Z64K) the numbers moved in the same direction and by about the same amount, with more scatter from run to run, and on a real VDC they may be different. The jiffy figures in the table at the top of the chapter were measured before this change and have not been measured again; one statement is still mostly BASIC, so they hardly change.

### Waiting for the chip

Why not skip some of the waits and let the CPU run ahead? An experiment in Z64K says that two waits per row are needed and that each one has a reason: one before the first register access after a copy was started (the previous row must be finished), and one before the byte count is written (the VDC must have read ahead the first byte). The waits in between are not needed and could be left out, which saves 5 to 17 per cent per row for short rows at 2 MHz. Waiting only every few rows fails. In the 160 × 100 mode the VDC has only about 16 free memory cycles per character row while it draws the screen, so even a copy of one byte can take longer than the CPU takes to the next row; only in the blank between two frames does the chip finish in time. Whoever writes his own code against `$D600` and `$D601` should wait for bit 7 before every access to `$D601`.

The refresh register 36 (the Kernal sets it to 5) has no influence on speed in that emulator when it is 0 or 1; larger values slow short rows down. It matters on real hardware, not for speed but for the memory itself: the VDC's RAM is dynamic. With register 36 at 0, the chip refreshes only while it draws the screen, and in the vertical blank for a few milliseconds not at all. Nobody has measured on a real machine yet whether that costs data; the figures and the test program (`docs/v3/P2g-wait-experiment.md`) are there for whoever tries. If you leave the register as the Kernal sets it, you are safe.

### FAST

The C128 can run its processor at twice the speed. The price is that the 40-column screen goes blank, which a program for the 80-column screen does not care about. `FAST` switches it on, `SLOW` off.

| What | `SLOW` | `FAST` |
|---|---|---|
| 60 tile blits, one statement each | 131 | 65 |
| 20 page copies | 35 | 23 |
| `RTV` of 8000 bytes | 26 | 15 |

Everything in which BASIC or the CPU has a share gets nearly twice as fast. Put `FAST` at the beginning of a program and `SLOW` at its end.

### Compiling

Since most of the time goes into BASIC interpreting statements, a compiler helps exactly where it hurts. The Blitz BASIC compiler for the C128 can compile programs that use VDC-BASIC. It asks for one thing: every VDC-BASIC instruction must have two colons in front of it.

```
100 ::VMC SH,P+Y*80+X,W,H,80
110 IF K$="L" THEN ::VMF P,0,8000
```

The interpreter reads the two colons as two empty statements and ignores them, so a program written this way runs both compiled and uncompiled. If you think you may compile a program one day, write the colons from the start.

With v3 one more rule applies. The compiled program must use the runtime that loads at `$4001`, above VDC-BASIC (Blitz! runtime image 2). The compiler picks it when the program contains a `GRAPHIC` statement, so a line like `63999 GRAPHIC 0`, which is never executed, is enough. The compiled program is started through `BOOT`, with `50 RUN"PROGRAM"`. Details are in the [README](../README.md#compiling-programs).

## A budget for VRAM

64 KiB is a lot until the second page, the clean background and three sheets have moved in. It pays to write the plan down before writing the program. For the 160 × 100 mode:

| Item | Bytes |
|---|---|
| The bitmap of 15s (fixed cost) | 16000 |
| One page, one stored full-screen picture | 8000 |
| A tile of 16 × 16 pixels | 128 |
| A font of 96 characters, 6 × 6 pixels | 1728 |
| A sprite | only its solid pixels, half a byte each |
| A world of twice the screen width | 16000, and the bitmap grows to 32000 |

The plan of the next chapter, as an example:

| Address | Bytes | Content |
|---|---|---|
| 0 | 16000 | bitmap |
| 16000 | 8000 | page 0 |
| 24000 | 8000 | page 1 |
| 32000 | 8000 | the room, clean |
| 40000 | 1024 | 8 tiles |
| 42000 | 172 | 2 sprites |
| 48000 | 1728 | font |

More than 15000 bytes are still free, enough for a second room in store or a hundred more tiles.

The lists for `VMS` and `VCL` do not count here. They live in the computer's RAM, in bank 0, together with the load buffer and the BASIC program. A sprite list is typically twice as long as the sprite's pixel data.

## With 16 KiB

A VDC with 16 KiB cannot do the 160 × 100 mode: bitmap and one page alone are 24000 bytes. What does work:

- Everything in text mode: direct access to characters and attributes, fill and copy, scrolling, changed character sets. In text mode 4096 bytes of VRAM are free, from 4096 to 8191; that is two more screens of characters to flip to with `DISP`.
- The 640 × 200 mode in two colours, as in chapter 4.
- A second picture kept in RAM and exchanged with `SWP`.

A program should check with `RGD(255)` and say so, as the listings in this book do.

## Things that go wrong without a message

VDC-BASIC checks that numbers are in range, and nothing else. The following mistakes all run without an error and produce a wrong picture, or none.

**The 64 KiB were not switched on.** Without `RGO 28,16`, addresses above 16383 land in the lower 16 KiB. Typical symptom: a picture that was stored "off screen" appears in the middle of the screen, or a sheet is destroyed when a page is cleared. Remember that `RST 1` switches them off again.

**A count of 0.** `VMF` with a count of 0 fills all 65536 bytes. `RTV`, `VTR` and `SWP` with a count of 0 transfer 65536 bytes. `VMC` with a count of 0 copies nothing. A repetition count of 0 means 256. These usually come from a variable that was never set.

**A variable name that BASIC reads differently.** `ST`, `TI`, `DS`, `ER` and `EL` belong to BASIC. And no name may contain a keyword, old or new: `ATTR`, `DISP`, `SYN`, `RST`, `SWP`, `CRSR`, and all the three-letter commands. Usually this gives a syntax error, which is the pleasant case.

**`PEEK` and `POKE` in the wrong bank.** `RTV`, `VTR`, `VMS`, `VCL` and `VMB` always use bank 0. `PEEK` and `POKE` use whatever `BANK` says, which is 15 unless you change it, and in bank 15 everything from `$4000` up is ROM. Wrap them in `BANK 0` ... `BANK 15`.

**The program grew into its buffer.** The listings load files to `$8000` and put lists at `$A000`. A BASIC program starts at `$4001`, so it can grow to 16 KiB before it reaches `$8000`. Loading a file then overwrites the end of the program.

**VDC-BASIC overwritten.** VDC-BASIC occupies `$1C00` to `$2EE9`, and `$3FFF` is the limit. `GRAPHIC 1` to `GRAPHIC 4` build the VIC bitmap and its colours in exactly that area. `GRAPHIC CLR` clears it. Both destroy VDC-BASIC without a message; the next new keyword crashes the machine. A program must not use them. The area `$1300`-`$1BFF` below VDC-BASIC is free, and lists that must lie below `$4000` can go there.

**Drawing outside the picture.** Nothing is clipped, not even by `VDL`, `VDR` and `VDB`. A blit that leaves the page at the bottom continues in whatever lies behind the page. One that leaves it at the right edge continues at the left edge, one row down; so does a line with an *x* beyond the end of the row.

**A copy onto itself in the wrong direction.** Copying to a higher address within the source range repeats the beginning of the source (chapter 3).

**A list in the wrong place.** Lists for `VCL` and `VMS`, and streams for `RXV`, cannot lie between `$D000` and `$DFFF`. A list in bank 1 (`VCL` type 9) must lie in `$4000`-`$CFFF` or `$E000`-`$FEFF`. None of this is checked.

**A stream that does not end.** `RXV` reads tokens until it meets a zero byte. A stream without one, or an address that points at the header of the file instead of 5 bytes behind it, makes it read garbage.

**The step that was always 80.** Blits, fonts and sprite lists are made for a certain row length. In a wide world (chapter 9) the step is the row length of that world, `VCS` needs its fourth parameter, and sprites must be converted for that row length.

And one that does give a message, but a confusing one: after a program has stopped with an error in a bitmap mode, the message is there, you just cannot read it. Type `RST 3` blind and press RETURN, then clear the screen.

[Next: The room](14-the-room.md)
