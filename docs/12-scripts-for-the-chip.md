# 12. Scripts for the chip

The room of chapter 7 took a little over two seconds to appear: sixty blits, each of which the VDC finishes in a blink. We have met this pattern in every chapter. The chip is fast and BASIC is not, and whatever keeps BASIC out of the way makes a program faster. `VMF` replaced a loop of 2000 statements. The long form of `VMC` replaced a loop of 32. Now we replace the sixty.

What does BASIC do sixty times? It reads the statement, looks up `SH`, `T`, `P`, `R` and `C`, multiplies and adds in floating point, and converts five results into whole numbers. Only then does `VMC` get its five numbers. All this effort produces the same five numbers every time the room is drawn, as long as the map does not change.

So let us work them out once and keep them.

## A list of commands

```
VCL address
```

"VDC command list". At the given address in RAM, bank 0, lies a list of entries. Each entry stands for one VDC-BASIC command: a byte that says which command, followed by the parameters as plain numbers, two bytes each, low byte first. A zero byte ends the list. `VCL` runs through it in machine language and performs one command after the other.

The most useful entries:

| Type byte | Parameters (two bytes each) | Same as |
|---|---|---|
| 1 | source, target, count | `VMC` with three parameters |
| 2 | source, target, count, repetitions, target step | `VMC` with five: the blit |
| 3 | source, target, count, repetitions, target step, source step | `VMC` with six |
| 4 | address, value, count | `VMF` with three |
| 5 | address, value, count, repetitions, step | `VMF` with five: the rectangle |
| 6 | address, value | `VMW` |
| 8 | src, dst | `RXV`: unpack a packed picture (v3) |
| 9 | source, target, list in bank 1 | `VMS`: a sprite whose list is in RAM bank 1 (v3) |
| 10 | address | none: the list continues at *address* (v3) |
| 12 | source, target, list | `VMS`: a sprite |
| 13 | n, then n pairs register, value | `SYN`: wait, then write the registers (n = 0: only wait) |
| 0 | | end of the list |

The complete table is in the reference: [VCL list format](reference/vcl-format.md).

A blit of a tile, `VMC 40128,16008,8,16,80`, becomes these eleven bytes:

```
2,  192,156,  136,62,  8,0,  16,0,  80,0
```

2 is the type. 40128 is 156 × 256 + 192, so its two bytes are 192 and 156, and so on.

## Writing the list

A list can be typed in as `DATA`, calculated by a tool on the PC, or written by the program itself. The last way is the most convenient: the program still contains the map in a readable form, and a loop turns it into a list.

<!-- listing: 12-list.bas -->
```
10 REM SCRIPTS FOR THE CHIP: THE ROOM OF CHAPTER 7 AS A COMMAND LIST
20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END
30 GOSUB 9000:P=16000
40 BF=DEC("8000"):SH=40000:FT=48000
50 BLOAD "TILES",B0,P(BF):RTV BF+2,SH,1024
60 BLOAD "FONT",B0,P(BF):RTV BF+3,FT,1728:VCS FT,3,6
70 VMP P+45*80+20,"WRITING THE LIST"
80 L=DEC("A000"):A=L:BANK 0:REM THE LIST STARTS AT L IN RAM
90 FOR R=0 TO 5:READ M$
100 : FOR C=0 TO 9:T=VAL(MID$(M$,C+1,1))
110 : POKE A,2:A=A+1:REM ENTRY TYPE 2: A BLIT WITH FIVE PARAMETERS
120 : W=SH+T*128:GOSUB 500:REM SOURCE
130 : W=P+R*16*80+C*8:GOSUB 500:REM TARGET
140 : W=8:GOSUB 500:W=16:GOSUB 500:W=80:GOSUB 500
150 : NEXT
160 NEXT
170 POKE A,5:A=A+1:REM ENTRY TYPE 5: A FILLED RECTANGLE
180 W=P+96*80:GOSUB 500:W=DEC("88"):GOSUB 500
190 W=80:GOSUB 500:W=4:GOSUB 500:W=80:GOSUB 500
200 POKE A,0:BANK 15:REM A ZERO ENDS THE LIST
210 T=TI:VCL L:T=TI-T
220 VMP P+85*80+2,"61 COMMANDS IN"+STR$(T)+" JIFFIES"
230 GETKEY K$
240 RST 3:PRINT CHR$(147):END
300 DATA "1111111111"
310 DATA "1000000221"
320 DATA "1007700021"
330 DATA "1007700001"
340 DATA "1200000001"
350 DATA "1111111111"
500 REM --- PUT THE NUMBER W INTO THE LIST AS TWO BYTES ---
510 H=INT(W/256):POKE A,W-256*H:POKE A+1,H:A=A+2:RETURN
9000 REM --- 160 X 100 PIXELS, 16 COLOURS. NEEDS 64 KIB VRAM ---
9010 IF RGD(255)=0 THEN PRINT "THIS PROGRAM NEEDS 64 KIB VRAM":END
9020 RST 1:RGO 28,16
9030 T=(RGD(4)+1)*8+(RGD(5) AND 31):V=RGD(7)
9040 RGW 9,1:RGW 4,T/2-1:RGW 6,100:RGW 7,V*4
9050 VMF 0,15,16000:VMF 16000,0,8000
9060 DISP 0:ATTR 16000:RGO 25,128
9070 RETURN
```

![The room, drawn by one statement](img/12-list.png)

Lines 90 to 160 are the loop of chapter 7, but where that one had a `VMC`, this one writes an entry into the list. `A` is the address of the next free byte. Line 110 pokes the type. Lines 120 to 140 hand each parameter to the little subroutine at line 500, which splits a number into its two bytes and pokes them.

Lines 170 to 190 add the red bar, an entry of type 5.

Line 200 closes the list with a zero.

Writing the list is slow, slower than drawing the room directly was. That is the investment.

Line 210 is the return on it. The room appears as if it had always been there. Line 220 prints how long it took: 14 jiffies for 61 commands, against 133 jiffies for the 60 blits in chapter 7. And nearly all of those 14 belong to the VDC now.

## Lists are data

Once a screen is a list, new things become easy.

**Save it.** A list is a block of memory. `BSAVE "ROOM",B0,P(L) TO P(A+1)` writes it to disk, and the next time the program loads it instead of building it. Levels can be files.

**Keep several.** One list per room, per menu, per kind of wall. Showing something is one `VCL` with the right address. The view of a first-person dungeon game is a natural case: each wall piece at each distance is a few blits, and what is visible from where the player stands decides which lists are run, back to front.

**Mix commands.** A list can clear a page, lay the tiles, draw the frame around the status display and place three sprites, in one go. A `SYN` entry (type 13) can stand anywhere in the list, as often as needed: it waits for the vertical blank and can set registers there, for example to show a page or to change the colours from that point on.

**Change one number.** The entries are at known places in memory. Poke a different source address into the third tile's entry, and that tile is a different one the next time the list runs. A tile that is animated, a door that opens.

Two things a list cannot do. It cannot decide anything: there is no "if". And every address in it is fixed when it is written. A list that draws onto page 0 always draws onto page 0. To use lists with page flipping, write one list per page, which can also show its page with a type 13 entry that writes R12 and R13, or let the list draw into a store from which the pages are refreshed, as in chapter 8.

## Three more entries

Version 3 added three entries to the list.

**Type 8** is `RXV` (chapter 6): `8, src, dst`. A list can unpack tiles or parts of a picture one after the other without BASIC evaluating a statement for each. An entry costs about 270 cycles of the CPU before the unpacking starts; the `RXV` statement of BASIC costs several thousand.

**Type 10** is a jump: `10, address`. The next entry is read from that address. It draws nothing. Two uses: a list can have a fixed entry address for the program while its real content moves around (a program that builds its list from the end backwards does not know where the beginning is going to be), and several short lists can be chained into one. A list that jumps back into itself never ends.

**Type 9** is `VMS` like type 12, with the sprite list in RAM bank 1 instead of bank 0. A program that keeps its sprite lists in bank 1 does not have to copy them down to bank 0 before they are used. The list must lie in `$4000`-`$CFFF` or `$E000`-`$FEFF` of bank 1, and the program must keep that area free of its variables itself (BASIC puts variables and strings there). While the entry runs, the lowest 16 KiB of RAM are common to both banks, so that VDC-BASIC stays visible; the `VCL` list itself stays in bank 0.

<!-- listing: 12-chain.bas -->
```
10 REM SCRIPTS FOR THE CHIP: UNPACK A PICTURE, JUMP, AND DRAW SPRITES FROM BANK 1
20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END
30 GOSUB 9000:P=16000
40 BF=DEC("8000"):SP=42000:L=DEC("A000"):M=DEC("A100"):G=DEC("9000")
50 BLOAD "DUNGEON.VRL",B0,P(BF):REM THE PACKED PICTURE STAYS IN RAM
60 BLOAD "GOBLIN0",B0,P(G)
70 BANK 0:LL=PEEK(G+2)+256*PEEK(G+3):DL=PEEK(G+4)+256*PEEK(G+5)
80 RTV G+6+LL,SP,DL:REM THE PIXEL DATA GOES TO VRAM ...
90 FOR I=0 TO LL-1:BANK 0:X=PEEK(G+6+I):BANK 1:POKE G+I,X:NEXT:REM ... THE SPRITE LIST TO BANK 1
100 BANK 0:A=L:REM THE FIXED ENTRY: A JUMP TO THE REAL LIST
110 POKE A,10:A=A+1:W=M:GOSUB 500
120 A=M:POKE A,8:A=A+1:W=BF+5:GOSUB 500:W=P:GOSUB 500:REM TYPE 8: UNPACK THE PICTURE
130 FOR X=20 TO 44 STEP 24:REM TYPE 9: A SPRITE WITH ITS LIST IN BANK 1, TWICE
140 : POKE A,9:A=A+1:W=SP:GOSUB 500:W=P+40*80+X:GOSUB 500:W=G:GOSUB 500
150 NEXT
160 POKE A,0:BANK 15
170 VCL L
180 GETKEY K$
190 RST 3:PRINT CHR$(147):END
500 REM --- PUT THE NUMBER W INTO THE LIST AS TWO BYTES ---
510 H=INT(W/256):POKE A,W-256*H:POKE A+1,H:A=A+2:RETURN
9000 REM --- 160 X 100 PIXELS, 16 COLOURS. NEEDS 64 KIB VRAM ---
9010 IF RGD(255)=0 THEN PRINT "THIS PROGRAM NEEDS 64 KIB VRAM":END
9020 RST 1:RGO 28,16
9030 T=(RGD(4)+1)*8+(RGD(5) AND 31):V=RGD(7)
9040 RGW 9,1:RGW 4,T/2-1:RGW 6,100:RGW 7,V*4
9050 VMF 0,15,16000:VMF 16000,0,8000
9060 DISP 0:ATTR 16000:RGO 25,128
9070 RETURN
```

![A picture unpacked by the list, two sprites drawn from bank 1](img/12-chain.png)

The list in this program lies at `L`, `$A000`. Its only entry is a jump (line 110) to the real list at `$A100`; the program always calls `VCL L`, whatever happens to the rest. The real list starts with an `RXV` entry for the packed dungeon of chapter 6 (line 120), followed by two sprites (lines 130 to 150). The sprite list of the goblin was copied to bank 1 in line 90, with `BANK 1` for the `POKE`, and the pixel data went to VRAM in line 80. Both entries of type 9 use the same list in bank 1, at `$9000`.

## Fine print

- The list must be in bank 0 (the sprite lists of type 9 are the exception, they are in bank 1). It can be anywhere there except `$D000`–`$DFFF`, and as long as you like.
- In the list every parameter is two bytes, also those that are a single byte in BASIC.
- Negative steps are written as 65536 minus the amount: −80 is 65456, the bytes 176 and 255.
- A type byte that `VCL` does not know stops the list with the message `INVALID`. Type 11 and all types above 13 are unknown.

## What we have so far

| Command | Does |
|---|---|
| `VCL address` | runs a list of copy, fill, write, unpack, sprite and `SYN` commands, with jumps |

That was the last command. You have now met all of VDC-BASIC. The remaining chapters put the pieces together.

[Next: Speed, memory and other limits](13-speed-and-limits.md)
