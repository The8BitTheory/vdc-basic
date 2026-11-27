# 6. Getting pictures into VRAM

Nobody wants to draw a dungeon wall with `VMW`. Pictures are drawn on a PC, with whatever program you like, and brought to the C128 as files. This chapter builds the road they travel on.

It has three stations. The disk drive can only deliver into the computer's memory. So a picture goes from disk into RAM, and from RAM across to VRAM:

```
disk  --BLOAD-->  RAM (bank 0)  --RTV-->  VRAM
```

## From RAM to VRAM

The second leg is new. `RTV`, "RAM to VRAM", copies a block of the computer's memory into the VDC's memory:

```
RTV address, vaddr, count
```

Its mirror image is `VTR vaddr, address, count`, "VRAM to RAM", for the way back.

Both always work with RAM bank 0, whatever `BANK` says, and they see the real RAM everywhere, also where BASIC would see ROM. They have to push every byte through the narrow door between the two memories, so they are slower than a copy inside VRAM. A full picture of 8000 bytes takes a little under half a second. Compared with the 915 jiffies of chapter 3 for a quarter of that, we will not complain.

## Where to put the buffer

Bank 0 holds your BASIC program, from `$4001` upwards (chapter 2 has the map). The variables are in bank 1 and do not concern us. Everything in bank 0 above the end of the program is free, and a short program ends far below `$8000`. So the listings in this book use `$8000` as the start of a buffer. That leaves 32 KiB up to the top of memory, and the program itself may grow to 16 KiB. If you write a longer program, move the buffer up.

## The file format

A picture file for this book is as simple as it can be:

| Bytes | Content |
|---|---|
| 2 | load address, as in every Commodore program file. Ignored. |
| 1 | width in bytes (half the width in pixels) |
| 1 | height in rows |
| width × height | the picture, row after row, without gaps |

<!-- listing: 06-viewer.bas -->
```
10 REM GETTING PICTURES INTO VRAM: A PICTURE VIEWER
20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END
30 GOSUB 9000:P=16000
40 BF=DEC("8000"):REM A BUFFER IN CPU RAM, BANK 0
50 BLOAD "DUNGEON",B0,P(BF)
60 BANK 0:W=PEEK(BF):H=PEEK(BF+1):BANK 15
70 RTV BF+2,P,W*H
80 GETKEY K$
90 RST 3:PRINT CHR$(147):END
9000 REM --- 160 X 100 PIXELS, 16 COLOURS. NEEDS 64 KIB VRAM ---
9010 IF RGD(255)=0 THEN PRINT "THIS PROGRAM NEEDS 64 KIB VRAM":END
9020 RST 1:RGO 28,16
9030 T=(RGD(4)+1)*8+(RGD(5) AND 31):V=RGD(7)
9040 RGW 9,1:RGW 4,T/2-1:RGW 6,100:RGW 7,V*4
9050 VMF 0,15,16000:VMF 16000,0,8000
9060 DISP 0:ATTR 16000:RGO 25,128
9070 RETURN
```

![A picture loaded from disk](img/06-viewer.png)

Line 50 loads the file into the buffer. The `P(BF)` tells `BLOAD` to put it at our address instead of the one stored in the file.

Line 60 reads width and height. Mind the `BANK 0`: without it, `PEEK` would look into the BASIC ROM, which also lives at `$4000`. `BANK 15` afterwards restores the normal state. This is the one place where `BANK` matters; `RTV` does not care.

Line 70 sends the picture across. The data starts two bytes into the buffer, after width and height. The picture in the file is a full screen, 80 bytes × 100 rows, so it can go straight to `P` in one piece.

That is a picture viewer in five lines.

## Making the files

The road needs a first station on the PC as well: something that turns a PNG into this format. The repository has it in `docs/tools/vdc-converter.html`. It is a single page that you open in a browser, from your disk; it needs no installation and no network.

You give it a picture in its final size, 160 × 100 pixels or smaller. It matches every pixel to the nearest of the 16 VDC colours, shows you the result, and saves a file. It also shows the BASIC lines that load the file. It can make four kinds of file:

| Kind | For | Chapter |
|---|---|---|
| Picture or tile sheet, 160 × 100 mode | full screens and anything to be copied as a rectangle | this one and 7 |
| Sprite with transparency | figures in front of a background | 11 |
| Font sheet | text in your own letters | 10 |
| Picture, 640 × 200 mode | still pictures in two colours | 4 |

The same converter works from the command line, which is handy when many files have to be made again and again: `node docs/tools/vdcconv.js picture in.png out`.

Then the file has to get onto a disk or a disk image, as a `PRG` file. For VICE, `c1541 image.d64 -write out name` does that.

## Pictures that are not on the screen

A full-screen picture can be sent straight to the screen. A smaller one cannot: `RTV` copies one unbroken block, and a small picture on the screen is not an unbroken block. Each of its rows starts 80 bytes after the previous one, with other people's pixels in between.

And there is a better reason to do it differently. Here is the idea that everything in the next part rests on:

> A picture does not have to be on the screen to be in VRAM.

We have 41536 bytes of VRAM that are not displayed. We can keep pictures there, packed tightly, and bring them to the screen with `VMC` whenever and wherever we want. The slow road from the disk is travelled once, at the start of the program. After that everything happens inside the chip.

<!-- listing: 06-title.bas -->
```
10 REM GETTING PICTURES INTO VRAM: A SMALL PICTURE, PARKED OFF SCREEN
20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END
30 GOSUB 9000:P=16000
40 BF=DEC("8000"):SH=32000:REM BUFFER IN RAM, STORE IN VRAM
50 BLOAD "TITLE",B0,P(BF)
60 BANK 0:W=PEEK(BF):H=PEEK(BF+1):BANK 15
70 RTV BF+2,SH,W*H:REM THE PICTURE IS NOW IN VRAM, BUT NOT ON SCREEN
80 X=3:Y=10:GOSUB 200
90 Y=58:GOSUB 200
100 GETKEY K$
110 RST 3:PRINT CHR$(147):END
200 REM --- SHOW THE STORED PICTURE AT BYTE X, ROW Y ---
210 FOR R=0 TO H-1
220 : VMC SH+R*W,P+(Y+R)*80+X,W
230 NEXT:RETURN
9000 REM --- 160 X 100 PIXELS, 16 COLOURS. NEEDS 64 KIB VRAM ---
9010 IF RGD(255)=0 THEN PRINT "THIS PROGRAM NEEDS 64 KIB VRAM":END
9020 RST 1:RGO 28,16
9030 T=(RGD(4)+1)*8+(RGD(5) AND 31):V=RGD(7)
9040 RGW 9,1:RGW 4,T/2-1:RGW 6,100:RGW 7,V*4
9050 VMF 0,15,16000:VMF 16000,0,8000
9060 DISP 0:ATTR 16000:RGO 25,128
9070 RETURN
```

![One stored picture, shown twice](img/06-title.png)

Line 70 puts the title picture at address 32000, in the free part of VRAM. Nothing appears on the screen.

The subroutine at line 200 shows it at a chosen position, row by row. Row `R` of the stored picture starts at `SH+R*W`, since the rows lie directly one after the other. It goes to row `Y+R` of the screen, `X` bytes from the left edge. One `VMC` per row.

Lines 80 and 90 call it twice. The second copy costs no loading and no extra memory.

(The store is called `SH`, for "sheet", and not `ST`. `ST` is taken: it is BASIC's status variable. `TI` and `DS` are reserved in the same way.)

It works. It is also visibly slow: the loop needs about a second for the 32 rows, and once more it is BASIC that takes the time, not the chip. Thirty-two statements for one picture is thirty-one too many. The next chapter fixes that.

## The way back, and two relatives

`VTR` brings a piece of VRAM into RAM, where it can be saved. A screenshot function is two lines:

```
VTR P,BF,8000
BSAVE "SHOT",B0,P(BF) TO P(BF+8000)
```

(A file saved this way has no width and height bytes in front. To load it with the viewer above, save from two bytes before the data and `POKE` 80 and 100 there first.)

`SWP vaddr, address, count` exchanges a block of VRAM with a block of RAM in one go: what was in VRAM is now in RAM and the other way round. That is a way to keep a second screen in reserve and flip between the two, and on a machine with 16 KiB, where VRAM has no room to spare, it is the only way.

`VCC address, vaddr, count` is a specialist for text mode. It copies character patterns of 8 bytes each from RAM into the character set, where each character has 16 bytes reserved, and skips the unused 8 each time. With it you can load a character set made for the 40-column screen: `VCC BF,DEC("2000"),0` replaces all 256 characters of the first set.

## Packed pictures: RXV

The picture of the viewer takes 8000 bytes on the disk and in RAM, and the dungeon is mostly large areas of one colour. A simple form of compression pays off: instead of a hundred equal bytes, store "100 times this byte". This is run-length coding, and the VDC-BASIC format for it is called VRL ("VDC run length"). `RXV` unpacks such a stream straight from RAM into VRAM:

```
RXV src, dst
```

A stream is a sequence of small tokens: literal bytes, runs of one value, skips that move on in VRAM without writing (which leave a picture's transparent parts and the gaps between rows alone), and a zero byte at the end. The exact layout is in the [VRL format](reference/vrl-format.md). You do not have to write streams by hand: `tools/vrl.py` encodes a file (`python3 tools/vrl.py encode picture.raw picture.vrl`), and it can also pack a rectangle with a row length, for a tile or a part of a picture.

<!-- listing: 06-rxv.bas -->
```
10 REM GETTING PICTURES INTO VRAM: A PACKED PICTURE, UNPACKED BY THE CHIP'S HELPER
20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END
30 GOSUB 9000:P=16000
40 BF=DEC("8000"):REM A BUFFER IN CPU RAM, BANK 0
50 BLOAD "DUNGEON.VRL",B0,P(BF):REM 2414 BYTES, NOT 8000
60 T=TI:RXV BF+5,P:T=TI-T:REM THE STREAM STARTS 5 BYTES AFTER THE HEADER
70 GETKEY K$
80 RST 3:PRINT CHR$(147):PRINT "RXV:";T;"JIFFIES"
90 BLOAD "DUNGEON",B0,P(BF)
100 T=TI:RTV BF+2,P,8000:T=TI-T
110 PRINT "RTV:";T;"JIFFIES":END
9000 REM --- 160 X 100 PIXELS, 16 COLOURS. NEEDS 64 KIB VRAM ---
9010 IF RGD(255)=0 THEN PRINT "THIS PROGRAM NEEDS 64 KIB VRAM":END
9020 RST 1:RGO 28,16
9030 T=(RGD(4)+1)*8+(RGD(5) AND 31):V=RGD(7)
9040 RGW 9,1:RGW 4,T/2-1:RGW 6,100:RGW 7,V*4
9050 VMF 0,15,16000:VMF 16000,0,8000
9060 DISP 0:ATTR 16000:RGO 25,128
9070 RETURN
```

![The packed dungeon, unpacked by RXV](img/06-rxv.png)

The file `DUNGEON.VRL` is the dungeon picture of the viewer, packed: 2414 bytes instead of 8000. A VRL file starts with a header of 5 bytes (`"VR"`, a version, the unpacked length), so the stream begins at `BF+5`. That is why line 60 says `RXV BF+5,P`. `RXV` does not look at the header.

Line 60 times it, and the last lines show the result after the picture: `RXV` needs 14 jiffies for the dungeon, and the `RTV` of the plain file 25. The disk has less to read, too. What `RXV` does with a long run is the interesting part. The chip's block fill writes a run of equal bytes by itself, while the CPU already reads the next token, so a big blue wall costs next to nothing. Short runs are cheaper to write by the CPU. The limit between the two is the *threshold* T, the byte at `$1C04` (default 14). You can change it with `POKE DEC("1C04"),T`; it changes the speed and never the result.

Like `RTV`, `RXV` always reads bank 0, whatever `BANK` says. Nothing is checked: a stream without its end byte makes `RXV` read on into whatever follows. Two bytes at `$1C06` and `$1C07` tell where the stream ended, so that a second stream directly behind the first can be unpacked next. Inside a command list (chapter 12), an `RXV` is an entry of type 8.

## What we have so far

| Command | Does |
|---|---|
| `RTV address, vaddr, count` | copies from RAM (bank 0) to VRAM |
| `VTR vaddr, address, count` | copies from VRAM to RAM |
| `SWP vaddr, address, count` | exchanges VRAM and RAM |
| `VCC address, vaddr, count` | copies 8-byte character patterns into the character set |
| `RXV address, vaddr` | unpacks a run-length coded (VRL) picture from RAM into VRAM (v3) |

We can now fill VRAM with pictures. In the next part they start to move.
