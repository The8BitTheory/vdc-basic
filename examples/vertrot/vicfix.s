; vicfix: vertrot on the VIC-II as a machine code program in fixed point (no BASIC program, no ROM
; floating point). Multicolour 160 x 200, SLOW; drawing, colours and page flip as in vicasm.s.
; Geometry: fixcore.inc with VIC = 1 (y as in the original), bit for bit the model fixref.py.
; JSR $1BFE / $1BFF (RTS) mark the drawing of each frame for vicrun.py; $0FFE/$0FFF = the page drawn into.
; At the end: $D7FF = 0 (debug cartridge), a key, text screen again, time per frame (jiffy clock).
; The path is the one of the listings:  DATA "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
;
; Build: cl65 -t none -C vicfix.cfg -o vicfix.prg vicfix.s       Start: LOAD"VICFIX",8 : RUN
        .define O(name) (name - fv)     ; offset of a 32 bit number in FV (fixcore.inc)
VIC     = 1

GETIN   = $FFE4
CHROUT  = $FFD2
MARK1   = $1BFE
MARK2   = $1BFF
SHOTPG  = $0FFE
GRAPHM  = $D8
SCRA    = $8C00
SCRB    = $CC00
COLRAM  = $D800
PTR     = $FB
SRC     = $FD
C_DIR   = 2             ; bit pairs of the lines (vic.bas: DRAW sources)
C_POINT = 3
C_WALL  = 1
C_V3H   = 1
C_V3S   = 2

.segment "LOADADDR"
        .word   $1C01
.segment "CODE"
        .word   @link, 10       ; 10 SYS7181
        .byte   $9E, "7181", 0
@link:  .word   0
; $1C0D
start:  lda     #$60            ; RTS at the two markers
        sta     MARK1
        sta     MARK2
        jsr     sqinit
        lda     #0
        sta     nla
        sta     nlb
        jsr     rowinit
        jsr     vicinit
        ldx     #3              ; PX=50:PY=50:AG=0
:       lda     k50,x
        sta     px,x
        sta     py,x
        dex
        bpl     :-
        lda     #32
        sta     ki
        lda     #40
        sta     oyb
        lda     #$E0            ; the first frame goes into page B ($E000)
        sta     pg
        lda     #0
        sta     fr
        sta     nla
        sta     nlb
        lda     $A2             ; TT=TI
        sta     tt
        lda     $A1
        sta     tt+1

frame:  jsr     MARK1
        lda     #0
        sta     SHOTPG
        lda     pg
        sta     SHOTPG+1
        jsr     clearpg         ; the lines of two frames ago away, wall and player again
        lda     #1
        sta     rec
        jsr     geom
        jsr     MARK2
@w1:    lda     $D011           ; wait for raster line 251
        bmi     @w1
        lda     $D012
        cmp     #251
        bne     @w1
        lda     $DD00           ; page A ($A000) = VIC bank 2 (%01), page B ($E000) = bank 3 (%00)
        and     #$FC
        ldx     pg
        cpx     #$A0
        bne     :+
        ora     #1
:       sta     $DD00
        lda     pg              ; draw into the other page
        eor     #$40
        sta     pg
        inc     fr              ; the next step of the path
        ldx     fr
        cpx     #pathlen+1
        bcc     :+
        jmp     theend
:       lda     path-1,x
        jsr     step
        jmp     frame

; step: one step of the path (A = U D L R A E): PX/PY += CS/SN (16.16) or the angle index +-1
step:   cmp     #'L'
        bne     :+
        dec     ki
        rts
:       cmp     #'R'
        bne     :+
        inc     ki
        rts
:       ldx     #O(cs)          ; U: PX+CS, PY+SN   D: PX-CS, PY-SN   A: PX+SN, PY-CS   E: PX-SN, PY+CS
        ldy     #O(sn)
        cmp     #'A'
        beq     @sw
        cmp     #'E'
        bne     @go
@sw:    ldx     #O(sn)
        ldy     #O(cs)
@go:    sta     stp
        stx     sx
        sty     sy
        lda     stp             ; sign of the x part: + for U and A
        cmp     #'U'
        beq     @xp
        cmp     #'A'
        beq     @xp
        ldx     #O(px)
        ldy     sx
        jsr     sub32
        jmp     @y
@xp:    ldx     #O(px)
        ldy     sx
        jsr     add32
@y:     lda     stp             ; sign of the y part: + for U and E
        cmp     #'U'
        beq     @yp
        cmp     #'E'
        beq     @yp
        ldx     #O(py)
        ldy     sy
        jmp     sub32
@yp:    ldx     #O(py)
        ldy     sy
        jmp     add32
add32:  clc                     ; v(X) += v(Y)
        lda     fv,x
        adc     fv,y
        sta     fv,x
        lda     fv+1,x
        adc     fv+1,y
        sta     fv+1,x
        lda     fv+2,x
        adc     fv+2,y
        sta     fv+2,x
        lda     fv+3,x
        adc     fv+3,y
        sta     fv+3,x
        rts
sub32:  sec                     ; v(X) -= v(Y)
        lda     fv,x
        sbc     fv,y
        sta     fv,x
        lda     fv+1,x
        sbc     fv+1,y
        sta     fv+1,x
        lda     fv+2,x
        sbc     fv+2,y
        sta     fv+2,x
        lda     fv+3,x
        sbc     fv+3,y
        sta     fv+3,x
        rts

theend: sec                     ; TT=TI-TT
        lda     $A2
        sbc     tt
        sta     tt
        lda     $A1
        sbc     tt+1
        sta     tt+1
        lda     #0
        sta     $D7FF           ; debug cartridge: end of the run
@key:   jsr     GETIN
        beq     @key
        lda     $DD00           ; VIC bank 0, the Kernal takes the VIC back (text screen)
        ora     #3
        sta     $DD00
        lda     #0
        sta     GRAPHM
        lda     port0
        sta     $01
        lda     bg0
        sta     $D021
        jmp     report

; --- VIC routines as in vicasm.s
; clearpg: the recorded lines of page pg again with bit pair 00, then the wall and the player
clearpg:
        lda     #0
        sta     rec
        sta     lcol
        jsr     listp           ; SRC = list, X = number
        stx     ln
        lda     #0
        sta     pli
@e:     lda     pli
        cmp     ln
        beq     @st
        asl
        asl
        tay
        lda     (SRC),y
        sta     lx1
        iny
        lda     (SRC),y
        sta     ly1
        iny
        lda     (SRC),y
        sta     lx2
        iny
        lda     (SRC),y
        sta     ly2
        lda     SRC             ; dline uses SRC
        pha
        lda     SRC+1
        pha
        jsr     dline
        pla
        sta     SRC+1
        pla
        sta     SRC
        inc     pli
        jmp     @e
@st:    lda     pg              ; list empty
        cmp     #$A0
        bne     :+
        lda     #0
        sta     nla
        beq     @wall
:       lda     #0
        sta     nlb
@wall:  lda     #37             ; the wall: GOSUB 3000 with (35,20)-(35,70) in view 1
        sta     lx1
        sta     lx2
        lda     #60
        sta     ly1
        lda     #110
        sta     ly2
        lda     #1
        sta     lcol
        jsr     dline
player: lda     #0              ; DRAW 2,79,90 TO 79,85:DRAW 3,79,90
        sta     rec
        lda     #79
        sta     lx1
        sta     lx2
        lda     #90
        sta     ly1
        lda     #85
        sta     ly2
        lda     #2
        sta     lcol
        jsr     dline
        lda     #90             ; (dline may have swapped the end points)
        sta     ly1
        sta     ly2
        lda     #3
        sta     lcol
        jsr     dline
        lda     #1              ; the lines of the frame are recorded again
        sta     rec
        rts

; listp: SRC = the line list of page pg, X = its number of lines
listp:  lda     pg
        cmp     #$A0
        bne     :+
        lda     #<lista
        sta     SRC
        lda     #>lista
        sta     SRC+1
        ldx     nla
        rts
:       lda     #<listb
        sta     SRC
        lda     #>listb
        sta     SRC+1
        ldx     nlb
        rts

;--------------------------------------------------------------------------------
; dline: line (lx1,ly1)-(lx2,ly2) with bit pair lcol into page pg, the Bresenham of VDC-BASIC's VDL;
; with rec = 1 the end points go into the list of the page first (at most 16 lines per page).
dline:  lda     rec
        beq     @go
        jsr     listp
        cpx     #16
        bcs     @go
        txa
        asl
        asl
        tay
        lda     lx1
        sta     (SRC),y
        iny
        lda     ly1
        sta     (SRC),y
        iny
        lda     lx2
        sta     (SRC),y
        iny
        lda     ly2
        sta     (SRC),y
        lda     pg
        cmp     #$A0
        bne     :+
        inc     nla
        bne     @go
:       inc     nlb
@go:    lda     $FF00           ; RAM 0 everywhere (the pages lie under the ROMs)
        sta     mmu
        lda     #$3E
        sta     $FF00
        jsr     line
        lda     mmu
        sta     $FF00
        rts

line:   lda     lx2
        sec
        sbc     lx1
        bcs     :+
        eor     #$FF
        adc     #1
:       sta     ddx
        lda     ly2
        sec
        sbc     ly1
        bcs     :+
        eor     #$FF
        adc     #1
:       sta     ddy
        lda     ddx             ; dx >= dy: x-major
        cmp     ddy
        bcs     @xmaj
        jmp     @ymaj
@xmaj:  lda     lx2             ; x1 <= x2
        cmp     lx1
        bcs     :+
        jsr     swap
:       lda     #1              ; sy = 1 if y2 > y1 else -1
        ldx     ly1
        cpx     ly2
        bcc     :+
        lda     #$FF
:       sta     dsy
        lda     ddy             ; e = 2*dy - dx
        ldx     ddx
        jsr     einit
@xl:    jsr     plot
        lda     cx
        cmp     lx2
        beq     @rts
        jsr     epos
        bcc     @xn
        lda     cy
        clc
        adc     dsy
        sta     cy
        lda     ddx
        jsr     esub2
@xn:    lda     ddy
        jsr     eadd2
        inc     cx
        jmp     @xl
@rts:   rts
@ymaj:  lda     ly2             ; y1 <= y2
        cmp     ly1
        bcs     :+
        jsr     swap
:       lda     #1              ; sx = 1 if x2 > x1 else -1
        ldx     lx1
        cpx     lx2
        bcc     :+
        lda     #$FF
:       sta     dsy
        lda     ddx             ; e = 2*dx - dy
        ldx     ddy
        jsr     einit
@yl:    jsr     plot
        lda     cy
        cmp     ly2
        beq     @rts
        jsr     epos
        bcc     @yn
        lda     cx
        clc
        adc     dsy
        sta     cx
        lda     ddy
        jsr     esub2
@yn:    lda     ddx
        jsr     eadd2
        inc     cy
        jmp     @yl

einit:  stx     dt2             ; e = 2*A - X, start at (lx1,ly1)
        asl
        sta     de
        lda     #0
        rol
        sta     de+1
        lda     de
        sec
        sbc     dt2
        sta     de
        lda     de+1
        sbc     #0
        sta     de+1
        lda     lx1
        sta     cx
        lda     ly1
        sta     cy
        rts
epos:   lda     de+1            ; carry set if e > 0
        bmi     @no
        ora     de
        beq     @no
        sec
        rts
@no:    clc
        rts
swap:   ldx     lx1
        lda     lx2
        sta     lx1
        stx     lx2
        ldx     ly1
        lda     ly2
        sta     ly1
        stx     ly2
        rts
esub2:  sta     dt2             ; e -= 2*A
        asl     dt2
        lda     #0
        rol
        sta     dt2+1
        lda     de
        sec
        sbc     dt2
        sta     de
        lda     de+1
        sbc     dt2+1
        sta     de+1
        rts
eadd2:  sta     dt2             ; e += 2*A
        asl     dt2
        lda     #0
        rol
        sta     dt2+1
        lda     de
        clc
        adc     dt2
        sta     de
        lda     de+1
        adc     dt2+1
        sta     de+1
        rts

; plot: pixel (cx,cy) with bit pair lcol: byte pg*256 + (cy/8)*320 + (cy AND 7) + (cx AND $FC)*2
plot:   ldy     cy
        lda     cx
        and     #$FC
        asl
        sta     SRC
        lda     #0
        rol
        sta     SRC+1
        lda     SRC
        clc
        adc     rowlo,y
        sta     SRC
        lda     SRC+1
        adc     rowhi,y
        adc     pg
        sta     SRC+1
        lda     cx
        and     #3
        tax
        lda     lcol
        asl
        asl
        ora     pair,x          ; index lcol*4 + (cx AND 3)
        tay
        lda     pval,y
        sta     pb
        ldy     #0
        lda     (SRC),y
        and     pmask,x
        ora     pb
        sta     (SRC),y
        rts
pair:   .byte   0, 1, 2, 3
pmask:  .byte   $3F, $CF, $F3, $FC
pval:   .byte   $00, $00, $00, $00
        .byte   $40, $10, $04, $01
        .byte   $80, $20, $08, $02
        .byte   $C0, $30, $0C, $03

rowinit:                        ; rowlo/rowhi = (y/8)*320 + (y AND 7), y = 0..199
        ldy     #0
@r:     tya
        lsr
        lsr
        lsr
        tax                     ; y/8
        lda     #0
        sta     pa
        sta     pa+1
:       cpx     #0
        beq     :+
        lda     pa
        clc
        adc     #<320
        sta     pa
        lda     pa+1
        adc     #>320
        sta     pa+1
        dex
        jmp     :-
:       tya
        and     #7
        clc
        adc     pa
        sta     rowlo,y
        lda     pa+1
        adc     #0
        sta     rowhi,y
        iny
        cpy     #200
        bne     @r
        rts

; vicinit: multicolour bitmap, colours of the cells, both pages cleared with borders, page A shown
vicinit:
        lda     #$FF            ; the Kernal leaves the VIC alone
        sta     GRAPHM
        lda     $01             ; colour memory: the VIC and the CPU use the bank at +$400
        sta     port0
        and     #$FC
        sta     $01
        lda     #<SCRA          ; screen memory of both pages and colour memory, row by row
        sta     PTR
        lda     #>SCRA
        sta     PTR+1
        lda     #<COLRAM
        sta     SRC
        lda     #>COLRAM
        sta     SRC+1
        lda     #0
        sta     pli
@row:   ldy     #39
@col:   lda     pli
        cmp     #4
        bcc     @blk
        cmp     #19
        bcs     @blk
        lda     scr,y
        sta     (PTR),y
        lda     PTR+1           ; the same at $CC00
        pha
        clc
        adc     #>(SCRB-SCRA)
        sta     PTR+1
        lda     scr,y
        sta     (PTR),y
        pla
        sta     PTR+1
        lda     pli
        cmp     #4
        beq     @bd
        cmp     #18
        beq     @bd
        lda     col,y
        jmp     @cw
@bd:    lda     colb,y
@cw:    sta     (SRC),y
        jmp     @nx
@blk:   lda     #0              ; outside the views: black
        sta     (PTR),y
        sta     (SRC),y
        lda     PTR+1
        pha
        clc
        adc     #>(SCRB-SCRA)
        sta     PTR+1
        lda     #0
        sta     (PTR),y
        pla
        sta     PTR+1
@nx:    dey
        bpl     @col
        lda     PTR
        clc
        adc     #40
        sta     PTR
        bcc     :+
        inc     PTR+1
:       lda     SRC
        clc
        adc     #40
        sta     SRC
        bcc     :+
        inc     SRC+1
:       inc     pli
        lda     pli
        cmp     #25
        bne     @row
        lda     #$A0            ; clear $A000-$BF3F and $E000-$FEFF (not the MMU at $FF00)
        ldx     #$20
        jsr     clrpages
        lda     #$E0
        ldx     #$1F
        jsr     clrpages
        lda     #$A0            ; borders on both pages
        sta     pg
        jsr     borders
        lda     #$E0
        sta     pg
        jsr     borders
        lda     #$A0            ; wall and player on page A, shown first
        sta     pg
        jsr     clearpg
        lda     #$3B            ; bitmap mode, multicolour, screen $x C00 / bitmap $x 000+$2000
        sta     $D011
        lda     #$D8
        sta     $D016
        lda     #$38
        sta     $D018
        lda     $D021           ; black background (the old one back at the end)
        sta     bg0
        lda     #0
        sta     $D021
        lda     $DD00           ; VIC bank 2: page A
        and     #$FC
        ora     #1
        sta     $DD00
        rts

clrpages:                       ; X pages of zeros from A*256 (writes go to RAM under the ROMs)
        sta     PTR+1
        lda     #0
        sta     PTR
        tay
:       sta     (PTR),y
        iny
        bne     :-
        inc     PTR+1
        dex
        bne     :-
        rts

borders:                        ; the borders of the three views, bit pairs as in vic.bas 8010-8050
        lda     #0
        sta     rec
        ldx     #0
@b:     lda     btab,x
        sta     lx1
        lda     btab+1,x
        sta     ly1
        lda     btab+2,x
        sta     lx2
        lda     btab+3,x
        sta     ly2
        lda     btab+4,x
        sta     lcol
        stx     ri
        jsr     dline
        lda     ri
        clc
        adc     #5
        tax
        cpx     #btabend-btab
        bne     @b
        rts
btab:   .byte   1, 39, 51, 39, 3,   52, 39, 52, 39, 2,   1, 150, 51, 150, 3,   52, 150, 52, 150, 2
        .byte   1, 39, 1, 150, 3,   52, 39, 52, 150, 2
        .byte   53, 39, 103, 39, 3, 104, 39, 104, 39, 2, 53, 150, 103, 150, 3, 104, 150, 104, 150, 2
        .byte   53, 39, 53, 150, 3, 104, 39, 104, 150, 2
        .byte   106, 39, 158, 39, 3, 106, 150, 158, 150, 3, 106, 39, 106, 150, 3, 158, 39, 158, 150, 3
btabend:

; per column: screen memory (01 high, 10 low), colour memory (11) inside and in the border rows 4/18
scr:    .byte   $7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B         ; 0-12
        .byte   $76                                                         ; 13
        .byte   $7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B             ; 14-25
        .byte   $75                                                         ; 26
        .byte   $79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79         ; 27-39
col:    .byte   6,1,1,1,1,1,1,1,1,1,1,1,1
        .byte   5
        .byte   1,1,1,1,1,1,1,1,1,1,1,1
        .byte   3
        .byte   3,3,3,3,3,3,3,3,3,3,3,3,3
colb:   .byte   6,6,6,6,6,6,6,6,6,6,6,6,6
        .byte   5
        .byte   5,5,5,5,5,5,5,5,5,5,5,5
        .byte   3
        .byte   3,3,3,3,3,3,3,3,3,3,3,3,3

path:   .byte   "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
pathlen = * - path


.include "fixcore.inc"
.include "fixend.inc"

.segment "BSS"
fr:     .res    1
tt:     .res    2
oyb:    .res    1
stp:    .res    1
sx:     .res    1
sy:     .res    1
dnum:   .res    2
dlead:  .res    1
lx1:    .res    1               ; lx1 ly1 lx2 ly2 follow each other
ly1:    .res    1
lx2:    .res    1
ly2:    .res    1
lcol:   .res    1
ddx:    .res    1
ddy:    .res    1
dsy:    .res    1
cx:     .res    1
cy:     .res    1
de:     .res    2
dt2:    .res    2
pa:     .res    2
pb:     .res    1
pli:    .res    1
ln:     .res    1
ri:     .res    1
mmu:    .res    1
rec:    .res    1
pg:     .res    1
nla:    .res    1
nlb:    .res    1
port0:  .res    1
bg0:    .res    1
lista:  .res    64
listb:  .res    64
rowlo:  .res    200
rowhi:  .res    200
