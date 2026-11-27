; vtfix: vertrot as a machine code program in fixed point (no BASIC program, no VDC-BASIC, no ROM
; floating point). VDC 160 x 100, 16 colours, FAST; drawing as in vtasm.s (own VDC routines, the
; Bresenham of VDL). Geometry: fixcore.inc, bit for bit the model fixref.py (16.16 / 24.8, muldiv), which
; gives the same picture as the floating point versions (run.py compares with ref.py).
; JSR $1BFE / $1BFF (RTS) mark the drawing of each frame for run.py.
; At the end: $D7FF = 0 (debug cartridge), a key, text screen again, time per frame (jiffy clock).
; The path is the one of the listings:  DATA "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
;
; Build: cl65 -t none -C vtfix.cfg -o vtfix.prg vtfix.s       Start: LOAD"VTFIX",8 : RUN

        .define O(name) (name - fv)     ; offset of a 32 bit number in FV (fixcore.inc)

VDCREG  = $D600
VDCDAT  = $D601
GETIN   = $FFE4
CHROUT  = $FFD2
SETVDC  = $E1DC         ; Kernal: VDC registers from its table, X = offset
FONT    = $C027         ; editor: ROM character set into VRAM
MARK1   = $1BFE
MARK2   = $1BFF
C_DIR   = 1             ; colours of the lines
C_POINT = 15
C_WALL  = 13
C_V3H   = 13
C_V3S   = 12

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
        lda     $D011           ; FAST, as BASIC does it (VIC off)
        and     #$6F
        sta     $D011
        lda     $D030
        ora     #1
        sta     $D030
        jsr     sqinit
        jsr     rowinit
        jsr     vdcinit
        jsr     tmpl
        ldx     #3              ; PX=50:PY=50:AG=0
:       lda     k50,x
        sta     px,x
        sta     py,x
        dex
        bpl     :-
        lda     #32
        sta     ki
        lda     #100
        sta     yo
        lda     #93
        sta     ph
        lda     #192
        sta     pl
        lda     #0
        sta     fr
        lda     $A2             ; TT=TI
        sta     tt
        lda     $A1
        sta     tt+1

frame:  jsr     MARK1
        lda     yo              ; VMC 32000,17600+YO*80,4400
        beq     :+
        lda     #<(17600+8000)
        ldx     #>(17600+8000)
        bne     :++
:       lda     #<17600
        ldx     #>17600
:       sta     cdst
        stx     cdst+1
        lda     #<32000
        sta     csrc
        lda     #>32000
        sta     csrc+1
        lda     #<4400
        sta     ccnt
        lda     #>4400
        sta     ccnt+1
        jsr     vmc
        lda     yo              ; OY = 20+YO
        clc
        adc     #20
        sta     oyb
        jsr     geom
        jsr     MARK2
@w1:    lda     VDCREG          ; SYN 20,PH,21,PL: wait for the start of the vertical blank
        and     #$20
        bne     @w1
@w2:    lda     VDCREG
        and     #$20
        beq     @w2
        sei
        lda     ph
        ldx     #20
        jsr     wreg
        lda     pl
        ldx     #21
        jsr     wreg
        cli
        lda     #100            ; YO=100-YO:PH=155-PH:PL=320-PL
        sec
        sbc     yo
        sta     yo
        lda     #155
        sec
        sbc     ph
        sta     ph
        lda     pl
        eor     #$40
        sta     pl
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

; player: the player of view 2 again (79,43+YO)-(79,45+YO) colour 1, (79,45+YO) colour 15
player: lda     #79
        sta     lx1
        sta     lx2
        lda     yo
        clc
        adc     #43
        sta     ly1
        adc     #2
        sta     ly2
        lda     #1
        sta     lcol
        jsr     dline
        lda     ly2             ; (dline may have swapped the end points: both 45+YO)
        cmp     ly1
        bcs     :+
        lda     ly1
:       sta     ly1
        sta     ly2
        lda     #15
        sta     lcol
        jmp     dline

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
        lda     #37             ; VDC registers as the Kernal sets them, character set
        sta     VDCREG
        lda     #$FF
        sta     VDCDAT
        ldx     #0
        jsr     SETVDC
        lda     VDCREG
        and     #7
        beq     :+
        ldx     #$3B
        jsr     SETVDC
:       bit     $0A03
        bpl     :+
        ldx     #$3E
        jsr     @pal
:       jsr     FONT
        jmp     report
@pal:   jmp     ($E192)

; --- VDC routines as in vtasm.s
;--------------------------------------------------------------------------------
; dline: line (lx1,ly1)-(lx2,ly2) in colour lcol, Bresenham as VDC-BASIC's VDL (vdcdraw.line_pixels):
; dx >= dy: from the left end, x by x; otherwise from the top end, y by y; the error term starts at
; 2*minor-major, one step in the minor direction whenever it is > 0.
dline:  lda     lx2
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

; plot: pixel (cx,cy) in colour lcol: byte 16000 + cy*80 + cx/2, upper nibble for even cx
plot:   ldy     cy
        lda     cx
        lsr
        clc
        adc     rowlo,y
        sta     pa
        lda     rowhi,y
        adc     #0
        sta     pa+1
        jsr     seta
        ldx     #31
        jsr     rreg
        sta     pb
        lda     cx
        lsr
        lda     lcol
        bcs     @odd
        asl
        asl
        asl
        asl
        sta     pc
        lda     pb
        and     #$0F
        ora     pc
        jmp     @w
@odd:   sta     pc
        lda     pb
        and     #$F0
        ora     pc
@w:     pha
        jsr     seta
        pla
        ldx     #31
        jmp     wreg
seta:   lda     pa+1
        ldx     #18
        jsr     wreg
        lda     pa
        ldx     #19
        jmp     wreg

; VDC registers
wreg:   stx     VDCREG
:       bit     VDCREG
        bpl     :-
        sta     VDCDAT
        rts
rreg:   stx     VDCREG
:       bit     VDCREG
        bpl     :-
        lda     VDCDAT
        rts

rowinit:                        ; rowlo/rowhi = 16000 + y*80, y = 0..199
        lda     #<16000
        sta     pa
        lda     #>16000
        sta     pa+1
        ldy     #0
@r:     lda     pa
        sta     rowlo,y
        lda     pa+1
        sta     rowhi,y
        lda     pa
        clc
        adc     #80
        sta     pa
        bcc     :+
        inc     pa+1
:       iny
        cpy     #200
        bne     @r
        rts

; vmc: copy ccnt bytes from csrc to cdst in VRAM (block copy, 255 bytes per R30 write)
vmc:    ldx     #24
        jsr     rreg
        ora     #$80
        jsr     wreg
        lda     cdst+1
        ldx     #18
        jsr     wreg
        lda     cdst
        ldx     #19
        jsr     wreg
        lda     csrc+1
        ldx     #32
        jsr     wreg
        lda     csrc
        ldx     #33
        jsr     wreg
blocks: lda     ccnt+1          ; ccnt bytes with R30 writes of up to 255
        bne     @255
        lda     ccnt
        beq     @rts
        cmp     #255
        bcc     :+
@255:   lda     #255
:       sta     cn
        ldx     #30
        jsr     wreg
        lda     ccnt
        sec
        sbc     cn
        sta     ccnt
        bcs     blocks
        dec     ccnt+1
        jmp     blocks
@rts:   rts

; vmf: fill ccnt bytes at cdst with the byte A (block fill)
vmf:    sta     cn
        ldx     #24
        jsr     rreg
        and     #$7F
        jsr     wreg
        lda     cdst+1
        ldx     #18
        jsr     wreg
        lda     cdst
        ldx     #19
        jsr     wreg
        lda     cn
        ldx     #31
        jsr     wreg            ; the first byte, the rest by block fill
        lda     ccnt
        bne     :+
        dec     ccnt+1
:       dec     ccnt
        jmp     blocks

; vdcinit: 9000-9060 (64 KiB, 160 x 100, bitmap of $0F bytes at 0, pages cleared, page A shown)
vdcinit:
        ldx     #28             ; RGO 28,16
        jsr     rreg
        ora     #16
        jsr     wreg
        ldx     #4              ; T=(R4+1)*8+(R5 AND 31), in scan lines (a word)
        jsr     rreg
        clc
        adc     #1
        sta     pa
        lda     #0
        sta     pa+1
        asl     pa
        rol     pa+1
        asl     pa
        rol     pa+1
        asl     pa
        rol     pa+1
        ldx     #5
        jsr     rreg
        and     #31
        clc
        adc     pa
        sta     pa
        bcc     :+
        inc     pa+1
:       lsr     pa+1            ; R4 = T/2-1
        ror     pa
        lda     pa
        sec
        sbc     #1
        ldx     #4
        jsr     wreg
        ldx     #7              ; R7 = R7*4
        jsr     rreg
        asl
        asl
        sta     pb
        lda     #1              ; R9 = 1
        ldx     #9
        jsr     wreg
        lda     #100            ; R6 = 100
        ldx     #6
        jsr     wreg
        lda     pb
        ldx     #7
        jsr     wreg
        lda     #0              ; VMF 0,15,16000
        sta     cdst
        sta     cdst+1
        lda     #<16000
        sta     ccnt
        lda     #>16000
        sta     ccnt+1
        lda     #15
        jsr     vmf
        lda     #<16000         ; VMF 16000,0,16000 (both pages)
        sta     cdst
        lda     #>16000
        sta     cdst+1
        lda     #<16000
        sta     ccnt
        lda     #>16000
        sta     ccnt+1
        lda     #0
        jsr     vmf
        lda     #0              ; DISP 0
        ldx     #12
        jsr     wreg
        lda     #0
        ldx     #13
        jsr     wreg
        lda     #>16000         ; ATTR 16000
        ldx     #20
        jsr     wreg
        lda     #<16000
        ldx     #21
        jsr     wreg
        ldx     #25             ; RGO 25,128
        jsr     rreg
        ora     #128
        jmp     wreg

; tmpl: 8000-8060: borders, wall, player on page A; template = rows 20-74; page B = page A
tmpl:   ldx     #0
@b:     lda     rects,x         ; VDR x,y,w,h,c as four lines (same pixels, one colour)
        sta     lx1
        sta     lx2
        lda     rects+1,x
        sta     ly1
        lda     rects+2,x       ; x2 = x+w-1
        clc
        adc     lx1
        sec
        sbc     #1
        sta     rx2
        lda     rects+3,x       ; y2 = y+h-1
        clc
        adc     ly1
        sec
        sbc     #1
        sta     ry2
        lda     rects+4,x
        sta     lcol
        stx     ri
        lda     ly1             ; top
        sta     ly2
        lda     rx2
        sta     lx2
        jsr     dline
        lda     ry2             ; bottom
        sta     ly1
        sta     ly2
        jsr     dline
        ldx     ri              ; left
        lda     rects,x
        sta     lx1
        sta     lx2
        lda     rects+1,x
        sta     ly1
        jsr     dline
        lda     rx2             ; right
        sta     lx1
        sta     lx2
        jsr     dline
        lda     ri
        clc
        adc     #5
        tax
        cpx     #15
        bne     @b
        ldx     #0              ; wall (37,30)-(37,55) colour 13, player (79,43)-(79,45) 1, (79,45) 15
@l:     lda     tlines,x
        sta     lx1
        lda     tlines+1,x
        sta     ly1
        lda     tlines+2,x
        sta     lx2
        lda     tlines+3,x
        sta     ly2
        lda     tlines+4,x
        sta     lcol
        stx     ri
        jsr     dline
        lda     ri
        clc
        adc     #5
        tax
        cpx     #15
        bne     @l
        lda     #<17600         ; VMC 17600,32000,4400
        sta     csrc
        lda     #>17600
        sta     csrc+1
        lda     #<32000
        sta     cdst
        lda     #>32000
        sta     cdst+1
        lda     #<4400
        sta     ccnt
        lda     #>4400
        sta     ccnt+1
        jsr     vmc
        lda     #<16000         ; VMC 16000,24000,8000
        sta     csrc
        lda     #>16000
        sta     csrc+1
        lda     #<24000
        sta     cdst
        lda     #>24000
        sta     cdst+1
        lda     #<8000
        sta     ccnt
        lda     #>8000
        sta     ccnt+1
        jmp     vmc
rects:  .byte   1, 19, 52, 57, 2        ; VDR 1,19,52,57,2
        .byte   53, 19, 52, 57, 4
        .byte   106, 19, 53, 57, 6
tlines: .byte   37, 30, 37, 55, 13      ; the wall: GOSUB 3000 with (35,10)-(35,35) in view 1
        .byte   79, 43, 79, 45, 1
        .byte   79, 45, 79, 45, 15

path:   .byte   "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
pathlen = * - path


.include "fixcore.inc"
.include "fixend.inc"

.segment "BSS"
yo:     .res    1
ph:     .res    1
pl:     .res    1
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
pc:     .res    1
cn:     .res    1
csrc:   .res    2
cdst:   .res    2
ccnt:   .res    2
ri:     .res    1
rx2:    .res    1
ry2:    .res    1
rowlo:  .res    200
rowhi:  .res    200
