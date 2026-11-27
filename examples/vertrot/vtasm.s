; vtasm: vertrot as a machine code program only (no BASIC program, no VDC-BASIC).
; The same picture as vertrot.bas .. vertrot5.bas, pixel for pixel: the geometry is computed with the
; floating point routines of the BASIC ROM in the order in which BASIC evaluates the statements of
; vertrot2.bas (a left operand that waits for its right one is pushed, i.e. rounded; the right one stays
; in FAC); lines are drawn with the Bresenham of VDC-BASIC's VDL (test/ref/vdcdraw.py), pixel by pixel
; (read, modify, write the VRAM byte); VDC set up, filled, copied and flipped directly.
; 160 x 100, 16 colours (chapter 5), pages at VRAM 16000 and 24000, template at 32000.
; JSR $1BFE / $1BFF (RTS) mark the drawing of each frame for run.py, as SYS 7166/7167 in the listings.
; At the end: $D7FF = 0 (debug cartridge), a key, text screen again, time per frame (jiffy clock).
; The path is the one of the listings:  DATA "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
;
; Build: cl65 -t none -C vtasm.cfg -o vtasm.prg vtasm.s       Start: LOAD"VTASM",8 : RUN

VDCREG  = $D600
VDCDAT  = $D601
GETIN   = $FFE4
CHROUT  = $FFD2
SETVDC  = $E1DC         ; Kernal: VDC registers from its table, X = offset
FONT    = $C027         ; editor: ROM character set into VRAM
FOUT    = $AF06         ; FAC -> string at (A/Y)
GIVAYF  = $AF03         ; FAC = signed integer A (high), Y (low)
SIN     = $AF42
COS     = $AF3F
MARK1   = $1BFE
MARK2   = $1BFF

; --- shared with vtview.s
MOVFM   = $AF63         ; FAC = (A/Y), current configuration (bank 0 here)
MOVMF   = $AF66         ; (X/Y) = FAC, rounded first
FSUBT   = $AF15         ; FAC = ARG - FAC
FADDT   = $AF1B         ; FAC = ARG + FAC   (A, Z = exponent of FAC)
FMULTT  = $AF21         ; FAC = ARG * FAC   (A, Z = exponent of FAC)
FDIVT   = $AF27         ; FAC = ARG / FAC   (A, Z = exponent of FAC)
FINT    = $AF2D         ; FAC = INT(FAC)
FCOMP   = $AF54         ; FAC against (A/Y): A = 0 equal, 1 FAC greater, $FF FAC smaller
INDFET  = $FF74         ; A = byte at ((A)),Y in bank X
INDSTA  = $FF77         ; A to ((STAVEC)),Y in bank X
STAVEC  = $02B9

FACEXP  = $63
FACSGN  = $68
ARGEXP  = $6A           ; $6B-$6E mantissa
ARGSGN  = $6F
ARISGN  = $70
VARTAB  = $2F
ARYTAB  = $31
PTR     = $FB           ; pointer into RAM 1 (INDFET/INDSTA) or target of copy5
SRC     = $FD           ; pointer into bank 0

.macro FLD p            ; FAC = p
        lda     #<p
        ldy     #>p
        jsr     MOVFM
.endmacro
.macro FARG p           ; ARG = p (A, Z = exponent of FAC afterwards)
        lda     #<p
        ldy     #>p
        jsr     ldarg
.endmacro
.macro FST p            ; p = FAC, rounded
        ldx     #<p
        ldy     #>p
        jsr     MOVMF
.endmacro
.macro COPY s, d        ; 5 bytes from s to d
        lda     #<d
        sta     PTR
        lda     #>d
        sta     PTR+1
        lda     #<s
        ldy     #>s
        jsr     copy5
.endmacro

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
.ifndef SLOW                    ; (run.py --slow: cl65 --asm-define SLOW=1, 1 MHz)
        lda     $D011           ; FAST, as BASIC does it (VIC off)
        and     #$6F
        sta     $D011
        lda     $D030
        ora     #1
        sta     $D030
.endif
        jsr     rowinit
        jsr     vdcinit
        jsr     tmpl
        ; 100-120: A1 B1 A2 B2 are constants (k70, k20, k70, k70); PX=50:PY=50:AG=0:SN=SIN(AG):CS=COS(AG)
        COPY    k50, px
        COPY    k50, py
        COPY    zero, ag
        jsr     sincos
        lda     #100
        sta     yo
        lda     #93
        sta     ph
        lda     #192
        sta     pl
        lda     #0
        sta     fr
        lda     $A2             ; 130 TT=TI
        sta     tt
        lda     $A1
        sta     tt+1

frame:  jsr     MARK1           ; 210
        ; 220 VMC 32000,17600+YO*80,4400
        lda     yo
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
        ; 310 OX=2:OY=20+YO:VW=49:VH=54
        COPY    k2, ox
        lda     yo
        beq     :+
        COPY    k120, oy
        jmp     :++
:       COPY    k20, oy
:       COPY    k49, vw
        COPY    k54, vh
        ; 340 LA=PX/2:LB=PY/2:LC=(CS*5+PX)/2:LD=(SN*5+PY)/2:LK=1
        jsr     fscript
        .byte   1
        .word   k2
        .byte   2
        .word   px
        .byte   7
        .byte   3
        .word   la
        .byte   1
        .word   k2
        .byte   2
        .word   py
        .byte   7
        .byte   3
        .word   lb
        .byte   1
        .word   k5
        .byte   2
        .word   cs
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   px
        .byte   2
        .word   ta
        .byte   5
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   lc
        .byte   1
        .word   k5
        .byte   2
        .word   sn
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   py
        .byte   2
        .word   ta
        .byte   5
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   ld
        .byte   0
        COPY    k1, lkv
        jsr     linecore
        ; 350 LA=PX/2:LB=PY/2:LC=LA:LD=LB:LK=15
        jsr     fscript
        .byte   1
        .word   k2
        .byte   2
        .word   px
        .byte   7
        .byte   3
        .word   la
        .byte   3
        .word   lc
        .byte   1
        .word   k2
        .byte   2
        .word   py
        .byte   7
        .byte   3
        .word   lb
        .byte   3
        .word   ld
        .byte   0
        COPY    k15, lkv
        jsr     linecore
        ; 410 OX=54
        COPY    k54, ox
        ; 430-480 rotation
        jsr     fscript
        .byte   1               ; T1=A1-PX
        .word   px
        .byte   2
        .word   k70
        .byte   4
        .byte   3
        .word   t1v
        .byte   1               ; U1=B1-PY
        .word   py
        .byte   2
        .word   k20
        .byte   4
        .byte   3
        .word   u1v
        .byte   1               ; T2=A2-PX
        .word   px
        .byte   2
        .word   k70
        .byte   4
        .byte   3
        .word   t2v
        .byte   1               ; U2=B2-PY
        .word   py
        .byte   2
        .word   k70
        .byte   4
        .byte   3
        .word   u2v
        .byte   1               ; Z1=T1*CS+U1*SN
        .word   cs
        .byte   2
        .word   t1v
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   sn
        .byte   2
        .word   u1v
        .byte   6
        .byte   2
        .word   ta
        .byte   5
        .byte   3
        .word   z1v
        .byte   1               ; Z2=T2*CS+U2*SN
        .word   cs
        .byte   2
        .word   t2v
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   sn
        .byte   2
        .word   u2v
        .byte   6
        .byte   2
        .word   ta
        .byte   5
        .byte   3
        .word   z2v
        .byte   1               ; T1=T1*SN-U1*CS
        .word   sn
        .byte   2
        .word   t1v
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   cs
        .byte   2
        .word   u1v
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   t1v
        .byte   1               ; T2=T2*SN-U2*CS
        .word   sn
        .byte   2
        .word   t2v
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   cs
        .byte   2
        .word   u2v
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   t2v
        ; 490 LA=(50-T1)/2:LB=(50-Z1)/2:LC=(50-T2)/2:LD=(50-Z2)/2:LK=13
        .byte   1
        .word   t1v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   la
        .byte   1
        .word   z1v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   lb
        .byte   1
        .word   t2v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   lc
        .byte   1
        .word   z2v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   k2
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   ld
        .byte   0
        COPY    k13, lkv
        jsr     linecore
        ; 492/493 the player again if the wall may have covered it (LA..LD as linecore left them)
        jsr     covered
        bcc     :+
        lda     #79
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
        lda     ly2
        sta     ly1
        lda     #15
        sta     lcol
        jsr     dline
:       ; 510 OX=107:VW=50
        COPY    k107, ox
        COPY    k50, vw
        ; --- 530-630 as in vtview.s
        ; 530 IF Z1<=0 AND Z2<=0 THEN 700
        lda     #<z1v
        ldy     #>z1v
        jsr     isle0
        bcc     :+
        lda     #<z2v
        ldy     #>z2v
        jsr     isle0
        bcc     :+
        jmp     v3done
        ; 540 intersection with (-.0001,.0001)-(-20,5) -> I1,J1
:       COPY    km4, e3
        COPY    kp4, f3
        COPY    km20, e4
        COPY    k5, f4
        jsr     isect
        COPY    ix, i1
        COPY    iy, j1
        ; 550 intersection with (.0001,.0001)-(20,5) -> I2,J2
        COPY    kp4, e3
        COPY    k20, e4
        jsr     isect
        COPY    ix, i2
        COPY    iy, j2
        ; 560/565 IF Z1<=0: IF J1>0 THEN T1=I1:Z1=J1 ELSE T1=I2:Z1=J2
        lda     #<z1v
        ldy     #>z1v
        jsr     isle0
        bcc     @z2
        lda     #<j1
        ldy     #>j1
        jsr     isle0
        bcs     :+
        COPY    i1, t1v
        COPY    j1, z1v
        jmp     @z2
:       COPY    i2, t1v
        COPY    j2, z1v
@z2:    ; 570/575 the same for T2,Z2 (with J1, as in the original)
        lda     #<z2v
        ldy     #>z2v
        jsr     isle0
        bcc     @proj
        lda     #<j1
        ldy     #>j1
        jsr     isle0
        bcs     :+
        COPY    i1, t2v
        COPY    j1, z2v
        jmp     @proj
:       COPY    i2, t2v
        COPY    j2, z2v
@proj:
        ; 580 X1=-T1*16/Z1:YA=-50/Z1:YB=50/Z1
        COPY    t1v, ta
        jsr     negta
        jsr     fscript
        .byte   1
        .word   k16
        .byte   2
        .word   ta
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   z1v
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   x1
        .byte   1
        .word   z1v
        .byte   2
        .word   km50
        .byte   7
        .byte   3
        .word   ya
        .byte   1
        .word   z1v
        .byte   2
        .word   k50
        .byte   7
        .byte   3
        .word   yb
        ; 590 X2=-T2*16/Z2:YC=-50/Z2:YD=50/Z2
        .byte   0
        COPY    t2v, ta
        jsr     negta
        jsr     fscript
        .byte   1
        .word   k16
        .byte   2
        .word   ta
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   z2v
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   x2
        .byte   1
        .word   z2v
        .byte   2
        .word   km50
        .byte   7
        .byte   3
        .word   yc
        .byte   1
        .word   z2v
        .byte   2
        .word   k50
        .byte   7
        .byte   3
        .word   yd
        .byte   0
        ; (50+P)/2 for P = X1 X2 YA YB YC YD, each as in the statements 600-630
        lda     #0
        sta     hi
@h:     lda     hi
        jsr     mul5
        clc
        adc     #<x1
        sta     hp
        lda     #>x1
        adc     #0
        sta     hp+1
        lda     hp
        ldy     hp+1
        jsr     MOVFM           ; FAC = P
        FARG    k50
        jsr     FADDT           ; 50+P
        FST     ta
        FLD     k2
        FARG    ta
        jsr     FDIVT           ; (50+P)/2
        ldx     hp
        ldy     hp+1
        jsr     MOVMF
        inc     hi
        lda     hi
        cmp     #6
        bne     @h
        ; 600-630: four lines, end points and colour from lntab
        lda     #0
        sta     li
@ln:    lda     #0
        sta     lj
@lp:    lda     li              ; entry li * 5 + lj of lntab (words)
        jsr     mul5
        clc
        adc     lj
        asl
        tax
        lda     lj              ; target la + 5 * lj (lkv for lj = 4, it follows ld... see WORK)
        jsr     mul5
        clc
        adc     #<la
        sta     PTR
        lda     #>la
        adc     #0
        sta     PTR+1
        lda     lj
        cmp     #4
        bne     :+
        lda     #<lkv
        sta     PTR
        lda     #>lkv
        sta     PTR+1
:       lda     lntab,x
        ldy     lntab+1,x
        jsr     copy5
        inc     lj
        lda     lj
        cmp     #5
        bne     @lp
        jsr     linecore
        inc     li
        lda     li
        cmp     #4
        bne     @ln

v3done: ; 700 show the page at the next vertical blank
        jsr     MARK2           ; 710
@w1:    lda     VDCREG          ; 720 SYN 20,PH,21,PL: wait for the start of the vertical blank
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
        lda     #100            ; 730 YO=100-YO:PH=155-PH:PL=320-PL
        sec
        sbc     yo
        sta     yo
        lda     #155
        sec
        sbc     ph
        sta     ph
        lda     pl              ; 128 <-> 192
        eor     #$40
        sta     pl
        ; 810-880: the next step of the path
        inc     fr
        ldx     fr
        cpx     #pathlen+1
        bcc     :+
        jmp     theend
:       lda     path-1,x
        cmp     #'U'
        bne     :+
        jsr     fscript         ; PX=PX+CS:PY=PY+SN
        .byte   1
        .word   cs
        .byte   2
        .word   px
        .byte   5
        .byte   3
        .word   px
        .byte   1
        .word   sn
        .byte   2
        .word   py
        .byte   5
        .byte   3
        .word   py
        .byte   0
        jmp     frame
:       cmp     #'D'
        bne     :+
        jsr     fscript         ; PX=PX-CS:PY=PY-SN
        .byte   1
        .word   cs
        .byte   2
        .word   px
        .byte   4
        .byte   3
        .word   px
        .byte   1
        .word   sn
        .byte   2
        .word   py
        .byte   4
        .byte   3
        .word   py
        .byte   0
        jmp     frame
:       cmp     #'L'
        bne     :+
        jsr     fscript         ; AG=AG-.1
        .byte   1
        .word   k01
        .byte   2
        .word   ag
        .byte   4
        .byte   3
        .word   ag
        .byte   0
        jsr     sincos
        jmp     frame
:       cmp     #'R'
        bne     :+
        jsr     fscript         ; AG=AG+.1
        .byte   1
        .word   k01
        .byte   2
        .word   ag
        .byte   5
        .byte   3
        .word   ag
        .byte   0
        jsr     sincos
        jmp     frame
:       cmp     #'A'
        bne     :+
        jsr     fscript         ; PX=PX+SN:PY=PY-CS
        .byte   1
        .word   sn
        .byte   2
        .word   px
        .byte   5
        .byte   3
        .word   px
        .byte   1
        .word   cs
        .byte   2
        .word   py
        .byte   4
        .byte   3
        .word   py
        .byte   0
        jmp     frame
:       jsr     fscript         ; E: PX=PX-SN:PY=PY+CS
        .byte   1
        .word   sn
        .byte   2
        .word   px
        .byte   4
        .byte   3
        .word   px
        .byte   1
        .word   cs
        .byte   2
        .word   py
        .byte   5
        .byte   3
        .word   py
        .byte   0
        jmp     frame

theend: ; 890-893
        sec                     ; TT=TI-TT (jiffies, the low 16 bits are enough)
        lda     $A2
        sbc     tt
        sta     tt
        lda     $A1
        sbc     tt+1
        sta     tt+1
        lda     #0
        sta     $D7FF           ; debug cartridge: end of the run
@key:   jsr     GETIN           ; GETKEY
        beq     @key
        lda     #37             ; RST 1: VDC registers as the Kernal sets them (as VDC-BASIC does)
        sta     VDCREG
        lda     #$FF
        sta     VDCDAT
        ldx     #0
        jsr     SETVDC
        lda     VDCREG
        and     #7
        beq     @ntsc
        ldx     #$3B
        jsr     SETVDC
@ntsc:  bit     $0A03
        bpl     @font
        ldx     #$3E
        jsr     @pal
@font:  jsr     FONT            ; RST 2: character set
        lda     #147
        jsr     CHROUT
        lda     #0              ; PRINT FR"FRAMES,"INT(TT/FR*1000/60+.5)"MS PER FRAME"
        ldy     fr
        jsr     GIVAYF
        jsr     prfac
        ldx     #0
@t1:    lda     txt1,x
        beq     @t1e
        jsr     CHROUT
        inx
        bne     @t1
@t1e:   lda     tt+1
        ldy     tt
        jsr     GIVAYF
        FST     ta
        lda     #0
        ldy     fr
        jsr     GIVAYF
        FARG    ta
        jsr     FDIVT           ; TT/FR
        FST     ta
        FLD     k1000
        FARG    ta
        jsr     FMULTT          ; *1000
        FST     ta
        FLD     k60
        FARG    ta
        jsr     FDIVT           ; /60
        FST     ta
        FLD     half
        FARG    ta
        jsr     FADDT           ; +.5
        jsr     FINT
        jsr     prfac
        ldx     #0
@t2:    lda     txt2,x
        beq     @t2e
        jsr     CHROUT
        inx
        bne     @t2
@t2e:   rts
@pal:   jmp     ($E192)

prfac:  jsr     FOUT            ; FAC as BASIC prints it, then a blank
        sta     SRC
        sty     SRC+1
        ldy     #0
@c:     lda     (SRC),y
        beq     @e
        jsr     CHROUT
        iny
        bne     @c
@e:     lda     #' '
        jmp     CHROUT

txt1:   .byte   "FRAMES,", 0
txt2:   .byte   "MS PER FRAME", 13, 0

; sincos: SN=SIN(AG):CS=COS(AG)
sincos: FLD     ag
        jsr     SIN
        FST     sn
        FLD     ag
        jsr     COS
        FST     cs
        rts

; covered (492): carry clear if the wall line lies away from the player: LA>=26 AND LC>=26 OR
; LA<=24 AND LC<=24 OR LB>=26 AND LD>=26 OR LB<=22 AND LD<=22
covered:
        lda     #<la
        ldy     #>la
        ldx     #0
        jsr     ge26
        bcc     @c2
        lda     #<lc
        ldy     #>lc
        jsr     ge26
        bcs     @away
@c2:    lda     #<la
        ldy     #>la
        jsr     le24
        bcc     @c3
        lda     #<lc
        ldy     #>lc
        jsr     le24
        bcs     @away
@c3:    lda     #<lb
        ldy     #>lb
        jsr     ge26
        bcc     @c4
        lda     #<ld
        ldy     #>ld
        jsr     ge26
        bcs     @away
@c4:    lda     #<lb
        ldy     #>lb
        jsr     le22
        bcc     @near
        lda     #<ld
        ldy     #>ld
        jsr     le22
        bcs     @away
@near:  sec
        rts
@away:  clc
        rts
; (A/Y) >= 26, <= 24, <= 22: carry set if true
ge26:   jsr     MOVFM
        lda     #<k26
        ldy     #>k26
        jsr     FCOMP
        cmp     #$FF            ; smaller: false
        beq     cfalse
        sec
        rts
le24:   jsr     MOVFM
        lda     #<k24
        ldy     #>k24
        jmp     lek
le22:   jsr     MOVFM
        lda     #<k22
        ldy     #>k22
lek:    jsr     FCOMP
        cmp     #1              ; greater: false
        beq     cfalse
        sec
        rts
cfalse: clc
        rts

; draw (for linecore): OX/OY + INT(L+.5) as integers, then the line in colour INT(LK)
draw:   lda     #0
        sta     vi
@dr:    FLD     half            ; FAC = .5
        lda     vi              ; ARG = LA, LB, LC or LD
        jsr     mul5
        clc
        adc     #<la
        pha
        lda     #>la
        adc     #0
        tay
        pla
        jsr     ldarg
        jsr     FADDT           ; L+.5
        jsr     FINT
        lda     vi
        and     #1
        tax
        lda     oxlo,x
        ldy     oxhi,x
        jsr     ldarg
        jsr     FADDT           ; OX+INT(...)
        jsr     facint
        ldx     vi
        lda     ilo
        sta     lx1,x           ; lx1 ly1 lx2 ly2 follow each other
        inc     vi
        lda     vi
        cmp     #4
        bne     @dr
        FLD     lkv
        jsr     facint
        lda     ilo
        sta     lcol
        jsr     dline
        lda     #0
        rts
oxlo:   .byte   <ox, <oy
oxhi:   .byte   >ox, >oy

mul5:   sta     yi              ; A * 5 (A < 51)
        asl
        asl
        clc
        adc     yi
        rts

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

; packed numbers (constants)
half:   .byte   $80,0,0,0,0
zero:   .byte   0,0,0,0,0
kp4:    .byte   $73,$51,$B7,$17,$5A     ; .0001 as BASIC 7.0 stores it
km4:    .byte   $73,$D1,$B7,$17,$5A     ; -.0001
k01:    .byte   $7D,$4C,$CC,$CC,$CD     ; .1
k1:     .byte   $81,0,0,0,0
k2:     .byte   $82,0,0,0,0
k5:     .byte   $83,$20,0,0,0
k12:    .byte   $84,$40,0,0,0
k13:    .byte   $84,$50,0,0,0
k15:    .byte   $84,$70,0,0,0
k16:    .byte   $85,0,0,0,0
k20:    .byte   $85,$20,0,0,0
km20:   .byte   $85,$A0,0,0,0
k22:    .byte   $85,$30,0,0,0
k24:    .byte   $85,$40,0,0,0
k26:    .byte   $85,$50,0,0,0
k49:    .byte   $86,$44,0,0,0
k50:    .byte   $86,$48,0,0,0
km50:   .byte   $86,$C8,0,0,0
k54:    .byte   $86,$58,0,0,0
k60:    .byte   $86,$70,0,0,0
k70:    .byte   $87,$0C,0,0,0
k107:   .byte   $87,$56,0,0,0
k120:   .byte   $87,$70,0,0,0
k1000:  .byte   $8A,$7A,0,0,0
fsop:   .byte   0
fslo:   .byte   0

; --- shared with vtview.s
lntab:  .word   x1, ya, x2, yc, k13     ; 600 top
        .word   x1, yb, x2, yd, k13     ; 610 bottom
        .word   x1, ya, x1, yb, k12     ; 620 left
        .word   x2, yc, x2, yd, k12     ; 630 right

; fscript: runs the floating point script that follows the JSR: 1 lo hi FAC = (p), 2 lo hi ARG = (p),
; 3 lo hi (p) = FAC rounded, 4 ARG-FAC, 5 ARG+FAC, 6 ARG*FAC, 7 ARG/FAC, 0 end (returns behind it)
fscript:
        pla
        sta     PTR
        pla
        sta     PTR+1
@next:  jsr     @getb
        beq     @end
        cmp     #4
        bcs     @arith
        sta     fsop
        jsr     @getb
        sta     fslo
        jsr     @getb
        tay
        lda     fsop
        cmp     #2
        beq     @arg
        bcs     @st
        lda     fslo
        jsr     MOVFM
        jmp     @next
@arg:   lda     fslo
        jsr     ldarg
        jmp     @next
@st:    ldx     fslo
        jsr     MOVMF
        jmp     @next
@arith: tax
        lda     fsvlo-4,x
        sta     @call+1
        lda     fsvhi-4,x
        sta     @call+2
        lda     FACEXP
@call:  jsr     FSUBT           ; MODIFIED
        jmp     @next
@end:   inc     PTR
        bne     :+
        inc     PTR+1
:       jmp     (PTR)
@getb:  inc     PTR
        bne     :+
        inc     PTR+1
:       ldy     #0
        lda     (PTR),y
        rts
fsvlo:  .byte   <FSUBT, <FADDT, <FMULTT, <FDIVT
fsvhi:  .byte   >FSUBT, >FADDT, >FMULTT, >FDIVT

; negta: ta = -ta (unary minus: flips the sign of a number that is not zero)
negta:  lda     ta
        beq     :+
        lda     ta+1
        eor     #$80
        sta     ta+1
:       rts

; isle0: carry set if the number at (A/Y) is <= 0
isle0:  sta     SRC
        sty     SRC+1
        ldy     #0
        lda     (SRC),y
        beq     @yes
        iny
        lda     (SRC),y
        bmi     @yes
        clc
        rts
@yes:   sec
        rts

; isect: 4000-4060 with E1=T1 F1=Z1 E2=T2 F2=Z2 and e3 f3 e4 f4 -> ix, iy
isect:
  ; 4020 IX=E1*F2-F1*E2
        jsr     fscript
        .byte   1
        .word   z2v
        .byte   2
        .word   t1v
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   t2v
        .byte   2
        .word   z1v
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   ix
        ; 4030 IY=E3*F4-F3*E4
        .byte   1
        .word   f4
        .byte   2
        .word   e3
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   e4
        .byte   2
        .word   f3
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   iy
        ; 4040 DT=(E1-E2)*(F3-F4)-(F1-F2)*(E3-E4)
        .byte   1
        .word   t2v
        .byte   2
        .word   t1v
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   f4
        .byte   2
        .word   f3
        .byte   4
        .byte   2
        .word   ta
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   z2v
        .byte   2
        .word   z1v
        .byte   4
        .byte   3
        .word   tb
        .byte   1
        .word   e4
        .byte   2
        .word   e3
        .byte   4
        .byte   2
        .word   tb
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   dt
        ; 4050 IX=(IX*(E3-E4)-(E1-E2)*IY)/DT
        .byte   1
        .word   e4
        .byte   2
        .word   e3
        .byte   4
        .byte   2
        .word   ix
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   t2v
        .byte   2
        .word   t1v
        .byte   4
        .byte   3
        .word   tb
        .byte   1
        .word   iy
        .byte   2
        .word   tb
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   dt
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   ix
        ; 4060 IY=(IX*(F3-F4)-(F1-F2)*IY)/DT  (with the new IX, as in the original)
        .byte   1
        .word   f4
        .byte   2
        .word   f3
        .byte   4
        .byte   2
        .word   ix
        .byte   6
        .byte   3
        .word   ta
        .byte   1
        .word   z2v
        .byte   2
        .word   z1v
        .byte   4
        .byte   3
        .word   tb
        .byte   1
        .word   iy
        .byte   2
        .word   tb
        .byte   6
        .byte   2
        .word   ta
        .byte   4
        .byte   3
        .word   ta
        .byte   1
        .word   dt
        .byte   2
        .word   ta
        .byte   7
        .byte   3
        .word   iy
        .byte   0

        rts

;--------------------------------------------------------------------------------
; linecore: the line la,lb - lc,ld in colour lkv, view ox oy vw vh (3010-3110 of vertrot2.bas):
; clipped, rounded and drawn (A = 0), or nothing to draw (A = 0), or without the expected VDC-BASIC
; Q1..Q4 stored (A = 1). Changes la..ld.
linecore:
@loop:  lda     #<la            ; outcodes
        ldy     #>la
        jsr     code
        sta     ca
        lda     #<lc
        ldy     #>lc
        jsr     code
        sta     cb
        ora     ca
        bne     :+
        jmp     draw
:       lda     ca
        and     cb
        beq     @clip
        lda     #0              ; outside
        rts
@clip:  lda     ca
        bne     :+
        lda     cb
:       sta     cc
        and     #12
        beq     @xside
        ; U = LA+(LC-LA)*(K-LB)/(LD-LB), V = K with K = VH (bit 8) or 0 (bit 4)
        ldx     #<vh
        ldy     #>vh
        lda     cc
        and     #8
        bne     :+
        ldx     #<zero
        ldy     #>zero
:       stx     pk
        sty     pk+1
        ldx     #0              ; P0 = LA, P1 = LC, Q0 = LB, Q1 = LD
        jsr     setp
        jsr     interp
        COPY    r, u
        lda     #<v
        sta     PTR
        lda     #>v
        sta     PTR+1
        lda     pk
        ldy     pk+1
        jsr     copy5
        jmp     @assign
@xside: ; V = LB+(LD-LB)*(K-LA)/(LC-LA), U = K with K = VW (bit 2) or 0 (bit 1)
        ldx     #<vw
        ldy     #>vw
        lda     cc
        and     #2
        bne     :+
        ldx     #<zero
        ldy     #>zero
:       stx     pk
        sty     pk+1
        ldx     #8              ; P0 = LB, P1 = LD, Q0 = LA, Q1 = LC
        jsr     setp
        jsr     interp
        COPY    r, v
        lda     #<u
        sta     PTR
        lda     #>u
        sta     PTR+1
        lda     pk
        ldy     pk+1
        jsr     copy5
@assign:                        ; (U,V) replaces the end point outside
        ldx     #9
        lda     cc
        cmp     ca
        beq     :+
        ldx     #19
:       ldy     #9
:       lda     u,y
        sta     la,x
        dex
        dey
        bpl     :-
        jmp     @loop

; setp: pp0 pp1 pq0 pq1 from ptab + X
setp:   ldy     #0
:       lda     ptab,x
        sta     pp0,y
        inx
        iny
        cpy     #8
        bne     :-
        rts
ptab:   .word   la, lc, lb, ld
        .word   lb, ld, la, lc

; interp: r = P0+(P1-P0)*(K-Q0)/(Q1-Q0), as BASIC evaluates it
interp: lda     pp0
        ldy     pp0+1
        jsr     MOVFM           ; FAC = P0
        lda     pp1
        ldy     pp1+1
        jsr     ldarg           ; ARG = P1
        jsr     FSUBT           ; P1-P0
        FST     ta
        lda     pq0
        ldy     pq0+1
        jsr     MOVFM           ; FAC = Q0
        lda     pk
        ldy     pk+1
        jsr     ldarg           ; ARG = K
        jsr     FSUBT           ; K-Q0
        FARG    ta
        jsr     FMULTT          ; (P1-P0)*(K-Q0)
        FST     ta
        lda     pq0
        ldy     pq0+1
        jsr     MOVFM           ; FAC = Q0
        lda     pq1
        ldy     pq1+1
        jsr     ldarg           ; ARG = Q1
        jsr     FSUBT           ; Q1-Q0
        FARG    ta
        jsr     FDIVT           ; .../(Q1-Q0)
        lda     pp0
        ldy     pp0+1
        jsr     ldarg
        jsr     FADDT           ; P0+...
        FST     r
        rts

; ldarg: ARG = packed number at (A/Y) in bank 0 (CONUPK of the ROM reads RAM 1); ends with LDA FACEXP
ldarg:  sta     SRC
        sty     SRC+1
        ldy     #4
        lda     (SRC),y
        sta     ARGEXP+4
        dey
        lda     (SRC),y
        sta     ARGEXP+3
        dey
        lda     (SRC),y
        sta     ARGEXP+2
        dey
        lda     (SRC),y
        sta     ARGSGN
        eor     FACSGN
        sta     ARISGN
        lda     ARGSGN
        ora     #$80
        sta     ARGEXP+1
        dey
        lda     (SRC),y
        sta     ARGEXP
        lda     FACEXP
        rts

; copy5: 5 bytes from (A/Y) to (PTR)
copy5:  sta     SRC
        sty     SRC+1
        ldy     #4
:       lda     (SRC),y
        sta     (PTR),y
        dey
        bpl     :-
        rts

; facint: FAC (a whole number 0..65535) -> ilo, X = high byte
facint: lda     #0
        sta     ilo
        tax
        lda     FACEXP
        beq     @done
        sec
        sbc     #$80            ; number of bits
        sta     cnt
        lda     FACEXP+1
        sta     ihi
        lda     FACEXP+2
        sta     ilo
        lda     #16
        sec
        sbc     cnt
        tay
        beq     @hi
@sh:    lsr     ihi
        ror     ilo
        dey
        bne     @sh
@hi:    ldx     ihi
@done:  rts

; code: outcode of the point (A/Y) (x), (A/Y)+5 (y); 1 x<0, 2 x>VW, 4 y<0, 8 y>VH
code:   sta     cp
        sty     cp+1
        lda     #0
        sta     oc
        lda     #<vw
        sta     lim
        lda     #>vw
        sta     lim+1
        lda     #1
        sta     obit
        jsr     code1
        lda     cp
        clc
        adc     #5
        sta     cp
        bcc     :+
        inc     cp+1
:       lda     #<vh
        sta     lim
        lda     #>vh
        sta     lim+1
        lda     #4
        sta     obit
        jsr     code1
        lda     oc
        rts
code1:  lda     cp
        ldy     cp+1
        sta     SRC
        sty     SRC+1
        ldy     #0
        lda     (SRC),y
        beq     :+              ; zero is not negative
        iny
        lda     (SRC),y
        bpl     :+
        lda     obit            ; < 0
        ora     oc
        sta     oc
        rts
:       lda     cp
        ldy     cp+1
        jsr     MOVFM
        lda     lim
        ldy     lim+1
        jsr     FCOMP
        cmp     #1
        bne     :+
        lda     obit            ; > limit
        asl
        ora     oc
        sta     oc
:       rts

.segment "BSS"
la:     .res    5               ; la lb lc ld vw vh ox oy follow each other
lb:     .res    5
lc:     .res    5
ld:     .res    5
vw:     .res    5
vh:     .res    5
ox:     .res    5
oy:     .res    5
lkv:    .res    5
u:      .res    5               ; u and v follow each other
v:      .res    5
r:      .res    5
ta:     .res    5
tb:     .res    5
px:     .res    5
py:     .res    5
ag:     .res    5
sn:     .res    5
cs:     .res    5
t1v:    .res    5
z1v:    .res    5
t2v:    .res    5
z2v:    .res    5
u1v:    .res    5
u2v:    .res    5
e3:     .res    5
f3:     .res    5
e4:     .res    5
f4:     .res    5
ix:     .res    5
iy:     .res    5
dt:     .res    5
i1:     .res    5
j1:     .res    5
i2:     .res    5
j2:     .res    5
x1:     .res    5               ; x1 x2 ya yb yc yd follow each other
x2:     .res    5
ya:     .res    5
yb:     .res    5
yc:     .res    5
yd:     .res    5
pp0:    .res    2
pp1:    .res    2
pq0:    .res    2
pq1:    .res    2
pk:     .res    2
cp:     .res    2
lim:    .res    2
hp:     .res    2
csrc:   .res    2
cdst:   .res    2
ccnt:   .res    2
tt:     .res    2
de:     .res    2
dt2:    .res    2
pa:     .res    2
vi:     .res    1
yi:     .res    1
ca:     .res    1
cb:     .res    1
cc:     .res    1
oc:     .res    1
obit:   .res    1
ilo:    .res    1
ihi:    .res    1
cnt:    .res    1
hi:     .res    1
li:     .res    1
lj:     .res    1
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
pb:     .res    1
pc:     .res    1
cn:     .res    1
yo:     .res    1
ph:     .res    1
pl:     .res    1
fr:     .res    1
rx2:    .res    1
ry2:    .res    1
ri:     .res    1
rowlo:  .res    200
rowhi:  .res    200
