; vicasm: vertrot on the VIC-II as a machine code program only (no BASIC program).
; Multicolour bitmap 160 x 200, SLOW (1 MHz, picture on). The same geometry as vic.bas (and vtasm.s):
; the floating point routines of the BASIC ROM in the order in which BASIC evaluates the statements;
; x halved, y as in the original. Two pages with a real page flip: bitmap $A000 / screen $8C00 (VIC bank
; 2) and bitmap $E000 / screen $CC00 (VIC bank 3), the same $D018 ($38), so a flip is one write to
; $DD00 at the start of the vertical blank. A page is cleared by drawing the lines it got two frames
; before again with bit pair 00, then the wall (view 1) and the player (view 2) again.
; Colours of the cells and bit pairs as in vicshow.s / vic.bas (01 yellow, 10 grey/brown, 11 white or
; the border colour); the rows outside the views are black. The Kernal leaves the VIC alone ($D8 = $FF).
; JSR $1BFE / $1BFF (RTS) mark the drawing of each frame for vicrun.py; $0FFE/$0FFF = the page drawn into.
; At the end: $D7FF = 0 (debug cartridge), a key, text screen again, time per frame (jiffy clock).
; The path is the one of the listings:  DATA "UUUUUUUUUURRRRRRRRRRAAAAAAAAAALLLLLLLLLLLLLLLLLLLLUUUUUUUUUUUUUURRRRRRRRRRRRRRRRDDDDDDDDDD"
;
; Build: cl65 -t none -C vicasm.cfg -o vicasm.prg vicasm.s       Start: LOAD"VICASM",8 : RUN

GETIN   = $FFE4
CHROUT  = $FFD2
FOUT    = $AF06         ; FAC -> string at (A/Y)
GIVAYF  = $AF03         ; FAC = signed integer A (high), Y (low)
SIN     = $AF42
COS     = $AF3F
MARK1   = $1BFE
MARK2   = $1BFF
SHOTPG  = $0FFE         ; for vicrun.py: address of the page drawn into
GRAPHM  = $D8
SCRA    = $8C00
SCRB    = $CC00
COLRAM  = $D800

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
        jsr     rowinit
        jsr     vicinit
        ; 100-120: A1 B1 A2 B2 are constants; PX=50:PY=50:AG=0:SN=SIN(AG):CS=COS(AG)
        COPY    k50, px
        COPY    k50, py
        COPY    zero, ag
        jsr     sincos
        lda     #$E0            ; the first frame goes into page B ($E000)
        sta     pg
        lda     #0
        sta     fr
        sta     nla
        sta     nlb
        lda     $A2             ; 130 TT=TI
        sta     tt
        lda     $A1
        sta     tt+1

frame:  jsr     MARK1           ; 210
        lda     #0
        sta     SHOTPG
        lda     pg
        sta     SHOTPG+1
        jsr     clearpg         ; the lines of two frames ago away, wall and player again
        ; 310 OX=2:OY=40:VW=49:VH=109
        COPY    k2, ox
        COPY    k40, oy
        COPY    k49, vw
        COPY    k109, vh
        ; 340 LA=PX/2:LB=PY:LC=(CS*5+PX)/2:LD=SN*5+PY:LK=2
        jsr     fscript
        .byte   1
        .word   k2
        .byte   2
        .word   px
        .byte   7
        .byte   3
        .word   la
        .byte   1
        .word   py
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
        .word   ld
        .byte   0
        COPY    k2, lkv
        jsr     linecore
        ; 350 LA=PX/2:LB=PY:LC=LA:LD=LB:LK=3
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
        .word   py
        .byte   3
        .word   lb
        .byte   3
        .word   ld
        .byte   0
        COPY    k3, lkv
        jsr     linecore
        ; 410 OX=54
        COPY    k54, ox
        ; 430-480 rotation, 490 LA=(50-T1)/2:LB=50-Z1:LC=(50-T2)/2:LD=50-Z2:LK=1
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
        .byte   1               ; LA=(50-T1)/2
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
        .byte   1               ; LB=50-Z1
        .word   z1v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   lb
        .byte   1               ; LC=(50-T2)/2
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
        .byte   1               ; LD=50-Z2
        .word   z2v
        .byte   2
        .word   k50
        .byte   4
        .byte   3
        .word   ld
        .byte   0
        COPY    k1, lkv
        jsr     linecore
        ; 492/493 the player again if the wall may have covered it
        jsr     covered
        bcc     :+
        jsr     player
:       ; 510 OX=107:VW=50
        COPY    k107, ox
        COPY    k50, vw
        ; --- 530-630 as in vtview.s (y not halved)
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
        ; (50+P)/2 for P = X1 X2, 50+P for YA YB YC YD, as in the statements 600-630 of vic.bas
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
        lda     hi              ; X1, X2: (50+P)/2; YA..YD: 50+P (y as in the original)
        cmp     #2
        bcs     @hy
        FST     ta
        FLD     k2
        FARG    ta
        jsr     FDIVT           ; (50+P)/2
@hy:    ldx     hp
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
        lda     $DD00           ; VIC bank 0, the Kernal takes the VIC back (text screen)
        ora     #3
        sta     $DD00
        lda     #0
        sta     GRAPHM
        lda     port0
        sta     $01
        lda     bg0
        sta     $D021
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
; LA<=24 AND LC<=24 OR LB>=51 AND LD>=51 OR LB<=44 AND LD<=44
covered:
        lda     #<k26
        ldx     #>k26
        jsr     setk
        lda     #<la
        ldy     #>la
        jsr     gek
        bcc     @c2
        lda     #<lc
        ldy     #>lc
        jsr     gek
        bcs     @away
@c2:    lda     #<k24
        ldx     #>k24
        jsr     setk
        lda     #<la
        ldy     #>la
        jsr     lek
        bcc     @c3
        lda     #<lc
        ldy     #>lc
        jsr     lek
        bcs     @away
@c3:    lda     #<k51
        ldx     #>k51
        jsr     setk
        lda     #<lb
        ldy     #>lb
        jsr     gek
        bcc     @c4
        lda     #<ld
        ldy     #>ld
        jsr     gek
        bcs     @away
@c4:    lda     #<k44
        ldx     #>k44
        jsr     setk
        lda     #<lb
        ldy     #>lb
        jsr     lek
        bcc     @near
        lda     #<ld
        ldy     #>ld
        jsr     lek
        bcs     @away
@near:  sec
        rts
@away:  clc
        rts
setk:   sta     kp
        stx     kp+1
        rts
; (A/Y) >= / <= the constant at kp: carry set if true
gek:    jsr     MOVFM
        lda     kp
        ldy     kp+1
        jsr     FCOMP
        cmp     #$FF            ; smaller: false
        beq     cfalse
        sec
        rts
lek:    jsr     MOVFM
        lda     kp
        ldy     kp+1
        jsr     FCOMP
        cmp     #1              ; greater: false
        beq     cfalse
        sec
        rts
cfalse: clc
        rts

; draw (for linecore): OX/OY + INT(L+.5) as integers, then the line with bit pair INT(LK)
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
        lda     #1              ; record it for clearing the page later
        sta     rec
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

; clearpg: the recorded lines of page pg again with bit pair 00, then the wall and the player
clearpg:
        lda     #0
        sta     rec
        sta     lcol
        jsr     listp           ; SRC = list, X = number
        stx     ln
        lda     #0
        sta     li
@e:     lda     li
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
        inc     li
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
        jmp     dline

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
        sta     li
@row:   ldy     #39
@col:   lda     li
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
        lda     li
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
:       inc     li
        lda     li
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

; packed numbers (constants)
half:   .byte   $80,0,0,0,0
zero:   .byte   0,0,0,0,0
kp4:    .byte   $73,$51,$B7,$17,$5A     ; .0001 as BASIC 7.0 stores it
km4:    .byte   $73,$D1,$B7,$17,$5A     ; -.0001
k01:    .byte   $7D,$4C,$CC,$CC,$CD     ; .1
k1:     .byte   $81,0,0,0,0
k2:     .byte   $82,0,0,0,0
k3:     .byte   $82,$40,0,0,0
k5:     .byte   $83,$20,0,0,0
k16:    .byte   $85,0,0,0,0
k20:    .byte   $85,$20,0,0,0
km20:   .byte   $85,$A0,0,0,0
k24:    .byte   $85,$40,0,0,0
k26:    .byte   $85,$50,0,0,0
k40:    .byte   $86,$20,0,0,0
k44:    .byte   $86,$30,0,0,0
k49:    .byte   $86,$44,0,0,0
k50:    .byte   $86,$48,0,0,0
km50:   .byte   $86,$C8,0,0,0
k51:    .byte   $86,$4C,0,0,0
k54:    .byte   $86,$58,0,0,0
k60:    .byte   $86,$70,0,0,0
k70:    .byte   $87,$0C,0,0,0
k107:   .byte   $87,$56,0,0,0
k109:   .byte   $87,$5A,0,0,0
k1000:  .byte   $8A,$7A,0,0,0
fsop:   .byte   0
fslo:   .byte   0
; --- shared with vtview.s
lntab:  .word   x1, ya, x2, yc, k1      ; 600 top
        .word   x1, yb, x2, yd, k1      ; 610 bottom
        .word   x1, ya, x1, yb, k2      ; 620 left
        .word   x2, yc, x2, yd, k2      ; 630 right

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
mmu:    .res    1
rec:    .res    1
pg:     .res    1
nla:    .res    1
nlb:    .res    1
ln:     .res    1
port0:  .res    1
bg0:    .res    1
kp:     .res    2
lista:  .res    64
listb:  .res    64
rowlo:  .res    200
rowhi:  .res    200
