; vtview: machine code for vertrot5.bas. Two entries:
;   SYS 4864  one line (as vtline.s): clip, round, draw. In: LA LB LC LD VW VH OX OY LK.
;   SYS 4867  the whole perspective view (lines 530-630 of vertrot4.bas). In: T1 Z1 T2 Z2 (the
;             rotated end points of the wall), OX OY VW VH (the view).
; Both use the floating point routines of the BASIC ROM in the order in which BASIC evaluates the
; statements (a left operand that waits for its right one is pushed, i.e. rounded with MOVMF; the
; right one stays in FAC), so the pixels are the same as those of the BASIC version. The lines are
; drawn by the line code of VDC-BASIC v3 (VDL after its parameter parsing, VDLCORE).
; Out: QV = 0. If the VDC-BASIC in memory is not the expected one (version byte $1C03 = $30 and
; LDX #6 at VDLCORE): SYS 4864 draws nothing, sets QV = 1 and Q1..Q4 = OX+INT(LA+.5), OY+INT(LB+.5),
; OX+INT(LC+.5), OY+INT(LD+.5) for a VDL in BASIC; SYS 4867 does nothing and sets QV = 2.
; The variables are looked up by name in the variable table (RAM 1, VARTAB $2F to ARYTAB $31) at the
; first call; all 18 must exist by then. The constants .0001 and -.0001 are the bytes BASIC 7.0
; stores for them (read from a variable on the C128).
;
; Build: cl65 -t none -C vtview.cfg -o vtview.prg vtview.s

VDLCORE = $25DE
VERSION = $1C03
ARG1    = $83

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

NV      = 18            ; LA LB LC LD VW VH OX OY LK Q1 Q2 Q3 Q4 QV T1 Z1 T2 Z2
V_QV    = 13
V_Q1    = 9

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
        .word   $1300

.segment "CODE"
        jmp     lineent         ; SYS 4864
        jmp     view3ent        ; SYS 4867

;--------------------------------------------------------------------------------
; first call: look up the variables, check VDC-BASIC; carry set if a variable is missing
init:   lda     ready
        bne     @ok
        jsr     lookup
        bcs     @rts
        inc     ready
        lda     VERSION         ; draw directly only with the expected VDC-BASIC
        cmp     #$30
        bne     @ok
        lda     VDLCORE
        cmp     #$A2            ; LDX #6
        bne     @ok
        lda     VDLCORE+1
        cmp     #6
        bne     @ok
        inc     direct
@ok:    clc
@rts:   rts

; fetch: variable vi (5 bytes) from RAM 1 to la + 5 * (vi) ... for vi = A .. X-1
fetchn: sta     vi
        stx     vend
@v:     lda     vi
        jsr     mul5
        clc
        adc     #<la
        sta     SRC
        lda     #>la
        adc     #0
        sta     SRC+1
        ldx     vi
        jsr     fetch
        inc     vi
        lda     vi
        cmp     vend
        bne     @v
        rts

; fetch: variable X from RAM 1 to (SRC)
fetch:  lda     adrlo,x
        sta     PTR
        lda     adrhi,x
        sta     PTR+1
        lda     #0
        sta     yi
@b:     ldy     yi
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldy     yi
        sta     (SRC),y
        iny
        sty     yi
        cpy     #5
        bne     @b
        rts

mul5:   sta     yi              ; A * 5 (A < 51)
        asl
        asl
        clc
        adc     yi
        rts

;--------------------------------------------------------------------------------
; SYS 4864: one line
lineent:
        jsr     init
        bcc     :+
        rts                     ; a variable is missing
:       lda     #0              ; LA LB LC LD VW VH OX OY LK
        ldx     #9
        jsr     fetchn
        jsr     linecore
        ldx     #V_QV
        cmp     #0
        bne     :+
        lda     #<zero          ; drawn or nothing to draw: QV = 0
        ldy     #>zero
        jmp     store
:       lda     #<one           ; Q1..Q4 for a VDL in BASIC: QV = 1
        ldy     #>one
        jmp     store

;--------------------------------------------------------------------------------
; SYS 4867: the perspective view
view3ent:
        jsr     init
        bcc     :+
        rts
:       lda     direct
        bne     :+
        ldx     #V_QV           ; QV = 2: BASIC draws the view itself
        lda     #<two
        ldy     #>two
        jmp     store
:       lda     #4              ; VW VH OX OY
        ldx     #8
        jsr     fetchn
        lda     #<t1v           ; T1 Z1 T2 Z2
        sta     SRC
        lda     #>t1v
        sta     SRC+1
        ldx     #14
        jsr     fetch
        lda     #<z1v
        sta     SRC
        lda     #>z1v
        sta     SRC+1
        ldx     #15
        jsr     fetch
        lda     #<t2v
        sta     SRC
        lda     #>t2v
        sta     SRC+1
        ldx     #16
        jsr     fetch
        lda     #<z2v
        sta     SRC
        lda     #>z2v
        sta     SRC+1
        ldx     #17
        jsr     fetch
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
v3done: ldx     #V_QV
        lda     #<zero
        ldy     #>zero
        jmp     store

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

; draw: Qk = OX/OY + INT(L+.5); VDL, or Q1..Q4 into the variables
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
        lda     direct
        beq     @q
        jsr     facint          ; integer into arg1..arg4
        lda     vi
        asl
        tay
        lda     ilo
        sta     ARG1,y
        txa
        sta     ARG1+1,y
        jmp     @next
@q:     FST     r
        lda     vi
        clc
        adc     #V_Q1
        tax
        lda     #<r
        ldy     #>r
        jsr     store
@next:  inc     vi
        lda     vi
        cmp     #4
        bne     @dr
        lda     direct
        bne     :+
        lda     #1              ; Q1..Q4 stored
        rts
:       FLD     lkv             ; arg5 = colour
        jsr     facint
        lda     ilo
        sta     ARG1+8
        stx     ARG1+9
        lda     $FF00           ; I/O on, as VDC-BASIC does before a command
        and     #$FE
        sta     $FF00
        jsr     VDLCORE
        lda     #0
        rts
oxlo:   .byte   <ox, <oy
oxhi:   .byte   >ox, >oy

; store: the 5 bytes at (A/Y) into variable X (in RAM 1)
store:  sta     SRC
        sty     SRC+1
        lda     adrlo,x
        sta     PTR
        lda     adrhi,x
        sta     PTR+1
        lda     #PTR
        sta     STAVEC
        lda     #0
        sta     yi
:       ldy     yi
        lda     (SRC),y
        ldx     #1
        jsr     INDSTA
        inc     yi
        lda     yi
        cmp     #5
        bne     :-
        rts

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

; lookup: addresses of the values of the variables in RAM 1; carry set if one is missing
lookup: ldx     #0
@var:   stx     vi
        lda     VARTAB
        sta     PTR
        lda     VARTAB+1
        sta     PTR+1
@scan:  lda     PTR             ; PTR >= ARYTAB: not found
        cmp     ARYTAB
        lda     PTR+1
        sbc     ARYTAB+1
        bcs     @no
        ldy     #0
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldx     vi
        cmp     name1,x
        bne     @next
        ldy     #1
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldx     vi
        cmp     name2,x
        beq     @yes
@next:  lda     PTR
        clc
        adc     #7
        sta     PTR
        bcc     @scan
        inc     PTR+1
        bne     @scan
@yes:   lda     PTR
        clc
        adc     #2
        sta     adrlo,x
        lda     PTR+1
        adc     #0
        sta     adrhi,x
        inx
        cpx     #NV
        bne     @var
        clc
        rts
@no:    sec
        rts

name1:  .byte   'L','L','L','L','V','V','O','O','L','Q','Q','Q','Q','Q','T','Z','T','Z'
name2:  .byte   'A','B','C','D','W','H','X','Y','K','1','2','3','4','V','1','1','2','2'

ready:  .byte   0
direct: .byte   0
half:   .byte   $80,0,0,0,0
one:    .byte   $81,0,0,0,0
two:    .byte   $82,0,0,0,0
zero:   .byte   0,0,0,0,0
kp4:    .byte   $73,$51,$B7,$17,$5A     ; .0001 as BASIC 7.0 stores it
km4:    .byte   $73,$D1,$B7,$17,$5A     ; -.0001
k20:    .byte   $85,$20,0,0,0
km20:   .byte   $85,$A0,0,0,0
k5:     .byte   $83,$20,0,0,0
k16:    .byte   $85,0,0,0,0
k50:    .byte   $86,$48,0,0,0
km50:   .byte   $86,$C8,0,0,0
k2:     .byte   $82,0,0,0,0
k13:    .byte   $84,$50,0,0,0
k12:    .byte   $84,$40,0,0,0
fsop:   .byte   0
fslo:   .byte   0

.segment "WORK"   ; $0E00-$0FFF (sprite area), no initial values
la:     .res    5               ; la lb lc ld vw vh ox oy follow each other (fetchn, draw)
lb:     .res    5
lc:     .res    5
ld:     .res    5
vw:     .res    5
vh:     .res    5
ox:     .res    5
oy:     .res    5
lkv:    .res    5
u:      .res    5               ; u and v follow each other (assign copies 10 bytes)
v:      .res    5
r:      .res    5
ta:     .res    5
tb:     .res    5
t1v:    .res    5
z1v:    .res    5
t2v:    .res    5
z2v:    .res    5
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
x1:     .res    5               ; x1 x2 ya yb yc yd follow each other (the halving loop)
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
adrlo:  .res    NV
adrhi:  .res    NV
vi:     .res    1
vend:   .res    1
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
