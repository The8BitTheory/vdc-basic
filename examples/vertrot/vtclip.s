; vtclip: Cohen-Sutherland clipping for vertrot3.bas in machine code. It uses the floating point
; routines of the BASIC ROM in the same order as the BASIC version (lines 3015-3110 of vertrot2.bas),
; so it gives the same pixels; only the interpreting of the statements is gone.
;
; Call: SYS 4864 (from a compiled program ::SYS 4864) with BANK 15.
; In:  the BASIC variables LA LB LC LD (end points in view coordinates), VW VH (largest x and y of
;      the view), OX OY (top left pixel of the view).
; Out: QV = 1 and Q1..Q4 = OX+INT(LA+.5), OY+INT(LB+.5), OX+INT(LC+.5), OY+INT(LD+.5) of the
;      clipped line, or QV = 0 if no part of the line is in the view.
; The variables are looked up by name in the variable table (RAM 1, VARTAB $2F to ARYTAB $31) at the
; first call; all 13 must exist by then. Scalars do not move afterwards (Blitz! has a fixed table, the
; interpreter adds new scalars behind the old ones).
;
; Build: cl65 -t none -C vtclip.cfg -o vtclip.prg vtclip.s

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
PTR     = $FB           ; pointer into RAM 1 for INDFET/INDSTA
SRC     = $FD           ; pointer into bank 0

NIN     = 8             ; LA LB LC LD VW VH OX OY are copied in
NV      = 13            ; plus Q1 Q2 Q3 Q4 QV

.segment "LOADADDR"
        .word   $1300

.segment "CODE"
entry:  lda     ready
        bne     go
        jsr     lookup
        bcc     :+
        rts                     ; a variable is missing
:       inc     ready

go:     lda     #0              ; copy the inputs from RAM 1 into val
        sta     vi
        sta     di
cin:    ldx     vi
        lda     adrlo,x
        sta     PTR
        lda     adrhi,x
        sta     PTR+1
        lda     #0
        sta     yi
cin2:   ldy     yi
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldx     di
        sta     val,x
        inc     di
        inc     yi
        lda     yi
        cmp     #5
        bne     cin2
        inc     vi
        lda     vi
        cmp     #NIN
        bne     cin

loop:   lda     #<la            ; 3020-3023: outcodes
        ldy     #>la
        jsr     code
        sta     ca
        lda     #<lc
        ldy     #>lc
        jsr     code
        sta     cb
        ora     ca              ; 3030
        bne     :+
        jmp     draw
:       lda     ca              ; 3040
        and     cb
        beq     clip
        ldx     #12             ; QV = 0
        lda     #<zero
        ldy     #>zero
        jmp     store

clip:   lda     ca              ; 3050
        bne     :+
        lda     cb
:       sta     cc
        and     #12
        beq     xside
        ; 3060/3070: U = LA+(LC-LA)*(K-LB)/(LD-LB), V = K with K = VH (bit 8) or 0 (bit 4)
        ldx     #<vh
        ldy     #>vh
        lda     cc
        and     #8
        bne     :+
        ldx     #<zero
        ldy     #>zero
:       stx     pk
        sty     pk+1
        lda     #<la
        sta     pp0
        lda     #>la
        sta     pp0+1
        lda     #<lc
        sta     pp1
        lda     #>lc
        sta     pp1+1
        lda     #<lb
        sta     pq0
        lda     #>lb
        sta     pq0+1
        lda     #<ld
        sta     pq1
        lda     #>ld
        sta     pq1+1
        jsr     interp          ; r = U
        ldx     #4
:       lda     r,x
        sta     u,x
        dex
        bpl     :-
        lda     pk
        ldy     pk+1
        ldx     #<v
        jsr     copy5
        jmp     assign
xside:  ; 3080/3090: V = LB+(LD-LB)*(K-LA)/(LC-LA), U = K with K = VW (bit 2) or 0 (bit 1)
        ldx     #<vw
        ldy     #>vw
        lda     cc
        and     #2
        bne     :+
        ldx     #<zero
        ldy     #>zero
:       stx     pk
        sty     pk+1
        lda     #<lb
        sta     pp0
        lda     #>lb
        sta     pp0+1
        lda     #<ld
        sta     pp1
        lda     #>ld
        sta     pp1+1
        lda     #<la
        sta     pq0
        lda     #>la
        sta     pq0+1
        lda     #<lc
        sta     pq1
        lda     #>lc
        sta     pq1+1
        jsr     interp          ; r = V
        ldx     #4
:       lda     r,x
        sta     v,x
        dex
        bpl     :-
        lda     pk
        ldy     pk+1
        ldx     #<u
        jsr     copy5
assign: ldx     #9              ; 3100/3110: (U,V) replaces the end point outside
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
        jmp     loop

draw:   ; 3015: Qk = OX/OY + INT(L+.5), QV = 1
        lda     #0
        sta     vi
dr:     lda     #<half
        ldy     #>half
        jsr     MOVFM           ; FAC = .5
        lda     vi              ; ARG = LA, LB, LC or LD
        asl
        asl
        adc     vi              ; *5 (carry is clear)
        adc     #<la
        ldy     #>la
        bcc     :+
        iny
:       jsr     ldarg
        jsr     FADDT           ; L+.5
        jsr     FINT
        lda     vi
        and     #1
        tax
        lda     oxlo,x
        ldy     #>ox
        jsr     ldarg
        jsr     FADDT           ; OX+INT(...)
        ldx     #<r
        ldy     #>r
        jsr     MOVMF
        lda     vi
        clc
        adc     #8
        tax
        lda     #<r
        ldy     #>r
        jsr     store
        inc     vi
        lda     vi
        cmp     #4
        bne     dr
        ldx     #12
        lda     #<one
        ldy     #>one
        ; fall through: QV = 1

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

; interp: r = P0+(P1-P0)*(K-Q0)/(Q1-Q0), as BASIC evaluates it: a left operand that waits for
; its right one is pushed (rounded, MOVMF), the right one stays in FAC.
interp: lda     pp0
        ldy     pp0+1
        jsr     MOVFM           ; FAC = P0
        lda     pp1
        ldy     pp1+1
        jsr     ldarg           ; ARG = P1
        jsr     FSUBT           ; P1-P0
        ldx     #<t1
        ldy     #>t1
        jsr     MOVMF
        lda     pq0
        ldy     pq0+1
        jsr     MOVFM           ; FAC = Q0
        lda     pk
        ldy     pk+1
        jsr     ldarg           ; ARG = K
        jsr     FSUBT           ; K-Q0
        lda     #<t1
        ldy     #>t1
        jsr     ldarg
        jsr     FMULTT          ; (P1-P0)*(K-Q0)
        ldx     #<t1
        ldy     #>t1
        jsr     MOVMF
        lda     pq0
        ldy     pq0+1
        jsr     MOVFM           ; FAC = Q0
        lda     pq1
        ldy     pq1+1
        jsr     ldarg           ; ARG = Q1
        jsr     FSUBT           ; Q1-Q0
        lda     #<t1
        ldy     #>t1
        jsr     ldarg
        jsr     FDIVT           ; .../(Q1-Q0)
        lda     pp0
        ldy     pp0+1
        jsr     ldarg
        jsr     FADDT           ; P0+...
        ldx     #<r
        ldy     #>r
        jmp     MOVMF

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

; copy5: 5 bytes from (A/Y) to X (low byte, same page as val)
copy5:  sta     SRC
        sty     SRC+1
        stx     cdst
        ldy     #4
:       lda     (SRC),y
cdst = * + 1
        sta     val,y
        dey
        bpl     :-
        rts

; code: outcode of the point (A/Y) (x), (A/Y)+5 (y); 1 x<0, 2 x>VW, 4 y<0, 8 y>VH
code:   sta     SRC
        sty     SRC+1
        lda     #0
        sta     oc
        lda     #<vw
        sta     lim
        lda     #1
        sta     obit
        jsr     code1
        lda     SRC
        clc
        adc     #5
        sta     SRC
        bcc     :+
        inc     SRC+1
:       lda     #<vh
        sta     lim
        lda     #4
        sta     obit
        jsr     code1
        lda     oc
        rts
code1:  ldy     #0
        lda     (SRC),y
        beq     :+              ; zero is not negative
        iny
        lda     (SRC),y
        bpl     :+
        lda     obit             ; < 0
        ora     oc
        sta     oc
        rts
:       lda     SRC
        ldy     SRC+1
        jsr     MOVFM
        lda     lim
        ldy     #>vw
        jsr     FCOMP
        cmp     #1
        bne     :+
        lda     obit             ; > limit
        asl
        ora     oc
        sta     oc
:       rts

; lookup: addresses of the values of the 13 variables in RAM 1; carry set if one is missing
lookup: ldx     #0
lk:     stx     vi
        lda     VARTAB
        sta     PTR
        lda     VARTAB+1
        sta     PTR+1
lk1:    lda     PTR             ; PTR >= ARYTAB: not found
        cmp     ARYTAB
        lda     PTR+1
        sbc     ARYTAB+1
        bcs     lkno
        ldy     #0
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldx     vi
        cmp     name1,x
        bne     lknext
        ldy     #1
        lda     #PTR
        ldx     #1
        jsr     INDFET
        ldx     vi
        cmp     name2,x
        beq     lkyes
lknext: lda     PTR
        clc
        adc     #7
        sta     PTR
        bcc     lk1
        inc     PTR+1
        bne     lk1
lkyes:  lda     PTR
        clc
        adc     #2
        sta     adrlo,x
        lda     PTR+1
        adc     #0
        sta     adrhi,x
        inx
        cpx     #NV
        bne     lk
        clc
        rts
lkno:   sec
        rts

name1:  .byte   'L','L','L','L','V','V','O','O','Q','Q','Q','Q','Q'
name2:  .byte   'A','B','C','D','W','H','X','Y','1','2','3','4','V'
oxlo:   .byte   <ox, <oy
ready:  .byte   0

.segment "WORK"
; all packed numbers lie in one page (copy5, ldarg of ox/oy, FCOMP of vw/vh use one high byte)
val:
la:     .res    5
lb:     .res    5
lc:     .res    5
ld:     .res    5
vw:     .res    5
vh:     .res    5
ox:     .res    5
oy:     .res    5
u:      .res    5               ; u and v must follow each other (assign copies 10 bytes)
v:      .res    5
r:      .res    5
t1:     .res    5
half:   .byte   $80,0,0,0,0
one:    .byte   $81,0,0,0,0
zero:   .byte   0,0,0,0,0
pp0:    .res    2
pp1:    .res    2
pk:     .res    2
pq0:    .res    2
pq1:    .res    2
adrlo:  .res    NV
adrhi:  .res    NV
vi:     .res    1
di:     .res    1
yi:     .res    1
ca:     .res    1
cb:     .res    1
cc:     .res    1
oc:     .res    1
lim:    .res    1
obit:    .res    1
