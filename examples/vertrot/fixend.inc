; fixend.inc: the last line of vtfix.s / vicfix.s, without floating point:
;   PRINT CHR$(147)FR"FRAMES,"INT((TT*1000+30*FR)/(60*FR))"MS PER FRAME"
; with muldiv (fixcore.inc); T1, U1, T2, U2 of the geometry serve as variables. Needs fr (frames)
; and tt (jiffies, a word).
report: lda     #147
        jsr     CHROUT
        ldx     #3
        lda     #0
:       sta     t1,x
        sta     u1,x
        dex
        bpl     :-
        lda     tt
        sta     t1
        lda     tt+1
        sta     t1+1
        lda     fr
        sta     u1
        jsr     xs
        .byte   X_LD, O(u1), X_MD, O(k30i), O(kint1), X_ST, O(u2)                   ; 30*FR
        .byte   X_LD, O(t1), X_MD, O(k1000i), O(kint1), X_ADD, O(u2), X_ST, O(t2)    ; TT*1000+30*FR
        .byte   X_LD, O(u1), X_MD, O(k60i), O(kint1), X_ST, O(u2)                   ; 60*FR
        .byte   X_LD, O(kint1), X_MD, O(t2), O(u2)                                  ; the quotient
        .byte   X_END
        lda     fr
        ldx     #0
        jsr     prdec
        ldx     #0
:       lda     txt1,x
        beq     :+
        jsr     CHROUT
        inx
        bne     :-
:       lda     acc
        ldx     acc+1
        jsr     prdec
        ldx     #0
:       lda     txt2,x
        beq     :+
        jsr     CHROUT
        inx
        bne     :-
:       rts
txt1:   .byte   "FRAMES,", 0
txt2:   .byte   "MS PER FRAME", 13, 0

; prdec: the word A/X in decimal as BASIC prints a number (a blank before and after)
prdec:  sta     dnum
        stx     dnum+1
        lda     #' '
        jsr     CHROUT
        lda     #0
        sta     dlead
        ldy     #8
@p:     ldx     #'0'
@s:     lda     dnum            ; subtract the power of ten as often as it goes
        sec
        sbc     dpow,y
        pha
        lda     dnum+1
        sbc     dpow+1,y
        bcc     @o
        sta     dnum+1
        pla
        sta     dnum
        inx
        bne     @s
@o:     pla
        txa
        cmp     #'0'
        bne     @d
        ldx     dlead
        bne     @d
        cpy     #0
        bne     @n
@d:     sta     dlead
        jsr     CHROUT
@n:     dey
        dey
        bpl     @p
        lda     #' '
        jmp     CHROUT
dpow:   .word   1, 10, 100, 1000, 10000
