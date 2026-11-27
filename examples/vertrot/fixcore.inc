; fixcore.inc: the geometry of vertrot in fixed point, for vtfix.s (VDC) and vicfix.s (VIC: VIC = 1).
; Bit for bit the model fixref.py: geometry 16.16, projected coordinates and clipping 24.8, all
; multiplying and dividing by muldiv (a*b/c with a 64 bit product, truncated towards zero).
; All 32 bit numbers, constants and variables, lie in one page (segment FV, page aligned) and are
; addressed by their offset in it (O_name); the script interpreter xs takes such offsets as operands.
; The including file provides: dline (line lx1,ly1 - lx2,ly2 in colour lcol), player (the player of
; view 2 again), the byte oyb (top row of the views on the page drawn into), the bytes lx1 ly1 lx2 ly2
; lcol (in this order) and the colours C_DIR C_POINT C_WALL C_V3H C_V3S.
; Uses SRC ($FD/$FE) for the script pointer.

.ifndef SRC
SRC     = $FD
.endif

.ifdef VIC
YSH     = 8             ; 16.16 -> 24.8 for y (as in the original)
.else
YSH     = 9             ; and halved (160 x 100)
.endif

; script opcodes (xs), operands are offsets in FV
X_END   = 0
X_LD    = 1             ; acc = v
X_ST    = 2             ; v = acc
X_ADD   = 3             ; acc += v
X_SUB   = 4             ; acc -= v
X_RSUB  = 5             ; acc = v - acc
X_MUL   = 6             ; acc = muldiv(acc, v, 1.0)
X_DIV   = 7             ; acc = muldiv(acc, 1.0, v)
X_MD    = 8             ; acc = muldiv(acc, v, w)
X_ASR   = 9             ; acc >>= n (arithmetic)
X_SHL   = 10            ; acc <<= n
X_NEG   = 11            ; acc = -acc

; O(name) = offset of a number in FV: the including file defines it at its top:  .define O(name) (name - fv)

;--------------------------------------------------------------------------------
; geom: one frame; px, py (16.16) and the angle index ki (k + 32) as they are
geom:   lda     ki              ; SN, CS from the table
        asl
        asl
        tax
        ldy     #0
:       lda     sintab,x
        sta     sn,y
        lda     costab,x
        sta     cs,y
        inx
        iny
        cpy     #4
        bne     :-
        ; view 1: LA=PX/2, LB=PY/2 (VIC: PY), LC=(CS*5+PX)/2, LD=(SN*5+PY)/2 (VIC: SN*5+PY)
        jsr     xs
        .byte   X_LD, O(px), X_ASR, 9, X_ST, O(la)
        .byte   X_LD, O(py), X_ASR, YSH, X_ST, O(lb)
        .byte   X_LD, O(cs), X_SHL, 2, X_ADD, O(cs), X_ADD, O(px), X_ASR, 9, X_ST, O(lc)
        .byte   X_LD, O(sn), X_SHL, 2, X_ADD, O(sn), X_ADD, O(py), X_ASR, YSH, X_ST, O(ld)
        .byte   X_END
        lda     #2
        ldx     #49
        jsr     setview
        lda     #C_DIR
        jsr     clipline
        jsr     xs              ; the player: from (LA,LB) to itself
        .byte   X_LD, O(px), X_ASR, 9, X_ST, O(la), X_ST, O(lc)
        .byte   X_LD, O(py), X_ASR, YSH, X_ST, O(lb), X_ST, O(ld)
        .byte   X_END
        lda     #C_POINT
        jsr     clipline
        ; view 2
        jsr     xs
        .byte   X_LD, O(px), X_RSUB, O(k70), X_ST, O(t1), X_ST, O(t2)           ; T1=T2=70-PX
        .byte   X_LD, O(py), X_RSUB, O(k20), X_ST, O(u1)                         ; U1=20-PY
        .byte   X_LD, O(py), X_RSUB, O(k70), X_ST, O(u2)                         ; U2=70-PY
        .byte   X_LD, O(t1), X_MUL, O(cs), X_ST, O(ta)                           ; Z1=T1*CS+U1*SN
        .byte   X_LD, O(u1), X_MUL, O(sn), X_ADD, O(ta), X_ST, O(z1)
        .byte   X_LD, O(t2), X_MUL, O(cs), X_ST, O(ta)                           ; Z2=T2*CS+U2*SN
        .byte   X_LD, O(u2), X_MUL, O(sn), X_ADD, O(ta), X_ST, O(z2)
        .byte   X_LD, O(t1), X_MUL, O(sn), X_ST, O(ta)                           ; T1=T1*SN-U1*CS
        .byte   X_LD, O(u1), X_MUL, O(cs), X_RSUB, O(ta), X_ST, O(t1)
        .byte   X_LD, O(t2), X_MUL, O(sn), X_ST, O(ta)                           ; T2=T2*SN-U2*CS
        .byte   X_LD, O(u2), X_MUL, O(cs), X_RSUB, O(ta), X_ST, O(t2)
        .byte   X_LD, O(t1), X_RSUB, O(k50), X_ASR, 9, X_ST, O(la)               ; LA=(50-T1)/2
        .byte   X_LD, O(z1), X_RSUB, O(k50), X_ASR, YSH, X_ST, O(lb)             ; LB=(50-Z1)/2, VIC 50-Z1
        .byte   X_LD, O(t2), X_RSUB, O(k50), X_ASR, 9, X_ST, O(lc)
        .byte   X_LD, O(z2), X_RSUB, O(k50), X_ASR, YSH, X_ST, O(ld)
        .byte   X_END
        jsr     covered         ; (492) before the clipping changes LA..LD
        php
        lda     #54
        ldx     #49
        jsr     setview
        lda     #C_WALL
        jsr     clipline
        plp
        bcc     :+
        jsr     player
:       ; view 3 (530-630)
        lda     #107
        ldx     #50
        jsr     setview
        ldx     #O(z1)          ; Z1>0 OR Z2>0, else nothing
        jsr     ispos
        bcs     @v3
        ldx     #O(z2)
        jsr     ispos
        bcs     @v3
        rts
@v3:    ldx     #O(z1)          ; the intersections only if one of them is <= 0
        jsr     ispos
        bcc     @near
        ldx     #O(z2)
        jsr     ispos
        bcc     @near
        jmp     @proj
@near:  ldx     #O(km4)         ; with (-.0001,.0001)-(-20,5) -> I1,J1
        ldy     #O(e3)
        jsr     cp4
        ldx     #O(km20)
        ldy     #O(e4)
        jsr     cp4
        jsr     isect
        ldx     #O(ix)
        ldy     #O(i1)
        jsr     cp8
        ldx     #O(kp4)         ; with (.0001,.0001)-(20,5) -> I2,J2
        ldy     #O(e3)
        jsr     cp4
        ldx     #O(k20)
        ldy     #O(e4)
        jsr     cp4
        jsr     isect
        ldx     #O(ix)
        ldy     #O(i2)
        jsr     cp8
        ldx     #O(z1)          ; Z1<=0: IF J1>0 THEN T1=I1:Z1=J1 ELSE T1=I2:Z1=J2
        jsr     ispos
        bcs     :+
        jsr     choose
        ldy     #O(t1)
        jsr     cp4
        inx
        inx
        inx
        inx
        ldy     #O(z1)
        jsr     cp4
:       ldx     #O(z2)          ; the same for T2,Z2 (with J1, as in the original)
        jsr     ispos
        bcs     @proj
        jsr     choose
        ldy     #O(t2)
        jsr     cp4
        inx
        inx
        inx
        inx
        ldy     #O(z2)
        jsr     cp4
@proj:  jsr     xs              ; X1=-T1*16/Z1:YA=-50/Z1:YB=50/Z1 in 24.8, the same for 2
        .byte   X_LD, O(t1), X_SHL, 4, X_NEG, X_MD, O(k256), O(z1), X_ST, O(x1)
        .byte   X_LD, O(km50), X_MD, O(k256), O(z1), X_ST, O(ya)
        .byte   X_LD, O(k50), X_MD, O(k256), O(z1), X_ST, O(yb)
        .byte   X_LD, O(t2), X_SHL, 4, X_NEG, X_MD, O(k256), O(z2), X_ST, O(x2)
        .byte   X_LD, O(km50), X_MD, O(k256), O(z2), X_ST, O(yc)
        .byte   X_LD, O(k50), X_MD, O(k256), O(z2), X_ST, O(yd)
        ; (50+X)/2, 50+Y (VDC: (50+Y)/2) in place
        .byte   X_LD, O(x1), X_ADD, O(k50f8), X_ASR, 1, X_ST, O(x1)
        .byte   X_LD, O(x2), X_ADD, O(k50f8), X_ASR, 1, X_ST, O(x2)
        .byte   X_LD, O(ya), X_ADD, O(k50f8), X_ASR, YSH-8, X_ST, O(ya)
        .byte   X_LD, O(yb), X_ADD, O(k50f8), X_ASR, YSH-8, X_ST, O(yb)
        .byte   X_LD, O(yc), X_ADD, O(k50f8), X_ASR, YSH-8, X_ST, O(yc)
        .byte   X_LD, O(yd), X_ADD, O(k50f8), X_ASR, YSH-8, X_ST, O(yd)
        .byte   X_END
        ldx     #0              ; 600-630: the four lines
@l:     stx     li
        lda     lntab,x
        tax
        ldy     #O(la)
        jsr     cp4
        ldx     li
        lda     lntab+1,x
        tax
        ldy     #O(lb)
        jsr     cp4
        ldx     li
        lda     lntab+2,x
        tax
        ldy     #O(lc)
        jsr     cp4
        ldx     li
        lda     lntab+3,x
        tax
        ldy     #O(ld)
        jsr     cp4
        ldx     li
        lda     lntab+4,x
        jsr     clipline
        lda     li
        clc
        adc     #5
        tax
        cpx     #20
        bne     @l
        rts
lntab:  .byte   O(x1), O(ya), O(x2), O(yc), C_V3H       ; top
        .byte   O(x1), O(yb), O(x2), O(yd), C_V3H       ; bottom
        .byte   O(x1), O(ya), O(x1), O(yb), C_V3S       ; left
        .byte   O(x2), O(yc), O(x2), O(yd), C_V3S       ; right

; choose: X = O(i1) if J1 > 0, else O(i2)
choose: ldx     #O(j1)
        jsr     ispos
        ldx     #O(i1)
        bcs     :+
        ldx     #O(i2)
:       rts

; isect: 4000-4060 with E1=T1 F1=Z1 E2=T2 F2=Z2, e3, F3=.0001, e4, F4=5 -> ix, iy
isect:  jsr     xs
        .byte   X_LD, O(t1), X_MUL, O(z2), X_ST, O(ta)                           ; IX=E1*F2-F1*E2
        .byte   X_LD, O(z1), X_MUL, O(t2), X_RSUB, O(ta), X_ST, O(ix)
        .byte   X_LD, O(e3), X_MUL, O(k5), X_ST, O(ta)                           ; IY=E3*F4-F3*E4
        .byte   X_LD, O(kp4), X_MUL, O(e4), X_RSUB, O(ta), X_ST, O(iy)
        .byte   X_LD, O(kp4), X_SUB, O(k5), X_ST, O(tb)                          ; DT=(E1-E2)*(F3-F4)
        .byte   X_LD, O(t1), X_SUB, O(t2), X_MUL, O(tb), X_ST, O(ta)             ;   -(F1-F2)*(E3-E4)
        .byte   X_LD, O(e3), X_SUB, O(e4), X_ST, O(tb)
        .byte   X_LD, O(z1), X_SUB, O(z2), X_MUL, O(tb), X_RSUB, O(ta), X_ST, O(dt)
        .byte   X_LD, O(e3), X_SUB, O(e4), X_MUL, O(ix), X_ST, O(ta)             ; IX=(IX*(E3-E4)
        .byte   X_LD, O(t1), X_SUB, O(t2), X_MUL, O(iy), X_RSUB, O(ta)           ;   -(E1-E2)*IY)/DT
        .byte   X_DIV, O(dt), X_ST, O(ix)
        .byte   X_LD, O(kp4), X_SUB, O(k5), X_MUL, O(ix), X_ST, O(ta)            ; IY=(IX*(F3-F4)
        .byte   X_LD, O(z1), X_SUB, O(z2), X_MUL, O(iy), X_RSUB, O(ta)           ;   -(F1-F2)*IY)/DT
        .byte   X_DIV, O(dt), X_ST, O(iy)
        .byte   X_END
        rts

; setview: OX = A, VW = X (whole numbers); OY = oyb and VH = vh8 stay
setview:
        sta     vox
        stx     vw8+1
        lda     #0
        sta     vw8
        sta     vw8+2
        sta     vw8+3
        rts

; covered (492, 24.8, before the clipping): carry clear if the wall line lies away from the player
covered:
        ldx     #O(la)
        ldy     #O(k26f8)
        jsr     ge32
        bcc     @c2
        ldx     #O(lc)
        jsr     ge32
        bcs     @away
@c2:    ldx     #O(la)
        ldy     #O(k24f8)
        jsr     le32
        bcc     @c3
        ldx     #O(lc)
        jsr     le32
        bcs     @away
@c3:    ldx     #O(lb)
        ldy     #O(ky1f8)
        jsr     ge32
        bcc     @c4
        ldx     #O(ld)
        jsr     ge32
        bcs     @away
@c4:    ldx     #O(lb)
        ldy     #O(ky0f8)
        jsr     le32
        bcc     @near
        ldx     #O(ld)
        jsr     le32
        bcs     @away
@near:  sec
        rts
@away:  clc
        rts

;--------------------------------------------------------------------------------
; clipline: Cohen-Sutherland on la lb lc ld (24.8) in the view vox/oyb with vw8, vh8; colour A
clipline:
        sta     lcol
@loop:  ldx     #O(la)
        jsr     code
        sta     ca
        ldx     #O(lc)
        jsr     code
        sta     cb
        ora     ca
        bne     :+
        jmp     @draw
:       lda     ca
        and     cb
        beq     :+
        rts                     ; outside
:       lda     ca
        bne     :+
        lda     cb
:       sta     cc
        and     #12
        beq     @xside
        ldx     #O(zero)        ; K = VH (bit 8) or 0 (bit 4)
        lda     cc
        and     #8
        beq     :+
        ldx     #O(vh8)
:       ldy     #O(tk)
        jsr     cp4
        jsr     xs              ; U = LA+(LC-LA)*(K-LB)/(LD-LB), V = K
        .byte   X_LD, O(tk), X_SUB, O(lb), X_ST, O(tb)
        .byte   X_LD, O(ld), X_SUB, O(lb), X_ST, O(tc)
        .byte   X_LD, O(lc), X_SUB, O(la), X_MD, O(tb), O(tc), X_ADD, O(la), X_ST, O(cu)
        .byte   X_LD, O(tk), X_ST, O(cv)
        .byte   X_END
        jmp     @as
@xside: ldx     #O(zero)        ; K = VW (bit 2) or 0 (bit 1)
        lda     cc
        and     #2
        beq     :+
        ldx     #O(vw8)
:       ldy     #O(tk)
        jsr     cp4
        jsr     xs              ; V = LB+(LD-LB)*(K-LA)/(LC-LA), U = K
        .byte   X_LD, O(tk), X_SUB, O(la), X_ST, O(tb)
        .byte   X_LD, O(lc), X_SUB, O(la), X_ST, O(tc)
        .byte   X_LD, O(ld), X_SUB, O(lb), X_MD, O(tb), O(tc), X_ADD, O(lb), X_ST, O(cv)
        .byte   X_LD, O(tk), X_ST, O(cu)
        .byte   X_END
@as:    ldy     #O(la)          ; (U,V) replaces the end point outside
        lda     cc
        cmp     ca
        beq     :+
        ldy     #O(lc)
:       ldx     #O(cu)
        jsr     cp8
        jmp     @loop
@draw:  ldx     #O(la)          ; OX/OY + floor(L+.5): the byte 1 of L+128 (0..255 inside the view)
        jsr     rnd
        clc
        adc     vox
        sta     lx1
        ldx     #O(lb)
        jsr     rnd
        clc
        adc     oyb
        sta     ly1
        ldx     #O(lc)
        jsr     rnd
        clc
        adc     vox
        sta     lx2
        ldx     #O(ld)
        jsr     rnd
        clc
        adc     oyb
        sta     ly2
        jmp     dline
rnd:    clc
        lda     fv,x
        adc     #128
        lda     fv+1,x
        adc     #0
        rts

; code: outcode of the point at offset X (x) and X+4 (y): 1 x<0, 2 x>VW, 4 y<0, 8 y>VH
code:   lda     #0
        sta     oc
        lda     fv+3,x
        bpl     :+
        lda     #1              ; x < 0
        bne     @y0
:       ldy     #O(vw8)
        jsr     gt32
        lda     #0
        bcc     @y0
        lda     #2
@y0:    sta     oc
        inx
        inx
        inx
        inx
        lda     fv+3,x
        bpl     :+
        lda     #4
        ora     oc
        rts
:       ldy     #O(vh8)
        jsr     gt32
        lda     oc
        bcc     :+
        ora     #8
:       rts

;--------------------------------------------------------------------------------
; 32 bit helpers on offsets in FV
cp4:    lda     fv,x            ; v(Y) = v(X), X and Y stay
        sta     fv,y
        lda     fv+1,x
        sta     fv+1,y
        lda     fv+2,x
        sta     fv+2,y
        lda     fv+3,x
        sta     fv+3,y
        rts
cp8:    jsr     cp4             ; 8 bytes
        txa
        clc
        adc     #4
        tax
        tya
        adc     #4
        tay
        jsr     cp4
        rts
ispos:  lda     fv+3,x          ; carry set if v(X) > 0
        bmi     @no
        ora     fv+2,x
        ora     fv+1,x
        ora     fv,x
        beq     @no
        sec
        rts
@no:    clc
        rts
gt32:   jsr     cmp32           ; carry set if v(X) > v(Y) (signed)
        bmi     :+
        beq     :+
        sec
        rts
:       clc
        rts
ge32:   jsr     cmp32           ; carry set if v(X) >= v(Y)
        bmi     :+
        sec
        rts
:       clc
        rts
le32:   jsr     cmp32           ; carry set if v(X) <= v(Y)
        bmi     :+
        beq     :+
        clc
        rts
:       sec
        rts
cmp32:  sec                     ; N/Z as for v(X) - v(Y) (signed; |v| < 2^30 here, no overflow)
        lda     fv,x
        sbc     fv,y
        sta     tq
        lda     fv+1,x
        sbc     fv+1,y
        ora     tq
        sta     tq
        lda     fv+2,x
        sbc     fv+2,y
        ora     tq
        sta     tq
        lda     fv+3,x
        sbc     fv+3,y
        bmi     @r              ; negative: N = 1
        ora     tq
        beq     @r              ; zero: Z = 1
        lda     #1              ; positive: N = 0, Z = 0
@r:     rts

asracc: cpx     #0              ; acc >>= X (arithmetic)
        beq     @e
:       lda     acc+3
        cmp     #$80
        ror     acc+3
        ror     acc+2
        ror     acc+1
        ror     acc
        dex
        bne     :-
@e:     rts

;--------------------------------------------------------------------------------
; xs: runs the script behind the JSR (opcodes X_..., operands offsets in FV), returns behind X_END
xs:     pla
        sta     SRC
        pla
        sta     SRC+1
@next:  jsr     @getb
        bne     :+
        inc     SRC             ; X_END: continue behind it
        bne     @j
        inc     SRC+1
@j:     jmp     (SRC)
:       sta     xop
        cmp     #X_ASR
        bcc     @mem
        beq     @asr
        cmp     #X_SHL
        beq     @shl
        lda     acc             ; X_NEG
        eor     #$FF
        clc
        adc     #1
        sta     acc
        lda     acc+1
        eor     #$FF
        adc     #0
        sta     acc+1
        lda     acc+2
        eor     #$FF
        adc     #0
        sta     acc+2
        lda     acc+3
        eor     #$FF
        adc     #0
        sta     acc+3
        jmp     @next
@asr:   jsr     @getb
        tax
        jsr     asracc
        jmp     @next
@shl:   jsr     @getb
        tax
:       asl     acc
        rol     acc+1
        rol     acc+2
        rol     acc+3
        dex
        bne     :-
        jmp     @next
@mem:   jsr     @getb           ; operand
        tax
        lda     xop
        cmp     #X_ST
        beq     @st
        bcc     @ld
        cmp     #X_SUB
        bcc     @add
        beq     @sub
        cmp     #X_MUL
        bcc     @rsub
        beq     @mul
        cmp     #X_DIV
        beq     @div
        ldy     #O(mb)          ; X_MD: mb = v, mc = w
        jsr     cpxy
        jsr     @getb
        tax
        ldy     #O(mc)
        jsr     cpxy
        jmp     @md
@mul:   ldy     #O(mb)
        jsr     cpxy
        ldx     #O(one)
        ldy     #O(mc)
        jsr     cpxy
        jmp     @md
@div:   ldy     #O(mc)
        jsr     cpxy
        ldx     #O(one)
        ldy     #O(mb)
        jsr     cpxy
@md:    ldx     #O(acc)
        ldy     #O(ma)
        jsr     cpxy
        jsr     muldiv
        jmp     @next
@ld:    ldy     #O(acc)
        jsr     cpxy
        jmp     @next
@st:    txa
        tay
        ldx     #O(acc)
        jsr     cpxy
        jmp     @next
@add:   clc
        ldy     #0
:       lda     acc,y
        adc     fv,x
        sta     acc,y
        inx
        iny
        tya
        eor     #4
        bne     :-              ; (EOR keeps the carry)
        jmp     @next
@sub:   sec
        ldy     #0
:       lda     acc,y
        sbc     fv,x
        sta     acc,y
        inx
        iny
        tya
        eor     #4
        bne     :-
        jmp     @next
@rsub:  sec
        ldy     #0
:       lda     fv,x
        sbc     acc,y
        sta     acc,y
        inx
        iny
        tya
        eor     #4
        bne     :-
        jmp     @next
@getb:  inc     SRC
        bne     :+
        inc     SRC+1
:       ldy     #0
        lda     (SRC),y
        rts
cpxy:   jmp     cp4

;--------------------------------------------------------------------------------
; muldiv: acc = ma*mb/mc (signed 32 bit, 64 bit product, truncated towards zero; mc = 1.0: a shift)
muldiv: lda     ma+3
        eor     mb+3
        eor     mc+3
        sta     msign
        ldx     #O(ma)
        jsr     abs32
        ldx     #O(mb)
        jsr     abs32
        ldx     #O(mc)
        jsr     abs32
        ldx     #7              ; prod = |ma| * |mb|, byte by byte with mul8
        lda     #0
:       sta     prod,x
        dex
        bpl     :-
        lda     #0
        sta     mi
@i:     ldx     mi
        lda     ma,x
        beq     @ni
        lda     #0
        sta     mj
@j:     ldx     mj
        ldy     mb,x
        beq     @nj
        ldx     mi
        lda     ma,x
        tax
        jsr     mul8
        lda     mi              ; add m8 at prod[i+j]
        clc
        adc     mj
        tax
        clc
        lda     prod,x
        adc     m8lo
        sta     prod,x
        lda     prod+1,x
        adc     m8hi
        sta     prod+1,x
        bcc     @nj
:       inx
        lda     prod+1,x
        adc     #0
        sta     prod+1,x
        bcs     :-
@nj:    inc     mj
        lda     mj
        cmp     #4
        bne     @j
@ni:    inc     mi
        lda     mi
        cmp     #4
        bne     @i
        lda     mc+3            ; mc = $00010000: the bytes 2..5
        ora     mc+1
        ora     mc
        bne     @div
        lda     mc+2
        cmp     #1
        bne     @div
        ldx     #3
:       lda     prod+2,x
        sta     acc,x
        dex
        bpl     :-
        jmp     @sign
@div:   ldx     #32             ; quotient of prod / mc; the remainder starts with the high half
@d:     asl     prod            ; (prod, rem) <<= 1, rem = prod+4..7
        rol     prod+1
        rol     prod+2
        rol     prod+3
        rol     prod+4
        rol     prod+5
        rol     prod+6
        rol     prod+7
        bcs     @sub            ; bit 32 set: rem > mc
        lda     prod+7
        cmp     mc+3
        bcc     @no
        bne     @sub
        lda     prod+6
        cmp     mc+2
        bcc     @no
        bne     @sub
        lda     prod+5
        cmp     mc+1
        bcc     @no
        bne     @sub
        lda     prod+4
        cmp     mc
        bcc     @no
@sub:   sec
        lda     prod+4
        sbc     mc
        sta     prod+4
        lda     prod+5
        sbc     mc+1
        sta     prod+5
        lda     prod+6
        sbc     mc+2
        sta     prod+6
        lda     prod+7
        sbc     mc+3
        sta     prod+7
        inc     prod            ; quotient bit
@no:    dex
        bne     @d
        ldx     #3
:       lda     prod,x
        sta     acc,x
        dex
        bpl     :-
@sign:  lda     msign
        bpl     :+
        ldx     #O(acc)
        jmp     neg32
:       rts

abs32:  lda     fv+3,x          ; v(X) = |v(X)|
        bpl     :+
neg32:  sec                     ; v(X) = -v(X)
        lda     #0
        sbc     fv,x
        sta     fv,x
        lda     #0
        sbc     fv+1,x
        sta     fv+1,x
        lda     #0
        sbc     fv+2,x
        sta     fv+2,x
        lda     #0
        sbc     fv+3,x
        sta     fv+3,x
:       rts

; mul8: m8 = X * Y (quarter squares: f(a+b) - f(|a-b|), f(n) = n*n/4)
mul8:   stx     m8a
        sty     m8b
        txa
        clc
        adc     m8b
        tax
        bcs     @h
        lda     sq0lo,x
        sta     m8lo
        lda     sq0hi,x
        sta     m8hi
        jmp     @d
@h:     lda     sq1lo,x
        sta     m8lo
        lda     sq1hi,x
        sta     m8hi
@d:     lda     m8a
        sec
        sbc     m8b
        bcs     :+
        eor     #$FF
        adc     #1
:       tax
        lda     m8lo
        sec
        sbc     sq0lo,x
        sta     m8lo
        lda     m8hi
        sbc     sq0hi,x
        sta     m8hi
        rts

; sqinit: the tables f(n) = n*n/4 for n = 0..511 (n*n kept as 24 bit, adding 2n+1)
sqinit: lda     #0
        sta     sqv
        sta     sqv+1
        sta     sqv+2
        sta     sqn
        sta     sqn+1
@n:     lda     sqv+2           ; f = n*n >> 2
        sta     m8hi
        lda     sqv+1
        sta     m8lo
        lda     sqv
        lsr     m8hi
        ror     m8lo
        ror
        lsr     m8hi
        ror     m8lo
        ror
        ldx     sqn
        ldy     sqn+1
        bne     :+
        sta     sq0lo,x
        lda     m8lo
        sta     sq0hi,x
        jmp     :++
:       sta     sq1lo,x
        lda     m8lo
        sta     sq1hi,x
:       lda     sqn             ; n*n += 2n+1
        asl
        sta     m8a
        lda     sqn+1
        rol
        sta     m8b
        sec
        lda     sqv
        adc     m8a
        sta     sqv
        lda     sqv+1
        adc     m8b
        sta     sqv+1
        lda     sqv+2
        adc     #0
        sta     sqv+2
        inc     sqn
        bne     @n
        inc     sqn+1
        lda     sqn+1
        cmp     #2
        bne     @n
        rts

.include "fixtab.inc"

.segment "FV"
; all 32 bit numbers, in one page; the constants first
fv:
zero:   .dword  0
one:    .dword  $10000
k5:     .dword  5 << 16
k20:    .dword  20 << 16
km20:   .dword  (-(20 << 16)) & $FFFFFFFF
k50:    .dword  50 << 16
km50:   .dword  (-(50 << 16)) & $FFFFFFFF
k70:    .dword  70 << 16
k256:   .dword  256
k50f8:  .dword  50 << 8
kp4:    .dword  7               ; .0001 in 16.16 (fixref.py: round)
km4:    .dword  (-7) & $FFFFFFFF
k26f8:  .dword  26 << 8
k24f8:  .dword  24 << 8
kint1:  .dword  1               ; whole numbers for the time per frame at the end
k30i:   .dword  30
k60i:   .dword  60
k1000i: .dword  1000
.ifdef VIC
ky1f8:  .dword  51 << 8
ky0f8:  .dword  44 << 8
vh8:    .dword  109 << 8
.else
ky1f8:  .dword  26 << 8
ky0f8:  .dword  22 << 8
vh8:    .dword  54 << 8
.endif
px:     .dword  0
py:     .dword  0
sn:     .dword  0
cs:     .dword  0
t1:     .dword  0
u1:     .dword  0
t2:     .dword  0
u2:     .dword  0
z1:     .dword  0
z2:     .dword  0
e3:     .dword  0
e4:     .dword  0
ix:     .dword  0               ; ix iy, i1 j1, i2 j2 follow each other (cp8)
iy:     .dword  0
i1:     .dword  0
j1:     .dword  0
i2:     .dword  0
j2:     .dword  0
dt:     .dword  0
ta:     .dword  0
tb:     .dword  0
tc:     .dword  0
tk:     .dword  0
cu:     .dword  0               ; cu cv follow each other (cp8)
cv:     .dword  0
x1:     .dword  0
x2:     .dword  0
ya:     .dword  0
yb:     .dword  0
yc:     .dword  0
yd:     .dword  0
la:     .dword  0               ; la lb lc ld follow each other (cp8)
lb:     .dword  0
lc:     .dword  0
ld:     .dword  0
vw8:    .dword  0
acc:    .dword  0
ma:     .dword  0
mb:     .dword  0
mc:     .dword  0
prod:   .res    8
fvend:
.assert fvend - fv <= 256, error, "FV does not fit into one page"

.segment "BSS"
sq0lo:  .res    256
sq0hi:  .res    256
sq1lo:  .res    256
sq1hi:  .res    256
ki:     .res    1               ; angle index k + 32
vox:    .res    1
oc:     .res    1
ca:     .res    1
cb:     .res    1
cc:     .res    1
tq:     .res    1
li:     .res    1
xop:    .res    1
msign:  .res    1
mi:     .res    1
mj:     .res    1
m8a:    .res    1
m8b:    .res    1
m8lo:   .res    1
m8hi:   .res    1
sqv:    .res    3
sqn:    .res    2
.segment "CODE"
