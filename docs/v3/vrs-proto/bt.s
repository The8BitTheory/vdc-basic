; prototype: fill VRAM with line-id pattern, then busy-wait R25 split like vdcsplit
        .setcpu "6502"
vreg = $d600
vdat = $d601
DELAY_HI = 20          ; patched per run
        .org $1c01
        .word $1c0b, 10
        .byte $9e, "7181", 0
        .word 0
        sei
        lda $d011
        and #$ef
        sta $d011
        lda #0
        sta $d030
        ; R25 = $07 text, no attr ; R26 = $f0 (white on black)
        ldx #26
        lda #$f0
        jsr wr
        ; fill: for A=0..$3fff: byte = (A/80) low, byte1 of chunk = (A/80) high
        ldx #18
        lda #0
        jsr wr
        inx
        jsr wr
        lda #0
        sta cnt
        sta cnt+1
        lda #$40
        sta pages
        lda #0
        sta lid
        sta lid+1
        ldx #31
        stx vreg
        ldy #0          ; byte within chunk
fl:     cpy #0
        bne f1
        lda lid
        jmp fw
f1:     cpy #1
        bne f2
        lda lid+1
        jmp fw
f2:     lda lid
fw:     bit vreg
        bpl fw
        sta vdat
        iny
        cpy #80
        bne nx
        ldy #0
        inc lid
        bne nx
        inc lid+1
nx:     inc cnt
        bne fl
        inc cnt+1
        lda cnt+1
        cmp #$40
        bne fl
restart:
        lda #25
        sta vreg
        lda #$20
w1:     bit vreg
        beq w1
        lda #$87
        sta vdat
        lda #$20
w2:     bit vreg
        bne w2
        ldy #DELAY_HI
d1:     ldx #49
d2:     dex
        bne d2
        dey
        bne d1
        lda #$07
        sta vdat
        jmp restart
wr:     stx vreg
ww:     bit vreg
        bpl ww
        sta vdat
        rts
cnt:   .word 0
pages: .byte 0
lid:   .word 0
