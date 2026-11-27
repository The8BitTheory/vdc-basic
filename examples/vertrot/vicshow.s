; vicshow: page flip for the VIC-II versions of vertrot written in BASIC 7.0 (vic.bas).
; BASIC 7.0 draws only into the bitmap at $2000 (GRAPHIC 3, multicolour 160 x 200). The picture on
; display is a second bitmap at $A000 in VIC bank 2 (screen memory, i.e. the colours of the bit pairs
; 01 and 10, at $8C00; colour memory $D800 for bit pair 11 is shared). The views lie in the character
; rows 4-18; this routine copies the 19 pages $2500-$37FF.
;
;   SYS 4867  once, after the borders and the parts that never change have been drawn into $2000:
;             colours of the cells (screen memory $8C00, colour memory), template = $2500-$37FF
;             (to $E000, RAM 0), whole bitmap $2000 -> $A000, VIC bank 2 on.
;   SYS 4864  every frame: wait for the start of the vertical blank, copy $2500-$37FF to $A500 (the
;             new picture on display), then the template back to $2500 (empty views for the next one).
;   SYS 4870  at the end: VIC bank 0 again (GRAPHIC 0 afterwards gives the text screen back).
;
; Colours of the cells (VIC colour numbers), view 1 = columns 0-12, view 2 = 14-25, view 3 = 27-39:
;   01 (screen high nibble): yellow 7 everywhere (the wall)
;   10 (screen low nibble):  dark grey 11 in views 1 and 2 (player), brown 9 in view 3; column 13
;                            blue 6 (right border of view 1), column 26 green 5 (right border of view 2)
;   11 (colour memory):      white 1 in views 1 and 2, cyan 3 in view 3; the border colour in the border
;                            cells: column 0 and rows 4/18 of view 1 blue, column 13 and rows 4/18 of
;                            view 2 green, column 26 cyan
; A white pixel in a border cell therefore shows the border colour, a grey one in column 13/26 blue or
; green: multicolour has 3 colours per 4 x 8 cell.
;
; Build: cl65 -t none -C vicshow.cfg -o vicshow.prg vicshow.s

TMPL    = $E000         ; template, RAM 0
SCREEN  = $8C00         ; screen memory of the picture on display
COLRAM  = $D800
NPAGES  = 19

.segment "LOADADDR"
        .word   $1300

.segment "CODE"
        jmp     show            ; SYS 4864
        jmp     setup           ; SYS 4867
        jmp     done            ; SYS 4870

show:   lda     $D011           ; wait for raster line 251 (start of the vertical blank)
        bmi     show
:       lda     $D012
        cmp     #251
        bne     :-
        lda     $FF00
        pha
        lda     #$3E            ; RAM 0 everywhere, I/O on
        sta     $FF00
        ldx     #0
@a:
.repeat NPAGES, I
        lda     $2500 + I * 256, x
        sta     $A500 + I * 256, x
.endrepeat
        inx
        beq     :+
        jmp     @a
:
@t:
.repeat NPAGES, I
        lda     TMPL + I * 256, x
        sta     $2500 + I * 256, x
.endrepeat
        inx
        beq     :+
        jmp     @t
:       pla
        sta     $FF00
        rts

setup:  lda     $FF00
        pha
        lda     #$3E
        sta     $FF00
        ldx     #0              ; template and the whole bitmap to page B
@c:
.repeat NPAGES, I
        lda     $2500 + I * 256, x
        sta     TMPL + I * 256, x
.endrepeat
.repeat 32, I
        lda     $2000 + I * 256, x
        sta     $A000 + I * 256, x
.endrepeat
        inx
        beq     :+
        jmp     @c
:       lda     #0              ; colours: screen memory, and colour memory in both banks
        sta     cbank           ; (bit 0 of $01 = the bank the CPU sees, bit 1 the one of the VIC,
        sei                     ; which BASIC 7.0 switches for its bitmap modes)
        lda     $01
        sta     port
@bank:  lda     port
        and     #$FE
        ldx     cbank
        beq     :+
        ora     #1
:       sta     $01
        lda     #<SCREEN        ; 25 rows: the same pattern in every row
        sta     $FB
        lda     #>SCREEN
        sta     $FC
        lda     #<COLRAM
        sta     $FD
        lda     #>COLRAM
        sta     $FE
        lda     #0
        sta     row
@r:     ldy     #39
@s:     lda     scr,y
        sta     ($FB),y
        lda     row             ; colour memory: rows 4 and 18 have the border colours
        cmp     #4
        beq     @b
        cmp     #18
        beq     @b
        lda     col,y
        jmp     @w
@b:     lda     colb,y
@w:     sta     ($FD),y
        dey
        bpl     @s
        lda     $FB
        clc
        adc     #40
        sta     $FB
        bcc     :+
        inc     $FC
:       lda     $FD
        clc
        adc     #40
        sta     $FD
        bcc     :+
        inc     $FE
:       inc     row
        lda     row
        cmp     #25
        bne     @r
        inc     cbank
        lda     cbank
        cmp     #2
        bne     @bank
        lda     port
        sta     $01
        cli
        lda     $DD00           ; VIC bank 2 ($8000-$BFFF)
        and     #$FC
        ora     #1
        sta     $DD00
        lda     #$38            ; screen $8C00, bitmap $A000 (shadow of $D018 for the bitmap mode)
        sta     $0A2D
        sta     $D018
        pla
        sta     $FF00
        rts

done:   lda     $DD00           ; VIC bank 0
        ora     #3
        sta     $DD00
        lda     #$78
        sta     $0A2D
        rts

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
row:    .byte   0
cbank:  .byte   0
port:   .byte   0
