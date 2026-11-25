; vdcbench.s -- Messprogramm fuer VDC-Zeiten (Spec docs/v3/P0-bench.md)
; ca65 -t none, laedt bei $2000, Aufruf per BANK15:SYS8192.
; Zeit: CIA2 Timer A/B in us (24 Bit). Ergebnisblock ab $4000 (RAM 0).

.setcpu "6502"
.include "vdcbasic_syms.inc"        ; remember_mem_conf, saved_mem_conf, vmc_execute, vms_execute

arg1 = $83
arg2 = $85
arg3 = $87
arg4 = $89
arg5 = $8B
arg6 = $8D

rp   = $FC                          ; Zeiger in den Ergebnisblock ($FC/$FD)
lp   = $FE                          ; Zeiger fuer Listenbau / LFSR ($FE/$FF)
cnt8 = $FB                          ; Wiederholungszaehler Fall 8-10

DST  = $5000                        ; Messbereich Ziel
SRC  = $7000                        ; Messbereich Quelle
R36A = $9000                        ; Reg-36-Testbereich

.macro vwait
:   bit $D600
    bpl :-
.endmacro
.macro vsel r
    ldx #r
    stx $D600
.endmacro
.macro vput
    vwait
    sta $D601
.endmacro

.segment "LOADADDR"
    .word $2000

.segment "BSS"
vlist:      .res 512                ; $3000: VMS-Liste
srcbuf:     .res 256                ; Literal-Quelle Fall 2
par:        .res 2
cur_case:   .res 1
cur_var:    .res 1
cur_cfg:    .res 1                  ; Konfiguration: Bit 0 Takt, Bit 4-7 Reg 36
cur_clk:    .res 1
cur_r36:    .res 1
cur_reps:   .res 1
cfg_idx:    .res 1
lidx:       .res 1
lstp:       .res 2
t_lo:       .res 1
t_mid:      .res 1
t_hi:       .res 1
acc_lo:     .res 1
acc_mid:    .res 1
acc_hi:     .res 1
rec_cnt:    .res 2
sv_sp:      .res 1
sv_ff00:    .res 1
sv_d011:    .res 1
sv_d030:    .res 1
sv_vdc:     .res 16
off:        .res 2
ccnt:       .res 1
lfsr:       .res 1
errs:       .res 3
r36_r:      .res 1
r36_fi:     .res 1
fcnt:       .res 2
lastj:      .res 1

.export par, cur_cfg

.segment "CODE"
.export main, bench_done, r36_wait_enter, xchk_a, xchk_b

start:      jmp main
version:    .byte 1
mode:       .byte 0                 ; $2004
r36_flag:   .byte 0                 ; $2005
r36_frames: .word 0                 ; $2006

; ---------------------------------------------------------------- VDC-Hilfen
vrd:                                ; X = Register -> A
    stx $D600
:   bit $D600
    bpl :-
    lda $D601
    rts
vwr:                                ; X = Register, A = Wert
    stx $D600
:   bit $D600
    bpl :-
    sta $D601
    rts
vready:
    vwait
    rts
set_addr:                           ; A = hi, Y = lo -> Reg 18/19
    ldx #18
    jsr vwr
    tya
    ldx #19
    jsr vwr
    rts
setup_fill:                         ; Reg 24 Bit 7 = 0, Ziel = DST
    ldx #24
    jsr vrd
    and #$7F
    ldx #24
    jsr vwr
    lda #>DST
    ldy #<DST
    jsr set_addr
    jmp vready
setup_copy:                         ; Reg 24 Bit 7 = 1, Ziel = DST
    ldx #24
    jsr vrd
    ora #$80
    ldx #24
    jsr vwr
    lda #>DST
    ldy #<DST
    jsr set_addr
    jmp vready

; ---------------------------------------------------------------- Zeitmessung
t_start:
    lda #$00
    sta $DD0E
    sta $DD0F
    lda #$FF
    sta $DD04
    sta $DD05
    sta $DD06
    sta $DD07
    lda #%01010001
    sta $DD0F
    lda #%00010001
    sta $DD0E
    rts
t_stop:
    lda #$00
    sta $DD0E
    lda $DD06
    eor #$FF
    sta t_hi
    lda $DD05
    eor #$FF
    sta t_mid
    lda $DD04
    eor #$FF
    sta t_lo
    rts
acc_clear:
    lda #0
    sta acc_lo
    sta acc_mid
    sta acc_hi
    rts
t_acc:
    clc
    lda acc_lo
    adc t_lo
    sta acc_lo
    lda acc_mid
    adc t_mid
    sta acc_mid
    lda acc_hi
    adc t_hi
    sta acc_hi
    rts
acc_to_t:
    lda acc_lo
    sta t_lo
    lda acc_mid
    sta t_mid
    lda acc_hi
    sta t_hi
    rts

; ---------------------------------------------------------------- Fall 0
case0:                              ; leer
    ldy #16
    jsr t_start
:   dey
    bne :-
    jmp t_stop
case12:                             ; leer, Einzelvorgang
    jsr acc_clear
    ldy #16
c12_l: jsr t_start
    jsr t_stop
    jsr t_acc
    vwait
    dey
    bne c12_l
    jmp acc_to_t

; ---------------------------------------------------------------- Fall 1, 2, 3
case1:                              ; stream_const
    lda #>DST
    ldy #<DST
    jsr set_addr
    lda par
    sta c1_n+1
    ldx #31
    stx $D600
    jsr vready
    ldy #16
    jsr t_start
xchk_a:
    lda #$55
c1_r:
c1_n: ldx #0
c1_i: bit $D600
    bpl c1_i
    sta $D601
    dex
    bne c1_i
    dey
    bne c1_r
xchk_b:
    jmp t_stop

case2:                              ; stream_lit
    lda #>DST
    ldy #<DST
    jsr set_addr
    lda par
    sta c2_n+1
    ldx #31
    stx $D600
    jsr vready
    ldy #16
    jsr t_start
c2_r:
    ldx #0
c2_i: bit $D600
    bpl c2_i
    lda srcbuf,x
    sta $D601
    inx
c2_n: cpx #0
    bne c2_i
    dey
    bne c2_r
    jmp t_stop

case3:                              ; new_addr
    lda #>DST
    ldy #<DST
    jsr set_addr
    jsr vready
    ldy #16
    jsr t_start
c3_r:
    vsel 18
    lda #>DST
    vput
    vsel 19
    lda #<DST
    vput
    vsel 31
    lda #$55
    vput
    dey
    bne c3_r
    jmp t_stop

; ---------------------------------------------------------------- Fall 4-7
.macro FILLM name, rmw, l1, issue
.proc name
    jsr setup_fill
.if l1 = 0
    lda par
    sec
    sbc #1
    sta pp+1
.endif
    ldy #16
.if issue
    jsr acc_clear
rep:
    jsr t_start
.else
    jsr t_start
rep:
.endif
.if rmw
    vsel 24
    vwait
    lda $D601
    and #$7F
    vput
.endif
    vsel 31
    lda #$AA
    vput
.if l1 = 0
    vsel 30
pp: lda #0
    vput
.endif
.if issue
    jsr t_stop
    jsr t_acc
    vwait
    dey
    bne rep
    jmp acc_to_t
.else
    vwait
    dey
    bne rep
    jmp t_stop
.endif
.endproc
.endmacro

.macro COPYM name, rmw, issue
.proc name
    jsr setup_copy
    lda par
    sta pp+1
    ldy #16
.if issue
    jsr acc_clear
rep:
    jsr t_start
.else
    jsr t_start
rep:
.endif
.if rmw
    vsel 24
    vwait
    lda $D601
    ora #$80
    vput
.endif
    vsel 32
    lda #>SRC
    vput
    vsel 33
    lda #<SRC
    vput
    vsel 30
pp: lda #0
    vput
.if issue
    jsr t_stop
    jsr t_acc
    vwait
    dey
    bne rep
    jmp acc_to_t
.else
    vwait
    dey
    bne rep
    jmp t_stop
.endif
.endproc
.endmacro

FILLM f4_n,  0, 0, 0
FILLM f4_n1, 0, 1, 0
FILLM f4_r,  1, 0, 0
FILLM f4_r1, 1, 1, 0
FILLM f5,    0, 0, 1
FILLM f5_1,  0, 1, 1
COPYM c6_n,  0, 0
COPYM c6_r,  1, 0
COPYM c7,    0, 1

case4:                              ; fill_done (cur_var: 0 norm, 1 rmw)
    ldx cur_var
    lda par+1
    bne c4_gen
    lda par
    cmp #1
    beq c4_one
c4_gen:
    cpx #0
    bne :+
    jmp f4_n
:   jmp f4_r
c4_one:
    cpx #0
    bne :+
    jmp f4_n1
:   jmp f4_r1
case5:                              ; fill_issue
    lda par+1
    bne c5_gen
    lda par
    cmp #1
    bne c5_gen
    jmp f5_1
c5_gen:
    jmp f5
case6:                              ; copy_done
    ldx cur_var
    bne :+
    jmp c6_n
:   jmp c6_r
case7:
    jmp c7

; ---------------------------------------------------------------- Fall 8-10
lput:                               ; A in die Liste ($FE/$FF), Zeiger weiter
    ldy #0
    sta (lp),y
    inc lp
    bne :+
    inc lp+1
:   rts

list_start:
    lda #<vlist
    sta lp
    lda #>vlist
    sta lp+1
    lda #0
    sta off
    sta off+1
    jmp lput                        ; Alpha 0

build1:                             ; w = par lo, h = par hi: h x Typ 1
    jsr list_start
    lda par+1
    jsr lput
    lda #0
    jsr lput
    lda par+1
    sta ccnt
:   lda #1
    jsr lput
    lda off
    jsr lput
    lda off+1
    jsr lput
    lda par
    jsr lput
    clc
    lda off
    adc #80
    sta off
    bcc :+
    inc off+1
:   dec ccnt
    bne :--
    rts

build8:                             ; n = par lo: n x Typ 8
    jsr list_start
    lda par
    jsr lput
    lda #0
    jsr lput
    lda par
    sta ccnt
:   lda #8
    jsr lput
    lda off
    jsr lput
    lda #0
    jsr lput
    inc off
    dec ccnt
    bne :-
    rts

case8:                              ; vmc
    lda #4
    sta cnt8
    jsr t_start
c8_r:
    jsr remember_mem_conf
    lda #<SRC
    sta arg1
    lda #>SRC
    sta arg1+1
    lda #<DST
    sta arg2
    lda #>DST
    sta arg2+1
    lda par
    sta arg3
    lda #0
    sta arg3+1
    lda par+1
    sta arg4
    lda #0
    sta arg4+1
    lda #80
    sta arg5
    lda #0
    sta arg5+1
    sta arg6
    sta arg6+1
    jsr vmc_execute
    dec cnt8
    bne c8_r
    jmp t_stop

case9:
    jsr build1
    jmp vms_common
case10:
    jsr build8
vms_common:
    lda #4
    sta cnt8
    jsr t_start
cv_r:
    jsr remember_mem_conf
    lda #<SRC
    sta arg1
    lda #>SRC
    sta arg1+1
    lda #<DST
    sta arg2
    lda #>DST
    sta arg2+1
    lda #<vlist
    sta arg3
    lda #>vlist
    sta arg3+1
    jsr vms_execute
    dec cnt8
    bne cv_r
    jmp t_stop

; ---------------------------------------------------------------- Messung + Datensatz
vdc_sync:                           ; auf den Beginn der VDC-Anzeige (Status Bit 5 faellt) synchronisieren:
:   lda $D600                       ; erst warten, bis Bit 5 gesetzt ist (Austastung) ...
    and #$20
    beq :-
:   lda $D600                       ; ... dann bis es faellt
    and #$20
    bne :-
    rts

measure_store:
    jsr vdc_sync                    ; macht die Messungen vom Rasterzustand des VDC unabhaengig
    ldx cur_case
    lda disp_lo,x
    sta ms_j+1
    lda disp_hi,x
    sta ms_j+2
ms_j: jsr $FFFF
    ldx cur_case
    lda reps_tab,x
    sta cur_reps
store_record:                       ; X = Index in rec_id/flag_tab
    lda rp+1
    cmp #$7F
    bcc sr_ok
    lda rp
    cmp #$F0
    bcc sr_ok
    jmp overflow
sr_ok:
    ldy #0
    lda rec_id,x
    sta (rp),y
    iny
    lda cur_var
    asl
    ora cur_cfg
    ora flag_tab,x
    sta (rp),y
    iny
    lda par
    sta (rp),y
    iny
    lda par+1
    sta (rp),y
    iny
    lda cur_reps
    sta (rp),y
    iny
    lda t_lo
    sta (rp),y
    iny
    lda t_mid
    sta (rp),y
    iny
    lda t_hi
    sta (rp),y
    clc
    lda rp
    adc #8
    sta rp
    bcc :+
    inc rp+1
:   inc rec_cnt
    bne :+
    inc rec_cnt+1
:   rts

overflow:
    ldy #0
    lda #$FE
    sta (rp),y
    jmp finish_term

run_list:                           ; lstp = Liste (Worte, Ende = 0)
    lda lstp
    sta rl1+1
    clc
    adc #1
    sta rl2+1
    lda lstp+1
    sta rl1+2
    adc #0
    sta rl2+2
    lda #0
    sta lidx
rl_l:
    ldx lidx
rl1: lda $FFFF,x
    sta par
rl2: lda $FFFF,x
    sta par+1
    ora par
    beq rl_end
    jsr measure_store
    inc lidx
    inc lidx
    jmp rl_l
rl_end:
    rts

run_L:                              ; Konfiguration A (nicht Kurzlauf): alle L = 1..256, sonst M
    lda cfg_idx
    bne rL_m
    lda mode
    and #4
    bne rL_m
    lda #1
    sta par
    lda #0
    sta par+1
rL_l:
    jsr measure_store
    inc par
    bne :+
    inc par+1
:   lda par+1
    beq rL_l
    lda par
    cmp #1
    bne rL_l
    rts
rL_m:
    lda #<lst_M
    sta lstp
    lda #>lst_M
    sta lstp+1
    jmp run_list

.macro RUN idx, var, list
    lda #idx
    sta cur_case
    lda #var
    sta cur_var
    lda #<list
    sta lstp
    lda #>list
    sta lstp+1
    jsr run_list
.endmacro
.macro RUN1 idx
    lda #idx
    sta cur_case
    lda #0
    sta cur_var
    sta par
    sta par+1
    jsr measure_store
.endmacro
.macro RUNL idx, var
    lda #idx
    sta cur_case
    lda #var
    sta cur_var
    jsr run_L
.endmacro

; ---------------------------------------------------------------- main
main:
    sei
    tsx
    stx sv_sp
    lda $FF00
    sta sv_ff00
    lda $D011
    sta sv_d011
    lda $D030
    sta sv_d030
    lda #$7F
    sta $DD0D                       ; CIA2: alle NMIs aus
    lda #$3E
    sta $FF00
    ; VDC-Register sichern
    ldy #13
:   ldx vdc_save,y
    jsr vrd
    sta sv_vdc,y
    dey
    bpl :-
    ; VIC austasten. Bit 7 loeschen: gelesen ist es Rasterzeile Bit 8, geschrieben Rastervergleich Bit 8.
    ; Bliebe es stehen (Lesen in Zeile >= 256), kaeme der Raster-IRQ des Kernals nie mehr, und der
    ; Reg-36-Test (wartet auf die Jiffy-Uhr $A2) hinge endlos (so in Z64K beobachtet).
    lda $D011
    and #$6F
    sta $D011
    ; Variablen
    lda #0
    sta rec_cnt
    sta rec_cnt+1
    ldx #0
:   txa
    eor #$A5
    sta srcbuf,x
    inx
    bne :-
    ; VDC-Konfiguration (Abschnitt 4.1)
    ldx #0
    lda #127
    jsr vwr
    ldx #4
    lda #159
    jsr vwr
    ldx #6
    lda #120
    jsr vwr
    ldx #7
    lda #133
    jsr vwr
    ldx #9
    lda #1
    jsr vwr
    ldx #25
    jsr vrd
    ora #$80
    ldx #25
    jsr vwr
    ldx #28
    jsr vrd
    ora #$10
    ldx #28
    jsr vwr
    ldx #26
    lda #$FF
    jsr vwr
    ldx #12
    lda #$B4
    jsr vwr
    ldx #13
    lda #$FF
    jsr vwr
    ldx #20
    lda #0
    jsr vwr
    ldx #21
    lda #0
    jsr vwr
    ; Bitmap $B4FF-$FFFF mit $0F fuellen (Block fill)
    ldx #24
    jsr vrd
    and #$7F
    ldx #24
    jsr vwr
    lda #$B4
    ldy #$FF
    jsr set_addr
    vsel 31
    lda #$0F
    vput                            ; 1 Byte, danach 75 x 256
    lda #75
    sta ccnt
bm_l: vsel 31
    lda #$0F
    vput
    vsel 30
    lda #255
    vput
    vwait
    dec ccnt
    bne bm_l
    ; Ergebnisblock-Kopf
    lda #<$4008
    sta rp
    lda #>$4008
    sta rp+1
    lda #0
    sta cfg_idx
cfg_loop:
    ldx cfg_idx
    lda cfg_clk,x
    sta cur_clk
    lda cfg_r36,x
    sta cur_r36
    asl
    asl
    asl
    asl
    ora cur_clk
    sta cur_cfg
    lda $D030
    and #$FE
    ora cur_clk
    sta $D030
    ldx #36
    lda cur_r36
    jsr vwr
    RUN1 0
    RUN1 12
    RUN 1, 0, lst_N
    RUN 2, 0, lst_N
    RUN1 3
    RUNL 4, 0
    RUNL 4, 1
    RUN 5, 0, lst_M
    RUNL 6, 0
    RUNL 6, 1
    RUN 7, 0, lst_M
    RUN 8, 0, lst_WH
    RUN 9, 0, lst_WH
    RUN 10, 0, lst_NN
    inc cfg_idx
    lda mode
    and #4
    bne cfg_done
    lda cfg_idx
    cmp #4
    beq cfg_done
    jmp cfg_loop
cfg_done:
    lda #$01                        ; ab hier 2 MHz
    ora sv_d030
    sta $D030
    lda mode
    and #2
    bne :+
    jsr r36_test
:   jmp finish_term

finish_term:
    ldy #0
    lda (rp),y
    cmp #$FE
    beq :+
    lda #$FF
    sta (rp),y
:   lda #'B'
    sta $4000
    lda #'R'
    sta $4001
    lda #1
    sta $4002
    sta $4003
    lda rec_cnt
    sta $4004
    lda rec_cnt+1
    sta $4005
    lda #0
    sta $4006
    sta $4007
    ldx sv_sp
    txs
    ; Zustand wiederherstellen
    ldy #13
:   ldx vdc_save,y
    lda sv_vdc,y
    jsr vwr
    dey
    bpl :-
    lda sv_d030
    sta $D030
    lda sv_d011
    and #$7F                        ; Rastervergleich Bit 8 = 0 (siehe oben)
    sta $D011
    lda sv_ff00
    sta $FF00
    cli
bench_done:
    rts

; ---------------------------------------------------------------- Reg-36-Test (Fall 11)
r36_test:
    lda #0
    sta r36_r
r36_rl:
    lda #0
    sta r36_fi
r36_fl:
    ldx r36_fi
    lda f_lo,x
    sta fcnt
    lda f_hi,x
    sta fcnt+1
    ; LFSR-Startwert = r*16 + (F AND 15) + 1
    lda r36_r
    asl
    asl
    asl
    asl
    sta lfsr
    lda fcnt
    and #15
    clc
    adc lfsr
    adc #1
    sta lfsr
    sta errs                        ; Startwert merken (errs wird danach geloescht)
    lda #$3E
    sta $FF00
    lda #>R36A
    ldy #<R36A
    jsr set_addr
    vsel 31
    ldy #32
r36_f1:
    ldx #0
r36_f2:
    lda lfsr
    lsr
    bcc :+
    eor #$B8
:   sta lfsr
    bit $D600
    bpl :-
    sta $D601
    dex
    bne r36_f2
    dey
    bne r36_f1
    ; Reg 36 = r
    ldx #36
    lda r36_r
    jsr vwr
    lda fcnt
    sta r36_frames
    lda fcnt+1
    sta r36_frames+1
    lda #0
    sta r36_flag
    sta $FF00
    cli
r36_wait_enter:
    lda mode
    and #1
    bne r36_hw
    lda $A2
    sta lastj
r36_j:
    lda $A2
    cmp lastj
    beq r36_j
    sta lastj
    lda fcnt
    bne :+
    dec fcnt+1
:   dec fcnt
    lda fcnt
    ora fcnt+1
    bne r36_j
    beq r36_end
r36_hw:
    lda r36_flag
    beq r36_hw
r36_end:
    sei
    lda #$3E
    sta $FF00
    ldx #36
    lda #5
    jsr vwr
    ; Startwert zurueck
    lda r36_r
    asl
    asl
    asl
    asl
    sta lfsr
    ldx r36_fi
    lda f_lo,x
    and #15
    clc
    adc lfsr
    adc #1
    sta lfsr
    lda #0
    sta errs
    sta errs+1
    sta errs+2
    lda #>R36A
    ldy #<R36A
    jsr set_addr
    vsel 31
    ldy #32
r36_v1:
    ldx #0
r36_v2:
    lda lfsr
    lsr
    bcc :+
    eor #$B8
:   sta lfsr
:   bit $D600
    bpl :-
    lda $D601
    cmp lfsr
    beq r36_ok
    inc errs
    bne r36_ok
    inc errs+1
    bne r36_ok
    inc errs+2
r36_ok:
    dex
    bne r36_v2
    dey
    bne r36_v1
    ; Datensatz
    lda #11
    sta cur_case
    lda r36_r
    asl
    asl
    asl
    asl
    ora #1
    sta cur_cfg
    lda #0
    sta cur_var
    ldx r36_fi
    lda f_lo,x
    sta par
    lda f_hi,x
    sta par+1
    lda r36_r
    sta cur_reps
    lda errs
    sta t_lo
    lda errs+1
    sta t_mid
    lda errs+2
    sta t_hi
    ldx #11
    jsr store_record
    inc r36_fi
    lda r36_fi
    cmp #3
    beq :+
    jmp r36_fl
:    inc r36_r
    lda r36_r
    cmp #6
    beq :+
    jmp r36_rl
:   rts

; ---------------------------------------------------------------- Tabellen
.segment "RODATA"
vdc_save: .byte 0,4,6,7,9,12,13,20,21,24,25,26,28,36
cfg_clk:  .byte 1,1,0,0
cfg_r36:  .byte 0,5,0,5
f_lo:     .byte <1, <50, <500
f_hi:     .byte >1, >50, >500

disp_lo:  .byte <case0,<case1,<case2,<case3,<case4,<case5,<case6,<case7,<case8,<case9,<case10,<case0,<case12
disp_hi:  .byte >case0,>case1,>case2,>case3,>case4,>case5,>case6,>case7,>case8,>case9,>case10,>case0,>case12
rec_id:   .byte 0,1,2,3,4,5,6,7,8,9,10,11,0
reps_tab: .byte 16,16,16,16,16,16,16,16,4,4,4,0,16
flag_tab: .byte 0,0,0,0,0,4,0,4,0,0,0,0,4

lst_M:    .word 1,2,3,4,5,6,8,10,12,16,24,32,48,64,96,128,192,256,0
lst_N:    .word 1,2,4,8,16,32,64,128,256,0
lst_NN:   .word 1,8,32,128,0
lst_WH:
.repeat 7, i
    .word (1 << i) | (1 << 8)
    .word (1 << i) | (16 << 8)
.endrepeat
    .word 0
