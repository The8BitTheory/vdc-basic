; zvt.s -- VRAM-Pruefhelfer fuer Z64K (test/zvt.py, run_rxv.py / run_draw.py --emu z64k)
;
; Ersetzt die Haltepunkte $1BFE/$1BFF des VICE-Monitor-Harness. Liegt bei $2F00-$38FF (hinter VDC-BASIC,
; das bei $2E67 endet, vor dem BASIC-Text bei $4001), Variablen bei $3900, Ergebnis bei $3A00. Der Treiber ruft per SYS:
;   $2F00 INIT    Blob (Bank 1, $1000) lesen, Zaehler loeschen
;   $2F03 PREP    beim ersten Vektor den ganzen VRAM mit dem Grundmuster laden (danach nie wieder ganz); Vorpruefung:
;                 CRC der Bereiche des Falls muss das Grundmuster sein (sonst Flag 8 und Bereiche neu laden);
;                 naechsten Vektor (Parameter, Strom) in den RAM schreiben; sind alle durch: Ende-Flag = 1
;   $2F06 CHECK   CRC-16 nur ueber die Bereiche des Falls und die Byte-Pruefungen des Vektors vergleichen, die Bereiche
;                 wieder mit dem Grundmuster laden; bei Pruefpunkt-Vektoren dazu CRC ueber den ganzen VRAM (64 KB)
;   $2F09 FIN     Ergebnisdatei RES auf Laufwerk 8 schreiben, Exitcode ueber $D7FF (0 = alles gleich, 10 = Abweichung)
;   $2F0C FINERR  wie FIN, aber Exitcode 2 (BASIC-Fehler; Fehlernummer vorher nach $3A04 POKEn)
;
; Blob (Bank 1 ab $1000, BASIC-Variablen liegen darunter; little endian):
;   Kopf:    nrec u16, pdst u16 (Ziel der Parameter), flagad u16 (Ende-Flag), diag u8 (Maske: 1 = Dump bei Abweichung im
;            Bereich, 2 = bei Abweichung der Gesamt-CRC), maxdiag u8, fullcrc u16 (Gesamt-CRC des Grundmusters),
;            injidx u16 ($FFFF = aus), injaddr u16 (Testhaken: im CHECK dieses Vektors VRAM[injaddr] ^= $55)
;   Vektor:  plen u8, Parameter[plen], slen u16, src u16, Strom[slen], crc u16 (Soll nach dem Fall, ueber die Bereiche),
;            precrc u16 (Soll vor dem Fall = Grundmuster), full u8, nr u8 (hoechstens 16),
;            nr x (start u16, ende u16 einschliesslich), nchk u8, nchk x (addr u16, wert u8)
; Muster: VRAM[256*p + y] = T1[y] xor T2[p] (Tabellen aus test/zvt.py, TABLES: crctl crcth t1 t2).
; Ergebnis ($3A00, wird als RES gespeichert): nfail u16, ndone u16, berr u8, 3 frei, dann je Abweichung
;   8 Byte: Vektornummer u16, Flags u8 (1 = CRC der Bereiche, 2 = Byte-Pruefung, 4 = Gesamt-CRC, 8 = Vorpruefung),
;   CRC-Ist u16, Adresse u16 (Pruefung), Ist u8
; Diagnose (diag <> 0): bei den ersten maxdiag Abweichungen wird der VRAM in vier Dateien Dnnnnk (nnnn = Nr. hex,
;   k = 0-3, je 16 KB, $8000-$BFFF als Puffer) auf Laufwerk 8 geschrieben (Flag 1/2: vor dem Wiederherstellen,
;   Flag 4: danach, zeigt den Schreibzugriff ausserhalb).
; Z64K: Ready-Bit ($D600 Bit 7) vor jedem $D601-Zugriff und jedem Datenwert nach einer Registerwahl.

        .setcpu "6502"

VDCA    = $D600
VDCD    = $D601
FETCH   = $FF74
SETBNK  = $FF68
SETLFS  = $FFBA
SETNAM  = $FFBD
SAVE    = $FFD8
RES     = $3A00

crc     = $FB           ; Null-Seite (C128: $FB-$FE frei)
dst     = $FB
ptr     = $FD

        .segment "LOADADDR"
        .addr $2F00

        .segment "CODE"
        jmp init
        jmp prep
        jmp check
        jmp fin
        jmp finerr

; ---------------------------------------------------------------- Blob lesen
getb:   lda rp
        sta ptr
        lda rp+1
        sta ptr+1
        lda #ptr
        ldx #1
        ldy #0
        jsr FETCH
        inc rp
        bne :+
        inc rp+1
:       rts

get16:  jsr getb
        sta tmp
        jsr getb
        tax
        lda tmp
        rts

; cnt Bytes aus dem Blob nach (dst)
blobcopy:
:       lda cnt
        ora cnt+1
        beq :+
        jsr getb
        ldy #0
        sta (dst),y
        inc dst
        bne @1
        inc dst+1
@1:     lda cnt
        bne @2
        dec cnt+1
@2:     dec cnt
        jmp :-
:       rts

; ---------------------------------------------------------------- VDC
wrreg:  stx VDCA
:       bit VDCA
        bpl :-
        sta VDCD
        rts

; VDC-Adresse A:0 setzen (A = hohes Byte), Register 31 waehlen
setaddr:
        ldx #18
        jsr wrreg
        lda #0
        ldx #19
        jsr wrreg
        ldx #31
        stx VDCA
:       bit VDCA
        bpl :-
        rts

; ---------------------------------------------------------------- INIT
init:   lda #0
        sta rp
        sta idx
        sta idx+1
        sta nfail
        sta nfail+1
        sta ndiag
        sta RES+4
        lda #$10
        sta rp+1
        jsr get16
        sta nrec
        stx nrec+1
        jsr get16
        sta pdst
        stx pdst+1
        jsr get16
        sta flagad
        stx flagad+1
        jsr getb
        sta diag
        jsr getb
        sta maxdiag
        jsr get16
        sta fullcrc
        stx fullcrc+1
        jsr get16
        sta injidx
        stx injidx+1
        jsr get16
        sta injaddr
        stx injaddr+1
        rts

; ---------------------------------------------------------------- PREP
prep:   lda flagad
        sta ptr
        lda flagad+1
        sta ptr+1
        lda idx
        cmp nrec
        lda idx+1
        sbc nrec+1
        bcc @have
        ldy #0
        lda #1
        sta (ptr),y
        rts
@have:  ldy #0
        tya
        sta (ptr),y
        lda idx                 ; erster Vektor: ganzen VRAM laden (danach nur noch die Bereiche der Faelle)
        ora idx+1
        bne @nf
        jsr restore
@nf:
        jsr getb
        sta cnt
        lda #0
        sta cnt+1
        lda pdst
        sta dst
        lda pdst+1
        sta dst+1
        jsr blobcopy            ; Parameter
        jsr get16
        sta cnt
        stx cnt+1
        jsr get16
        sta dst
        stx dst+1
        jsr blobcopy            ; Strom
        jsr get16
        sta expcrc
        stx expcrc+1
        jsr get16
        sta precrc
        stx precrc+1
        jsr getb
        sta full
        jsr getb
        asl
        asl
        sta nr4
        sta cnt
        lda #0
        sta cnt+1
        lda #<rng
        sta dst
        lda #>rng
        sta dst+1
        jsr blobcopy            ; Bereiche (start u16, ende u16 einschliesslich)
        jsr getb
        sta nchk3
        asl
        clc
        adc nchk3
        sta nchk3
        sta cnt
        lda #0
        sta cnt+1
        lda #<chkb
        sta dst
        lda #>chkb
        sta dst+1
        jsr blobcopy            ; Pruefungen
        lda idx
        sta cur
        lda idx+1
        sta cur+1
        inc idx
        bne :+
        inc idx+1
:       lda #0                  ; Vorpruefung: sind die Bereiche des Falls noch Grundmuster? (faengt Schreibzugriffe
        sta prefail             ; frueherer Faelle ab, die sonst durch das Wiederherstellen verschwaenden)
        jsr crcranges
        lda crc
        cmp precrc
        bne @pb
        lda crc+1
        cmp precrc+1
        beq @pok
@pb:    lda #8
        sta prefail
        jsr restoreranges
@pok:   rts

; VRAM = T1[y] xor T2[p]
restore:
        php
        sei
        lda #0
        jsr setaddr
        ldx #0
@pg:    lda t2,x
        sta crc
        ldy #0
@by:    lda t1,y
        eor crc
:       bit VDCA
        bpl :-
        sta VDCD
        iny
        bne @by
        inx
        bne @pg
        plp
        rts

; ---------------------------------------------------------------- CHECK
check:  jsr inject
        jsr crcranges
        lda crc
        sta gotcrc
        lda crc+1
        sta gotcrc+1
        lda prefail
        sta flags
        lda gotcrc
        cmp expcrc
        bne @bad
        lda gotcrc+1
        cmp expcrc+1
        beq @crcok
@bad:   lda flags
        ora #1
        sta flags
@crcok: ldx #0
@mc:    cpx nchk3
        beq @mce
        lda chkb,x
        sta ptr
        lda chkb+1,x
        sta ptr+1
        ldy #0
        lda (ptr),y
        cmp chkb+2,x
        beq @next
        sta actv
        lda ptr
        sta actad
        lda ptr+1
        sta actad+1
        lda flags
        ora #2
        sta flags
@next:  inx
        inx
        inx
        bne @mc
@mce:   lda #0
        sta dumped
        lda flags
        and #3
        beq @nd1
        lda diag
        and #1
        beq @nd1
        jsr dodump              ; Abweichung im Bereich: VRAM vor dem Wiederherstellen sichern
@nd1:   jsr restoreranges
        lda full
        beq @nofull
        jsr crcfull             ; Pruefpunkt: ganzer VRAM muss wieder das Grundmuster sein
        lda crc
        cmp fullcrc
        bne @fb
        lda crc+1
        cmp fullcrc+1
        beq @nofull
@fb:    lda flags
        ora #4
        sta flags
        lda dumped
        bne @nofull
        lda diag
        and #2
        beq @nofull
        jsr dodump
@nofull:
        lda flags
        beq @ret
        inc nfail
        bne :+
        inc nfail+1
:       lda nfail+1
        bne @nostore
        lda nfail
        cmp #129
        bcs @nostore
        sec
        sbc #1
        sta tmp
        lda #0
        sta ptr+1
        lda tmp
        asl
        rol ptr+1
        asl
        rol ptr+1
        asl
        rol ptr+1
        clc
        adc #<(RES+8)
        sta ptr
        lda ptr+1
        adc #>(RES+8)
        sta ptr+1
        ldy #0
        lda cur
        sta (ptr),y
        iny
        lda cur+1
        sta (ptr),y
        iny
        lda flags
        sta (ptr),y
        iny
        lda gotcrc
        sta (ptr),y
        iny
        lda gotcrc+1
        sta (ptr),y
        iny
        lda actad
        sta (ptr),y
        iny
        lda actad+1
        sta (ptr),y
        iny
        lda actv
        sta (ptr),y
@nostore:
@ret:   rts

dodump: lda ndiag
        cmp maxdiag
        bcs @r
        inc ndiag
        inc dumped
        jsr dump
@r:     rts

; ---------------------------------------------------------------- Bereiche (rng, nr4 Byte)
; VDC-Adresse auf ra setzen, Register 31 waehlen
vsetra: lda ra+1
        ldx #18
        jsr wrreg
        lda ra
        ldx #19
        jsr wrreg
        ldx #31
        stx VDCA
:       bit VDCA
        bpl :-
        rts

; naechsten Bereich nach ra/re laden (ri += 4) und VDC-Adresse setzen
nextrng:
        ldx ri
        lda rng,x
        sta ra
        lda rng+1,x
        sta ra+1
        lda rng+2,x
        sta re
        lda rng+3,x
        sta re+1
        txa
        clc
        adc #4
        sta ri
        jmp vsetra

; Z = 0, wenn die Bereiche nicht genau $0000-$FFFF sind (nr4 = 4, start 0, ende $FFFF)
isall:  lda nr4
        cmp #4
        bne @no
        lda rng
        ora rng+1
        bne @no
        lda rng+2
        and rng+3
        cmp #$FF
        rts
@no:    lda #1                  ; Z = 0
        rts

; CRC-16 ueber alle Bereiche nacheinander -> crc
crcranges:
        jsr isall
        bne @ranges
        jmp crcfull             ; ein Bereich $0000-$FFFF: schnelle Schleife
@ranges:
        php
        sei
        lda #$FF
        sta crc
        sta crc+1
        lda #0
        sta ri
@next:  lda ri
        cmp nr4
        beq @done
        jsr nextrng
@lp:
:       bit VDCA
        bpl :-
        lda VDCD
        eor crc+1
        tax
        lda crc
        eor crcth,x
        sta crc+1
        lda crctl,x
        sta crc
        lda ra
        cmp re
        bne @n
        lda ra+1
        cmp re+1
        beq @next
@n:     inc ra
        bne @lp
        inc ra+1
        jmp @lp
@done:  plp
        rts

; Bereiche mit dem Grundmuster T1[y] xor T2[p] ueberschreiben
restoreranges:
        jsr isall
        bne @ranges
        jmp restore             ; ein Bereich $0000-$FFFF: schnelle Schleife
@ranges:
        php
        sei
        lda #0
        sta ri
@next:  lda ri
        cmp nr4
        beq @done
        jsr nextrng
@lp:    ldx ra
        lda t1,x
        ldx ra+1
        eor t2,x
:       bit VDCA                ; Z64K: Ready-Bit vor jedem $D601-Zugriff
        bpl :-
        sta VDCD
        lda ra
        cmp re
        bne @n
        lda ra+1
        cmp re+1
        beq @next
@n:     inc ra
        bne @lp
        inc ra+1
        jmp @lp
@done:  plp
        rts

; Testhaken (nur wenn zvt.py einen Fehler einschleusen soll, injidx = $FFFF sonst): VDC-Byte injaddr ^= $55
inject: lda cur
        cmp injidx
        bne @r
        lda cur+1
        cmp injidx+1
        bne @r
        php
        sei
        lda injaddr
        sta ra
        lda injaddr+1
        sta ra+1
        jsr vsetra
:       bit VDCA
        bpl :-
        lda VDCD
        eor #$55
        pha
        jsr vsetra
:       bit VDCA
        bpl :-
        pla
        sta VDCD
        plp
@r:     rts

; CRC-16 ueber den ganzen VRAM (64 KB) -> crc
crcfull:
        php
        sei
        lda #0
        jsr setaddr
        lda #$FF
        sta crc
        sta crc+1
        lda #0
        sta pgc
@pg:    ldy #0
@by:
:       bit VDCA
        bpl :-
        lda VDCD
        eor crc+1
        tax
        lda crc
        eor crcth,x
        sta crc+1
        lda crctl,x
        sta crc
        iny
        bne @by
        dec pgc
        bne @pg
        plp
        rts

; ---------------------------------------------------------------- Diagnose: VRAM in vier Dateien
dump:   lda #0
        sta dk
@lp:    php
        sei
        lda dk
        asl
        asl
        asl
        asl
        asl
        asl
        jsr setaddr
        lda #0
        sta ptr
        lda #$80
        sta ptr+1
        ldx #$40
@pg:    ldy #0
@by:
:       bit VDCA                ; Z64K: Ready-Bit vor jedem $D601-Zugriff
        bpl :-
        lda VDCD
        sta (ptr),y
        iny
        bne @by
        inc ptr+1
        dex
        bne @pg
        plp
        lda #'D'
        sta fname
        lda cur+1
        jsr hex2
        sta fname+1
        stx fname+2
        lda cur
        jsr hex2
        sta fname+3
        stx fname+4
        lda dk
        clc
        adc #'0'
        sta fname+5
        lda #6
        sta fnlen
        lda #<fname
        sta nameptr
        lda #>fname
        sta nameptr+1
        lda #$00
        sta sstart
        lda #$80
        sta sstart+1
        lda #$00
        sta send
        lda #$C0
        sta send+1
        jsr dosave
        inc dk
        lda dk
        cmp #4
        beq :+
        jmp @lp                 ; zu weit fuer bne
:       rts

; A -> A = hohe, X = niedrige Hexziffer (ASCII)
hex2:   pha
        lsr
        lsr
        lsr
        lsr
        jsr hex1
        tax
        pla
        and #15
        jsr hex1
        pha
        txa
        tay
        pla
        tax
        tya
        rts
hex1:   cmp #10
        bcc :+
        adc #6
:       adc #'0'
        rts

; ---------------------------------------------------------------- Dateien
dosave: lda #0
        ldx #0
        jsr SETBNK
        lda #1
        ldx #8
        ldy #1
        jsr SETLFS
        lda fnlen
        ldx nameptr
        ldy nameptr+1
        jsr SETNAM
        lda sstart
        sta ptr
        lda sstart+1
        sta ptr+1
        lda #ptr
        ldx send
        ldy send+1
        jmp SAVE

savres: lda nfail
        sta RES
        lda nfail+1
        sta RES+1
        lda idx
        sta RES+2
        lda idx+1
        sta RES+3
        lda #<resname
        sta nameptr
        lda #>resname
        sta nameptr+1
        lda #3
        sta fnlen
        lda #<RES
        sta sstart
        lda #>RES
        sta sstart+1
        ; Ende = RES + 8 + 8 * min(nfail, 128)
        lda nfail+1
        bne @max
        lda nfail
        cmp #129
        bcc @n
@max:   lda #128
@n:     sta tmp
        lda #0
        sta send+1
        lda tmp
        asl
        rol send+1
        asl
        rol send+1
        asl
        rol send+1
        clc
        adc #<(RES+8)
        sta send
        lda send+1
        adc #>(RES+8)
        sta send+1
        jmp dosave

fin:    jsr savres
        lda nfail
        ora nfail+1
        beq :+
        lda #10
        .byte $2C               ; bit abs: ueberspringt lda #0
:       lda #0
        sta $D7FF
        rts

finerr: jsr savres
        lda #2
        sta $D7FF
        rts

resname: .byte "RES"

        .segment "TABLES"
        .align 256
        .include "zvt_tables.inc"

        .segment "VARS"
rp:     .res 2
nrec:   .res 2
idx:    .res 2
cur:    .res 2
pdst:   .res 2
flagad: .res 2
diag:   .res 1
maxdiag: .res 1
ndiag:  .res 1
nfail:  .res 2
cnt:    .res 2
expcrc: .res 2
gotcrc: .res 2
nchk3:  .res 1
flags:  .res 1
actad:  .res 2
actv:   .res 1
pgc:    .res 1
tmp:    .res 1
dk:     .res 1
fnlen:  .res 1
nameptr: .res 2
sstart: .res 2
send:   .res 2
fname:  .res 8
chkb:   .res 24
ra:     .res 2
re:     .res 2
ri:     .res 1
nr4:    .res 1
precrc: .res 2
full:   .res 1
prefail: .res 1
fullcrc: .res 2
injidx: .res 2
injaddr: .res 2
dumped: .res 1
rng:    .res 64
