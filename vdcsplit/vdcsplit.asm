 *= $1c01
byt $0c,$1c,$0a,$00,$9e,$20,$37,$31,$38,$32,$00,$00,$00
         sei
         lda $d011
         and #$ef
         sta $d011
         jsr initclockslide
         jsr key1
         lda #$05
         sta delay256+1
         lda #250
         sta testbitdelay

restart
         lda register
         sta $d600

         lda #$20
loop     bit $d600
         beq loop
         inc $d020
         lda prevalue
         sta $d601

         lda #17       ;Prepare lightpen for read
         sta $d600
 ; prepare cia ports
         ldx #$ff
         ldy #$00   
         stx $dc00     ; port A = $ff (inactive)
         sty $dc02     ; ddr A = $00  (all input)
         stx $dc03     ; ddr B = $ff  (all output)
         stx $dc01     ; port B = $ff (inactive)
         lda #$20
loop2    bit $d600
         bne loop2
 ; now trigger the lp latch
         sty $dc01     ; port B = $00 (active)
         stx $dc01     ; port B = $ff (inactive)
         dec $d020

         lda $d601     ; Read lightpen X value
         sta temp      ; and store to use later to help remove VDC jitter
 ; restore cia setup
 
         stx $dc02     ; ddr A = $ff  (all output)
         sty $dc03     ; ddr B = $00  (all input)
         stx $dc01     ; port B = $ff (inactive)
         ldx #$7f
         stx $dc00     ; port A = $7f
         lda register
         sta $d600
         lda #70       ;start remove as much jitter as possible
         sec           
         sbc temp
         ror
         adc #0
         sta temp
         sec
         lda #255
         sbc temp
         sta wait+1
wait     jsr clockslide  ; Most of the jitter removed.

         lda #$00        ; 8
         clc             ; 10
         sbc #0          ; 12
testbitdelay=*-1
         sta jumpdelay+1 ; 16
jumpdelay
         jsr clockslide  ;              14 (when delay = 0)
delay256
         ldy #0
         beq enddelay ;2            (  2)
         ldx #49       ;2            (  4)
         dex           ;2*49=98      (102) 
         bne *-1       ;3*48+2=146   (248)
         bit $ea       ;3            (251)
         dey           ;2            (253)
         jmp delay256+2  ;3          (256)
enddelay

         lda postvalue
         sta $d601

         jsr checkkeyboard
         jmp restart

endtest      lda $d011
         ora #$10
         sta $d011
         cli
         rts
initclockslide
         lda #$c9
         ldx #0
         sta clockslide,x
         inx
         bne *-4
         lda #$c5
         ldx #$ea
         ldy #$60
         sta clockslide+254
         stx clockslide+255
         sty clockslide+256
         rts

checkkeyboard
         lda #$00
         beq keyscanner
         dec checkkeyboard+1
         rts
keyscanner
         lda #$06
         sta checkkeyboard+1
         lda #$ff
         sta $dc02
         lda #$00
         sta $dc03

         ldy #0
         lda #$fe
sclp1
         sta nextmask
         sta $dc00
         ldx $dc01
         cpx #$ff
         bne checkbit       
         iny
         lda #$00
nextmask=*-1
         rol
         bcs sclp1
         rts
checkbit
         txa
         ldx #$08
notfound
         dex
         rol
         bcs notfound
         tya
         asl
         asl
         asl
         asl
         sta rownumber
         txa
         asl
         adc #$00
rownumber=*-1
         sta actionkey
         jmp (jumptable)
actionkey=*-2

;START OF KEYBOARD ACTIONS
keya
         ldx testbitdelay
         bne tb1update
         lda delay256+1
         beq endtb1
         dec delay256+1
tb1update
         dex
         stx testbitdelay
         txa
endtb1   rts

keyd
         ldx testbitdelay
         cpx #$ff
         bne tb2update
         lda delay256+1
         cmp #$d
         beq endtb2
         inc delay256+1
tb2update
         inx
         stx testbitdelay
         txa
endtb2   rts
key1
         lda #$19
         sta $d600
busy
         lda $d600
         bpl busy
         lda #$07
         sta $d601
         lda #$1a
         ldx #$0f
         ldy #$06
         jmp updatevdc
key2
         lda #$19
         ldx #$07
         ldy #$87
         jmp updatevdc
key3
         lda #$19
         ldx #$47
         ldy #$c7
         jmp updatevdc
key4
         lda #$19
         ldx #$87
         ldy #$07
         jmp updatevdc
key5
         lda #$19
         ldx #$c7
         ldy #$47
         jmp updatevdc
keyx     
         pla
         pla
         jmp endtest
         
undefined
         rts
updatevdc
         sta register
         stx prevalue
         sty postvalue
         rts

register byt 0
prevalue byt 0
postvalue byt 0

align $100
jumptable
        ;row0
word undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined
        ;row1
word key3,undefined,keya,key4,undefined,undefined,undefined,undefined
        ;row2
word key5,undefined,keyd,undefined,undefined,undefined,undefined,keyx
        ;row3
word undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined
        ;row4
word undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined
        ;row5
word undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined
        ;row6
word undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined
        ;row7
word key1,undefined,undefined,key2,undefined,undefined,undefined,undefined

temp byt 0
clockslide=(*+$ff)&$ff00        ; jsr clockslide+(255-x) =14+x cycles  257 bytes total