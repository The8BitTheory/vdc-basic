; vdcsys: VDC-BASIC commands as SYS calls, for programs converted by vdc2sys.py, so that Blitz!
; compiles everything (no :: and no interpreter for the extension). The parameters come one by one
; through USR: Q=USR(value) puts the whole number value (fractions cut off, as VDC-BASIC does; negative
; values as 16 bit two's complement) on a stack; the SYS of a command takes them from there.
;
;   SYS 4864  install: USR vector ($1218 JMP) to the parameter routine; checks VDC-BASIC v3
;   SYS 4867  VDL x1,y1,x2,y2,c         the line code of VDC-BASIC (VDL after its parsing, VDLCORE)
;   SYS 4870  VMC source,target,count   block copy (the three parameter form)
;   SYS 4873  VMF vaddr,byte,count      block fill (the three parameter form)
;   SYS 4876  SYN reg,byte[,reg,byte]...  wait for the start of the vertical blank, write with SEI
;   SYS 4879  RGW reg,byte    4882 RGO reg,byte    4885 RGA reg,byte
;   SYS 4888  RGD(reg): the value into RES (PEEK(RES)), RGD(255) by VDC-BASIC's capacity check
;   SYS 4891  RST flags       4894 DISP vaddr      4897 ATTR vaddr
;   SYS 4900  VDM mode,base,stride      VDC-BASIC's VDM after its parsing (VDMCORE)
;   SYS 4903  VDR x,y,w,h,c   as four VDL (the same pixels in one colour; no c = 2)
; VDLCORE, VDMCORE and CAPACITY are addresses inside vdcbasic3.bin (acme -l of src/vdcbasic.asm);
; SYS 4864 checks the version byte and the code there and stops with a BRK if they differ.
;
; Build: cl65 -t none -C vdcsys.cfg -o vdcsys.prg vdcsys.s

VDLCORE = $25DE         ; vdl + 3: ldx #6 (check of the coordinates), then the line
VDMCORE = $25AC         ; vdm + 6: lda arg1+1
CAPACITY = $1D94        ; vdc_check_capacity: ldy #$ff ... A = 0 (16 KiB) or not 0 (64 KiB)
VERSION = $1C03
ARG1    = $83
VDCREG  = $D600
VDCDAT  = $D601
USRVEC  = $1218
SETVDC  = $E1DC
FONT    = $C027
FACEXP  = $63
FACSGN  = $68

.segment "LOADADDR"
        .word   $1300
.segment "CODE"
        jmp     install         ; 4864
        jmp     c_vdl           ; 4867
        jmp     c_vmc           ; 4870
        jmp     c_vmf           ; 4873
        jmp     c_syn           ; 4876
        jmp     c_rgw           ; 4879
        jmp     c_rgo           ; 4882
        jmp     c_rga           ; 4885
        jmp     c_rgd           ; 4888
        jmp     c_rst           ; 4891
        jmp     c_disp          ; 4894
        jmp     c_attr          ; 4897
        jmp     c_vdm           ; 4900
        jmp     c_vdr           ; 4903
res:    .byte   0               ; 4906: result of RGD

install:
        lda     VERSION         ; VDC-BASIC v3 with the expected code?
        cmp     #$30
        bne     @bad
        lda     VDLCORE
        cmp     #$A2
        bne     @bad
        lda     VDLCORE+1
        cmp     #6
        bne     @bad
        lda     VDMCORE
        cmp     #$A5
        bne     @bad
        lda     VDMCORE+1
        cmp     #ARG1+1
        bne     @bad
        lda     CAPACITY
        cmp     #$A0
        bne     @bad
        lda     #$4C            ; JMP param
        sta     USRVEC
        lda     #<param
        sta     USRVEC+1
        lda     #>param
        sta     USRVEC+2
        lda     #0
        sta     np
        rts
@bad:   brk

; param (USR): FAC -> whole number (16 bit) onto the stack; FAC stays as it is
param:  ldx     np
        cpx     #32
        bcs     @full
        lda     #0
        sta     plo,x
        sta     phi,x
        lda     FACEXP
        beq     @done           ; 0
        sec
        sbc     #$80            ; number of bits of the whole part
        bcc     @done           ; |value| < 1 (exponent below $81): 0
        beq     @done
        cmp     #17
        bcs     @big            ; 65536 or more: the low 16 bits (VDC-BASIC would give an error)
        sta     cnt
        lda     FACEXP+1
        sta     phi,x
        lda     FACEXP+2
        sta     plo,x
        lda     #16
        sec
        sbc     cnt
        tay
        beq     @sign
:       lsr     phi,x
        ror     plo,x
        dey
        bne     :-
@sign:  lda     FACSGN
        bpl     @done
        sec                     ; negative: two's complement
        lda     #0
        sbc     plo,x
        sta     plo,x
        lda     #0
        sbc     phi,x
        sta     phi,x
@done:  inc     np
@full:  rts
@big:   cmp     #25             ; 2^16..2^24: the bytes below the top one
        bcs     @done
        sta     cnt
        lda     FACEXP+2
        sta     phi,x
        lda     FACEXP+3
        sta     plo,x
        lda     #24
        sec
        sbc     cnt
        tay
        beq     @sign
:       lsr     FACEXP+1        ; (FAC changes here, only for values >= 65536)
        ror     phi,x
        ror     plo,x
        dey
        bne     :-
        jmp     @sign

; args: the first A parameters to arg1.. (words), stack empty; I/O on
args:   asl
        sta     cnt
        ldx     #0
        ldy     #0
:       cpy     cnt
        beq     :+
        lda     plo,x
        sta     ARG1,y
        lda     phi,x
        sta     ARG1+1,y
        inx
        iny
        iny
        bne     :-
:       lda     #0
        sta     np
ioon:   lda     $FF00
        and     #$FE
        sta     $FF00
        rts

c_vdl:  lda     #5
        jsr     args
        jmp     VDLCORE

c_vdm:  lda     #3
        jsr     args
        jmp     VDMCORE

c_vdr:  lda     #5              ; VDR x,y,w,h,c: top, bottom (h>=2), left and right (h>=3) as VDL
        jsr     args
        ldx     #9
:       lda     ARG1,x
        sta     rx,x
        dex
        bpl     :-
        lda     rw              ; w = 0 or h = 0: nothing
        ora     rw+1
        beq     @rtsn
        lda     rh
        ora     rh+1
        bne     :+
@rtsn:  rts
:
        clc                     ; x2 = x+w-1, y2 = y+h-1
        lda     rx
        adc     rw
        sta     rx2
        lda     rx+1
        adc     rw+1
        sta     rx2+1
        lda     rx2
        bne     :+
        dec     rx2+1
:       dec     rx2
        clc
        lda     ry
        adc     rh
        sta     ry2
        lda     ry+1
        adc     rh+1
        sta     ry2+1
        lda     ry2
        bne     :+
        dec     ry2+1
:       dec     ry2
        ldx     #0              ; top: (x,y)-(x2,y)
        ldy     #O_Y
        jsr     hline
        lda     rh+1            ; h >= 2: bottom (x,y2)-(x2,y2)
        bne     :+
        lda     rh
        cmp     #2
        bcc     @rts
:       ldx     #0
        ldy     #O_Y2
        jsr     hline
        lda     rh+1            ; h >= 3: left (x,y+1)-(x,y2-1), right (x2,...) if w >= 2
        bne     :+
        lda     rh
        cmp     #3
        bcc     @rts
:       ldx     #O_X
        jsr     vline
        lda     rw+1
        bne     :+
        lda     rw
        cmp     #2
        bcc     @rts
:       ldx     #O_X2
        jmp     vline
@rts:   rts
O_X     = 0
O_Y     = 2
O_X2    = rx2 - rx
O_Y2    = ry2 - rx
hline:  lda     rx              ; (x,v)-(x2,v), v at offset Y
        sta     ARG1
        lda     rx+1
        sta     ARG1+1
        lda     rx2
        sta     ARG1+4
        lda     rx2+1
        sta     ARG1+5
        lda     rx,y
        sta     ARG1+2
        sta     ARG1+6
        lda     rx+1,y
        sta     ARG1+3
        sta     ARG1+7
        jmp     vdlc
vline:  lda     rx,x            ; (u,y+1)-(u,y2-1), u at offset X
        sta     ARG1
        sta     ARG1+4
        lda     rx+1,x
        sta     ARG1+1
        sta     ARG1+5
        clc
        lda     ry
        adc     #1
        sta     ARG1+2
        lda     ry+1
        adc     #0
        sta     ARG1+3
        lda     ry2
        sec
        sbc     #1
        sta     ARG1+6
        lda     ry2+1
        sbc     #0
        sta     ARG1+7
vdlc:   lda     rc
        sta     ARG1+8
        lda     rc+1
        sta     ARG1+9
        jmp     VDLCORE

c_vmc:  lda     #3              ; source, target, count
        jsr     args
        ldx     #24             ; block copy
        jsr     rreg
        ora     #$80
        jsr     wreg
        lda     ARG1+3          ; target
        ldx     #18
        jsr     wreg
        lda     ARG1+2
        ldx     #19
        jsr     wreg
        lda     ARG1+1          ; source
        ldx     #32
        jsr     wreg
        lda     ARG1
        ldx     #33
        jsr     wreg
        jmp     blocks

c_vmf:  lda     #3              ; vaddr, byte, count
        jsr     args
        lda     ARG1+4          ; count 0: nothing here
        ora     ARG1+5
        beq     @rts
        ldx     #24             ; block fill
        jsr     rreg
        and     #$7F
        jsr     wreg
        lda     ARG1+1
        ldx     #18
        jsr     wreg
        lda     ARG1
        ldx     #19
        jsr     wreg
        lda     ARG1+2          ; the first byte, the rest by block fill
        ldx     #31
        jsr     wreg
        lda     ARG1+4
        bne     :+
        dec     ARG1+5
:       dec     ARG1+4
        jmp     blocks
@rts:   rts
; blocks: ARG1+4/5 bytes with R30 writes of up to 255
blocks: lda     ARG1+5
        bne     @255
        lda     ARG1+4
        beq     @rts
        cmp     #255
        bcc     :+
@255:   lda     #255
:       sta     cnt
        ldx     #30
        jsr     wreg
        lda     ARG1+4
        sec
        sbc     cnt
        sta     ARG1+4
        bcs     blocks
        dec     ARG1+5
        jmp     blocks
@rts:   rts

c_syn:  lda     np              ; the pairs, then wait and write with interrupts off
        sta     npairs
        lda     #0
        sta     np
        jsr     ioon
:       lda     VDCREG          ; wait until the VDC draws the text window
        and     #$20
        bne     :-
:       lda     VDCREG          ; then until it enters the vertical blank
        and     #$20
        beq     :-
        sei
        ldy     #0
@p:     cpy     npairs
        bcs     @e
        ldx     plo,y
        lda     plo+1,y
        jsr     wreg
        iny
        iny
        bne     @p
@e:     cli
        rts

c_rgw:  jsr     rv              ; X = register, A = value
        jmp     wreg
c_rgo:  jsr     rv
        sta     cnt
        jsr     rreg
        ora     cnt
        jmp     wreg
c_rga:  jsr     rv
        sta     cnt
        jsr     rreg
        and     cnt
        jmp     wreg
rv:     lda     #2
        jsr     args
        ldx     ARG1
        lda     ARG1+2
        rts

c_rgd:  lda     #1
        jsr     args
        ldx     ARG1
        cpx     #255
        bne     :+
        jsr     CAPACITY
        sta     res
        rts
:       jsr     rreg
        sta     res
        rts

c_rst:  lda     #1              ; bit 0: registers as the Kernal sets them, bit 1: character set
        jsr     args
        lsr     ARG1
        bcc     @f
        lda     #37
        sta     VDCREG
        lda     #$FF
        sta     VDCDAT
        ldx     #0
        jsr     SETVDC
        lda     VDCREG
        and     #7
        beq     :+
        ldx     #$3B
        jsr     SETVDC
:       bit     $0A03
        bpl     @f
        ldx     #$3E
        jsr     @pal
@f:     lsr     ARG1
        bcc     :+
        jmp     FONT
:       rts
@pal:   jmp     ($E192)

c_disp: ldx     #12
        bne     ad
c_attr: ldx     #20
ad:     stx     cnt
        lda     #1
        jsr     args
        ldx     cnt
        lda     ARG1+1
        jsr     wreg
        inx
        lda     ARG1
        jmp     wreg

wreg:   stx     VDCREG
:       bit     VDCREG
        bpl     :-
        sta     VDCDAT
        rts
rreg:   stx     VDCREG
:       bit     VDCREG
        bpl     :-
        lda     VDCDAT
        rts

np:     .byte   0               ; number of parameters on the stack
npairs: .byte   0
cnt:    .byte   0
rx:     .word   0               ; VDR: x y w h c, x2 y2
ry:     .word   0
rw:     .word   0
rh:     .word   0
rc:     .word   0
rx2:    .word   0
ry2:    .word   0
plo:    .res    32              ; the parameter stack
phi:    .res    32
