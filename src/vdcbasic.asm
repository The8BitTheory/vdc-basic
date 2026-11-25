; vdcbasic
; made from Felix 'skOi!nk' Rosenhahn's "vdc-tool 2"
; by Mac Bacon
;  5 Jan 2003 improved label names
;   removed unneeded checks of vdc "ready" flag
; 31 Oct 2013 beautified
;  1 Nov 2013 renamed to vdcbasic
;   optimized loops for speed, everything else for size (80% left)
;   fixed bug: functions trashed $16/$17
;   fixed bug: vtr and rtv did not count correctly
;  3 Nov 2013 fixed bug: vmf wrote one extra byte
; 10 Nov 2013 split into BASIC part and vdc lib
; 18 Nov 2013 BANKING and SELFMOD are now configurable
;   reading fake register 255 returns capacity
;   fixed RST
; 23 Nov 2013 changed RST to use flag bits
; 24 Nov 2013 RST now resets r37 as well
; 25 Nov 2013 reading fake register 254 returns VDC version
; 30 Nov 2013 added SYN instruction
;  7 Dec 2013 added DISP, ATTR and CRSR instructions, also added installation message
; 16 Mar 2014 extended SYN: now allows writing registers
;
; by Goodwell:
; v2c
; 29 Sep 2023   extended VMC: now allows repetitions and offset-increment for target-address
; v2d
; 10 Feb 2024   extended VMC: added parameter for source-address increment per repetition
; v2e
; 23 Mai 2025   introducing VMP and VCS: Print and Print-Setup commands
; v2f
; Juni 2025     introducing decrements for VMC source and target addresses
; v2g
; October 2025  introducing vmb (vmp with fixed string-address)
;               additional parameter for vcs to set screen-width
;               vmp now handles zero-length strings correctly (don't print anything)
; v2h
; November 2026 vcl: lists longer than 256 bytes, command 7 keeps an increment of 0
;               vms: list may lie above $4000, command 2 no longer inherits increments from vmc
;               vmb: length 0 prints nothing
;               vcl: command 13 = syn, with optional register writes
;               vcl: unknown command no longer hangs (printed INVALID with the KERNAL banked out)
;               vcl: command 13 with high byte of n <> 0 does not wait if already in the vertical border
; v3
; 2026          moved to $1c00 (BASIC text now starts at $4001, see boot/boot.bas),
;               header with fixed addresses at $1c00-$1c0f
;               rxv: unpack a VRL stream from bank 0 into VRAM (token $fe41), vcl command 8 = rxv
;               vms: command 3 = source skip (read position n bytes on, nothing written)
;               vdm, vdl, vdr, vdb: drawing in bitmap (1 bpp) and chunky (4 bpp), tokens $fe42, $fe3e-$fe40 (P2c)
;               vcl: command 9 = vms with the list in bank 1 (16 KB common RAM during the entry, P2d)

; TODO    disp, attr and crsr should accept values <0 and >65535!
!macro message {!pet "vdc basic v3 installed"}

  !to "vdcbasic3.bin", cbm

  !source <6502/std.a>    ; for +bit16
  !source <6502/opcodes.a>  ; for AND/ORA self-mods
  !source <cbm/c128/kernal.a> ; for k_primm
  !source "./src/vdclib.a"  ; macros and code parts

;build params
RXV_T_DEFAULT = 14      ; vorläufig, siehe docs/v3/P2b-rxv.md 5
RXV_TRACK_DST = 0       ; 1: RXV tracks the VRAM address in software instead of reading R18/R19 (docs/v3/P2b-rxv.md 3.4)

; zp
linnum  = $16 ; uint16 for POKE, PEEK(), etc.

arg1  = $83 ; actually colors and scale factors for graphics
arg2  = $85 ; word
arg3  = $87 ; word
arg4  = $89 ; word - used as byte by VMC for nr of repetitions and VMP for length of string
arg5  = $8B ; word - by now only used by VMC for target address increase (only used as byte)
arg6  = $8D ; word - by now only used by VMC for source address increase (only used as byte)

arg_address     = $3f ; word. target position of VMP output and address to read VCL from
arg_address2    = $77 ; word. used to hold target offset of VMS

;$a-$14 should also be safely available. 11 bytes
vcl_command_id  = $a    ;byte
multi1          = $b    ;word. for multiplications
multi2          = $d    ;word. for multiplications


;$26-$2c should also be safely available. 7 bytes
offset_1        = $26   ;byte
offset_2        = $27   ;byte
arg_bank        = $28   ;byte
arg_loop        = $29   ;byte
vms_alpha       = $2a   ;byte

; basic
b_skip_comma      = $795c ; if comma: skip, otherwise: syntax error
b_parse_uint16_comma_uint8  = $8803 ; read unsigned 16-bit value to linnum, comma, unsigned 8-bit value to X
b_parse_comma_uint16    = $880f ; skip comma, read unsigned 16-bit value to AAYY (also stored in linnum)
b_parse_uint16      = $8812 ; read unsigned 16-bit value to AAYY (also stored in linnum)
b_parse_uint8_to_X    = $87f4 ; read unsigned 8-bit value to X
b_parse_string             = $877b ; $24-$25 (36-37) contain the address of the string and y the length
c_copy_rom_font_to_vram   = $c027
e_set_vdc_registers   = $e1dc ; a kernel routine to set several registers in a row

k_fetch                 = $02a2 ; read a value from any bank

; constants
FIRST_0xCE_TOKEN  = $0b ; BASIC 7 goes up to $ce $0a ("POINTER"), so we start at $ce $0b
FIRST_0xFE_TOKEN  = $27 ; BASIC 7 goes up to $fe $26 ("SLOW"), so we start at $fe $27
MODIFIED8   = $ff ; dummy value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; entry point: intercept four BASIC vectors
;    * = $0ac6
    * = $1c00
; header: fixed addresses, part of the interface (docs/v3/P2a-relocate.md, 2.1)
!if * != $1c00 { !error "header: entry not at $1c00" }
    jmp install             ; $1c00: SYS 7168 installs VDC-BASIC
!if * != $1c03 { !error "header: version not at $1c03" }
    !byte $30               ; $1c03: version, BCD ($30 = v3.0)
!if * != $1c04 { !error "header: threshold not at $1c04" }
    !byte RXV_T_DEFAULT     ; $1c04: threshold T for block fill (RXV, VDL/VDR/VDB spans), P2b
!if * != $1c05 { !error "header: reserved byte not at $1c05" }
    !byte 0                 ; $1c05: reserved
!if * != $1c06 { !error "header: rxv_end not at $1c06" }
rxv_end_ptr
    !word 0                 ; $1c06: RXV, address behind the end token of the last stream, P2b
!if * != $1c08 { !error "header: rxv_vram not at $1c08" }
rxv_vram_ptr
    !word 0                 ; $1c08: RXV, VRAM address behind the last byte written or skipped, P2b
!if * != $1c0a { !error "header: reserved bytes not at $1c0a" }
    !byte 0,0,0,0,0,0       ; $1c0a-$1c0f: reserved
!if * != $1c10 { !error "header: install not at $1c10" }
install
    ; three are consecutive
    ldx #$05
-     lda vectors, x
      sta $030c, x
      dex
      bpl -
    ; fourth is set separately
    lda #<call_function
    sta $02fc
    lda #>call_function
    sta $02fd
    ; output installation message
    lda #0
    sta $ff00
    jsr k_primm
    !by 13
    +message
    !by 13, 0 ; message terminator
    rts
vectors ; table of three vectors
    !word tokenize, detokenize, execute_instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; intercepted BASIC vectors
tokenize
    pha
    ; try new commands
    lda #>instruction_strings
    ldy #<instruction_strings
    jsr $43e2
    bcs tokenize_instruction
    ; try new functions
    lda #>function_strings
    ldy #<function_strings
    jsr $43e2
    bcs tokenize_function
    ; not found -> pass to BASIC
    pla
    sec
    jmp $4321

tokenize_instruction
    tax
    pla
    txa
    and #$7f
    clc
    adc #FIRST_0xFE_TOKEN
    ldx #$00
    jmp $43b2

tokenize_function
    tax
    pla
    txa
    and #$7f
    clc
    adc #FIRST_0xCE_TOKEN
    ldx #$ff
    jmp $43b2

detokenize
    ora #$80
    inx
    beq detokenize_function
;detokenize_instruction
    sec
    sbc #FIRST_0xFE_TOKEN
    tax
    lda #>instruction_strings
    ldy #<instruction_strings
    jmp $516a

detokenize_function
    sec
    sbc #FIRST_0xCE_TOKEN
    tax
    lda #>function_strings
    ldy #<function_strings
    jmp $516a

execute_instruction
    ldx #0
    stx $ff00
    and #$7f
    sec
    sbc #FIRST_0xFE_TOKEN
    asl
    tay
    lda instruction_ptrs + 1, y
    pha
    lda instruction_ptrs, y
    pha
    jmp chrget

call_function
    ldx #0
    stx $ff00
    sec
    sbc #FIRST_0xCE_TOKEN
    asl
    tay
    lda function_ptrs + 1, y
    sta $58
    lda function_ptrs, y
    sta $57
    jsr $56
    clc
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; strings and pointers
instruction_strings
    !pet "rgW", "rgA", "rgO"
    !pet "vmW", "vmA", "vmO"
    !pet "vmF", "vmC"
    !pet "rtV", "vtR", "vcC", "swP"
    !pet "rsT", "syN"
    !pet "disP", "attR", "crsR"
    !pet "vcS", "vmP", "zzZ", "vcL"
    !pet "vmS", "vmB"
    !pet "vdL", "vdR", "vdB"    ; $fe3e-$fe40, P2c
    !pet "rxV"                  ; $fe41
    !pet "vdM"                  ; $fe42, P2c
    !byte 0 ; terminator
instruction_ptrs
    !word rgw - 1, rga - 1, rgo - 1
    !word vmw - 1, vma - 1, vmo - 1
    !word vmf - 1, vmc - 1
    !word rtv - 1, vtr - 1, vcc - 1, swp - 1
    !word rst - 1, syn - 1
    !word disp - 1, attr - 1, crsr - 1
    !word vcs - 1, vmp - 1, zzz - 1, vcl - 1
    !word vms -1, vmb -1
    !word vdl - 1, vdr - 1, vdb - 1
.rxv_entry
    !word rxv - 1
.vdm_entry
    !word vdm - 1
!if .rxv_entry - instruction_ptrs != 2 * ($41 - FIRST_0xFE_TOKEN) {
    !error "instruction table: rxv must be token $fe41"
}
!if .vdm_entry - instruction_ptrs != 2 * ($42 - FIRST_0xFE_TOKEN) {
    !error "instruction table: vdm must be token $fe42"
}

function_strings
    !pet "rgD", "vmD"
    !byte 0 ; terminator
function_ptrs
    !word rgd, vmd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; new functions
shared_function_entry
    jsr $7956 ; parse opening quote and expression?
    jsr $8815 ; make sure uint16
    jmp io_on

rgd ; read VDC register
    lda linnum + 1  ; we must backup this because it might be in use by caller
    pha
    lda linnum
    pha
    jsr shared_function_entry
    ;FIXME - make sure reg is uint8!
    ldx linnum
    cpx #254  ; first fake register
    bcs .read_fake_register
    ; normal registers
    jsr vdc_reg_X_to_A
    jmp $80db ; end of PEEK, does TAY:PLA:STA$16:PLA:STA$17:JMP$84d4

.read_fake_register
    inx ; 255?
    beq .check_capacity
    ;inx  ; 254?
    ;beq .return_version
;.return_version ; read fake "VDC version" register
    lda vdc_state
    and #%...#####
    jmp $80db ; end of PEEK, does TAY:PLA:STA$16:PLA:STA$17:JMP$84d4

.check_capacity ; read fake "VRAM capacity" register
    jsr vdc_check_capacity
    jmp $80db ; end of PEEK, does TAY:PLA:STA$16:PLA:STA$17:JMP$84d4

    +addcode_vdc_check_capacity

vmd ; read VRAM location
    lda linnum + 1  ; we must backup this because it might be in use by caller
    pha
    lda linnum
    pha
    jsr shared_function_entry
    ldy linnum
    lda linnum + 1
    jsr vram_AAYY_to_A
    jmp $80db ; end of PEEK, does TAY:PLA:STA$16:PLA:STA$17:JMP$84d4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; new instructions (the simple ones)
simple_instruction_shared_entry ; read args (uint16, uint8) and activate I/O
    jsr b_parse_uint16_comma_uint8  ; >> linnum, X
io_on
    lda $ff00 ; this should be $01
    and #$fe  ; activate I/O (just to make sure; no need to revert it later on)
    sta $ff00
    rts

chrget = $0380
chrgot = $0386
syn ; wait for end of text window, then write register values
    jsr chrgot  ; anything else?
    bne .syn_with_args
.just_syn ; wait for end of text window
syn_wait  ; (also used by vcl) changes A only
    jsr io_on
    ; wait until we are in text window
-     lda vdc_state
      and #$20
      bne -
    ; wait until we are out of text window
syn_wait_level ; (used by vcl, I/O must be on) returns at once if already in the border, changes A only
-     lda vdc_state
      and #$20
      beq -
    rts

.syn_with_args ; read reg/value pairs into buffer, SYN, write data to vdc regs
    ; remember start of array
    tsx
    stx .spbuf
    bne .entry
    ;--
-     jsr b_skip_comma
.entry      jsr b_parse_uint8_to_X  ; parse reg
      txa
      pha
      jsr b_skip_comma
      jsr b_parse_uint8_to_X  ; parse value
      txa
      pha
      jsr chrgot  ; more?
      bne -
    ; remember end of array
    tsx
    stx .lowest_sp
    ; remember interrupt mask
    php
    jsr .just_syn ; this will activate i/o
    sei
    ; write buffered reg/value pairs to vdc
    ldy .spbuf
-     lda $0100, y
      dey
      tax
      lda $0100, y
      dey
      jsr A_to_vdc_reg_X
.lowest_sp = * + 1: cpy #MODIFIED8
      bne -
    ; restore interrupt mask
    plp
    ; restore original stack pointer
.spbuf = * + 1: ldx #MODIFIED8
    txs
    rts

!zone register_access

    +addcode_vdc_read

rgw ; VDC register = value
    jsr simple_instruction_shared_entry ; >> linnum, X
    txa
    ldx linnum
    jmp A_to_vdc_reg_X

rga ; VDC register &= value
    lda #opcode_AND_8
    +bit16  ; skip the next instruction
rgo ; VDC register |= value
    lda #opcode_ORA_8
    sta .SELFMOD
    jsr simple_instruction_shared_entry ; >> linnum, X
    stx linnum + 1  ; store value in unused high-byte of address
    ldx linnum
    jsr vdc_reg_X_to_A
.SELFMOD  and linnum + 1  ; MODIFIED to perform either AND $xx or ORA $xx
    jmp A_to_vdc_reg_X

!zone sixteen_bit_register_access

crsr ; set cursor address
    lda #vdcr_crsr_hi
    +bit16  ; skip the next instruction
attr ; set address of attribute buffer
    lda #vdcr_attr_hi
    +bit16  ; skip the next instruction
disp ; set address of display buffer
    lda #vdcr_display_hi
    pha
;FIXME - accept values <0 and >65535 and wrap them accordingly!
    jsr b_parse_uint16  ; >> linnum
    jsr io_on
    pla
    tax
    lda linnum + 1  ; get high byte for first register (yes, sixteen-bit VDC registers are big-endian)
    jsr A_to_vdc_reg_X
    inx
    lda linnum  ; get low byte for second register
    jmp A_to_vdc_reg_X

!zone VRAM_access

    +addcode_A_to_vram_XXYY

vmw ; VRAM location = value
    jsr simple_instruction_shared_entry ; >> linnum, X
    
vmw_execute
    txa
    ldy linnum
    ldx linnum + 1
    jmp A_to_vram_XXYY



vma ; VRAM location &= value
    lda #opcode_AND_8
    +bit16  ; skip the next instruction
vmo ; VRAM location |= value
    lda #opcode_ORA_8
    sta .SELFMOD
    jsr simple_instruction_shared_entry ; >> linnum, X
    ldy linnum
    stx linnum  ; store value in (no longer needed) low-byte of address
    lda linnum + 1
    jsr vram_AAYY_to_A
.SELFMOD  and linnum  ; MODIFIED to perform either AND $xx or ORA $xx
    ldx linnum + 1
    jmp A_to_vram_XXYY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; new instructions (the more complicated ones)
complex_instruction_parse3args
    jsr b_parse_uint16
    sty arg1
    sta arg1 + 1
    jsr b_parse_comma_uint16
    sty arg2
    sta arg2 + 1
    jsr b_parse_comma_uint16
    sty arg3
    sta arg3 + 1
    rts

; sibbling to complex_instruction_block_entry. for all instructions without repeat support
complex_instruction_shared_entry ; read args (uint16, uint16, uint16), remember CR, activate full RAM with I/O
    jsr complex_instruction_parse3args
    jmp remember_mem_conf

; sibbling to complex_instruction_shared_entry. for all instructions that support repeats
complex_instruction_block_entry
    jsr complex_instruction_parse3args

    jsr chrgot  ; anything else?
    beq +

    ; parse nr of repetitions
    jsr b_skip_comma
    jsr b_parse_uint8_to_X
    stx arg4

    ;parse target address increase
    jsr b_parse_int16_AAYY
    sty arg5
    sta arg5+1
    
    jmp ++

    ; set nr of repetitions to 1
+   ldx #1
    stx arg4

++  jsr chrgot ; do we have another parameter?
    beq +
    
    ; parse source address increase per repetition
    jsr b_parse_int16_AAYY
    sty arg6
    sta arg6+1

    jmp remember_mem_conf

    ; set source address increase to 0
+   ldx #0
    stx arg6
    stx arg6+1

    ldy arg3    ;this is to keep compatibility with complex_instruction_shared_entry

    ; remember memory configuration for shared exit
remember_mem_conf
    ldx $ff00
    stx .cr
    ldx #$3e  ; full RAM with I/O
    stx $ff00
    rts; > AAYY = arg3

complex_instruction_shared_exit ; restore memory configuration
.cr = * + 1:  lda #MODIFIED8
    sta $ff00
    rts
saved_mem_conf = .cr    ; global name, for code in other zones
    +vdclib_assert_below_basic_rom  ; reached from rtv and vtr with BASIC ROM banked in

!zone fill_and_copy

    +addcode_vdc_do_YYAA_cycles

vmf ; fill VRAM with value
    jsr complex_instruction_block_entry  ; > AAYY = arg3
    ; decrement byte counter because the first one will be written manually

vmf_execute
    tya ; take a look at low byte
    bne +
      dec arg3 + 1
+   dec arg3
    ; clear BLOCK COPY register bit to get BLOCK WRITE:
    ldx #24
    jsr vdc_reg_X_to_A
    and #$7f
    jsr A_to_vdc_reg_X
    
    ; write first byte
-   lda arg2
    ;set target
    ldy arg1
    ldx arg1 + 1
    jsr A_to_vram_XXYY
    ;set count
    lda arg3
    ldy arg3 + 1
    jsr vdc_do_YYAA_cycles
    
    dec arg4
    beq +
    
    clc
    lda arg1
    adc arg5
    sta arg1

    lda arg1+1
    adc arg5+1
    sta arg1+1

    jmp -
    
+   jmp complex_instruction_shared_exit

; this is parsing VMC parameters from basic
vmc ; copy VRAM to VRAM
    jsr complex_instruction_block_entry ; > AAYY = arg3
    
; this can be used with pre-filled parameters, eg via VMP or VCL or VMS
vmc_execute
    ; set register bit for BLOCK COPY:
    ldx #24
    jsr vdc_reg_X_to_A
    ora #128
    jsr A_to_vdc_reg_X

    ; set source
block_copy_source
    ldy arg1
    lda arg1 + 1
    ldx #32
    jsr AY_to_vdc_regs_Xp1

    ; set target
block_copy_target
    ldy arg2
    lda arg2 + 1
    jsr AY_to_vdc_regs_18_19

    ; set count
    lda arg3
    ldy arg3 + 1
    jsr vdc_do_YYAA_cycles
    
    ; set nr repeats
    dec arg4
    beq .vmc_done

    ; increase target address
    clc
    lda arg2
    adc arg5
    sta arg2

    lda arg2+1
    adc arg5+1
    sta arg2+1    

    ; should source address be increased?
    ; check HB
    lda arg6+1
    cmp #0
    bne +       ; yes. jump to increasing source address

    ; check LB (only when HB equals zero)
    lda arg6
    cmp #0
    beq block_copy_target       ; LB not zero. jump to updating target address register (ie leave source address register alone)

    ; increase source address
+   clc
    lda arg1
    adc arg6
    sta arg1

    lda arg1+1
    adc arg6+1
    sta arg1+1
    jmp block_copy_source      ; jump to updating source address register

.vmc_done

    jmp complex_instruction_shared_exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Video Memory Sprite-Copy. Like VMC with a mix of VCL.
; vram-source is read continuously, target address can have gaps (used for transparency)
; effectively executes a number of VMC calls
; parameters: vram-source, vram-target, address of command list (bank 0).
; list: alpha value (byte), number of commands (word), then the commands. Lists may be longer than 256 bytes.
vms
    ;jsr complex_instruction_parse3args
    jsr complex_instruction_shared_entry

; entry with pre-filled parameters (arg1 = source, arg2 = target, arg3 = list address), used by VCL
vms_execute
    ; the caller's memory configuration was stored by remember_mem_conf.
    ; the whole list is read with RAM 0 and I/O mapped in ($3e), so it may lie
    ; under the BASIC ROM; the caller's configuration is restored at the end.
    lda #$3e
vms_execute_a               ; A = $ff00 while the list is read: $3e (bank 0) or $7e (bank 1, VCL 9, P2d)
    ldx saved_mem_conf
    stx vms_saved_cr
    sta saved_mem_conf      ; vmc_execute ends with $ff00 = saved_mem_conf: the configuration stays A
    sta $ff00

    ldx #24
    jsr vdc_reg_X_to_A
    ora #128
    jsr A_to_vdc_reg_X

    ; set source
    ldy arg1
    sty vms_block_source
    lda arg1 + 1
    sta vms_block_source+1
    ldx #32
    jsr AY_to_vdc_regs_Xp1

    ;target address offset (where on screen to put the sprite)
    lda arg2
    sta arg_address2
    lda arg2+1
    sta arg_address2+1

    lda arg3
    sta arg_address
    lda arg3+1
    sta arg_address+1

    ldy #0
    lda (arg_address),y
    sta vms_alpha 

    iny
    lda (arg_address),y
    sta arg_loop    ; number of commands, low byte

    iny
    lda (arg_address),y
    sta vms_count_hi ; number of commands, high byte

    iny
    sty offset_1
    ora arg_loop
    bne .vms_arg_loop
    jmp .vms_done   ; no commands

    ;add target-offset to vram-target address    
    ;will need to be made ready for indfet later, I guess
.vms_arg_loop
    clc             ; move the list pointer past the bytes read so far: offset_1 stays small
    lda offset_1
    adc arg_address
    sta arg_address
    bcc +
    inc arg_address + 1
+   ldy #0
    sty offset_1

    ; read vms-command byte (1=block copy with target offset and length, 2=rectangle, 3=source skip, 8=half byte)
    lda (arg_address),y
    iny

    cmp #1
    beq .vms_block_copy_1
    cmp #2
    beq .vms_block_copy_2
    cmp #3
    beq .vms_source_skip
    cmp #8
    bne +
    jmp .vms_rgo
+   jmp .vms_invalid

;------- block copy
.vms_block_copy_1
    clc
    lda (arg_address),y
    adc arg_address2
    sta arg2
    iny

    lda (arg_address),y
    adc arg_address2+1
    sta arg2+1
    iny

    ;read word 2 -> set arg3    
    clc
    lda (arg_address),y
    sta arg3
    iny

    ; write the values of regs 32/33 to vms_block_source
    clc
    adc vms_block_source
    sta vms_block_source
    bcc +
    inc vms_block_source+1

    ;lda (arg_address),Y
+   lda #0
    sta arg3+1
    ;iny
    sty offset_1

    ldy #1
    sty arg4

    ;execute vmc
    jmp .vms_block_copy_execute

; command 2 is intended as a single-use command for now.
;  it is intended to be used for square, non-transparent sprites.
.vms_block_copy_2
    clc
    lda (arg_address),y
    sta arg3
    iny

    lda (arg_address),y
    sta arg3+1
    iny

    ;read word 2 -> set arg3
    lda (arg_address),y
    sta arg4
    iny

    lda (arg_address),y
    sta arg4+1
    iny
    sty offset_1

    ldx #1
    jsr vdc_reg_X_to_A
    sta arg5
    lda #0          ; do not inherit increments from an earlier VMC
    sta arg5+1
    sta arg6
    sta arg6+1
    lda arg_address2 ; the rectangle goes to the target address itself
    sta arg2
    lda arg_address2+1
    sta arg2+1

.vms_block_copy_execute
    ;execute vmc
    jsr block_copy_target
    jmp .vms_check_more


;--------- source skip: read position += n (word), nothing written (clipping at the viewport edge)
.vms_source_skip
    clc
    lda (arg_address),y
    adc vms_block_source
    sta vms_block_source
    iny
    lda (arg_address),y
    adc vms_block_source+1
    sta vms_block_source+1
    iny
    sty offset_1
    ldy vms_block_source    ; A = high byte
    jmp .vms_set_source

;--------- OR value
.vms_rgo
    jsr io_on

    ; load sprite-part into A
    sty offset_1
    lda vms_block_source+1
    ldy vms_block_source
    jsr vram_AAYY_to_A
    sta multi1

    ; load the background-part into A
    ; load low-byte from arguments
    ldy offset_1
    lda (arg_address),y
    iny
    sty offset_1
    clc
    adc arg_address2
    sta multi2  ;temporarily used, as this value needs to go into Y later
    pha

    ; load high-byte from arguments
    lda (arg_address),y
    inc offset_1
    adc arg_address2+1
    pha ;push high-byte to stack

    ldy multi2  ; picking this up from before
    jsr vram_AAYY_to_A
    sta multi2

    ; check if lower-nybble should be transparent (rightmost pixel in byte)
    lda #%00001111
    and multi1      ; remove left pixel from foreground (multi1)
    cmp vms_alpha
    bne +

    ; make right pixel transparent.
    lda #%11110000
    and multi1      ; remove right pixel from foreground (multi1)
    sta multi1
    lda #%00001111
    jmp ++

    ; make left pixel transparent
+   sta multi1
    lda #%11110000

++  and multi2      ; remove pixel from background (multi2)  
    ora multi1      ; merge foreground and background
    sta multi1      ; store new foreground

    ; write the combined-part into vram
    pla ;pull high-byte of vram target address from stack
    tax
    pla ;pull low-byte of vram target address from stack
    tay
    lda multi1
    jsr A_to_vram_XXYY

    ; increase block-copy (regs 32/33) source by 1 (because we wrote that 1 byte manually)
    lda vms_block_source
    clc
    adc #1
    sta vms_block_source
    tay
    bcc +
    inc vms_block_source+1
+   lda vms_block_source+1
.vms_set_source             ; A = high, Y = low byte of the read position -> R32/R33
    ldx #32
    jsr AY_to_vdc_regs_Xp1

    ; check for more vms parameters (16-bit count)
.vms_check_more
    lda arg_loop
    bne +
    dec vms_count_hi
+   dec arg_loop
    lda arg_loop
    ora vms_count_hi
    beq .vms_done
    jmp .vms_arg_loop

.vms_invalid
    lda vms_saved_cr    ; the Kernal must be visible for primm
    sta $ff00
    jsr k_primm
    !pet "invalid vms command"
    !byte 0

.vms_done
    lda vms_saved_cr    ; restore caller's memory configuration
    sta $ff00
    rts

!zone transfer_stuff

VDCLIB_OPTION_SELFMOD = 1
VDCLIB_OPTION_BANKING = 1

rtv_vtr_swp_shared_setup
    jsr AY_to_vdc_regs_18_19
    ldx #31 ; VRAM register
    stx vdc_reg
    ldx #0
    ; negate int16 for easier counting:
    txa
    sec
    sbc arg3
    sta arg3
    txa
    sbc arg3 + 1
    sta arg3 + 1
    rts ; > X=0 (callers depend on it)

!zone RAM_to_VRAM

VDC_RAM_READPTR   = arg1
VDC_RAM_WRITEPTR  = arg2
VDC_COUNTER   = arg3

    +addcode_ram_to_vram
    +addcode_vram_to_ram

rtv ; copy RAM to VRAM
    jsr complex_instruction_shared_entry
    ldy arg2
    lda arg2 + 1
    jsr rtv_vtr_swp_shared_setup
    jsr ram_to_vram
    jmp complex_instruction_shared_exit
    +vdclib_assert_below_basic_rom

!zone VRAM_to_RAM

vtr ; copy VRAM to RAM
    jsr complex_instruction_shared_entry
    ldy arg1
    lda arg1 + 1
    jsr rtv_vtr_swp_shared_setup
    jsr vram_to_ram
    jmp complex_instruction_shared_exit
    +vdclib_assert_below_basic_rom


!zone exchange_RAM_and_VRAM

swp ; exchange contents of RAM and VRAM
    jsr complex_instruction_shared_entry
    ldy arg1
    lda arg1 + 1
    jsr rtv_vtr_swp_shared_setup  ; >> X=0
    ; get lowbyte into Y and clear base pointer's lowbyte instead
    ldy arg2
    stx arg2
---       +vdc_ldx  ; read byte from VRAM
        ; exchange with byte from RAM
        sta $ff01 ; full RAM (A is dummy)
        lda (arg2), y
        pha ; RAM-to-VRAM byte is now on stack
        txa
        sta (arg2), y ; VRAM-to-RAM is now done
        dec $ff00 ; enable I/O
        ; restore previous VRAM address
        ldx #18
        stx vdc_reg
        lda arg1 + 1
        +vdc_sta
        inx
        stx vdc_reg
        lda arg1
        +vdc_sta
        ldx #31
        stx vdc_reg
        ; write byte to VRAM
        pla
        +vdc_sta
        ; RAM-to-VRAM is now done
        ; increment RAM pointer
        iny
        beq .fix_ram_hi
.back_ram     ; increment VRAM pointer (needed when "restoring" VRAM address in next iteration)
        inc arg1
        beq .fix_vram_hi
.back_vram      ; check whether done
        inc arg3
        bne ---
      inc arg3 + 1
      bne ---
    jmp complex_instruction_shared_exit

.fix_vram_hi  inc arg1 + 1
    jmp .back_vram

.fix_ram_hi inc arg2 + 1
    jmp .back_ram

!zone copy_charset

vcc ; copy charset from RAM to VRAM
    jsr complex_instruction_shared_entry
    ; get low byte of RAM pointer into Y and clear base pointer's low byte instead
    ldy arg1
    ldx #0
    stx arg1
---     ; set VRAM pointer
      ldx #18
      lda arg2 + 1
      stx vdc_reg
      sta vdc_data
      inx
      lda arg2
      stx vdc_reg
      sta vdc_data
      ldx #31 ; prepare VRAM access
      stx vdc_reg
      ; prepare target address for next iteration
      clc
      adc #16
      sta arg2
      bcc +
        inc arg2 + 1
+     ; set loop counter (TODO - make bytes per character an optional parameter?)
      lda #8  ; character size
      sta arg3 + 1
      ldx #%00001110  ; kernal and I/O, but RAM instead of BASIC ROM, so vcc also works above $4000
      ; loop to copy a single character pattern
--        ; read byte from RAM
        sta $ff01 ; full RAM (A is dummy)
        lda (arg1), y
        ; increment RAM pointer
        iny
        beq .fix_hi
.back       ; write byte to VRAM
        stx $ff00 ; kernal and I/O
        +vdc_sta
        ; check whether done with this char
        dec arg3 + 1
        bne --
      ; all characters done?
      dec arg3
      bne ---
    jmp complex_instruction_shared_exit

.fix_hi   inc arg1 + 1
    jmp .back
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; "rst" instruction
rst   jsr b_parse_uint8_to_X
    jsr io_on
    txa
    lsr
    bcc +
      pha
      jsr reset_vdc_registers
      pla
+   lsr
    bcc .rts
      jmp c_copy_rom_font_to_vram

reset_vdc_registers
    ; reset r37 (ignored by kernal)
    ldx #37
    stx vdc_reg
    ldx #$ff
    stx vdc_data
    ; this was copied from kernel rom at $e179
    inx;ldx #0    ; standard contents
    jsr e_set_vdc_registers
    lda vdc_state ; check VDC version
    and #$07
    beq +
      ldx #$3b  ; offset of alterations for non-null version
      jsr e_set_vdc_registers
+   bit $0a03
    bpl .rts
      ldx #$3e  ; offset of alterations for PAL system
      jmp ($e192) ; either e_set_vdc_registers or $fc67, depending on kernal
.rts    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vmp - VDC Print instruction
;  this command basically chains VMC in a way that full strings can be printed, not just single characters
;  parameters: target address, location of string, length of string

!zone print_to_vdc {

vmb
;parse target address (where to render the text to)
    jsr b_parse_uint16
    sty arg_address
    sta arg_address + 1

;parse location of string in bank 0
    jsr b_skip_comma

    jsr b_parse_uint16
    sty $24
    sta $25

;parse  length of string
    jsr b_skip_comma

    jsr b_parse_uint8_to_X
    stx vmp_length
    txa
    bne +
    rts             ; nothing to print
+
    lda #$3f
    sta vmp_bank

    jmp vmp_prepare

; todo: check all parameter values that are passed to VMC_execute.
;       and compare them to the basic execution to print text
vmp
    
    ;parse target address (where to render the text to)
    jsr b_parse_uint16
    sty arg_address
    sta arg_address + 1

    ;parse location and length of string (location in bank 1)
    jsr b_skip_comma

    ;bank 15
    lda #15
    sta $2
    ;address HB/LB
    lda #>b_parse_string ;$877b
    sta $3
    lda #<b_parse_string
    sta $4
    jsr $02cd ;jsrfar

    ;$877b writes string length to A, which is stored to $6 by JSRFAR
    lda $6
    beq .vmp_done
    sta vmp_length

    lda #$7f
    sta vmp_bank

vmp_prepare
    ;parse_string ($877b) writes string address to $24/$25
    ; prepare for indirect FETCH
    lda #$24
    sta $02aa

    ;arg3(count16) is not changed in VMC, so we can set it here already
    lda arg_charset_width
    sta arg3
    ;setting arg3+1 to zero below, when we have the value handy anyways

    ;read virtual screen width from register 1 and store it in arg5 (arg4 would work, too. but for VMC it's arg5 anyways)
    ;  we could do this in VCS as well. but that would require another persistent byte.
    ;  it's sufficiently fast here, I guess
    lda arg_screen_width
    sta arg5

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
    ; iterate over characters - from 0 to arg3-1

    
    ldy #0
    sty offset_1      ;keeps track of current character position we're iterating
    sty arg3+1          ;setting this here (instead of a couple lines above) because we have zero handy
    sty arg5+1
    sty arg6
    sty arg6+1

    ; load next (first) character of second parameter (arg2)
    ; the address of that is stored in $24/$25
    ;lda (arg2),y   ;not this. we need to use FETCH
.vmp_next_character
    ldy offset_1
    ldx vmp_bank        ;bank 1
    jsr k_fetch
    inc offset_1

    ;  calculate offset of character in charset
    ; sub 32 from offset. quick and dirty.
    sec
    sbc #31

    tax

    lda arg_charset_address
    sta arg1
    lda arg_charset_address+1
    sta arg1+1

-   dex
    beq +
    clc
    lda arg1
    adc arg_charset_size
    sta arg1
    bcc -
    inc arg1+1
    jmp -

    ;vmc_arg1=arg_charset_address+arg_charset_offset*n
    ;vmc_arg2=vcp_arg1
    ;vmc_arg3=arg_charset_width
    ;vmc_arg4=arg_charset_height
    ;vmc_arg5=arg5
    
+   lda arg_address
    sta arg2
    lda arg_address+1
    sta arg2+1

    ;arg3 has been set above already

    lda arg_charset_height
    sta arg4

    jsr remember_mem_conf
    jsr vmc_execute

    dec vmp_length
    beq .vmp_done
    
    clc
    lda arg_address
    adc arg_charset_width
    sta arg_address
    bcc +
    inc arg_address+1
+   jmp .vmp_next_character
    
.vmp_done
    rts

; VCS: VDC Charset Set - sets the parameters for the VMP command. these settings stick, so they don't need to be given for each VMP call
vcs
    jsr complex_instruction_parse3args

    ;parse address of the charset in vram.
    lda arg1
    sta arg_charset_address
    lda arg1+1
    sta arg_charset_address+1

    ;parse width of characters in bytes
    lda arg2
    sta arg_charset_width

    ;parse height of characters in scanlines
    lda arg3
    sta arg_charset_height

    jsr chrgot
    beq +

;   screen-width parameter found, read it
    jsr b_skip_comma
    jsr b_parse_uint8_to_X
    stx arg_screen_width
    jmp ++

;   no screen-width parameter found, pull it from the vdc-register    
+   jsr io_on               ; I/O may be banked out after parsing; reading $d600/$d601 would hit RAM ($ff)
    ldx #1
    jsr vdc_reg_X_to_A
    sta arg_screen_width

    ;calc size of each character in bytes (width*height)
++  lda #0
    ldy arg_charset_height

    clc
-   adc arg_charset_width
    sta arg_charset_size
    dey
    bne -

    rts
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vcl - "VDC Command List" instruction
;  this command reads bytes from memory and interprets them as parameters to the instructions
;  first byte is the type of command
;  0: exit vcl
;  1: vmc with 3 parameters
;  2: vmc with 5 parameters
;  3: vmc with 6 parameters
;  4: vmf with 3 parameters
;  5: vmf with 5 parameters
;  6: vmw with 2 parameters
;  7: vmw with 4 parameters (vaddr, byte, reps, step)
;  8: rxv with 2 parameters (src, dst)
;  9: vms with 3 parameters, list in bank 1 (P2d)
; 10-11: reserved
; 12: vms with 3 parameters
; 13: syn: n, then n pairs of register, value (n = 0: just wait for end of text window)
; ff: sleep with 1 parameter (jiffies to sleep)
; 
;  bytes after that are the parameters (can be 1 or 2 bytes each)
;  after last parameter, next byte is checked.
;  0 leads to end of vcl instruction
;  >0 interprets next bytes according to the value here (ie 1-7)


;  vcl parameters: <address of list>,[<bank of list-address>]
!zone command_list {
.vcl_syn                    ; 13, n, n * (reg, value): entry is 3 + 4 * n bytes, may be longer than 256
    lda arg_address         ; base - 1: n at y = 2/3, pairs at y = 4, 8, ...; y wraps to 0 only at the end of a pair
    bne +
    dec arg_address + 1
+   dec arg_address
    ldy #2
    lda (arg_address),y     ; n (low byte)
    sta arg_loop
    php                     ; wait with the caller's interrupt flag, write registers with sei
    iny                     ; y = 3
    lda (arg_address),y     ; high byte of n: 0 = wait for the start of the border, else only wait if not in it
    beq +
    jsr syn_wait_level      ; keeps X and Y, ends with A = $20
    bne ++
+   jsr syn_wait            ; keeps X and Y
++  iny                     ; y = 4
    sei
    ldx arg_loop
    beq .vcl_syn_end
-     lda (arg_address),y   ; register
      tax
      iny
      iny
      lda (arg_address),y   ; value
      jsr A_to_vdc_reg_X    ; keeps Y
      iny
      iny
      bne +
      inc arg_address + 1
+     dec arg_loop
      bne -
.vcl_syn_end
    plp
    sty offset_1            ; next entry is at arg_address + y
    jmp .vcl_next_command

vcl
    jsr b_parse_uint16
    sty arg_address
    sta arg_address + 1

    ldy #0
    sty offset_1
    lda $ff00               ; remember caller's memory configuration once for the whole list
    sta vcl_saved_cr

.vcl_next_command
    jsr remember_mem_conf

    clc             ; move the list pointer past the bytes read so far: offset_1 stays small
    lda offset_1
    adc arg_address
    sta arg_address
    bcc +
    inc arg_address + 1
+   ldy #0
    sty offset_1
    sty offset_2

    ; clear args
    ldx #11
    tya
-   sta arg1,x
    dex
    bpl -

; read first byte (command type)
    ldy offset_1
    lda (arg_address),y
    bne +
    jmp .vcl_done           ; acc contains zero. the VCL is done

+   cmp #13                 ; only 1-12 have a parameter table entry, 14+ are unknown
    bcc +
    beq .vcl_syn            ; 13 has a variable length
    jmp .vcl_handle_unknown
+   sta vcl_command_id
    inc offset_1

    ;  1,2,3: vmc
    ;  4,5: vmf
    ;  6,7: vmw

    tax
    lda vcl_parameter_bytes,x
    sta arg_loop

-   ldy offset_1
    lda (arg_address),y
    inc offset_1

    ldx offset_2
    sta arg1,x
    inc offset_2
    
    cpx arg_loop
    bne -

    ; commands without repetition parameter (1, 4): args were cleared, so treat 0 repetitions as 1
    ; (VMC and VMF only: for VMW, arg4 is the address increment)
    lda vcl_command_id
    cmp #6
    bcs +
    lda arg4
    ora arg4 + 1
    bne +
    inc arg4
+
    ; VMC (1-3)
    lda #3
    cmp vcl_command_id
    bmi +
    jsr vmc_execute
    jmp .vcl_next_command

    ; VMF (4-5)
+   lda #5
    cmp vcl_command_id
    bmi +
    ldy arg3
    jsr vmf_execute
    jmp .vcl_next_command

    ; VMW (6-7)
+   lda #7
    cmp vcl_command_id
    bmi .vcl_check_vms

    lda arg3
    bne .vcl_vmw_next   
    inc arg3            ;if arg3 is zero, set it to 1

.vcl_vmw_next
    ldy arg1
    sty linnum
    ldx arg1+1
    stx linnum+1

    ldx arg2
    
    ; 3 jiffies with vcl block for 100 scanlines height
    jsr io_on
    jsr vmw_execute

    dec arg3
    bne +
    jmp .vcl_next_command      ; if no more repetitions, go to end of command
+

    clc
    lda arg1
    adc arg4
    sta arg1

    lda arg1+1
    adc arg4+1
    sta arg1+1

    jmp .vcl_vmw_next

    ; VMS (12): source, target, address of command list (bank 0)
    ; VMS (9): the same with the list in bank 1 (docs/v3/P2d-vms-bank1.md). during the entry the bottom
    ;  16 KB are common RAM ($d506), so VDC-BASIC ($1c00-$3fff) stays visible with $ff00 = $7e
.vcl_check_vms
    lda vcl_command_id
    cmp #8
    beq .vcl_rxv
    cmp #12
    beq +
    cmp #9
    bne .vcl_handle_unknown
+   lda arg_address         ; VMS uses arg_address and offset_1 for its own list: keep VCL's
    pha
    lda arg_address + 1
    pha
    lda offset_1
    pha
    lda $d506               ; $ff00 = $3e here (I/O on)
    sta vcl_saved_rcr
    lda vcl_saved_cr        ; VMS prints its error with k_primm: give it the caller's configuration to restore
    sta $ff00
    jsr remember_mem_conf   ; $ff00 = $3e again
    lda #$3e                ; type 12: read the list from bank 0
    ldx vcl_command_id
    cpx #12
    beq +
    lda vcl_saved_rcr       ; type 9: common RAM 16 KB at the bottom ...
    and #$f0
    ora #$07
    sta $d506
    lda #$7e                ; ... and read the list from bank 1 (RAM, I/O)
+   jsr vms_execute_a       ; returns with the caller's configuration (bank 0)
    jsr remember_mem_conf   ; $ff00 = $3e: I/O for $d506
    lda vcl_saved_rcr       ; common RAM as before
    sta $d506
    pla
    sta offset_1
    pla
    sta arg_address + 1
    pla
    sta arg_address
    jmp .vcl_next_command

    ; RXV (8): source (bank 0), target. memory configuration is $3e, as rxv_execute expects
.vcl_rxv
    jsr rxv_execute
    jmp .vcl_next_command

    ;handle unknown command-id
.vcl_handle_unknown
    jsr .vcl_done           ; k_primm needs the KERNAL: restore the caller's configuration first
    jsr k_primm
    !pet "invalid"
    !byte 0

.vcl_done
    lda vcl_saved_cr        ; restore caller's memory configuration (commands may have changed .cr)
    sta $ff00
    rts

}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vdm, vdl, vdr, vdb - drawing in bitmap (mode 0, 1 bpp) or chunky (mode 1, 4 bpp). spec: docs/v3/P2c-draw.md
;  VDM mode,base,stride   VDL x1,y1,x2,y2,c   VDR x,y,w,h,c (outline)   VDB x,y,w,h,c (filled)
;  no clipping, addresses wrap at $ffff. run with $ff00 = 0 like execute_instruction (no RAM access
;  except the own state). a pixel position is the byte address plus a "bit index" p: mode 0 p = x and 7
;  (bit 7 - p), mode 1 p = 0 (high nibble, even x) or 4 (low nibble, odd x). a pixel covers the bits
;  p .. p + w1 (w1 = 0 or 3): mask = lmask(p) and rmask(p + w1).
draw_ad     = $0a           ; word: VRAM address for pix_rmw, span_draw, xor_run
draw_mk     = $0c           ; mask for pix_rmw
draw_t      = $0d           ; pix_rmw: byte read
draw_cu     = $0e           ; word: VDL walking address, VDR/VDB row address
draw_p      = $10           ; bit index of the current pixel
draw_e      = $11           ; word, signed: Bresenham error term
draw_cnt    = $13           ; word: negative counter (counts up to 0)
draw_ln     = $8d           ; word: run length for run/xor_run, byte distance for span_prep
draw_d2     = $26           ; word: VDL 2 * minor delta ($26-$2a: work bytes of VCL/VMS, free here)
draw_d2m    = $28           ; word: VDL 2 * minor - 2 * major
draw_sp     = $2a           ; bit index of the first pixel of a span
draw_sa     = arg5          ; word: VDL first byte of the current run (c is no longer needed)
; parameters (arg1-arg5)
draw_x1     = arg1          ; VDL x1, VDR/VDB x
draw_y1     = arg2          ; VDL y1, VDR/VDB y
draw_x2     = arg3          ; VDL x2, VDR/VDB w
draw_y2     = arg4          ; VDL y2, VDR/VDB h
draw_c      = arg5

!macro draw_seek {          ; R18/R19 = draw_ad; keeps A, changes X and Y
    ldy draw_ad + 1
    ldx #18
    stx vdc_reg
    +vdc_sty
    inx
    ldy draw_ad
    stx vdc_reg
    +vdc_sty
}

!macro draw_add_stride .z { ; word at .z += stride
    clc
    lda .z
    adc draw_stride
    sta .z
    lda .z + 1
    adc draw_stride + 1
    sta .z + 1
}

!zone draw {
;--------------------------------------------------------------------------------
; VDM mode,base,stride
vdm
    jsr complex_instruction_parse3args
    jsr .end_check
    lda arg1 + 1
    bne .err14
    lda arg1
    cmp #2
    bcs .err14
    tay
    lda arg3
    ora arg3 + 1
    beq .err14              ; stride 0
    sty draw_mode
    ldx #3                  ; base, stride
-   lda arg2, x
    sta draw_base, x
    dex
    bpl -
    rts

.end_check                  ; ?SYNTAX ERROR if the statement goes on (too many parameters), then I/O on
    jsr chrgot
    bne .err11
    jmp io_on
.err11
    ldx #11
    +bit16                  ; skip the next instruction
.err14
    ldx #14
    jmp ($0300)             ; BASIC error vector

;--------------------------------------------------------------------------------
; VDL x1,y1,x2,y2,c
vdl
    jsr .parse5
    ldx #6                  ; all four coordinates 0-16383
-   lda arg1 + 1, x
    cmp #$40
    bcs .err14
    dex
    dex
    bpl -
    jsr .setup
    jsr draw_begin
    jsr .diffs
    lda draw_dx             ; dx >= dy: x-major
    cmp draw_dy
    lda draw_dx + 1
    sbc draw_dy + 1
    bcs +
    jmp .ymajor
+   bit draw_dxn
    bpl +
    jsr .swap               ; x1 <= x2
    jsr .diffs
+   lda draw_dy
    ora draw_dy + 1
    bne .xslope
    ; dy = 0: one span from x1 to x2
    jsr .row_y1
    lda draw_x2
    ldx draw_x2 + 1
    jsr .xrange
    jsr .row_span
    jmp draw_end

.xslope
    jsr .set_ss             ; ss = +-stride
    ldx #2                  ; minor = dy, major = dx
    ldy #0
    jsr .slope
    jsr .addr_pt
    lda draw_d2             ; 2 dy >= dx: runs of 1-2 pixels, one pixel per step
    cmp draw_dx
    lda draw_d2 + 1
    sbc draw_dx + 1
    bcc .xruns
    lda #$80                ; x-major
    jsr .ppx_line
    jmp draw_end

.xruns                      ; run slicing: a span whenever y changes
    lda draw_ad
    sta draw_cu
    sta draw_sa
    lda draw_ad + 1
    sta draw_cu + 1
    sta draw_sa + 1
    lda draw_p
    sta draw_sp
.xl
    inc draw_cnt
    bne +
    inc draw_cnt + 1
    beq .xl_end
+   lda draw_e + 1
    bmi .xl_flat
    ora draw_e
    beq .xl_flat
    jsr .endrun             ; run ends at x: span, then next row
    clc
    lda draw_cu
    adc draw_ss
    sta draw_cu
    lda draw_cu + 1
    adc draw_ss + 1
    sta draw_cu + 1
    jsr .xadv
    lda draw_cu
    sta draw_sa
    lda draw_cu + 1
    sta draw_sa + 1
    lda draw_p
    sta draw_sp
    clc
    lda draw_e
    adc draw_d2m
    sta draw_e
    lda draw_e + 1
    adc draw_d2m + 1
    sta draw_e + 1
    jmp .xl
.xl_flat
    jsr .xadv
    clc
    lda draw_e
    adc draw_d2
    sta draw_e
    lda draw_e + 1
    adc draw_d2 + 1
    sta draw_e + 1
    jmp .xl
.xl_end
    jsr .endrun
    jmp draw_end

.xadv                       ; one pixel to the right: p += pw, carry into draw_cu
    lda draw_p
    clc
    adc draw_pw
    cmp #8
    bcc +
    and #7
    inc draw_cu
    bne +
    inc draw_cu + 1
+   sta draw_p
    rts

.endrun                     ; span from (draw_sa, draw_sp) to (draw_cu, draw_p) in one row
    lda draw_p
    ora draw_w1
    sta draw_ep
    lda draw_cu
    cmp draw_sa
    bne .er_span
    lda draw_cu + 1
    cmp draw_sa + 1
    bne .er_span
    ldx draw_ep             ; inside one byte
    lda .rmtab, x
    ldx draw_sp
    and .lmtab, x
    sta draw_mk
    lda draw_cu
    sta draw_ad
    lda draw_cu + 1
    sta draw_ad + 1
    jmp .pix_rmw_f
.er_span
    sec
    lda draw_cu
    sta draw_ea
    sbc draw_sa
    sta draw_ln
    lda draw_cu + 1
    sta draw_ea + 1
    sbc draw_sa + 1
    sta draw_ln + 1
    jsr .span_prep
    lda draw_sa
    sta draw_ad
    lda draw_sa + 1
    sta draw_ad + 1
    jmp .span_draw

.ymajor                     ; one pixel per row
    bit draw_dyn
    bpl +
    jsr .swap               ; y1 <= y2
    jsr .diffs
+   ldx #0                  ; minor = dx, major = dy
    ldy #2
    jsr .slope
    jsr .set_ss             ; ss = +stride (y1 <= y2)
    jsr .addr_pt
    lda #0                  ; y-major
    ldx draw_dx
    bne +
    ldx draw_dx + 1
    bne +
    lda #$40                ; dx = 0: vertical run
+   jsr .ppx_line
    jmp draw_end

.set_ss                     ; draw_ss = stride, negated if draw_dyn bit 7 (y2 < y1)
    lda draw_stride
    ldx draw_stride + 1
    bit draw_dyn
    bpl +
    eor #$ff
    clc
    adc #1
    pha
    txa
    eor #$ff
    adc #0
    tax
    pla
+   sta draw_ss
    stx draw_ss + 1
    rts

.ppx_line                   ; A = draw_majx; one pixel per step from (draw_ad, draw_p), see .ppx
    sta draw_majx
    lda draw_p
    jsr .pmask
    sta draw_mk
    lda draw_dxn            ; x direction (bit 7: left), bit 6: chunky
    and #$80
    ldx draw_mode
    beq +
    ora #$40
+   sta draw_xs
    jmp .ppx

.diffs                      ; draw_dx = |x2 - x1|, draw_dxn bit 7 = (x2 < x1); the same for y at offset 2
    ldx #2
-   sec
    lda arg3, x
    sbc arg1, x
    sta draw_dx, x
    lda arg3 + 1, x
    sbc arg1 + 1, x
    sta draw_dx + 1, x
    sta draw_dxn, x
    bpl +
    lda #0
    sec
    sbc draw_dx, x
    sta draw_dx, x
    lda #0
    sbc draw_dx + 1, x
    sta draw_dx + 1, x
+   dex
    dex
    bpl -
    rts

.swap                       ; (x1, y1) <-> (x2, y2)
    ldx #3
-   lda arg1, x
    ldy arg3, x
    sta arg3, x
    sty arg1, x
    dex
    bpl -
    rts

.slope                      ; X = offset of the minor delta, Y = of the major delta (0 = dx, 2 = dy)
                            ; d2 = 2 minor, e = 2 minor - major, d2m = 2 minor - 2 major, cnt = -(major + 1)
    lda draw_dx, x
    asl
    sta draw_d2
    lda draw_dx + 1, x
    rol
    sta draw_d2 + 1
    sec
    lda draw_d2
    sbc draw_dx, y
    sta draw_e
    lda draw_d2 + 1
    sbc draw_dx + 1, y
    sta draw_e + 1
    sec
    lda draw_e
    sbc draw_dx, y
    sta draw_d2m
    lda draw_e + 1
    sbc draw_dx + 1, y
    sta draw_d2m + 1
    lda draw_dx, y
    eor #$ff
    sta draw_cnt
    lda draw_dx + 1, y
    eor #$ff
    sta draw_cnt + 1
    rts

.addr_pt                    ; draw_ad = address of (x1, y1), draw_p = its bit index
    lda draw_y1
    ldx draw_y1 + 1
    jsr .row_addr
    lda draw_x1
    ldx draw_x1 + 1
    jsr .xsplit
    sta draw_p
    clc
    lda draw_ad
    adc draw_tx
    sta draw_ad
    lda draw_ad + 1
    adc draw_tx + 1
    sta draw_ad + 1
    rts

;--------------------------------------------------------------------------------
; VDR x,y,w,h,c: top, bottom, left column, right column; every pixel once
vdr
    jsr .rect_args
    beq .r_done
    jsr .row_y1             ; top
    lda draw_cu
    sta draw_ra
    lda draw_cu + 1
    sta draw_ra + 1
    jsr .row_span
    lda draw_y2 + 1         ; h >= 2: bottom
    bne +
    lda draw_y2
    cmp #2
    bcc .r_done
+   ldx #2                  ; y + h - 1
    jsr .last
    jsr .row_addr
    jsr .ad_to_cu
    jsr .row_span
    lda draw_y2 + 1         ; h >= 3: columns of h - 2 pixels from row y + 1
    bne +
    lda draw_y2
    cmp #3
    bcc .r_done
+   lda draw_ra
    sta draw_cu
    lda draw_ra + 1
    sta draw_cu + 1
    +draw_add_stride draw_cu
    ldx #0                  ; left column: draw_bo0, draw_sp
    lda draw_sp
    jsr .column
    lda draw_x2 + 1         ; w >= 2: right column
    bne +
    lda draw_x2
    cmp #2
    bcc .r_done
+   ldx #2                  ; right column: draw_bo1, draw_p1
    lda draw_p1
    jsr .column
.r_done
    jmp draw_end

.column                     ; A = bit index, X = 0: left column (draw_bo0), X = 2: right (draw_bo1)
                            ; draw_cu = row address of the first pixel, h - 2 pixels
    pha
    clc
    lda draw_cu
    adc draw_bo0, x
    sta draw_ad
    lda draw_cu + 1
    adc draw_bo0 + 1, x
    sta draw_ad + 1
    pla
    jsr .pmask
    sta draw_mk
    sec                     ; cnt = -(h - 2) = 2 - h
    lda #2
    sbc draw_y2
    sta draw_cnt
    lda #0
    sbc draw_y2 + 1
    sta draw_cnt + 1
    lda #0                  ; downwards
    sta draw_dyn
    jsr .set_ss
    lda #$40                ; vertical run
    sta draw_majx
    jmp .ppx

;--------------------------------------------------------------------------------
; VDB x,y,w,h,c: filled, one span per row
vdb
    jsr .rect_args
    beq .b_done
    jsr .row_y1
    lda draw_y2             ; cnt = -h
    eor #$ff
    clc
    adc #1
    sta draw_cnt
    lda draw_y2 + 1
    eor #$ff
    adc #0
    sta draw_cnt + 1
-   jsr .row_span
    +draw_add_stride draw_cu
    inc draw_cnt
    bne -
    inc draw_cnt + 1
    bne -
.b_done
    jmp draw_end

;--------------------------------------------------------------------------------
; shared parts of VDR and VDB: parameters, checks, x range; Z = 1: w = 0 or h = 0, nothing to draw
.rect_args
    jsr .parse5
    ldx #2                  ; x + w <= 65536, y + h <= 65536
-   clc
    lda arg1, x
    adc arg3, x
    tay
    lda arg1 + 1, x
    adc arg3 + 1, x
    bcc +
    sty draw_tmp
    ora draw_tmp
    bne .err14b
+   dex
    dex
    bpl -
    jsr .setup
    jsr draw_begin
    lda draw_x2
    ora draw_x2 + 1
    beq +
    lda draw_y2
    ora draw_y2 + 1
    beq +
    ldx #0                  ; last x = x + w - 1
    jsr .last
    jsr .xrange
    lda #1                  ; Z = 0
+   rts

.last                       ; X = 0: x + w - 1, X = 2: y + h - 1 -> A = low, X = high (w, h >= 1)
    clc
    lda arg1, x
    adc arg3, x
    tay
    lda arg1 + 1, x
    adc arg3 + 1, x
    tax
    tya
    bne +
    dex
+   dey
    tya
    rts
.err14b
    jmp .err14

.xrange                     ; x1 = first x, A/X = last x -> draw_bo0/bo1 (byte offsets), draw_sp, draw_p1,
                            ; draw_ep, then .span_prep
    jsr .xsplit
    sta draw_p1
    ora draw_w1
    sta draw_ep
    lda draw_tx
    sta draw_bo1
    lda draw_tx + 1
    sta draw_bo1 + 1
    lda draw_x1
    ldx draw_x1 + 1
    jsr .xsplit
    sta draw_sp
    sec
    lda draw_bo1
    sbc draw_tx
    sta draw_ln
    lda draw_bo1 + 1
    sbc draw_tx + 1
    sta draw_ln + 1
    lda draw_tx
    sta draw_bo0
    lda draw_tx + 1
    sta draw_bo0 + 1
    jmp .span_prep

.row_y1                     ; draw_cu = row address of y1
    lda draw_y1
    ldx draw_y1 + 1
    jsr .row_addr
.ad_to_cu
    lda draw_ad
    sta draw_cu
    lda draw_ad + 1
    sta draw_cu + 1
    rts

.row_span                   ; span from draw_cu + draw_bo0 to draw_cu + draw_bo1
    clc
    lda draw_cu
    adc draw_bo0
    sta draw_ad
    lda draw_cu + 1
    adc draw_bo0 + 1
    sta draw_ad + 1
    clc
    lda draw_cu
    adc draw_bo1
    sta draw_ea
    lda draw_cu + 1
    adc draw_bo1 + 1
    sta draw_ea + 1
    ; fall through to .span_draw

;--------------------------------------------------------------------------------
; span: first byte draw_ad (bits from draw_sp on), last byte draw_ea (bits up to draw_ep)
.span_draw                  ; prepared by .span_prep; changes draw_ad, draw_ln
    lda draw_lm
    bit draw_single
    bpl .sd_multi
    sta draw_mk             ; inside one byte
    jmp .pix_rmw_f
.sd_multi
    cmp #$ff
    beq .sd_noleft
    sta draw_mk             ; left edge, afterwards the VDC address is draw_ad + 1
    jsr .pix_rmw_f
    inc draw_ad
    bne .sd_mid
    inc draw_ad + 1
    jmp .sd_mid
.sd_noleft
    bit draw_xor
    bmi .sd_mid             ; xor_run sets the address itself
    jsr .seek
.sd_mid
    lda draw_mid
    ora draw_mid + 1
    beq .sd_right
    lda draw_mid
    sta draw_ln
    lda draw_mid + 1
    sta draw_ln + 1
    jsr .run
.sd_right
    lda draw_rm
    cmp #$ff
    beq .sd_done
    sta draw_mk             ; right edge: the VDC address is draw_ea now (behind the middle)
    lda draw_ea
    sta draw_ad
    lda draw_ea + 1
    sta draw_ad + 1
    sta draw_vh             ; so R18 is right already
    jmp .pix_rmw_19
.sd_done
    rts

.span_prep                  ; draw_ln = last byte - first byte, draw_sp, draw_ep
                            ; -> draw_lm, draw_rm, draw_single, draw_mid (number of full middle bytes)
    ldx draw_sp
    lda .lmtab, x
    sta draw_lm
    ldx draw_ep
    lda .rmtab, x
    sta draw_rm
    lda draw_ln
    ora draw_ln + 1
    bne .sp_multi
    lda draw_lm             ; one byte: draw_lm = both masks
    and draw_rm
    sta draw_lm
    lda #$80
    sta draw_single
    rts
.sp_multi
    lda #0
    sta draw_single
    sec                     ; mid = n - 1 + (lm = $ff) + (rm = $ff)
    lda draw_ln
    sbc #1
    sta draw_mid
    lda draw_ln + 1
    sbc #0
    sta draw_mid + 1
    lda draw_lm
    jsr .inc_if_full
    lda draw_rm
.inc_if_full
    cmp #$ff
    bne +
    inc draw_mid
    bne +
    inc draw_mid + 1
+   rts

;--------------------------------------------------------------------------------
; VDC primitives
.pr_wrap                    ; the read moved the address to (hi + 1, 0): R18 and R19 (A kept)
    jsr .seek
    ldy draw_ad + 1         ; after the write the address is (hi + 1, 0)
    iny
    sty draw_vh
    jmp .pr_write

.pix_rmw_f                  ; as .pix_rmw, but the VDC address is unknown: force R18
    lda draw_ad + 1
    eor #$ff
    sta draw_vh
.pix_rmw                    ; byte at draw_ad: bits in draw_mk get the colour (or are inverted)
                            ; draw_vh = high byte of the current VDC address (R18 is skipped if equal)
                            ; afterwards R31 is selected, the VDC address is draw_ad + 1, draw_vh its high byte
    ldy draw_ad + 1
    cpy draw_vh
    beq +
    sty draw_vh
    ldx #18
    stx vdc_reg
    +vdc_sty
+
.pix_rmw_19                 ; entry with R18 = high byte of draw_ad already
    ldy draw_ad
    ldx #19                 ; R19 last (the order of the ROM): the read gets the byte at draw_ad
    stx vdc_reg
    +vdc_sty
    ldx #31
    stx vdc_reg
    +vdc_lda
    sta draw_t
draw_pix_op                 ; MODIFIED: eor #value (set/clear/colour) or lda #$ff (invert)
    eor #MODIFIED8
    and draw_mk
    eor draw_t
    cpy #$ff
    beq .pr_wrap
    ldx #19                 ; the read moved the address to draw_ad + 1, same high byte: R19 only
    stx vdc_reg
    +vdc_sty
.pr_write
    ldx #31
    stx vdc_reg
    +vdc_sta
.pr_tail                    ; MODIFIED: rts, or clc while .ppx runs (falls through to the next pixel)
    rts
    ; .ppx continued: count, Bresenham step, next pixel (C = 0)
    inc draw_cnt
    bne +
    inc draw_cnt + 1
    bne +
    jmp .ppx_end
+   bit draw_majx
    bvs .ppx_y              ; vertical run: y only
    lda draw_e + 1
    bmi .ppx_flat
    ora draw_e
    beq .ppx_flat
    lda draw_e              ; e > 0: x and y step
    adc draw_d2m
    sta draw_e
    lda draw_e + 1
    adc draw_d2m + 1
    sta draw_e + 1
    clc
    lda draw_ad
    adc draw_ss
    sta draw_ad
    lda draw_ad + 1
    adc draw_ss + 1
    sta draw_ad + 1
    jmp .ppx_x
.ppx_flat                   ; major axis only
    lda draw_e
    adc draw_d2
    sta draw_e
    lda draw_e + 1
    adc draw_d2 + 1
    sta draw_e + 1
    bit draw_majx
    bmi .ppx_x
    clc
.ppx_y                      ; y step: draw_ad += ss
    lda draw_ad
    adc draw_ss
    sta draw_ad
    lda draw_ad + 1
    adc draw_ss + 1
    sta draw_ad + 1
    jmp .pix_rmw
.ppx_x                      ; x step: move draw_mk, carry into draw_ad
    bit draw_xs
    bvs .ppx_xc
    bmi .ppx_x0l
    lsr draw_mk             ; mode 0, right
    bcc .ppx_next
    ror draw_mk             ; $80
.ppx_inc
    inc draw_ad
    bne .ppx_next
    inc draw_ad + 1
.ppx_next
    jmp .pix_rmw
.ppx_x0l                    ; mode 0, left
    asl draw_mk
    bcc .ppx_next
    rol draw_mk             ; $01
.ppx_dec
    lda draw_ad
    bne +
    dec draw_ad + 1
+   dec draw_ad
    jmp .pix_rmw
.ppx_xc                     ; mode 1: $f0 <-> $0f
    lda draw_mk
    eor #$ff
    sta draw_mk
    eor draw_xs             ; bit 7: next byte (right: now $f0, left: now $0f)
    bpl .ppx_next
    bit draw_xs
    bpl .ppx_inc
    bmi .ppx_dec            ; always

.ppx                        ; one pixel per step: -draw_cnt pixels from draw_ad (mask draw_mk); draw_majx bit 7:
                            ; x-major, bit 6: vertical run (y only); draw_ss = y step, draw_xs bit 7: x to the
                            ; left, bit 6: chunky; Bresenham: draw_e, draw_d2, draw_d2m (not for a vertical run)
    lda #$18                ; clc: .pix_rmw goes on with the next pixel
    sta .pr_tail
    jmp .pix_rmw_f
.ppx_end
    lda #$60                ; rts
    sta .pr_tail
    rts

.run                        ; draw_ln >= 1 middle bytes from the current VDC address (fill or invert)
    bit draw_xor
    bmi .xor_run
    lda draw_ln + 1
    bne .run_fill
    lda draw_ln
    cmp rxv_T
    bcs .run_fill
    tax                     ; L < T: CPU
    ldy #31
    sty vdc_reg
    lda draw_fv
-   +vdc_sta
    dex
    bne -
    rts
.run_fill                   ; first byte by CPU, then L - 1 by block fill
    bit draw_r24
    bmi +
    ldx #24                 ; block mode = fill, once per command
    jsr vdc_reg_X_to_A
    and #$7f
    jsr A_to_vdc_reg_X
    sec
    ror draw_r24
+   ldx #31
    stx vdc_reg
    lda draw_fv
    +vdc_sta
    ldy draw_ln + 1
    lda draw_ln
    bne +
    dey
+   sec
    sbc #1
    jmp vdc_do_YYAA_cycles  ; does not wait for the end

.xor_run                    ; invert draw_ln >= 1 bytes from draw_ad, in pieces of up to 64 bytes
    lda draw_ln + 1
    bne +
    lda draw_ln
    cmp #65
    bcc ++
+   lda #64
++  sta draw_xk
    jsr .seek31
-   +vdc_lda
    sta draw_buf, y
    iny
    cpy draw_xk
    bne -
    jsr .seek31
-   lda draw_buf, y
    eor #$ff
    +vdc_sta
    iny
    cpy draw_xk
    bne -
    clc
    lda draw_ad
    adc draw_xk
    sta draw_ad
    bcc +
    inc draw_ad + 1
+   sec
    lda draw_ln
    sbc draw_xk
    sta draw_ln
    bcs +
    dec draw_ln + 1
+   ora draw_ln + 1
    beq +
    jmp .xor_run
+   rts

.seek31                     ; R18/R19 = draw_ad, select R31, Y = 0
    jsr .seek
    ldx #31
    stx vdc_reg
    ldy #0
    rts
.seek                       ; R18/R19 = draw_ad
    +draw_seek
    rts

;--------------------------------------------------------------------------------
; addresses
.row_addr                   ; A = y lo, X = y hi -> draw_ad = base + y * stride (mod 65536); changes A, X, Y
    stx draw_yh
    ldy draw_base
    sty draw_ad
    ldy draw_base + 1
    sty draw_ad + 1
    ldy draw_stride
    sty draw_ml
    ldy draw_stride + 1
    sty draw_mh
    tay
    beq .ra_hi
-   lsr                     ; bits of y lo, lowest first
    tay
    bcc +
    clc
    lda draw_ad
    adc draw_ml
    sta draw_ad
    lda draw_ad + 1
    adc draw_mh
    sta draw_ad + 1
+   asl draw_ml
    rol draw_mh
    tya
    bne -
.ra_hi                      ; y hi: only the low byte of (y hi * stride lo) counts, added to the high byte
    lda draw_yh
    beq .ra_done
    ldy draw_stride
    sty draw_ml
    lda #0
-   lsr draw_yh
    bcc +
    clc
    adc draw_ml
+   asl draw_ml
    ldy draw_yh
    bne -
    clc
    adc draw_ad + 1
    sta draw_ad + 1
.ra_done
    rts

.xsplit                     ; A/X = x -> draw_tx = x >> 3 (mode 0) or x >> 1 (mode 1), A = bit index
    sta draw_tx
    stx draw_tx + 1
    and draw_pandm
    ldy draw_mode
    beq +
    asl
    asl
+   ldy draw_sh
-   lsr draw_tx + 1
    ror draw_tx
    dey
    bne -
    rts

.pmask                      ; A = bit index -> A = mask of one pixel; changes X, Y
    tax
    ora draw_w1
    tay
    lda .rmtab, y
    and .lmtab, x
    rts

;--------------------------------------------------------------------------------
; parameters and drawing state
.parse5                     ; five values -> arg1-arg5 (left to right: missing 11, > 65535 14), end of statement
    jsr b_parse_uint16
    ldx #0
-   sty arg1, x
    sta arg1 + 1, x
    inx
    inx
    cpx #10
    beq +
    stx draw_tmp            ; $0a-$14 belong to the expression evaluator while parsing
    jsr b_parse_comma_uint16
    ldx draw_tmp
    bne -                   ; always
+   jmp .end_check

.setup                      ; c for the current mode, mode constants, pix_rmw operation, fill value
    ldx draw_mode
    lda draw_c + 1
    bne .err14c
    lda draw_c
    cmp .t_cmax, x
    bcc +
.err14c
    jmp .err14
+    txa                     ; mode constants: 4 bytes from .t_mode + 4 * mode
    asl
    asl
    tax
    ldy #0
-   lda .t_mode, x
    sta draw_sh, y
    inx
    iny
    cpy #4
    bne -
    lda draw_c
    ldx draw_mode
    bne .chunky
    cmp #2
    beq .invert
    lda #0                  ; c = 0 -> $00, c = 1 -> $ff
    sec
    sbc draw_c
    jmp .set_value
.chunky
    asl                     ; c * 17
    asl
    asl
    asl
    ora draw_c
.set_value
    sta draw_fv
    sta draw_pix_op + 1
    lda #$49                ; eor #
    ldy #0
    beq .set_op             ; always
.invert
    lda #$ff
    sta draw_pix_op + 1
    lda #$a9                ; lda #
    ldy #$80
.set_op
    sta draw_pix_op
    sty draw_xor
    lda #0
    sta draw_r24
    rts

.lmtab  !byte $ff, $7f, $3f, $1f, $0f, $07, $03, $01     ; bits from index p to the right
.rmtab  !byte $80, $c0, $e0, $f0, $f8, $fc, $fe, $ff     ; bits up to index p
.t_cmax !byte 3, 16         ; c < cmax
.t_mode !byte 3, 7, 1, 0    ; mode 0: sh (x >> sh = byte offset), pandm (x and pandm = pixel in byte),
        !byte 1, 1, 4, 3    ; mode 1: pw (bits per pixel), w1 (bits per pixel - 1)
}

draw_begin                  ; breakpoints for the cycle measurement (test/draw/run_draw.py)
    rts
draw_end
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rxv - unpack a VRL stream (docs/reference/vrl-format.md) from bank 0 into VRAM
;  RXV src,dst   src: first token in bank 0 (not the file header), dst: VRAM address
;  afterwards $1c06 = address behind the end token, $1c08 = VRAM address behind the last byte
;  written or skipped. spec: docs/v3/P2b-rxv.md
;  the stream must not lie in $d000-$dfff or $ff00-$ff04 (read with $ff00 = $3e)
!zone rxv {
rxv_T   = $1c04             ; threshold T: runs with L < T by CPU, others by block fill
.ptr    = arg1              ; stream base, moved on after >= 128 bytes; Y = offset (< 128 at a token)
.cnt    = arg3              ; L - 1 for a block fill, skip length
.y      = arg5              ; Y saved around vdc_do_YYAA_cycles and skip
.dst    = arg2              ; RXV_TRACK_DST: VRAM address of the next byte

rxv
    jsr b_parse_uint16
    sty arg1
    sta arg1 + 1
    jsr b_parse_comma_uint16
    sty arg2
    sta arg2 + 1
    jsr remember_mem_conf   ; $ff00 = $3e, caller's configuration kept for the exit

; entry with arg1 = src, arg2 = dst, $ff00 = $3e (used by VCL); changes arg1-arg5
rxv_execute
    ldx #24                 ; block mode = fill, keep the other bits of R24
    jsr vdc_reg_X_to_A
    and #$7f
    jsr A_to_vdc_reg_X
    lda rxv_T               ; token threshold: run token b goes to the CPU if b < T + $7e
    clc
    adc #$7e
    bcs +
    cmp #$fe
    bcc ++
+   lda #$fe                ; T >= 128: all short runs by CPU
++  sta .ttok
    ldy .dst
    lda .dst + 1
    jsr AY_to_vdc_regs_18_19
    ldx #31                 ; R31 stays selected between the tokens
    stx vdc_reg
    ldy #0
.next
    cpy #$80
    bcc .tok
    tya                     ; move the base on so that Y stays small
    clc
    adc .ptr
    sta .ptr
    bcc +
    inc .ptr + 1
+   ldy #0
.tok
    lda (.ptr), y
    iny
    tax
    beq .end
    bmi .run
    ; literal: X = n (1..127) bytes follow
!if RXV_TRACK_DST {
    jsr .track_x
}
-     lda (.ptr), y
      iny
      +vdc_sta
      dex
      bne -
    beq .next               ; always

.end                        ; Y points behind the $00
    tya
    clc
    adc .ptr
    sta rxv_end_ptr
    lda .ptr + 1
    adc #0
    sta rxv_end_ptr + 1
!if RXV_TRACK_DST {
    lda .dst
    sta rxv_vram_ptr
    lda .dst + 1
    sta rxv_vram_ptr + 1
} else {
    ldx #19                 ; waits for a running block fill
    jsr vdc_reg_X_to_A
    sta rxv_vram_ptr
    dex
    jsr vdc_reg_X_to_A
    sta rxv_vram_ptr + 1
}
    jmp complex_instruction_shared_exit

.run                        ; X = b ($80..$ff)
    cpx #$fe
    bcs .skip_or_long
.ttok = * + 1
    cpx #MODIFIED8          ; b < T + $7e: L < T
    bcs .run_fill
    txa                     ; C = 0: A = b - $7e = L
    sbc #$7d
    tax
    lda (.ptr), y           ; value
    iny
.cpu_run                    ; A = value, X = L >= 1
!if RXV_TRACK_DST {
    pha
    jsr .track_x
    pla
}
-     +vdc_sta
      dex
      bne -
    beq .next               ; always

.run_fill                   ; C = 1, X = b: L - 1 = b - $7f (1..126)
    txa
    sbc #$7f
    sta .cnt
    lda #0
    sta .cnt + 1
    beq .fill_value         ; always

.skip_or_long
    bne .long
    ; skip: $fe lo hi, VRAM address += n
    lda (.ptr), y
    sta .cnt
    iny
    lda (.ptr), y
    sta .cnt + 1
    iny
    sty .y
!if RXV_TRACK_DST {
    clc
    lda .dst
    adc .cnt
    sta .dst
    tay
    lda .dst + 1
    adc .cnt + 1
    sta .dst + 1
    ldx #18
} else {
    ldx #19                 ; read the current address: waits for a running block fill
    jsr vdc_reg_X_to_A
    clc
    adc .cnt
    tay
    dex
    jsr vdc_reg_X_to_A      ; keeps C
    adc .cnt + 1
}
    jsr AY_to_vdc_regs_Xp1  ; R18 = A, R19 = Y
    jmp .reselect

.long                       ; $ff lo hi value: L = hi * 256 + lo (0 = 65536)
    lda (.ptr), y
    tax                     ; lo
    iny
    lda (.ptr), y
    iny
    sta .cnt + 1
    bne .long_fill          ; L >= 256: always block fill
    txa
    beq .long_fill          ; L = 65536
    cpx rxv_T
    bcs .long_fill          ; L >= T
    lda (.ptr), y
    iny
    jmp .cpu_run
.long_fill                  ; X = lo, .cnt + 1 = hi: .cnt = L - 1
    txa
    bne +
    dec .cnt + 1            ; lo = 0 borrows (65536 -> $ffff)
+   dex
    stx .cnt
.fill_value
    lda (.ptr), y           ; value
    iny
    ; block fill: A = value, .cnt = L - 1
    +vdc_sta                ; first byte, also sets the fill value
    sty .y
!if RXV_TRACK_DST {
    sec                     ; .dst += L
    lda .dst
    adc .cnt
    sta .dst
    lda .dst + 1
    adc .cnt + 1
    sta .dst + 1
}
    lda .cnt
    ldy .cnt + 1
    jsr vdc_do_YYAA_cycles  ; R30: lo (if not 0), then hi times 0 (= 256); does not wait for the end
.reselect
    ldx #31
    stx vdc_reg
    ldy .y
    jmp .next

!if RXV_TRACK_DST {
.track_x                    ; .dst += X
    txa
    clc
    adc .dst
    sta .dst
    bcc +
    inc .dst + 1
+   rts
}
}

; dummy entry to skip token $fe3a
zzz
    rts

b_parse_int16_AAYY
    JSR chrget    ; CHRGET

    JSR $77EF    ; Evaluate expression
    JSR $77DA    ; Confirm numeric
    JSR $849F    ; Float to fixed

    rts


arg_charset_address !word 0 ; the address in VRAM where the character set is stored
arg_charset_width   !byte 0 ; width in bytes of one character
arg_charset_height  !byte 0 ; height in scanlines of one character
arg_charset_size    !byte 0 ; the product of width*height. used to calculate offset of character in charset
arg_screen_width    !byte 0 ; the number of bytes in a scanline. can be eg 160 for printing with empty lines in between

vmp_length          !byte 0 ; length of the text to print

vcl_saved_cr        !byte 0     ; memory configuration at VCL entry
vms_count_hi        !byte 0     ; VMS: number of commands, high byte
vms_saved_cr        !byte 0     ; VMS: memory configuration of the caller
vcl_saved_rcr       !byte 0     ; VCL 9: $d506 of the caller
vcl_parameter_bytes !byte 0,5,9,11,5,9,3,7,3,5,0,0,5    ;first byte is dummy-byte. values -1 because of end-loop check (bne). 8 = rxv, 9 = vms (bank 1), 10-11 not implemented, 12 = vms. 13 = syn is handled separately (variable length)

vms_block_source    !word 0;
vmp_bank            !byte 0

; drawing (P2c): state of VDM (5 bytes, as after installation: mode 0, base 0, stride 80) and work bytes
draw_mode           !byte 0     ; 0 = bitmap (1 bpp), 1 = chunky (4 bpp)
draw_base           !word 0     ; VRAM address of pixel (0, 0)
draw_stride         !word 80    ; bytes per pixel row (directly after draw_base, see vdm)
draw_tmp            !byte 0
draw_sh             !byte 0     ; mode constants, set per command
draw_pandm          !byte 0
draw_pw             !byte 0
draw_w1             !byte 0
draw_fv             !byte 0     ; fill value of the middle bytes
draw_xor            !byte 0     ; bit 7: invert (mode 0, c = 2)
draw_r24            !byte 0     ; bit 7: R24 already switched to block fill
draw_yh             !byte 0
draw_ml             !byte 0
draw_mh             !byte 0
draw_tx             !word 0
draw_bo0            !word 0     ; byte offset of the first x
draw_bo1            !word 0     ; byte offset of the last x (directly after draw_bo0, see .column)
draw_p1             !byte 0     ; bit index of the last x (draw_sp: bit index of the first x)
draw_ep             !byte 0     ; last bit of a span (bit index + w1)
draw_lm             !byte 0
draw_rm             !byte 0
draw_single         !byte 0
draw_mid            !word 0
draw_ea             !word 0
draw_ra             !word 0
draw_ss             !word 0
draw_dx             !word 0     ; |x2 - x1|, |y2 - y1| (in this order)
draw_dy             !word 0
draw_dxn            !byte 0, 0  ; signs (bit 7) of x2 - x1 and y2 - y1 (at draw_dxn + 2)
draw_dyn            !byte 0
draw_xk             !byte 0
draw_vh             !byte 0     ; high byte of the VDC address after .pix_rmw
draw_majx           !byte 0     ; .ppx: bit 7 x-major, bit 6 vertical run
draw_xs             !byte 0     ; .ppx: bit 7 x to the left, bit 6 chunky
draw_buf            !fill 64, 0 ; middle bytes of an inverted span (xor_run)
!if draw_bo1 - draw_bo0 != 2 | draw_dy - draw_dx != 2 | draw_dyn - draw_dxn != 2 | draw_stride - draw_base != 2 {
    !error "draw: layout of the work bytes"
}

; also VCL 9 (P2d): with 16 KB common RAM all code and data must be below $4000
!if * > $4000 { !error "VDC-BASIC v3 endet bei ", *, ", erlaubt bis $3FFF" }
