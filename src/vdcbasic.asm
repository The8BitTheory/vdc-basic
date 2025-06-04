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
; Mai 2025      introducing decrements for VMC source and target addresses

; TODO    disp, attr and crsr should accept values <0 and >65535!
!macro message {!pet "vdc basic v2f installed"}

  !to "vdcbasic2f.bin", cbm

  !source <6502/std.a>    ; for +bit16
  !source <6502/opcodes.a>  ; for AND/ORA self-mods
  !source <cbm/c128/kernal.a> ; for k_primm
  !source "./src/vdclib.asm"  ; macros and code parts

;build params
release_vdl = 0
release_vdr = 0
release_vdb = 0

; zp
linnum  = $16 ; uint16 for POKE, PEEK(), etc.

arg1  = $83 ; actually colors and scale factors for graphics
arg2  = $85 ; word
arg3  = $87 ; word
arg4  = $89 ; word - used as byte by VMC for nr of repetitions and VMP for length of string
arg5  = $8B ; word - by now only used by VMC for target address increase (only used as byte)
arg6  = $8D ; word - by now only used by VMC for source address increase (only used as byte)

arg_address     = $72 ; word. target position of VMP output and address to read VCL from
arg_address2    = $77 ; word. used to hold target offset of VMS

;$9-$14 should also be safely available. 11 bytes
vcl_command_id  = $9
multi1          = $a    ;word. for multiplications
multi2          = $c    ;word. for multiplications

;$26-$2c should also be safely available. 7 bytes
offset_1        = $26
offset_2        = $27
arg_bank        = $28
arg_loop        = $29
vms_alpha       = $2a

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
    * = $1300
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
    !pet "vmS"
    ;!pet "vdL", "vdR", "vdB"
    !byte 0 ; terminator
instruction_ptrs
    !word rgw - 1, rga - 1, rgo - 1
    !word vmw - 1, vma - 1, vmo - 1
    !word vmf - 1, vmc - 1
    !word rtv - 1, vtr - 1, vcc - 1, swp - 1
    !word rst - 1, syn - 1
    !word disp - 1, attr - 1, crsr - 1
    !word vcs - 1, vmp - 1, zzz - 1, vcl - 1
    !word vms -1
    ;!word vdl -1, vdr -1, vdb -1, 
    
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
    jsr io_on
    ; wait until we are in text window
-     lda vdc_state
      and #$20
      bne -
    ; wait until we are out of text window
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
; parameters: vram-source, vram-target, address of command-bytes (target-increment, nr bytes to copy). first command-byte holds number of commands
vms
    ;jsr complex_instruction_parse3args
    jsr complex_instruction_shared_entry

    ldx #24
    jsr vdc_reg_X_to_A
    ora #128
    jsr A_to_vdc_reg_X

    ; set source
    ldy arg1
    lda arg1 + 1
    ldx #32
    jsr AY_to_vdc_regs_Xp1

    lda arg2
    sta arg_address2
    lda arg2+1
    sta arg_address2+1

    lda arg3
    sta arg_address
    lda arg3+1
    sta arg_address+1

    jsr complex_instruction_shared_exit

    lda #<arg_address
    sta $02aa

    ldy #0
    ldx #$3f        ;bank 1
    jsr k_fetch
    sta vms_alpha 

    iny
    ldx #$3f        ;bank 1
    jsr k_fetch
    
    ; X holds the nr of commands to execute
    tax
    iny

    sty offset_1

    ;add target-offset to vram-target address    
    ;will need to be made ready for indfet later, I guess
-   stx arg_loop

    jsr remember_mem_conf   ;also sets mmu to block 0
    ldy offset_1

    ; read vms-command byte (1=block copy with target-addr-increase and length, 2=OR with target-addr increase (length implicitly 1))
    lda (arg_address),y
    iny

;------- block copy
    cmp #1  ;block copy
    bne +

    clc
    lda (arg_address),y
    adc arg_address2
    sta arg2
    iny

    ;lda (arg_address),y
    lda #0
    adc arg_address2+1
    sta arg2+1
    ;iny

    ;read word 2 -> set arg3    
    clc
    lda (arg_address),y
    sta arg3
    iny

    ;lda (arg_address),Y
    lda #0
    sta arg3+1
    ;iny
    sty offset_1

    ldy #1
    sty arg4

    ;execute vmc with 3 params
    jsr block_copy_target

;--------- OR value
+   cmp #2  ;or-value
    bne +


    ; check for more vms parameters
    ldx arg_loop
    dex
    ;stx arg_loop is done at -
    bne -

+   rts

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

!zone VRAM_to_RAM

vtr ; copy VRAM to RAM
    jsr complex_instruction_shared_entry
    ldy arg1
    lda arg1 + 1
    jsr rtv_vtr_swp_shared_setup
    jsr vram_to_ram
    jmp complex_instruction_shared_exit


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
      ldx #0  ; ROMs and I/O
      ; loop to copy a single character pattern
--        ; read byte from RAM
        sta $ff01 ; full RAM (A is dummy)
        lda (arg1), y
        ; increment RAM pointer
        iny
        beq .fix_hi
.back       ; write byte to VRAM
        stx $ff00 ; ROMs and I/O
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

    ;$877b writes string address to $24/$25
    
    ; prepare for indirect FETCH
    lda #$24
    sta $02aa

    ;$877b writes string length to A, which is stored to $6 by JSRFAR
    lda $6
    sta vmp_length

    ;arg3(count16) is not changed in VMC, so we can set it here already
    lda arg_charset_width
    sta arg3
    ;setting arg3+1 to zero below, when we have the value handy anyways

    ;read virtual screen width from register 1 and store it in arg5 (arg4 would work, too. but for VMC it's arg5 anyways)
    ;  we could do this in VCS as well. but that would require another persistent byte.
    ;  it's sufficiently fast here, I guess
    ldx #1
    jsr vdc_reg_X_to_A
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
    ldx #$7f        ;bank 1
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

    ;calc size of each character in bytes (width*height)
    lda #0
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
;  7: vmw with 4 parameters (reserved, not yet implemented)
;  8: vmo with 2 parameters
;  9: vmo with 4 parameters (reserved)
; 10: vma with 2 parameters
; 11: vma with 4 parameters (reserved)
; 12: vms with 3 parameters
; ff: sleep with 1 parameter (jiffies to sleep)
; 
;  bytes after that are the parameters (can be 1 or 2 bytes each)
;  after last parameter, next byte is checked.
;  0 leads to end of vcl instruction
;  >0 interprets next bytes according to the value here (ie 1-7)


;  vcl parameters: <address of list>,[<bank of list-address>]
!zone command_list {
vcl
    jsr b_parse_uint16
    sty arg_address
    sta arg_address + 1

    jsr chrgot
    beq ++

    jsr b_skip_comma
    jsr b_parse_uint8_to_X
    
    ; check X-register and set MMU-mapping for chosen bank.
    txa
    bne +   ;not zero, check for 1
    ldx #$3f
    jmp ++

+   cpx #1
    bne +   ;not 1, goto default handling
    ldx #$7f
    jmp ++

+   ldx #01 ; default case, this sets bank 14
    jmp .vcl_command

++  stx arg_bank

; --- interpret commands ---
.vcl_command
    lda #arg_address
    sta $02aa

    ldy #0
    sty offset_1

.vcl_next_command
    ldy #0
    sty offset_2

    ; clear args
    ldx #11
    tya
-   sta arg1,x
    dex
    bpl -

; read first byte (command type)
    ldy offset_1
    ldx arg_bank
    jsr k_fetch
    bne +
    jmp .vcl_done           ; acc contains zero. the VCL is done

+   sta vcl_command_id
    inc offset_1

    ;  1,2,3: vmc
    ;  4,5: vmf
    ;  6,7: vmw

    tax
    lda vcl_parameter_bytes,x
    sta arg_loop

-   ldy offset_1
    jsr k_fetch
    inc offset_1

    ldx offset_2
    sta arg1,x
    inc offset_2
    
    cpx arg_loop
    bne -
    
    ; VMC (1-3)
    lda #3
    cmp vcl_command_id
    bmi +
    jsr remember_mem_conf
    jsr vmc_execute
    jmp .vcl_next_command

    ; VMF (4-5)
+   lda #5
    cmp vcl_command_id
    bmi +
    jsr remember_mem_conf
    ldy arg3
    jsr vmf_execute
    jmp .vcl_next_command

    ; VMW (6-7)
+   lda #7
    cmp vcl_command_id
    bmi .vcl_handle_unknown

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
    beq .vcl_next_command      ; if no more repitions, go to end of command

    clc
    lda arg1
    adc arg4
    sta arg1

    lda arg1+1
    adc arg4+1
    sta arg1+1

    jmp .vcl_vmw_next

    ;handle unknown command-id
.vcl_handle_unknown
    jsr k_primm
    !pet "Invalid"
    !byte 0
    rts

.vcl_done
    rts

}

!if release_vdl {
; draw line (x1,y1,x2,y2,color)
vdl
    rts
}

!if release_vdr {
; draw rectangle (x1,y1,x2,y2,<color>,<fill>)
vdr
    jsr complex_instruction_parse3args
    
    ;y2
    jsr b_parse_comma_uint16
    sty arg4
    sta arg4+1

    ;color
    jsr b_skip_comma
    jsr b_parse_uint8_to_X
    stx arg5

    ;assume fill for now
    ;jsr chrget

    ;line width in bytes (usually 80)
    ldx #1
    jsr vdc_reg_X_to_A
    sta arg6

    ;multiply y1 with line width
    ;add x1 to that
    ; this is the start-address -> arg1

    ;color is arg5 --> arg2

    ;subtract x1 from x2
    ; this is the length --> arg3

    ;subtract y1 from y2
    ; this is the nr of repetitions

    ;address-increment is arg6

    rts
}

; draw box 4 x/y pairs
vdb
    rts

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

vmp_length          !byte 0 ; length of the text to print

vcl_parameter_bytes !byte 0,5,9,11,5,9,3,7    ;first byte is dummy-byte. values -1 because of end-loop check (bne)