;ACME 0.94.2

!source <cbm/c128/vdc.a>

; check VRAM capacity
!macro addcode_vdc_check_capacity {
	!ifndef vdc_check_capacity {vdc_check_capacity}
	!if vdc_check_capacity = * {
		; on 16 KiB hardware, lower and upper half of address space access the same memory
		; on 64 KiB hardware, lower and upper half of address space access different memory
		; the RAM chip type register's bit value does not matter for this!
		; therefore, this algo should do the trick:
		; read and remember value from upper memory ($bfff)
		ldy #$ff
		lda #$bf
		jsr vram_AAYY_to_A	; Y kept
		pha
		; overwrite with inverted value from corresponding lower memory ($3fff)
		;ldy #$ff
		lda #$3f
		jsr vram_AAYY_to_A	; Y kept
		sta .old
		eor #$ff
		;ldy #$ff
		ldx #$bf
		jsr A_to_vram_XXYY	; Y kept
		; check whether lower memory has magically changed
		;ldy #$ff
		lda #$3f
		jsr vram_AAYY_to_A	; Y kept
.old = * + 1:	eor #MODIFIED8	; this should give zero or 255
		;beq .64k	; value hasn't changed, so we have 64 KiB
		;bne .16k	; value has changed, so we have 16 KiB
		sta .result
		; now we restore the upper memory location - in case of 16 KiB, this will also restore the changed lower memory location
		pla
		;ldy #$ff
		ldx #$bf
		jsr A_to_vram_XXYY
.result = * + 1:ldx #MODIFIED8
		inx	; we return 0 for 16 KiB, 1 for 64 KiB
		txa
		rts
	}
	+addcode_vdc_read
	+addcode_A_to_vram_XXYY
}


; write A to VDC RAM address XXYY
!macro addcode_A_to_vram_XXYY {
	!ifndef A_to_vram_XXYY {A_to_vram_XXYY}
	!if A_to_vram_XXYY = * {
		pha
		txa
		jsr AY_to_vdc_regs_18_19
		ldx #31
		pla
		jmp A_to_vdc_reg_X
	}
	+addcode_vdc_reg_write
}


; write to VDC registers
!macro addcode_vdc_reg_write {
	!ifndef AY_to_vdc_regs_18_19 {AY_to_vdc_regs_18_19}
	!if AY_to_vdc_regs_18_19 = * {
AY_to_vdc_regs_18_19 ; write A and Y to consecutive VDC registers 18 and 19 (VRAM address)
		ldx #18
AY_to_vdc_regs_Xp1 ; write A and Y to consecutive VDC registers X and X+1
		jsr A_to_vdc_reg_X
		tya
		inx
A_to_vdc_reg_X ; write A to VDC register X
		stx vdc_reg
A_to_vdc_data ; write A to currently selected VDC register
		+vdc_sta
		rts
	}
}


; read from VDC registers or VRAM
!macro addcode_vdc_read {
	!ifndef vram_AAYY_to_A {vram_AAYY_to_A}
	!if vram_AAYY_to_A = * {
vram_AAYY_to_A ; read VDC RAM address AAYY into A
		jsr AY_to_vdc_regs_18_19
vram_to_A ; read VDC register 31 (VRAM data) into A
		ldx #31
vdc_reg_X_to_A ; read VDC register X into A
		stx vdc_reg
vdc_data_to_A ; read currently selected VDC register to A
		+vdc_lda
		rts
	}
	+addcode_vdc_reg_write
}


; you must define VDC_COUNTER (16bits), preferably in zero page
; VDC_RAM0_READPTR must be 16bits in zero page
; VDC_RAM0_WRITEPTR must be 16bits in zero page

; for BLOCK-COPY operations, SET flag bit and write to registers 18/19 and 32/33
; for BLOCK-WRITE operations, CLEAR flag bit and write to registers 18/19 and 31
; this can be used for filling and copying
; when doing a WRITE operation, remember to do the first write yourself and pass a decremented counter!

!macro addcode_vdc_do_YYAA_cycles {
	!ifndef vdc_do_YYAA_cycles {vdc_do_YYAA_cycles}
	!if vdc_do_YYAA_cycles = * {
		ldx #30	; cycle register
		stx vdc_reg
		tax	; check low byte
		beq +
			+vdc_sta	; copy/write partial page
+		tya	; check high byte
		beq +
			; copy/write whole pages
			lda #0
-				+vdc_sta
				dey
				bne -
+		rts
	}
	+addcode_vdc_reg_write
}


; copy -VDC_COUNTER bytes from RAM to VRAM
; VDC_COUNTER must be negative!

; set VDCLIB_OPTION_BANKING to add code to access RAM 0 under I/O (slower)
; set VDCLIB_OPTION_SELFMOD to assemble self-modifying version (faster)
; you must define VDC_RAM_READPTR (16bits), it must be in zero page unless using VDCLIB_OPTION_SELFMOD

!macro addcode_ram_to_vram {
	!ifndef ram_to_vram {ram_to_vram}
	!if ram_to_vram = * {
		; get lowbyte into Y and clear base pointer's lowbyte instead
		ldy VDC_RAM_READPTR
		!if VDCLIB_OPTION_BANKING | (VDCLIB_OPTION_SELFMOD = 0) {
			ldx #0	; all ROMs and I/O, also new low-byte of zp ptr
		}
		!if VDCLIB_OPTION_SELFMOD {
			lda VDC_RAM_READPTR + 1
			sta .hi
		} else {
			stx VDC_RAM_READPTR
		}
---				; read byte from RAM
				!if VDCLIB_OPTION_BANKING {
					sta $ff01	; full RAM (A is dummy)
				}
				!if VDCLIB_OPTION_SELFMOD {
.hi = * + 2	:			lda $fe00, y	; high-byte will get MODIFIED
				} else {
					lda (VDC_RAM_READPTR), y
				}
				!if VDCLIB_OPTION_BANKING {
					stx $ff00	; ROMs and I/O
				}
				; write byte to VRAM
				+vdc_sta
				; increment RAM pointer
				iny
				beq .fix_hi
.back				; check whether done
				inc VDC_COUNTER
				bne ---
			inc VDC_COUNTER + 1
			bne ---
		rts

.fix_hi		!if VDCLIB_OPTION_SELFMOD {
			inc .hi
		} else {
			inc VDC_RAM_READPTR + 1
		}
		jmp .back
	}
}


; copy -VDC_COUNTER bytes from VRAM to RAM
; VDC_COUNTER must be negative!

; set VDCLIB_OPTION_BANKING to add code to access RAM 0 under I/O (slower)
; set VDCLIB_OPTION_SELFMOD to assemble self-modifying version (faster)
; you must define VDC_RAM_WRITEPTR (16bits), it must be in zero page unless using VDCLIB_OPTION_SELFMOD

!macro addcode_vram_to_ram {
	!ifndef vram_to_ram {vram_to_ram}
	!if vram_to_ram = * {
		; get lowbyte into Y and clear base pointer's lowbyte instead
		ldy VDC_RAM_WRITEPTR
		!if VDCLIB_OPTION_BANKING | (VDCLIB_OPTION_SELFMOD = 0) {
			ldx #0	; all ROMs and I/O, also new low-byte of zp ptr
		}
		!if VDCLIB_OPTION_SELFMOD {
			lda VDC_RAM_WRITEPTR + 1
			sta .hi
		} else {
			stx VDC_RAM_WRITEPTR
		}
---				; read byte from VRAM
				+vdc_lda
				; write byte to RAM
				!if VDCLIB_OPTION_BANKING {
					sta $ff01	; full RAM (A is dummy)
				}
				!if VDCLIB_OPTION_SELFMOD {
.hi = * + 2	:			sta $fe00, y	; high-byte will get MODIFIED
				} else {
					sta (VDC_RAM_WRITEPTR), y
				}
				!if VDCLIB_OPTION_BANKING {
					stx $ff00	; ROMs and I/O
				}
				; increment RAM pointer
				iny
				beq .fix_hi
.back				; check whether done
				inc VDC_COUNTER
				bne ---
			inc VDC_COUNTER + 1
			bne ---
		rts

.fix_hi		!if VDCLIB_OPTION_SELFMOD {
			inc .hi
		} else {
			inc VDC_RAM_WRITEPTR + 1
		}
		jmp .back
	}
}
