This VDC-Basic version is taken from https://www.forum64.de/index.php?thread/54960-vdc-basic-re-release

It was initially published there on November 27, 2013.
An update to it was published on March 16, 2014.

This was improved from it's original version and published by Forum64 user Mac Bacon.
The original version was created by Felix 'skOi!nk' Rosenhahn around the year 1993.

The current version is VDC-Basic v2c.
This extends the VMC command for two additional parameters: repetitions and target-address-increase per repetition

vdcbasic.bin is compiled for address $1300.
vdcbasicac6.bin is compiled for address $0ac6.


# Functions:
	rgd(register)	read VDC register
	vmd(address)	read VDC RAM

		reading fake register 254 returns VDC version:
			0 for 8563 rev 7A,
			1 for 8563 rev 8/9,
			2 for 8568

		reading fake register 255 returns VDC RAM capacity:
			0 for 16 KiB,
			1 for 64 KiB
		(this will write to address $bfff, but will restore the
		previous contents immediately)

# Instructions:
	rgw REGISTER, BYTE	VDC register = value
	rga REGISTER, BYTE	VDC register = VDC register AND value
	rgo REGISTER, BYTE	VDC register = VDC register OR value

	vmw ADDRESS, BYTE	VDC RAM location = value
	vma ADDRESS, BYTE	VDC RAM location = VDC RAM location AND value
	vmo ADDRESS, BYTE	VDC RAM location = VDC RAM location OR value

	vmf VRAM_TARGET, BYTE, COUNT16		fill VDC RAM with value
	vmc VRAM_SOURCE, VRAM_TARGET, COUNT16	copy within VDC RAM
	vmc VRAM_SOURCE, VRAM_TARGET, COUNT16, REPETITIONS, TARGET-INCR
		copy within VDC RAM.
		Repetition repeats the copy command the defined number of times.
		Source address keeps increasing continuously, target address is increased by defined amount each repetition
		Designed for being used with soft-sprites in mind
	vcs VRAM_ADDRESS_CHARSET, CHAR_WIDTH, CHAR_HEIGHT	setup charset for graphic printing via VMP (width in bytes, height in scanlines)
        vmp VRAM_TARGET_ADDRESS, String		print the given String at the specified VRAM address. VCS needs to be called before for correct behavior
	vmc VRAM_SOURCE, VRAM_TARGET, COUNT16, REPETITIONS, TARGET-INCR, SOURCE-INCR
                SOURCE-INCR allows to change how the source address is incremented. Can be left out if it's supposed to be the same as COUNT16
	rtv RAM0_SOURCE, VRAM_TARGET, COUNT16	copy from CPU RAM to VDC RAM
	vtr VRAM_SOURCE, RAM0_TARGET, COUNT16	copy from VDC RAM to CPU RAM
	vcc RAM0_SOURCE, VRAM_TARGET, COUNT8	copy charset patterns from CPU RAM to VDC RAM, adding eight-byte gaps
	swp VRAM_START, RAM0_START, COUNT16	exchange parts of VDC RAM and CPU RAM
	rst FLAGBITS			"rst 1" resets registers (including r37),
					"rst 2" resets charset
					(so "rst 3" resets both)
	syn [REG, BYTE [, REG, BYTE]*]	wait for end of display window, then
					write values to VDC registers
	disp ADDRESS		set start of display	(rgw 12, HI: rgw 13, LO)
	attr ADDRESS		set start of attributes	(rgw 20, HI: rgw 21, LO)
	crsr ADDRESS		set cursor address	(rgw 14, HI: rgw 15, LO)
