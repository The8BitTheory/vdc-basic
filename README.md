# VDC-BASIC

A BASIC 7.0 extension for the Commodore 128 that opens up the 80-column chip (VDC 8563/8568). It adds 23 instructions and 2 functions for reading and writing VDC registers and video RAM, and for filling, copying and blitting inside video RAM at the speed of the chip.

Current version: **v3** (in development), file `vdcbasic3.bin`, load address `$1C00`. BASIC programs start at `$4001`; the boot loader `boot/boot.bas` (on disk as `BOOT`) sets this up, loads and installs VDC-BASIC:

```
RUN "BOOT"
```

The guide and its listings still describe v2h (`vdcbasic2h.bin` at `$1300`, `BLOAD "VDCBASIC2H",B0 : SYS DEC("1300")`); they move to v3 in a later step.

### Memory and loading (v3)

| Bank 0 | Use |
|---|---|
| `$1300`–`$1BFF` | free (e.g. for lists that must lie below `$4000`) |
| `$1C00`–`$1C0F` | header with fixed addresses: `$1C00` `JMP install` (`SYS 7168`), `$1C03` version (`$30` = v3.0), `$1C04` block fill threshold, `$1C06`–`$1C09` results of `RXV` (planned), the rest reserved |
| `$1C10`–`$2505` | code and variables of VDC-BASIC; up to `$3FFF` is reserved for it |
| `$4000` | 0 (the byte before the program text) |
| `$4001`– | BASIC program, or a Blitz! program with runtime image 2 |

`BOOT` runs twice: started at `$1C01`, it sets the start of BASIC text to `$4001` (`$2D/$2E`) and loads itself there with `RUN"BOOT"`; the second pass loads `VDCBASIC3` to `$1C00`, installs it with `SYS 7168`, checks the version byte and ends with `NEW`. To start a program instead, replace line 50 with `50 RUN"PROGRAM"`. Put `BOOT` first on the disk so that `-autostart disk.d64` in VICE (which loads the first file) works; `tools/mkboot.py` writes a C128 boot sector that runs `BOOT` when the machine is switched on with the disk in the drive.

**`GRAPHIC 1`–`4` must not be used with v3**: the VIC bitmap (`$2000`–`$3FFF`) and its colour memory (`$1C00`–`$1FFF`) overwrite VDC-BASIC. `GRAPHIC 0` and `GRAPHIC 5` are fine, `GRAPHIC CLR` is not.

This file is the technical view: what is in the repository, how to build and test it, how the extension hooks into BASIC. How to *use* the commands is the subject of the guide.

## Documentation

| | |
|---|---|
| [Programmer's Guide](docs/README.md) | 14 chapters that build on each other, from the first register write to a small game. Every listing is tested. |
| [Command reference](docs/reference/commands.md) | All commands with syntax, ranges and limits |
| [VCL list format](docs/reference/vcl-format.md), [VMS list format](docs/reference/vms-format.md) | Byte layout of command lists and sprite lists |
| [Converter](docs/tools/README.md) | Browser page and command line tool that turn pictures into files for VDC-BASIC programs; file formats |

## Repository layout

| Path | Content |
|---|---|
| `src/vdcbasic.asm` | The extension: BASIC hooks, parameter parsing, all commands |
| `src/vdclib.a` | VDC access routines as ACME macros (register and VRAM access, block operations, RAM ↔ VRAM transfer). Usable without the BASIC part. |
| `vdcbasic3.bin` | Current binary (v3, `$1C00`), assembled from `src/` |
| `boot/boot.bas` | Boot loader for v3 (`BOOT`) |
| `vdcbasic2h.bin` | v2h (`$1300`), used by the guide and by `test/bench` |
| `vdcbasic.bin`, `vdcbasicac6.bin`, `vdcbasic2c.bin` | Binaries of earlier versions |
| `BASIC V7.0 VDC.txt` | Keyword and token table: a BASIC dialect file for RetroDevStudio / C64 Studio, also read by `test/bastok.py` |
| `docs/` | The guide, the reference, listings, screenshots, source pictures, converter |
| `test/` | Tokeniser, emulator test runner, behaviour tests |
| `tools/` | Earlier converters that run on the C128 itself (BMP to VDC data, transparency lists). Written for v2f; their file formats differ from the ones used in the guide. |
| `examples/` | Demo programs and disk images from earlier versions |

## Building

The source is written for the [ACME](https://sourceforge.net/projects/acme-crossass/) cross assembler and uses its standard library (`6502/std.a`, `6502/opcodes.a`, `cbm/c128/kernal.a`, `cbm/c128/vdc.a`).

```
ACME=/path/to/ACME_Lib acme src/vdcbasic.asm
```

This writes `vdcbasic3.bin` into the current directory (`acme -l build/vdcbasic3.sym …` also writes the symbol list that the monitor scripts in `test/` use).

Things to know before changing the code:

- **Size.** The code must end below `$4000`, where BASIC ROM and the program text begin; an assertion at the end of the source checks this. v3 occupies `$1C00`–`$2505`; 6906 bytes are left.
- **Load address and header.** `* = $1c00` near the top of the source, followed by the 16-byte header. Its addresses are an interface (programs `PEEK` and `POKE` them); assertions check that every field stays where it is.
- **BASIC ROM.** Parts of the code run with the BASIC ROM mapped in and must therefore stay below `$4000`; the macro `vdclib_assert_below_basic_rom` checks this at assembly time.
- **Unfinished parts.** `VDL`, `VDR` and `VDB` (line, rectangle, box) exist as stubs behind the build switches `release_vdl`, `release_vdr`, `release_vdb` and are switched off. Token `$FE $3A` is a placeholder and must stay in the table so that the later tokens keep their numbers.

## How it hooks into BASIC

BASIC 7.0 has four vectors that exist for extensions. `SYS 7168` (`SYS DEC("1C00")`) points them at VDC-BASIC and prints the installation message:

| Vector | Called by BASIC when |
|---|---|
| `$030C` | a line is tokenised and a word is not one of its own keywords |
| `$030E` | a line is listed and contains a token it does not know |
| `$0310` | an unknown instruction token is to be executed |
| `$02FC` | an unknown function token is to be evaluated |

BASIC 7.0 reserves the prefixes `$FE` (instructions) and `$CE` (functions) for two-byte tokens and uses them up to `$FE $26` and `$CE $0A`. VDC-BASIC continues from there:

| Tokens | Keywords |
|---|---|
| `$FE $27`–`$FE $3D` | `RGW RGA RGO VMW VMA VMO VMF VMC RTV VTR VCC SWP RST SYN DISP ATTR CRSR VCS VMP` (placeholder) `VCL VMS VMB` |
| `$CE $0B`, `$CE $0C` | `RGD`, `VMD` |

A program that contains the new tokens lists and runs correctly only while VDC-BASIC is installed.

The commands that move blocks of memory switch to a configuration with RAM bank 0 and I/O visible (`$3E` in the MMU register) and restore the caller's configuration afterwards. That is why all RAM addresses given to `RTV`, `VTR`, `SWP`, `VCC`, `VMB`, `VCL` and `VMS` refer to bank 0, whatever `BANK` says.

The zero page and other locations the extension uses are listed at the end of the [command reference](docs/reference/commands.md#memory-used-by-vdc-basic).

## Tests

```
test/run.sh test/*.bas                           # behaviour tests, interpreted
test/run.sh -c test/*.bas                        # the same tests compiled with Blitz! (runtime image 2)
test/run.sh -l -d docs/img docs/listings/*.bas   # the listings of the guide (still v2h), with screenshots
test/boot_sector.py                              # BOOT via the C128 boot sector and via -autostart disk.d64
```

Each test is a BASIC listing. `test/bastok.py` tokenises it, `x128` from [VICE](https://vice-emu.sourceforge.io/) runs it headless in warp mode with a 64 KiB VDC, and the listing reports its result as an exit code through VICE's debug cartridge. Nothing waits for a fixed time; the cycle limit only guards against hangs. A run takes about two seconds.

Requirements: `x128` and `c1541` from VICE, Python 3, Pillow for the screenshots. Details in [`test/README.md`](test/README.md).

The tests cover the behaviour the reference describes, the worked examples of the two list formats, statements made in the text of the guide, and the defects fixed in v2h.

## Compiling programs

Programs that use VDC-BASIC can be compiled with the Blitz BASIC compiler for the C128. The only requirement is that every VDC-BASIC instruction is prefixed with two colons:

```
100 ::VMC SH,P+Y*80+X,W,H,80
```

To the interpreter the two colons are empty statements, so the same source also runs uncompiled.

With v3 the compiled program must use runtime image 2, which loads at `$4001`, above VDC-BASIC. The compiler chooses image 2 when the program contains a `GRAPHIC` statement; a line such as `63999 GRAPHIC 0` that is never executed is enough. Image 1 (load address `$1C01`) copies the program to `$1C00` and destroys VDC-BASIC. Load the compiled program through `BOOT` (`50 RUN"PROGRAM"`). Further findings (`docs/v3/P2a-relocate.md`, section 9):

- An assignment inside an extension reaches the compiled program: `::A=VMD(2000)` works, while `VMD`/`RGD` in a compiled expression are a syntax error for the compiler.
- An error inside an extension is caught by `TRAP` with the right `ER`. After that, however, extensions no longer work correctly in the compiled program.

## Known limits

- `DISP`, `ATTR` and `CRSR` accept 0–65535 only; negative values are not wrapped.
- `VCL` entry types 8–11 (`VMO`/`VMA`) and a "sleep" entry are planned in the source comments and not implemented.
- Parameters are checked for their range and for nothing else: no clipping, no check that a target lies inside the picture.
- The repetition count of `VMC`/`VMF` is one byte; 0 means 256.

The guide collects the practical consequences in [chapter 13](docs/13-speed-and-limits.md).

## History

| Version | Date | Changes |
|---|---|---|
| vdc-tool 2 | about 1993 | Original by Felix 'skOi!nk' Rosenhahn |
| vdcbasic | Nov 2013 – Mar 2014 | Re-release by Mac Bacon: renamed, split into BASIC part and VDC library, loops optimised for speed, several bugs fixed; `RST` with flag bits; pseudo registers 254 (chip version) and 255 (VRAM size); `SYN`, `DISP`, `ATTR`, `CRSR`. Published in the [Forum64 thread "VDC-Basic Re-Release"](https://www.forum64.de/index.php?thread/54960-vdc-basic-re-release) on 27 Nov 2013, updated 16 Mar 2014. |
| v2c | Sep 2023 | `VMC`: repetitions and target step |
| v2d | Feb 2024 | `VMC`: source step |
| v2e | May 2025 | `VCS` and `VMP`: text in bitmap modes |
| v2f | Jun 2025 | `VMC`: negative steps |
| v2g | Oct 2025 | `VMB`; row length parameter for `VCS`; `VMP` with an empty string; `VCL` and `VMS` |
| v3 | (in development) | Moved to `$1C00`, header with fixed addresses at `$1C00`–`$1C0F`, BASIC text at `$4001`, boot loader `BOOT`. Same commands as v2h. |
| v2h | Nov 2026 | Fixes: `VCL` lists longer than 256 bytes; `VMS` lists above `$4000`; `VMS` entry type 2 no longer inherits steps from `VMC`; `VMB` with length 0; `VCL` type 7 with step 0; `VCL` with an unknown entry type no longer hangs. New: `VCL` entry type 13 (`SYN` with register/value pairs, e.g. an atomic page flip inside a list), optionally without waiting when the border has already begun. Tests, guide and reference added. |

## Credits

- **Felix 'skOi!nk' Rosenhahn** wrote the original, "vdc-tool 2", around 1993.
- **Mac Bacon** turned it into vdcbasic in 2013/2014: the cleaned-up and optimised code, the VDC library `vdclib.a`, and the commands listed above.
- **Goodwell** (The 8-Bit Theory) has written everything since v2: the extended `VMC` and `VMF`, `VCS`, `VMP`, `VMB`, `VMS`, `VCL`, the tests and the documentation.

## License

The work of Goodwell in this repository, that is all code added since v2, the tests, the tools and the documentation, is released under the [MIT License](LICENSE).

The parts that go back to Felix Rosenhahn and Mac Bacon were published without a license statement: the original was passed on from disk to disk in the 1990s, the re-release was posted in a public forum for everyone to use. They are included here in that spirit and with credit, but the MIT License cannot speak for them. If you are one of the two authors and would like this handled differently, please open an issue.
