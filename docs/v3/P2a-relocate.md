# P2a: VDC-BASIC to `$1C00`, boot loader, Blitz! image 2 test

As of: 2026-11-25. Detailed specification for step P2a from `dungeonboringengine/docs/plan-0.2.md` (sections 3 and 7.1). Implementation: Opus. Review: Opus. Prerequisite for P2b (`RXV`) and P2c (`VDM`/`VDL`/`VDR`/`VDB`).

## 1. Goal and result

- VDC-BASIC runs from `$1C00` (bank 0) instead of from `$1300`. Space: `$1C00–$3FFF` = 9216 bytes (v2h: 2294 bytes).
- BASIC text starts at `$4001`. A boot loader sets this up, loads and installs VDC-BASIC and optionally starts a program.
- The same program runs interpreted and as a Blitz! compilate with runtime image 2 (`$4001`).
- A fixed header at `$1C00` contains the entry point, version and the patchable bytes for P2b/P2c.
- This step does not change the behavior of any command. New commands come only with P2b/P2c.

File: `vdcbasic3.bin`, load address `$1C00`. `vdcbasic2h.bin` remains unchanged in the repo (P0 uses it).

## 2. Changes in the source (`src/vdcbasic.asm`)

| Place | today | new |
|---|---|---|
| Output file | `!to "vdcbasic2h.bin", cbm` | `!to "vdcbasic3.bin", cbm` |
| Origin | `* = $1300` | `* = $1C00`, then the header (2.1), installation code from `$1C10` |
| Message | `vdc basic v2h installed` | `vdc basic v3 installed` |
| Version comment | – | entry "v3, 2026: relocated to `$1C00`, header with fixed addresses" |
| Build switches | `release_vdl/vdr/vdb = 0` | stay 0 in P2a; P2b replaces them with fixed entries with a stub (2.2) |
| Size check | no overall check | at the end of the file: `!if * > $4000 { !error "VDC-BASIC v3 endet bei ", *, ", erlaubt bis $3FFF" }` |
| Header check | – | before each header field `!if * != $1C0x { !error … }` (addresses are an interface) |
| `+vdclib_assert_below_basic_rom` | 3 calls | stay; they are covered by the overall check but do no harm |

Absolute addresses in the code: there are no references to `$1300–$1BFF` except via labels. The ROM entry points (`$43E2`, `$4321`, `$43B2`, `$516A`, `$7956`, `$8815`, `$80DB`, `$877B`, `$87F4`, `$8803`, `$880F`, `$8812`, `$795C`, `$77EF`, `$77DA`, `$849F`, `$C027`, `$E1DC`, `$E192`), the Kernal cells (`$02A2`, `$02AA`, `$02CD`, `$02FC`, `$030C–$0311`) and the zero page stay the same. Self-modifying code (`.SELFMOD`, `.cr`, `.spbuf`, `.lowest_sp`) moves along, which is uncritical (RAM).

The code must lie completely below `$4000`: parts run with `$FF00 = $00` (BASIC ROM from `$4000` mapped in), and the installation program is called with `BANK 15` via `SYS`.

### 2.1 Header (fixed addresses, interface)

| Address | Size | Content | set by |
|---|---|---|---|
| `$1C00` | 3 | `JMP install` (`SYS DEC("1C00")` = `SYS 7168`) | P2a |
| `$1C03` | 1 | Version, BCD: `$30` = v3.0 | P2a |
| `$1C04` | 1 | **Threshold T** for block fill (`RXV`, spans of `VDL`/`VDR`/`VDB`), default see P2b, 5 | P2b |
| `$1C05` | 1 | reserved, 0 | – |
| `$1C06–$1C07` | 2 | `RXV`: address after the end token of the most recently unpacked stream | P2b |
| `$1C08–$1C09` | 2 | `RXV`: VRAM address after the last written or skipped byte | P2b |
| `$1C0A–$1C0F` | 6 | reserved, 0 | – |

In P2a, `$1C04–$1C0F` are laid out with `!byte 14, 0, 0,0, 0,0, 0,0,0,0,0,0` so that the addresses are fixed from now on. All fields lie below `$4000` and are therefore reachable with `BANK 15` via `PEEK`/`POKE` (reads and writes go to RAM 0).

### 2.2 Tokens

`$FE27–$FE3D` and `$CE0B/$CE0C` unchanged. Fixed for v3 (implementation P2b/P2c, only documented here):

| Token | Command | Origin |
|---|---|---|
| `$FE3A` | placeholder `zzZ` | stays free: the second byte `$3A` is `:`, BASIC and Blitz! would read it as a separator when skipping statements |
| `$FE3E` | `VDL` | order in the source since `e4d1832` (after `VMS`, `VMB`) |
| `$FE3F` | `VDR` | ditto |
| `$FE40` | `VDB` | ditto |
| `$FE41` | `RXV` | new (P2b) |
| `$FE42` | `VDM` | new (P2c) |

The conditions `!if release_vdl` etc. must be set **all three** when switching on, otherwise the numbers of `RXV`/`VDM` shift. P2b therefore rebuilds the tables so that `VDL`/`VDR`/`VDB` are always entered; until P2c they point to a common stub `ldx #11 : jmp ($0300)` (BASIC error vector, error 11 = `?SYNTAX ERROR`; runs with `$FF00 = 0` like `execute_instruction`). The switches `release_vdl/vdr/vdb` then go away.

### 2.3 Token list and tokenizer

- `BASIC V7.0 VDC.txt`: comment "vdcbasic v3", the lines `VDL;fe3e;`, `VDR;fe3f;`, `VDB;fe40;`, `RXV;fe41;`, `VDM;fe42;` come with P2b/P2c.
- `test/bastok.py`: new option `--start ADDR` (default `$1C01`, as today) so that test programs are produced with correct line pointers for `$4001` (3.3). The invocation stays compatible: `bastok.py [--start 0x4001] in.bas out.prg`.

## 3. Boot loader

### 3.1 Problem

A BASIC program at `$1C01` cannot load VDC-BASIC to `$1C00` without overwriting itself while it is running. The loader therefore first sets the BASIC start to `$4001`, reloads **itself** there and only then loads VDC-BASIC.

### 3.2 File `boot/boot.bas` (one file, two passes)

```
10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:END
30 SYS 7168
40 IF PEEK(DEC("1C03"))<>DEC("30") THEN PRINT"WRONG VDC-BASIC":END
50 NEW
```

Flow:

1. `RUN` (by hand, via autostart or boot sector) starts `BOOT` at `$1C01`. `PEEK(46)` = `$1C` → line 10: `$4000 = 0`, text start `$2D/$2E = $4001`, `RUN"BOOT"` loads the same file to the new text start (BASIC 7 loads relative to `$2D/$2E`) and starts it.
2. Second pass at `$4001`: `PEEK(46)` = 64 → line 20 loads `VDCBASIC3` to its load address `$1C00` (does not touch `$4001+`), line 30 installs, line 40 checks the header.
3. Line 50: `NEW` ends and leaves an empty BASIC memory from `$4001`. For a game/program the user replaces line 50 with `50 RUN"PROGRAMM"` (interpreted or Blitz! compilate, 4).

Notes:

- `POKE 16384,0` writes with `BANK 15` into the RAM under the BASIC ROM (write accesses to ROM areas go to RAM 0).
- The variables remain in bank 1 from `$0400`; `$2D/$2E` affects only the text.
- Anyone using `GRAPHIC 1`–`4` (VIC bitmap) overwrites VDC-BASIC (color RAM `$1C00–$1FFF`, bitmap `$2000–$3FFF`). This is a new, hard restriction of v3 → README, reference "Loading", chapter 1 of the guide. `GRAPHIC 0`/`5` are allowed, `GRAPHIC CLR` is not.
- The P0 benchmark program (`test/bench`, `$2000–$7FFF`) continues to load v2h at `$1300` and is not affected by v3.

### 3.3 Autostart

| Kind | Implementation | Purpose |
|---|---|---|
| VICE | `x128 … -8 disk.d64 -autostartprgmode 1 -autostart boot.prg` (the tokenized `BOOT`, load address `$1C01`) | tests |
| VICE, disk image | `-autostart disk.d64`: loads the **first** directory entry → `BOOT` must be the first file on the disk | user |
| C128 boot sector | track 1, sector 0: `"CBM"`, address 0, bank 0, 0 additional blocks, message `"VDC-BASIC V3"`, `0`, empty file name `0`, then code `LDX #<cmd : LDY #>cmd : JMP $AFA5` with `cmd` = `RUN"BOOT"` + `0` in the sector buffer (`$0B00 + offset`) | real hardware, VICE without `-autostart` |

Boot sector tool: `tools/mkboot.py OUT.bin [--name BOOT] [--message "VDC-BASIC V3"]` produces the 256 bytes; write with `c1541 disk.d64 -bwrite OUT.bin 1 0` **after** writing the files (c1541 allocates track 1 only when the disk is full; the tool first checks with `-bread` that sector 1/0 is empty, otherwise abort).

**To verify (mandatory test `boot-sector`, 7.3):** format of the boot sector fields and the entry point `$AFA5` (BASIC: execute direct command from buffer). The information comes from the literature on the C128 Kernal (`BOOT_CALL`, `$F890`), not from this repo. Test without `-autostart`, with `-truedrive`/`+trapdevice8` (the Kernal reads the sector via block command), breakpoint on `$1C00` (installation reached) and on the BASIC error vector.

## 4. Memory map (bank 0)

| Area | v2h | v3 |
|---|---|---|
| `$0000–$12FF` | system, Kernal, BASIC | unchanged (engine: `$0B00–$0FFF` parameter block, see plan 3) |
| `$1300–$1BFF` | VDC-BASIC v2h (`$1300–$1BF6`) | **free**: user or engine buffers (VCL, VMS lists); the tests put lists "below `$4000`" here |
| `$1C00–$1C0F` | BASIC text | **VDC-BASIC header** (2.1) |
| `$1C10–` about `$2E00` | BASIC text | **VDC-BASIC v3 code and variables** (end address from the symbol list, enter in README and reference) |
| up to `$3FFF` | BASIC text | reserve for VDC-BASIC (the overall check allows up to `$3FFF`) |
| `$4000` | – | 0 (byte before the program text) |
| `$4001–` | BASIC text or data | BASIC text or Blitz! compilate (image 2: runtime `$4001–$6C36`, then header, bytecode, variables) |
| above program end | free | free, e.g. load buffer; readable with `$3E`, but not `$D000–$DFFF` (I/O) and `$FF00–$FF04` |

Bank 1: unchanged (BASIC variables from `$0400`).

**Common RAM:** The engine plans 8 KB common RAM at the bottom (`$0000–$1FFF`, plan 3). Then bank 1 `$1C00–$1FFF` is the same memory as VDC-BASIC. Consequence for P6: variables or data of the engine in bank 1 must not lie there (set the BASIC variable start in bank 1 to ≥ `$2000`). For VDC-BASIC itself nothing changes.

**Programs become 9 KB "higher":** A program that previously started at `$1C01` now ends `$2400` bytes further up. Fixed load buffers (Crawl: `BE` from `$94E8`) must lie above the new program end → open item from plan 3, belongs to P6/P7, only noted here.

## 5. Blitz! compilate with image 2

### 5.1 What exists

- `~/git/blitz-compiler/blitz128/blitzc/js/cli.js`: Blitz!-128 compiler as a Node program, headless, byte-identical to the original (51 reference runs, including the `$4001` variant). Default: improved runtime V261123 (contains the fix for the 10-byte-short image 2).
- Node is not in the `PATH` of non-interactive shells: `~/.nvm/versions/node/v26.13.1/bin/node`. The scripts look for `$NODE`, otherwise `node` in the `PATH`, otherwise `~/.nvm/versions/node/*/bin/node` (newest).
- Image 2 is chosen as soon as the program contains a `GRAPHIC` command (manual, directive `**NG`). There is no option for it. Example in the repo: `blitzc/tests/src/ri2.bas`.
- `::` before a statement = extension: the text up to the next `:` is passed at runtime to the interpreter, which calls VDC-BASIC via `$0310`. Normal variables are passed to extensions (`**FI` variables are not).
- The compilate `C/NAME` loads at `$4001`. Its loader checks `$2E`: if it does not contain `$40`, it copies the program to `$4000` – after loading at `$1C01`, VDC-BASIC would then already be overwritten. Therefore the boot loader always does the loading (line 50: `RUN"C/NAME"` or under any file name).

This makes the whole path automatable: compile with Node, run in `x128` like the interpreted tests, result via the exit code of the debug cartridge. A manual minimal path is not needed.

### 5.2 Preliminary experiment E1 (before all other changes, result into this file, section 9)

With v2h code, assembled only to `$1C00`, one small test program each, interpreted and compiled:

| Question | Check |
|---|---|
| E1a: Does the image 2 runtime touch `$1C00–$3FFF` or `$1300–$1BFF`? | Monitor: `watch store 1300 3fff` **after** the installation (breakpoint at the end of `install` activates the watchpoints); every hit with PC outside `$1C00–$3FFF` is a finding (hits with PC in the VDC-BASIC code are its own variables). |
| E1b: Does an assignment in an extension come back? | `10 A=1:::A=VMD(2000)+5:IF A=…` or a simpler form without function: `::A=7` and then `IF A<>7`. |
| E1c: Functions `VMD`/`RGD` in a compiled expression | Expectation: compiler error; record the message. |
| E1d: `TRAP`/`ER` on an error **inside** an extension (`::VMW 70000,1`) | Is `TRAP 9000` jumped to, is `ER` = 14? |
| E1e: Does a never-executed `GRAPHIC 0` in the last line suffice for image 2? | Load address of `c.prg` = `$4001`. |

The answers decide 5.3 (variant A or B).

### 5.3 Preprocessing `test/blitzprep.py`

`blitzprep.py in.bas out.bas` turns a test listing into a Blitz!-suitable version:

1. Every statement that begins with a VDC-BASIC command word (list from `BASIC V7.0 VDC.txt`, section "VDC Basic Tokens", only `$FE` tokens) gets `::` in front. Statement boundaries: `:` outside strings, start of line. If a VDC-BASIC command stands directly after `THEN` or `ELSE`, the tool aborts with the line number (exit code 2): such lines are rewritten in the test (`IF … THEN 500` with subroutine), because an extension reaches "to the next `:`" and the meaning inside the `IF` is not verified.
2. Function calls `VMD(…)`/`RGD(…)`:
   - **Variant A** (E1b: assignment comes back): every call is pulled in front of the statement: `IF VMD(2100)<>17 OR VMD(2101)<>34 THEN …` → `::Q0=VMD(2100):::Q1=VMD(2101):IF Q0<>17 OR Q1<>34 THEN …` (helper variables `Q0`…`Q9`, otherwise unused in the test; nested calls from the inside out). Only permitted if the statement does not contain a `THEN` part with further calls, otherwise abort as above.
   - **Variant B** (E1b negative): functions remain forbidden; affected tests read the VRAM via `::VTR adr,ram,n` into RAM 0 (`$1300+`) and check with `BANK 0:PEEK`. The tests are rewritten once by hand for this so that they run in both modes; `blitzprep.py` reports remaining function calls as errors.
3. At the end `63999 GRAPHIC 0` (never executed, forces image 2), if E1e confirms this; otherwise a form that E1 identifies as suitable.
4. `REM **NE` as the first line (suppresses the extension list).

### 5.4 Flow in the test run (`test/run.sh -c`)

```
python3 test/blitzprep.py t.bas tb.bas
python3 test/bastok.py tb.bas tb.prg                     # source for the compiler, start $1C01 like all references of blitzc
$NODE $BLITZC tb.prg $WORK/blitz --name T                # -> $WORK/blitz/c.prg, z.prg, screen.txt
# Check: load address of c.prg = $4001 (otherwise FAIL "not image 2"), screen.txt without error message
c1541 … -write $WORK/blitz/c.prg t                       # as "T" onto the disk, BOOT loads RUN"T"
```

`BLITZC` from the environment, default `$HOME/git/blitz-compiler/blitz128/blitzc/js/cli.js`. If Node or `cli.js` is missing: message `SKIP (blitzc not found)` and exit code 3 (not 0), so that a skipped compilate run does not count as green.

## 6. Tests and harness

### 6.1 `test/run.sh`

| Item | new |
|---|---|
| Binary file | `BIN=${VDCBASIC:-$HERE/../vdcbasic3.bin}`, on the disk as `vdcbasic3` |
| Disk | `boot` (from `boot/boot.bas`, line 50 replaced by `50 RUN"T"`) as the first file, `vdcbasic3`, `t` (test program, tokenized with `--start 0x4001`, or compilate), with `-l` the data of the listings |
| Start | `-autostart boot.prg` instead of `t.prg` |
| new option | `-c`: compilate run (5.4); `-l` and `-c` are mutually exclusive (the listings are only checked interpreted) |
| Rest | unchanged: debug cartridge, `-limitcycles` as guard, retry on 143, screenshot |

`run.sh` needs no monitor; the port rule (6.4) applies to the scripts with monitor (`boot-sector`, E1, P2b, P2c).

### 6.2 Test listings (`test/*.bas`)

Line 2 (`bload"vdcbasic2h",b0,u8:sys dec("1300")`) is dropped in all tests; it is replaced by

```
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
```

(checks that `BOOT` installed v3). Addresses that are occupied in v3:

| Test | today | new | Reason |
|---|---|---|---|
| `vms-list-at-3000.bas` | list at `$3000` | rename to `vms-list-at-1300.bas`, list at `$1300` | `$3000` is VDC-BASIC; the test should continue to check a list **below** `$4000` |
| `vcl-long-list.bas`, `vcl-vmw-increment0.bas`, `vms-type2-stale.bas` | `$3000` | `$1300` (lists > 256 bytes fit: space up to `$1BFF`) | ditto |
| `vmb-zero-length.bas` | `$3000` | `$1300` | ditto |
| `vms-list-at-5000.bas` | `$5000` | rename to `vms-list-at-a000.bas`, `$A000` | `$5000` may lie in the program text (from `$4001`); the test should check a list **above** `$4000` |
| `reference.bas`, `vcl-syn.bas`, `vcl-syn-level.bas`, `vcl-unknown.bas` | `$5000` | `$A000` | ditto |
| `reference-examples.bas` | `$4000` | `$A000` | `$4000` = byte before the program text |
| all | – | `$A000` also holds for compilates: runtime + bytecode of the tests end below `$7000` | |

A test **`layout.bas`** is new: checks `PEEK(45)=1`, `PEEK(46)=64`, `PEEK(16384)=0`, header `$1C03 = $30`, `$1C04` = default T, and that the four BASIC vectors point to addresses in `$1C00–$3FFF`.

### 6.3 Guide listings (`docs/listings/*.bas`)

- Line 20 `BLOAD "VDCBASIC2H",B0:SYS DEC("1300")` → `20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END` (the guide explains the boot loader in chapter 1; loading and installing is handled by `BOOT`).
- Buffer `BF=DEC("4000")` → `BF=DEC("8000")`; `11-walk.bas` `A=DEC("5000")` → `A=DEC("A000")`. Largest file: `dungeon` 8004 bytes → `$8000–$9F44`, no overlap with `$A000`.
- `02-glyph.bas` (`G=DEC("2000")`) is a VRAM address → unchanged.
- Prose: chapter 1 (loading, `$1300`, "a little more than 2 KiB"), chapter 6 (BASIC from `$1C01`, buffer `$4000`), reference "Loading" and "Memory used by VDC-BASIC", README (version, load address, size, section "Compiling programs" with boot loader and image 2, section "Building"), `docs/OUTLINE.md`, `test/README.md`.
- The screenshots `docs/img/*.png` must be **unchanged** after the run with `-d docs/img` (`git status` clean). Deviations (e.g. visible installation message) are to be justified or fixed individually.

### 6.4 Monitor scripts (port rule)

Common module `test/vicemon.py` (for E1, `boot-sector`, P2b, P2c):

- Port from `--port`, otherwise `DBE_VICE_PORT`, otherwise 6631; range 6631–6649; bind test on `127.0.0.1` beforehand; occupied → next free one with a message, with explicit `--port` abort (exit code 2).
- Start with `-remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>`, no `-binarymonitor`.
- Terminate only the own `Popen` (`terminate`, after timeout `kill`, in the `finally`); no `pkill`/`killall`.
- Event-driven: breakpoints on labels from the ACME symbol list (`acme -l build/vdcbasic3.sym`) or on fixed addresses, BASIC error vector `($0300)`; no fixed wait times; timeout only as a guard. Connection setup with retry until the port is open (like `dungeonboringengine/test/vice_drive.py`).

### 6.5 Test list P2a

| Test | Kind | Expectation |
|---|---|---|
| `test/run.sh test/*.bas` | interpreted | all PASS (incl. `layout.bas`) |
| `test/run.sh -c test/*.bas` | Blitz! image 2 | all PASS; exceptions only with justification in `test/README.md` (e.g. tests that check interpreter errors, if E1d is negative) |
| `test/run.sh -l -d docs/img docs/listings/*.bas` | listings | all PASS, screenshots unchanged |
| `test/boot_sector.py` | monitor | `BOOT` starts via the boot sector without `-autostart`, breakpoint `$1C00` reached, then exit code of the debug cartridge from a `RUN"T"` = 0 |
| `test/e1_blitz.py` | monitor | E1a: no write accesses of the runtime to `$1300–$3FFF`; results E1b–E1e logged |
| Build | ACME | `vdcbasic3.bin` begins with `00 1C 4C`, header fields at the fixed addresses, end ≤ `$3FFF` |

## 7. Scope and order of work

1. E1 (5.2) with a v2h state assembled to `$1C00`, enter findings in section 9.
2. Source (2), header, overall check; build `vdcbasic3.bin`.
3. `boot/boot.bas`, `bastok.py --start`, `run.sh` (boot, `-c`), `blitzprep.py`.
4. Convert tests (6.2), `layout.bas`.
5. Listings and documentation (6.3).
6. `tools/mkboot.py`, `test/boot_sector.py`.

## 8. Acceptance

| Check | Expectation |
|---|---|
| Build | error-free; end of VDC-BASIC ≤ `$3FFF` (assertion), header at `$1C00–$1C0F` |
| Behavior | no change compared to v2h except address and message (`git diff` in `src/` only 2, 2.1) |
| interpreted | `test/run.sh test/*.bas` all PASS |
| compiled | `test/run.sh -c test/*.bas` all PASS (or documented exception), compilates have load address `$4001` |
| Listings | `test/run.sh -l -d docs/img docs/listings/*.bas` all PASS, `docs/img` unchanged |
| Boot | `boot_sector.py` green; `BOOT` also via `-autostart disk.d64` |
| Blitz runtime | E1a: no foreign write accesses to `$1300–$3FFF` |
| Tests | without `sleep`; monitor scripts observe the port rule |
| Documentation | README, reference, guide chapters 1/6, listings, `test/README.md` adjusted |

## 9. Findings E1

Carried out on 2026-11-25 with `test/e1_blitz.py` (VICE 3.10 `x128`, blitzc `cli.js`, improved runtime V261123), VDC-BASIC v2h assembled only to `$1C00` (`build/e1/`), loaded via the boot loader (`boot.bas` without line 40, line 50 `RUN"T"`). Each program ran interpreted and as a compilate; result via the debug cartridge.

| Question | Result |
|---|---|
| E1a | **No** write accesses to `$1300–$3FFF` with PC outside the VDC-BASIC code, neither interpreted nor with image 2 (programs `e1b-*`, `e1d-trap`). Control `e1a-control` (`BANK 0:POKE DEC("1300"),1`): hit interpreted (PC `$02B8`, `k_stash`) and compiled (PC `$57BE`, runtime) – so the watchpoint works. Note VICE 3.10: the condition must be parenthesized, `watch store 1300 3fff if (pc < $1c00) \|\| (pc > $xxxx)`; without parentheses it never triggers. |
| E1b | **Yes**: `::A=7` and then `IF A<>7` – A is 7. Also `::A=VMD(2000)+5` (after `::VMW 2000,42`) yields 47. → **Variant A** for `blitzprep.py`. |
| E1c | Compiler error `?SYNTAX ERROR IN 20` for `A=VMD(2000)` outside an extension (as expected). |
| E1d | **Yes**: error in `::VMW 70000,1` jumps to `TRAP 9000`, `ER` = 14. |
| E1e | **Yes**: a never-executed `63999 GRAPHIC 0` yields load address `$4001` (image 2). Without `GRAPHIC`, image 1 results (load address `$1C01`); its loader copies the program to `$1C00` and overwrites VDC-BASIC (watchpoint: write accesses `$1C00…` from PC `$4048`). Image 1 is therefore unusable with v3. |

Consequences: 5.3 variant A, step 3 as described (`63999 GRAPHIC 0`).

## 10. Decisions of the user (2026-11-25)

1. `GRAPHIC 1`–`4` is taboo with v3 – accepted; noted in the README (memory/loading).
2. Guide and listings (`docs/listings`, chapters 1/6, reference, 6.3) are converted only in P8, not in P2a. Until then `test/run.sh -l` continues to check the listings with v2h at `$1300` (without boot loader).
3. `vrl.py` moves to vdc-basic (`tools/vrl.py`, canonical copy); P2a builds in no dependency via `DBE_TOOLS`.
4. For later (P2c/P5b): new VMS list command "source n bytes further" (source skip) for sprite clipping at the viewport edge.

## 11. Implementation notes and deviations (2026-11-25)

State of the acceptance: `test/run.sh test/*.bas` and `test/run.sh -c test/*.bas` each 16/16 PASS (incl. `layout.bas`), compilates with load address `$4001`; `test/run.sh -l -d docs/img docs/listings/*.bas` 23/23 PASS (v2h, see below); `test/boot_sector.py` green (boot sector and `-autostart disk.d64`); `test/e1_blitz.py --bin vdcbasic3.bin --sym build/vdcbasic3.sym` green (same findings as with the E1 build). `vdcbasic3.bin`: `00 1C 4C 10 1C 30 0E 00 …`, code `$1C00–$2505` (2310 bytes, 6906 bytes reserve up to `$3FFF`).

- **Listings (6.3) not converted** (decision 2): `run.sh -l` continues to load `vdcbasic2h` at `$1300` without `BOOT`. Two screenshots (`03-fill`, `12-list`) deviated in one run; both show measured jiffy numbers (timing jitter, v2h path unchanged) and were restored from `HEAD`.
- **Additionally relocated tests** (not listed in 6.2): `vcl-with-vms.bas` (`$5000`/`$6000` → `$A000`/`$B000`, also the list address in the `DATA` bytes), `vms-long-list.bas` (`$5000` → `$A000`). `$6000` would lie in the middle of the runtime in the compilate.
- **`chapter-claims.bas`**: The check "keyword in variable name" (`SYNC=1` with `TRAP`) is now at the end (lines 500–540). Finding: after an error caught by `TRAP` **inside an extension**, further extensions in the compilate no longer work (`::B=5` does not come back, `::VMW` reports `?ILLEGAL QUANTITY` or hangs) – reproduced without a VDC-BASIC command (`::A=1/0`), hence a property of the Blitz! runtime; `RESUME` does not help. E1d therefore holds only for "error, then end/abort". Documented in README and `test/README.md`.
- **`blitzprep.py`**: A `::` written by the listing itself (e.g. `IF R=9 THEN::VMF …`, chapter 13 statement in `chapter-claims.bas`) stays in place and does not count as an abort case. Function calls in statements after the `THEN` part, separated by `:`, are pulled forward normally (they belong to the conditional part anyway). Lines 0 and 63999 are reserved for the tool.
- **Boot sector (3.3)**: Entry point `$AFA5` confirmed (jumps to `$4DCD`: TXTPTR = X/Y, tokenize text, execute). Deviation: X/Y must point to `cmd − 1` (CHRGET increments before reading), `cmd` is plain text `RUN"BOOT"` + 0, not tokenized. `tools/mkboot.py --disk D64` checks sector 1/0 and writes it.
- **VICE monitor**: Conditions with `||` must be parenthesized (`if (pc < $1c00) || (pc > $2508)`). When started without `-autostart`, the monitor stops before the KERNAL initialization; `boot_sector.py` therefore sets the error breakpoint fixed at `$4D3F`.
- **Open – P0 benchmark program**: `test/bench/run_bench.py` builds `src/vdcbasic.asm` and requires equality with `vdcbasic2h.bin`; since P2a the source produces v3, so the bench build aborts. `test/bench/` was not touched here (P0 committed by the coordinator); suggestion: bench builds from `git show <v2h-commit>:src/vdcbasic.asm` or directly uses `vdcbasic2h.bin` plus a checked-in symbol list.
- **Not in P2a**: `RXV`/`VDM`, token stubs `VDL`/`VDR`/`VDB` (P2b), token list unchanged except for the comment "vdcbasic v3".
