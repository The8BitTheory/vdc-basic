# anim: cost of an animation step on the VDC

Measurement program for the animation plan of the engine (`dungeonboringengine/docs/vda-animation-notes.md`).
Question: what does one animation step cost when the source lies in VRAM (block copy) or in CPU RAM
(CPU copy or `RXV` stream), and how long does loading the assets into VRAM take?

## What runs

Standalone program, calls VDC-BASIC v3 (`vdcbasic3.bin`, `$1C00`) directly: `VCL` behind its parameter parsing
(`vcl + 3`), so every case runs exactly the path a game takes through a `VCL` list (types 2, 3, 5, 8, 12).
`RTV` through a small entry below `$4000` (its RAM loop banks the ROMs in). Chunky mode as Crawl
(attributes `$0000`, 80 bytes per row, bitmap `$B400`-`$FFFF` = `$0F`), 2 MHz, R36 = 0, VIC blanked,
interrupts off. Time: CIA 2 timer A+B (µs), K = 12 samples per case, each started at a different position in the
frame (`vdc_sync` + delay); the median of the empty case `none` (timer and call overhead) is subtracted.
After each case a Fletcher-16 over the target rectangle is compared with a Python model of the VRAM;
a wrong result is reported as `FAIL`.

| Group | Cases | Variants |
|---|---|---|
| base | `none`, `vcl-empty`, `vcl-vmc1` (1 byte), `vcl-rxv0` (empty stream) | fixed costs |
| opaque patch | `eyes` 8×4, `portrait3` (eyes 8×4, mouth 6×3, sparkle 2×4), `torch` 4×12, `gate2` 20×42 and `gate1` 30×64 (baked frame of a sinking gate: corridor behind + gate), `wings1` (gate1 as three columns of 10 bytes, two-wing gate), `view` 70×96 (whole viewport from the off-screen buffer) | `vmc`: packed in VRAM, VCL type 2/3. `cpu`: raw in RAM, CPU loop per row (`bit $d600`/`sta $d601`, address per row; best case for a possible "RTV rectangle"). `rxv14`/`rxv2`: one VRL stream in RAM, row ends as skips, VCL type 8, threshold T = 14 (default in the binary) and 2 (P0 hardware value) |
| transparent | `grille1` 30×64 (bars), `goblin` 16×40 (ellipse), byte-granular | `vms`: VRAM data + run list (VCL 12). `cpu`: one row entry per opaque run. `rxv14`/`rxv2`: transparent bytes as skips |
| layered (R2) | `r2gate`: corridor layer, goblin behind, opaque gate rows; `r2grille`: the same with the grille (VMS list truncated to 40 rows) | one VCL list |
| preload | `pl-gate1` (1920), `pl-behind` (1920, dark), `pl-goblin` (528), `pl-portrait` (50), `pl-bg` (9600, brick wall) into a scratch area | `rtv` (VDC-BASIC `RTV`), `cpu`, `rxv14`/`rxv2` (linear stream) |

Content is generated (`anim.py`, fixed seed): brick wall with grout and some noise, dark corridor, wooden gate with
planks, grain and iron bands, grille, noisy portrait parts and torch, goblin. The table shows the stream size
next to the raw size, so results can be transferred to other art.

## Usage

```sh
test/bench/anim/anim.py build              # build/anim/anim.d81 (hardware) and anim-emu.d81
test/bench/anim/anim.py vice               # x128 warp, about 5 s; results/vice3.10-<date>.*
test/bench/anim/anim.py z64k --out build/anim-z   # Z64K; results/z64k-<date>.*
test/bench/anim/anim.py eval ANIMRES.prg --source c128   # hardware result
```

`eval` needs `cases.json` of the same build (`--out`, default `build/anim`).

## Real hardware (C128, 80 columns, 64 KB VDC)

1. Disk: `test/hw/vdc-anim-bench.d81` (copy of `build/anim/anim.d81`).
2. On the C128: `RUN"BOOT"` (or boot from the disk). BOOT installs VDC-BASIC v3 and starts `ANIM`.
   The screen stays dark for about a minute (VIC blanked, VDC in chunky mode, test pictures).
3. At the end the text screen comes back with `ANIMRES WRITTEN, FERTIG`; the file `ANIMRES` is on the disk.
4. On the PC: `c1541 -attach vdc-anim-bench.d81 -read animres ANIMRES.prg`, then
   `test/bench/anim/anim.py eval ANIMRES.prg --source c128` → `results/c128-<date>.{json,md}`.

## Memory

| Area (bank 0) | Content |
|---|---|
| `$1300`-`$13FF` | entry (`SYS 4864`) and `RTV` call |
| `$1400`-`$1BFF` | result block (`ANIMRES`) |
| `$1C00`-`$3FFF` | VDC-BASIC v3 |
| `$4001`- | loader (BASIC) |
| `$4800`-`$5FFF` | program, case table, VCL lists |
| `$6000`-`$CFFF`, `$E000`-`$FEFF` | data: VRL streams, raw patches, VMS lists, row lists |

VRAM: screen `$0000`, background/off-screen buffer `$2600`, asset pool `$4C00`-`$6FFF`, scratch `$7000`-`$B3FF`.
