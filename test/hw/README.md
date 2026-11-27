# Hardware test disk

`vdc-hw-tests.d81` holds every test program that still has to run on a real C128 (80 columns, 64 KB VDC).
Copy it to a 1581 / SD2IEC / Ultimate, then `RUN"MENU"` (or `DLOAD"MENU"`, `RUN`) and press 1–4.

| Key | Program | What it checks | Result |
|---|---|---|---|
| 1 | `LOADER-HW` | benchmark, threshold T (docs/v3/P0-bench.md, section 11); about 1 min, screen unsteady | file `BENCHRES` on the disk |
| 2 | `LOADER` + `W` | wait experiment, reg 36 = 0/1/5 at 1 and 2 MHz, DRAM refresh risk (docs/v3/P2g-wait-experiment.md, section 6); about 10 min | files `RES0`–`RES5`, then `FERTIG` |
| 3 | `BOOT-R30` → `R30` | reg 30 = 0 fills 256 bytes (docs/v3/P2b-rxv.md 3.3) | text `r30=0 fills 256 bytes: ok` |
| 4 | `BOOT-RXV` → `RXV` | R18/R19 readable after block fill and reg 31 write (3.4) | text `rxv-hw: all ok` |

Evaluate the result files on the PC as described in the two documents (`c1541 -attach vdc-hw-tests.d81 -read benchres BENCHRES.prg`,
then `test/bench/run_bench.py --from-file … --source c128`, or `test/bench/waits/waits.py eval RES0.prg … RES5.prg`).

Rebuild: `test/hw/mkhwdisk.py` (needs `build/bench/bench.d64` from `test/bench/run_bench.py --short`).

## Repeat run for the `ACt` outlier (`vdc-act-repeat.d81`)

First hardware run (2026-11-27, `test/bench/waits/results/c128-20261127/`): `ACt` at 1 MHz, R36 = 0, L = 16 was wrong in 1 of 3 repetitions.
`vdc-act-repeat.d81` runs the set `hwact` (`ACt`, `R-ACt`, `AC` at 1 MHz, R36 = 0, six identical configurations = 18 repetitions per cell, a few minutes):
`RUN"LOADER"`, then `RES0`–`RES5` appear on the disk. Result (2026-11-27): 18 of 18 correct, see `test/bench/waits/results/c128-20261127-act/`. Evaluate: `test/bench/waits/waits.py eval RES0.prg … RES5.prg`.
Rebuild: `waits.py build --set hwact --out build/p2g/hwact`, then `c1541 -format "act repeat,ac" d81 test/hw/vdc-act-repeat.d81 -write build/p2g/hwact/loader.prg loader -write build/p2g/hwact/w.prg w`.
