# Real hardware, C128: repeat run for the `ACt` outlier (2026-11-27)

Disk `test/hw/vdc-act-repeat.d81` (set `hwact`: `ACt`, `R-ACt`, `AC` at 1 MHz, R36 = 0, six identical configurations, 3 repetitions each = 18 per cell),
`res0`–`res5.prg`, evaluation `report.md`/`act.json` (`waits.py eval`).

Result: no wrong repetition (`F`) in any cell of `ACt`, `R-ACt` and `AC` (18 of 18 correct per cell, all lengths).
The single `F1` of the first run (`../c128-20261127/`, `ACt`, L = 16) did not recur; it stays a one-off event (cause unknown).
