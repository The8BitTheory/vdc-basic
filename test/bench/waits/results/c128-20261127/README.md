# Real hardware, C128 (64 KB VDC), 2026-11-27

Run with `test/hw/vdc-hw-tests.d81` (vdc-basic commit abb30af), menu items 1 and 2, machine in FAST mode.
`res0`–`res5.prg`: result blocks of the wait experiment (docs/v3/P2g-wait-experiment.md section 6);
`report.md`/`hw.json`: `waits.py eval`. Bench (menu item 1): `../../c128-20261127.{json,md}`, raw `../../c128-20261127-benchres.prg`.

Findings
- Same picture as Z64K (`../z64k-20261126/hw/`): A, C, AC/2, AC/8 wrong in all repetitions; AC, p2f, full, p2f-AC, S-AC, R-ACt etc. correct.
- No data loss at R36 = 0 with `p2f` (all waits) in this run.
- One deviation: `ACt` at 1 MHz, R36 = 0, L = 16: one of 3 repetitions wrong (`F1`). Z64K had none. To be repeated.
- Bench: threshold T = 2 (A, C) and 6 (B, D); 8 plausibility warnings only because the expected values come from the VICE model (fill_done(256)-fill_done(2) = 1616 cycles instead of about 168).
