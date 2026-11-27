# Notes for agents

## Tests

- Only run tests related to the change. Which ones follows from the changed code and its callers. Example: a change to the VMC row loop affects the VMC/VMF/VCL/VMS tests, not the drawing tests (`VDL`/`VDR`/`VDB`), which do not use that code.
- Long runs (e.g. complete suites in Z64K) only if the change really affects them. State briefly in the report which tests ran and which were deliberately skipped.
- Do not re-measure old versions: take baseline values from the recorded results (specs, result files) and store new results there with version/checksum/date.

## Optimisations: measure first, then implement

- Before implementing, build a small prototype (core path only, e.g. in sim65 or as a micro-benchmark) and measure it on real scenes. Time box about 15–20 minutes.
- Decide on the measured number, not on an estimate from instruction counting. If the gain is small, drop it before the full implementation with all tests starts.
- Full implementation only after that, with a fixed time frame. In the report, put forecast and actual value side by side.
- Record rejected attempts with measurements and reason in the related spec so they are not repeated.

## Test selection by kind of change

Source: `dungeonboringengine/docs/analysis/fable-2026-11-26.md` 3.4. Paths refer to the respective repo (engine = dungeonboringengine, VDC-BASIC = vdc-basic, game = dungeoncrawlerjam2025). Run times: (m) measured, (d) from docs, (e) estimated. Put the commands of the matching row into every task, together with what does not run.

| Changed | Needed | Run time | Not needed |
|---|---|---|---|
| `engine/view.s`, `VIEW` parts of `lib.s` | `run_sim.py` (rebuilds), `run_crawl.py --fix --no-cycles`; at the end `vice_view.py --only c --fix --no-sim` | 38 s + 3 s (m) + ≈ 1 min (e) | VDC-BASIC tests, Z64K, `vice_zp`, `vice_blitzvars`, game timing |
| `lib.s` set functions (`MAPINIT`, `GEOMSET`, `TEXSET` …) | `run_sim.py`; `vice_smoke.py --only c` | 38 s (m) + < 1 min (e) | full `vice_view`, demo |
| `engine/stub.s`, MMU, zero page, `dbe.inc`, `lib.cfg` (addresses) | `vice_smoke.py`, `vice_zp.py` (+ `--funcs`), `vice_view.py --only c --fix`, `vice_blitzvars.py`; then `vice_crawl.py` | ≈ 1 min each (e); `vice_crawl` ≈ 25 s (d) | full sim65 sets (the stub does not run there) |
| `tools/dbeview.py` (reference) | `unittest -p test_dbeview.py`, `run_sim.py`, `run_crawl.py` | ≈ 50 s + 38 s + 3 s (m) | VICE |
| `tools/dbetex.py`, `dbespr.py`, `dbegeom.py`, `dbemap.py`, `crawlconv.py` | the one matching `unittest` module; if the output changes `test_dbeview`, for Crawl data `test_crawl01` | 5–50 s (m) | sim65, VICE (except format changes) |
| VDC-BASIC VMC/VMF row loop, `block_copy_target` | old-vs-new difference test (today only `build/p2f/h/diff.py`), `run.sh` with the 29 listings for VMC/VMF/VCL/VMS/VMP/VMB in VICE, the same with `-z`; `vice_crawl.py`; one timing run | VICE ≈ 2 min (e), Z64K ≈ 4–5 min (d, pro rata), 25 s (d) | RXV and drawing suites, engine sim65 |
| VDC-BASIC `VCL` dispatcher, VMS | `run.sh test/vcl-*.bas test/vms-*.bas` (VICE, + `-c`), `-z` only these; `vice_view.py --only c --fix` | ≈ 1–2 min (e) + ≈ 2 min Z64K (d, pro rata) | drawing, RXV |
| VDC-BASIC `RXV` | `run_rxv.py` (VICE), `--emu z64k` | VICE < 1 min (e), Z64K 1 min 18 s (d) | everything else |
| VDC-BASIC drawing commands | `test/ref` unit tests, `run_draw.py --quick`; for acceptance the full set and once `--emu z64k` | ≈ 30 s (d), Z64K 3 min 25 s (d) | engine, game |
| VDC-BASIC only moved (size changed) | `run.sh test/smoke.bas test/layout.bas`, full `run_draw.py` in VICE, `vice_crawl.py` | ≈ 1 min (e) | Z64K |
| `game/crawl.bas` logic or AI | `vice_crawl.py`, `--timing` one run per configuration | 25 s (d) + ≈ 1–2 min (e) | engine and VDC-BASIC tests |
| Docs or specs only | none | 0 | everything |
