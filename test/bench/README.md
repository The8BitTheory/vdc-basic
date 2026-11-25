# vdcbench: Messprogramm für VDC-Zeiten

Spezifikation: `docs/v3/P0-bench.md`. Misst CPU-Streaming über Reg 31, Block fill/copy, `VMC`/`VMS`
(VDC-BASIC v2h) und Reg 36 und bestimmt die Schwelle T (Block fill gegen CPU-Streaming).

## Aufruf

```sh
test/bench/run_bench.py [--short] [--no-r36] [--port N] [--out DIR] [--timeout S]
test/bench/run_bench.py --from-file BENCHRES.prg --source c128     # Hardware-Ergebnis auswerten
```

- `--short`: Kurzlauf (nur Konfiguration A, L aus M), `--no-r36`: Reg-36-Test auslassen.
- VICE-Monitor-Port: `--port`, sonst `DBE_VICE_PORT`, sonst 6631; ist er belegt, wird (ohne `--port`)
  der nächste freie Port bis 6649 genommen, mit ausdrücklichem `--port` Abbruch mit Exitcode 2.
- Ergebnisse: `results/<quelle>-<JJJJMMTT>.json` und `.md` (VICE: vorläufig).

## Abhängigkeiten

`acme`, `ca65`/`ld65` (cc65), `c1541`, `x128` (VICE 3.10, Remote-Monitor), Python 3 (nur Standardbibliothek).
Zwischendateien liegen in `build/bench/` (nicht einchecken).

## Dateien

`vdcbench.s`/`vdcbench.cfg` Messprogramm; `loader.bas` (VICE), `loader-hw.bas` (echte Hardware, `BSAVE`
der Ergebnisse); `run_bench.py` Harness; `evaluate.py` Auswertung.

## Echte Hardware (C128 mit 64-KB-VDC)

1. `test/bench/run_bench.py --short` einmal laufen lassen (baut `build/bench/bench.d64` mit
   `VDCBASIC2H`, `VDCBENCH`, `LOADER-HW`), das Image auf eine echte Diskette bzw. SD2IEC/Ultimate bringen.
2. Am C128 im 80-Zeichen-Modus: `LOAD"LOADER-HW",8` und `RUN`. Der Bildschirm bleibt dabei schwarz/
   unruhig (VIC ausgetastet, VDC im Bitmap-Modus); Dauer etwa 1 min (Reg-36-Test mit F = 500 dauert
   allein 6 x 10 s). Am Ende steht `FERTIG`; der Lader hat `BENCHRES` (16 KB, $4000-$7FFF) auf die Disk geschrieben.
3. `BENCHRES` auf den PC holen, z. B. `c1541 -attach bench.d64 -read benchres BENCHRES.prg`.
4. `test/bench/run_bench.py --from-file BENCHRES.prg --source c128` schreibt
   `results/c128-<JJJJMMTT>.json` und `.md` (`"provisional": false`).

Hinweise zur Umsetzung: In den Fällen 8-10 zählt `$FB` die Wiederholungen (statt `Y`); die Gegenprobe
setzt `xchk_a`/`xchk_b` innerhalb des CIA-Fensters (CIA-Wert minus 15 Zyklen Rahmen gegen Stoppuhr);
`vdc_sync` startet jeden Fall am Anzeigebeginn; `cpu_byte` stammt nur aus N <= 16 (Spec 10.2).
`--timeout` (Standard 120 s) gilt für den ganzen Lauf.
