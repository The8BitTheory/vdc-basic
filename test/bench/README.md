# vdcbench: Messprogramm für VDC-Zeiten

Spezifikation: `docs/v3/P0-bench.md`. Misst CPU-Streaming über Reg 31, Block fill/copy, `VMC`/`VMS`
(VDC-BASIC v2h) und Reg 36 und bestimmt die Schwelle T (Block fill gegen CPU-Streaming).

## Aufruf

```sh
test/bench/run_bench.py [--emu vice|z64k] [--short] [--no-r36] [--skip-build] [--port N] [--out DIR] [--timeout S]
test/bench/run_bench.py --from-file BENCHRES.prg --source c128     # Hardware-Ergebnis auswerten
```

- `--short`: Kurzlauf (nur Konfiguration A, L aus M), `--no-r36`: Reg-36-Test auslassen.
- VICE-Monitor-Port: `--port`, sonst `DBE_VICE_PORT`, sonst 6631; ist er belegt, wird (ohne `--port`)
  der nächste freie Port bis 6649 genommen, mit ausdrücklichem `--port` Abbruch mit Exitcode 2.
- Ergebnisse: `results/<quelle>-<JJJJMMTT>.json` und `.md` (VICE: vorläufig).

## Z64K als zweites Backend (`--emu z64k`)

Z64K (Java, GUI, genauerer VDC als VICE) läuft unter einem eigenen Xvfb (Displaynummer per `-displayfd`,
nur dieses Xvfb und Java werden per PID beendet). Es läuft der Hardware-Pfad: `loader-hw` (unverändert),
darum erzeugt `run_bench.py` `build/bench/loader-z64k.prg` = `loader-hw.bas` + `POKE 8196,<Modus>`
(für `--short`/`--no-r36`) + `POKE $D7FF,0` nach dem `BSAVE` (nur dieser Lader; `loader-hw.bas` greift nicht
auf `$D7FF` zu).

Aufruf von Z64K (verifiziert): `java -cp Z64K.jar test/bench/Z64KRun.java c128 -vdc64kb -warp -debugcart
-limitcycles N -8 bench.d64 -autoload loader-z64k.prg`. `-debugcart`: Schreiben nach `$D7FF` beendet Z64K sofort
mit diesem Wert als Exitcode; `-limitcycles N`: Exitcode 1; `-exitscreenshot DATEI` (VIC-II-Bild) möglich.
Z64K schreibt Diskimages **nie** zurück (`BSAVE` landet nur im Speicher, nur der GUI-Dialog speichert);
`Z64KRun.java` startet Z64K deshalb im selben Prozess und sichert das Image von Laufwerk 8 in einem
Shutdown-Hook (Reflection auf `config.ConfigManager`, `CBMConfig.saveDisk`). Danach liest `run_bench.py`
`BENCHRES` per `c1541` aus dem Image und wertet wie `--from-file` aus (`results/z64k-<JJJJMMTT>.json/.md`,
`"source": "z64k"`, mit Vergleichstabelle gegen die neueste `vice3.10-*.json`). Ereignisquelle ist das
Prozessende (Exitcode 0); `--timeout` (Standard 600 s je Lauf) und `-limitcycles` sind nur Wächter. Volllauf (ein Lauf) etwa 85 s, Kurzlauf etwa 70 s. Der Lader enthält `TRAP`: ein BASIC-Fehler beendet Z64K
sofort mit Exitcode 2 (ereignisgesteuert statt Warten auf den Wächter). Früher hing der Reg-36-Test in Z64K
(über 1 h): `vdcbench` schrieb beim Austasten des VIC das gelesene Bit 7 von `$D011` (Rasterzeile Bit 8) als
Rastervergleich Bit 8 zurück; je nach Zeitpunkt kam danach kein Raster-IRQ mehr, die Jiffy-Uhr stand. Seit
Bit 7 gelöscht wird, läuft der Volllauf durch (Fehler betrifft auch echte Hardware). `--moncommands` ist in Z64K ohne Wirkung. Zusätzlich nötig: `Xvfb`, `java` (21,
mit `jdk.compiler` für den Quelltext-Start), Z64K.jar in `~/.local/share/z64k/` (oder `Z64K_JAR`).
`--skip-build` verwendet ein vorhandenes `build/bench` (ohne Neubau und ohne den `cmp`-Test).

## Abhängigkeiten

`acme`, `git`, `ca65`/`ld65` (cc65), `c1541`, `x128` (VICE 3.10, Remote-Monitor), Python 3 (nur Standardbibliothek).
Zwischendateien liegen in `build/bench/` (nicht einchecken).

## Dateien

`vdcbench.s`/`vdcbench.cfg` Messprogramm; `loader.bas` (VICE), `loader-hw.bas` (echte Hardware, `BSAVE`
der Ergebnisse); `run_bench.py` Harness; `Z64KRun.java` Z64K-Start mit Image-Sicherung; `evaluate.py` Auswertung.

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

Hinweis: Gemessen wird VDC-BASIC v2h (`$1300`, eingecheckte `vdcbasic2h.bin`). `run_bench.py` baut dafür die Quelle aus
dem letzten Commit, der `vdcbasic2h.bin` geändert hat (`git show`, nach `build/bench/v2h/`), unabhängig von `src/`.
