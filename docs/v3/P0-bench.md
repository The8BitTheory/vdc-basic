# P0: Messprogramm VDC-Zeiten (Block fill, CPU-Streaming, VMC/VMS, Reg 36)

Stand: 2026-11-25. Detailspezifikation zu Schritt P0 aus `dungeonboringengine/docs/plan-0.2.md`. Umsetzung: Sonnet (Messprogramm, Harness, Auswertung), Haiku (VICE-Läufe). Review: Opus.

## 1. Zweck und Ergebnis

Gemessen werden die Kosten der VDC-Zugriffe, die `RXV` (VRL-Decoder, P2b) und die Engine (P6) verwenden:

| Teil | Messgröße |
|---|---|
| (a) CPU-Streaming über Reg 31 | Zeit je Byte (konstanter Wert und Literal aus RAM), Kosten einer neuen Adresse |
| (b) Block fill | Zeit je Lauflänge L = 1 … 256 inklusive Setup (Reg 24 RMW, erstes Byte, Reg 30) und Wartezeit bis fertig; dazu die CPU-Zeit bis zum Schreiben von Reg 30 („issue“) |
| (c) Block copy | wie (b), mit Quelladresse Reg 32/33 |
| (d) `VMC` / `VMS` von VDC-BASIC v2h | Zeit je Byte, je Zeile, je Befehl; `VMS` Typ 1 und Typ 8 |
| (e) Reg 36 | Datenerhalt im VRAM außerhalb der Anzeige bei Reg 36 = 0 … 5 |

**Hauptergebnis:** die Schwelle **T** = kleinste Lauflänge, ab der Block fill (bis fertig) schneller ist als CPU-Streaming, für alle Längen ≥ T. Dazu die Kostenparameter für das Zyklenmodell von `tools/vrl.py` (P1, Abschnitt 8.3).

**Alle Ergebnisse aus VICE sind vorläufig** (Abschnitt 2). Das Messprogramm läuft deshalb unverändert auch auf echter Hardware (Abschnitt 11); erst diese Werte gelten als endgültig.

## 2. VDC-Timing in VICE 3.10 (aus dem Quelltext)

`~/.local/src/svn-mirror-3.10.0/vice/src/vdc/vdc-mem.c` bildet die Busy-Zeit des VDC nur grob nach (`vdc_status_clear_clock`, Einheit `maincpu_clk`):

| Zugriff | VICE-Modell |
|---|---|
| Schreiben/Lesen Reg 31, Schreiben Reg 18/19 | busy 43 Takte im sichtbaren Bereich (`display_enable`), sonst 4 |
| Block fill | sofort ausgeführt, busy `L × 66 / 100` Takte |
| Block copy | sofort ausgeführt, busy `L × 120 / 100` Takte |
| Reg 36 (Refresh) | ohne Wirkung („unsupported“), VRAM verliert nie Daten |
| andere Register | kein Busy |

Folgen:

- In VICE wird T sehr klein ausfallen (Fill fast kostenlos, CPU-Byte 4–45 Takte je nach Rasterposition). Auf echter Hardware hängt beides an der Speicherbandbreite des VDC und an der Anzeige. **T aus VICE darf nicht fest in `RXV` eingebaut werden** (siehe `review-notes.md`: T als patchbares Byte).
- Ob `maincpu_clk` im 2-MHz-Modus von x128 doppelt zählt, ist nicht geklärt. Gemessen wird deshalb mit dem CIA-Timer in µs (Abschnitt 5); die Zyklen der CPU werden daraus abgeleitet.
- Der Reg-36-Test kann in VICE nur das Harness prüfen (erwartet: 0 Fehler).
- Jede JSON-Ausgabe aus VICE trägt `"provisional": true` mit diesem Grund.

**Plausibilitätsprüfung (Pflicht, Warnung in der Ausgabe):** Weicht das Ergebnis deutlich vom VICE-Modell ab, ist eher das Messprogramm falsch. Erwartung in VICE: `fill_done(256) − fill_done(2)` etwa 254 × 0,66 Takte; `copy_done(256) − copy_done(1)` etwa 255 × 1,2 Takte; Streaming je Byte zwischen etwa 6 und 45 Takten.

## 3. Dateien (alle unter `test/bench/` im Repo vdc-basic)

| Datei | Inhalt |
|---|---|
| `vdcbench.s` | Messprogramm, ca65 (`ca65 -t none`), lädt bei `$2000` |
| `vdcbench.cfg` | ld65-Konfiguration: `LOADADDR` (2 Byte `$2000`), `CODE`+`RODATA`+`DATA` ab `$2000` bis `$2FFF`, `BSS` `$3000–$37FF` (VMS-Listen, Variablen) |
| `loader.bas` | BASIC-Lader für VICE (Text, mit `test/bastok.py` tokenisiert) |
| `loader-hw.bas` | Lader für echte Hardware: wie `loader.bas`, danach `BSAVE` der Ergebnisse |
| `run_bench.py` | Build, Disk, VICE-Lauf über den Remote-Monitor, Auswertung |
| `evaluate.py` | nur Auswertung: Ergebnisblock (`.prg`) → JSON + Markdown; wird von `run_bench.py` importiert |
| `results/` | Ergebnisse: `<quelle>-<JJJJMMTT>.json` und `.md`, `<quelle>` = `vice3.10` oder `c128` (Hardware) |
| `README.md` | Kurzanleitung (Aufruf, Abhängigkeiten) |

Build-Zwischendateien nach `build/bench/` (nicht einchecken; `.gitignore` ergänzen).

### 3.1 Build

```sh
ACME=$HOME/.local/share/acme/ACME_Lib acme -f cbm -o build/bench/vdcbasic2h.bin \
     -l build/bench/vdcbasic2h.sym src/vdcbasic.asm
cmp build/bench/vdcbasic2h.bin vdcbasic2h.bin        # muss identisch sein, sonst Abbruch
```

`run_bench.py` wandelt aus der ACME-Symbolliste (Zeilen `\tname\t= $xxxx\t; …`) genau diese Symbole in `build/bench/vdcbasic_syms.inc` (ca65: `name = $xxxx`): `remember_mem_conf`, `saved_mem_conf`, `vmc_execute`, `vms_execute`. Fehlt eines → Abbruch.

```sh
ca65 -t none -I build/bench -o build/bench/vdcbench.o test/bench/vdcbench.s
ld65 -C test/bench/vdcbench.cfg -Ln build/bench/vdcbench.lbl -m build/bench/vdcbench.map \
     -o build/bench/vdcbench.prg build/bench/vdcbench.o
```

Disk (wie `test/run.sh`): `c1541 -format "bench,01" d64 build/bench/bench.d64 -write build/bench/vdcbasic2h.bin vdcbasic2h -write build/bench/vdcbench.prg vdcbench`; Lader mit `python3 test/bastok.py test/bench/loader.bas build/bench/loader.prg`.

## 4. Ablauf und Speicher

| Bereich (Bank 0) | Inhalt |
|---|---|
| `$1300–$1BFF` | VDC-BASIC v2h (`BLOAD`, `SYS 4864`) |
| `$1C01–` | BASIC-Lader (wenige Zeilen, keine Variablen) |
| `$2000–$2FFF` | Messprogramm |
| `$3000–$37FF` | VMS-Listen, Variablen des Messprogramms |
| `$4000–$7FFF` | Ergebnisblock (RAM 0 unter dem BASIC-ROM; geschrieben mit `$FF00 = $3E`) |

Kopf des Messprogramms bei `$2000`:

| Adresse | Inhalt |
|---|---|
| `$2000` | `JMP main` |
| `$2003` | Version des Messprogramms (1) |
| `$2004` | Modus, vom Harness vor dem Start gesetzt: Bit 0 = Reg-36-Wartezeit vom Harness (1) oder über die Jiffy-Uhr (0, echte Hardware); Bit 1 = Reg-36-Test auslassen; Bit 2 = Kurzlauf (nur Konfiguration 2 MHz/Reg 36 = 0, L nur aus der Teilmenge) |
| `$2005` | `r36_flag` (Harness setzt 1 = Wartezeit vorbei) |
| `$2006–$2007` | `r36_frames`: Frames der aktuellen Wartezeit |

`loader.bas`:

```
10 BLOAD"VDCBASIC2H",B0,P(4864):SYS4864
20 BLOAD"VDCBENCH",B0,P(8192)
30 BANK15:SYS8192
40 BANK15:POKE DEC("D7FF"),0
```

`loader-hw.bas` ersetzt Zeile 40 durch `40 BSAVE"@BENCHRES",B0,P(16384) TO P(32768):PRINT"FERTIG"`.

`main`:

1. Zustand sichern: `$FF00`, `$D011`, `$D030`, CIA2-ICR-Maske (wird auf `$7F` gesetzt = alle CIA2-NMIs aus), VDC-Register 12/13, 20/21, 24, 25, 26, 28, 36.
2. VIC austasten (`$D011` Bit 4 = 0), damit keine Badlines Zyklen stehlen.
3. VDC-Konfiguration (Abschnitt 4.1) setzen.
4. Ergebnisblock-Kopf schreiben.
5. Für jede Konfiguration (Abschnitt 6.2): `SEI`, `$FF00 = $3E`, Takt setzen (`$D030` Bit 0), Messfälle 0–10 ausführen, `$FF00` zurück, `CLI`.
6. Reg-36-Test (Abschnitt 9), außer Modus Bit 1.
7. Abschlussdatensatz (`$FF`) schreiben, Zustand wiederherstellen, Label **`bench_done`**, `RTS`.

### 4.1 VDC-Konfiguration (wie Crawl, `crawl.bas` 10065–10135)

Alle Registeränderungen außer den hier genannten bleiben auf dem Kernal-Stand. Reg 24, 25, 28 nur per Read-Modify-Write.

| Reg | Wert | Bedeutung |
|---|---|---|
| 0 | 127 | horizontale Gesamtlänge |
| 4 | 159 | vertikale Gesamtlänge |
| 6 | 120 | angezeigte Zeilen |
| 7 | 133 | vertikale Sync-Position |
| 9 | 1 | 2 Rasterzeilen je Zeile |
| 25 | Bit 7 = 1 | Bitmap-Modus (RMW) |
| 28 | Bit 4 = 1 | 64-KB-RAM (RMW) |
| 26 | `$FF` | Farben |
| 12/13 | `$B4FF` | Bitmap (wie `VR(0)` in Crawl) |
| 20/21 | `$0000` | Attribute (`AP`) |
| 36 | je Konfiguration 0 oder 5 | Refresh |

VRAM-Bereiche: Anzeige Attribute `$0000–$257F`, Bitmap `$B4FF–$FFFF`; **Messbereich Ziel** `$5000–$6FFF`, **Messbereich Quelle** (Copy, VMC, VMS) `$7000–$8FFF`, **Reg-36-Testbereich** `$9000–$AFFF`. Vor Fall 0 wird die Bitmap mit `$0F` gefüllt (Block fill, wie Crawl), damit die Anzeige dem Chunky-Modus entspricht.

## 5. Zeitmessung (CIA 2, µs)

CIA 2 Timer A zählt φ2 (1 MHz, auch im 2-MHz-Modus), Timer B zählt die Unterläufe von Timer A. Das ergibt 32 Bit in µs; gespeichert werden 24 Bit.

```
t_start:                  ; X, Y bleiben erhalten
    lda #$00
    sta $DD0E             ; Timer A stopp
    sta $DD0F             ; Timer B stopp
    lda #$FF
    sta $DD04
    sta $DD05
    sta $DD06
    sta $DD07
    lda #%01010001        ; Timer B: zählt Unterläufe von A, force load, start
    sta $DD0F
    lda #%00010001        ; Timer A: φ2, continuous, force load, start
    sta $DD0E
    rts
t_stop:                   ; -> t_lo/t_mid/t_hi = $FFFFFFFF - Zählerstand (24 Bit)
    lda #$00
    sta $DD0E             ; A stopp (B zählt dann nicht mehr)
    ... TB hi/lo, TA hi/lo lesen, invertieren (EOR #$FF), 24 Bit ablegen
    rts
```

- Die Messung umfasst `jsr t_start` bis `jsr t_stop`. Fall 0 (leer) misst genau diesen Rahmen mit derselben Schleifenstruktur; `evaluate.py` zieht ihn ab.
- Jede Messung wiederholt den Vorgang `reps`-mal in einer Schleife **innerhalb** des Zeitfensters (Ausnahme: „issue“-Fälle, 6.1). Die Schleife ist überall gleich: `ldy #reps` … `dey / bne` (Y bleibt für den Fall frei: Fälle, die Y brauchen, sichern es in einer Zeropage-Zelle `$FB`).
- Zeropage: nur `$FA–$FF` und die Argumente von VDC-BASIC `$83–$8E` (nur Fall 8–10).
- Die Zeit in CPU-Zyklen ist µs × 1 (1 MHz) bzw. µs × 2 (2 MHz). Das ist eine Umrechnung, keine zweite Messung.

**Gegenprobe (einmal, nur VICE):** Für Fall 1 mit N = 256 setzt das Harness zusätzlich Haltepunkte auf die Labels `xchk_a` und `xchk_b` direkt vor `t_start` und nach `t_stop` und liest dort die Stoppuhr des Monitors (`r`, letzte Spalte, wie `test/vice_drive.py` im Engine-Repo). Die Differenz in µs (1 MHz) muss ±20 mit dem CIA-Wert übereinstimmen; im 2-MHz-Modus wird nur das Verhältnis protokolliert (klärt die Frage aus Abschnitt 2).

## 6. Messfälle

Makros (ca65):

```
.macro vwait
:   bit $D600
    bpl :-
.endmacro
.macro vsel r             ; Register wählen
    ldx #r
    stx $D600
.endmacro
.macro vput               ; A ins gewählte Register
    vwait
    sta $D601
.endmacro
```

Vor jedem Fall: Zieladresse (Reg 18/19) auf den Anfang des Messbereichs Ziel setzen, bei Copy/VMC/VMS Quelle auf den Messbereich Quelle; dann `vwait` (VDC bereit). Alles außerhalb des Zeitfensters.

| ID | Name | Parameter p | Ablauf je Wiederholung (im Zeitfenster) | reps |
|---|---|---|---|---|
| 0 | `empty` | – | nichts (nur Schleife) | 16 |
| 1 | `stream_const` | N | `vsel 31` einmal vor dem Fenster, `lda #$55`; dann N-mal `vwait / sta $D601` (Schleife `ldx #N … dex / bne`, N = 256 als `ldx #0`) | 16 |
| 2 | `stream_lit` | N | wie 1, aber `lda src,x` aus einem RAM-Puffer (`ldx #0 … inx / cpx #N / bne`, N = 256 als `cpx #0`) | 16 |
| 3 | `new_addr` | – | Reg 18 und 19 schreiben (`vsel 18 / lda #hi / vput / vsel 19 / lda #lo / vput`), `vsel 31`, ein Byte `vput` | 16 |
| 4 | `fill_done` | L | [Variante rmw: `vsel 24 / vwait / lda $D601 / and #$7F / vput`] · `vsel 31 / lda #$AA / vput` (erstes Byte) · für L ≥ 2: `vsel 30 / lda #L−1 / vput` · `vwait` (fertig) | 16 |
| 5 | `fill_issue` | L | wie 4 ohne das abschließende `vwait`; Messung je Einzelvorgang (6.1) | 16 |
| 6 | `copy_done` | L | [rmw: `vsel 24 / vwait / lda $D601 / ora #$80 / vput`] · Quelle: `vsel 32 / lda #hi / vput / vsel 33 / lda #lo / vput` · `vsel 30 / lda #L&255 / vput` · `vwait` | 16 |
| 7 | `copy_issue` | L | wie 6 ohne das letzte `vwait`; Einzelvorgang (6.1) | 16 |
| 8 | `vmc` | w, h | `jsr remember_mem_conf`; `arg1` = Quelle, `arg2` = Ziel, `arg3` = w, `arg4` = h, `arg5` = 80, `arg6` = 0; `jsr vmc_execute` | 4 |
| 9 | `vms1` | w, h | Liste bei `$3000`: Alpha 0, Anzahl h, h × Typ 1 (Offset r × 80, Länge w); `jsr remember_mem_conf`; `arg1` = Quelle, `arg2` = Ziel, `arg3` = `$3000`; `jsr vms_execute` | 4 |
| 10 | `vms8` | n | Liste bei `$3000`: Alpha 0, Anzahl n, n × Typ 8 (Offset 0 … n−1); sonst wie 9 | 4 |

Hinweise:

- Bei 4 und 6 wird die Zieladresse **nicht** neu gesetzt: Nach einer Block-Operation steht Reg 18/19 hinter dem Ende, das Ziel läuft also durch (wie in `RXV`). 16 × 256 Byte = 4 KB passen in den Messbereich (8 KB).
- Variante „norm“ von 4: Reg 24 Bit 7 = 0 einmal vor dem Fenster setzen. Variante „norm“ von 6: Bit 7 = 1 vorher setzen.
- Reg 30 = 0 bedeutet beim VDC 256. Fill L = 256 schreibt also Reg 30 = 255, Copy L = 256 Reg 30 = 0.
- Fall 8–10: Die Routinen laufen mit `$FF00 = $3E` (gesetzt durch `remember_mem_conf`) und stellen am Ende den gemerkten Wert her; der ist dann ebenfalls `$3E`. `vms_execute` braucht `saved_mem_conf` (setzt `remember_mem_conf`).
- Die Zieladressen von 8–10 beginnen je Wiederholung neu am Anfang des Messbereichs (Teil der Setup-Kosten, die sich bei der Auswertung über die Steigung herauskürzen).

### 6.1 Einzelvorgänge („issue“)

Für 5 und 7 gilt je Wiederholung: `jsr t_start`, Vorgang, `jsr t_stop`, Zeit zu einer 24-Bit-Summe addieren, danach `vwait` außerhalb des Fensters. Gespeichert wird die Summe; Fall 0 wird dafür zusätzlich in derselben Form gemessen (Datensatz mit Variante-Bit 2 = „einzeln“).

### 6.2 Parameter und Konfigurationen

| Konfiguration | Takt | Reg 36 | Parameter |
|---|---|---|---|
| A (Engine) | 2 MHz | 0 | Fall 4, 6: L = 1 … 256 (alle), beide Varianten; Fall 5, 7: L ∈ M |
| B | 2 MHz | 5 | L ∈ M |
| C | 1 MHz | 0 | L ∈ M |
| D | 1 MHz | 5 | L ∈ M |

M = {1, 2, 3, 4, 5, 6, 8, 10, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256}. Fall 1, 2: N ∈ {1, 2, 4, 8, 16, 32, 64, 128, 256}. Fall 8, 9: w ∈ {1, 2, 4, 8, 16, 32, 64} × h ∈ {1, 16}. Fall 10: n ∈ {1, 8, 32, 128}. Im Kurzlauf (Modus Bit 2) nur Konfiguration A mit L ∈ M.

## 7. Ergebnisblock (Bank 0, ab `$4000`)

| Offset | Inhalt |
|---|---|
| 0–1 | `"B" "R"` |
| 2 | Version 1 |
| 3 | Version des Messprogramms |
| 4–5 | Anzahl Datensätze |
| 6–7 | reserviert 0 |
| 8 … | Datensätze zu 8 Byte, Ende = Datensatz mit Fall `$FF` |

Datensatz:

| Byte | Inhalt |
|---|---|
| 0 | Fall-ID (0–11, `$FF` = Ende) |
| 1 | Konfiguration: Bit 0 = 2 MHz, Bit 1 = Variante rmw, Bit 2 = Einzelvorgang (6.1), Bit 4–7 = Reg-36-Wert |
| 2–3 | Parameter (L, N, n; bei 8/9: Low = w, High = h; bei 11: Frames) |
| 4 | reps (bei 11: Reg-36-Wert) |
| 5–7 | Zeit in µs, 24 Bit, roh (inklusive Rahmen); bei 11: Anzahl fehlerhafter Bytes |

Platzbedarf etwa 1 700 Datensätze = 13,6 KB (< 16 KB). Das Messprogramm prüft die Grenze `$7FF8` und bricht sonst mit Datensatz `$FE` ab.

## 8. Harness `run_bench.py`

Aufruf: `test/bench/run_bench.py [--short] [--no-r36] [--port N] [--out test/bench/results] [--from-file BENCHRES.prg --source c128]`

**Monitor-Port (Pflicht, gilt für alle VICE-Tests dieses Projekts):**

- Port aus `--port`, sonst aus der Umgebungsvariablen `DBE_VICE_PORT`, sonst **6631** (nicht 6510, das ist der VICE-Standard und wird von anderen Sitzungen benutzt). Parallel laufende Tests dieses Projekts nutzen 6631–6649, jeder Test/Agent einen eigenen Port.
- Vor dem Start prüfen, ob der Port frei ist (`socket.bind(("127.0.0.1", port))` und sofort schließen). Belegt: nächsten freien Port im Bereich 6631–6649 nehmen und das melden; ist keiner frei oder wurde `--port` ausdrücklich angegeben und ist belegt, mit klarer Meldung abbrechen (Exitcode 2).
- VICE bekommt den Port ausdrücklich: `-remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>`. Kein `-binarymonitor`.
- Beenden nur des selbst gestarteten Prozesses (`Popen`-Objekt: `terminate()`, nach Timeout `kill()`, im `finally`). Kein `pkill`/`killall` auf `x128`/`x64sc`.
- Der Monitor verbindet sich nur mit genau diesem Port; antwortet dort kein VICE mit Prompt, Abbruch (nicht an einen fremden VICE anhängen).

Ablauf (ereignisgesteuert, keine festen Wartezeiten; Timeout nur als Wächter, Standard 600 s):

1. Build (3.1), Disk, Lader.
2. VICE starten:
   ```
   x128 -default -warp -sounddev dummy -80col -VDC64KB -debugcart \
        +drive8truedrive -trapdevice8 -8 build/bench/bench.d64 \
        -autostartprgmode 1 -autostart build/bench/loader.prg \
        -remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>
   ```
   `<port>` nach der Portregel oben.
3. Monitor verbinden (Klasse `Monitor` aus `dungeonboringengine/test/vice_drive.py` übernehmen: Verbindungsaufbau mit Wiederholung nur bis der Port offen ist, Prompt-Zählung). Haltepunkte aus `build/bench/vdcbench.lbl` (`al C:xxxx .name`): `main`, `bench_done`, `r36_wait_enter`, `xchk_a`, `xchk_b`; dazu der BASIC-Fehlervektor (`($0300)`, wie `vice_drive.py`).
4. Bei `main`: Modus nach `$2004` schreiben (`> 2004 xx`), weiter.
5. Bei `xchk_a`/`xchk_b`: Stoppuhr lesen (5, Gegenprobe), weiter.
6. Bei `r36_wait_enter`: `r36_frames` lesen; Haltepunkt auf die IRQ-Routine aus `($0314)` aktivieren; so oft `x`, bis F Treffer gezählt sind; Haltepunkt deaktivieren; `> 2005 01`; weiter. (Das Programm wartet in dieser Zeit in einer Schleife auf `r36_flag`, mit `CLI`, `$FF00 = $00`.)
7. Bei `bench_done`: `bank ram`, `save "build/bench/benchres.prg" 0 4000 7fff`, dann `quit`.
8. Bei BASIC-Fehler: Fehlernummer und Zeile ausgeben, Exitcode 1.
9. `evaluate.py` aufrufen.

`--from-file`: nur Auswertung eines auf echter Hardware gespeicherten `BENCHRES` (PRG, Ladeadresse `$4000`).

## 9. Reg-36-Test (Fall 11)

Für r = 0 … 5 und F ∈ {1, 50, 500}:

1. `SEI`, `$FF00 = $3E`: Testbereich `$9000–$AFFF` (8 KB) über Reg 31 mit einem 8-Bit-LFSR füllen (Galois, Polynom `$B8`: `lsr; bcc +; eor #$B8`; Startwert `r × 16 + (F AND 15) + 1`, nie 0).
2. Reg 36 = r (direkt schreiben, kein Bit muss erhalten werden).
3. `r36_frames` = F, `r36_flag` = 0, `$FF00 = $00`, `CLI`. Label `r36_wait_enter`, dann warten:
   - Modus Bit 0 = 1: bis `r36_flag` ≠ 0 (Harness zählt die IRQs, 8.6).
   - Modus Bit 0 = 0: bis sich die Jiffy-Uhr (`$A2`) F-mal geändert hat.
4. `SEI`, `$FF00 = $3E`, Reg 36 = 5, Testbereich zurücklesen (Reg 31), mit demselben LFSR vergleichen, Fehler zählen.
5. Datensatz: Fall 11, Konfiguration Bit 4–7 = r, Parameter = F, reps = r, Zeit-Feld = Fehlerzahl.

Takt: 2 MHz. Erwartung VICE: 0 Fehler überall (Reg 36 wird nicht emuliert).

## 10. Auswertung `evaluate.py`

### 10.1 Rohwerte

Für jeden Datensatz: `us = roh − leer` (passender Fall-0-Datensatz derselben Konfiguration; bei Bit 2 der Einzelvorgangs-Leerwert × reps), dann `us_per_op = us / reps`, `cycles_per_op = us_per_op × MHz`.

### 10.2 Abgeleitete Größen je Konfiguration (Zyklen der CPU)

| Schlüssel | Berechnung |
|---|---|
| `cpu_byte` | Steigung (lineare Regression) von Fall 1 über N, geteilt durch 1 (je Byte) |
| `cpu_run_fixed` | Achsenabschnitt derselben Regression |
| `cpu_lit_byte` | Steigung von Fall 2 |
| `new_addr` | Fall 3 je Vorgang − `cpu_byte` |
| `fill_fixed`, `fill_byte` | Regression `fill_done(L)` = fixed + byte × L über L ≥ 2, Variante norm |
| `fill_fixed_rmw` | dasselbe, Variante rmw |
| `fill_issue_fixed`, `fill_issue_byte` | Regression über Fall 5 |
| `copy_fixed`, `copy_byte`, `copy_fixed_rmw` | wie fill, Fall 6 (L ≥ 1) |
| `vmc_byte`, `vmc_row`, `vmc_call` | Modell `t = call + h × (row + w × byte)`, kleinste Quadrate über Fall 8 |
| `vms1_byte`, `vms1_cmd`, `vms1_call` | dasselbe Modell über Fall 9 (je Befehl = je Zeile) |
| `vms8_byte`, `vms8_call` | Regression über Fall 10 |
| `T`, `T_rmw` | siehe 10.3 |

### 10.3 Schwelle T

`t_cpu(L) = cpu_run_fixed + cpu_byte × L` (CPU-Lauf), `t_fill(L)` = gemessenes `fill_done(L)` (Konfiguration A: jeder Wert; sonst linear zwischen den Werten aus M interpoliert). **T = kleinstes L ≥ 2, für das `t_fill(L') < t_cpu(L')` für alle L' von L bis 256 gilt**; gibt es keines, T = 257 („Block fill lohnt nie“). `T_rmw` ebenso mit Variante rmw. Zusätzlich `T_issue` mit `fill_issue` statt `fill_done` (gilt, wenn `RXV` während des Fills schon das nächste Token liest; nur Information).

### 10.4 Ausgabe

`results/<quelle>-<JJJJMMTT>.json`:

```json
{
  "meta": {"source": "vice3.10", "date": "2026-11-25", "bench_version": 1,
           "vdcbasic": "v2h", "provisional": true,
           "provisional_reason": "VICE 3.10: VDC-Busy-Modell grob (43/4 Takte je Reg-31-Zugriff, Fill 0,66, Copy 1,2 Takte je Byte), Reg 36 ohne Wirkung",
           "vdc_config": {"r0": 127, "r4": 159, "r6": 120, "r7": 133, "r9": 1, "bitmap": true, "ram64k": true},
           "warnings": []},
  "records": [{"case": "fill_done", "variant": "norm", "clock_mhz": 2, "r36": 0, "param": 17,
               "reps": 16, "us_total": 123, "us_per_op": 7.69, "cycles_per_op": 15.4}],
  "derived": {"A": {"cpu_byte": 0.0, "...": 0.0, "T": 0}, "B": {}, "C": {}, "D": {}},
  "engine": {"config": "A", "cpu_byte": 0.0, "cpu_lit_byte": 0.0, "cpu_run_fixed": 0.0, "new_addr": 0.0,
             "fill_fixed": 0.0, "fill_byte": 0.0, "copy_fixed": 0.0, "copy_byte": 0.0, "T": 0},
  "r36": [{"r36": 0, "frames": 1, "errors": 0}]
}
```

`engine` ist die Schnittstelle zu `tools/vrl.py --bench` (P1): Schlüssel und Einheit (CPU-Zyklen bei 2 MHz) sind fest.

`results/<…>.md` (Markdown, für die Doku): Kopf mit Quelle, Datum und dem Vermerk **„vorläufig (VICE)“**; Tabellen:

1. Kostenparameter je Konfiguration (Zeilen = Schlüssel aus 10.2, Spalten A–D, Zyklen; µs in Klammern).
2. CPU gegen Block fill für L ∈ M in Konfiguration A: `t_cpu`, `t_fill` norm, `t_fill` rmw, `t_issue`, schneller (`CPU`/`Fill`).
3. T, T_rmw, T_issue je Konfiguration.
4. `VMC` / `VMS` je Byte, je Zeile/Befehl, je Aufruf.
5. Reg 36: r × F → Fehler.
6. Warnungen (Plausibilität, Abschnitt 2; Gegenprobe 5).

## 11. Echte Hardware

`loader-hw.bas` auf Diskette, `RUN`. Modus `$2004` bleibt 0 (Jiffy-Uhr), der Lader speichert `BENCHRES`. Auswertung: `run_bench.py --from-file BENCHRES.prg --source c128`. Die JSON-Ausgabe trägt dann `"provisional": false`. Auf echter Hardware ist der Reg-36-Test aussagekräftig; F = 500 (etwa 8 s) zeigt, ob Daten außerhalb der Anzeige ohne Refresh verloren gehen.

## 12. Abnahme

| Prüfung | Erwartung |
|---|---|
| `run_bench.py` | läuft ohne Rückfrage durch, Exitcode 0, erzeugt `.json` und `.md` |
| Laufzeit | Kurzlauf < 1 min, Volllauf < 10 min (Warp) |
| Port | Standard 6631 bzw. `DBE_VICE_PORT`; belegter Port wird erkannt; nur der eigene VICE-Prozess wird beendet |
| keine festen Wartezeiten | kein `sleep` im Harness außer dem Wiederholen des Verbindungsaufbaus (wie `vice_drive.py`) |
| Wiederholbarkeit | zwei Läufe in VICE: alle `us_total` gleich oder ±1 |
| Plausibilität | Warnungen nach Abschnitt 2 sind leer oder begründet |
| Gegenprobe | CIA gegen Monitor-Stoppuhr ±20 µs (1 MHz) |
| Reg 36 in VICE | 0 Fehler |
| Repo | nur `test/bench/*`, `docs/v3/P0-bench.md`, `.gitignore`; `vdcbasic2h.bin` unverändert |
