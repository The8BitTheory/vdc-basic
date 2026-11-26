# P2g – Experiment: weniger VDC-Ready-Waits (Z64K)

Stand: 2026-11-26. Auftrag des Nutzers: prüfen, ob die Ready-Waits in der VMC-Zeilenschleife (P2f) reduziert werden können, z. B. „nur jede 2. bis 8. Zeile warten“, dazu Reg 36 (Refresh-Zyklen je Zeile) auf 0 oder 1. **Nur Experiment:** `src/vdcbasic.asm` ist unverändert, nichts ist committet; die Entscheidung trifft der Nutzer.

## 1. Kurzfassung

- **In Z64K braucht eine Zeile genau zwei Waits:** vor dem **ersten Zugriff nach R30** (R18 bzw. R32; wartet auf das Ende der Kopie der Vorzeile) und **vor R30** (wartet auf das Vorlesen, das das Schreiben von R19 auslöst). Die Waits vor R19 (und mit Quellschritt vor R33 und R18) sind überflüssig: alle 936 Läufe ohne sie sind bytegleich.
- **„Nur jede N-te Zeile warten“ geht in Z64K nicht.** Bei N = 2, 4, 8 und ganz ohne Wait waren 2 von 1 080 Läufen richtig, und zwar bei jeder Länge (4–40 Byte), bei 1 und 2 MHz und bei jedem Reg 36. Grund: Im Chunky-Modus der Engine hat der VDC während der Anzeige nur etwa **16 freie Speicherzyklen je Zeichenzeile** (2 Rasterzeilen, 126 µs). Schon 1 Byte Kopie braucht dann bis zu 127 µs, die CPU aber nur 16 µs (2 MHz) bzw. 33 µs (1 MHz) von R30 bis zum nächsten R18. Fertig ist der VDC rechtzeitig nur in der Austastung, und das lässt sich in der Schleife nicht billiger prüfen als mit dem Wait selbst.
- **Ersparnis ohne die überflüssigen Waits** (P2f-Schleife sonst gleich, 2 MHz, Reg 36 = 0 wie Crawl): −5 … −17 % je Zeile bei 4–24 Byte, bei 40 Byte −1 %. Mit Quellschritt −5 … −10 %. Bei 1 MHz −2 … −6 % bzw. −4 … −10 %. **Crawl-Bild (≈ 435 Zeilen): ≈ 4–5 ms bei 2 MHz, ≈ 3 ms bei 1 MHz**, also höchstens ≈ 5 % bzw. 2 % des Crawl-Laufs.
- **Der größere Hebel ist die Schleife selbst** (Nebenbefund): Eine nicht entrollte Schleife nur für Längen 1–255 ohne Füllweiche, mit Adresse vorher in A/Y (`R-ACt`, 79 statt 98 Takte) spart bei 2 MHz −19 … −35 % je Zeile bei 4–24 Byte (mit Quellschritt −8 … −19 %). Das wären **≈ 12 ms je Crawl-Bild** (2 MHz) bzw. ≈ 8 ms (1 MHz). Entrollen bringt kaum mehr.
- **Reg 36 in Z64K:** Der Wert zählt nur die Refresh-Zyklen je Rasterzeile (0 = keiner). Datenverlust bildet Z64K **nicht** nach. 0 und 1 sind gleich schnell (±2 %). 5 ist bei 2 MHz und 8–24 Byte deutlich langsamer (P2f-Schleife bis +52 %). **Hardware-Risiko:** Mit 0 (Crawl seit 1.0) refresht der VDC nur über die Anzeige. In der vertikalen Austastung gibt es etwa 5 ms lang gar keinen Refresh. Reg 36 = 1 kostet in Z64K nichts gegenüber 0 und ist sicherer. Klären muss das ein Test auf echter Hardware (Abschnitt 6).

## 2. Was Z64K beim Zugriff macht

Quelle: `Z64K.jar`, Klasse `mos/videochip/vdc/MOS8563` (Bytecode mit `jdk.internal.classfile` aus Java 21 gelesen, Werkzeug in `build/p2g/jar/`). Der VDC hat einen Speicherzyklus je Zeichentakt (0,5 µs), und jeder Zyklus ist genau einer von: Grafik holen, Attribut holen, Refresh, Blockbefehl/CPU-Zugriff („BUSY“) oder frei. Das Statusbit 7 ist `!busy`.

| Zugriff | Z64K | wenn noch busy |
|---|---|---|
| `$D600` schreiben (Register wählen) | sofort | ohne Folgen |
| R18 (Adresse high) | sofort, **kein** busy | ändert das Ziel einer laufenden Kopie → falsche Bytes |
| R19 (Adresse low) | sofort; im Ruhezustand busy, liest das Byte an der Adresse vor (1 Zyklus) | ändert das Ziel einer laufenden Kopie, kein Vorlesen |
| R32/R33 (Quelle) | sofort, kein busy | ändert die Quelle einer laufenden Kopie |
| R30 (Zähler) | busy, startet Kopie/Füllen | **nur der Zähler wird gesetzt, kein Start** → Zeile fehlt (bzw. die laufende Kopie wird länger) |
| R31 schreiben | busy: schreiben, weiter, vorlesen | nur das Datenlatch ändert sich, **kein Schreiben** |
| R31 lesen | gibt den vorgelesenen Wert, dann weiter und vorlesen | alter Wert, kein Weiterzählen |

Kosten in Speicherzyklen: Kopie 2 je Byte + 3 (Ende, Vorlesen), Füllen 1 je Byte + 3. In der Austastung ist jeder Zyklus frei: Kopie ≈ 1 µs/Byte, Füllen ≈ 0,5 µs/Byte (gemessene Minima). In der Anzeige des Chunky-Modus (R9 = 1, Bitmap + Attribute) holt der VDC je Zeichenzeile 2 × 80 Grafik- und 80 Attributbytes. Von 256 Zeichentakten bleiben ≈ 16 − 2 × R36 frei, einmal je Zeichenzeile (126 µs). Gemessen passen in ein solches Fenster: Kopie von 6 Byte (R36 = 0), 4 Byte (R36 = 1) und 1 Byte (R36 = 5).

Reg 36: Am Ende der horizontalen Anzeige werden `R36 & 15` Refresh-Zyklen eingelegt (0 = keiner). Das VRAM ist ein `int[]` ohne Zerfall. R36 hat keinen Einfluss auf die Darstellung oder auf andere Zugriffe. Datenverlust durch fehlenden Refresh kann Z64K also nicht zeigen.

## 3. Methode

- Programm `test/bench/waits/waits.py` (erzeugt `waits.a`, ACME). Es läuft eigenständig ohne VDC-BASIC und ist damit auch das Hardware-Testprogramm. Code liegt bei `$2000`, Ergebnisse bei `$6000` (Bank 0, `BSAVE "RESn"`).
- Bildschirm wie Crawl: R0 127, R4 159, R6 120, R7 133, R9 1, R25 |= `$80`, R28 |= 24, R26 `$FF`, Bitmap `$B4FF` = `$0F`, Attribute ab `$0000`. Der VIC ist ausgetastet (auch bei 1 MHz keine Badlines), die Interrupts sind aus, gemessen wird mit CIA 2 (Timer A + B kaskadiert, µs). Am Ende stellt das Programm die gesicherten VDC-Register wieder her.
- **Schleifenfälle:** Kopie von L Byte je Zeile aus `$8000` (LFSR-Muster) nach `$0000` (Zeilenabstand 80, sichtbarer Attributbereich). Gemessen werden 8 und 128 Zeilen; Zeit je Zeile = (t128 − t8) / 120. Jede Messung beginnt am Anzeigebeginn (Statusbit 5) mit drei Verzögerungen (0, ⅓, ⅔ Bild). Nach jeder der 3 Wiederholungen wird ein Fletcher-16 über `$0000–$27FF` gegen den in Python berechneten Sollwert geprüft (vorher Ziel gelöscht). Bei `A`, `C` und `none` mit L = 8 und 40 werden zusätzlich die Zeilen 0–15 ausgelesen.
- **Dauerfälle:** Zeit vom Registerzugriff bis zum Ready-Bit (Abfrage alle 7 Takte), 200 Stichproben mit zufälliger Pause (0–1 275 Takte) nach dem Leerlauf. Die Stichproben überwiegen in der Austastung, weil sie dort schnell fertig sind. Median und Anteil sind deshalb zu günstig; das Maximum zeigt die Anzeige.
- **Varianten** (Wait = `bit $d600 / bpl`, 6 Takte wenn bereit; „A“ = erster Zugriff der Zeile, „B“ = übrige Adressregister, „C“ = vor R30):

| Variante | Aufbau | CPU-Takte/Zeile |
|---|---|---|
| `p2f` | Kopie der P2f-Schleife (nicht entrollt, Waits A, B, C) | 98 |
| `p2f-AC` | dieselbe ohne Wait vor R19 | 92 |
| `p2fS` / `p2fS-AC` | P2f mit Quellschritt (R32/R33 je Zeile) / ohne Waits vor R33, R18, R19 | 156 / 138 |
| `R-ACt` / `RS-ACt` | nicht entrollt, nur L 1–255, ohne Füllweiche; Adresse vorher in A/Y, Register vorher gewählt; Waits A, C (dec/bne) | 79 / 125 |
| `full`, `AC`, `A`, `C`, `none` | 8-fach entrollt, Ziel: alle Waits / A+C / nur A / nur C / keine | 78 / 72 / 66 / 66 / 60 |
| `AC/N`, `all/N` (N = 2, 4, 8) | entrollt; A nur in jeder N-ten Zeile (C jede) bzw. A, B, C nur in jeder N-ten Zeile | 62–69 |
| `S-…`, `ACt`, `S-ACt` | dasselbe mit Quellschritt; `…t` wie `R-ACt`, aber entrollt | |

- Konfigurationen: 1 und 2 MHz × Reg 36 = 5 (Kernal), 1, 0; L ∈ {4, 8, 12, 16, 24, 40}. Jede Konfiguration ist ein eigener Z64K-Lauf (eigenes Xvfb, 25–150 s). Wiederholte Fälle aus verschiedenen Läufen weichen um bis zu ±4 % ab, innerhalb eines Laufs (3 Lagen) kaum.

## 4. Ergebnisse

### 4.1 Welche Zugriffe brauchen ein Wait? (Frage 1)

| Zugriff in der Zeile | Wait nötig? | Beleg |
|---|---|---|
| R18 (ohne Quelle) bzw. R32 (mit Quelle), erster Zugriff nach R30 | **ja**: Kopie der Vorzeile muss fertig sein | `C` (ohne): 0/108 richtig; Auszug: Zeilen „falsch“ (Ziel mitten in der Kopie geändert) |
| R33, R18 (mit Quelle), R19 | **nein** (R18, R32, R33 machen nicht busy) | `AC`, `S-AC`, `p2f-AC`, `p2fS-AC`, `ACt`, `S-ACt`, `R-ACt`, `RS-ACt`: 936/936 richtig; Dauer R18: 0 µs |
| R30 | **ja**: Vorlesen nach R19 muss fertig sein (Median 28–38 µs, max 116–126 µs) | `A` (ohne): 0/108; Auszug: 13–16 von 16 Zeilen „leer“ (Start verloren) |
| R31 (VMF, nach R19) und R30 nach R31 | ja (beide machen busy) | aus dem Code; Dauer R31: wie R19 |
| `$D600` (Register wählen) | nein | P2f wählt schon jetzt ohne Wait |

Die Hypothese stimmt: Nach R30 läuft die Kopie; erst der nächste Zugriff muss auf ihr Ende warten, die Adressregister danach nicht. R30 muss aber erneut warten, und zwar auf das Vorlesen, das R19 auslöst.

### 4.2 Dauer bis Ready (Frage 2)

Median / max in µs; in Klammern der Anteil der Stichproben, die nach ≤ 16 µs (2 MHz) bzw. ≤ 33 µs (1 MHz) fertig waren. Das ist die CPU-Zeit von R30 bis zum nächsten R18 ohne Wait. Der Anteil ist zu günstig, siehe Abschnitt 3.

| Zugriff | 2 MHz R36=0 | 2 MHz R36=1 | 2 MHz R36=5 | 1 MHz R36=0 | 1 MHz R36=1 | 1 MHz R36=5 |
|---|---|---|---|---|---|---|
| R19 (Vorlesen) | 38 / 116 (39 %) | 34 / 120 (40 %) | 35 / 124 (40 %) | 35 / 119 (49 %) | 35 / 119 (50 %) | 28 / 126 (51 %) |
| R31 schreiben | 38 / 120 (39 %) | 42 / 123 (35 %) | 42 / 125 (34 %) | 28 / 119 (53 %) | 28 / 119 (54 %) | 35 / 126 (48 %) |
| Kopie 1 | 46 / 120 (36 %) | 46 / 120 (35 %) | 51 / 127 (31 %) | 35 / 119 (49 %) | 35 / 119 (49 %) | 35 / 126 (46 %) |
| Kopie 4 | 50 / 124 (33 %) | 50 / 127 (32 %) | 158 / 255 (36 %) | 35 / 126 (47 %) | 42 / 126 (44 %) | 161 / 252 (28 %) |
| Kopie 6 | 50 / 127 (32 %) | 161 / 244 (30 %) | 283 / 379 (34 %) | 35 / 126 (44 %) | 154 / 245 (29 %) | 280 / 378 (29 %) |
| Kopie 8 | 148 / 244 (35 %) | 158 / 251 (34 %) | 412 / 505 (33 %) | 154 / 245 (28 %) | 154 / 245 (28 %) | 406 / 504 (29 %) |
| Kopie 16 | 284 / 372 (20 %) | 288 / 376 (18 %) | 652 / 756 (15 %) | 280 / 371 (31 %) | 287 / 371 (30 %) | 658 / 756 (34 %) |
| Kopie 40 | 636 / 752 (0 %) | 651 / 756 (0 %) | – / 1 766 (0 %) | 651 / 749 (0 %) | 658 / 756 (0 %) | 1 547 / 1 764 (0 %) |
| Kopie 64 | 864 / 1 130 (0 %) | 850 / 1 256 (0 %) | – / 2 775 (0 %) | 1 022 / 1 127 (0 %) | 1 148 / 1 253 (0 %) | – / 2 772 (0 %) |
| Füllen 40 | 294 / 379 (0 %) | 410 / 497 (0 %) | 884 / 1 008 (0 %) | 287 / 378 (30 %) | 406 / 497 (29 %) | 896 / 1 008 (37 %) |

(„–“: Median in der Austastung, nicht aussagekräftig.) Minimum in der Austastung: Kopie L ≈ L µs, Füllen ≈ L/2 µs. **Garantiert fertig ist der VDC beim nächsten Zugriff bei keiner Länge**, nicht einmal bei 1 Byte: Liegt der Start in der Anzeige hinter dem freien Fenster, dauert es bis zum nächsten Fenster (bis 127 µs). Die Maxima sind Vielfache von ≈ 126 µs (eine Zeichenzeile).

### 4.3 Weniger Waits in der Schleife (Frage 3)

Richtig (Fletcher gleich) über alle 6 Konfigurationen × 6 Längen × 3 Wiederholungen:

| Varianten | richtig |
|---|---|
| `p2f`, `full`, `S-full`, `p2fS` (alle Waits) | 756 / 756 |
| ohne B: `AC`, `S-AC`, `p2f-AC`, `p2fS-AC`, `ACt`, `S-ACt`, `R-ACt`, `RS-ACt` | 936 / 936 |
| `A` (ohne C), `C` (ohne A) | 0 / 216 |
| `AC/2`, `AC/4`, `AC/8`, `S-AC/2…8` | 2 / 648 |
| `all/2`, `all/4`, `all/8`, `none`, `S-none` | 0 / 540 |

Zeit je Zeile (µs; bei 1 MHz = Takte), nur die richtigen Varianten. `p2f…` und `R-…` stammen aus denselben Läufen (Mittel aus zwei Läufen, wo vorhanden):

**2 MHz, Reg 36 = 0 (Crawl):**

| Variante | L=4 | L=8 | L=12 | L=16 | L=24 | L=40 |
|---|---|---|---|---|---|---|
| `p2f` (heute) | 111 | 115 | 131 | 139 | 164 | 216 |
| `p2f-AC` | 105 (−5 %) | 108 (−6 %) | 118 (−10 %) | 121 (−13 %) | 137 (−17 %) | 214 (−1 %) |
| `R-ACt` | 90 (−19 %) | 91 (−21 %) | 92 (−30 %) | 93 (−33 %) | 106 (−35 %) | 211 (−2 %) |
| `full` → `AC` (entrollt) | 89 → 86 | 90 → 87 | 90 → 84 | 98 → 85 | 136 → 120 | 213 → 212 |
| `p2fS` (heute) | 169 | 180 | 211 | 214 | 228 | 234 |
| `p2fS-AC` | 153 (−9 %) | 162 (−10 %) | 198 (−6 %) | 205 (−4 %) | 223 (−3 %) | 225 (−4 %) |
| `RS-ACt` | 139 (−18 %) | 147 (−19 %) | 177 (−16 %) | 186 (−13 %) | 209 (−8 %) | 221 (−5 %) |

**1 MHz, Reg 36 = 0:**

| Variante | L=4 | L=8 | L=12 | L=16 | L=24 | L=40 |
|---|---|---|---|---|---|---|
| `p2f` | 183 | 224 | 226 | 237 | 239 | 248 |
| `p2f-AC` | 177 (−3 %) | 218 (−3 %) | 220 (−3 %) | 233 (−2 %) | 235 (−2 %) | 233 (−6 %) |
| `R-ACt` | 162 (−11 %) | 205 (−9 %) | 207 (−8 %) | 225 (−5 %) | 226 (−5 %) | 224 (−10 %) |
| `p2fS` | 219 | 268 | 271 | 316 | 361 | 388 |
| `p2fS-AC` | 210 (−4 %) | 255 (−5 %) | 256 (−5 %) | 288 (−9 %) | 330 (−9 %) | 377 (−3 %) |
| `RS-ACt` | 203 (−7 %) | 242 (−10 %) | 244 (−10 %) | 263 (−17 %) | 298 (−17 %) | 355 (−9 %) |

Warum die Ersparnis größer ist als die 6 Takte je Wait: Die Kopien laufen fast nur in der Austastung (≈ 25 % des Bildes, dort alle Zyklen frei). Je weniger CPU-Zeit eine Zeile braucht, desto mehr Zeilen schafft die Schleife dort, und desto weniger fallen in die Anzeige (≈ 6 Byte Kopie je 126 µs). Bei 40 Byte bestimmt der VDC die Zeit, und die CPU-Seite bringt nichts. Alle Tabellen (alle Varianten, Reg 36 = 1 und 5) stehen in `test/bench/waits/results/z64k-20261126/report.md`.

### 4.4 Reg 36 (Frage 4)

| | Z64K |
|---|---|
| Modell | `R36 & 15` Refresh-Zyklen je Rasterzeile, 0 = keiner; kein Datenzerfall |
| Anzeige, Stabilität | Die Darstellung hängt nicht an R36 (nur der Refresh-Zähler liest es). Alle Z64K-Läufe (21 Sätze und der Satz `hw`) sind ohne Hänger durchgelaufen. Das VDC-Bild selbst lässt sich nicht prüfen (`-exitscreenshot` zeigt nur den VIC). |
| 0 gegen 1 | gleich schnell (Schleifen ±2 %); freie Zyklen je Zeichenzeile 16 gegen 14 |
| 0 gegen 5 (Kernal) | `p2f` mit 5: 2 MHz L 4–24 um 17–52 % langsamer (174 gegen 115 µs bei L = 8), L = 40 um 8 % schneller; 1 MHz L 4–40 um 4–38 % langsamer. Entrollte Varianten mit 40 Byte sind mit 5 teils schneller. |
| nur während der Liste? | In Z64K wirkt R36 nur auf die Geschwindigkeit der VDC-Befehle. R36 nur um VCL/VMC herum zu senken (2 Registerzugriffe, ≈ 30 Takte) brächte dieselbe Ersparnis. |

**Hardware-Risiko (nicht in Z64K prüfbar):** Die 64-KB-VDCs nutzen DRAM, das alle paar ms refresht werden muss. Mit R36 = 0 geschieht das nur durch die Lesezugriffe der Anzeige. In der vertikalen Austastung (80 von 320 Rasterzeilen, ≈ 5 ms) gibt es dann keinen Refresh, und Bereiche außerhalb der Anzeige (Asset-Pool, Fonts) hängen an den Zugriffen der Anzeige. Mit R36 = 1 gibt es 320 Refresh-Zyklen je Bild statt 0. Das ist weniger als der Kernal-Wert (1 600), in Z64K aber genauso schnell wie 0. R36 nur während der Liste zu senken verkürzt die Zeit ohne Refresh, beseitigt sie aber nicht (VCL läuft ≈ 0,1–0,3 s je Bild). Zum Datenerhalt gibt es schon den Reg-36-Test im P0-Bench (`test/bench/loader-hw.bas`, Fall 11: 8 KB außerhalb der Anzeige, 1/50/500 Bilder, R36 = 0–5). Er sollte auf echter Hardware laufen, bevor R36 = 0 bleibt.

### 4.5 Fazit (Frage 5)

Wertebasis Crawl-Bild: P6g (`dungeonboringengine/docs/specs/P6g-vcl-analysis.md` 4.1, Mittel je Bild) mit 279 VMC-Zeilen ohne Quellschritt (meist Wandkerne, Ø ≈ 10 Byte), 106 Zeilen mit Quellschritt (Ø ≈ 10–22 Byte) und 53 VMF-Zeilen. Crawl-Lauf (`dungeoncrawlerjam2025/test/timing-results.json`, 1.1 P2f): Z64K kompiliert 230 Jiffies (3,82 s) bei 2 MHz und 429 (7,13 s) bei 1 MHz, 39 Tasten. Die Ersparnis je Zeile stammt aus 4.3 (L ≈ 8–12). VMF ist nicht gemessen; angesetzt ist dieselbe Ersparnis wie ohne Quellschritt.

| Änderung | je Crawl-Bild 2 MHz | 1 MHz | Anteil am Crawl-Lauf (obere Schätzung: 39 volle Bilder) |
|---|---|---|---|
| Waits vor R19 (bzw. R33/R18/R19) weglassen (`p2f-AC`, `p2fS-AC`) | ≈ 4–5 ms | ≈ 3 ms | 2 MHz ≈ 5 %, 1 MHz ≈ 2 % |
| zusätzlich schnelle Schleife für L < 256 ohne Füllweiche (`R-ACt`, `RS-ACt`) | ≈ 12 ms | ≈ 8 ms | 2 MHz ≈ 12 %, 1 MHz ≈ 4 % |
| Wait nur jede N-te Zeile | – | – | in Z64K falsch, nicht verwendbar |

- **Sicher erscheint (in Z64K):** die Waits vor R19, R33 und R18 im Quellschritt weglassen und die beiden übrigen behalten. Bei VMF entfällt nur der Wait vor R19; R31 und R30 brauchen ihre. Das ist eine kleine Änderung an `.vmc_src_set`/`.vmc_dst_step` (Ersatz `+vdc_sta` → `sta vdc_data`, Code wird kürzer).
- **Lohnender** ist ein eigener schneller Weg für die häufigste Form (VMC, Länge 1–255, ohne Füllschritt) wie `R-ACt` (≈ 40 Byte). Dafür ist Platz zu prüfen: `vdcbasic3.bin` endet bei `$2EE9`.
- **Nicht** verwendbar: Waits nur jede N-te Zeile, also auch das Entrollen allein dafür. Reg 36 = 0 statt 1 bringt in Z64K nichts.
- **Auf echter Hardware zu prüfen:** (1) Sind die Varianten ohne B dort auch bytegleich? Wie der echte 8563 auf Schreibzugriffe auf R18/R19/R32/R33 während busy reagiert, ist nicht belegt; Z64K ist ein Modell, VICE prüft es gar nicht. (2) Scheitern `A`, `C` und `AC/N` dort genauso, also gilt das Bandbreitenbild der Anzeige? (3) Freie Zyklen je Zeichenzeile im Chunky-Modus (Dauertabelle). (4) Datenerhalt bei R36 = 0/1 (P0-Bench Fall 11).

## 5. Fehlerbilder (Auszüge Zeilen 0–15, L = 8/40)

| Variante | Bild |
|---|---|
| `A` (ohne Wait vor R30) | 13–16 Zeilen „leer“: R30 traf das Vorlesen nach R19, die Kopie startete nicht |
| `C` (ohne Wait vor R18) | 6–16 Zeilen „falsch“, z. T. richtig: R18/R19 hat das Ziel einer laufenden Kopie umgesetzt |
| `none` | 9–15 leer, 1–7 falsch |

## 6. Echte Hardware: Testprogramm

`test/bench/waits/waits.py build --set hw` erzeugt `build/p2g/hw/waits.d64` mit `LOADER` und `W`. Der Satz `hw` umfasst 14 Varianten (`p2f`, `p2f-AC`, `R-ACt`, `p2fS`, `p2fS-AC`, `RS-ACt`, `full`, `AC`, `ACt`, `A`, `C`, `AC/2`, `AC/8`, `S-AC`) × 6 Längen und alle Dauerfälle. Das Ganze läuft in 6 Konfigurationen: 2 MHz und 1 MHz mit R36 = 0, 1, 5.

1. Image auf Diskette/SD2IEC/Ultimate, am C128 (64-KB-VDC) `LOAD"LOADER",8` und `RUN`. Während des Laufs ist der 40-Zeichen-Schirm schwarz und der VDC zeigt Muster. Je Konfiguration schreibt der Lader `RES0` … `RES5` (je ≈ 15 KB) und meldet `RESn OK`, am Ende `FERTIG`. In Z64K dauert eine Konfiguration im Warp-Modus ≈ 2 min; auf echter Hardware sind es geschätzt 1–2 min, zusammen etwa 10 min.
2. Dateien holen, z. B. `c1541 -attach waits.d64 -read res0 RES0.prg` usw.
3. Auswerten: `test/bench/waits/waits.py eval RES0.prg … RES5.prg --json hw.json` (Tabellen wie oben, `F` = Fletcher falsch).

Auswertung der Hardware-Läufe: Ist `p2f` (alle Waits) falsch, liegt es nicht an den Waits. Dann ist der Datenerhalt zu prüfen: Die Quelle `$8000–$93FF` liegt außerhalb der Anzeige, besonders bei R36 = 0/1. In Z64K lief der Satz `hw` einmal vollständig durch (Lader, 6 × `BSAVE`, rc 0). Ergebnis wie oben: alle Varianten mit A und C waren bytegleich (1 080/1 080), `A` und `C` 0/108, `AC/2` 1/108, `AC/8` 0/108. Ablage: `test/bench/waits/results/z64k-20261126/hw/` (`res0–5.prg`, `report.md`); das ist die Z64K-Referenz für den Hardware-Vergleich.

## 7. Dateien, Rohdaten

- `test/bench/waits/waits.py`: Generator, Z64K-Lauf (`z64k --sets …`), Auswertung (`eval`). Sätze: `z<MHz>r<R36>` (alle Varianten + Dauer, Fassung 1 der Dauermessung ohne Zufallslage), `tz…`/`rz…` (Nachträge `…t`, `p2f-AC`, `p2fS…`, `R-…`), `dz…` (nur Dauer, zufällige Lage, 200 Stichproben), `hw`.
- Rohdaten (Ergebnisblöcke `$6000…`, Fassung 2 mit allen Dauer-Stichproben): `test/bench/waits/results/z64k-20261126/*.prg`. Gesamtbericht: `…/report.md` (`waits.py eval` über alle Dateien; Dauer nur aus `dz*`).
- Zwischendateien: `build/p2g/` (nicht einchecken). Dort liegt auch das Z64K-Bytecode-Listing (`build/p2g/jar/MOS8563.yaml`).
- Vergleichswerte P2f (Textmodus, Kernal-R36): `docs/v3/P2f-vmc-loop.md` 4. Die Werte hier sind höher, weil der Chunky-Modus viel mehr Speicherzyklen für die Anzeige braucht.
