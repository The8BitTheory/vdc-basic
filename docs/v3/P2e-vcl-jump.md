# P2e – VCL-Eintrag Typ 10: weiter bei Adresse

## 1. Zweck

Die Dungeon Boring Engine 0.2 baut ihre VCL-Liste abwärts vom Pufferende auf (sie läuft von vorne nach hinten durch die Sicht, die Liste muss von hinten nach vorne malen). Der Listenanfang wandert deshalb mit jedem Bild, und das BASIC-Programm muss ihn für `VCL` lesen (`PEEK`-Rechnung, in BASIC teuer). Mit einem Sprung-Eintrag schreibt die Engine an eine feste Stelle (Pufferanfang) einen Verweis auf den echten Anfang; das Spiel ruft `VCL` immer mit derselben Adresse auf. Allgemein: feste Einsprungadresse für eine Liste, die sich verschiebt, und Verketten mehrerer Listen.

## 2. Format

| Typ | Bytes | Parameter | Wirkung |
|---|---|---|---|
| 10 | 3 | Adresse (Wort, Low zuerst) | der nächste Eintrag wird ab *Adresse* gelesen (Bank 0) |

Keine BASIC-Entsprechung. Es wird nichts gezeichnet, der Speicherzustand (`$FF00`) bleibt wie bei jedem Eintrag. Für die Zieladresse gelten dieselben Regeln wie für die Startadresse von `VCL` (Bank 0, nicht `$D000–$DFFF`). Ein Sprung zurück in die eigene Liste endet nie (nicht geprüft). Typ 11 und alles über 13 bleiben unbekannt (`INVALID`).

## 3. Umsetzung

`src/vdcbasic.asm`, Verteiler in `.vcl_next_command`: nach der Prüfung `< 13` vergleicht `CMP #10`; bei 10 liest der Code die beiden Bytes hinter dem Typbyte (`offset_1` ist dort 0, Y = 0) und setzt `arg_address` darauf, dann weiter mit `.vcl_next_command` (das `offset_1` = 0 addiert). Der Code steht im Verteiler selbst, weil die kurzen Sprünge zu `.vcl_syn` keinen weiteren Abstand vertragen. Eintrag 10 der Tabelle `vcl_parameter_bytes` bleibt unbenutzt (0).

Größe: `vdcbasic3.bin` 4753 → 4771 Byte (+18, nur Code), Ende `$2EA0` (vorher `$2E8E`; die Z64K-Hilfe `zvt` bei `$2F00` bleibt frei, Prüfung `!if * > $4000` unverändert). Kopf unverändert: `$1C00` `JMP $1C10`, `$1C03` = `$30`.

## 4. Zyklen

VICE (`x128 -80col -VDC64KB`, 1 MHz), Stoppuhr des Monitors zwischen zwei `SYS`-Marken mit gesetztem I-Flag: `VCL` über 200 verkettete Sprünge (je 3 Byte, jeder auf den nächsten, dann Ende) gegen `VCL` auf eine leere Liste, dreimal: 212–217 Zyklen je Sprung. Das ist im Wesentlichen die Verwaltung, die jeder Eintrag hat (`remember_mem_conf`, Parameter löschen, Typ lesen); der Sprung selbst kostet etwa 30 Zyklen davon. Messprogramm: `build/p2e/` (nicht im Repository).

## 5. Tests

| Test | Prüft |
|---|---|
| `test/vcl-jump.bas` | Kette `$A000` → `$A100` → `$1300` (auch unter `$4000`); die Bytes hinter einem Sprung werden nicht gelesen; Sprung als erster Eintrag, Sprung auf einen Sprung, Ende-Byte am Ziel; Liste von 300 Byte vor dem Sprung (Verwaltung von `offset_1` über die Seitengrenze hinweg). Gegenprobe: mit dem alten `vdcbasic3.bin` endet der Test mit 10 (`INVALID`). |

Ergebnis: siehe Abschnitt 6.

## 6. Ergebnis

Umgesetzt wie in 3, +18 Byte, Ende `$2EA0`, Kopf unverändert. Tests: VICE `test/run.sh test/*.bas` 49/49 interpretiert und `-c` 49/49 kompiliert (Blitz!); Z64K `test/run.sh -z test/vcl-*.bas` 14/14 interpretiert und `-z -c` mit `vcl-jump.bas`, `vcl-with-vms.bas`, `vcl-vms-bank1.bas` 3/3 (zwei Läufe brachen im ersten Durchgang mit rc 125 ab, ein Absturz des Java-Starters bei drei parallelen Starts; die Wiederholung war grün). Benutzer: dungeonboringengine P6j (feste Listenadresse, `BUFSET` X Bit 0) und Crawl 128 1.1.
