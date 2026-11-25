# RXV: Zyklen in VICE (vorläufig)

Erzeugt von `test/rxv/run_rxv.py --cycles F` (1 MHz) und `--fast --cycles-append F` (2 MHz). Stoppuhr des
VICE-Monitors von `rxv_execute` bis `complex_instruction_shared_exit`, IRQ für die Messung gesperrt (I-Flag
über den Monitor), je Vektor das Minimum aus 8 Läufen (die Rasterlage streut). Die Stoppuhr zählt
Systemtakte zu 1 MHz: ohne `FAST` 1 CPU-Zyklus je Takt, mit `FAST` 2; die Tabellen zeigen **CPU-Zyklen**.
Der CPU-Pfad (Vergleich mit dem Listing, docs/v3/P2b-rxv.md 6) steht in „1 MHz, R6 = 0“: dort wartet die
CPU nicht auf den VDC. Bei 2 MHz begrenzt in VICE der VDC.

**Vorläufig:** VICE modelliert den VDC-Zugriff vereinfacht (Busy-Zeit je Reg-31-Zugriff im sichtbaren
Bereich); die Werte taugen nur zum Vergleich der Pfade untereinander. Block-fill-Zeilen enthalten das
Warten auf das Ende des Fills beim Lesen von R18/R19 am Ende. Werte auf echter Hardware stehen aus.

## 1 MHz, ohne sichtbare Zeilen (R6 = 0), interpretiert

| Pfad | Zyklen | Herleitung |
|---|---|---|
| Aufruf ohne Token (`00`): Setup + Ende | 251.0 | empty |
| Literal je Byte | 23.1 | (lit127 − lit1) / 126 |
| Literal-Token fest | 18.9 | lit1 − empty − 1 Byte |
| Lauf L < T (CPU) je Byte | 15.0 | (L13 − L2) / 11, T = 14 |
| Lauf L < T (CPU) fest | 42.0 | L2 − empty − 2 Byte |
| Lauf L ≥ T (Block fill), L = 14 | 119.0 | fill14 − empty, inkl. Warten am Ende |
| Lauf L ≥ T (Block fill), L = 127 | 162.0 | fill127 − empty, inkl. Warten am Ende |
| Langer Lauf L = 300 (Block fill, 2 Seiten) | 269.0 | lrun300 − empty, inkl. Warten am Ende |
| Skip | 186.0 | skip − empty |

## 1 MHz, normale Anzeige (R6 = 25), interpretiert

| Pfad | Zyklen | Herleitung |
|---|---|---|
| Aufruf ohne Token (`00`): Setup + Ende | 251.0 | empty |
| Literal je Byte | 25.9 | (lit127 − lit1) / 126 |
| Literal-Token fest | 16.1 | lit1 − empty − 1 Byte |
| Lauf L < T (CPU) je Byte | 15.0 | (L13 − L2) / 11, T = 14 |
| Lauf L < T (CPU) fest | 42.0 | L2 − empty − 2 Byte |
| Lauf L ≥ T (Block fill), L = 14 | 119.0 | fill14 − empty, inkl. Warten am Ende |
| Lauf L ≥ T (Block fill), L = 127 | 182.0 | fill127 − empty, inkl. Warten am Ende |
| Langer Lauf L = 300 (Block fill, 2 Seiten) | 319.0 | lrun300 − empty, inkl. Warten am Ende |
| Skip | 229.0 | skip − empty |

Ganze Ströme (1 MHz, IRQ an, ein Lauf): `lrun65536/T14` 48563, `dungeon` 233310, `long-stream` 188870

## 2 MHz, ohne sichtbare Zeilen (R6 = 0), interpretiert

| Pfad | Zyklen | Herleitung |
|---|---|---|
| Aufruf ohne Token (`00`): Setup + Ende | 262.0 | empty |
| Literal je Byte | 23.2 | (lit127 − lit1) / 126 |
| Literal-Token fest | 20.8 | lit1 − empty − 1 Byte |
| Lauf L < T (CPU) je Byte | 25.3 | (L13 − L2) / 11, T = 14 |
| Lauf L < T (CPU) fest | 25.5 | L2 − empty − 2 Byte |
| Lauf L ≥ T (Block fill), L = 14 | 126.0 | fill14 − empty, inkl. Warten am Ende |
| Lauf L ≥ T (Block fill), L = 127 | 216.0 | fill127 − empty, inkl. Warten am Ende |
| Langer Lauf L = 300 (Block fill, 2 Seiten) | 472.0 | lrun300 − empty, inkl. Warten am Ende |
| Skip | 196.0 | skip − empty |

## 2 MHz, normale Anzeige (R6 = 25), interpretiert

| Pfad | Zyklen | Herleitung |
|---|---|---|
| Aufruf ohne Token (`00`): Setup + Ende | 264.0 | empty |
| Literal je Byte | 23.2 | (lit127 − lit1) / 126 |
| Literal-Token fest | 22.8 | lit1 − empty − 1 Byte |
| Lauf L < T (CPU) je Byte | 24.0 | (L13 − L2) / 11, T = 14 |
| Lauf L < T (CPU) fest | 38.0 | L2 − empty − 2 Byte |
| Lauf L ≥ T (Block fill), L = 14 | 124.0 | fill14 − empty, inkl. Warten am Ende |
| Lauf L ≥ T (Block fill), L = 127 | 214.0 | fill127 − empty, inkl. Warten am Ende |
| Langer Lauf L = 300 (Block fill, 2 Seiten) | 472.0 | lrun300 − empty, inkl. Warten am Ende |
| Skip | 192.0 | skip − empty |

Ganze Ströme (2 MHz, IRQ an, ein Lauf): `lrun65536/T14` 91712, `dungeon` 299308, `long-stream` 207182
