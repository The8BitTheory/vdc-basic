# P2c: `VDM`, `VDL`, `VDR`, `VDB` – Linie, Rechteck, Box in Bitmap (1 bpp) und chunky (4 bpp)

Stand: 2026-11-25. Detailspezifikation zu Schritt P2c aus `dungeonboringengine/docs/plan-0.2.md` (Abschnitt 7.3–7.5). Voraussetzung: P2a (Kopf, Tokens), P2b (Schwelle T bei `$1C04`, Lauf-Routine). Umsetzung: Opus (Assembler), Sonnet (Referenzmodell, Tests). Review: Opus.

## 1. Befehle

| Befehl | Token | Syntax | Wirkung |
|---|---|---|---|
| `VDM` | `$FE42` | `VDM modus,basis,stride` | Zeichenfläche festlegen (bleibt bis zum nächsten `VDM`) |
| `VDL` | `$FE3E` | `VDL x1,y1,x2,y2,c` | Linie, beide Endpunkte eingeschlossen |
| `VDR` | `$FE3F` | `VDR x,y,w,h,c` | Rechteck, nur Rand (w × h Pixel außen) |
| `VDB` | `$FE40` | `VDB x,y,w,h,c` | Rechteck, gefüllt |

Die Tokens `$FE3E–$FE40` stehen seit P2b mit einem Syntax-Stub in der Tabelle (P2a, 2.2); P2c ersetzt die Stubs. Einträge in `BASIC V7.0 VDC.txt`: `VDL;fe3e;`, `VDR;fe3f;`, `VDB;fe40;`, `VDM;fe42;`.

### 1.1 `VDM modus,basis,stride`

| Parameter | Bereich | Bedeutung |
|---|---|---|
| `modus` | 0, 1 | 0 = Bitmap 1 bpp, 1 = chunky 4 bpp |
| `basis` | 0–65535 | VRAM-Adresse des Bytes mit Pixel (0,0) |
| `stride` | 1–65535 | Bytes je Pixelzeile |

Andere Werte: `?ILLEGAL QUANTITY ERROR` (14). Zustand nach der Installation: Modus 0, Basis 0, Stride 80. Der Zustand liegt im Datenteil (5 Byte: Modus, Basis, Stride), nicht im Kopf.

### 1.2 Pixel und Adressen

Byteadresse eines Pixels: `basis + y · stride + (x >> 3)` (Modus 0) bzw. `basis + y · stride + (x >> 1)` (Modus 1), alles modulo 65536.

| Modus | Pixel im Byte |
|---|---|
| 0 | Bit `7 − (x AND 7)`: Bit 7 = linkes Pixel, 1 = Vordergrund |
| 1 | x gerade → **High-Nibble** (Bits 7–4), x ungerade → Low-Nibble |

**Begründung der Nibble-Reihenfolge (Modus 1):**

- Chunky heißt: Bitmap fest `$0F`, Pixel im Attribut-RAM (Plan 2, Crawl, Guide Kapitel 5). Die linken 4 Bildpunkte eines Bytes haben im Bitmap-Byte `$0F` Bits = 0, die rechten Bits = 1.
- Im Bitmap-Modus mit Attributen färbt der VDC 0-Bits mit dem High-Nibble und 1-Bits mit dem Low-Nibble des Attributs. Beleg: VICE `src/vdc/vdc-draw.c`, `draw_std_bitmap`: Tabellenindex `(attr & 0xF0) + ((attr & 0x0F) << 8)` gegenüber `regs[26] << 4` im Monochrom-Fall; Reg 26 hat Vordergrund in Bits 7–4 und Hintergrund in 3–0, also landet das Attribut-Low-Nibble im Vordergrund-Platz (1-Bits), das High-Nibble im Hintergrund-Platz (0-Bits).
- Daraus: linkes Pixel = High-Nibble. Das stimmt mit dem Guide („The upper four bits are the left pixel“, Kapitel 5, mit Screenshots getestet), mit `dungeonboringengine/tools/crawlsprites.py` (`c % 2 == 0` → `>> 4`) und mit `VMS` Typ 8 („lower nybble … rightmost pixel“) überein.
- Das gilt nur für Bitmap-Byte `$0F` und Reg 24 Bit 6 = 0 (kein Reverse). `VDM` prüft das nicht: Modus 1 legt nur die Zuordnung x ↔ Nibble fest.

### 1.3 Farbe und Zeichenart `c`

| Modus | `c` | Wirkung |
|---|---|---|
| 0 | 0 | Pixel löschen (Bit = 0) |
| 0 | 1 | Pixel setzen (Bit = 1) |
| 0 | 2 | Pixel umkehren (XOR) |
| 1 | 0–15 | Nibble = c |

Andere Werte: Fehler 14. Kein XOR im Modus 1 (bei Farben selten sinnvoll, würde den schnellen Pfad für Mittelteile verbieten).

**Jedes Pixel wird genau einmal bearbeitet** – auch an den Ecken von `VDR` und bei Linien. Damit ist zweimal dasselbe mit c = 2 wieder das Ausgangsbild.

### 1.4 Geometrie und Grenzen (kein Clipping)

- Es gibt **kein Clipping**, wie bei allen VDC-BASIC-Befehlen. Ein x jenseits der Zeilenbreite landet in der nächsten Zeile, Adressen laufen über `$FFFF` nach `$0000` um. Begründung: Tempo und Größe; die Engine zeichnet nur in ihren Viewport. Steht in der Referenz.
- `VDL`: alle vier Koordinaten 0–16383, sonst Fehler 14 (der Fehlerterm passt so in 16 Bit mit Vorzeichen, 4.3). Das deckt Bitmap 640 × 800 und chunky 160 × 400 reichlich ab.
- `VDR`/`VDB`: x, y, w, h 0–65535 mit `x + w ≤ 65536` und `y + h ≤ 65536`, sonst Fehler 14. w = 0 oder h = 0: nichts zeichnen, kein Fehler.
- Fehlende Parameter: Fehler 11.

## 2. Speicher, Konfiguration, IRQ

- Die Zeichenbefehle lesen kein CPU-RAM außer dem eigenen Zustand; sie laufen in der Konfiguration von `execute_instruction` (`$FF00 = 0`, I/O ein, `io_on`), ohne `remember_mem_conf`. Der Code liegt unter `$4000` (P2a).
- Zeropage: `arg1`–`arg6` (`$83–$8E`) für Parameter und Zwischenwerte, `$0A–$14` (`vcl_command_id`, `multi1`, `multi2`) für Multiplikation und Fehlerterm. Nichts davon muss über den Befehl hinaus erhalten bleiben. Datenteil: Zustand `VDM` (5 Byte), Puffer `draw_buf` (64 Byte) für XOR-Mittelteile.
- IRQ: wie `RXV` (P2b, 3.6): I-Flag unverändert, keine Interruptroutine darf den VDC benutzen.
- Reg 24 Bit 7 = 0 (Block fill) per Read-Modify-Write, nur wenn ein Mittelteil per Block fill geschrieben wird.

## 3. Grundoperationen (Assembler)

| Routine | Eingang | Wirkung |
|---|---|---|
| `draw_addr` | x, y | Byteadresse nach 1.2: `y · stride` (16 × 16 → 16 Bit, Schiebe-Addier-Schleife über die Bits von y, höchstens 14 Runden), plus `basis`, plus `x >> 3` bzw. `x >> 1`; dazu Bitmaske bzw. Nibble-Seite |
| `vdc_seek` | Adresse | Reg 18/19 schreiben (`AY_to_vdc_regs_18_19`) |
| `pix_rmw` | Adresse, Maske | `vdc_seek`; Reg 31 lesen (Adresse +1); Byte ändern; `vdc_seek`; Reg 31 schreiben. Ändern: Modus 0 `ORA m` / `AND ~m` / `EOR m`; Modus 1 `(b AND keep) ORA (cc AND m)` mit `cc = c · 17` |
| `vdc_run` | Wert, Länge L ≥ 1 (16 Bit), Reg 31 angewählt, Adresse gesetzt | L < T (`$1C04`): CPU-Schleife über Reg 31; sonst erstes Byte per Reg 31, Rest mit `vdc_do_YYAA_cycles` (Block fill). Dieselbe Entscheidung wie `RXV`; `RXV` kann seine eigene inline-Fassung behalten, `vdc_run` ist für die Zeichenbefehle |
| `xor_run` | Länge m ≥ 1, Adresse gesetzt | in Stücken zu ≤ 64 Byte: Adresse merken, m Bytes über Reg 31 in `draw_buf` lesen, Adresse zurück, Bytes mit `EOR #$FF` schreiben |

VDC-Lesen: Nach dem Schreiben von Reg 18/19 liefert Reg 31 das Byte an der Adresse und erhöht die Adresse (wie `vram_AAYY_to_A` in v2h).

### 3.1 Waagerechte Spanne `span(x0, x1, zeile)` (x0 ≤ x1)

Modus 1 (chunky), Bytes `b0 = x0 >> 1`, `b1 = x1 >> 1`:

| Fall | Ablauf |
|---|---|
| `b0 = b1`, x0 = x1 | ein Nibble: `pix_rmw` |
| `b0 = b1`, x0 gerade, x1 = x0 + 1 | ein volles Byte: `vdc_seek`, Reg 31 = cc |
| `b0 < b1` | `vdc_seek(b0)`; x0 ungerade → linker Rand: Reg 31 lesen, `vdc_seek(b0)`, Low-Nibble ersetzen, schreiben; Mittelteil `m = (Anzahl voller Bytes)` per `vdc_run(cc, m)` ohne neue Adresse; x1 gerade → rechter Rand bei der jetzt aktuellen Adresse `b1`: lesen, `vdc_seek(b1)`, High-Nibble ersetzen, schreiben |

Modus 0 (Bitmap), `b0 = x0 >> 3`, `b1 = x1 >> 3`, linke Maske `lm = $FF >> (x0 AND 7)`, rechte Maske `rm = ($FF << (7 − (x1 AND 7))) AND $FF`:

| Fall | Ablauf |
|---|---|
| `b0 = b1` | ein Byte mit `lm AND rm`: `pix_rmw` |
| `b0 < b1` | Rand links nur, wenn `lm ≠ $FF` (sonst zählt das Byte zum Mittelteil); Mittelteil: c = 1 → `vdc_run($FF, m)`, c = 0 → `vdc_run($00, m)`, c = 2 → `xor_run(m)`; Rand rechts nur, wenn `rm ≠ $FF` |

Adressen: höchstens drei `vdc_seek` je Spanne (Start, nach dem Lesen des linken Rands, nach dem Lesen des rechten Rands). m = 0 ist erlaubt (zwei Ränder nebeneinander).

### 3.2 Senkrechter Lauf `vrun(x, y0, y1)` (y0 ≤ y1)

Adresse und Maske einmal mit `draw_addr`, dann je Zeile `pix_rmw` und Adresse + stride (16 Bit). Ein senkrechter Lauf berührt immer nur ein Bit bzw. Nibble je Byte; ein Block fill hilft hier nicht. Der Plan („Inkrement = stride“) ist damit umgesetzt: die Adresse wird nicht neu berechnet, nur weitergezählt.

### 3.3 `VDB` (gefüllt)

Ränder, Masken und m einmal aus x und w berechnen, dann für jede der h Zeilen: `vdc_seek(zeilenadresse)`, Ränder und Mittelteil wie 3.1, Zeilenadresse + stride. Bei vollen Bytes ohne Ränder (Modus 1 mit x und w gerade; Modus 0 mit x und w durch 8 teilbar, c ≠ 2) ist das je Zeile ein `vdc_seek` und ein `vdc_run` – derselbe Aufwand wie `VMF` mit Wiederholungen, ohne den BASIC-Parser je Zeile.

### 3.4 `VDR` (Rand)

In dieser Reihenfolge, jedes Pixel genau einmal:

1. obere Zeile: `span(x, x+w−1, y)`
2. wenn h ≥ 2: untere Zeile `span(x, x+w−1, y+h−1)`
3. wenn h ≥ 3: linke Spalte `vrun(x, y+1, y+h−2)`
4. wenn h ≥ 3 und w ≥ 2: rechte Spalte `vrun(x+w−1, y+1, y+h−2)`

Pixelzahl für w, h ≥ 2: `2w + 2h − 4`.

### 3.5 `VDL` (Linie, Bresenham mit Lauf-Ausgabe)

**Verbindliche Pixelmenge** (das Referenzmodell, 5, ist die Definition):

```
dx = |x2 − x1|, dy = |y2 − y1|
wenn dx ≥ dy:                          # waagerecht dominiert
    wenn x1 > x2: Endpunkte tauschen
    sy = +1 wenn y2 > y1, sonst −1
    e = 2·dy − dx; y = y1
    für x = x1 … x2:
        plot(x, y)
        wenn e > 0: y += sy; e −= 2·dx
        e += 2·dy
sonst:                                 # senkrecht dominiert, Rollen von x und y getauscht
    wenn y1 > y2: Endpunkte tauschen
    sx = +1 wenn x2 > x1, sonst −1
    e = 2·dx − dy; x = x1
    für y = y1 … y2:
        plot(x, y)
        wenn e > 0: x += sx; e −= 2·dy
        e += 2·dx
```

Eigenschaften: `max(dx, dy) + 1` Pixel, 8-zusammenhängend, beide Endpunkte enthalten, jedes Pixel einmal. Durch das Tauschen hängt das Ergebnis nicht von der Reihenfolge der Endpunkte ab. Mit Koordinaten ≤ 16383 gilt |e| ≤ 32766 (16 Bit mit Vorzeichen).

**Ausgabe in Läufen (Run-Slicing):** Der Assembler rechnet den Fehlerterm wie oben je Pixel (ein paar Zyklen CPU), schreibt aber nicht je Pixel, sondern je Lauf:

- waagerecht dominiert: Pixel mit gleichem y bilden eine waagerechte Spanne → `span(x_start, x, y)` beim Zeilenwechsel und am Ende. dy = 0 ist eine einzige Spanne mit Block-fill-Mittelteil.
- senkrecht dominiert: Pixel mit gleichem x bilden einen senkrechten Lauf → `vrun(x, y_start, y)`. dx = 0 ist ein einziger Lauf.
- Die Zeilenadresse wird mit ±stride weitergezählt, nicht neu multipliziert.

Ein echtes Run-Slicing mit Division (Lauflängen ohne Schritt je Pixel) ist als spätere Optimierung erlaubt, wenn die Tests (gleiche Pixelmenge) grün bleiben. Der Aufwand je Pixel für den Fehlerterm (≈ 15 Zyklen) ist klein gegen einen VDC-Zugriff.

## 4. Fehler

| Fall | Fehler |
|---|---|
| Parameter fehlt, Komma fehlt, zu viele Parameter | 11 `?SYNTAX ERROR` |
| `modus` ≠ 0/1, `stride` = 0 | 14 |
| `c` außerhalb 1.3 für den aktuellen Modus | 14 |
| `VDL`-Koordinate > 16383 | 14 |
| `x + w > 65536` oder `y + h > 65536` | 14 |

Prüfungen vor dem ersten VRAM-Zugriff; bei einem Fehler bleibt das VRAM unverändert. Reihenfolge bei mehreren Ursachen: 11.2.

## 5. Python-Referenzmodell `test/ref/vdcdraw.py`

Nur Standardbibliothek. Die Pixelmenge und alle Byte-Ergebnisse sind hier definiert; Assembler und Doku richten sich danach.

```python
class VdcError(Exception):          # .code = BASIC-Fehlernummer (11, 14)
class Vdc:
    def __init__(self, vram=None)     # vram: bytes(65536) oder None (Nullen)
    ram: bytearray                    # 65536 Byte
    mode, base, stride                # Zustand wie nach der Installation: 0, 0, 80
    def vdm(self, mode, base, stride)
    def vdl(self, x1, y1, x2, y2, c)
    def vdr(self, x, y, w, h, c)
    def vdb(self, x, y, w, h, c)
def line_pixels(x1, y1, x2, y2) -> list[tuple[int, int]]   # 3.5, in Zeichenreihenfolge
def rect_pixels(x, y, w, h) -> list[tuple[int, int]]       # 3.4
```

- Alle Befehle prüfen wie 4 und werfen `VdcError` vor jeder Änderung.
- Pixel setzen über eine einzige Funktion `_plot(x, y, c)` nach 1.2/1.3; Adressen modulo 65536.
- Das Modell kennt T nicht: das Ergebnis hängt nicht von T ab.

Selbsttests `test/ref/test_vdcdraw.py` (unittest, `python3 -m unittest discover -s test/ref`):

| Prüfung | Inhalt |
|---|---|
| kleine Bilder von Hand | je Modus: Punkt, waagerechte und senkrechte Linie, 45°, Linie (0,0)–(4,1) = Pixel (0,0),(1,0),(2,0),(3,1),(4,1) (nach 3.5 nachgerechnet), `VDR 1,1,3,3`, `VDB` mit ungeraden Rändern; erwartete Bytes als Literale im Test |
| Linien | für alle Paare in einem 9 × 9-Gitter: Pixelzahl `max(dx,dy)+1`, 8-zusammenhängend, Endpunkte enthalten, keine Dopplung, vertauschte Endpunkte = gleiche Menge |
| XOR | `vdl`/`vdr`/`vdb` mit c = 2 zweimal = Ausgangsbild (Zufallsbild) |
| Rand | `len(rect_pixels)` = `2w+2h−4` für w, h ≥ 2; w = 1 bzw. h = 1 = eine Linie |
| Fehler | alle Fälle aus 4 werfen mit dem richtigen Code und lassen `ram` unverändert |
| Umlauf | `basis = $FFF0`: Pixel landen bei `$0000+` |

## 6. Tests Assembler gegen Modell

### 6.1 Harness `test/draw/run_draw.py`

Aufbau wie `test/rxv/run_rxv.py` (P2b, 7.1), gemeinsame Teile in `test/vramtest.py` (Grundmuster, Haltepunkte `$1BFE` Vorbereitung / `$1BFF` Prüfen, VRAM-Dump über `bank vdc`) und `test/vicemon.py` (Portregel P2a 6.4):

1. Fälle aus `test/draw/cases.py`; je Fall eine Folge von Befehlen (`VDM`, `VDL`, `VDR`, `VDB`) und ein T.
2. Treiber `drawt.bas` (erzeugt): je Fall `SYS DEC("1BFE"):POKE DEC("1C04"),T`, die Befehle aus `DATA` (Befehlskennung + Parameter, ausgeführt über `ON K GOSUB`), `SYS DEC("1BFF")`.
3. Vorbereitung: Grundmuster `random.Random(13)` ins VRAM; Prüfen: VRAM-Dump gegen `Vdc(grundmuster)` mit denselben Befehlen, byteweise über alle 64 KB. Abweichung: Fall, erste Adresse, Ist/Soll, Exitcode 1.
4. Zyklen je Fall aus der Monitor-Stoppuhr zwischen Haltepunkten auf `draw_begin` (nach dem Parsen) und `draw_end`; Ausgabe `test/draw/cycles.md`, Vergleich mit 7 (Warnung, kein Fehler).

Der Treiber läuft interpretiert; zusätzlich einmal als Kompilat (P2a, 5.4), Ergebnis muss gleich sein.

### 6.2 Fälle

| Gruppe | Fälle |
|---|---|
| S1 Spannen chunky | `VDB x,y,w,1,c` für x = 0…3, w = 1…12, c ∈ {0, 7, 15}; Mittelteil m = T−1, T, T+1 für T ∈ {0, 2, 14, 255} (x gerade und ungerade); w = 160 (volle Zeile) |
| S0 Spannen Bitmap | x = 0…7, w = 1…24, c ∈ {0, 1, 2}; m um T wie S1; w = 640 |
| V senkrecht | `VDL x,y0,x,y1,c` beide Modi, x mit allen Bitlagen bzw. beiden Nibbles, Länge 1, 2, 100, Endpunkte in beiden Reihenfolgen |
| L Linien | 32 Endpunkte auf einem Kreis (Radius 37) um (80, 50) bzw. (320, 100): alle Oktanten; dx = dy, dx = dy ± 1; dy = 1, dx = 300 (Bitmap, lange Spannen); dx = 1, dy = 150; Einzelpunkt; jede Linie auch mit vertauschten Endpunkten; c = 2 zweimal |
| R `VDR` | w, h ∈ {0, 1, 2, 3, 10} alle Kombinationen, beide Modi; c = 2 (Ecken einmal) |
| B `VDB` | ungerade/gerade Ränder; 70 × 96 chunky (Mittelteil per Fill); 640 × 200 Bitmap; w = 0, h = 0 |
| G Geometrie | stride 160, 37, 255, 300; basis ungerade; basis `$FFC0` mit Umlauf; x jenseits der Zeile (x = 200 bei stride 80, chunky) |
| Z Zufall | 200 Befehle aus `random.Random(11)`: Modus, Befehl, Lage, Größe (klein und groß), c, T |

Fehlerfälle laufen im normalen Lauf (`test/run.sh`, interpretiert und `-c`):

| Datei | Inhalt |
|---|---|
| `draw-errors.bas` | alle Fälle aus 4 über `TRAP`, je Fall `ER` prüfen und per `VMD` an einer Stelle prüfen, dass nichts gezeichnet wurde |
| `draw-basic.bas` | je Befehl ein kleiner Fall mit `VMD`-Prüfung (Rauchtest ohne Monitor) |

## 7. Zyklenziele (CPU-Zyklen bei 2 MHz, ohne BASIC-Parser)

| Pfad | Ziel | Bemerkung |
|---|---|---|
| `draw_addr` (Multiplikation, Maske) | ≤ 250 | einmal je Befehl bzw. je `VDR`-Seite |
| `pix_rmw` | ≤ 130 | 2 × `vdc_seek` + Lesen + Schreiben |
| Spanne, fester Anteil mit zwei Rändern | ≤ 330 | dazu Mittelteil: 15 je Byte unter T, darüber Block fill |
| senkrechter Lauf | ≤ 140 je Pixel | `pix_rmw` + 16-Bit-Addition |
| schräge Linie (≈ 45°) | ≤ 160 je Pixel | Fehlerterm + `pix_rmw` |
| `VDB` volle Bytes | ≤ 60 je Zeile + Mittelteil | `vdc_seek` + `vdc_run` |
| `VDB 0,0,140,96,c` chunky | ≤ 96 × (60 + Fill von 70 Byte) | Viewport der Engine leeren |

Vergleich: ein Pixel in BASIC mit `VMA` + `VMO` kostet zwei Befehle mit Parser, etwa 10 000 Zyklen. Werte aus VICE sind wegen des groben VDC-Modells nur relativ zu lesen (P0, Abschnitt 2).

## 8. Größe

Schätzung je Teil (Byte):

| Teil | Byte |
|---|---|
| Parameter (5 Werte, gemeinsam für `VDL`/`VDR`/`VDB`) + Bereichsprüfungen | 90 |
| `VDM` + Zustand | 50 |
| `draw_addr` (Multiplikation, Masken) | 70 |
| `pix_rmw` (beide Modi, setzen/löschen/XOR) | 70 |
| `vdc_run`, `xor_run` + Puffer 64 | 60 + 64 |
| Spanne (beide Modi) | 220 |
| senkrechter Lauf | 60 |
| `VDL` (beide Richtungen, 16-Bit-Fehlerterm, Lauf-Ausgabe) | 260 |
| `VDR`, `VDB` | 140 |
| **Summe P2c** | **≈ 1 100** |

Gesamt v3: v2h 2 294 + Kopf 16 + Stubs/Tabellen ≈ 20 + `RXV`/VCL 8 ≈ 400 (P2b) + P2c ≈ 1 100 → **≈ 3,8 KB** von 9 216 Byte (`$1C00–$3FFF`). Reserve ≈ 5,4 KB. Die Gesamtprüfung aus P2a bricht beim Assemblieren ab, falls die Schätzung kippt.

## 9. Doku

| Datei | Änderung |
|---|---|
| `docs/reference/commands.md` | neuer Abschnitt „Drawing“: `VDM`, `VDL`, `VDR`, `VDB` mit Pixel-/Nibble-Belegung (1.2, mit Begründung kurz), Farbe/XOR, kein Clipping, Grenzen, Fehler; Übersicht; Speicher (Zustand, Puffer) |
| `BASIC V7.0 VDC.txt` | vier Tokens |
| `README.md` | Befehlsliste, Tokentabelle, „Known limits“ (kein Clipping, kein XOR in Modus 1), Abschnitt „Unfinished parts“ streichen |
| `test/README.md` | Referenzmodell, `run_draw.py` |
| Guide | Kapitel dazu mit P8 (Bitmap-Kapitel 4/10 und chunky-Kapitel 5 können auf `VDL`/`VDB` verweisen) |

## 10. Abnahme

| Prüfung | Erwartung |
|---|---|
| `test/ref/test_vdcdraw.py` | grün |
| `test/draw/run_draw.py` | alle Fälle 6.2 bytegleich mit dem Modell über 64 KB interpretiert, als Kompilat mit `--quick` (11.4) |
| T-Unabhängigkeit | S0/S1/B mit allen T gleich |
| `test/run.sh test/*.bas` und `-c` | alle PASS (inkl. `draw-errors.bas`, `draw-basic.bas`) |
| Zyklen | `cycles.md` erzeugt, Abweichungen von 7 begründet |
| Größe | P2c ≤ 1 400 Byte, Gesamtende ≤ `$3FFF` |
| Doku | 9 vollständig |

## 11. Festlegungen aus dem Referenzmodell

Stand 2026-11-25, Review des Modells `test/ref/` (Opus). Verbindlich; Abschnitte 1–6 gelten, wo hier nichts anderes steht.

### 11.1 Pixelmenge und Ausgabe

- `line_pixels` ist der klassische Bresenham (Wikipedia `plotLineLow/High`): Schritt nur bei `e > 0`, bei Gleichstand (e = 0) bleibt die Nebenachse stehen. Nach dem Tauschen läuft die Hauptachse immer aufwärts; daher ist die Menge unabhängig von der Reihenfolge der Endpunkte. Die Lauf-Ausgabe (3.5) rechnet denselben Fehlerterm je Pixel und fasst nur zusammen, sie ergibt also dieselbe Menge. Ein späteres Run-Slicing mit Division muss diese Menge exakt treffen (Rundung bei Gleichstand beachten).
- `rect_pixels`-Reihenfolge: oben, unten, links, rechts (wie 3.4). Die Reihenfolge ist für das Ergebnis ohne Bedeutung, weil jedes Pixel genau einmal vorkommt (geprüft per XOR-Bitzählung).
- `VDB` im Modell zeilenweise, Pixel für Pixel; der Assembler darf jede Reihenfolge wählen.
- Nibble-/Bit-Belegung wie 1.2 (Bit 7 bzw. High-Nibble = linkes Pixel), im Modell `_plot` und `pixel`.

### 11.2 Parameter und Fehler

Die 6502-Fassung liest jeden Parameter mit der ROM-Routine `getadr` (`b_parse_uint16` `$8812` bzw. `b_parse_comma_uint16` `$880F`, Kern `$8815`). Nachgesehen in `basichi-318019-04.bin`: negatives Vorzeichen oder Exponent ≥ `$91` (Wert ≥ 65536) → `$7D28` (`?ILLEGAL QUANTITY`, 14); sonst `qint`, Nachkommastellen werden abgeschnitten. Fehlendes Komma: `b_skip_comma` `$795C` → `?SYNTAX ERROR` (11). Nummern 11 und 14 passen zu BASIC 7.0.

Daraus verbindlich (Modell `_args`, dann `_num`):

1. Parameter **von links nach rechts**; je Parameter: fehlt → 11, außerhalb 0–65535 → 14. Der erste Fehler gewinnt.
2. Erst wenn alle Parameter gelesen sind: befehlsspezifische Bereiche (`modus` 0/1, `stride` ≥ 1, `c` je Modus, `VDL` ≤ 16383, `x + w ≤ 65536`, `y + h ≤ 65536`) → 14.
3. Beispiele: `VDL 70000,0` → 14; `VDL 20000,0` (Rest fehlt) → 11; `VDM 2,0` → 11.
4. w, h 0–65535 (w = 65536 ist durch Schritt 1 schon 14). `basis` 0–65535, `stride` 1–65535.
5. Nicht ganzzahlige Werte: ROM schneidet ab. Das Modell nimmt nur `int` und wirft sonst `TypeError` (Fehler im Test, kein BASIC-Fehler); Testfälle mit Brüchen gibt es nicht. Zeichenketten (`?TYPE MISMATCH`, 22) und zu viele Parameter (11) prüft nur `draw-errors.bas`, nicht das Modell.
6. Ein fehlgeschlagenes `VDM` lässt den Zustand unverändert.

### 11.3 Testvektoren

- Generator `test/ref/gen_vectors.py`; Format `vdcdraw-vectors-1` (Kopf der Datei). Grundmuster `random.Random(13)`, je Fall `diff` (geänderte Läufe) und `sha1` über 64 KB. Die S1-Mittelteilbreiten und die Kreisrundung (`round`) sind wie im Generator festgelegt.
- S0/S1 „mid“-Fälle haben **genau m** volle Mittelbytes (x = 0 ohne Ränder, x = 1 bzw. 3 mit beiden Rändern; im Test geprüft). Der erste Entwurf lag um 1 bzw. 2 daneben und hätte m = T − 1 nie getroffen.
- Jeder Fall beginnt mit `VDM`; der Zustand hängt nie vom Vorfall ab.
- Der Harness importiert `gen_vectors.build()` direkt (≈ 9 s) und vergleicht den VRAM-Dump mit dem Modell; die JSON-Datei (≈ 600 KB) wird nicht eingecheckt.

### 11.4 Laufzeit in VICE (Empfehlung für 6.1)

1 520 Fälle (S0 645, Z 200, V 192, S1 166, L 157, R 103, B 48, G 12). Je Fall: Grundmuster laden (`load`, 64 KB, Bank `vdc`), Befehle interpretiert, `save` 64 KB, Vergleich in Python. Geschätzt 0,2–0,4 s je Fall im Warp → voller Satz 5–10 min je Durchgang, interpretiert plus Kompilat das Doppelte.

- `run_draw.py` unterstützt `--group` und `--quick`. `--quick` (`gen_vectors.is_quick`: S0-Raster nur x ∈ {0, 3, 7}, Z nur 000–049) hat 1 010 Fälle und ist der Standard für Entwicklungsläufe; der volle Satz für die Abnahme (10) und nur interpretiert, das Kompilat läuft mit `--quick`.
- Alle Fälle in **einer** VICE-Sitzung (Haltepunkte `$1BFE`/`$1BFF`, ereignisgesteuert, Timeout nur als Wächter). Kein Neustart je Fall.
- Vergleich über die vollen 64 KB bleibt Pflicht (findet Schreibzugriffe neben dem Ziel). `diff` dient nur der Fehlermeldung.


## 12. Befunde aus der Umsetzung (6502)

Stand 2026-11-25, Umsetzung `src/vdcbasic.asm` (Zone `draw`), Harness `test/draw/run_draw.py`. Ergebnis: alle 1 520 Fälle bytegleich mit dem Modell über 64 KB (interpretiert), `-c --quick` 1 010/1 010, `test/run.sh` beide Modi grün.

### 12.1 Größe (Abweichung von 8 und 10)

Die Zeilen unten stammen vor den Optimierungen; deren Netto-Effekt steht in der letzten Zeile.

| Teil | Byte |
|---|---|
| `VDM` + Fehlerausgänge | 53 |
| `VDL` (Unterscheidung, Tausch, Fehlerterm, Lauf-Ausgabe, schnelle Pfade dy = 0 / dx = 0) | 626 |
| `VDR` | 182 |
| `VDB` | 54 |
| gemeinsam: Parameter, Prüfungen, `.setup`, x-Bereich, Spanne, `pix_rmw`, `run`/`xor_run`, Multiplikation, Masken | 938 |
| Daten: Zustand `VDM` 5, Arbeitsbytes 38, `draw_buf` 64 | 107 |
| Optimierungen 1–4 (netto), neue Daten `draw_vh`, `draw_majx`, `draw_xs` | +102 |
| **Summe P2c** | **≈ 2 064** |

Die Optimierungen im Einzelnen: (1) `pix_rmw` setzt Reg 18 nur bei geändertem High-Byte (`draw_vh`), nach dem Lesen nur Reg 19 zurück (nicht bei Low-Byte $FF); Reg 19 immer zuletzt: +58 Byte. (2) gemeinsame Pixelschleife `.ppx` für y-major, senkrechte Läufe (auch VDR-Spalten) und x-major mit 2dy ≥ dx; Ende von `pix_rmw` self-modifying (rts/clc), ersetzt `.yl` und `.vcol`: +74 Byte. (3) rechter Rand ohne Reg-18-Prüfung (`.pix_rmw_19`): ±0 Byte. (4) Wrap-Pfad über `.seek` und `x+w−1` / `y+h−1` als Unterprogramm `.last`: −30 Byte.

Ziel ≤ 1 400 verfehlt; vdcbasic3.bin 4 711 Byte, Ende v3 ≈ $2E67, 4 505 Byte Reserve bis $3FFF (Assertion bleibt). Gründe: jede VDC-Warteschleife steht inline (`+vdc_sta`/`+vdc_lda`/`+vdc_sty`, 8 Byte, 14-mal), `pix_rmw` setzt die Adresse zweimal inline (2 × 29 Byte) statt per Unterprogramm, und es gibt eigene schnelle Pfade (Linie dy = 0 als eine Spanne, dx = 0 als senkrechter Lauf, Lauf innerhalb eines Bytes ohne Spannenrechnung). Kürzer ginge vor allem auf Kosten von `pix_rmw` (Seek als Unterprogramm: −60 Byte, +24 Zyklen je Pixel); das wurde nicht gemacht. Nur der Wrap-Pfad nutzt inzwischen ein `.seek`-Unterprogramm (Optimierung 4).

### 12.2 Zyklen (Abweichung von 7)

Werte aus `test/draw/cycles.md` (VICE, 1 MHz, R6 = 0, vorläufig; maßgeblich):

| Pfad | Ziel | gemessen | Begründung |
|---|---|---|---|
| senkrechter Lauf je Pixel | ≤ 140 | 144 | `pix_rmw` (2 Seeks, Lesen, Schreiben) plus 16-Bit-Addition stride und Zähler; gemeinsame Pixelschleife (Optimierung 2) und Reg-18-Prüfung (1) brachten −25 (169 → 144), Ziel knapp verfehlt |
| schräge Linie 45° je Pixel | ≤ 160 | 196 | je Pixel ein Lauf: `pix_rmw` + Lauf-Ende + Zeilenwechsel ±stride + x-Schritt + 16-Bit-Fehlerterm; Optimierungen brachten −119 (315 → 196). Ziel ohne größeren Umbau (eigene Zeilenschleife für VDB) nicht erreichbar |
| Spanne mit zwei Rändern je Zeile (`VDB`), Bitmap | ≤ 330 | 375 | zwei `pix_rmw`, dazu Zeilenadresse, Rand-/Mittelteil-Entscheidung; −37 (412 → 375). Ziel ohne größeren Umbau (eigene Zeilenschleife für VDB) nicht erreichbar |
| Spanne mit zwei Rändern je Zeile (`VDB`), chunky | ≤ 330 | 446 | wie Bitmap; −32 (478 → 446). Ziel ohne größeren Umbau nicht erreichbar |
| Mittelteil unter T je Byte | 15 | 15 | wie geplant |
| `VDB` volle Bytes je Zeile | ≤ 60 + Mittelteil | 386 bei m = 10 (CPU, davon 150 Mittelteil), 309,5 bei m = 70 (Block fill) | Seek, zwei 16-Bit-Additionen für Anfang/Ende der Zeile, Entscheidungen in `span_draw`, erstes Byte + Zählerregister beim Block fill |
| `VDR 3,y,10,10,1` gesamt | — | 4 255 (vorher 4 537) | zehn Zeilen mit je zwei Rändern und Spanne |
| flache Linie, Fehlerterm je Pixel | ≈ 15 (3.5) | 74 | 16-Bit-Fehlerterm, 16-Bit-Zähler und x-Schritt (Bitindex, Übertrag in die Adresse) je Pixel |

Offen, ohne die Pixelmenge zu ändern: Run-Slicing mit Division (3.5) für flache Linien, `.xadv`/`.endrun` in die Schleife ziehen (45°). Für 45° und Spanne ist ohne größeren Umbau (eigene Zeilenschleife für VDB) kein Ziel erreichbar. Zahlen von echter Hardware stehen aus.

### 12.3 Rechter Rand ohne Reg-18-Prüfung

3.1 liest den rechten Rand an der nach dem Mittelteil aktuellen Adresse. Die Umsetzung setzt davor nur noch Reg 19 neu (`.pix_rmw_19`, ohne Prüfung von Reg 18; ±0 Byte). Hardware-Annahme: Schreiben allein von Reg 19 setzt die Adresse korrekt, wenn Reg 18 stimmt. VICE liest immer direkt an der Adresse und würde einen Fehler hier nicht zeigen; auf echter Hardware zu prüfen. Der linke Rand läuft wie in 3.1: nach dem Zurückschreiben steht die Adresse auf dem ersten Mittelbyte.

### 12.4 Zeropage, Parameter, Messpunkte

- `$0A`–`$14` erst nach dem Parsen: der Ausdrucksauswerter des ROM benutzt diesen Bereich (`$0D` VALTYP usw.); der Schleifenindex beim Lesen der fünf Parameter liegt daher im Datenteil (`draw_tmp`).
- Zusätzlich zu 2: `$26`–`$2A` (Arbeitsbytes von `VCL`/`VMS`, während eines Zeichenbefehls frei) für `draw_d2`, `draw_d2m`, `draw_sp`, und `arg5` (`c`) nach der Prüfung als `draw_sa`. Steht in der Referenz unter „Memory used“.
- Zu viele Parameter prüft der Befehl selbst (`chrgot` nach dem letzten Wert) vor dem ersten VRAM-Zugriff; BASIC würde den Syntaxfehler erst nach dem Zeichnen melden.
- `draw_begin` (nach Parsen und Prüfen, vor der Geometrie) und `draw_end` sind globale Labels mit `RTS` für die Stoppuhr; jeder Zeichenbefehl ruft `draw_begin` einmal und endet mit `JMP draw_end` (12 bzw. 3 Zyklen).
- Reg 24 Bit 7 wird nur beim ersten Mittelteil per Block fill gelöscht (`draw_r24`), wie 2 verlangt.

### 12.5 Tests

- `draw-errors.bas` ist auf 13 Dateien `draw-errors*.bas` verteilt (je Datei ein Fehler, Blitz!-Grenze wie bei `rxv-errors*.bas`). Die Prüfung „nichts gezeichnet“ liest das VRAM nach dem Fehler direkt über `$D600`/`$D601`, weil im Kompilat danach keine Erweiterung mehr arbeitet. „Fehlgeschlagenes `VDM` lässt den Zustand unverändert“ (11.2, 6.) ist aus demselben Grund nicht im Kompilat prüfbar; der Code speichert den Zustand erst nach allen Prüfungen.
- `test/vramtest.py` (gemeinsame Teile mit `run_rxv.py`, 6.1) gibt es noch nicht; `run_draw.py` ist eigenständig nach dem Muster von `run_rxv.py` gebaut. Die Befehle stehen nicht als `DATA` im Treiber (1 520 Fälle passen nicht in den BASIC-Speicher), sondern werden je Fall in den Parameterblock `$1B00` geschrieben und per `PEEK` gelesen; T und R6 ebenso.
- Laufzeit: voller Satz interpretiert ≈ 30 s, `-c --quick` ähnlich (statt 5–10 min nach 11.4): `bload`/`bsave` über den Monitor sind schnell.
- Zeitmessgruppe C (`--cycles`, 27 Pfade × 8 × R6 0/25) ist nicht Teil von `gen_vectors`, wird aber ebenfalls gegen das Modell geprüft.
- Z64K-Fassung (`run_draw.py --emu z64k`, `test/z64k/zvt.s`, `test/README.md`): je Fall werden nur die vom Modell geänderten Bereiche (mit Rand 32 Byte, höchstens 16 Bereiche) per CRC geprüft und wiederhergestellt statt aller 64 KB; Schreibzugriffe daneben fängt die Vorprüfung des nächsten Falls und eine Gesamt-CRC an jedem 64. Fall, der schuldige Fall wird durch Wiederholung des Fensters mit Gesamt-CRC je Fall benannt. Voller Satz (1 520 Fälle): etwa 30 min (geschätzt aus 3,5 s je Fall und Start) → 3 min 25 s, grün. Der Vergleich über die vollen 64 KB je Fall bleibt in der VICE-Fassung (Pflicht oben).
- P2b-Review: `vcl-rxv-twice.bas` (zwei Typ-8-Einträge hintereinander, beide mit Block fill) und `vcl-rxv-vms.bas` (Typ 12, 8, 12, 8: Wechsel von Reg 24 Bit 7) ergänzt, beide Modi grün.
