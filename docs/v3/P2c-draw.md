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

Prüfungen vor dem ersten VRAM-Zugriff; bei einem Fehler bleibt das VRAM unverändert.

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
| `test/draw/run_draw.py` | alle Fälle 6.2 bytegleich mit dem Modell über 64 KB, interpretiert und als Kompilat |
| T-Unabhängigkeit | S0/S1/B mit allen T gleich |
| `test/run.sh test/*.bas` und `-c` | alle PASS (inkl. `draw-errors.bas`, `draw-basic.bas`) |
| Zyklen | `cycles.md` erzeugt, Abweichungen von 7 begründet |
| Größe | P2c ≤ 1 400 Byte, Gesamtende ≤ `$3FFF` |
| Doku | 9 vollständig |
