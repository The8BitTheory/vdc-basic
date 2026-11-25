# P2b: `RXV src,dst` – VRL aus Bank 0 ins VRAM entpacken (+ VCL-Typ 8)

Stand: 2026-11-25. Detailspezifikation zu Schritt P2b aus `dungeonboringengine/docs/plan-0.2.md` (Abschnitt 7.2). Format: `dungeonboringengine/docs/specs/P1-vrl.md` (VRL v1), Referenz `dungeonboringengine/tools/vrl.py`. Voraussetzung: P2a (Kopf bei `$1C00`). Umsetzung und Review: Opus.

## 1. Befehl

```
RXV src,dst
```

| Parameter | Bereich | Bedeutung |
|---|---|---|
| `src` | 0–65535 | Adresse des ersten Tokens in **Bank 0**, unabhängig von `BANK` (wie `RTV`, `VMS`, `VCL`) |
| `dst` | 0–65535 | VRAM-Adresse des ersten Zielbytes |

- Entscheidung zu review-notes D.7: `src` zeigt auf den **Strom**, nicht auf den Dateikopf. Für eine VRL-Datei: `BLOAD"BILD.VRL",B0,P(BE):RXV BE+5,ZIEL` (Kopf `"VR"`, Version, U = 5 Byte). Eingebettete Ströme (DTS, P5) gehen ohne Kopf. Es gibt keine zweite Form; der Kopf wird nicht geprüft.
- Token `$FE $41` (P2a, 2.2). Eintrag in `BASIC V7.0 VDC.txt`: `RXV;fe41;`.
- Nach dem Befehl stehen im Kopf (P2a, 2.1): `$1C06/$1C07` = Adresse hinter dem Ende-Token (zum Verketten von Strömen), `$1C08/$1C09` = VRAM-Adresse hinter dem letzten geschriebenen oder übersprungenen Byte. Beide Low/High.

## 2. Speicher und Register

| Name | Lage | Inhalt |
|---|---|---|
| `rxv_ptr` | `arg1` (`$83/$84`) | Stromzeiger (Basis), wird nach jeweils ≥ 128 gelesenen Byte nachgezogen |
| Y | Register | Versatz im Strom relativ zu `rxv_ptr`, immer < 128 am Tokenanfang |
| `arg2` (`$85/$86`) | | `dst` beim Start, danach Hilfszelle |
| `rxv_cnt` | `arg3` (`$87/$88`) | Zählwerte (L − 1 für Block fill, Skip-Länge) |
| `rxv_len` | `arg4` (`$89/$8A`) | Länge eines langen Laufs |
| `rxv_y` | `arg5` (`$8B`) | Sicherung von Y um `vdc_do_YYAA_cycles` und Skip |
| `rxv_ttok` | Variable im Datenteil | Tokenschwelle für kurze Läufe (3.2) |
| `RXV_T` | **`$1C04`** | Schwelle T (5) |

Speicherkonfiguration: der ganze Befehl läuft mit `$FF00 = $3E` (RAM 0 + I/O, wie `VMS`), am Ende wird die gemerkte Konfiguration zurückgeschrieben (`complex_instruction_shared_exit`). Folge: Der Strom darf **nicht** in `$D000–$DFFF` (dort sieht die CPU I/O) und nicht in `$FF00–$FF04` liegen. Kein Test darauf (Kosten); steht in der Referenz.

VDC-Register:

| Reg | Verwendung |
|---|---|
| 24 | einmal je Aufruf Bit 7 = 0 (Block **fill**), Read-Modify-Write, alle anderen Bits bleiben (Bit 6 = Reverse) |
| 18/19 | Zieladresse: einmal am Anfang schreiben; bei Skip **lesen**, n addieren, schreiben; am Ende lesen (für `$1C08`) |
| 31 | Daten: Literale, CPU-Läufe, erstes Byte eines Block fill. Bleibt zwischen den Token angewählt (`$D600 = 31`) |
| 30 | Block fill: Anzahl der weiteren Bytes |

## 3. Algorithmus

### 3.1 Ablauf

```
rxv:                                   ; aus BASIC
    jsr b_parse_uint16       -> arg1   ; src
    jsr b_parse_comma_uint16 -> arg2   ; dst
    jsr remember_mem_conf               ; $FF00 = $3E, alte Konfiguration gemerkt
rxv_execute:                            ; Einsprung für VCL: arg1 = src, arg2 = dst, $FF00 = $3E
    Reg 24: A = Reg24 AND $7F, zurückschreiben
    rxv_ttok = min(RXV_T + $7E, $FE)    ; 16-Bit-sicher: bei Übertrag $FE
    Reg 18/19 = arg2 (AY_to_vdc_regs_18_19: A = High, Y = Low)
    $D600 = 31
    Y = 0
next:
    if Y >= $80: rxv_ptr += Y; Y = 0    ; cpy #$80 / bcc tok
tok:
    b = (rxv_ptr),Y; Y++                 ; lda (arg1),y / iny / tax
    b = $00          -> end
    b = $01..$7F     -> lit (X = n = b)
    b = $80..$FD     -> run
    b = $FE          -> skip
    b = $FF          -> lrun

lit:                                     ; X = n, 1..127
    repeat X times: A = (rxv_ptr),Y; Y++; warte bis VDC bereit; $D601 = A
    -> next

run:                                     ; X = b, L = b - $7E (2..127)
    A = (rxv_ptr),Y; Y++                 ; Wert v
    if b < rxv_ttok:                     ; L < T  -> CPU
        X = b - $7E
        -> cpu_run
    else:
        rxv_cnt = b - $7F (= L - 1), High = 0
        -> fill

lrun:                                    ; $FF lo hi v
    rxv_len = Wort; A = v; Y += 3
    if rxv_len.hi = 0 and rxv_len.lo != 0 and rxv_len.lo < RXV_T:
        X = rxv_len.lo; -> cpu_run
    rxv_cnt = rxv_len - 1 (16 Bit; L = 0 bedeutet 65536 -> rxv_cnt = $FFFF)
    -> fill

cpu_run:                                 ; A = v, X = L >= 1
    repeat X times: warte bis VDC bereit; $D601 = A
    -> next

fill:                                    ; A = v, rxv_cnt = L - 1, Reg 31 angewählt
    warte bis VDC bereit; $D601 = A      ; erstes Byte (setzt Reg 31 = v für den Fill)
    rxv_y = Y
    A = rxv_cnt.lo; Y = rxv_cnt.hi
    jsr vdc_do_YYAA_cycles               ; wählt Reg 30; schreibt lo (falls != 0), dann hi-mal 0 (= 256)
    $D600 = 31; Y = rxv_y
    -> next                              ; NICHT auf das Ende warten: der VDC füllt, während die CPU das nächste Token liest

skip:                                    ; $FE lo hi
    n = Wort; Y += 2; rxv_y = Y
    hi = Reg 18 (vdc_reg_X_to_A wartet auf Bereit -> ein laufender Fill ist fertig)
    lo = Reg 19
    (hi:lo) += n (mod 65536; n = 0 bedeutet 65536 = unverändert, folgt von selbst)
    Reg 18/19 = (hi:lo)
    $D600 = 31; Y = rxv_y
    -> next

end:
    $1C06/$1C07 = rxv_ptr + Y            ; hinter dem $00
    $1C09 = Reg 18, $1C08 = Reg 19       ; wartet auf Bereit
    jmp complex_instruction_shared_exit
```

„Warte bis VDC bereit“ ist `bit $D600 / bpl *-3` (Makro `+vdc_sta`). Jede Datenoperation wartet selbst; dadurch überlappt ein Block fill mit dem Lesen des nächsten Tokens („issue“, P0 Fall 5).

### 3.2 Schwelle im Tokenvergleich

Kurzer Lauf: L = b − `$7E`, also gilt L < T genau dann, wenn b < T + `$7E`. `rxv_ttok` wird einmal je Aufruf berechnet; der Vergleich je Token ist dann ein `cpx rxv_ttok`. Für T ≥ 128 ist `rxv_ttok` = `$FE` (alle kurzen Läufe per CPU). T = 0 oder 1: alle Läufe per Block fill (ein Lauf mit L = 1 schreibt dann nur das erste Byte, ohne Reg 30).

Langer Lauf: per CPU genau dann, wenn L ≤ 255 und L < T. Läufe ≥ 256 gehen immer per Block fill.

### 3.3 Reg 30 = 0

`vdc_do_YYAA_cycles` (aus `vdclib.a`, auch von `VMF`/`VMC` benutzt) schreibt für ganze Seiten Reg 30 = 0 und verlässt sich darauf, dass der VDC dann 256 Byte füllt.

- VICE 3.10: gesichert (`vdc-mem.c`, `vdc_perform_fillcopy`: `blklen = regs[30] ? regs[30] : 256`).
- Echte Hardware: in der Literatur so beschrieben, `VMF` mit mehr als 256 Byte läuft seit 2013 so. **Zu prüfen** auf einem C128 mit dem Test `vdc-reg30-zero.bas` (7.4); das Listing gibt das Ergebnis auch als Text aus und läuft ohne Debug-Cartridge.
- Falls die Hardware abweicht, betrifft das `VMF`/`VMC` genauso; Abhilfe dann zentral in `vdc_do_YYAA_cycles` (Stücke zu 255), nicht in `RXV`.

### 3.4 Skip über das Lesen von Reg 18/19

Der Decoder führt die Zieladresse nicht in Software mit (spart je Token etwa 20 Zyklen). Beim Skip liest er die aktuelle Adresse aus Reg 18/19. Nach Reg-31-Schreibzugriffen und nach einem Block fill steht dort die Adresse hinter dem letzten Byte (VICE: `vdc-mem.c`, beide Pfade aktualisieren `regs[18/19]`).

**Zu prüfen auf echter Hardware:** Lesbarkeit von Reg 18/19 nach Block fill und nach Reg-31-Schreiben. Test: `rxv-hw.bas` (7.4, Vektoren mit Skip direkt nach Literal und direkt nach Fill). Rückfallebene, falls die Hardware abweicht: Build-Schalter `RXV_TRACK_DST = 1` – der Decoder addiert die Länge jedes Literals/Laufs zu `arg2` und schreibt bei Skip `arg2 + n` (16 Bit) ohne zu lesen; `$1C08` kommt dann aus `arg2`. Beide Varianten müssen dieselben Tests bestehen.

### 3.5 Grenzen und Fehlerverhalten

| Fall | Verhalten |
|---|---|
| `src`/`dst` fehlt, Komma fehlt, dritter Parameter | `?SYNTAX ERROR` (Fehler 11) |
| Wert außerhalb 0–65535 | `?ILLEGAL QUANTITY ERROR` (14) |
| Strom ohne `$00`, ungültige Längen am Speicherende | nicht geprüft: der Decoder liest weiter (Zeiger läuft über `$FFFF` nach `$0000`), Ergebnis undefiniert |
| `src` zeigt auf den Dateikopf statt auf den Strom | nicht erkannt (`"V"` = `$56` wird als 86 Literale gelesen) → Referenz warnt |
| Strom in `$D000–$DFFF` oder `$FF00–$FF04` | liest I/O bzw. MMU → undefiniert |
| Ziel läuft über `$FFFF` | VRAM-Adresse läuft nach `$0000` um (Verhalten des VDC); bei 16-KB-VDC spiegelt der Speicher |
| STOP-Taste | wird nicht abgefragt |
| `T` (`$1C04`) | jeder Wert 0–255 erlaubt (3.2); das Ergebnis im VRAM hängt **nicht** von T ab, nur die Zeit |

### 3.6 IRQ

`RXV` ändert das I-Flag nicht (wie `RTV`, `VMC`). Interrupts bleiben während des Entpackens an; die Kernal-IRQ-Routine läuft auch bei `$FF00 = $3E` (Einsprung im RAM bei `$FF05`). Voraussetzung wie bei allen VDC-BASIC-Befehlen: Keine Interruptroutine greift auf den VDC zu (sie würde `$D600` umstellen, danach gingen die Datenbytes in ein falsches Register). Steht schon in der Referenz („Conventions“), dort um `RXV` ergänzen.

## 4. VCL-Typ 8 = `RXV`

| Byte | Inhalt |
|---|---|
| 0 | 8 |
| 1–2 | `src` (Bank 0) |
| 3–4 | `dst` (VRAM) |

Länge 5 Byte. Umsetzung in `.vcl_next_command`: `vcl_parameter_bytes[8]` = 3 (Anzahl − 1); im Zweig `.vcl_check_vms` vor der Prüfung auf 12: `cmp #8 / beq .vcl_rxv`; `.vcl_rxv: jsr rxv_execute : jmp .vcl_next_command`. `rxv_execute` benutzt `arg1`–`arg5` (von VCL je Eintrag gelöscht) und berührt `arg_address`, `offset_1`, `vcl_saved_cr` nicht. Die Speicherkonfiguration ist bei VCL schon `$3E` (`remember_mem_conf` je Eintrag), `complex_instruction_shared_exit` stellt genau diese wieder her – wie bei Typ 1–5.

Bisher war Typ 8 für „VMO mit 2 Parametern“ reserviert (Kommentar im Quelltext, `vcl-format.md`). Typ 8 wird `RXV`; 9–11 bleiben reserviert. Kommentar, `vcl-format.md` und README („Known limits“) anpassen.

Kosten: Ein VCL-Eintrag kostet etwa 270 CPU-Zyklen bis zum Aufruf (5 Byte lesen ≈ 150, Argumente löschen ≈ 90, Verzweigung ≈ 30) gegenüber einigen tausend Zyklen für das Parsen von `RXV S,D` in BASIC. Für viele kleine Ströme (Tiles, Teilbilder) daher VCL verwenden.

## 5. Schwelle T (`$1C04`)

- Dokumentiertes, patchbares Byte (review-notes D.8): `POKE DEC("1C04"),T` in BASIC (`BANK 15` genügt, die Zelle liegt unter `$4000`), im Kompilat ebenso. Wirkt auf `RXV` und auf die Mittelteile von `VDL`/`VDR`/`VDB` (P2c).
- Standardwert im Binary: **14** (Platzhalter aus dem Kostenmodell von P1, `vrl.py threshold()` mit den Platzhaltern). Ersetzen durch T aus der P0-Messung auf **echter Hardware** (`test/bench/results/c128-*.json`, `engine.T`); T aus VICE ist nicht übertragbar (P0, Abschnitt 2). Im Quelltext als `RXV_T_DEFAULT = 14 ; vorläufig, siehe docs/v3/P2b-rxv.md 5`.
- Kein Fake-Register über `RGW`: `RGW` schreibt VDC-Register; ein Pseudo-Register 253 wäre eine Sonderbehandlung im schnellen Pfad. `POKE` reicht.

## 6. Zyklenschätzung (CPU-Zyklen bei 2 MHz, VDC sofort bereit)

Aus dem Ablauf 3.1 gezählt; nach der Umsetzung aus dem Listing nachzählen und in dieser Tabelle ersetzen.

| Pfad | fest | je Byte | Bemerkung |
|---|---|---|---|
| Token lesen + verzweigen (`tok`) | ≈ 20 | – | inkl. `cpy #$80`; Nachziehen von `rxv_ptr` ≈ 20 je ≥ 128 Strombytes |
| Literal | – | 22 | `lda (zp),y / iny / bit / bpl / sta / dex / bne` |
| Lauf per CPU | ≈ 24 | 15 | |
| Lauf per Block fill | ≈ 100 bis Ausgabe | VDC | danach arbeitet der VDC allein; die CPU wartet erst beim nächsten Datenzugriff |
| jede weitere volle Seite (L > 256) | ≈ 12 + Wartezeit | – | die CPU wartet hier auf den vorigen Fill |
| langer Lauf (Zuschlag) | ≈ 45 | – | |
| Skip | ≈ 145 | – | 2 Registerlesungen, 16-Bit-Addition, 2 Registerschreibungen |
| Ende | ≈ 80 | – | Zeiger in den Kopf |
| Aufruf-Setup (`rxv_execute`) | ≈ 120 | – | ohne BASIC-Parser |

Für `vrl.py stats` (P1, 8.2) ergibt das: `tok` 20, `cpu_lit_byte` 22, `cpu_byte` 15, `cpu_run_fixed` 24, `new_addr` 145, `fill_fixed` 100 + VDC-Anlauf, `fill_byte` und `fill_chunk` aus P0. Diese Werte in die P0-JSON (`engine`) bzw. als Platzhalter in `vrl.py` übernehmen ist Teil der Abnahme (eigener kleiner Commit im Engine-Repo, nach Rücksprache).

In VICE ist der Reg-31-Zugriff im sichtbaren Bereich mit 43 Takten Busy modelliert; Zyklenmessungen in VICE sind deshalb nur relativ (Pfade untereinander) aussagekräftig.

## 7. Tests

### 7.1 Harness `test/rxv/run_rxv.py`

- Importiert `vrl.py` aus `$DBE_TOOLS` (Standard `~/git/dungeonboringengine/tools`); fehlt es → Abbruch mit Meldung, Exitcode 2.
- Monitor über `test/vicemon.py` (P2a, 6.4: Port 6631–6649, bind-Test, `-remotemonitoraddress`, nur eigene PID).
- **Schritt 0 (einmalig, Ergebnis in 9):** prüfen, dass der VICE-Monitor mit `bank vdc` VRAM liest und schreibt (`m`, `>`, `save`, `load`; Quelltext `c128mem.c`: Bank `vdc` mit `vdc_ram_read`/`vdc_ram_store` vorhanden). Rückfallebene: VRAM-Inhalt per `VTR` in RAM 0 kopieren und von dort sichern.

Ablauf (ein VICE-Lauf für alle Vektoren):

1. Vektoren erzeugen (7.2): je Vektor Strom, `src`, `dst`, T. Alle Ströme in eine Datei `RXVDATA` (Ladeadresse `$8000`, Ströme bis `$CFFF`), Sonderlagen (7.2, Gruppe P) per Monitor direkt ins RAM.
2. Treiberprogramm `rxvt.bas` (vom Harness erzeugt, interpretiert; Kompilat zusätzlich mit `-c`-Weg aus P2a 5.4):
   ```
   10 POKE DEC("1BFE"),96:POKE DEC("1BFF"),96:BLOAD"RXVDATA",B0,P(32768)
   20 READ S,D,T:IF S<0 THEN 90
   30 SYS DEC("1BFE"):POKE DEC("1C04"),T:RXV S,D:SYS DEC("1BFF"):GOTO 20
   90 BANK 15:POKE DEC("D7FF"),0
   100 DATA …,-1,0,0
   ```
   Kein `RGO 28,16` nötig, wenn `-VDC64KB` gesetzt ist und der Kernal Reg 28 passend lädt; der Harness prüft `RGD(28) AND 16` einmal am Anfang.
3. Haltepunkt `$1BFE` (Vorbereitung): Harness schreibt das Grundmuster (64 KB, `random.Random(7)`, fest) mit `bank vdc` + `load` ins VRAM.
4. Haltepunkt `$1BFF` (Prüfen): `save` VRAM `0000–ffff`, `m 1c06 1c09` (Bank RAM). Erwartung: Grundmuster, darüber `vrl.decode(stream)` ab `dst` geschrieben, Adressen **modulo 65536** (Umlauf); `$1C06` = `src + len(stream)`; `$1C08` = `(dst + Summe aller Literal-, Lauf- und Skip-Längen) mod 65536` (aus `vrl.decode_tokens`, Skips am Ende zählen mit). Jede Abweichung: Vektorname, erste abweichende Adresse, Ist/Soll, Exitcode 1.
5. BASIC-Fehlervektor `($0300)` als Haltepunkt → Fehler melden.

Ausgabe: je Vektor eine Zeile, am Ende Anzahl; zusätzlich die Zeit je Vektor aus der Monitor-Stoppuhr zwischen Haltepunkten auf `rxv_execute` und `complex_instruction_shared_exit` (nur Information, Tabelle in `test/rxv/cycles.md`).

### 7.2 Testvektoren

Strom in Hex ohne Dateikopf; „v“ = fester Wert je Vektor (z. B. `$A5`). Das Grundmuster ist zufällig, verglichen wird byteweise über den ganzen VRAM.

**Gruppe B – Grundformen** (T = Standard)

| Name | Strom | Prüft |
|---|---|---|
| `empty` | `00` | nichts geschrieben; `$1C06` = src + 1; `$1C08` = dst |
| `lit1` | `01 05 00` | |
| `lit127` | `7F` + 127 Byte + `00` | längstes Literal |
| `lit128` | `list(range(128))` kodiert (`7F … 01 7F 00`) | zwei Literal-Token |
| `zero-data` | `02 00 00 80 00 00` | Wert 0 in Literal und Lauf |
| `foreign` | `80 07 80 07 00`, `FF 01 00 07 00`, `FE 05 00 00` | Folgen, die der Encoder nie erzeugt |
| `rect` | `encode_rect([1,2,3,4],2,2,80)` = `02 01 02 FE 4E 00 02 03 04 00` | Zeilensprung per Skip |
| `rect-mask` | `encode_rect([1,2,3,4],2,2,10,mask=[1,0,0,1])` | Skip über Maske + Stride |

**Gruppe R – Läufe** (jeder Vektor mit T ∈ {0, 1, 2, 3, 14, 127, 128, 255}; das VRAM-Ergebnis muss für alle T gleich sein)

| Name | Strom | Prüft |
|---|---|---|
| `run2`, `run3` | `80 v 00`, `81 v 00` | kürzeste Läufe |
| `runT-1`, `runT`, `runT+1` | kurzer Lauf mit L = 13, 14, 15 (Token `$8B`, `$8C`, `$8D`) | Grenze bei T = 14 |
| `run127` | `FD v 00` | längster kurzer Lauf, `rxv_ttok`-Deckel |
| `lrun1` | `FF 01 00 v 00` | L = 1 per Fill-Pfad ohne Reg 30 (bei T ≤ 1) |
| `lrun128`, `lrun255` | `FF 80 00 v 00`, `FF FF 00 v 00` | lange Läufe < 256: `lrun128` bei T = 255 per CPU, `lrun255` immer per Fill (255 < T nie erfüllt) |
| `lrun256`, `lrun257` | `FF 00 01 v 00`, `FF 01 01 v 00` | Reg 30 = 255 bzw. 0 (eine volle Seite) |
| `lrun511`, `lrun512`, `lrun513` | `FF FF 01 …`, `FF 00 02 …`, `FF 01 02 …` | Teilseite + Seiten |
| `lrun65535` | `FF FF FF v 00` | 255 Seiten + 254 |
| `lrun65536` | `FF 00 00 v 00` | ganzer VRAM = v, `$1C08` = dst |
| `fill-fill` | `FF 00 01 07 FF 00 01 09 00` | Fill direkt nach Fill (Wert in Reg 31 wechselt) |
| `fill-lit` | `FF 00 02 07 01 09 00` | Literal wartet auf laufenden Fill |
| `fill-run` | `FF 00 02 07 81 09 00` | CPU-Lauf nach Fill |

**Gruppe S – Skips**

| Name | Strom | Prüft |
|---|---|---|
| `skip-first` | `FE 01 00 01 05 00` | führender Skip |
| `skip0` | `FE 00 00 01 05 00` | n = 0 = 65536 → Adresse unverändert |
| `skip-max` | `FE FE FF 01 01 00` mit dst = `$0010` | Skip 65534 → Umlauf auf `$000E` |
| `skip-end` | `01 05 FE 05 00 00` | Skip vor Ende: nichts geschrieben, `$1C08` = dst + 6 |
| `fill-skip` | `FF 00 01 07 FE 10 00 01 09 00` | Skip liest 18/19 **während** eines Fills (Warten) |
| `lit-skip` | `03 01 02 03 FE 02 00 01 04 00` | Skip nach Reg-31-Schreiben |
| `skip-skip` | `FE 03 00 FE 04 00 01 05 00` | zwei Skips nacheinander |

**Gruppe P – Lage im RAM**

| Name | src | Prüft |
|---|---|---|
| `page-cross-lit` | `$80FE`, Literal 127 | Strom über Seitengrenze |
| `norm-edge` | `$8000`: Literal 126, Literal 127, Literal 127, Lauf, `00` | Y erreicht 127 → 255 vor dem Nachziehen (3.1 `next`) |
| `below-4000` | `$1300` | Strom unter `$4000` |
| `below-d000` | Strom endet bei `$CFFF` | letzte Seite vor I/O |
| `long-stream` | `$8000`, > 4 KB Strom | viele Nachzieh-Schritte |

**Gruppe Z – Ziel**

| Name | dst | Prüft |
|---|---|---|
| `wrap` | `$FFF0`, Literal 32 | Umlauf `$FFFF` → `$0000` |
| `dst0`, `dstFFFF` | `$0000`, `$FFFF` | Ränder |

**Gruppe X – Zufall und echte Daten**

- `rand-*`: 60 Folgen aus `random.Random(3)`, Länge 0…3000, Alphabet {2, 5, 256} Werte, `SKIP`-Anteil {0, 10, 50} %, lange Läufe eingestreut; `vrl.encode`, dst zufällig; T zufällig aus {0, 2, 14, 255}.
- `dungeon`: `docs/listings/data/dungeon` (8000 Byte Bild) mit `vrl.encode` → dst `$0000`, T Standard.
- `wall`: ein Wandbild aus P3/P5, sobald vorhanden (sonst entfällt).

### 7.3 Tests im normalen Lauf (`test/run.sh`, interpretiert und `-c`)

| Datei | Inhalt |
|---|---|
| `rxv-basic.bas` | kleiner Strom per `POKE` bei `$A000`, `RXV`, Prüfung mit `VMD`; `$1C06`/`$1C08` per `PEEK` |
| `rxv-errors.bas` | `RXV` ohne Parameter → `ER` = 11; `RXV 70000,0` → 14; `RXV 1,2,3` → 11 (über `TRAP`) |
| `vcl-rxv.bas` | VCL-Liste: Typ 8 (`RXV`), danach Typ 6 (`VMW`), danach Ende; prüft beide Wirkungen (Listenzeiger nach `RXV` intakt) und dass `PEEK(DEC("FF00"))` nach `VCL` wieder dem Wert vorher entspricht |
| `rxv-config.bas` | `RXV` aus `BANK 0` und `BANK 15` gerufen: gleiche Wirkung, `$FF00` danach wie vorher; Reg 24 Bit 6 bleibt erhalten |

### 7.4 Hardware-Listings (laufen ohne Harness auf einem C128, Ausgabe auf dem Bildschirm, in VICE zusätzlich mit Debug-Cartridge)

| Datei | Prüft |
|---|---|
| `test/vdc-reg30-zero.bas` | Reg 30 = 0 füllt 256 Byte: `RGA 24,127`, Adresse setzen, `RGW 31,v`, `RGW 30,0`; `VMD(a+256)` = v, `VMD(a+257)` unverändert |
| `test/rxv-hw.bas` | die Vektoren `fill-skip`, `lit-skip`, `lrun257`, `lrun65536` (mit Rücksicherung des Bildschirms) und `rand-0…4`, Sollwerte als Prüfsumme (16 Bit, vom Harness erzeugt) im Listing |

## 8. Doku

| Datei | Änderung |
|---|---|
| `docs/reference/commands.md` | neuer Abschnitt „Unpacking“ mit `RXV` (Syntax, Bank 0, Strom statt Kopf, Speicherverbote, `$1C04`/`$1C06`/`$1C08`, Umlauf, keine Prüfung); Übersichtstabelle; „Memory used by VDC-BASIC“ um den Kopf und `arg1`–`arg5` |
| `docs/reference/vrl-format.md` (neu) | Tokentabelle und Dateirahmen VRL v1 (aus P1, 2 und 3), Verweis auf das Werkzeug |
| `docs/reference/vcl-format.md` | Typ 8 = `RXV`, Typ 9–11 reserviert |
| `BASIC V7.0 VDC.txt` | `RXV;fe41;` |
| `README.md` | Zahl der Befehle, Tokentabelle, Kopf, Versionsgeschichte v3 |
| `test/README.md` | `test/rxv/run_rxv.py`, `DBE_TOOLS`, Portregel |

Ein Guide-Kapitel zu `RXV` folgt mit P8.

## 9. Abnahme

| Prüfung | Erwartung |
|---|---|
| `test/rxv/run_rxv.py` | alle Vektoren 7.2 bytegleich mit `vrl.decode` über den ganzen VRAM, Zeiger `$1C06`/`$1C08` richtig, Exitcode 0 |
| T-Unabhängigkeit | Gruppe R mit allen T gleich |
| `test/run.sh test/*.bas` und `-c` | alle PASS, inkl. 7.3 |
| Größe | `RXV` + VCL-Typ 8 ≤ 400 Byte (Symbolliste) |
| Zyklen | Tabelle 6 aus dem Listing nachgezählt, `cycles.md` aus VICE |
| Doku | 8 vollständig |
| Befunde | Schritt 0 (Monitor-Bank `vdc`), 3.3/3.4 auf Hardware als offen oder erledigt vermerkt |

## 10. Befunde

(wird bei der Umsetzung ausgefüllt: Schritt 0, Hardware 3.3/3.4)
