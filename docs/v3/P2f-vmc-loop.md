# P2f – Zeilenschleife von VMC/VMF ohne JSR

Stand: 2026-11-26. Anlass: `dungeonboringengine/docs/specs/P6g-vcl-analysis.md` (Vorschläge V3/V5): In der Crawl-Sicht hängen die Kosten an der Zahl der Operationen, eine VMC-Zeile kostete 187 Zyklen (mit Quellschritt 281), eine VMF-Zeile 237.

## 1. Was

`vmc_execute`, `block_copy_target` (Einsprung von VMS) und `vmf_execute` teilen sich eine Zeilenschleife ohne JSR/RTS:

- Registernummer direkt nach `$D600` (`STX`), vor **jedem** Zugriff auf `$D601` das Ready-Bit (`BIT $D600 / BPL`), dann `STA $D601`.
- Ziel bleibt in `arg2` (Zeropage), Schritt `arg5`; wie bisher wird erst nach `dec arg4` addiert (letzte Zeile: kein Schritt, die Zeropage am Ende ist dieselbe wie vorher).
- Quellschritt (`arg6`) wird **einmal je Aufruf** geprüft: Die Schleife springt am Ende entweder auf `.vmc_dst_step` (nur R18/R19) oder auf `.vmc_src_step` (`arg1 += arg6`, R32/R33, dann R18/R19); das Low-Byte des Rücksprungs wird selbst geändert (beide Marken liegen in einer Seite, `!if` prüft das). Bei `arg6` = 0 zählt der VDC die Quelle selbst weiter, R32/R33 werden nicht neu gesetzt (unverändert).
- R30: Low-Byte direkt, wenn es nicht 0 ist; ist das High-Byte nicht 0, folgt der bestehende Seitenweg (`vdc_do_YY_pages`, neue Marke im Makro `addcode_vdc_do_YYAA_cycles`, Code unverändert). Damit gilt für jede Länge (0, < 256, ≥ 256) genau die Schreibfolge von `vdc_do_YYAA_cycles`.
- VMF: dieselbe Schleife mit einem Füllschritt (R31 = Wert) nach R18/R19. Ein selbst geänderter Befehl vor dem Füllschritt ist `JMP` (VMC, überspringt ihn) oder `BIT abs` (VMF, 4 Takte, fällt hinein). Damit Ziel und Schritt in denselben Zellen stehen, tauscht VMF `arg1` und `arg2` vor der Schleife und danach wieder zurück; die Zeropage am Ende ist wie vorher.
- R24 (Bit 7 Kopieren/Füllen) und der Kopf von VMF (Zähler − 1, auch mit Y aus dem 6-Parameter-Aufruf) sind unverändert.
- `complex_instruction_shared_exit` liegt jetzt direkt hinter der Schleife (der Ausgang ist ein `BEQ`), `remember_mem_conf` schreibt `saved_mem_conf` (dieselbe Adresse wie vorher `.cr`).

Reihenfolge der Registerzugriffe je Zeile wie vorher: (R32, R33), R18, R19, (R31), R30. Die Adressrechnung für die nächste Zeile läuft nach R30, während der VDC kopiert/füllt. Versuch: die Rechnung zwischen R19 und R30 (VICE hält den VDC nach R18/R19 43 Takte beschäftigt) war in VICE etwa gleich, in Z64K aber deutlich langsamer (VMC 8 Byte 1 MHz 171 statt 150 Takte je Zeile), verworfen.

## 2. Warum

Je Zeile entfallen fünf bis sieben JSR/RTS-Paare (`AY_to_vdc_regs_18_19` mit zwei inneren Aufrufen, `vdc_do_YYAA_cycles`, bei VMF `A_to_vram_XXYY`) und die Prüfung von `arg6` je Zeile. Alle Aufrufer profitieren: BASIC `VMC`/`VMF`, `VCL` Typ 1–5, `VMS` Typ 1/2 (`block_copy_target`), `VMP`/`VMB`.

## 3. Größe

`vdcbasic3.bin` 4771 → 4842 Byte (+71), Ende `$2EA0` → `$2EE9` (unter `zvt` `$2F00` und `$4000`). Kopf unverändert: `$1C00` `JMP $1C10`, `$1C03` = `$30`.

## 4. Zyklen je Zeile

Messprogramm (`build/p2f/h/timing.py`, `stub.a`, nicht im Repository): ein Stub bei `$1B10` ruft `vmc_execute` / `vmf_execute` / `block_copy_target` mit gesetztem I-Flag, CIA-2-Timer A misst (Takte bei 1 MHz, µs bei 2 MHz); je Fall 1 und 201 Zeilen, je 5 (VICE) bzw. 9 (Z64K) Wiederholungen, Wert = (Mittel 201 − Mittel 1) / 200. VMC/VMS: Länge 8 bzw. 40, Zielschritt 80; „Quelle“ = Quellschritt 80; VMF: Länge 8/40. VICE ist deterministisch, Z64K streut von Lauf zu Lauf um bis zu ±10 % (Lage zum Bildaufbau).

| Fall | VICE 1 MHz vorher → nachher | VICE 2 MHz (µs) | Z64K 1 MHz | Z64K 2 MHz (µs) |
|---|---|---|---|---|
| VMC, 8 Byte | 180 → 136 (-25 %) | 112 → 84 (-25 %) | 207 → 144 (-30 %) | 120 → 80 (-33 %) |
| VMC, 8 Byte, Quellschritt | 274 → 200 (-27 %) | 152 → 122 (-20 %) | 279 → 214 (-23 %) | 162 → 125 (-23 %) |
| VMC, 40 Byte | 181 → 149 (-18 %) | 123 → 114 (-7 %) | 251 → 187 (-26 %) | 157 → 155 (-1 %) |
| VMF, 8 Byte | 230 → 173 (-25 %) | 156 → 110 (-30 %) | 241 → 181 (-25 %) | 142 → 99 (-30 %) |
| VMF, 40 Byte | 231 → 174 (-25 %) | 146 → 112 (-23 %) | 268 → 192 (-28 %) | 164 → 126 (-23 %) |
| `block_copy_target` (VMS Typ 2), 8 Byte | 183 → 137 (-25 %) | 113 → 83 (-27 %) | 209 → 144 (-31 %) | 118 → 82 (-30 %) |

Reine CPU-Zeit der neuen Schleife (gezählt): VMC 98 Takte je Zeile, mit Quellschritt +58, VMF +20 (Füllschritt); der Rest ist Warten auf das Ready-Bit. Bei 40 Byte je Zeile bestimmt in Z64K bei 2 MHz der VDC die Zeit (kaum Gewinn). Die Vorher-Werte stammen aus demselben Messprogramm mit dem Stand P2e (nicht neu zu messen); zum Vergleich P6g (VICE, im Spiel gemessen): VMC-Zeile 187, mit Quellschritt 281, VMF 237.

## 5. Tests

- Differenztest alt gegen neu (`build/p2f/h/diff.py`, VICE-Monitor): 1000 Vektoren (`vmc_execute`, `vmf_execute` mit Y = arg3 und mit fremdem Y, `block_copy_target`), Länge 0–2047, Zeilen 1–256 (0), Schritte und Quellschritte auch negativ (`$FFB0`, `$FFFF`, `$FF00`), `$0100`, zufällige 16-Bit-Werte, `arg4`-High-Byte gesetzt. Verglichen: ganzes VRAM (Zufallsmuster vorher), `arg1`–`arg6` danach, R18/R19/R24/R30/R32/R33. Ergebnis: 1000/1000 gleich.
- VICE: `test/run.sh test/*.bas` 49/49 interpretiert, `-c` 49/49 kompiliert; `test/rxv/run_rxv.py` 398/398; `test/draw/run_draw.py` (voller Satz) 1520/1520.
- Z64K: `test/run.sh -z` mit allen 29 Listings, die VMC/VMF/VCL/VMS/VMP/VMB benutzen, interpretiert 29/29 und `-z -c` 29/29. Wiederholt werden mussten 1 bzw. 10 Läufe: Der Java-Starter brach mit rc 125 ab (bei drei bis vier parallelen Starts), bzw. ein Start blieb vor der Emulation stehen (8 s CPU, nur JIT-Threads). Die Wiederholung war jeweils grün.
- Z64K `test/rxv/run_rxv.py --emu z64k`: 238/238. Im ersten Lauf erreichte einer der drei Starts `-limitcycles` (Wächter) bei 159/238 gleichen Vektoren und 0 Abweichungen. Die Wiederholung brachte 238/238 (RXV benutzt die VMC/VMF-Schleife nicht).
- `run_draw.py --emu z64k` wurde abgebrochen: VDM/VDL/VDR/VDB rufen weder `vmc_execute` noch die VMF-Schleife auf, und deren Hilfen sind im Code unverändert (in `vdclib.a` kam nur die Marke `vdc_do_YY_pages` dazu). Für die Zeichenbefehle hat sich nur die Lage verschoben, das deckt der volle VICE-Lauf ab (1520/1520).

## 6. Wirkung auf Crawl 128 1.1

Nur `libs/vdcbasic3.prg` ersetzt (gleiches Format, Ladeadresse `$1C00`). `test/vice_crawl.py` (Pixeltest gegen 1.0f, interpretiert und kompiliert): **PASS**. Zeiten (VICE: Takte, zwei identische Läufe; Z64K: T9 in Jiffies, je zwei gleiche Läufe):

| Konfiguration | CPU | 1.0f | 1.1 P2e (Sprung) | **1.1 P2f** | gegen P2e | gegen 1.0f |
|---|---|---|---|---|---|---|
| VICE, kompiliert | 2 MHz | 3 385 841 | 3 353 392 | **3 104 710** (3,15 s) | −7,4 % | −8,3 % |
| VICE, kompiliert | 1 MHz | 6 318 623 | 6 337 353 | **5 870 128** (5,96 s) | −7,4 % | −7,1 % |
| VICE, interpretiert | 2 MHz | 21 550 022 | 21 908 130 | **21 652 569** (21,98 s) | −1,2 % | +0,5 % |
| VICE, interpretiert | 1 MHz | 43 727 387 | 44 549 257 | **44 078 865** (44,74 s) | −1,1 % | +0,8 % |
| Z64K, kompiliert | 2 MHz | 260 | 256 | **230** (3,82 s) | −10,2 % | −11,5 % |
| Z64K, kompiliert | 1 MHz | 463 | 459 | **429** (7,13 s) | −6,5 % | −7,3 % |

Die Messwerte stehen dauerhaft im Spiel-Repository in `test/timing-results.json`. Release-Images `crawl128-110.d81`/`.d64` neu gebaut.
