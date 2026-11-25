# P2a: VDC-BASIC nach `$1C00`, Boot-Lader, Blitz!-Image-2-Test

Stand: 2026-11-25. Detailspezifikation zu Schritt P2a aus `dungeonboringengine/docs/plan-0.2.md` (Abschnitte 3 und 7.1). Umsetzung: Opus. Review: Opus. Voraussetzung für P2b (`RXV`) und P2c (`VDM`/`VDL`/`VDR`/`VDB`).

## 1. Ziel und Ergebnis

- VDC-BASIC läuft ab `$1C00` (Bank 0) statt ab `$1300`. Platz: `$1C00–$3FFF` = 9216 Byte (v2h: 2294 Byte).
- BASIC-Text beginnt bei `$4001`. Ein Boot-Lader stellt das ein, lädt und installiert VDC-BASIC und startet auf Wunsch ein Programm.
- Dasselbe Programm läuft interpretiert und als Blitz!-Kompilat mit Runtime-Image 2 (`$4001`).
- Ein fester Kopf bei `$1C00` enthält Einsprung, Version und die patchbaren Bytes für P2b/P2c.
- Dieser Schritt ändert kein Verhalten der Befehle. Neue Befehle kommen erst mit P2b/P2c.

Datei: `vdcbasic3.bin`, Ladeadresse `$1C00`. `vdcbasic2h.bin` bleibt unverändert im Repo (P0 nutzt es).

## 2. Änderungen im Quelltext (`src/vdcbasic.asm`)

| Stelle | heute | neu |
|---|---|---|
| Ausgabedatei | `!to "vdcbasic2h.bin", cbm` | `!to "vdcbasic3.bin", cbm` |
| Origin | `* = $1300` | `* = $1C00`, danach der Kopf (2.1), Installationscode ab `$1C10` |
| Meldung | `vdc basic v2h installed` | `vdc basic v3 installed` |
| Versionskommentar | – | Eintrag „v3, 2026: nach `$1C00` verlegt, Kopf mit festen Adressen“ |
| Build-Schalter | `release_vdl/vdr/vdb = 0` | bleiben in P2a 0; P2b ersetzt sie durch feste Einträge mit Stub (2.2) |
| Größenprüfung | keine Gesamtprüfung | am Dateiende: `!if * > $4000 { !error "VDC-BASIC v3 endet bei ", *, ", erlaubt bis $3FFF" }` |
| Kopfprüfung | – | vor jedem Kopffeld `!if * != $1C0x { !error … }` (Adressen sind Schnittstelle) |
| `+vdclib_assert_below_basic_rom` | 3 Aufrufe | bleiben; sie sind durch die Gesamtprüfung abgedeckt, schaden aber nicht |

Absolute Adressen im Code: Es gibt keine Verweise auf `$1300–$1BFF` außer über Labels. Die ROM-Einsprünge (`$43E2`, `$4321`, `$43B2`, `$516A`, `$7956`, `$8815`, `$80DB`, `$877B`, `$87F4`, `$8803`, `$880F`, `$8812`, `$795C`, `$77EF`, `$77DA`, `$849F`, `$C027`, `$E1DC`, `$E192`), die Kernal-Zellen (`$02A2`, `$02AA`, `$02CD`, `$02FC`, `$030C–$0311`) und die Zeropage bleiben gleich. Selbstmodifizierender Code (`.SELFMOD`, `.cr`, `.spbuf`, `.lowest_sp`) wandert mit, das ist unkritisch (RAM).

Der Code muss vollständig unter `$4000` liegen: Teile laufen mit `$FF00 = $00` (BASIC-ROM ab `$4000` eingeblendet), und das Installationsprogramm wird mit `BANK 15` per `SYS` aufgerufen.

### 2.1 Kopf (feste Adressen, Schnittstelle)

| Adresse | Größe | Inhalt | gesetzt von |
|---|---|---|---|
| `$1C00` | 3 | `JMP install` (`SYS DEC("1C00")` = `SYS 7168`) | P2a |
| `$1C03` | 1 | Version, BCD: `$30` = v3.0 | P2a |
| `$1C04` | 1 | **Schwelle T** für Block fill (`RXV`, Spannen von `VDL`/`VDR`/`VDB`), Standard siehe P2b, 5 | P2b |
| `$1C05` | 1 | reserviert, 0 | – |
| `$1C06–$1C07` | 2 | `RXV`: Adresse hinter dem Ende-Token des zuletzt entpackten Stroms | P2b |
| `$1C08–$1C09` | 2 | `RXV`: VRAM-Adresse hinter dem letzten geschriebenen oder übersprungenen Byte | P2b |
| `$1C0A–$1C0F` | 6 | reserviert, 0 | – |

In P2a werden `$1C04–$1C0F` mit `!byte 14, 0, 0,0, 0,0, 0,0,0,0,0,0` angelegt, damit die Adressen ab jetzt feststehen. Alle Felder liegen unter `$4000`, sind also mit `BANK 15` per `PEEK`/`POKE` erreichbar (Lesen und Schreiben gehen an RAM 0).

### 2.2 Tokens

Unverändert `$FE27–$FE3D` und `$CE0B/$CE0C`. Festgelegt für v3 (Umsetzung P2b/P2c, hier nur dokumentiert):

| Token | Befehl | Herkunft |
|---|---|---|
| `$FE3A` | Platzhalter `zzZ` | bleibt frei: das zweite Byte `$3A` ist `:`, BASIC und Blitz! würden es beim Überspringen von Anweisungen als Trenner lesen |
| `$FE3E` | `VDL` | Reihenfolge im Quelltext seit `e4d1832` (nach `VMS`, `VMB`) |
| `$FE3F` | `VDR` | dito |
| `$FE40` | `VDB` | dito |
| `$FE41` | `RXV` | neu (P2b) |
| `$FE42` | `VDM` | neu (P2c) |

Die Bedingungen `!if release_vdl` usw. müssen beim Einschalten **alle drei** gesetzt sein, sonst verschieben sich die Nummern von `RXV`/`VDM`. P2b baut deshalb die Tabellen so um, dass `VDL`/`VDR`/`VDB` immer eingetragen sind; bis P2c zeigen sie auf einen gemeinsamen Stub `ldx #11 : jmp ($0300)` (BASIC-Fehlervektor, Fehler 11 = `?SYNTAX ERROR`; läuft mit `$FF00 = 0` wie `execute_instruction`). Die Schalter `release_vdl/vdr/vdb` entfallen dann.

### 2.3 Tokenliste und Tokeniser

- `BASIC V7.0 VDC.txt`: Kommentar „vdcbasic v3“, die Zeilen `VDL;fe3e;`, `VDR;fe3f;`, `VDB;fe40;`, `RXV;fe41;`, `VDM;fe42;` kommen mit P2b/P2c.
- `test/bastok.py`: neue Option `--start ADDR` (Standard `$1C01`, wie heute), damit Testprogramme mit korrekten Zeilenzeigern für `$4001` erzeugt werden (3.3). Aufruf bleibt kompatibel: `bastok.py [--start 0x4001] in.bas out.prg`.

## 3. Boot-Lader

### 3.1 Problem

Ein BASIC-Programm bei `$1C01` kann VDC-BASIC nicht nach `$1C00` laden, ohne sich selbst zu überschreiben, während es läuft. Deshalb setzt der Lader zuerst den BASIC-Anfang auf `$4001`, lädt **sich selbst** dorthin nach und lädt erst dann VDC-BASIC.

### 3.2 Datei `boot/boot.bas` (eine Datei, zwei Durchläufe)

```
10 IF PEEK(46)<>64 THEN POKE 16384,0:POKE 45,1:POKE 46,64:RUN"BOOT"
20 BLOAD"VDCBASIC3",B0,U8:IF DS THEN PRINT DS$:END
30 SYS 7168
40 IF PEEK(DEC("1C03"))<>DEC("30") THEN PRINT"WRONG VDC-BASIC":END
50 NEW
```

Ablauf:

1. `RUN` (von Hand, über Autostart oder Bootsektor) startet `BOOT` bei `$1C01`. `PEEK(46)` = `$1C` → Zeile 10: `$4000 = 0`, Textanfang `$2D/$2E = $4001`, `RUN"BOOT"` lädt dieselbe Datei an den neuen Textanfang (BASIC 7 lädt relativ zu `$2D/$2E`) und startet sie.
2. Zweiter Durchlauf bei `$4001`: `PEEK(46)` = 64 → Zeile 20 lädt `VDCBASIC3` an seine Ladeadresse `$1C00` (berührt `$4001+` nicht), Zeile 30 installiert, Zeile 40 prüft den Kopf.
3. Zeile 50: `NEW` beendet und lässt einen leeren BASIC-Speicher ab `$4001` zurück. Für ein Spiel/Programm ersetzt der Nutzer Zeile 50 durch `50 RUN"PROGRAMM"` (interpretiert oder Blitz!-Kompilat, 4).

Hinweise:

- `POKE 16384,0` schreibt bei `BANK 15` ins RAM unter dem BASIC-ROM (Schreibzugriffe auf ROM-Bereiche gehen an RAM 0).
- Die Variablen liegen weiter in Bank 1 ab `$0400`; `$2D/$2E` betrifft nur den Text.
- Wer `GRAPHIC 1`–`4` (VIC-Bitmap) benutzt, überschreibt VDC-BASIC (Farb-RAM `$1C00–$1FFF`, Bitmap `$2000–$3FFF`). Das ist eine neue, harte Einschränkung von v3 → README, Referenz „Loading“, Kapitel 1 des Guides. `GRAPHIC 0`/`5` sind erlaubt, `GRAPHIC CLR` nicht.
- Das P0-Messprogramm (`test/bench`, `$2000–$7FFF`) lädt weiter v2h bei `$1300` und ist von v3 nicht betroffen.

### 3.3 Autostart

| Art | Umsetzung | Zweck |
|---|---|---|
| VICE | `x128 … -8 disk.d64 -autostartprgmode 1 -autostart boot.prg` (das tokenisierte `BOOT`, Ladeadresse `$1C01`) | Tests |
| VICE, Disk-Image | `-autostart disk.d64`: lädt den **ersten** Directory-Eintrag → `BOOT` muss als erste Datei auf die Disk | Nutzer |
| C128-Bootsektor | Track 1, Sektor 0: `"CBM"`, Adresse 0, Bank 0, 0 Zusatzblöcke, Meldung `"VDC-BASIC V3"`, `0`, leerer Dateiname `0`, danach Code `LDX #<cmd : LDY #>cmd : JMP $AFA5` mit `cmd` = `RUN"BOOT"` + `0` im Sektorpuffer (`$0B00 + Offset`) | echte Hardware, VICE ohne `-autostart` |

Bootsektor-Werkzeug: `tools/mkboot.py OUT.bin [--name BOOT] [--message "VDC-BASIC V3"]` erzeugt die 256 Byte; Einspielen mit `c1541 disk.d64 -bwrite OUT.bin 1 0` **nach** dem Schreiben der Dateien (c1541 belegt Track 1 erst bei voller Disk; das Werkzeug prüft vorher mit `-bread`, dass Sektor 1/0 leer ist, sonst Abbruch).

**Zu prüfen (Pflichttest `boot-sector`, 7.3):** Format der Bootsektor-Felder und der Einsprung `$AFA5` (BASIC: Direktbefehl aus Puffer ausführen). Die Angaben stammen aus der Literatur zum C128-Kernal (`BOOT_CALL`, `$F890`), nicht aus diesem Repo. Test ohne `-autostart`, mit `-truedrive`/`+trapdevice8` (der Kernal liest den Sektor per Blockbefehl), Haltepunkt auf `$1C00` (Installation erreicht) und auf den BASIC-Fehlervektor.

## 4. Speicherkarte (Bank 0)

| Bereich | v2h | v3 |
|---|---|---|
| `$0000–$12FF` | System, Kernal, BASIC | unverändert (Engine: `$0B00–$0FFF` Parameterblock, siehe Plan 3) |
| `$1300–$1BFF` | VDC-BASIC v2h (`$1300–$1BF6`) | **frei**: Nutzer bzw. Puffer der Engine (VCL-, VMS-Listen); die Tests legen hier Listen „unter `$4000`“ ab |
| `$1C00–$1C0F` | BASIC-Text | **Kopf VDC-BASIC** (2.1) |
| `$1C10–` etwa `$2E00` | BASIC-Text | **Code und Variablen VDC-BASIC v3** (Endadresse aus der Symbolliste, in README und Referenz eintragen) |
| bis `$3FFF` | BASIC-Text | Reserve für VDC-BASIC (die Gesamtprüfung erlaubt bis `$3FFF`) |
| `$4000` | – | 0 (Byte vor dem Programmtext) |
| `$4001–` | BASIC-Text oder Daten | BASIC-Text bzw. Blitz!-Kompilat (Image 2: Runtime `$4001–$6C36`, danach Header, Bytecode, Variablen) |
| über Programmende | frei | frei, z. B. Ladepuffer; mit `$3E` lesbar, aber nicht `$D000–$DFFF` (I/O) und `$FF00–$FF04` |

Bank 1: unverändert (BASIC-Variablen ab `$0400`).

**Gemeinsames RAM:** Die Engine plant 8 KB common RAM unten (`$0000–$1FFF`, Plan 3). Dann ist Bank 1 `$1C00–$1FFF` derselbe Speicher wie VDC-BASIC. Folge für P6: Variablen bzw. Daten der Engine in Bank 1 dürfen dort nicht liegen (BASIC-Variablenanfang in Bank 1 auf ≥ `$2000` setzen). Für VDC-BASIC selbst ändert sich nichts.

**Programme werden 9 KB „höher“:** Ein Programm, das bisher bei `$1C01` begann, endet jetzt `$2400` Byte weiter oben. Feste Ladepuffer (Crawl: `BE` ab `$94E8`) müssen über dem neuen Programmende liegen → offener Punkt aus Plan 3, gehört zu P6/P7, hier nur vermerkt.

## 5. Blitz!-Kompilat mit Image 2

### 5.1 Was vorhanden ist

- `~/git/blitz-compiler/blitz128/blitzc/js/cli.js`: Blitz!-128-Compiler als Node-Programm, headless, byte-identisch zum Original (51 Referenzläufe, auch die `$4001`-Variante). Standard: verbesserte Runtime V261123 (enthält die Korrektur des 10 Byte kurzen Image 2).
- Node liegt nicht im `PATH` nicht-interaktiver Shells: `~/.nvm/versions/node/v26.13.1/bin/node`. Die Skripte suchen `$NODE`, sonst `node` im `PATH`, sonst `~/.nvm/versions/node/*/bin/node` (neueste).
- Image 2 wird gewählt, sobald das Programm einen `GRAPHIC`-Befehl enthält (Handbuch, Direktive `**NG`). Es gibt keine Option dafür. Beispiel im Repo: `blitzc/tests/src/ri2.bas`.
- `::` vor einer Anweisung = Extension: der Text bis zum nächsten `:` wird zur Laufzeit dem Interpreter übergeben, der über `$0310` VDC-BASIC aufruft. Normale Variablen werden an Extensions übergeben (`**FI`-Variablen nicht).
- Das Kompilat `C/NAME` lädt bei `$4001`. Sein Lader prüft `$2E`: steht dort nicht `$40`, kopiert er das Programm nach `$4000` – nach einem Laden bei `$1C01` wäre VDC-BASIC dann schon überschrieben. Deshalb lädt immer der Boot-Lader (Zeile 50: `RUN"C/NAME"` bzw. unter beliebigem Dateinamen).

Damit ist der ganze Weg automatisierbar: Kompilieren mit Node, Ausführen in `x128` wie die interpretierten Tests, Ergebnis über den Exitcode der Debug-Cartridge. Ein manueller Minimalweg ist nicht nötig.

### 5.2 Vorab-Experiment E1 (vor allen anderen Änderungen, Ergebnis in diese Datei, Abschnitt 9)

Mit v2h-Code, nur nach `$1C00` assembliert, je ein kleines Testprogramm, interpretiert und kompiliert:

| Frage | Prüfung |
|---|---|
| E1a: Berührt die Image-2-Runtime `$1C00–$3FFF` oder `$1300–$1BFF`? | Monitor: `watch store 1300 3fff` **nach** der Installation (Haltepunkt am Ende von `install` aktiviert die Watchpoints); jeder Treffer mit PC außerhalb `$1C00–$3FFF` ist ein Befund (Treffer mit PC im VDC-BASIC-Code sind dessen eigene Variablen). |
| E1b: Kommt eine Zuweisung in einer Extension zurück? | `10 A=1:::A=VMD(2000)+5:IF A=…` bzw. eine einfachere Form ohne Funktion: `::A=7` und danach `IF A<>7`. |
| E1c: Funktionen `VMD`/`RGD` in kompiliertem Ausdruck | Erwartung: Compilerfehler; Meldung festhalten. |
| E1d: `TRAP`/`ER` bei einem Fehler **innerhalb** einer Extension (`::VMW 70000,1`) | Wird `TRAP 9000` angesprungen, steht `ER` = 14? |
| E1e: Reicht ein nie ausgeführtes `GRAPHIC 0` in der letzten Zeile für Image 2? | Ladeadresse von `c.prg` = `$4001`. |

Die Antworten entscheiden über 5.3 (Variante A oder B).

### 5.3 Vorverarbeitung `test/blitzprep.py`

`blitzprep.py in.bas out.bas` macht aus einem Testlisting eine Blitz!-taugliche Fassung:

1. Jede Anweisung, die mit einem VDC-BASIC-Befehlswort beginnt (Liste aus `BASIC V7.0 VDC.txt`, Abschnitt „VDC Basic Tokens“, nur `$FE`-Tokens), bekommt `::` davor. Anweisungsgrenzen: `:` außerhalb von Strings, Zeilenanfang. Steht ein VDC-BASIC-Befehl direkt nach `THEN` oder `ELSE`, bricht das Werkzeug mit Zeilennummer ab (Exitcode 2): solche Zeilen werden im Test umgeschrieben (`IF … THEN 500` mit Unterprogramm), weil eine Extension „bis zum nächsten `:`“ reicht und die Bedeutung im `IF` nicht geprüft ist.
2. Funktionsaufrufe `VMD(…)`/`RGD(…)`:
   - **Variante A** (E1b: Zuweisung kommt zurück): jeder Aufruf wird vor die Anweisung gezogen: `IF VMD(2100)<>17 OR VMD(2101)<>34 THEN …` → `::Q0=VMD(2100):::Q1=VMD(2101):IF Q0<>17 OR Q1<>34 THEN …` (Hilfsvariablen `Q0`…`Q9`, im Test sonst nicht benutzt; geschachtelte Aufrufe von innen nach außen). Nur zulässig, wenn die Anweisung kein `THEN`-Teil mit weiteren Aufrufen enthält, sonst Abbruch wie oben.
   - **Variante B** (E1b negativ): Funktionen bleiben verboten; betroffene Tests lesen das VRAM per `::VTR adr,ram,n` in RAM 0 (`$1300+`) und prüfen mit `BANK 0:PEEK`. Die Tests werden dafür einmal von Hand umgeschrieben, so dass sie in beiden Modi laufen; `blitzprep.py` meldet verbliebene Funktionsaufrufe als Fehler.
3. Am Ende `63999 GRAPHIC 0` (nie ausgeführt, erzwingt Image 2), wenn E1e das bestätigt; sonst eine Form, die E1 als tauglich ermittelt.
4. `REM **NE` als erste Zeile (unterdrückt die Extension-Liste).

### 5.4 Ablauf im Testlauf (`test/run.sh -c`)

```
python3 test/blitzprep.py t.bas tb.bas
python3 test/bastok.py tb.bas tb.prg                     # Quelle für den Compiler, Start $1C01 wie alle Referenzen von blitzc
$NODE $BLITZC tb.prg $WORK/blitz --name T                # -> $WORK/blitz/c.prg, z.prg, screen.txt
# Prüfen: Ladeadresse von c.prg = $4001 (sonst FAIL "not image 2"), screen.txt ohne Fehlermeldung
c1541 … -write $WORK/blitz/c.prg t                       # als "T" auf die Disk, BOOT lädt RUN"T"
```

`BLITZC` aus der Umgebung, Standard `$HOME/git/blitz-compiler/blitz128/blitzc/js/cli.js`. Fehlen Node oder `cli.js`: Meldung `SKIP (blitzc not found)` und Exitcode 3 (nicht 0), damit ein ausgelassener Kompilatlauf nicht als grün gilt.

## 6. Tests und Harness

### 6.1 `test/run.sh`

| Punkt | neu |
|---|---|
| Binärdatei | `BIN=${VDCBASIC:-$HERE/../vdcbasic3.bin}`, auf der Disk als `vdcbasic3` |
| Disk | `boot` (aus `boot/boot.bas`, Zeile 50 durch `50 RUN"T"` ersetzt) als erste Datei, `vdcbasic3`, `t` (Testprogramm, tokenisiert mit `--start 0x4001`, bzw. Kompilat), bei `-l` die Daten der Listings |
| Start | `-autostart boot.prg` statt `t.prg` |
| neue Option | `-c`: Kompilatlauf (5.4); `-l` und `-c` schließen sich aus (die Listings werden nur interpretiert geprüft) |
| Rest | unverändert: Debug-Cartridge, `-limitcycles` als Wächter, Wiederholung bei 143, Screenshot |

`run.sh` braucht keinen Monitor; die Portregel (6.4) gilt für die Skripte mit Monitor (`boot-sector`, E1, P2b, P2c).

### 6.2 Testlistings (`test/*.bas`)

Zeile 2 (`bload"vdcbasic2h",b0,u8:sys dec("1300")`) entfällt in allen Tests; sie wird ersetzt durch

```
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
```

(prüft, dass `BOOT` v3 installiert hat). Adressen, die in v3 belegt sind:

| Test | heute | neu | Grund |
|---|---|---|---|
| `vms-list-at-3000.bas` | Liste bei `$3000` | umbenennen in `vms-list-at-1300.bas`, Liste bei `$1300` | `$3000` ist VDC-BASIC; der Test soll weiter eine Liste **unter** `$4000` prüfen |
| `vcl-long-list.bas`, `vcl-vmw-increment0.bas`, `vms-type2-stale.bas` | `$3000` | `$1300` (Listen > 256 Byte passen: Platz bis `$1BFF`) | dito |
| `vmb-zero-length.bas` | `$3000` | `$1300` | dito |
| `vms-list-at-5000.bas` | `$5000` | umbenennen in `vms-list-at-a000.bas`, `$A000` | `$5000` kann im Programmtext liegen (ab `$4001`); der Test soll eine Liste **über** `$4000` prüfen |
| `reference.bas`, `vcl-syn.bas`, `vcl-syn-level.bas`, `vcl-unknown.bas` | `$5000` | `$A000` | dito |
| `reference-examples.bas` | `$4000` | `$A000` | `$4000` = Byte vor dem Programmtext |
| alle | – | `$A000` gilt auch für Kompilate: Runtime + Bytecode der Tests enden unter `$7000` | |

Ein Test **`layout.bas`** kommt neu dazu: prüft `PEEK(45)=1`, `PEEK(46)=64`, `PEEK(16384)=0`, Kopf `$1C03 = $30`, `$1C04` = Standard-T, und dass die vier BASIC-Vektoren auf Adressen in `$1C00–$3FFF` zeigen.

### 6.3 Listings des Guides (`docs/listings/*.bas`)

- Zeile 20 `BLOAD "VDCBASIC2H",B0:SYS DEC("1300")` → `20 IF PEEK(46)<>64 THEN PRINT "RUN"CHR$(34)"BOOT"CHR$(34)" FIRST":END` (der Guide erklärt in Kapitel 1 den Boot-Lader; Laden und Installieren übernimmt `BOOT`).
- Puffer `BF=DEC("4000")` → `BF=DEC("8000")`; `11-walk.bas` `A=DEC("5000")` → `A=DEC("A000")`. Größte Datei: `dungeon` 8004 Byte → `$8000–$9F44`, kein Überlapp mit `$A000`.
- `02-glyph.bas` (`G=DEC("2000")`) ist eine VRAM-Adresse → unverändert.
- Prosa: Kapitel 1 (Laden, `$1300`, „a little more than 2 KiB“), Kapitel 6 (BASIC ab `$1C01`, Puffer `$4000`), Referenz „Loading“ und „Memory used by VDC-BASIC“, README (Version, Ladeadresse, Größe, Abschnitt „Compiling programs“ mit Boot-Lader und Image 2, Abschnitt „Building“), `docs/OUTLINE.md`, `test/README.md`.
- Die Screenshots `docs/img/*.png` müssen nach dem Lauf mit `-d docs/img` **unverändert** sein (`git status` sauber). Abweichungen (z. B. sichtbare Installationsmeldung) einzeln begründen oder beheben.

### 6.4 Monitor-Skripte (Portregel)

Gemeinsames Modul `test/vicemon.py` (für E1, `boot-sector`, P2b, P2c):

- Port aus `--port`, sonst `DBE_VICE_PORT`, sonst 6631; Bereich 6631–6649; vorher bind-Test auf `127.0.0.1`; belegt → nächster freier mit Meldung, bei ausdrücklichem `--port` Abbruch (Exitcode 2).
- Start mit `-remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>`, kein `-binarymonitor`.
- Nur das eigene `Popen` beenden (`terminate`, nach Timeout `kill`, im `finally`); kein `pkill`/`killall`.
- Ereignisgesteuert: Haltepunkte auf Labels aus der ACME-Symbolliste (`acme -l build/vdcbasic3.sym`) bzw. auf feste Adressen, BASIC-Fehlervektor `($0300)`; keine festen Wartezeiten; Timeout nur als Wächter. Verbindungsaufbau mit Wiederholung, bis der Port offen ist (wie `dungeonboringengine/test/vice_drive.py`).

### 6.5 Testliste P2a

| Test | Art | Erwartung |
|---|---|---|
| `test/run.sh test/*.bas` | interpretiert | alle PASS (inkl. `layout.bas`) |
| `test/run.sh -c test/*.bas` | Blitz! Image 2 | alle PASS; Ausnahmen nur mit Begründung in `test/README.md` (z. B. Tests, die Interpreterfehler prüfen, falls E1d negativ) |
| `test/run.sh -l -d docs/img docs/listings/*.bas` | Listings | alle PASS, Screenshots unverändert |
| `test/boot_sector.py` | Monitor | `BOOT` startet über den Bootsektor ohne `-autostart`, Haltepunkt `$1C00` erreicht, danach Exitcode der Debug-Cartridge aus einem `RUN"T"` = 0 |
| `test/e1_blitz.py` | Monitor | E1a: keine Schreibzugriffe der Runtime auf `$1300–$3FFF`; Ergebnisse E1b–E1e protokolliert |
| Build | ACME | `vdcbasic3.bin` beginnt mit `00 1C 4C`, Kopffelder an den festen Adressen, Ende ≤ `$3FFF` |

## 7. Umfang und Reihenfolge der Arbeit

1. E1 (5.2) mit einem nach `$1C00` assemblierten v2h-Stand, Befunde in Abschnitt 9 eintragen.
2. Quelltext (2), Kopf, Gesamtprüfung; `vdcbasic3.bin` bauen.
3. `boot/boot.bas`, `bastok.py --start`, `run.sh` (Boot, `-c`), `blitzprep.py`.
4. Tests umstellen (6.2), `layout.bas`.
5. Listings und Doku (6.3).
6. `tools/mkboot.py`, `test/boot_sector.py`.

## 8. Abnahme

| Prüfung | Erwartung |
|---|---|
| Build | fehlerfrei; Ende von VDC-BASIC ≤ `$3FFF` (Assertion), Kopf an `$1C00–$1C0F` |
| Verhalten | keine Änderung gegenüber v2h außer Adresse und Meldung (`git diff` in `src/` nur 2, 2.1) |
| interpretiert | `test/run.sh test/*.bas` alle PASS |
| kompiliert | `test/run.sh -c test/*.bas` alle PASS (oder dokumentierte Ausnahme), Kompilate haben Ladeadresse `$4001` |
| Listings | `test/run.sh -l -d docs/img docs/listings/*.bas` alle PASS, `docs/img` unverändert |
| Boot | `boot_sector.py` grün; `BOOT` auch per `-autostart disk.d64` |
| Blitz-Runtime | E1a: keine fremden Schreibzugriffe auf `$1300–$3FFF` |
| Tests | ohne `sleep`; Monitor-Skripte halten die Portregel ein |
| Doku | README, Referenz, Guide-Kapitel 1/6, Listings, `test/README.md` angepasst |

## 9. Befunde E1

Durchgeführt am 2026-11-25 mit `test/e1_blitz.py` (VICE 3.10 `x128`, blitzc `cli.js`, Improved runtime V261123), VDC-BASIC v2h nur nach `$1C00` assembliert (`build/e1/`), geladen über den Boot-Lader (`boot.bas` ohne Zeile 40, Zeile 50 `RUN"T"`). Jedes Programm lief interpretiert und als Kompilat; Ergebnis über die Debug-Cartridge.

| Frage | Ergebnis |
|---|---|
| E1a | **Keine** Schreibzugriffe auf `$1300–$3FFF` mit PC außerhalb des VDC-BASIC-Codes, weder interpretiert noch mit Image 2 (Programme `e1b-*`, `e1d-trap`). Gegenprobe `e1a-control` (`BANK 0:POKE DEC("1300"),1`): Treffer interpretiert (PC `$02B8`, `k_stash`) und kompiliert (PC `$57BE`, Runtime) – der Watchpoint wirkt also. Hinweis VICE 3.10: die Bedingung muss geklammert sein, `watch store 1300 3fff if (pc < $1c00) \|\| (pc > $xxxx)`; ohne Klammern löst er nie aus. |
| E1b | **Ja**: `::A=7` und danach `IF A<>7` – A ist 7. Auch `::A=VMD(2000)+5` (nach `::VMW 2000,42`) liefert 47. → **Variante A** für `blitzprep.py`. |
| E1c | Compilerfehler `?SYNTAX ERROR IN 20` für `A=VMD(2000)` außerhalb einer Extension (wie erwartet). |
| E1d | **Ja**: Fehler in `::VMW 70000,1` springt `TRAP 9000` an, `ER` = 14. |
| E1e | **Ja**: ein nie ausgeführtes `63999 GRAPHIC 0` ergibt Ladeadresse `$4001` (Image 2). Ohne `GRAPHIC` entsteht Image 1 (Ladeadresse `$1C01`); dessen Lader kopiert das Programm nach `$1C00` und überschreibt VDC-BASIC (Watchpoint: Schreibzugriffe `$1C00…` von PC `$4048`). Image 1 ist mit v3 also unbrauchbar. |

Folgen: 5.3 Variante A, Schritt 3 wie beschrieben (`63999 GRAPHIC 0`).

## 10. Entscheidungen des Nutzers (2026-11-25)

1. `GRAPHIC 1`–`4` ist mit v3 tabu – akzeptiert; im README (Speicher/Laden) vermerkt.
2. Guide und Listings (`docs/listings`, Kapitel 1/6, Referenz, 6.3) werden erst in P8 umgestellt, nicht in P2a. Bis dahin prüft `test/run.sh -l` die Listings weiter mit v2h bei `$1300` (ohne Boot-Lader).
3. `vrl.py` wandert nach vdc-basic (`tools/vrl.py`, kanonische Kopie); P2a baut keine Abhängigkeit über `DBE_TOOLS` ein.
4. Für später (P2c/P5b): neuer VMS-Listenbefehl „Quelle n Byte weiter“ (source skip) für Sprite-Clipping am Viewport-Rand.

## 11. Umsetzungsnotizen und Abweichungen (2026-11-25)

Stand der Abnahme: `test/run.sh test/*.bas` und `test/run.sh -c test/*.bas` je 16/16 PASS (inkl. `layout.bas`), Kompilate mit Ladeadresse `$4001`; `test/run.sh -l -d docs/img docs/listings/*.bas` 23/23 PASS (v2h, siehe unten); `test/boot_sector.py` grün (Bootsektor und `-autostart disk.d64`); `test/e1_blitz.py --bin vdcbasic3.bin --sym build/vdcbasic3.sym` grün (gleiche Befunde wie mit dem E1-Build). `vdcbasic3.bin`: `00 1C 4C 10 1C 30 0E 00 …`, Code `$1C00–$2505` (2310 Byte, 6906 Byte Reserve bis `$3FFF`).

- **Listings (6.3) nicht umgestellt** (Entscheidung 2): `run.sh -l` lädt weiter `vdcbasic2h` bei `$1300` ohne `BOOT`. Zwei Screenshots (`03-fill`, `12-list`) wichen in einem Lauf ab; beide zeigen gemessene Jiffy-Zahlen (Timing-Jitter, v2h-Pfad unverändert) und wurden aus `HEAD` zurückgeschrieben.
- **Zusätzlich verlegte Tests** (in 6.2 nicht aufgeführt): `vcl-with-vms.bas` (`$5000`/`$6000` → `$A000`/`$B000`, auch die Listenadresse in den `DATA`-Bytes), `vms-long-list.bas` (`$5000` → `$A000`). `$6000` läge im Kompilat mitten in der Runtime.
- **`chapter-claims.bas`**: Die Prüfung „Schlüsselwort im Variablennamen“ (`SYNC=1` mit `TRAP`) steht jetzt am Ende (Zeilen 500–540). Befund: Nach einem per `TRAP` abgefangenen Fehler **innerhalb einer Extension** funktionieren weitere Extensions im Kompilat nicht mehr (`::B=5` kommt nicht zurück, `::VMW` meldet `?ILLEGAL QUANTITY` bzw. hängt) – reproduziert ohne VDC-BASIC-Befehl (`::A=1/0`), also eine Eigenschaft der Blitz!-Runtime; `RESUME` hilft nicht. E1d gilt damit nur für „Fehler, dann Ende/Abbruch“. Dokumentiert in README und `test/README.md`.
- **`blitzprep.py`**: Ein vom Listing selbst geschriebenes `::` (z. B. `IF R=9 THEN::VMF …`, Kapitel-13-Aussage in `chapter-claims.bas`) bleibt stehen und gilt nicht als Abbruchfall. Funktionsaufrufe in Anweisungen hinter dem `THEN`-Teil, die durch `:` getrennt sind, werden normal vorgezogen (sie gehören ohnehin zum bedingten Teil). Zeilen 0 und 63999 sind für das Werkzeug reserviert.
- **Bootsektor (3.3)**: Einsprung `$AFA5` bestätigt (springt nach `$4DCD`: TXTPTR = X/Y, Text tokenisieren, ausführen). Abweichung: X/Y müssen auf `cmd − 1` zeigen (CHRGET erhöht vor dem Lesen), `cmd` ist Klartext `RUN"BOOT"` + 0, nicht tokenisiert. `tools/mkboot.py --disk D64` prüft Sektor 1/0 und schreibt ihn.
- **VICE-Monitor**: Bedingungen mit `||` müssen geklammert sein (`if (pc < $1c00) || (pc > $2508)`). Beim Start ohne `-autostart` hält der Monitor vor der KERNAL-Initialisierung; `boot_sector.py` setzt den Fehler-Haltepunkt deshalb fest auf `$4D3F`.
- **Offen – P0-Messprogramm**: `test/bench/run_bench.py` baut `src/vdcbasic.asm` und verlangt Gleichheit mit `vdcbasic2h.bin`; seit P2a erzeugt der Quelltext v3, der Bench-Build bricht daher ab. `test/bench/` wurde hier nicht angefasst (P0 vom Koordinator committet); Vorschlag: Bench baut aus `git show <v2h-Commit>:src/vdcbasic.asm` oder nutzt direkt `vdcbasic2h.bin` plus eine eingecheckte Symbolliste.
- **Nicht in P2a**: `RXV`/`VDM`, Token-Stubs `VDL`/`VDR`/`VDB` (P2b), Tokenliste unverändert bis auf den Kommentar „vdcbasic v3“.
