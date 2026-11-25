# P2d: VCL-Typ 9 – VMS-Listen aus Bank 1

Stand: 2026-11-25. Anlass: Vorschlag der Engine (`dungeonboringengine/docs/specs/P6-engine.md`, Abschnitt 6a): Die Engine kopiert heute jede VMS-Liste aus ihren Sprite-Sätzen (Bank 1) in den Puffer in Bank 0, etwa 16 Zyklen je Byte, zusammen ~5–30k Zyklen je Bild. Ein VCL-Eintrag, der die Liste direkt in Bank 1 liest, spart diese Kopie für alle unbeschnittenen Listen. Voraussetzung: P2a (VDC-BASIC bei `$1C00–$3FFF`), P2b (VMS-Typ 3).

## 1. Eintrag

| Typ | Bytes | Parameter | Wirkung |
|---|---|---|---|
| 9 | 7 | source, target, list (je Wort, Low/High) | wie Typ 12 (`VMS source,target,list`), aber *list* ist eine Adresse in **RAM-Bank 1** |

- Listenformat unverändert ([`vms-format.md`](../reference/vms-format.md)): Alpha, Anzahl (Wort), Befehle 1, 2, 3, 8. Beliebige Länge, auch über Seitengrenzen.
- Lage der Liste: ganz in Bank 1 **`$4000–$CFFF`** oder **`$E000–$FEFF`**. Nicht geprüft (Kosten); außerhalb ist das Ergebnis undefiniert (unter `$4000` liest der Eintrag Bank 0, in `$D000–$DFFF` I/O, ab `$FF00` die MMU).
- Unbekannter VMS-Befehl: wie bei Typ 12 `INVALID VMS COMMAND`, die VCL-Liste läuft mit dem nächsten Eintrag weiter.
- Interrupts bleiben an.
- Nur über `VCL`, keine BASIC-Form (wie Typ 7).

## 2. Entscheidung: 16 KB gemeinsames RAM während des Eintrags

### 2.1 Das Problem des Vorschlags

Der Vorschlag liest die Liste mit `$FF00 = $7E`. Der Code von VDC-BASIC liegt aber in Bank 0 bei `$1C00–$2E8E`. Mit der Kernal-Vorgabe von `$D506` (1 KB unten gemeinsam, `$0000–$03FF`) holt die CPU nach `STA $FF00` den nächsten Befehl aus Bank 1 – dort liegen BASIC-Variablen: Absturz. Auch die 8 KB der Engine (`$D506` = …`$06`, nur während eines Engine-Aufrufs) decken `$2000–$2E8E` nicht.

### 2.2 Gewählter Weg

Für die Dauer eines Typ-9-Eintrags schaltet `VCL` das gemeinsame RAM auf **16 KB unten** (`$D506` = (alt AND `$F0`) OR `$07`). Dann ist Bank 0 `$0000–$3FFF` – also ganz VDC-BASIC mit Code, Variablen, Zeropage und Stack – auch in der Konfiguration `$7E` (RAM-Bank 1, I/O) sichtbar. Die VMS-Routine läuft unverändert mit `$FF00 = $7E` statt `$3E` und liest die Liste wie bisher mit `LDA (arg_address),Y`. Danach: `$FF00` zurück auf Bank 0, erst dann `$D506` zurück (umgekehrt würde der Code verschwinden).

Ablauf in `.vcl_check_vms` (Typ 12 und 9 gemeinsam):

```
push arg_address, offset_1
vcl_saved_rcr = $D506                  ; $FF00 = $3E
$FF00 = vcl_saved_cr; remember_mem_conf ; für k_primm im Fehlerpfad von VMS, danach $FF00 = $3E
A = $3E
if Typ 9: $D506 = (vcl_saved_rcr AND $F0) OR $07; A = $7E
jsr vms_execute_a                      ; A = Konfiguration zum Lesen der Liste
remember_mem_conf                      ; $FF00 = $3E (Bank 0, I/O)
$D506 = vcl_saved_rcr
pull
```

`vms_execute_a` (neuer Einsprung vor `vms_execute`, das jetzt nur `LDA #$3E` voranstellt) schreibt A nach `$FF00` und in `saved_mem_conf`. Grund: `vmc_execute` endet mit `$FF00 = saved_mem_conf`; die Befehle 1 und 2 riefen bisher je Befehl `remember_mem_conf` auf (setzt `$3E`), das hätte mitten in der Liste auf Bank 0 umgeschaltet. Die beiden Aufrufe entfallen, `saved_mem_conf` wird einmal je Liste gesetzt. Nebeneffekt: Typ 12 und `VMS` aus BASIC werden je Befehl 1/2 um 26 Zyklen schneller.

### 2.3 Warum das sicher ist

- **Code und Daten unter `$4000`**: VDC-BASIC endet bei `$2E8E`; die Gesamtprüfung am Dateiende (`!if * > $4000`, P2a) sichert das auch für Typ 9 (Kommentar dort ergänzt).
- **Zeropage und Stack** liegen ohnehin immer in Bank 0 (MMU-Vorgabe `$D507–$D50A`).
- **Interrupts**: Ein IRQ/NMI bei `$FF00 = $7E` nimmt den Vektor aus Bank 1 `$FFFA–$FFFF` und läuft über die Kernal-Interruptweiche in Bank 1 (`$FF05` ff.), die `$FF00 = 0` setzt. Die Weiche (`$FF05–$FF44`) und die Vektoren kopiert der Kernal beim Reset in RAM-Bank 1, unabhängig von der Engine (Review: aus reinem BASIC nach `BOOT` Byte für Byte gleich dem Kernal-ROM; Typ 9 mit 403-Byte-Listen bei `$9000` und `$F000`, 300 Durchläufe mit `MOVSPR` und `PLAY` im IRQ, grün). Eine Liste darf diese Bytes deshalb nicht überschreiben (Grenze `$FEFF`). Dieselbe Weiche benutzt schon der Kernal-`FETCH` (`$02A2`, ohne `SEI`) und die Engine während ihrer Aufrufe. Der Kernal-/BASIC-IRQ arbeitet in Bank 0; dort ändert das gemeinsame RAM nichts. Der Test mit der 403-Byte-Liste dauert ~31 000 Zyklen, also fällt mindestens ein IRQ in das Fenster; er läuft grün.
- **Bank 1 `$0000–$3FFF`** (BASIC-Variablen ab `$0400`) ist während des Eintrags verdeckt; in dieser Zeit liest sie niemand (BASIC steht in `VCL`, die IRQ-Routinen greifen nicht auf Bank 1 zu). Dieselbe Annahme trifft die Engine mit 8 KB.
- **Fehlerpfad**: `INVALID VMS COMMAND` druckt mit `k_primm` in der Konfiguration des Aufrufers (Bank 0, Kernal); das gemeinsame RAM stört dort nicht. Danach stellt `VCL` `$D506` wie sonst zurück (Test `vcl-vms-bank1-invalid.bas`).
- `$D506` Bits 4–7 (VIC-Bank u. a.) bleiben unverändert.

### 2.4 Verworfene Wege (Zyklen)

Heute: Engine kopiert ~16 Zyklen je Listenbyte (Bank 1 → Bank 0), VDC-BASIC liest danach mit `LDA (zp),Y` (5 Zyklen).

| Weg | Mehrkosten je Listenbyte gegenüber Typ 12 | gegenüber heute (Kopie + Typ 12) | Befund |
|---|---|---|---|
| Kernal `FETCH` `$02A2` (wie `VMP`) | `LDX #$7F` 2 + `JSR` 6 + Rumpf 4+4+2+5+4 + `RTS` 6 = 33 statt 5 → **+28** | +12 je Byte | langsamer als heute, verworfen |
| eigener Lese-Stub im immer gemeinsamen RAM | wie `FETCH` ohne `LDX`, ≈ +25 | +9 | kein Platz: `$0000–$03FF` ist belegt (Blitz! `$0202–$02A1`, Kernal `$02A2–$03FF`); `$0B00`/`$0E00` sind nur bei 8 KB gemeinsam, das gilt nur während Engine-Aufrufen |
| blockweise in einen Bank-0-Puffer kopieren (aus gemeinsamem Code) | ≈ +13–16 (Kopierschleife) | ±0 | nur eine Verlagerung der heutigen Kopie, dazu Puffer und Code; verworfen |
| **16 KB gemeinsam** (gewählt) | **0** (gleiche Befehle) | **−16** | einmalig +15 Zyklen je Eintrag |

## 3. Speicher

| Name | Lage | Inhalt |
|---|---|---|
| `vcl_saved_rcr` | Datenteil (`$2E10`) | `$D506` des Aufrufers während eines Typ-9/12-Eintrags |
| `vcl_parameter_bytes[9]` | Datenteil | 5 (6 Parameterbytes, wie Typ 12) |

Codegröße: `vdcbasic3.bin` 4711 → 4753 Byte (+42: Code +41, Daten +1), Ende `$2E8E` (Z64K-Hilfe `zvt` bei `$2F00` bleibt frei).

## 4. Zyklen (VICE, vorläufig)

Gemessen mit der Stoppuhr des VICE-Monitors, IRQ während der Messung aus, von der Typprüfung in `.vcl_check_vms` bis zum Ende der Wiederherstellung (also inklusive VMS). Listen: n Befehle Typ 1 mit Länge 1 (4 Byte je Befehl), `x128 -80col -VDC64KB`, 1 MHz.

| Fall | n = 0 | n = 100 (403 Byte) | je Befehl | je Listenbyte |
|---|---|---|---|---|
| Typ 9 (Bank 1), neu | 392 | 30 634 | 302 | 75,6 |
| Typ 12 (Bank 0), neu | 336–379 | 30 573–30 966 | 302–306 | 75,5–76,6 |
| Typ 12, vor P2d | 276–319 | 33 430–33 651 | 332 | 83,1 |

- Je Listenbyte sind Typ 9 und Typ 12 gleich: es sind dieselben Befehle. Die Streuung (einige hundert Zyklen je Lauf) kommt von den Wartezeiten auf den VDC (Block copy, Registerzugriffe), nicht vom Listenlesen.
- Fester Mehraufwand Typ 9 gegenüber Typ 12: nachgezählt 15 Zyklen (`LDA`/`AND`/`ORA`/`STA $D506`, `LDA #$7E`, ein nicht genommener Sprung). Beide Typen zahlen jetzt je Eintrag ≈ 42 Zyklen für das Merken/Zurückschreiben von `$D506` und ein zusätzliches `remember_mem_conf`; dafür entfallen 26 Zyklen je VMS-Befehl 1/2.
- Ersparnis für die Engine: die Kopie (~16 Zyklen je Listenbyte, ~5–30k je Bild) entfällt für jede Liste, die verbatim ausgegeben wird; dazu weniger Pufferplatz (VMSLEN).

## 5. Tests

| Datei | Prüft |
|---|---|
| `test/vcl-vms-bank1.bas` | In einer VCL-Liste Typ 9 und Typ 12 abwechselnd, jeweils dieselbe Liste in Bank 1 bzw. Bank 0. Liste A (Befehle 1, 3, 8, Quell-Skip 256) mit Kopf bei `$80FC` (Seitengrenze mitten im ersten Befehl); Liste B: 100 Befehle Typ 1, 403 Byte (> 256). In Bank 0 liegen an den Bank-1-Adressen **andere** gültige Listen (Köder): liest Typ 9 versehentlich Bank 0, ändert sich das Ergebnis. Prüfung: feste Sollwerte, Typ-9- gegen Typ-12-Ergebnis Byte für Byte, `$D506` danach unverändert. |
| `test/vcl-vms-bank1-invalid.bas` | Typ 9 mit unbekanntem VMS-Befehl in Bank 1: Meldung, `$D506` unverändert, der folgende VCL-Eintrag (Typ 6) läuft. |

Ergebnis (2026-11-25): `test/run.sh test/*.bas` 47/47 grün, `test/run.sh -c test/*.bas` 47/47 grün; Z64K siehe 6. Der alte Stand (vor P2d) fällt in `vcl-vms-bank1.bas` mit Code 10 durch (Typ 9 = `INVALID`).

## 6. Z64K

`test/run.sh -z test/vcl-*.bas test/vms-*.bas` (interpretiert): 17/17 grün. `test/run.sh -z -c` mit `vcl-vms-bank1.bas`, `vcl-vms-bank1-invalid.bas`, `vcl-with-vms.bas`, `vcl-vms-source-skip.bas`, `vms-long-list.bas`: 5/5 grün. Z64K emuliert MMU und `$D506` unabhängig von VICE; beide stimmen überein.

## 7. Was die Engine anpassen muss

- Für jede **unbeschnittene** VMS-Liste einen Eintrag Typ 9 (`9, src, dst, list`) mit der Adresse der Liste im Sprite-Satz (Bank 1) schreiben statt der Kopie in den Puffer und Typ 12. Beschnittene Listen (`clip_image`, P5b 6.2) baut die Engine weiter in den Puffer und gibt sie als Typ 12 aus.
- Lage: Liste ganz in Bank 1 `$4000–$CFFF` oder `$E000–$FEFF` – **nicht `$2000`** wie im Vorschlag (unter `$4000` ist während des Eintrags Bank 0 sichtbar). Die Vorgabe `$7000–$8FFF` für S2 passt. Die Prüfung beim Setzen der Sprite-Sätze (`SPRITESET`) entsprechend.
- Die Liste muss im Sprite-Satz so stehen, wie VDC-BASIC sie liest: Alpha, Anzahl, Befehle, zusammenhängend.
- `VCL` selbst bleibt in Bank 0 (Puffer). Nichts am Stub ändern: Typ 9 setzt `$D506` selbst und stellt den Wert des Aufrufers wieder her.
- Für die Größenrechnung (P6 5.10): Typ 9 kostet 7 Byte VCL statt 7 Byte VCL + Listenlänge im Puffer.

## 8. Ergebnis (2026-11-25)

Umgesetzt in `src/vdcbasic.asm` (`.vcl_check_vms`, `vms_execute_a`, `vcl_saved_rcr`). Größe +42 Byte, Ende `$2E8E`. VICE: `run.sh` 47/47 interpretiert und kompiliert; Z64K: 17/17 interpretiert, 5/5 kompiliert (Auswahl). Zyklen je Listenbyte Typ 9 = Typ 12 (≈ 76 bei Befehlen Typ 1 der Länge 1, VDC-dominiert); Typ 9 spart der Engine die Kopie (~16 Zyklen je Byte).
