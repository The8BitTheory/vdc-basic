# Zurückgestellt: VMS-Befehl 1 schneller machen

Stand: 2026-11-26. **Status: zurückgestellt.** Derzeit gibt es keinen Aufrufer, für den es sich lohnt: Crawl 1.1 benutzt im Rundgang keinen VMS-Befehl 1 (nur VMC Typ 2 und VMF Typ 5). Wieder aufnehmen, wenn ein Spiel mit vielen transparenten Wandrändern, Gittern oder Sprites (Engine 0.2, `byte_edges` aus) auf die VMS-Zeit kommt.

## Befund

VMS-Befehl 1 kopiert einen Lauf von wenigen Bytes (die deckenden Stücke einer Sprite-Zeile). Gemessen (P6g, `dungeonboringengine/docs/specs/P6g-vcl-analysis.md`, vor P2f): **≈ 308 Zyklen je Befehl** (Bench v2h: 318). Die Bytes selbst kosten fast nichts; teuer sind die festen Kosten je Befehl. Seit P2f läuft Befehl 1 ebenfalls über die neue Zeilenschleife (`block_copy_target`); ein Wert nach P2f ist **nicht gemessen** – vor einer Optimierung einmal messen (nur die neue Fassung, Vorher-Wert = dieser Stand).

Wo die Zyklen hingehen (`src/vdcbasic.asm`, `.vms_arg_loop` … `.vms_check_more`):

1. **Liste lesen und verteilen (≈ 100 Zyklen):**
   - Vor jedem Befehl wird der Listenzeiger mit 16 Bit neu berechnet (`offset_1` auf `arg_address` addieren, `offset_1 = 0`), statt mit Y weiterzulesen.
   - Verteiler als Vergleichskette (`cmp #1 / #2 / #3 / #8`).
   - Ziel-Offset mit 16 Bit zur Zieladresse addieren.
   - Quelladresse in Software mitführen (`vms_block_source`), obwohl der VDC sie bei Befehl 1 selbst weiterzählt; gebraucht wird sie nur für Befehl 3 (source skip) und 8 (halbes Byte).
2. **Allgemeiner VMC-Weg für genau eine Zeile:** Befehl 1 setzt Länge und Zeilenzahl 1 (`arg3`, `arg4`) und springt in `block_copy_target` – mit den Entscheidungen für mehrere Zeilen (Quellschritt, selbstmodifizierte Sprünge, Seitenweg ab 256 Byte), obwohl es immer eine kurze Zeile ist.
3. **Zwei VDC-Waits je Befehl:** auf das Vorlesen nach R19 (vor R30) und auf das Ende der vorigen Kopie. Bei Läufen von 1–6 Byte wartet die CPU das bei jedem Befehl ab; im Chunky-Modus hat der VDC während der Anzeige nur ≈ 16 freie Speicherzyklen je Zeichenzeile (P2g), die Kopie dauert dann bis ≈ 127 µs.
4. **Schleifenende:** 16-Bit-Befehlszähler herunterzählen, Sprung zurück.

## Mögliche Maßnahmen

- **Eigene enge Schleife für Befehl 1:** Listenzeiger in Y führen (nur beim Überlauf nachziehen), Befehl 1 als ersten Fall prüfen, R18/R19/R30 direkt schreiben (ohne `block_copy_target`), Quellverfolgung nur bei Befehl 3 und 8. Waits nur dort, wo P2g sie als nötig zeigt (vor dem ersten Zugriff nach R30 und vor R30).
- **Kompakteres Listenformat (größerer Eingriff):** Ziel-Offset als 8-Bit-Abstand zum vorigen Lauf statt als 16-Bit-Absolutwert. Spart Lesen und Addition, ist aber eine Formatänderung (`docs/reference/vms-format.md`); Engine (`clipvms_e`, Referenz `dbeview`) und Konverter (`dbetex`, `dbespr`) müssten mitziehen.
- **Schätzung:** etwa die Hälfte der heutigen Kosten je Befehl. Unbelegt, erst nach Messung.

## Verwandt

- `docs/v3/P2f-vmc-loop.md` (Zeilenschleife VMC/VMF), `docs/v3/P2g-wait-experiment.md` (welche Waits nötig sind, schneller Weg `R-ACt`, Reg 36).
- VMS-Befehl 8 (halbes Byte, ≈ 706 Zyklen, P6g) ist der zweite teure VMS-Pfad (Wandränder bei pixelgenauen Kanten); gleiche Fixkosten plus zwei VRAM-Lesezugriffe und ein Schreibzugriff.
