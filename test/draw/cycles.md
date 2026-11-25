# VDM/VDL/VDR/VDB: Zyklen in VICE (vorläufig)

Erzeugt von `test/draw/run_draw.py --cycles F`. Stoppuhr des VICE-Monitors von `draw_begin` (nach Parsen
und Prüfen) bis `draw_end`, IRQ für die Messung gesperrt (I-Flag über den Monitor), je Fall das Minimum
aus 8 Läufen. 1 MHz, Treiber interpretiert. Die Werte sind **CPU-Zyklen**.

**Vorläufig:** VICE modelliert den VDC-Zugriff vereinfacht (Busy-Zeit je Reg-31-Zugriff im sichtbaren
Bereich, 43 Takte statt 4 im Rand); Werte auf echter Hardware stehen aus. Ziele aus docs/v3/P2c-draw.md,
Abschnitt 7; Abweichungen sind dort in Abschnitt 12 begründet.

## ohne sichtbare Zeilen (R6 = 0): CPU-Pfad

| Pfad | Zyklen | Ziel (7) | Herleitung |
|---|---|---|---|
| Punkt Bitmap (`VDL` x,y,x,y; draw_addr + pix_rmw + Ablauf) | 881.0 | — | pt-m0, y = 10 |
| Punkt chunky | 819.0 | — | pt-m1 |
| Punkt bei y = 200 (längere Multiplikation) | 998.0 | — | pt-y200 |
| senkrechter Lauf je Pixel | 144.3 | ≤ 140 | (v101 − v1) / 100 |
| schräge Linie 45° je Pixel | 196.4 | ≤ 160 | (d101 − d1) / 100 |
| `VDB` mit einer Zeile, zwei Ränder, m = 0, Bitmap (ganzer Befehl) | 1054.0 | — | VDB 3,y,10,1 |
| Spanne mit zwei Rändern, m = 0, Bitmap, je Zeile | 375.3 | ≤ 330 | (h11 − h1) / 10, VDB 3,y,10,h |
| Spanne mit zwei Rändern, m = 0, chunky, je Zeile | 446.3 | ≤ 330 | (h11 − h1) / 10, VDB 1,y,4,h |
| `VDB` volle Bytes je Zeile, m = 10 < T (CPU) | 386.0 | ≤ 60 + 150 | (h11 − h1) / 10, VDB 0,y,20,h chunky |
| Mittelteil unter T je Byte (chunky) | 15.0 | 15 | (m12 − m2) / 10 |
| Mittelteil m = 14 (Block fill, T = 14) | 994.0 | — | VDB 0,y,28,1 |
| Mittelteil m = 70 (Block fill) | 994.0 | — | VDB 0,y,140,1 |
| `VDB` volle Bytes je Zeile (70 Byte, Block fill) | 309.5 | ≤ 60 + Fill | (b70x96 − b70x1) / 95 |
| `VDB 0,0,140,96,c` chunky gesamt | 30257.0 | ≤ 96 × (60 + Fill) | b70x96 |
| XOR-Mittelteil je Byte (Bitmap, c = 2) | 49.0 | — | (m9 − m1) / 8 |
| `VDR 3,y,10,10,1` gesamt | 4255.0 | — | r10x10 |
| flache Linie dy = 1, dx = 300 (2 Spannen), je Pixel Fehlerterm | 74.0 | ≈ 15 | (l300x1 − l300x0) / 300, Näherung |

## normale Anzeige (R6 = 25)

| Pfad | Zyklen | Ziel (7) | Herleitung |
|---|---|---|---|
| Punkt Bitmap (`VDL` x,y,x,y; draw_addr + pix_rmw + Ablauf) | 924.0 | — | pt-m0, y = 10 |
| Punkt chunky | 819.0 | — | pt-m1 |
| Punkt bei y = 200 (längere Multiplikation) | 998.0 | — | pt-y200 |
| senkrechter Lauf je Pixel | 195.2 | ≤ 140 | (v101 − v1) / 100 |
| schräge Linie 45° je Pixel | 233.3 | ≤ 160 | (d101 − d1) / 100 |
| `VDB` mit einer Zeile, zwei Ränder, m = 0, Bitmap (ganzer Befehl) | 1054.0 | — | VDB 3,y,10,1 |
| Spanne mit zwei Rändern, m = 0, Bitmap, je Zeile | 416.4 | ≤ 330 | (h11 − h1) / 10, VDB 3,y,10,h |
| Spanne mit zwei Rändern, m = 0, chunky, je Zeile | 441.7 | ≤ 330 | (h11 − h1) / 10, VDB 1,y,4,h |
| `VDB` volle Bytes je Zeile, m = 10 < T (CPU) | 381.7 | ≤ 60 + 150 | (h11 − h1) / 10, VDB 0,y,20,h chunky |
| Mittelteil unter T je Byte (chunky) | 15.0 | 15 | (m12 − m2) / 10 |
| Mittelteil m = 14 (Block fill, T = 14) | 994.0 | — | VDB 0,y,28,1 |
| Mittelteil m = 70 (Block fill) | 994.0 | — | VDB 0,y,140,1 |
| `VDB` volle Bytes je Zeile (70 Byte, Block fill) | 341.3 | ≤ 60 + Fill | (b70x96 − b70x1) / 95 |
| `VDB 0,0,140,96,c` chunky gesamt | 33337.0 | ≤ 96 × (60 + Fill) | b70x96 |
| XOR-Mittelteil je Byte (Bitmap, c = 2) | 65.1 | — | (m9 − m1) / 8 |
| `VDR 3,y,10,10,1` gesamt | 4513.0 | — | r10x10 |
| flache Linie dy = 1, dx = 300 (2 Spannen), je Pixel Fehlerterm | 74.5 | ≈ 15 | (l300x1 − l300x0) / 300, Näherung |
