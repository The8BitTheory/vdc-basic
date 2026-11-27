# VDC-Messung (vice3.10, 2026-11-25)

**vorläufig (VICE)**: VICE 3.10: VDC-Busy-Modell grob (43/4 Takte je Reg-31-Zugriff, Fill 0,66, Copy 1,2 Takte je Byte), Reg 36 ohne Wirkung.

Messprogramm Version 1, VDC-BASIC v2h. Alle Werte in CPU-Zyklen der jeweiligen Konfiguration (µs in Klammern).

Konfigurationen: A = 2 MHz, Reg 36 = 0 (Engine); B = 2 MHz, Reg 36 = 5; C = 1 MHz, Reg 36 = 0; D = 1 MHz, Reg 36 = 5

## 1. Kostenparameter je Konfiguration

| Schlüssel | A | B | C | D |
|---|---|---|---|---|
| `cpu_byte` | 99.2 (49.6) | 99.2 (49.6) | 57.0 (57.0) | 57.0 (57.0) |
| `cpu_run_fixed` | -9.0 (-4.5) | -9.5 (-4.7) | -8.1 (-8.1) | -8.1 (-8.1) |
| `cpu_byte_avg` | 58.2 (29.1) | 58.2 (29.1) | 34.0 (34.0) | 34.0 (34.0) |
| `cpu_lit_byte` | 105.4 (52.7) | 105.4 (52.7) | 56.0 (56.0) | 56.0 (56.0) |
| `cpu_lit_byte_avg` | 57.6 (28.8) | 57.6 (28.8) | 39.6 (39.6) | 39.6 (39.6) |
| `new_addr` | 192.7 (96.4) | 192.6 (96.3) | 93.2 (93.2) | 93.2 (93.2) |
| `fill_fixed` | 128.1 (64.0) | 127.2 (63.6) | 76.4 (76.4) | 76.4 (76.4) |
| `fill_byte` | 1.3 (0.7) | 1.3 (0.7) | 0.7 (0.7) | 0.7 (0.7) |
| `fill_fixed_rmw` | 157.3 (78.7) | 157.0 (78.5) | 104.4 (104.4) | 104.4 (104.4) |
| `fill_byte_rmw` | 1.3 (0.7) | 1.3 (0.7) | 0.7 (0.7) | 0.7 (0.7) |
| `fill_issue_fixed` | 120.1 (60.1) | 119.7 (59.8) | 71.0 (71.0) | 71.0 (71.0) |
| `fill_issue_byte` | -0.0 (-0.0) | -0.0 (-0.0) | 0.0 (0.0) | 0.0 (0.0) |
| `copy_fixed` | 66.3 (33.1) | 65.9 (33.0) | 60.4 (60.4) | 60.4 (60.4) |
| `copy_byte` | 2.4 (1.2) | 2.4 (1.2) | 1.2 (1.2) | 1.2 (1.2) |
| `copy_fixed_rmw` | 95.4 (47.7) | 95.1 (47.6) | 88.4 (88.4) | 88.4 (88.4) |
| `copy_byte_rmw` | 2.4 (1.2) | 2.4 (1.2) | 1.2 (1.2) | 1.2 (1.2) |
| `copy_issue_fixed` | 56.6 (28.3) | 56.3 (28.2) | 54.0 (54.0) | 54.0 (54.0) |
| `copy_issue_byte` | 0.0 (0.0) | 0.0 (0.0) | 0.0 (0.0) | 0.0 (0.0) |

## 2. CPU gegen Block fill (Konfiguration A, Zyklen)

| L | t_cpu | t_fill norm | t_fill rmw | t_issue | schneller (norm) |
|---|---|---|---|---|---|
| 1 | 90.1 | 116.8 | 146.5 | 19.1 | CPU |
| 2 | 189.3 | 121.4 | 157.2 | 120.4 | Fill |
| 3 | 288.4 | 135.4 | 164.6 | 120.4 | Fill |
| 4 | 387.6 | 135.4 | 165.0 | 120.8 | Fill |
| 5 | 486.7 | 135.0 | 164.2 | 120.0 | Fill |
| 6 | 585.9 | 135.4 | 163.0 | 119.1 | Fill |
| 8 | 784.2 | 135.4 | 163.9 | 120.0 | Fill |
| 10 | 982.5 | 143.2 | 172.1 | 120.4 | Fill |
| 12 | 1180.8 | 143.8 | 172.4 | 120.0 | Fill |
| 16 | 1577.4 | 151.9 | 180.8 | 120.4 | Fill |
| 24 | 2370.7 | 160.5 | 188.8 | 120.4 | Fill |
| 32 | 3163.9 | 168.9 | 196.9 | 120.2 | Fill |
| 48 | 4750.3 | 193.0 | 222.2 | 119.8 | Fill |
| 64 | 6336.8 | 213.2 | 247.4 | 120.2 | Fill |
| 96 | 9509.7 | 249.1 | 280.4 | 118.8 | Fill |
| 128 | 12682.6 | 298.0 | 327.0 | 120.2 | Fill |
| 192 | 19028.4 | 383.8 | 412.6 | 119.5 | Fill |
| 256 | 25374.1 | 466.4 | 494.9 | 120.5 | Fill |

## 3. Schwelle T (kleinste Lauflänge, ab der Fill für alle größeren L schneller ist; 257 = nie)

| Konfiguration | T | T_rmw | T_issue |
|---|---|---|---|
| A | 2 | 2 | 2 |
| B | 2 | 2 | 2 |
| C | 2 | 2 | 2 |
| D | 2 | 2 | 2 |

## 4. VMC / VMS (Zyklen, µs in Klammern)

| Befehl | je Byte | je Zeile/Befehl | je Aufruf |
|---|---|---|---|
| vmc (A) | 1.0 (0.51) | 283.0 (141.5) | 180.3 (90.1) |
| vms1 (A) | 0.0 (0.01) | 427.2 (213.6) | 286.3 (143.1) |
| vms8 (A) | 865.6 (432.79) | (je Byte = je Befehl) | 765.0 (382.5) |
| vmc (B) | 1.0 (0.51) | 283.0 (141.5) | 179.3 (89.6) |
| vms1 (B) | 0.0 (0.01) | 427.3 (213.6) | 285.9 (143.0) |
| vms8 (B) | 865.6 (432.81) | (je Byte = je Befehl) | 761.4 (380.7) |
| vmc (C) | -0.0 (-0.00) | 191.0 (191.0) | 181.0 (181.0) |
| vms1 (C) | 0.0 (0.02) | 318.0 (318.0) | 276.5 (276.5) |
| vms8 (C) | 661.4 (661.45) | (je Byte = je Befehl) | 334.7 (334.7) |
| vmc (D) | -0.0 (-0.00) | 191.0 (191.0) | 181.0 (181.0) |
| vms1 (D) | 0.0 (0.02) | 318.0 (318.0) | 276.5 (276.5) |
| vms8 (D) | 661.4 (661.45) | (je Byte = je Befehl) | 334.7 (334.7) |

## 5. Reg 36: Fehler (Bytes von 8192)

| r \ F | 1 | 50 | 500 |
|---|---|---|---|
| 0 | 0 | 0 | 0 |
| 1 | 0 | 0 | 0 |
| 2 | 0 | 0 | 0 |
| 3 | 0 | 0 | 0 |
| 4 | 0 | 0 | 0 |
| 5 | 0 | 0 | 0 |

## 6. Warnungen und Gegenprobe

- Gegenprobe Konfig A (2 MHz): CIA 119195 µs, Monitor-Stoppuhr 119188 Zyklen, Verhältnis 0.9999, Abweichung -0.5 µs
- Gegenprobe Konfig B (2 MHz): CIA 119156 µs, Monitor-Stoppuhr 119149 Zyklen, Verhältnis 0.9999, Abweichung -0.5 µs
- Gegenprobe Konfig C (1 MHz): CIA 139538 µs, Monitor-Stoppuhr 139524 Zyklen, Verhältnis 0.9999, Abweichung -1.0 µs
- Gegenprobe Konfig D (1 MHz): CIA 139538 µs, Monitor-Stoppuhr 139524 Zyklen, Verhältnis 0.9999, Abweichung -1.0 µs
- keine Warnungen
