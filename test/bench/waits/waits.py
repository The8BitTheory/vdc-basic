#!/usr/bin/env python3
"""P2g: Experiment "weniger VDC-Ready-Waits" (docs/v3/P2g-wait-experiment.md).

Eigenstaendiges Messprogramm (ohne VDC-BASIC), laeuft in Z64K und auf echter Hardware (C128, 64-KB-VDC).
Chunky-Modus wie Crawl (R0 127, R4 159, R6 120, R7 133, R9 1, R25 Bit 7, R28 |= 24, R26 $FF,
Bitmap $B4FF = $0F, Attribute ab $0000), VIC ausgetastet, Interrupts aus, Messung mit CIA 2 (Timer A+B, 1 MHz).

Zwei Fallarten:
  Schleife: entrollte Kopie der VMC-Zeilenschleife (P2f) mit weggelassenen Waits; 8 und 128 Zeilen,
            je 3 Wiederholungen an verschiedenen Bildlagen; nach jeder Wiederholung Fletcher-16 ueber
            VRAM $0000-$27FF (Ziel) gegen den in Python berechneten Sollwert.
  Dauer:    Zeit vom Registerzugriff bis Ready-Bit (Blockkopie L Byte, Fill, R19, R18, R31), 64 Stichproben.

  waits.py build [--set NAME] [--out DIR]      Programm + Disk. NAME: hw (echte Hardware, 6 Konfigurationen,
                                               Lader schreibt RES0..RES5), z<MHz>r<R36> (alle Varianten + Dauer),
                                               dz.. (nur Dauer), tz.. / rz.. (Nachtraege), sz.. (kurz), full, short
  waits.py z64k [--sets z1r5,z2r0 ...] [--jobs 3] [--out DIR]   in Z64K laufen lassen, auswerten
  waits.py eval RES.prg [--json OUT]           Ergebnisblock (BSAVE "RES") auswerten
"""
import argparse, json, os, shutil, subprocess, sys, concurrent.futures

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, '..', '..', '..'))
sys.path.insert(0, os.path.join(REPO, 'test'))

ROWS = 128                      # lange Messung (8 Zeilen kurz), Vielfaches von 8
DPAGES = (ROWS * 80 + 255) // 256   # Ziel $0000.. (Pruefbereich)
SEED = 0x5A
KSAMP = 200                     # Stichproben je Dauer-Fall
WINDOW_US = {1: 33, 2: 16}      # CPU-Zeit von R30 bis zum naechsten R18 (entrollt, ohne Wait): 33 Takte
LENS = [4, 8, 12, 16, 24, 40]
DUR_COPY = [1, 2, 4, 6, 8, 12, 16, 24, 32, 40, 64]
DUR_FILL = [8, 40]
MODE = [(0, 127), (4, 159), (6, 120), (7, 133), (9, 1), (12, 0xB4), (13, 0xFF), (20, 0), (21, 0), (26, 0xFF)]
RMW = [(25, 0x80), (28, 0x18)]

# ---------------------------------------------------------------- Varianten
# (name, typ, A-Periode, B, C-Periode); typ 'p2f' = Kopie der P2f-Schleife (nicht entrollt),
# 2 = nur Ziel (R18, R19, R30), 3 = mit Quelle (R32, R33, R18, R19, R30).
# A = Wait vor dem ersten Zugriff der Zeile (R18 bzw. R32: wartet auf das Ende der Kopie der Vorzeile),
# B = Waits vor den uebrigen Adressregistern, C = Wait vor R30.  Periode N: nur in Zeilen mit Index % N == 0.
VARIANTS = [
    ('p2f', 'p2f', 1, 1, 1),
    ('full', 2, 1, 1, 1),
    ('AC', 2, 1, 0, 1),
    ('A', 2, 1, 0, 0),
    ('C', 2, 0, 0, 1),
    ('AC/2', 2, 2, 0, 1),
    ('AC/4', 2, 4, 0, 1),
    ('AC/8', 2, 8, 0, 1),
    ('all/2', 2, 2, 2, 2),
    ('all/4', 2, 4, 4, 4),
    ('all/8', 2, 8, 8, 8),
    ('none', 2, 0, 0, 0),
    ('S-full', 3, 1, 1, 1),
    ('S-AC', 3, 1, 0, 1),
    ('S-AC/2', 3, 2, 0, 1),
    ('S-AC/4', 3, 4, 0, 1),
    ('S-AC/8', 3, 8, 0, 1),
    ('S-none', 3, 0, 0, 0),
    ('ACt', 4, 1, 0, 1),        # wie AC, Werte vorher in A/Y: kuerzer von Ready bis R19 bzw. R30
    ('S-ACt', 5, 1, 0, 1),
    ('p2f-AC', 'p2f', 1, 0, 1),  # P2f-Schleife ohne Wait vor R19
    ('p2fS', 'p2fS', 1, 1, 1),   # P2f-Schleife mit Quellschritt (wie VCL Typ 3)
    ('p2fS-AC', 'p2fS', 1, 0, 1),  # ... ohne Waits vor R33, R18, R19
    ('R-ACt', 6, 1, 0, 1),       # nicht entrollt wie ACt (dec/bne), nur Laenge 1..255, ohne Fuellschritt-Weiche
    ('RS-ACt', 7, 1, 0, 1),      # dasselbe mit Quellschritt
]
VNAME = [v[0] for v in VARIANTS]
DUMP_VARIANTS = {'none', 'A', 'C'}
HW_VARIANTS = ('p2f', 'p2f-AC', 'R-ACt', 'p2fS', 'p2fS-AC', 'RS-ACt', 'full', 'AC', 'ACt', 'A', 'C', 'AC/2', 'AC/8', 'S-AC')
DUMP_LENS = {8, 40}
DUMP_BYTES = 16 * 80
# Dauer-Ops: (name, Register, Wert) ; Wert None = Laenge
DUR_OPS = [('base', 36, 'r36'), ('copy', 30, None), ('R19', 19, 0), ('R18', 18, 0x40), ('R31', 31, 0x55), ('fill', 30, None)]

CYC = {'lda#': 2, 'ldx#': 2, 'ldy#': 2, 'lda': 3, 'ldy': 3, 'sta': 3, 'sty$': 4, 'adc': 3, 'stx$': 4, 'sta$': 4, 'bit$': 4,
       'inx': 2, 'clc': 2, 'dec': 5, 'jmp': 3, 'beq': 2, 'bne': 2, 'bpl': 2, 'rts': 6}


def wait():
    return ['bit $d600', 'bpl *-3']


def row_code(typ, r, A, B, C):
    """Code einer Zeile r (0..7) einer entrollten Variante."""
    on = lambda n: n and r % n == 0
    c = []
    if typ in (4, 5):           # "tight": Adresse vorher in A (hi) / Y (lo), Register vorher angewaehlt
        if typ == 5:
            c += ['ldy src', 'lda src+1', 'ldx #32', 'stx $d600'] + (wait() if on(A) else []) + ['sta $d601',
                  'inx', 'stx $d600', 'sty $d601']
        c += ['ldy dst', 'lda dst+1', 'ldx #18', 'stx $d600'] + (wait() if on(A) and typ == 4 else []) + [
              'sta $d601', 'inx', 'stx $d600', 'sty $d601',
              'ldx #30', 'lda len', 'stx $d600'] + (wait() if on(C) else []) + ['sta $d601']
        if typ == 5:
            c += ['clc', 'lda src', 'adc sstep', 'sta src', 'lda src+1', 'adc sstep+1', 'sta src+1']
        c += ['clc', 'lda dst', 'adc step', 'sta dst', 'lda dst+1', 'adc step+1', 'sta dst+1']
        return c
    if typ == 3:
        c += ['ldx #32', 'stx $d600', 'lda src+1'] + (wait() if on(A) else []) + ['sta $d601',
              'inx', 'stx $d600', 'lda src'] + (wait() if on(B) else []) + ['sta $d601']
        c += ['ldx #18', 'stx $d600', 'lda dst+1'] + (wait() if on(B) else []) + ['sta $d601']
    else:
        c += ['ldx #18', 'stx $d600', 'lda dst+1'] + (wait() if on(A) else []) + ['sta $d601']
    c += ['inx', 'stx $d600', 'lda dst'] + (wait() if on(B) else []) + ['sta $d601',
          'ldx #30', 'stx $d600', 'lda len'] + (wait() if on(C) else []) + ['sta $d601']
    if typ == 3:
        c += ['clc', 'lda src', 'adc sstep', 'sta src', 'lda src+1', 'adc sstep+1', 'sta src+1']
    c += ['clc', 'lda dst', 'adc step', 'sta dst', 'lda dst+1', 'adc step+1', 'sta dst+1']
    return c


def variant_code(i):
    name, typ, A, B, C = VARIANTS[i]
    lab = f'v{i}'
    if typ in ('p2f', 'p2fS'):    # VMC-Zeilenschleife aus src/vdcbasic.asm (P2f), nicht entrollt; B = 0: ohne Waits vor R33/R18/R19
        w = lambda on: wait() if on else []
        c = [f'{lab}']
        if typ == 'p2fS':       # mit Quellschritt (.vmc_src_step), wie P2f: erst rechnen, dann R32/R33
            c += ['    jmp .set', '.srcstep clc', 'lda src', 'adc sstep', 'sta src', 'lda src+1', 'adc sstep+1',
                  'sta src+1', '.set ldx #32', 'stx $d600', 'lda src+1'] + wait() + ['sta $d601', 'inx', 'stx $d600',
                  'lda src'] + w(B) + ['sta $d601']
        c += ['.dst ldx #18', 'stx $d600', 'lda dst+1'] + (wait() if typ == 'p2f' or B else []) + ['sta $d601', 'inx',
              'stx $d600', 'lda dst'] + w(B) + ['sta $d601', 'jmp .count', '.count ldx #30', 'stx $d600', 'lda len',
              'beq +'] + wait() + ['sta $d601', '+ ldy lenhi', 'beq .counted', '.counted dec cnt', 'beq .exit', 'clc',
              'lda dst', 'adc step', 'sta dst', 'lda dst+1', 'adc step+1', 'sta dst+1',
              'jmp .srcstep' if typ == 'p2fS' else 'jmp .dst', '.exit rts']
        return c
    if typ in (6, 7):           # nicht entrollt: eine Zeile wie ACt/S-ACt, dann dec cnt / bne
        return [lab, '.row'] + row_code(typ - 2, 0, A, B, C) + ['dec cnt', 'bne .row', 'rts']
    out = [lab]
    for r in range(8):
        out += row_code(typ, r, A, B, C)
    out += ['dec cnt', 'beq +', f'jmp {lab}', '+ rts']
    return out


def count_cycles(lines):
    """Takte einer Zeilenfolge ohne genommene Waits (bit+bpl = 6), Sprungbefehle als nicht genommen."""
    t = 0
    for l in lines:
        l = l.split(' ', 1)[1] if l.startswith(('.', '+')) and ' ' in l else l
        if l.startswith(('.', '+', 'v')) and ' ' not in l:
            continue
        op, _, arg = l.partition(' ')
        key = op + ('#' if arg.startswith('#') else '$' if arg.startswith('$') else '')
        t += CYC.get(key, CYC.get(op, 0))
    return t


def cpu_cycles_per_row(i):
    name, typ, A, B, C = VARIANTS[i]
    if typ in ('p2f', 'p2fS'):
        body = [l for l in variant_code(i)[1:-1] if l.strip() != 'jmp .set']
        return count_cycles(body) + 1       # beq .counted genommen (+1)
    if typ in (6, 7):
        return count_cycles(row_code(typ - 2, 0, A, B, C)) + 5 + 3      # dec, bne (genommen)
    tot = sum(count_cycles(row_code(typ, r, A, B, C)) for r in range(8)) + 5 + 2 + 3   # dec, beq, jmp
    return tot / 8


# ---------------------------------------------------------------- Faelle
def cases_for(short=False, only=None, dur=False):
    cs = []
    lens = [8, 40] if short else LENS
    vs = range(len(VARIANTS)) if not short else [VNAME.index(n) for n in ('p2f', 'full', 'AC', 'AC/2', 'AC/8', 'S-full', 'S-AC')]
    if only:                    # nur Schleifen dieser Varianten (+ Dauer-Faelle mit dur=True)
        cs = [(1, VNAME.index(n), L, 1 if (n in DUMP_VARIANTS and L in DUMP_LENS) else 0) for L in LENS for n in only]
        if not dur:
            return cs
    for L in (lens if not only else []):
        for v in vs:
            flags = 1 if (VNAME[v] in DUMP_VARIANTS and L in DUMP_LENS and not short) else 0
            cs.append((1, v, L, flags))
    for o, (oname, reg, val) in enumerate(DUR_OPS):
        if oname == 'copy':
            for L in ([8, 40] if short else DUR_COPY):
                cs.append((2, o, L, 0))
        elif oname == 'fill':
            for L in ([] if short else DUR_FILL):
                cs.append((2, o, L, 0))
        else:
            cs.append((2, o, 0, 0))
    return cs


def case_set(name):
    """(Konfigurationen [(MHz, R36)], Faelle [(Art, Variante/Op, L, Flags)])"""
    if name == 'hw':            # echte Hardware: Auswahl, alle sechs Konfigurationen (Lader: RES0..RES5)
        return [(m, r) for m in (2, 1) for r in (0, 1, 5)], cases_for(only=HW_VARIANTS, dur=True)
    if name == 'hwact':         # Wiederholung des ACt-Ausreissers (1 MHz, R36 = 0): sechs gleiche Konfigurationen = 18 Wiederholungen
        return [(1, 0)] * 6, cases_for(only=('ACt', 'R-ACt', 'AC'))
    if name in ('full', 'short'):
        return [(m, r) for m in (1, 2) for r in (5, 1, 0)], cases_for(short=(name == 'short'))
    if name.startswith('dz') and 'r' in name:             # dz...: nur Dauer-Faelle
        mhz, r36 = name[2:].split('r')
        return [(int(mhz), int(r36))], [c for c in cases_for() if c[0] == 2]
    if name.startswith('rz') and 'r' in name[2:]:         # rz...: Nachtrag nicht entrollte "tight"-Schleifen
        mhz, r36 = name[2:].split('r')
        return [(int(mhz), int(r36))], cases_for(only=('p2f', 'p2f-AC', 'R-ACt', 'p2fS', 'p2fS-AC', 'RS-ACt'))
    if name.startswith('tz') and 'r' in name:             # tz...: Nachtrag "tight"
        mhz, r36 = name[2:].split('r')
        return [(int(mhz), int(r36))], cases_for(only=('p2f', 'p2f-AC', 'p2fS', 'p2fS-AC', 'full', 'AC', 'ACt', 'S-full', 'S-AC', 'S-ACt'))
    if name.startswith(('z', 'sz')) and 'r' in name:      # sz...: Kurzsatz
        mhz, r36 = name.lstrip('s')[1:].split('r')
        return [(int(mhz), int(r36))], cases_for(short=name.startswith('s'))
    raise SystemExit(f'unbekannter Satz {name}')


def result_size(cfgs, cs):
    n = 0
    for k, v, L, fl in cs:
        n += 6 + (24 + (DUMP_BYTES if fl & 1 else 0) if k == 1 else 7 + 2 * KSAMP)
    return 8 + n


# ---------------------------------------------------------------- Assembler-Quelle
TEMPLATE = r'''
; P2g wait experiment - generated by test/bench/waits/waits.py, do not edit
!cpu 6502
dst = $60
src = $62
step = $64
sstep = $66
len = $68
cnt = $69
s1 = $6a
s2 = $6b
rp = $6c
cp = $6e
lfsr = $70
tmp = $71
fval = $72
faddr = $73
pages = $75
k = $76
mn = $77
mx = $79
sm = $7b
rep = $7e
rows = $7f
lenhi = $5f
rolled = $5e
e = $5c
fnp = $5a
dlfsr = $59
opval = $58
opreg = $57
ROWS = %(ROWS)d
DPAGES = %(DPAGES)d
SEED = %(SEED)d
RES = $6000
K = %(K)d

    * = $2000
    jmp main
cfg_sel !byte 0             ; $2003: Index der Konfiguration (POKE vom Lader)
    !byte 1                 ; version

main
    php
    sei
    ldx #$2f
-   lda $50, x
    sta zsave, x
    dex
    bpl -
    lda $ff00
    sta sv_ff00
    lda #$3e
    sta $ff00
    lda $d011
    sta sv_d011
    and #$6f                ; VIC aus, Bit 7 (Raster Bit 8) loeschen
    sta $d011
    lda $d030
    sta sv_d030
    ldx #36
    jsr vdc_r
    sta sv_r36
    ldx #24
    jsr vdc_r
    sta sv_r24
    jsr fast
    ldy #NSAVE - 1
-   ldx save_regs, y
    jsr vdc_r
    sta mode_save, y
    dey
    bpl -
    ldy #NMODE - 1
-   ldx save_regs, y
    lda mode_vals, y
    jsr vdc_w
    dey
    bpl -
    ldx #25
    jsr vdc_r
    ora #$80
    jsr vdc_w
    ldx #28
    jsr vdc_r
    ora #$18
    jsr vdc_w
    jsr set_fill
    lda #$00                ; Bitmap $b400-$ffff = $0f
    sta faddr
    lda #$b4
    sta faddr + 1
    lda #$0f
    sta fval
    lda #76
    sta pages
    jsr fill
    lda #0
    sta lenhi
    lda #$7f
    sta $dd0d
    lda #<RES
    sta rp
    lda #>RES
    sta rp + 1
    ldy #5
-   lda magic, y
    sta (rp), y
    dey
    bpl -
    lda #8
    jsr rp_add
    lda cfg_sel
    sta cfg
cfg_loop
    ldx cfg
    lda cfg_mhz, x
    beq all_done
    sta c_mhz
    lda cfg_r36, x
    sta c_r36
    inc cfg
    lda #<cases
    sta cp
    lda #>cases
    sta cp + 1
case_loop
    ldy #0
    lda (cp), y
    beq all_done
    ldy #3
-   lda (cp), y
    sta c_kind, y
    dey
    bpl -
    ldy #0
    lda c_kind
    sta (rp), y
    iny
    lda c_var
    sta (rp), y
    iny
    lda c_len
    sta (rp), y
    iny
    lda c_mhz
    sta (rp), y
    iny
    lda c_r36
    sta (rp), y
    iny
    lda c_flags
    sta (rp), y
    lda #6
    jsr rp_add
    clc
    lda cp
    adc #4
    sta cp
    bcc +
    inc cp + 1
+   jsr fast
    ldx #36
    lda c_r36
    jsr vdc_w
    lda c_kind
    cmp #1
    bne +
    jsr loop_case
    jmp case_loop
+   jsr dur_case
    jmp case_loop

all_done
    ldy #6
    lda rp
    sta RES, y
    lda rp + 1
    sta RES + 1, y
    jsr fast
    ldy #NSAVE - 1
-   ldx save_regs, y
    lda mode_save, y
    jsr vdc_w
    dey
    bpl -
    ldx #36
    lda sv_r36
    jsr vdc_w
    ldx #24
    lda sv_r24
    jsr vdc_w
    lda sv_d030
    sta $d030
    lda sv_d011
    and #$7f
    sta $d011
    lda sv_ff00
    sta $ff00
    ldx #$2f
-   lda zsave, x
    sta $50, x
    dex
    bpl -
    plp
    rts

rp_add
    clc
    adc rp
    sta rp
    bcc +
    inc rp + 1
+   rts

fast
    lda sv_d030
    ora #1
    sta $d030
    rts
setspeed
    lda c_mhz
    lsr                     ; 1 -> 0, 2 -> 1
    and #1
    sta tmp
    lda sv_d030
    and #$fe
    ora tmp
    sta $d030
    rts

vdc_w                       ; R(X) = A, wartet vorher, X/Y bleiben
    stx $d600
-   bit $d600
    bpl -
    sta $d601
    rts
vdc_r                       ; A = R(X)
    stx $d600
-   bit $d600
    bpl -
    lda $d601
    rts
set_fill
    ldx #24
    jsr vdc_r
    and #$7f
    jmp vdc_w
set_copy
    ldx #24
    jsr vdc_r
    ora #$80
    jmp vdc_w

fill                        ; faddr, fval, pages (je 256 Byte)
    ldx #18
    lda faddr + 1
    jsr vdc_w
    inx
    lda faddr
    jsr vdc_w
    ldx #31
    lda fval
    jsr vdc_w
    ldx #30
    lda #255
    jsr vdc_w
    ldy pages
    dey
    beq +
-   lda #0
    jsr vdc_w
    dey
    bne -
+   rts

write_pattern               ; $8000.. = LFSR-Folge, c_len * ROWS Byte
    ldx #18
    lda #$80
    jsr vdc_w
    inx
    lda #0
    jsr vdc_w
    ldx #31
    stx $d600
    lda #SEED
    sta lfsr
    lda #ROWS
    sta tmp
--  ldy c_len
-   lda lfsr
    lsr
    bcc +
    eor #$b8
+   sta lfsr
    bit $d600
    bpl *-3
    sta $d601
    dey
    bne -
    dec tmp
    bne --
    rts

vdc_sync                    ; Status Bit 5 (Austastung) steigt, dann faellt es
-   lda $d600
    and #$20
    beq -
-   lda $d600
    and #$20
    bne -
    ldx rep                 ; Lage im Bild je Wiederholung
    lda c_mhz
    cmp #2
    bne +
    inx
    inx
    inx
+   ldy dly_tab, x
    beq ++
--  ldx #0
-   dex
    bne -
    dey
    bne --
++  rts

loop_case
    jsr write_pattern
    ldx c_var
    lda var_lo, x
    sta fnp
    lda var_hi, x
    sta fnp + 1
    lda var_rolled, x
    sta rolled
    lda #0
    sta rep
.rl jsr fast
    jsr set_fill
    lda #0
    sta faddr
    sta faddr + 1
    sta fval
    lda #DPAGES
    sta pages
    jsr fill
    jsr set_copy
    jsr setspeed
    lda #8
    jsr timed
    lda #ROWS
    jsr timed
    jsr fast
    jsr verify
    inc rep
    lda rep
    cmp #3
    bne .rl
    lda c_flags
    and #1
    beq +
    jsr dump
+   rts

timed                       ; A = Zeilen
    sta rows
    lda #0
    sta dst
    sta dst + 1
    sta src
    sta step + 1
    sta sstep + 1
    lda #$80
    sta src + 1
    lda #80
    sta step
    lda c_len
    sta len
    sta sstep
    lda rows
    ldx rolled
    bne +
    lsr
    lsr
    lsr
+   sta cnt
    ldx #32
    lda #$80
    jsr vdc_w
    inx
    lda #0
    jsr vdc_w
    ldx #31                 ; idle abwarten (Vorlesen nach R19 aus fill/verify)
    stx $d600
-   bit $d600
    bpl -
    jsr vdc_sync
    lda #$ff
    sta $dd04
    sta $dd05
    sta $dd06
    sta $dd07
    lda #$7f
    sta $dd0d
    lda #$51
    sta $dd0f
    lda #$11
    sta $dd0e
    jsr callfn
    lda #0
    sta $dd0e
    sta $dd0f
    ldy #0
    lda $dd04
    eor #$ff
    sta (rp), y
    iny
    lda $dd05
    eor #$ff
    sta (rp), y
    iny
    lda $dd06
    eor #$ff
    sta (rp), y
    lda #3
    jmp rp_add
callfn
    jmp (fnp)

verify                      ; Fletcher-16 (mod 256) ueber $0000 .. DPAGES*256-1
    ldx #18
    lda #0
    jsr vdc_w
    inx
    jsr vdc_w
    ldx #31
    stx $d600
    lda #0
    sta s1
    sta s2
    lda #DPAGES
    sta pages
    ldy #0
-   bit $d600
    bpl -
    lda $d601
    clc
    adc s1
    sta s1
    clc
    adc s2
    sta s2
    iny
    bne -
    dec pages
    bne -
    ldy #0
    lda s1
    sta (rp), y
    iny
    lda s2
    sta (rp), y
    lda #2
    jmp rp_add

dump                        ; $0000 .. 16*80-1 nach rp
    ldx #18
    lda #0
    jsr vdc_w
    inx
    jsr vdc_w
    ldx #31
    stx $d600
    lda #<(16*80)
    sta e
    lda #>(16*80)
    sta e + 1
    ldy #0
-   bit $d600
    bpl -
    lda $d601
    sta (rp), y
    inc rp
    bne +
    inc rp + 1
+   lda e
    bne +
    dec e + 1
+   dec e
    lda e
    ora e + 1
    bne -
    rts

dur_case
    lda c_var
    cmp #5                  ; fill
    bne +
    jsr set_fill
    jmp ++
+   jsr set_copy
++  ldx c_var
    lda op_reg, x
    sta opreg
    lda op_val, x
    cpx #0
    bne +
    lda c_r36
+   cpx #1
    beq +
    cpx #5
    bne ++
+   lda c_len
++  sta opval
    jsr setspeed
    lda #$ff
    sta mn
    sta mn + 1
    lda #0
    sta mx
    sta mx + 1
    sta sm
    sta sm + 1
    sta sm + 2
    lda #$a5
    sta dlfsr
    lda #K
    sta k
    lda #$7f
    sta $dd0d
.dl ldx #18
    lda #$40
    jsr vdc_w
    inx
    lda #0
    jsr vdc_w
    ldx #32
    lda #$80
    jsr vdc_w
    inx
    lda #0
    jsr vdc_w
    ldx opreg
    stx $d600
-   bit $d600
    bpl -
    lda dlfsr               ; Pause 0..255 x 5 Takte: zufaellige Lage zur Zeile (nach dem Ready-Abwarten)
    lsr
    bcc +
    eor #$b8
+   sta dlfsr
    tax
-   dex
    bne -
    lda #$ff
    sta $dd04
    sta $dd05
    lda opval
    ldx #$11
    ldy #0
    stx $dd0e
    sta $d601
-   bit $d600
    bpl -
    sty $dd0e
    lda $dd04
    eor #$ff
    sta e
    lda $dd05
    eor #$ff
    sta e + 1
    ldy #0                  ; Stichprobe speichern
    lda e
    sta (rp), y
    iny
    lda e + 1
    sta (rp), y
    lda #2
    jsr rp_add
    lda e                   ; min
    cmp mn
    lda e + 1
    sbc mn + 1
    bcs +
    lda e
    sta mn
    lda e + 1
    sta mn + 1
+   lda mx                  ; max
    cmp e
    lda mx + 1
    sbc e + 1
    bcs +
    lda e
    sta mx
    lda e + 1
    sta mx + 1
+   clc
    lda sm
    adc e
    sta sm
    lda sm + 1
    adc e + 1
    sta sm + 1
    bcc +
    inc sm + 2
+   dec k
    beq +
    jmp .dl
+   ldy #6
-   lda mn, y
    sta (rp), y
    dey
    bpl -
    lda #7
    jmp rp_add

; ---------------------------------------------------------------- Daten
magic !text "P2G"
    !byte 2, <ROWS, DPAGES
dly_tab !byte 0, 5, 10, 0, 10, 20
save_regs !byte %(SAVE_REGS)s
mode_vals !byte %(MODE_VALS)s
NMODE = %(NMODE)d
NSAVE = %(NSAVE)d
mode_save !fill NSAVE, 0
op_reg !byte %(OP_REG)s
op_val !byte %(OP_VAL)s
var_lo !byte %(VAR_LO)s
var_hi !byte %(VAR_HI)s
var_rolled !byte %(VAR_ROLLED)s
sv_ff00 !byte 0
sv_d011 !byte 0
sv_d030 !byte 0
sv_r36 !byte 0
sv_r24 !byte 0
c_kind !byte 0
c_var !byte 0
c_len !byte 0
c_flags !byte 0
c_mhz !byte 0
c_r36 !byte 0
cfg !byte 0
cfg_mhz !byte %(CFG_MHZ)s, 0
cfg_r36 !byte %(CFG_R36)s, 0
zsave !fill $30, 0
data_end
; ---------------------------------------------------------------- Varianten (generiert)
%(VARIANTS)s

cases
%(CASES)s
    !byte 0
code_end
!if data_end > $3f00 { !error "Daten muessen unter $4000 liegen (Bank 15 beim SYS)" }
!if code_end > RES { !error "Programm zu gross" }
'''


def gen_source(cfgs, cs):
    vlines = []
    for i in range(len(VARIANTS)):
        vlines.append(f'!zone v{i}')
        for l in variant_code(i):
            if l.startswith(('v', '.', '+')):
                vlines.append(l)
            else:
                vlines.append('    ' + l)
    regs = [r for r, _ in MODE] + [r for r, _ in RMW]
    src = TEMPLATE % dict(
        ROWS=ROWS, DPAGES=DPAGES, K=KSAMP, SEED=SEED, VARIANTS='\n'.join(vlines),
        SAVE_REGS=', '.join(map(str, regs)), MODE_VALS=', '.join(str(v) for _, v in MODE),
        NMODE=len(MODE), NSAVE=len(regs),
        OP_REG=', '.join(str(r) for _, r, _ in DUR_OPS),
        OP_VAL=', '.join(str(v if isinstance(v, int) else 0) for _, _, v in DUR_OPS),
        VAR_LO=', '.join(f'<v{i}' for i in range(len(VARIANTS))),
        VAR_HI=', '.join(f'>v{i}' for i in range(len(VARIANTS))),
        VAR_ROLLED=', '.join('1' if v[1] in ('p2f', 'p2fS', 6, 7) else '0' for v in VARIANTS),
        CFG_MHZ=', '.join(str(m) for m, _ in cfgs), CFG_R36=', '.join(str(r) for _, r in cfgs),
        CASES='\n'.join('    !byte ' + ', '.join(map(str, c)) for c in cs))
    return src


LOADER = '''10 TRAP 90
20 BLOAD"W",B0,P(8192)
25 FOR I=0 TO {NCFG}
30 BANK15:POKE 8195,I:SYS8192
40 BANK0:E=PEEK(24582)+256*PEEK(24583):BANK15
50 F$="@RES"+CHR$(48+I):BSAVE(F$),B0,P(24576) TO P(E)
55 PRINT F$;" OK"
58 NEXT
60 PRINT"FERTIG"{EXIT}
70 END
90 {ERR}
'''


def build(setname, out):
    os.makedirs(out, exist_ok=True)
    cfgs, cs = case_set(setname)
    size = result_size(cfgs, cs)
    if 0x6000 + size > 0xC000:
        raise SystemExit(f'Ergebnisblock zu gross: {size}')
    open(os.path.join(out, 'waits.a'), 'w').write(gen_source(cfgs, cs))
    subprocess.run(['acme', '-f', 'cbm', '-o', 'w.prg', '-l', 'w.sym', 'waits.a'], cwd=out, check=True)
    tok = [sys.executable, os.path.join(REPO, 'test', 'bastok.py')]
    for name, ex, er in (('loader', '', 'PRINT"FEHLER";ER;EL'), ('loader-z64k', ':POKE DEC("D7FF"),0',
                                                                  'BANK15:POKE DEC("D7FF"),2')):
        txt = LOADER.replace('{EXIT}', ex).replace('{ERR}', er).replace('{NCFG}', str(len(cfgs) - 1))
        open(os.path.join(out, name + '.bas'), 'w').write(txt)
        subprocess.run(tok + [os.path.join(out, name + '.bas'), os.path.join(out, name + '.prg')], check=True)
    d64 = os.path.join(out, 'waits.d64')
    subprocess.run(['c1541', '-format', 'waits,02', 'd64', d64, '-write', os.path.join(out, 'loader.prg'), 'loader',
                    '-write', os.path.join(out, 'w.prg'), 'w'], check=True, stdout=subprocess.DEVNULL)
    json.dump({'set': setname, 'configs': cfgs, 'cases': cs, 'result_size': size}, open(os.path.join(out, 'cases.json'), 'w'))
    return d64, len(cs) * len(cfgs), size


# ---------------------------------------------------------------- Auswertung
def lfsr_seq(n, seed=SEED):
    s, out = seed, []
    for _ in range(n):
        c = s & 1
        s >>= 1
        if c:
            s ^= 0xB8
        out.append(s)
    return out


def expected_fletcher(L):
    mem = bytearray(DPAGES * 256)
    src = lfsr_seq(L * ROWS)
    for r in range(ROWS):
        mem[r * 80:r * 80 + L] = bytes(src[r * L:(r + 1) * L])
    s1 = s2 = 0
    for b in mem:
        s1 = (s1 + b) & 255
        s2 = (s2 + s1) & 255
    return s1, s2


def analyse_dump(dump, L):
    """Zeilen 0..15 des Ziels gegen die Sollwerte: je Zeile 'ok', 'leer' (R30 verloren), 'falsch'."""
    src = lfsr_seq(L * ROWS)
    res = []
    for r in range(16):
        row = dump[r * 80:(r + 1) * 80]
        want = bytes(src[r * L:(r + 1) * L])
        if row[:L] == want and not any(row[L:]):
            res.append('ok')
        elif not any(row):
            res.append('leer')
        else:
            res.append('falsch')
    return res


def parse(data):
    """data: Bytes ab $4000 (ohne Ladeadresse)."""
    if data[:3] != b'P2G':
        raise SystemExit('kein P2G-Ergebnisblock')
    end = data[6] | data[7] << 8
    ver = data[3]
    p, n = 8, end - 0x6000
    recs = []
    while p < n:
        kind, v, L, mhz, r36, fl = data[p:p + 6]
        p += 6
        if kind == 1:
            reps = []
            for _ in range(3):
                t8 = data[p] | data[p + 1] << 8 | data[p + 2] << 16
                tl = data[p + 3] | data[p + 4] << 8 | data[p + 5] << 16
                f = (data[p + 6], data[p + 7])
                reps.append((t8, tl, f))
                p += 8
            rec = dict(kind='loop', var=VNAME[v], len=L, mhz=mhz, r36=r36, reps=reps)
            if fl & 1:
                rec['dump'] = analyse_dump(data[p:p + DUMP_BYTES], L)
                p += DUMP_BYTES
            exp = expected_fletcher(L)
            rec['ok'] = [r[2] == exp for r in reps]
            per = [(tl - t8) / (ROWS - 8) for t8, tl, _ in reps]
            rec['per_row_us'] = sum(per) / 3
            rec['per_row_spread'] = (min(per), max(per))
            recs.append(rec)
        else:
            ks = KSAMP if ver >= 2 else 64      # Fassung 1: 64 Stichproben, nicht gespeichert (Lage nicht zufaellig)
            samples = [data[p + 2 * i] | data[p + 2 * i + 1] << 8 for i in range(ks)] if ver >= 2 else None
            p += 2 * ks if ver >= 2 else 0
            mn = data[p] | data[p + 1] << 8
            mx = data[p + 2] | data[p + 3] << 8
            sm = data[p + 4] | data[p + 5] << 8 | data[p + 6] << 16
            p += 7
            assert samples is None or (sum(samples) == sm and min(samples) == mn and max(samples) == mx)
            recs.append(dict(kind='dur', op=DUR_OPS[v][0], len=L, mhz=mhz, r36=r36, min=mn, max=mx, mean=sm / ks,
                             samples=samples, ver=ver))
    return recs


def report(recs):
    out = []
    loops = [r for r in recs if r['kind'] == 'loop']
    durs = [r for r in recs if r['kind'] == 'dur']
    if any(d.get('samples') for d in durs):     # Fassung 1 (Lage nicht zufaellig) nur ohne neuere Werte zeigen
        durs = [d for d in durs if d.get('samples')]
    confs = sorted({(r['mhz'], r['r36']) for r in recs}, key=lambda c: (c[0], -c[1] if c[1] else 0))
    base = {(d['mhz'], d['r36']): d['min'] for d in durs if d['op'] == 'base'}
    if durs:
        out.append(f'## Dauer bis Ready (µs, CIA, Grundlinie abgezogen)\n')
        out.append(f'Zelle: min / Median / Mittel / max über {KSAMP} Stichproben in zufälliger Lage zur Zeile; '
                   f'in Klammern Anteil fertig nach ≤ {WINDOW_US[2]} µs (2 MHz) bzw. ≤ {WINDOW_US[1]} µs (1 MHz) = '
                   'Zeit von R30 bis zum nächsten R18 in der entrollten Schleife ohne Wait. '
                   'Ältere Läufe (Fassung 1): min / Mittel / max über 64 Stichproben, Lage nicht zufällig.\n')
        ops = []
        for d in durs:
            key = (d['op'], d['len'])
            if key not in ops:
                ops.append(key)
        out.append('| Zugriff | ' + ' | '.join(f'{m} MHz R36={r}' for m, r in confs) + ' |')
        out.append('|---|' + '---|' * len(confs))
        for op, L in ops:
            cells = []
            for c in confs:
                d = [x for x in durs if (x['op'], x['len'], x['mhz'], x['r36']) == (op, L) + c]
                if d:
                    b = base.get(c, 0)
                    d = d[0]
                    if d.get('samples'):
                        sm = sorted(x - b for x in d['samples'])
                        thr = WINDOW_US[c[0]]
                        share = sum(1 for x in sm if x <= thr) * 100 // len(sm)
                        cells.append(f'{sm[0]} / {sm[len(sm) // 2]} / {d["mean"] - b:.0f} / {sm[-1]} ({share} %)')
                    else:
                        cells.append(f'{d["min"] - b} / {d["mean"] - b:.1f} / {d["max"] - b}')
                else:
                    cells.append('')
            out.append(f'| {op}{" " + str(L) if L else ""} | ' + ' | '.join(cells) + ' |')
        out.append('')
    if loops:
        out.append('## Zeilenschleife: Zeit je Zeile (µs, CIA) und Prüfung (3 Wiederholungen)\n')
        out.append('Zelle: µs je Zeile (1 MHz: = Takte), `F` = Fletcher falsch in n von 3 Wiederholungen.\n')
        lens = sorted({r['len'] for r in loops})
        for c in confs:
            out.append(f'### {c[0]} MHz, R36 = {c[1]}\n')
            out.append('| Variante | CPU-Takte/Zeile | ' + ' | '.join(f'L={L}' for L in lens) + ' |')
            out.append('|---|---|' + '---|' * len(lens))
            for vi, v in enumerate(VNAME):
                cells = []
                any_ = False
                for L in lens:
                    r = [x for x in loops if (x['var'], x['len'], x['mhz'], x['r36']) == (v, L) + c]
                    if not r:
                        cells.append('')
                        continue
                    any_ = True
                    r = r[0]
                    bad = 3 - sum(r['ok'])
                    s = f'{r["per_row_us"]:.1f}' + (f' F{bad}' if bad else '')
                    if 'dump' in r:
                        dd = r['dump']
                        s += f' [{dd.count("ok")}ok/{dd.count("leer")}leer/{dd.count("falsch")}falsch]'
                    cells.append(s)
                if any_:
                    out.append(f'| {v} | {cpu_cycles_per_row(vi):.1f} | ' + ' | '.join(cells) + ' |')
            out.append('')
    return '\n'.join(out)


def load_res(path):
    data = open(path, 'rb').read()
    if data[:2] == b'\x00\x60':
        data = data[2:]
    return data


def run_z64k(setname, out, timeout):
    import z64k
    d64, n, size = build(setname, out)
    img = os.path.join(out, 'out.d64')
    r = z64k.run(os.path.join(out, 'loader-z64k.prg'), d64=d64, limit=4_000_000_000, timeout=timeout, save=img,
                 log=os.path.join(out, 'z64k.log'))
    if r.rc != 0:
        return setname, f'rc {r.rc}', r.secs
    res = os.path.join(out, 'res.prg')
    subprocess.run(['c1541', '-attach', r.image, '-read', 'res0', res], check=True, stdout=subprocess.DEVNULL)
    return setname, res, r.secs


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest='cmd', required=True)
    b = sub.add_parser('build')
    b.add_argument('--set', default='full')
    b.add_argument('--out', default=os.path.join(REPO, 'build', 'p2g', 'hw'))
    z = sub.add_parser('z64k')
    z.add_argument('--sets', default='z1r5,z1r1,z1r0,z2r5,z2r1,z2r0')
    z.add_argument('--jobs', type=int, default=3)
    z.add_argument('--timeout', type=int, default=1800)
    z.add_argument('--out', default=os.path.join(REPO, 'build', 'p2g', 'z64k'))
    e = sub.add_parser('eval')
    e.add_argument('res', nargs='+')
    e.add_argument('--json')
    a = ap.parse_args()
    if a.cmd == 'build':
        d64, n, size = build(a.set, a.out)
        print(f'{d64}: {n} Faelle, Ergebnisblock {size} Byte')
        for i, v in enumerate(VARIANTS):
            print(f'  {v[0]:8} CPU {cpu_cycles_per_row(i):.1f} Takte/Zeile')
    elif a.cmd == 'z64k':
        sets = a.sets.split(',')
        with concurrent.futures.ThreadPoolExecutor(a.jobs) as ex:
            futs = [ex.submit(run_z64k, s, os.path.join(a.out, s), a.timeout) for s in sets]
            done = [f.result() for f in futs]
        recs = []
        for s, res, secs in done:
            print(f'{s}: {res} ({secs:.0f} s)')
            if res.startswith('rc'):
                continue
            recs += parse(load_res(res))
        json.dump(recs, open(os.path.join(a.out, 'results.json'), 'w'), indent=1)
        txt = report(recs)
        open(os.path.join(a.out, 'report.md'), 'w').write(txt)
        print(txt)
    else:
        recs = []
        for p in a.res:
            recs += parse(load_res(p))
        if a.json:
            json.dump(recs, open(a.json, 'w'), indent=1)
        print(report(recs))


if __name__ == '__main__':
    main()
