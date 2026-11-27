#!/usr/bin/env python3
"""Auswertung des Ergebnisblocks von vdcbench (BENCHRES, PRG ab $4000) -> JSON + Markdown.

Aufruf: evaluate.py BENCHRES.prg [--source vice3.10|c128] [--out DIR]
Siehe docs/v3/P0-bench.md, Abschnitt 10.
"""
import argparse, datetime, json, os, sys

CASES = {0: 'empty', 1: 'stream_const', 2: 'stream_lit', 3: 'new_addr', 4: 'fill_done', 5: 'fill_issue',
         6: 'copy_done', 7: 'copy_issue', 8: 'vmc', 9: 'vms1', 10: 'vms8', 11: 'r36'}
CONFIGS = {(2, 0): 'A', (2, 5): 'B', (1, 0): 'C', (1, 5): 'D'}
M = [1, 2, 3, 4, 5, 6, 8, 10, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256]
N_DISP = 16            # Fall 1/2: Fenster mit N <= 16 liegen nach vdc_sync ganz in der Anzeige (< 15 ms)
FRAME_CYCLES = 15       # xchk_a/xchk_b liegen innerhalb des CIA-Fensters (rts + jmp + lda/sta)
PROV_REASON = ('VICE 3.10: VDC-Busy-Modell grob (43/4 Takte je Reg-31-Zugriff, Fill 0,66, Copy 1,2 Takte je Byte), '
               'Reg 36 ohne Wirkung')


def parse(path):
    d = open(path, 'rb').read()
    if d[0] | d[1] << 8 != 0x4000:
        sys.exit('BENCHRES: Ladeadresse ist nicht $4000')
    d = d[2:]
    if d[:2] != b'BR':
        sys.exit('BENCHRES: Kennung "BR" fehlt')
    recs, i = [], 8
    while i + 8 <= len(d):
        r = d[i:i + 8]
        if r[0] == 0xFF:
            break
        if r[0] == 0xFE:
            sys.exit('BENCHRES: Messprogramm hat wegen Platzmangel abgebrochen (Datensatz $FE)')
        recs.append({'id': r[0], 'cfg': r[1], 'par': r[2] | r[3] << 8, 'reps': r[4],
                     'raw': r[5] | r[6] << 8 | r[7] << 16})
        i += 8
    return d[3], recs


def linreg(xs, ys):
    n = len(xs)
    mx, my = sum(xs) / n, sum(ys) / n
    sxx = sum((x - mx) ** 2 for x in xs)
    if sxx == 0:
        return 0.0, my
    b = sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / sxx
    return b, my - b * mx


def lstsq(rows, ys):
    """Kleinste Quadrate fuer y = sum(c_k * row_k) ueber die Normalgleichungen."""
    k = len(rows[0])
    a = [[sum(r[i] * r[j] for r in rows) for j in range(k)] + [sum(r[i] * y for r, y in zip(rows, ys))]
         for i in range(k)]
    for i in range(k):
        p = max(range(i, k), key=lambda r: abs(a[r][i]))
        a[i], a[p] = a[p], a[i]
        if abs(a[i][i]) < 1e-12:
            return [0.0] * k
        for r in range(k):
            if r != i:
                f = a[r][i] / a[i][i]
                a[r] = [x - f * y for x, y in zip(a[r], a[i])]
    return [a[i][k] / a[i][i] for i in range(k)]


def interp(table, L):
    """table: {L: wert}; linear zwischen den Stuetzstellen."""
    if L in table:
        return table[L]
    ks = sorted(table)
    lo = max(k for k in ks if k < L)
    hi = min(k for k in ks if k > L)
    return table[lo] + (table[hi] - table[lo]) * (L - lo) / (hi - lo)


def threshold(cpu_fixed, cpu_byte, fill):
    ok = {L: interp(fill, L) < cpu_fixed + cpu_byte * L for L in range(2, 257)}
    T = 257
    for L in range(256, 1, -1):
        if ok[L]:
            T = L
        else:
            break
    return T


PROV_REASON_Z64K = ('Z64K (Emulator): VDC genauer als VICE, aber kein Hardware-Ergebnis; erst Messwerte vom echten C128 '
                    'gelten als endgültig')
CMP_KEYS = ('cpu_byte', 'cpu_lit_byte', 'new_addr', 'fill_fixed', 'fill_byte', 'copy_fixed', 'copy_byte', 'T',
            'vmc_call', 'vmc_row', 'vmc_byte', 'vms1_call', 'vms1_cmd', 'vms1_byte', 'vms8_call', 'vms8_byte')


def vice_reference(outdir):
    """Neueste VICE-Messung in outdir (results/vice3.10-*.json) oder None."""
    import glob
    fs = sorted(glob.glob(os.path.join(outdir, 'vice3.10-*.json')))
    return (fs[-1], json.load(open(fs[-1]))) if fs else None


def compare_md(res, ref):
    """Vergleichstabelle VICE gegen Z64K (Konfigurationen A-D, Zyklen je Operation)."""
    name, r = ref
    o = ['', f'## 7. Vergleich VICE 3.10 gegen Z64K (Zyklen; Referenz `{os.path.basename(name)}`)', '',
         'Verhältnis = Z64K / VICE. `T` = Schwelle in Bytes (257 = nie).', '']
    for cfg in 'ABCD':
        a, b = r['derived'].get(cfg), res['derived'].get(cfg)
        if not a or not b:
            continue
        o += [f'### Konfiguration {cfg}', '', '| Schlüssel | VICE | Z64K | Verhältnis |', '|---|---|---|---|']
        for k in CMP_KEYS:
            x, y = a.get(k), b.get(k)
            if x is None or y is None:
                continue
            ratio = f'{y / x:.2f}' if x else '-'
            o.append(f'| `{k}` | {x:g} | {y:g} | {ratio} |')
        o.append('')
    return o


def evaluate(path, source, outdir, xchk, provisional=True):
    vice_model = not source.startswith('z64k')    # Erwartungswerte (Abschnitt 2) gelten nur fuer VICE
    ver, recs = parse(path)
    today = datetime.date.today()
    warnings = []
    # Leerwerte je Konfiguration
    empty, empty1 = {}, {}
    for r in recs:
        mhz, r36 = (2 if r['cfg'] & 1 else 1), r['cfg'] >> 4
        r['mhz'], r['r36'], r['var'], r['single'] = mhz, r36, (r['cfg'] >> 1) & 1, (r['cfg'] >> 2) & 1
        if r['id'] == 0:
            (empty1 if r['single'] else empty)[(mhz, r36)] = r['raw']
    rows, r36res = [], []
    for r in recs:
        if r['id'] == 11:
            r36res.append({'r36': r['reps'], 'frames': r['par'], 'errors': r['raw']})
            continue
        key = (r['mhz'], r['r36'])
        if r['id'] != 0:
            e = empty1.get(key) if r['single'] else empty.get(key)
            if e is None:
                sys.exit(f'Leerwert fuer Konfiguration {key} fehlt')
            r['us'] = r['raw'] - e
        else:
            r['us'] = r['raw']
        r['us_op'] = r['us'] / r['reps']
        r['cyc_op'] = r['us_op'] * r['mhz']
        rows.append({'case': CASES[r['id']] + ('_single' if r['id'] == 0 and r['single'] else ''),
                     'variant': 'rmw' if r['var'] else 'norm', 'clock_mhz': r['mhz'], 'r36': r['r36'],
                     'param': r['par'], 'reps': r['reps'], 'us_total': r['raw'],
                     'us_per_op': round(r['us_op'], 3), 'cycles_per_op': round(r['cyc_op'], 3)})
    work = [r for r in recs if r['id'] not in (0, 11)]

    def series(cfgkey, cid, var=0, minp=0):
        return {r['par']: r['cyc_op'] for r in work
                if (r['mhz'], r['r36']) == cfgkey and r['id'] == cid and r['var'] == var and r['par'] >= minp}

    derived = {}
    thr = {}
    for key, name in CONFIGS.items():
        if not any((r['mhz'], r['r36']) == key for r in work):
            continue
        d = {}
        # cpu_byte: nur Fenster in der Anzeige (Phase nach vdc_sync fest); ueber alle N gemittelt
        # vermischt die Regression Anzeige- und Austastphase (Spec 10.2).
        s1 = series(key, 1)
        s1d = {n: v for n, v in s1.items() if n <= N_DISP}
        b, a = linreg(list(s1d), list(s1d.values()))
        d['cpu_byte'], d['cpu_run_fixed'] = b, a
        d['cpu_byte_avg'] = s1[256] / 256 if 256 in s1 else None
        s2 = series(key, 2)
        s2d = {n: v for n, v in s2.items() if n <= N_DISP}
        d['cpu_lit_byte'] = linreg(list(s2d), list(s2d.values()))[0]
        d['cpu_lit_byte_avg'] = s2[256] / 256 if 256 in s2 else None
        s3 = series(key, 3)
        d['new_addr'] = s3[0] - b if 0 in s3 else None
        for nm, cid, var, mp in (('fill', 4, 0, 2), ('fill_rmw', 4, 1, 2), ('fill_issue', 5, 0, 2),
                                 ('copy', 6, 0, 1), ('copy_rmw', 6, 1, 1), ('copy_issue', 7, 0, 1)):
            s = series(key, cid, var, mp)
            bb, aa = linreg(list(s), list(s.values()))
            if nm.endswith('_rmw'):
                nm = nm[:-4]
                d[nm + '_fixed_rmw'], d[nm + '_byte_rmw'] = aa, bb
            else:
                d[nm + '_fixed'], d[nm + '_byte'] = aa, bb
        for nm, cid in (('vmc', 8), ('vms1', 9)):
            pts = [(r['par'] & 255, r['par'] >> 8, r['cyc_op']) for r in work
                   if (r['mhz'], r['r36']) == key and r['id'] == cid]
            c = lstsq([[1, h, h * w] for w, h, _ in pts], [y for *_, y in pts])
            row = 'row' if nm == 'vmc' else 'cmd'
            d[f'{nm}_call'], d[f'{nm}_{row}'], d[f'{nm}_byte'] = c
        s10 = series(key, 10)
        d['vms8_byte'], d['vms8_call'] = linreg(list(s10), list(s10.values()))
        fd, fr, fi = series(key, 4, 0, 2), series(key, 4, 1, 2), series(key, 5, 0, 2)
        d['T'] = threshold(a, b, fd)
        d['T_rmw'] = threshold(a, b, fr)
        d['T_issue'] = threshold(a, b, fi)
        derived[name] = d
        thr[name] = (series(key, 4, 0, 1), series(key, 4, 1, 1), series(key, 5, 0, 1))
        # Plausibilitaet (Abschnitt 2)
        f_all = series(key, 4, 0, 1)
        if vice_model and 2 in f_all and 256 in f_all:
            v, e = f_all[256] - f_all[2], 254 * 0.66
            if not 0.4 * e <= v <= 2.5 * e:
                warnings.append(f'Konfig {name}: fill_done(256)-fill_done(2) = {v:.0f} Zyklen, erwartet etwa {e:.0f}')
        c_all = series(key, 6, 0, 1)
        if vice_model and 1 in c_all and 256 in c_all:
            v, e = c_all[256] - c_all[1], 255 * 1.2
            if not 0.4 * e <= v <= 2.5 * e:
                warnings.append(f'Konfig {name}: copy_done(256)-copy_done(1) = {v:.0f} Zyklen, erwartet etwa {e:.0f}')
        mhz = key[0]
        if provisional and vice_model and not 43 * mhz <= b <= 62 * mhz:   # VICE: 43 Takte zu 1 MHz busy + Abfrage/Refresh
            warnings.append(f'Konfig {name}: cpu_byte = {b:.1f} Zyklen, erwartet in VICE {43 * mhz} bis {62 * mhz}')
    # Gegenprobe CIA gegen Monitor-Stoppuhr
    xc = []
    for x in xchk:
        mhz, r36 = (2 if x['cfg'] & 1 else 1), x['cfg'] >> 4
        cia = next((r['raw'] for r in recs if r['id'] == 1 and r['par'] == 256 and (r['mhz'], r['r36']) == (mhz, r36)), None)
        if cia is None:
            continue
        sw_us = x['sw_cycles'] / 1.0       # Stoppuhr in Zyklen des Hauptprozessors
        e = {'config': CONFIGS.get((mhz, r36), '?'), 'clock_mhz': mhz, 'cia_us': cia,
             'monitor_cycles': x['sw_cycles'], 'ratio_cycles_per_cia_us': round(x['sw_cycles'] / cia, 4)}
        if True:                            # Stoppuhr zaehlt auch bei 2 MHz in 1-MHz-Takten (maincpu_clk)
            e['diff_us'] = round(cia - FRAME_CYCLES / mhz - x['sw_cycles'], 1)
            if abs(e['diff_us']) > 20:
                warnings.append(f'Gegenprobe Konfig {e["config"]}: CIA {cia} us gegen Monitor {x["sw_cycles"]} Zyklen, '
                                f'Abweichung {e["diff_us"]} us (> 20)')
        xc.append(e)
    if not xchk and provisional and vice_model:          # Gegenprobe gibt es nur in VICE
        warnings.append('Gegenprobe: keine Stoppuhrwerte erhalten')
    cfgA = derived.get('A', {})
    engine = {'config': 'A', 'cycles_clock_mhz': 2}
    for k in ('cpu_byte', 'cpu_byte_avg', 'cpu_lit_byte', 'cpu_run_fixed', 'new_addr', 'fill_fixed', 'fill_byte',
              'copy_fixed', 'copy_byte', 'T'):
        engine[k] = round(cfgA[k], 3) if isinstance(cfgA.get(k), float) else cfgA.get(k)
    for dd in derived.values():
        for k, v in list(dd.items()):
            if isinstance(v, float):
                dd[k] = round(v, 3)
    meta = {'source': source, 'date': today.isoformat(), 'bench_version': ver, 'vdcbasic': 'v2h',
            'provisional': bool(provisional),
            'provisional_reason': (PROV_REASON if vice_model else PROV_REASON_Z64K) if provisional else '',
            'vdc_config': {'r0': 127, 'r4': 159, 'r6': 120, 'r7': 133, 'r9': 1, 'bitmap': True, 'ram64k': True},
            'crosscheck': xc, 'warnings': warnings}
    res = {'meta': meta, 'records': rows, 'derived': derived, 'engine': engine, 'r36': r36res}
    os.makedirs(outdir, exist_ok=True)
    stem = os.path.join(outdir, f'{source}-{today.strftime("%Y%m%d")}')
    json.dump(res, open(stem + '.json', 'w'), indent=1)
    md = markdown(res, derived, thr, work)
    ref = None if vice_model else vice_reference(outdir)
    if ref:
        md += '\n'.join(compare_md(res, ref)) + '\n'
    open(stem + '.md', 'w').write(md)
    print(f'Ergebnis: {stem}.json, {stem}.md ({len(rows)} Datensaetze, {len(warnings)} Warnungen)')
    for w in warnings:
        print('WARNUNG:', w)
    return res


def markdown(res, derived, thr, work):
    meta = res['meta']
    o = [f'# VDC-Messung ({meta["source"]}, {meta["date"]})', '']
    if meta['provisional']:
        o += ['**vorläufig (' + ('Z64K' if meta['source'].startswith('z64k') else 'VICE') + ')**: '
              + meta['provisional_reason'] + '.', '']
    o += [f'Messprogramm Version {meta["bench_version"]}, VDC-BASIC {meta["vdcbasic"]}. '
          'Alle Werte in CPU-Zyklen der jeweiligen Konfiguration (µs in Klammern).', '']
    names = [n for n in 'ABCD' if n in derived]
    mhz = {'A': 2, 'B': 2, 'C': 1, 'D': 1}
    cfgdesc = {'A': '2 MHz, Reg 36 = 0 (Engine)', 'B': '2 MHz, Reg 36 = 5', 'C': '1 MHz, Reg 36 = 0', 'D': '1 MHz, Reg 36 = 5'}
    o += ['Konfigurationen: ' + '; '.join(f'{n} = {cfgdesc[n]}' for n in names), '']

    def cell(n, k):
        v = derived[n].get(k)
        if v is None:
            return '-'
        if k.startswith('T'):
            return str(v)
        return f'{v:.1f} ({v / mhz[n]:.1f})'
    o += ['## 1. Kostenparameter je Konfiguration', '', '| Schlüssel | ' + ' | '.join(names) + ' |',
          '|---|' + '---|' * len(names)]
    keys = [k for k in derived[names[0]] if not k.startswith('T') and not k.startswith('vm')]
    for k in keys:
        o.append(f'| `{k}` | ' + ' | '.join(cell(n, k) for n in names) + ' |')
    o.append('')
    if 'A' in derived:
        fd, fr, fi = thr['A']
        d = derived['A']
        o += ['## 2. CPU gegen Block fill (Konfiguration A, Zyklen)', '',
              '| L | t_cpu | t_fill norm | t_fill rmw | t_issue | schneller (norm) |', '|---|---|---|---|---|---|']
        for L in M:
            tc = d['cpu_run_fixed'] + d['cpu_byte'] * L
            a, b, c = (t[L] if L in t else interp(t, L) for t in (fd, fr, fi))
            o.append(f'| {L} | {tc:.1f} | {a:.1f} | {b:.1f} | {c:.1f} | {"Fill" if a < tc else "CPU"} |')
        o.append('')
    o += ['## 3. Schwelle T (kleinste Lauflänge, ab der Fill für alle größeren L schneller ist; 257 = nie)', '',
          '| Konfiguration | T | T_rmw | T_issue |', '|---|---|---|---|']
    for n in names:
        o.append(f'| {n} | {derived[n]["T"]} | {derived[n]["T_rmw"]} | {derived[n]["T_issue"]} |')
    o += ['', '## 4. VMC / VMS (Zyklen, µs in Klammern)', '',
          '| Befehl | je Byte | je Zeile/Befehl | je Aufruf |', '|---|---|---|---|']
    for n in names:
        d = derived[n]
        m = mhz[n]
        for nm, row in (('vmc', 'row'), ('vms1', 'cmd')):
            o.append(f'| {nm} ({n}) | {d[nm + "_byte"]:.1f} ({d[nm + "_byte"] / m:.2f}) | '
                     f'{d[nm + "_" + row]:.1f} ({d[nm + "_" + row] / m:.1f}) | {d[nm + "_call"]:.1f} ({d[nm + "_call"] / m:.1f}) |')
        o.append(f'| vms8 ({n}) | {d["vms8_byte"]:.1f} ({d["vms8_byte"] / m:.2f}) | (je Byte = je Befehl) | '
                 f'{d["vms8_call"]:.1f} ({d["vms8_call"] / m:.1f}) |')
    o += ['', '## 5. Reg 36: Fehler (Bytes von 8192)', '']
    fs = sorted({x['frames'] for x in res['r36']})
    if fs:
        o += ['| r \\ F | ' + ' | '.join(str(f) for f in fs) + ' |', '|---|' + '---|' * len(fs)]
        for r in sorted({x['r36'] for x in res['r36']}):
            o.append(f'| {r} | ' + ' | '.join(str(next(x['errors'] for x in res['r36'] if x['r36'] == r and x['frames'] == f)) for f in fs) + ' |')
    else:
        o.append('(Reg-36-Test nicht ausgeführt)')
    o += ['', '## 6. Warnungen und Gegenprobe', '']
    for e in meta['crosscheck']:
        o.append(f'- Gegenprobe Konfig {e["config"]} ({e["clock_mhz"]} MHz): CIA {e["cia_us"]} µs, Monitor-Stoppuhr '
                 f'{e["monitor_cycles"]} Zyklen, Verhältnis {e["ratio_cycles_per_cia_us"]}'
                 + (f', Abweichung {e["diff_us"]} µs' if 'diff_us' in e else ''))
    for w in meta['warnings']:
        o.append(f'- WARNUNG: {w}')
    if not meta['warnings']:
        o.append('- keine Warnungen')
    return '\n'.join(o) + '\n'


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('file')
    ap.add_argument('--source', default='vice3.10')
    ap.add_argument('--out', default=os.path.join(os.path.dirname(os.path.abspath(__file__)), 'results'))
    a = ap.parse_args()
    evaluate(a.file, a.source, a.out, [], provisional=(a.source != 'c128'))
