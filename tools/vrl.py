#!/usr/bin/env python3
"""VRL: RLE-Strom für `RXV` (VDC-BASIC v3) – Encoder, Decoder, Statistik.

Strom (Token; Wörter Low/High):
  $00            Ende
  $01-$7F  n Bytes   n Literale (1..127)
  $80-$FD  v         Lauf: L = b-$80+2 (2..127) mal v
  $FE      lo hi     Skip: Ziel um n weiter (0 = 65536), nichts schreiben
  $FF      lo hi v   langer Lauf: L = hi*256+lo (0 = 65536) mal v
Dateirahmen (PRG): Ladeadresse (2), "VR", Version 1, entpackte Länge U (Wort), Strom.
Skips am Stromende werden nicht kodiert; U <= 65535. Details: docs/specs/P1-vrl.md,
docs/formats.md (VRL v1).

Aufruf:
  vrl.py encode IN OUT [--load ADDR] [--raw] [--in-prg]
  vrl.py rect   IN OUT --width W --height H --stride S [--mask MASK] [--load ADDR] [--raw] [--in-prg]
  vrl.py decode IN OUT [--raw] [--base BASE] [--out-prg ADDR]
  vrl.py stats  IN [--raw] [-T N] [--bench P0.json] [--json]
Exitcode: 0 ok, 1 Formatfehler, 2 falscher Aufruf.
"""
import argparse
import collections
import json
import struct
import sys

SKIP = None
VERSION = 1
MAX_U = 65535

Token = collections.namedtuple("Token", "kind length value offset")

PLACEHOLDER = {
    "tok": 30, "cpu_lit_byte": 22, "cpu_byte": 15, "cpu_run_fixed": 10,
    "new_addr": 80, "fill_fixed": 200, "fill_byte": 1, "fill_chunk": 30,
}


def _check_len(n):
    if n > MAX_U:
        raise ValueError("VRL: entpackte Länge über 65535")


def encode(data):
    """Folge aus int | None -> Strom (inkl. $00), größenoptimal (Spec 4.2)."""
    seq = list(data)
    for v in seq:
        if v is not None and not (isinstance(v, int) and 0 <= v <= 255):
            raise ValueError("VRL: Wert außerhalb 0..255: %r" % (v,))
    while seq and seq[-1] is None:      # Skips am Ende entfallen
        seq.pop()
    n = len(seq)
    _check_len(n)

    # Hilfstabellen rückwärts in O(n)
    skip = [0] * (n + 1)    # Länge der Skip-Strecke ab i
    rep = [0] * (n + 1)     # Länge der Wiederholung gleicher Werte ab i (kein SKIP)
    lit = [0] * (n + 1)     # längste Strecke ohne SKIP ab i
    for i in range(n - 1, -1, -1):
        v = seq[i]
        if v is None:
            skip[i] = skip[i + 1] + 1
        else:
            lit[i] = lit[i + 1] + 1
            rep[i] = rep[i + 1] + 1 if (i + 1 < n and seq[i + 1] == v) else 1

    # best[i] = Bytes << 20 | Token (Bytes zuerst, dann Anzahl Token).
    # Kandidatenschlüssel je Zielposition j (best << 16 | 65535 - j): kleiner = besser, bei
    # Gleichstand größeres j = mehr Abdeckung. Läufe (feste Kosten 2) und Literale (Kosten
    # 1 + m = 1 + j - i, daher best[j] + j*UNIT) liegen in getrennten Feldern, damit die
    # Minimumsuche über eine Scheibe in C läuft (O(n * 127) ohne Python-Schleife je Kandidat).
    UNIT = 1 << 20
    INF = 1 << 60
    best = [INF] * (n + 1)
    key_run = [INF << 16] * (n + 1)
    key_lit = [INF << 16] * (n + 1)
    choice = [None] * (n + 1)   # (Art, Länge)

    def publish(j, val):
        best[j] = val
        key_run[j] = (val << 16) | (65535 - j)
        key_lit[j] = ((val + j * UNIT) << 16) | (65535 - j)

    publish(n, 0)
    MASK = 0xFFFF
    for i in range(n - 1, -1, -1):
        if skip[i]:
            k = skip[i]
            publish(i, best[i + k] + 3 * UNIT + 1)
            choice[i] = ("skip", k)
            continue
        R = rep[i]
        cands = []      # (Bytes+Token, -Abdeckung, Art 0 = Lauf / 1 = Literal, Art, Länge)
        if R >= 2:
            kk = min(key_run[i + 2:i + min(127, R) + 1])
            L = 65535 - (kk & MASK) - i
            cands.append((best[i + L] + 2 * UNIT + 1, -L, 0, "run", L))
        if R > 127:
            cands.append((best[i + R] + 4 * UNIT + 1, -R, 0, "run", R))
        kk = min(key_lit[i + 1:i + min(127, lit[i]) + 1])
        m = 65535 - (kk & MASK) - i
        cands.append((best[i + m] + (1 + m) * UNIT + 1, -m, 1, "lit", m))
        c = min(cands)
        publish(i, c[0])
        choice[i] = (c[3], c[4])

    out = bytearray()
    i = 0
    while i < n:
        kind, L = choice[i]
        if kind == "skip":
            out += bytes((0xFE, L & 255, L >> 8))
        elif kind == "run":
            if L <= 127:
                out += bytes((0x80 + L - 2, seq[i]))
            else:
                out += bytes((0xFF, L & 255, L >> 8, seq[i]))
        else:
            out.append(L)
            out += bytes(seq[i:i + L])
        i += L
    out.append(0)
    return bytes(out)


def encode_rect(data, width, height, stride, mask=None):
    """Rechteck width x height mit Stride (und optionaler Maske) kodieren (Spec 4.5)."""
    if width < 1 or height < 1 or stride < width:
        raise ValueError("VRL: ungültige Rechteckgeometrie")
    if stride > 65535:
        raise ValueError("VRL: stride über 65535")
    data = list(data)
    if len(data) != width * height:
        raise ValueError("VRL: Datenlänge passt nicht zu width x height")
    if mask is not None:
        mask = list(mask)
        if len(mask) != len(data):
            raise ValueError("VRL: Maskenlänge passt nicht zu width x height")
    # Zielfolge nur bis hinter das letzte geschriebene Byte (Skips am Ende entfallen ohnehin),
    # damit sie bei großem Stride und fast leerer Maske klein bleibt.
    last = len(data) - 1 if mask is None else max((k for k, m in enumerate(mask) if m), default=-1)
    total = 0 if last < 0 else (last // width) * stride + last % width + 1
    _check_len(total)
    seq = [SKIP] * total
    for k in range(last + 1):
        if mask is None or mask[k]:
            seq[(k // width) * stride + k % width] = data[k]
    return encode(seq)


def decode_tokens(stream):
    """Token bis einschließlich Ende als Liste von Token(kind, length, value, offset)."""
    s = bytes(stream)
    toks = []
    p = 0
    n = len(s)
    while True:
        if p >= n:
            raise ValueError("VRL: unerwartetes Ende bei Offset %d" % p)
        b = s[p]
        if b == 0:
            toks.append(Token("end", 0, None, p))
            return toks
        if b <= 0x7F:
            if p + 1 + b > n:
                raise ValueError("VRL: Literal über das Stromende hinaus bei Offset %d" % p)
            toks.append(Token("lit", b, s[p + 1:p + 1 + b], p))
            p += 1 + b
        elif b <= 0xFD:
            if p + 2 > n:
                raise ValueError("VRL: Lauf über das Stromende hinaus bei Offset %d" % p)
            toks.append(Token("run", b - 0x80 + 2, s[p + 1], p))
            p += 2
        elif b == 0xFE:
            if p + 3 > n:
                raise ValueError("VRL: Skip über das Stromende hinaus bei Offset %d" % p)
            toks.append(Token("skip", (s[p + 1] | s[p + 2] << 8) or 65536, None, p))
            p += 3
        else:
            if p + 4 > n:
                raise ValueError("VRL: langer Lauf über das Stromende hinaus bei Offset %d" % p)
            toks.append(Token("lrun", (s[p + 1] | s[p + 2] << 8) or 65536, s[p + 3], p))
            p += 4


def decode(stream, base=None):
    """Strom ausführen; Kopie von base (ggf. mit 0 verlängert), Skips lassen base unverändert."""
    out = bytearray(base) if base is not None else bytearray()
    pos = 0
    for t in decode_tokens(stream):
        if t.kind == "end":
            break
        if pos + t.length > 65536:
            raise ValueError("VRL: Ziel über 65536 hinaus bei Offset %d" % t.offset)
        if len(out) < pos + t.length:
            out.extend(bytes(pos + t.length - len(out)))
        if t.kind == "lit":
            out[pos:pos + t.length] = t.value
        elif t.kind in ("run", "lrun"):
            out[pos:pos + t.length] = bytes((t.value,)) * t.length
        pos += t.length
    return out


def unpacked_length(stream):
    """U: Summe aller Literal-, Lauf- und Skip-Längen."""
    return sum(t.length for t in decode_tokens(stream))


def pack_file(stream, load=0x2000):
    """PRG mit Kopf (Spec 3); U aus dem Strom."""
    u = unpacked_length(stream)
    _check_len(u)
    return struct.pack("<H", load) + b"VR" + bytes((VERSION,)) + struct.pack("<H", u) + bytes(stream)


def unpack_file(data):
    """-> (load, version, U, stream); prüft Magic, Version und U."""
    data = bytes(data)
    if len(data) < 8:
        raise ValueError("VRL: Datei zu kurz")
    load, = struct.unpack_from("<H", data, 0)
    if data[2:4] != b"VR":
        raise ValueError("VRL: falsches Magic")
    version = data[4]
    if version != VERSION:
        raise ValueError("VRL: unbekannte Version %d" % version)
    u, = struct.unpack_from("<H", data, 5)
    stream = data[7:]
    toks = decode_tokens(stream)
    stream = stream[:toks[-1].offset + 1]
    if sum(t.length for t in toks) != u:
        raise ValueError("VRL: entpackte Länge im Kopf passt nicht zum Strom")
    return load, version, u, stream


def _params(params):
    p = dict(PLACEHOLDER)
    if params:
        p.update({k: v for k, v in params.items() if k in PLACEHOLDER})
    return p


def _cpu_cost(L, p):
    return p["cpu_run_fixed"] + L * p["cpu_byte"]


def _fill_cost(L, p):
    return p["fill_fixed"] + L * p["fill_byte"] + (-(-L // 256) - 1) * p["fill_chunk"]


def threshold(params=None):
    """Kleinstes L >= 2, ab dem Block fill gewinnt (Spec 8.2); sonst 65537."""
    p = _params(params)
    if not p["fill_byte"] < p["cpu_byte"]:
        return 65537
    for L in range(2, 65537):
        if _fill_cost(L, p) < _cpu_cost(L, p):
            return L
    return 65537


def load_bench(path):
    """`engine` aus der P0-JSON lesen; fehlende Schlüssel bleiben Platzhalter, T wird übernommen."""
    with open(path, encoding="utf-8") as f:
        doc = json.load(f)
    if not isinstance(doc, dict) or not isinstance(doc.get("engine"), dict):
        raise ValueError("VRL: Schlüssel \"engine\" fehlt in %s" % path)
    eng = doc["engine"]
    res = dict(PLACEHOLDER)
    for k, v in eng.items():
        if k in PLACEHOLDER or k == "T":
            res[k] = v
    return res


HIST_CLASSES = [(2, 2), (3, 4), (5, 8), (9, 16), (17, 32), (33, 64), (65, 128), (129, 256)]


def _hist_label(i):
    return "> 256" if i == len(HIST_CLASSES) else (
        str(HIST_CLASSES[i][0]) if HIST_CLASSES[i][0] == HIST_CLASSES[i][1]
        else "%d-%d" % HIST_CLASSES[i])


def stats(stream, T=None, params=None):
    """Statistik und Zyklenschätzung (Spec 8)."""
    params = params or {}
    p = _params(params)
    if T is None:
        T = params.get("T") if params.get("T") is not None else threshold(p)
    toks = decode_tokens(stream)
    tokens = {"lit": 0, "run": 0, "lrun": 0, "skip": 0}
    bk = {"lit": 0, "run_cpu": 0, "run_fill": 0}
    hist = [0] * (len(HIST_CLASSES) + 1)
    written = skipped = 0
    cycles = 0
    for t in toks:
        if t.kind == "end":
            continue
        cycles += p["tok"]
        tokens[t.kind] += 1
        if t.kind == "lit":
            written += t.length
            bk["lit"] += t.length
            cycles += t.length * p["cpu_lit_byte"]
        elif t.kind == "skip":
            skipped += t.length
            cycles += p["new_addr"]
        else:
            L = t.length
            written += L
            for i, (lo, hi) in enumerate(HIST_CLASSES):
                if lo <= L <= hi:
                    hist[i] += 1
                    break
            else:
                hist[-1] += 1
            if L < T:
                bk["run_cpu"] += L
                cycles += _cpu_cost(L, p)
            else:
                bk["run_fill"] += L
                cycles += _fill_cost(L, p)
    return {
        "stream_bytes": len(stream),
        "unpacked": written + skipped,
        "written": written,
        "skipped": skipped,
        "ratio": len(stream) / written if written else None,
        "tokens": tokens,
        "bytes_by_kind": bk,
        "run_hist": {_hist_label(i): c for i, c in enumerate(hist)},
        "T": T,
        "cycles": int(round(cycles)),
    }


# ---------------------------------------------------------------- CLI

def _addr(s):
    s = s.strip()
    v = int(s[1:], 16) if s.startswith("$") else int(s, 0)
    if not 0 <= v <= 0xFFFF:
        raise argparse.ArgumentTypeError("Adresse außerhalb 0..65535: " + s)
    return v


def _read(path, skip_prg=False):
    with open(path, "rb") as f:
        d = f.read()
    if skip_prg:
        if len(d) < 2:
            raise ValueError("VRL: PRG-Eingabe ohne Ladeadresse")
        d = d[2:]
    return d


def _write(path, data):
    with open(path, "wb") as f:
        f.write(data)


def _stream_in(args):
    d = _read(args.IN)
    return d if args.raw else unpack_file(d)[3]


def _emit(args, stream):
    _write(args.OUT, stream if args.raw else pack_file(stream, args.load))


def _cmd_encode(a):
    _emit(a, encode(_read(a.IN, a.in_prg)))


def _cmd_rect(a):
    mask = _read(a.mask) if a.mask else None
    _emit(a, encode_rect(_read(a.IN, a.in_prg), a.width, a.height, a.stride, mask))


def _cmd_decode(a):
    base = _read(a.base) if a.base else None
    out = decode(_stream_in(a), base)
    if a.out_prg is not None:
        out = struct.pack("<H", a.out_prg) + bytes(out)
    _write(a.OUT, bytes(out))


def _cmd_stats(a):
    params = load_bench(a.bench) if a.bench else None
    st = stats(_stream_in(a), a.T, params)
    if a.json:
        print(json.dumps(st, indent=2))
        return
    for k in ("stream_bytes", "unpacked", "written", "skipped"):
        print("%-14s %d" % (k, st[k]))
    print("%-14s %s" % ("ratio", "-" if st["ratio"] is None else "%.3f" % st["ratio"]))
    print("%-14s %s" % ("tokens", "  ".join("%s=%d" % kv for kv in st["tokens"].items())))
    print("%-14s %s" % ("bytes_by_kind", "  ".join("%s=%d" % kv for kv in st["bytes_by_kind"].items())))
    print("run_hist")
    for k, v in st["run_hist"].items():
        print("  %-8s %d" % (k, v))
    print("%-14s %d" % ("T", st["T"]))
    print("%-14s %d" % ("cycles", st["cycles"]))


def main(argv=None):
    ap = argparse.ArgumentParser(description="VRL-Werkzeug (RLE-Strom für RXV)")
    sub = ap.add_subparsers(dest="cmd", required=True)

    def common(p, out=True):
        p.add_argument("IN")
        if out:
            p.add_argument("OUT")

    def enc_opts(p):
        p.add_argument("--load", type=_addr, default=0x2000)
        p.add_argument("--raw", action="store_true")
        p.add_argument("--in-prg", action="store_true")

    p = sub.add_parser("encode"); common(p); enc_opts(p); p.set_defaults(fn=_cmd_encode)
    p = sub.add_parser("rect"); common(p); enc_opts(p)
    p.add_argument("--width", type=int, required=True)
    p.add_argument("--height", type=int, required=True)
    p.add_argument("--stride", type=int, required=True)
    p.add_argument("--mask")
    p.set_defaults(fn=_cmd_rect)
    p = sub.add_parser("decode"); common(p)
    p.add_argument("--raw", action="store_true")
    p.add_argument("--base")
    p.add_argument("--out-prg", type=_addr, default=None)
    p.set_defaults(fn=_cmd_decode)
    p = sub.add_parser("stats"); common(p, out=False)
    p.add_argument("--raw", action="store_true")
    p.add_argument("-T", type=int, default=None)
    p.add_argument("--bench")
    p.add_argument("--json", action="store_true")
    p.set_defaults(fn=_cmd_stats)

    args = ap.parse_args(argv)
    try:
        args.fn(args)
    except (ValueError, OSError) as e:
        print("vrl.py: %s" % e, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
