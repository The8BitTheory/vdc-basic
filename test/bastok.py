#!/usr/bin/env python3
"""Tokenise a BASIC 7.0 + VDC-BASIC text listing into a C128 PRG.

usage: bastok.py in.bas out.prg

Lines starting with '#' and empty lines are skipped. Letters are case-insensitive
and become unshifted PETSCII. {n} inside a string inserts the byte n (decimal).
Keywords come from "BASIC V7.0 VDC.txt" in the repository root.
"""
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
TOKENFILE = os.path.join(HERE, "..", "BASIC V7.0 VDC.txt")
START = 0x1C01


def load_tokens():
    toks = {}
    seen_header = False
    for line in open(TOKENFILE, encoding="latin-1"):
        line = line.strip()
        if line == "ExOpcodes":
            break
        if not seen_header:
            seen_header = line.startswith("Token;")
            continue
        if not line or line.startswith("#"):
            continue
        parts = line.split(";")
        toks[parts[0].upper()] = bytes.fromhex(parts[1])
    toks["GO"] = b"\xcb"
    return toks


def tokenize_line(text, toks, maxlen):
    out = bytearray()
    i = 0
    quote = False
    data = False
    up = text.upper()
    while i < len(up):
        c = up[i]
        if quote:
            if c == "{":
                j = up.index("}", i)
                out.append(int(up[i + 1:j]))
                i = j + 1
                continue
            if c == '"':
                quote = False
            out.append(ord(c))
            i += 1
            continue
        if c == '"':
            quote = True
            out.append(ord(c))
            i += 1
            continue
        if data:
            if c == ":":
                data = False
            out.append(ord(c))
            i += 1
            continue
        if c == " ":
            out.append(32)
            i += 1
            continue
        for n in range(min(maxlen, len(up) - i), 0, -1):
            word = up[i:i + n]
            if word in toks:
                out += toks[word]
                i += n
                if word == "REM":
                    out += up[i:].encode("latin-1")
                    i = len(up)
                elif word == "DATA":
                    data = True
                break
        else:
            out.append(ord(c))
            i += 1
    return bytes(out)


def main():
    src, dst = sys.argv[1], sys.argv[2]
    toks = load_tokens()
    maxlen = max(map(len, toks))
    prg = bytearray([START & 255, START >> 8])
    addr = START
    for raw in open(src, encoding="utf-8"):
        raw = raw.rstrip("\r\n")
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        m = re.match(r"\s*(\d+)\s*(.*)", raw)
        if not m:
            sys.exit("no line number: " + raw)
        body = tokenize_line(m.group(2), toks, maxlen)
        if len(body) > 250:
            sys.exit("line too long: " + m.group(1))
        addr += len(body) + 5
        prg += bytes([addr & 255, addr >> 8])
        num = int(m.group(1))
        prg += bytes([num & 255, num >> 8]) + body + b"\0"
    prg += b"\0\0"
    open(dst, "wb").write(prg)


if __name__ == "__main__":
    main()
