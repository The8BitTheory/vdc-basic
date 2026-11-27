#!/usr/bin/env python3
"""Turn a VDC-BASIC test listing into a version for the Blitz! 128 compiler (runtime image 2).

usage: blitzprep.py in.bas out.bas

- Every statement that starts with a VDC-BASIC instruction ($FE tokens from "BASIC V7.0 VDC.txt")
  gets "::" in front of it: Blitz! hands the text up to the next ":" to the interpreter (extension).
- Function calls VMD(...)/RGD(...) outside such statements are moved in front of their statement:
  IF VMD(1)<>2 THEN ...  ->  ::Q0=VMD(1):IF Q0<>2 THEN ...   (helper variables Q0..Q9;
  an assignment inside an extension comes back to the compiled program, E1b in docs/v3/P2a-relocate.md)
- REM **NE as the first line (no extension list), 63999 GRAPHIC 0 at the end (never executed,
  makes the compiler use runtime image 2 at $4001, E1e).

Exit code 2 with the line number if a VDC-BASIC instruction follows THEN/ELSE directly, if a function
call sits in the THEN/ELSE part of its statement, or if the listing already uses Q0..Q9.
Lines starting with '#' and empty lines are copied unchanged (bastok.py skips them).
"""
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
TOKENFILE = os.path.join(HERE, "..", "BASIC V7.0 VDC.txt")


def load_vdc_words():
    instr, func = [], []
    sect = False
    for line in open(TOKENFILE, encoding="latin-1"):
        line = line.strip()
        if line.startswith("#VDC Basic Tokens"):
            sect = True
            continue
        if not sect:
            continue
        if line == "ExOpcodes":
            break
        if not line or line.startswith("#"):
            continue
        word, tok = line.split(";")[:2]
        (instr if tok.lower().startswith("fe") else func).append(word.upper())
    return sorted(instr, key=len, reverse=True), func


INSTR, FUNCS = load_vdc_words()


class PrepError(Exception):
    pass


def split_statements(body):
    """Split at ':' outside strings; REM and DATA swallow the rest / up to ':'."""
    parts, cur, quote, i = [], "", False, 0
    up = body.upper()
    while i < len(body):
        c = body[i]
        if quote:
            quote = c != '"'
            cur += c
        elif c == '"':
            quote = True
            cur += c
        elif up.startswith("REM", i):
            cur += body[i:]
            break
        elif c == ":":
            parts.append(cur)
            cur = ""
        else:
            cur += c
        i += 1
    parts.append(cur)
    return parts


def mask_strings(s):
    """Replace string contents by spaces so that searches ignore them."""
    return re.sub(r'"[^"]*("|$)', lambda m: '"' + " " * (len(m.group(0)) - 1), s)


def starts_with_instr(stmt):
    u = stmt.lstrip().upper()
    return any(u.startswith(w) for w in INSTR)


def then_else_split(stmt):
    """Position after the first THEN/ELSE keyword (outside strings) or None."""
    m = re.search(r"(THEN|ELSE)", mask_strings(stmt).upper())
    return m.end() if m else None


def find_innermost_call(stmt):
    """(start, end) of a function call without further calls in its arguments, or None."""
    masked = mask_strings(stmt).upper()
    best = None
    for f in FUNCS:
        for m in re.finditer(re.escape(f) + r"\s*\(", masked):
            depth, j = 1, m.end()
            while j < len(masked) and depth:
                depth += {"(": 1, ")": -1}.get(masked[j], 0)
                j += 1
            if depth:
                raise PrepError("unbalanced parentheses")
            inner = masked[m.end():j - 1]
            if not any(re.search(re.escape(g) + r"\s*\(", inner) for g in FUNCS):
                if best is None or m.start() < best[0]:
                    best = (m.start(), j)
    return best


def prep_statement(stmt, qn, prefixed=False):
    """Return list of statements replacing stmt. prefixed: the listing already has '::' in front."""
    s = stmt.strip()
    if not s or s.upper().startswith("REM") or s.upper().startswith("DATA"):
        return [stmt], qn
    if starts_with_instr(s):
        return [s if prefixed else "::" + s], qn
    te = then_else_split(s)
    if te is not None:
        rest = s[te:].lstrip()
        if starts_with_instr(rest):
            raise PrepError("VDC-BASIC instruction after THEN/ELSE")
    hoisted = []
    while True:
        call = find_innermost_call(s)
        if call is None:
            break
        te = then_else_split(s)
        if te is not None and call[0] >= te:
            raise PrepError("function call in the THEN/ELSE part")
        if qn > 9:
            raise PrepError("more than ten function calls in one line")
        var = f"Q{qn}"
        qn += 1
        hoisted.append("::" + var + "=" + s[call[0]:call[1]])
        s = s[:call[0]] + var + s[call[1]:]
    return hoisted + [s], qn


def prep_line(body):
    out, qn = [], 0
    parts = split_statements(body)
    for i, stmt in enumerate(parts):
        # an empty statement in front means the listing itself wrote "::" (e.g. "THEN::VMF ...")
        new, qn = prep_statement(stmt, qn, i > 0 and not parts[i - 1].strip())
        out += new
    return ":".join(out)


def main():
    if len(sys.argv) != 3:
        sys.exit("usage: blitzprep.py in.bas out.bas")
    src, dst = sys.argv[1:]
    out = ["0 rem **ne"]
    for raw in open(src, encoding="utf-8"):
        raw = raw.rstrip("\r\n")
        if not raw.strip() or raw.lstrip().startswith("#"):
            out.append(raw)
            continue
        m = re.match(r"\s*(\d+)\s*(.*)", raw)
        if not m:
            sys.exit("no line number: " + raw)
        if int(m.group(1)) in (0, 63999):
            print(f"{src}: line {m.group(1)} is reserved for blitzprep", file=sys.stderr)
            sys.exit(2)
        if re.search(r"\bQ\d", mask_strings(m.group(2)).upper()):
            print(f"{src}: line {m.group(1)}: uses a helper variable Q0..Q9", file=sys.stderr)
            sys.exit(2)
        try:
            out.append(m.group(1) + " " + prep_line(m.group(2)))
        except PrepError as e:
            print(f"{src}: line {m.group(1)}: {e}", file=sys.stderr)
            sys.exit(2)
    out.append("63999 graphic 0")
    open(dst, "w", encoding="utf-8").write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
