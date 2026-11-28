#!/usr/bin/env python3
"""Turn the VDC-BASIC commands of a BASIC 7.0 listing into USR/SYS calls of vdcsys.s, so that Blitz!
compiles the whole program (no :: and no interpreter for the extension).

usage: vdc2sys.py in.bas out.bas [--init-line N]      (default N = 25)

- A statement  VDL a,b,c,d,e  (with or without :: in front) becomes
  YY=USR(a):YY=USR(b):YY=USR(c):YY=USR(d):YY=USR(e):SYS 4867   (each USR puts one parameter on the
  stack of vdcsys, the SYS executes the command). Supported: VDL VMC VMF SYN RGW RGO RGA RST DISP ATTR
  VDM VDR; VMC and VMF only in their three parameter form.
- RGD(e) in an expression is computed in front of the statement: YY=USR(e):SYS 4888:Y0=PEEK(4906),
  and Y0 (Y1, ...) takes its place.
- IF ... THEN::cmd becomes IF ... THEN <the converted statements>.
- Line N is added: BLOAD"VDCSYS",B0:SYS 4864 (loads and installs vdcsys; VDC-BASIC v3 must be there).
The names YY and Y0..Y9 must not be used by the program (checked). Anything else the converter does not
know (other VDC-BASIC keywords, other forms) is an error with the line number.
"""
import re, sys

CMDS = {'VDL': (4867, 5), 'VMC': (4870, 3), 'VMF': (4873, 3), 'SYN': (4876, None), 'RGW': (4879, 2),
        'RGO': (4882, 2), 'RGA': (4885, 2), 'RST': (4891, 1), 'DISP': (4894, 1), 'ATTR': (4897, 1),
        'VDM': (4900, 3), 'VDR': (4903, 5)}
RGD_SYS, RES = 4888, 4906
OTHER = ['RGD', 'VMD', 'VMW', 'VMA', 'VMO', 'RTV', 'VTR', 'VCC', 'SWP', 'CRSR', 'VCS', 'VMP', 'VCL', 'VMS',
         'VMB', 'VDB', 'RXV']
DUMMY = 'YY'


def split_top(s, sep):
    """Split at sep outside strings and parentheses."""
    out, cur, depth, q = [], '', 0, False
    for ch in s:
        if ch == '"':
            q = not q
        elif not q:
            if ch == '(':
                depth += 1
            elif ch == ')':
                depth -= 1
            elif ch == sep and depth == 0:
                out.append(cur)
                cur = ''
                continue
        cur += ch
    out.append(cur)
    return out


def statements(rest):
    """Statements of a line (split at ':' outside strings); the rest after REM or DATA stays whole."""
    out, cur, q, i = [], '', False, 0
    while i < len(rest):
        ch = rest[i]
        if ch == '"':
            q = not q
        if not q:
            if ch == ':':
                out.append(cur)
                cur = ''
                i += 1
                continue
            up = rest[i:].upper()
            if cur.strip() == '' and (up.startswith('REM') or up.startswith('DATA')):
                cur += rest[i:]
                break
        cur += ch
        i += 1
    out.append(cur)
    return out


class Conv:
    def __init__(self, ln):
        self.ln = ln
        self.tmp = 0

    def err(self, msg):
        sys.exit(f'line {self.ln}: {msg}')

    def hoist_rgd(self, s):
        """RGD(e) -> Yk, with the statements that compute it."""
        pre = []
        while True:
            m = re.search(r'\bRGD\s*\(', s, re.I)
            if not m:
                return pre, s
            depth, j = 1, m.end()
            while depth:
                if j >= len(s):
                    self.err('RGD( without )')
                depth += {'(': 1, ')': -1}.get(s[j], 0)
                j += 1
            inner = s[m.end():j - 1]
            if self.tmp > 9:
                self.err('too many RGD in one line')
            v = f'Y{self.tmp}'
            self.tmp += 1
            pre += [f'{DUMMY}=USR({inner})', f'SYS {RGD_SYS}', f'{v}=PEEK({RES})']
            s = s[:m.start()] + v + s[j:]

    def stmt(self, s):
        """One statement -> list of statements."""
        t = s.strip()
        if not t or t.upper().startswith(('REM', 'DATA')):
            return [s] if t else []
        pre, t = self.hoist_rgd(t)
        m = re.match(r'IF\b(.*?)\bTHEN\b(.*)$', t, re.I)
        if m:
            cond, then = m.group(1), m.group(2).strip()
            if then and not re.match(r'\d', then):
                body = self.stmt(then)
                return pre + [f'IF{cond}THEN ' + ':'.join(body)]
            return pre + [t]
        m = re.match(r'([A-Z]+)\b\s*(.*)$', t, re.I)
        word = m.group(1).upper() if m else ''
        if word in CMDS:
            addr, n = CMDS[word]
            args = [a.strip() for a in split_top(m.group(2), ',')] if m.group(2).strip() else []
            if n is not None and len(args) != n:
                self.err(f'{word} with {len(args)} parameters (supported: {n})')
            if n is None and len(args) % 2:
                self.err('SYN with an odd number of parameters')
            return pre + [f'{DUMMY}=USR({a})' for a in args] + [f'SYS {addr}']
        for w in OTHER:
            if re.match(rf'{w}\b', t, re.I):
                self.err(f'{w} is not supported')
        return pre + [t]


def convert(text, init_line):
    lines, out, used = [], [], set()
    for raw in text.splitlines():
        m = re.match(r'\s*(\d+)\s?(.*)$', raw)
        if not m:
            if raw.strip():
                sys.exit(f'not a BASIC line: {raw!r}')
            continue
        lines.append((int(m.group(1)), m.group(2)))
    for ln, rest in lines:
        if not rest.upper().startswith(('REM', 'DATA')):
            code = re.sub(r'"[^"]*"', '', rest)
            for ident in re.findall(r'\b([A-Z][A-Z0-9]*)', code.upper()):
                used.add(ident[:2])
    if DUMMY in used or any(f'Y{k}' in used for k in range(10)):
        sys.exit('the program uses YY or Y0..Y9')
    if any(ln == init_line for ln, _ in lines):
        sys.exit(f'line {init_line} exists already (--init-line)')
    done = False
    for ln, rest in lines:
        if not done and ln > init_line:
            out.append(f'{init_line} BLOAD"VDCSYS",B0:SYS 4864:REM VDC-BASIC AS USR/SYS (VDC2SYS.PY)')
            done = True
        c = Conv(ln)
        parts = statements(rest)
        res = []
        i = 0
        while i < len(parts):
            p = parts[i]
            # IF ... THEN::cmd: the empty statement after THEN belongs to the THEN part
            if re.search(r'\bTHEN\s*$', p, re.I) and i + 1 < len(parts) and parts[i + 1].strip() == '':
                j = i + 1
                while j < len(parts) and parts[j].strip() == '':
                    j += 1
                if j < len(parts):
                    body = []
                    for q in parts[j:]:
                        body += c.stmt(q)
                    pre, cond = c.hoist_rgd(p)
                    res += pre + [cond.rstrip() + ' ' + ':'.join(body)]
                    break
            res += c.stmt(p)
            i += 1
        line = f'{ln} ' + ':'.join(res)
        if len(line) > 250:
            sys.exit(f'line {ln}: longer than 250 characters after the conversion')
        out.append(line)
    return '\n'.join(out) + '\n'


if __name__ == '__main__':
    args = sys.argv[1:]
    n = 25
    if '--init-line' in args:
        k = args.index('--init-line')
        n = int(args[k + 1])
        del args[k:k + 2]
    if len(args) != 2:
        sys.exit(__doc__)
    open(args[1], 'w').write(convert(open(args[0], encoding='latin-1').read(), n))
