#!/usr/bin/env python3
"""Messprogramm vdcbench in VICE (x128, headless) bauen, starten, auswerten.

Aufruf: run_bench.py [--short] [--no-r36] [--port N] [--out DIR] [--timeout S]
                     [--from-file BENCHRES.prg --source c128]
Siehe README.md und docs/v3/P0-bench.md (Abschnitt 8).

Ereignisgesteuert: Haltepunkte ueber den Remote-Monitor, keine festen Wartezeiten.
Das Timeout ist nur ein Waechter gegen Haenger.
"""
import argparse, os, re, socket, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
BUILD = os.path.join(ROOT, 'build', 'bench')
sys.path.insert(0, HERE)
import evaluate

PROMPT = re.compile(rb'\(C:\$([0-9a-f]{4})\) ')
SYMS = ['remember_mem_conf', 'saved_mem_conf', 'vmc_execute', 'vms_execute']


def run(cmd, **kw):
    r = subprocess.run(cmd, **kw)
    if r.returncode:
        sys.exit(f'Fehler ({r.returncode}): {" ".join(cmd)}')


class Monitor:
    """Remote-Monitor von VICE (nach test/vice_drive.py im Engine-Repo)."""

    def __init__(self, port, tmo, proc):
        self.deadline = time.time() + tmo   # Waechter fuer den ganzen Lauf, nicht je recv
        t0 = time.time()
        while True:
            if proc.poll() is not None:
                raise RuntimeError(f'eigener VICE-Prozess vor dem Monitor-Verbindungsaufbau beendet ({proc.returncode})')
            try:
                self.s = socket.create_connection(('127.0.0.1', port))
                break
            except OSError:
                if time.time() - t0 > 30:
                    raise
                time.sleep(0.05)        # nur bis VICE den Port oeffnet
        self.buf = b''
        self.pc = None
        self.s.sendall(b'\n')           # haelt die laufende Emulation an: zwei Prompts
        self._prompt(2)

    def _prompt(self, n=1):
        while len(PROMPT.findall(self.buf)) < n:
            rest = self.deadline - time.time()
            if rest <= 0:
                raise TimeoutError('Waechter-Timeout (--timeout) abgelaufen')
            self.s.settimeout(rest)
            try:
                x = self.s.recv(65536)
            except socket.timeout:
                raise TimeoutError('Waechter-Timeout (--timeout) abgelaufen') from None
            if not x:
                raise EOFError('VICE beendet')
            self.buf += x
        end = [m.end() for m in PROMPT.finditer(self.buf)][n - 1]
        out, self.buf = self.buf[:end].decode(errors='replace'), self.buf[end:]
        return out

    def cmd(self, c):
        self.s.sendall(c.encode() + b'\n')
        return self._prompt()

    def go(self):
        """Weiterlaufen bis zum naechsten Haltepunkt; liefert die PC aus dem Prompt."""
        self.s.sendall(b'x\n')
        out = self._prompt()
        self.pc = int(re.findall(r'\(C:\$([0-9a-f]{4})\)', out)[-1], 16)
        return self.pc

    def mem(self, a, n=1):
        t = self.cmd(f'm {a:04x} {a + n - 1:04x}')
        data = {}
        for mm in re.finditer(r'>C:([0-9a-f]{4})((?:\s+[0-9a-f]{2}){1,16})', t):
            for k, x in enumerate(mm.group(2).split()):
                data[int(mm.group(1), 16) + k] = int(x, 16)
        return [data[a + k] for k in range(n)]

    def stopwatch(self):
        t = self.cmd('r')
        return int(re.findall(r'(\d+)\s*$', t.strip().splitlines()[-2])[0])

    def bp(self, a):
        return re.search(r'BREAK: (\d+)', self.cmd(f'break {a:04x}')).group(1)


def port_free(p):
    s = socket.socket()
    # wie VICE (socket.c) mit SO_REUSEADDR: TIME_WAIT des Vorlaufs gilt nicht als belegt,
    # ein lauschender fremder VICE weiterhin schon
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        s.bind(('127.0.0.1', p))
        return True
    except OSError:
        return False
    finally:
        s.close()


def choose_port(arg):
    if arg is not None:
        if not port_free(arg):
            print(f'Port {arg} ist belegt (explizit angegeben)', file=sys.stderr)
            sys.exit(2)
        return arg
    p0 = int(os.environ.get('DBE_VICE_PORT', '6631'))
    if port_free(p0):
        return p0
    for p in range(6631, 6650):
        if port_free(p):
            print(f'Port {p0} belegt, nehme {p}', flush=True)
            return p
    print('Kein freier Port in 6631-6649', file=sys.stderr)
    sys.exit(2)


def build():
    os.makedirs(BUILD, exist_ok=True)
    env = dict(os.environ, ACME=os.path.expanduser('~/.local/share/acme/ACME_Lib'))
    run(['acme', '-f', 'cbm', '-o', 'build/bench/vdcbasic2h.bin', '-l', 'build/bench/vdcbasic2h.sym',
         'src/vdcbasic.asm'], cwd=ROOT, env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if open(os.path.join(BUILD, 'vdcbasic2h.bin'), 'rb').read() != open(os.path.join(ROOT, 'vdcbasic2h.bin'), 'rb').read():
        sys.exit('build/bench/vdcbasic2h.bin weicht von vdcbasic2h.bin ab')
    sym = open(os.path.join(BUILD, 'vdcbasic2h.sym')).read()
    lines = []
    for n in SYMS:
        m = re.search(r'^\s*' + n + r'\s*=\s*\$([0-9a-fA-F]+)', sym, re.M)
        if not m:
            sys.exit(f'Symbol {n} fehlt in der ACME-Symbolliste')
        lines.append(f'{n} = ${m.group(1)}')
    open(os.path.join(BUILD, 'vdcbasic_syms.inc'), 'w').write('\n'.join(lines) + '\n')
    b = 'build/bench/'
    run(['ca65', '-t', 'none', '-I', 'build/bench', '-o', b + 'vdcbench.o', 'test/bench/vdcbench.s'], cwd=ROOT)
    run(['ld65', '-C', 'test/bench/vdcbench.cfg', '-Ln', b + 'vdcbench.lbl', '-m', b + 'vdcbench.map',
         '-o', b + 'vdcbench.prg', b + 'vdcbench.o'], cwd=ROOT)
    for n in ('loader', 'loader-hw'):
        run(['python3', os.path.join(ROOT, 'test', 'bastok.py'), os.path.join(HERE, n + '.bas'),
             os.path.join(BUILD, n + '.prg')])
    d64 = os.path.join(BUILD, 'bench.d64')     # auch fuer echte Hardware: LOAD"LOADER-HW",8 / RUN
    if os.path.exists(d64):
        os.remove(d64)
    run(['c1541', '-format', 'bench,01', 'd64', d64, '-write', os.path.join(BUILD, 'vdcbasic2h.bin'), 'vdcbasic2h',
         '-write', os.path.join(BUILD, 'vdcbench.prg'), 'vdcbench',
         '-write', os.path.join(BUILD, 'loader-hw.prg'), 'loader-hw'], stdout=subprocess.DEVNULL)


def labels():
    lab = {}
    for line in open(os.path.join(BUILD, 'vdcbench.lbl')):
        m = re.match(r'al ([0-9A-Fa-f]+) \.(\S+)', line)
        if m:
            lab[m.group(2)] = int(m.group(1), 16)
    return lab


def vice_run(port, short, no_r36, tmo):
    lab = labels()
    mode = 1 | (2 if no_r36 else 0) | (4 if short else 0)
    cmd = ['x128', '-default', '-warp', '-sounddev', 'dummy', '-80col', '-VDC64KB', '-debugcart',
           '+drive8truedrive', '-trapdevice8', '-8', os.path.join(BUILD, 'bench.d64'),
           '-autostartprgmode', '1', '-autostart', os.path.join(BUILD, 'loader.prg'),
           '-remotemonitor', '-remotemonitoraddress', f'ip4://127.0.0.1:{port}']
    out = os.path.join(BUILD, 'benchres.prg')
    if os.path.exists(out):
        os.remove(out)
    vice = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    xchk = []
    m = None
    try:
        m = Monitor(port, tmo, vice)
        for n in ('main', 'bench_done', 'r36_wait_enter', 'xchk_a', 'xchk_b'):
            m.cmd(f'al C:{lab[n]:04x} .{n}')
            m.bp(lab[n])
        ev = m.mem(0x300, 2)            # BASIC-Fehlervektor
        err_at = ev[0] | ev[1] << 8
        m.bp(err_at)
        pend = None
        pc = m.go()
        while True:
            if pc == err_at:
                x = int(re.findall(r'\.;[0-9a-f]{4} [0-9a-f]{2} ([0-9a-f]{2})', m.cmd('r'))[-1], 16)
                lo, hi = m.mem(0x3B, 2)
                print(f'BASIC-Fehler {x} in Zeile {lo | hi << 8}', flush=True)
                sys.exit(1)
            elif pc == lab['main']:
                m.cmd(f'> 2004 {mode:02x}')
                print('main erreicht, Messung laeuft ...', flush=True)
            elif pc == lab['xchk_a']:
                p = m.mem(lab['par'], 2)
                if p[0] | p[1] << 8 == 256:
                    pend = (m.stopwatch(), m.mem(lab['cur_cfg'])[0])
            elif pc == lab['xchk_b']:
                if pend:
                    xchk.append({'cfg': pend[1], 'sw_cycles': m.stopwatch() - pend[0]})
                    pend = None
            elif pc == lab['r36_wait_enter']:
                f = m.mem(0x2006, 2)
                frames = f[0] | f[1] << 8
                v = m.mem(0x314, 2)
                irq = m.bp(v[0] | v[1] << 8)
                for _ in range(frames):
                    m.go()
                m.cmd(f'delete {irq}')
                m.cmd('> 2005 01')
            elif pc == lab['bench_done']:
                m.cmd('bank ram')
                m.cmd(f'save "{out}" 0 4000 7fff')
                break
            else:
                sys.exit(f'unerwarteter Haltepunkt ${pc:04x}')
            pc = m.go()
        try:
            m.s.sendall(b'quit\n')
        except OSError:
            pass
        try:
            vice.wait(10)
        except subprocess.TimeoutExpired:
            pass
    except TimeoutError as e:
        last = f', letzter Haltepunkt ${m.pc:04x}' if m and m.pc is not None else ''
        sys.exit(f'{e} nach {tmo} s{last}')
    finally:
        if vice.poll() is None:
            vice.terminate()
            try:
                vice.wait(10)
            except subprocess.TimeoutExpired:
                vice.kill()
    return out, xchk


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--short', action='store_true')
    ap.add_argument('--no-r36', action='store_true')
    ap.add_argument('--port', type=int)
    ap.add_argument('--out', default=os.path.join(HERE, 'results'))
    ap.add_argument('--timeout', type=int, default=120)   # Waechter; Volllauf dauert ~15 s
    ap.add_argument('--from-file')
    ap.add_argument('--source', default='vice3.10')
    a = ap.parse_args()
    if a.from_file:
        evaluate.evaluate(a.from_file, a.source, a.out, [], provisional=(a.source != 'c128'))
        return
    port = choose_port(a.port)
    build()
    print(f'VICE-Monitor-Port {port}', flush=True)
    prg, xchk = vice_run(port, a.short, a.no_r36, a.timeout)
    if not os.path.exists(prg):
        sys.exit('benchres.prg wurde nicht geschrieben')
    evaluate.evaluate(prg, 'vice3.10', a.out, xchk, provisional=True)


if __name__ == '__main__':
    main()
