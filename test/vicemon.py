#!/usr/bin/env python3
"""Gemeinsame Hilfen fuer die Monitor-Skripte (E1, boot-sector, P2b, P2c).

- Port: --port, sonst DBE_VICE_PORT, sonst 6631; Bereich 6631-6649; bind-Test auf 127.0.0.1.
  Belegt -> naechster freier Port mit Meldung; bei ausdruecklichem --port Abbruch (Exitcode 2).
- VICE mit -remotemonitor -remotemonitoraddress ip4://127.0.0.1:<port>, kein -binarymonitor.
- Nur der eigene Prozess wird beendet (terminate, nach Timeout kill); kein pkill/killall.
- Ereignisgesteuert: Haltepunkte, keine festen Wartezeiten; Timeouts nur als Waechter.
"""
import glob, os, re, socket, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..'))
PROMPT = re.compile(rb'\(C:\$([0-9a-f]{4})\) ')


def port_free(p):
    s = socket.socket()
    # wie VICE mit SO_REUSEADDR: TIME_WAIT eines Vorlaufs gilt nicht als belegt
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        s.bind(('127.0.0.1', p))
        return True
    except OSError:
        return False
    finally:
        s.close()


def choose_port(arg=None):
    if arg is not None:
        if not port_free(arg):
            print(f'Port {arg} ist belegt (explizit angegeben)', file=sys.stderr)
            sys.exit(2)
        return arg
    p0 = int(os.environ.get('DBE_VICE_PORT', '6631'))
    if not 6631 <= p0 <= 6649:
        print(f'DBE_VICE_PORT={p0} liegt nicht in 6631-6649', file=sys.stderr)
        sys.exit(2)
    if port_free(p0):
        return p0
    for p in range(6631, 6650):
        if port_free(p):
            print(f'Port {p0} belegt, nehme {p}', flush=True)
            return p
    print('Kein freier Port in 6631-6649', file=sys.stderr)
    sys.exit(2)


class Exited(Exception):
    """VICE hat sich beendet (z. B. ueber die Debug-Cartridge)."""


class Monitor:
    """Remote-Monitor von VICE (nach dungeonboringengine/test/vice_drive.py)."""

    def __init__(self, port, tmo, proc):
        self.proc = proc
        self.deadline = time.time() + tmo   # Waechter fuer den ganzen Lauf
        while True:
            if proc.poll() is not None:
                raise RuntimeError(f'VICE vor dem Monitor-Verbindungsaufbau beendet ({proc.returncode})')
            try:
                self.s = socket.create_connection(('127.0.0.1', port))
                break
            except OSError:
                if time.time() > self.deadline:
                    raise TimeoutError('Monitor-Port oeffnet nicht')
                time.sleep(0.05)        # Wiederholung nur bis VICE den Port oeffnet
        self.buf = b''
        self.pc = None
        self.s.sendall(b'\n')           # haelt die Emulation an: zwei Prompts
        self._prompt(2)

    def _prompt(self, n=1):
        while len(PROMPT.findall(self.buf)) < n:
            rest = self.deadline - time.time()
            if rest <= 0:
                raise TimeoutError('Waechter-Timeout abgelaufen')
            self.s.settimeout(rest)
            try:
                x = self.s.recv(65536)
            except socket.timeout:
                raise TimeoutError('Waechter-Timeout abgelaufen') from None
            except ConnectionResetError:
                x = b''
            if not x:
                raise Exited()
            self.buf += x
        end = [m.end() for m in PROMPT.finditer(self.buf)][n - 1]
        out, self.buf = self.buf[:end].decode(errors='replace'), self.buf[end:]
        return out

    def cmd(self, c):
        self.s.sendall(c.encode() + b'\n')
        return self._prompt()

    def go(self):
        """Weiterlaufen bis zum naechsten Halt; liefert (PC, Text). Exited, wenn VICE endet."""
        self.s.sendall(b'x\n')
        out = self._prompt()
        self.pc = int(re.findall(r'\(C:\$([0-9a-f]{4})\)', out)[-1], 16)
        return self.pc, out

    def mem(self, a, n=1):
        t = self.cmd(f'm {a:04x} {a + n - 1:04x}')
        data = {}
        for mm in re.finditer(r'>C:([0-9a-f]{4})((?:\s+[0-9a-f]{2}){1,16})', t):
            for k, x in enumerate(mm.group(2).split()):
                data[int(mm.group(1), 16) + k] = int(x, 16)
        return [data[a + k] for k in range(n)]

    def bp(self, a, cond=None):
        c = f'break {a:04x}' + (f' if {cond}' if cond else '')
        return int(re.search(r'BREAK: (\d+)', self.cmd(c)).group(1))

    def reg_x(self):
        return int(re.findall(r'\.;[0-9a-f]{4} [0-9a-f]{2} ([0-9a-f]{2})', self.cmd('r'))[-1], 16)


def run_vice(args, port, tmo, script):
    """x128 mit Remote-Monitor starten, script(monitor) ausfuehren, eigenen Prozess beenden.

    Liefert (Rueckgabe von script, Exitcode von VICE oder None)."""
    cmd = ['x128'] + args + ['-remotemonitor', '-remotemonitoraddress', f'ip4://127.0.0.1:{port}']
    vice = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        m = Monitor(port, tmo, vice)
        res = script(m)
        try:
            vice.wait(max(1, m.deadline - time.time()))
        except subprocess.TimeoutExpired:
            pass
        return res, vice.poll()
    finally:
        if vice.poll() is None:
            vice.terminate()
            try:
                vice.wait(10)
            except subprocess.TimeoutExpired:
                vice.kill()
                vice.wait()


def symbols(path):
    """ACME-Symbolliste (acme -l) -> {name: wert}."""
    sym = {}
    for line in open(path):
        m = re.match(r'\s*([A-Za-z_][\w.]*)\s*=\s*\$([0-9a-fA-F]+)', line)
        if m:
            sym[m.group(1)] = int(m.group(2), 16)
    return sym


def find_node():
    n = os.environ.get('NODE')
    if n:
        return n
    for d in os.environ.get('PATH', '').split(os.pathsep):
        p = os.path.join(d, 'node')
        if os.access(p, os.X_OK):
            return p
    c = sorted(glob.glob(os.path.expanduser('~/.nvm/versions/node/*/bin/node')),
               key=lambda p: [int(x) for x in re.findall(r'\d+', p.split('/node/')[1])])
    return c[-1] if c else None


def blitz_runtime():
    """Runtime for blitzc: env BLITZ_RUNTIME (original|fixed|improved), default improved (= blitzc default)."""
    return os.environ.get('BLITZ_RUNTIME', 'improved')


def find_blitzc():
    p = os.environ.get('BLITZC', os.path.expanduser('~/git/blitz-compiler/blitz128/blitzc/js/cli.js'))
    return p if os.path.exists(p) else None
