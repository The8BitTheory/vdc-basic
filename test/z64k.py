#!/usr/bin/env python3
"""Gemeinsame Z64K-Anbindung fuer die Testsuite (run.sh -z, run_rxv.py, run_draw.py, run_bench.py).

Z64K (Java, GUI, genauerer VDC als VICE) laeuft unter einem eigenen Xvfb (Displaynummer per -displayfd,
nie eine feste Nummer). Beendet werden nur die eigenen Prozesse (Java, Xvfb) per PID.
Ereignis ist das Prozessende: -debugcart, Schreiben nach $D7FF beendet Z64K mit diesem Exitcode.
-limitcycles (Exitcode 1) und der Gesamt-Timeout sind nur Waechter gegen Haenger.
Z64K schreibt Diskimages nie selbst zurueck: test/z64k/Z64KRun.java sichert Laufwerk 8 im Shutdown-Hook.

Modul:   run(prg, d64=..., limit=..., timeout=..., save=Pfad) -> Result
CLI:     test/z64k.py --prg X.prg --d64 T.d64 [--limit N] [--timeout S] [--save OUT.d64]
         Exitcode = Exitcode von Z64K (124 = Waechter-Timeout, 125 = Z64K nicht startbar).
"""
import argparse, collections, glob, os, select, shutil, socket, subprocess, sys, tempfile, threading, time

HERE = os.path.dirname(os.path.abspath(__file__))
RUNNER = os.path.join(HERE, 'z64k', 'Z64KRun.java')
JAR = os.environ.get('Z64K_JAR', os.path.expanduser('~/.local/share/z64k/Z64K.jar'))
EXIT_TIMEOUT, EXIT_MISSING = 124, 125
_xlock = threading.Lock()      # Xvfb-Starts nacheinander (parallele Laeufe)

Result = collections.namedtuple('Result', 'rc image log secs')   # rc: Exitcode, image: gesichertes d64 oder None


def available():
    """None, wenn alles da ist, sonst eine Meldung."""
    for t in ('Xvfb', 'java'):
        if not shutil.which(t):
            return f'{t} fehlt (fuer Z64K noetig)'
    if not os.path.exists(JAR):
        return f'Z64K.jar nicht gefunden: {JAR} (Umgebungsvariable Z64K_JAR)'
    return None


def start_xvfb():
    """Eigenes Xvfb auf freier Displaynummer (-displayfd waehlt sie)."""
    r, w = os.pipe()
    xv = subprocess.Popen(['Xvfb', '-displayfd', str(w), '-screen', '0', '1024x768x24', '-nolisten', 'tcp'],
                          pass_fds=[w], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    os.close(w)
    try:
        if not select.select([r], [], [], 30)[0]:
            raise RuntimeError('Xvfb meldet keine Displaynummer')
        num = os.read(r, 32).decode().strip()
        if not num.isdigit():
            raise RuntimeError('Xvfb beendet vor der Displaynummer')
    except BaseException:
        xv.kill()
        raise
    finally:
        os.close(r)
    # bereit, wenn der Unix-Socket des Displays Verbindungen annimmt (nicht schon bei der Meldung der Nummer)
    path = f'/tmp/.X11-unix/X{num}'
    end = time.time() + 30
    while True:
        c = socket.socket(socket.AF_UNIX)
        try:
            c.connect(path)
            break
        except OSError:
            if xv.poll() is not None or time.time() > end:
                xv.kill()
                raise RuntimeError('Xvfb nimmt keine Verbindungen an')
            time.sleep(0.02)        # nur bis der Socket bereit ist (Ereignis: connect gelingt)
        finally:
            c.close()
    return xv, ':' + num


def _stop(p, grace):
    if p.poll() is None:
        p.terminate()
        try:
            p.wait(grace)
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()


def run(prg, d64=None, limit=200_000_000, timeout=600, save=None, log=None, extra=()):
    """Z64K (c128, VDC 64 KB, Warp, debugcart) mit prg per -autoload starten.

    d64: Image fuer Laufwerk 8 (Z64K veraendert die Datei nicht). save=Pfad: das Image von Laufwerk 8
    wird beim Beenden dorthin gesichert (Result.image, None wenn es fehlt). log: Pfad der Konsolenausgabe
    (sonst temporaer); Result.log enthaelt immer den Text."""
    msg = available()
    if msg:
        return Result(EXIT_MISSING, None, msg, 0.0)
    tmp = tempfile.mkdtemp(prefix='z64k-')
    image = save
    logp = log or os.path.join(tmp, 'z64k.log')
    cmd = ['java', '-cp', JAR, RUNNER, 'c128', '-vdc64kb', '-warp', '-debugcart', '-limitcycles', str(limit)]
    if d64:
        cmd += ['-8', d64]
    cmd += list(extra) + ['-autoload', prg]
    t0 = time.time()
    for attempt in range(4):
        with _xlock:
            xv, disp = start_xvfb()
        env = dict(os.environ, DISPLAY=disp)
        if image:
            env['Z64K_SAVE'] = image
        try:
            with open(logp, 'w') as lf:
                jp = subprocess.Popen(cmd, env=env, stdout=lf, stderr=subprocess.STDOUT)
                try:
                    rc = jp.wait(max(1, timeout - (time.time() - t0)))   # Ereignis: Prozessende; Timeout nur Waechter
                except subprocess.TimeoutExpired:
                    _stop(jp, 15)
                    rc = EXIT_TIMEOUT
        finally:
            _stop(xv, 10)
        # Xvfb gerade gestartet, aber Java kommt nicht an das Display (selten): Start wiederholen
        if rc == 1 and "Can't connect to X11" in open(logp, errors='replace').read(4000):
            continue
        break
    else:
        rc = EXIT_MISSING           # viermal kein X11-Display: nicht als -limitcycles (rc 1) melden
    secs = time.time() - t0
    if image and not os.path.exists(image):      # Z64K haengt ggf. eine Endung an
        found = sorted(glob.glob(image + '*'))
        image = found[0] if found else None
    text = open(logp, errors='replace').read()
    shutil.rmtree(tmp, ignore_errors=True)
    return Result(rc, image, text, secs)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--check', action='store_true', help='nur pruefen, ob Xvfb, java und Z64K.jar da sind')
    ap.add_argument('--prg')
    ap.add_argument('--d64')
    ap.add_argument('--limit', type=int, default=200_000_000)
    ap.add_argument('--timeout', type=int, default=600)
    ap.add_argument('--save', help='Image von Laufwerk 8 hierher sichern')
    ap.add_argument('--log', help='Konsolenausgabe von Z64K hierher schreiben')
    a = ap.parse_args()
    if a.check:
        msg = available()
        if msg:
            print(msg, file=sys.stderr)
        return 1 if msg else 0
    if not a.prg:
        ap.error('--prg fehlt')
    r = run(a.prg, a.d64, a.limit, a.timeout, a.save, a.log)
    if r.rc == EXIT_MISSING:
        print(r.log, file=sys.stderr)
    elif r.rc == EXIT_TIMEOUT:
        print(f'Waechter-Timeout nach {a.timeout} s', file=sys.stderr)
    elif r.rc not in (0,):
        print('\n'.join(r.log.splitlines()[-4:]), file=sys.stderr)
    return r.rc


if __name__ == '__main__':
    sys.exit(main())
