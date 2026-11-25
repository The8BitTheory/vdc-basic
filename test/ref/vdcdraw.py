"""Referenzmodell fuer VDM/VDL/VDR/VDB (docs/v3/P2c-draw.md, Abschnitt 5).

Nur Standardbibliothek. Pixelmenge und Byte-Ergebnisse sind verbindlich.
"""


class VdcError(Exception):
    """BASIC-Fehler; .code = Fehlernummer (11 SYNTAX, 14 ILLEGAL QUANTITY)."""

    def __init__(self, code, msg=""):
        super().__init__("?error %d %s" % (code, msg))
        self.code = code


def _args(*vals):
    """Allgemeiner Durchgang wie die ROM-Routine getadr ($8812/$8815), links nach rechts:
    None = Parameter fehlt (11); ausserhalb 0..65535 = 14. Der erste Fehler gewinnt.
    Nicht-int ist ein Fehler im Test, kein BASIC-Fehler (ROM wuerde Brueche abschneiden)."""
    for v in vals:
        if v is None:
            raise VdcError(11, "missing parameter")
        if not isinstance(v, int) or isinstance(v, bool):
            raise TypeError("parameter must be int: %r" % (v,))
        if v < 0 or v > 65535:
            raise VdcError(14, "illegal quantity")


def _num(v, lo, hi):
    """Befehlsspezifischer Bereich (nach _args): ausserhalb lo..hi = 14."""
    if v < lo or v > hi:
        raise VdcError(14, "illegal quantity")
    return v


def line_pixels(x1, y1, x2, y2):
    """Bresenham nach 3.5, in Zeichenreihenfolge (nach dem Tauschen der Endpunkte)."""
    dx, dy = abs(x2 - x1), abs(y2 - y1)
    out = []
    if dx >= dy:
        if x1 > x2:
            x1, y1, x2, y2 = x2, y2, x1, y1
        sy = 1 if y2 > y1 else -1
        e = 2 * dy - dx
        y = y1
        for x in range(x1, x2 + 1):
            out.append((x, y))
            if e > 0:
                y += sy
                e -= 2 * dx
            e += 2 * dy
    else:
        if y1 > y2:
            x1, y1, x2, y2 = x2, y2, x1, y1
        sx = 1 if x2 > x1 else -1
        e = 2 * dx - dy
        x = x1
        for y in range(y1, y2 + 1):
            out.append((x, y))
            if e > 0:
                x += sx
                e -= 2 * dy
            e += 2 * dx
    return out


def rect_pixels(x, y, w, h):
    """Rand nach 3.4: oben, unten, linke Spalte, rechte Spalte; jedes Pixel einmal."""
    out = []
    if w <= 0 or h <= 0:
        return out
    out += [(i, y) for i in range(x, x + w)]
    if h >= 2:
        out += [(i, y + h - 1) for i in range(x, x + w)]
    if h >= 3:
        out += [(x, j) for j in range(y + 1, y + h - 1)]
        if w >= 2:
            out += [(x + w - 1, j) for j in range(y + 1, y + h - 1)]
    return out


class Vdc:
    def __init__(self, vram=None):
        if vram is None:
            self.ram = bytearray(65536)
        else:
            if len(vram) != 65536:
                raise ValueError("vram must be 65536 bytes")
            self.ram = bytearray(vram)
        self.mode, self.base, self.stride = 0, 0, 80  # Zustand nach Installation

    # --- Befehle -------------------------------------------------------
    def vdm(self, mode, base, stride):
        _args(mode, base, stride)
        mode = _num(mode, 0, 1)
        base = _num(base, 0, 65535)
        stride = _num(stride, 1, 65535)
        self.mode, self.base, self.stride = mode, base, stride

    def _check_c(self, c):
        return _num(c, 0, 2 if self.mode == 0 else 15)

    def vdl(self, x1, y1, x2, y2, c):
        _args(x1, y1, x2, y2, c)
        for v in (x1, y1, x2, y2):
            _num(v, 0, 16383)
        self._check_c(c)
        for (x, y) in line_pixels(x1, y1, x2, y2):
            self._plot(x, y, c)

    def _rect_args(self, x, y, w, h, c):
        _args(x, y, w, h, c)
        self._check_c(c)
        if x + w > 65536 or y + h > 65536:
            raise VdcError(14, "rectangle exceeds 65536")

    def vdr(self, x, y, w, h, c):
        self._rect_args(x, y, w, h, c)
        for (px, py) in rect_pixels(x, y, w, h):
            self._plot(px, py, c)

    def vdb(self, x, y, w, h, c):
        self._rect_args(x, y, w, h, c)
        for py in range(y, y + h):
            for px in range(x, x + w):
                self._plot(px, py, c)

    # --- Pixel (1.2 / 1.3) --------------------------------------------
    def addr(self, x, y):
        sh = 3 if self.mode == 0 else 1
        return (self.base + y * self.stride + (x >> sh)) & 0xFFFF

    def _plot(self, x, y, c):
        a = self.addr(x, y)
        b = self.ram[a]
        if self.mode == 0:
            m = 0x80 >> (x & 7)
            if c == 0:
                b &= ~m
            elif c == 1:
                b |= m
            else:
                b ^= m
        else:
            if x & 1:
                b = (b & 0xF0) | c
            else:
                b = (b & 0x0F) | (c << 4)
        self.ram[a] = b & 0xFF

    def pixel(self, x, y):
        """Lesehilfe: Bit (Modus 0) bzw. Nibble (Modus 1)."""
        b = self.ram[self.addr(x, y)]
        if self.mode == 0:
            return (b >> (7 - (x & 7))) & 1
        return (b >> 4) if not (x & 1) else (b & 15)

    # --- Befehlsfolgen fuer Testvektoren -------------------------------
    def run(self, cmd):
        """cmd = ["VDL", x1, y1, x2, y2, c] usw. (Name gross/klein egal)."""
        fn = {"VDM": self.vdm, "VDL": self.vdl, "VDR": self.vdr, "VDB": self.vdb}[cmd[0].upper()]
        fn(*cmd[1:])
