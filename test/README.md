# Tests

Each `.bas` file is a BASIC 7.0 listing that loads `vdcbasic2g.bin`, checks one behaviour by reading VRAM back with `VMD`, and reports the result as an exit code through the VICE debug cartridge (`POKE $D7FF`; 0 = pass, 200 + n = BASIC error n).

```
test/run.sh test/*.bas
```

Needs `x128` and `c1541` from VICE and Python 3. `bastok.py` tokenises the listings using the keyword table in `BASIC V7.0 VDC.txt`. The runs are headless and in warp mode; the cycle limit is only a guard against hangs.

To rebuild the binary: `ACME=<path to ACME_Lib> acme src/vdcbasic.asm` in the repository root.
