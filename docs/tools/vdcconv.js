#!/usr/bin/env node
/* Command line front end for the converter in vdc-converter.html.
 *
 * usage: node vdcconv.js picture in.png out.prg
 *        node vdcconv.js sprite  in.png out.prg [key colour 0-15] [bytes per row]
 *        node vdcconv.js font    in.png out.prg cellwidth cellheight
 *        node vdcconv.js hires   in.png out.prg [threshold]
 *
 * The conversion code is taken from the HTML page, so both give the same files.
 * Reads PNG only (8 bits per channel, not interlaced).
 */
const fs = require('fs'), path = require('path'), zlib = require('zlib');

const html = fs.readFileSync(path.join(__dirname, 'vdc-converter.html'), 'utf8');
const core = /<script id="core">([\s\S]*?)<\/script>/.exec(html)[1];
const mod = {};
new Function('module', core)(mod);
const VDC = mod.exports;

function readPng(file) {
  const b = fs.readFileSync(file);
  let o = 8, w, h, depth, type, pal, trns, idat = [];
  while (o < b.length) {
    const len = b.readUInt32BE(o), tag = b.toString('latin1', o + 4, o + 8), d = b.subarray(o + 8, o + 8 + len);
    if (tag === 'IHDR') { w = d.readUInt32BE(0); h = d.readUInt32BE(4); depth = d[8]; type = d[9]; if (d[12]) throw new Error('interlaced PNG'); }
    else if (tag === 'PLTE') pal = d; else if (tag === 'tRNS') trns = d; else if (tag === 'IDAT') idat.push(d);
    o += len + 12;
  }
  const ch = { 0: 1, 2: 3, 3: 1, 4: 2, 6: 4 }[type];
  if (depth !== 8 && !(type === 3 && depth <= 8)) throw new Error('unsupported PNG bit depth');
  const raw = zlib.inflateSync(Buffer.concat(idat)), bpp = Math.max(1, ch * depth >> 3), stride = Math.ceil(w * ch * depth / 8);
  const px = Buffer.alloc(stride * h);
  for (let y = 0; y < h; y++) {
    const f = raw[y * (stride + 1)], line = raw.subarray(y * (stride + 1) + 1, (y + 1) * (stride + 1));
    for (let x = 0; x < stride; x++) {
      const a = x >= bpp ? px[y*stride + x - bpp] : 0, u = y ? px[(y-1)*stride + x] : 0, c = (y && x >= bpp) ? px[(y-1)*stride + x - bpp] : 0;
      let v = line[x];
      if (f === 1) v += a; else if (f === 2) v += u; else if (f === 3) v += (a + u) >> 1;
      else if (f === 4) { const p = a + u - c, pa = Math.abs(p - a), pb = Math.abs(p - u), pc = Math.abs(p - c); v += (pa <= pb && pa <= pc) ? a : (pb <= pc ? u : c); }
      px[y*stride + x] = v & 255;
    }
  }
  const rgba = new Uint8Array(w * h * 4);
  for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
    const i = (y*w + x) * 4;
    if (type === 3) {
      const v = depth === 8 ? px[y*stride + x] : (px[y*stride + (x*depth >> 3)] >> (8 - depth - (x*depth & 7))) & ((1 << depth) - 1);
      rgba.set([pal[v*3], pal[v*3+1], pal[v*3+2], trns && v < trns.length ? trns[v] : 255], i);
    } else {
      const s = y*stride + x*ch;
      if (type === 0) rgba.set([px[s], px[s], px[s], 255], i);
      else if (type === 4) rgba.set([px[s], px[s], px[s], px[s+1]], i);
      else rgba.set([px[s], px[s+1], px[s+2], type === 6 ? px[s+3] : 255], i);
    }
  }
  return { w, h, rgba };
}

const [mode, src, dst, a1, a2] = process.argv.slice(2);
if (!dst) { console.error('usage: vdcconv.js picture|sprite|font|hires in.png out.prg [options]'); process.exit(2); }
const { w, h, rgba } = readPng(src), idx = VDC.quantize(rgba, w, h);
let r;
if (mode === 'picture') r = VDC.picture(idx, w, h);
else if (mode === 'font') r = VDC.font(idx, w, h, +a1, +a2);
else if (mode === 'hires') r = VDC.hires(rgba, w, h, a1 === undefined ? 128 : +a1);
else if (mode === 'sprite') {
  let key = a1 === undefined ? -1 : +a1;
  if (key < 0) key = VDC.freeColour(idx);
  r = VDC.sprite(idx, w, h, key, a2 === undefined ? 80 : +a2);
} else { console.error('unknown mode ' + mode); process.exit(2); }
fs.writeFileSync(dst, r.bytes);
const { bytes, ...rest } = r;
console.log(path.basename(dst) + ': ' + bytes.length + ' bytes ' + JSON.stringify(rest));
