# Tools

| File | What it is |
|---|---|
| `vdc-converter.html` | Converter from pictures to files for VDC-BASIC programs. Open it in a browser; it works from disk, without network. |
| `vdcconv.js` | The same converter for the command line (Node.js, PNG input only). It runs the conversion code contained in the HTML page. |

```
node vdcconv.js picture in.png out            # picture or tile sheet, 160x100 mode
node vdcconv.js sprite  in.png out [key] [rowbytes]   # sprite with transparency for VMS
node vdcconv.js font    in.png out cellwidth cellheight
node vdcconv.js hires   in.png out [threshold]        # 640x200, two colours
```

## File formats

Every file starts with a 2-byte load address that `BLOAD "NAME",B0,P(address)` ignores. After it:

| Kind | Layout |
|---|---|
| Picture | width in bytes, height in rows, then width × height bytes of data |
| Font | glyph width in bytes, glyph height, number of glyphs, then the glyphs one after the other, starting with the space |
| Sprite | width in bytes, height, list length (word), data length (word), the [VMS list](../reference/vms-format.md), the sprite data |

In the 160×100 mode one byte is two pixels, the left one in the upper four bits. In the 640×200 mode one byte is eight pixels, the leftmost in bit 7.

The source pictures of the guide are in `docs/art/`, the converted files in `docs/listings/data/`.
