# VDL: a negative value is an illegal quantity (14). One error per file: in a Blitz! program no extension works any more after a caught error,
# so the VRAM is read back through $D600/$D601 directly (line 8000)
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
5 vmw 0,165
10 r=1:trap 20:vdl -1,0,0,0,1:goto 8995
20 r=2:if er<>14 then 8995
30 v=0:gosub 8000:r=3:if q<>165 then 8995
40 goto 8990
8000 bank 15:poke dec("d600"),18:poke dec("d601"),int(v/256):poke dec("d600"),19:poke dec("d601"),v-256*int(v/256)
8010 poke dec("d600"),31:q=peek(dec("d601")):return
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
