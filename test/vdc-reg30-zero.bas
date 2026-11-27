# Hardware check (docs/v3/P2b-rxv.md 3.3): R30 = 0 makes a block fill write 256 bytes after the first one.
# Runs on a real C128 without the debug cartridge (result as text); in VICE also as exit code.
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=12288:v=165
20 vmf a-8,90,300
30 rga 24,127:rgw 18,a/256:rgw 19,a and 255:rgw 31,v:rgw 30,0
40 r=10:if vmd(a)<>v or vmd(a+1)<>v or vmd(a+255)<>v then 8990
50 r=11:if vmd(a+256)<>v then 8990
60 r=12:if vmd(a+257)<>90 or vmd(a-1)<>90 then 8990
70 r=0
8990 if r=0 then print "r30=0 fills 256 bytes: ok"
8991 if r then print "r30=0: fail";r
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
