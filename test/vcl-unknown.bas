# VCL: an unknown entry type (14) prints INVALID and ends the list; BASIC carries on
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("5000"):bank 0
20 for i=0 to 11:read b:poke a+i,b:next
30 data 6, 184,11, 44,0, 14, 6, 185,11, 55,0, 0
40 vmf 3000,0,2:vcl a
50 if vmd(3000)<>44 then r=10:goto 8995
60 if vmd(3001)<>0 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
