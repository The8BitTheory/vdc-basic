# VCL type 7 (VMW with repetitions) with increment 0
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("3000"):bank 0
20 for i=0 to 9:read b:poke a+i,b:next
30 data 7, 52,8, 9,0, 3,0, 0,0, 0
40 vmf 2100,0,8
50 vcl a
60 if vmd(2100)<>9 then r=10:goto 8995
70 if vmd(2101)<>0 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
