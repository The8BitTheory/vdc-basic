# RXV from BANK 0 and BANK 15: same result, $FF00 as before; R24 bit 6 (reverse) is kept, bit 7 (copy) cleared
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("1300"):bank 0
20 for i=0 to 6:read b:poke a+i,b:next
30 data 2,17,34, 255,0,1,51, 0
50 bank 0:c=peek(dec("ff00")):vmf 3000,99,600
60 rgo 24,192:rxv a,3000:q=rgd(24)
70 r=10:if peek(dec("ff00"))<>c then 8995
80 r=11:if (q and 192)<>64 then 8995
90 gosub 500:if r then 8995
100 bank 15:c=peek(dec("ff00")):vmf 3000,99,600
110 rxv a,3000:q=rgd(24)
120 r=12:if peek(dec("ff00"))<>c then 8995
130 r=13:if (q and 192)<>64 then 8995
140 gosub 500:if r then 8995
150 rga 24,191:goto 8990
500 r=20:if vmd(3000)<>17 or vmd(3001)<>34 then return
510 r=21:if vmd(3002)<>51 or vmd(3257)<>51 then return
520 r=22:if vmd(3258)<>99 then return
530 r=0:return
8990 r=0
8995 rga 24,191:bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
