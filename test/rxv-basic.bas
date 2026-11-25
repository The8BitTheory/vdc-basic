# RXV: a small stream at $A000 (literal, CPU run, skip, literal, long run by block fill), header $1C06/$1C08
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):bank 0
20 for i=0 to 15:read b:poke a+i,b:next
30 data 3,17,34,51, 129,68, 254,2,0, 1,85, 255,44,1,102, 0
40 vmf 3000,99,320
50 rxv a,3000
60 if vmd(3000)<>17 or vmd(3001)<>34 or vmd(3002)<>51 then r=10:goto 8995
70 if vmd(3003)<>68 or vmd(3005)<>68 then r=11:goto 8995
80 if vmd(3006)<>99 or vmd(3007)<>99 then r=12:goto 8995
90 if vmd(3008)<>85 or vmd(3009)<>102 or vmd(3308)<>102 then r=13:goto 8995
100 if vmd(3309)<>99 then r=14:goto 8995
110 if peek(dec("1c06"))+256*peek(dec("1c07"))<>a+16 then r=15:goto 8995
120 if peek(dec("1c08"))+256*peek(dec("1c09"))<>3309 then r=16:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
