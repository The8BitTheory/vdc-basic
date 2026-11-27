# VCL: a RXV entry (type 8, stream above $4000) followed by a VMW entry; $FF00 unchanged afterwards
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):s=dec("a100"):bank 0
20 for i=0 to 10:read b:poke a+i,b:next
30 data 8, 0,161, 184,11, 6, 194,11, 77,0, 0
40 for i=0 to 5:read b:poke s+i,b:next
50 data 2,17,34, 129,51, 0
60 vmf 3000,99,20
70 c=peek(dec("ff00"))
80 vcl a
90 r=10:if peek(dec("ff00"))<>c then 8995
100 r=11:if vmd(3000)<>17 or vmd(3001)<>34 or vmd(3002)<>51 or vmd(3004)<>51 then 8995
110 r=12:if vmd(3005)<>99 then 8995
120 r=13:if vmd(3010)<>77 or vmd(3009)<>99 then 8995
130 r=14:if peek(dec("1c06"))+256*peek(dec("1c07"))<>s+6 then 8995
140 r=15:if peek(dec("1c08"))+256*peek(dec("1c09"))<>3005 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
