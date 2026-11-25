# VCL: two RXV entries (type 8) directly after each other, both with a run >= T (block fill); $1C06/$1C08 from the second
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):s=dec("a100"):t=dec("a200"):bank 0
20 for i=0 to 10:read b:poke a+i,b:next
30 data 8, 0,161, 184,11, 8, 0,162, 28,12, 0
40 for i=0 to 5:read b:poke s+i,b:next
50 data 2,17,34, 142,51, 0
60 for i=0 to 4:read b:poke t+i,b:next
70 data 1,77, 144,99, 0
80 vmf 2990,0,200
90 vcl a
100 r=10:if vmd(3000)<>17 or vmd(3001)<>34 or vmd(3002)<>51 or vmd(3017)<>51 or vmd(3018)<>0 then 8995
110 r=11:if vmd(3100)<>77 or vmd(3101)<>99 or vmd(3118)<>99 or vmd(3119)<>0 or vmd(3099)<>0 then 8995
120 r=12:if peek(dec("1c06"))+256*peek(dec("1c07"))<>t+5 then 8995
130 r=13:if peek(dec("1c08"))+256*peek(dec("1c09"))<>3119 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
