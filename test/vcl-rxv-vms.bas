# VCL: VMS (type 12, block copy: R24 bit 7 = 1) and RXV (type 8, block fill: bit 7 = 0) alternating: 12, 8, 12, 8
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):s=dec("a100"):t=dec("a200"):l=dec("b000"):bank 0
20 for i=0 to 6:read b:poke l+i,b:next
30 data 0, 1,0, 1,0,0,3
40 for i=0 to 24:read b:poke a+i,b:next
50 data 12, 208,7, 52,8, 0,176, 8, 0,161, 184,11, 12, 208,7, 152,8, 0,176, 8, 0,162, 28,12, 0
60 for i=0 to 5:read b:poke s+i,b:next
70 data 2,17,34, 142,51, 0
80 for i=0 to 4:read b:poke t+i,b:next
90 data 1,77, 144,99, 0
100 vmf 2000,0,1300:vmw 2000,5:vmw 2001,6:vmw 2002,7
110 vcl a
120 r=10:if vmd(2100)<>5 or vmd(2101)<>6 or vmd(2102)<>7 or vmd(2103)<>0 then 8995
130 r=11:if vmd(2200)<>5 or vmd(2201)<>6 or vmd(2202)<>7 or vmd(2203)<>0 then 8995
140 r=12:if vmd(3000)<>17 or vmd(3002)<>51 or vmd(3009)<>51 or vmd(3017)<>51 or vmd(3018)<>0 then 8995
150 r=13:if vmd(3100)<>77 or vmd(3101)<>99 or vmd(3110)<>99 or vmd(3118)<>99 or vmd(3119)<>0 then 8995
160 r=14:if rgd(24)>127 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
