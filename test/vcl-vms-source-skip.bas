# VCL type 12 (VMS) with a list that uses type 3 (source skip), followed by a VMW entry
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):l=dec("b000"):bank 0
20 read n:for i=0 to n-1:read b:poke l+i,b:next
30 data 14, 0, 3,0, 1,0,0,1, 3,3,1, 1,1,0,2
40 for i=0 to 12:read b:poke a+i,b:next
50 data 12, 208,7, 52,8, 0,176, 6, 60,8, 77,0, 0
60 vmf 2000,0,400:vmw 2000,17:vmw 2260,34:vmw 2261,51:vmw 2259,9:vmw 2262,9
70 vmf 2100,170,8
80 vcl a
90 r=10:if vmd(2100)<>17 or vmd(2101)<>34 or vmd(2102)<>51 then 8995
100 r=11:if vmd(2103)<>170 then 8995
110 r=12:if vmd(2108)<>77 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
