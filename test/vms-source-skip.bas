# VMS type 3 (source skip): read position moves on by n bytes, nothing is written; also with types 1 and 8 and n > 255
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("1300"):bank 0
20 read n:for i=0 to n-1:read b:poke a+i,b:next
30 data 27, 5, 7,0, 1,0,0,1, 3,2,0, 1,1,0,1, 3,0,0, 8,2,0, 3,44,1, 1,3,0,1
40 vmf 2000,0,400:vmw 2000,17:vmw 2001,34:vmw 2002,51:vmw 2003,68:vmw 2004,85:vmw 2305,123
50 vmf 2100,170,8
60 vms 2000,2100,a
70 r=10:if vmd(2100)<>17 or vmd(2101)<>68 then 8995
80 r=11:if vmd(2102)<>90 then 8995
90 r=12:if vmd(2103)<>123 or vmd(2104)<>170 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
