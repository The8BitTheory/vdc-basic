# VMS: list at $3000 in bank 0, entry types 1 and 8
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("3000"):bank 0
20 read n:for i=0 to n-1:read b:poke a+i,b:next
30 data 10, 5, 2,0, 1,0,0,3, 8,3,0
40 vmf 2100,0,8:vmw 2000,17:vmw 2001,34:vmw 2002,51:vmw 2003,69:vmw 2103,170
50 vms 2000,2100,a
60 if vmd(2100)<>17 or vmd(2101)<>34 or vmd(2102)<>51 then r=10:goto 8995
65 rem last byte: right nibble 5 is alpha, so left nibble 4 over background a
70 if vmd(2103)<>74 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
