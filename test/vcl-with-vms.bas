# VCL: a VMS entry (type 12) with its list above $4000, followed by a VMW entry
1 trap 9000
2 bload"vdcbasic2g",b0,u8:sys dec("1300")
10 a=dec("5000"):l=dec("6000"):bank 0
20 for i=0 to 6:read b:poke l+i,b:next
30 data 0, 1,0, 1,0,0,3
40 for i=0 to 12:read b:poke a+i,b:next
50 data 12, 208,7, 52,8, 0,96, 6, 60,8, 77,0, 0
60 vmf 2100,0,16:vmw 2000,17:vmw 2001,34:vmw 2002,51
70 vcl a
80 if vmd(2100)<>17 or vmd(2101)<>34 or vmd(2102)<>51 then r=10:goto 8995
90 if vmd(2108)<>77 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
