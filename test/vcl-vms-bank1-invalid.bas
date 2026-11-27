# VCL type 9 with an invalid VMS command in the bank 1 list: message, $D506 restored, the next VCL entry runs
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 bank 15:d=peek(dec("d506")):v=dec("1300"):a=dec("c000")
20 bank 1:poke a,0:poke a+1,2:poke a+2,0:poke a+3,7
30 bank 0:for i=0 to 12:read b:poke v+i,b:next
40 data 9, 32,78, 88,27, 0,192, 6, 88,27, 99,0, 0
50 vmw 7000,0
60 vcl v
70 bank 15:r=9:if peek(dec("d506"))<>d then 8995
80 r=10:if vmd(7000)<>99 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
