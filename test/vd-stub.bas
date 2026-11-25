# VDL, VDR, VDB (tokens $FE3E-$FE40) are reserved until P2c and give ?SYNTAX ERROR; RXV keeps token $FE41
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 r=1:trap 20:vdr 1,2,3,4:goto 8995
20 r=2:if er<>11 then 8995
30 goto 8990
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
