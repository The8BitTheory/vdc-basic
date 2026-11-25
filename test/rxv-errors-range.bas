# RXV: a value above 65535 is an illegal quantity error (14). One error per file: in a Blitz! program no extension works any more after a caught error
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 r=1:trap 20:rxv 70000,0:goto 8995
20 r=2:if er<>14 then 8995
30 goto 8990
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
