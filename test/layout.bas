# v3 memory layout after BOOT: BASIC text at $4001, header at $1C00, BASIC vectors point into VDC-BASIC
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 r=1:if peek(45)<>1 or peek(46)<>64 then 8995
20 r=2:bank 0:x=peek(16384):bank 15:if x<>0 then 8995
30 r=3:if peek(dec("1c00"))<>76 or peek(dec("1c04"))<>14 then 8995
40 for i=0 to 3:a=dec("30c")+2*i:if i=3 then a=dec("2fc")
50 v=peek(a)+256*peek(a+1):r=4+i:if v<dec("1c00") or v>dec("3fff") then 8995
60 next
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
