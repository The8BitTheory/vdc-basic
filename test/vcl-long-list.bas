# VCL: a list longer than 256 bytes (60 VMW entries, 301 bytes) at $1300
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("1300"):bank 0
20 for i=0 to 59:p=a+i*5:poke p,6:poke p+1,(3000+i)and255:poke p+2,int((3000+i)/256):poke p+3,i+1:poke p+4,0:next
30 poke a+300,0:vmf 3000,0,60
40 vcl a
50 for i=0 to 59:if vmd(3000+i)<>i+1 then r=10:goto 8995
60 next
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
