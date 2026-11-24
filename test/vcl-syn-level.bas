# VCL: type 13 with the high byte of n set waits only when outside the border; registers are still written
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("5000"):bank 0:c=rgd(26)
20 rem 13,n=0 (edge): every call waits for the next border
30 poke a,13:poke a+1,0:poke a+2,0:poke a+3,0
40 t=ti:for i=1 to 50:vcl a:next:e=ti-t
50 rem 13,n=0 with high byte 1 (level): calls inside the border return at once
60 poke a+2,1
70 t=ti:for i=1 to 50:vcl a:next:l=ti-t
80 if e<45 then r=10:goto 8995
90 if l*4>e*3 then r=11:goto 8995
100 rem 13,n=1 with high byte 1: r26=165 is written
110 poke a,13:poke a+1,1:poke a+2,1:poke a+3,26:poke a+4,0:poke a+5,165:poke a+6,0:poke a+7,0
120 vcl a:x=rgd(26):rgw 26,c
130 if x<>165 then r=12:goto 8995
140 goto 8990
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
