# VCL: SYN entries (type 13): n = 0, n = 2 register writes, and a 323-byte entry with n = 80
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):bank 0:c=rgd(26):d=rgd(27)
20 rem 13,n=0, then vmw 3000,11
30 p=a:poke p,13:p=p+1:w=0:gosub 1000:poke p,6:p=p+1:w=3000:gosub 1000:w=11:gosub 1000:poke p,0
40 vmf 3000,0,8:vcl a
50 if vmd(3000)<>11 then r=10:goto 8995
60 rem 13,n=2: r26=165, r27=7 (high bytes ignored), then vmw 3001,22
70 p=a:poke p,13:p=p+1:w=2:gosub 1000:w=26:gosub 1000:w=256+165:gosub 1000:w=27:gosub 1000:w=7:gosub 1000
80 poke p,6:p=p+1:w=3001:gosub 1000:w=22:gosub 1000:poke p,0
90 vcl a:x=rgd(26):y=rgd(27):rgw 26,c:rgw 27,d
100 if x<>165 or y<>7 then r=11:goto 8995
110 if vmd(3001)<>22 then r=12:goto 8995
120 rem 13,n=80: r26=i (i=1..79), last r26=90, then vmw 3002,33
130 p=a:poke p,13:p=p+1:w=80:gosub 1000:for i=1 to 79:w=26:gosub 1000:w=i:gosub 1000:next
140 w=26:gosub 1000:w=90:gosub 1000:if p-a<>323 then r=13:goto 8995
150 poke p,6:p=p+1:w=3002:gosub 1000:w=33:gosub 1000:poke p,0
160 vcl a:x=rgd(26):rgw 26,c
170 if x<>90 then r=14:goto 8995
180 if vmd(3002)<>33 then r=15:goto 8995
190 if vmd(3000)<>11 or vmd(3001)<>22 then r=16:goto 8995
200 goto 8990
1000 poke p,w and 255:poke p+1,int(w/256):p=p+2:return
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
