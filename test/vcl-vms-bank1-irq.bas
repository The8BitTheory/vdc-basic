# VCL type 9, 403-byte list in bank 1 at $9000 and at $F000, 300 times, IRQ running (sprite MOVSPR, PLAY), no engine
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 bank 15:d=peek(dec("d506")):v=dec("1300")
20 bank 1:p=dec("9000"):gosub 9100:p=dec("f000"):gosub 9100
30 bank 0:for i=0 to 14:read b:poke v+i,b:next
40 data 9, 32,78, 136,19, 0,144, 9, 32,78, 112,23, 0,240, 0
50 for i=0 to 299:vmw 20000+i,(i*7+1)and255:next
60 sprite 1,1,2:movspr 1,100,100:movspr 1,45#7
70 play "v1o4t0u8 cdefgab cdefgab cdefgab"
80 t=ti:for k=1 to 300:vcl v:next:t=ti-t
90 bank 15:r=9:if peek(dec("d506"))<>d then 8995
100 r=10:if t<30 then 8995
110 r=14:for i=0 to 99:if vmd(5000+2*i)<>((i*7+1)and255) or vmd(6000+2*i)<>((i*7+1)and255) then 8995
120 next
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
9100 poke p,0:poke p+1,100:poke p+2,0
9110 for i=0 to 99:q=p+3+4*i:poke q,1:poke q+1,(2*i)and255:poke q+2,int(2*i/256):poke q+3,1:next:return
