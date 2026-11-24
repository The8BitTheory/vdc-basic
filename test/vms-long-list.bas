# VMS: a list longer than 256 bytes (100 type 1 entries of one byte each), above $4000
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("5000"):bank 0:poke a,0:poke a+1,100:poke a+2,0
20 for i=0 to 99:p=a+3+i*4:poke p,1:poke p+1,i*2:poke p+2,0:poke p+3,1:vmw 2000+i,i+1:next
30 vmf 2100,0,200
40 vms 2000,2100,a
50 for i=0 to 99:if vmd(2100+i*2)<>i+1 or vmd(2101+i*2)<>0 then r=10:goto 8995
60 next
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
