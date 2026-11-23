# VMS entry type 2 after a VMC with negative increments
1 trap 9000
2 bload"vdcbasic2g",b0,u8:sys dec("1300")
10 a=dec("3000"):bank 0
20 for i=0 to 7:read b:poke a+i,b:next
30 data 0, 1,0, 2, 2,0, 2,0
40 for i=0 to 3:vmw 2000+i,i+1:next:vmf 2100,0,200
50 vmc 2010,2400,2,2,-80,-2
60 vms 2000,2100,a
70 if vmd(2100)<>1 or vmd(2101)<>2 then r=10:goto 8995
80 if vmd(2180)<>3 or vmd(2181)<>4 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
