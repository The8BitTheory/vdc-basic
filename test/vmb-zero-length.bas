# VMB with length 0 prints nothing
1 trap 9000
2 bload"vdcbasic2g",b0,u8:sys dec("1300")
10 bank 0:poke dec("3000"),32
20 vmf 2000,255,8:vmf 2100,0,300
30 vcs 2000,1,1,80
40 vmb 2100,dec("3000"),0
50 if vmd(2100)<>0 then r=10:goto 8995
60 vmb 2100,dec("3000"),1:if vmd(2100)<>255 or vmd(2101)<>0 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
