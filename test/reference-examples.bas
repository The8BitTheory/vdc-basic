# The worked examples of docs/reference/vcl-format.md and vms-format.md. Needs 64 KiB VRAM.
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 a=dec("4000"):bank 0
20 for i=0 to 18:read b:poke a+i,b:next
30 data 4, 0,0, 0,0, 64,31
40 data 2, 32,78, 52,3, 4,0, 16,0, 80,0
50 data 0
55 vmf 20000,170,64:vmf 0,255,8100
60 vcl a
70 r=1:if vmd(0)<>0 or vmd(7999)<>0 or vmd(8000)<>255 then 8995
80 r=2:if vmd(820)<>170 or vmd(823)<>170 or vmd(824)<>0 or vmd(820+15*80+3)<>170 or vmd(820+16*80)<>0 then 8995
100 for i=0 to 16:read b:poke a+i,b:next
110 data 5, 4,0, 8,0,0, 1,1,0,1, 8,2,0, 1,80,0,2
120 for i=0 to 4:read b:vmw 20000+i,b:next
130 data 81,17,21,34,34
140 vmf 0,153,200:vms 20000,0,a
150 r=3:if vmd(0)<>145 or vmd(1)<>17 or vmd(2)<>25 then 8995
160 r=4:if vmd(80)<>34 or vmd(81)<>34 or vmd(82)<>153 or vmd(3)<>153 then 8995
8990 r=0
8995 rst 3:bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
