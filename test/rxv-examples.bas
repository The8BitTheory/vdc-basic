# Examples of docs/reference/vrl-format.md (2x2 rectangle with row length 80) and vms-format.md (type 3, clipped sprite)
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 bank 0:a=dec("a000")
20 for i=0 to 9:read b:poke a+i,b:next
30 data 2,1,2, 254,78,0, 2,3,4, 0
40 vmf 1000,99,90
50 rxv a,1000
60 r=10:if vmd(1000)<>1 or vmd(1001)<>2 or vmd(1080)<>3 or vmd(1081)<>4 then 8995
70 r=11:if vmd(1002)<>99 or vmd(1079)<>99 or vmd(1082)<>99 then 8995
100 rem sprite data $51 $11 $15 $22 $22 at 2000, first row cut off: target one row lower
110 l=dec("1300"):for i=0 to 9:read b:poke l+i,b:next
120 data 5, 2,0, 3,3,0, 1,0,0,2
130 vmf 2000,0,8:vmw 2000,81:vmw 2001,17:vmw 2002,21:vmw 2003,34:vmw 2004,34
140 vmf 3000,170,200
150 vms 2000,3080,l
160 r=12:if vmd(3080)<>34 or vmd(3081)<>34 or vmd(3082)<>170 or vmd(3079)<>170 then 8995
170 r=13:if vmd(3000)<>170 or vmd(3001)<>170 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
