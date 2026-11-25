# VCL type 9 (VMS list in bank 1) against type 12 with the same list in bank 0 (docs/v3/P2d-vms-bank1.md)
# list A: types 1, 3, 8, source skip 256, header at $80FC (crosses a page boundary)
# list B: 100 type 1 entries, 403 bytes (longer than 256). Bank 0 holds decoy lists at the bank 1 addresses
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 bank 15:d=peek(dec("d506")):v=dec("1300"):a1=dec("80fc"):a0=dec("a0fc"):b1=dec("9000"):b0=dec("b000")
20 bank 1:restore 30:for i=0 to 23:read b:poke a1+i,b:next
30 data 5,6,0, 1,0,0,3, 3,2,0, 8,5,0, 1,80,0,4, 3,0,1, 1,160,0,2
40 bank 0:restore 30:for i=0 to 23:read b:poke a0+i,b:next
50 for i=0 to 6:read b:poke a1+i,b:poke b1+i,b:next
60 data 5,1,0, 1,1,0,1
70 bank 1:p=b1:gosub 9100:bank 0:p=b0:gosub 9100
80 for i=0 to 28:read b:poke v+i,b:next
90 data 9, 32,78, 184,11, 252,128
91 data 12, 32,78, 160,15, 252,160
92 data 9, 32,78, 136,19, 0,144
93 data 12, 32,78, 112,23, 0,176
94 data 0
100 for i=0 to 299:vmw 20000+i,(i*7+1)and255:next
110 vmf 3000,170,250:vmf 4000,170,250:vmf 5000,170,250:vmf 6000,170,250
120 vcl v
130 bank 15:r=9:if peek(dec("d506"))<>d then 8995
140 r=10:if vmd(3000)<>1 or vmd(3001)<>8 or vmd(3002)<>15 or vmd(3003)<>170 then 8995
150 r=11:if vmd(3005)<>164 or vmd(3080)<>43 or vmd(3083)<>64 or vmd(3084)<>170 then 8995
160 r=12:if vmd(3160)<>71 or vmd(3161)<>78 or vmd(3162)<>170 then 8995
170 r=13:for i=0 to 170:if vmd(3000+i)<>vmd(4000+i) then 8995
180 next
190 r=14:for i=0 to 99:if vmd(5000+2*i)<>((i*7+1)and255) then 8995
200 next
210 r=15:if vmd(5001)<>170 or vmd(5199)<>170 or vmd(5200)<>170 then 8995
220 r=16:for i=0 to 200:if vmd(5000+i)<>vmd(6000+i) then 8995
230 next
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
9100 poke p,0:poke p+1,100:poke p+2,0
9110 for i=0 to 99:q=p+3+4*i:poke q,1:poke q+1,(2*i)and255:poke q+2,int(2*i/256):poke q+3,1:next:return
