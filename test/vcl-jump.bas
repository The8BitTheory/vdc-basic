# VCL type 10 (P2e): the list continues at the address (bank 0). Chain $A000 -> $A100 -> $1300 (below $4000), the bytes
# after a jump entry are not read, a jump as the first entry, a jump to a jump, the end byte after a jump ends the list,
# and a list that starts in front of the jump target with more than 256 bytes (offset bookkeeping across the jump)
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 a=dec("a000"):s=dec("a100"):t=dec("1300"):u=dec("a200"):bank 0
20 for i=0 to 10:read b:poke a+i,b:next
30 data 6, 184,11, 44,0, 10, 0,161, 6, 187,11
40 for i=0 to 7:read b:poke s+i,b:next
50 data 6, 185,11, 55,0, 10, 0,19
60 for i=0 to 5:read b:poke t+i,b:next
70 data 6, 186,11, 66,0, 0
80 vmf 3000,0,10
90 vcl a
100 r=10:if vmd(3000)<>44 or vmd(3001)<>55 or vmd(3002)<>66 then 8995
110 r=11:if vmd(3003)<>0 then 8995
# jump as the first entry, jump to a jump, end byte at the target
120 poke u,10:poke u+1,3:poke u+2,162:poke u+3,10:poke u+4,6:poke u+5,162:poke u+6,0
130 vmf 3000,0,10:vcl u
140 r=12:if vmd(3000)<>0 or vmd(3001)<>0 or vmd(3002)<>0 then 8995
# 60 entries type 6 (300 bytes) from $A300, then a jump to $1300 (66 at 3002); the jump lies at offset 300
150 v=dec("a300"):for i=0 to 59:poke v+5*i,6:poke v+5*i+1,(3010+i) and 255:poke v+5*i+2,int((3010+i)/256)
160 poke v+5*i+3,i+1:poke v+5*i+4,0:next
170 poke v+300,10:poke v+301,0:poke v+302,19
180 vmf 3000,0,80:vcl v
190 r=13:if vmd(3002)<>66 then 8995
200 for i=0 to 59:if vmd(3010+i)<>i+1 then r=14:goto 8995
210 next
220 r=15:if vmd(3070)<>0 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
