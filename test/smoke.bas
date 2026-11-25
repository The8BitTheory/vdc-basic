# VDC-BASIC loads, installs, and VMW/VMD reach VRAM
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 vmw 2100,65:if vmd(2100)<>65 then r=10:goto 8995
20 if rgd(255)<>1 then r=11:goto 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
