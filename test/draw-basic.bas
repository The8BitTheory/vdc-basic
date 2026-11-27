# VDM, VDL, VDR, VDB: one small case per command, checked with VMD (smoke test without the monitor; the full check is test/draw/run_draw.py)
1 trap 9000
2 if peek(dec("1c03"))<>dec("30") then r=199:goto 8995
10 vmf 0,0,4000:vmf 8191,0,1
20 vdl 3,0,3,0,1
30 r=10:if vmd(0)<>16 then 8995
40 vdm 0,1000,80:vdl 0,0,7,0,1:vdl 8,1,8,3,1
50 r=11:if vmd(1000)<>255 or vmd(1081)<>128 or vmd(1161)<>128 or vmd(1241)<>128 then 8995
60 vdl 0,5,9,8,1
70 r=12:if vmd(1400)<>192 or vmd(1480)<>56 or vmd(1560)<>7 or vmd(1641)<>192 then 8995
80 vdr 0,10,16,3,1
90 r=13:if vmd(1800)<>255 or vmd(1801)<>255 or vmd(1880)<>128 or vmd(1881)<>1 then 8995
100 r=14:if vmd(1960)<>255 or vmd(1961)<>255 or vmd(1882)<>0 then 8995
110 vdm 1,3000,80:vdb 1,0,4,2,5
120 r=15:if vmd(3000)<>5 or vmd(3001)<>85 or vmd(3002)<>80 or vmd(3080)<>5 or vmd(3082)<>80 then 8995
130 r=16:if vmd(2999)<>0 or vmd(3003)<>0 or vmd(3160)<>0 then 8995
140 vmw 3200,165:vmw 3201,90:vdm 0,3200,80:vdb 3,0,10,1,2
150 r=17:if vmd(3200)<>186 or vmd(3201)<>162 then 8995
160 vdb 3,0,10,1,2
170 r=18:if vmd(3200)<>165 or vmd(3201)<>90 then 8995
180 poke dec("1c04"),0:vdm 1,3300,80:vdb 0,0,160,1,9:poke dec("1c04"),2
190 r=19:if vmd(3300)<>153 or vmd(3379)<>153 or vmd(3380)<>0 then 8995
200 vdm 0,0,80:vdb 65528,0,8,1,1
210 r=20:if vmd(8191)<>255 then 8995
220 vdr 5,5,0,3,1:vdb 5,5,3,0,1
230 r=21:if vmd(405)<>0 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
