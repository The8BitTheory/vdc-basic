# Statements made in the text of chapters 1-3
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 rem overlapping copy to a higher address repeats the first row
20 for i=0 to 79:vmw i,i:next:vmf 80,255,1920
30 r=1:vmc 0,80,1920:if vmd(80)<>0 or vmd(85)<>5 or vmd(165)<>5 or vmd(1925)<>5 then 8995
40 rem distance 8 repeats a pattern of eight bytes
50 a=2048:for i=0 to 7:vmw a+i,i+1:next
60 r=2:vmc a,a+8,792:if vmd(a+8)<>1 or vmd(a+15)<>8 or vmd(a+792)<>1 or vmd(a+799)<>8 then 8995
70 rem a keyword inside a variable name is a syntax error
80 r=3:trap 100:sync=1:goto 8995
100 r=4:if er<>11 then 8995
200 rem chapter 7: scroll down with one vmc, last row first
210 trap 9000:for i=0 to 24:vmf i*80,i+1,80:next
220 r=5:vmc 23*80,24*80,80,24,-80,-80
230 for i=1 to 24:if vmd(i*80)<>i or vmd(i*80+79)<>i then 8995
240 next:if vmd(0)<>1 then 8995
250 rem chapter 8: syn with register pairs sets the attribute start like attr
260 r=6:q=24000:syn 20,q/256,21,q and 255:if rgd(20)<>93 or rgd(21)<>192 then 8995
8990 r=0
8995 rst 3:print chr$(147):bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
