# Behaviour checks behind the command reference. Exit code = number of the first failing check.
1 trap 9000
2 bload"vdcbasic2h",b0,u8:sys dec("1300")
10 r=1:rgw 26,243:if rgd(26)<>243 then 8995
11 r=2:rga 26,240:if rgd(26)<>240 then 8995
12 r=3:rgo 26,5:if rgd(26)<>245 then 8995
13 r=4:if rgd(255)<>1 or rgd(254)>2 then 8995
20 r=5:vmw 2100,170:if vmd(2100)<>170 then 8995
21 r=6:vma 2100,15:if vmd(2100)<>10 then 8995
22 r=7:vmo 2100,64:if vmd(2100)<>74 then 8995
30 vmf 2100,0,1900
31 r=8:vmf 2100,7,1:if vmd(2100)<>7 or vmd(2101)<>0 then 8995
32 r=9:vmf 2110,8,256:if vmd(2110)<>8 or vmd(2365)<>8 or vmd(2366)<>0 then 8995
33 r=10:vmf 2500,9,2,3,80:if vmd(2500)<>9 or vmd(2501)<>9 or vmd(2581)<>9 or vmd(2661)<>9 then 8995
34 r=11:if vmd(2502)<>0 or vmd(2740)<>0 or vmd(2579)<>0 then 8995
40 vmf 2100,0,1900:for i=0 to 5:vmw 2100+i,i+1:next
41 r=12:vmc 2100,2300,6:if vmd(2300)<>1 or vmd(2305)<>6 or vmd(2306)<>0 then 8995
42 r=13:vmc 2100,2400,2,3,80:if vmd(2400)<>1 or vmd(2401)<>2 or vmd(2480)<>3 or vmd(2561)<>6 then 8995
43 r=14:vmc 2100,2700,2,2,80,3:if vmd(2700)<>1 or vmd(2780)<>4 or vmd(2781)<>5 then 8995
44 r=15:vmc 2100,3000,2,2,-80:if vmd(3000)<>1 or vmd(2920)<>3 or vmd(2921)<>4 then 8995
45 r=16:vmc 2104,3100,2,2,80,-2:if vmd(3100)<>5 or vmd(3101)<>6 or vmd(3180)<>3 or vmd(3181)<>4 then 8995
50 a=dec("5000"):bank 0:for i=0 to 15:poke a+i,i+1:next:bank 15
51 r=17:vmf 2100,0,1900:rtv a,2100,3:if vmd(2100)<>1 or vmd(2102)<>3 or vmd(2103)<>0 then 8995
52 r=18:vtr 2100,a+256,3:bank 0:x=peek(a+256):y=peek(a+258):bank 15:if x<>1 or y<>3 then 8995
53 r=19:vmw 2100,99:swp 2100,a,3:bank 0:x=peek(a):bank 15:if x<>99 or vmd(2100)<>1 then 8995
54 bank 0:poke a,1:bank 15
55 r=20:vmf 3500,99,40:vcc a,3500,2:if vmd(3500)<>1 or vmd(3507)<>8 or vmd(3508)<>99 or vmd(3516)<>9 or vmd(3523)<>16 then 8995
60 r=21:disp 4660:if rgd(12)<>18 or rgd(13)<>52 then 8995
61 disp 0
62 r=22:attr 4660:if rgd(20)<>18 or rgd(21)<>52 then 8995
63 attr 2048
64 r=23:crsr 4660:if rgd(14)<>18 or rgd(15)<>52 then 8995
65 r=24:syn:syn 26,18:if rgd(26)<>18 then 8995
66 r=25:rgw 26,0:rst 1:if rgd(26)<>240 then 8995
70 vmf 2100,0,1900:for i=0 to 7:vmw 3600+i,i+1:next
71 r=26:vcs 3600,1,2:a$="! ":vmp 3700,a$:if vmd(3700)<>3 or vmd(3780)<>4 or vmd(3701)<>1 or vmd(3781)<>2 then 8995
72 r=27:vcs 3600,1,2,40:vmp 3800,"!":if vmd(3800)<>3 or vmd(3840)<>4 or vmd(3880)<>0 then 8995
73 r=28:vmp 3900,"":if vmd(3900)<>0 then 8995
74 r=29:bank 0:poke a,34:poke a+1,33:bank 15:vmb 3900,a,2:if vmd(3900)<>5 or vmd(3940)<>6 or vmd(3901)<>3 then 8995
80 vmf 2100,0,1900:for i=0 to 5:vmw 2100+i,i+1:next
81 bank 0:for i=0 to 63:read b:poke a+i,b:next:bank 15
82 data 1,52,8,128,12,3,0,2,52,8,138,12,2,0,2,0,80,0,3,52,8,148,12,2,0,2,0,80,0,3,0,4,158,12,7,0,3,0,5,168,12,8,0,2,0,2,0,80,0,6,178,12,9,0,7,188,12,5,0,3,0,2,0,0
83 vcl a
84 r=30:if vmd(3200)<>1 or vmd(3202)<>3 or vmd(3203)<>0 then 8995
85 r=31:if vmd(3210)<>1 or vmd(3291)<>4 then 8995
86 r=32:if vmd(3220)<>1 or vmd(3300)<>4 or vmd(3301)<>5 then 8995
87 r=33:if vmd(3230)<>7 or vmd(3232)<>7 or vmd(3233)<>0 then 8995
88 r=34:if vmd(3240)<>8 or vmd(3321)<>8 or vmd(3242)<>0 then 8995
89 r=35:if vmd(3250)<>9 or vmd(3251)<>0 then 8995
90 r=36:if vmd(3260)<>5 or vmd(3262)<>5 or vmd(3264)<>5 or vmd(3261)<>0 or vmd(3266)<>0 then 8995
8990 r=0
8995 bank 15:poke dec("d7ff"),r:end
9000 r=200+er:goto 8995
