if(stat"11"!=4)stop"aerror:c pLEASE LAUNCH pico-8\nIN hi-res WITH command line:\nb-displays_x 2 -displays_y 2"
function a_(aa,ab)local ac,ad,c,ae,af,ag,ah={},{},0,1<<ab,0,0x3fff.ffff,1repeat
if(c>=ae)add(ac,c-ae)c=0
local ai=ad[ah]or 0x1.0001local mid=af+(ag-af)*(ai/(ai+(ai<<16)))local y=($aa>>>2<=mid)and 1or 0
if(y!=0)ag=mid else af=mid+0x0.0001
while(af^^ag)&0x3fc0==0do af=af<<8&0x3fff.ff ag=ag<<8&0x3fff.ff|0x0.00ff aa-=1end ad[ah]=ai+(0x.0001<<y*16)ah+=ah+y
if(ah>=2*ae)ah=1
c+=c+y until c==0return ac end function aj(a,b)return a..b end ak=max local function al(l,am)local v,i=l[1],1while v!=am do i+=1v,l[i]=l[i],v end l[1]=am end function an(ao,ap,k,aq)local l=ao[k]
if(not l)l={unpack(ap)}ao[k]=l
local v=l[1+aq]al(l,v)al(ap,v)return v end function ar(as,at,au,a,b,c,d)local av=au&16==0and b local ay=au&32==0and c local aw=a!=0return({function(r)return unpack(r,a,b)end,function(r)return(aw==r[0]and as[b]or at)(r)end,function(r)r[a]=av or r[b]return at(r)end,function(r)r[a]=not(av or r[b])return at(r)end,function(r)r[a]=(av or r[b])==(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])+(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])-(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])*(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])/(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])\(ay or r[c])return at(r)end,function(r)r[a]=(av or r[b])%(ay or r[c])return at(r)end,function(r)return(aw==((av or r[b])<(ay or r[c]))and as[d]or at)(r)end,function(r)return(aw==((av or r[b])<=(ay or r[c]))and as[d]or at)(r)end,function(r)local x,y,z=r[a](unpack(r,a+1,b))for j=c,d do r[j],x,y=x,y,z end return at(r)end,function(r)r[a]=d(av or r[b],ay or r[c])return at(r)end,function(r)d(r[a],av or r[b],ay or r[c])return at(r)end})[1+au%16]end function ax(az,b_,ba)local as,at={}for i=b_ or#az,1,-5do at=ar(as,at,unpack(az,i-4,i))add(as,at)end return function(...)return at{...}end end ax(a_(837,8))(_ENV,rawget,split"ak,_ENV,rawget,rawset,unpack,pack,ax,shl,a_,an,memset,rawlen,band,bor,sgn,shr,peek4,lshr,aj,peek,bxor,peek2,0.5,0.25,0x8.e666,0x8060.004,0x.6666,0x.199a,0x.0001,0x.000a,0x.e666,0x400a.0988,0x400d.0c0b,0x4010.0f0e,0x4013.9211,0x40c0.2f2e,0x40b4.3332,0x40c0.3a39,0x.03,ipairs,qa,df,ii,cc,peek,poke,stat,qb,pt,om,ak,dl,dm,qc,h,w,km,a,mw,bx,memcpy,dh,di,min,qd,qe,eb,my,qf,ai,o_,_map_display,kh,kj,cos,{,em,pi,ms,sin,qg,add,qh,sspr,poke4,camera,qi,},er,bl,qj,nh,eg,eq,eo,kk,bo,et,eu,d_,ge,br,hv,id,ie,hp,gx,fg,ff,po,qk,unpack,fl,bt,bp,bz,c_,pf,ql,pk,bk,qm,lp,ix,qn,il,iq,ip,qo,qp,qq,iz,qr,qs,max,qt,hi,hg,ho,i_,ib,peek2,cls,peek4,li,lh,qu,bu,db,qv,srand,ky,rnd,x,y,c,qy,qw,jb,=,deli,cz,t,ij,sfx,he,mid,ez,hb,qx,ir,iv,iy,qz,btnp,r_,type,hl,ra,,table,rect,rb,rc,rd,chr,re,rf,rg,rh,clip,ri,jv,sgn,ju,js,je,rj,jq,HIGH SCORERS,rk,rl,rm,rn,é to play,ó music on/off,ãë start level ,/,entering the,ba,ro,tower completed!,BONUS POINTS,jy,TIME LEFT 10?,TECHNIQUE 10?,	EXTRAS    10?,TIME UP,jp,rp,jd,rq,io,lo,CATCH SOME FISH,FOR BONUSES,CONGRATULATIONS,YOU HAVE,COMPLETED,\nYOUR MISSION,GAME OVER,please enter,your initials,rr,memset,ot,a_,rs,rt,ru,rv,ry,rw,rx,rz,s_,sa,sb,kl,cs,sc,sd,cartdata,carlc27843_nebulus,rectfill,pal,d,v,se,sf,q,flip,m,bf,sg,sh,s,si,e,sj,sk,sl,poke2,n,sm,yield,pr,pairs,costatus,sn,dead,pa,coresume,so,ord,sub,all,split,ig,hz,sp,cocreate,sq,iu,ps,pq,is,pp,music,it,am,ox,ik,sr,ss,st,su,sv,sy,iw,pe,ph,sw,sx,sz,t_,ta,tb,tc,ow,_init,0x1c14.0004,0x1868.0006,0x1604.0006,0x1069.0008,0xa4d.0008")function bb(bc,t)for k,bd in pairs(bc)do local be,bf for i=1,#bd,2do local bg,bh=be,bf be,bf=bd[i],tonum(bd[i+1])if t==be then _ENV[k]=bf
if(k=="bi")sfx(bi)
break end if t<be then
if(bh and bf)_ENV[k]=bh+(t-bg)*(bf-bh)\(be-bg)
break end end end end function bj(p)if not p then repeat p=bk bk=bk%8+1until 1<<p&bl.bm==0end return p end function bn()
if(bo>=9or#bp>=4)return
local bq=(br+73)\8local bs=bt\16if bs<bu and bs<bq then repeat local t=bv[@(-14900+bt)]if t then local by,v,p=unpack(t)local a=(bt%16*16+8)%256local y=bs*8add(bp,{a=a,y=y,v=v,c=0,bw=0,bx=6,by=by,p=bj(p)})bt+=1return end bt+=1until bt&15==0else
if(bz<0)return
bz-=1
if(bz>=0)return
c_*=-1sfx"32"add(bp,{x=c_>0and-9or 132,y=br,v=c_,c=0,bx=7,p=bj()|256})end end function ca()for i=#bp,1,-1do local e=bp[i]if not e.a then deli(bp,i)else cb(e,4)end end end function cb(e,bx)e.bx=bx e.c=0
if(bx==4)e.cc=126
end function cd(e)return ce(e.a\2,e.y,-2,2,15)end function cf(e)return e.a and e.bx!=4and e.bx!=6end function cg(ch,ci)local cj=ck[ci or 1]for e in all(bp)do if e!=ch then if cf(e)then local cl=(ch.a\2-e.a\2+64)%128-64+cj.cm local cn=e.y-ch.y+cj.co
if((cj[cn]or 0)>>cl&1>0)return e
elseif not e.a and ci==3then local cl=64-e.x local cn=br-e.y
if(cl>-10and cl<10and cn>=-16and cn<14)return e
end end end end function cp(e)local cq=e.a\16%16local cr=-14900+(e.y\8-1)*16local c=cs[@(cr+cq)]
if(c==1)return 2

if(c)return 0
local cl=e.a\2%8
if(cl>1and cl<6)return 1
cl=(cl>1and 1or-1)return cs[@(cr+(cq+cl&15))]and(sgn(e.v)!=cl and 2or 0)or 1end function ct(e)local cu=e.a e.a=(e.a+e.v)%256
if(cd(e)or cg(e))e.v*=-1e.a=cu
end function cv(e)local cn=cy[1+e.c]local cw=cn while true do e.y+=cn
if(e.y<0)e.y=0cb(e,4)return 0

if(not cd(e)and not cg(e))return
e.y-=cn cn-=sgn(cn)
if(cn==0)return cw<0
end end local cx={function(e)ct(e)local r=cv(e)
if(r==0)return
e.c=min(e.c+1,20)if r then if e.v==0and bo==1and br==e.y then local cz=(d_-e.a+128)%256-128
if(abs(cz)<64)e.v=sgn(cz)
end sfx"33"e.c=0end e.cc=124end,function(e)ct(e)e.cc=@(-12884+e.c%16)e.c+=1local r=cp(e)
if(r==0)return

if(r==1)cb(e,3)e.c=20else e.v*=-1
end,function(e)e.cc=@(-12884+e.c%16)e.c+=1local r=cv(e)
if(r==0)return

if(r)cb(e,2)
end,function(e)e.c+=1e.cc=@(-12840+e.c)
if(e.c>23)return 1
end,function(e)e.c-=1e.cc=123
if(e.c<0)cb(e,2)
end,function(e)e.c+=1e.cc=@(-12864+e.c)if e.c>23then e.c=0
if(not cg(e))cb(e,e.by)e.c=e.bw
end end,function(e)
if(e.c>202)bz=600return 1
e.x+=(e.c<128and e.c&1or 1)*e.v e.c+=1e.cc=@(-12916+e.c%32)end,function(e)local da=e.y e.y+=e.v
if(e.y<0)e.y=0cb(e,4)return

if(cd(e)or cg(e))e.v*=-1e.y=da
e.c+=1e.cc=@(-12948+e.c%32)+db end,function(e)ct(e)e.c+=1e.cc=@(-12948+e.c%32)+db end}function dc()local dd=br-96for i=#bp,1,-1do local e=bp[i]if cf(e)and(e.y<dd or cd(e))then cb(e,4)de(e)elseif cx[e.bx](e)then deli(bp,i)elseif e.a then de(e)end end end function df(dg,dh,di)dg.dh=dh dg.di=di local dj=dg.a local dk=dg.dl\2local cz=di*64+dh\2for y=1,dg.dm do poke(cz,peek(dj,dk))dj+=dk cz+=64end end function dn(dp)for dq=dp.dr,dp.ds do local dt=du[dq]dt.t-=1if dt.t<=0then dt.t+=dt.dv dt.i=(dt.i+dt.dy)%dt.dw local dx=dt.dx+dt.i local dz=dt.dz for b=1,dt.e_ do poke(dz,peek(dx,dt.ea))poke(dz+dt.eb,peek(dx+dt.eb,dt.ea))dx+=dt.ec dz+=dt.ed end end end end function ee(a)camera()clip()local ef=eg*64poke(24405,0)local bg=a*1.5\1local eh,ei=0,0for ej=1,2do local ek=0for el in all(em)do local en=eo[el]local dh=(ep[el]+bg)%48
if(en!=ek)poke4(24320,peek4(en+ei,4))ek=en
sspr(dh,0,1,eg,el+eh,128-eg,1,eg)end ei,eh=eq,64end poke(24405,0x60)end function er()for a=28992,29440,64do memcpy(a+16,a,16)memcpy(a+48,a+32,16)end end function es(bx)bo=bx+0et=0
if(bo>2)eu=0
ev=0end function ey(ew)es"2"ex=ez f_=fa[ew]end function fb(fc)es"3"fd=1ex=0fe=-1if fc==1then ff=@(-13236+fg*4)elseif fc==2then ex=(fg==0and-1or 1)ff=@(-13212+fg*4)elseif fc==3then ex=(fg==0and 1or-1)ff=@(-13204+fg*4)elseif fc==5then et=1else fd=3ff=@(-13236+fg*4)end end function fh(fi,fj)fk=fi for i,e in ipairs(fl)do
if(e.cr==fj)deli(fl,i)break
end es"7"end function fm()es"9"br=0ca()sfx"38"end function fn()for i=#fl,1,-1do local e=fl[i]if e.fo>0then e.fo-=1
if(e.fo==0)sfx"43"
else poke(e.cr,e.t&192)e.cr-=16e.t=@e.cr local fp=e.t&192==128poke(e.cr,fp and 134or 198)
if(fp)deli(fl,i)
end end end function ce(x,y,fq,fr,fs)local ft=x+fq local fu=x+fr local fv=(y+fs)\8local fy=y\8for fw=ft\8,fu\8do local fx=fw*8local cq=fw&15local cr=-14900+fv*16+cq for fz=fv,fy,-1do local t=@cr local g_=ga[t]if g_ then if(ft<fx+g_[2]and fu>fx+g_[1])then return t,cr end end cr-=16end end return false,nil end function gb(cz)local gc=d_+cz local gd=gc\2
if(ce(gd,br,-4,3,17))return 1
d_=gc ge=gd end function gf()local bg=d_%16
if(bg==8)return
gb(bg<8and 1or-1)return 1end function gg(cn)local gh=max(0,br+cn)
if(ce(ge,gh,-4,3,17))return 1
br=gh end function gi()local fz=br\8-1local gj=(ge-6)\8&15local gk=(gj+1)&15local cr=-14900+fz*16local gl=cs[@(cr+gj)]local gm=cs[@(cr+gk)]local gn=(gl and 2or 0)+(gm and 1or 0)local t=gl or gm or 0return go[1+t][1+ge%8]>>(gn*4+fg*2)&3end function gp()local cq=ge\8&15local fz=br\8local t=@(-14900+fz*16+cq)return(t==7or t==8)and t end function gq()local cq=ge\8&15local fz=br\8-1return-14900+fz*16+cq end function gr()local cr=gq()local t=@cr
if(t==70)return cr,-1

if(t==134)return cr,1

if(t==198)return cr,0
return cr,nil end function gs()local cr=gq()
if(@cr==2)poke(cr,0)sfx"40"
end function gt()
if(ez==0and@gq()==3)gb(1)
end function gu()
if(bo>=9)return

if(br<=0)fm()return

if(bo==8)return

if(bo==5and(et>=28or et<116))return
if gv then
if(bo==7and br&7!=0)return
else local e=cg({a=d_,y=br},3)if e then gy=e.a and 40or 30else
if(bo!=7or not ce(ge,br,-4,3,17))return
gy=30end
if(bo==7and br&7!=0)gv=1return
end
if(bo==7)gw(gr())
gv=nil sfx"37"es"8"gx=max(gx-1)end function gw(fj)local t=@fj&192poke(fj,t|6)
if(t!=128)add(fl,{fo=600,cr=fj,t=t})
end local gz={function()gs()local fc=gi()
if(fc!=0)return fb(fc)
gt()if ez==0then
if(eu>0)sfx"34"
eu=0if h_ then
if(not ha)sfx"39"es"6"
else ff=@(-13244+fg*4)if hb!=0then local fj,fi=gr()
if(fi==0or fi==hb)return fh(hb,fj)
if hb>0then hc=gp()
if(hc)sfx"35"return es"5"
end end end elseif(ez>0)!=(fg>0)then es"4"else
if(h_)eu=0sfx"36"return ey(1)

if(gb(ez))return ey(2)
eu+=1ff=@(-13364+fg*32+eu%32)
if(eu&15==0)sfx"34"
end end,function()eu+=1ff=@(-13364+fg*32+eu%32)gb(ex)local cw=f_[1+et]local cn=cw while gg(cn)do cn+=(cn<0and 1or-1)if cn==0then
if(cw<0)sfx"34"return es"1"
break end end et+=1if 1+et>#f_ then fb(gg(-1)and 5or 4)end end,function()if et==0then fe+=1
if(gb(ex))ex=0
while gg(-fd)do fd-=1if fd==0then
if(ex==0or fe>=4and(fe&1>0))et+=1
break end end end if et==0then fd=mid(1,4,fd+fe%2)else
if(et==1)sfx"34"
hd=0ff=@(-13229+fg*8+et)et+=1
if(et==9)es"1"
end end,function()ff=@(-13300+fg*28+et)et+=1if et==28then fg^^=1es"1"end end,function()if et<16then ff=@(-13196+fg*16+et)gf()elseif et<40then ff=@(-13180+et)elseif et<104then ff=nil
if(hc==8)sfx"49"he(5)fl={}return es"10"
d_+=(fg>0and 2or-2)ge=d_\2elseif et<128then ff=@(-13244+et)elseif et<129then
if(not gg(-4))return

if(ez!=0)fg=(ez>0and 1or 0)
sfx"34"else ff=@(-13221+fg*16+et)end et+=1
if(et>144)return es"1"

if(et&15==0)sfx"48"
end,function()gt()ff=@(-13060+fg*16+et)local cz=fg*2-1
if(et==4)ha={t=1,a=d_+cz*2,y=br,bh=cz*2,cc=65}
et+=1
if(et==16)es"1"
end,function()if et==0then
if(gf())return
et+=1local fj=gr()poke(fj,@fj&192|(fk>0and 5or 0))end br+=fk
if(br&7!=0)return
local fj=gr()local t=@fj&192if fk<0then sfx"42"
if(t!=128and(t!=192or hb<0))poke(fj,t)return
else sfx"41"
if(t!=64and(t!=192or hb>0))poke(fj,t|5)return
end gw(fj)es"1"end,function()if et<gy then ff=@(-13028+fg*16+et%16)ev=ev%1+hf[min(1+et,32)]br+=ev\1
if(br<0)fm()return
et+=1else ff=@(-13236+fg*4)br-=4
if(br<0)fm()return

if(ce(ge,br,-4,3,17))return
fb(1)end end,function()if et<48then ff=@(-12996+fg*24+et\2)else ff=nil
if(et==112)hg-=1he(hg>0and 4or 8)
end et+=1end,function()if et<1then bp={}return elseif et<31then elseif et<43then ff=@(-13147+et)
if(et==42)sfx"45"
elseif et<44then if bu>6then br-=8local cz=-14900+(bu-6)*16poke(cz,peek(cz+16,5*16))bu-=1return end fg=1elseif et<56then ff=@(-13148+et)elseif et<72then ff=@(-13148+fg*16+et)else bb(hh,et-72)if not et then et=0hi+=1he(hi<=8and 7or 8)return end end et+=1end,function()bb(hj,et)
if(et==nil)return es"1"
et+=1end,}function de(s)add(hk[1+(s.a\16)],s)end function hl(a,hm)local hn=ho ho+=a>>16
if(ho/5000!=hn/5000)hg=min(hg+1,8)
hp=min(hp+(hm>>16),0x.01f4)end function hq()local c=ha.t-24if c>0then
if(c>=8)ha=nil return
ha.cc=65+c\2else ha.a+=ha.bh ha.a%=256ha.y+=hr[ha.t]local t,cr=ce(ha.a\2,ha.y,-1,0,7)if t then
if(t==4)poke(cr,0)hl(50,2)sfx"40"
ha.t=24else local e=cg(ha)if e then if e.bx==2then cb(e,5)e.c=180elseif e.bx!=5and e.bx!=8and e.bx!=9then sfx"40"cb(e,4)hl(100,5)end ha.t=24end end end ha.t+=1de(ha)end function hs()local cz=d_ for i=1,16do hk[i]={}end fn()
if(ha)hq()
gz[bo]()gu()cz=d_-cz d_%=256for i=1,4do ht[i]-=cz/hu[i]end local cn=max(br,bo>9and 16)-hv hv+=ceil(abs(cn)/8)*sgn(cn)hy=hv-100hw=hy+199hx=hw+32bn()dc()
if(hz==0)i_+=.25
ia=min(ib-(100-min(-hy,hw+1))\4,ib-1)local ic=br\8-id\8if ic>0then id=br hl(10*ic,0)end if bo<9then if ie==0then
if(ig==2)he(6)
return end ie-=1end end function ih()df(ii[130+ij%16\2],10,24)
if(ik()and#il<6and rnd()<.05)add(il,{x=132,y=64+rnd"88",im=rnd"2"-1,c=bj(),a=0})
for i=#il,1,-1do local f=il[i]f.x-=1f.y+=f.im
if(f.y<56or f.y>160)f.im*=-1
f.a+=.25f.a%=8if f.x<-4then deli(il,i)elseif f.io then local cl=18-abs(f.x-ge)local cn=f.y-ip+20
if(cl>0and cn>-cl and cn<2*cl)deli(il,i)hl(50,0)sfx"53"
elseif iq and abs(f.x-iq)<8and abs(f.y-ir+0)<16then f.io,f.im,iq=1,0,nil sfx"40"end end end function _update60()is(stat(54))
if(btnp"5")it(not iu)
ez=tonum(btn"1")-tonum(btn"0")hb=tonum(btn"2")-tonum(btn"3")h_=btn"4"iv=iy==ig and btnp"4"ij+=1
if(iy!=ig and hz>30)iw()
hx=255
if(ix)for u in all(iz)do u.x=(u.x+u.j_)%u.w end
ja[iy]()for b in all(jb)do if b.a then local t=t()+b.cz b.x+=sin(t)*b.cz b.y+=b.a+abs(cos(t))b.p=@(-12816+b.t%16)b.t-=1
if(b.t==0or b.y>160)b.a=nil
end end end function jc(t)for i,d in ipairs{t\1000%10,t\100%10,t\10%10,10,t%10}do poke4(jd,8,d,29+10*i-(d==1and 2or 0),8)jd+=16end end function je(cc,a,y,jf,jg)poke4(jg and jh or jd,7,cc,a,y,jf)if jg then jh+=20else jd+=20end end function ji(jj,jk,jl)for i,u in ipairs(iz)do jj+=u.dm local jm=min(jl,jj)local jn=max(jk,jj-u.dm+1)local cn=hx-jj if jm>=jn and cn<jo then local dj=24576+(hx-jm-jp)*64if u.cc==0then poke4(jd,5,dj,jm-jn+1,0x1111.1111,0)jd+=20else poke4(jd,9,cn,i,dj,jm-jn)jd+=20end end end end function jq(s,x,y,c,jr)local cn=hx-y-7
if(cn+7+js<jp or cn-js>jp+127or cn>=jo)return
add(jt,s)poke4(jd,6,#jt,x+ju,cn,jr and c or jv|js,jr and 1or 0)jd+=24end function jy(s,n,y)jq(s..(n\100%10)..(n\10%10)..(n%10),0,y)end function jw(jx,jz,k_,ka)poke4(jd,11,jh-jd)jd+=8local kb=jd local kc=-14900+jx*16local kd=hx-jx*8-7for ke=1,16do k_>><=1if k_&1>0then local cq=(kf+kg[ke])&15local cr=kc+cq local j_=kh[(cq*16+8-d_)%256]local im=kd local ki for fz=jx,jz do local t=@cr if t!=0then if t==7or t==8then local ft=kj[(cq*16-d_)%256]local fu=kj[(cq*16+15-d_)%256]if fu>=ft then poke4(jh,2,ft,fu,im,kk[ft-32],kk[fu-32])jh+=24end else local dg=kl[t]if dg then
if(not ki)ki=jd+16poke4(jd,1,ki,j_-dg.dl\2,0)jd=ki
poke4(jd,dg.km,im)jd+=8end end end cr+=16im-=8end
if(ki)poke4(ki-4,jd)
for s in all(hk[1+cq])do je(s.cc,s.a,s.y,s.p)end end end
if(ka and ff)je(ff,d_,br,0,bo==5)
poke4(jh,11,kb-jh)jh+=8end function kn(jp,ko,kp,kq)
if(kq<kp)return
local cz=24576+(hx-kq-jp)*64local bg=((ko-kq)%eg)*64poke4(jd,4,cz,bg,kq,kp)jd+=20end function kr(jk,jl,ks,kt)for ku=(jk-4)\16,jl\16do local kv=ky[1+ku]if kv then local x=(d_*2+kv.x)&511
if(x>=ks and x<kt)je(71,x,kv.y,kv.c^^ij&1)
end end end function kw(jk,jl,kp,kq)local jx=kp\8local jz=kq\8local kx=hx-jl-jp local kz=hx+1-kp-jp kz=mid(kx,jo-jp,kz)local l_=kx>>8|kz<<8local la=(bu-6)*8kn(jp,la-(bl.h-6)*8,max(kp,la),kq)kn(jp,0,kp,min(kq,la-1))local lb=min(ia,jl)if lb>=kp then local lc=ia-lb local cz=24576+(hx-lb-jp)*64local dj=-15444+lc*8poke4(jd,3,dj,cz,kp,lb,bu*8)jd+=24end local ld=le[lf]local lg=max(max(ia,kq)+1,jk)if jl>=lg then poke4(jd,5,24576+kx*64,jl+1-lg,lh,li)jd+=20local lj=mid(kx,kz,hx+1-lg-jp)poke4(jd,10,128|kx>>8|lj<<8,ld)jd+=12kr(lg,jl,-2,130)end poke4(jd,10,lk[1]|l_,ld)jd+=12kr(kp,kq,-2,34)jw(jx,jz,ll)poke4(jd,10,lk[2]|l_,ld)jd+=12kr(kp,kq,94,130)jw(jx,jz,lm)poke4(jd,10,128|l_,ld)jd+=12jw(jx,jz,ln,1)if bo==7and et>0then je(58,d_,br-8)end for e in all(bp)do
if(not e.a)je(e.cc,e.x,e.y,e.p)
end lo(64)end function lo(x)if lp then je(lp,x+lq,ip,256)for i=1,3do je(lp+i,x-20+i*10,ip-20,256)end end end function lr(ls,lt,lu)local lv,ly=4992,lu*64for be=1,0,-1do local jp=be*128local lw=max(0,ls-jp)local lx=min(127,lt-jp)if lw<=lx then local lz=(lw-lx+lu)\lu for gn=0,1do _map_display(be*2+gn)local m_=lv+gn*32local dj=24576+lx*64for i=1,lz do poke4(m_,peek4(dj,8))m_+=64dj+=ly end end lv+=lz*64lt+=lz*lu end end end function ma(mb,mc)local md=24576+mb*64local me=24576+mc*64for dy=2,3do _map_display(dy)if mc>mb then local mf=md-me for dj=me,31168,128do poke4(dj+mf,peek4(dj,16))end elseif mc<mb then local mg=me-md for cz=31104+mb%2*64,md,-128do poke4(cz,peek4(cz+mg,16))end end end end function mh(lw,lx,mi,lu)poke4(24360,0)poke(24371,2)poke4(24320,0x0302.0100,0x0706.0504,0x0b0a.0908,0x0f0e.0d0c)local di,mj=78,lu*64for gn=0,1do _map_display(2+gn)local dh=gn*64local mk,ld=-32704,0for ej=0,1do poke4(24352,mk,0,ld)local ml,mm,mn,mo,mf,di=mi,1,mp[1],ht[1]&3,lw*64+ej*32,di for cn=lw,lx,lu do while 1+ml>=mn do poke4(24416,peek4(-19068+mm*32+ej*16,4))mo=ht[mm]&3mm+=1mn=mp[mm]end poke2(24369,mq[1+ml]>><mo)local mr=dh-ms[i_+ml&31]sspr(mr,di,64,1,0,cn)if mr<0then poke2(24576+mf,peek2(24578+mf))elseif mr>64then poke2(24606+mf,peek2(24604+mf))end mf+=mj ml+=lu di+=1end mk,ld=0x8080.004,0x.ffc end end poke2(24369,0)poke(24371,0)memcpy(24416,-18908,16)end function mt(mu,x,mv)local dg=ii[$mu]local my=dg.mw.my[1]if x<mx+64and x+dg.dl>mx then for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke4(24320,peek4(my[ej],4))for jd=mu,mv-1,8do local cc,y=peek4(jd,2)local dg=ii[cc]sspr(dg.dh,dg.di,dg.dl,dg.dm,x,y)end end end return 16+mv-mu end function na(nb,nc,nd,ne,nf)local ng=nd+7local pal=nh.ni for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke(24320,pal[ne],pal[nf],pal[7])
if(nc>nb)rectfill(nb+1,nd,nc-1,ng,2)
line(nb,nd,nb,ng,0)line(nc,nd,nc,ng,1)pal=nh.nj end return 24end function nk(dj,cz,nl,nm,nn)local no=16
if(mx!=0)cz,no=cz+16,-16
for y=nm,nl,-1do local np,nq=peek4(dj,2)dj+=8poke4(cz,np,np,np,np)poke4(cz+32,nq,nq,nq,nq)
if(y>=nn)poke4(cz+no,np,np,np,np)poke4(cz+no+32,nq,nq,nq,nq)
cz+=64end return 24end function nr(cz,bg,kq,kp)local ef=eg*64local dj=8192-ef
if(mx==0)cz+=16else dj+=16
local ns=cz^^16local l,r,poke4=lh,li,poke4 for nt=kp,kq do poke4(ns,l,l,l,l)poke4(ns+32,r,r,r,r)poke4(cz,peek4(dj+bg,4))poke4(cz+32,peek4(dj+bg+32,4))cz+=64ns+=64bg=(bg+64)%ef end return 20end function nu(cz,h,l,r)for y=1,h do poke4(cz,l,l,l,l,l,l,l,l,r,r,r,r,r,r,r,r)cz+=64end return 20end function nv(ny,cl,cn,c,jr)local s=jt[ny]if jr==1then for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke(24321,c&15)print(s,cl,cn,1)c\=16end else local nw,dj=c%1,c\1local p=ii[158]local nx,nz=ord(s,1,2)local r=o_[nz]local oa=r<<5&31r+=du[23+nx].i%oa for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke4(24320,peek4(r,4))poke(24320,16)local x=cl for i=3,#s do local nx=ord(s,i)i+=1nx=max(nx-47)local dh,di=p.dh+nx%16*8,p.di+nx\16*8sspr(dh,di,8,8,x,cn+dj*sin(nw+x/48))x+=8end r+=oa+7end end return 24end function ob(cc,m_,jj,jf)local cl=jf>=256and m_ or kh[(m_-d_)%256]local dg=ii[cc%200]local dl,my=dg.dl,dg.mw.my[1+jf&255]cl-=dl\2if cl<mx+64and cl+dl>mx then local dm,oc=dg.dm,cc>=200local cn=hx-jj-dm+1for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke4(24320,peek4(my[ej],4))sspr(dg.dh,dg.di,dl,dm,cl,cn,dl,dm,oc)end end return 20end function od(i,cl,cn)local dg=ii[82]if cl<mx+64and cl+dg.dl>mx then local my=dg.mw.my[1+i]for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke4(24320,peek4(my[ej],3))sspr(dg.dh,dg.di,dg.dl,dg.dm,cl,cn)end end return 16end function oe(y,i,dj,of)local u=iz[i]local p,og,oh=ii[u.cc],u.x\1,u.w==16local my=p.mw.my[1]for ej=1,2do poke4(24352,mz[ej],0,n_[ej])poke4(24320,peek4(my[ej],4))sspr(p.dh+og,p.di,p.dl-og,p.dm,mx,y)sspr(p.dh,p.di,og,p.dm,mx+p.dl-og,y)local a=dj for _=0,of do
if(oh)poke4(a+8,peek4(a,2))
poke4(a+16,peek4(a,4))a+=64end dj+=32end return 20end function oi(mk,ld)n_[1]=mx>>16|ld n_[2]=mx-64>>>16|ld local oj=mk<<16&255local ok=mk&255oj=mid(0,oj-mx,64)ok=mid(0,ok-mx,64)mz[1]=mk&0xff00.ff|oj>>16|ok mz[2]=mz[1]+0x40.004return 12end function ol(om)return om end function on(jd,oo)while jd!=oo do jd+=op[$jd](peek4(jd+4,5))end end function oq()poke4(jd,5,24576,32,0,0)jd+=20poke4(jd,10,-32640,0)jd+=12jc(ie\6)for i=0,hg-1do je(69,100+i%4*7,hx-7-i\4*8,256)end local s=ho for i=0,7do je(72+(s%0x.000a<<16),30-i*4,hx-8,256)s/=10end end function _draw()jo=os[1+hz]local g=(iy==2)dn(g and bl or ot)if g then ee(d_)
if(ff)df(ii[ff%200],40,24)
for i,e in ipairs(bp)do
if(e.cc)df(ii[e.cc],unpack(ou[i]))
end ll=0lm=0ln=0kf=d_\16&15for ke,ov in ipairs(kg)do local j=(kf+ov)&15local a=(j*16+8-d_)%256if a<=64or a>=192then ln|=1<<>ke else local og=kh[a]
if(og<40)ll|=1<<>ke

if(og>88)lm|=1<<>ke
end end end local oy for be=1,2do lf=be local dy=be&2jp=dy*64jt={}jd=-18516jh=-16468if jp==0then
if(g)oq()
else poke4(jd,5,31232,24,0,0)jd+=20end local jl=hx-jp local jk=jl-127local kp if g then jk=max(jk,hy)jl=min(jl,hw)kp=max(jk,0)local kq=min(jl,bu*8-1)kw(jk,jl,kp,kq)elseif iy>1then if jp==0then
if(not lp)poke4(jd,3,-15444,29504-ib*64,1,ib,0)jd+=24

if(iy>=4)oq()else poke4(jd,5,24576,77-ib,lh,li)jd+=20
end poke4(jd,10,mid(0,jo-jp,128)<<8|128,le[be])jd+=12ji(hx-232,jk,jl)ow()
if(not lp and 69<jo)oy=ox
else poke4(jd,5,24576,128,lh,li)jd+=20end for b in all(jb)do
if(b.a)je(b.p,b.x,b.y,256)
end for gn=0,1do _map_display(dy+gn)mx=gn*64on(-18516,jd)end end
if(g and hy<0)oz()

if(oy)oy()

if(hz>0)p_()hz=(hz+1)%60else pa()
end function pb(ls,lt,pc)local pd=jo-128if hz==1then pe(ls,lt,0)elseif pd>=ls then if hz>30and pf<-hy then local pg=min(-hy-pf,21)
if(pf>0)ph(ls,(min(pd,ls+pf-1)))
ls+=pf lt=ls+pg-1pc-=pf lr(pc-pg+1,pc,-1)mh(ls,lt,pf,1)pe(ls,lt,pf)pf+=pg else ph(ls,(min(pd,lt)))end end end function oz()pi^^=1local ls=hy+104local lt=103local pc=ls+127if hz==0then local pj=pc-lt+ls lr(pj,pc-pi,-2)
if(pk and pk!=ls)ma(ls+1-pi,pk+1-pi)
pk=ls mh(ls+pi,lt,pi,2)else pb(ls,lt,pc)end end function p_()poke2(24321,7)local pl=1+hz*.05for dy=0,3do _map_display(dy)poke4(24360,le[1+dy\2])local cn=dy\2*128local pm=dy%2*4for pn=0,8do local w=15*sin(pl/4+pn*.03)\1if w>0then for el=0,4do local y=pn*32+20*sin(pl+(pm+el)*0.1)\1
if(hz>=30)y=257-y
local t,b=y-2*w+1,y+2*w if max(t,0)<jo then local l,r=el*16-w+1,el*16+w poke4(24352,-32704)ovalfill(l,t,r,b,1)poke4(24352,0x8080.004)ovalfill(l+64,t,r+64,b,2)end end end end local y=max(jo-cn)nu(24576+y*64,128-y,0x7777.7777,0)end end function po(d,s,f,b,e)poke(d,f(s,b or 1,e or#s))end am"{X{X0X0X80X1501X10X1X-20724X10X-25124X15636XX20X0X10X110X}X{X0X0X16X257X4X9X-25196X36X-25836X15342X{X0X5X10X15X20X25X30X35X}X16X0X10X80X}X{X0X0X16X70X3X1X-25842X3X-26162X15268X{X40X45X50X55X}X16X0X10X40X}X{X0X0X32X50X8X1X-26186X12X-26186X15203X{X1048X1056X1064X1072X}X8X2X16X16X}X{X0X0X20X146X13X1X-26242X28X-26242X15022X{X1536X1541X1546X1551X}X20X2X10X40X}X{X0X0X8X21X3X1X-26248X3X-26248X14997X{X1080X1082X1084X1086X}X8X2X4X16X}X{X0X0X7X19X7X1X-26262X7X-26262X14968X{X1590X}X7X2X6X6X}X{X0X0X24X9X14X1X-26290X14X-26300X14944XX24X1X96X96X}X{X0X0X5X11X4X10X-26380X40X-26380X14892X{X2106X}X5X2X4X4X}X{X0X0X8X49X7X1X-26394X7X-26394X14833X{X2086X2088X2090X2092X2094X2096X2098X2100X2102X2104X}X8X2X4X40X}X{X0X0X16X39X9X11X-26592X99X-26592X14690X{X1561X}X16X2X10X10X}X{X0X0X16X191X7X9X-26718X63X-27358X14433XX16X0X10X80X}X{X0X0X16X185X5X9X-27448X45X-28216X14201XX16X0X12X96X}X{X0X0X16X242X8X9X-28360X72X-29000X13884XX16X0X10X80X}X{X0X0X16X230X6X9X-29108X54X-29748X13597XX16X0X10X80X}X{X0X0X16X232X6X9X-29856X54X-30496X13308XX16X0X10X80X}X{X0X0X16X61X5X9X-30586X45X-30826X13200XX16X0X10X30X}X{X0X0X16X80X4X9X-30898X36X-31138X13083XX16X0X10X30X}X{X0X0X23X127X7X1X-31152X7X-32210X12946X{X6272X}X23X0X92X92X}X{X0X0X20X95X5X1X-32220X5X-32540X12844XX20X0X4X32X}X{X0X0X28X37X4X1X-32548X4X-32548X12802X{X2824X}X28X2X16X16X}X{X0X0X23X90X3X1X-32554X3X-32554X12708X{X2848X}X23X2X32X32X}X{X0X0X16X37X2X1X-32558X2X-32558X12668X{X1566X}X16X2X16X16X}X{X0X0X8X19X2X1X-32562X2X-32562X12646X{X1574X}X8X2X16X16X}X{X0X0X8X16X2X1X-32566X2X-32566X12627X{X1582X}X8X2X16X16X}X{X0X0X18X62X3X1X-32572X3X-32572X12561X{X2870X}X18X2X16X16X}X{X0X0X24X159X3X1X-32578X3X-32578X12398X{X2832X}X24X2X32X32X}X{X0X0X30X102X3X1X-32584X3X-32584X12292X{X2816X}X30X2X16X16X}X{X{X6X6X6X6X5X5X0X}X{X0X0X0X1X0X1X0X}X24X238X8X1X23968X48X23728X12004XX24X1X48X48X}X{X{X6X6X6X6X13X5X0X}X{X0X0X0X1X0X0X0X}X24X202X16X1X23188X270X22978X11524XX24X1X48X48X}X{X{X14X14X14X14X4X2X0X}X{X0X0X0X0X0X0X0X}X24X289X15X1X22798X90X22503X11139XX24X1X48X48X}X{X{X7X7X7X6X13X12X0X}X{X0X0X0X0X0X1X0X}X24X228X5X1X22443X30X22213X10879XX24X1X48X48X}X{X{X6X6X6X5X5X5X0X}X{X1X1X1X0X0X1X0X}X24X199X9X1X22105X54X21903X10623XX24X1X48X48X}X{X{X12X12X12X12X12X1X0X}X{X0X0X0X1X1X0X0X}X24X220X16X1X21339X282X21111X10113XX24X1X48X48X}X{X{X11X11X11X11X3X4X0X}X{X0X0X1X1X0X1X0X}X22X208X12X1X20967X72X20756X9830XX22X1X48X48X}X{X{X8X8X8X2X4X2X0X}X{X1X1X1X0X1X1X0X}X24X242X14X1X20204X276X19958X9308XX24X1X48X48X}X{X0X0X49X45X0X1X0X0X19762X9258XX49X0X8X8X}X{X0X0X68X88X0X1X0X0X19490X9165XX68X0X8X8X}X{X0X0X64X68X0X1X0X0X19234X9092XX64X0X8X8X}X{X0X0X1X6X5X8X19154X40X19151X9045XX1X0X6X6X}X{X0X0X24X323X9X1X19133X9X17597X8708X{X4736X}X24X0X128X128X}X{X0X0X6X19X6X1X17585X6X17549X8681X{X1593X}X6X0X12X12X}X{X0X0X19X39X6X1X17537X6X17423X8634X{X2864X}X19X0X12X12X}X{X0X0X36X89X3X1X17417X3X17327X8541XX36X1X24X24X}X{X0X0X38X84X7X1X17313X7X17226X8447X{X5816X}X38X1X16X16X}X{XniXnjXhXomXaiXqfXqdXebXqcXqbXqaXdmXbxXdlXwX}X=X}XspX=X{X{X83X32X2X1X1409X11X47X12XTOWER OF EYESX1X100X154X146X}X{X91X32X6X3X161X15X71X4XREALM OF ROBOTSX753X120X154X147X}X{X99X0X8X7X449X13X49X8XTRAP OF TRICKSX1889X140X156X148X}X{X107X6X10X9X225X15X66X8XSLIPPERY SLIDEX2673X160X155X149X}X{X83X36X12X11X1217X17X96X20XBROKEN PATHX3729X180X154X150X}X{X91X6X15X13X385X15X64X0XSWIMMERS DELIGHTX5265X200X155X151X}X{X99X232X17X16X321X17X80X28XNASTY ONEX6289X220X154X152X}X{X107X160X23X18X289X19X96X16XEDGE OF DOOMX7569X240X154X153X}X{XdbXbmXdsXdrXtdXriXhXroXbaXomXieXqqXqjX}X=X}XquX=X{X{X0X27X24X}X{XbmXdsXdrX}X=X}XotX=X{X{X38X16X112X90X}X{X32X16X112X96X}X{X16X16X112X112X}X{XdmXdlXdhXdiX}X=X}XscX=X{X{X16X0X0X}X{X16X32X24X}X{X16X0X48X}X{X16X16X0X}X{X24X40X24X}X{X16X16X48X}X{XrqXomXxX}X=X}XrpX=X{X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X6X1X23191X6X16X0X270X3.75X5X23284X10X0X}X{X12X1X23196X6X16X0X270X3.75X8X23344X19X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X16X1X21339X6X16X0X282X1.875X16X21435X31X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X7X1X20212X6X14X0X276X7.5X1X20288X7X0X}X{X8X1X20213X6X14X0X276X7.5X3X20330X10X0X}X{X8X1X20216X6X14X0X276X7.5X1X20390X8X0X}X{X7X1X20217X6X14X0X276X7.5X1X20438X7X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X72X1X0X0X0X0X0X4X0X0X0X0X}X{X72X-1X0X0X0X0X0X4X0X0X0X0X}X{X72X0X0X0X0X0X0X60X0X0X0X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{XdwXdyXdzXe_XedXiXebXdvXeaXdxXecXtX}X=X}XduX=X{X0X128X}XleX=X{X32X0x80.006X}XlkX=X{X0X0X0X3X2X1X2X69X=X1X70X=X2X133X=X1X134X=X2X197X=X1X198X=X}XcsX=X{X{X0X7X}X{X0X7X}X{X0X7X}X{X1X6X}X{X1X6X}X{X1X6X}X{X1X6X}X69X=X{X1X6X}X70X=X{X1X6X}X133X=X{X1X6X}X134X=X{X1X6X}X197X=X{X1X6X}X198X=X}XgaX=X{X{X48X0X}X{X60X0X}X{X72X0X}X{X84X0X}X}XouX=X{X-6400X-12800X}XrhX=X{X0X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0xeeee.eeeeX0xbbbb.bbbbX0x7777.7777X0xdddd.ddddX}XmqX=X{X1X25X49X73X101X}XmpX=X{X0X0X0X0X}XhtX=X{X4X3X2X1X}XhuX=X{X{X3X3X2X2X2X2X1X1X1X1X0X0X0X0X-1X-1X-1X-1X-2X-2X-2X-2X-3X-3X}X{X2X2X2X2X1X1X0X0X-1X-1X-2X-2X-2X-2X}X}XfaX=X{X{X965X2821X2821X1285X1285X1285X229X229X}X{X965X2821X2821X1285X1285X1285X229X229X}X{X3045X3045X2821X1285X1285X1285X229X3045X}X{X3045X3045X2821X1285X1285X1285X229X3045X}X}XgoX=X{X0X1X0X1X0X1X0X1X0X1X0X1X0X0X0X0X1X0X1X0X0X0X0X0X}XhrX=X{X3X3X2X2X1X1X1X1X1X1X0X0X0X0X-1X-1X-2X-2X-2X-2X-3X-3X-3X-3X-3X-3X-3X-3X-3X-3X-4X-4X}XhfX=X{X-8X7X-7X6X-6X5X-5X4X-4X3X-3X2X-2X1X-1X0X}XkgX=X{X{X2X1X0X}X33X=X{X1X1X}X34X=X{X1X0X}X35X=X{X8X1X}X36X=X{X8X2X}X37X=X{X9X1X}X38X=X{X9X2X}X39X=X}XbvX=X{X2X2X2X2X1X1X1X1X0X0X0X0X-1X-1X-1X-1X-2X-2X-2X-2X-4X}XcyX=X{X{X16X16X56X56X124X124X124X254X254X254X511X511X511X511X511X511X511X511X511X511X511X254X254X254X124X124X124X56X56X16X16X4X16X}X{X28X28X28X62X62X62X127X127X127X127X127X127X127X127X127X127X127X62X62X62X28X28X28X3X16X}X{X24X24X60X60X126X126X126X255X255X255X255X255X255X255X255X255X255X255X255X255X255X255X255X126X126X126X60X60X24X3X15X}X{XcmXcoX}X=X}XckX=X{X15X21X26X31X34X38X41X44X47X2X4X7X9X12X14X16X18X20X23X25X27X29X31X33X35X37X39X41X43X45X47X1X3X5X7X9X11X13X15X17X19X21X23X25X28X30X32X34X36X39X41X44X46X1X4X7X10X14X17X22X27X33X0X0X0X=X}XepX=X{X5X4X3X2X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X2X3X4X5X6X6X0X=X}XkkX=X{X-20696X0xaf4c.0009X-20630X-20616X-20582X-20540X-20492X-20680X-20640X-20608X-20498X0xb028.ffeeX0xb03c.0019X-20704X0X=X}XseX=X{X-20405X-20203X-20020X-19863X-19820X-19786X-19689X-19649X-19609X-19512X-19380X-19355X-19312X-20408X0X=X}XslX=X{X{X-20405X0X0X1X0X7X0X0X0X0X13344X0X}X{X-20408X0X0X2X2X7X0X0X0X0X13412X0X}X{X-20408X0X0X3X4X7X0X0X0X0X13480X0X}X{XaXdXeXiXteXmXnXpXqXsXsfxXtX}X=X}XsjX=X{X{X0XnilX96X1X}XffX=X{X224XnilX}XetX=X{X96X-4X136X16X}XbrX=X{X0X61X224XnilX}XlpX=X{X0X0X80X0X96X-8X136X-8X152X0X}XlqX=X{X0X-24X80X16X152X16X224X-20X}XipX=X{X0X44X80X54X136X55X152X56X224X34X}XbiX=X}XhjX=X{X{X0X1X128XnilX}XffX=X{X216XnilX}XetX=X{X88X16X128X-4X}XbrX=X{X0X61X216XnilX}XlpX=X{X0X0X72X0X88X-8X128X-8X144X0X}XlqX=X{X0X-20X72X16X144X16X216X-20X}XipX=X{X0X44X72X54X128X55X144X56X}XbiX=X}XhhX=X{X{X145X30X1X16X0X}X{X144X24X0.5X32X0X}X{X143X18X0.25X16X0X}X{X142X8X0X16X0X}X{X0X0X0X0X0X}X{X141X8X0X16X0X}X{X140X16X0.25X16X0X}X{X139X23X0.5X32X0X}X{X138X28X1X16X0X}X{XccXdmXj_XwXxX}X=X}XizX=X{X256X256X256X256X256X256X256X256X256X256X256X256X234X230X202X198X196X164X164X139X134X107X106X102X74X70X42X38X38X11X0X8X12X40X44X44X72X76X104X108X136X136X140X168X172X200X201X198X229X236X256X256X256X256X256X256X256X256X256X256X}XosX=X{X8X9X8X12X8X8X8X8X9X8X}Xo_X=X{X}XhkX=X{X0X0X}XmzX=X{X0X0X}Xn_X=X{X&maxX&nilX&swX&sxX&szX&t_X&taX&tbX&tcX}XrrX=X{X&maxX&hsX&srX&ssX&stX&suX&ihX&svX&syX}XjaX=X{X&mtX&naX&nkX&nrX&nuX&nvX&obX&odX&oeX&oiX&olX}XopX=X{X00020000 JMPX00015000 CBCX00007000 CSWX\n00005000 JDRX}XrbX=X{X{XC64 ORIGINALX16X125XJOHN M PHILLIPSX4X102X\n<1987 HEWSONX16X81X}X{XCPC CONVERSIONX8X125XCHRIS WOODX24X102X\n<1987 HEWSONX16X81X}X{XCPC MUSICX28X125XDAVE ROGERSX20X102X\n<1987 HEWSONX16X81X}X{XPICO>8 PORTX20X125X@CARLC27843X20X102X}X}XrkX=X£J•p§Ä§†£B§h§à§®XsdX=X\0\0			\0\0\0\0\0	\0	\0\0\0\0??		??\0\0	\0	\0\r	0%'\0\n\0	\0	\0	\0\0\0\0\0\0\0\0 ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿTÿ\0 ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰Tÿ\0\0\0\0	))))))))'''''''%%%%%%%%#\0ßõåÇÅÄÄSÄ\0ÄÄÄSÄxSmLs\0t\0enCCCsCCCCCt$CCst\0CCeCCes\0C	sC	sC	s\0C	sC	tsC	sC	s\0C	sC	t\0CsCsCCCN&\nP\0\nCsCen\rCCCsCCensCsCCsCCCsCCC	C	C	C	esCme\0CmCmeCmCC\rxSms\0nesCs\0CCCCsCCCCCCe\nCCsCCesC\ne	CCsCCseP\0\neCCCCCCCesCP\0PCs	CsCCsCsCse\0CsCsCsCCC	sC	sC	s!C	e\0s\rCmCmeCmeCmCC\rxSN\nP-\nNP-NP-N&\nP-\nNP-N\nP+\nNP+NP+NP+N&P*NP*NP(NP(N\nP-\nNP-NP-N&\nP(\nNP-N\nP/\nNP/NP/NP/N&P2NP2N&P4NP4RP@NP>P@P>P@\nP>P@\nP>P@P>PCPBPC\nRP@NP>P@P>P@\nP;P>\nP;P>P\0RNP2NP-NP*NP&N&P1NP-NP*NP%NP*NP-NP1NP-N&P*NP-NP1NP-RPJPIPEPDPBPD\nPEPJPIPL\nPJPI\nPERP4\nP4P4P4\nP4P6\nP6P6P6P7P7P9P9RNP#NP*NP/NP*N&P#NP*NP-NP*NP#NP*NP+NP*N&P-NP+NP*NP&RNP2P1NP/P1N&P\0P2NP2P1NP/P1N&P\0P2NP5P4NP2P4N&P\0P5eN(P9(NPP8PNPP6PNPP3PNPP9PNPP8PNPP6PNPP3PRPJPIPEPDPBPD\nPEPEPRP4\nP1P-\nP*P-P*P1P/P1P4P6P4P1P-RP\0PRXrtX=X\0à$08nﬁE◊DNQpJO¢å…ùì:Ì¥Âœl=4&1gp:6\0.¨»√Ù2ªû…p÷tÀdd÷ƒˆ\\,Û¬^WoT\\¡rÒz:ﬂ'7U¬ú¢ßÖ])ù-ó7¶9)b:ßúÓ¨qÇÑË{®≠HAôÒâìñŸäoàa©w∞`Zó=$v¡\\ã@û´:ÒVö4¸zâámSw¥◊ÔæP8µ†]Ü™‘DåuïkØìAßMJ≈Sqíg«4.≈'*π†ﬁ≤ü∞—^‡/«lÇ¡y¸’‰à–∞è-l?!áòiïõÍ‹ÂTÍbUs¯]NÜ¡’.êQJ‘<:˜&`ÀÎÒ¨m2‰àóÊ‘Ë€OÎ˚Ô∞„ù§Õ`úó[‘é™ø‡É_gµŸ∫œûA¡Ob4_˛‘Äa+™Œ-~Óåd∑O	sÛÓÜÎb]„	∞/ÛıHæ'áp@õIZè–ﬁua¸Øﬂ<≠©∑»‡T˝∑s:g4€ÂÑG ≤W—goîÕc‹ú{y√Ã∑)tƒÑZ1˘}?G˛◊‹o‰uÆ§¯˘#ıÔt≈a¿ãÈöüdﬂ¢9¥\nA≠ËY,o\\™3dtq≥5Ÿ∏`≤Ò™ã\nÁ7=QÎ1Æ—û&˝gxƒ¥ªõªx`{u3pyØ‚ëﬁòtY∫slÁ∫S Êh.»‹ÚÜq£¿òttìtŸ¬°ﬁÊÌÒtaSúp¬@ü»•¸(\\uø≤¯e<ÈÔä¯Àø™G\"Ï˘Ö≥qüR†Béx†ef¥â&˜ÏöÓSFÓE}u5¢^•ãø;Ul£G@[yØ∫;w‰\0I\nÁ∏•ˆÕÉóSiXruX=X… ÀÃÕŒœ–—“”‘\n		\n‘”“—’\r…÷’\r◊ÿŸ⁄€ ! !‘”“—‘\n	…ÍÎÍ\"#\"’ÏÌÓ\r$%&ÔÒÚÛÙ'()*+,\0stuvwxyz{|}||Ä~Ä~~Ä~Ä5676XrsX=X\0sß\nTæo‘˘∆µÔv0Z¿°éW8u»‡∞û©ak˘x)»éyﬂÀÚõ∫ÙÇ·›≤7ˆÿ(Éõêßòì(æ\r ††]-	N’¢ıËy•®◊F-æπ›HwXrvX="function is(p)
if(not iu or pp!=0)return
while pq+1&3!=p do poke4(13344+pq%4*204,unpack(pr,1+ps,52+ps))ps+=51ps%=#pr pq+=1end end function pt(pu,pv,py)local ao,ap,pw,of,px,pz={},{},peek(py-4,4)local bf=1
while(pz>=1<<bf)bf+=1
local q_=a_(py-8,bf)for i=0,pz do add(ap,$py>>py%1*8<<32-px>>>16-px)py+=px>>3end for y=pv,pv+of do for x=pu,pu+pw do sset(x,y,an(ao,ap,y>pv and sget(x,y-1)or 0,deli(q_)))end end return py+.9end