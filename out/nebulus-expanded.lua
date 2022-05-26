--nebulus
--carlc27843
-- TODO: could move this to psboot, ~8 tokens
if(stat"11"!=4)stop"\faerror:\fc pLEASE LAUNCH pico-8\nIN hi-res WITH command line:\n\fb-displays_x 2 -displays_y 2" --
function fpaq1d(cdata,bits) -- decomp
 local ddata,pcs,c,cmax,lo,hi,pctx={},{},0,1<<bits,0,0x3fff.ffff,1
 -- note: cdata points to end of data - 4. the beginning must be padded with 0's' to avoid being influenced by adjacent data.
 repeat
  if(c>=cmax)add(ddata,c-cmax)c=0
  local pc=pcs[pctx] or 0x1.0001 -- hi 16bits=1's count, lo 16bits=0's count
  local mid=lo+(hi-lo)*(pc/(pc+(pc<<16)))
  local y=($cdata>>>2<=mid) and 1 or 0
  if(y!=0)hi=mid else lo=mid+0x0.0001
  while (lo^^hi)&0x3fc0==0 do
   lo=lo<<8&0x3fff.ff
   hi=hi<<8&0x3fff.ff|0x0.00ff
   cdata-=1
  end
  pcs[pctx]=pc+(0x.0001<<y*16)
  pctx+=pctx+y -- roll bit into predictor context
  if(pctx>=2*cmax)pctx=1
  c+=c+y
 until c==0
 return ddata
end

function concat(a,b) return a..b end -- note: only used if picoscript code uses ".." operator

-- set global "progress" to a harmless function so it can be safely called immediately; 
-- it will be replaced with the picoscript "progess" function when it's decoded
-- FIXME: could we use the same name as an existing PICO-8 function that we don't care
-- if we redefine it?
progress=max 

local function vlval(l,val) -- shift elements of l forward and move val to the front, for prediction in px9 and psboot
 local v,i=l[1],1
 while v!=val do
  i+=1
  v,l[i]=l[i],v
 end
 l[1]=val
end

function unpr(pr,el,k,v_) -- unpredict original value of predicted v_, for prediction in px9 and psboot
 local l=pr[k]
 if(not l)l={unpack(el)}pr[k]=l
 local v=l[1+v_]
 vlval(l,v)
 vlval(el,v)
 return v
end

function mkop(nfs,nf,op,a,b,c,d)
 local kb=op&16==0 and b -- note: kb never nil, as picoscript changes nil constant to r[] lookup
 local kc=op&32==0 and c -- note: kc never nil, as picoscript changes nil constant to r[] lookup
 local af=a!=0
 return ({
  function(r) return unpack(r,a,b) end, -- opcode.ret
  function(r) return (af==r[0] and nfs[b] or nf)(r) end, -- opcode.jump_if_x
  function(r) r[a]=kb or r[b] return nf(r) end, -- opcode.valop_b
  function(r) r[a]=not (kb or r[b]) return nf(r) end, -- opcode.valop_not_b
  function(r) r[a]=(kb or r[b])==(kc or r[c]) return nf(r) end, -- opcode.valop_b_eq_c
  function(r) r[a]=(kb or r[b])+(kc or r[c]) return nf(r) end, -- opcode.numop_b_add_c
  function(r) r[a]=(kb or r[b])-(kc or r[c]) return nf(r) end, -- opcode.numop_b_sub_c
  function(r) r[a]=(kb or r[b])*(kc or r[c]) return nf(r) end, -- opcode.numop_b_mul_c
  function(r) r[a]=(kb or r[b])/(kc or r[c]) return nf(r) end, -- opcode.numop_b_div_c
  function(r) r[a]=(kb or r[b])\(kc or r[c]) return nf(r) end, -- opcode.numop_b_idiv_c
  function(r) r[a]=(kb or r[b])%(kc or r[c]) return nf(r) end, -- opcode.numop_b_mod_c
  function(r) return (af==((kb or r[b])<(kc or r[c])) and nfs[d] or nf)(r) end, -- opcode.jump_b_lt_c
  function(r) return (af==((kb or r[b])<=(kc or r[c])) and nfs[d] or nf)(r) end, -- opcode.jump_b_le_c
  function(r) local x,y,z=r[a](unpack(r,a+1,b))
   for j=c,d do r[j],x,y=x,y,z end return nf(r) end, -- opcode.call
  function(r) r[a]=d(kb or r[b],kc or r[c]) return nf(r) end, -- opcode.calld2
  function(r) d(r[a],kb or r[b],kc or r[c]) return nf(r) end -- opcode.calld3
 })[1+op%16]
end

function psfun(code,ncode,name)
 --name=name or "psboot"
 --printh("psfun def="..name.." #code="..#code)
 local nfs,nf={}
 for i=ncode or #code,1,-5 do
  nf=mkop(nfs,nf,unpack(code,i-4,i))
  add(nfs,nf) -- note: pscompile reverses nfs indexes
 end
 return function(...)
  --dname=name -- FIXME: DEBUG
  return nf{...}
 end
end
psfun(fpaq1d(837,8))(_ENV,rawget,split"SYMBOL(progress),SYMBOL(_ENV),SYMBOL(rawget),SYMBOL(rawset),SYMBOL(unpack),SYMBOL(pack),SYMBOL(psfun),SYMBOL(shl),SYMBOL(fpaq1d),SYMBOL(unpr),SYMBOL(memset),SYMBOL(rawlen),SYMBOL(band),SYMBOL(bor),SYMBOL(sgn),SYMBOL(shr),SYMBOL(peek4),SYMBOL(lshr),SYMBOL(concat),SYMBOL(peek),SYMBOL(bxor),SYMBOL(peek2),0.5,0.25,0x8.e666,0x8060.004,0x.6666,0x.199a,0x.0001,0x.000a,0x.e666,0x400a.0988,0x400d.0c0b,0x4010.0f0e,0x4013.9211,0x40c0.2f2e,0x40b4.3332,0x40c0.3a39,0x.03,SYMBOL(ipairs),SYMBOL(scels),SYMBOL(pcelput),SYMBOL(pcels),SYMBOL(pceli),SYMBOL(peek),SYMBOL(poke),SYMBOL(stat),SYMBOL(rom),SYMBOL(pxcd),SYMBOL(ofs),SYMBOL(progress),SYMBOL(sw),SYMBOL(sh),SYMBOL(ram),SYMBOL(h),SYMBOL(w),SYMBOL(id),SYMBOL(a),SYMBOL(pixmap),SYMBOL(st),SYMBOL(memcpy),SYMBOL(sx),SYMBOL(sy),SYMBOL(min),SYMBOL(pram),SYMBOL(pokelrpal),SYMBOL(ps),SYMBOL(pals),SYMBOL(pn),SYMBOL(pc),SYMBOL(colorramps),SYMBOL(_map_display),SYMBOL(towox),SYMBOL(towix),SYMBOL(cos),{,SYMBOL(towxis),SYMBOL(waveintlc),SYMBOL(wavex),SYMBOL(sin),SYMBOL(gwdx),SYMBOL(add),SYMBOL(tbl),SYMBOL(sspr),SYMBOL(poke4),SYMBOL(camera),SYMBOL(gwavedraw),},SYMBOL(gwavecopy),SYMBOL(tlev),SYMBOL(tile),SYMBOL(tilepix),SYMBOL(tileh),SYMBOL(tilepalstride),SYMBOL(towbp),SYMBOL(towbl),SYMBOL(plst),SYMBOL(plsc),SYMBOL(plwc),SYMBOL(pla),SYMBOL(plx),SYMBOL(ply),SYMBOL(camy),SYMBOL(plmaxy),SYMBOL(plt),SYMBOL(pscex),SYMBOL(psctq),SYMBOL(pldir),SYMBOL(plpceli),SYMBOL(mpoke),SYMBOL(tcells),SYMBOL(unpack),SYMBOL(elevs),SYMBOL(aimo),SYMBOL(ais),SYMBOL(aibt),SYMBOL(aibd),SYMBOL(wavebakh),SYMBOL(_nil),SYMBOL(wavedy),SYMBOL(aipal),SYMBOL(pcelputpixmap),SYMBOL(sbpceli),SYMBOL(undrscroll),SYMBOL(skyinit),SYMBOL(fish),SYMBOL(stunx),SYMBOL(sby),SYMBOL(pylonx),SYMBOL(cacheuitowers),SYMBOL(sky),SYMBOL(undrs),SYMBOL(glti),SYMBOL(gwavesx),SYMBOL(max),SYMBOL(maxlevel),SYMBOL(curlevel),SYMBOL(plivs),SYMBOL(pscor),SYMBOL(wavep),SYMBOL(skyh),SYMBOL(peek2),SYMBOL(cls),SYMBOL(peek4),SYMBOL(spcr),SYMBOL(spcl),SYMBOL(levels),SYMBOL(mapcellh),SYMBOL(aipceli),SYMBOL(tileinitlevel),SYMBOL(srand),SYMBOL(stars),SYMBOL(rnd),SYMBOL(x),SYMBOL(y),SYMBOL(c),SYMBOL(levelreset),SYMBOL(bubblesi),SYMBOL(bubbles),=,SYMBOL(deli),SYMBOL(da),SYMBOL(t),SYMBOL(glsc),SYMBOL(sfx),SYMBOL(glstate),SYMBOL(mid),SYMBOL(plvx),SYMBOL(plvy),SYMBOL(addbubbles),SYMBOL(stuny),SYMBOL(glb),SYMBOL(glst),SYMBOL(gameinit),SYMBOL(btnp),SYMBOL(glwait),SYMBOL(type),SYMBOL(plscore),SYMBOL(chkhall),,table,SYMBOL(rect),SYMBOL(hall),SYMBOL(halli),SYMBOL(hallc),SYMBOL(chr),SYMBOL(levelinit),SYMBOL(nsndsong),SYMBOL(glueinit),SYMBOL(waverams),SYMBOL(clip),SYMBOL(glh),SYMBOL(textphs),SYMBOL(sgn),SYMBOL(textofs),SYMBOL(textamp),SYMBOL(qpcel),SYMBOL(easetext),SYMBOL(qtext),HIGH SCORERS,SYMBOL(ttxt),SYMBOL(gs),SYMBOL(gx),SYMBOL(gy),é to play,ó music on/off,ãë start level ,/,entering the,SYMBOL(name),SYMBOL(midx),tower completed!,BONUS POINTS,SYMBOL(qbonus),TIME LEFT 10?,TECHNIQUE 10?,	EXTRAS    10?,TIME UP,SYMBOL(diy),SYMBOL(rpylons),SYMBOL(dq),SYMBOL(len),SYMBOL(stun),SYMBOL(qsub),CATCH SOME FISH,FOR BONUSES,CONGRATULATIONS,YOU HAVE,COMPLETED,\nYOUR MISSION,GAME OVER,please enter,your initials,SYMBOL(drfuns),SYMBOL(memset),SYMBOL(glev),SYMBOL(fpaq1d),SYMBOL(ranimdata),SYMBOL(snddata),SYMBOL(sfxdata),SYMBOL(wpals),SYMBOL(colorrampinit),SYMBOL(pixmapinit),SYMBOL(towerinit),SYMBOL(waveinit),SYMBOL(bblinit),SYMBOL(nsndinit),SYMBOL(sfxinit),SYMBOL(cellpcels),SYMBOL(cellplats),SYMBOL(pylonpcels),SYMBOL(pylondcells),SYMBOL(cartdata),carlc27843_nebulus,SYMBOL(rectfill),SYMBOL(pal),SYMBOL(d),SYMBOL(v),SYMBOL(sndsfxs),SYMBOL(sndrt),SYMBOL(q),SYMBOL(flip),SYMBOL(m),SYMBOL(vb),SYMBOL(ve),SYMBOL(tt),SYMBOL(s),SYMBOL(nsndnote),SYMBOL(e),SYMBOL(sndchns),SYMBOL(ra),SYMBOL(sndsubs),SYMBOL(poke2),SYMBOL(n),SYMBOL(nsndstep),SYMBOL(yield),SYMBOL(sndpkts),SYMBOL(pairs),SYMBOL(costatus),SYMBOL(nsndrecordco),dead,SYMBOL(nsndrecord),SYMBOL(coresume),SYMBOL(pt),SYMBOL(ord),SYMBOL(sub),SYMBOL(all),SYMBOL(split),SYMBOL(glstn),SYMBOL(bblf),SYMBOL(pixmaps),SYMBOL(cocreate),SYMBOL(nsndrecordaux),SYMBOL(sndmus),SYMBOL(sndpkt),SYMBOL(sndpat),SYMBOL(nsndupdate),SYMBOL(nsong),SYMBOL(music),SYMBOL(nsndmusic),SYMBOL(val),SYMBOL(gwavesdraw),SYMBOL(updlevelfishp),SYMBOL(updtitle),SYMBOL(updlevelinit),SYMBOL(updlevelexit),SYMBOL(updlevelfail),SYMBOL(updgameover),SYMBOL(updhallfame),SYMBOL(gtrans),SYMBOL(wavebackup),SYMBOL(waverestore),SYMBOL(drtitle),SYMBOL(drlevelinit),SYMBOL(drlevelexit),SYMBOL(drlevelfail),SYMBOL(drlevelfish),SYMBOL(drgameover),SYMBOL(drhallfame),SYMBOL(qglue),SYMBOL(_init),0x1c14.0004,0x1868.0006,0x1604.0006,0x1069.0008,0xa4d.0008")






--sndpat=0 -- set in nsndinit()
---local sndrt
--towbp={} -- towbp[xi] = addr of blend palette for column xi
--towix={} -- towix[a] = screen x of inner radius at angle a
--towox={} -- towox[a] = screen x of outer radius at angle a
--tilepix,tileh,tilepalstride,tilespritesheet -- tileset globals
--local tlev -- global
--local mapcellh -- global
--local aipceli -- global offset for this level's enemy's pceli
--local cellpcels -- global
--local skyh -- global
-- local spcl,spcr -- global space lo/hi dwords
---local skytop
--local aipal -- next game sprite palette

---local diy,dix -- display offset relative to topleft
--dclips,dcams={0,0},{0,0}  -- clip and cam for lo/hi halves of each display
-- bblminy,255 is extent of transition bubbles this frame

--pcels={}
---
--wavex={}
--waveintlc=wave interlace about to be rendered: 0 for even world scanlines, 1 for odd world scanlines
--wavedy=last frame's wave draw y, used for copying scanlines during vertical movement
--wavep=wave phase
--wavebakh=height of waves backed up during bubble transition bblf>30

-- glue state
-- glst = current glue state
-- glsc = glue state counter
-- glstn = next glue state
-- glb = glue buttons pressed
--glst global, used by picoscript functions
--glstn global
--glsc global, used by picoscript functions
--glb global, used by picoscript functions



--local curlevel
-- wboty=worldy at bottom of game view
-- wtopy=worldy at top of game view where viewy=statuspixh (below status)
-- wvy=worldy at top of screen where viewy=0 (behind status)
--local wboty,wtopy,wvy -- global for drawwaves!

-- dq=draw queue pointer
-- dq2=draw queue 2 pointer
-- sq=string queue
---local dq,dq2,sq -- draw queue pointer

-- pla=player angle; tower rotation 0-352 (22 units per cell)
-- plx=player x, 11 units per cell
-- ply=player y in world above sea level, 8 units per cell
-- plj=player cell column
-- plst=player state
-- plsc=player state counter
-- plt=player timer countdown, starts at 120
-- plvx=-1 if left pressed, +1 if right pressed, else 0
-- plvy=-1, 0 or 1
-- plfire=fire button pressed
-- plpceli=player pcel index (sprite) or nil if invisible
-- pljvx=player jump x velocity
-- pljc=player jumping (1) or climbing (2) if state=jumping
-- plwc=walk animation counter, for animation continuity with jump/walk
-- plfs=player fall speed
-- plfc=player fall count frames (gravity after 2 frames)
-- pldr=player door type when walking through door
-- pledy=player elevator dy
-- plivs=player lives remaining
-- pscor=player score
-- pscex=player extra score
-- psctq=player technique score
-- camy=camera y, interpolates towards ply

--global ply,plmaxy,plpceli,plsc,plivs
---local pla,plx,plj,pldir,plvx,plvy,plfire,pljvx,pljdy,plfs,plfc,pldr,pledy
-- submarine
-- sbx=submarine x
-- sby=submarine y
-- sbpceli=submarine pcel index
-- globals set by ganim: sbx,sby,sbpceli

-- plfdy=player fractional unapplied accumulating dy
-- pldt=player delayed tumble flag
-- pltt=player tumble time
---local plfdy,pldt,pltt
-- plshot=table describing pogo's shotball {t,a,y,va,pceli}
-- elevs=table of elevators parked that will soon drop or are currently dropping
---local plshot,elevs
-- colbl=bitmask of cdjs indexes for columns behind and to left of tower
-- colbr=bitmask of cdjs indexes for columns behind and to right of tower
-- colf=bitmask of cdjs indexes for columns in front of tower
-- colsprs=table of sprites in each column
---local colbl,colbr,colf,colsprs

-- ais=array of enemies
-- aimo=enemy spawn map offset
-- aibt=enemy bonker timer (-1 if active)
-- aibd=enemy bonker direction
---local ais,aimo,aibt,aibd

function ganim(kf,t)
 --  animate globals with lerp
 for k,vs in pairs(kf) do
  local tb,vb
  for i=1,#vs,2 do
   local ta,va=tb,vb
   tb,vb=vs[i],tonum(vs[i+1])
   if t==tb then
    _ENV[k]=vb 
    if(k=='SYMBOL(sfxid)')sfx(sfxid) 
    break
   end
   if t<tb then
    if(va and vb)_ENV[k]=va+(t-ta)*(vb-va)\(tb-ta)
    break
   end
  end
 end
end

function getaipal(p)
 if not p then
  repeat
   p=aipal 
   aipal=aipal%8+1
  until 1<<p&tlev.aiskippal==0
 end
 return p
end

function aispawn()
 -- no spawn during drown or demolish, or if hit enemy limit
 if(plst>=9 or #ais>=4)return
 -- only spawn where view moved up tower
 local vwcy=(ply+73)\8
 local aicy=aimo\16
 if aicy<mapcellh and aicy<vwcy then
  repeat -- scan for spawn tile until end of row
   local t=aispawnt[@(-14900+aimo)]
   if t then
    local nst,v,p=unpack(t)
    local a=(aimo%16*16+8)%256
    local y=aicy*8
    add(ais,{a=a,y=y,v=v,c=0,cr=0,st=6,nst=nst,p=getaipal(p)})
    aimo+=1
    return
   end
   aimo+=1
  until aimo&15==0
 else
  -- handle bonker timed spawn
  if(aibt<0)return
  aibt-=1
  if(aibt>=0)return
  aibd*=-1
  sfx"32"
  add(ais,{x=aibd>0 and -9 or 132,y=ply,v=aibd,c=0,st=7,p=getaipal()|256})
 end
end

function aikill()
 -- kill enemies
 for i=#ais,1,-1 do
  local e=ais[i]
  if not e.a then
   deli(ais,i)
  else
   aistate(e,4) 
  end
 end
end

function aistate(e,st)
 e.st=st
 e.c=0
 if(st==4)e.pceli=126
end

function aimcollides(e)
 return mcollides(e.a\2,e.y,-2,2,15)
end

function ainotbonker(e)
 return e.a and e.st!=4 and e.st!=6
end

function aiecollides(ce,csz)
 local mask=collmask[csz or 1]
 for e in all(ais) do
  -- todo: simplify state/type checking
  if e!=ce then
   if ainotbonker(e) then
    local dx=(ce.a\2-e.a\2+64)%128-64+mask.xadj
    local dy=e.y-ce.y+mask.yadj
    if((mask[dy] or 0)>>dx&1>0)return e
   elseif not e.a and csz==3 then
    local dx=64-e.x
    local dy=ply-e.y
    if(dx>-10 and dx<10 and dy>=-16 and dy<14)return e
   end
  end
 end
end

function aichkplat(e)
 -- 0 = on platform
 -- 1 = not near platform
 -- 2 = just off platform, do reverse
 local cx=e.a\16%16
 local ma=-14900+(e.y\8-1)*16
 local c=cellplats[@(ma+cx)]
 if(c==1)return 2
 if(c)return 0
 local dx=e.a\2%8
 if(dx>1 and dx<6)return 1
 dx=(dx>1 and 1 or -1)
 return cellplats[@(ma+(cx+dx&15))] and (sgn(e.v)!=dx and 2 or 0) or 1
end

function aihorz(e)
 local oa=e.a
 e.a=(e.a+e.v)%256
 if(aimcollides(e) or aiecollides(e))e.v*=-1 e.a=oa
end

function aibounce(e)
 local dy=aibouncedys[1+e.c]
 local ody=dy
 while true do
  e.y+=dy
  if(e.y<0)e.y=0 aistate(e,4)return 0
  if(not aimcollides(e) and not aiecollides(e))return
  e.y-=dy
  dy-=sgn(dy)
  if(dy==0)return ody<0
 end
end

local aifuns={
 function(e) -- bounce 1
  aihorz(e)
  local r=aibounce(e)
  if(r==0)return
  e.c=min(e.c+1,20)
  if r then -- couldn't move down
   if e.v==0 and plst==1 and ply==e.y then -- maybe attack
    local da=(pla-e.a+128)%256-128
    if(abs(da)<64)e.v=sgn(da)
   end
   sfx"33"
   e.c=0
  end
  e.pceli=124
 end,
 function(e) -- roll 2
  aihorz(e)
  e.pceli=@(-12884+e.c%16)
  e.c+=1
  local r=aichkplat(e)
  if(r==0)return
  if(r==1)aistate(e,3)e.c=20 else e.v*=-1
 end,
 function(e) -- rollfall 3
  e.pceli=@(-12884+e.c%16)
  e.c+=1
  local r=aibounce(e)
  if(r==0)return
  if(r)aistate(e,2) -- can't move down, roll
 end,
 function(e) -- warpout 4
  e.c+=1
  e.pceli=@(-12840+e.c)
  if(e.c>23)return 1
 end,
 function(e) -- rollstun 5
  e.c-=1
  e.pceli=123
  if(e.c<0)aistate(e,2)
 end,
 function(e) -- warpin 6
  e.c+=1
  e.pceli=@(-12864+e.c)
  if e.c>23 then
   e.c=0
   if(not aiecollides(e))aistate(e,e.nst)e.c=e.cr
  end
 end,
 function(e) -- bonker 7
  if(e.c>202)aibt=600 return 1
  e.x+=(e.c<128 and e.c&1 or 1)*e.v
  e.c+=1
  e.pceli=@(-12916+e.c%32)
 end,
 function(e) -- vertical 8
  local oy=e.y
  e.y+=e.v
  if(e.y<0)e.y=0 aistate(e,4)return
  if(aimcollides(e) or aiecollides(e))e.v*=-1 e.y=oy
  e.c+=1
  e.pceli=@(-12948+e.c%32)+aipceli
 end,
 function(e) -- horizontal 9
  aihorz(e)
  e.c+=1
  e.pceli=@(-12948+e.c%32)+aipceli
 end
}
function aiupdate()
 local aiboty=ply-96
 for i=#ais,1,-1 do
  local e=ais[i]
  if ainotbonker(e) and (e.y<aiboty or aimcollides(e)) then
   aistate(e,4)
   addcolspr(e)
  elseif aifuns[e.st](e) then
   deli(ais,i)
  elseif e.a then
   addcolspr(e)
  end
 end
end

function pcelput(pcel,sx,sy)
 pcel.sx=sx
 pcel.sy=sy
 local sa=pcel.a
 local sc=pcel.sw\2
 local da=sy*64+sx\2
 for y=1,pcel.sh do
  poke(da,peek(sa,sc))
  sa+=sc
  da+=64
 end
end













function cycleupdate(lev)
 -- update palette cycles
 for cyclei=lev.cyclelo,lev.cyclehi do
  local cycle=cycles[cyclei]
  cycle.t-=1
  if cycle.t<=0 then
   cycle.t+=cycle.rt
   cycle.i=(cycle.i+cycle.di)%cycle.cyclen
   local srca=cycle.srca+cycle.i
   local dsta=cycle.dsta
   for b=1,cycle.dstcount do
    poke(dsta,peek(srca,cycle.seglen))
    poke(dsta+cycle.ps,peek(srca+cycle.ps,cycle.seglen))
    srca+=cycle.srcstride
    dsta+=cycle.dststride
   end
  end
 end
end

function towercache(a)
 
 camera()clip() -- fixme: pokes?
 -- cache one row of the tower
 -- tileh rasters of rotated/repeated/shaded tile cached in screen format lo/hi
 local tsz=tileh*64
 poke(24405,0) -- draw to spritesheet
 -- draw lo/hi palette tower
 local ta=a*1.5\1
 local xofs,palofs=0,0
 for lohi=1,2 do
  local lastbp=0
  for xi in all(towxis) do
   local bp=towbp[xi]
   local sx=(towtx[xi]+ta)%48
   if(bp!=lastbp)poke4(24320,peek4(bp+palofs,4))lastbp=bp
   sspr(sx,0,1,tileh,xi+xofs,128-tileh,1,tileh)
  end
  palofs,xofs=tilepalstride,64
 end
 poke(24405,0x60) -- draw to screen
 
end

function gwavecopy()
 -- copy glue waves from lhs of screen to rhs of screen
 for a=28992,29440,64 do
  memcpy(a+16,a,16)
  memcpy(a+48,a+32,16)
 end
end











function plstate(st)
 plst=st+0 -- note: convert st to number; we pass strings for reduced tokens
 plsc=0
 if(plst>2)plwc=0 -- walk/jump preserve plwc
 plfdy=0
end

function pljump(jc)
 plstate"2" --
 pljvx=plvx
 pljdy=pljdys[jc]
end

function plfall(fc)
 plstate"3" --
 plfs=1
 pljvx=0
 plfc=-1
 if fc==1 then
  plpceli=@(-13236+pldir*4)
 elseif fc==2 then
  pljvx=(pldir==0 and -1 or 1)
  plpceli=@(-13212+pldir*4)
 elseif fc==3 then
  pljvx=(pldir==0 and 1 or -1)
  plpceli=@(-13204+pldir*4)
 elseif fc==5 then
  plsc=1
 else --if fc==4 then
  plfs=3
  plpceli=@(-13236+pldir*4)
 end
end

function plelev(edy,ema)
 pledy=edy
 for i,e in ipairs(elevs) do
  if(e.ma==ema)deli(elevs,i)break
 end
 plstate"7" --
end

function pldrown()
 plstate"9" --
 ply=0
 aikill()
 sfx"38"
end

function updateelevs()
 for i=#elevs,1,-1 do
  local e=elevs[i]
  if e.dt>0 then
   e.dt-=1
   if(e.dt==0)sfx"43"
  else
   poke(e.ma,e.t&192)
   e.ma-=16
   e.t=@e.ma
   local bot=e.t&192==128
   poke(e.ma,bot and 134 or 198)
   if(bot)deli(elevs,i)
  end
 end
end

function mcollides(x,y,ldx,rdx,tdy)
 local lx=x+ldx
 local rx=x+rdx
 local cty=(y+tdy)\8
 local cby=y\8
 for cxx=lx\8,rx\8 do
  local tx=cxx*8
  local cx=cxx&15
  local ma=-14900+cty*16+cx
  for cy=cty,cby,-1 do
   local t=@ma
   local tlr=cellsizes[t]
   if tlr then
    if(lx<tx+tlr[2] and rx>tx+tlr[1]) then
     return t,ma
    end
   end
   ma-=16
  end
 end
 return false,nil
end

function plmovex(da)
 local na=pla+da
 local nx=na\2
 if(mcollides(nx,ply,-4,3,17))return 1
 pla=na
 plx=nx
end

function plcenterx()
 local ta=pla%16
 if(ta==8)return
 plmovex(ta<8 and 1 or -1)
 return 1
end

function plmovey(dy)
 local ny=max(0,ply+dy)
 if(mcollides(plx,ny,-4,3,17))return 1
 ply=ny
end

function plchkfall()
 local cy=ply\8-1
 local lcx=(plx-6)\8&15
 local rcx=(lcx+1)&15
 local ma=-14900+cy*16
 local tl=cellplats[@(ma+lcx)]
 local tr=cellplats[@(ma+rcx)]
 local lr=(tl and 2 or 0)+(tr and 1 or 0)
 local t=tl or tr or 0
 return plfallcodes[1+t][1+plx%8]>>(lr*4+pldir*2)&3
end

function plchkdoor()
 local cx=plx\8&15
 local cy=ply\8
 local t=@(-14900+cy*16+cx)
 return (t==7 or t==8) and t
end

function plma()
 local cx=plx\8&15
 local cy=ply\8-1
 return -14900+cy*16+cx
end

function plchkelev()
 local ma=plma()
 local t=@ma
 if(t==70)return ma,-1
 if(t==134)return ma,1
 if(t==198)return ma,0
 return ma,nil
end

function plbreak()
 local ma=plma()
 if(@ma==2)poke(ma,0)sfx"40" --
end

function plslide()
 if(plvx==0 and @plma()==3)plmovex(1)
end

function plchkcollide()
 -- no collisions while drowning or arriving/departing
 if(plst>=9)return
 -- drown if hit water
 if(ply<=0)pldrown()return
 -- no collisions while tumbling
 if(plst==8)return
 -- no collisions during door while at least half-way in
 if(plst==5 and (plsc>=28 or plsc<116))return
 if pldt then
  -- delay tumble on elevator until reaches cell boundary
  if(plst==7 and ply&7!=0)return
 else
  -- enemy collisions
  local e=aiecollides({a=pla,y=ply},3)
  if e then
   pltt=e.a and 40 or 30
  else
   -- only check tile collisions when on elevator (all other movement prevents overlap)
   if(plst!=7 or not mcollides(plx,ply,-4,3,17))return
   pltt=30
  end
  -- begin delay tumble if on elevator and not on cell boundary
  if(plst==7 and ply&7!=0)pldt=1 return
 end
 if(plst==7)plpark(plchkelev())
 pldt=nil
 sfx"37"
 plstate"8" --
 psctq=max(psctq-1)
end

function plpark(ema)
 local t=@ema&192
 poke(ema,t|6)
 if(t!=128)add(elevs,{dt=600,ma=ema,t=t})
end

local plfuns={
 function() -- walk
  plbreak()
  local fc=plchkfall()
  if(fc!=0)return plfall(fc)
  plslide()
  if plvx==0 then -- idle
   if(plwc>0)sfx"34" --
   plwc=0
   if plfire then
    if(not plshot)sfx"39"plstate"6" --
   else
    -- set idle pose
    plpceli=@(-13244+pldir*4)
    if plvy!=0 then -- up/down pressed
     -- elevator?
     local ema,edy=plchkelev()
     if(edy==0 or edy==plvy)return plelev(plvy,ema)
     if plvy>0 then
      -- door?
      pldr=plchkdoor()
      if(pldr)sfx"35"return plstate"5" --
     end
    end
   end
  elseif (plvx>0)!=(pldir>0) then
   plstate"4" --
  else
   if(plfire)plwc=0 sfx"36"return pljump(1) -- reset walk cycle when pogo jumps
   if(plmovex(plvx))return pljump(2) -- continue walk cycle when pogo climbs
   plwc+=1
   plpceli=@(-13364+pldir*32+plwc%32)
   if(plwc&15==0)sfx"34" --
  end
 end, 
 function() -- jump
  plwc+=1
  plpceli=@(-13364+pldir*32+plwc%32)
  plmovex(pljvx)
  local ody=pljdy[1+plsc]
  local dy=ody
  while plmovey(dy) do
   dy+=(dy<0 and 1 or -1)
   if dy==0 then
    if(ody<0)sfx"34" return plstate"1" -- landed
    break
   end
  end
  plsc+=1
  if 1+plsc>#pljdy then
   plfall(plmovey(-1) and 5 or 4)
  end
 end, 
 function() -- fall
  if plsc==0 then -- falling
   plfc+=1
   if(plmovex(pljvx))pljvx=0
   while plmovey(-plfs) do
    plfs-=1
    if plfs==0 then -- collided
     if(pljvx==0 or plfc>=4 and (plfc&1>0))plsc+=1 -- inital slip before land 4 angles = 2 collision pixels
     break
    end
   end
  end
  if plsc==0 then -- still falling
   plfs=mid(1,4,plfs+plfc%2)
  else -- has landed
   if(plsc==1)sfx"34"
   plvjx=0
   plpceli=@(-13229+pldir*8+plsc)
   plsc+=1
   if(plsc==9)plstate"1" --
  end
 end,
 function() -- turn
  plpceli=@(-13300+pldir*28+plsc)
  plsc+=1
  if plsc==28 then
   pldir^^=1
   plstate"1" --
  end
 end,
 function() -- door
  if plsc<16 then
   plpceli=@(-13196+pldir*16+plsc)
   plcenterx()
  elseif plsc<40 then
   plpceli=@(-13180+plsc)
  elseif plsc<104 then
   plpceli=nil
   if(pldr==8)sfx"49"glstate(5)elevs={}return plstate"10" --
   pla+=(pldir>0 and 2 or -2)
   plx=pla\2
  elseif plsc<128 then
   plpceli=@(-13244+plsc)
  elseif plsc<129 then
   -- todo: handle tower being destroyed
   if(not plmovey(-4))return -- keep falling until land
   if(plvx!=0)pldir=(plvx>0 and 1 or 0)
   sfx"34" --
  else -- if plsc<145 then
   plpceli=@(-13221+pldir*16+plsc)
  end
  plsc+=1
  if(plsc>144)return plstate"1" --
  if(plsc&15==0)sfx"48" --
 end,
 function() -- shoot
  plslide()
  plpceli=@(-13060+pldir*16+plsc)
  local da=pldir*2-1
  if(plsc==4)plshot={t=1,a=pla+da*2,y=ply,va=da*2,pceli=65}
  plsc+=1
  if(plsc==16)plstate"1" --
 end,
 function() -- ride elevator
  if plsc==0 then
   if(plcenterx())return -- first, center pogo on elevator
   plsc+=1
   -- replace elevator tile with pillar or empty
   local ema=plchkelev()
   poke(ema,@ema&192|(pledy>0 and 5 or 0))
  end
  -- going up/down
  ply+=pledy
  if(ply&7!=0)return
  local ema=plchkelev()
  local t=@ema&192
  if pledy<0 then
   sfx"42"
   if(t!=128 and (t!=192 or plvy<0))poke(ema,t)return
  else
   sfx"41"
   if(t!=64 and (t!=192 or plvy>0))poke(ema,t|5)return
  end
  -- ride finished
  plpark(ema)
  plstate"1" --
 end,
 function() -- tumble
  if plsc<pltt then -- tumble arc up/down
   plpceli=@(-13028+pldir*16+plsc%16)
   plfdy=plfdy%1+pltumbledys[min(1+plsc,32)]
   ply+=plfdy\1
   if(ply<0)pldrown()return
   plsc+=1
  else -- enter fall state once no collisions with tiles
   plpceli=@(-13236+pldir*4)
   ply-=4
   if(ply<0)pldrown()return
   if(mcollides(plx,ply,-4,3,17))return
   plfall(1)
  end
 end,
 function() -- drown
  if plsc<48 then
   plpceli=@(-12996+pldir*24+plsc\2)
  else
   plpceli=nil
   if(plsc==112)plivs-=1 glstate(plivs>0 and 4 or 8)
  end
  plsc+=1
 end,
 function() -- demolish
  if plsc<1 then
   ais={}
   return -- wait for updlevelexit to increment plsc
  elseif plsc<31 then
   -- plsc incremented below
  elseif plsc<43 then
   plpceli=@(-13147+plsc)
   if(plsc==42)sfx"45" --
  elseif plsc<44 then
   -- note: don't update plpceli, just hold last frame
   if mapcellh>6 then -- remove row of tower
    ply-=8
    local da=-14900+(mapcellh-6)*16
    poke(da,peek(da+16,5*16))
    mapcellh-=1
    return
   end
   -- todo: wait for camera to catch up to pogo
   pldir=1
  elseif plsc<56 then
   plpceli=@(-13148+plsc)
  elseif plsc<72 then
   plpceli=@(-13148+pldir*16+plsc)
  else -- if plsc<73 then
   ganim(subdepart,plsc-72)
   if not plsc then
    plsc=0
    curlevel+=1
    glstate(curlevel<=8 and 7 or 8)
    return
   end
  end
  plsc+=1
 end,
 function() -- arrive in submarine
  ganim(subarrive,plsc)
  if(plsc==nil)--[[cheatexit()]]return plstate"1" --
  plsc+=1
 end,
}

function addcolspr(s)
 add(colsprs[1+(s.a\16)],s)
end

function plscore(a,ex)
 local oscor=pscor
 pscor+=a>>16
 if(pscor/5000!=oscor/5000)plivs=min(plivs+1,8) -- extra life every 5000
 pscex=min(pscex+(ex>>16),0x.01f4)
end

function updateshot()
 local c=plshot.t-24
 if c>0 then -- fading out, no collision
  if(c>=8)plshot=nil return
  plshot.pceli=65+c\2
 else -- active, check collisions
  plshot.a+=plshot.va
  plshot.a%=256
  plshot.y+=plshotdys[plshot.t]
  local t,ma=mcollides(plshot.a\2,plshot.y,-1,0,7)
  if t then
   if(t==4)poke(ma,0)plscore(50,2)sfx"40" --
   plshot.t=24
  else
   local e=aiecollides(plshot)
   if e then
    if e.st==2 then
     aistate(e,5)
     e.c=180
    elseif e.st!=5 and e.st!=8 and e.st!=9 then
     sfx"40"
     aistate(e,4)
     plscore(100,5)
    end
    plshot.t=24
   end
  end
 end
 plshot.t+=1
 addcolspr(plshot)
end






function plupdate()
 
 local da=pla

 --colsprs={}for i=1,16 do add(colsprs,{})end
 for i=1,16 do colsprs[i]={} end
 updateelevs()
 if(plshot)updateshot()


  plfuns[plst]() -- update pogo player
  plchkcollide()


 da=pla-da
 pla%=256
 for i=1,4 do wbandx[i]-=da/wbandvs[i] end
 
 local dy=max(ply,plst>9 and 16)-camy
 camy+=ceil(abs(dy)/8)*sgn(dy)

 wboty=camy-100 -- worldy at bottom of view
 wtopy=wboty+199 -- worldy at top of view where viewy=statpixh

 wvy=wtopy+32 -- worldy at top of screen

 aispawn()
 aiupdate()

 if(bblf==0)wavep+=.25 -- don't animate water during bubbles as we draw cached water

 skytop=min(skyh-(100-min(-wboty,wtopy+1))\4,skyh-1)

 local tiledy=ply\8-plmaxy\8
 if tiledy>0 then
  plmaxy=ply
  plscore(10*tiledy,0)
 end

 if plst<9 then
  if plt==0 then -- time up
   if(glstn==2)glstate(6)
   return
  end
  plt-=1
 end

 
end





function updlevelfish() -- levelfish
 -- animate propellor
 pcelput(pcels[130+glsc%16\2],10,24)
 -- note: fish has same number of colors as enemies so can use getaipal() to cycle between them
 if(updlevelfishp() and #fish<6 and rnd()<.05)add(fish,{x=132,y=64+rnd"88",vy=rnd"2"-1,c=getaipal(),a=0})
 for i=#fish,1,-1 do
  local f=fish[i]
  f.x-=1
  f.y+=f.vy -- move fish
  if(f.y<56 or f.y>160)f.vy*=-1 -- bounce off top/bottom of ocean
  f.a+=.25 f.a%=8 -- animate fish spinning
  if f.x<-4 then
   deli(fish,i)
  elseif f.stun then
   -- stunned: check collision with sub
   local dx=18-abs(f.x-plx) -- horz distance from bow or stern when >0
   local dy=f.y-sby+20 -- vert distance from bow or stern
   if(dx>0 and dy>-dx and dy<2*dx)deli(fish,i)plscore(50,0)sfx"53"
  elseif stunx and abs(f.x-stunx)<8 and abs(f.y-stuny+0)<16 then
   -- not stunned and collided with torpedo bubble
   f.stun,f.vy,stunx=1,0,nil
   sfx"40"
  end
 end
end






function _update60()
 
 

 
 nsndupdate(stat(54))
 

 if(btnp"5")nsndmusic(not sndmus)

 -- plbuttons: available to all modes
 plvx=tonum(btn"1")-tonum(btn"0")
 plvy=tonum(btn"2")-tonum(btn"3")
 plfire=btn"4"
 
 glb=glst==glstn and btnp"4"
 glsc+=1
 if(glst!=glstn and bblf>30)gtrans()  -- transition

 wvy=255 -- default has bottom of glue screen at worldy=0
 
 if(undrscroll)for u in all(undrs) do u.x=(u.x+u.vx)%u.w end -- scroll underwater scene

 updfuns[glst]()

 -- update bubbles (after updfun so that new bubbles set p=pceli)
 for b in all(bubbles) do
  if b.a then
   local t=t()+b.da
   b.x+=sin(t)*b.da
   b.y+=b.a+abs(cos(t))
   b.p=@(-12816+b.t%16)
   b.t-=1
   if(b.t==0 or b.y>160)b.a=nil
  end
 end

 
 
end

function qtimer(t)
 for i,d in ipairs{t\1000%10,t\100%10,t\10%10,10,t%10} do
  poke4(dq,8,d,29+10*i-(d==1 and 2 or 0),8)dq+=16
 end
end

function qpcel(pceli,a,y,palflags,qi)
 poke4(qi and dq2 or dq,7,pceli,a,y,palflags)
 if qi then dq2+=20 else dq+=20 end
end

function qundr(wy,wminy,wmaxy)
 -- iterate each undr strip, q for each visible one
 for i,u in ipairs(undrs) do
  wy+=u.sh
  local ty=min(wmaxy,wy)
  local by=max(wminy,wy-u.sh+1)
  local dy=wvy-wy
  if ty>=by and dy<bblminy then
   -- unclipped sspr camera-relative coord for top of strip
   -- index of strip
   -- screen address of top y
   -- height-1 to blit
   local sa=24576+(wvy-ty-diy)*64
   if u.pceli==0 then -- horizon pure ocean
    poke4(dq,5,sa,ty-by+1,0x1111.1111,0)dq+=20
   else -- underwater section
    poke4(dq,9,dy,i,sa,ty-by)dq+=20
   end
  end
  --break
 end
end

function qtext(s,x,y,c,font)
 local dy=wvy-y-7 -- world y to screen y
 if(dy+7+textamp<diy or dy-textamp>diy+127 or dy>=bblminy)return
 add(sq,s)
 poke4(dq,6,#sq,x+textofs,dy,font and c or textphs|textamp,font and 1 or 0)dq+=24
end
function qbonus(s,n,y)
 qtext(s..(n\100%10)..(n\10%10)..(n%10),0,y)
end

function qplats(cminy,cmaxy,cols,pl)
 -- enqueue doors on dq2 and invoke it first to draw doors before plats
 poke4(dq,11,dq2-dq)dq+=8
 local dqret=dq
 local minma=-14900+cminy*16
 local minvy=wvy-cminy*8-7
 for dji=1,16 do
  cols>><=1
  if cols&1>0 then
   local cx=(plj+cdjs[dji])&15
   local ma=minma+cx
   local vx=towox[(cx*16+8-pla)%256]
   local vy=minvy
   local dqcells
   for cy=cminy,cmaxy do
    local t=@ma
    if t!=0 then
     if t==7 or t==8 then
      local lx=towix[(cx*16-pla)%256]
      local rx=towix[(cx*16+15-pla)%256]
      if rx>=lx then
       -- doors inserted into dq2
       poke4(dq2,2,lx,rx,vy,towbl[lx-32],towbl[rx-32])dq2+=24
      end
     else
      -- plats inserted into dq1
      local pcel=cellpcels[t]
      if pcel then
       if(not dqcells)dqcells=dq+16 poke4(dq,1,dqcells,vx-pcel.sw\2,0)dq=dqcells
       poke4(dq,pcel.id,vy)dq+=8
      end
     end
    end
    ma+=16
    vy-=8
   end
   if(dqcells)poke4(dqcells-4,dq)
   for s in all(colsprs[1+cx]) do qpcel(s.pceli,s.a,s.y,s.p)end
  end
 end
 -- pogo queued on dq2 if entering door, else dq1
 if(pl and plpceli)qpcel(plpceli,pla,ply,0,plst==5)
 -- jump from end of dq2 back to dq1
 poke4(dq2,11,dqret-dq2)dq2+=8
end

function qtower(diy,tofsy,wtminy,wtmaxy)
 if(wtmaxy<wtminy)return
 local da=24576+(wvy-wtmaxy-diy)*64
 local ta=((tofsy-wtmaxy)%tileh)*64
 poke4(dq,4,da,ta,wtmaxy,wtminy)dq+=20
end

function qstars(wminy,wmaxy,minx,maxx)
 for scy=(wminy-4)\16,wmaxy\16 do
  local star=stars[1+scy]
  if star then
   local x=(pla*2+star.x)&511
   if(x>=minx and x<maxx)qpcel(71,x,star.y,star.c^^glsc&1)
  end
 end
end

function qgame(wminy,wmaxy,wtminy,wtmaxy)
 
 local cminy=wtminy\8 -- bottom-most cell just within view, clipped to tower
 local cmaxy=wtmaxy\8 -- top-most cell just within view, clipped to tower

 local clipgamet=wvy-wmaxy-diy -- inclusive min y, as used by clip
 local clipgameb=wvy+1-wtminy-diy -- exclusive max y, as used by clip
 clipgameb=mid(clipgamet,bblminy-diy,clipgameb) -- clip under bubbles
 local clipairy=clipgamet>>8|clipgameb<<8 -- clip top and bottom above ocean, as used by clip

 -- cached tower including space left/right of tower
 -- note: top 6 rows need consistent texmapping during demolish
 local both=(mapcellh-6)*8
 qtower(diy,both-(tlev.h-6)*8,max(wtminy,both),wtmaxy)
 qtower(diy,0,wtminy,min(wtmaxy,both-1))

 -- sky texture left/right of tower
 local wskymaxy=min(skytop,wmaxy)
 if wskymaxy>=wtminy then
  local so=skytop-wskymaxy
  local da=24576+(wvy-wskymaxy-diy)*64
  local sa=-15444+so*8
  poke4(dq,3,sa,da,wtminy,wskymaxy,mapcellh*8)dq+=24
 end

 local cam=camgametb[dtb] -- camera for top/bottom displays

 -- above tower
 local wspaceminy=max(max(skytop,wtmaxy)+1,wminy) -- world min y of space
 if wmaxy>=wspaceminy then
  -- space above tower
  poke4(dq,5,24576+clipgamet*64,wmaxy+1-wspaceminy,spcl,spcr)dq+=20
  -- clip/camera for stars above tower
  local clipspaceb=mid(clipgamet,clipgameb,wvy+1-wspaceminy-diy) -- exclusive min y for clipping, under bubbles
  poke4(dq,10,128|clipgamet>>8|clipspaceb<<8,cam)dq+=12
  -- stars in space above tower
  qstars(wspaceminy,wmaxy,-2,130)
 end

 -- stars and plats behind/left tower
 poke4(dq,10,cliptowerlr[1]|clipairy,cam)dq+=12
 qstars(wtminy,wtmaxy,-2,34)
 qplats(cminy,cmaxy,colbl)
 -- stars and plats behind/right tower
 poke4(dq,10,cliptowerlr[2]|clipairy,cam)dq+=12
 qstars(wtminy,wtmaxy,94,130)
 qplats(cminy,cmaxy,colbr)

 -- clip/camera for objects in front of tower
 poke4(dq,10,128|clipairy,cam)dq+=12
 -- plats in front of tower + pogo
 qplats(cminy,cmaxy,colf,1)
 -- elevator under pogo
 if plst==7 and plsc>0 then
  qpcel(58,pla,ply-8)
 end
 -- bonker in front of pogo
 for e in all(ais) do
  if(not e.a)qpcel(e.pceli,e.x,e.y,e.p)
 end
 -- submarine in front of pogo
 qsub(64)

 
end

function qsub(x)
 if sbpceli then
  qpcel(sbpceli,x+sbx,sby,256)
  for i=1,3 do
   qpcel(sbpceli+i,x-20+i*10,sby-20,256)
  end
 end
end

function reflcopy(sminy,smaxy,ystep)
 -- need compressed size temporarily
 -- assume ystep is negative (copy bottom up)
 
 local waddr,sastep=4992,ystep*64
 for tb=1,0,-1 do
  local diy=tb*128
  local dminy=max(0,sminy-diy)
  local dmaxy=min(127,smaxy-diy)
  if dminy<=dmaxy then
   local steps=(dminy-dmaxy+ystep)\ystep -- note: overall positive (dmaxy+1-dminy+ystep-1 negated as ystep<0)
   for lr=0,1 do
    _map_display(tb*2+lr)
    local wa=waddr+lr*32
    local sa=24576+dmaxy*64
    for i=1,steps do
     poke4(wa,peek4(sa,8))
     wa+=64
     sa+=sastep
    end
   end
   waddr+=steps*64
   smaxy+=steps*ystep -- note: overall subtract (not -= since ystep<0)
  end
 end
 

end

function refldup(cdy,csy)
 -- need compressed size temporarily
 -- duplicate wave scanlines
 --  from csy down (interlaced scanlines we did draw last frame)
 --  to cdy down (interlaced scanlines we won't be drawing this frame)
 
 local cda=24576+cdy*64
 local csa=24576+csy*64
 for di=2,3 do
  _map_display(di)
  if csy>cdy then -- copy up, so iterate down
   local dofs=cda-csa
   for sa=csa,31168,128 do
    poke4(sa+dofs,peek4(sa,16))
   end
  elseif csy<cdy then -- copy down, so iterate up
   local sofs=csa-cda
   for da=31104+cdy%2*64,cda,-128 do
    poke4(da,peek4(da+sofs,16))
   end
  end
 end
 

end

function reflblend(dminy,dmaxy,wimin,ystep) -- interlaced sspr version
 
 poke4(24360,0)
 poke(24371,2) -- bit0=transparency, bit1=sprite fillp enable, bit2=apply sprite fillp to other draw apis
 -- identity draw palette as lookup into secondary palette for fill pattern colors
 poke4(24320,0x0302.0100,0x0706.0504,0x0b0a.0908,0x0f0e.0d0c)
 local sy,dstep=78,ystep*64 -- src spritesheet wave y
 --local wavex,wavep,wfills=wavex,wavep,wfills -- cache globals?
 for lr=0,1 do
  _map_display(2+lr)
  local sx=lr*64 -- src spritesheet wave x
  local clp,cam=-32704,0
  for lohi=0,1 do
   poke4(24352,clp,0,cam)
   local wi,wbi,wbni,fx,dofs,sy=wimin,1,wbands[1],wbandx[1]&3,dminy*64+lohi*32,sy
   for dy=dminy,dmaxy,ystep do
    while 1+wi>=wbni do -- TODO: subtract 1 in wbands?
     poke4(24416,peek4(-19068+wbi*32+lohi*16,4))
     fx=wbandx[wbi]&3
     wbi+=1
     wbni=wbands[wbi]-- or 101 (sentinel inserted at build time in buildwaves)
    end
    poke2(24369,wfills[1+wi]>><fx)
    local wx=sx-wavex[wavep+wi&31]
    sspr(wx,sy,64,1,0,dy)
    -- duplicate 4 pixels on left/right where wave gaps show; wrong but less obviously wrong
    if wx<0 then
     poke2(24576+dofs,peek2(24578+dofs))
    elseif wx>64 then
     poke2(24606+dofs,peek2(24604+dofs))
    end
    dofs+=dstep
    wi+=ystep
    sy+=1
   end
   clp,cam=0x8080.004,0x.ffc
  end
 end
 poke2(24369,0) -- disable fillp pattern for subsequent draw api calls
 poke(24371,0) -- bit0=transparency, bit1=sprite fillp enable, bit2=apply sprite fillp to other draw apis
 memcpy(24416,-18908,16) -- restore secondary palette for hi color screen palette
 
end



function dcell(dqb,x,dqe)
 
 local pcel=pcels[$dqb] -- use first pcel for clip width and pal of whole column
 local pals=pcel.pixmap.pals[1]
 if x<dix+64 and x+pcel.sw>dix then -- horizontal cull whole column
  for lohi=1,2 do
   poke4(24352,dclips[lohi],0,dcams[lohi])
   poke4(24320,peek4(pals[lohi],4)) -- assume all cells share same palette
   for dq=dqb,dqe-1,8 do
    local pceli,y=peek4(dq,2)
    local pcel=pcels[pceli]
    sspr(pcel.sx,pcel.sy,pcel.sw,pcel.sh,x,y)
   end
  end
 end
 
 return 16+dqe-dqb
end

function ddoor(x1,x2,y1,lc,rc)
 
 local y2=y1+7
 local pal=tilepix.doorlpal
 for lohi=1,2 do
  poke4(24352,dclips[lohi],0,dcams[lohi])
  poke(24320,pal[lc],pal[rc],pal[7])
  if(x2>x1)rectfill(x1+1,y1,x2-1,y2,2)
  line(x1,y1,x1,y2,0)
  line(x2,y1,x2,y2,1)
  pal=tilepix.doorrpal
 end
 
 return 24
end

function dsky(sa,da,miny,maxy,towpixh)
 
 local aofs=16 -- when dix=0, "above" tower is da+16, and draw sky left of tower
 if(dix!=0)da,aofs=da+16,-16 -- when dix==64, "above" tower is da, and draw sky right of tower
 for y=maxy,miny,-1 do
  local sl,sr=peek4(sa,2)sa+=8
  -- left/right of tower
  poke4(da,sl,sl,sl,sl)poke4(da+32,sr,sr,sr,sr)
  -- above tower
  if(y>=towpixh)poke4(da+aofs,sl,sl,sl,sl)poke4(da+aofs+32,sr,sr,sr,sr)
  da+=64
 end
 
 return 24
end

function dtower(da,ta,wtmaxy,wtminy)
 
 local tsz=tileh*64
 local sa=8192-tsz
 if(dix==0)da+=16 else sa+=16
 local ds=da^^16
 local l,r,poke4=spcl,spcr,poke4
 for ry=wtminy,wtmaxy do
  poke4(ds,l,l,l,l)poke4(ds+32,r,r,r,r)
  poke4(da,peek4(sa+ta,4))poke4(da+32,peek4(sa+ta+32,4))
  da+=64 ds+=64
  ta=(ta+64)%tsz
 end
 
 return 20
end

function dclear(da,h,l,r)
 
 for y=1,h do -- clear lo and hi colors
  poke4(da,
   l,l,l,l,l,l,l,l,
   r,r,r,r,r,r,r,r)
  da+=64
 end
 
 return 20
end

function dtext(si,dx,dy,c,font)
 
 local s=sq[si]
 if font==1 then
  for lohi=1,2 do
   poke4(24352,dclips[lohi],0,dcams[lohi])
   poke(24321,c&15)
   print(s,dx,dy,1)
   c\=16
  end
 else
  local sp,sa=c%1,c\1 -- phase, amplitude
  local p=pcels[158]
  -- assume cycle/ramp at start of string (TODO: make parameters instead?)
  local ci,ri=ord(s,1,2) -- 1-based cycle index, 1-based ramp index
  local r=colorramps[ri] -- ramp
  local rlen=r<<5&31 -- ramplen
  r+=cycles[23+ci].i%rlen
  for lohi=1,2 do
   poke4(24352,dclips[lohi],0,dcams[lohi])
   poke4(24320,peek4(r,4))
   poke(24320,16) -- transparent background
   local x=dx
   for i=3,#s do
    local ci=ord(s,i)i+=1
    ci=max(ci-47)
    local sx,sy=p.sx+ci%16*8,p.sy+ci\16*8
    sspr(sx,sy,8,8,x,dy+sa*sin(sp+x/48))
    x+=8
   end -- for i
   r+=rlen+7
  end -- for lohi
 end
 
 return 24
end

function dpcel(pceli,wa,wy,palflags)
 
 local dx=palflags>=256 and wa or towox[(wa-pla)%256]
 local pcel=pcels[pceli%200]
 local sw,pals=pcel.sw,pcel.pixmap.pals[1+palflags&255]
 dx-=sw\2
 if dx<dix+64 and dx+sw>dix then -- horizontal cull
  local sh,flipx=pcel.sh,pceli>=200
  local dy=wvy-wy-sh+1
  for lohi=1,2 do
   poke4(24352,dclips[lohi],0,dcams[lohi])
   poke4(24320,peek4(pals[lohi],4))
   sspr(pcel.sx,pcel.sy,sw,sh,dx,dy,sw,sh,flipx)
  end
 end
 
 return 20
end

function ddigit(i,dx,dy)
 
 local pcel=pcels[82]
 if dx<dix+64 and dx+pcel.sw>dix then -- horizontal cull
  local pals=pcel.pixmap.pals[1+i]
  for lohi=1,2 do
   poke4(24352,dclips[lohi],0,dcams[lohi])
   poke4(24320,peek4(pals[lohi],3))
   sspr(pcel.sx,pcel.sy,pcel.sw,pcel.sh,dx,dy)
  end
 end
 
 return 16
end

function dundr(y,i,sa,h_1)
 
 local u=undrs[i]
 local p,ox,ws=pcels[u.pceli],u.x\1,u.w==16
 local pals=p.pixmap.pals[1]
 for lohi=1,2 do
  poke4(24352,dclips[lohi],0,dcams[lohi])
  poke4(24320,peek4(pals[lohi],4))
  -- draw dword-aligned sprite on lhs of screen
  sspr(p.sx+ox,p.sy,p.sw-ox,p.sh,dix,y)
  sspr(p.sx,p.sy,ox,p.sh,dix+p.sw-ox,y)
  -- repeat-copy dword-aligned pixels to fill screen horizontally
  local a=sa
  for _=0,h_1 do
   if(ws)poke4(a+8,peek4(a,2))
   poke4(a+16,peek4(a,4))
   a+=64
  end
  sa+=32
 end
 
 return 20
end

function dclipcam(clp,cam)
 
 -- clp encodes (clpx>>16 | clpy>>8 | (clpx+clpw) | (clpy+clph)<<8)
 -- cam encodes (camx=0>>16 | camy) -- note: camx always 0
 -- compute camera lo/hi
 dcams[1]=dix>>16|cam -- dcamlo
 dcams[2]=dix-64>>>16|cam -- dcamhi
 -- compute clip lo/hi
 local clpminx=clp<<16&255
 local clpmaxx=clp&255
 clpminx=mid(0,clpminx-dix,64)
 clpmaxx=mid(0,clpmaxx-dix,64)
 dclips[1]=clp&0xff00.ff|clpminx>>16|clpmaxx -- dcliplo
 dclips[2]=dclips[1]+0x40.004 -- dcliphi
 
 return 12
end

function djmp(ofs)
 return ofs
end

--local dfuns={dcell,ddoor,dsky,dtower,dclear,dtext,dpcel,ddigit,dundr,dclipcam,djmp}
function drawq(dq,dqend)
 
 while dq!=dqend do
  dq+=dfuns[$dq](peek4(dq+4,5))
 end
 
end







function dstatus()
 
 poke4(dq,5,24576,32,0,0)dq+=20
 poke4(dq,10,-32640,0)dq+=12
 qtimer(plt\6) -- timer
 for i=0,plivs-1 do -- lives remaining hearts
  qpcel(69,100+i%4*7,wvy-7-i\4*8,256)
 end
 local s=pscor
 for i=0,7 do
  
  qpcel(72+(s%0x.000a<<16),30-i*4,wvy-8,256)
  s/=10
 end
 
end

function _draw()
 
 

 bblminy=bblyextents[1+bblf]
 
 local g=(glst==2)
 cycleupdate(g and tlev or glev)

 if g then
  -- cache tower row to spritesheet
  towercache(pla)
  -- update pogo spritesheet
  if(plpceli)pcelput(pcels[plpceli%200],40,24)
  -- copy ai frames to spritesheet. fixme: pcel.sx and sy never cleared
  for i,e in ipairs(ais) do
    if(e.pceli)pcelput(pcels[e.pceli],unpack(scelais[i]))
  end
  -- calculate tower column visibility
  colbl=0
  colbr=0
  colf=0
  plj=pla\16&15 -- player column j
  for dji,dj in ipairs(cdjs) do
    local j=(plj+dj)&15
    local a=(j*16+8-pla)%256
    if a<=64 or a>=192 then -- in front and visible
    colf|=1<<>dji
    else
    local ox=towox[a]
    if(ox<40)colbl|=1<<>dji -- behind left but visible
    if(ox>88)colbr|=1<<>dji -- behind right but visible
    end
  end
 end -- if g

 local dgw -- drawgwaves

 for tb=1,2 do
  dtb=tb
  local di=tb&2 -- display index: top=0, bottom=2
  diy=di*64 --  display y: top=0, bottom=128

  -- generate pass: status + game above water
  sq={}
  dq=-18516
  dq2=-16468
  
  if diy==0 then -- status
   if(g)dstatus()
  else -- footer, only need to draw black to clear transition bubbles
   poke4(dq,5,31232,24,0,0)dq+=20
  end

  local wmaxy=wvy-diy
  local wminy=wmaxy-127
  local wtminy
  if g then
   wminy=max(wminy,wboty) -- world inclusive min y of display
   wmaxy=min(wmaxy,wtopy) -- world inclusive max y of display
   wtminy=max(wminy,0) -- world tower inclusive min y within display, clipped to tower
   local wtmaxy=min(wmaxy,mapcellh*8-1) -- world tower inclusive max y within display, clipped to tower
   -- game above water
   qgame(wminy,wmaxy,wtminy,wtmaxy)
  elseif glst>1 then
   -- note: sbpceli is true when submarine visible, i.e. gluestate.levelfish
   -- if sbpceli, then don't draw sky, and don't draw waves
   -- clear sky above water
   if diy==0 then
    if(not sbpceli)poke4(dq,3,-15444,29504-skyh*64,1,skyh,0)dq+=24
    if(glst>=4)dstatus() else poke4(dq,5,24576,77-skyh,spcl,spcr)dq+=20
   end
   -- clip/camera for game view
   poke4(dq,10,mid(0,bblminy-diy,128)<<8|128,camgametb[tb])dq+=12
   qundr(wvy-232,wminy,wmaxy)
   -- call glstate specific draw
   qglue()
   if(not sbpceli and 69<bblminy)dgw=gwavesdraw
  else
   -- draw black screen on boot, so transition bubbles don't appear elongated
   -- note: fast enough to do whole screen, don't need to draw to mid(0,bblminy-diy,128); saves 9 tokens
   poke4(dq,5,24576,128,spcl,spcr)dq+=20
  end
  
  for b in all(bubbles) do -- just queue bubbles on both top/bottom to reduce token usage
   if(b.a)qpcel(b.p,b.x,b.y,256)
  end

   --assert(dq<-16468)

  for lr=0,1 do
   _map_display(di+lr)
   dix=lr*64
   drawq(-18516,dq)
  end
 end -- top/bottom loop

 if(g and wboty<0)drawwaves()
 if(dgw)dgw()  -- gwavesdraw 8%

 -- bubble transition, and record music in background until complete, not during transition bubbles
 if(bblf>0)bbldraw()bblf=(bblf+1)%60 else nsndrecord() -- FIXME: should nsndrecord!

 -- DEBUG horz line between displays
 --_map_display(2)memset(0x6000,3,64)
 --_map_display(3)memset(0x6000,3,64)

 
 
end

function drawbblwaves(sminy,smaxy,rmaxy)
 -- transition bubbles are active
 local sbblminy=bblminy-128 -- inclusive bottom display y of bubble occlude region (unclipped, may be negative)
 if bblf==1 then
  -- game to glue, on first frame backup all visible waves and don't draw waves
  wavebackup(sminy,smaxy,0)
 elseif sbblminy>=sminy then
  if bblf>30 and wavebakh<-wboty then -- note: -wboty=wave height
   -- glue to game, draw up to cachewavehpf each frame and backup
   local drawh=min(-wboty-wavebakh,21)
   if(wavebakh>0)waverestore(sminy,(min(sbblminy,sminy+wavebakh-1))) -- restore waves already backed up
   sminy+=wavebakh -- inclusive bottom display y of top of ocean region to draw
   smaxy=sminy+drawh-1 -- inclusive bottom display y of bottom of ocean region to draw
   rmaxy-=wavebakh -- reflect this part of the tower into the draw region
   reflcopy(rmaxy-drawh+1,rmaxy,-1)
   reflblend(sminy,smaxy,wavebakh,1)
   wavebackup(sminy,smaxy,wavebakh) -- append drawn region to backup
   wavebakh+=drawh
  else
   -- game to glue (backup complete) or glue to game (backup complete): restore each frame
   waverestore(sminy,(min(sbblminy,smaxy)))
  end
 end
end


function drawwaves()
 -- copy reflection from displays
 waveintlc^^=1 -- toggle interlace
 local sminy=wboty+104 -- inclusive bottom display y of top of ocean
 local smaxy=103 -- inclusive bottom display y of bottom of ocean
 local rmaxy=sminy+127 -- inclusive screen y of bottom to be reflected (note: whole screen y, not bottom display y!)
 if bblf==0 then -- bubbles inactive
  local rminy=rmaxy-smaxy+sminy -- inclusive screen y of top to be reflected
  -- copy interlaced region to be reflected to wavespritesheet
  reflcopy(rminy,rmaxy-waveintlc,-2)
  -- if view moved vertically, duplicate last drawn interlaced scanlines
  if(wavedy and wavedy!=sminy)refldup(sminy+1-waveintlc,wavedy+1-waveintlc)
  wavedy=sminy
  -- draw interlaced reflection/waves from wavespritesheet to screen
  reflblend(sminy+waveintlc,smaxy,waveintlc,2)
 else -- bubbles active
  drawbblwaves(sminy,smaxy,rmaxy)
 end
end





 -- need tokens temporarily
function bbldraw()
 -- https://www.lexaloffle.com/bbs/?tid=3588 (by emu)
 poke2(24321,7)
 local ft=1+bblf*.05
 for di=0,3 do
  _map_display(di)
  poke4(24360,camgametb[1+di\2])
  local dy=di\2*128
  -- draw visible bubbles
  local xib=di%2*4
  for yi=0,8 do
   local w=15*sin(ft/4+yi*.03)\1
   if w>0 then
    for xi=0,4 do
     local y=yi*32+20*sin(ft+(xib+xi)*0.1)\1
     if(bblf>=30)y=257-y
     local t,b=y-2*w+1,y+2*w
     if max(t,0)<bblminy then
      local l,r=xi*16-w+1,xi*16+w
      poke4(24352,-32704)
      ovalfill(l,t,r,b,1)
      poke4(24352,0x8080.004)
      ovalfill(l+64,t,r+64,b,2)
     end
    end
   end
  end
  -- fast draw band of full bubbles
  local y=max(bblminy-dy)
  dclear(24576+y*64,128-y,0x7777.7777,0)
 end
end




function mpoke(d,s,f,b,e)
 poke(d,f(s,b or 1,e or #s))
end



val"{X{X0X0X80X1501X10X1X-20724X10X-25124X15636XX20X0X10X110X}X{X0X0X16X257X4X9X-25196X36X-25836X15342X{X0X5X10X15X20X25X30X35X}X16X0X10X80X}X{X0X0X16X70X3X1X-25842X3X-26162X15268X{X40X45X50X55X}X16X0X10X40X}X{X0X0X32X50X8X1X-26186X12X-26186X15203X{X1048X1056X1064X1072X}X8X2X16X16X}X{X0X0X20X146X13X1X-26242X28X-26242X15022X{X1536X1541X1546X1551X}X20X2X10X40X}X{X0X0X8X21X3X1X-26248X3X-26248X14997X{X1080X1082X1084X1086X}X8X2X4X16X}X{X0X0X7X19X7X1X-26262X7X-26262X14968X{X1590X}X7X2X6X6X}X{X0X0X24X9X14X1X-26290X14X-26300X14944XX24X1X96X96X}X{X0X0X5X11X4X10X-26380X40X-26380X14892X{X2106X}X5X2X4X4X}X{X0X0X8X49X7X1X-26394X7X-26394X14833X{X2086X2088X2090X2092X2094X2096X2098X2100X2102X2104X}X8X2X4X40X}X{X0X0X16X39X9X11X-26592X99X-26592X14690X{X1561X}X16X2X10X10X}X{X0X0X16X191X7X9X-26718X63X-27358X14433XX16X0X10X80X}X{X0X0X16X185X5X9X-27448X45X-28216X14201XX16X0X12X96X}X{X0X0X16X242X8X9X-28360X72X-29000X13884XX16X0X10X80X}X{X0X0X16X230X6X9X-29108X54X-29748X13597XX16X0X10X80X}X{X0X0X16X232X6X9X-29856X54X-30496X13308XX16X0X10X80X}X{X0X0X16X61X5X9X-30586X45X-30826X13200XX16X0X10X30X}X{X0X0X16X80X4X9X-30898X36X-31138X13083XX16X0X10X30X}X{X0X0X23X127X7X1X-31152X7X-32210X12946X{X6272X}X23X0X92X92X}X{X0X0X20X95X5X1X-32220X5X-32540X12844XX20X0X4X32X}X{X0X0X28X37X4X1X-32548X4X-32548X12802X{X2824X}X28X2X16X16X}X{X0X0X23X90X3X1X-32554X3X-32554X12708X{X2848X}X23X2X32X32X}X{X0X0X16X37X2X1X-32558X2X-32558X12668X{X1566X}X16X2X16X16X}X{X0X0X8X19X2X1X-32562X2X-32562X12646X{X1574X}X8X2X16X16X}X{X0X0X8X16X2X1X-32566X2X-32566X12627X{X1582X}X8X2X16X16X}X{X0X0X18X62X3X1X-32572X3X-32572X12561X{X2870X}X18X2X16X16X}X{X0X0X24X159X3X1X-32578X3X-32578X12398X{X2832X}X24X2X32X32X}X{X0X0X30X102X3X1X-32584X3X-32584X12292X{X2816X}X30X2X16X16X}X{X{X6X6X6X6X5X5X0X}X{X0X0X0X1X0X1X0X}X24X238X8X1X23968X48X23728X12004XX24X1X48X48X}X{X{X6X6X6X6X13X5X0X}X{X0X0X0X1X0X0X0X}X24X202X16X1X23188X270X22978X11524XX24X1X48X48X}X{X{X14X14X14X14X4X2X0X}X{X0X0X0X0X0X0X0X}X24X289X15X1X22798X90X22503X11139XX24X1X48X48X}X{X{X7X7X7X6X13X12X0X}X{X0X0X0X0X0X1X0X}X24X228X5X1X22443X30X22213X10879XX24X1X48X48X}X{X{X6X6X6X5X5X5X0X}X{X1X1X1X0X0X1X0X}X24X199X9X1X22105X54X21903X10623XX24X1X48X48X}X{X{X12X12X12X12X12X1X0X}X{X0X0X0X1X1X0X0X}X24X220X16X1X21339X282X21111X10113XX24X1X48X48X}X{X{X11X11X11X11X3X4X0X}X{X0X0X1X1X0X1X0X}X22X208X12X1X20967X72X20756X9830XX22X1X48X48X}X{X{X8X8X8X2X4X2X0X}X{X1X1X1X0X1X1X0X}X24X242X14X1X20204X276X19958X9308XX24X1X48X48X}X{X0X0X49X45X0X1X0X0X19762X9258XX49X0X8X8X}X{X0X0X68X88X0X1X0X0X19490X9165XX68X0X8X8X}X{X0X0X64X68X0X1X0X0X19234X9092XX64X0X8X8X}X{X0X0X1X6X5X8X19154X40X19151X9045XX1X0X6X6X}X{X0X0X24X323X9X1X19133X9X17597X8708X{X4736X}X24X0X128X128X}X{X0X0X6X19X6X1X17585X6X17549X8681X{X1593X}X6X0X12X12X}X{X0X0X19X39X6X1X17537X6X17423X8634X{X2864X}X19X0X12X12X}X{X0X0X36X89X3X1X17417X3X17327X8541XX36X1X24X24X}X{X0X0X38X84X7X1X17313X7X17226X8447X{X5816X}X38X1X16X16X}X{XSYMBOL(doorlpal)XSYMBOL(doorrpal)XSYMBOL(h)XSYMBOL(ofs)XSYMBOL(pc)XSYMBOL(pn)XSYMBOL(pram)XSYMBOL(ps)XSYMBOL(ram)XSYMBOL(rom)XSYMBOL(scels)XSYMBOL(sh)XSYMBOL(st)XSYMBOL(sw)XSYMBOL(w)X}X=X}XSYMBOL(pixmaps)X=X{X{X83X32X2X1X1409X11X47X12XTOWER OF EYESX1X100X154X146X}X{X91X32X6X3X161X15X71X4XREALM OF ROBOTSX753X120X154X147X}X{X99X0X8X7X449X13X49X8XTRAP OF TRICKSX1889X140X156X148X}X{X107X6X10X9X225X15X66X8XSLIPPERY SLIDEX2673X160X155X149X}X{X83X36X12X11X1217X17X96X20XBROKEN PATHX3729X180X154X150X}X{X91X6X15X13X385X15X64X0XSWIMMERS DELIGHTX5265X200X155X151X}X{X99X232X17X16X321X17X80X28XNASTY ONEX6289X220X154X152X}X{X107X160X23X18X289X19X96X16XEDGE OF DOOMX7569X240X154X153X}X{XSYMBOL(aipceli)XSYMBOL(aiskippal)XSYMBOL(cyclehi)XSYMBOL(cyclelo)XSYMBOL(glc)XSYMBOL(glh)XSYMBOL(h)XSYMBOL(midx)XSYMBOL(name)XSYMBOL(ofs)XSYMBOL(plt)XSYMBOL(sky)XSYMBOL(tile)X}X=X}XSYMBOL(levels)X=X{X{X0X27X24X}X{XSYMBOL(aiskippal)XSYMBOL(cyclehi)XSYMBOL(cyclelo)X}X=X}XSYMBOL(glev)X=X{X{X38X16X112X90X}X{X32X16X112X96X}X{X16X16X112X112X}X{XSYMBOL(sh)XSYMBOL(sw)XSYMBOL(sx)XSYMBOL(sy)X}X=X}XSYMBOL(pylonpcels)X=X{X{X16X0X0X}X{X16X32X24X}X{X16X0X48X}X{X16X16X0X}X{X24X40X24X}X{X16X16X48X}X{XSYMBOL(len)XSYMBOL(ofs)XSYMBOL(x)X}X=X}XSYMBOL(rpylons)X=X{X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X6X1X23191X6X16X0X270X3.75X5X23284X10X0X}X{X12X1X23196X6X16X0X270X3.75X8X23344X19X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X16X1X21339X6X16X0X282X1.875X16X21435X31X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X7X1X20212X6X14X0X276X7.5X1X20288X7X0X}X{X8X1X20213X6X14X0X276X7.5X3X20330X10X0X}X{X8X1X20216X6X14X0X276X7.5X1X20390X8X0X}X{X7X1X20217X6X14X0X276X7.5X1X20438X7X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{X4X1X-26179X1X8X0X12X3.75X1X-26178X4X0X}X{X72X1X0X0X0X0X0X4X0X0X0X0X}X{X72X-1X0X0X0X0X0X4X0X0X0X0X}X{X72X0X0X0X0X0X0X60X0X0X0X0X}X{X8X-1X-26237X1X13X0X28X2.5X8X-26229X15X0X}X{XSYMBOL(cyclen)XSYMBOL(di)XSYMBOL(dsta)XSYMBOL(dstcount)XSYMBOL(dststride)XSYMBOL(i)XSYMBOL(ps)XSYMBOL(rt)XSYMBOL(seglen)XSYMBOL(srca)XSYMBOL(srcstride)XSYMBOL(t)X}X=X}XSYMBOL(cycles)X=X{X0X128X}XSYMBOL(camgametb)X=X{X32X0x80.006X}XSYMBOL(cliptowerlr)X=X{X0X0X0X3X2X1X2X69X=X1X70X=X2X133X=X1X134X=X2X197X=X1X198X=X}XSYMBOL(cellplats)X=X{X{X0X7X}X{X0X7X}X{X0X7X}X{X1X6X}X{X1X6X}X{X1X6X}X{X1X6X}X69X=X{X1X6X}X70X=X{X1X6X}X133X=X{X1X6X}X134X=X{X1X6X}X197X=X{X1X6X}X198X=X}XSYMBOL(cellsizes)X=X{X{X48X0X}X{X60X0X}X{X72X0X}X{X84X0X}X}XSYMBOL(scelais)X=X{X-6400X-12800X}XSYMBOL(waverams)X=X{X0X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0x4444.4444X0x1111.1111X0x2222.2222X0x8888.8888X0x4444.4444X0x1111.1111X26214.4X0x8888.8888X26214.4X0x9999.9999X13107.2X0xcccc.ccccX26214.4X0x9999.9999X0x7777.7777X0xcccc.ccccX0xbbbb.bbbbX0xeeee.eeeeX0xdddd.ddddX0x7777.7777X0xbbbb.bbbbX0xeeee.eeeeX0x9999.9999X0x7777.7777X0xeeee.eeeeX0xbbbb.bbbbX0x7777.7777X0xdddd.ddddX}XSYMBOL(wfills)X=X{X1X25X49X73X101X}XSYMBOL(wbands)X=X{X0X0X0X0X}XSYMBOL(wbandx)X=X{X4X3X2X1X}XSYMBOL(wbandvs)X=X{X{X3X3X2X2X2X2X1X1X1X1X0X0X0X0X-1X-1X-1X-1X-2X-2X-2X-2X-3X-3X}X{X2X2X2X2X1X1X0X0X-1X-1X-2X-2X-2X-2X}X}XSYMBOL(pljdys)X=X{X{X965X2821X2821X1285X1285X1285X229X229X}X{X965X2821X2821X1285X1285X1285X229X229X}X{X3045X3045X2821X1285X1285X1285X229X3045X}X{X3045X3045X2821X1285X1285X1285X229X3045X}X}XSYMBOL(plfallcodes)X=X{X0X1X0X1X0X1X0X1X0X1X0X1X0X0X0X0X1X0X1X0X0X0X0X0X}XSYMBOL(plshotdys)X=X{X3X3X2X2X1X1X1X1X1X1X0X0X0X0X-1X-1X-2X-2X-2X-2X-3X-3X-3X-3X-3X-3X-3X-3X-3X-3X-4X-4X}XSYMBOL(pltumbledys)X=X{X-8X7X-7X6X-6X5X-5X4X-4X3X-3X2X-2X1X-1X0X}XSYMBOL(cdjs)X=X{X{X2X1X0X}X33X=X{X1X1X}X34X=X{X1X0X}X35X=X{X8X1X}X36X=X{X8X2X}X37X=X{X9X1X}X38X=X{X9X2X}X39X=X}XSYMBOL(aispawnt)X=X{X2X2X2X2X1X1X1X1X0X0X0X0X-1X-1X-1X-1X-2X-2X-2X-2X-4X}XSYMBOL(aibouncedys)X=X{X{X16X16X56X56X124X124X124X254X254X254X511X511X511X511X511X511X511X511X511X511X511X254X254X254X124X124X124X56X56X16X16X4X16X}X{X28X28X28X62X62X62X127X127X127X127X127X127X127X127X127X127X127X62X62X62X28X28X28X3X16X}X{X24X24X60X60X126X126X126X255X255X255X255X255X255X255X255X255X255X255X255X255X255X255X255X126X126X126X60X60X24X3X15X}X{XSYMBOL(xadj)XSYMBOL(yadj)X}X=X}XSYMBOL(collmask)X=X{X15X21X26X31X34X38X41X44X47X2X4X7X9X12X14X16X18X20X23X25X27X29X31X33X35X37X39X41X43X45X47X1X3X5X7X9X11X13X15X17X19X21X23X25X28X30X32X34X36X39X41X44X46X1X4X7X10X14X17X22X27X33X0X0X0X=X}XSYMBOL(towtx)X=X{X5X4X3X2X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X1X2X3X4X5X6X6X0X=X}XSYMBOL(towbl)X=X{X-20696X0xaf4c.0009X-20630X-20616X-20582X-20540X-20492X-20680X-20640X-20608X-20498X0xb028.ffeeX0xb03c.0019X-20704X0X=X}XSYMBOL(sndsfxs)X=X{X-20405X-20203X-20020X-19863X-19820X-19786X-19689X-19649X-19609X-19512X-19380X-19355X-19312X-20408X0X=X}XSYMBOL(sndsubs)X=X{X{X-20405X0X0X1X0X7X0X0X0X0X13344X0X}X{X-20408X0X0X2X2X7X0X0X0X0X13412X0X}X{X-20408X0X0X3X4X7X0X0X0X0X13480X0X}X{XSYMBOL(a)XSYMBOL(d)XSYMBOL(e)XSYMBOL(i)XSYMBOL(isfx)XSYMBOL(m)XSYMBOL(n)XSYMBOL(p)XSYMBOL(q)XSYMBOL(s)XSYMBOL(sfx)XSYMBOL(t)X}X=X}XSYMBOL(sndchns)X=X{X{X0XnilX96X1X}XSYMBOL(plpceli)X=X{X224XnilX}XSYMBOL(plsc)X=X{X96X-4X136X16X}XSYMBOL(ply)X=X{X0X61X224XnilX}XSYMBOL(sbpceli)X=X{X0X0X80X0X96X-8X136X-8X152X0X}XSYMBOL(sbx)X=X{X0X-24X80X16X152X16X224X-20X}XSYMBOL(sby)X=X{X0X44X80X54X136X55X152X56X224X34X}XSYMBOL(sfxid)X=X}XSYMBOL(subarrive)X=X{X{X0X1X128XnilX}XSYMBOL(plpceli)X=X{X216XnilX}XSYMBOL(plsc)X=X{X88X16X128X-4X}XSYMBOL(ply)X=X{X0X61X216XnilX}XSYMBOL(sbpceli)X=X{X0X0X72X0X88X-8X128X-8X144X0X}XSYMBOL(sbx)X=X{X0X-20X72X16X144X16X216X-20X}XSYMBOL(sby)X=X{X0X44X72X54X128X55X144X56X}XSYMBOL(sfxid)X=X}XSYMBOL(subdepart)X=X{X{X145X30X1X16X0X}X{X144X24X0.5X32X0X}X{X143X18X0.25X16X0X}X{X142X8X0X16X0X}X{X0X0X0X0X0X}X{X141X8X0X16X0X}X{X140X16X0.25X16X0X}X{X139X23X0.5X32X0X}X{X138X28X1X16X0X}X{XSYMBOL(pceli)XSYMBOL(sh)XSYMBOL(vx)XSYMBOL(w)XSYMBOL(x)X}X=X}XSYMBOL(undrs)X=X{X256X256X256X256X256X256X256X256X256X256X256X256X234X230X202X198X196X164X164X139X134X107X106X102X74X70X42X38X38X11X0X8X12X40X44X44X72X76X104X108X136X136X140X168X172X200X201X198X229X236X256X256X256X256X256X256X256X256X256X256X}XSYMBOL(bblyextents)X=X{X8X9X8X12X8X8X8X8X9X8X}XSYMBOL(colorramps)X=X{X}XSYMBOL(colsprs)X=X{X0X0X}XSYMBOL(dclips)X=X{X0X0X}XSYMBOL(dcams)X=X{X&SYMBOL(max)X&SYMBOL(nil)X&SYMBOL(drtitle)X&SYMBOL(drlevelinit)X&SYMBOL(drlevelexit)X&SYMBOL(drlevelfail)X&SYMBOL(drlevelfish)X&SYMBOL(drgameover)X&SYMBOL(drhallfame)X}XSYMBOL(drfuns)X=X{X&SYMBOL(max)X&SYMBOL(plupdate)X&SYMBOL(updtitle)X&SYMBOL(updlevelinit)X&SYMBOL(updlevelexit)X&SYMBOL(updlevelfail)X&SYMBOL(updlevelfish)X&SYMBOL(updgameover)X&SYMBOL(updhallfame)X}XSYMBOL(updfuns)X=X{X&SYMBOL(dcell)X&SYMBOL(ddoor)X&SYMBOL(dsky)X&SYMBOL(dtower)X&SYMBOL(dclear)X&SYMBOL(dtext)X&SYMBOL(dpcel)X&SYMBOL(ddigit)X&SYMBOL(dundr)X&SYMBOL(dclipcam)X&SYMBOL(djmp)X}XSYMBOL(dfuns)X=X{X00020000 JMPX00015000 CBCX00007000 CSWX\n00005000 JDRX}XSYMBOL(hall)X=X{X{XC64 ORIGINALX16X125XJOHN M PHILLIPSX4X102X\n<1987 HEWSONX16X81X}X{XCPC CONVERSIONX8X125XCHRIS WOODX24X102X\n<1987 HEWSONX16X81X}X{XCPC MUSICX28X125XDAVE ROGERSX20X102X\n<1987 HEWSONX16X81X}X{XPICO>8 PORTX20X125X@CARLC27843X20X102X}X}XSYMBOL(ttxt)X=X£J•p§Ä§†£B§h§à§®XSYMBOL(pylondcells)X=X\0\0			\0\0\0\0\0	\0	\0\0\0\0??		??\0\0	\0	\0\r	0%'\0\n\0	\0	\0	\0\0\0\0\0\0\0\0 ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿTÿ\0 ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰ÿ‰Tÿ\0\0\0\0	))))))))'''''''%%%%%%%%#\0ßõåÇÅÄÄSÄ\0ÄÄÄSÄxSmLs\0t\0enCCCsCCCCCt$CCst\0CCeCCes\0C	sC	sC	s\0C	sC	tsC	sC	s\0C	sC	t\0CsCsCCCN&\nP\0\nCsCen\rCCCsCCensCsCCsCCCsCCC	C	C	C	esCme\0CmCmeCmCC\rxSms\0nesCs\0CCCCsCCCCCCe\nCCsCCesC\ne	CCsCCseP\0\neCCCCCCCesCP\0PCs	CsCCsCsCse\0CsCsCsCCC	sC	sC	s!C	e\0s\rCmCmeCmeCmCC\rxSN\nP-\nNP-NP-N&\nP-\nNP-N\nP+\nNP+NP+NP+N&P*NP*NP(NP(N\nP-\nNP-NP-N&\nP(\nNP-N\nP/\nNP/NP/NP/N&P2NP2N&P4NP4RP@NP>P@P>P@\nP>P@\nP>P@P>PCPBPC\nRP@NP>P@P>P@\nP;P>\nP;P>P\0RNP2NP-NP*NP&N&P1NP-NP*NP%NP*NP-NP1NP-N&P*NP-NP1NP-RPJPIPEPDPBPD\nPEPJPIPL\nPJPI\nPERP4\nP4P4P4\nP4P6\nP6P6P6P7P7P9P9RNP#NP*NP/NP*N&P#NP*NP-NP*NP#NP*NP+NP*N&P-NP+NP*NP&RNP2P1NP/P1N&P\0P2NP2P1NP/P1N&P\0P2NP5P4NP2P4N&P\0P5eN(P9(NPP8PNPP6PNPP3PNPP9PNPP8PNPP6PNPP3PRPJPIPEPDPBPD\nPEPEPRP4\nP1P-\nP*P-P*P1P/P1P4P6P4P1P-RP\0PRXSYMBOL(snddata)X=X\0à$08nﬁE◊DNQpJO¢å…ùì:Ì¥Âœl=4&1gp:6\0.¨»√Ù2ªû…p÷tÀdd÷ƒˆ\\,Û¬^WoT\\¡rÒz:ﬂ'7U¬ú¢ßÖ])ù-ó7¶9)b:ßúÓ¨qÇÑË{®≠HAôÒâìñŸäoàa©w∞`Zó=$v¡\\ã@û´:ÒVö4¸zâámSw¥◊ÔæP8µ†]Ü™‘DåuïkØìAßMJ≈Sqíg«4.≈'*π†ﬁ≤ü∞—^‡/«lÇ¡y¸’‰à–∞è-l?!áòiïõÍ‹ÂTÍbUs¯]NÜ¡’.êQJ‘<:˜&`ÀÎÒ¨m2‰àóÊ‘Ë€OÎ˚Ô∞„ù§Õ`úó[‘é™ø‡É_gµŸ∫œûA¡Ob4_˛‘Äa+™Œ-~Óåd∑O	sÛÓÜÎb]„	∞/ÛıHæ'áp@õIZè–ﬁua¸Øﬂ<≠©∑»‡T˝∑s:g4€ÂÑG ≤W—goîÕc‹ú{y√Ã∑)tƒÑZ1˘}?G˛◊‹o‰uÆ§¯˘#ıÔt≈a¿ãÈöüdﬂ¢9¥\nA≠ËY,o\\™3dtq≥5Ÿ∏`≤Ò™ã\nÁ7=QÎ1Æ—û&˝gxƒ¥ªõªx`{u3pyØ‚ëﬁòtY∫slÁ∫S Êh.»‹ÚÜq£¿òttìtŸ¬°ﬁÊÌÒtaSúp¬@ü»•¸(\\uø≤¯e<ÈÔä¯Àø™G\"Ï˘Ö≥qüR†Béx†ef¥â&˜ÏöÓSFÓE}u5¢^•ãø;Ul£G@[yØ∫;w‰\0I\nÁ∏•ˆÕÉóSiXSYMBOL(sfxdata)X=X… ÀÃÕŒœ–—“”‘\n		\n‘”“—’\r…÷’\r◊ÿŸ⁄€ ! !‘”“—‘\n	…ÍÎÍ\"#\"’ÏÌÓ\r$%&ÔÒÚÛÙ'()*+,\0stuvwxyz{|}||Ä~Ä~~Ä~Ä5676XSYMBOL(ranimdata)X=X\0sß\nTæo‘˘∆µÔv0Z¿°éW8u»‡∞û©ak˘x)»éyﬂÀÚõ∫ÙÇ·›≤7ˆÿ(Éõêßòì(æ\r ††]-	N’¢ıËy•®◊F-æπ›HwXSYMBOL(wpals)X="
-- 117 tokens, but adds up to 4% cpu (high variance)


-- picoscript saves 189 tokens, but adds ~10% cpu






function nsndupdate(p)
 if(not sndmus or nsong!=0)return -- not playing main song
 -- note: due to fade out, keep streaming music even when it's off (it remains unused)
 -- p=current pattern id
 -- t=current tick within pattern (divide by musicspd to get row)
 while sndpat+1&3!=p do
  poke4(13344+sndpat%4*204,unpack(sndpkts,1+sndpkt,52+sndpkt))
  sndpkt+=51
  sndpkt%=#sndpkts
  sndpat+=1
 end
end




-- px9 https://www.lexaloffle.com/bbs/?tid=34058
function pxcd(x0,y0,src)
 -- note: src+0 points to el[] array
 -- note: src-4 points to [w-1,h-1,elbits,elcount] bytes
 -- note: src-8 points to end of fpaq1'd dat buffer
 local pr,el,w_1,h_1,eb,ec_1={},{},peek(src-4,4)

 local vb=1 while(ec_1>=1<<vb)vb+=1 -- bits per predicted value
 local dat=fpaq1d(src-8,vb) -- dat = predicted values in reversed order for deli below

 for i=0,ec_1 do -- setup el = list of elements
  add(el,$src>>src%1*8<<32-eb>>>16-eb)
  src+=eb>>3
 end

 -- unpredict values and set them with vset
 for y=y0,y0+h_1 do
  for x=x0,x0+w_1 do
   sset(x,y,unpr(pr,el,y>y0 and sget(x,y-1) or 0,deli(dat)))
  end
 end

 --return psrc+7/8,w,h_1+1 -- note: only need src+7/8, but +.9 is fewer chars (fractional bits ignored by caller)
 return src+.9
end
