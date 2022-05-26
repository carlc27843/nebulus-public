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
`if0[[]]
function trace(...)
 local s=""
 for v in all{...} do s..=((v==nil and "[nil]") or (type(v)=="table" and #v) or tostr(v)).."," end
 printh("$$$trace:"..s)
end -- fixme: DEBUG: KILLME
trace=printh
function concat(a,b) return tostr(a or "[nil]")..tostr(b or "[nil]") end
[[]]`
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
 local kb=op&`fbits.rkb`==0 and b -- note: kb never nil, as picoscript changes nil constant to r[] lookup
 local kc=op&`fbits.rkc`==0 and c -- note: kc never nil, as picoscript changes nil constant to r[] lookup
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
 })[1+op%`fbits.code.mask+1`]
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
`perfenable(false)`

`picoscript[[]]
function progress()
 if not pt then
  pt=0
  poke(`p8mem.FEATURE_CTRL`,17) -- 1=enable map_dispay, 16=enable top 32K
  poke(`p8mem.SCREEN_MODE`,1) -- horizontal stretch 64x128
  poke(`p8mem.PAL_SCREEN`,`0`,`10`,`12+0x80`) -- ui colors: black, yellow, deep blue
  --poke4(`p8mem.CLIP`,`mkclip(0,0,64,128)`) -- can skip clip since we're not in hicolor mode; the second 64 pixels per row are ignored
else
  local t=stat(1)
  if((pt+t)\4==pt\4)return -- draw every 4 frames
  pt+=t
 end
 local w=min(pt/160,1)*`128-16-16-2-2`\1
 for i=0,3 do
  _map_display(i)
  camera(i%2*64,i\2*128)
  if pt==0 then
   cls(0)
   rect(`16`,`128`,`128-16-1`,`128+16-1`,1) -- progress bar border
  else
   rectfill(`16+2`,`128+2`,`16+2`+w-1,`128+16-2-1`,2) -- progress bar
  end
 end
 --poke4(`p8mem.CLIP`,`mkclip(0,0,128,128)`,`0`,0) -- reset clip and camera
 flip()
end
[[]]`

`picoscript[[]]
function tbl(it,t)
 t.n=nil -- picoscript's {} calls pack which adds a .n, which messes up ganim for subarrive
 for v in it do
  if v=='{' then
   add(t,(tbl(it,{})))
  elseif v=='}' then
   return t
  elseif v=='=' then
   local f=deli(t)
   if type(f)=="table" then
    -- f is table of fields, applied inside every element st of t dict
    for _,st in pairs(t) do
     -- apply fields to trailing #f-most elements of st array
     for j=#f,1,-1 do
      st[f[j]]=deli(st)
     end
    end
   else
    -- f is single field key, value is last element of t array
    t[f]=deli(t)
   end
  else
   add(t,ord(v)==`ord'&'` and _ENV[sub(v,2)] or v)
  end
 end
end
function val(s)
 tbl(all((split(s,(sub(s,2))))),_ENV)
end
[[]]`


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
-- bblminy,`bblmaxy` is extent of transition bubbles this frame

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

`picoscript[[]]
function glstate(st)
 glstn=st
 bblf=1
end
[[]]`

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
   aipal=aipal%`aipalsize`+1
  until 1<<p&tlev.aiskippal==0
 end
 return p
end

function aispawn()
 -- no spawn during drown or demolish, or if hit enemy limit
 if(plst>=`pogostate.drown` or #ais>=`maxais`)return
 -- only spawn where view moved up tower
 local vwcy=(ply+`(19-10)*cellpixh+1`)\`cellpixh`
 local aicy=aimo\`mapcellw`
 if aicy<mapcellh and aicy<vwcy then
  repeat -- scan for spawn tile until end of row
   local t=aispawnt[@(`towdram`+aimo)]
   if t then
    local nst,v,p=unpack(t)
    local a=(aimo%`mapcellw`*`cella`+`cella\2`)%`maxa`
    local y=aicy*`cellpixh`
    add(ais,{a=a,y=y,v=v,c=0,cr=0,st=`aistate.warpin`,nst=nst,p=getaipal(p)})
    aimo+=1
    return
   end
   aimo+=1
  until aimo&`mapcellw-1`==0
 else
  -- handle bonker timed spawn
  if(aibt<0)return
  aibt-=1
  if(aibt>=0)return
  aibd*=-1
  sfx"`getsfxid('sfx-bonker')`"
  add(ais,{x=aibd>0 and `bonkerlx` or `bonkerrx`,y=ply,v=aibd,c=0,st=`aistate.bonker`,p=getaipal()|`qpcel.flag2d`})
 end
end

function aikill()
 -- kill enemies
 for i=#ais,1,-1 do
  local e=ais[i]
  if not e.a then
   deli(ais,i)
  else
   aistate(e,`aistate.warpout`) 
  end
 end
end

function aistate(e,st)
 e.st=st
 e.c=0
 if(st==`aistate.warpout`)e.pceli=`animdirframe('enemy_warpout',nil,nil)`
end

function aimcollides(e)
 return mcollides(e.a\`cella\cellpixw`,e.y,`aidims`)
end

function ainotbonker(e)
 return e.a and e.st!=`aistate.warpout` and e.st!=`aistate.warpin`
end

function aiecollides(ce,csz)
 local mask=collmask[csz or `colltype.enemy`]
 for e in all(ais) do
  -- todo: simplify state/type checking
  if e!=ce then
   if ainotbonker(e) then
    local dx=(ce.a\`cella\cellpixw`-e.a\`cella\cellpixw`+`mappixw\2`)%`mappixw`-`mappixw\2`+mask.xadj
    local dy=e.y-ce.y+mask.yadj
    if((mask[dy] or 0)>>dx&1>0)return e
   elseif not e.a and csz==`colltype.pogo` then
    local dx=`pogoscrx`-e.x
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
 local cx=e.a\`cella`%`mapcellw`
 local ma=`towdram`+(e.y\`cellpixh`-1)*`mapcellw`
 local c=cellplats[@(ma+cx)]
 if(c==`platcell.ELEVATOR`)return 2
 if(c)return 0
 local dx=e.a\`cella\cellpixw`%`cellpixw`
 if(dx>1 and dx<`cellpixw-2`)return 1
 dx=(dx>1 and 1 or -1)
 return cellplats[@(ma+(cx+dx&`mapcellw-1`))] and (sgn(e.v)!=dx and 2 or 0) or 1
end

function aihorz(e)
 local oa=e.a
 e.a=(e.a+e.v)%`maxa`
 if(aimcollides(e) or aiecollides(e))e.v*=-1 e.a=oa
end

function aibounce(e)
 local dy=aibouncedys[1+e.c]
 local ody=dy
 while true do
  e.y+=dy
  if(e.y<0)e.y=0 aistate(e,`aistate.warpout`)return 0
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
  e.c=min(e.c+1,`#aibouncedys-1`)
  if r then -- couldn't move down
   if e.v==0 and plst==`pogostate.walk` and ply==e.y then -- maybe attack
    local da=(pla-e.a+`maxa\2`)%`maxa`-`maxa\2`
    if(abs(da)<`maxa\4`)e.v=sgn(da)
   end
   sfx"`getsfxid('sfx-bounce')`"
   e.c=0
  end
  e.pceli=`animdirframe('enemy_ball_red',nil,nil)`
 end,
 function(e) -- roll 2
  aihorz(e)
  e.pceli=`animdirframe('enemy_ball_black',nil,'e.c',{loop=true})`
  e.c+=1
  local r=aichkplat(e)
  if(r==0)return
  if(r==1)aistate(e,`aistate.rollfall`)e.c=`#aibouncedys-1` else e.v*=-1
 end,
 function(e) -- rollfall 3
  e.pceli=`animdirframe('enemy_ball_black',nil,'e.c',{loop=true})`
  e.c+=1
  local r=aibounce(e)
  if(r==0)return
  if(r)aistate(e,`aistate.roll`) -- can't move down, roll
 end,
 function(e) -- warpout 4
  e.c+=1
  e.pceli=`animdirframe('enemy_warpout',nil,'e.c')`
  if(e.c>`animlen('enemy_warpout')-1`)return 1
 end,
 function(e) -- rollstun 5
  e.c-=1
  e.pceli=`animdirframe('enemy_ball_black',nil,nil,nil)`
  if(e.c<0)aistate(e,`aistate.roll`)
 end,
 function(e) -- warpin 6
  e.c+=1
  e.pceli=`animdirframe('enemy_warpin',nil,'e.c')`
  if e.c>`animlen('enemy_warpin')-1` then
   e.c=0
   if(not aiecollides(e))aistate(e,e.nst)e.c=e.cr
  end
 end,
 function(e) -- bonker 7
  if(e.c>`128+64+10`)aibt=`bonkertime` return 1
  e.x+=(e.c<128 and e.c&1 or 1)*e.v
  e.c+=1
  e.pceli=`animdirframe('enemy_bonker',nil,'e.c',{loop=true})`
 end,
 function(e) -- vertical 8
  local oy=e.y
  e.y+=e.v
  if(e.y<0)e.y=0 aistate(e,`aistate.warpout`)return
  if(aimcollides(e) or aiecollides(e))e.v*=-1 e.y=oy
  e.c+=1
  e.pceli=`animdirframe('enemy',nil,'e.c',{loop=true})`+aipceli
 end,
 function(e) -- horizontal 9
  aihorz(e)
  e.c+=1
  e.pceli=`animdirframe('enemy',nil,'e.c',{loop=true})`+aipceli
 end
}
function aiupdate()
 local aiboty=ply-`12*cellpixh`
 for i=#ais,1,-1 do
  local e=ais[i]
  if ainotbonker(e) and (e.y<aiboty or aimcollides(e)) then
   aistate(e,`aistate.warpout`)
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

`picoscript[[]]
function pcelputpixmap(i)
 local pixmap=pixmaps[i]
 for i,da in ipairs(pixmap.scels) do
  pcelput(pcels[pixmap.pceli+i],da%64*2,da\64)
 end
end
[[]]`

`picoscript[[]]
function pokelrpal(pal,pallen,lpal,rpal)
 for i=0,pallen-1 do
  local c=peek(pal+i)
  -- transparency in bit 4
  -- lpal index in bits 0..3 inclusive
  -- rpal index in bit 5 (bits 6..7 zeroed)
  poke(lpal+i,c%32)
  poke(rpal+i,c&16|c\32)
 end
end
[[]]`

`picoscript[[]]
function pixmapinit()
 -- one time decompress pixmaps
 pcels={}
 for pixmap in all(pixmaps) do
  local rom=pixmap.rom
  local pal=pxcd(0,0,rom+pixmap.ofs)
  progress()
  local sw=pixmap.sw
  local sh=pixmap.sh
  local da=pixmap.ram
  local scel=all(pixmap.scels)
  pixmap.pceli=#pcels -- note: one less than first pceli
  for y=0,pixmap.h-1,sh do
   for x=0,pixmap.w-1,sw do
    local pcel={id=#pcels+1,a=da,sw=sw,sh=sh,pixmap=pixmap}
    add(pcels,pcel)
    if pixmap.st==`pixstore.cram` then
     memcpy(da,rom,pal-rom)
     da+=pal-rom
    else -- if pixmap.st==`pixstore.dram` or pixmap.st==`pixstore.spritesheet` then
     local ds=sw\2
     if pixmap.st==`pixstore.spritesheet` then
      da,ds=scel(),64
      pcel.sy,pcel.sx=da\64,da%64*2
      da+=`waveramB` -- temporarily copy spritesheet sprites to waveramB/waveramA
     end
     --local sa=`p8mem.SCREEN`+y*64+x\2
     local sa=y*64+x\2
     for sy=1,sh do
      --poke(da,peek(sa,sw\2))
      memcpy(da,sa,sw\2)
      da+=ds
      sa+=64
     end
    end
   end
  end
  --memcpy(pixmap.pram,pal,pixmap.ps) -- todo: could store 32-color palette, and separate to lpal/rpal at runtime
  local lpal=pixmap.pram
  pokelrpal(pal,pixmap.ps,lpal,lpal+pixmap.ps) -- expand lpal/rpal
  pixmap.pals={}
  for i=1,pixmap.pn do
   add(pixmap.pals,{lpal,lpal+pixmap.ps})
   lpal+=pixmap.pc
  end
  progress()
 end
 memcpy(0,`waveramB`,`0x2000`) -- copy spritesheet stored pixmaps to spritesheet
end
[[]]`

`picoscript[[]]
function colorrampinit()
 -- expand color ramps to repeat initial seglen-1 for draw palette memcpys
 local rom=`colorramprom`
 local ram=`colorrampram`
 for i,len in ipairs(colorramps) do
  -- convert colorramps[i] from ramplen to ram|ramplen>>5
  colorramps[i]=len>>5|ram
  local lpal,rpal=ram,ram+len+`colorrampseglen-1`
  pokelrpal(rom,len,lpal,rpal)
  pokelrpal(rom,`colorrampseglen-1`,lpal+len,rpal+len)
  rom+=len
  ram=rpal+len+`colorrampseglen-1`
 end
end
[[]]`

`picoscript[[]]
function towerinit()
 -- setup towix[a] and towox[a]
 towix,towox={},{}
 for a=0,`maxa-1` do
  local c=cos((a-`maxa/4`+.5)/`maxa`)
  towix[a]=64+(c*`towir`)\1
  towox[a]=64+(c*`towor`+.5)\1
 end
 -- order to iterate in towercache to minimize blend palette swaps
 towxis={}
 for xi=0,`2*towir-1`\2 do
  add(towxis,xi)
  add(towxis,`2*towir-1`-xi)
 end
end
[[]]`

`picoscript[[]]
function waveinit()
 waveintlc=0
 wavex={}
 for i=0,31 do
  wavex[i]=(0.5+sin(i/32)*3)\1
 end
end
[[]]`

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
 `perfenter('towercache')`
 camera()clip() -- fixme: pokes?
 -- cache one row of the tower
 -- tileh rasters of rotated/repeated/shaded tile cached in screen format lo/hi
 local tsz=tileh*64
 poke(`p8mem.ADDR_SCREEN`,0) -- draw to spritesheet
 -- draw lo/hi palette tower
 local ta=a*`mapcellw*texw/maxa`\1
 local xofs,palofs=0,0
 for lohi=1,2 do
  local lastbp=0
  for xi in all(towxis) do
   local bp=towbp[xi]
   local sx=(towtx[xi]+ta)%`texw*2`
   if(bp!=lastbp)poke4(`p8mem.PAL_DRAW`,peek4(bp+palofs,`16\4`))lastbp=bp
   sspr(sx,0,1,tileh,xi+xofs,128-tileh,1,tileh)
  end
  palofs,xofs=tilepalstride,64
 end
 poke(`p8mem.ADDR_SCREEN`,0x60) -- draw to screen
 `perfleave(towercache)`
end

function gwavecopy()
 -- copy glue waves from lhs of screen to rhs of screen
 for a=`p8mem.SCREEN+(glueundryt-gwave.sh)*64`,`p8mem.SCREEN+(glueundryt-1)*64`,64 do
  memcpy(a+16,a,16)
  memcpy(a+`32+16`,a+32,16)
 end
end

`picoscript[[]]
function gwavedraw(ox)
 local dx=gwdx
 for i=1,`gwave.count` do
  local wh=sin(gwdx/`gwave.period`+.25*i)*`gwave.wh`\1
  if wh>0 then
   local sy=`gwave.sy`+(wh*wh-wh)\2
   sspr(`gwave.sx`,sy,`gwave.sw`,wh,dx%32+ox,`glueundryt`-wh)
   sspr(`gwave.sx`,sy,`gwave.sw`,wh,dx%32+ox-32,`glueundryt`-wh)
  end
  dx+=8
 end
end
[[]]`

`picoscript[[]]
function gwavesdraw()
 gwdx-=1 -- scroll waves left
 _map_display(0)
 poke4(`p8mem.CAM_X`,`mkcamera(0,0)`)
 -- lo palette
 poke4(`p8mem.PAL_DRAW`,$(`gwave.bpixmap.pramlo`))
 poke4(`p8mem.CLIP`,`mkclip(0,0,32,128)`) -- clip to 32 pixels wide window
 gwavedraw(0)
 -- hi palette
 poke4(`p8mem.PAL_DRAW`,$(`gwave.bpixmap.pramhi`))
 poke4(`p8mem.CLIP`,`mkclip(64,0,32,128)`)
 gwavedraw(64)
 -- copy 32 pixel waves from lhs of display 0 to rhs of display 0
 gwavecopy() -- 0.3%
 -- copy display 0 gwaves to waveramA
 -- 0.7% rest of function
 memcpy(`waveramA`,`p8mem.SCREEN+(glueundryt-gwave.sh)*64`,`gwave.sh*64`)
 -- copy gwaves from waveramA to display 1
 _map_display(1)
 memcpy(`p8mem.SCREEN+(glueundryt-gwave.sh)*64`,`waveramA`,`gwave.sh*64`) 
end
[[]]`

`picoscript[[]]
function tileinitlevel()
 local pcel=pcels[tlev.tile]
 tilepix=pcel.pixmap
 tileh=pcel.sh
 tilepalstride=tilepix.ps
 -- decompress wall pcel to 0,0 on spritesheet
 pxcd(0,0,tilepix.ram+tilepix.ofs)
 -- setup pointers to tile's palettes per blend level at xi
 local tilelpal=tilepix.pals[1][1]
 towbp={}
 for xi=0,`2*towir-1` do
  towbp[xi]=tilelpal+(towbl[xi]-1)*tilepix.pc
 end
end
[[]]`

`picoscript[[]]
--function lvlget(x,y)return @(`towdram`+y*16+x)end
--function lvlset(x,y,c)poke(`towdram`+y*16+x,c)end
function levelreset()
 plst=`pogostate.arrive`
 plsc=0
 plwc=0
 pla=`cella\2`
 plx=pla\`cella\cellpixw`
 ply=`2*cellpixh`
 camy=ply
 plmaxy=ply
 plt=tlev.plt*60
 pscex=0
 psctq=100
 pldir=1
 plpceli=`animdirframe('pogo_idler')`
 mpoke(`towdram`,tcells,unpack,tlev.ofs,tlev.ofs+tlev.h*16-1)
 elevs={}
 aimo=0
 ais={}
 aibt=`bonkertime`
 aibd=-1
 wavebakh=0
 wavedy=_nil -- prevent next reflblend from vertically scrolling display buffers
 aipal=1
end
[[]]`

`picoscript[[]]
function glueinit()
 -- called when transition from game to glue
 -- copy fontscifi to spritesheet
 flip() -- prevent screen tearing as exceed 100% cpu
 pcelputpixmap(`getbpixmap("fontscifi").id`)
 -- expand gwave to spritesheet
 pxcd(`gwave.sx`,`gwave.sy`,`gwave.bpixmap.ram+gwave.bpixmap.ofs`)
 -- restore submarine propellor in upright position
 pcelput(pcels[`getpceli('submarine-propellor')`],`scel.submarine.scels[2][1]`,`scel.submarine.scels[2][2]`)
 local undrhorizonsh=0 -- normally no gap on horizon
 sbpceli=nil -- ensure no submarine (which also controls sky/waves)
 undrscroll=1 -- scroll underwater scene
 if glstn==`gluestate.title` then
  pcelputpixmap(`getbpixmap("title").id`) -- copy "nebulus" title sprite to spritesheet
  skyinit(`getpceli("sky-sunset-blend")`) -- show sunset-blend sky on title/hiscores screens
 else
  if glstn==`gluestate.levelfish` then
   -- levelfish: setup fish,bubbles and pylon on spritesheet
   pcelputpixmap(`getbpixmap("fish").id`)
   pcelputpixmap(`getbpixmap("bubble").id`)
   pxcd(`gpylon.sx`,`gpylon.sy`,`gpylon.bpixmap.ram+gpylon.bpixmap.ofs`) -- note: overwrites gwave!
   fish={}stunx=nil
   sbpceli=`getpceli("submarine")`
   plx=`16`
   sby=`gluefish.sub_starty` -- note: sbx=0 since ganim set it last for subdepart
   undrhorizonsh=`undrhorizonfullsh` -- expand horizon dark blue gap so ocean covers gameplay screen
   pylonx=128 -- pylon starts just off rhs of screen
  else
   -- levelinit: cache ui towers
   cacheuitowers()
  end
  skyinit(tlev.sky) -- show next level's sky on glue screen
 end
 undrs[`undrhorizonidx`].sh=undrhorizonsh
 glti=0
 gwavesx={}
 gwdx=0
 -- setup globals for levelinit
 maxlevel=max(maxlevel,curlevel)
 poke(`p8mem.PERSISTENT_DATA+(8+1+3)*4`,maxlevel)
 --curlevel=1
end
function gameinit()
 -- called when transition from glue to game
 plivs=3
 pscor=0
 wavep=0
end
function skyinit(skypceli)
 -- expand sky
 local psky=pcels[skypceli]
 skyh=psky.sh
 local sa=psky.a
 local da=`towskyram`
 for y=1,skyh do
  local sl,sr=peek2(sa,2)
  `compileassert(towskyw==32,'towskyw==32')` -- hard coded width handling...
  poke4(da,sl|sl>>>16,sr|sr>>>16)
  sa+=`2*2`
  da+=`2*4`
 end
 spcl,spcr=peek4(`towskyram`,2)
end
function levelinit()
 -- note: curlevel should be set to current level before calling
 -- setup cell colors for level
 memcpy(`getbpixmap("cell-cpc").pramlo`,`getbpixmap("cell-cpc-pals").pramlo-5`+5*curlevel,5)
 memcpy(`getbpixmap("cell-cpc").pramhi`,`getbpixmap("cell-cpc-pals").pramhi-5`+5*curlevel,5)
 tlev=levels[curlevel]
 mapcellh=tlev.h
 aipceli=tlev.aipceli
 tileinitlevel()
 -- note: don't clear old spritesheet coords for all pixmaps, as some are permanent?!
 --for pix in all(pixmaps) do
 -- pix.sx=nil
 -- pix.sy=nil
 --end
 -- note: cells permanently in spritesheet
 -- note: shotball permanently in spritesheet
 -- note: submarine permanently in spritesheet
 --skyinit(tlev.sky) -- done in glueinit
 -- setup stars
 flip() -- prevent screen tearing as exceed 100% cpu
 srand(curlevel)
 stars={}
 local skipc,skip,skych=0,3,pcels[tlev.sky].sh\`cellpixh`
 for cy=0,mapcellh+`viewpixh\cellpixh`,`cellsperstar` do
  -- note: entry may be false if overlaps sky
  local star=false
  if cy>=skych then
   skipc+=1
   if(skipc>=skip)skipc=0 skip-=1 star={x=rnd(`starspixw`)\1,y=cy*`cellpixh`+rnd(`cellpixh*cellsperstar-starpixh`)\1,c=rnd(10)\1+`qpcel.flag2d`}
  end
  add(stars,star)
 end
 flip() -- prevent screen tearing as exceed 100% cpu
 levelreset()
end
[[]]`

function plstate(st)
 plst=st+0 -- note: convert st to number; we pass strings for reduced tokens
 plsc=0
 if(plst>`pogostate.jump`)plwc=0 -- walk/jump preserve plwc
 plfdy=0
end

function pljump(jc)
 plstate"`pogostate.jump`" --
 pljvx=plvx
 pljdy=pljdys[jc]
end

function plfall(fc)
 plstate"`pogostate.fall`" --
 plfs=1
 pljvx=0
 plfc=-1
 if fc==`pogofall.down` then
  plpceli=`animdirframe('pogo_falll','pldir')`
 elseif fc==`pogofall.fwd` then
  pljvx=(pldir==0 and -1 or 1)
  plpceli=`animdirframe('pogo_fallfwdl','pldir')`
 elseif fc==`pogofall.bwd` then
  pljvx=(pldir==0 and 1 or -1)
  plpceli=`animdirframe('pogo_fallbwdl','pldir')`
 elseif fc==`pogofall.landing` then
  plsc=1
 else --if fc==`pogofall.afterjump` then
  plfs=3
  plpceli=`animdirframe('pogo_falll','pldir')`
 end
end

function plelev(edy,ema)
 pledy=edy
 for i,e in ipairs(elevs) do
  if(e.ma==ema)deli(elevs,i)break
 end
 plstate"`pogostate.elevator`" --
end

function pldrown()
 plstate"`pogostate.drown`" --
 ply=0
 aikill()
 sfx"`getsfxid('sfx-pogo-drown')`"
end

function updateelevs()
 for i=#elevs,1,-1 do
  local e=elevs[i]
  if e.dt>0 then
   e.dt-=1
   if(e.dt==0)sfx"`getsfxid('sfx-elevator-fall')`"
  else
   poke(e.ma,e.t&`cell.MARKER_MID`)
   e.ma-=`mapcellw`
   e.t=@e.ma
   local bot=e.t&`cell.MARKER_MID`==`cell.MARKER_BOT`
   poke(e.ma,bot and `cell.ELEVATOR|cell.MARKER_BOT` or `cell.ELEVATOR|cell.MARKER_MID`)
   if(bot)deli(elevs,i)
  end
 end
end

function mcollides(x,y,ldx,rdx,tdy)
 local lx=x+ldx
 local rx=x+rdx
 local cty=(y+tdy)\`cellpixh`
 local cby=y\`cellpixh`
 for cxx=lx\`cellpixw`,rx\`cellpixw` do
  local tx=cxx*`cellpixw`
  local cx=cxx&`mapcellw-1`
  local ma=`towdram`+cty*`mapcellw`+cx
  for cy=cty,cby,-1 do
   local t=@ma
   local tlr=cellsizes[t]
   if tlr then
    if(lx<tx+tlr[2] and rx>tx+tlr[1]) then
     return t,ma
    end
   end
   ma-=`mapcellw`
  end
 end
 return false,nil
end

function plmovex(da)
 local na=pla+da
 local nx=na\`cella\cellpixw`
 if(mcollides(nx,ply,`pogodims`))return 1
 pla=na
 plx=nx
end

function plcenterx()
 local ta=pla%`cella`
 if(ta==`cella\2`)return
 plmovex(ta<`cella\2` and 1 or -1)
 return 1
end

function plmovey(dy)
 local ny=max(0,ply+dy)
 if(mcollides(plx,ny,`pogodims`))return 1
 ply=ny
end

function plchkfall()
 local cy=ply\`cellpixh`-1
 local lcx=(plx-`pogofallcodes.dx`)\`cellpixw`&`mapcellw-1`
 local rcx=(lcx+1)&`mapcellw-1`
 local ma=`towdram`+cy*`mapcellw`
 local tl=cellplats[@(ma+lcx)]
 local tr=cellplats[@(ma+rcx)]
 local lr=(tl and 2 or 0)+(tr and 1 or 0)
 local t=tl or tr or 0
 return plfallcodes[1+t][1+plx%`cellpixw`]>>(lr*4+pldir*2)&3
end

function plchkdoor()
 local cx=plx\`cellpixw`&`mapcellw-1`
 local cy=ply\`cellpixh`
 local t=@(`towdram`+cy*`mapcellw`+cx)
 return (t==`cell.DOOR` or t==`cell.EXIT`) and t
end

function plma()
 local cx=plx\`cellpixw`&`mapcellw-1`
 local cy=ply\`cellpixh`-1
 return `towdram`+cy*`mapcellw`+cx
end

function plchkelev()
 local ma=plma()
 local t=@ma
 if(t==`cell.MARKER_TOP|cell.ELEVATOR`)return ma,-1
 if(t==`cell.MARKER_BOT|cell.ELEVATOR`)return ma,1
 if(t==`cell.MARKER_MID|cell.ELEVATOR`)return ma,0
 return ma,nil
end

function plbreak()
 local ma=plma()
 if(@ma==`cell.PLATFORM_BROKEN`)poke(ma,`cell.EMPTY`)sfx"`getsfxid('sfx-hit-enemy')`" --
end

function plslide()
 if(plvx==0 and @plma()==`cell.PLATFORM_SLIDE`)plmovex(1)
end

function plchkcollide()
 -- no collisions while drowning or arriving/departing
 if(plst>=`pogostate.drown`)return
 -- drown if hit water
 if(ply<=0)pldrown()return
 -- no collisions while tumbling
 if(plst==`pogostate.tumble`)return
 -- no collisions during door while at least half-way in
 if(plst==`pogostate.door` and (plsc>=`(pogodoor.walkin.loframe+pogodoor.walkin.hiframe+1)\2` or plsc<`(pogodoor.walkout.loframe+pogodoor.walkout.hiframe+1)\2`))return
 if pldt then
  -- delay tumble on elevator until reaches cell boundary
  if(plst==`pogostate.elevator` and ply&`cellpixh-1`!=0)return
 else
  -- enemy collisions
  local e=aiecollides({a=pla,y=ply},`colltype.pogo`)
  if e then
   pltt=e.a and `20*2` or `15*2`
  else
   -- only check tile collisions when on elevator (all other movement prevents overlap)
   if(plst!=`pogostate.elevator` or not mcollides(plx,ply,`pogodims`))return
   pltt=`15*2`
  end
  -- begin delay tumble if on elevator and not on cell boundary
  if(plst==`pogostate.elevator` and ply&`cellpixh-1`!=0)pldt=1 return
 end
 if(plst==`pogostate.elevator`)plpark(plchkelev())
 pldt=nil
 sfx"`getsfxid('sfx-pogo-tumble')`"
 plstate"`pogostate.tumble`" --
 psctq=max(psctq-1)
end

function plpark(ema)
 local t=@ema&`cell.MARKER_MID`
 poke(ema,t|`cell.ELEVATOR`)
 if(t!=`cell.MARKER_BOT`)add(elevs,{dt=`elevholdtime`,ma=ema,t=t})
end

local plfuns={
 function() -- walk
  plbreak()
  local fc=plchkfall()
  if(fc!=0)return plfall(fc)
  plslide()
  if plvx==0 then -- idle
   if(plwc>0)sfx"`getsfxid('sfx-pogo-walk')`" --
   plwc=0
   if plfire then
    if(not plshot)sfx"`getsfxid('sfx-pogo-shoot')`"plstate"`pogostate.shoot`" --
   else
    -- set idle pose
    plpceli=`animdirframe('pogo_idlel','pldir')`
    if plvy!=0 then -- up/down pressed
     -- elevator?
     local ema,edy=plchkelev()
     if(edy==0 or edy==plvy)return plelev(plvy,ema)
     if plvy>0 then
      -- door?
      pldr=plchkdoor()
      if(pldr)sfx"`getsfxid('sfx-pogo-enter-door')`"return plstate"`pogostate.door`" --
     end
    end
   end
  elseif (plvx>0)!=(pldir>0) then
   plstate"`pogostate.turn`" --
  else
   if(plfire)plwc=0 sfx"`getsfxid('sfx-pogo-jump')`"return pljump(`pogojump.jump`) -- reset walk cycle when pogo jumps
   if(plmovex(plvx))return pljump(`pogojump.climb`) -- continue walk cycle when pogo climbs
   plwc+=1
   plpceli=`animdirframe('pogo_walkl','pldir','plwc',{loop=true})`
   if(plwc&`animlen('pogo_walkl')\2-1`==0)sfx"`getsfxid('sfx-pogo-walk')`" --
  end
 end, 
 function() -- jump
  plwc+=1
  plpceli=`animdirframe('pogo_walkl','pldir','plwc',{loop=true})`
  plmovex(pljvx)
  local ody=pljdy[1+plsc]
  local dy=ody
  while plmovey(dy) do
   dy+=(dy<0 and 1 or -1)
   if dy==0 then
    if(ody<0)sfx"`getsfxid('sfx-pogo-walk')`" return plstate"`pogostate.walk`" -- landed
    break
   end
  end
  plsc+=1
  if 1+plsc>#pljdy then
   plfall(plmovey(-1) and `pogofall.landing` or `pogofall.afterjump`)
  end
 end, 
 function() -- fall
  if plsc==0 then -- falling
   plfc+=1
   if(plmovex(pljvx))pljvx=0
   while plmovey(-plfs) do
    plfs-=1
    if plfs==0 then -- collided
     if(pljvx==0 or plfc>=`2*(cella\cellpixw)` and (plfc&1>0))plsc+=1 -- inital slip before land 4 angles = 2 collision pixels
     break
    end
   end
  end
  if plsc==0 then -- still falling
   plfs=mid(1,4,plfs+plfc%2)
  else -- has landed
   if(plsc==1)sfx"`getsfxid('sfx-pogo-walk')`"
   plvjx=0
   plpceli=`animdirframe('pogo_landl','pldir','plsc',{frameoffset=-1})`
   plsc+=1
   if(plsc==`1+animlen('pogo_landl')`)plstate"`pogostate.walk`" --
  end
 end,
 function() -- turn
  plpceli=`animdirframe('pogo_turnltor','pldir','plsc')`
  plsc+=1
  if plsc==`animlen('pogo_turnltor')` then
   pldir^^=1
   plstate"`pogostate.walk`" --
  end
 end,
 function() -- door
  if plsc<`pogodoor.turnin.hiframe+1` then
   plpceli=`animdirframe(pogodoor.turnin.aname,'pldir','plsc',{frameoffset=-pogodoor.turnin.loframe})`
   plcenterx()
  elseif plsc<`pogodoor.walkin.hiframe+1` then
   plpceli=`animdirframe(pogodoor.walkin.aname,nil,'plsc',{frameoffset=-pogodoor.walkin.loframe})`
  elseif plsc<`pogodoor.rotate.hiframe+1` then
   plpceli=nil
   if(pldr==`cell.EXIT`)sfx"`getsfxid('sfx-pogo-walk-exit')`"glstate(`gluestate.levelexit`)elevs={}return plstate"`pogostate.demolish`" --
   pla+=(pldir>0 and `doorrotspda` or `-doorrotspda`)
   plx=pla\`cella\cellpixw`
  elseif plsc<`pogodoor.walkout.hiframe+1` then
   plpceli=`animdirframe(pogodoor.walkout.aname,nil,'plsc',{frameoffset=-pogodoor.walkout.loframe})`
  elseif plsc<`pogodoor.fall.hiframe+1` then
   -- todo: handle tower being destroyed
   if(not plmovey(-4))return -- keep falling until land
   if(plvx!=0)pldir=(plvx>0 and 1 or 0)
   sfx"`getsfxid('sfx-pogo-walk')`" --
  else -- if plsc<`pogodoor.turnout.hiframe+1` then
   plpceli=`animdirframe(pogodoor.turnout.aname,'pldir','plsc',{frameoffset=-pogodoor.turnout.loframe})`
  end
  plsc+=1
  if(plsc>`pogodoor.turnout.hiframe`)return plstate"`pogostate.walk`" --
  if(plsc&`animlen('pogo_walkl')\2-1`==0)sfx"`getsfxid('sfx-pogo-walk-tower')`" --
 end,
 function() -- shoot
  plslide()
  plpceli=`animdirframe('pogo_shootl','pldir','plsc')`
  local da=pldir*2-1
  if(plsc==`1\animspd`)plshot={t=1,a=pla+da*`cella\cellpixw`,y=ply,va=da*`pogoshotaspd`,pceli=`getpceli('shotball')`}
  plsc+=1
  if(plsc==`animlen('pogo_shootl')`)plstate"`pogostate.walk`" --
 end,
 function() -- ride elevator
  if plsc==0 then
   if(plcenterx())return -- first, center pogo on elevator
   plsc+=1
   -- replace elevator tile with pillar or empty
   local ema=plchkelev()
   poke(ema,@ema&`cell.MARKER_MID`|(pledy>0 and `cell.PILLAR` or `cell.EMPTY`))
  end
  -- going up/down
  ply+=pledy
  if(ply&`cellpixh-1`!=0)return
  local ema=plchkelev()
  local t=@ema&`cell.MARKER_MID`
  if pledy<0 then
   sfx"`getsfxid('sfx-elevator-down')`"
   if(t!=`cell.MARKER_BOT` and (t!=`cell.MARKER_MID` or plvy<0))poke(ema,t)return
  else
   sfx"`getsfxid('sfx-elevator-up')`"
   if(t!=`cell.MARKER_TOP` and (t!=`cell.MARKER_MID` or plvy>0))poke(ema,t|`cell.PILLAR`)return
  end
  -- ride finished
  plpark(ema)
  plstate"`pogostate.walk`" --
 end,
 function() -- tumble
  if plsc<pltt then -- tumble arc up/down
   plpceli=`animdirframe('pogo_tumblel','pldir','plsc',{loop=true})`
   plfdy=plfdy%1+pltumbledys[min(1+plsc,`#pogotumbledys`)]
   ply+=plfdy\1
   if(ply<0)pldrown()return
   plsc+=1
  else -- enter fall state once no collisions with tiles
   plpceli=`animdirframe('pogo_falll','pldir')`
   ply-=4
   if(ply<0)pldrown()return
   if(mcollides(plx,ply,`pogodims`))return
   plfall(`pogofall.down`)
  end
 end,
 function() -- drown
  if plsc<`animlen('pogo_drownl')*2` then
   plpceli=`animdirframe('pogo_drownl','pldir','plsc\\2')`
  else
   plpceli=nil
   if(plsc==`animlen('pogo_drownl')*2+64`)plivs-=1 glstate(plivs>0 and `gluestate.levelinit` or `gluestate.gameover`)
  end
  plsc+=1
 end,
 function() -- demolish
  if plsc<`pogodemolish.waitglue.hiframe+1` then
   ais={}
   return -- wait for updlevelexit to increment plsc
  elseif plsc<`pogodemolish.pause.hiframe+1` then
   -- plsc incremented below
  elseif plsc<`pogodemolish.walkout1.hiframe+1` then
   plpceli=`animdirframe(pogodemolish.walkout1.aname,nil,'plsc',{frameoffset=-pogodemolish.walkout1.loframe})`
   if(plsc==`pogodemolish.walkout1.hiframe`)sfx"`getsfxid('sfx-demolish')`" --
  elseif plsc<`pogodemolish.demolish.hiframe+1` then
   -- note: don't update plpceli, just hold last frame
   if mapcellh>6 then -- remove row of tower
    ply-=`cellpixh`
    local da=`towdram`+(mapcellh-6)*`mapcellw`
    poke(da,peek(da+`mapcellw`,5*`mapcellw`))
    mapcellh-=1
    return
   end
   -- todo: wait for camera to catch up to pogo
   pldir=1
  elseif plsc<`pogodemolish.walkout2.hiframe+1` then
   plpceli=`animdirframe(pogodemolish.walkout2.aname,nil,'plsc',{frameoffset=-pogodemolish.walkout2.loframe})`
  elseif plsc<`pogodemolish.turnout.hiframe+1` then
   plpceli=`animdirframe(pogodemolish.turnout.aname,'pldir','plsc',{frameoffset=-pogodemolish.turnout.loframe})`
  else -- if plsc<`pogodemolish.depart.hiframe+1` then
   ganim(subdepart,plsc-`pogodemolish.depart.loframe`)
   if not plsc then
    plsc=`pogodemolish.waitglue.loframe`
    curlevel+=1
    glstate(curlevel<=`#rlevels` and `gluestate.levelfish` or `gluestate.gameover`)
    return
   end
  end
  plsc+=1
 end,
 function() -- arrive in submarine
  ganim(subarrive,plsc)
  if(plsc==nil)--[[cheatexit()]]return plstate"`pogostate.walk`" --
  plsc+=1
 end,
}

function addcolspr(s)
 add(colsprs[1+(s.a\`cella`)],s)
end

function plscore(a,ex)
 local oscor=pscor
 pscor+=a>>16
 if(pscor/5000!=oscor/5000)plivs=min(plivs+1,8) -- extra life every 5000
 pscex=min(pscex+(ex>>16),`numrepr(500>>16,{hexfrac=true})`)
end

function updateshot()
 local c=plshot.t-`pogoshota\pogoshotaspd`
 if c>0 then -- fading out, no collision
  if(c>=`shotball_anim.animlen`)plshot=nil return
  plshot.pceli=`shotball_anim.pceli`+c\`shotball_anim.ooanimspd`
 else -- active, check collisions
  plshot.a+=plshot.va
  plshot.a%=`maxa`
  plshot.y+=plshotdys[plshot.t]
  local t,ma=mcollides(plshot.a\`cella\cellpixw`,plshot.y,`shotdims`)
  if t then
   if(t==`cell.CUBE`)poke(ma,`cell.EMPTY`)plscore(50,2)sfx"`getsfxid('sfx-hit-enemy')`" --
   plshot.t=`pogoshota\pogoshotaspd`
  else
   local e=aiecollides(plshot)
   if e then
    if e.st==`aistate.roll` then
     aistate(e,`aistate.rollstun`)
     e.c=`3*60`
    elseif e.st!=`aistate.rollstun` and e.st!=`aistate.vertical` and e.st!=`aistate.horizontal` then
     sfx"`getsfxid('sfx-hit-enemy')`"
     aistate(e,`aistate.warpout`)
     plscore(100,5)
    end
    plshot.t=`pogoshota\pogoshotaspd`
   end
  end
 end
 plshot.t+=1
 addcolspr(plshot)
end

`if0[[]]
function cheatexit()
 local tgty=mapcellh*`cellpixh`-`cellpixh*4`
 --if(ply==tgty)ais={}plstate(`pogostate.demolish`) -- debug: W again = demolish
 local tgtx
 for x=0,`mapcellw-1` do
  tgtx=x
  if(@(`towdram`+(tlev.h-2)*`mapcellw`+x)==`cell.EXIT`)break
 end
 ply=tgty
 plx=tgtx*`cellpixw`+`cellpixw\2`
 pla=plx*`cella\cellpixw`
 printh("ply="..ply.." plx="..plx.." pla="..pla)
end
[[]]`
`if0[[]]
function cheatexit() -- hacky cheatexit that only works on level 1, but fewer tokens
 ply=344
 plx=36
 pla=72
end
[[]]`

`if0[[]]
function updcheats()
 if btnp(`btni.U0`,1) then -- debug: E = decrease time, and 9 lives
  --plt=`10*10`
  plivs=1
  pscor+=1000>>16
 end
 --if btnp(`btni.U0`,1) then -- debug: E = tumble
 -- pltt=`15*2` --`#pogotumbledys*2`--cpc:20/15
 -- plstate(`pogostate.tumble`)
 --end
 if btnp(`btni.X0`,1)  then -- debug: Q goes to state 'arrive'
  plstate(`pogostate.arrive`)
 end
 if btnp(`btni.O0`,1) and plst!=`pogostate.elevator` then -- debug: W = top row
  local tgty=mapcellh*`cellpixh`-`cellpixh*4`
  --if(ply==tgty)ais={}plstate(`pogostate.demolish`) -- debug: W again = demolish
  local tgtx
  for x=0,`mapcellw-1` do
   tgtx=x
   if(@(`towdram`+(tlev.h-2)*`mapcellw`+x)==`cell.EXIT`)break
  end
  ply=tgty
  plx=tgtx*`cellpixw`+`cellpixw\2`
  pla=plx*`cella\cellpixw`
 end

 local newlevel=curlevel
 if(btnp(`btni.L0`,1))newlevel=1+(newlevel-2)%8 -- S decreases level
 if(btnp(`btni.R0`,1))newlevel=1+newlevel%8 -- F increases level
 if newlevel!=curlevel then
  curlevel=newlevel 
  levelinit()
 end

 if btn(`btni.D0`,1) then -- debug fly/noclip movement
  pla+=plvx
  plx=pla\`cella\cellpixw`
  ply+=plvy
  plfall(`pogofall.down`)
  --plpceli=`getpceli('shotball')` -- debug: testing collision
  plpceli=`animdirframe('pogo_idler')`
  return true -- skip normal movement/collision
 end
end
[[]]`

function plupdate()
 `perfenter('plupdate')`
 local da=pla

 --colsprs={}for i=1,`mapcellw` do add(colsprs,{})end
 for i=1,`mapcellw` do colsprs[i]={} end
 updateelevs()
 if(plshot)updateshot()

`if0[[]]
 if not updcheats() then
[[]]`
  plfuns[plst]() -- update pogo player
  plchkcollide()
`if0[[]]
 end
[[]]`

 da=pla-da
 pla%=`maxa`
 for i=1,`#wbandx` do wbandx[i]-=da/wbandvs[i] end
 
 local dy=max(ply,plst>`pogostate.drown` and 16)-camy
 camy+=ceil(abs(dy)/8)*sgn(dy)

 wboty=camy-`viewpixh\2-max(viewpixh\2-maxwaveh)` -- worldy at bottom of view
 wtopy=wboty+`viewpixh-1` -- worldy at top of view where viewy=statpixh

 wvy=wtopy+`statpixh` -- worldy at top of screen

 aispawn()
 aiupdate()

 if(bblf==0)wavep+=.25 -- don't animate water during bubbles as we draw cached water

 skytop=min(skyh-(`maxwaveh`-min(-wboty,wtopy+1))\4,skyh-1)

 local tiledy=ply\`cellpixh`-plmaxy\`cellpixh`
 if tiledy>0 then
  plmaxy=ply
  plscore(10*tiledy,0)
 end

 if plst<`pogostate.drown` then
  if plt==0 then -- time up
   if(glstn==`gluestate.playing`)glstate(`gluestate.levelfail`)
   return
  end
  plt-=1
 end

 `perfleave('plupdate')`
end

`picoscript[[]]
function addbubbles(x,y,n,t)
 for i=1,n do
  bubblesi=bubblesi%8+1
  bubbles[bubblesi]={x=x+rnd(10)-5,y=y+rnd(10)-5,a=rnd(),da=.1+rnd(.4),t=t}
 end
end
[[]]`

`picoscript[[]]
function updlevelfishp()
 -- picoscript parts of updlevelfish
 if(glsc%`gluefish.ticksperdepthsfx`==0)sfx(`getsfxid('sfx-depth-bell')`)
 if glsc<`gluefish.ticksuntiltextdone` then
  -- pause until "catch some fish for bonuses" message scrolls off
  local t=glsc/`gluefish.ticksuntiltextdone/2`
  sby+=sin(t)/(1+t)
  return
 end
 local arriving=glsc>=`gluefish.ticksuntilarrived` -- arrived at next tower?
 if arriving then
  if(glsc==`gluefish.ticksuntiltransition`)glstate(`gluestate.levelinit`)
  undrscroll=nil -- stop scrolling uderwater scene
  plx+=mid(-1,`gluefish.sub_targetx`-plx,1)
  sby+=mid(-1,`gluefish.sub_targety`-sby,1)
 else
  if glsc>`gluefish.ticksuntilpylon` then
   pylonx=max(`gluefish.pylon_targetx`,pylonx-0.5) -- scroll pylon
  end
  -- move sub; player control
  plx=mid(plx+plvx,`gluefish.sub_minx`,`gluefish.sub_maxx`)
  sby=mid(sby+plvy*2,`gluefish.miny`,`gluefish.maxy`)
 end
 -- move stun
 if stunx then
  stunx+=4
  addbubbles(stunx,stuny,1,30)
  if(stunx>`128-gluefish.fish_bubble_rw`)stunx=nil
 end
 -- launch stun
 if not arriving and glb and not stunx then
  --addbubbles(plx,sby,8,60)
  sfx(`getsfxid('sfx-pogo-shoot')`)
  stunx,stuny=plx,sby-16
 end
 return glsc<`gluefish.ticksuntilpylon` -- spawn fish until pylon appears
end
[[]]`

`if1[[]]
function updlevelfish() -- levelfish
 -- animate propellor
 pcelput(pcels[`getpceli('submarine-propellor')`+glsc%16\2],`scel.submarine.scels[2][1]`,`scel.submarine.scels[2][2]`)
 -- note: fish has same number of colors as enemies so can use getaipal() to cycle between them
 if(updlevelfishp() and #fish<6 and rnd()<.05)add(fish,{x=`128+4`,y=`gluefish.miny+8`+rnd"`gluefish.maxy-gluefish.miny-2*8`",vy=rnd"2"-1,c=getaipal(),a=0})
 for i=#fish,1,-1 do
  local f=fish[i]
  f.x-=1
  f.y+=f.vy -- move fish
  if(f.y<`gluefish.miny` or f.y>`gluefish.maxy`)f.vy*=-1 -- bounce off top/bottom of ocean
  f.a+=.25 f.a%=8 -- animate fish spinning
  if f.x<`-4` then
   deli(fish,i)
  elseif f.stun then
   -- stunned: check collision with sub
   local dx=`30\2+6\2`-abs(f.x-plx) -- horz distance from bow or stern when >0
   local dy=f.y-sby+`12+16\2` -- vert distance from bow or stern
   if(dx>0 and dy>-dx and dy<2*dx)deli(fish,i)plscore(50,0)sfx"`getsfxid('sfx-score')`"
  elseif stunx and abs(f.x-stunx)<`gluefish.fish_bubble_rw` and abs(f.y-stuny+`gluefish.fish_bubble_dy`)<`gluefish.fish_bubble_rh` then
   -- not stunned and collided with torpedo bubble
   f.stun,f.vy,stunx=1,0,nil
   sfx"`getsfxid('sfx-hit-enemy')`"
  end
 end
end
[[]]`

`picoscript[[]]
function glwait()
 return glst==glstn and (glb or glsc==`2.5*60`)  -- if press key or wait for a couple seconds
end
-- playing
function updtitle() -- title
 if(glb)gameinit()glstate(`gluestate.levelinit`)return
 if(btnp(`btni.L0`))curlevel-=1
 if(btnp(`btni.R0`))curlevel+=1
 curlevel=mid(1,maxlevel,curlevel)
 --curlevel=mid(1,maxlevel,curlevel+(btnp(`btni.L0`) and -1 or 0)+(btnp(`btni.R0`) and 1 or 0))
 if glsc>=`gltitle.ticksperscreen` then
  glsc=0
  glti=(glti+1)%`2*gltitle.count`
 end
end
function updlevelinit() -- levelinit
 if(glwait())glstate(`gluestate.playing`)
 --if(glwait())glstate(`gluestate.levelfish`)sbx=0 -- FIXME: USE gluestate.playing!
end
function updlevelexit() -- levelexit
 --assert(plst==`pogostate.demolish`)
 -- apply bonuses to score over time
 local dscor=0
 if glsc>`1.5*60-1` then
  if plt>0 then -- timer bonus countdown
   if(glti!=1)glti=1 sfx(`getsfxid('sfx-score')`)
   plt=max(plt-60) dscor=10
  elseif psctq>0 then -- technique bonus countdown
   if(glti!=2)glti=2 sfx(`getsfxid('sfx-score')`)
   psctq-=1 dscor=10
  elseif pscex>0 then -- extras bonus countdown
   if(glti!=3)glti=3 sfx(`getsfxid('sfx-score')`)
   pscex-=0x.0001 dscor=10
  end
  if(dscor>0)glsc-=1 -- pin glsc while adding score, so that glwait can resume at 2.5 seconds
 end
 if glwait() then
  glstate(`gluestate.playing`)
  plsc+=1 -- stop pausing, walk out
  -- immediately apply all bonuses to score
  dscor+=(plt+59)\60*10 plt=0
  dscor+=psctq*10 psctq=0
  dscor+=(pscex<<16)*10 pscex=0
 end
 plscore(dscor,0) -- add to score, including increasing lives
end
function updlevelfail() -- levelfail
 --assert(plt==0) -- plt==0 for timeout
 if glwait() then
  -- reduce lives, if none left go to game over
  plivs-=1
  glstate(plivs>0 and `gluestate.levelinit` or `gluestate.gameover`)
 end
end
function updgameover() -- gameover
 if glwait() then
  glstate(chkhall() and `gluestate.hallfame` or `gluestate.title`)
 end
end
function chkhall() -- check if belongs in hall of fame
 -- build score string (build it in reverse)
 local psr,pn="",pscor
 for i=0,7 do psr..=(pn%`numrepr(10>>16,{hexfrac=true})`<<16) pn/=10 end
 local ps=""
 for i=8,1,-1 do ps..=sub(psr,i,i) end
 -- insert into hall of fame
 for i=1,4 do
  local hs=hall[i]
  for di=1,8 do
   local dp,dh=ord(ps,di),ord(hs,2+di)
   if(dp<dh)break -- player score < hall score, try next entry
   if dp>dh then -- new high score at index i
    -- copy hall entries down, copying color (first two chars)
    for j=4,i+1,-1 do
     local hj,hu=hall[j],hall[j-1]
     hall[j]=sub(hj,1,2)..sub(hu,3)
    end
    -- overwrite entry i, but keep color (first two chars)
    halli=i
    hallc=0
    hall[i]=sub(hall[i],1,2)..ps..chr(`ord" "`,`ord"A"`+hallc)
    return 1
   end
  end
 end
 return nil
end
function updhallfame() -- hallfame
 if glst==glstn then
  local s=hall[halli]
  if #s==`2+8+1+3` and glb then
   sfx(`getsfxid('sfx-sub-splash')`,3) -- note: channel 3 so doesn't override music that is immediately started
   -- save hall to persistent data
   local a=`p8mem.PERSISTENT_DATA`
   for i=1,4 do
    local hs=hall[i]
    --for j=1,`8+1+3` do poke(a,(ord(hs,2+j)))a+=1 end -- write score and name (not color)
    mpoke(a,sub(hs,3),ord)a+=`8+1+3`
   end
   glstate(`gluestate.title`)
   return
  end
  local hs,hc=sub(s,1,#s-1)
  if(btnp(`btni.U0`))sfx(`getsfxid('sfx-elevator-up')`)hallc=(hallc+1)%26 hc=hallc -- character up
  if(btnp(`btni.D0`))sfx(`getsfxid('sfx-elevator-down')`)hallc=(hallc-1)%26 hc=hallc -- character down
  if(btnp(`btni.L0`))sfx(`getsfxid('sfx-pogo-jump')`)hs=(#hs>`2+8+1` and sub(hs,1,#hs-1) or hs) hc=hallc -- backspace
  if(btnp(`btni.O0`) or btnp(`btni.R0`))sfx(`getsfxid('sfx-hit-enemy')`)hs=s hallc=0 hc=0 -- accept character
  if(hc and #hs<`2+8+1+3`)hall[halli]=hs..chr(`ord"A"`+hc)
 end
end
[[]]`

`picoscript[[]]
function gtrans()
 -- heavy lifting transition between glue states (screen is full of bubbles)
 -- note: call levelinit() on gluestate.levelfish so that underwater pylon is correct color for next level
 if(glstn==`gluestate.levelinit` or glstn==`gluestate.levelfish`)levelinit()nsndsong(`song.enter`)
 if(glstn==`gluestate.levelfail`)nsndsong(`song.fail`)
 if(glstn==`gluestate.gameover`)nsndsong(`song.gameover`)
 if(glstn>=`gluestate.title`)glueinit()
 if(glstn==`gluestate.title`)curlevel=1 nsndsong(`song.title`) -- re-start title song
 if(glst==`gluestate.levelinit` and glstn==`gluestate.playing`)nsndsong(`song.title`) -- re-start title song
 glst=glstn
 glsc=0
 flip()
end
[[]]`

function _update60()
 `perfframe()`
 `perfenter('_udpate60')`

 `perfenter('nsndupdate')`
 nsndupdate(stat(54))
 `perfleave('nsndupdate')`

 if(btnp"`btni.X0`")nsndmusic(not sndmus)

 -- plbuttons: available to all modes
 plvx=tonum(btn"`btni.R0`")-tonum(btn"`btni.L0`")
 plvy=tonum(btn"`btni.U0`")-tonum(btn"`btni.D0`")
 plfire=btn"`btni.O0`"
 
 glb=glst==glstn and btnp"`btni.O0`"
 glsc+=1
 if(glst!=glstn and bblf>`bblframes\2`)gtrans()  -- transition

 wvy=`128+128-1` -- default has bottom of glue screen at worldy=0
 
 if(undrscroll)for u in all(undrs) do u.x=(u.x+u.vx)%u.w end -- scroll underwater scene

 updfuns[glst]()

 -- update bubbles (after updfun so that new bubbles set p=pceli)
 for b in all(bubbles) do
  if b.a then
   local t=t()+b.da
   b.x+=sin(t)*b.da
   b.y+=b.a+abs(cos(t))
   b.p=`animdirframe('bubble_wobble',nil,'b.t',{loop=true})`
   b.t-=1
   if(b.t==0 or b.y>`gluefish.maxy`)b.a=nil
  end
 end

 `perfleave('_udpate60')`
 `perfprint()`
end

function qtimer(t)
 for i,d in ipairs{t\1000%10,t\100%10,t\10%10,10,t%10} do
  poke4(dq,`draw.ddigit`,d,`timerx`+10*i-(d==1 and 2 or 0),`timery`)dq+=`4*4`
 end
end

function qpcel(pceli,a,y,palflags,qi)
 poke4(qi and dq2 or dq,`draw.dpcel`,pceli,a,y,palflags)
 if qi then dq2+=`5*4` else dq+=`5*4` end
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
   local sa=`p8mem.SCREEN`+(wvy-ty-diy)*64
   if u.pceli==0 then -- horizon pure ocean
    poke4(dq,`draw.dclear`,sa,ty-by+1,`numrepr(undrhorizonclo)`,`numrepr(undrhorizonchi)`)dq+=`5*4`
   else -- underwater section
    poke4(dq,`draw.dundr`,dy,i,sa,ty-by)dq+=`5*4`
   end
  end
  --break
 end
end

function qtext(s,x,y,c,font)
 local dy=wvy-y-`8-1` -- world y to screen y
 if(dy+7+textamp<diy or dy-textamp>diy+127 or dy>=bblminy)return
 add(sq,s)
 poke4(dq,`draw.dtext`,#sq,x+textofs,dy,font and c or textphs|textamp,font and 1 or 0)dq+=`6*4`
end
function qbonus(s,n,y)
 qtext(s..(n\100%10)..(n\10%10)..(n%10),0,y)
end

function qplats(cminy,cmaxy,cols,pl)
 -- enqueue doors on dq2 and invoke it first to draw doors before plats
 poke4(dq,`draw.djmp`,dq2-dq)dq+=`2*4`
 local dqret=dq
 local minma=`towdram`+cminy*`mapcellw`
 local minvy=wvy-cminy*`cellpixh`-`cellpixh-1`
 for dji=1,16 do
  cols>><=1
  if cols&1>0 then
   local cx=(plj+cdjs[dji])&`mapcellw-1`
   local ma=minma+cx
   local vx=towox[(cx*`cella`+`cella\2`-pla)%`maxa`]
   local vy=minvy
   local dqcells
   for cy=cminy,cmaxy do
    local t=@ma
    if t!=`cell.EMPTY` then
     if t==`cell.DOOR` or t==`cell.EXIT` then
      local lx=towix[(cx*`cella`-pla)%`maxa`]
      local rx=towix[(cx*`cella`+`cella-1`-pla)%`maxa`]
      if rx>=lx then
       -- doors inserted into dq2
       poke4(dq2,`draw.ddoor`,lx,rx,vy,towbl[lx-`64-towir`],towbl[rx-`64-towir`])dq2+=`6*4`
      end
     else
      -- plats inserted into dq1
      local pcel=cellpcels[t]
      if pcel then
       if(not dqcells)dqcells=dq+`4*4` poke4(dq,`draw.dcell`,dqcells,vx-pcel.sw\2,0)dq=dqcells
       poke4(dq,pcel.id,vy)dq+=`2*4`
      end
     end
    end
    ma+=`mapcellw`
    vy-=`cellpixh`
   end
   if(dqcells)poke4(dqcells-4,dq)
   for s in all(colsprs[1+cx]) do qpcel(s.pceli,s.a,s.y,s.p)end
  end
 end
 -- pogo queued on dq2 if entering door, else dq1
 if(pl and plpceli)qpcel(plpceli,pla,ply,0,plst==`pogostate.door`)
 -- jump from end of dq2 back to dq1
 poke4(dq2,`draw.djmp`,dqret-dq2)dq2+=`2*4`
end

function qtower(diy,tofsy,wtminy,wtmaxy)
 if(wtmaxy<wtminy)return
 local da=`p8mem.SCREEN`+(wvy-wtmaxy-diy)*64
 local ta=((tofsy-wtmaxy)%tileh)*`64`
 poke4(dq,`draw.dtower`,da,ta,wtmaxy,wtminy)dq+=`5*4`
end

function qstars(wminy,wmaxy,minx,maxx)
 for scy=(wminy-`starpixh-1`)\`cellpixh*cellsperstar`,wmaxy\`cellpixh*cellsperstar` do
  local star=stars[1+scy]
  if star then
   local x=(pla*`starspixw\maxa`+star.x)&`starspixw-1`
   if(x>=minx and x<maxx)qpcel(`getpceli("star")`,x,star.y,star.c^^glsc&1)
  end
 end
end

function qgame(wminy,wmaxy,wtminy,wtmaxy)
 `perfenter('qgame')`
 local cminy=wtminy\`cellpixh` -- bottom-most cell just within view, clipped to tower
 local cmaxy=wtmaxy\`cellpixh` -- top-most cell just within view, clipped to tower

 local clipgamet=wvy-wmaxy-diy -- inclusive min y, as used by clip
 local clipgameb=wvy+1-wtminy-diy -- exclusive max y, as used by clip
 clipgameb=mid(clipgamet,bblminy-diy,clipgameb) -- clip under bubbles
 local clipairy=clipgamet>>8|clipgameb<<8 -- clip top and bottom above ocean, as used by clip

 -- cached tower including space left/right of tower
 -- note: top 6 rows need consistent texmapping during demolish
 local both=(mapcellh-6)*`cellpixh`
 qtower(diy,both-(tlev.h-6)*`cellpixh`,max(wtminy,both),wtmaxy)
 qtower(diy,0,wtminy,min(wtmaxy,both-1))

 -- sky texture left/right of tower
 local wskymaxy=min(skytop,wmaxy)
 if wskymaxy>=wtminy then
  local so=skytop-wskymaxy
  local da=`p8mem.SCREEN`+(wvy-wskymaxy-diy)*64
  local sa=`towskyram`+so*`2*4`
  poke4(dq,`draw.dsky`,sa,da,wtminy,wskymaxy,mapcellh*`cellpixh`)dq+=`6*4`
 end

 local cam=camgametb[dtb] -- camera for top/bottom displays

 -- above tower
 local wspaceminy=max(max(skytop,wtmaxy)+1,wminy) -- world min y of space
 if wmaxy>=wspaceminy then
  -- space above tower
  poke4(dq,`draw.dclear`,`p8mem.SCREEN`+clipgamet*64,wmaxy+1-wspaceminy,spcl,spcr)dq+=`5*4`
  -- clip/camera for stars above tower
  local clipspaceb=mid(clipgamet,clipgameb,wvy+1-wspaceminy-diy) -- exclusive min y for clipping, under bubbles
  poke4(dq,`draw.dclipcam`,`mkclip(0,0,128,0)`|clipgamet>>8|clipspaceb<<8,cam)dq+=`3*4`
  -- stars in space above tower
  qstars(wspaceminy,wmaxy,`0-starpixw\2`,`128+starpixw\2`)
 end

 -- stars and plats behind/left tower
 poke4(dq,`draw.dclipcam`,cliptowerlr[1]|clipairy,cam)dq+=`3*4`
 qstars(wtminy,wtmaxy,`0-starpixw\2`,`64-towir+starpixw\2`)
 qplats(cminy,cmaxy,colbl)
 -- stars and plats behind/right tower
 poke4(dq,`draw.dclipcam`,cliptowerlr[2]|clipairy,cam)dq+=`3*4`
 qstars(wtminy,wtmaxy,`64+towir-starpixw\2`,`128+starpixw\2`)
 qplats(cminy,cmaxy,colbr)

 -- clip/camera for objects in front of tower
 poke4(dq,`draw.dclipcam`,`mkclip(0,0,128,0)`|clipairy,cam)dq+=`3*4`
 -- plats in front of tower + pogo
 qplats(cminy,cmaxy,colf,1)
 -- elevator under pogo
 if plst==`pogostate.elevator` and plsc>0 then
  qpcel(`getpceli('cell-cpc',platcell.ELEVATOR)`,pla,ply-`cellpixh`)
 end
 -- bonker in front of pogo
 for e in all(ais) do
  if(not e.a)qpcel(e.pceli,e.x,e.y,e.p)
 end
 -- submarine in front of pogo
 qsub(64)

 `perfleave('qgame')`
end

function qsub(x)
 if sbpceli then
  qpcel(sbpceli,x+sbx,sby,`qpcel.flag2d`)
  for i=1,3 do
   qpcel(sbpceli+i,x`-10-10`+i*10,sby-20,`qpcel.flag2d`)
  end
 end
end

function reflcopy(sminy,smaxy,ystep)
`if1[[]] -- need compressed size temporarily
 -- assume ystep is negative (copy bottom up)
 `perfenter('reflcopy')`
 local waddr,sastep=`wavespritesheet`,ystep*64
 for tb=1,0,-1 do
  local diy=tb*128
  local dminy=max(0,sminy-diy)
  local dmaxy=min(127,smaxy-diy)
  if dminy<=dmaxy then
   local steps=(dminy-dmaxy+ystep)\ystep -- note: overall positive (dmaxy+1-dminy+ystep-1 negated as ystep<0)
   for lr=0,1 do
    _map_display(tb*2+lr)
    local wa=waddr+lr*32
    local sa=`p8mem.SCREEN`+dmaxy*64
    for i=1,steps do
     poke4(wa,peek4(sa,`32\4`))
     wa+=64
     sa+=sastep
    end
   end
   waddr+=steps*64
   smaxy+=steps*ystep -- note: overall subtract (not -= since ystep<0)
  end
 end
 `perfleave('reflcopy')`
[[]]`
end

function refldup(cdy,csy)
`if1[[]] -- need compressed size temporarily
 -- duplicate wave scanlines
 --  from csy down (interlaced scanlines we did draw last frame)
 --  to cdy down (interlaced scanlines we won't be drawing this frame)
 `perfenter('refl.dupintlc')`
 local cda=`p8mem.SCREEN`+cdy*64
 local csa=`p8mem.SCREEN`+csy*64
 for di=2,3 do
  _map_display(di)
  if csy>cdy then -- copy up, so iterate down
   local dofs=cda-csa
   for sa=csa,`p8mem.SCREEN+(statpixh+viewpixh-128-1)*64`,`2*64` do
    poke4(sa+dofs,peek4(sa,`64\4`))
   end
  elseif csy<cdy then -- copy down, so iterate up
   local sofs=csa-cda
   for da=`p8mem.SCREEN+(statpixh+viewpixh-128-2)*64`+cdy%2*64,cda,`-2*64` do
    poke4(da,peek4(da+sofs,`64\4`))
   end
  end
 end
 `perfleave('refl.dupintlc')`
[[]]`
end

function reflblend(dminy,dmaxy,wimin,ystep) -- interlaced sspr version
 `perfenter('reflblend')`
 poke4(`p8mem.CAM_X`,`mkcamera(0,0)`)
 poke(`p8mem.FILLP_CONTROL`,`(1<<1)`) -- bit0=transparency, bit1=sprite fillp enable, bit2=apply sprite fillp to other draw apis
 -- identity draw palette as lookup into secondary palette for fill pattern colors
 poke4(`p8mem.PAL_DRAW`,0x0302.0100,0x0706.0504,0x0b0a.0908,0x0f0e.0d0c)
 local sy,dstep=`wavespritesheet\64`,ystep*64 -- src spritesheet wave y
 --local wavex,wavep,wfills=wavex,wavep,wfills -- cache globals?
 for lr=0,1 do
  _map_display(2+lr)
  local sx=lr*64 -- src spritesheet wave x
  local clp,cam=`mkclip(0,0,64,128)`,`mkcamera(0,0)`
  for lohi=0,1 do
   poke4(`p8mem.CLIP`,clp,`0`,cam)
   local wi,wbi,wbni,fx,dofs,sy=wimin,1,wbands[1],wbandx[1]&3,dminy*64+lohi*32,sy
   for dy=dminy,dmaxy,ystep do
    while 1+wi>=wbni do -- TODO: subtract 1 in wbands?
     poke4(`p8mem.PAL_SECOND`,peek4(`wpalsram-2*16`+wbi*`2*16`+lohi*`16`,`16\4`))
     fx=wbandx[wbi]&3
     wbi+=1
     wbni=wbands[wbi]-- or `maxwaveh+1` (sentinel inserted at build time in buildwaves)
    end
    poke2(`p8mem.FILLP`,wfills[1+wi]>><fx)
    local wx=sx-wavex[wavep+wi&31]
    sspr(wx,sy,64,1,0,dy)
    -- duplicate 4 pixels on left/right where wave gaps show; wrong but less obviously wrong
    if wx<0 then
     poke2(`p8mem.SCREEN`+dofs,peek2(`p8mem.SCREEN+2`+dofs))
    elseif wx>64 then
     poke2(`p8mem.SCREEN+32-2`+dofs,peek2(`p8mem.SCREEN+32-4`+dofs))
    end
    dofs+=dstep
    wi+=ystep
    sy+=1
   end
   clp,cam=`mkclip(64,0,64,128)`,`mkcamera(-64,0)`
  end
 end
 poke2(`p8mem.FILLP`,0) -- disable fillp pattern for subsequent draw api calls
 poke(`p8mem.FILLP_CONTROL`,`0`) -- bit0=transparency, bit1=sprite fillp enable, bit2=apply sprite fillp to other draw apis
 memcpy(`p8mem.PAL_SECOND`,`pal2ram`,16) -- restore secondary palette for hi color screen palette
 `perfleave('reflblend')`
end

`picoscript[[]]
function wavebackup(dminy,dmaxy,bakyofs)
 -- cache waves from displays to waveram, used during bubble transition
 local sz,bofs=(dmaxy+1-dminy)*64,bakyofs*64
 for di=2,3 do
  _map_display(di)
  memcpy(waverams[di-1]+bofs,`p8mem.SCREEN`+dminy*64,sz)
 end
end

function waverestore(dminy,dmaxy)
 local sz=(dmaxy+1-dminy)*64
 for di=2,3 do
  _map_display(di)
  memcpy(`p8mem.SCREEN`+dminy*64,waverams[di-1],sz)
 end
end
[[]]`

function dcell(dqb,x,dqe)
 `perfenter('d.dcell')`
 local pcel=pcels[$dqb] -- use first pcel for clip width and pal of whole column
 local pals=pcel.pixmap.pals[1]
 if x<dix+64 and x+pcel.sw>dix then -- horizontal cull whole column
  for lohi=1,2 do
   poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
   poke4(`p8mem.PAL_DRAW`,peek4(pals[lohi],`16\4`)) -- assume all cells share same palette
   for dq=dqb,dqe-1,`2*4` do
    local pceli,y=peek4(dq,2)
    local pcel=pcels[pceli]
    sspr(pcel.sx,pcel.sy,pcel.sw,pcel.sh,x,y)
   end
  end
 end
 `perfleave('d.dcell')`
 return `4+3*4`+dqe-dqb
end

function ddoor(x1,x2,y1,lc,rc)
 `perfenter('d.ddoor')`
 local y2=y1+`cellpixh-1`
 local pal=tilepix.doorlpal
 for lohi=1,2 do
  poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
  poke(`p8mem.PAL_DRAW`,pal[lc],pal[rc],pal[`towblends+1`])
  if(x2>x1)rectfill(x1+1,y1,x2-1,y2,2)
  line(x1,y1,x1,y2,0)
  line(x2,y1,x2,y2,1)
  pal=tilepix.doorrpal
 end
 `perfleave('d.ddoor')`
 return `4+5*4`
end

function dsky(sa,da,miny,maxy,towpixh)
 `perfenter('d.dsky')`
 local aofs=16 -- when dix=0, "above" tower is da+16, and draw sky left of tower
 if(dix!=0)da,aofs=da+16,-16 -- when dix==64, "above" tower is da, and draw sky right of tower
 for y=maxy,miny,-1 do
  local sl,sr=peek4(sa,2)sa+=`2*4`
  -- left/right of tower
  poke4(da,sl,sl,sl,sl)poke4(da+32,sr,sr,sr,sr)
  -- above tower
  if(y>=towpixh)poke4(da+aofs,sl,sl,sl,sl)poke4(da+aofs+32,sr,sr,sr,sr)
  da+=64
 end
 `perfleave('d.dsky')`
 return `4+5*4`
end

function dtower(da,ta,wtmaxy,wtminy)
 `perfenter('d.dtower')`
 local tsz=tileh*64
 local sa=`128*64`-tsz
 if(dix==0)da+=`32\2` else sa+=`32\2`
 local ds=da^^`32\2`
 local l,r,poke4=spcl,spcr,poke4
 for ry=wtminy,wtmaxy do
  poke4(ds,l,l,l,l)poke4(ds+32,r,r,r,r)
  poke4(da,peek4(sa+ta,4))poke4(da+32,peek4(sa+ta+32,4))
  da+=64 ds+=64
  ta=(ta+`64`)%tsz
 end
 `perfleave('d.dtower')`
 return `4+4*4`
end

function dclear(da,h,l,r)
 `perfenter('d.dclear')`
 for y=1,h do -- clear lo and hi colors
  poke4(da,
   l,l,l,l,l,l,l,l,
   r,r,r,r,r,r,r,r)
  da+=64
 end
 `perfleave('d.dclear')`
 return `4+4*4`
end

function dtext(si,dx,dy,c,font)
 `perfenter('d.dtext')`
 local s=sq[si]
 if font==1 then
  for lohi=1,2 do
   poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
   poke(`p8mem.PAL_DRAW+1`,c&15)
   print(s,dx,dy,1)
   c\=16
  end
 else
  local sp,sa=c%1,c\1 -- phase, amplitude
  local p=pcels[`getpceli("fontscifi")`]
  -- assume cycle/ramp at start of string (TODO: make parameters instead?)
  local ci,ri=ord(s,1,2) -- 1-based cycle index, 1-based ramp index
  local r=colorramps[ri] -- ramp
  local rlen=r<<5&31 -- ramplen
  r+=cycles[`glev.cyclelo-1`+ci].i%rlen
  for lohi=1,2 do
   poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
   poke4(`p8mem.PAL_DRAW`,peek4(r,`16\4`))
   poke(`p8mem.PAL_DRAW`,16) -- transparent background
   local x=dx
   for i=3,#s do
    local ci=ord(s,i)i+=1
    ci=max(ci-`ord'0'-1`)
    local sx,sy=p.sx+ci%16*8,p.sy+ci\16*8
    sspr(sx,sy,8,8,x,dy+sa*sin(sp+x/48))
    x+=8
   end -- for i
   r+=rlen+`colorrampseglen-1`
  end -- for lohi
 end
 `perfleave('d.dtext')`
 return `4+5*4`
end

function dpcel(pceli,wa,wy,palflags)
 `perfenter('d.dpcel')`
 local dx=palflags>=`qpcel.flag2d` and wa or towox[(wa-pla)%`maxa`]
 local pcel=pcels[pceli%`pceliflipx`]
 local sw,pals=pcel.sw,pcel.pixmap.pals[1+palflags&`qpcel.maskpal`]
 dx-=sw\2
 if dx<dix+64 and dx+sw>dix then -- horizontal cull
  local sh,flipx=pcel.sh,pceli>=`pceliflipx`
  local dy=wvy-wy-sh+1
  for lohi=1,2 do
   poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
   poke4(`p8mem.PAL_DRAW`,peek4(pals[lohi],`16\4`))
   sspr(pcel.sx,pcel.sy,sw,sh,dx,dy,sw,sh,flipx)
  end
 end
 `perfleave('d.dpcel')`
 return `4+4*4`
end

function ddigit(i,dx,dy)
 `perfenter('d.ddigit')`
 local pcel=pcels[`getpceli("timerdigit")`]
 if dx<dix+64 and dx+pcel.sw>dix then -- horizontal cull
  local pals=pcel.pixmap.pals[1+i]
  for lohi=1,2 do
   poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
   poke4(`p8mem.PAL_DRAW`,peek4(pals[lohi],`(9+3)\4`))
   sspr(pcel.sx,pcel.sy,pcel.sw,pcel.sh,dx,dy)
  end
 end
 `perfleave('d.ddigit')`
 return `4+3*4`
end

function dundr(y,i,sa,h_1)
 `perfenter('d.dundr')`
 local u=undrs[i]
 local p,ox,ws=pcels[u.pceli],u.x\1,u.w==16
 local pals=p.pixmap.pals[1]
 for lohi=1,2 do
  poke4(`p8mem.CLIP`,dclips[lohi],`0`,dcams[lohi])
  poke4(`p8mem.PAL_DRAW`,peek4(pals[lohi],`16\4`))
  -- draw dword-aligned sprite on lhs of screen
  sspr(p.sx+ox,p.sy,p.sw-ox,p.sh,dix,y)
  sspr(p.sx,p.sy,ox,p.sh,dix+p.sw-ox,y)
  -- repeat-copy dword-aligned pixels to fill screen horizontally
  local a=sa
  for _=0,h_1 do
   if(ws)poke4(a+`2*4`,peek4(a,`2`))
   poke4(a+`4*4`,peek4(a,`4`))
   a+=64
  end
  sa+=32
 end
 `perfleave('d.dundr')`
 return `4+4*4`
end

function dclipcam(clp,cam)
 `perfenter('d.dclipcam')`
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
 dclips[2]=dclips[1]+`numrepr(64>>16|64)` -- dcliphi
 `perfleave('d.dclipcam')`
 return `4+2*4`
end

function djmp(ofs)
 return ofs
end

--local dfuns={dcell,ddoor,dsky,dtower,dclear,dtext,dpcel,ddigit,dundr,dclipcam,djmp}
function drawq(dq,dqend)
 `perfenter('drawq')`
 while dq!=dqend do
  dq+=dfuns[$dq](peek4(dq+4,`drawmaxparams`))
 end
 `perfleave('drawq')`
end

`picoscript[[]]
function cacheuitowers()
 pcelputpixmap(`getbpixmap("uitowerbg").id`)
 pcelputpixmap(`getbpixmap("uitower").id`)
 -- prepare to cache to spritesheet
 poke(`p8mem.ADDR_SCREEN`,0) -- draw to spritesheet
 clip()camera() --poke4(`p8mem.CLIP`,`mkclip(0,0,128,128)`,`0`,`mkcamera(0,0)`)
 for i=0,15 do poke(`p8mem.PAL_DRAW`+i,i)end --poke4(`p8mem.PAL_DRAW`,0x0302.0100,0x0706.0504,0x0b0a.0908,0x0f0e.0d0c)
 local pcel=pcels[`getpceli("gluetower")`]
 pcel.sx=`gluetower.sx`
 pcel.sy=`gluetower.sy`
 rectfill(`gluetower.sx`,`gluetower.sy`,`gluetower.sx+8*gluetower.sw-1`,`gluetower.sy+gluetower.sh-1`,`gluetower.trans`)
 for i=0,7 do
  local tlev=levels[1+i]
  local x=i*`gluetower.sw`
  poke(`p8mem.PAL_DRAW+gluetower.bgc`,`gluetower.minc`+i) -- remap tower color 1 to unique index
  sspr(`gluetower.bg.scel[1]`,`gluetower.bg.scel[2]`,`gluetower.bg.pix.sw`,`gluetower.bg.pix.sh`,x,`gluetower.sy+gluetower.sh-gluetower.bg.pix.sh`)
  if(1+i>=curlevel)sspr(`gluetower.fg.scel[1]`,`gluetower.fg.scel[2]`,`gluetower.fg.pix.sw`,tlev.glh,x,`gluetower.sy+gluetower.sh-gluetower.bg.pix.sh+1`-tlev.glh)
 end
 -- stop cache to spritesheet
 poke(`p8mem.ADDR_SCREEN`,0x60) -- draw to screen
end
[[]]`

`picoscript[[]]
-- playing=2
function easetext(ticksperscreen)
-- 1 sec in, 4 sec hold, 1 sec out

 -- x comes in fast, eases to zero in middle
 -- pauses for a while
 -- then x eases out, accelerating off left
 --
 -- y sin waves, largest amplitude on right
 -- when in middle, does one wave then eases to zero amplitude
 -- before moves off, eases up to max amplitude for one wave
 -- then goes off left
 -- phase is based on x coordinate
 local t=min(glsc,ticksperscreen-glsc)
 textphs=t/60%1

 if t<`gltitle.ticksxease` then
  local s=1-t/`gltitle.ticksxease`
  textofs=sgn(t-glsc)*128*s*s -- negative when ease out, positive ease in
 end
 if t<`gltitle.ticksystop` then
  local s=1-max(t-`gltitle.ticksyease`)/`gltitle.ticksystop-gltitle.ticksyease`
  textamp=`gltitle.amplitude`*s*s\1
 end
end
function drtitle() -- title
 qpcel(`getpceli("title")`,64,`gluey.title`,`qpcel.flag2d`)

 easetext(`gltitle.ticksperscreen`)
 if glti%2==1 then
  `qtextmid("\2\3HIGH SCORERS",gluey.subtitle+12)`
  for i=1,4 do
   qtext(hall[i],`hallx`,`gluey.line1+14+16`-i*16)
  end
 else
  -- credits title text stored in tables
  local t=ttxt[1+glti\2]
  for i=1,#t,3 do
   local s,x,y=unpack(t,i,i+2)
   gs=s gx=x gy=y
   qtext(s,x,y)
  end
 end
 -- NEBULUS           (special font)
 --
 -- written by        (orange peach)
 -- john m  phillips  (light yellow)
 -- (C)1987 hewson    (white, note: (C) is single symbol)
 --
 -- one player        (grey blue)
 -- sound on          (grey blue)
 textofs=0
 textamp=0

 `qtextmid("\142 to play",gluey.play+20,3,1)`
 `qtextmid("\151 music on/off",gluey.play+10,3,1)`
 qtext("\139\145 start level "..curlevel.."/"..maxlevel,`64-10*4-2`,`gluey.play`,3,1)
end
function drlevelinit() -- levelinit
 `qtextmid("entering the",gluey.intro,26,1)`
 local lev=levels[curlevel]
 qtext(lev.name,lev.midx,`gluey.subtitle2`)

 -- draw cached towers
 qpcel(`getpceli("gluetower")`,64,`gluetower.bg.y`,`qpcel.flag2d`)

 -- >> game on <<     (light yellow)
 -- player one        (light green)
 --
 -- entering the      (light grey)
 -- tower of eyes     (deep blue, same color as tower)
 --
 -- graph of 8 towers (blue, peachy orange, purple, orange, light green, grey blue, yellow, white)
end
function drlevelexit() -- levelexit
 `qtextmid("tower completed!",gluey.intro,26,1)`
 
 --textphs,textamp=glsc/60%1,`gltitle.amplitude`
 `qtextmid("\2\3BONUS POINTS",gluey.subtitle)`

 --textamp=0
 qbonus("\3\4TIME LEFT 10?",plt\60,`gluey.line1`) -- note: "?" is multiplication symbol
 qbonus("\3\2TECHNIQUE 10?",psctq,`gluey.line2`)
 qbonus("\3\9EXTRAS    10?",pscex<<16,`gluey.line3`)

 -- tower completed   (white)
 -- bonus points      (light grey)
 --
 -- time left 10x020  (light yellow)
 -- technique 10x000  (peach)
 -- extras    10x019  (grey blue)
end
function drlevelfail() -- levelfail
 --textphs,textamp=glsc/60%1,`gltitle.amplitude`
 `qtextmid("\2\3TIME UP",gluey.subtitle2)`
end
function drlevelfish() -- levelfish
 -- pylons
 if pylonx<128 then
  local pi=diy==0 and 0 or `gpylon.npylons`
  for i=1,`gpylon.npylons` do
   -- setup dcell draw call for each pylon segment visible on top/bottom displays
   local p=rpylons[pi+i]
   local dqb=dq+`4*4`
   local dqe=dqb+p.len
   poke4(dq,`draw.dcell`,dqb,pylonx+p.x,dqe)
   memcpy(dqb,`gpylon.dcellram`+p.ofs,p.len)
   dq=dqe
  end
 end
 -- fish
 for f in all(fish) do
  if(f.stun)qpcel(`gluefish.bubble.bpixmap.pceli+3`,f.x,f.y,`qpcel.flag2d`)
  qpcel(`gluefish.fish.bpixmap.pceli`+f.a\1,f.x,f.y,`qpcel.flag2d`+(f.stun and 0 or f.c))
 end
 -- stun bubble
 if(stunx)qpcel(`gluefish.bubble.bpixmap.pceli+3`,stunx,stuny,`qpcel.flag2d`)
 -- submarine 
 qsub(plx)
 -- text
 if glsc<`gluefish.ticksuntiltextdone` then
  easetext(`gluefish.ticksuntiltextdone`)
  `qtextmid("\2\3CATCH SOME FISH",gluey.subtitle2)`
  `qtextmid("\2\3FOR BONUSES",gluey.line1)`
 end
end
function drgameover() -- gameover
 if plivs>0 then -- victory
  textphs,textamp=glsc/60%1,`gltitle.amplitude`
  `qtextmid("\2\4CONGRATULATIONS",gluey.subtitle)`
  textamp=0
  `qtextmid("\1\3YOU HAVE",gluey.line1)`
  `qtextmid("\1\2COMPLETED",gluey.line2)`
  `qtextmid("\1\x0aYOUR MISSION",gluey.line3)`
else
 `qtextmid("\2\3GAME OVER",gluey.subtitle)`
end
 --if victory:
 -- congratulations   (white)
 -- player one        (light green)
 -- you have          (light grey)
 -- completed         (grey)
 -- your mission      (darker grey)
 --
end
function drhallfame() -- hallfame
 `qtextmid("\2\3HIGH SCORERS",gluey.subtitle+12)`
 for i=1,4 do
  qtext(hall[i],`hallx`,`gluey.line1+14+16`-i*16)
 end
 `qtextmid("please enter",gluey.play+10,3,1)`
 `qtextmid("your initials",gluey.play,3,1)`

 -- high scorers      (light grey)
 --
 -- 00042640   a      (white -- or wherever they were placed, enter name here)
 -- 00005000   jmp    (light yellow)
 -- 00003000   jmp    (orange peach)
 -- 00001000   jmp    (peach)
 --
 -- please enter      (deep purple)
 -- your initials     (deep purple)
end
[[]]`

`picoscript[[]]
function qglue()
 textofs,textamp=0,0 -- default text animation (note: textphs doesn't matter)
 drfuns[glst]()
end
[[]]`

function dstatus()
 `perfenter('_draw.status')`
 poke4(dq,`draw.dclear`,`p8mem.SCREEN`,`statpixh`,0,0)dq+=`5*4`
 poke4(dq,`draw.dclipcam`,`mkclip(0,0,128,128)`,`mkcamera(0,0)`)dq+=`3*4`
 qtimer(plt\`60\10`) -- timer
 for i=0,plivs-1 do -- lives remaining hearts
  qpcel(`getpceli("pogoface")`,`128-28`+i%4*7,wvy-7-i\4*8,`qpcel.flag2d`)
 end
 local s=pscor
 for i=0,7 do
  
  qpcel(`getpceli("scoredigits")`+(s%`numrepr(10>>16,{hexfrac=true})`<<16),`7*4+2`-i*4,wvy-8,`qpcel.flag2d`)
  s/=10
 end
 `perfleave('_draw.status')`
end

function _draw()
 `perfframe()`
 `perfenter('_draw')`

 bblminy=bblyextents[1+bblf]
 
 local g=(glst==`gluestate.playing`)
 cycleupdate(g and tlev or glev)

 if g then
  -- cache tower row to spritesheet
  towercache(pla)
  -- update pogo spritesheet
  if(plpceli)pcelput(pcels[plpceli%`pceliflipx`],`scelcoord('pogo')`)
  -- copy ai frames to spritesheet. fixme: pcel.sx and sy never cleared
  for i,e in ipairs(ais) do
    if(e.pceli)pcelput(pcels[e.pceli],unpack(scelais[i]))
  end
  -- calculate tower column visibility
  colbl=0
  colbr=0
  colf=0
  plj=pla\`cella`&`mapcellw-1` -- player column j
  for dji,dj in ipairs(cdjs) do
    local j=(plj+dj)&`mapcellw-1`
    local a=(j*`cella`+`cella\2`-pla)%`maxa`
    if a<=`maxa\4` or a>=`3*maxa\4` then -- in front and visible
    colf|=1<<>dji
    else
    local ox=towox[a]
    if(ox<`64-towir+platpixw\2`)colbl|=1<<>dji -- behind left but visible
    if(ox>`64+towir-platpixw\2`)colbr|=1<<>dji -- behind right but visible
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
  dq=`drawqram`
  dq2=`drawqram+drawq1size`
  
  if diy==0 then -- status
   if(g)dstatus()
  else -- footer, only need to draw black to clear transition bubbles
   poke4(dq,`draw.dclear`,`p8mem.SCREEN+(statpixh+viewpixh-128)*64`,`256-(statpixh+viewpixh)`,0,0)dq+=`5*4`
  end

  local wmaxy=wvy-diy
  local wminy=wmaxy-127
  local wtminy
  if g then
   wminy=max(wminy,wboty) -- world inclusive min y of display
   wmaxy=min(wmaxy,wtopy) -- world inclusive max y of display
   wtminy=max(wminy,0) -- world tower inclusive min y within display, clipped to tower
   local wtmaxy=min(wmaxy,mapcellh*`cellpixh`-1) -- world tower inclusive max y within display, clipped to tower
   -- game above water
   qgame(wminy,wmaxy,wtminy,wtmaxy)
  elseif glst>`gluestate.boot` then
   -- note: sbpceli is true when submarine visible, i.e. gluestate.levelfish
   -- if sbpceli, then don't draw sky, and don't draw waves
   -- clear sky above water
   if diy==0 then
    if(not sbpceli)poke4(dq,`draw.dsky`,`towskyram`,`p8mem.SCREEN+glueundryt*64`-skyh*64,1,skyh,0)dq+=`6*4`
    if(glst>=`gluestate.levelinit`)dstatus() else poke4(dq,`draw.dclear`,`p8mem.SCREEN`,`glueundryt`-skyh,spcl,spcr)dq+=`5*4`
   end
   -- clip/camera for game view
   poke4(dq,`draw.dclipcam`,mid(0,bblminy-diy,128)<<8|`mkclip(0,0,128,0)`,camgametb[tb])dq+=`3*4`
   qundr(wvy-`glueundryb`,wminy,wmaxy)
   -- call glstate specific draw
   qglue()
   if(not sbpceli and `glueundryt-gwave.sh`<bblminy)dgw=gwavesdraw
  else
   -- draw black screen on boot, so transition bubbles don't appear elongated
   -- note: fast enough to do whole screen, don't need to draw to mid(0,bblminy-diy,128); saves 9 tokens
   poke4(dq,`draw.dclear`,`p8mem.SCREEN`,128,spcl,spcr)dq+=`5*4`
  end
  
  for b in all(bubbles) do -- just queue bubbles on both top/bottom to reduce token usage
   if(b.a)qpcel(b.p,b.x,b.y,`qpcel.flag2d`)
  end

   --assert(dq<`drawqram+drawq1size`)

  for lr=0,1 do
   _map_display(di+lr)
   dix=lr*64
   drawq(`drawqram`,dq)
  end
 end -- top/bottom loop

 if(g and wboty<0)drawwaves()
 if(dgw)dgw()  -- gwavesdraw 8%

 -- bubble transition, and record music in background until complete, not during transition bubbles
 if(bblf>0)bbldraw()bblf=(bblf+1)%60 else nsndrecord() -- FIXME: should nsndrecord!

 -- DEBUG horz line between displays
 --_map_display(2)memset(0x6000,3,64)
 --_map_display(3)memset(0x6000,3,64)

 `perfleave('_draw')`
 `perfprint()`
end

`picoscriptD[[]]
function drawbblwaves(sminy,smaxy,rmaxy)
 -- transition bubbles are active
 local sbblminy=bblminy-128 -- inclusive bottom display y of bubble occlude region (unclipped, may be negative)
 if bblf==1 then
  -- game to glue, on first frame backup all visible waves and don't draw waves
  wavebackup(sminy,smaxy,0)
 elseif sbblminy>=sminy then
  if bblf>30 and wavebakh<-wboty then -- note: -wboty=wave height
   -- glue to game, draw up to cachewavehpf each frame and backup
   local drawh=min(-wboty-wavebakh,`cachewavehpf`)
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
[[]]`

`picoscriptD[[]]
function drawwaves()
 -- copy reflection from displays
 waveintlc^^=1 -- toggle interlace
 local sminy=wboty+`statpixh+viewpixh-128` -- inclusive bottom display y of top of ocean
 local smaxy=`statpixh+viewpixh-1-128` -- inclusive bottom display y of bottom of ocean
 local rmaxy=sminy+`-1+128` -- inclusive screen y of bottom to be reflected (note: whole screen y, not bottom display y!)
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
[[]]`

`picoscript[[]]
function bblinit()
 memset(`bblclrram`,`(bblcolor%16)<<4|(bblcolor%16)`,32)
 memset(`bblclrram+32`,`(bblcolor\16)<<4|(bblcolor\16)`,32)
end
[[]]`

`if0[[]]function bbldraw()end[[]]`
`if1[[]] -- need tokens temporarily
function bbldraw()
 -- https://www.lexaloffle.com/bbs/?tid=3588 (by emu)
 poke2(`p8mem.PAL_DRAW+1`,`(bblcolor&15)|(bblcolor\16*256)`)
 local ft=1+bblf*.05
 for di=0,3 do
  _map_display(di)
  poke4(`p8mem.CAM_X`,camgametb[1+di\2])
  local dy=di\2*128
  -- draw visible bubbles
  local xib=di%2*4
  for yi=0,8 do
   local w=`getbblwcode('ft','yi')`
   if w>0 then
    for xi=0,4 do
     local y=`getbblycode('ft','yi','(xib+xi)')`
     `getbblyflipcode('bblf','y')`
     local t,b=y-2*w+1,y+2*w
     if max(t,0)<bblminy then
      local l,r=`getbblxcode('xi')`-w+1,`getbblxcode('xi')`+w
      poke4(`p8mem.CLIP`,`clipscreen[1]`)
      ovalfill(l,t,r,b,1)
      poke4(`p8mem.CLIP`,`clipscreen[2]`)
      ovalfill(l+64,t,r+64,b,2)
     end
    end
   end
  end
  -- fast draw band of full bubbles
  local y=max(bblminy-dy)
  dclear(`p8mem.SCREEN`+y*64,128-y,`numrepr(bblcolorlo)`,`numrepr(bblcolorhi)`)
 end
end
[[]]`

`picoscript[[]]

`if0[[]]
function memhex(a,len)
 local s=""
 while len>0 do
  s..=sub(tostr(@a,true),5,6)
  a+=1
  len-=1
 end
 return s
end
[[]]`

function sfxinit()
 local s=`bsfxram`
 local d=`p8mem.SFX+bsfxid*68`
 while s!=`bsfxram+bsfxs.dsize` do
  local ctrl=$s s+=4
  poke4(d+64,ctrl)
  local len=(ctrl&255)*2
  memcpy(d,s,len)
  s+=len
  d+=68
 end
end
[[]]`

function mpoke(d,s,f,b,e)
 poke(d,f(s,b or 1,e or #s))
end

`picoscript[[]]
function _init()
 `perfframe()`
 `perfenter('init')`
 glev=glev[1] -- was a val/struct just to SYMBOL-ize field names
 --poke(`p8mem.FEATURE_CTRL`,17) -- 1=enable map_dispay, 16=enable top 32K (done in psboot!)
 --memset(`p8mem.PAL_SCREEN`,0,16) -- clear screen palette (done in psboot!)
 tcells=fpaq1d(`rcelldata.rom+rcelldata.csize-4`,8)
 --for i=0,`ranimdata.size*4-1` do poke(`ranimdata.ram`+i,(peek(ranimdata.rom+i\4))) end
 for i=0,`ranimdata.size*4-1` do poke(`ranimdata.ram`+i,(ord(ranimdata,1+i\4))) end
 mpoke(`sndram`,snddata,ord)
 --mpoke(`bsfxram`,[[fpaq1d(bsfxrom+bsfxs.csize-4,8)]],unpack)
 mpoke(`bsfxram`,sfxdata,ord) -- write compressed sfxdata to sfxram
 mpoke(`bsfxram`,fpaq1d(`bsfxram+bsfxs.csize-4`,8),unpack) -- unpaq compressed ram to table, then poke to sfxram
 memcpy(`pal2ram`,`pal2rom`,16)
 --memcpy(`wpalsram`,[[wpalsrom]],wpalssize)
 mpoke(`wpalsram`,wpals,ord) -- write compressed wpals to wpalsram
 mpoke(`wpalsram`,fpaq1d(`wpalsram+wpalscsize-4`,8),unpack) -- unpack to table, then poke to ram
 -- note: rom items < 0x2000 (overlapping spritesheet) already moved by here
 --`perfmark('memcpys')`
 colorrampinit()
 -- note: some pixmaps are poked directly to spritesheet 0x0000-0x1fff,
 -- so must init all things that are located at rom address <0x2000 above here.
 pixmapinit()
 --`perfmark('pixmapinit')`
 --`perfmark('clss')`
 progress()towerinit()
 --`perfmark('towerinit')`
 waveinit()
 --`perfmark('waveinit')`
 bblinit()
 --`perfmark('bblinit')`
 progress()nsndinit()
 progress()sfxinit()
 --`perfmark('nsndinit')`
 -- setup map from cell to pcel index
 cellpcels={}
 for c,p in pairs(cellplats) do
  cellpcels[c]=pcels[`getpceli('cell-cpc')`+p]
 end
 -- setup pylon pcels, point to the cell pixmap for shared level-specific palette
 for pcel in all(pylonpcels) do
  pcel.pixmap=pixmaps[`getbpixmap('cell-cpc').id`]
  add(pcels,pcel)
 end
 -- expand pylon dcells from bytes to int16.16 for memcpy'ing to drawq for dcells() call
 for i=1,`#gpylon.dcells` do poke4(`gpylon.dcellram-4`+i*4,(ord(pylondcells,i))) end
 --
 bubbles={}bubblesi=0
 glst=`gluestate.boot`
 glstate(`gluestate.title`)glsc=0
 
 local a=`p8mem.PERSISTENT_DATA`
 if cartdata("carlc27843_nebulus") and @a!=0 then
  -- read hall from persistent data
  for i=1,4 do
   local hs=sub(hall[i],1,2) -- preserve color from default hall of fame
   for j=1,`8+1+3` do hs..=chr(@a) a+=1 end -- read score and name
   hall[i]=hs
  end
 end
 maxlevel=mid(@a,1,`#rlevels`) -- if no persistent data this will be mid(0,1,8)=1, else read byte of persistent data after hall of fame
 `perfleave('init')`
 `perfprint()`
 --progress(1)
 --for di=0,3 do _map_display(di)cls() end
 pal() -- reset screen palette  (lo 16 screen colors)
 memcpy(`p8mem.PAL_SECOND`,`pal2ram`,16) -- initialize secondary palette (hi 16 screen colors)
 --poke(`p8mem.SCREEN_MODE`,1) -- horizontal stretch 64x128
 poke(`p8mem.HICOLOR_MODE`,0x20) -- hicolor 5 bitplane mode (do this after pixmapinit so don't have to clear secondary palette to keep screen black)
end
[[]]`

`varvals{
 vardecl('pixmaps',tblrepr(rpixmaps,{struct=true,tokenize=true,hexfrac=true})),
 vardecl('levels',tblrepr(rlevels,{struct=true,tokenize=true})),
 vardecl('glev',tblrepr({glev},{struct=true,tokenize=true})),
 vardecl('pylonpcels',tblrepr(gpylon.pcels,{struct=true,tokenize=true})),
 vardecl('rpylons',tblrepr(gpylon.rpylons,{struct=true,tokenize=true})),
 vardecl('cycles',tblrepr(rcycles,{struct=true,tokenize=true})),
 vardecl('camgametb',tblrepr(camgametb,{tokenize=true,hex=true})),
 vardecl('cliptowerlr',tblrepr(cliptowerlr,{tokenize=true,hex=true})),
 vardecl('cellplats',tblrepr(cellplats,{tokenize=true})),
 vardecl('cellsizes',tblrepr(cellsizes,{tokenize=true})),
 vardecl('scelais',tblrepr(scel.ais.scels,{tokenize=true})),
 vardecl('waverams',tblrepr({waveramA,waveramB},{tokenize=true})),
 vardecl('wfills',tblrepr(wfills,{tokenize=true})),
 vardecl('wbands',tblrepr(wbands,{tokenize=true})),
 vardecl('wbandx',tblrepr(wbandx,{tokenize=true})),
 vardecl('wbandvs',tblrepr(wbandvs,{tokenize=true})),
 vardecl('pljdys',tblrepr(pogojumpdys,{tokenize=true})),
 vardecl('plfallcodes',tblrepr(pogofallcodes.ts,{tokenize=true})),
 vardecl('plshotdys',tblrepr(pogoshotdys,{tokenize=true})),
 vardecl('pltumbledys',tblrepr(pogotumbledys,{tokenize=true})),
 vardecl('cdjs',tblrepr({-8,7,-7,6,-6,5,-5,4,-4,3,-3,2,-2,1,-1,0},{tokenize=true})), -- back-to-front order of relative columns
 vardecl('aispawnt',tblrepr(aispawnt,{tokenize=true})),
 vardecl('aibouncedys',tblrepr(aibouncedys,{tokenize=true})),
 vardecl('collmask',tblrepr(collmask,{struct=true,tokenize=true})),
 vardecl('towtx',tblrepr(towtx,{tokenize=true})),-- towtx[xi] = tile x at tower column xi
 vardecl('towbl',tblrepr(towbl,{tokenize=true})), -- towbl[xi] = blend level for column xi
 vardecl('sndsfxs',tblrepr(snd.sfxs,{tokenize=true,hexfrac=true})),
 vardecl('sndsubs',tblrepr(snd.subs,{tokenize=true})),
 vardecl('sndchns',tblrepr(snd.chns,{struct=true,tokenize=true})),
 vardecl('subarrive',tblrepr(subarrive,{tokenize=true})),
 vardecl('subdepart',tblrepr(subdepart,{tokenize=true})),
 vardecl('undrs',tblrepr(undrs,{struct=true,tokenize=true})),
 vardecl('bblyextents',tblrepr(bblyextents,{tokenize=true})),
 vardecl('colorramps',tblrepr(colorramps,{tokenize=true})),
 vardecl('colsprs',tblrepr({},{tokenize=true})),
 vardecl('dclips',tblrepr({0,0},{tokenize=true})),
 vardecl('dcams',tblrepr({0,0},{tokenize=true})),
 -- note: '&' prefix makes val() resolve global lookup. SYMBOL() wrap allows minifier to find symbol.
 vardecl('drfuns',tblrepr({ 
  '&SYMBOL(max)', -- gluestate.boot: do nothing
  '&SYMBOL(nil)', -- gluestate.playing: not used
  '&SYMBOL(drtitle)',
  '&SYMBOL(drlevelinit)',
  '&SYMBOL(drlevelexit)',
  '&SYMBOL(drlevelfail)',
  '&SYMBOL(drlevelfish)',
  '&SYMBOL(drgameover)',
  '&SYMBOL(drhallfame)',
 },{tokenize=true})),
 vardecl('updfuns',tblrepr({
  '&SYMBOL(max)', -- gluestate.boot: do nothing
  '&SYMBOL(plupdate)', -- gluestate.playing
  '&SYMBOL(updtitle)',
  '&SYMBOL(updlevelinit)',
  '&SYMBOL(updlevelexit)',
  '&SYMBOL(updlevelfail)',
  '&SYMBOL(updlevelfish)',
  '&SYMBOL(updgameover)',
  '&SYMBOL(updhallfame)',
 },{tokenize=true})),
 vardecl('dfuns',tblrepr({
  '&SYMBOL(dcell)',
  '&SYMBOL(ddoor)',
  '&SYMBOL(dsky)',
  '&SYMBOL(dtower)',
  '&SYMBOL(dclear)',
  '&SYMBOL(dtext)',
  '&SYMBOL(dpcel)',
  '&SYMBOL(ddigit)',
  '&SYMBOL(dundr)',
  '&SYMBOL(dclipcam)',
  '&SYMBOL(djmp)'
 },{tokenize=true})),
 --vardecl('sndenvis',tblrepr(snd.envis,{tokenize=true})), -- debug only!
 vardecl('hall',tblrepr(hall,{tokenize=true,hexfrac=true})),
 vardecl('ttxt',tblrepr(gltitletext,{tokenize=true,hexfrac=true})),
 vardecl('pylondcells',strrepr(gpylon.dcells,{quote='',quotequote='"'})),
 vardecl('snddata',strrepr(snd.bc.data,{quote='',quotequote='"'})),
 vardecl('sfxdata',strrepr(rsfxdata,{quote='',quotequote='"'})),
 vardecl('ranimdata',strrepr(ranimdata.bc.data,{quote='',quotequote='"'})),
 vardecl('wpals',strrepr(wpalsc,{quote='',quotequote='"'})),
}`
-- 117 tokens, but adds up to 4% cpu (high variance)
`picoscript[[]]
function nsndnote(chn,x,p,d)
 if(chn.d>0)return
 chn.d=d
 chn.v=nil
 if p>0 then
  local sfx=sndsfxs[x]
  p+=sfx<<16 --  pitchAdjust
  if x<`snd.numrefs` then
   -- sfx ref - set track notes to use sfx instrument
   -- setup tracker note, 8<<12=sfx instrument, 3<<12=retrigger on first note
   p|=x<<6|`8<<12`|sndrt -- pitch|sfxi|instrument=true|remove-row0-continue
   -- if sfxi changes, or pitch changes, or prev volume is zero, then will auto-retrigger
   -- otherwise, must set effect 3 to retrigger
   if(chn.q&`63|7<<6|8<<12`==p and chn.q&`7<<9`!=0)p^^=`3<<12`  -- if same pitch and instrument, retrigger
   p|=chn.m<<9 -- mix amplitude
  else
   -- sfx val - copy sfx to track
   sfx+=4
   chn.v=sfx
   chn.vb=sfx+@(sfx-2)*2 -- loopStart
   chn.ve=sfx+@(sfx-1)*2 -- loopEnd
   chn.tt=0
  end
 end
 chn.q=p -- by default, rest tracker note = 0 volume, and sfx value base pitch in q
end
[[]]`

-- picoscript saves 189 tokens, but adds ~10% cpu
`picoscript[[]]
function nsndstep(chn,a)
 while chn.d==0 do
  local c,p,d=peek(chn.a,3)
  --if(chn.i==4)printh("nsnd: chn"..chn.i.." a="..tostr(chn.a,true).." c="..chr(c).." p="..p.." d="..d)
  --printh("nsnd: chn"..chn.i.." c="..chr(c).." p="..p.." d="..d.." a="..chn.a)
  chn.a+=3
  if c==`ord"P"` then -- play note
   if(p>0)p+=chn.s-`21+12`
   nsndnote(chn,chn.e,p,d)
  elseif c==`ord"N"` then -- noise
   --printh("chn"..chn.i.." noise ne="..chn.n.." x="..`snd:envi('chn.n')`.." p="..p.." d="..d)
   nsndnote(sndchns[3],chn.n,p,d)
  elseif c==`ord"S"` then --- stop
   chn.a-=3 -- stay on stop
   nsndnote(chn,0,0,1) -- output 0 to tracker
  elseif c==`ord"C"` then -- call
   chn.ra=chn.a-1
   chn.a=sndsubs[p]
  elseif c==`ord"R"` then -- return
   chn.a=chn.ra
  elseif c==`ord"L"` then -- launch
   --printh("nsndlaunch chn="..p.." sub="..d)
   local lchn=sndchns[p]
   lchn.a=sndsubs[d]
   lchn.d=0
  else
   chn.a-=1
   chn[chr(c)]=p
  end
 end
 if chn.v then -- copyby value, not using sfx instrument
  if(chn.v==chn.ve)chn.v=chn.vb
  poke2(a,chn.q+%chn.v+chn.tt)
  chn.tt^^=chn.t
  chn.v+=2
 else -- copyby ref, using sfx instrument
  poke2(a,chn.q^^sndrt) -- invert effect 3 to supress/cause retrigger on row 0
  chn.q&=`~(3<<12)\1` -- clear retrigger after first note
 end
 chn.d-=1
end
[[]]`

`picoscript[[]]
function nsndrecordaux()
 -- sndpkts is single array, divided into 3*68 byte packets of music sfx data per pattern
 repeat
  sndrt=`3<<12` -- retrigger when sndrow=0
  for ra=`sndmusicrecram`,`sndmusicrecram+32*2-2`,2 do -- a = each note address in sfx 8
   local a=ra
   for chn in all(sndchns) do -- setup one row in each channel
    nsndstep(chn,a)
    a+=68 -- music in sfx 8/9/10
   end
   sndrt=0
   yield()
  end
  for a=`sndmusicrecram`,`sndmusicrecram+3*68-4`,4 do
   add(sndpkts,$a)
  end
  yield()
 until sndchns[1].x
end
[[]]`

`picoscript[[]]
function nsndrecord()
 while stat(1)<.90 do
  if(costatus(nsndrecordco)=='dead')nsndrecord=max return
  coresume(nsndrecordco)
 end
end
[[]]`

function nsndupdate(p)
 if(not sndmus or nsong!=`song.title`)return -- not playing main song
 -- note: due to fade out, keep streaming music even when it's off (it remains unused)
 -- p=current pattern id
 -- t=current tick within pattern (divide by musicspd to get row)
 while sndpat+1&3!=p do
  poke4(`sndmusicram`+sndpat%4*`3*68`,unpack(sndpkts,1+sndpkt,`1+3*68\4`+sndpkt))
  sndpkt+=`3*68\4`
  sndpkt%=#sndpkts
  sndpat+=1
 end
end

`picoscript[[]]
function nsndinit()
 -- pattern0: sfx=8,9,10,unused (loop start)
 -- pattern1: sfx=11,12,13,unused
 -- pattern2: sfx=14,15,16,unused
 -- pattern3: sfx=17,18,19 (loop end)
 -- pattern4: sfx=songenter1,songenter2,unused,unused (stop)
 -- pattern5: sfx=songgameover1,songgameover2,songgameover3,unused (stop)
 poke4(`p8mem.SONG`,0x400a.0988,0x400d.0c0b,0x4010.0f0e,0x4013.9211,
  `numrepr(0x40c0|(getsfxid"sfx-song-enter-chn2">>8)|(getsfxid"sfx-song-enter-chn1">>16),{hexfrac=true})`,
  `numrepr(0x4080|(getsfxid"sfx-song-gameover-chn3")|(getsfxid"sfx-song-gameover-chn2">>8)|(getsfxid"sfx-song-gameover-chn1">>16),{hexfrac=true})`,
  `numrepr(0x40c0|(getsfxid"sfx-song-fail-chn2">>8)|(getsfxid"sfx-song-fail-chn1">>16),{hexfrac=true})`)
 -- poke ref sfxs
 for i=0,`(snd.numrefs-1)*68`,68 do
  local a=sndsfxs[i\68]
  memcpy(`0x3200`+i,a+4,`32*2`)
  poke4(`0x3200+64`+i,$a)
 end
 for a=`sndmusicram+64`,`sndmusicram+(4*3-1)*68+64`,68 do
  poke4(a,`numrepr(snd.musicspd>>8,{hexfrac=true})`)  -- spd=3
 end
 memcpy(`sndmusicrecram`,`sndmusicram`,`3*68`) -- copy ctrl spd from record sfx packets
 --nsndlaunch(sndchns[1],1) -- main song
 --nsndlaunch(sndchns[3],0) -- drums repeatedly one-shot and stop
 sndpkts={}
 nsndrecordco=cocreate(nsndrecordaux)
 -- record first 8 packets; 4 are streamed initially, +4 to prevent looping immediately if CPU 
 -- starved on startup (by transitioning into game very quickly)
 while(#sndpkts!=`3*68\4* 8`)coresume(nsndrecordco) 
 sndmus=1 -- music enabled, but song not yet set so don't begin playing
 --nsong=`song.title`
end
[[]]`

`picoscript[[]]
function nsndmusic(enable)
 sndmus=enable
 if sndmus then -- music is now on
  sndpkt=0 -- start playback at beginning of pkts
  sndpat=0
  nsndupdate(0)
  if(nsong==`song.title`)music(0,0,`(1<<0)|(1<<1)`) -- music 0, zero fade in, channels 0,1 (channel 2=drums can be overriden by sfx)
 else -- music is now off
  sndmus=nil
  if(nsong==`song.title`)music(-1,`1200`)
 end
 --menuitem(1,sndmsg,nsndmusic)
end
function nsndsong(song)
 nsong=song
 if(song==`song.title`)nsndmusic(sndmus) else music(song)
end
 [[]]`
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
`if0[[]]
function perfprintaux(level,it,name,markt,out,acc)
 for p in it do
  local k,t,n=unpack(p)
  if k==1 then -- enter
   add(out,'')local oi=#out
   t=perfprintaux(level..' ',it,n,t,out,acc)-t
   local a=(acc[n] or {t=0,c=0})
   a.t+=t
   a.c+=1
   acc[n]=a
   out[oi]=level..n.." incl="..t.." acc="..a.t
  elseif k==-1 then -- leave
   return t
  else -- k==0 -- mark
   add(out,level..name.."-"..n.." mark="..(t-markt))
   markt=t
  end
 end
end

function perfprint()
 --assert(false)
 local out={}
 local acc={}
 perfprintaux('',all(perfs),"frame",0,out,acc)
 for s in all(out)do printh(s)end
 for n,a in pairs(acc)do printh(n.." c="..(a.c).." t="..(a.t))end
end
[[]]`