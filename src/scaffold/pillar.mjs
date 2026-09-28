import { visibleTreeBlock, traceVisibleTreeBlock } from './reach.mjs'
import { inspectTree } from '../tree/inspect.mjs'
import { recoverFarm } from '../farm/attention.mjs'
import { safeFullBlock } from '../enclosure/blocks.mjs'
import { isAir } from '../lib/world.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
const key=p=>`${p.x},${p.y},${p.z}`
export const pillarMaterial = name => /^(dirt|coarse_dirt|cobblestone|(?:[a-z_]+)_(?:planks|log|wood|stem|hyphae))$/.test(name)
export const pillarStock = api => Object.entries(api.inv()).filter(([n])=>pillarMaterial(n)).reduce((sum,[,n])=>sum+n,0)
const near=(api,p)=>Math.floor(api.pos().x)===p.x&&Math.floor(api.pos().z)===p.z&&Math.abs(api.pos().y-p.y)<.25
async function settledFeet(api,report,label,target=null){
 let stable=0,last=null
 for(let n=0;n<20;n++){
  await api.checkpoint?.()
  const p=api.pos(),grounded=api.grounded?.()!==false
  const aligned=target?near(api,target):Math.abs(p.y-Math.round(p.y))<.25
  const cell={x:Math.floor(p.x),y:Math.floor(p.y),z:Math.floor(p.z)},k=key(cell)
  if(grounded&&aligned&&k===last)stable++
  else stable=grounded&&aligned?1:0
  if(stable>=2)return true
  last=k
  await api.pause?.(.1)
 }
 const p=api.pos()
 return stop(report,`${label} did not settle on grounded feet at ${key(p)}; pillar action skipped and supports retained`)
}
const gotoRecoverable=async(api,args,report)=>{
 try{await api.act('goto',args);return true}
 catch(e){return recoverFarm(error=>{report.attention.push(error.message);api.acknowledgeFailure?.('goto');return false})(e)}
}
const save=(api,r,report)=>{
 report.cleanup_left=r.cells.filter(p=>!api.block(p.x,p.y,p.z)||api.block(p.x,p.y,p.z).name===p.item).map(p=>({...p}))
 api.scaffolds(r.id,r);api.report?.(report)
}
const stop=(report,message)=>{report.attention.push(message);return false}

// A persisted cell is a placement intent, not by itself proof that it was
// placed. Older journals were written immediately after checking the entire
// column was air, however, so an exact matching block in a planned pillar
// column can be recovered when the associated forestry journal confirms it
// was not one of the tree's original logs. Cleanup still separately requires
// the player to stand above the cell and validates its landing.
function recoverPillarIntent(api,r,report){
 if(!Array.isArray(r.cells)||!Array.isArray(r.verified))return false
 const forestry=api.forestry?.(r.id)
 const sameRoot=forestry?.root&&r.root&&['x','y','z'].every(axis=>forestry.root[axis]===r.root[axis])
 if(!sameRoot||forestry.species!==r.species||forestry.form!==r.form||forestry.phase!=='harvesting'||!Array.isArray(forestry.wood))return false
 const treeWood=new Map(forestry.wood.map(p=>[key(p),p.name])),clearance=new Map((r.clearance??[]).map(p=>[key(p),p.name]))
 let changed=false
 for(const p of r.cells){
  const k=key(p)
  if(r.verified.includes(k))continue
  // A trunk location may also be a valid pillar column after the tree log was
  // chopped. It is safe to infer replacement only when the current support
  // material differs from that exact original log; equal/unknown material is
  // ambiguous and remains untouched.
  if(treeWood.has(k)&&(!treeWood.get(k)||treeWood.get(k)===p.item))continue
  if(clearance.has(k)&&(!clearance.get(k)||clearance.get(k)===p.item))continue
  const planned=(r.columns??[]).some(c=>c.x===p.x&&c.z===p.z&&p.y>=c.y&&p.y<=c.top)
  const block=api.block(p.x,p.y,p.z)
  if(!planned||block?.name!==p.item||!safeFullBlock(block)||!safeFullBlock(api.block(p.x,p.y-1,p.z)))continue
  r.verified.push(k)
  p.provenance='recovered-journaled-pillar-intent'
  changed=true
 }
 if(changed){save(api,r,report);return true}
 return false
}

// Only one solid column exists at a time. Descent removes exactly one owned
// support per step, checking the landing before digging and arrival afterwards.
export async function descendPillar(api,r,report,targetY=null){
 const remaining=r.cells.filter(p=>!api.block(p.x,p.y,p.z)||api.block(p.x,p.y,p.z).name===p.item).sort((a,b)=>b.y-a.y)
 for(const p of remaining){
  if(targetY!==null&&p.y<targetY)continue
  await api.checkpoint?.()
  if(!r.verified.includes(key(p)))return stop(report,`unverified temporary pillar at ${key(p)}; inspect before cleanup`)
  if(!near(api,{...p,y:p.y+1}))return stop(report,`temporary pillar cleanup needs body above ${key(p)}; supports retained`)
  if(!await settledFeet(api,report,'before temporary pillar descent',{...p,y:p.y+1}))return false
  const below=api.block(p.x,p.y-1,p.z)
  if(!safeFullBlock(below))return stop(report,`temporary pillar landing changed below ${key(p)}; supports retained`)
  if(api.scaffoldOccupied?.({...p,top:p.y+1}))return stop(report,`another entity occupies temporary pillar ${key(p)}`)
  const zones=(await api.act('zones')).zones??[]
  if(foreignZone(zones,api.me?.(),p))throw Error(`protected zone at ${key(p)} prevents pillar cleanup`)
  if(api.block(p.x,p.y,p.z)?.name!==p.item)return stop(report,`temporary pillar changed at ${key(p)}`)
  try {await api.act('dig',{x:p.x,y:p.y,z:p.z,batch:true})}finally{save(api,r,report)}
  if(!await settledFeet(api,report,'after temporary pillar descent',p))return false
  if(!isAir(api.block(p.x,p.y,p.z)?.name)||!near(api,p))return stop(report,`temporary pillar descent unverified at ${key(p)}`)
  r.cells=r.cells.filter(q=>key(q)!==key(p));r.verified=r.verified.filter(k=>k!==key(p));save(api,r,report)
 }
 return true
}
export async function cleanupPillar(api,r,report){
 recoverPillarIntent(api,r,report)
 const remaining=r.cells.filter(p=>api.block(p.x,p.y,p.z)?.name===p.item).sort((a,b)=>b.y-a.y)
 // A one-cell legacy pillar is a safe, bounded step from nearby ground. Walk
 // onto the verified support explicitly so cleanup does not depend on the
 // player already being stranded above it from the earlier run.
 let approachFailed=false
 if(remaining.length===1&&!near(api,{...remaining[0],y:remaining[0].y+1})){
  const p=remaining[0],support=api.block(p.x,p.y,p.z)
  if(!r.verified.includes(key(p))){stop(report,`unverified temporary pillar at ${key(p)}; inspect before cleanup`);approachFailed=true}
  else if(!safeFullBlock(support)||!safeFullBlock(api.block(p.x,p.y-1,p.z))){stop(report,`temporary pillar landing changed below ${key(p)}; supports retained`);approachFailed=true}
  else if(!isAir(api.block(p.x,p.y+1,p.z)?.name)||!isAir(api.block(p.x,p.y+2,p.z)?.name)){stop(report,`temporary pillar headroom is obstructed above ${key(p)}; supports retained`);approachFailed=true}
  else if(api.scaffoldOccupied?.({...p,top:p.y+1})){stop(report,`another entity occupies temporary pillar ${key(p)}`);approachFailed=true}
  else {
   const zones=(await api.act('zones')).zones??[]
   if(foreignZone(zones,api.me?.(),p))throw Error(`protected zone at ${key(p)} prevents pillar cleanup`)
   if(await gotoRecoverable(api,{x:p.x,y:p.y+1,z:p.z,range:0,dig:false,into:true},report)){
    if(!await settledFeet(api,report,`owned temporary pillar top ${key(p)}`,{...p,y:p.y+1}))approachFailed=true
   } else approachFailed=true
  }
 }
 if(approachFailed){save(api,r,report);return report}
 try {await descendPillar(api,r,report)}finally{
  save(api,r,report)
  if(!report.cleanup_left.length)api.scaffolds(r.id,null)
 }
 return report
}
export function startPillar(api,tree,plan,id){
 if(!api.scaffolds)throw Error('scaffold provenance storage is unavailable')
 if(api.scaffolds(id))throw Error('existing scaffold journal requires cleanup before building')
 const r={id,root:tree.root,kind:'pillar',species:tree.species,form:tree.form,clearance:plan.clearance??[],columns:plan.columns,platforms:[],cells:[],verified:[]}
 api.scaffolds(id,r);return r
}
async function reachPillarSpot(api,spots,report,r){
 const pos=api.pos(),ordered=[...spots].sort((a,b)=>{
  const cost=p=>Math.hypot(p.x+.5-pos.x,p.z+.5-pos.z)*3+Math.abs(p.y-pos.y)
  return cost(a)-cost(b)
 })
 const spot=ordered[0]
 if(!spot)return null
 const same=Math.floor(pos.x)===spot.x&&Math.floor(pos.z)===spot.z
 if(!await descendPillar(api,r,report,same?spot.y:null))return null
  if(!same||!r.cells.length){
  if(r.cells.length)return null
  // Reclaim the previous column before spending another stack on a new one.
  if(same || Math.abs(api.pos().y-r.root.y-1)<.25)await api.act('collect',{range:3})
  const base=spot.scaffold?{x:spot.x,y:spot.scaffold.y,z:spot.z}:spot
  if(spot.scaffold&&r.clearance.some(p=>p.x===base.x&&p.z===base.z)){
   if(!await gotoRecoverable(api,{...spot.scaffold.exit,range:0,dig:false,into:true},report))return null
   if(!await settledFeet(api,report,'temporary pillar side exit',spot.scaffold.exit))return null
   // From the safe side exit, clear only the body's feet and head cells.
   // Higher trunk blocks can be hidden by the canopy (birch at y+2 is the
   // common case); enter this verified two-cell opening and clear upward from
   // inside the column as the pillar rises.
   if(!await clearTrunk(api,r,base,base.y+1,report))return null
  }
  if(!await gotoRecoverable(api,{...base,range:0,dig:false,into:true},report))return null
  if(!await settledFeet(api,report,'temporary pillar base',base))return null
  if(!near(api,base)){stop(report,`did not reach temporary pillar base ${key(base)}`);return null}
  }
  if(!spot.scaffold)return spot
  while(api.pos().y<spot.y-.25){
   await api.checkpoint?.()
  if(!await settledFeet(api,report,'before temporary pillar ascent'))return null
  const groundedPos=api.pos()
  if(Math.floor(groundedPos.x)!==spot.x||Math.floor(groundedPos.z)!==spot.z){stop(report,`temporary pillar ascent left its column at ${key(groundedPos)}; supports retained`);return null}
  const p={x:spot.x,y:Math.floor(groundedPos.y),z:spot.z}
  if(!await clearTrunk(api,r,p,p.y+3,report))return null
  if(!await settledFeet(api,report,'before temporary pillar ascent',{...p,y:p.y}))return null
  const item=Object.keys(api.inv()).find(n=>pillarMaterial(n)&&api.inv()[n]>0)
  if(!item){stop(report,'no temporary pillar blocks carried; supply dirt or wood');return null}
  if(![0,1,2,3].every(dy=>isAir(api.block(p.x,p.y+dy,p.z)?.name))){stop(report,`temporary pillar headroom changed at ${key(p)}`);return null}
  const cell={...p,item,placementIntent:'air-before-pillar_up'}
  r.cells.push(cell);save(api,r,report)
  let result,completed=false,failure,deferred=false
  try {result=await api.act('pillar_up',{steps:1,item});completed=true}
  catch(e){
   failure=e
   if(e.message==='pillar_up requires grounded feet and no vehicle'||/(?:^|\/)pillar_up: pillar_up requires grounded feet and no vehicle$/.test(e.message)){
    api.acknowledgeFailure?.('pillar_up')
    stop(report,`temporary pillar ascent lost grounded footing at ${key(p)}; retry after landing; support intent retained`)
    deferred=true
   }else throw e
  }
  finally{
   const observed=api.block(p.x,p.y,p.z)
   cell.lastAttempt={completed,observed:observed?.name??null,feet:api.pos(),inventory:api.inv()[item]??0,...(failure?{error:failure.message}:{})}
   if(completed&&observed?.name===item&&safeFullBlock(observed)){
    cell.provenance='pillar_up-confirmed'
    cell.ascent={feet:api.pos(),result}
    if(!r.verified.includes(key(p)))r.verified.push(key(p))
   }
   save(api,r,report)
  }
  if(deferred)return null
  if(!await settledFeet(api,report,'after temporary pillar ascent',{...p,y:p.y+1}))return null
  if(!r.verified.includes(key(p))||!near(api,{...p,y:p.y+1})){
   const feet=api.pos()
   stop(report,`temporary pillar ascent did not converge at ${key(p)} (feet ${key(feet)}); ${r.verified.includes(key(p))?'placed support retained with confirmed provenance':'placement remains unverified'}`)
   return null
  }
 }
 if(!await clearTrunk(api,r,spot,spot.y+5,report))return null
 const support={x:spot.x,y:spot.y-1,z:spot.z},owned=r.cells.find(p=>key(p)===key(support))
 if(!owned||!r.verified.includes(key(support))){stop(report,`temporary pillar support is not verified at ${key(support)}`);return null}
 await api.checkpoint?.()
 if(!await settledFeet(api,report,'before centering temporary pillar platform',spot))return null
 await api.act('center_work_stand',{x:spot.x,y:spot.y,z:spot.z,support:owned.item})
 const centered=api.pos()
 if(!near(api,spot)||Math.hypot(centered.x-(spot.x+.5),centered.z-(spot.z+.5))>.05){stop(report,`temporary pillar work platform could not be centered at ${key(spot)}`);return null}
 return spot
}

export async function reachPillar(api,spots,report,r){
 recoverPillarIntent(api,r,report)
 const pos=api.pos(),cost=p=>Math.hypot(p.x+.5-pos.x,p.z+.5-pos.z)*3+Math.abs(p.y-pos.y)
 const ordered=[...spots].sort((a,b)=>cost(a)-cost(b)),failures=[]
 for(const spot of ordered.slice(0,3)){
  const local={attention:[]}
  const reached=await reachPillarSpot(api,[spot],local,r).catch(recoverFarm(e=>{local.attention.push(e.message);return null}))
  report.cleanup_left=local.cleanup_left??report.cleanup_left
  if(reached)return reached
  failures.push(...local.attention)
  // A partial ascent must be recovered, never bypassed to another column.
  if(r.cells.length)break
 }
 report.attention.push(...new Set(failures.length?failures:['no verified temporary pillar work platform is reachable']))
 return null
}

async function clearTrunk(api,r,base,top,report){
 const wood=r.clearance.filter(p=>p.x===base.x&&p.z===base.z&&p.y<=top).sort((a,b)=>b.y-a.y)
 const pending=wood.filter(p=>!isAir(api.block(p.x,p.y,p.z)?.name)&&!r.cells.some(q=>key(q)===key(p)))
 while(pending.length){
  await api.checkpoint?.()
  const index=pending.findIndex(p=>visibleTreeBlock(api.block,api.pos(),p))
  if(index<0){
   const trace=traceVisibleTreeBlock(api.block,api.pos(),pending[0])
   const hit=trace.tooFar?'target is beyond 4.5-block reach':trace.obstruction?`center-ray obstruction ${trace.obstruction.name}@${key(trace.obstruction)}`:'center-ray found no reachable target'
   return stop(report,`trunk wood at ${key(pending[0])} is not visibly reachable from feet ${key(api.pos())} (eye ${trace.eye.x.toFixed(2)},${trace.eye.y.toFixed(2)},${trace.eye.z.toFixed(2)}; ${hit}); leaves retained`)
  }
  const [p]=pending.splice(index,1)
  if(api.block(p.x,p.y,p.z)?.name!==p.name)return stop(report,`tree changed at ${key(p)}; trunk climb stopped`)
  if(inspectTree(api.block,r.root,r.species,r.form).protected.length)return stop(report,'protected nest/hive/heart appeared during trunk climb')
  await api.act('dig',{x:p.x,y:p.y,z:p.z,batch:true})
  if(!isAir(api.block(p.x,p.y,p.z)?.name))return stop(report,`trunk clearance unverified at ${key(p)}`)
 }
 return true
}
