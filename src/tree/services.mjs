// Planned forestry services only. Never infer ownership from a nearby container.
import { planSpec } from '../lib/plan.mjs'
import { isAir } from '../lib/world.mjs'
import { dryStandable } from '../navigation/walk.mjs'
import { cellOf } from '../farm/field.mjs'
import { recoverFarm } from '../farm/attention.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
const key=p=>`${p.x},${p.y},${p.z}`
const soil=new Set(['dirt','grass_block'])
export async function maintainTreeServices(api,cells,report){
 const services=cells.filter(c=>['flower','chest','composter','table','fence','gate','block','torch'].includes(planSpec(c)?.kind)).flatMap(c=>planSpec(c).kind==='torch'?[c,{...c,y:c.y+1,spec:{kind:'block',item:'torch'},torchTop:true}]:[c])
 if(!services.length)return
 const zones=(await api.act('zones')).zones??[]
 const attempt=async(name,args)=>api.act(name,args).then(()=>true,recoverFarm(e=>{report.attention.push(e.message);return false}))
 for(const c of services){
  await api.checkpoint()
  const spec=planSpec(c),at={x:c.x,y:c.y+1,z:c.z},ground={x:c.x,y:c.y,z:c.z}
  const read=p=>api.block(p.x,p.y,p.z)
  let top=read(at),floor=read(ground)
  if(!top||!floor){
   if(!await attempt('goto',{...at,range:3,dig:false}))continue
   top=read(at);floor=read(ground)
  }
  if(!top||!floor){report.attention.push(`planned ${spec.kind} unloaded at ${key(at)}`);continue}
  if(top.name===spec.item)continue
  if(!isAir(top.name)){report.attention.push(`planned ${spec.kind} blocked by ${top.name} at ${key(at)}; retained`);continue}
  const flower=spec.kind==='flower',repair=flower&&!soil.has(floor.name)||isAir(floor.name)
  if(c.torchTop&&!/_fence$/.test(floor.name)){report.attention.push(`torch waits for planned post at ${key(ground)}`);continue}
  if(repair){
   if(!isAir(floor.name)&&floor.name!=='dirt_path'){report.attention.push(`flower/support needs suitable soil at ${key(ground)}; retained ${floor.name}`);continue}
   if(!(api.inv().dirt>0)){report.attention.push(`missing dirt to repair planned ${spec.kind} support at ${key(ground)}`);continue}
   if([ground,at].some(p=>foreignZone(zones,api.me?.(),p)))throw Error(`protected zone at ${key(ground)} does not invite forestry service work`)
   // Stand beside the support before removing it; never dig beneath the body.
   const spots=[[1,0],[-1,0],[0,1],[0,-1]].map(([dx,dz])=>({x:c.x+dx,y:c.y+1,z:c.z+dz})).filter(p=>dryStandable((x,y,z)=>cellOf(api.block(x,y,z)),p))
   if(!spots.length){report.attention.push(`no safe standing cell to repair support at ${key(ground)}`);continue}
   if(!await attempt('goto',{...spots[0],range:0,dig:false}))continue
   const pos=api.pos()
   if(Math.hypot(pos.x-spots[0].x-.5,pos.z-spots[0].z-.5)>.8||Math.abs(pos.y-spots[0].y)>.6||Math.floor(pos.x)===c.x&&Math.floor(pos.z)===c.z){report.attention.push(`cannot repair support under body at ${key(ground)}`);continue}
   await api.checkpoint()
   if(read(at)?.name!==top.name||read(ground)?.name!==floor.name){report.attention.push(`service site changed at ${key(at)}; retry next sweep`);continue}
   if(!isAir(floor.name)&&!await attempt('dig',ground))continue
   if(!await attempt('place',{...ground,item:'dirt'}))continue
   floor=read(ground)
   if(!soil.has(floor?.name)){report.attention.push(`support repair unverified at ${key(ground)}`);continue}
   report.support_repaired=(report.support_repaired??0)+1
  }
  if(!floor.solid){report.attention.push(`planned ${spec.kind} lacks solid support at ${key(ground)}`);continue}
  if(!(api.inv()[spec.item]>0)){report.attention.push(`missing ${spec.item} for planned service at ${key(at)}`);continue}
  if(foreignZone(zones,api.me?.(),at))throw Error(`protected zone at ${key(at)} does not invite forestry service work`)
  await api.checkpoint()
  if(!isAir(read(at)?.name)){report.attention.push(`service site changed at ${key(at)}; retained`);continue}
  if(!await attempt('place',{...at,item:spec.item}))continue
  if(read(at)?.name!==spec.item)report.attention.push(`planned ${spec.kind} placement unverified at ${key(at)}`)
  else report.services_placed=(report.services_placed??0)+1
 }
}
