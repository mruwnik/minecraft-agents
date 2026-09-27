import { planSpec } from '../lib/plan.mjs'
import { planChests, depositTarget, STORAGE_BLOCKS } from '../lib/storage.mjs'
import { workRefusal } from '../lib/places.mjs'
import { recoverFarm } from '../farm/attention.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
import { treeSpec, checkTree, harvestStands } from './inspect.mjs'
import { treeProfile } from './profiles.mjs'
import { planScaffoldAccess } from '../scaffold/access.mjs'
const key=p=>`${p.x},${p.y},${p.z}`

// One bounded read per configured chest per sweep; every withdrawal is limited
// by that snapshot and the current inventory deficit. Never search nearby stores.
export async function treeSupplies(api,plan,source,report){
 const target=depositTarget(source,api.places())
 if(target?.error)throw Error(target.error)
 let chests=[]
 if(target?.kind==='plan')chests=planChests(plan.cells)
 else if(target?.kind==='cell')chests=[target]
 else if(target?.kind==='place'){
  const place=api.places().find(p=>p.name===target.name),refusal=workRefusal(place,api.me?.())
  if(refusal)throw Error(refusal)
  chests=[target]
 }
 const stocks=[]
 const attempt=async(name,args)=>api.act(name,args).catch(recoverFarm(e=>{report.attention.push(e.message);return null}))
 const zones=chests.length?(await api.act('zones')).zones??[]:[]
 for(const at of chests){
  if(foreignZone(zones,api.me?.(),at))throw Error(`protected zone at ${key(at)} does not invite forestry supply work`)
  const block=api.block(at.x,at.y,at.z)
  if(block&&!STORAGE_BLOCKS.includes(block.name))continue
  await api.checkpoint()
  const result=await attempt('chest_contents',{x:at.x,y:at.y,z:at.z})
  if(result)stocks.push({at,items:{...(result.items??{})}})
 }
 const take=async wanted=>{
  for(const stock of stocks){
   const items=Object.fromEntries(Object.entries(wanted).map(([name,n])=>[name,Math.min(stock.items[name]??0,Math.max(0,n-(api.inv()[name]??0)))]).filter(([,n])=>n>0))
   if(!Object.keys(items).length)continue
   await api.checkpoint()
   await attempt('withdraw',{x:stock.at.x,y:stock.at.y,z:stock.at.z,items})
   // A partial/failing server transfer is never optimistically counted, nor
   // requested again from the stale snapshot in this sweep.
   for(const [name,n]of Object.entries(items))stock.items[name]-=n
  }
 }
 const available=name=>(api.inv()[name]??0)+stocks.reduce((n,s)=>n+(s.items[name]??0),0)
 const names=()=>[...new Set([...Object.keys(api.inv()),...stocks.flatMap(s=>Object.keys(s.items))])]
 const craft=async(item,count)=>{
  const before=api.inv()[item]??0
  await api.checkpoint();await attempt('craft',{item,count})
  const gained=Math.max(0,(api.inv()[item]??0)-before)
  if(gained<count)report.attention.push(`craft ${item} produced ${gained}/${count}; inspect supplied materials`)
  return gained
 }
 return{take,available,names,craft}
}

export async function provisionTreeBasics(api,plan,supply,report){
 const want={}
 const add=(item,n=1)=>{want[item]=(want[item]??0)+n}
 for(const c of plan.cells){
  const s=planSpec(c),t=treeSpec(c)
  if(t){const p=treeProfile(t.species,t.form);add(p.plant,p.width**2);continue}
  if(s?.item&&api.block(c.x,c.y+1,c.z)?.name!==s.item)add(s.item)
  if(s?.kind==='torch'&&api.block(c.x,c.y+2,c.z)?.name!=='torch')add('torch')
  if(s?.kind==='flower'&&['dirt_path','air'].includes(api.block(c.x,c.y,c.z)?.name))add('dirt')
 }
 await supply.take(want)
 // Tables fit the inventory grid. Use available planks, or one available log;
 // preserve all planting items and never fell another tree to make the table.
 const tables=plan.cells.filter(c=>planSpec(c)?.kind==='table'&&api.block(c.x,c.y+1,c.z)?.name!=='crafting_table')
 const missing=Math.max(0,tables.length-(api.inv().crafting_table??0))
 for(let i=0;i<missing;i++){
  let planks=supply.names().find(n=>n.endsWith('_planks')&&supply.available(n)>=4)
  if(!planks){
   const log=supply.names().find(n=>n.endsWith('_log')&&supply.available(n)>0)
   if(log){await supply.take({[log]:1});if(api.inv()[log]>0){planks=log.replace(/_log$/,'_planks');await supply.craft(planks,4)}}
  }
  if(!planks){report.attention.push('missing crafting_table or 4 matching planks (or 1 log) for planned table');break}
  await supply.take({[planks]:4})
  if((api.inv()[planks]??0)<4)break
  await supply.craft('crafting_table',1)
 }
}

export async function provisionTreeScaffold(api,plan,cell,supply,report){
 const s=treeSpec(cell),tree=checkTree(api.block,cell,s.species,s.form,plan.cells)
 if(tree.attention.length||tree.state!=='mature')return
 const zones=(await api.act('zones')).zones??[]
 const access=planScaffoldAccess(api,tree,harvestStands(tree,api.block),zones)
 if(!access.count||access.count>256||access.attention.some(s=>!s.startsWith('whole-tree access missing: need ')))return
 await supply.take({scaffolding:access.count})
 const short=Math.max(0,access.count-(api.inv().scaffolding??0))
 if(!short)return
 const batches=Math.ceil(short/6),bamboo=6*batches,string=batches
 await supply.take({bamboo,string})
 const lack=[['bamboo',bamboo],['string',string]].filter(([n,v])=>(api.inv()[n]??0)<v).map(([n,v])=>`${v-(api.inv()[n]??0)} ${n}`)
 if(lack.length){report.attention.push(`need ${short} scaffolding: missing ${lack.join(' and ')} to craft ${batches*6}`);return}
 const table=plan.cells.find(c=>planSpec(c)?.kind==='table'&&api.block(c.x,c.y+1,c.z)?.name==='crafting_table')
 if(!table){report.attention.push(`need ${short} scaffolding: provide a planned crafting_table with safe support`);return}
 await api.checkpoint()
 const arrived=await api.act('goto',{x:table.x,y:table.y+1,z:table.z,range:2,dig:false}).then(()=>true,recoverFarm(e=>{report.attention.push(e.message);return false}))
 if(arrived)await supply.craft('scaffolding',batches*6)
}
