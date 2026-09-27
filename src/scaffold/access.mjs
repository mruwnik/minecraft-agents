import { treeHarvestSequence } from './reach.mjs'
import { isAir } from '../lib/world.mjs'
import { digFromHere } from '../lib/dig.mjs'
import { foreignZone } from '../../library/farm/shared/clutter.mjs'
import { recoverFarm } from '../farm/attention.mjs'
const key = p => `${p.x},${p.y},${p.z}`
export const scaffoldId = root => `tree_${root.x}_${root.y}_${root.z}`.replaceAll('-', 'n')
const maxBlocks = 256
const ground = b => b?.solid && (!b.shapes || b.shapes.some(s => s.join(',') === '0,0,0,1,1,1')) && !/farmland|water|lava|_leaves$|_log$|_roots$|scaffolding|composter|_fence$|_wall$|_gate$/.test(b.name)
const air = (api,p) => isAir(api.block(p.x,p.y,p.z)?.name)
const center = p => ({x:p.x+.5,y:p.y,z:p.z+.5})
const arrived = (api,p) => Math.hypot(api.pos().x-(p.x+.5),api.pos().z-(p.z+.5))<=.7 && Math.abs(api.pos().y-p.y)<=.25
const cellsOf = c => Array.from({length:c.top-c.y+1},(_,i)=>({x:c.x,y:c.y+i,z:c.z}))
const decksOf = c => cellsOf(c).map(p=>({...p,y:p.y+1}))
const missing = (api,record) => record.cells.filter(p=>api.block(p.x,p.y,p.z)?.name==='scaffolding' || !api.block(p.x,p.y,p.z))
const publish = (api,record,report) => {
  api.scaffolds(record.id,record)
  report.cleanup_left=missing(api,record).map(p=>({...p}))
  api.report?.(report)
}
const attempt = (api,name,args,report) => api.act(name,args).then(()=>true,recoverFarm(e=>{report.attention.push(e.message);return false}))

// Rungs are dependency-ordered horizontal runs, at most six cells from a
// grounded column. Three-block vertical separation preserves two clear head cells.
function platformsFor(api,column,root,zones) {
  const result=[],levels=[]
  for(let y=column.top;y>=column.y;y-=3)levels.push(y)
  for(const y of levels){
    let from={x:column.x,y,z:column.z}
    for(let distance=1;distance<=6;distance++){
      const dx=root.x-from.x,dz=root.z-from.z
      if(!dx&&!dz)break
      const p={x:from.x+(Math.abs(dx)>=Math.abs(dz)?Math.sign(dx):0),y,z:from.z+(Math.abs(dx)<Math.abs(dz)?Math.sign(dz):0)}
      if(![0,1,2].every(dy=>air(api,{...p,y:y+dy}))||foreignZone(zones,api.me?.(),p))break
      result.push({...p,from:{...from},distance,column:key(column)});from=p
    }
  }
  return result
}
const platformRoute=(record,p)=>{
 const route=[],seen=new Set();let q=p
 while(q?.from&&!seen.has(key(q))){seen.add(key(q));route.unshift({...q,y:q.y+1});q=record.platforms?.find(v=>key(v)===key(q.from))??q.from}
 if(q)route.unshift({x:q.x,y:q.y+1,z:q.z})
 return route
}
const allDecks=record=>[
 ...record.columns.flatMap(c=>decksOf(c).map(p=>({...p,scaffold:c}))),
 ...(record.platforms??[]).map(p=>({x:p.x,y:p.y+1,z:p.z,scaffold:record.columns.find(c=>key(c)===p.column),route:platformRoute(record,p)}))
]

// Greedy bounded column cover. Every column starts on verified dry ground, is
// entirely air before building, and covers work from independent scaffold decks.
export function planScaffoldAccess (api, tree, access, zones=[]) {
  const need=tree.blocks.filter(b=>!access.get(key(b))?.length)
  if(!need.length)return {columns:[],count:0,attention:[]}
  if(!api.navigationCapabilities?.().scaffolding)return {columns:[],count:0,attention:['whole-tree access missing; this body has no verified scaffolding climb/descent capability']}
  const root=tree.root, radius=tree.profile.radius+3
  const candidates=[]
  const highest=Math.max(...tree.blocks.map(b=>b.y))
  for(let x=root.x-radius;x<=root.x+radius+tree.profile.width-1;x++)for(let z=root.z-radius;z<=root.z+radius+tree.profile.width-1;z++){
    if(!ground(api.block(x,root.y,z)))continue
    const y=root.y+1
    const exit=[[-1,0],[1,0],[0,-1],[0,1]].map(([dx,dz])=>({x:x+dx,y,z:z+dz})).find(p=>ground(api.block(p.x,p.y-1,p.z))&&air(api,p)&&air(api,{...p,y:p.y+1})&&!foreignZone(zones,api.me?.(),p))
    if(!exit)continue
    let clearTo=y-1
    for(let h=y;h<=Math.min(root.y+48,highest+3);h++){
      const p={x,y:h,z}
      if(!air(api,p)||foreignZone(zones,api.me?.(),p))break
      clearTo=h
    }
    // Two clear cells above the highest deck are required for the climbing body.
    const top=Math.min(clearTo-2,highest+1)
    if(top<y)continue
    const column={x,y,z,top,exit}
    const platforms=platformsFor(api,column,root,zones)
    const decks=[...decksOf(column),...platforms.map(p=>({...p,y:p.y+1}))]
    const covers=need.filter(b=>decks.some(d=>digFromHere(center(d),b)))
    if(covers.length)candidates.push({...column,platforms,covers})
  }
  const left=new Set(need.map(key)), columns=[], platforms=[]
  while(left.size&&columns.length<12){
    const best=candidates.map(c=>({...c,score:c.covers.filter(b=>left.has(key(b))).length/(c.top-c.y+1+c.platforms.length)})).sort((a,b)=>b.score-a.score||a.top-b.top)[0]
    if(!best||!best.score)break
    columns.push({x:best.x,y:best.y,z:best.z,top:best.top,exit:best.exit})
    platforms.push(...best.platforms)
    for(const b of best.covers)left.delete(key(b))
    for (let i=candidates.length-1;i>=0;i--) { const c=candidates[i]; if ((c.x===best.x&&c.z===best.z)||(c.x===best.exit.x&&c.z===best.exit.z)||(c.exit.x===best.x&&c.exit.z===best.z)||[...cellsOf(c),...c.platforms].some(p=>[...cellsOf(best),...best.platforms].some(q=>p.x===q.x&&p.z===q.z&&Math.abs(p.y-q.y)<=2))) candidates.splice(i,1) }
  }
  const count=columns.reduce((n,c)=>n+c.top-c.y+1,0)+platforms.length
  const attention=[]
  if(left.size)attention.push(`whole-tree access missing for ${left.size} blocks (${[...left].slice(0,8).join(' ')}); no clear safe scaffold columns cover them`)
  if(count>maxBlocks)attention.push(`scaffold plan requires ${count} blocks, above bounded limit ${maxBlocks}; provide existing access`)
  if(!left.size&&count<=maxBlocks){
    const cells=[...columns.flatMap(cellsOf),...platforms],set=new Set(cells.map(key))
    const virtual=(x,y,z)=>set.has(`${x},${y},${z}`)?{name:'scaffolding',solid:false}:api.block(x,y,z)
    const planned=new Map([...access].map(([k,v])=>[k,[...v]]))
    addScaffoldAccess(planned,tree,{columns,platforms,verified:[...set]})
    const sequence=treeHarvestSequence(tree,planned,virtual)
    if(sequence.missing.length)attention.push(`whole-tree visible access missing for ${sequence.missing.length} blocks; provide a different platform before any tree is cut`)
  }
  if((api.inv().scaffolding??0)<count)attention.push(`whole-tree access missing: need ${count} scaffolding, carry ${api.inv().scaffolding??0}; supply ${Math.max(0,count-(api.inv().scaffolding??0))} before any tree is cut`)
  return {columns,platforms,count,attention}
}

export async function cleanupScaffold (api,id,report={attention:[]}) {
  const record=api.scaffolds?.(id)
  if(!record)return {cleanup_left:[],attention:report.attention}
  report.cleanup_left=missing(api,record)
  api.report?.(report)
  try {
    const zones=(await api.act('zones')).zones??[]
    const foreign=record.cells.find(p=>foreignZone(zones,api.me?.(),p))
    if(foreign)throw new Error(`protected zone at ${key(foreign)} prevents scaffold cleanup`)
    // Lateral decks can rest on canopy and survive loss of the column. Remove
    // each recorded side cell from its still-intact parent, never underfoot.
    for(const p of [...(record.platforms??[])].reverse()){
      if(api.block(p.x,p.y,p.z)?.name!=='scaffolding')continue
      if(!record.verified.includes(key(p))){report.attention.push(`unverified scaffold platform at ${key(p)}; inspect ownership before cleanup`);return report}
      const attached=[[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]].map(([dx,dy,dz])=>({x:p.x+dx,y:p.y+dy,z:p.z+dz})).find(q=>api.block(q.x,q.y,q.z)?.name==='scaffolding'&&!record.verified.includes(key(q)))
      if(attached){report.attention.push(`unrecorded scaffold connects at ${key(attached)}; retain supports until its ownership is resolved`);return report}
      if([p,...(record.platforms??[])].some(q=>api.scaffoldOccupied?.({...q,top:q.y}))){report.attention.push(`another entity occupies scaffold platform; cleanup waits`);return report}
      if(api.block(p.from.x,p.from.y,p.from.z)?.name!=='scaffolding'||!record.verified.includes(key(p.from))){report.attention.push(`scaffold platform support changed at ${key(p.from)}; retain suspended remainder`);return report}
      const parent=record.platforms.find(q=>key(q)===key(p.from)),column=record.columns.find(c=>key(c)===p.column)
      const spot={...p.from,y:p.from.y+1,scaffold:column,...(parent?{route:platformRoute(record,parent)}:{})}
      if(!await reachTreePlatform(api,[spot],report,record))return report
      if(Math.floor(api.pos().x)===p.x&&Math.floor(api.pos().z)===p.z){report.attention.push(`cannot remove occupied scaffold platform ${key(p)}`);return report}
      if(!await attempt(api,'dig',{x:p.x,y:p.y,z:p.z,batch:true},report))return report
      if(api.block(p.x,p.y,p.z)?.name==='scaffolding'){report.attention.push(`scaffold platform removal unverified at ${key(p)}`);return report}
      publish(api,record,report)
    }
    for(const c of [...record.columns].reverse()){
      const owned=[...cellsOf(c),...(record.platforms??[]).filter(p=>p.column===key(c))].filter(p=>record.cells.some(q=>key(p)===key(q)))
      const standing=owned.filter(p=>api.block(p.x,p.y,p.z)?.name==='scaffolding')
      if(!standing.length){
        if(owned.some(p=>!api.block(p.x,p.y,p.z)))report.attention.push(`scaffold cleanup unloaded at ${key(c)}; load it before cleanup`)
        continue
      }
      if(standing.some(p=>!record.verified.includes(key(p)))){report.attention.push(`unverified scaffold placement at ${key(c)}; inspect ownership before cleanup`);continue}
      const foreignScaffold=standing.flatMap(p=>[[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]].map(([dx,dy,dz])=>({x:p.x+dx,y:p.y+dy,z:p.z+dz}))).find(p=>api.block(p.x,p.y,p.z)?.name==='scaffolding'&&!record.verified.includes(key(p)))
      if(foreignScaffold){report.attention.push(`unrecorded scaffold connects at ${key(foreignScaffold)}; retain supports until its ownership is resolved`);continue}
      if([c,...(record.platforms??[]).map(p=>({...p,top:p.y}))].some(p=>api.scaffoldOccupied?.(p))){report.attention.push(`another entity occupies scaffold ${key(c)}; cleanup waits`);continue}
      if(!ground(api.block(c.exit.x,c.exit.y-1,c.exit.z))||!air(api,c.exit)||!air(api,{...c.exit,y:c.exit.y+1})){report.attention.push(`safe scaffold exit changed at ${key(c.exit)}`);continue}
      await api.checkpoint?.()
      // Descend while every support is intact. No support is dug under a body.
      if(!await leaveScaffold(api,record,report))continue
      if(!await attempt(api,'goto',{...c.exit,range:0,dig:false,into:true},report))continue
      if(!arrived(api,c.exit)){report.attention.push(`scaffold descent unverified; reach ${key(c.exit)} before cleanup`);continue}
      const base={x:c.x,y:c.y,z:c.z}
      if(!record.verified.includes(key(base))||api.block(base.x,base.y,base.z)?.name!=='scaffolding'){report.attention.push(`scaffold base missing or changed at ${key(base)}; suspended remainder retained`);continue}
      if([c,...(record.platforms??[]).map(p=>({...p,top:p.y}))].some(p=>api.scaffoldOccupied?.(p))){report.attention.push(`another entity entered scaffold ${key(c)}; cleanup waits`);continue}
      if(!await attempt(api,'dig',{...base,batch:true},report))continue
      await api.pause?.(.6)
      const remains=owned.filter(p=>api.block(p.x,p.y,p.z)?.name==='scaffolding'||!api.block(p.x,p.y,p.z))
      if(remains.length)report.attention.push(`scaffold collapse incomplete: ${remains.map(key).join(' ')}`)
      publish(api,record,report)
    }
  } finally {
    publish(api,record,report)
    if(!report.cleanup_left.length)api.scaffolds(id,null)
  }
  return {cleanup_left:report.cleanup_left,attention:report.attention}
}

export async function buildScaffoldAccess (api,tree,plan,report) {
  if(plan.attention.length){report.attention.push(...plan.attention);return null}
  if(!plan.columns.length)return null
  if(!api.scaffolds)throw new Error('scaffold provenance storage is unavailable')
  const id=scaffoldId(tree.root)
  if(api.scaffolds(id))throw new Error('existing scaffold journal requires cleanup before building')
  const record={id,root:tree.root,columns:plan.columns,platforms:plan.platforms??[],cells:[],verified:[]}
  api.scaffolds(id,record)
  try {
    for(const c of plan.columns){
      if(!await leaveScaffold(api,record,report))return record
      if(!await attempt(api,'goto',{...c.exit,range:0,dig:false,into:true},report))return record
      if(!arrived(api,c.exit))await api.pause?.(.2)
      if(!arrived(api,c.exit)){report.attention.push(`scaffold starting position unverified at ${key(c.exit)}`);return record}
      for(const p of cellsOf(c)){
        await api.checkpoint?.()
        if(!air(api,p)){report.attention.push(`scaffold column changed at ${key(p)}; retained for inspection`);return record}
        const before=api.inv().scaffolding??0
        record.cells.push(p);publish(api,record,report)
        let ok=false
        try {ok=await attempt(api,p.y===c.y?'place':'scaffold_extend',p.y===c.y?{...p,item:'scaffolding'}:{...p,base_y:c.y},report)} finally {
          if(api.block(p.x,p.y,p.z)?.name==='scaffolding'&&(api.inv().scaffolding??0)<before)record.verified.push(key(p))
          publish(api,record,report)
        }
        if(!ok||!record.verified.includes(key(p))){report.attention.push(`scaffold placement not verified at ${key(p)}`);return record}
        // Build upward from the ground by clicking the owned base's side.
        // Harvest verifies each climb before any dig; no per-block deck hopping.
      }
    }
    for(const p of record.platforms){
      await api.checkpoint?.()
      const c=record.columns.find(c=>key(c)===p.column)
      const parent=record.platforms.find(q=>key(q)===key(p.from))
      const spot={...p.from,y:p.from.y+1,scaffold:c,...(parent?{route:platformRoute(record,parent)}:{})}
      if(!await reachTreePlatform(api,[spot],report,record))return record
      if(!air(api,p)){report.attention.push(`scaffold platform changed at ${key(p)}`);return record}
      const before=api.inv().scaffolding??0
      record.cells.push({x:p.x,y:p.y,z:p.z});publish(api,record,report)
      let ok=false
      try {ok=await attempt(api,'scaffold_side',{x:p.x,y:p.y,z:p.z,from_x:p.from.x,from_y:p.from.y,from_z:p.from.z},report)}finally{
        if(api.block(p.x,p.y,p.z)?.name==='scaffolding'&&(api.inv().scaffolding??0)<before)record.verified.push(key(p))
        publish(api,record,report)
      }
      if(!ok||!record.verified.includes(key(p))){report.attention.push(`scaffold platform placement not verified at ${key(p)}`);return record}
    }
  } finally {publish(api,record,report)}
  return record
}
export function addScaffoldAccess (access,tree,record) {
  if(!record)return access
  for(const b of tree.blocks){
    const decks=allDecks(record).filter(d=>record.verified.includes(`${d.x},${d.y-1},${d.z}`)&&digFromHere(center(d),b))
    access.set(key(b),[...(access.get(key(b))??[]),...decks].sort((a,c)=>Math.hypot(a.x-b.x,a.z-b.z)-Math.hypot(c.x-b.x,c.z-b.z)||c.y-a.y))
  }
  return access
}

const near=(p,q)=>Math.floor(q.x)===p.x&&Math.floor(q.z)===p.z&&Math.hypot(p.x+.5-q.x,p.z+.5-q.z)<=.8&&Math.abs(p.y-q.y)<=.6
async function follow(api,goals,report){
 for(const p of goals){
  if(near(p,api.pos()))continue
  await api.checkpoint?.()
  if(!await attempt(api,'goto',{x:p.x,y:p.y,z:p.z,range:0,dig:false,into:true},report))return false
  if(!near(p,api.pos()))await api.pause?.(.2)
  if(!near(p,api.pos())){report.attention.push(`did not reach verified work platform ${key(p)} (at ${key(api.pos())})`);return false}
 }
 return true
}
// Retrace a supported side deck before descending its own vertical column.
// Never route diagonally from a high platform to a different column's dry exit.
export async function leaveScaffold(api,record,report){
 const pos=api.pos(),deck=allDecks(record).find(d=>near(d,pos))
 const column=deck?.scaffold??record.columns.find(c=>Math.floor(pos.x)===c.x&&Math.floor(pos.z)===c.z&&pos.y>c.y+.6)
 if(!column)return true
 const back=deck?.route?[...deck.route].reverse():[]
 return follow(api,[...back,{x:column.x,y:column.y,z:column.z},column.exit],report)
}
export async function reachTreePlatform(api,spots,report,structure=[]){
 const record=Array.isArray(structure)?{columns:structure,platforms:[]}:structure
 const current=spots.find(p=>near(p,api.pos()))
 if(current)return current
 const ordered=[...spots].sort((a,b)=>{
  const pos=api.pos(),cost=p=>Math.hypot(p.x+.5-pos.x,p.z+.5-pos.z)+Math.abs(p.y-pos.y)
  return cost(a)-cost(b)
 })
 const failures=[]
 for(const spot of ordered.slice(0,3)){
  const local={attention:[]},column=spot.scaffold,goals=[]
  const pos=api.pos(),inside=column&&Math.floor(pos.x)===column.x&&Math.floor(pos.z)===column.z
  const from=allDecks(record).find(d=>near(d,pos)),same=column&&from?.scaffold&&key(column)===key(from.scaffold)
  if(same&&from.route){
    const previous=from.route,next=spot.route??[spot]
    let common=0;while(common<previous.length&&common<next.length&&key(previous[common])===key(next[common]))common++
    // Return only to the common supported junction, not needlessly to ground.
    goals.push(...previous.slice(Math.max(0,common-1)).reverse(),...next.slice(common),spot)
  }else{
    if(!inside&&!await leaveScaffold(api,record,local)){failures.push(...local.attention);continue}
    if(column&&!inside)goals.push(column.exit,{x:column.x,y:column.y,z:column.z})
    goals.push(...(spot.route??[]),spot)
  }
  if(await follow(api,goals,local))return spot
  failures.push(...local.attention)
 }
 report.attention.push(...new Set(failures.length?failures:['no verified tree work platform is reachable']))
 return null
}
