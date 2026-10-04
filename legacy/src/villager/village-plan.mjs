import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { readBlueprintSource } from '../blueprint/source.mjs'
import { BLUEPRINT_DIR } from '../blueprint/build.mjs'
import { readBlueprintManifest } from '../blueprint/manifest.mjs'
import { compileBlueprintStructure, concreteBlueprint } from '../blueprint/compiler.mjs'
import { materialCandidates } from '../blueprint/materials.mjs'
import { rotate, turnsFor, blueprintCells, matchesCell, stateOf, REGISTRY } from '../blueprint/format.mjs'
import { rotateBlueprintPosition } from '../blueprint/transform.mjs'
import { validatePopulation, populationWorkspaces } from './population.mjs'
import { safeFullBlock } from '../enclosure/blocks.mjs'
import { readVillageInspection } from './inspection.mjs'
import { canonicalBlueprint } from '../blueprint/schema.mjs'
import { existingObjects } from '../blueprint/v2.mjs'
export const VILLAGER_ROSTER_FILE=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../../../state/villagers.json')
export function villagePlan(api,a,io={}) {
  const saved=api.places().find(p=>p.name===a.place), manifest=saved && readBlueprintManifest(saved.note,io.stateDir)
  if (a.place && !saved && a.name===undefined && a.plan===undefined) throw new Error(`no village blueprint place ${a.place}`)
  const supplied=a.name!==undefined||a.plan!==undefined?readBlueprintSource(a,BLUEPRINT_DIR).document:null
  const prior=saved && readVillageInspection(saved.name,manifest?.at ?? saved,io.inspectionDir)
  const source=supplied ?? (prior?.buildSourceHash===manifest?.sourceHash?prior?.source:null) ?? manifest?.source ?? readBlueprintSource(a,BLUEPRINT_DIR).document
  const ir=compileBlueprintStructure(source), intent=validatePopulation(source.population)
  if(manifest && supplied&&!compatibleVillageIntent(manifest.source,supplied))throw new Error('village physical source differs from saved blueprint snapshot; only declared additive workstations in authored air may change')
  const at=manifest?.at ?? (saved?{x:saved.x,y:saved.y,z:saved.z}:{x:a.x,y:a.y,z:a.z}), facing=manifest?.facing ?? a.facing ?? source.front ?? 'south'
  if (![at.x,at.y,at.z].every(Number.isInteger)) throw new Error('village needs integer blueprint anchor x/y/z')
  const turns=turnsFor(source.front ?? 'south',facing)
  const world=local=>{const [x,y,z]=rotateBlueprintPosition(local,ir.width,ir.depth,turns);return{x:at.x+x,y:at.y+y,z:at.z+z}}
  const compatible=manifest?{}:existingObjects(ir,at,facing,api.block ?? (()=>null),{strictMultipart:false})
  const assignments=manifest?.allocation.assignments ?? Object.fromEntries(ir.objects.map(o=>[o.id,compatible[o.id] ?? o.block ?? materialCandidates(source.materials[o.material])[0]]))
  const bp=rotate(concreteBlueprint(ir,assignments),turns)
  const cells=blueprintCells(bp).map(c=>({...c,x:at.x+c.dx,y:at.y+c.dy,z:at.z+c.dz}))
  const inside=p=>p && p.x>=at.x && p.x<at.x+bp.width && p.z>=at.z && p.z<at.z+bp.depth && p.y>=at.y-0.5 && p.y<at.y+ir.height
  const entrances=ir.objects.filter(o=>o.initialState.open===false).map(o=>world(o.footprint[0].at))
  return {source,ir,intent,at,facing,saved,manifest,bp,cells,world,inside,entrances}
}
export function villageStructure(plan,blockAt) {
  const missing=[],unknown=[],additions=[]
  for(const c of plan.cells) {const b=blockAt(c.x,c.y,c.z);if(!b)unknown.push(c);else if(!matchesCell(b,c.spec.alts)){
    const row={...c,actual:b.name}
    // Extra furniture is observed, never automatically demolished. Actual bed
    // headroom and aisle connectivity still decide whether it is harmless.
    if(c.spec.alts?.every(a=>a.name==='air'))additions.push(row);else missing.push(row)
  }}
  const beds=plan.ir.objects.filter(o=>o.type==='bed').map(o=>{
    const halves=o.footprint.map(c=>plan.world(c.at)),blocks=halves.map(p=>blockAt(p.x,p.y,p.z))
    const valid=blocks.every(b=>b?.name?.endsWith('_bed')) && blocks[0]?.name===blocks[1]?.name && halves.every((p,i)=>stateOf(blocks[i]).part===(i?'head':'foot') && [1,2].every(d=>['air','cave_air','void_air'].includes(blockAt(p.x,p.y+d,p.z)?.name)))
    return {id:o.id,halves,valid,reachability:'unknown'}
  })
  const openEntrances=plan.entrances.filter(p=>![false,'false'].includes(stateOf(blockAt(p.x,p.y,p.z)).open))
  return {missing,unknown,additions,beds,openEntrances,usableBeds:beds.filter(b=>b.valid).length,complete:!missing.length&&!unknown.length&&!openEntrances.length}
}

export function villageShelter(plan,blockAt) {
  const {width,depth}=plan.bp, height=plan.ir.height, at=plan.at, issues=[]
  if(height<4||width*depth*height>16384)return{status:'unknown',issues:['housing bounding volume must have three-block headroom and at most 16384 cells for bounded validation']}
  const roof=at.y+height-1, key=p=>`${p.x},${p.y},${p.z}`, gates=new Map(plan.entrances.map(p=>[key(p),p]))
  for(let x=at.x;x<at.x+width;x++)for(let z=at.z;z<at.z+depth;z++){
    if(!safeFullBlock(blockAt(x,at.y-1,z)))issues.push(`unsafe/unloaded floor ${x},${at.y-1},${z}`)
    if(!safeFullBlock(blockAt(x,roof,z)))issues.push(`unsafe/unloaded roof ${x},${roof},${z}`)
    if(x!==at.x&&x!==at.x+width-1&&z!==at.z&&z!==at.z+depth-1)continue
    for(let y=at.y;y<roof;y++){
      const g=gates.get(key({x,y,z})) ?? gates.get(key({x,y:y-1,z})),b=blockAt(x,y,z)
      if(g&&y===g.y){const s=stateOf(b),axis=x===at.x||x===at.x+width-1?['east','west']:['north','south'];if(!b?.name?.endsWith('_fence_gate')||!axis.includes(s.facing)||![false,'false'].includes(s.open))issues.push(`boundary gate is not closed on its wall axis ${x},${y},${z}`)}
      else if(g&&y===g.y+1){if(!['air','cave_air','void_air'].includes(b?.name))issues.push(`boundary gate head is obstructed ${x},${y},${z}`)}
      else if(!safeFullBlock(b))issues.push(`boundary is not a full safe wall ${x},${y},${z}`)
    }
  }
  // Understate light propagation: only known air and small nonopaque furniture
  // transmit it. Unknown/complex shapes never create a claimed light path.
  const pass=b=>['air','cave_air','void_air','torch','wall_torch','lantern','soul_lantern','end_rod'].includes(b?.name)||b?.name?.endsWith('_bed')||b?.name?.endsWith('_fence_gate')
  const light=new Map(),queue=[]
  for(let x=at.x+1;x<at.x+width-1;x++)for(let z=at.z+1;z<at.z+depth-1;z++)for(let y=at.y;y<roof;y++){const p={x,y,z},n=REGISTRY.blocksByName[blockAt(x,y,z)?.name]?.emitLight ?? 0;if(n){light.set(key(p),n);queue.push(p)}}
  for(let i=0;i<queue.length;i++){const p=queue[i],n=light.get(key(p))-1;if(n<=0)continue;for(const[dx,dy,dz]of[[1,0,0],[-1,0,0],[0,1,0],[0,-1,0],[0,0,1],[0,0,-1]]){const q={x:p.x+dx,y:p.y+dy,z:p.z+dz};if(q.x<=at.x||q.x>=at.x+width-1||q.z<=at.z||q.z>=at.z+depth-1||q.y<at.y||q.y>=roof||!pass(blockAt(q.x,q.y,q.z))||n<=(light.get(key(q))??0))continue;light.set(key(q),n);queue.push(q)}}
  let lightMinimum=15
  for(let x=at.x+1;x<at.x+width-1;x++)for(let z=at.z+1;z<at.z+depth-1;z++)for(let y=at.y;y<roof;y++)if(pass(blockAt(x,y,z))&&safeFullBlock(blockAt(x,y-1,z))){const n=light.get(key({x,y,z}))??0;lightMinimum=Math.min(lightMinimum,n);if(n<8)issues.push(`conservative interior light below 8 at ${x},${y},${z}`)}
  return{status:issues.length?'violated':'satisfied',issues:issues.slice(0,32),lightMinimum,lighting:'conservative emitted block-light propagation; no sky light or workstation inference'}
}

// Conservative two-block pedestrian connectivity over full supporting blocks.
// Beds are destinations, never used as required aisle footing. Partial stairs
// and unknown chunks do not create fictitious paths.
export function reachableVillageBeds(plan,structure,blockAt,residents=[]) {
  const clear=b=>['air','cave_air','void_air','torch','wall_torch'].includes(b?.name)
  const nodes=new Map(),key=p=>`${p.x},${p.y},${p.z}`
  for(let x=plan.at.x;x<plan.at.x+plan.bp.width;x++)for(let z=plan.at.z;z<plan.at.z+plan.bp.depth;z++)for(let y=plan.at.y;y<plan.at.y+plan.ir.height;y++)
    if(clear(blockAt(x,y,z))&&clear(blockAt(x,y+1,z))&&safeFullBlock(blockAt(x,y-1,z)))nodes.set(key({x,y,z}),{x,y,z})
  const destinations=bed=>[...nodes.values()].filter(p=>bed.halves.some(h=>p.y===h.y&&Math.abs(p.x-h.x)+Math.abs(p.z-h.z)===1))
  const flood=start=>{const reached=new Set(start.map(key)),queue=[...start];for(let i=0;i<queue.length;i++){const p=queue[i];for(const [dx,dz]of [[1,0],[-1,0],[0,1],[0,-1]])for(const dy of [-1,0,1]){const q={x:p.x+dx,y:p.y+dy,z:p.z+dz},k=key(q);if(nodes.has(k)&&!reached.has(k)&&clear(blockAt(p.x,Math.max(p.y,q.y)+1,p.z))){reached.add(k);queue.push(q)}}}return reached}
  const seedsFor=r=>{const pos=r.lastPosition;if(!pos)return[];const source=blockAt(Math.floor(pos.x),Math.floor(pos.y),Math.floor(pos.z));if(!clear(source)&&!source?.name?.endsWith('_bed'))return[];return[...nodes.values()].filter(p=>Math.abs(p.x-Math.floor(pos.x))+Math.abs(p.z-Math.floor(pos.z))<=1&&Math.hypot(p.x+.5-pos.x,p.y-pos.y,p.z+.5-pos.z)<=1.8)}
  const seeds=residents.flatMap(seedsFor)
  let reached=flood(seeds)
  if(!residents.length){ // An empty habitat needs one connected usable bed area.
    const considered=new Set();let best=new Set()
    for(const p of nodes.values()){if(considered.has(key(p)))continue;const component=flood([p]);component.forEach(k=>considered.add(k));if(component.size>best.size)best=component}
    reached=best
  }
  const beds=structure.beds.map(b=>({...b,reachability:destinations(b).some(p=>reached.has(key(p)))?'verified conservative floor connectivity':'unknown'}))
  const residentBeds=residents.map(r=>{const reachable=flood(seedsFor(r));return{uuid:r.uuid,beds:beds.filter(b=>b.valid&&destinations(b).some(p=>reachable.has(key(p)))).map(b=>b.id)}})
  const assigned=new Map(),visit=(n,seen)=>{for(const bed of residentBeds[n].beds){if(seen.has(bed))continue;seen.add(bed);if(!assigned.has(bed)||visit(assigned.get(bed),seen)){assigned.set(bed,n);return true}}return false}
  residentBeds.forEach((_,n)=>visit(n,new Set()))
  return{...structure,beds,residentBeds,residentBedCapacity:assigned.size,usableBeds:beds.filter(b=>b.valid&&b.reachability!=='unknown').length}
}

// A desired workspace may add its exact functional block to authored empty
// interior space. This does not alter the saved build allocation or authorize
// replacement of a wall, bed, light, floor, or another workstation.
export function compatibleVillageIntent(before,after){
  const withoutIntent=doc=>{const{population,structure,...rest}=doc;return canonicalBlueprint(rest)}
  if(withoutIntent(before)!==withoutIntent(after))return false
  const old=compileBlueprintStructure(before),next=compileBlueprintStructure(after)
  const stations=new Set(populationWorkspaces(after.population ?? {}).map(w=>w.at.join(',')))
  const identity=(ir,c)=>c?.objectId?{at:c.at,states:c.states,object:ir.objects.find(o=>o.id===c.objectId)}:c
  const oldCells=new Map(old.cells.map(c=>[c.at.join(','),c])),newCells=new Map(next.cells.map(c=>[c.at.join(','),c]))
  if(oldCells.size!==newCells.size)return false
  for(const [key,c] of oldCells){const n=newCells.get(key)
    if(canonicalBlueprint(identity(old,c))===canonicalBlueprint(identity(next,n)))continue
    if(c.require!=='air'||!stations.has(key)||!n?.objectId)return false
    const obj=next.objects.find(o=>o.id===n.objectId)
    if(obj?.footprint.length!==1||!obj.block)return false
  }
  return true
}
