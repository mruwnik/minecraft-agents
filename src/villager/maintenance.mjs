import { readVillagerRoster, mergeVillagerObservation, saveVillagerObservation } from './roster.mjs'
import { villagerObservation, entityUuid } from './observation.mjs'
import { populationReport, populationWorkspaces } from './population.mjs'
import { villagePlan, villageStructure, villageShelter, reachableVillageBeds, VILLAGER_ROSTER_FILE } from './village-plan.mjs'
import { JOB_BLOCK_PROFESSION } from './trade.mjs'
import { habitatOwnership, habitatThreats } from '../enclosure/guards.mjs'
import { saveVillageInspection } from './inspection.mjs'
import { safeFullBlock } from '../enclosure/blocks.mjs'
import { CompositeHandBack } from '../composite.mjs'

const time=()=>new Date().toISOString()
const ordinaryFailure=error=>{if(error?.reason||error instanceof TypeError||error instanceof SyntaxError||error instanceof ReferenceError||/^cancelled\b/i.test(error?.message??''))throw error;return error}
const childFinished=result=>{if(result?.stopped&&result.stopped!=='done')throw new CompositeHandBack(result.stopped);return result}
const onlyBlueprint=a=>Object.fromEntries(['name','plan','file','origin','place','x','y','z','facing','supply'].filter(k=>a[k]!==undefined).map(k=>[k,a[k]]))
const workstationCell=(plan,c)=>populationWorkspaces(plan.intent).some(w=>{const p=plan.world(w.at);return p.x===c.x&&p.y===c.y&&p.z===c.z&&c.spec.alts?.every(a=>JOB_BLOCK_PROFESSION[a.name]===w.profession)})
const repairableCell=(plan,c)=>c.spec.alts?.every(a=>a.name==='torch'||a.name?.endsWith('_bed'))||workstationCell(plan,c)
const safetyPlan=plan=>({x:plan.at.x,y:plan.at.y,z:plan.at.z,width:Math.max(plan.bp.width,plan.bp.depth),shell:plan.cells,gates:plan.entrances})
async function inspectionApproach(api,plan,target){
  const state=villageStructure(plan,api.block)
  if(!plan.inside(api.pos())||state.unknown.length||state.openEntrances.length||state.missing.some(c=>!repairableCell(plan,c)))return false
  habitatOwnership(api,safetyPlan(plan),{clear:[]},'village')
  await habitatThreats(api,safetyPlan(plan),'village')
  const spots=[]
  for(let dx=-2;dx<=2;dx++)for(let dz=-2;dz<=2;dz++)for(const dy of [0,-1]){
    const p={x:Math.floor(target.x)+dx,y:Math.floor(target.y)+dy,z:Math.floor(target.z)+dz}
    if(plan.inside(p)&&Math.hypot(p.x+.5-target.x,p.y-target.y,p.z+.5-target.z)<=2.5&&['air','torch'].includes(api.block(p.x,p.y,p.z)?.name)&&api.block(p.x,p.y+1,p.z)?.name==='air'&&safeFullBlock(api.block(p.x,p.y-1,p.z)))spots.push(p)
  }
  spots.sort((p,q)=>Math.hypot(p.x-api.pos().x,p.z-api.pos().z)-Math.hypot(q.x-api.pos().x,q.z-api.pos().z))
  for(const p of spots.slice(0,8)){await api.checkpoint?.();const path=await api.act('path_to',{...p,range:0,into:true,stroll:true,route:true});if(path.status==='success'&&!path.gates){await api.act('goto',{...p,range:0,into:true});return !villageStructure(plan,api.block).openEntrances.length}}
  return false
}
export async function inspectVillage(api,a,io={}) {
  const plan=villagePlan(api,a,io), staleMs=(a.freshFor ?? 120)*1000
  if (!Number.isFinite(staleMs)||staleMs<1000||staleMs>3600000) throw new Error('freshFor= must be 1..3600 seconds')
  let roster=(io.readRoster ?? (()=>readVillagerRoster(VILLAGER_ROSTER_FILE)))()
  const rows=(await api.act('entity',{name:'villager',uuid:true,count:1000})).found ?? []
  if(rows.length>=1000) throw new Error('village observation limit reached; no intervention is safe')
  for(const row of rows){const o=villagerObservation(row);if(!o.uuid||!o.position)continue
    const hasProfession=Boolean(o.metadata?.[19] ?? o.metadata?.[18]) || typeof row.profession==='string'
    roster=mergeVillagerObservation(roster,{uuid:o.uuid,at:time(),by:api.me(),position:o.position,baby:o.baby,profession:hasProfession?(row.profession ?? o.profession):'unknown',level:hasProfession?o.level:undefined})
  }
  for(const r of Object.values(roster.villagers))if(r.workstationClaim&&!r.workstationClaim.invalidatedAt){
    const c=r.workstationClaim,b=api.block(c.x,c.y,c.z)
    if(b&&b.name!==c.block){
      const input={uuid:r.uuid,at:time(),by:api.me(),invalidateWorkstationClaim:'workstation block observed changed or missing'}
      roster=mergeVillagerObservation(roster,input)
      if(!io.readRoster)(io.saveObservation ?? (input=>saveVillagerObservation(VILLAGER_ROSTER_FILE,input)))(input)
    }
  }
  const relevant=()=>Object.values(roster.villagers).filter(r=>plan.inside(r.lastPosition)||(typeof a.place==='string'&&r.place?.name===a.place))
  // Only exact visible identities may be inspected. An unobserved record is
  // retained as unknown, never replaced or interpreted as a dead villager.
  const inspectionNeeded=[],inspectionErrors=[]
  if(a.inspect!==false) for(const row of rows){const o=villagerObservation(row);if(!o.uuid||o.baby!==false||!plan.inside(o.position))continue
    const record=roster.villagers[o.uuid],needs=[...plan.intent.roles,...populationWorkspaces(plan.intent)].some(r=>r.trade&&r.profession===record.profession&&(!record.offers||!Number.isFinite(Date.parse(record.offers.observedAt))||Date.now()<Date.parse(record.offers.observedAt)||Date.now()-Date.parse(record.offers.observedAt)>staleMs))
    if(!needs)continue
    if(!plan.inside(api.pos())||Math.hypot(api.pos().x-o.position.x,api.pos().y-o.position.y,api.pos().z-o.position.z)>3){
      if(!a.approachInspection||!await inspectionApproach(api,plan,o.position)){inspectionNeeded.push(o.uuid);continue}
    }
    await api.checkpoint?.()
    const fresh=((await api.act('entity',{name:'villager',uuid:true,count:1000})).found ?? []).find(e=>e.uuid===o.uuid)
    if(!fresh)continue
    const p=villagerObservation(fresh).position
    if(!p||!plan.inside(p)||Math.hypot(api.pos().x-p.x,api.pos().y-p.y,api.pos().z-p.z)>3){inspectionNeeded.push(o.uuid);continue}
    let offers
    try{offers=await api.act('trades',{uuid:o.uuid,id:fresh.id,place:a.place})}catch(error){ordinaryFailure(error);inspectionNeeded.push(o.uuid);inspectionErrors.push({uuid:o.uuid,error:error.message});break}
    roster=mergeVillagerObservation(roster,{uuid:o.uuid,at:time(),by:api.me(),profession:offers.profession,level:offers.level,offers:offers.offers ?? []})
  }
  // Runtime trades records confirmed evidence in the shared store. Merge that
  // store after inspection instead of inventing lock evidence from use counters.
  const persisted=(io.readRoster ?? (()=>readVillagerRoster(VILLAGER_ROSTER_FILE)))()
  for(const [uuid,r] of Object.entries(persisted.villagers)) if(roster.villagers[uuid]) roster.villagers[uuid]={...roster.villagers[uuid],...(r.lockEvidence?{lockEvidence:r.lockEvidence}:{}),...(r.purchases?{purchases:r.purchases}:{}),...(r.workstationClaim?{workstationClaim:r.workstationClaim}:{})}
  const records=relevant(), report=populationReport(plan.intent,records,{staleMs,inside:plan.inside,stationWorld:plan.world,blockAt:api.block})
  const unseen=records.filter(r=>!rows.some(e=>e.uuid===r.uuid)).map(r=>r.uuid)
  const displaced=records.filter(r=>typeof a.place==='string'&&r.place?.name===a.place&&!plan.inside(r.lastPosition)).map(r=>r.uuid)
  const structure=reachableVillageBeds(plan,villageStructure(plan,api.block),api.block,records.filter(r=>plan.inside(r.lastPosition)))
  const shelter=villageShelter(plan,api.block)
  const capacity=Math.max(report.population,plan.intent.target)
  const capacityStatus=structure.usableBeds>=capacity&&structure.residentBedCapacity>=report.population?'satisfied':structure.beds.filter(b=>b.valid).length>=capacity?'unknown':'violated'
  const result={plan,rows,records,report:{...report,population:report.population,intent:plan.intent,populationSatisfied:report.satisfied,satisfied:report.satisfied&&structure.complete&&shelter.status==='satisfied'&&capacityStatus==='satisfied'&&!displaced.length&&!unseen.length,unseen,displaced,inspectionNeeded,inspectionErrors,structure,shelter,capacityStatus,requiredBeds:capacity,bedReachability:'conservative full-floor connectivity; unsupported partial footing remains unknown'},roster}
  result.report.status=result.report.satisfied?'satisfied':unseen.length||report.unknown.length||structure.unknown.length||capacityStatus==='unknown'||report.roles.some(r=>r.status==='unknown')||report.workspaces.some(w=>w.status==='unknown')?'unknown':'violated'
  ;(io.saveInspection ?? saveVillageInspection)(plan,result.report,io.inspectionDir)
  return result
}

export async function maintainVillage(api,a,io={}) {
  if(a.planOnly===true) return (await inspectVillage(api,a,io)).report
  const maxPasses=a.passes ?? 12
  if(!Number.isInteger(maxPasses)||maxPasses<1||maxPasses>32)throw new Error('passes= must be 1..32')
  const actions=[],blocked=[]
  const snapshot=(extra={})=>inspectVillage(api,{...a,approachInspection:true,...extra},io)
  for(let pass=0;pass<maxPasses;pass++) {
    await api.checkpoint?.()
    let s=await snapshot(), {plan,report}=s
    api.report?.({...report,pass,actions})
    if(report.unseen.length){blocked.push(`inspect known unobserved UUIDs before intervention: ${report.unseen.join(', ')}`);break}
    if(report.displaced.length){blocked.push(`known residents are observed outside the habitat: ${report.displaced.join(', ')}; configure explicit return transport, not replacement`);break}
    if(report.unknown.length||report.inspectionNeeded.length){blocked.push(`inspect unknown metadata/offers from a closed interior stance before intervention: ${[...new Set([...report.unknown,...report.inspectionNeeded])].join(', ')}`);break}
    if(report.structure.unknown.length){blocked.push('load every planned structure cell before repair or population changes');break}
    const safety=safetyPlan(plan)
    try{habitatOwnership(api,safety,{clear:[]},'village');await habitatThreats(api,safety,'village')}catch(error){ordinaryFailure(error);blocked.push(error.message);break}
    if(report.structure.openEntrances.length){
      if(report.structure.missing.some(c=>!repairableCell(plan,c))){blocked.push('boundary is incomplete; close/recover it explicitly before managing residents');break}
      for(const p of report.structure.openEntrances)await api.act('toggle',{...p,open:false})
      actions.push({action:'closeEntrances'});continue
    }
    // Let the claim workflow observe unemployment before placing an absent
    // workstation. Pre-building it could trigger a profession change before
    // the exact UUID transition was observed, losing attribution evidence.
    const deferStations=report.structure.missing.length&&report.structure.missing.every(c=>workstationCell(plan,c)&&['air','cave_air','void_air'].includes(c.actual))&&s.records.some(r=>r.age==='adult'&&r.profession==='unemployed'&&!r.lockEvidence&&plan.inside(r.lastPosition))
    if(report.structure.missing.length&&!deferStations){
      // Occupied construction has a different safety contract from an empty
      // blueprint build. Refuse all mutation until bounded repair has an
      // independently verified closed boundary; no demolition or gate opening.
      if(report.population||(plan.manifest&&report.structure.missing.every(c=>repairableCell(plan,c)))){
        const repaired=await repairOccupiedVillage(api,s)
        if(repaired.blocked){blocked.push(repaired.blocked);break}
        actions.push({action:'repair',cells:repaired.cells});continue
      }
      if(report.structure.missing.some(c=>!['air','cave_air','void_air'].includes(c.actual))){blocked.push('repair would remove an existing addition; inspect it explicitly; no demolition performed');break}
      const buildArgs=plan.manifest?{place:a.place,...(a.supply?{supply:a.supply}:{})}:onlyBlueprint(a)
      let built
      try{built=childFinished(await (io.build ? io.build(api,buildArgs,io):api.act('blueprint.build',buildArgs)))}catch(error){ordinaryFailure(error);blocked.push(`physical repair: ${error.message}`);break}
      actions.push({action:'blueprint.build',result:built})
      if(built.left||built.stuck||built.night){blocked.push('physical blueprint repair paused; resume the saved build first');break}
      continue
    }
    if(report.capacityStatus!=='satisfied'){blocked.push(`need ${report.requiredBeds} reachable usable paired beds with headroom, including one per observed resident; verified ${report.structure.usableBeds}; inspect disconnected/unknown paths or expand the blueprint without displacing residents`);break}
    if(report.shelter.status!=='satisfied'){blocked.push(`safe sleeping enclosure is not verified: ${report.shelter.issues.join('; ')}`);break}
    if(report.population<plan.intent.target){
      if(a.imports?.length){
        const route=a.imports.find(route=>!s.records.some(r=>r.uuid===route.uuid&&plan.inside(r.lastPosition)))
        if(!route){blocked.push('provided import routes exhausted before target');break}
        try{await executeVillageImport(api,route);const incoming=((await api.act('entity',{name:'villager',uuid:true,count:1000})).found ?? []).find(e=>e.uuid===route.uuid);if(!incoming||!plan.inside(villagerObservation(incoming).position)||incoming.vehicle||incoming.vehicleId!=null)throw new Error('exact imported UUID is not observed on foot inside the destination');actions.push({action:'import',uuid:route.uuid})}catch(error){ordinaryFailure(error);blocked.push(`import ${route.uuid}: ${error.message}; inspect actual boat/passenger before resuming`);break}
        continue
      }
      const adults=s.records.filter(r=>r.age==='adult'&&plan.inside(r.lastPosition))
      if(adults.length<2){blocked.push(`need ${2-adults.length} observed adult founders; provide imports=[{uuid,steps:[{action,args}]}] with an explicit tested boat or walking route`);break}
      // The current breeder owns the established roofed-house geometry. Do not
      // pretend arbitrary authored buildings share that physical contract.
      const knownHouse=plan.bp.width===10&&plan.bp.depth===10&&plan.ir.height===4&&plan.facing==='south'
      const breedArgs=a.breed ?? (knownHouse?{x:plan.at.x+1,y:plan.at.y,z:plan.at.z+1,size:8,entryX:plan.at.x+9,entryZ:plan.at.z+8,airlock:true}:null)
      if(!breedArgs){blocked.push('provide breed={x,y,z,size,entryX,entryZ,airlock} for the verified existing habitat; the built-in ten-bed south-facing house is derived automatically, arbitrary geometry is not');break}
      let result
      try{const args={...breedArgs,target:plan.intent.target};result=childFinished(await (io.breed ? io.breed(api,args):api.act('villager.breed',args)))}catch(error){ordinaryFailure(error);blocked.push(`breeding: ${error.message}; inspect the persistent food receipt before resuming`);break}
      actions.push({action:'villager.breed',result});continue
    }
    if(report.satisfied){const result={...report,status:'satisfied',actions};(io.saveInspection ?? saveVillageInspection)(plan,result,io.inspectionDir);return result}
    const workspace=report.workspaces.find(w=>w.status!=='satisfied')
    const role=workspace?{...workspace,workstation:populationWorkspaces(plan.intent).find(w=>w.id===workspace.id).at}:plan.intent.roles.find(r=>report.roles.find(x=>x.id===r.id)?.status!=='satisfied')
    if(!role){blocked.push('population roles are satisfied but the overall village proof is incomplete; inspect the housing/presence report before intervention');break}
    const candidate=s.records.find(r=>r.age==='adult'&&plan.inside(r.lastPosition)&&!report.assigned.includes(r.uuid)&&!r.lockEvidence&&['unemployed',role.profession].includes(r.profession))
    if(!candidate){blocked.push(`no safely eligible observed unlocked adult for ${role.id}; inspect unknowns or wait for a baby to mature; locked traders are preserved, and an unverified locked workstation association must be confirmed without removing its station`);break}
    if(!role.workstation){blocked.push(`role ${role.id} needs an explicit local workstation coordinate in population.roles`);break}
    const cell=plan.world(role.workstation),block=Object.keys(JOB_BLOCK_PROFESSION).find(b=>JOB_BLOCK_PROFESSION[b]===role.profession)
    try {
      const args={...cell,uuid:candidate.uuid,block,pen:false,buy:true,...(workspace||role.workstation?{proveStation:true,claimHabitat:{at:plan.at,width:plan.bp.width,depth:plan.bp.depth,height:plan.ir.height,gates:plan.entrances}}:{}),tries:a.tries ?? 40,...role.trade}
      const result=childFinished(await (io.roll ? io.roll(api,args):api.act('villager.roll',args)))
      if(result.workstationClaim){const input={uuid:candidate.uuid,at:time(),by:api.me(),workstationClaim:result.workstationClaim};(io.saveObservation ?? (input=>saveVillagerObservation(VILLAGER_ROSTER_FILE,input)))(input)}
      actions.push({action:'villager.roll',uuid:candidate.uuid,role:role.id,result})
      if(!result.locked){blocked.push(`role ${role.id} was not inventory-confirmed locked; resume after inspecting offers/supplies`);break}
    } catch(error){ordinaryFailure(error);blocked.push(`role ${role.id}: ${error.message}`);break}
  }
  const last=await snapshot({approachInspection:false,inspect:false}), final=last.report
  const complete=final.satisfied&&!blocked.length
  const result={...final,status:complete?'satisfied':'blocked',actions,blocked:complete?[]:blocked.length?blocked:['bounded maintenance pass limit reached; resume after checking report']}
  ;(io.saveInspection ?? saveVillageInspection)(last.plan,result,io.inspectionDir)
  return result
}

export async function executeVillageImport(api,route) {
  if(!route||!entityUuid(route.uuid)||!Array.isArray(route.steps)||!route.steps.length||route.steps.length>16)throw new Error('import needs exact uuid and 1..16 explicit transport steps')
  const allowed=new Set(['boat.board','boat.ferry','boat.stage','boat.receive','boat.dock','boat.undock','goto','wait'])
  for(const step of route.steps){if(!allowed.has(step.action)||!step.args||typeof step.args!=='object'||Array.isArray(step.args))throw new Error('import steps must be existing boat transport/goto/wait commands with args')
    if(step.action.startsWith('boat.')&&step.args.uuid!==route.uuid)throw new Error('every passenger transport step must retain the exact imported UUID')
    if(step.action==='goto'&&step.args.dig===true)throw new Error('village import walking does not authorize terrain excavation')
  }
  const results=[];let boat
  for(const step of route.steps){await api.checkpoint?.();const args={...step.args}
    if(step.action.startsWith('boat.')&&step.action!=='boat.board'&&args.boat===undefined&&boat!==undefined)args.boat=boat
    const result=childFinished(await api.act(step.action,args));results.push(result);if(Number.isInteger(result?.boat))boat=result.boat
  }
  return results
}

export async function repairOccupiedVillage(api,s) {
  const {plan,report}=s,missing=report.structure.missing
  const safe=c=>repairableCell(plan,c)
  if(missing.some(c=>!safe(c)))return{blocked:'occupied boundary/structure repair is unsafe: secure residents and supply an explicit bounded repair route; no blocks changed'}
  if(!plan.inside(api.pos()))return{blocked:'start occupied bed/light repair inside the closed habitat; no exterior gate crossing is automatic'}
  const cells=[]
  for(const cell of missing){
    if(cell.spec.alts[0]?.states?.part==='head'){
      const mate=report.structure.beds.find(b=>b.halves.some(h=>h.x===cell.x&&h.y===cell.y&&h.z===cell.z))
      if(mate?.halves.some(p=>!['air','cave_air','void_air'].includes(api.block(p.x,p.y,p.z)?.name)))return{blocked:'partial occupied bed needs explicit inspection; no existing half is removed'}
      continue
    }
    await api.checkpoint?.()
    const rows=(await api.act('entity',{name:'*',uuid:true,count:1000})).found ?? []
    if(rows.length>=1000)return{blocked:'repair occupancy query reached its limit'}
    const xyz=e=>villagerObservation(e).position
    const mate=cell.spec.alts[0]?.name?.endsWith('_bed')?report.structure.beds.find(b=>b.halves.some(h=>h.x===cell.x&&h.y===cell.y&&h.z===cell.z)):null
    const targets=mate?.halves ?? [cell]
    if(targets.some(p=>!['air','cave_air','void_air'].includes(api.block(p.x,p.y,p.z)?.name)))return{blocked:'partial occupied bed or nonempty repair cell needs explicit inspection; no existing blocks are removed'}
    if(rows.some(e=>{const p=xyz(e);return p&&targets.some(t=>p.x+(e.width??.6)/2>t.x&&p.x-(e.width??.6)/2<t.x+1&&p.z+(e.width??.6)/2>t.z&&p.z-(e.width??.6)/2<t.z+1&&p.y+(e.height??1.95)>t.y&&p.y<t.y+1)}))return{blocked:'repair cell is occupied; wait for the resident to move before resuming'}
    const spots=[[1,0],[-1,0],[0,1],[0,-1]].map(([dx,dz])=>({x:cell.x+dx,y:cell.y,z:cell.z+dz})).filter(p=>plan.inside(p)&&['air','torch'].includes(api.block(p.x,p.y,p.z)?.name)&&['air'].includes(api.block(p.x,p.y+1,p.z)?.name)&&safeFullBlock(api.block(p.x,p.y-1,p.z)))
    let stand
    for(const p of spots){const path=await api.act('path_to',{...p,range:0,into:true,stroll:true,route:true});if(path.status==='success'&&!path.gates){stand=p;break}}
    if(!stand)return{blocked:'no closed-gate interior repair stance is proven reachable'}
    await api.act('goto',{...stand,range:0,into:true})
    const boundary=villageStructure(plan,api.block)
    if(boundary.unknown.length||boundary.openEntrances.length||boundary.missing.some(c=>!safe(c))||!plan.inside(api.pos()))return{blocked:'enclosure changed during repair approach; no placement performed'}
    const latest=(await api.act('entity',{name:'*',uuid:true,count:1000})).found ?? []
    if(latest.length>=1000||latest.some(e=>{const p=xyz(e);return p&&targets.some(t=>p.x+(e.width??.6)/2>t.x&&p.x-(e.width??.6)/2<t.x+1&&p.z+(e.width??.6)/2>t.z&&p.z-(e.width??.6)/2<t.z+1&&p.y+(e.height??1.95)>t.y&&p.y<t.y+1)}))return{blocked:'repair cell became occupied during approach; no placement performed'}
    const alt=cell.spec.alts[0]
    await api.act('place',{item:alt.name,x:cell.x,y:cell.y,z:cell.z,...(alt.states?.facing?{facing:alt.states.facing}:{})})
    cells.push({x:cell.x,y:cell.y,z:cell.z})
  }
  return{cells}
}
