import { matchesVillagerOutput, JOB_BLOCK_PROFESSION, VILLAGER_ENCHANTS } from './trade.mjs'

const professions = new Set(Object.values(JOB_BLOCK_PROFESSION))
export function validatePopulation(value) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('blueprint population must be an object')
  for (const k of Object.keys(value)) if (!['target','roles','surplus','workspaces'].includes(k)) throw new Error(`blueprint population.${k} is unsupported`)
  const target = value.target
  if (!Number.isInteger(target) || target < 0 || target > 100) throw new Error('blueprint population needs target 0..100 (a minimum; surplus is allowed)')
  if (value.surplus !== undefined && value.surplus !== 'allow') throw new Error('blueprint population surplus must be allow; removal is never implicit')
  if (!Array.isArray(value.roles ?? []) || (value.roles ?? []).length > 100) throw new Error('blueprint population roles must be an array of at most 100 roles')
  const ids = new Set()
  for (const role of value.roles ?? []) {
    if (!role || typeof role !== 'object' || Array.isArray(role)) throw new Error('blueprint population role must be an object')
    for (const k of Object.keys(role)) if (!['id','count','profession','trade','workstation'].includes(k)) throw new Error(`blueprint population role.${k} is unsupported`)
    if (!/^[a-z0-9][a-z0-9_-]{0,63}$/.test(role.id ?? '') || ids.has(role.id)) throw new Error('blueprint population role IDs must be unique')
    ids.add(role.id)
    if (!professions.has(role.profession) || !Number.isInteger(role.count ?? 1) || (role.count ?? 1) < 1 || (role.count ?? 1) > 100) throw new Error('blueprint population role needs a known profession and count 1..100')
    if (role.workstation !== undefined && (role.count ?? 1) !== 1) throw new Error('a role.workstation binds one trader; use distinct workspaces for multiple stations')
    if (role.workstation !== undefined && (!Array.isArray(role.workstation) || role.workstation.length !== 3 || !role.workstation.every(Number.isInteger))) throw new Error('blueprint population workstation must be local [x,y,z]')
    if (role.trade !== undefined) {
      const t = role.trade
      if (!t || typeof t !== 'object' || Array.isArray(t) || Object.keys(t).some(k => !['output','enchant','level','atLeast'].includes(k)) || !/^[a-z0-9_]+$/.test(t.output ?? '')) throw new Error('blueprint population trade needs output and optional enchant/level/atLeast')
      if (t.enchant !== undefined && !VILLAGER_ENCHANTS.includes(t.enchant)) throw new Error('blueprint population enchant must be a supported enchantment name')
      if (t.level !== undefined && (!t.enchant || !Number.isInteger(t.level) || t.level < 1 || t.level > 255)) throw new Error('blueprint population enchant level must be 1..255')
      if (t.atLeast !== undefined && (typeof t.atLeast !== 'boolean' || !t.enchant || t.level === undefined)) throw new Error('blueprint population atLeast needs enchant and level')
    }
  }
  if ((value.roles ?? []).reduce((n,r) => n + (r.count ?? 1), 0) > target) throw new Error('blueprint population role count exceeds target')
  const workspaces=value.workspaces ?? []
  if(!Array.isArray(workspaces)||workspaces.length>100)throw new Error('blueprint population workspaces must be an array of at most 100 stations')
  for(const w of workspaces){
    if(!w||typeof w!=='object'||Array.isArray(w)||Object.keys(w).some(k=>!['id','at','profession','trade'].includes(k)))throw new Error('blueprint workspace needs id, at, profession and optional trade')
    if(!Array.isArray(w.at)||w.at.length!==3||!w.at.every(Number.isInteger))throw new Error('workspace at must be local [x,y,z]')
    validatePopulation({target:1,roles:[{id:w.id,profession:w.profession,...(w.trade?{trade:w.trade}:{}),workstation:w.at}]})
  }
  const stations=[...workspaces,...(value.roles ?? []).filter(r=>r.workstation).map(r=>({id:r.id,at:r.workstation,profession:r.profession,trade:r.trade,role:r.id}))]
  const stationIds=new Set(),cells=new Set()
  for(const w of stations){if(stationIds.has(w.id)||cells.has(w.at.join(',')))throw new Error('workspace IDs and positions must be distinct');stationIds.add(w.id);cells.add(w.at.join(','))}
  if(stations.length>target)throw new Error('workspace count exceeds population target')
  const requiredByProfession=new Map()
  for(const profession of professions){const global=(value.roles ?? []).filter(r=>r.profession===profession).reduce((n,r)=>n+(r.count??1),0),located=stations.filter(w=>w.profession===profession).length;requiredByProfession.set(profession,Math.max(global,located))}
  if([...requiredByProfession.values()].reduce((a,b)=>a+b,0)>target)throw new Error('population target cannot satisfy the simultaneous profession and workspace requirements')
  return { ...structuredClone(value), target, surplus:'allow', roles:value.roles ?? [],workspaces }

}

export function populationReport(intent, records, {now=Date.now(), staleMs=120000, inside=()=>true, stationWorld=p=>({x:p[0],y:p[1],z:p[2]}), blockAt=()=>null}={}) {
  intent = validatePopulation(intent)
  const all = [...new Map(records.map(r => [r.uuid,r])).values()].filter(r => r.uuid)
  const fresh = r => Number.isFinite(Date.parse(r.lastSeenAt)) && now-Date.parse(r.lastSeenAt) <= staleMs && now >= Date.parse(r.lastSeenAt)
  const residents = all.filter(r => inside(r.lastPosition)), present=residents.filter(fresh)
  const unknown = residents.filter(r => !fresh(r) || !['adult','baby'].includes(r.age) || !r.profession || r.profession === 'unknown')
  const slots=intent.roles.flatMap(r => Array.from({length:r.count ?? 1},()=>r))
  const freshOffers=r=>Number.isFinite(Date.parse(r.offers?.observedAt))&&now>=Date.parse(r.offers.observedAt)&&now-Date.parse(r.offers.observedAt)<=staleMs
  const bound=(r,local,profession)=>stationAssociation(r,stationWorld(local),profession,{now,staleMs,blockAt}).status==='verified'
  const edges=slots.map(role => present.filter(r => r.age==='adult' && r.profession===role.profession && r.lockEvidence?.profession===role.profession && r.lockEvidence?.basis==='inventory-confirmed villager trade; profession is trade-locked' && (!role.workstation||bound(r,role.workstation,role.profession)) && (!role.trade || (freshOffers(r) && r.offers.profession===role.profession && r.offers.items.some(o => matchesVillagerOutput(o,role.trade,{includeDisabled:true}))))).map(r=>r.uuid))
  const assigned=new Map()
  const visit=(s,seen)=>{for(const uuid of edges[s]) {if(seen.has(uuid)) continue; seen.add(uuid); if(!assigned.has(uuid)||visit(assigned.get(uuid),seen)){assigned.set(uuid,s);return true}}return false}
  slots.forEach((_,s)=>visit(s,new Set()))
  const roles=intent.roles.map(role=>{const uuids=[...assigned].filter(([,s])=>slots[s].id===role.id).map(([u])=>u),uncertain=unknown.length||present.some(r=>r.profession===role.profession&&(!r.lockEvidence||(role.workstation&&!bound(r,role.workstation,role.profession))||(role.trade&&!freshOffers(r))));return{id:role.id,required:role.count ?? 1,uuids,status:uuids.length===(role.count ?? 1)?'satisfied':uncertain?'unknown':'violated',stock:uuids.map(uuid=>{const r=present.find(r=>r.uuid===uuid),offers=role.trade?r.offers?.items.filter(o=>matchesVillagerOutput(o,role.trade,{includeDisabled:true})):r.offers?.items;return{uuid,observedAt:r.offers?.observedAt,status:!freshOffers(r)?'unknown':offers?.some(o=>!o.tradeDisabled&&!(Number.isFinite(o.maximumNbTradeUses)&&Number(o.nbTradeUses)>=o.maximumNbTradeUses))?'available':'exhausted',restock:'not inferred from workstation proximity'}})}})
  const workspaces=populationWorkspaces(intent).map(w=>{
    const at=stationWorld(w.at),block=Object.keys(JOB_BLOCK_PROFESSION).find(b=>JOB_BLOCK_PROFESSION[b]===w.profession)
    const candidates=present.filter(r=>r.age==='adult'&&r.profession===w.profession&&r.lockEvidence?.profession===w.profession&&r.lockEvidence?.basis==='inventory-confirmed villager trade; profession is trade-locked'&&(!w.trade||(freshOffers(r)&&r.offers.profession===w.profession&&r.offers.items.some(o=>matchesVillagerOutput(o,w.trade,{includeDisabled:true})))))
    const verified=candidates.filter(r=>bound(r,w.at,w.profession))
    const actual=blockAt(at.x,at.y,at.z)
    return {...w,at,block,uuids:verified.map(r=>r.uuid),status:verified.length===1?'satisfied':!actual?'unknown':actual.name!==block?'violated':'unknown',associations:candidates.map(r=>({uuid:r.uuid,...stationAssociation(r,at,w.profession,{now,staleMs,blockAt})}))}
  })
  // One current claim cannot fill two distinct workspaces.
  const workspaceAssignments=new Set()
  for(const w of workspaces)if(w.status==='satisfied'){if(workspaceAssignments.has(w.uuids[0])){w.status='unknown';w.uuids=[]}else workspaceAssignments.add(w.uuids[0])}
  const uncertain = residents.some(r => !fresh(r))
  return {population:present.length,known:residents.length,target:intent.target,surplus:Math.max(0,present.length-intent.target),populationStatus:present.length>=intent.target?'satisfied':uncertain?'unknown':'violated',roles,workspaces,unknown:unknown.map(r=>r.uuid),satisfied:present.length>=intent.target && roles.every(r=>r.status==='satisfied')&&workspaces.every(w=>w.status==='satisfied'),assigned:[...assigned.keys()]}
}

export const populationWorkspaces=intent=>[...(intent.workspaces ?? []),...(intent.roles ?? []).filter(r=>r.workstation).map(r=>({id:r.id,at:r.workstation,profession:r.profession,trade:r.trade,role:r.id}))]
export const WORKSTATION_CLAIM_BASIS='exact UUID observed unemployed then adopted profession at a sole matching workstation in a verified closed habitat; rival claimants excluded'
export function stationAssociation(record,at,profession,{now=Date.now(),staleMs=120000,blockAt=()=>null}={}){
  const c=record.workstationClaim,block=Object.keys(JOB_BLOCK_PROFESSION).find(b=>JOB_BLOCK_PROFESSION[b]===profession),actual=blockAt(at.x,at.y,at.z)
  if(!actual)return {status:'unknown',reason:'workstation block is not loaded'}
  if(actual.name!==block)return {status:'invalid',reason:'required workstation block changed or is missing'}
  if(!c||c.basis!==WORKSTATION_CLAIM_BASIS||c.profession!==profession||c.block!==block||['x','y','z'].some(k=>c[k]!==at[k]))return {status:'unknown',reason:'no verified exact UUID workstation association'}
  const ms=Date.parse(c.observedAt)
  if(!Number.isFinite(ms)||ms>now||now-ms>staleMs||c.invalidatedAt)return {status:'unknown',reason:'workstation association is stale or invalidated'}
  return {status:'verified',observedAt:c.observedAt,source:c.observedBy,basis:c.basis}
}
