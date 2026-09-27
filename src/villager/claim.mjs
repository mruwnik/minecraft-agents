// A UUID is identity proof, not workstation ownership. Crowded rerolls permit
// only rivals positively observed unable to take this sole matching station.
import { villageShelter } from './village-plan.mjs'
import { entityUuid, villagerObservation } from './observation.mjs'
const distance = (a,b) => Math.hypot(a.x-b.x,a.y-b.y,a.z-b.z)
export async function verifyCrowdedClaimants (api, { uuid, cell, block }) {
  if (!entityUuid(uuid)) throw new Error('crowded workstation rolling needs an exact uuid=')
  const rows = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found
  if (rows.length >= 100 || rows.some(e => !villagerObservation(e).position)) throw new Error('nearby claimant census is incomplete; isolate before rolling')
  const target = rows.find(e => e.uuid === uuid)
  if (!target || !villagerObservation(target).position || distance(villagerObservation(target).position,cell)>8) throw new Error(`villager ${uuid} is no longer in the workstation area`)
  if (villagerObservation(target).baby !== false) throw new Error('crowded rolling requires a positively observed adult target')
  const rivals = rows.filter(e => e.uuid !== uuid && villagerObservation(e).position && distance(villagerObservation(e).position,cell)<=16)
  for (const rival of rivals) {
    const observed = villagerObservation(rival)
    if (!observed.uuid || observed.baby === undefined) throw new Error('a nearby rival has unknown UUID/age; isolate before rolling')
    if (observed.baby || observed.nitwit) continue
    if (observed.profession === 'unemployed' || observed.profession === 'unknown') throw new Error(`rival ${rival.uuid} can claim ${block}; isolate before rolling`)
    // Profession alone does not prove locked. Refresh identity before opening.
    const fresh = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found.find(e=>e.uuid===rival.uuid)
    if (!fresh) throw new Error(`rival ${rival.uuid} became unobserved; cannot verify its lock`)
    const trades = await api.act('trades', { id: fresh.id, uuid: fresh.uuid, ...cell })
    if (!(Number(trades.level)>1 || trades.offers?.some(o => Number(o.nbTradeUses)>0))) throw new Error(`rival ${rival.uuid} is not proven trade-locked; isolate before rolling`)
  }
  const latest = (await api.act('entity', { name: 'villager', uuid: true, count: 100 })).found
  const latestRivals = latest.filter(e => e.uuid !== uuid && villagerObservation(e).position && distance(villagerObservation(e).position,cell)<=16)
  if (latestRivals.length !== rivals.length || latestRivals.some(e => {
    const old = rivals.find(r=>r.uuid===e.uuid)
    const a = villagerObservation(e), b = villagerObservation(old)
    return !old || a.baby !== b.baby || a.profession !== b.profession
  })) throw new Error('nearby claimants changed during verification; no workstation mutation is safe')
  const stations = (await api.act('find_blocks', { block, maxDistance:24,count:100 })).positions ?? []
  if (stations.length >= 100) throw new Error('workstation census is incomplete; isolate before rolling')
  if (stations.some(p=>distance(p,cell)<=16 && (p.x!==cell.x||p.y!==cell.y||p.z!==cell.z))) throw new Error(`another ${block} can receive the claim; isolate the requested workstation`)
  return target
}

// Paper AcquirePoi searches farther than the local trading census. A causal
// profession transition identifies this station only when a freshly checked
// closed habitat excludes paths to external POIs, and its loaded interior has
// exactly one matching workstation. This is an inference, not a Brain query.
export async function verifyWorkstationEnclosure(api,{uuid,cell,block,habitat,allowAbsent=false}){
  if(!habitat||!habitat.at||!['x','y','z'].every(k=>Number.isInteger(habitat.at[k]))||!['width','depth','height'].every(k=>Number.isInteger(habitat[k]))||habitat.width<3||habitat.depth<3||habitat.height<4||habitat.width*habitat.depth*habitat.height>16384||!Array.isArray(habitat.gates)||!habitat.gates.every(p=>['x','y','z'].every(k=>Number.isInteger(p[k]))))throw new Error('station confirmation needs bounded verified claimHabitat={at,width,depth,height,gates}; an open-world profession change does not prove POI ownership')
  const plan={at:habitat.at,bp:{width:habitat.width,depth:habitat.depth},ir:{height:habitat.height},entrances:habitat.gates}
  const shelter=villageShelter(plan,api.block)
  if(shelter.status!=='satisfied')throw new Error(`station confirmation habitat is not closed and safe: ${shelter.issues.join('; ')}`)
  const rows=(await api.act('entity',{name:'villager',uuid:true,count:1000})).found ?? []
  if(rows.length>=1000)throw new Error('station claimant census is incomplete')
  const target=rows.find(e=>e.uuid===uuid),p=villagerObservation(target).position,a=habitat.at
  const inside=q=>q&&q.x>a.x&&q.x<a.x+habitat.width-1&&q.z>a.z&&q.z<a.z+habitat.depth-1&&q.y>=a.y-.5&&q.y<a.y+habitat.height-1
  if(!inside(p)||!inside(cell))throw new Error('exact target and workstation must remain inside the closed claim habitat')
  const stations=[]
  for(let x=a.x;x<a.x+habitat.width;x++)for(let z=a.z;z<a.z+habitat.depth;z++)for(let y=a.y;y<a.y+habitat.height;y++){
    const b=api.block(x,y,z);if(!b)throw new Error('every claim habitat cell must be loaded')
    if(b.name===block)stations.push({x,y,z})
  }
  if(allowAbsent&&stations.length===0&&['air','cave_air','void_air'].includes(api.block(cell.x,cell.y,cell.z)?.name))return target
  if(stations.length!==1||['x','y','z'].some(k=>stations[0][k]!==cell[k]))throw new Error('claim habitat must contain exactly the requested matching workstation; no nearby-station/proximity inference is allowed')
  return target
}
