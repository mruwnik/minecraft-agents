// A UUID is identity proof, not workstation ownership. Crowded rerolls permit
// only rivals positively observed unable to take this sole matching station.
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
