import { inAnyZone, workRefusal } from '../lib.mjs'
import { breedPreflight, breedKey, breedGate } from './breed.mjs'

export async function habitatThreats (api, plan) {
  const found = (await api.act('entity', { name: '*', hostile: true, uuid: true, count: 1000 })).found ?? []
  if (found.length >= 1000) throw new Error('hostile census reached its observation limit')
  for (const e of found) {
    const [x, y, z] = String(e.exact ?? e.at).split(',').map(Number)
    if (![x, y, z].every(Number.isFinite)) throw new Error('nearby hostile has no confirmed position')
    const dx = Math.max(plan.x - 1 - x, 0, x - (plan.x + plan.width + 1))
    const dz = Math.max(plan.z - 1 - z, 0, z - (plan.z + plan.width + 1))
    if (Math.hypot(dx, dz) <= 8 && y >= plan.y - 3 && y <= plan.y + 8) throw new Error(`hostile ${e.uuid ?? e.id} near breeder at ${e.exact ?? e.at}; secure the area before building or feeding`)
  }
}
export function habitatOwnership (api, plan, preflight) {
  const points = [...plan.shell, ...plan.gates, ...plan.lights, ...plan.beds.flatMap(b => [b.foot, b.head]), ...preflight.clear]
  const foreign = api.zones().find(z => points.some(p => inAnyZone([z], p)) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase()))
  if (foreign) throw new Error(`breeder crosses protected zone ${foreign.name}`)
  const owner = api.places().filter(p => Math.hypot(p.x - plan.x, p.z - plan.z) <= (p.radius ?? 8) + plan.width).map(p => workRefusal(p, api.me())).find(Boolean)
  if (owner) throw new Error(owner)
}
export async function closeHabitatGates (api, plan, gateItem, cleanup = false) {
  for (const p of plan.gates) {
    const b = api.block(p.x, p.y, p.z)
    if (breedGate(b?.name) && b.properties?.open === true) await (cleanup ? api.cleanupAct : api.act)('toggle', { ...p, open: false })
  }
}
export function habitatSecure (api, plan, material, gateItem, openEntry = false) {
  const state = breedPreflight(plan, api.block, material, gateItem)
  if (state.needed.length || state.missingBeds.length || state.clear.length) throw new Error('breeder enclosure, lights or beds are no longer complete')
  for (const p of plan.gates) {
    if ((openEntry === true && (p === plan.entry || p === plan.innerGate)) || (openEntry === 'outer' && p === plan.entry)) continue
    if (api.block(p.x, p.y, p.z)?.properties?.open !== false) throw new Error('breeder gate is not observed closed; no food delivered')
  }
}
export async function enterHabitatMain (api, plan, gateItem) {
  const body = plan.innerGate ? api.pos() : null
  if (plan.innerGate && body.x >= plan.innerGate.x && body.x < plan.entry.x + 1 && body.z >= plan.entry.z && body.z < plan.entry.z + 1 && body.y >= plan.y - 0.1 && body.y < plan.y + 1 && breedGate(api.block(plan.innerGate.x, plan.innerGate.y, plan.innerGate.z)?.name)) {
    // The service booth is separated from residents by its inner gate.
    // Close its outer gate before entering; close the inner gate again from
    // the adjacent main-room side before walking farther into the house.
    await closeHabitatGates(api, plan, gateItem)
    if (api.block(plan.entry.x, plan.entry.y, plan.entry.z)?.properties?.open !== false) throw new Error('outer airlock gate must be closed before entering the main room')
    await api.act('toggle', { ...plan.innerGate, open: true })
    await api.act('goto', { x: plan.innerGate.x - 1, y: plan.y, z: plan.innerGate.z, range: 0, into: true })
    await api.act('toggle', { ...plan.innerGate, open: false })
  }
  await api.act('goto', { ...plan.center, range: 0, into: true })
  await closeHabitatGates(api, plan, gateItem)
}
export function habitatMetadata (plan) {
  return { target: plan.target, size: plan.width, x: plan.x, y: plan.y, z: plan.z, gate: plan.gate, entry: plan.entry, airlock: plan.airlock, innerGate: plan.innerGate, insideEntry: plan.entry ? { ...plan.entry, x: plan.entry.x - 1 } : { ...plan.gate, x: plan.gate.x + 1 }, bedCount: plan.beds.length, roofY: plan.y + 3 }
}
