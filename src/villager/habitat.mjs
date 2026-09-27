import { breedPreflight } from './breed.mjs'
import { woodenGate as breedGate } from '../enclosure/blocks.mjs'
import { closeHabitatGates, requireClosedGates, habitatThreats as threats, habitatOwnership as ownership } from '../enclosure/guards.mjs'
export { closeHabitatGates } from '../enclosure/guards.mjs'
export const habitatThreats = (api, plan) => threats(api, plan, 'breeder')
export const habitatOwnership = (api, plan, preflight) => ownership(api, plan, preflight, 'breeder')

export function habitatSecure (api, plan, material, gateItem, openEntry = false) {
  const state = breedPreflight(plan, api.block, material, gateItem)
  if (state.needed.length || state.missingBeds.length || state.clear.length) throw new Error('breeder enclosure, lights or beds are no longer complete')
  requireClosedGates(api, plan, openEntry, 'breeder gate is not observed closed; no food delivered')
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
