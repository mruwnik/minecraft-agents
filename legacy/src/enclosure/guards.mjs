import { inAnyZone } from '../lib/world.mjs'
import { workRefusal } from '../lib/places.mjs'
import { woodenGate } from './blocks.mjs'

export async function habitatThreats (api, plan, label = 'enclosure') {
  const found = (await api.act('entity', { name: '*', hostile: true, uuid: true, count: 1000 })).found ?? []
  if (found.length >= 1000) throw new Error('hostile census reached its observation limit')
  for (const e of found) {
    const [x, y, z] = String(e.exact ?? e.at).split(',').map(Number)
    if (![x, y, z].every(Number.isFinite)) throw new Error('nearby hostile has no confirmed position')
    const dx = Math.max(plan.x - 1 - x, 0, x - (plan.x + plan.width + 1))
    const dz = Math.max(plan.z - 1 - z, 0, z - (plan.z + plan.width + 1))
    if (Math.hypot(dx, dz) <= 8 && y >= plan.y - 3 && y <= plan.y + 8) throw new Error(`hostile ${e.uuid ?? e.id} near ${label} at ${e.exact ?? e.at}; secure the area before work`)
  }
}
export function habitatOwnership (api, plan, preflight, label = 'enclosure') {
  const points = [...plan.shell, ...plan.gates, ...(plan.lights ?? []), ...(plan.beds ?? []).flatMap(b => [b.foot, b.head]), ...(preflight.clear ?? [])]
  const foreign = api.zones().find(z => points.some(p => inAnyZone([z], p)) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase()))
  if (foreign) throw new Error(`${label} crosses protected zone ${foreign.name}`)
  const owner = api.places().filter(p => Math.hypot(p.x - plan.x, p.z - plan.z) <= (p.radius ?? 8) + plan.width).map(p => workRefusal(p, api.me())).find(Boolean)
  if (owner) throw new Error(owner)
}
export async function closeHabitatGates (api, plan, gateItem, cleanup = false) {
  for (const p of plan.gates) {
    const b = api.block(p.x, p.y, p.z)
    if (woodenGate(b?.name) && b.properties?.open === true) await (cleanup ? api.cleanupAct : api.act)('toggle', { ...p, open: false })
  }
}

export function requireClosedGates (api, plan, openEntry = false, message = "enclosure gate is not closed") {
  for (const p of plan.gates) {
    if ((openEntry === true && (p === plan.entry || p === plan.innerGate)) || (openEntry === 'outer' && p === plan.entry)) continue
    if (api.block(p.x, p.y, p.z)?.properties?.open !== false) throw new Error(message)
  }
}
