import { inAnyZone, workRefusal } from './lib.mjs'
import { breedPreflight, breedKey } from './villager-breed.mjs'

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
    if (b?.name === gateItem && b.properties?.open === true) await (cleanup ? api.cleanupAct : api.act)('toggle', { ...p, open: false })
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
  if (plan.innerGate && body.x >= plan.innerGate.x && body.x < plan.entry.x + 1 && body.z >= plan.entry.z && body.z < plan.entry.z + 1 && body.y >= plan.y - 0.1 && body.y < plan.y + 1 && api.block(plan.innerGate.x, plan.innerGate.y, plan.innerGate.z)?.name === gateItem) {
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
export async function buildHabitat (api, plan, preflight, { material, gateItem, bedItem }, refresh = async () => {}) {
  try {
    for (const p of preflight.clear) {
      await refresh()
      const b = api.block(p.x, p.y, p.z)
      if (!['air', 'cave_air', 'void_air'].includes(b?.name)) {
        if (b?.name !== p.name) throw new Error(`planned vegetation cell changed at ${breedKey(p)}; no digging`)
        await api.act('dig', { x: p.x, y: p.y, z: p.z })
      }
      if (!['air', 'cave_air', 'void_air'].includes(api.block(p.x, p.y, p.z)?.name)) throw new Error(`planned vegetation removal did not clear ${breedKey(p)}`)
      await closeHabitatGates(api, plan, gateItem)
    }
    // Starting outside lets a finished wall column obscure the next floor
    // face even when the generic placement range check passes. Begin inside
    // the clear room so wall placement proceeds from its accessible side.
    await enterHabitatMain(api, plan, gateItem)
    for (const p of preflight.needed.filter(p => p.item !== 'torch')) {
      await refresh()
      if (p.item === gateItem) {
        // Place from the adjacent dry side, never from the gate's own cell
        // or the far end of a roofed service booth.
        const stand = { x: p.x + (breedKey(p) === breedKey(plan.gate) ? 1 : -1), y: p.y, z: p.z }
        const feet = api.block(stand.x, stand.y, stand.z)
        const head = api.block(stand.x, stand.y + 1, stand.z)
        if (!['air', 'cave_air', 'void_air', 'torch'].includes(feet?.name) || !['air', 'cave_air', 'void_air'].includes(head?.name)) throw new Error(`gate placement needs clear adjacent stance at ${breedKey(stand)}`)
        await api.act('goto', { ...stand, range: 0, into: true })
        await closeHabitatGates(api, plan, gateItem)
      }
      await api.act('place', { ...p })
      await closeHabitatGates(api, plan, gateItem)
      if (api.block(p.x, p.y, p.z)?.name !== p.item) throw new Error(`breeder placement not observed at ${breedKey(p)}`)
    }
    await enterHabitatMain(api, plan, gateItem)
    for (const b of preflight.missingBeds) {
      await refresh()
      const stand = { ...b.foot, x: b.foot.x - 1 }
      const feet = api.block(stand.x, stand.y, stand.z)
      if (!['air', 'cave_air', 'void_air', 'torch'].includes(feet?.name) || !['air', 'cave_air', 'void_air'].includes(api.block(stand.x, stand.y + 1, stand.z)?.name)) throw new Error(`bed placement needs clear adjacent aisle at ${breedKey(stand)}`)
      await api.act('goto', { ...stand, range: 0, into: true })
      await closeHabitatGates(api, plan, gateItem)
      await api.act('place', { ...b.foot, item: bedItem, facing: 'south' })
      await closeHabitatGates(api, plan, gateItem)
    }
    for (const p of preflight.needed.filter(p => p.item === 'torch')) {
      await refresh()
      await api.act('place', { ...p })
      await closeHabitatGates(api, plan, gateItem)
    }
    await enterHabitatMain(api, plan, gateItem)
    habitatSecure(api, plan, material, gateItem)
    await refresh()
  } catch (error) {
    try { await closeHabitatGates(api, plan, gateItem, true) } catch (closeError) { throw new Error(`${error.message}; gate closure pending: ${closeError.message}`) }
    throw error
  }
}
export function habitatMetadata (plan) {
  return { target: plan.target, size: plan.width, x: plan.x, y: plan.y, z: plan.z, gate: plan.gate, entry: plan.entry, airlock: plan.airlock, innerGate: plan.innerGate, insideEntry: plan.entry ? { ...plan.entry, x: plan.entry.x - 1 } : { ...plan.gate, x: plan.gate.x + 1 }, bedCount: plan.beds.length, roofY: plan.y + 3 }
}
