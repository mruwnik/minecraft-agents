import { requireClosedGates } from '../enclosure/guards.mjs'
import { safeFullBlock, cellKey } from '../enclosure/blocks.mjs'
import { roomPlan, roomInside } from '../enclosure/layout.mjs'
import { entityUuid } from './passenger.mjs'

export function boatHabitatPlan (a) {
  const { x, y, z, entryX, entryZ } = a
  const width = a.size ?? 8
  if (![x, y, z, width, entryX, entryZ].every(Number.isInteger) || width < 8 || width > 15 || entryX !== x + width || entryZ !== z + width - 1 || a.airlock !== true) throw new Error('boat.receive requires a size=8..15 room with airlock=true and an east entry on its last row')
  return { ...roomPlan({ x, y, z, width, entry: { x: entryX, y, z: entryZ }, airlock: true }), beds: [] }
}
export const boatHabitatInside = roomInside
export function boatHabitatPreflight (p, blockAt) {
  const read = q => { const b = blockAt(q.x, q.y, q.z); if (!b) throw new Error(`unloaded enclosure at ${cellKey(q)}`); return b }
  for (const q of p.shell) if (!safeFullBlock(read(q))) throw new Error(`arrival enclosure wall or roof incomplete at ${cellKey(q)}`)
  for (let dx = -1; dx <= p.width; dx++) for (let dz = -1; dz <= p.width; dz++) if (!safeFullBlock(read({ x: p.x + dx, y: p.y - 1, z: p.z + dz }))) throw new Error('arrival enclosure needs a safe full floor')
  for (const q of p.gates) {
    const b = read(q)
    if (!/_fence_gate$/.test(b.name) || !['east', 'west'].includes(b.properties?.facing)) throw new Error('arrival enclosure needs east/west wooden fence gates')
    if (!['air', 'cave_air', 'void_air'].includes(read({ ...q, y: q.y + 1 }).name)) throw new Error('arrival gate needs clear headroom')
  }
  for (const q of p.lights) if (read(q).name !== 'torch') throw new Error(`arrival enclosure floor light missing at ${cellKey(q)}`)
  const reserved = new Set([...p.shell, ...p.gates.flatMap(q => [q, { ...q, y: q.y + 1 }])].map(cellKey))
  for (let dx = 0; dx < p.width; dx++) for (let dz = 0; dz < p.width; dz++) for (let dy = 0; dy < 3; dy++) {
    const q = { x: p.x + dx, y: p.y + dy, z: p.z + dz }
    if (reserved.has(cellKey(q))) continue
    const b = read(q)
    if (['air', 'cave_air', 'void_air', 'torch', 'wall_torch'].includes(b.name) || (dy === 0 && /_bed$/.test(b.name))) continue
    throw new Error(`arrival room blocked at ${cellKey(q)} by ${b.name}`)
  }
  return { needed: [], missingBeds: [], clear: [] }
}
export function boatHabitatCensus (p, rows) {
  return rows.filter(e => ['villager', 'cow', 'sheep', 'pig'].includes(e.name)).map(e => {
    if (!entityUuid(e.uuid)) throw new Error('resident UUID is unknown')
    const [x, y, z] = String(e.exact ?? e.at).split(',').map(Number)
    if (![x, y, z].every(Number.isFinite)) throw new Error('resident position is unknown')
    return { ...e, position: { x, y, z } }
  }).filter(e => boatHabitatInside(p, e.position)).map(e => {
    if (e.vehicleId != null) throw new Error('existing resident must be observed on foot')
    return e
  })
}
export function boatHabitatSecure (api, p, _material, _gate, openEntry = false) {
  boatHabitatPreflight(p, api.block)
  requireClosedGates(api, p, openEntry, 'arrival enclosure gate is not closed')
}
