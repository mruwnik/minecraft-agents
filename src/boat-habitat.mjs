import { breedFullBlock, breedKey } from './villager-breed.mjs'
import { entityUuid } from './lib/boat-passenger.mjs'

export function boatHabitatPlan (a) {
  const { x, y, z, entryX, entryZ } = a
  const width = a.size ?? 8
  if (![x, y, z, width, entryX, entryZ].every(Number.isInteger) || width < 8 || width > 15 || entryX !== x + width || entryZ !== z + width - 1 || a.airlock !== true) throw new Error('boat.receive requires a size=8..15 room with airlock=true and an east entry on its last row')
  const gate = { x: x - 1, y, z: z + Math.floor(width / 2) }
  const entry = { x: entryX, y, z: entryZ }
  const innerGate = { x: x + width - 3, y, z: entryZ }
  const shell = []
  for (let dx = -1; dx <= width; dx++) for (let dz = -1; dz <= width; dz++) {
    if (dx === -1 || dx === width || dz === -1 || dz === width) for (let dy = 0; dy < 3; dy++) {
      if (dy < 2 && ((dx === -1 && dz === gate.z - z) || (dx === width && dz === entry.z - z))) continue
      shell.push({ x: x + dx, y: y + dy, z: z + dz })
    }
    shell.push({ x: x + dx, y: y + 3, z: z + dz })
  }
  for (const dx of [width - 3, width - 2, width - 1]) for (let dy = 0; dy < 3; dy++) shell.push({ x: x + dx, y: y + dy, z: z + width - 2 })
  shell.push({ ...innerGate, y: y + 2 })
  const lights = []
  const xs = [...new Set([...Array.from({ length: Math.ceil(width / 4) }, (_, i) => Math.min(width - 1, 1 + i * 4)), width - 1])]
  for (let dz = 2; dz < width; dz += 3) for (const dx of xs) {
    const q = { x: x + dx, y, z: z + dz }
    if (!shell.some(s => breedKey(s) === breedKey(q))) lights.push(q)
  }
  return { x, y, z, width, airlock: true, gate, entry, innerGate, gates: [gate, entry, innerGate], shell, beds: [], lights, center: { x: x + Math.floor(width / 2), y, z: z + Math.floor(width / 2) } }
}
export function boatHabitatInside (p, q) {
  return q.x >= p.x && q.x < p.x + p.width && q.z >= p.z && q.z < p.z + p.width && q.y >= p.y - 0.1 && q.y < p.y + 3 && !(q.x >= p.innerGate.x && q.z >= p.z + p.width - 2)
}
export function boatHabitatPreflight (p, blockAt) {
  const read = q => { const b = blockAt(q.x, q.y, q.z); if (!b) throw new Error(`unloaded enclosure at ${breedKey(q)}`); return b }
  for (const q of p.shell) if (!breedFullBlock(read(q))) throw new Error(`arrival enclosure wall or roof incomplete at ${breedKey(q)}`)
  for (let dx = -1; dx <= p.width; dx++) for (let dz = -1; dz <= p.width; dz++) if (!breedFullBlock(read({ x: p.x + dx, y: p.y - 1, z: p.z + dz }))) throw new Error('arrival enclosure needs a safe full floor')
  for (const q of p.gates) {
    const b = read(q)
    if (!/_fence_gate$/.test(b.name) || !['east', 'west'].includes(b.properties?.facing)) throw new Error('arrival enclosure needs east/west wooden fence gates')
    if (!['air', 'cave_air', 'void_air'].includes(read({ ...q, y: q.y + 1 }).name)) throw new Error('arrival gate needs clear headroom')
  }
  for (const q of p.lights) if (read(q).name !== 'torch') throw new Error(`arrival enclosure floor light missing at ${breedKey(q)}`)
  const reserved = new Set([...p.shell, ...p.gates.flatMap(q => [q, { ...q, y: q.y + 1 }])].map(breedKey))
  for (let dx = 0; dx < p.width; dx++) for (let dz = 0; dz < p.width; dz++) for (let dy = 0; dy < 3; dy++) {
    const q = { x: p.x + dx, y: p.y + dy, z: p.z + dz }
    if (reserved.has(breedKey(q))) continue
    const b = read(q)
    if (['air', 'cave_air', 'void_air', 'torch', 'wall_torch'].includes(b.name) || (dy === 0 && /_bed$/.test(b.name))) continue
    throw new Error(`arrival room blocked at ${breedKey(q)} by ${b.name}`)
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
  for (const q of p.gates) {
    if ((openEntry === true && (q === p.entry || q === p.innerGate)) || (openEntry === 'outer' && q === p.entry)) continue
    if (api.block(q.x, q.y, q.z)?.properties?.open !== false) throw new Error('arrival enclosure gate is not closed')
  }
}
