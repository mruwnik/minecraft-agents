import { cellKey } from './blocks.mjs'

// Physical room layout shared by breeding and boat arrival; callers validate their policy.
export function roomPlan ({ x, y, z, width, entry = null, airlock = false }) {
  const at = (dx, dy, dz) => ({ x: x + dx, y: y + dy, z: z + dz })
  const gate = at(-1, 0, Math.floor(width / 2))
  const innerGate = airlock ? at(width - 3, 0, width - 1) : null
  const shell = []
  for (let dx = -1; dx <= width; dx++) for (let dz = -1; dz <= width; dz++) {
    if (dx === -1 || dx === width || dz === -1 || dz === width) for (let dy = 0; dy < 3; dy++) {
      if (dx === -1 && dz === gate.z - z && dy < 2) continue
      if (entry && dx === width && dz === entry.z - z && dy < 2) continue
      shell.push(at(dx, dy, dz))
    }
    shell.push(at(dx, 3, dz))
  }
  if (airlock) {
    for (const dx of [width - 3, width - 2, width - 1]) for (let dy = 0; dy < 3; dy++) shell.push(at(dx, dy, width - 2))
    shell.push({ ...innerGate, y: y + 2 })
  }
  const lights = []
  const xs = [...new Set([...Array.from({ length: Math.ceil(width / 4) }, (_, i) => Math.min(width - 1, 1 + i * 4)), width - 1])]
  // These floor torches cover the unobstructed sleeping room. Exterior roof
  // lighting needs an actual access route and is not claimed by this plan.
  for (let dz = 2; dz < width; dz += 3) for (const dx of xs) {
    const p = at(dx, 0, dz)
    if (!shell.some(q => cellKey(q) === cellKey(p))) lights.push(p)
  }
  return { x, y, z, width, gate, entry, airlock, innerGate, gates: [gate, ...(entry ? [entry] : []), ...(innerGate ? [innerGate] : [])], shell, lights, center: at(Math.floor(width / 2), 0, Math.floor(width / 2)) }
}

export function roomInside (plan, p) {
  return !(plan.airlock && p.x >= plan.innerGate.x && p.z >= plan.z + plan.width - 2) && p.x >= plan.x && p.x < plan.x + plan.width && p.z >= plan.z && p.z < plan.z + plan.width && p.y >= plan.y - 0.1 && p.y < plan.y + 3
}
