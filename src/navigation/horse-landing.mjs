import { createTerrainGeometry, terrainProfile } from './terrain.mjs'

// Ordinary dry full-block footing only, but clearance follows collision
// shapes: leaf litter and other empty groundcover need not be removed.
export function dryHorseLanding (blockAt, position) {
  if (!['x', 'y', 'z'].every(key => Number.isFinite(position?.[key]))) return false
  const y = position.y
  if (!Number.isInteger(y)) return false
  const half = 0.32
  for (let x = Math.floor(position.x - half); x <= Math.floor(position.x + half); x++) {
    for (let z = Math.floor(position.z - half); z <= Math.floor(position.z + half); z++) {
      const floor = terrainProfile(blockAt(x, y - 1, z))
      if (!floor.loaded || floor.hazardous || floor.liquid || floor.crop || floor.leaf ||
          !floor.support.some(s => s[0] === 0 && s[1] === 0 && s[2] === 0 && s[3] === 1 && s[4] === 1 && s[5] === 1)) return false
    }
  }
  return createTerrainGeometry(blockAt, { dry: true, openDoors: false, avoidCrops: true })
    .clearBox([position.x - half, y, position.z - half, position.x + half, y + 1.85, position.z + half])
}

// Verified against AbstractHorse.getDismountLocationForPassenger and
// BlockGetter.getBlockFloorHeight in the local 26.2 server. Horses first try
// the side determined by THEIR yaw and the rider's main hand. An arbitrary
// safe neighbour says nothing about the landing the server will select.
// We accept only the first standing candidate on that preferred side. If no
// such candidate exists, crouching/opposite-side/fallback exits stay unproved.
export function checkedHorseLanding (blockAt, horse, { mainHand = 'right' } = {}) {
  const p = horse.position
  if (![p?.x, p?.y, p?.z, horse.yaw, horse.width, horse.height].every(Number.isFinite) ||
      horse.width <= 0 || horse.width > 1.4 || horse.height <= 0 || horse.height > 1.61 || !['left', 'right'].includes(mainHand)) throw new Error('horse landing needs confirmed dimensions, heading and rider main hand')
  const yaw = Math.PI - horse.yaw + (mainHand === 'right' ? Math.PI / 2 : -Math.PI / 2)
  const dx = -Math.sin(yaw), dz = Math.cos(yaw), reach = (horse.width + 0.6 + 0.00001) / 2
  const x = p.x + dx / Math.max(Math.abs(dx), Math.abs(dz)) * reach
  const z = p.z + dz / Math.max(Math.abs(dx), Math.abs(dz)) * reach
  const cache = new Map()
  const at = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!cache.has(key)) {
      const block = blockAt(x, y, z)
      if (!block) throw new Error(`horse landing cannot predict unloaded cell ${key}; remain mounted`)
      if (block.name === 'scaffolding') throw new Error(`horse landing cannot predict context-dependent scaffolding ${key}; remain mounted`)
      cache.set(key, terrainProfile(block, { openDoors: false }))
    }
    return cache.get(key)
  }
  // Vanilla checks physical collision before its next candidate. Hazards must
  // not masquerade as collision and cause us to skip an unsafe first landing.
  const physicalClear = candidate => {
    const box = [candidate.x - 0.3, candidate.y, candidate.z - 0.3, candidate.x + 0.3, candidate.y + 1.8, candidate.z + 0.3]
    for (let bx = Math.floor(box[0]); bx <= Math.floor(box[3] - 1e-7); bx++) for (let bz = Math.floor(box[2]); bz <= Math.floor(box[5] - 1e-7); bz++) for (let by = Math.floor(box[1]) - 1; by <= Math.floor(box[4] - 1e-7); by++) {
      if (at(bx, by, bz).shapes.some(s => box[0] < bx + s[3] - 1e-7 && box[3] > bx + s[0] + 1e-7 && box[1] < by + s[4] - 1e-7 && box[4] > by + s[1] + 1e-7 && box[2] < bz + s[5] - 1e-7 && box[5] > bz + s[2] + 1e-7)) return false
    }
    return true
  }
  const upper = p.y + horse.height + 0.75, bx = Math.floor(x), bz = Math.floor(z)
  for (let by = Math.floor(p.y); by < upper; by++) {
    const shapes = at(bx, by, bz).shapes
    let floor
    if (shapes.length) floor = Math.max(...shapes.map(s => s[4]))
    else {
      const below = at(bx, by - 1, bz).shapes
      const top = below.length ? Math.max(...below.map(s => s[4])) : -Infinity
      floor = top >= 1 ? top - 1 : -Infinity
    }
    if (by + floor > upper) break
    if (!Number.isFinite(floor) || floor >= 1) continue
    const candidate = { x, y: by + floor, z }
    if (!physicalClear(candidate)) continue
    if (!dryHorseLanding(blockAt, candidate)) throw new Error(`server's first horse landing is not checked dry full-footprint footing at ${JSON.stringify(candidate)}; remain mounted`)
    return { ...candidate, side: mainHand, horseYaw: horse.yaw }
  }
  throw new Error(`no checked preferred standing horse exit near ${JSON.stringify({ x, z })}; server fallback is unproved, remain mounted`)
}
