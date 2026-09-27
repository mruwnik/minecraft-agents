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
