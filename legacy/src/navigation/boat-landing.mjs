import { createTerrainGeometry, terrainProfile } from './terrain.mjs'

const wrapped = angle => Math.atan2(Math.sin(angle), Math.cos(angle))
// Matches the local server's AbstractBoat dismount rule: a square escape
// vector based on passenger look direction, then the boat-top cell or one
// below it. Restrict landings to ordinary full-block, dry standing platforms.
export function checkedBoatLanding (blockAt, boat, target) {
  if (![boat.position, target].every(p => ['x', 'y', 'z'].every(k => Number.isFinite(p?.[k]))) ||
      !Number.isFinite(boat.yaw) || Math.abs((boat.width ?? 1.375) - 1.375) > 0.001 || Math.abs((boat.height ?? 0.5625) - 0.5625) > 0.001) throw new Error('boat landing needs a known ordinary boat pose and finite landing coordinates')
  if (!['x', 'y', 'z'].every(k => Number.isInteger(target[k]))) throw new Error('boat landing target must name an integer dry feet cell')
  const dx = target.x + 0.5 - boat.position.x, dz = target.z + 0.5 - boat.position.z
  if (Math.hypot(dx, dz) < 0.01) throw new Error('boat landing needs a shore beside the boat')
  const yaw = Math.atan2(-dx, -dz)
  // Vanilla clamps a passenger's relative yaw to 105 degrees. Keep additional
  // margin instead of predicting an exit using a look the server will reject.
  if (Math.abs(wrapped(yaw - boat.yaw)) > Math.PI / 2) throw new Error('landing is behind the boat; approach the shore facing it first')
  const reach = (1.375 * Math.SQRT2 + 0.6 + 0.00001) / 2
  const divisor = Math.max(Math.abs(dx), Math.abs(dz))
  const x = boat.position.x + dx / divisor * reach, z = boat.position.z + dz / divisor * reach
  const upper = Math.floor(boat.position.y + 0.5625)
  if (![upper, upper - 1].includes(target.y) || Math.floor(x) !== target.x || Math.floor(z) !== target.z) throw new Error('the server dismount point does not reach the requested shore cell')
  const underUpper = terrainProfile(blockAt(Math.floor(x), upper - 1, Math.floor(z)))
  if (!underUpper.loaded || underUpper.liquid) throw new Error('the server would fall back to a water dismount; remain aboard')
  const half = 0.32
  for (let bx = Math.floor(x - half); bx <= Math.floor(x + half); bx++) for (let bz = Math.floor(z - half); bz <= Math.floor(z + half); bz++) {
    const floor = terrainProfile(blockAt(bx, target.y - 1, bz))
    if (!floor.loaded || floor.hazardous || floor.liquid || floor.crop || floor.leaf || !floor.support.some(s => s[0] === 0 && s[1] === 0 && s[2] === 0 && s[3] === 1 && s[4] === 1 && s[5] === 1)) throw new Error('boat landing needs dry full-block support beneath the entire player')
  }
  const geometry = createTerrainGeometry(blockAt, { dry: true, openDoors: false, avoidCrops: true })
  if (!geometry.clearBox([x - half, target.y, z - half, x + half, target.y + 1.85, z + half])) throw new Error('boat landing has obstructed player clearance')
  // Requiring empty space at this height excludes a higher partial surface
  // which vanilla would consider first. The accepted server floor is exactly y.
  return { x, y: target.y, z, yaw }
}
