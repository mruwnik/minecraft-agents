// Physical state geometry shared by local frontier planning and native walking.
// Names express gameplay semantics (damage, fluids, handles, climbing), never a
// list of decorative blocks which are allowed to be air.
import { breaksUnderfoot } from '../lib/path.mjs'

const FULL = [[0, 0, 0, 1, 1, 1]]
const EPS = 1e-7
export const PLAYER_HALF = 0.3001
export const PLAYER_HEIGHT = 1.8
const intersects = (a, b) => a[0] < b[3] - EPS && a[3] > b[0] + EPS && a[1] < b[4] - EPS && a[4] > b[1] + EPS && a[2] < b[5] - EPS && a[5] > b[2] + EPS
const horizontal = shape => shape[0] < 0.5 + PLAYER_HALF && shape[3] > 0.5 - PLAYER_HALF && shape[2] < 0.5 + PLAYER_HALF && shape[5] > 0.5 - PLAYER_HALF
const propertiesOf = block => block?.properties ?? block?.getProperties?.() ?? {}

// Closed wooden doors can be planned in their opened state because doorTick
// operates their handle. Preserve the swung panel: crossing its hinge side is
// still a collision. Unknown door states are not guessed.
function openedDoor (properties) {
  const right = properties.hinge === 'right'
  if (!['left', 'right'].includes(properties.hinge)) return null
  if (properties.facing === 'north') return right ? [[0.8125, 0, 0, 1, 1, 1]] : [[0, 0, 0, 0.1875, 1, 1]]
  if (properties.facing === 'south') return right ? [[0, 0, 0, 0.1875, 1, 1]] : [[0.8125, 0, 0, 1, 1, 1]]
  if (properties.facing === 'east') return right ? [[0, 0, 0.8125, 1, 1, 1]] : [[0, 0, 0, 1, 1, 0.1875]]
  if (properties.facing === 'west') return right ? [[0, 0, 0, 1, 1, 0.1875]] : [[0, 0, 0.8125, 1, 1, 1]]
  return null
}

export function terrainProfile (block, { openDoors = true, scaffolding = false, climbableVines = false } = {}) {
  if (!block) return { loaded: false, shapes: [], support: [], reason: 'unloaded terrain' }
  const name = block.name, properties = propertiesOf(block)
  const liquid = /^(?:water|flowing_water|bubble_column|kelp|kelp_plant|seagrass|tall_seagrass)$/.test(name) || properties.waterlogged === true
  const hazardous = /^(?:lava|flowing_lava|fire|soul_fire|powder_snow|magma_block|cactus|wither_rose|cobweb|nether_portal|end_portal|end_gateway|pointed_dripstone)$/.test(name) ||
    /^(?:campfire|soul_campfire)$/.test(name) && properties.lit !== false || name === 'sweet_berry_bush' && Number(properties.age ?? 1) > 0
  const crop = breaksUnderfoot(name)
  const door = /_door$/.test(name), gate = /_fence_gate$/.test(name)
  const opening = openDoors && properties.open !== true && ((door && name !== 'iron_door') || gate)
  let shapes = Array.isArray(block.shapes) ? block.shapes : block.solid === true || block.boundingBox === 'block' ? FULL : []
  if (opening && gate) shapes = []
  if (opening && door) shapes = openedDoor(properties) ?? shapes
  // Legacy injected readers can report an open passage as solid without shapes.
  if (!Array.isArray(block.shapes) && properties.open === true && (door || gate)) shapes = []
  const modernVine = /^(?:weeping_vines|twisting_vines|cave_vines)(?:_plant)?$/.test(name)
  const climbable = name === 'ladder' || name === 'vine' || scaffolding && name === 'scaffolding' || climbableVines && modernVine
  const scaffold = name === 'scaffolding'
  const noSupport = name === 'bamboo' || /(?:_fence|_wall|_fence_gate|_door)$/.test(name) || hazardous
  const support = noSupport ? [] : shapes.filter(shape => shape[4] <= 1 + EPS && shape[4] > EPS && horizontal(shape))
  // Registry scaffolding shapes omit this context-dependent collision; the
  // matching physics adapter supplies the suspended bottom's landing plate.
  if (scaffold && scaffolding && properties.bottom === true && Number(properties.distance) > 0) support.push([0, 0, 0, 1, 0.125, 1])
  // Scaffolding's collision depends on whether the body approaches from above;
  // its validated physics adapter supplies that behavior. Inside it is climbable.
  const body = scaffold && scaffolding ? [] : shapes
  const reason = hazardous ? `hazard: ${name}` : liquid ? `fluid: ${name}` : (scaffold && !scaffolding || modernVine && !climbableVines) ? `climbing ${name} requires a verified physics/controller adapter` : null
  return { loaded: true, name, properties, shapes: body, support, liquid, hazardous, crop, climbable, scaffold, opening, reason,
    leaf: /_leaves$/.test(name), noSupport }
}

export const terrainClimbable = (feet, below) => feet.climbable ||
  /_trapdoor$/.test(feet.name ?? '') && feet.properties.open === true && below.name === 'ladder' &&
  feet.properties.facing !== undefined && feet.properties.facing === below.properties.facing

export function createTerrainGeometry (blockAt, { openDoors = true, scaffolding = false, climbableVines = false, dry = true, avoidCrops = true, leaves = false } = {}) {
  const profiles = new WeakMap()
  const profile = block => {
    if (!block || typeof block !== 'object') return terrainProfile(null)
    if (!profiles.has(block)) profiles.set(block, terrainProfile(block, { openDoors, scaffolding, climbableVines }))
    return profiles.get(block)
  }
  const get = (x, y, z) => profile(blockAt(x, y, z))
  const clearBox = box => {
    for (let x = Math.floor(box[0]); x <= Math.floor(box[3] - EPS); x++) for (let z = Math.floor(box[2]); z <= Math.floor(box[5] - EPS); z++) {
      // A fence/wall shape can project half a block above its own cell.
      for (let y = Math.floor(box[1]) - 1; y <= Math.floor(box[4] - EPS); y++) {
        const p = get(x, y, z)
        if (!p.loaded) return false
        const cell = [x, y, z, x + 1, y + 1, z + 1]
        if (intersects(box, cell) && (p.hazardous || dry && p.liquid || avoidCrops && p.crop)) return false
        if (p.shapes.some(shape => intersects(box, [x + shape[0], y + shape[1], z + shape[2], x + shape[3], y + shape[4], z + shape[5]]))) return false
      }
    }
    return true
  }
  const bodyBox = (x, height, z) => [x + 0.5 - PLAYER_HALF, height, z + 0.5 - PLAYER_HALF, x + 0.5 + PLAYER_HALF, height + PLAYER_HEIGHT, z + 0.5 + PLAYER_HALF]
  const stand = (x, y, z, { climb = true } = {}) => {
    const below = get(x, y - 1, z), feet = get(x, y, z)
    if (!below.loaded || !feet.loaded) return null
    const climbable = terrainClimbable(feet, below)
    const surfaces = []
    const footCap = feet.scaffold && scaffolding && feet.properties.bottom === true && Number(feet.properties.distance) > 0 ? 0.125 : 0.1
    for (const [p, base, cap] of [[below, y - 1, y], [feet, y, y + footCap]]) {
      if (p.hazardous || p.leaf && !leaves) continue
      for (const shape of p.support) {
        const height = base + shape[4]
        if (height <= cap + EPS) surfaces.push({ height, support: p })
      }
    }
    surfaces.sort((a, b) => b.height - a.height)
    for (const surface of surfaces) if (clearBox(bodyBox(x, surface.height, z))) return { x, y, z, ...surface, grounded: true, climbable }
    if (!dry && feet.liquid && !feet.hazardous && clearBox(bodyBox(x, y, z))) return { x, y, z, height: y, grounded: false, swimming: true, climbable: false, support: null }
    if (climb && climbable && !feet.hazardous && !(dry && feet.liquid) && clearBox(bodyBox(x, y, z))) return { x, y, z, height: y, grounded: false, climbable: true, support: null }
    return null
  }
  const edge = (from, to, { diagonal = false, maxDrop = 1 } = {}) => {
    const dx = to.x - from.x, dz = to.z - from.z, rise = to.height - from.height
    if (Math.abs(dx) + Math.abs(dz) > (diagonal ? 2 : 1) || Math.abs(dx) > 1 || Math.abs(dz) > 1 || rise > 1.01 || -rise > maxDrop + 0.01) return false
    if (to.support?.name === 'farmland' && rise < -1.01) return false
    if (!dx && !dz && !(from.climbable || to.climbable)) return false
    if (rise < 0) for (let y = Math.floor(to.height); y <= Math.floor(from.height); y++) {
      const p = get(to.x, y, to.z)
      if (p.scaffold && p.properties.bottom === true && Number(p.properties.distance) > 0 && from.height >= y + 0.125 && to.height < y + 0.125) return false
    }
    // Lift before crossing an upward step; cross before dropping a downward
    // step. The swept body prevents clipping a thin side panel or overhang.
    const high = Math.max(from.height, to.height)
    const a = bodyBox(from.x, from.height, from.z), b = bodyBox(to.x, to.height, to.z)
    if (rise > 0 && !clearBox([a[0], from.height, a[2], a[3], high + PLAYER_HEIGHT, a[5]])) return false
    if (rise < 0 && !clearBox([b[0], to.height, b[2], b[3], high + PLAYER_HEIGHT, b[5]])) return false
    return clearBox([Math.min(a[0], b[0]), high, Math.min(a[2], b[2]), Math.max(a[3], b[3]), high + PLAYER_HEIGHT, Math.max(a[5], b[5])])
  }
  return { profile, get, clearBox, stand, edge, bodyBox }
}

export function patchTerrainWaypoints (source) {
  const marker = '// terrain-aware waypoint adapter'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '      const b = bot.blockAt(new Vec3(curPoint.x, curPoint.y, curPoint.z))'
  if (!source.includes(anchor)) return { status: 'anchor missing', source }
  return { status: 'patched', source: source.replace(anchor, `      ${marker}\n      if (stateMovements.resolveTerrainWaypoint) {\n        const terrainPoint = stateMovements.resolveTerrainWaypoint(curPoint)\n        if (terrainPoint) { Object.assign(curPoint, terrainPoint); continue }\n      }\n${anchor}`) }
}

// Native emptyBlocks is indexed by block type, so a snow type whose minimum
// state has no collision also misclassifies its thicker, collidable states.
export function patchTerrainStart (source) {
  const marker = '// terrain-aware grounded start adapter'
  if (source.includes(marker)) return { status: 'already patched', source }
  const anchor = '      start = new Move(p.x, p.y + offset, p.z, movements.countScaffoldingItems(), 0)'
  if (!source.includes(anchor)) return { status: 'anchor missing', source }
  const replacement = `      ${marker}\n      const terrainStart = movements.resolveTerrainStart?.(startPos, bot.entity.onGround)\n      start = new Move(terrainStart?.x ?? p.x, terrainStart?.y ?? (p.y + offset), terrainStart?.z ?? p.z, movements.countScaffoldingItems(), 0)`
  return { status: 'patched', source: source.replace(anchor, replacement) }
}
