// Visibility to the exact face cursor used by Mineflayer's generic_place.
// Unlike farm top clicks, construction also uses side and underside faces.
import prismarineBlock from 'prismarine-block'
import minecraftData from 'minecraft-data'

const Block = prismarineBlock('26.1')
const registry = minecraftData('26.1')
const cache = new Map()
const EPS = 1e-7
const fluids = /^(water|lava|flowing_water|flowing_lava|bubble_column)$/
const faces = [[0, -1, 0], [0, 1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]

const shapesOf = block => {
  if (!block) return [[0, 0, 0, 1, 1, 1]] // unknown is opaque
  if (fluids.test(block.name)) return []
  if (block.shapes) return block.shapes
  const properties = block.getProperties?.() ?? block.properties ?? {}
  const key = `${block.name}:${JSON.stringify(properties)}`
  if (!cache.has(key)) {
    try {
      const defaults = Block.fromStateId(registry.blocksByName[block.name].defaultState).getProperties()
      cache.set(key, Block.fromProperties(block.name, { ...defaults, ...properties }, 0).shapes)
    } catch { cache.set(key, [[0, 0, 0, 1, 1, 1]]) }
  }
  return cache.get(key)
}

// Exact segment/AABB intersection; touching the cursor's face at t=1 is allowed.
const intersects = (from, delta, box, cell) => {
  let lo = 0; let hi = 1 - EPS
  for (const [axis, i] of [['x', 0], ['y', 1], ['z', 2]]) {
    const min = cell[axis] + box[i]; const max = cell[axis] + box[i + 3]
    if (Math.abs(delta[axis]) < EPS) {
      if (from[axis] <= min + EPS || from[axis] >= max - EPS) return false
    } else {
      const a = (min - from[axis]) / delta[axis]; const b = (max - from[axis]) / delta[axis]
      lo = Math.max(lo, Math.min(a, b) + EPS); hi = Math.min(hi, Math.max(a, b) - EPS)
      if (lo > hi) return false
    }
  }
  return hi >= 0 && lo <= hi
}

export function clearClickRay (worldAt, from, point, reach = 4.5, reference = null) {
  const delta = { x: point.x - from.x, y: point.y - from.y, z: point.z - from.z }
  if (Math.hypot(delta.x, delta.y, delta.z) > reach) return false
  const cell = { x: Math.floor(from.x), y: Math.floor(from.y), z: Math.floor(from.z) }
  const step = {}; const next = {}; const stride = {}
  for (const axis of ['x', 'y', 'z']) {
    step[axis] = Math.sign(delta[axis])
    stride[axis] = delta[axis] ? Math.abs(1 / delta[axis]) : Infinity
    next[axis] = delta[axis] ? (cell[axis] + (step[axis] > 0 ? 1 : 0) - from[axis]) / delta[axis] : Infinity
  }
  for (let n = 0; n < 64; n++) {
    const block = worldAt(cell.x, cell.y, cell.z)
    if (!block) return false
    const isReference = reference && cell.x === reference.x && cell.y === reference.y && cell.z === reference.z
    const shapes = shapesOf(block)
    const full = shapes.some(s => s.every((v, i) => v === (i < 3 ? 0 : 1)))
    if ((!isReference || full) && shapes.some(box => intersects(from, delta, box, cell))) return false
    // Fences/walls extend into the air cell above their own voxel.
    const below = { ...cell, y: cell.y - 1 }
    const under = worldAt(below.x, below.y, below.z)
    const belowReference = reference && below.x === reference.x && below.y === reference.y && below.z === reference.z
    if (under && !belowReference && shapesOf(under).some(box => box[4] > 1 && intersects(from, delta, box, below))) return false
    const t = Math.min(next.x, next.y, next.z)
    if (t >= 1 - EPS) return true
    for (const axis of ['x', 'y', 'z']) if (next[axis] <= t + EPS) { cell[axis] += step[axis]; next[axis] += stride[axis] }
  }
  return false
}

export function placementSight (job, feet, worldAt, hasBox, exactFeet = false) {
  // Planning feet cells are centred; execution supplies the actual body position.
  let y = feet.y
  if (!exactFeet) {
    const floor = worldAt(feet.x, feet.y - 1, feet.z)
    if (floor && !fluids.test(floor.name)) {
      const tops = shapesOf(floor).filter(s => s[0] <= .5 && s[3] >= .5 && s[2] <= .5 && s[5] >= .5).map(s => s[4])
      if (tops.length) y = feet.y - 1 + Math.max(...tops)
    }
  }
  const eye = { x: feet.x + (exactFeet ? 0 : .5), y: y + 1.62, z: feet.z + (exactFeet ? 0 : .5) }
  let choices = job.against ? [[job.against.dx, job.against.dy, job.against.dz]] : faces
  if (job.along) choices = choices.filter(([dx, , dz]) => job.along === 'x' ? dx !== 0 : dz !== 0)
  if (job.block?.states?.axis === 'y') choices = choices.filter(([dx, , dz]) => dx === 0 && dz === 0)
  // The vertical face fixes slab/stair half regardless of cursor height.
  if (/_(slab|stairs)$/.test(job.block?.name ?? '')) choices = choices.filter(([, dy]) => job.half === 'top' ? dy !== -1 : job.half === 'bottom' ? dy !== 1 : true)
  for (const [dx, dy, dz] of choices) {
    const support = worldAt(job.x + dx, job.y + dy, job.z + dz)
    if (!support || !hasBox(support.name) || fluids.test(support.name)) continue
    const cursorY = dy ? .5 - dy * .5 : job.half === 'top' ? .75 : job.half === 'bottom' || job.facing ? .25 : .5
    const point = { x: job.x + dx + .5 - dx * .5, y: job.y + dy + cursorY, z: job.z + dz + .5 - dz * .5 }
    // Reference shapes may protrude beyond their voxel (a fence top) or occupy
    // only part of its face (a slab). The cursor packet still uses the voxel face.
    // Approach that face from its exterior; ignore only the clicked reference,
    // never intervening blocks. This also forbids clicking its hidden back face.
    if ((dx && (eye.x - point.x) * dx > EPS) || (dy && (eye.y - point.y) * dy > EPS) || (dz && (eye.z - point.z) * dz > EPS)) continue
    if (clearClickRay(worldAt, eye, point, 4.5, { x: job.x + dx, y: job.y + dy, z: job.z + dz })) return { dx, dy, dz }
  }
  return null
}
