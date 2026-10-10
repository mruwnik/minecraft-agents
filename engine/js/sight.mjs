// Why JavaScript: predicate over Mineflayer block objects (boundingBox), called per block by raw-world.mjs and the sense primitives.
// What blocks sight, per block.

// What stops the eye, shared by the entity check (prim-sense canSee) and block memory (raw-world sightTable): a full
// collision box blocks unless it is one of these (beds are 9/16 high); lava and powder snow have no box but are opaque. Leaves are seen
// through but still stop arrows: line of fire (jobs.lib.cost.danger) reads collision kinds, not this table.
export const SEE_THROUGH = /glass|fence|^iron_bars$|^water$|^fire$|grass$|^snow$|^vine$|^ladder$|torch$|_bed$|_leaves$/
export const OPAQUE_WITHOUT_BOX = /^(lava|powder_snow)$/
export const blocksSight = block => Boolean(block) &&
  (OPAQUE_WITHOUT_BOX.test(block.name) || (block.boundingBox === 'block' && !SEE_THROUGH.test(block.name)))

// whether the eye at `from` has an unobstructed line to `to` ({x, y, z} points): no cell on the way (half-block steps) blocks sight.
// blockAt takes a Vec3-like {x, y, z} of whole cells and answers a Mineflayer block or null.
export const clearLine = (blockAt, from, to) => {
  const dx = to.x - from.x
  const dy = to.y - from.y
  const dz = to.z - from.z
  const n = Math.ceil(Math.hypot(dx, dy, dz) * 2)
  for (let i = 1; i < n; i++) {
    const k = i / n
    if (blocksSight(blockAt({ x: Math.floor(from.x + dx * k), y: Math.floor(from.y + dy * k), z: Math.floor(from.z + dz * k) }))) return false
  }
  return true
}
