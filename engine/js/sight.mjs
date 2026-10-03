// Line of sight over a block grid: Amanatides-Woo traversal of the cells a segment passes through.
// Shared by primitives.mjs (real blocks) and fake.mjs (fake cells) so both agree on what blocks sight.

// Blocks the segment from `from` to `to` ({x, y, z}, world coordinates) crosses are asked of `solidAt({x, y, z})`
// with integer cell coordinates. The start and end cells never count (the body's own cell, the target's cell).
// True when no crossed cell is solid. The walk is bounded by the segment, so its length is the cap.
export function lineClear (from, to, solidAt) {
  const d = { x: to.x - from.x, y: to.y - from.y, z: to.z - from.z }
  const cur = { x: Math.floor(from.x), y: Math.floor(from.y), z: Math.floor(from.z) }
  const end = { x: Math.floor(to.x), y: Math.floor(to.y), z: Math.floor(to.z) }
  const axes = ['x', 'y', 'z']
  const step = Object.fromEntries(axes.map(a => [a, Math.sign(d[a])]))
  const tDelta = Object.fromEntries(axes.map(a => [a, d[a] === 0 ? Infinity : Math.abs(1 / d[a])]))
  const tMax = Object.fromEntries(axes.map(a => [a, d[a] === 0 ? Infinity : (d[a] > 0 ? cur[a] + 1 - from[a] : from[a] - cur[a]) * tDelta[a]]))
  const budget = Math.abs(end.x - cur.x) + Math.abs(end.y - cur.y) + Math.abs(end.z - cur.z)
  for (let i = 0; i < budget; i++) {
    const axis = axes.reduce((best, a) => (tMax[a] < tMax[best] ? a : best))
    if (tMax[axis] > 1) return true
    cur[axis] += step[axis]
    tMax[axis] += tDelta[axis]
    if (cur.x === end.x && cur.y === end.y && cur.z === end.z) return true
    if (solidAt(cur)) return false
  }
  return true
}
