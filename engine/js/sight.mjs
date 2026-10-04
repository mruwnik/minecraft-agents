// Line of sight over a block grid: Amanatides-Woo traversal of the cells a segment passes through.
// Shared by primitives.mjs (real blocks) and engine.fake (fake cells) so both agree on what blocks sight.

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

// Whether the segment from `from` to `to` meets the box [x0, y0, z0, x1, y1, z1] (world coordinates): slab method.
function segmentHitsBox (from, to, box) {
  const [lo, hi] = [box.slice(0, 3), box.slice(3)]
  const o = [from.x, from.y, from.z]
  const d = [to.x - from.x, to.y - from.y, to.z - from.z]
  const span = [0, 1]
  for (let i = 0; i < 3; i++) {
    if (d[i] === 0) {
      if (o[i] < lo[i] || o[i] > hi[i]) return false
      continue
    }
    const [t0, t1] = [(lo[i] - o[i]) / d[i], (hi[i] - o[i]) / d[i]]
    span[0] = Math.max(span[0], Math.min(t0, t1))
    span[1] = Math.min(span[1], Math.max(t0, t1))
    if (span[0] > span[1]) return false
  }
  return true
}

// Like lineClear, but a crossed cell blocks only where the segment meets one of its collision boxes.
// `shapesAt({x, y, z})` gives the cell's boxes in local coordinates (0..1 across the cell; a fence post reaches 1.5).
export function rayClear (from, to, shapesAt) {
  return lineClear(from, to, cell => shapesAt(cell).some(b =>
    segmentHitsBox(from, to, [b[0] + cell.x, b[1] + cell.y, b[2] + cell.z, b[3] + cell.x, b[4] + cell.y, b[5] + cell.z])))
}
