// Ferrying a villager by boat: its uuid, boat status, tow waypoints, and the route/dock plan.

export const villagerUuid = text => typeof text === 'string' && /^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(text)

// Keep transport checks on UUID because entity IDs can change when the body reconnects.
export function villagerBoatStatus (state, boatId, uuid) {
  const boat = state?.boats?.find(b => b.id === boatId)
  if (!boat) return `boat ${boatId} is no longer in sight`
  if (!boat.passengers?.some(p => p.uuid === uuid && p.name === 'villager')) return `villager ${uuid} is not in boat ${boatId}`
  return null
}

export function villagerTowWaypoints (from, to, stride = 4) {
  const horizontal = Math.hypot(to.x - from.x, to.z - from.z)
  if (!Number.isFinite(horizontal) || horizontal < 0.5) return []
  const count = Math.ceil(horizontal / stride)
  const points = []
  for (let i = 1; i <= count; i++) {
    const t = i / count
    const p = { x: Math.floor(from.x + (to.x - from.x) * t), y: Math.floor(from.y + (to.y - from.y) * t), z: Math.floor(from.z + (to.z - from.z) * t) }
    if (!points.length || points.at(-1).x !== p.x || points.at(-1).y !== p.y || points.at(-1).z !== p.z) points.push(p)
  }
  return points
}

// Plan the boat's hull, not the driver's walking route. Half-block centers
// permit the boat to line up with a two-block opening; its 1.375-block hull
// still has to clear every cell swept between centers, including corners.
export function villagerBoatRoute ({ from, to, blockAt, margin = 8 }) {
  const half = 1.375 / 2
  const step = 0.5
  const minX = Math.floor(Math.min(from.x, to.x) - margin)
  const maxX = Math.ceil(Math.max(from.x, to.x) + margin)
  const minZ = Math.floor(Math.min(from.z, to.z) - margin)
  const maxZ = Math.ceil(Math.max(from.z, to.z) + margin)
  const minY = Math.floor(Math.min(from.y, to.y)) - 4
  // A roof is clearance, not a walkable deck above the boat. Search only at
  // and below the current hull; a solid at its level becomes an uphill lip.
  const maxY = Math.floor(from.y)
  const cache = new Map()
  const get = (x, y, z) => {
    const k = `${x},${y},${z}`
    if (!cache.has(k)) cache.set(k, blockAt(x, y, z))
    return cache.get(k)
  }
  const cells = (lo, hi) => {
    const out = []
    for (let n = Math.floor(lo); n <= Math.floor(hi - 1e-8); n++) out.push(n)
    return out
  }
  const inspect = (x, z) => {
    const footprint = cells(x - half, x + half).flatMap(cx => cells(z - half, z + half).map(cz => ({ x: cx, z: cz })))
    let support = -Infinity
    let supportAt = null
    for (const p of footprint) {
      let column = -Infinity
      for (let y = maxY; y >= minY; y--) {
        const b = get(p.x, y, p.z)
        if (!b) return { error: 'unloaded terrain', at: { x: p.x, y, z: p.z } }
        if (b.solid) { column = y + 1; break }
        if (b.name === 'water' && get(p.x, y + 1, p.z)?.name !== 'water') { column = y + 0.5; break }
      }
      if (!Number.isFinite(column)) return { error: 'unsupported water or ground', at: { ...p, y: minY } }
      if (column > support) {
        support = column
        supportAt = { x: p.x, y: Math.floor(column - 0.5), z: p.z }
      }
    }
    // The passenger and boat need two body cells above the support. Plants are
    // passable; solid blocks, including a lip in any swept column, are not.
    for (const p of footprint) for (let y = Math.floor(support); y <= Math.floor(support + 1.5); y++) {
      const b = get(p.x, y, p.z)
      if (!b) return { error: 'unloaded terrain', at: { x: p.x, y, z: p.z } }
      if (b.solid) return { error: `hull blocked by ${b.name}`, at: { x: p.x, y, z: p.z } }
    }
    return { y: support, supportAt }
  }
  const quantize = p => ({ x: Math.round(p.x / step) * step, z: Math.round(p.z / step) * step })
  let start = quantize(from)
  const goal = quantize(to)
  const id = p => `${p.x},${p.z}`
  const first = inspect(from.x, from.z)
  if (first.error) return first
  const landing = inspect(goal.x, goal.z)
  if (landing.error) return { ...landing, error: `landing ${landing.error}` }
  const landingCell = get(Math.floor(to.x), Math.floor(to.y), Math.floor(to.z))
  const expectedLanding = landingCell?.name === 'water' ? Math.floor(to.y) + 0.5 : to.y
  if (landing.y > expectedLanding + 0.01) {
    return { error: `landing hull needs height ${landing.y} above intended ${expectedLanding}`, at: landing.supportAt }
  }
  const sweep = (a, b, height) => {
    const count = Math.max(1, Math.ceil(Math.hypot(b.x - a.x, b.z - a.z) / 0.125))
    let y = height
    for (let i = 1; i <= count; i++) {
      const t = i / count
      const p = { x: a.x + (b.x - a.x) * t, z: a.z + (b.z - a.z) * t }
      const check = inspect(p.x, p.z)
      if (check.error) return check
      if (check.y > y + 0.01) return { error: `upward boat step from ${y} to ${check.y}`, at: { x: p.x, y: Math.floor(check.y - 1), z: p.z } }
      if (y - check.y > 1.01) return { error: `unsafe boat drop from ${y} to ${check.y}`, at: { x: p.x, y: Math.floor(check.y), z: p.z } }
      y = check.y
    }
    return { y }
  }
  let startCheck = sweep(from, start, first.y)
  if (startCheck.error) {
    // Rounding an exact center can push a fitting hull into a narrow bank.
    // Try adjacent lattice centers only through fully checked connectors.
    const candidates = []
    for (const dx of [-step, 0, step]) for (const dz of [-step, 0, step]) {
      const p = { x: start.x + dx, z: start.z + dz }
      if (p.x >= minX && p.x <= maxX && p.z >= minZ && p.z <= maxZ) candidates.push(p)
    }
    candidates.sort((a, b) => Math.hypot(a.x - from.x, a.z - from.z) - Math.hypot(b.x - from.x, b.z - from.z))
    for (const candidate of candidates) {
      const check = sweep(from, candidate, first.y)
      if (!check.error) { start = candidate; startCheck = check; break }
    }
    if (startCheck.error) return startCheck
  }
  const queue = [{ ...start, y: startCheck.y }]
  const seen = new Map([[id(start), queue[0]]])
  let blocker = null
  let found = null
  for (let i = 0; i < queue.length && i < 30000; i++) {
    const p = queue[i]
    if (id(p) === id(goal)) { found = p; break }
    for (const [dx, dz] of [[-step, 0], [step, 0], [0, -step], [0, step]]) {
      const next = { x: p.x + dx, z: p.z + dz }
      if (next.x < minX || next.x > maxX || next.z < minZ || next.z > maxZ || seen.has(id(next))) continue
      // Check the entire swept strip, not just the endpoint.
      const check = sweep(p, next, p.y)
      if (check.error) { blocker ??= check; continue }
      const node = { ...next, y: check.y, prev: p }
      seen.set(id(next), node)
      queue.push(node)
    }
  }
  if (!found) return blocker
    ? { error: `no complete nonascending boat route within scan area; first blocked step: ${blocker.error}`, at: blocker.at }
    : { error: 'no nonascending boat route within scan area', at: { ...goal, y: to.y } }
  const points = []
  for (let p = found; p; p = p.prev) points.push({ x: p.x, y: p.y, z: p.z })
  points.reverse()
  return { points }
}

// A boat is enclosed where it lands, then released through one lower gate cell.
// A temporary solid block under that cell keeps its water gap one block high;
// remove that block last when reopening the two-wide channel for the return.
// cell.y is the first air layer over river water and may also be solid shore
// ground; standing dry cells are one higher.
export function villagerDockPlan (cell, river) {
  if (Math.abs(river.x ?? 0) + Math.abs(river.z ?? 0) !== 1) throw new Error('dock river side must be one cardinal direction')
  const side = { x: -river.z, z: river.x }
  const at = (d, t, y = cell.y) => ({ x: cell.x + d * river.x + t * side.x, y, z: cell.z + d * river.z + t * side.z })
  const ring = []
  for (let d = -2; d <= 2; d++) for (let t = -2; t <= 2; t++) if (Math.abs(d) === 2 || Math.abs(t) === 2) ring.push(at(d, t))
  const gateBase = [at(2, 0), at(2, 1)]
  const gate = gateBase.flatMap(p => [p, { ...p, y: p.y + 1 }])
  const service = at(2, 0)
  const serviceFoundation = { ...service, y: cell.y - 1 }
  const open = p => gate.some(g => g.x === p.x && g.y === p.y && g.z === p.z) || (p.x === service.x && p.y === service.y && p.z === service.z)
  const walls = ring.flatMap(p => [0, 1, 2, 3].map(n => ({ ...p, y: p.y + n }))).filter(p => !open(p))
  const interior = []
  for (let d = -1; d <= 1; d++) for (let t = -1; t <= 1; t++) interior.push(at(d, t))
  return { cell, river, ring, gate, gateBase, service, serviceFoundation, serviceStand: at(3, 0, cell.y - 1), walls, interior }
}
