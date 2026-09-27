import { createTerrainGeometry } from './navigation/terrain.mjs'

// Finite, deterministic search waypoints; the origin is scanned before any walk.
export function searchWaypoints (origin, { pattern = 'spiral', radius = 96, spacing = 16, steps = 32 } = {}) {
  const n = Math.floor(radius / spacing)
  const cells = []
  const add = (x, z) => { if ((x || z) && Math.hypot(x * spacing, z * spacing) <= radius && cells.length < steps) cells.push([x, z]) }
  if (pattern === 'sweep') {
    for (let z = -n; z <= n && cells.length < steps; z++) {
      for (let i = -n; i <= n; i++) add((z + n) % 2 ? -i : i, z)
    }
  } else {
    for (let ring = 1; ring <= n && cells.length < steps; ring++) {
      for (let x = -ring; x < ring; x++) add(x, -ring)
      for (let z = -ring; z < ring; z++) add(ring, z)
      for (let x = ring; x > -ring; x--) add(x, ring)
      for (let z = ring; z > -ring; z--) add(-ring, z)
    }
  }
  return cells.filter(([x, z]) => (x || z) && Math.hypot(x * spacing, z * spacing) <= radius)
    .slice(0, steps).map(([x, z]) => ({ x: Math.floor(origin.x) + x * spacing, z: Math.floor(origin.z) + z * spacing }))
}

// Require loaded solid ground and two clear head cells. Unknown columns and hazards are never goals.
export function searchSurface (blockAt, x, z, y) {
  const geometry = createTerrainGeometry(blockAt, { leaves: true, openDoors: false, scaffolding: false })
  for (let h = Math.floor(y) + 16; h >= Math.floor(y) - 16; h--) {
    const p = geometry.get(x, h, z)
    if (!p.loaded || p.hazardous || p.liquid || p.crop) return null
    if (!p.shapes.length) continue
    const thin = p.shapes.every(shape => shape[4] <= 0.1)
    const standing = geometry.stand(x, thin ? h : h + 1, z, { climb: false })
    return standing ? { x, y: standing.y, z } : null
  }
  return null
}

export const SEARCH_HEADINGS = { north: [0, -1], east: [1, 0], south: [0, 1], west: [-1, 0] }
// Frontiers are nearby, loaded legs, including sidesteps and a retreat to escape blocked alleys.
export function outwardCandidates (at, heading, spacing) {
  const [dx, dz] = SEARCH_HEADINGS[heading]
  const distance = Math.min(16, spacing)
  const side = [-dz, dx]
  return [[1, 0], [0.75, 0.5], [0.75, -0.5], [0.25, 0.75], [0.25, -0.75], [0, 1], [0, -1], [-0.5, 0.5], [-0.5, -0.5]]
    .map(([forward, across]) => ({ x: Math.floor(at.x + distance * (dx * forward + side[0] * across)), z: Math.floor(at.z + distance * (dz * forward + side[1] * across)) }))
}

// A frontier goal must connect to the feet we actually occupy. Reading a column's
// highest block confuses a canopy or roof with the ground and repeatedly asks A*
// to climb it. This bounded local flood follows loaded dry footing, one step up
// up or a checked fall of at most three blocks, and returns reachable elevations
// for requested columns. The native walk's maxDropDown=4 likewise means at most
// three feet-level blocks; longer falls can hurt and are never frontier edges.
export function connectedFrontiers (blockAt, from, candidates, { maxNodes = 1800, maxReads = 20000, now = Date.now, scaffolding = false, climbableVines = false, openDoors = true } = {}) {
  const started = now()
  maxNodes = Math.max(1, Math.min(1800, maxNodes))
  maxReads = Math.max(1, Math.min(20000, maxReads))
  let limited = null
  const cached = new Map()
  let reads = 0
  const cell = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!cached.has(key)) {
      if (reads >= maxReads) { limited = 'read budget'; return null }
      cached.set(key, blockAt(x, y, z)); reads++
    }
    return cached.get(key)
  }
  const start = { x: Math.floor(from.x), y: Math.floor(from.y), z: Math.floor(from.z), steps: 0 }
  const leafTraversal = /_leaves$/.test(cell(start.x, start.y - 1, start.z)?.name ?? '') ||
    from.y > Math.floor(from.y) && /_leaves$/.test(cell(start.x, start.y, start.z)?.name ?? '')
  const geometry = createTerrainGeometry(cell, { leaves: leafTraversal, scaffolding, climbableVines, openDoors })
  const stances = new Map()
  const stand = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!stances.has(key)) stances.set(key, geometry.stand(x, y, z))
    return stances.get(key)
  }
  const reach = Math.min(24, Math.ceil(Math.max(4, ...candidates.map(p => Math.max(Math.abs(p.x - start.x), Math.abs(p.z - start.z))))) + 2)
  const wanted = new Set(candidates.map(p => `${p.x},${p.z}`))
  const reached = new Map()
  const queue = []
  const seen = new Set()
  // Slabs and farmland can put actual feet inside the support block's cell.
  if (!stand(start.x, start.y, start.z) && from.y > Math.floor(from.y) && stand(start.x, start.y + 1, start.z)) start.y++
  if (stand(start.x, start.y, start.z)) { Object.assign(start, stand(start.x, start.y, start.z)); queue.push(start); seen.add(`${start.x},${start.y},${start.z}`) }
  let expanded = 0
  for (let i = 0; i < queue.length && expanded < maxNodes; i++) {
    if (limited) break
    const at = queue[i]
    expanded++
    const column = `${at.x},${at.z}`
    // Starting atop leaves may traverse a connected canopy to descend, but leaves
    // never become the next frontier destination. Ordinary ground remains the goal.
    if (wanted.has(column) && !reached.has(column) && at.grounded && !at.support?.leaf) reached.set(column, at)
    if (reached.size === wanted.size) break
    const directions = [[1, 0], [-1, 0], [0, 1], [0, -1], [0, 0]]
    for (const [dx, dz] of directions) {
      const x = at.x + dx; const z = at.z + dz
      if (Math.max(Math.abs(x - start.x), Math.abs(z - start.z)) > reach) continue
      for (const dy of dx || dz ? [0, 1, -1, -2, -3] : [1, -1]) {
        const y = at.y + dy
        if (Math.abs(y - start.y) > 16) continue
        const key = `${x},${y},${z}`
        if (seen.has(key)) continue
        const next = stand(x, y, z)
        if (!next || !geometry.edge(at, next, { maxDrop: 3 })) continue
        seen.add(key)
        queue.push({ ...next, steps: at.steps + 1 })
        if (dx || dz) break
      }
    }
  }

  const goals = candidates.map((p, index) => {
    const goal = reached.get(`${p.x},${p.z}`)
    return goal ? { x: goal.x, y: goal.y, z: goal.z, height: goal.height, steps: goal.steps, index, score: index * 2 + Math.abs(goal.y - start.y) * 2 + Math.max(0, goal.steps - Math.abs(p.x - start.x) - Math.abs(p.z - start.z)) } : null
  }).filter(Boolean).sort((a, b) => a.score - b.score || a.index - b.index)
  if (!limited && expanded >= maxNodes && reached.size < wanted.size) limited = 'node budget'
  const originReason = queue.length === 0 ? 'origin has no loaded safe footing or clear headroom'
    : leafTraversal && !goals.length && !limited ? 'origin is on leaves; no connected safe descent to ground was found' : null
  return { goals, expanded, reads, ms: now() - started, capped: Boolean(limited), limited, originReason, escapingCanopy: leafTraversal }
}
