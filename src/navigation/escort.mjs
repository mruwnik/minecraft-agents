import { createTerrainGeometry, terrainProfile, PLAYER_HALF, PLAYER_HEIGHT } from './terrain.mjs'

export const equine = name => ['horse', 'donkey', 'mule', 'skeleton_horse', 'zombie_horse'].includes(name)
export const escortAtDestination = (position, destination, range = 4) => Math.hypot(position.x - destination.x, position.y - destination.y, position.z - destination.z) <= range
const EPS = 1e-6

// A led horse needs its own corridor, although the leader still walks using
// native player physics. The horse can step early when its wider front reaches
// a rise: its support envelope may be one block above the player's feet.
export function createEscortCorridor (blockAt, { from, to, width = 1.4, height = 1.6, maxY = 320, valleyDepth = 2, lateral = 8, padding = 0.02, allowHighLogs = false } = {}) {
  const cache = new Map(), sky = new Map(), skyFailures = new Map()
  const at = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!cache.has(key)) cache.set(key, blockAt(x, y, z))
    return cache.get(key)
  }
  const geometry = createTerrainGeometry(at, { openDoors: false, dry: true, avoidCrops: true })
  const half = Math.max(0.7, width / 2) + padding
  const dx = to.x - from.x, dz = to.z - from.z, distance2 = dx * dx + dz * dz
  const surface = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (sky.has(key)) return sky.get(key)
    let clear = true
    for (let by = Math.ceil(y + height); by < maxY; by++) {
      const p = terrainProfile(at(x, by, z), { openDoors: false })
      // Canopy above the body is allowed; solid roofs/geology and unknown sky
      // are not certified as a surface route. Mounted routes may also pass
      // below high tree branches; the full rider body box still checks logs
      // and leaves at any height it actually intersects.
      const highTree = allowHighLogs && /(?:_log|_wood)$|^(?:crimson|warped)_(?:stem|hyphae)$/.test(p.name ?? '')
      if (!p.loaded || p.hazardous || p.liquid || !p.leaf && !highTree && p.shapes.length) {
        clear = false; skyFailures.set(key, { reason: 'surface column is obstructed or unloaded', cell: { x, y: by, z }, block: p.name ?? 'unloaded' }); break
      }
    }
    sky.set(key, clear)
    return clear
  }
  const stance = (position, onFailure) => {
    const reject = (reason, details = {}) => { onFailure?.({ reason, at: { ...position }, ...details }); return null }
    const { x, y, z } = position
    const t = distance2 ? Math.max(0, Math.min(1, ((x - from.x) * dx + (z - from.z) * dz) / distance2)) : 0
    if (y < from.y + (to.y - from.y) * t - valleyDepth || Math.hypot(x - from.x - dx * t, z - from.z - dz * t) > lateral) return reject('outside the bounded surface corridor')
    const supports = []
    for (let bx = Math.floor(x - half); bx <= Math.floor(x + half - EPS); bx++) for (let bz = Math.floor(z - half); bz <= Math.floor(z + half - EPS); bz++) {
      let top = null
      for (let by = Math.floor(y + 1) - 1; by >= Math.floor(y) - 2; by--) {
        const p = terrainProfile(at(bx, by, bz), { openDoors: false })
        if (!p.loaded || p.hazardous || p.liquid || p.crop || p.leaf || p.noSupport || /^(?:farmland|slime_block|honey_block|soul_sand)$/.test(p.name)) return reject('unsafe or unsupported footing block', { cell: { x: bx, y: by, z: bz }, block: p.name ?? 'unloaded' })
        const shape = p.support.find(s => s[0] <= Math.max(0, x - half - bx) + EPS && s[3] >= Math.min(1, x + half - bx) - EPS &&
          s[2] <= Math.max(0, z - half - bz) + EPS && s[5] >= Math.min(1, z + half - bz) - EPS)
        if (shape) { top = by + shape[4]; break }
        // A narrow physical obstacle is not support, even with solid ground below.
        if (p.shapes.length) return reject('collision shape does not support the full horse footprint', { cell: { x: bx, y: by, z: bz }, block: p.name })
      }
      if (top === null) return reject('no supporting surface within the checked step depth', { column: { x: bx, z: bz }, fromY: Math.floor(y + 1) - 1, toY: Math.floor(y) - 2 })
      supports.push(top)
    }
    const floor = Math.max(...supports)
    if (floor - Math.min(...supports) > 1 + EPS || floor < y - EPS || floor > y + 1 + EPS) return reject('support heights exceed the one-block step envelope', { floor, lowest: Math.min(...supports) })
    if (!geometry.clearBox([x - half, floor, z - half, x + half, floor + Math.max(1.8, height), z + half])) return reject('horse or rider body clearance is obstructed', { box: [x - half, floor, z - half, x + half, floor + Math.max(1.8, height), z + half] })
    for (let bx = Math.floor(x - half); bx <= Math.floor(x + half - EPS); bx++) for (let bz = Math.floor(z - half); bz <= Math.floor(z + half - EPS); bz++) if (!surface(bx, floor, bz)) return reject('not a checked surface column', skyFailures.get(`${bx},${floor},${bz}`))
    return { ...position, height: floor }
  }
  const edge = (a, b) => {
    if (Math.abs(a.y - b.y) > 1 + EPS) return false
    const n = Math.max(1, Math.ceil(Math.hypot(b.x - a.x, b.z - a.z) / 0.2))
    let previous
    for (let i = 0; i <= n; i++) {
      // Steps occur at terrain boundaries, not along an imaginary ramp.
      const p = { x: a.x + (b.x - a.x) * i / n, y: i === n ? b.y : Math.min(a.y, b.y), z: a.z + (b.z - a.z) * i / n }
      const current = stance(p)
      if (!current || previous && Math.abs(current.height - previous.height) > 1 + EPS) return false
      if (previous && !geometry.clearBox([Math.min(previous.x, current.x) - half, Math.max(previous.height, current.height), Math.min(previous.z, current.z) - half,
        Math.max(previous.x, current.x) + half, Math.max(previous.height, current.height) + Math.max(1.8, height), Math.max(previous.z, current.z) + half])) return false
      previous = current
    }
    return true
  }
  const followingEdge = (a, b) => {
    const landing = p => {
      if (!geometry.clearBox([p.x - half, p.y, p.z - half, p.x + half, p.y + height, p.z + half])) return null
      for (const y of [p.y, Math.floor(p.y), Math.floor(p.y) - 1]) {
        const s = stance({ ...p, y })
        if (s && s.height <= p.y + EPS && p.y - s.height <= 1) return { x: p.x, y: s.height, z: p.z }
      }
      return null
    }
    // `a` is the horse's observed body; `b` is the leader, whose narrower
    // body may still stand below an adjacent terrace. Check the player there,
    // then use the horse's planned support envelope at that endpoint. Requiring
    // the horse to occupy the player's lower Y wrongly forbids early stepping.
    if (!geometry.clearBox([b.x - PLAYER_HALF, b.y, b.z - PLAYER_HALF, b.x + PLAYER_HALF, b.y + PLAYER_HEIGHT, b.z + PLAYER_HALF])) return false
    const from = landing(a), target = stance(b)
    const to = target ? { x: b.x, y: target.height, z: b.z } : null
    return !!from && !!to && edge(from, to)
  }
  return { stance, edge, followingEdge, reads: () => cache.size }
}

export function configureEscortMoves (moves, options) {
  const original = moves.getNeighbors.bind(moves)
  let corridor = createEscortCorridor(options.blockAt, options)
  moves.refreshEscortTerrain = () => { corridor = createEscortCorridor(options.blockAt, options) }
  moves.escortCorridor = () => corridor
  moves.canDig = false; moves.allowSprinting = false; moves.allowParkour = false
  // Native maxDropDown counts from feet to the landing SUPPORT block: two
  // means a one-block feet descent. The geometry filter also enforces <=1.
  moves.allow1by1towers = false; moves.maxDropDown = 2; moves.scafoldingBlocks = []
  // Native post-processing otherwise shortcuts a valid wide route using only
  // player-sized physics. Its documented exclusion hook disables that shortcut.
  moves.exclusionAreasStep.push(() => 0)
  moves.getNeighbors = node => {
    const began = performance.now()
    try {
      return original(node).filter(next => {
        if (next.toBreak?.length || next.toPlace?.length || Math.abs(next.x - node.x) + Math.abs(next.z - node.z) !== 1) return false
        const resolve = p => moves.resolveTerrainWaypoint?.(p) ?? { x: p.x + 0.5, y: p.y, z: p.z + 0.5 }
        const from = resolve(node), to = resolve(next)
        return from && to && corridor.edge(from, to)
      })
    } finally {
      const ms = performance.now() - began
      if (ms > 1000) options.reportPerformance?.('escort.neighbors', ms, { reads: corridor.reads() })
    }
  }
  return moves
}

// Runtime is dependency-injected so cancellation, actual native goal ownership,
// bounded waiting and absence of an unsafe fetch walk are exercised together.
export async function runSurfaceEscort ({ position, animals, destination, check, pause, start, stop, refresh, corridor, now = Date.now, health, threatened = () => false, pathFailed = () => false, grounded = () => true, arrived = e => escortAtDestination(e.position, destination), leaderArrived = p => escortAtDestination(p, destination, 1.5) }) {
  const initialHealth = health(), started = now()
  const expectedAnimals = animals().length
  let lastProgress = started, best = Infinity, walking = false, tautSince = null
  const finish = why => ({ arrived: !why, why, leadsAttached: animals().length })
  try {
    while (true) {
      check()
      const herd = animals(), p = position(), time = now()
      if (!herd.length || herd.length !== expectedAnimals) return finish('a led horse was lost or its lead broke')
      if (health() < initialHealth || health() < 16 || threatened()) return finish('escort stopped for nearby danger or damage; leads remain attached')
      if (herd.every(arrived) && leaderArrived(p)) return finish(null)
      const distance = Math.hypot(p.x - destination.x, p.y - destination.y, p.z - destination.z)
      if (distance < best - 0.5) { best = distance; lastProgress = time }
      if (time - lastProgress >= 20000 || time - started >= 180000) return finish('no sustained escort progress; stopped without fetching through unchecked terrain')
      if (walking && pathFailed()) return finish('no checked horse-width surface route to this destination; choose a nearer surface checkpoint')
      refresh()
      const distances = herd.map(e => Math.hypot(e.position.x - p.x, e.position.y - p.y, e.position.z - p.z))
      // A taut lead is a request to wait, never permission to drag the horse off
      // a drop or perform a fresh unrestricted player route to retrieve it.
      const terrain = corridor()
      const hold = Math.max(...distances) > 5 || grounded() && herd.some(e => !(terrain.followingEdge ?? terrain.edge)(e.position, p))
      if (hold) {
        if (walking) { stop(); walking = false }
        tautSince ??= time
        if (time - tautSince >= 10000) return finish('horse cannot follow the checked corridor; stopped with leads attached')
      } else {
        tautSince = null
        if (!walking) { start(); walking = true }
      }
      await pause()
    }
  } finally { stop() }
}
