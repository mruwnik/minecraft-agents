// Vanilla's Mth.getSeed(x, 0, z) in exact 64-bit arithmetic (Block.box offset, up to 0.25 each axis). x * 3129871
// wraps as a 32-bit int first (Math.imul): any mismatch here puts a stalk somewhere the server does not, and the body walks into it.
const seed = (x, z) => {
  let l = BigInt.asIntN(64, BigInt(Math.imul(x, 3129871)) ^ (BigInt(z) * 116129781n))
  l = BigInt.asIntN(64, l * l * 42317861n + l * 11n)
  return l >> 16n
}

const clampedAxis = l => Math.max(-0.25, Math.min(0.25, (Number(l & 15n) / 15 - 0.5) * 0.5))

// Server-true bamboo collision box, block-local: Block.box(6.5, 0, 6.5, 9.5, 16, 9.5) shifted by the per-block seed.
export const stalkShape = (x, z) => {
  const l = seed(x, z)
  const dx = clampedAxis(l)
  const dz = clampedAxis(l >> 8n)
  return [0.40625 + dx, 0, 0.40625 + dz, 0.59375 + dx, 1, 0.59375 + dz]
}

const STEP = 0.05
const BODY = 0.32
const LANE = 0.31
const CLEAR = 0.45
const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]

const cellsUnder = (px, pz, w) => [...new Set([Math.floor(px - w), Math.floor(px + w)])]
  .flatMap(x => [...new Set([Math.floor(pz - w), Math.floor(pz + w)])].map(z => [x, z]))

// A body boxed in by bamboo (by the shape the pathfinder plans with) still has room between the server's offset stalks.
// Its free space, searched on a fine grid, leads out; string-pulled into straight runs the body can walk.
export function groveExit ({ from, bambooAt, openAt, radius = 8 }) {
  // the BFS and its 0.02-step string-pull revisit the same cells thousands of times; a grove with no exit otherwise
  // reruns bambooAt/openAt (bot.blockAt round trips) and the BigInt stalk-shape math on every one of them
  const cells = new Map()
  const cellAt = (x, z) => {
    const key = `${x},${z}`
    const cached = cells.get(key)
    if (cached) return cached
    const bamboo = bambooAt(x, z)
    const info = { bamboo, open: openAt(x, z), box: bamboo ? stalkShape(x, z) : null }
    cells.set(key, info)
    return info
  }
  const stalks = (px, pz) => [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => [Math.floor(px) + dx, Math.floor(pz) + dz]))
    .map(([x, z]) => [x, z, cellAt(x, z)])
    .filter(([, , info]) => info.bamboo)
    .map(([x, z, info]) => { const [minX, , minZ, maxX, , maxZ] = info.box; return [x + minX, z + minZ, x + maxX, z + maxZ] })
  const free = (px, pz, w) => cellsUnder(px, pz, w).every(([x, z]) => { const info = cellAt(x, z); return info.open || info.bamboo }) &&
    stalks(px, pz).every(([minX, minZ, maxX, maxZ]) => px + w <= minX || px - w >= maxX || pz + w <= minZ || pz - w >= maxZ)
  const out = (px, pz) => cellsUnder(px, pz, CLEAR).every(([x, z]) => { const info = cellAt(x, z); return info.open && !info.bamboo })
  const near = (px, pz) => Math.abs(px - from.x) <= radius && Math.abs(pz - from.z) <= radius
  const point = ([i, j]) => ({ x: i * STEP, z: j * STEP })

  const [si, sj] = [Math.round(from.x / STEP), Math.round(from.z / STEP)]
  const around = [...Array(13).keys()].map(d => d - 6)
  const start = around.flatMap(di => around.map(dj => [si + di, sj + dj]))
    .filter(([i, j]) => free(i * STEP, j * STEP, BODY))
    .sort((a, b) => Math.hypot(a[0] - si, a[1] - sj) - Math.hypot(b[0] - si, b[1] - sj))[0]
  if (!start) return null

  const cameFrom = new Map([[String(start), null]])
  const queue = [start]
  for (const cell of queue) {
    const { x, z } = point(cell)
    if (out(x, z)) return stringPull(from, trail(cameFrom, cell).map(point), (a, b) => clearRun(a, b, free))
    for (const [di, dj] of SIDES) {
      const next = [cell[0] + di, cell[1] + dj]
      const p = point(next)
      if (cameFrom.has(String(next)) || !near(p.x, p.z) || !free(p.x, p.z, BODY)) continue
      cameFrom.set(String(next), cell)
      queue.push(next)
    }
  }
  return null
}

const trail = (cameFrom, cell) => {
  const cells = []
  for (let c = cell; c; c = cameFrom.get(String(c))) cells.unshift(c)
  return cells
}

const clearRun = (a, b, free) => {
  const n = Math.max(1, Math.ceil(Math.hypot(b.x - a.x, b.z - a.z) / 0.02))
  return [...Array(n + 1).keys()].every(i => free(a.x + (b.x - a.x) * i / n, a.z + (b.z - a.z) * i / n, LANE))
}

// From where the body is, the furthest point of the path it can reach in a straight line; the exit always ends it
const stringPull = (from, path, clear) => {
  const waypoints = []
  let at = from
  for (let i = 0; i < path.length - 1 || !waypoints.length;) {
    const far = path.findLastIndex((p, m) => m > i && clear(at, p))
    i = far > i ? far : Math.min(i + 1, path.length - 1)
    at = path[i]
    waypoints.push(at)
  }
  return waypoints
}

// Sneaking near a waypoint keeps the body from overshooting into the stalk beyond it
export const steer = (pos, { x, z }) => {
  const [dx, dz] = [x - pos.x, z - pos.z]
  const distance = Math.hypot(dx, dz)
  return { yaw: Math.atan2(-dx, -dz), sneak: distance < 0.6, arrived: distance <= 0.1 }
}
