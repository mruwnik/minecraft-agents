// Why JavaScript: bench harness over binary chunk files and prismarine blocks; the searches it times are the
// ClojureScript ones (out/search-bench.cjs, `npx shadow-cljs compile search-bench`).
// The search bench: times the job-side reach searches (engine.jobs.reach: walkable-way?, enclosed?, nearest-danger,
// dangers) on the recorded world of the planner bench, with blockAt answered the way the body's primitives answer it
// (a prismarine Block per read, its state properties copied). Bodies stand at the bench queries' start cells; the mobs
// are standable cells around them. Each case runs --rounds times; the table gives the median ms and the blocks read.
//   cd engine && node bench-lang/search.mjs [--rounds 3] [--bodies 40] [--out results.json]
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import { fileURLToPath } from 'node:url'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import vec3 from 'vec3'
import { decodeColumnFile, restoreColumn } from '../js/view.mjs'
import { stateProperties } from '../js/use-on.mjs'
import { benchDir } from './courses.mjs'

const { Vec3 } = vec3
const require = createRequire(import.meta.url)
const OUT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out/search-bench.cjs')
const ChunkColumn = prismarineChunk(prismarineRegistry('26.1'))

const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined

// {blockAt(x, y, z) -> prismarine Block | null}: the recorded columns, read lazily
const recordedWorld = dir => {
  const columns = new Map()
  const columnAt = (cx, cz) => {
    const key = `${cx},${cz}`
    if (!columns.has(key)) {
      const file = path.join(dir, 'chunks', `${cx}.${cz}.bin`)
      const column = fs.existsSync(file)
        ? (decoded => restoreColumn(new ChunkColumn({ minY: decoded.header.minY, worldHeight: decoded.header.worldHeight }), decoded))(decodeColumnFile(fs.readFileSync(file)))
        : null
      columns.set(key, column)
    }
    return columns.get(key)
  }
  return {
    blockAt: (x, y, z) => {
      const column = columnAt(x >> 4, z >> 4)
      if (!column || y < -64 || y >= 320) return null
      const block = column.getBlock(new Vec3(x & 15, y, z & 15))
      block.position = new Vec3(x, y, z)
      return block
    }
  }
}

// the primitives the reach searches read: blockAt as the body's (js/primitives.mjs blockAt), self and entities
const primitivesOver = (world, body, mobs, reads) => ({
  blockAt: ({ x, y, z }) => {
    reads.n++
    const block = world.blockAt(x, y, z)
    if (!block) return null
    const properties = stateProperties(block)
    return { name: block.name, pos: { x, y, z }, ...(Object.keys(properties).length > 0 && { properties }) }
  },
  self: () => ({ pos: { x: body.x + 0.5, y: body.y, z: body.z + 0.5 }, health: 20 }),
  entities: ({ radius = 16 } = {}) => mobs
    .map((m, i) => ({ id: i + 1, name: m.name ?? 'zombie', kind: 'hostile', visible: true, pos: { x: m.x + 0.5, y: m.y, z: m.z + 0.5 }, distance: Math.hypot(m.x - body.x, m.y - body.y, m.z - body.z) }))
    .filter(e => e.distance <= radius)
    .sort((a, b) => a.distance - b.distance)
})

const open = b => !b || b.boundingBox === 'empty'
const standable = (world, x, y, z) => open(world.blockAt(x, y, z)) && open(world.blockAt(x, y + 1, z)) && world.blockAt(x, y - 1, z)?.boundingBox === 'block'

// a deterministic spread of standable cells within radius of body: the nearest per ring of distance and level band
const mobCells = (world, body, radius) => {
  const found = []
  for (let dx = -radius; dx <= radius; dx++) {
    for (let dz = -radius; dz <= radius; dz++) {
      for (let dy = -8; dy <= 8; dy++) {
        const d = Math.hypot(dx, dy, dz)
        if (d < 3 || d > radius) continue
        if (standable(world, body.x + dx, body.y + dy, body.z + dz)) found.push({ x: body.x + dx, y: body.y + dy, z: body.z + dz, d, band: Math.sign(dy) })
      }
    }
  }
  const picks = new Map()
  for (const c of found) {
    const key = `${Math.floor(c.d / 3)}:${c.band}`
    if (!picks.has(key)) picks.set(key, c)
  }
  return [...picks.values()]
}

const median = xs => [...xs].sort((a, b) => a - b)[Math.floor(xs.length / 2)]
const quantile = (xs, q) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(q * xs.length))]

const time = (rounds, reads, fn) => {
  const ms = []
  let result
  let n = 0
  for (let r = 0; r < rounds; r++) {
    reads.n = 0
    const t = performance.now()
    result = fn()
    ms.push(performance.now() - t)
    n = reads.n
  }
  return { ms: median(ms), reads: n, result }
}

function main (argv) {
  const rounds = Number(flagValue(argv, '--rounds') ?? 3)
  const bodies = Number(flagValue(argv, '--bodies') ?? 40)
  const out = flagValue(argv, '--out')
  const lib = require(OUT)
  const dir = benchDir()
  const world = recordedWorld(dir)
  const queries = JSON.parse(fs.readFileSync(path.join(dir, 'queries.json'), 'utf8')).slice(0, bodies)
  const rows = []
  for (const q of queries) {
    const body = q.from
    const mobs8 = mobCells(world, body, 8)
    const mobs16 = mobCells(world, body, 16)
    const reads = { n: 0 }
    const p = primitivesOver(world, body, mobs16, reads)
    rows.push({ search: 'enclosed?', id: q.id, ...time(rounds, reads, () => lib.enclosed(p)) })
    for (const m of mobs16) rows.push({ search: 'walkable-way?', id: `${q.id} mob ${m.x - body.x},${m.y - body.y},${m.z - body.z}`, ...time(rounds, reads, () => lib.walkableWay(p, { x: m.x + 0.5, y: m.y, z: m.z + 0.5 }, { x: body.x + 0.5, y: body.y, z: body.z + 0.5 })) })
    const p8 = primitivesOver(world, body, mobs8, reads)
    rows.push({ search: 'nearest-danger r8', id: `${q.id} (${mobs8.length} mobs)`, ...time(rounds, reads, () => lib.nearestDanger(p8, 8, 16)) })
    rows.push({ search: 'dangers r8', id: `${q.id} (${mobs8.length} mobs)`, ...time(rounds, reads, () => lib.dangers(p8, 8, 16)) })
  }
  const bySearch = Object.groupBy(rows, r => r.search)
  console.log('search                 cases   median ms   p95 ms    max ms  >100ms  median reads  max reads')
  for (const [search, rs] of Object.entries(bySearch)) {
    const ms = rs.map(r => r.ms)
    const reads = rs.map(r => r.reads)
    console.log(`${search.padEnd(22)} ${String(rs.length).padStart(5)} ${median(ms).toFixed(2).padStart(11)} ${quantile(ms, 0.95).toFixed(1).padStart(8)} ${Math.max(...ms).toFixed(1).padStart(9)} ${String(ms.filter(x => x > 100).length).padStart(7)} ${String(median(reads)).padStart(13)} ${String(Math.max(...reads)).padStart(10)}`)
  }
  console.log('\nslowest:')
  for (const r of [...rows].sort((a, b) => b.ms - a.ms).slice(0, 12)) console.log(`  ${r.ms.toFixed(1).padStart(8)} ms  ${String(r.reads).padStart(7)} reads  ${r.search}  ${r.id}  -> ${JSON.stringify(r.result)}`)
  if (out) fs.writeFileSync(out, JSON.stringify(rows, null, 1))
}

main(process.argv.slice(2))
