// Repeatable pathfinding benchmark: `freeze` copies the chunk files and picks the queries once, `run` plans them
// with a chosen planner adapter (./bench-NAME.mjs; bench-baseline.mjs is the mineflayer-pathfinder one), so results do not move when the live world does.
//   node engine/js/path/bench.mjs freeze --world claude [--out dir]
//   node engine/js/path/bench.mjs run --planner baseline [--dir dir] [--out results.json]
import fs from 'node:fs'
import path from 'node:path'
import { performance } from 'node:perf_hooks'
import { fileURLToPath, pathToFileURL } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import { createSnapshot, loadRecordedWorld, UNLOADED } from './snapshot.mjs'

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../..')
const DEFAULT_DIR = path.join(ROOT, 'state/bench/pathfinding/claude-1')
const TOP_Y = 300 // surface scans start here, as the original baseline did
const AIR_LIFT = 40
const MARGIN = 96 // blocks of world kept around each query area, so searches have room to wander

const set = (name, x0, z0, extra) => ({ name, x0, z0, span: 80, n: 30, minD: 20, maxD: 64, kind: 'near', ...extra })
export const SETS = [
  set('plains', 2897, 3161),
  set('darkforest', 3350, 2680),
  set('snowslope', 2820, 3100),
  set('plains-short', 2897, 3161, { minD: 5, maxD: 20 }),
  set('plains-far', 2897, 3161, { kind: 'xz', minD: 64, maxD: 64, n: 15 }),
  set('darkforest-far', 3350, 2680, { kind: 'xz', minD: 64, maxD: 64, n: 15 }),
  set('plains-air', 2897, 3161, { kind: 'air', n: 10 })
]

// ---- query generation ----

const SOLID = 1
const FREE = 2

// one flag byte per state id: SOLID = full block that is not leaves/liquid, FREE = no collision and not liquid
const stateFlags = registry => {
  const flags = new Uint8Array(registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0) + 1)
  const liquid = /water|lava/
  registry.blocksArray.forEach(block => {
    const flag = block.boundingBox === 'block' && !/leaves/.test(block.name) && !liquid.test(block.name)
      ? SOLID
      : block.boundingBox === 'empty' && !liquid.test(block.name) ? FREE : 0
    flags.fill(flag, block.minStateId, block.maxStateId + 1)
  })
  return flags
}

const flagAt = (snapshot, flags, x, y, z) => {
  const id = snapshot.stateAt(x, y, z)
  return id === UNLOADED ? 0 : flags[id]
}

// highest standable feet y at x,z: feet and head free, a solid block below; null when buried or unloaded
const surfaceY = (snapshot, flags, x, z) => {
  for (let y = TOP_Y; y > snapshot.minY + 4; y--) {
    const below = flagAt(snapshot, flags, x, y - 1, z)
    const feet = flagAt(snapshot, flags, x, y, z)
    if (feet === FREE && below === SOLID && flagAt(snapshot, flags, x, y + 1, z) === FREE) return y
    if (below === SOLID && feet !== FREE) return null
  }
  return null
}

const makeRandom = seed => {
  let state = seed
  return () => {
    state = (state * 1103515245 + 12345) & 0x7fffffff
    return state / 0x7fffffff
  }
}

const goalFor = (kind, g) => {
  if (kind === 'xz') return { kind: 'xz', x: g.x, z: g.z, range: 2 }
  return { kind: 'near', x: g.x, y: kind === 'air' ? g.y + AIR_LIFT : g.y, z: g.z, range: 1 }
}

export function makeQueries ({ snapshot, sets, seed, registry = prismarineRegistry('26.1') }) {
  const flags = stateFlags(registry)
  const random = makeRandom(seed)
  return sets.flatMap(({ name, x0, z0, span, n, minD, maxD, kind }) => {
    const found = []
    for (let tries = 0; found.length < n && tries < n * 40; tries++) {
      const x = Math.floor(x0 + (random() - 0.5) * span)
      const z = Math.floor(z0 + (random() - 0.5) * span)
      const angle = random() * Math.PI * 2
      const distance = minD + random() * (maxD - minD)
      const y = surfaceY(snapshot, flags, x, z)
      if (y === null) continue
      const gx = Math.floor(x + Math.cos(angle) * distance)
      const gz = Math.floor(z + Math.sin(angle) * distance)
      const gy = surfaceY(snapshot, flags, gx, gz)
      if (gy === null && kind !== 'xz') continue
      found.push({
        id: `${name}-${found.length}`,
        set: name,
        from: { x, y, z },
        goal: goalFor(kind, { x: gx, y: gy, z: gz })
      })
    }
    return found
  })
}

// ---- freeze ----

const columnRange = ({ x0, z0, span, maxD }) => {
  const r = span / 2 + maxD + MARGIN
  return { cxMin: Math.floor((x0 - r) / 16), cxMax: Math.floor((x0 + r) / 16), czMin: Math.floor((z0 - r) / 16), czMax: Math.floor((z0 + r) / 16) }
}

const rangeColumns = ({ cxMin, cxMax, czMin, czMax }) =>
  Array.from({ length: cxMax - cxMin + 1 }, (_, i) => cxMin + i).flatMap(cx =>
    Array.from({ length: czMax - czMin + 1 }, (_, j) => [cx, czMin + j]))

export function freeze ({ world, out = DEFAULT_DIR, sets = SETS, seed = 20260101, stateDir = path.join(ROOT, 'state') }) {
  const source = path.join(stateDir, 'worlds', world, 'chunks')
  const wanted = new Map(sets.flatMap(s => rangeColumns(columnRange(s))).map(([cx, cz]) => [`${cx}.${cz}.bin`, true]))
  const names = [...wanted.keys()].filter(name => fs.existsSync(path.join(source, name)))
  fs.mkdirSync(path.join(out, 'chunks'), { recursive: true })
  names.forEach(name => fs.copyFileSync(path.join(source, name), path.join(out, 'chunks', name)))
  const snapshot = createSnapshot({})
  const t0 = performance.now()
  loadRecordedWorld(snapshot, path.join(out, 'chunks'))
  const loadMs = performance.now() - t0
  const queries = makeQueries({ snapshot, sets, seed })
  fs.writeFileSync(path.join(out, 'queries.json'), JSON.stringify(queries, null, 1))
  const bytes = names.reduce((sum, name) => sum + fs.statSync(path.join(out, 'chunks', name)).size, 0)
  return { columns: names.length, megabytes: bytes / 1e6, loadMs, queries: queries.length }
}

// ---- run ----

const percentile = (xs, p) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(p * xs.length))]
const sum = xs => xs.reduce((a, b) => a + b, 0)

export const summarize = results => {
  const status = {}
  results.forEach(r => { status[r.status] = (status[r.status] ?? 0) + 1 })
  const ms = results.map(r => r.ms)
  const expanded = sum(results.map(r => Math.max(1, r.expanded)))
  return {
    n: results.length,
    status,
    ms: { p50: percentile(ms, 0.5), p90: percentile(ms, 0.9), p99: percentile(ms, 0.99), max: Math.max(...ms) },
    expandedP50: percentile(results.map(r => r.expanded), 0.5),
    usPerNode: sum(ms) * 1000 / expanded,
    lookupsPerNode: sum(results.map(r => r.lookups ?? 0)) / expanded
  }
}

const formatLine = (name, s) =>
  `${name}: n=${s.n} ${JSON.stringify(s.status)} ms p50=${s.ms.p50.toFixed(1)} p90=${s.ms.p90.toFixed(1)} p99=${s.ms.p99.toFixed(1)} max=${s.ms.max.toFixed(1)}` +
  ` expanded p50=${s.expandedP50} us/node=${s.usPerNode.toFixed(0)} lookups/node=${s.lookupsPerNode.toFixed(1)}`

export async function run ({ planner, dir = DEFAULT_DIR, out, log = console.log }) {
  const { plan } = await import(`./bench-${planner}.mjs`)
  const snapshot = createSnapshot({})
  const t0 = performance.now()
  const columns = loadRecordedWorld(snapshot, path.join(dir, 'chunks'))
  log(`loaded ${columns} columns in ${(performance.now() - t0).toFixed(0)} ms`)
  const queries = JSON.parse(fs.readFileSync(path.join(dir, 'queries.json'), 'utf8'))
  const results = queries.map(query => {
    const { path: steps, ...rest } = plan(snapshot, query)
    return { id: query.id, set: query.set, ...rest, pathLength: steps.length }
  })
  const bySet = Map.groupBy(results, r => r.set)
  for (const [name, rs] of bySet) log(formatLine(name, summarize(rs)))
  log(formatLine('all', summarize(results)))
  if (out) fs.writeFileSync(out, JSON.stringify(results))
  return results
}

// ---- cli ----

const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined

async function main (argv) {
  const [command] = argv
  if (command === 'freeze') {
    const stats = freeze({ world: flagValue(argv, '--world') ?? 'claude', out: flagValue(argv, '--out') ?? DEFAULT_DIR })
    console.log(`froze ${stats.columns} columns, ${stats.megabytes.toFixed(1)} MB, load ${stats.loadMs.toFixed(0)} ms, ${stats.queries} queries`)
    return
  }
  if (command === 'run') {
    await run({ planner: flagValue(argv, '--planner') ?? 'baseline', dir: flagValue(argv, '--dir'), out: flagValue(argv, '--out') })
    return
  }
  console.error('usage: bench.mjs freeze --world W [--out DIR] | run --planner NAME [--dir DIR] [--out FILE]')
  process.exitCode = 2
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) await main(process.argv.slice(2))
