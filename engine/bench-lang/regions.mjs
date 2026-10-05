// Why JavaScript: bench harness over binary chunk files; the region map it times is the ClojureScript one
// (engine.path.regions through out/planner-bench.cjs, `npx shadow-cljs compile planner-bench`).
// The region map bench over the frozen world (engine/test/fixtures/pathfinding/claude-1):
//   build   - every section of the columns within --radius blocks of the first query's start, in one map
//   memory  - the map's typed arrays after that build (what one body holds for a view of that radius)
//   query   - route for every world query on that map, asked twice (warm: the second, every section it needs built; the
//             bench world has areas outside the radius), and on a fresh map per query (cold: sections built on demand)
//   update  - --changes seeded block changes at node cells (a block at the feet, the floor dug, alternately): invalidate
//             and rebuild the dropped sections, then one route through the changed place
//   cd engine && node --expose-gc bench-lang/regions.mjs [--radius 160] [--changes 200] [--no-cold]
import fs from 'node:fs'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import { defaultStateTable } from '../js/path/blocks.mjs'
import { createSnapshot, loadRecordedWorld } from '../js/path/snapshot.mjs'
import * as space from '../js/path/space.mjs'
import { benchDir } from './courses.mjs'

const require = createRequire(import.meta.url)
const HERE = path.dirname(fileURLToPath(import.meta.url))
const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined
const argv = process.argv.slice(2)
const RADIUS = Number(flagValue(argv, '--radius') ?? 160)
const CHANGES = Number(flagValue(argv, '--changes') ?? 200)
const COLD = !argv.includes('--no-cold')

const build = require(path.join(HERE, '../out/planner-bench.cjs'))
const registry = prismarineRegistry('26.1')
const time = fn => { const t = performance.now(); const v = fn(); return [performance.now() - t, v] }
const q = (xs, p) => { const s = [...xs].sort((a, b) => a - b); return s[Math.min(s.length - 1, Math.floor(p * s.length))] }
const row = xs => xs.length === 0 ? 'none' : `p50 ${q(xs, 0.5).toFixed(2)} p95 ${q(xs, 0.95).toFixed(2)} max ${Math.max(...xs).toFixed(2)} ms`

const dir = benchDir()
const snapshot = createSnapshot({})
loadRecordedWorld(snapshot, path.join(dir, 'chunks'))
const world = () => ({ snapshot, table: defaultStateTable(), space })
const queries = JSON.parse(fs.readFileSync(path.join(dir, 'queries.json'), 'utf8'))
const centre = queries[0].from

const heap = () => { globalThis.gc?.(); return process.memoryUsage().heapUsed }
for (const [cx, cz] of snapshot.columns()) for (let y = -64; y < 320; y += 16) snapshot.stateAt(cx * 16, y, cz * 16)
const heap0 = heap()
const rm = build.regionsCreate(world())
const columns = snapshot.columns().filter(([cx, cz]) => Math.abs(cx * 16 + 8 - centre.x) <= RADIUS && Math.abs(cz * 16 + 8 - centre.z) <= RADIUS)
for (const [cx, cz] of columns) build.regionsQueueColumn(rm, cx, cz)
const [buildMs] = time(() => { while (build.regionsPending(rm) > 0) build.regionsBuildStep(rm, 1000) })
const heapMB = ((heap() - heap0) / 1048576).toFixed(1)
const mem = build.regionsMemory(rm)
console.log(`build: ${columns.length} columns in ${(buildMs / 1000).toFixed(1)} s (${(buildMs / columns.length).toFixed(1)} ms a column); ` +
  `${mem.sections} sections, ${mem.nodes} nodes, ${mem.regions} regions; memory ${(mem.bytes / 1048576).toFixed(1)} MB of typed arrays, heap +${heapMB} MB${globalThis.gc ? '' : ' (run with --expose-gc)'}`)

const warm = []
const cold = []
const answers = {}
for (const { from, goal } of queries) {
  build.regionsRoute(rm, from, [goal])
  const [ms, r] = time(() => build.regionsRoute(rm, from, [goal]))
  warm.push(ms)
  answers[r.status] = (answers[r.status] ?? 0) + 1
  if (COLD) cold.push(time(() => build.regionsRoute(build.regionsCreate(world()), from, [goal]))[0])
}
console.log(`query (${queries.length}): warm ${row(warm)}; cold ${COLD ? row(cold) : 'not run'}; answers ${JSON.stringify(answers)}`)

let seed = 7
const rnd = n => { seed = (seed * 1103515245 + 12345) % 2147483648; return Math.floor(seed / 7) % n }
const stone = registry.blocksByName.stone.defaultState
const updates = []
const routes = []
for (let k = 0; k < CHANGES; k++) {
  const { from } = queries[rnd(queries.length)]
  const x = from.x + rnd(9) - 4
  const z = from.z + rnd(9) - 4
  const y = k % 2 === 0 ? from.y : from.y - 1
  const old = snapshot.stateAt(x, y, z)
  snapshot.setState(x, y, z, k % 2 === 0 ? stone : 0)
  updates.push(time(() => { build.regionsInvalidate(rm, x, y, z, old); while (build.regionsPending(rm) > 0) build.regionsBuildStep(rm, 1000) })[0])
  routes.push(time(() => build.regionsRoute(rm, from, [{ kind: 'near', x: x + 6, y: from.y, z: z + 6, range: 2 }]))[0])
}
console.log(`update (${CHANGES} changes): invalidate + rebuild ${row(updates)}; route after ${row(routes)}`)
