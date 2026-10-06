// Why JavaScript: bench harness over binary chunk files and prismarine block names; the searches it times are the
// ClojureScript planner's (out/planner-bench.cjs, `npx shadow-cljs compile planner-bench`).
// The nearest-of-many bench: bodies at every 4th planner-bench query start, targets the logs (and, separately, the ores)
// with an air face within --radius blocks (at most 32, nearest in a line first), each reached within 3 blocks. Compared:
//   line  - the straight-line nearest target, then one search to it (what the jobs did)
//   each  - one search per target, the cheapest found wins (the exact answer by many searches)
//   set   - one search over the goal set (query.goals), at most 20000 nodes (jobs.lib.targets)
// All at weight 1.2 over the walks' wide box (margin 256, yMargin 96). Truth: the least cost of the per-target searches.
// The table gives ms p50/p95/max per variant, how often its target was unreachable while another was reachable
// ("wrong"), and cost / truth.
//   cd engine && node bench-lang/targets.mjs [--radius 24] [--out results.json]
import fs from 'node:fs'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import { defaultStateTable } from '../js/path/blocks.mjs'
import { createSnapshot, loadRecordedWorld, UNLOADED } from '../js/path/snapshot.mjs'
import * as space from '../js/path/space.mjs'
import { benchDir } from './courses.mjs'
import { quantile } from './goto.mjs'

const require = createRequire(import.meta.url)
const HERE = path.dirname(fileURLToPath(import.meta.url))
const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined
const argv = process.argv.slice(2)
const R = Number(flagValue(argv, '--radius') ?? 24)
const RANGE = 3
const MAX_TARGETS = 32
const SET_NODES = 20000
const out = flagValue(argv, '--out')

const build = require(path.join(HERE, '../out/planner-bench.cjs'))
const registry = prismarineRegistry('26.1')
const nameOf = id => id === UNLOADED ? 'unloaded' : (registry.blocksByStateId[id]?.name ?? 'unknown')
const options = extra => ({ table: defaultStateTable(), space, weight: 1.2, margin: 256, yMargin: 96, ...extra })
const cost = r => r.status === 'found' ? r.path.cost.seconds + 2 * r.path.cost.risk : Infinity
const time = fn => { const t = performance.now(); const v = fn(); return [performance.now() - t, v] }

const snapshot = createSnapshot({})
loadRecordedWorld(snapshot, path.join(benchDir(), 'chunks'))
const queries = JSON.parse(fs.readFileSync(path.join(benchDir(), 'queries.json'), 'utf8'))
const AIR = new Set(['air', 'cave_air'])
const exposed = (x, y, z) => [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]]
  .some(([a, b, c]) => AIR.has(nameOf(snapshot.stateAt(x + a, y + b, z + c))))
const KINDS = { logs: n => n.endsWith('_log'), ores: n => n.endsWith('_ore') }

function targetsNear (from, test) {
  const found = []
  for (let dx = -R; dx <= R; dx++) for (let dz = -R; dz <= R; dz++) for (let dy = -R; dy <= R; dy++) {
    if (dx * dx + dy * dy + dz * dz > R * R) continue
    const x = from.x + dx, y = from.y + dy, z = from.z + dz
    const id = snapshot.stateAt(x, y, z)
    if (id === UNLOADED || id === 0 || !test(nameOf(id)) || !exposed(x, y, z)) continue
    found.push({ x, y, z, d: Math.hypot(dx, dy, dz) })
  }
  return found.sort((a, b) => a.d - b.d).slice(0, MAX_TARGETS)
}

const goal = t => ({ kind: 'near', x: t.x, y: t.y, z: t.z, range: RANGE })
const rows = []
for (const kind of Object.keys(KINDS)) {
  for (const { from } of queries.filter((_, i) => i % 4 === 0)) {
    const targets = targetsNear(from, KINDS[kind])
    if (targets.length < 2) continue
    const [eachMs, each] = time(() => targets.map(t => build.planTuned(snapshot, { from, goal: goal(t) }, options({}))))
    const truth = Math.min(...each.map(cost))
    const [lineMs, line] = time(() => build.planTuned(snapshot, { from, goal: goal(targets[0]) }, options({})))
    const [setMs, set] = time(() => build.planTuned(snapshot, { from, goals: targets.map(goal) }, options({ maxNodes: SET_NODES })))
    rows.push({ kind, from, targets: targets.length, truth, eachMs, lineMs, setMs, lineCost: cost(line), setCost: cost(set), setReason: set.reason, setExpanded: set.expanded })
  }
}

const reachable = rows.filter(r => Number.isFinite(r.truth))
const stat = xs => ({ p50: +quantile(xs, 0.5).toFixed(2), p95: +quantile(xs, 0.95).toFixed(2), max: +Math.max(...xs).toFixed(2) })
const ratio = key => stat(reachable.filter(r => Number.isFinite(r[key]) && r.truth > 0).map(r => r[key] / r.truth))
const summary = {
  queries: rows.length,
  reachable: reachable.length,
  targetsP50: quantile(rows.map(r => r.targets), 0.5),
  ms: { line: stat(rows.map(r => r.lineMs)), each: stat(rows.map(r => r.eachMs)), set: stat(rows.map(r => r.setMs)) },
  msUnreachable: { set: rows.some(r => !Number.isFinite(r.truth)) ? stat(rows.filter(r => !Number.isFinite(r.truth)).map(r => r.setMs)) : null },
  wrong: { line: reachable.filter(r => !Number.isFinite(r.lineCost)).length, set: reachable.filter(r => !Number.isFinite(r.setCost)).length },
  costOverTruth: { line: ratio('lineCost'), set: ratio('setCost') },
  setReasonsWhenNone: rows.filter(r => !Number.isFinite(r.setCost)).reduce((a, r) => ({ ...a, [r.setReason]: (a[r.setReason] ?? 0) + 1 }), {})
}
console.log(JSON.stringify(summary, null, 1))
if (out) fs.writeFileSync(out, JSON.stringify({ summary, rows }))
