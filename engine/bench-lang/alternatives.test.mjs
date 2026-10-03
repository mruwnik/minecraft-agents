// Alternative paths (engine.path.alternatives): up to k clearly different paths for one query, best first, with true costs.
// "Different", as the ns docstring says it: a path is returned only when, against the paths returned before it, its set of
// kinds (ladder, water, door; none of them is on foot) is not one of theirs, or fewer than 60% of its cells lie within 2 blocks
// of their cells (blocks: the largest of |dx|, |dy|, |dz|).
//   cd engine && node --test bench-lang/alternatives.test.mjs     (needs the dev build, see versions.mjs)
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'
import { fixtureSnapshot } from '../js/path/fixture.mjs'
import { courseNames, courseSnapshot } from '../js/path/courses.mjs'
import { defaultStateTable } from '../js/path/blocks.mjs'
import * as space from '../js/path/space.mjs'

const require = createRequire(import.meta.url)
const build = require(path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../out/planner-bench.cjs'))
const options = { table: defaultStateTable(), space }
const plan = (snapshot, query, extra) => build.planTuned(snapshot, query, { ...options, ...extra })
const alternatives = (snapshot, query, k, extra) => build.planAlternatives(snapshot, query, { ...options, ...extra }, k)

// stone floor whose top face is y=63, so feet cells are y=64; everything outside x,z -2..40 is air
const world = (fill = []) => fixtureSnapshot({ fill: [[-2, 60, -2, 40, 63, 40, 'stone'], ...fill] })
const near = (x, y, z, range = 0) => ({ kind: 'near', x, y, z, range })
const query = (from, goal) => ({ from, goal })

// ---- the sentence, written out again here so the test checks it and not the code's own reading of it ----

const KIND_OF = { 7: 'ladder', 8: 'ladder', 9: 'ladder', 10: 'ladder' }
const kindsOf = p => [...new Set(p.steps.flatMap(s => [
  KIND_OF[s.move],
  (s.move >= 11 || s.swim) && 'water',
  s.opens && 'door'
]).filter(Boolean))].sort().join('+')
const chebyshev = (a, b) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y), Math.abs(a.z - b.z))
const distanceTo = (cell, earlier) => Math.min(...earlier.flatMap(p => p.steps.map(s => chebyshev(cell, s))))
const differentFrom = (p, earlier) => {
  if (!earlier.map(kindsOf).includes(kindsOf(p))) return true
  const distances = p.steps.map(s => distanceTo(s, earlier))
  return distances.filter(d => d <= 2).length / distances.length < 0.6
}
const allDifferent = paths => paths.every((p, i) => i === 0 || differentFrom(p, paths.slice(0, i)))

// ---- hand-built worlds ----

// a 4-high wall at x=10 for z <= 30: over it by a ladder each side (x=9 up, x=11 down), or on foot round its end at z=31
const ladderWall = extra => world([
  [10, 64, -2, 10, 67, 30, 'stone'],
  ...(extra ?? [[9, 64, 2, 9, 67, 2, 'ladder', { facing: 'west' }], [11, 64, 2, 11, 67, 2, 'ladder', { facing: 'east' }]])
])
const overTheWall = query({ x: 5, y: 64, z: 2 }, near(15, 64, 2))

// a wall at x=10 across the floor with two gaps: z 4..5 (near the line from start to goal) and z 14..15
const twoGaps = () => world([[10, 64, -2, 10, 65, 3, 'stone'], [10, 64, 6, 10, 65, 13, 'stone'], [10, 64, 16, 10, 65, 40, 'stone']])
const throughAGap = query({ x: 4, y: 64, z: 4 }, near(16, 64, 4))

test('a ladder over a wall and the walk round its end are both returned, the cheaper (ladder) first', () => {
  const r = alternatives(ladderWall(), overTheWall, 3)
  assert.equal(r.status, 'found')
  assert.equal(r.paths.length, 2)
  assert.deepEqual(r.paths.map(kindsOf), ['ladder', ''])
  assert.ok(r.paths[0].total < r.paths[1].total)
  assert.deepEqual(r.paths.map(p => p.differs), ['best', 'on foot instead of by ladder'])
})

// the true cost of a level walk, from its own steps (planner.mjs): a block at walking speed, a diagonal sqrt 2 of one, a
// diagonal slid along a blocked corner 0.15 s more
const WALK_S = 1 / 4.317
const STEP_S = { 1: WALK_S, 2: Math.SQRT2 * WALK_S, 6: Math.SQRT2 * WALK_S + 0.15 }
const levelWalkCost = p => ({
  moves: [...new Set(p.steps.map(s => s.move))].filter(m => !STEP_S[m] && m !== 0),
  seconds: p.steps.slice(1).reduce((sum, s) => sum + STEP_S[s.move], 0)
})
const close = (a, b) => Math.abs(a - b) < 1e-9

test('the walk round has its true cost, not the penalised one: its own steps priced at walking speed', () => {
  const r = alternatives(ladderWall(), overTheWall, 3)
  const { moves, seconds } = levelWalkCost(r.paths[1])
  assert.deepEqual(moves, [])
  assert.ok(close(r.paths[1].cost.seconds, seconds), `${r.paths[1].cost.seconds} vs ${seconds}`)
  assert.deepEqual([r.paths[1].cost.risk, r.paths[1].total], [0, r.paths[1].cost.seconds])
})

test('two gaps in a wall 10 blocks apart are two ways on foot, the near gap first, the far one named by its side', () => {
  const r = alternatives(twoGaps(), throughAGap, 3)
  assert.equal(r.paths.length, 2)
  assert.ok(r.paths[0].steps.some(s => s.x === 10 && s.z >= 4 && s.z <= 5))
  assert.ok(r.paths[1].steps.some(s => s.x === 10 && s.z >= 14 && s.z <= 15))
  assert.match(r.paths[1].differs, /^on foot, a separate way to the south \(\d+% of its cells over 2 blocks from earlier paths\)$/)
})

test('the far gap has its true cost, not the penalised one: its own steps priced at walking speed', () => {
  const r = alternatives(twoGaps(), throughAGap, 3)
  const { moves, seconds } = levelWalkCost(r.paths[1])
  assert.deepEqual(moves, [])
  assert.ok(close(r.paths[1].cost.seconds, seconds), `${r.paths[1].cost.seconds} vs ${seconds}`)
  assert.deepEqual([r.paths[1].cost.risk, r.paths[1].total], [0, r.paths[1].cost.seconds])
})

// a 1-wide corridor: x 2..20 at z=2, walled in on both sides
const corridor = width => world([[0, 64, 0, 22, 65, 3 + width, 'stone'], [1, 64, 2, 21, 65, 1 + width, 'air']])

const single = [
  ['a 1-wide corridor has one way', corridor(1), query({ x: 2, y: 64, z: 2 }, near(20, 64, 2))],
  ['a 2-wide corridor: the side-by-side lanes are one way', corridor(2), query({ x: 2, y: 64, z: 2 }, near(20, 64, 2))],
  ['a 3-wide corridor: lanes 1 or 2 blocks over are one way', corridor(3), query({ x: 2, y: 64, z: 3 }, near(20, 64, 3))],
  ['open ground: a path shifted a block or two over is not another way', world(), query({ x: 2, y: 64, z: 10 }, near(30, 64, 10))]
]
for (const [name, snapshot, q] of single) {
  test(`${name}: exactly one path, the plain plan's`, () => {
    const r = alternatives(snapshot, q, 3)
    assert.equal(r.paths.length, 1)
    assert.deepEqual(r.paths[0].steps, plan(snapshot, q).path.steps)
  })
}

test('k=1 is the plain plan: one search, one path', () => {
  const r = alternatives(ladderWall(), overTheWall, 1)
  assert.equal(r.searches, 1)
  assert.equal(r.paths.length, 1)
})

test('no path at all: status none, no paths, no error', () => {
  const sealed = world([[0, 64, 0, 4, 65, 4, 'stone'], [2, 64, 2, 2, 65, 2, 'air']])
  const r = alternatives(sealed, query({ x: 2, y: 64, z: 2 }, near(20, 64, 20)), 3, { goalFlood: 0 })
  assert.notEqual(r.status, 'found')
  assert.ok(r.paths.length <= 1)
  assert.ok(r.paths.every(p => p.differs === 'best'))
})

// ---- every course of the fixture set ----

const courses = courseNames().map(name => ({ name, ...courseSnapshot(name) }))
const results = courses.map(c => {
  const q = query(c.from, c.goal)
  return { ...c, single: plan(c.snapshot, q), alts: alternatives(c.snapshot, q, 3) }
})
const withPaths = results.filter(r => r.alts.status === 'found')

test('the course set has courses with 1, 2 and 3 alternatives', () => {
  const counts = new Set(withPaths.map(r => r.alts.paths.length))
  assert.deepEqual([...counts].sort(), [1, 2, 3])
})

test('on every course the first path is the plain plan: same status, steps, cost and summary', () => {
  for (const { name, single, alts } of results) {
    assert.equal(alts.status, single.status, name)
    if (single.status !== 'found') continue
    assert.deepEqual([alts.paths[0].steps, alts.paths[0].cost, alts.paths[0].summary], [single.path.steps, single.path.cost, single.path.summary], name)
  }
})

test('on every course the paths returned obey the sentence, in order', () => {
  const broken = withPaths.filter(r => !allDifferent(r.alts.paths)).map(r => r.name)
  assert.deepEqual(broken, [])
})

test('on every course no alternative costs less than the first (the first is the optimum at weight 1)', () => {
  const cheaper = withPaths.filter(r => r.alts.paths.some(p => p.total < r.alts.paths[0].total - 1e-9)).map(r => r.name)
  assert.deepEqual(cheaper, [])
})

test('on every course each alternative is a walk from the start to the goal', () => {
  for (const { name, from, goal, alts } of withPaths) {
    for (const p of alts.paths) {
      assert.deepEqual([p.steps[0].x, p.steps[0].y, p.steps[0].z], [Math.floor(from.x), Math.floor(from.y), Math.floor(from.z)], name)
      const last = p.steps.at(-1)
      assert.ok(Math.hypot(last.x - goal.x, last.z - goal.z) <= (goal.range ?? 0) * 1.0824 + 1e-9 + 1.5, name)
      assert.ok(p.steps.slice(1).every((s, i) => Math.abs(s.x - p.steps[i].x) <= 5 && Math.abs(s.z - p.steps[i].z) <= 5), name)
    }
  }
})
