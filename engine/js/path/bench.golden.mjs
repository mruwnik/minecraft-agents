// Why JavaScript: opt-in test of the JS bench baseline planner (bench-baseline.mjs), which is itself JavaScript.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { makeQueries } from './bench.mjs'
import { plan } from './bench-baseline.mjs'
import { fixtureSnapshot } from './fixture.mjs'

const floor = fixtureSnapshot({ fill: [[0, 60, 0, 47, 63, 47, 'stone']] })
const sets = [
  { name: 'a', x0: 24, z0: 24, span: 16, n: 5, minD: 4, maxD: 10, kind: 'near' },
  { name: 'b', x0: 24, z0: 24, span: 16, n: 3, minD: 8, maxD: 8, kind: 'xz' },
  { name: 'c', x0: 24, z0: 24, span: 16, n: 2, minD: 4, maxD: 10, kind: 'air' }
]

test('makeQueries is deterministic for a seed and differs across seeds', () => {
  const one = makeQueries({ snapshot: floor, sets, seed: 1 })
  assert.deepEqual(makeQueries({ snapshot: floor, sets, seed: 1 }), one)
  assert.notDeepEqual(makeQueries({ snapshot: floor, sets, seed: 2 }), one)
})

test('makeQueries yields the requested counts, standable starts, and the goal kinds', () => {
  const queries = makeQueries({ snapshot: floor, sets, seed: 1 })
  assert.deepEqual(queries.map(q => q.set), ['a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'c'])
  assert.ok(queries.every(q => q.from.y === 64))
  assert.deepEqual(queries.map(q => q.goal.kind), ['near', 'near', 'near', 'near', 'near', 'xz', 'xz', 'xz', 'near', 'near'])
  assert.ok(queries.slice(8).every(q => q.goal.y === 104))
  assert.equal(new Set(queries.map(q => q.id)).size, queries.length)
})

test('baseline walks 10 blocks on a flat floor', () => {
  const r = plan(floor, { id: 'walk', from: { x: 2, y: 64, z: 2 }, goal: { kind: 'near', x: 12, y: 64, z: 2, range: 0 } })
  assert.equal(r.status, 'success')
  assert.ok(r.expanded > 0 && r.lookups > 0 && r.path.length >= 10)
  assert.deepEqual(r.path.at(-1), { x: 12, y: 64, z: 2 })
})

test('baseline cannot cross a wide deep moat to a pillar', () => {
  const moat = fixtureSnapshot({
    fill: [[0, 60, 0, 29, 63, 29, 'stone'], [6, 61, 6, 23, 63, 23, 'air'], [13, 61, 13, 16, 63, 16, 'stone']]
  })
  const r = plan(moat, { id: 'moat', from: { x: 2, y: 64, z: 2 }, goal: { kind: 'near', x: 14, y: 64, z: 14, range: 1 } }, { timeout: 500 })
  assert.ok(['noPath', 'timeout'].includes(r.status), r.status)
})
