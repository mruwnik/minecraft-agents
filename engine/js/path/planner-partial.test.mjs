// The partial end of a plan: the node nearest the goal among those reached without a step the body cannot undo (a one-way
// step), and result.oneWay when a nearer node lies behind one. Hand-built worlds; stone to y-1 puts the feet cells at y.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { fixtureSnapshot } from './fixture.mjs'
import { plan, MOVE } from './planner.mjs'

const near = (x, y, z, range = 0) => ({ kind: 'near', x, y, z, range })
const run = (fill, from, goal, options) => plan(fixtureSnapshot({ fill }), { from, goal }, { goalFlood: 0, ...options })
const lastCell = r => { const s = r.path.steps.at(-1); return [s.x, s.y, s.z] }
const stone = (x0, z0, x1, z1, feet, to = 59) => [x0, to, z0, x1, feet - 1, z1, 'stone']
const slab = (x0, z0, x1, z1, feet) => [x0, feet, z0, x1, feet, z1, 'stone_slab', { type: 'bottom' }]
const oneWayOf = ({ oneWay: { move, x, y, z } }) => ({ move, x, y, z })

// a plateau, a pool with a bank 5 below it, an island in the pool, then a pit no jump crosses, then the goal's platform
const island = [
  stone(-2, -2, 5, 6, 70),
  [6, 60, -2, 13, 65, 6, 'water'], stone(6, -2, 13, 6, 60, 55),
  stone(14, -2, 17, 6, 66),
  stone(26, -2, 33, 6, 66)
]
const plateau = { x: 2, y: 70, z: 2 }

test('an island reachable only by a drop into water: the partial ends on the shore nearest the goal, the drop is reported', () => {
  const r = run(island, plateau, near(28, 66, 2))
  assert.equal(r.status, 'partial')
  assert.deepEqual(lastCell(r), [5, 70, 2])
  assert.ok(r.path.steps.every(s => s.move !== MOVE.DROP))
  assert.deepEqual(oneWayOf(r), { move: MOVE.DROP, x: 6, y: 65, z: 2 })
  assert.ok(r.oneWay.distance < 12)
})

test('no way to a nearer cell by the drop: oneWay is null', () => {
  const r = run([stone(-2, -2, 5, 6, 70), stone(26, -2, 33, 6, 66)], plateau, near(28, 66, 2))
  assert.equal(r.status, 'partial')
  assert.equal(r.oneWay, null)
})

// a platform, a ledge one below it, a 3-drop to a lower floor, a pit, the goal's platform
const ledge = [stone(-2, -2, 10, 6, 70), stone(11, -2, 12, 6, 69), stone(13, -2, 20, 6, 66), stone(26, -2, 33, 6, 66)]

test('a goal behind a 3-drop: the partial ends on the returnable ledge nearer than the drop edge, the 3-drop is reported', () => {
  const r = run(ledge, plateau, near(28, 66, 2))
  assert.equal(r.status, 'partial')
  assert.deepEqual(lastCell(r), [12, 69, 2])
  assert.deepEqual(r.path.steps.map(s => s.move).filter(m => m === MOVE.DROP), [MOVE.DROP])
  assert.deepEqual(oneWayOf(r), { move: MOVE.DROP, x: 13, y: 66, z: 2 })
})

// the lower floor is reached by a 3-drop (cheap) and by a flight of 1-drops round the back (dear): the nearest node is the
// same cell either way and must not be lost to the cheap way
const twoWays = [
  stone(-2, -2, 5, 6, 70),
  stone(-2, 7, 5, 7, 69), stone(-2, 8, 5, 8, 68), stone(-2, 9, 5, 12, 67),
  stone(6, -2, 20, 12, 67),
  stone(26, -2, 33, 6, 67)
]

test('a node reached by a cheap 3-drop and a dearer flight of steps is the partial end by the flight', () => {
  const r = run(twoWays, plateau, near(28, 67, 2))
  assert.equal(r.status, 'partial')
  assert.deepEqual(lastCell(r), [20, 67, 2])
  assert.equal(r.path.cost.maxDrop, 1)
  assert.equal(r.oneWay, null)
})

// a platform, a ledge below it by `drop`, the floor beyond the ledge's reach (a pit), the goal
const ledgeBelow = (platform, ledgeFeet) => [...platform, stone(11, -2, 12, 6, ledgeFeet), stone(26, -2, 33, 6, 70)]
const flat = [stone(-2, -2, 10, 6, 70)]

test('a drop of exactly 1 is undone by a jump up: the ledge is a partial end and nothing is behind it', () => {
  const r = run(ledgeBelow(flat, 69), plateau, near(28, 70, 2))
  assert.equal(r.status, 'partial')
  assert.deepEqual(lastCell(r), [12, 69, 2])
  assert.equal(r.oneWay, null)
})

test('a drop of 1.5 (off a slab) is not undone: the platform edge is the partial end, the drop is reported', () => {
  const r = run(ledgeBelow([...flat, slab(-2, -2, 10, 6, 70)], 69), plateau, near(28, 70, 2))
  assert.equal(r.status, 'partial')
  assert.deepEqual(lastCell(r), [10, 70, 2])
  assert.deepEqual(oneWayOf(r), { move: MOVE.DROP, x: 11, y: 69, z: 2 })
})

test('a gap jump down is one-way, a level gap jump is not', () => {
  const pit = (landing) => [stone(-2, -2, 5, 6, 70), stone(8, -2, 14, 6, landing), stone(26, -2, 33, 6, 70)]
  const down = run(pit(69), plateau, near(28, 70, 2))
  const level = run(pit(70), plateau, near(28, 70, 2))
  assert.deepEqual(lastCell(down), [5, 70, 2])
  assert.deepEqual(oneWayOf(down), { move: MOVE.GAP, x: 9, y: 69, z: 2 })
  assert.deepEqual(lastCell(level), [14, 70, 2])
  assert.equal(level.oneWay, null)
})

test('complete plans are unchanged: a goal past a 3-drop is found and the drop is in the plan', () => {
  const r = run([stone(-2, -2, 10, 6, 70), stone(11, -2, 20, 6, 67)], plateau, near(18, 67, 2))
  assert.equal(r.status, 'found')
  assert.ok(r.path.steps.some(s => s.move === MOVE.DROP))
  assert.equal(r.oneWay, null)
})

test('no returnable node 2 blocks nearer: the result is none, with the one-way step reported', () => {
  const r = run([stone(0, -2, 3, 6, 70), [4, 60, -2, 13, 65, 6, 'water'], stone(4, -2, 13, 6, 60, 55), stone(14, -2, 17, 6, 66), stone(26, -2, 33, 6, 66)],
    plateau, near(28, 66, 2))
  assert.equal(r.status, 'none')
  assert.equal(r.path, null)
  assert.deepEqual(oneWayOf(r), { move: MOVE.DROP, x: 4, y: 65, z: 2 })
})

// ---- a gap wider than any jump: nothing the walker could learn makes a way, so the reason is exhausted ----

const gapOf = cells => [stone(-2, -2, 5, 6, 70), stone(6 + cells, -2, 20, 6, 70)]
const gapCases = [[3, 'found', null, [12, 70, 2]], [4, 'partial', 'exhausted', [5, 70, 2]]]
for (const [cells, status, reason, end] of gapCases) {
  test(`a gap ${cells} wide with no way round: ${status}${reason ? ` ${reason}` : ''}, ending ${end}`, () => {
    const r = run(gapOf(cells), plateau, near(12, 70, 2))
    assert.deepEqual([r.status, r.reason, lastCell(r)], [status, reason, status === 'found' ? [12, 70, 2] : end])
    assert.equal(r.oneWay, null)
  })
}
