import { test } from 'node:test'
import assert from 'node:assert/strict'
import { fixtureSnapshot } from './fixture.mjs'
import { plan, createSearch, MOVE } from './planner.mjs'

// stone floor whose top face is y=64, so feet cells are y=64; everything outside x,z -2..40 is air (or unloaded)
const world = ({ fill = [], blocks = [] } = {}) =>
  fixtureSnapshot({ fill: [[-2, 60, -2, 40, 63, 40, 'stone'], ...fill], blocks })
const near = (x, y, z, range = 0) => ({ kind: 'near', x, y, z, range })
const start = { x: 2, y: 64, z: 2 }
const run = (snapshot, goal, options, from = start) => plan(snapshot, { from, goal }, options)
const cells = r => r.path.steps.map(s => [s.x, s.y, s.z])
const moves = r => r.path.steps.map(s => s.move)
const lastCell = r => cells(r).at(-1)
const distanceToGoal = (r, [gx, gz]) => Math.hypot(lastCell(r)[0] - gx, lastCell(r)[2] - gz)

test('flat walk of 10 blocks: found, 11 steps, walking speed, no risk', () => {
  const r = run(world(), near(12, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.reason, null)
  assert.deepEqual(lastCell(r), [12, 64, 2])
  assert.equal(r.path.steps.length, 11)
  assert.ok(Math.abs(r.path.cost.seconds - 10 / 4.317) < 0.01)
  assert.deepEqual({ ...r.path.cost, seconds: 0 }, { seconds: 0, risk: 0, maxDrop: 0, jumps: 0, unknown: 0 })
  assert.ok(r.expanded > 0 && r.ms >= 0)
})

test('a near goal accepts any standable cell within its range', () => {
  const r = run(world(), near(12, 64, 2, 3))
  assert.equal(r.status, 'found')
  assert.equal(r.path.steps.length, 8)
})

test('an xz goal ignores height', () => {
  const r = run(world(), { kind: 'xz', x: 12, z: 2, range: 1 })
  assert.equal(r.status, 'found')
  assert.equal(lastCell(r)[1], 64)
  assert.ok(Math.abs(lastCell(r)[0] - 12) <= 1)
})

test('diagonal across open floor uses diagonal moves', () => {
  const r = run(world(), near(7, 64, 7))
  assert.equal(r.status, 'found')
  assert.deepEqual(moves(r), [MOVE.START, ...Array(5).fill(MOVE.DIAGONAL)])
})

const corners = [
  ['block on one side', [[3, 64, 2, 'stone'], [3, 65, 2, 'stone']]],
  ['block on the other side', [[2, 64, 3, 'stone'], [2, 65, 3, 'stone']]],
  ['fence on one side (needs the whole column clear)', [[3, 64, 2, 'oak_fence']]]
]
for (const [name, blocks] of corners) {
  test(`diagonal does not cut a corner: ${name}`, () => {
    const r = run(world({ blocks }), near(3, 64, 3))
    const at = cells(r).findIndex(([x, , z]) => x === 2 && z === 2)
    assert.notDeepEqual(cells(r)[at + 1], [3, 64, 3])
  })
}

test('strictCorners: false allows a diagonal past one blocked side', () => {
  const w = world({ blocks: [[3, 64, 2, 'stone'], [3, 65, 2, 'stone']] })
  assert.deepEqual(cells(run(w, near(3, 64, 3), { strictCorners: false })), [[2, 64, 2], [3, 64, 3]])
})

test('a block touching the diagonal only at its far corner does not stop the diagonal', () => {
  const r = run(world({ blocks: [[4, 64, 3, 'stone']] }), near(3, 64, 3))
  assert.deepEqual(cells(r), [[2, 64, 2], [3, 64, 3]])
})

test('a one block step up is a jump', () => {
  const r = run(world({ fill: [[5, 64, -2, 9, 64, 40, 'stone']] }), near(7, 65, 2))
  assert.equal(r.status, 'found')
  assert.deepEqual(lastCell(r), [7, 65, 2])
  assert.equal(moves(r).filter(m => m === MOVE.JUMP).length, 1)
  assert.equal(r.path.cost.jumps, 1)
})

test('a jump needs headroom over the start cell', () => {
  const low = world({ fill: [[5, 64, -2, 9, 64, 40, 'stone']], blocks: [[4, 66, 2, 'stone']] })
  const r = run(low, near(5, 65, 2), {}, { x: 4, y: 64, z: 2 })
  assert.notEqual(r.path?.steps.at(1).move, MOVE.JUMP)
})

test('a 2 block wall forces a detour round its end', () => {
  const r = run(world({ fill: [[5, 64, -2, 5, 65, 8, 'stone']] }), near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.ok(cells(r).every(([x, y, z]) => !(x === 5 && z <= 8) || y > 65))
  assert.ok(cells(r).some(([x, , z]) => x === 5 && z === 9))
})

test('a 2 block wall across everything: exhausted, none when no closer than the start', () => {
  const sealed = world({ fill: [[5, 64, -2, 5, 65, 40, 'stone']] })
  const r = run(sealed, near(8, 64, 2), {}, { x: 4, y: 64, z: 2 })
  assert.deepEqual([r.status, r.reason, r.path], ['none', 'exhausted', null])
})

test('exhausted still returns a partial path when it gets 2+ blocks closer', () => {
  const sealed = world({ fill: [[5, 64, -2, 5, 65, 40, 'stone']] })
  const r = run(sealed, near(8, 64, 2))
  assert.deepEqual([r.status, r.reason], ['partial', 'exhausted'])
  assert.deepEqual(lastCell(r), [4, 64, 2])
})

test('bottom slabs are half steps: a slab staircase climbs 2 blocks without jumping', () => {
  const stairs = world({
    fill: [[5, 64, -2, 5, 64, 40, 'stone'], [7, 64, -2, 40, 65, 40, 'stone']],
    blocks: [...Array.from({ length: 43 }, (_, i) => [4, 64, i - 2, 'oak_slab', { type: 'bottom' }]),
      ...Array.from({ length: 43 }, (_, i) => [6, 65, i - 2, 'oak_slab', { type: 'bottom' }])]
  })
  const r = run(stairs, near(9, 66, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.jumps, 0)
  assert.deepEqual(lastCell(r), [9, 66, 2])
  assert.deepEqual(r.path.steps.map(s => s.h).slice(0, 6), [0, 0, 8, 0, 8, 0])
})

test('a top slab ceiling 1.5 above the floor blocks the walk; a 2 high tunnel does not', () => {
  const ceiling = (y, type) => world({ fill: [[5, y, -2, 7, y, 40, type === undefined ? 'stone' : 'oak_slab', type === undefined ? {} : { type }]] })
  const blocked = run(ceiling(65, 'top'), near(9, 64, 2))
  assert.deepEqual([blocked.status, blocked.reason], ['partial', 'exhausted'])
  assert.deepEqual(lastCell(blocked), [4, 64, 2])
  assert.equal(run(ceiling(66), near(9, 64, 2)).status, 'found')
})

test('a fence line is not stepped over: exhausted when sealed, a detour when not', () => {
  const sealed = run(world({ fill: [[5, 64, -2, 5, 64, 40, 'oak_fence']] }), near(8, 64, 2))
  assert.deepEqual([sealed.status, sealed.reason], ['partial', 'exhausted'])
  const open = run(world({ fill: [[5, 64, -2, 5, 64, 8, 'oak_fence']] }), near(8, 64, 2))
  assert.equal(open.status, 'found')
  assert.ok(cells(open).every(([x, y, z]) => !(x === 5 && z <= 8)))
})

test('a one block wall is jumped', () => {
  const r = run(world({ fill: [[5, 64, -2, 5, 64, 40, 'stone']] }), near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.jumps, 1)
})

const platform = d => world({ fill: [[0, 64, 0, 4, 63 + d, 4, 'stone']] })
const onPlatform = d => ({ x: 2, y: 64 + d, z: 2 })

test('drops of 1 to 3 blocks are free', () => {
  for (const d of [1, 2, 3]) {
    const r = run(platform(d), near(8, 64, 2), {}, onPlatform(d))
    assert.equal(r.status, 'found')
    assert.deepEqual({ risk: r.path.cost.risk, maxDrop: r.path.cost.maxDrop }, { risk: 0, maxDrop: d })
    assert.ok(moves(r).includes(MOVE.DROP))
  }
})

test('a drop of 4 is refused at the default maxDrop and costs 1 hp of risk when allowed', () => {
  const refused = run(platform(4), near(8, 64, 2), {}, onPlatform(4))
  assert.deepEqual([refused.status, refused.reason], ['partial', 'exhausted'])
  assert.ok(cells(refused).every(([, y]) => y === 68))
  const allowed = run(platform(4), near(8, 64, 2), { maxDrop: 6 }, onPlatform(4))
  assert.equal(allowed.status, 'found')
  assert.deepEqual({ risk: allowed.path.cost.risk, maxDrop: allowed.path.cost.maxDrop }, { risk: 1, maxDrop: 4 })
})

test('a drop into water is forbidden in stage 1', () => {
  const pond = world({ fill: [[0, 64, 0, 4, 66, 4, 'stone'], [5, 61, -2, 40, 63, 40, 'water']] })
  const r = run(pond, near(8, 63, 2), {}, { x: 2, y: 67, z: 2 })
  assert.equal(r.status, 'none')
})

test('a gap of 1, 2 or 3 cells is jumped; 4 is not', () => {
  for (const gap of [1, 2, 3]) {
    const r = run(world({ fill: [[5, 60, -2, 4 + gap, 63, 40, 'air']] }), near(5 + gap + 3, 64, 2))
    assert.equal(r.status, 'found')
    assert.deepEqual([r.path.cost.jumps, r.path.cost.risk], [1, 1])
    assert.ok(moves(r).includes(MOVE.GAP))
  }
  const wide = run(world({ fill: [[5, 60, -2, 8, 63, 40, 'air']] }), near(12, 64, 2))
  assert.deepEqual([wide.status, wide.reason], ['partial', 'exhausted'])
})

test('a gap jump needs headroom over the gap', () => {
  const r = run(world({ fill: [[5, 60, -2, 6, 63, 40, 'air'], [5, 65, -2, 6, 65, 40, 'stone']] }), near(10, 64, 2))
  assert.notEqual(r.status, 'found')
})

test('a gap jump may land one lower', () => {
  const r = run(world({ fill: [[5, 60, -2, 6, 63, 40, 'air'], [7, 63, -2, 40, 63, 40, 'air']] }), near(10, 63, 2))
  assert.equal(r.status, 'found')
})

test('a lava pool is walked round when a detour exists, at no risk', () => {
  const r = run(world({ fill: [[5, 63, 0, 7, 63, 4, 'lava']] }), near(10, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.risk, 0)
  assert.ok(!moves(r).includes(MOVE.GAP))
})

test('a lava strip across everything is gap jumped, with risk', () => {
  const r = run(world({ fill: [[5, 63, -2, 7, 63, 40, 'lava']] }), near(10, 64, 2))
  assert.equal(r.status, 'found')
  assert.ok(r.path.cost.risk >= 1)
  assert.ok(moves(r).includes(MOVE.GAP))
})

for (const y of [64, 65]) {
  test(`cobweb at y=${y} is never entered, jumped or stood in`, () => {
    const r = run(world({ fill: [[5, y, -2, 5, y, 40, 'cobweb']] }), near(8, 64, 2))
    assert.notEqual(r.status, 'found')
    assert.ok(r.path === null || cells(r).every(([x]) => x < 5))
  })
}

test('a magma strip is avoided when the detour is cheap', () => {
  const r = run(world({ fill: [[5, 63, -2, 5, 63, 4, 'magma_block']] }), near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.risk, 0)
})

test('a magma strip is crossed when the detour costs more than 2 s extra', () => {
  const r = run(world({ fill: [[5, 63, -2, 5, 63, 8, 'magma_block']] }), near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.risk, 1)
})

test('a magma strip across everything is crossed, with risk', () => {
  const r = run(world({ fill: [[5, 63, -2, 5, 63, 40, 'magma_block']] }), near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.ok(r.path.cost.risk > 0)
})

test('soul sand is slower to cross than stone', () => {
  const sand = run(world({ fill: [[3, 63, -2, 8, 63, 40, 'soul_sand']] }), near(10, 64, 2))
  const stone = run(world(), near(10, 64, 2))
  assert.ok(sand.path.cost.seconds > stone.path.cost.seconds * 1.5)
})

test('goal inside a block is not standable; with range 1 the cell above serves', () => {
  const inside = run(world(), near(8, 63, 2))
  assert.deepEqual([inside.status, inside.reason, inside.path], ['none', 'goal-not-standable', null])
  assert.equal(run(world(), near(8, 63, 2, 1)).status, 'found')
})

for (const goal of [near(200, 64, 200, 1), { kind: 'xz', x: 200, z: 200, range: 2 }]) {
  test(`unloaded ${goal.kind} goal: partial path toward it`, () => {
    const r = run(world(), goal)
    assert.deepEqual([r.status, r.reason], ['partial', 'goal-unloaded'])
    assert.ok(distanceToGoal(r, [200, 200]) < Math.hypot(198, 198) - 20)
  })
}

for (const [name, from] of [['inside a block', { x: 2, y: 63, z: 2 }], ['in the air', { x: 2, y: 70, z: 2 }], ['unloaded', { x: 200, y: 64, z: 2 }]]) {
  test(`start ${name} is not standable`, () => {
    const r = run(world(), near(8, 64, 2), {}, from)
    assert.deepEqual([r.status, r.reason, r.path], ['none', 'start-not-standable', null])
  })
}

test('maxNodes small: budget, with a partial path toward the goal', () => {
  const r = run(world(), near(35, 64, 2), { maxNodes: 50 })
  assert.deepEqual([r.status, r.reason], ['partial', 'budget'])
  assert.ok(r.path.steps.length > 1)
  assert.ok(distanceToGoal(r, [35, 2]) < 33)
})

test('same snapshot and query give an identical path', () => {
  const w = world({ fill: [[5, 64, -2, 5, 65, 30, 'stone'], [12, 64, 5, 12, 64, 40, 'oak_fence']] })
  const a = run(w, near(20, 64, 20))
  const b = run(w, near(20, 64, 20))
  assert.deepEqual(a.path, b.path)
  assert.equal(a.expanded, b.expanded)
})

for (const slice of [1, 7, 100]) {
  test(`createSearch in slices of ${slice} matches plan`, () => {
    const w = world({ fill: [[5, 64, -2, 5, 65, 30, 'stone']] })
    const whole = run(w, near(20, 64, 20))
    const search = createSearch(w, { from: start, goal: near(20, 64, 20) })
    const done = Array.from({ length: 200000 }, () => 0).some(() => search.step(slice))
    const sliced = search.result()
    assert.ok(done)
    assert.deepEqual([sliced.status, sliced.reason, sliced.expanded, sliced.path], [whole.status, whole.reason, whole.expanded, whole.path])
  })
}

test('summary: a step up and a drop of 4', () => {
  const w = world({ fill: [[4, 64, -2, 6, 64, 40, 'stone'], [7, 61, -2, 40, 63, 40, 'air']] })
  const r = run(w, near(9, 61, 2), { maxDrop: 6 })
  assert.equal(r.status, 'found')
  assert.equal(r.path.summary, '7 blocks, 1 step up, 1 drop of 4')
})

test('summary: lava beside the path counts the risk and is named', () => {
  const w = world({ fill: [[4, 64, -2, 6, 65, 1, 'stone'], [4, 64, 3, 6, 64, 40, 'lava']] })
  const r = run(w, near(8, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.risk, 1.5)
  assert.equal(r.path.summary, '6 blocks, passes 1 cell from lava')
})

test('weighted search still finds a path, expanding no more nodes', () => {
  const w = world({ fill: [[5, 64, -2, 5, 65, 30, 'stone']] })
  const exact = run(w, near(20, 64, 20))
  const greedy = run(w, near(20, 64, 20), { weight: 3 })
  assert.equal(greedy.status, 'found')
  assert.ok(greedy.expanded <= exact.expanded)
})
