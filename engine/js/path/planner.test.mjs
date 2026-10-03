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
// the goal flood would call a sealed goal 'goal-enclosed' before the main search runs: off, to test what the search does
const search = (snapshot, goal, options, from) => run(snapshot, goal, { goalFlood: 0, ...options }, from)
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

test('a flat open 20 block query expands under 40 nodes and never starts the goal flood', () => {
  const r = run(world(), near(22, 64, 2))
  assert.equal(r.status, 'found')
  assert.ok(r.expanded < 40, `expanded ${r.expanded}`)
  assert.equal(r.stats.flooded, 0)
  assert.equal(r.stats.masks, 0) // no tight cell: no mask is ever built
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

// ---- corner slide: one side blocked, the other free: a diagonal that hugs the free side ----

const wallAt = (x, z) => [x, 64, z, x, 65, z, 'stone']
const notch = [2, 62, 3, 2, 63, 3, 'air'] // 2 deep hole beside the start: no floor to step through
const slides = [
  ['a walk past a block corner, the other side a 2 deep notch', { fill: [wallAt(3, 2), notch] }, near(3, 64, 3), [MOVE.CORNER]],
  ['a jump up past a block corner', { fill: [wallAt(3, 2), notch, [3, 64, 3, 3, 64, 3, 'stone']] }, near(3, 65, 3), [MOVE.JUMP]]
]
for (const [name, extra, goal, expected] of slides) {
  test(`corner slide: ${name}`, () => {
    const r = run(world(extra), goal)
    assert.deepEqual([cells(r).length, moves(r)[1], r.path.steps[1].corner], [2, expected[0], true])
    assert.ok(r.path.summary.includes('1 corner slide'))
  })
}

test('a corner slide is not chosen when the L route over a standable side is cheaper', () => {
  const r = run(world({ fill: [wallAt(3, 2)] }), near(3, 64, 3))
  assert.ok(r.path.steps.every(s => !s.corner && s.move !== MOVE.CORNER))
})

test('both sides blocked: no diagonal between them', () => {
  const r = run(world({ fill: [wallAt(3, 2), wallAt(2, 3)] }), near(3, 64, 3))
  const at = cells(r).findIndex(([x, , z]) => x === 2 && z === 2)
  assert.notDeepEqual(cells(r)[at + 1], [3, 64, 3])
  assert.ok(r.path.steps.every(s => !s.corner))
})

test('a lava side is never slid past', () => {
  const r = run(world({ fill: [[3, 64, 2, 3, 64, 2, 'lava'], notch] }), near(3, 64, 3))
  assert.ok(r.path.steps.every(s => !s.corner))
})

// the 0.62 wide body brushes both side cells on a diagonal, so only collision and things it must not touch stop it
const passableSides = [
  ['a water pool', { fill: [[3, 63, 2, 3, 64, 2, 'water']] }],
  ['a hole with no floor', { fill: [[3, 60, 2, 3, 63, 2, 'air']] }],
  ['a cell open above the floor', {}]
]
for (const [name, extra] of passableSides) {
  test(`diagonal past the corner of ${name} is allowed`, () => {
    assert.deepEqual(cells(run(world(extra), near(3, 64, 3))), [[2, 64, 2], [3, 64, 3]])
  })
}

const blockedSides = [
  ['lava', [[3, 64, 2, 'lava']]],
  ['cobweb', [[3, 64, 2, 'cobweb']]],
  ['a fence', [[3, 64, 2, 'oak_fence']]],
  ['a block at head height', [[3, 65, 2, 'stone']]]
]
for (const [name, blocks] of blockedSides) {
  test(`diagonal past ${name} on one side is refused`, () => {
    const r = run(world({ blocks }), near(3, 64, 3))
    assert.notDeepEqual(cells(r), [[2, 64, 2], [3, 64, 3]])
  })
}

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
  const r = search(sealed, near(8, 64, 2), {}, { x: 4, y: 64, z: 2 })
  assert.deepEqual([r.status, r.reason, r.path], ['none', 'exhausted', null])
})

test('exhausted still returns a partial path when it gets 2+ blocks closer', () => {
  const sealed = world({ fill: [[5, 64, -2, 5, 65, 40, 'stone']] })
  const r = search(sealed, near(8, 64, 2))
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
  const blocked = search(ceiling(65, 'top'), near(9, 64, 2))
  assert.deepEqual([blocked.status, blocked.reason], ['partial', 'exhausted'])
  assert.deepEqual(lastCell(blocked), [4, 64, 2])
  assert.equal(run(ceiling(66), near(9, 64, 2)).status, 'found')
})

test('a fence line is not stepped over: exhausted when sealed, a detour when not', () => {
  // the fence runs past the floor's edge: a post at the very edge leaves room to stand beside it
  const sealed = search(world({ fill: [[5, 64, -3, 5, 64, 41, 'oak_fence']] }), near(8, 64, 2))
  assert.deepEqual([sealed.status, sealed.reason], ['partial', 'exhausted'])
  const open = run(world({ fill: [[5, 64, -3, 5, 64, 8, 'oak_fence']] }), near(8, 64, 2))
  assert.equal(open.status, 'found')
  // it rounds the fence's end: the end cell (z 8) has room beside the post, nothing earlier does
  assert.ok(open.path.steps.filter(s => s.x === 5).every(s => s.z >= 8))
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
  const refused = search(platform(4), near(8, 64, 2), {}, onPlatform(4))
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
  const wide = search(world({ fill: [[5, 60, -2, 8, 63, 40, 'air']] }), near(12, 64, 2))
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

// gaps of 1..3 cells then ground 1 higher: the jump arc needs feet + 2.5 headroom, and only gaps of 1 or 2 may climb
const gapUp = (gap, extra = []) => world({ fill: [[5, 60, -2, 4 + gap, 63, 40, 'air'], [5 + gap, 64, -2, 40, 64, 40, 'stone'], ...extra] })
const gapUps = [
  ['gap 1 up 1 is found', 1, [], 'found'],
  ['gap 2 up 1 is found', 2, [], 'found'],
  ['gap 3 up 1 is refused', 3, [], 'partial'],
  ['gap 2 up 1 under a ceiling at feet + 2 is refused', 2, [[5, 66, -2, 6, 66, 40, 'stone']], 'partial']
]
for (const [name, gap, extra, status] of gapUps) {
  test(`a gap jump landing one higher: ${name}`, () => {
    const r = search(gapUp(gap, extra), near(5 + gap + 3, 65, 2))
    assert.equal(r.status, status)
  })
}

test('summary names a jump up over a gap', () => {
  const r = search(gapUp(2), near(10, 65, 2))
  assert.ok(r.path.summary.includes('1 jump up over a gap'))
})

test('a gap jump up costs 0.3 s more than the level one', () => {
  const up = search(gapUp(2), near(10, 65, 2))
  const level = search(world({ fill: [[5, 60, -2, 6, 63, 40, 'air']] }), near(10, 64, 2))
  assert.ok(Math.abs(up.path.cost.seconds - level.path.cost.seconds - 0.3) < 0.02)
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

// ---- standing on partial blocks: a feet cell whose own collision tops at 1..15/16 is stood in, at h = top ----

const row = (x0, x1, name, props, y = 64) => [x0, y, -2, x1, y, 40, name, props]
// [name, floor cell y the row sits in, fill, h]: tops above 9/16 are above a step, so those blocks replace the floor
const partials = [
  ['a bed', 64, ['red_bed', { part: 'foot', facing: 'east' }], 9],
  ['a bottom slab', 64, ['oak_slab', { type: 'bottom' }], 8],
  ['snow, 4 layers', 64, ['snow', { layers: 4 }], 6],
  ['soul sand', 63, ['soul_sand'], 14],
  ['a chest', 63, ['chest'], 14],
  ['farmland', 63, ['farmland'], 15],
  ['a dirt path', 63, ['dirt_path'], 15],
  ['an enchanting table', 63, ['enchanting_table'], 12],
  ['snow, 8 layers', 63, ['snow', { layers: 8 }], 14]
]
for (const [name, y, [block, props], h] of partials) {
  test(`a row of ${name} is walked across at h=${h} in its own cell, without jumping`, () => {
    const r = run(world({ fill: [row(4, 6, block, props, y)] }), near(9, 64, 2))
    assert.equal(r.status, 'found')
    assert.equal(r.path.cost.jumps, 0)
    assert.deepEqual(r.path.steps.filter(s => s.x >= 4 && s.x <= 6).map(s => [s.y, s.h]), [[y, h], [y, h], [y, h]])
  })
}

test('soul sand is crossed slowly but is found', () => {
  const sand = run(world({ fill: [row(3, 8, 'soul_sand', {}, 63)] }), near(10, 64, 2))
  assert.equal(sand.status, 'found')
  assert.ok(sand.path.cost.seconds > run(world(), near(10, 64, 2)).path.cost.seconds * 1.5)
})

test('a staircase of snow layers 2, 4, 6, 8 and then a full block is climbed without a jump', () => {
  const layers = [2, 4, 6, 8]
  const w = world({ fill: [...layers.map((n, i) => row(4 + i, 4 + i, 'snow', { layers: n })), row(8, 12, 'stone')] })
  const r = run(w, near(10, 65, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.jumps, 0)
  assert.ok(!moves(r).includes(MOVE.JUMP))
  assert.deepEqual(r.path.steps.filter(s => s.x >= 4 && s.x <= 7).map(s => s.h), [2, 6, 10, 14])
})

test('from a full block onto a chest and off again, no jump', () => {
  const w = world({ fill: [row(3, 3, 'stone'), row(4, 4, 'chest'), row(5, 6, 'stone')] })
  const r = run(w, near(6, 65, 2), {}, { x: 3, y: 65, z: 2 })
  assert.equal(r.status, 'found')
  assert.ok(!moves(r).includes(MOVE.JUMP))
  const onChest = r.path.steps.find(s => s.x === 4)
  assert.deepEqual([onChest.y, onChest.h], [64, 14])
})

test('standing on a partial block needs the 1.8 headroom above its top', () => {
  const w = world({ fill: [row(5, 5, 'chest'), [5, 66, -2, 5, 66, 40, 'stone']] })
  const r = run(w, near(5, 64, 2), {}, { x: 3, y: 64, z: 2 })
  assert.notEqual(r.status, 'found')
})

// ---- stairs: a bottom stairs block is climbed without a jump, walking in its facing direction ----

const stairs = (x, y, facing) => [x, y, -2, x, y, 40, 'oak_stairs', { facing, half: 'bottom', shape: 'straight' }]
const flight = [
  stairs(4, 64, 'east'), stairs(5, 65, 'east'), stairs(6, 66, 'east'), stairs(7, 67, 'east'),
  [5, 64, -2, 5, 64, 40, 'stone'], [6, 64, -2, 6, 65, 40, 'stone'], [7, 64, -2, 7, 66, 40, 'stone'],
  [8, 64, -2, 12, 67, 40, 'stone']
]

test('a four step stair flight is climbed with walks only', () => {
  const r = run(world({ fill: flight }), near(9, 68, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.jumps, 0)
  assert.deepEqual([...new Set(moves(r))].sort(), [MOVE.START, MOVE.WALK])
  assert.deepEqual(cells(r).filter(([x]) => x >= 4 && x <= 7).map(c => c[1]), [65, 66, 67, 68])
})

test('descending the flight is walks and drops', () => {
  const r = run(world({ fill: flight }), near(2, 64, 2), {}, { x: 9, y: 68, z: 2 })
  assert.equal(r.status, 'found')
  assert.ok(moves(r).every(m => [MOVE.START, MOVE.WALK, MOVE.DROP].includes(m)))
})

// walls keep the stairs reachable only from the way the case names: sides blocked, low end blocked, and the high end 3 up
const sealed3 = [[5, 64, 2, 5, 66, 2, 'stone'], [3, 64, 1, 5, 64, 1, 'stone']]
const approaches = [
  ['from the low side', 'east', { x: 2, y: 64, z: 2 }, [[4, 64, 1, 4, 66, 1, 'stone'], [4, 64, 3, 4, 66, 3, 'stone']], false],
  ['from the side', 'east', { x: 4, y: 64, z: 3 }, [[3, 64, 2, 3, 66, 2, 'stone'], ...sealed3], true],
  ['against the facing direction', 'west', { x: 4, y: 64, z: 3 }, [[3, 64, 2, 3, 66, 2, 'stone'], ...sealed3], true]
]
for (const [name, facing, from, walls, jumps] of approaches) {
  test(`stairs entered ${name}: jump ${jumps ? 'needed' : 'not needed'}`, () => {
    const w = world({ fill: [[4, 64, 2, 4, 64, 2, 'oak_stairs', { facing, half: 'bottom', shape: 'straight' }], ...walls] })
    const r = run(w, near(4, 65, 2), {}, from)
    assert.equal(r.status, 'found')
    assert.equal(r.path.cost.jumps > 0, jumps)
  })
}

test('a top half stairs block is a full obstacle: it takes a jump', () => {
  const w = world({ fill: [[4, 64, -2, 4, 64, 40, 'oak_stairs', { facing: 'east', half: 'top', shape: 'straight' }]] })
  const r = run(w, near(6, 64, 2))
  assert.equal(r.status, 'found')
  assert.equal(r.path.cost.jumps, 1)
})

// ---- goal side flood: a goal nothing can reach is reported cheaply ----

const floating = world({ fill: [[10, 66, 10, 14, 66, 14, 'stone']] })

// the flood runs only once the forward search has expanded floodAfter nodes; small here so tests need no huge worlds
const flooding = { floodAfter: 20 }

test('a goal on a platform 3 above the ground with no way up: goal-enclosed, cheaply', () => {
  const r = run(floating, near(12, 67, 12), flooding)
  assert.equal(r.reason, 'goal-enclosed')
  assert.ok(r.expanded > 0 && r.expanded < 4000)
})

test('an easy query never floods: the flood needs floodAfter expansions first', () => {
  const r = run(world(), near(12, 64, 2), { floodAfter: 20, goalFlood: 1 })
  assert.deepEqual([r.status, r.expanded < 20], ['found', true])
})

test('without the flood a sealed platform is exhausted by the forward search', () => {
  const r = run(floating, near(12, 67, 12))
  assert.equal(r.reason, 'exhausted')
})

test('a goal on the ground, the start on a pillar it can drop from: found', () => {
  const r = run(world({ fill: [[2, 64, 2, 2, 66, 2, 'stone']] }), near(8, 64, 2), {}, { x: 2, y: 67, z: 2 })
  assert.equal(r.status, 'found')
})

test('a flood larger than its budget hands over to the main search', () => {
  const small = run(floating, near(12, 67, 12), { ...flooding, goalFlood: 10 })
  assert.notEqual(small.reason, 'goal-enclosed')
  assert.notEqual(small.status, 'found')
  assert.equal(run(world(), near(20, 64, 20), { goalFlood: 10 }).status, 'found')
})

test('a goal sealed in by a wall is goal-enclosed, with a path to the nearest reachable cell', () => {
  const r = run(world({ fill: [[5, 64, -2, 5, 65, 40, 'stone']] }), near(8, 64, 2), flooding)
  assert.deepEqual([r.status, r.reason], ['partial', 'goal-enclosed'])
  assert.deepEqual(lastCell(r), [4, 64, 2])
})

test('a sealed goal no nearer than the start is none, as exhausted is', () => {
  const r = run(world({ fill: [[5, 64, -2, 5, 65, 40, 'stone']] }), near(8, 64, 2), flooding, { x: 4, y: 64, z: 2 })
  assert.deepEqual([r.status, r.reason, r.path], ['none', 'goal-enclosed', null])
})

test('an xz goal does not flood', () => {
  const r = run(floating, { kind: 'xz', x: 12, z: 12, range: 0 }, flooding)
  assert.notEqual(r.reason, 'goal-enclosed')
})

// ---- search box: nodes far outside the start-goal box are not searched; 'box' says that is why it ended ----

const bar = (z1, height = 2) => world({ fill: [[5, 64, -2, 5, 63 + height, z1, 'stone']] })
const boxes = [
  ['margin 3, the detour lies outside', bar(10), { margin: 3 }, 'partial', 'box'],
  ['default margin, the detour is inside', bar(10), {}, 'found', null],
  ['margin 3, sealed to the world edge', bar(40), { margin: 3 }, 'partial', 'box'],
  ['default margin, sealed to the world edge', bar(40), {}, 'partial', 'exhausted']
]
for (const [name, w, options, status, reason] of boxes) {
  test(`box: ${name}`, () => {
    const r = search(w, near(8, 64, 2), options)
    assert.deepEqual([r.status, r.reason], [status, reason])
  })
}

const step = world({ fill: [[5, 64, -2, 40, 64, 40, 'stone']] })
const heights = [
  ['near goal at y 65 spans 64..65', near(8, 65, 2), { yMargin: 0 }, 'found', null],
  ['an xz goal does not widen the y box', { kind: 'xz', x: 8, z: 2, range: 0 }, { yMargin: 0 }, 'partial', 'box'],
  ['yMargin 1 reaches y 65', { kind: 'xz', x: 8, z: 2, range: 0 }, { yMargin: 1 }, 'found', null],
  ['default yMargin', { kind: 'xz', x: 8, z: 2, range: 0 }, {}, 'found', null]
]
for (const [name, goal, options, status, reason] of heights) {
  test(`yMargin: ${name}`, () => {
    const r = run(step, goal, options)
    assert.deepEqual([r.status, r.reason], [status, reason])
  })
}
