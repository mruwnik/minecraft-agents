// Water, drops into and out of tight cells, and portals: hand-built worlds and the live courses.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { courseSnapshot } from './courses.mjs'
import { fixtureSnapshot, stateId } from './fixture.mjs'
import { plan, MOVE, DEFAULT_COSTS } from './planner.mjs'
import { defaultStateTable, WATER, PORTAL } from './blocks.mjs'

const table = defaultStateTable()
const planCourse = (name, options) => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return plan(snapshot, { from, goal }, options)
}
const verdict = r => `${r.status}${r.reason ? `/${r.reason}` : ''}`
const near = (x, y, z, range = 1) => ({ kind: 'near', x, y, z, range })
const run = (snapshot, from, goal, options) => plan(snapshot, { from, goal }, options)
const moves = r => r.path.steps.map(s => s.move)
const kindAt = (snapshot, x, y, z) => table.kind[snapshot.stateAt(x, y, z)]
const hazardAt = (snapshot, x, y, z) => table.hazard[snapshot.stateAt(x, y, z)]

// stone to y 63: feet cells are y 64
const world = (fill = [], blocks = []) => fixtureSnapshot({ fill: [[-2, 60, -2, 60, 63, 40, 'stone'], ...fill], blocks })
const from2 = { x: 2, y: 64, z: 2 }

// ---- portals ----

test('course portal: the path goes around, no body cell (feet or head) is a portal cell', () => {
  const { snapshot, from, goal } = courseSnapshot('portal')
  const r = plan(snapshot, { from, goal })
  assert.equal(verdict(r), 'found')
  assert.ok(r.path.steps.every(s => hazardAt(snapshot, s.x, s.y, s.z) !== PORTAL && hazardAt(snapshot, s.x, s.y + 1, s.z) !== PORTAL))
})

const portalProps = { nether_portal: { axis: 'x' } }
const portalAt = name => [8, 64, 2, 8, 65, 2, name, portalProps[name]]

// a one wide corridor whose only way through is a portal
const corridor = name => world([[8, 64, -2, 8, 70, 40, 'stone'], portalAt(name)])
const portals = ['nether_portal', 'end_portal', 'end_gateway']
for (const name of portals) {
  test(`${name} filling the only way: not found`, () => {
    const r = run(corridor(name), from2, near(14, 64, 2))
    assert.notEqual(r.status, 'found')
    assert.ok((r.path?.steps ?? []).every(s => s.x < 8))
  })
}

test('the same corridor with the portal removed is passable', () => {
  const open = world([[8, 64, -2, 8, 70, 40, 'stone'], [8, 64, 2, 8, 65, 2, 'air']])
  assert.equal(run(open, from2, near(14, 64, 2)).status, 'found')
})

// the goal is the portal: the body may enter it, and only there
const goalCases = [
  ['a nether portal two high, goal on its feet cell', 'nether_portal', near(8, 64, 2, 0)],
  ['an end gateway, goal on it', 'end_gateway', near(8, 64, 2, 0)]
]
for (const [what, name, goal] of goalCases) {
  test(`goal in a portal: ${what} is found, the last step is in it`, () => {
    const r = run(world([portalAt(name)]), from2, goal)
    assert.equal(r.status, 'found')
    assert.deepEqual([r.path.steps.at(-1).x, r.path.steps.at(-1).z], [8, 2])
  })
}

test('with a goal beside the portal, the portal cell is still not entered', () => {
  const open = world([[8, 64, 2, 8, 65, 2, 'nether_portal', { axis: 'x' }]])
  const r = run(open, from2, near(8, 64, 3, 0))
  assert.equal(r.status, 'found')
  assert.ok(r.path.steps.every(s => !(s.x === 8 && s.z === 2)))
})

// ---- drops into and out of tight cells ----

// a mound x 0..4 (stand 66, 2 up) in a field; bamboo columns where given; the start is on top
const mound = bamboo => world([
  [0, 64, 0, 4, 65, 4, 'stone'],
  ...bamboo.map(([x, z]) => [x, 64, z, x, 70, z, 'bamboo'])
])
const onMound = { x: 2, y: 66, z: 2 }

// [what, bamboo columns, goal]: the landing cell (5,64,2) has bamboo within one cell
const intoTight = [
  ['one bamboo beside the landing', [[6, 2]], near(5, 64, 2, 0)],
  ['bamboo on both sides of the landing', [[6, 1], [6, 3]], near(5, 64, 2, 0)],
  ['bamboo diagonal to the landing', [[6, 3]], near(5, 64, 2, 0)]
]
for (const [what, bamboo, goal] of intoTight) {
  test(`a drop of 2 into a tight cell: ${what} is found by a DROP`, () => {
    const r = run(mound(bamboo), onMound, goal)
    assert.equal(r.status, 'found')
    assert.ok(moves(r).includes(MOVE.DROP))
    assert.deepEqual([r.path.steps.at(-1).x, r.path.steps.at(-1).y, r.path.steps.at(-1).z], [5, 64, 2])
  })
}

test('a drop into a tight cell lands on a point the bamboo leaves free', () => {
  const r = run(mound([[6, 2]]), onMound, near(5, 64, 2, 0))
  const last = r.path.steps.at(-1)
  assert.ok(Math.abs(last.px - 6.5) >= 0.31 + 0.0625 - 1e-9 || Math.abs(last.pz - 2.5) >= 0.31 + 0.0625 - 1e-9)
})

// out of a tight takeoff cell: bamboo on the mound beside the edge cell
test('a drop out of a tight cell is found', () => {
  const world2 = world([[0, 64, 0, 4, 65, 4, 'stone'], [3, 66, 2, 3, 70, 2, 'bamboo']])
  const r = run(world2, { x: 4, y: 66, z: 2 }, near(5, 64, 2, 0))
  assert.equal(r.status, 'found')
  assert.ok(moves(r).includes(MOVE.DROP))
})

// gap jumps from or into a tight cell stay refused
const gapWorld = bamboo => world([
  [5, 60, -2, 5, 63, 40, 'air'],
  ...bamboo.map(([x, z]) => [x, 64, z, x, 70, z, 'bamboo'])
])
// a row of bamboo along the whole gap's far (or near) side, so that every cell of the edge is tight
const row = x => Array.from({ length: 43 }, (_, k) => [x, k - 2])
const gapCases = [
  ['into a tight cell', row(7), from2, near(6, 64, 2, 0), ['partial', 'none']],
  ['out of a tight cell', row(3), { x: 4, y: 64, z: 2 }, near(7, 64, 2, 0), ['partial', 'none']],
  ['between plain cells (control)', [], from2, near(7, 64, 2, 0), ['found']]
]
for (const [what, bamboo, from, goal, statuses] of gapCases) {
  test(`a gap jump ${what}: ${statuses[0]}`, () => {
    assert.ok(statuses.includes(run(gapWorld(bamboo), from, goal).status))
  })
}

// ---- water: a pond ----

const lake = world([[10, 61, -2, 20, 63, 40, 'water']])

test('swimming across a pond: found, swims, water seconds and air reported', () => {
  const r = run(lake, from2, near(26, 64, 2))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /swims \d+/)
  assert.ok(r.path.cost.waterSeconds > 4)
  assert.ok(r.path.cost.airMin === DEFAULT_COSTS.airSupply)
  assert.ok(r.path.steps.some(s => kindAt(lake, s.x, s.y, s.z) === WATER))
})

test('swimming costs swimH seconds a block, overridable through options.costs', () => {
  const base = run(lake, from2, near(26, 64, 2)).path.cost.seconds
  const slow = run(lake, from2, near(26, 64, 2), { costs: { swimH: 5 } }).path.cost.seconds
  assert.ok(slow > base + 10)
})

test('the default water costs are the stated ones', () => {
  const { swimH, swimUp, swimDown, exit, current, bubbleUp, bubbleDown, airSupply, airLimit, maxWaterDrop, dripleaf } = DEFAULT_COSTS
  assert.deepEqual({ swimH, swimUp, swimDown, exit, current, bubbleUp, bubbleDown, airSupply, airLimit, maxWaterDrop, dripleaf },
    { swimH: 0.5, swimUp: 0.3, swimDown: 0.35, exit: 0.6, current: 0.3, bubbleUp: 0.08, bubbleDown: 0.12, airSupply: 15, airLimit: 12, maxWaterDrop: 64, dripleaf: 0.2 })
})

test('a start in the water: found, and the way out is an EXIT or a walk out', () => {
  const r = run(lake, { x: 15, y: 63, z: 2 }, near(2, 64, 2))
  assert.equal(r.status, 'found')
  assert.ok(moves(r).includes(MOVE.EXIT))
})

test('a goal in the water is standable and reached', () => {
  const r = run(lake, from2, near(15, 62, 2, 0))
  assert.equal(r.status, 'found')
  assert.deepEqual([r.path.steps.at(-1).x, r.path.steps.at(-1).y, r.path.steps.at(-1).z], [15, 62, 2])
})

// ---- water: drops ----

// a mound `high` blocks tall at x 0..4, a pond `deep` blocks deep from x 5 east
const drop = (high, deep) => world([[0, 64, 0, 4, 63 + high, 4, 'stone'], [5, 64 - deep, -2, 20, 63, 40, 'water']])
// [high, deep]: any height into water of any depth, even one block, is a DROP with no risk
const drops = [[3, 1], [8, 1], [8, 3], [20, 1], [40, 2], [60, 4]]
for (const [high, deep] of drops) {
  test(`a drop of ${high} into ${deep} deep water: a DROP with no risk`, () => {
    const r = run(drop(high, deep), { x: 2, y: 64 + high, z: 2 }, near(12, 63, 2, 1))
    assert.equal(r.status, 'found')
    assert.ok(moves(r).includes(MOVE.DROP))
    assert.equal(r.path.cost.risk, 0)
    assert.equal(r.path.cost.maxDrop, 0)
    assert.equal(r.path.cost.waterDrop, high + 1) // from the stand height to the floor of the water cell the body lands in
  })
}

test('the drop summary says it lands in water', () => {
  const r = run(drop(8, 2), { x: 2, y: 72, z: 2 }, near(12, 63, 2, 1))
  assert.match(r.path.summary, /drops 9 into water/)
})

test('a drop higher than costs.maxWaterDrop is refused', () => {
  const r = run(drop(20, 2), { x: 2, y: 84, z: 2 }, near(12, 63, 2, 1), { costs: { maxWaterDrop: 10 } })
  assert.notEqual(r.status, 'found')
})

test('a drop onto land of the same height is still limited by maxDrop', () => {
  const r = run(world([[0, 64, 0, 4, 71, 4, 'stone']]), { x: 2, y: 72, z: 2 }, near(8, 64, 2, 0), { goalFlood: 0 })
  assert.notEqual(r.status, 'found')
})

// a pond beyond the mound, with a stone roof over its two nearest columns or without
const roofed = roof => world([
  [0, 64, 0, 4, 71, 4, 'stone'],
  [5, 61, -2, 20, 63, 40, 'water'],
  ...(roof ? [[5, 66, -2, 6, 66, 40, 'stone']] : [])
])
test('a roof over the landing columns stops the fall into water through them; the same pond uncovered is reached', () => {
  const found = roof => run(roofed(roof), { x: 2, y: 72, z: 2 }, near(12, 63, 2, 1), { goalFlood: 0 }).status === 'found'
  assert.deepEqual([found(true), found(false)], [false, true])
})

// ---- water: columns ----

// a stone block x 10..12, z 0..2 with a one wide column of `fluid` at (11, z 1), `depth` cells deep from y 64 up; entered from
// the west at the bottom, left over the top onto a cap at stand height 64 + depth
const column = (depth, { bottom = 'stone', fluid = 'water', props = {} } = {}) => world([
  [10, 64, 0, 12, 63 + depth, 2, 'stone'],
  [11, 63, 1, 11, 63, 1, bottom],
  [11, 64, 1, 11, 63 + depth, 1, fluid, props],
  [10, 64, 1, 10, 65, 1, 'air'],
  [13, 63 + depth, 0, 16, 63 + depth, 2, 'stone']
])
const bubbleUp = { fluid: 'bubble_column', props: { drag: false }, bottom: 'soul_sand' }
const bubbleDown = { fluid: 'bubble_column', props: { drag: true }, bottom: 'magma_block' }
const up = depth => [from2, near(14, 64 + depth, 1, 1)]
const topOf = depth => ({ x: 12, y: 64 + depth, z: 1 })
const down = depth => [topOf(depth), near(5, 64, 1, 1)]

test('a 20 deep plain water column up: found, "water column up"', () => {
  const r = run(column(20), ...up(20))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /water column up (18|19|20)/)
  assert.ok(r.path.cost.airMin < DEFAULT_COSTS.airSupply)
  assert.match(r.path.summary, /lowest air \d+ s/)
})

test('a 40 deep plain water column up is refused for air', () => {
  const r = run(column(40), ...up(40))
  assert.notEqual(r.status, 'found')
  assert.equal(r.reason, 'air')
})

test('the same 40 deep column with a higher airLimit is passable', () => {
  const r = run(column(40), ...up(40), { costs: { airLimit: 14 } })
  assert.equal(r.status, 'found')
})

test('a 40 deep bubble column (soul sand) up is found: the bubbles refill the air', () => {
  const r = run(column(40, bubbleUp), ...up(40))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /bubble lift up 3\d/)
  assert.equal(r.path.cost.airMin, DEFAULT_COSTS.airSupply)
})

test('a bubble column rises at bubbleUp seconds a block', () => {
  const base = run(column(20, bubbleUp), ...up(20)).path.cost.seconds
  const slow = run(column(20, bubbleUp), ...up(20), { costs: { bubbleUp: 1.08 } }).path.cost.seconds
  assert.ok(Math.abs(slow - base - 19) < 1.5)
})

test('a magma (drag) column cannot be swum up', () => {
  const r = run(column(20, bubbleDown), ...up(20))
  assert.notEqual(r.status, 'found')
})

test('a magma column down is found, "magma column down"', () => {
  const r = run(column(20, bubbleDown), ...down(20))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /magma column down 1\d/)
})

test('a soul sand (lift) column cannot be swum down', () => {
  const r = run(column(20, bubbleUp), ...down(20))
  assert.notEqual(r.status, 'found')
})

test('a plain 20 deep water column down: found, "water column down"', () => {
  const r = run(column(20), ...down(20))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /water column down/)
})

test('a plain column down costs swimDown a block, a drag column bubbleDown', () => {
  const plain = run(column(20), ...down(20)).path.cost.seconds
  const magma = run(column(20, bubbleDown), ...down(20)).path.cost.seconds
  assert.ok(plain - magma > 19 * (DEFAULT_COSTS.swimDown - DEFAULT_COSTS.bubbleDown) - 2)
})

// ---- water: breath ----

// a channel of water `len` long inside stone, two high (so every node in it has its head under water), entered and left through
// air cells at its ends; with `shaft`, a 3 high water shaft up to open air in the middle where the body can breathe
const tunnel = (len, shaft) => {
  const mid = 11 + (len >> 1)
  return world([
    [10, 64, -2, 12 + len, 68, 40, 'stone'],
    [11, 64, 1, 10 + len, 65, 1, 'water'],
    [10, 64, 1, 10, 65, 1, 'air'],
    [11 + len, 64, 1, 12 + len, 65, 1, 'air'],
    ...(shaft ? [[mid, 66, 1, mid, 68, 1, 'air'], [mid, 66, 1, mid, 67, 1, 'water']] : [])
  ])
}
// [length, shaft, found]: 30 blocks of swimming is 15 s, past the 12 s limit; a breath in the middle makes it two halves
const tunnels = [[10, false, true], [30, false, false], [30, true, true]]
for (const [len, shaft, reachable] of tunnels) {
  test(`a submerged channel ${len} long${shaft ? ' with an air shaft' : ''}: ${reachable ? 'found' : 'refused for air'}`, () => {
    const r = run(tunnel(len, shaft), from2, near(13 + len, 64, 1, 0), { goalFlood: 0 })
    assert.deepEqual([r.status === 'found', r.reason], [reachable, reachable ? null : 'air'])
  })
}

test('a breath in the shaft refills the air: the lowest air of the shafted channel is above the limit of the plain one', () => {
  const r = run(tunnel(30, true), from2, near(43, 64, 1, 0), { goalFlood: 0 })
  assert.ok(r.path.cost.airMin > DEFAULT_COSTS.airSupply - DEFAULT_COSTS.airLimit)
  assert.ok(r.path.cost.waterSeconds > 15)
})

// ---- water: exits ----

// a pool of water cells to y 63 whose far bank is `bank` blocks above the cell: stand heights 64 (flush), 65, 66
const bankWorld = bank => world([
  [10, 61, -2, 14, 63, 40, 'water'],
  [15, 64, -2, 20, 63 + bank, 40, 'stone']
])
// Live, 26.1: a floating body leaves the water onto land flush with the water's top face (5/5), not onto land one higher (0/5,
// 0.42-0.63 short at the peak) or two (0/5). The bank here is `bank` above flush.
const banks = [[0, true], [1, false], [2, false]]
for (const [bank, reachable] of banks) {
  test(`a bank ${bank} above the flush stand height: ${reachable ? 'found' : 'not found'}`, () => {
    const r = run(bankWorld(bank), from2, near(18, 64 + bank, 2, 0), { goalFlood: 0, margin: 8 })
    assert.equal(r.status === 'found', reachable)
  })
}

// a lake 3 deep walled in on every side by stone: the far bank is the only way on, `bank` above flush
const closedLake = bank => fixtureSnapshot({ fill: [
  [-2, 50, 0, 30, 63, 6, 'stone'],   [10, 61, 0, 14, 63, 6, 'water'], [15, 64, 0, 30, 63 + bank, 6, 'stone']
] })
const lakeBanks = [
  ['flush banks', 0, 'found', null],
  ['banks 1 above flush', 1, 'partial', 'exhausted'],
  ['banks 2 above flush', 2, 'partial', 'exhausted']
]
for (const [what, bank, status, reason] of lakeBanks) {
  test(`a lake with ${what}: ${status}${reason ? ' ' + reason : ''}`, () => {
    const r = run(closedLake(bank), { x: 5, y: 64, z: 3 }, near(20, 64 + bank, 3, 0), { goalFlood: 0 })
    assert.deepEqual([r.status, r.reason], [status, reason])
  })
}

// 1 deep, a floor under it: the body is standing, not floating, so the walk and jump rules apply (jump up to 1.25)
const wade = rise => world([
  [8, 64, -2, 12, 64, 40, 'water'],
  [13, 64, -2, 20, 63 + rise, 40, 'stone']
])
const wadeCases = [
  ['a block the same height as the water surface (+1 over the floor)', 1, MOVE.JUMP],
  ['no block: level ground beyond', 0, MOVE.WALK]
]
for (const [what, rise, move] of wadeCases) {
  test(`wading in water 1 deep onto ${what}: found, the step out is a ${move === MOVE.JUMP ? 'JUMP' : 'WALK'}`, () => {
    const r = run(wade(rise), from2, near(14, 64 + rise, 2, 0), { goalFlood: 0, margin: 8 })
    assert.equal(r.status, 'found')
    const out = r.path.steps.findIndex((s, k) => k > 0 && !s.swim && r.path.steps[k - 1].swim)
    assert.equal(r.path.steps[out].move, move)
  })
}

test('wading in water 1 deep: a block two over the floor is not climbed out onto', () => {
  const r = run(wade(2), from2, near(14, 66, 2, 0), { goalFlood: 0, margin: 8 })
  assert.notEqual(r.status, 'found')
})

// ---- courses: water ----

const found = [
  'bubble-up', 'magma-down', 'water20-up', 'water20-down', 'water2-up', 'waterfall-up', 'waterfall-down',
  'dropshaft-1deep', 'dropshaft-2deep', 'drop8-open', 'drop3-water', 'lake20-flush'
]
for (const name of found) {
  test(`course ${name}: found`, () => {
    assert.equal(verdict(planCourse(name)), 'found')
  })
}

test('course drop8-open: a drop of 8 into the pond', () => {
  const r = planCourse('drop8-open')
  assert.match(r.path.summary, /drops 8 into water/)
})

test('course water20-up: climbs the column, water column up', () => {
  assert.match(planCourse('water20-up').path.summary, /water column up/)
})

test('course bubble-up: bubble lift up, the air never runs down', () => {
  const r = planCourse('bubble-up')
  assert.match(r.path.summary, /bubble lift up/)
  assert.equal(r.path.cost.airMin, DEFAULT_COSTS.airSupply)
})

test('course magma-down: magma column down', () => {
  assert.match(planCourse('magma-down').path.summary, /magma column down/)
})

test('course waterfall-up: swims up the falling water, current included', () => {
  const r = planCourse('waterfall-up')
  assert.match(r.path.summary, /water column up/)
  const slow = planCourse('waterfall-up', { costs: { current: 0 } })
  assert.ok(r.path.cost.seconds > slow.path.cost.seconds + 1)
})

// shallow and channel courses the planner now crosses without a land route
const crosses = ['farm-channels', 'lilypads', 'coral', 'dripleaf', 'lake20-wade', 'stream3', 'swamp', 'frozen-river', 'dropshaft-1deep-in', 'drop8-water']
for (const name of crosses) {
  test(`course ${name}: found`, () => {
    assert.equal(verdict(planCourse(name)), 'found')
  })
}

// every bank of these lakes is more than flush with the water's top face (lake20: 2 up, lake20-high: 2 up, a 3 high trench wall in
// river-current): the body cannot climb out, so there is no way across
const sealed = ['lake20', 'lake20-high', 'river-current']
for (const name of sealed) {
  test(`course ${name}: no way across, ${name === 'river-current' ? 'the trench walls are 3 high' : 'the banks are too high to climb out onto'}`, () => {
    assert.notEqual(planCourse(name).status, 'found')
  })
}

// a pond whose only crossing is five big dripleaf leaves: they lie on the water's top cell, so the cells under them are closed
const leafPond = world([[10, 61, -2, 14, 63, 40, 'water'], [10, 63, -2, 14, 63, 40, 'big_dripleaf', { tilt: 'none', waterlogged: false, facing: 'east' }]])
test('a pond crossed on five big dripleaf leaves: half a hp of risk each, costs.dripleaf seconds each', () => {
  const goal = near(16, 64, 2, 0)
  const base = run(leafPond, from2, goal).path.cost
  const slow = run(leafPond, from2, goal, { costs: { dripleaf: 3.2 } }).path.cost
  assert.equal(base.risk, 2.5)
  assert.ok(Math.abs(slow.seconds - base.seconds - 5 * 3) < 1e-6)
})

test('moves into flowing water cost costs.current more', () => {
  const base = planCourse('waterfall-up').path.cost.seconds
  const dear = planCourse('waterfall-up', { costs: { current: 0.4 } }).path.cost.seconds
  assert.ok(dear > base)
})

// ---- water: dominance of the surface over open-water diving ----

// stone to y 63 under a lake 6 deep (y 58..63), 20 wide (x 10..29), z -2..14, spans the world; banks at y 63 are stone, feet cells y 64
const deepLake = (fill = []) => fixtureSnapshot({ fill: [[-2, 50, -2, 40, 63, 14, 'stone'], [10, 58, -2, 29, 63, 14, 'water'], ...fill] })

test('a lake 6 deep and 20 wide is crossed at the surface without filling the volume with nodes', () => {
  const r = run(deepLake(), { x: 2, y: 64, z: 6 }, near(36, 64, 6))
  assert.equal(r.status, 'found')
  assert.ok(r.expanded < 700, `expanded ${r.expanded}`)
})

test('an underwater tunnel under a stone ceiling between two pools is still found', () => {
  // two pools (x 10..14, x 22..26, air over them) joined by a water-filled 1x2 passage (y 62..63) under solid stone: no way over
  const snapshot = fixtureSnapshot({ fill: [
    [-2, 50, -2, 40, 75, 14, 'stone'],
    [10, 58, 4, 14, 63, 8, 'water'], [22, 58, 4, 26, 63, 8, 'water'], [15, 62, 6, 21, 63, 6, 'water'],
    [10, 64, 4, 14, 70, 8, 'air'], [22, 64, 4, 26, 70, 8, 'air']
  ] })
  const r = run(snapshot, { x: 12, y: 63, z: 6 }, near(24, 63, 6, 0))
  assert.equal(r.status, 'found')
  assert.ok(r.path.steps.some(s => s.x === 18 && s.y <= 63))
})

// ---- magma: where a route may end, and the columns beside a lift ----

const onMagma = (snapshot, s) => {
  const id = snapshot.stateAt(s.x, s.y, s.z)
  return table.bubble[id] === 2 || snapshot.stateAt(s.x, s.y - 1, s.z) === stateId('magma_block')
}

// the body died idling on magma: a route never ends on a magma block or in a magma bubble column
const magmaEnds = [
  // [what, world, goal, status]: the nearest node that is not on magma satisfies the goal, or the route is partial
  ['a goal range 1 around a magma block with stone beside it', world([[9, 63, 2, 10, 63, 2, 'magma_block']]), near(10, 64, 2, 1), 'found'],
  ['a goal on a magma block', world([[10, 63, 2, 10, 63, 2, 'magma_block']]), near(10, 64, 2, 0), 'partial'],
  ['a goal in a magma bubble column', column(20, bubbleDown), near(11, 74, 1, 0), 'partial']
]
for (const [what, snapshot, goal, status] of magmaEnds) {
  test(`${what}: ${status}, the last step is not on magma`, () => {
    const r = run(snapshot, from2, goal, { goalFlood: 0 })
    assert.equal(r.status, status)
    assert.ok(!onMagma(snapshot, r.path.steps.at(-1)))
  })
}

// a one-wide corridor (stone walls at z 1 and 3) past a magma bubble column cell that opens off it at (10, 64, 3)
const beside = world([
  [-2, 64, 1, 40, 66, 1, 'stone'], [-2, 64, 3, 40, 66, 3, 'stone'],
  [10, 63, 3, 10, 63, 3, 'magma_block'], [10, 64, 3, 10, 66, 3, 'bubble_column', { drag: true }]
])
const besideCases = [
  ['by default: 1 risk', {}, 1],
  ['costs.besideMagmaColumn 0', { besideMagmaColumn: 0 }, 0],
  ['costs.besideMagmaColumn 3', { besideMagmaColumn: 3 }, 3]
]
for (const [what, costs, risk] of besideCases) {
  test(`walking past a magma bubble column at the same level, ${what}`, () => {
    const r = run(beside, from2, near(18, 64, 2, 0), { goalFlood: 0, costs })
    assert.equal(r.status, 'found')
    assert.equal(r.path.cost.risk, risk)
  })
}

// a lift (soul sand) at x 9 and a magma pool x 11..15, side by side in a one-wide stone wall y 64..70 (stand 71 on top; walls 10 higher either side keep the ground out and leave the platform one cell wide); platform cell
// (10, 71, 3) between them. The lift's top is left sideways onto that platform; the pool is the only way on to x 17.
const pools = (extra = []) => world([
  [8, 64, 3, 18, 70, 3, 'stone'], [8, 64, 2, 18, 80, 2, 'stone'], [8, 64, 4, 18, 80, 4, 'stone'],
  [9, 63, 3, 9, 63, 3, 'soul_sand'], [9, 64, 3, 9, 70, 3, 'bubble_column', { drag: false }],
  [11, 63, 3, 15, 63, 3, 'magma_block'], [11, 64, 3, 15, 70, 3, 'bubble_column', { drag: true }],
  ...extra
])
const inLift = { x: 9, y: 64, z: 3, px: 9.5, pz: 3.5 }
test('after leaving the lift sideways the path does not drop into the magma pool beside the platform', () => {
  const snapshot = pools()
  const r = run(snapshot, inLift, near(17, 71, 3, 0), { goalFlood: 0 })
  assert.notEqual(r.status, 'found')
  assert.ok((r.path?.steps ?? []).every(s => table.bubble[snapshot.stateAt(s.x, s.y, s.z)] !== 2))
})

test('the step after the exit may enter the magma column when the path goes down it', () => {
  // an opening at the pool's bottom east end: the goal is out of it at the floor
  const down = pools([[16, 64, 3, 17, 65, 3, 'air']])
  const r = run(down, inLift, near(17, 64, 3, 0), { goalFlood: 0 })
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /magma column down/)
})
