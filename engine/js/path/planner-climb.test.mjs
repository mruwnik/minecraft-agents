// Climbing: ladders, vines, scaffolding and trapdoors over ladders, on the live courses and on small hand-built worlds.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { courseSnapshot } from './courses.mjs'
import { fixtureSnapshot } from './fixture.mjs'
import { plan, MOVE, DEFAULT_COSTS } from './planner.mjs'

const planCourse = (name, options) => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return plan(snapshot, { from, goal }, options)
}
const verdict = r => `${r.status}${r.reason ? `/${r.reason}` : ''}`
const moveSet = r => new Set(r.path.steps.map(s => s.move))
const near = (x, y, z, range = 1) => ({ kind: 'near', x, y, z, range })

const found = [
  'ladder-up', 'ladder-down', 'vine-up', 'vine-down', 'ladder6', 'vines6',
  'lad-shaft20-up', 'lad-shaft20-down', 'lad-wall20-up', 'lad-wall20-down',
  'lad-raised1', 'lad-trap-closed', 'twisting-up', 'scaffold-up', 'scaffold-down', 'weeping'
]
for (const name of found) {
  test(`course ${name}: found`, () => {
    const r = planCourse(name)
    const goal = courseSnapshot(name).goal
    assert.equal(verdict(r), 'found')
    assert.ok(Math.hypot(r.path.steps.at(-1).x - goal.x, r.path.steps.at(-1).z - goal.z) <= 1.5)
    assert.ok(Math.abs(r.path.steps.at(-1).y - goal.y) <= 1)
  })
}

// [course, the move it must use]
const uses = [
  ['lad-raised1', MOVE.JUMP_CLIMB],
  ['lad-trap-closed', MOVE.OPEN],
  ['ladder-up', MOVE.CLIMB_UP],
  ['vine-up', MOVE.CLIMB_UP],
  ['twisting-up', MOVE.CLIMB_UP],
  ['scaffold-up', MOVE.CLIMB_UP],
  ['ladder-down', MOVE.CLIMB_DOWN],
  ['vine-down', MOVE.CLIMB_DOWN],
  ['scaffold-down', MOVE.CLIMB_DOWN],
  ['lad-wall20-down', MOVE.CLIMB_DOWN]
]
for (const [name, move] of uses) {
  test(`course ${name} uses move ${move}`, () => {
    assert.ok(moveSet(planCourse(name)).has(move))
  })
}

test('the closed wooden trapdoor over the ladder is one OPEN, recorded with its block position', () => {
  const { path } = planCourse('lad-trap-closed')
  const opens = path.steps.filter(s => s.move === MOVE.OPEN)
  assert.equal(opens.length, 1)
  assert.deepEqual(opens[0].opens, [{ x: 2880, y: 166, z: 3216 }])
  assert.equal(path.cost.opens, 1)
})

// Live, 26.1: ladder facing west, an OPEN trapdoor above it facing east (mismatched): the body climbs and steps off, 5/5.
// The same with the trapdoor facing west (matching, which vanilla makes climbable): stuck at feet y 74.12, 0/5.
// (The matching case is measured once, 2026-10-03, and may be a test-geometry artifact.)
test('an open trapdoor over a ladder, facing differently: climbed without an OPEN', () => {
  const { path } = run(trapOver({ open: true, facing: 'east' }), from5, near(13, 73, 5))
  assert.equal(path.cost.opens, 0)
  assert.ok(path.cost.climbed >= 5)
})

test('course lad-trap-open (open trapdoor facing the ladder\'s way): not found', () => {
  assert.notEqual(planCourse('lad-trap-open').status, 'found')
})

// [course, blocks climbed]: ladder cells 161..166 are 5 climbs and the step up; the 20 high shafts are 19 or 20
test('the cost vector counts blocks climbed, up and down', () => {
  assert.equal(planCourse('ladder-up').path.cost.climbed, 5)
  // (with drops over 1 refused: a fall of 3 off the ladder's front is free, so by default the body steps off the ladder and falls;
  // even then it steps off the last rung, a drop of 1 out of the ladder's tight cell, so 4 of the 5 rungs are climbed)
  assert.equal(planCourse('ladder-down', { maxDrop: 1 }).path.cost.climbed, 4)
  assert.equal(planCourse('weeping').path.cost.climbed, 0)
})

const summaries = [
  ['ladder-up', /ladder up 5/],
  ['vine-down', /vines down/],
  ['scaffold-up', /scaffolding up/],
  ['lad-trap-closed', /opens 1 trapdoor/]
]
for (const [name, pattern] of summaries) {
  test(`summary of ${name} matches ${pattern}`, () => {
    assert.match(planCourse(name).path.summary, pattern)
  })
}

test('a climbed block costs climbUp seconds up and climbDown seconds down, both overridable through options.costs', () => {
  const base = planCourse('ladder-up').path.cost
  const slow = planCourse('ladder-up', { costs: { climbUp: 5 } }).path.cost
  assert.ok(Math.abs(slow.seconds - base.seconds - base.climbed * (5 - DEFAULT_COSTS.climbUp)) < 1e-6)
  const down = planCourse('ladder-down').path.cost
  const slowDown = planCourse('ladder-down', { costs: { climbDown: 3 } }).path.cost
  assert.ok(Math.abs(slowDown.seconds - down.seconds - down.climbed * (3 - DEFAULT_COSTS.climbDown)) < 1e-6)
})

test('an OPEN costs costs.open seconds', () => {
  const base = planCourse('lad-trap-closed').path.cost.seconds
  const dear = planCourse('lad-trap-closed', { costs: { open: 11 } }).path.cost.seconds
  assert.ok(Math.abs(dear - base - (11 - DEFAULT_COSTS.open)) < 1e-6)
})

test('default costs are the vanilla rates', () => {
  assert.deepEqual(
    { up: DEFAULT_COSTS.climbUp, down: DEFAULT_COSTS.climbDown, open: DEFAULT_COSTS.open },
    { up: 0.43, down: 0.33, open: 1.0 }
  )
})

// the live server pops the ladders whose wall the tunnel's fill removed (y 171 and 172): the replay drops them too, so the
// body climbs to 170 and steps up into the tunnel
test('lad-midlanding: the ladder cells in the tunnel are gone, the climb ends at 170 and the tunnel is entered', () => {
  const r = planCourse('lad-midlanding')
  assert.equal(verdict(r), 'found')
  assert.match(r.path.summary, /ladder up/)
  const into = r.path.steps.findIndex(s => s.x === 2881 && s.y === 171)
  assert.ok(into > 0 && r.path.steps[into - 1].x === 2880 && r.path.steps[into - 1].y === 170)
})

// ---- the ladder with a gap ----

test('ladder-down by default: stepping off the ladder into a fall of 3 (no damage) is cheaper than climbing all the way', () => {
  const r = planCourse('ladder-down')
  assert.equal(verdict(r), 'found')
  assert.equal(r.path.cost.risk, 0)
  assert.ok(r.path.cost.maxDrop === 3)
})

test('lad-gap going up: refused through the one block gap, with its own reason', () => {
  const r = planCourse('lad-gap')
  assert.notEqual(r.status, 'found')
  assert.equal(r.reason, 'ladder-gap')
})

const gapDown = () => {
  const { snapshot } = courseSnapshot('lad-gap')
  return plan(snapshot, { from: { x: 2886, y: 171, z: 3216, px: 2886.5, pz: 3216.5 }, goal: near(2860, 161, 3216) })
}
test('lad-gap going down: the short fall through the gap is caught by the ladder below', () => {
  const r = gapDown()
  assert.equal(verdict(r), 'found')
  assert.ok(r.path.steps.some((s, k) => s.move === MOVE.DROP && r.path.steps[k - 1].x === 2880 && r.path.steps[k - 1].y === 165))
})

// ---- hand-built worlds: stone floor to y 63, so feet cells are y 64 ----

const world = fill => fixtureSnapshot({ fill: [[-2, 60, -2, 60, 63, 40, 'stone'], ...fill] })
const run = (snapshot, from, goal, options) => plan(snapshot, { from, goal }, options)
const from5 = { x: 2, y: 64, z: 5 }

// a ladder up the west face of a stone block 10 high, exit at the top
const shaft = world([
  [10, 64, 0, 20, 73, 10, 'stone'],
  [9, 64, 5, 9, 73, 5, 'ladder', { facing: 'west' }]
])
test('a ladder up a wall 10 high with the exit at the top: found, climbing 9 or 10', () => {
  const r = run(shaft, from5, near(15, 74, 5))
  assert.equal(r.status, 'found')
  assert.ok(r.path.cost.climbed >= 9)
  assert.match(r.path.summary, /ladder up (9|10)/)
})

// a one-wide shaft with a ladder (facing west) up to a trapdoor ceiling; the cap beside it is the goal's floor
const trapShaft = (trap, props = { open: false, facing: 'east' }) => world([
  [8, 64, 4, 10, 72, 6, 'stone'],
  [9, 64, 5, 9, 72, 5, 'air'],
  [8, 64, 5, 8, 65, 5, 'air'],
  [11, 72, 4, 15, 72, 6, 'stone'],
  [9, 64, 5, 9, 71, 5, 'ladder', { facing: 'west' }],
  [9, 72, 5, 9, 72, 5, trap, { half: 'top', ...props }]
])
const trapOver = props => trapShaft('oak_trapdoor', props)
// [trapdoor, statuses it may end in]: iron ones cannot be opened by hand
const trapCases = [['oak_trapdoor', ['found']], ['copper_trapdoor', ['found']], ['iron_trapdoor', ['partial', 'none']]]
for (const [trap, statuses] of trapCases) {
  test(`a closed ${trap} over the ladder: ${statuses[0]}`, () => {
    const r = run(trapShaft(trap), from5, near(13, 73, 5))
    assert.ok(statuses.includes(r.status), r.status)
  })
}

// [what, trapdoor state, status]: the opened state keeps the facing, so a closed trapdoor facing like the ladder stays shut for the climb
const trapFacings = [
  ['open, facing east (differs from the ladder)', { open: true, facing: 'east' }, 'found', 0],
  ['open, facing south (differs)', { open: true, facing: 'south' }, 'found', 0],
  ['open, facing west (matches the ladder)', { open: true, facing: 'west' }, 'partial', 0],
  ['closed, facing east (differs)', { open: false, facing: 'east' }, 'found', 1],
  ['closed, facing north (differs)', { open: false, facing: 'north' }, 'found', 1],
  ['closed, facing west (matches the ladder)', { open: false, facing: 'west' }, 'partial', 0]
]
for (const [what, props, status, opens] of trapFacings) {
  test(`a trapdoor over a ladder facing west, ${what}: ${status}`, () => {
    const r = run(trapOver(props), from5, near(13, 73, 5), { goalFlood: 0 })
    assert.equal(r.status, status)
    assert.equal(r.path.steps.filter(st => st.move === MOVE.OPEN).length, opens)
  })
}

// ---- scaffolding, measured live 3/3 each: up a 6 high tower holding jump, down it sneaking, and through a block at ground level ----

const tower6 = world([[9, 64, 5, 9, 69, 5, 'scaffolding', { bottom: false, waterlogged: false, stability_distance: 0 }]])
const scaffoldCases = [
  ['climbing a 6 high tower', { x: 5, y: 64, z: 5 }, near(9, 70, 5, 0), {}, /scaffolding up/],
  // (no drops: a fall of up to 3 off the tower is free, so by default the body would step off early)
  ['descending it', { x: 9, y: 70, z: 5, px: 9.5, pz: 5.5 }, near(5, 64, 5, 0), { maxDrop: 0 }, /scaffolding down/]
]
for (const [what, from, goal, options, summary] of scaffoldCases) {
  test(`scaffolding: ${what}: found`, () => {
    const r = run(tower6, from, goal, { goalFlood: 0, ...options })
    assert.equal(r.status, 'found')
    assert.match(r.path.summary, summary)
    assert.ok(r.path.cost.climbed >= 5)
  })
}

test('scaffolding: a wall of scaffolding blocks across the way is walked through at ground level', () => {
  const wall = world([[8, 64, -2, 8, 66, 40, 'scaffolding', { bottom: false, waterlogged: false, stability_distance: 0 }]])
  const r = run(wall, from5, near(14, 64, 5, 0), { goalFlood: 0 })
  assert.equal(r.status, 'found')
  assert.ok(r.path.steps.some(st => st.x === 8 && st.y === 64))
  assert.equal(r.path.cost.climbed, 0)
})

// a vine shaft cut into a platform: weeping vines from the top (y 69) to y 65, floor under them at y 64
const vineShaft = world([
  [5, 64, 0, 14, 69, 10, 'stone'],
  [13, 64, 5, 13, 69, 5, 'air'],
  [14, 64, 5, 14, 65, 5, 'air'],
  [13, 65, 5, 13, 68, 5, 'weeping_vines_plant'],
  [13, 69, 5, 13, 69, 5, 'weeping_vines']
])
test('weeping vines down a shaft: found, down the vines and out along the floor', () => {
  const r = run(vineShaft, { x: 8, y: 70, z: 5 }, near(18, 64, 5))
  assert.equal(r.status, 'found')
  assert.ok(r.path.cost.climbed >= 4)
  assert.match(r.path.summary, /vines down/)
})

test('the same vines going up from the floor: found', () => {
  const r = run(vineShaft, { x: 18, y: 64, z: 5 }, near(8, 70, 5))
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /vines up/)
})

// ---- leaving a ladder part way up: in front of it (away from the wall) and to either side, but never through its back ----

// a ladder 12 high up the west face of a stone wall (ladder cells x 9, y 64..75, facing west: its strip is on the east side of
// the cell); one standing block on the ladder's front or side at height h is the only floor there
const SIDES = { front: [8, 5], north: [9, 4], south: [9, 6] }
const tower = ([ix, iz], h) => world([
  [10, 64, 0, 14, 80, 10, 'stone'],
  [9, 64, 5, 9, 75, 5, 'ladder', { facing: 'west' }],
  [ix, 63 + h, iz, ix, 63 + h, iz, 'stone']
])
const exitCases = Object.keys(SIDES).flatMap(side => [4, 8].flatMap(h => [['up', side, h], ['down', side, h]]))
for (const [way, side, h] of exitCases) {
  test(`a ladder shaft 12 high: ${way} the ladder, stepping off at height ${h} onto the floor on its ${side}`, () => {
    const [ix, iz] = SIDES[side]
    const island = { x: ix, y: 64 + h, z: iz }
    const query = way === 'up'
      ? [{ x: 2, y: 64, z: 5 }, { kind: 'near', ...island, range: 0 }]
      : [{ ...island, px: ix + 0.5, pz: iz + 0.5 }, near(2, 64, 5, 0)]
    // (no drops: a fall of up to 3 off the ladder is free, so by default the body would step off early instead of climbing)
    const r = plan(tower(SIDES[side], h), { from: query[0], goal: query[1] }, { goalFlood: 0, maxDrop: 0 })
    assert.equal(r.status, 'found')
    assert.ok(r.path.cost.climbed >= h - 1, `climbed ${r.path.cost.climbed}`)
    assert.match(r.path.summary, way === 'up' ? /ladder up/ : /ladder down/)
  })
}
