// Doors, gates and trapdoors: opened by hand (wood, copper) or by redstone (iron: a button, lever or plate on the approach side).
// No timing model: plate doors close ~1 s after the body leaves the plate (stone), buttons 1 s (stone) / 1.5 s (wood); the executor
// deals with it, the planner only says what is opened.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { courseSnapshot } from './courses.mjs'
import { fixtureSnapshot } from './fixture.mjs'
import { plan, DEFAULT_COSTS } from './planner.mjs'
import { defaultStateTable } from './blocks.mjs'

const table = defaultStateTable()
const planCourse = (name, options) => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return plan(snapshot, { from, goal }, options)
}
const run = (snapshot, from, goal, options) => plan(snapshot, { from, goal }, options)
const near = (x, y, z, range = 0) => ({ kind: 'near', x, y, z, range })
const openedBy = r => r.path.steps.flatMap(s => s.opens ?? [])

test('default costs: open 1.0 s, openRedstone 1.5 s', () => {
  assert.deepEqual([DEFAULT_COSTS.open, DEFAULT_COSTS.openRedstone], [1.0, 1.5])
})

// ---- state table ----

const stateOf = (name, props) => {
  const snapshot = fixtureSnapshot({ blocks: [[0, 64, 0, name, props]] })
  return snapshot.stateAt(0, 64, 0)
}
// [block, props, openable (1 hand, 2 redstone only), the same block open?]
const openables = [
  ['oak_door', { half: 'lower', open: false }, 1],
  ['bamboo_door', { half: 'upper', open: false }, 1],
  ['crimson_door', { half: 'lower', open: false }, 1],
  ['copper_door', { half: 'lower', open: false }, 1],
  ['oak_fence_gate', { open: false }, 1],
  ['warped_fence_gate', { open: false }, 1],
  ['oak_trapdoor', { open: false }, 1],
  ['copper_trapdoor', { open: false }, 1],
  ['iron_door', { half: 'lower', open: false }, 2],
  ['iron_trapdoor', { open: false }, 2],
  ['oak_door', { half: 'lower', open: true }, 0],
  ['oak_fence_gate', { open: true }, 0],
  ['stone', {}, 0]
]
for (const [name, props, openable] of openables) {
  test(`state table: ${name} ${JSON.stringify(props)} is openable ${openable}`, () => {
    const id = stateOf(name, props)
    assert.equal(table.openable[id], openable)
    assert.equal(table.openState[id], openable === 0 ? 0 : stateOf(name, { ...props, open: true }))
  })
}

// ---- the courses ----

// [course, summary pattern, what the opened block is]
const opening = [
  ['door-closed', /opens 1 door\b/, { x: 2880, y: 161, z: 3216 }],
  ['wood-door', /opens 1 door\b/, { x: 2880, y: 161, z: 3216 }],
  ['fence-gate', /opens 1 gate\b/, { x: 2880, y: 161, z: 3216 }]
]
for (const [name, summary, block] of opening) {
  test(`course ${name}: found, the one block opened by hand, costs.open`, () => {
    const r = planCourse(name)
    assert.equal(r.status, 'found')
    assert.match(r.path.summary, summary)
    assert.deepEqual(openedBy(r), [block])
    assert.equal(r.path.cost.opens, 1)
  })
}

test('course door-open: found with nothing opened (the old planner\'s false negative)', () => {
  const r = planCourse('door-open')
  assert.equal(r.status, 'found')
  assert.deepEqual(openedBy(r), [])
  assert.doesNotMatch(r.path.summary, /opens/)
})

test('course gate-airlock: found, two gates', () => {
  const r = planCourse('gate-airlock')
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /opens 2 gates/)
  assert.equal(openedBy(r).length, 2)
})

test('course plate-door: a wooden door opened by the plate in front of it, no hand', () => {
  const r = planCourse('plate-door')
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /steps on 1 plate/)
  assert.deepEqual(openedBy(r), [{ x: 2880, y: 161, z: 3216, via: 'plate', at: { x: 2879, y: 161, z: 3216 } }])
})

test('course iron-button: an iron door opened by the stone button beside it', () => {
  const r = planCourse('iron-button')
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /presses 1 button/)
  assert.deepEqual(openedBy(r), [{ x: 2880, y: 161, z: 3216, via: 'button', at: { x: 2879, y: 162, z: 3215 } }])
})

test('course iron-door: no activator, a wall', () => {
  assert.notEqual(planCourse('iron-door').status, 'found')
})

test('opening costs seconds: costs.open for a hand-opened door, costs.openRedstone for a button', () => {
  const hand = planCourse('door-closed').path.cost.seconds
  const dearHand = planCourse('door-closed', { costs: { open: 6 } }).path.cost.seconds
  assert.ok(Math.abs(dearHand - hand - 5) < 1e-6)
  const button = planCourse('iron-button').path.cost.seconds
  const dearButton = planCourse('iron-button', { costs: { openRedstone: 6.5 } }).path.cost.seconds
  assert.ok(Math.abs(dearButton - button - 5) < 1e-6)
})

// ---- hand-built worlds: stone to y 63 (feet cells y 64), a wall x 8 across everything ----

const world = fill => fixtureSnapshot({ fill: [[-2, 60, -2, 60, 63, 40, 'stone'], ...fill] })
const wall = (...extra) => world([[8, 64, -2, 8, 67, 40, 'stone'], ...extra])
const start = { x: 2, y: 64, z: 5 }
const goal = near(14, 64, 5)
const door = (name, z, props = {}) => [
  [8, 64, z, 8, 64, z, name, { half: 'lower', facing: 'east', ...props }],
  [8, 65, z, 8, 65, z, name, { half: 'upper', facing: 'east', ...props }]
]

// [what, the wall's fill, status, opened blocks]
const handCases = [
  ['a copper door', wall(...door('copper_door', 5)), 'found', 1],
  ['a bamboo door', wall(...door('bamboo_door', 5)), 'found', 1],
  ['double wooden doors, hinges apart', wall(...door('oak_door', 5, { hinge: 'left' }), ...door('oak_door', 6, { hinge: 'right' })), 'found', 1],
  ['an iron door and nothing to open it', wall(...door('iron_door', 5)), 'partial', 0],
  ['a closed oak trapdoor over the opening in a wall (a ceiling, opened by hand)', wall([8, 64, 5, 8, 64, 5, 'air'], [8, 65, 5, 8, 65, 5, 'oak_trapdoor', { half: 'bottom', open: false, facing: 'north' }]), 'found', 1]
]
for (const [what, snapshot, status, opens] of handCases) {
  test(`${what} in a wall: ${status}, ${opens} opened`, () => {
    const r = run(snapshot, start, goal, { goalFlood: 0 })
    assert.equal(r.status, status)
    assert.equal(openedBy(r).length, opens)
  })
}

// an iron door with a button on one side only (stone_button on the wall's face, at z 4 beside the door, head height)
const button = x => [x, 65, 4, x, 65, 4, 'stone_button', { face: 'wall', facing: x < 8 ? 'west' : 'east' }]
const ironDoor = side => wall(...door('iron_door', 5), button(side))
const sides = [
  ['west', 7, { x: 2, y: 64, z: 5 }, near(14, 64, 5), 'found', 'button'],
  ['east', 9, { x: 2, y: 64, z: 5 }, near(14, 64, 5), 'partial', undefined],
  ['east', 9, { x: 14, y: 64, z: 5 }, near(2, 64, 5), 'found', 'button'],
  ['west', 7, { x: 14, y: 64, z: 5 }, near(2, 64, 5), 'partial', undefined]
]
for (const [side, x, from, to, status, via] of sides) {
  test(`an iron door with a button on its ${side} face only, from the ${from.x < 8 ? 'west' : 'east'}: ${status}`, () => {
    const r = run(ironDoor(x), from, to, { goalFlood: 0 })
    assert.equal(r.status, status)
    assert.deepEqual(openedBy(r).map(o => o.via), via ? [via] : [])
  })
}

test('an iron door with a lever on this side: found, pulls 1 lever', () => {
  const lever = [7, 65, 4, 7, 65, 4, 'lever', { face: 'wall', facing: 'west' }]
  const r = run(wall(...door('iron_door', 5), lever), start, goal, { goalFlood: 0 })
  assert.equal(r.status, 'found')
  assert.deepEqual(openedBy(r).map(o => o.via), ['lever'])
  assert.match(r.path.summary, /pulls 1 lever/)
})

test('a button more than 4 blocks from the cell in front of the door is out of reach', () => {
  const far = [3, 65, 4, 3, 65, 4, 'stone_button', { face: 'wall', facing: 'west' }]
  const r = run(wall(...door('iron_door', 5), [2, 64, 3, 2, 66, 3, 'stone'], far), start, goal, { goalFlood: 0 })
  assert.equal(r.status === 'found' && openedBy(r).length > 0, false)
})

test('a plate in front of an iron door opens it from this side only', () => {
  const plate = x => [x, 64, 5, x, 64, 5, 'stone_pressure_plate']
  const west = run(wall(...door('iron_door', 5), plate(7)), start, goal, { goalFlood: 0 })
  const east = run(wall(...door('iron_door', 5), plate(9)), start, goal, { goalFlood: 0 })
  assert.deepEqual([west.status, east.status], ['found', 'partial'])
  assert.deepEqual(openedBy(west).map(o => o.via), ['plate'])
})

// a closed wooden floor hatch over a ladder shaft: stone to y 63 with the shaft cut down to y 56, the hatch at y 63
const hatch = facing => fixtureSnapshot({ fill: [
  [-2, 50, -2, 60, 63, 40, 'stone'],
  [8, 56, 5, 8, 62, 5, 'air'], [3, 56, 5, 7, 57, 5, 'air'],
  [8, 56, 5, 8, 62, 5, 'ladder', { facing: 'west' }],
  [8, 63, 5, 8, 63, 5, 'oak_trapdoor', { half: 'bottom', open: false, facing }]
] })
const hatchCases = [
  ['facing east, against the ladder\'s west', 'east', 'found'],
  ['facing north', 'north', 'found'],
  ['facing west, like the ladder (measured: a trapdoor facing the ladder\'s way stops the body)', 'west', 'partial']
]
for (const [what, facing, status] of hatchCases) {
  test(`a closed floor hatch ${what}, ladder below, going down: ${status}`, () => {
    const r = run(hatch(facing), { x: 2, y: 64, z: 5 }, near(4, 56, 5), { goalFlood: 0, maxDrop: 1 })
    assert.equal(r.status, status)
    assert.equal(openedBy(r).length, status === 'found' ? 1 : 0)
    assert.ok(status !== 'found' || /opens 1 trapdoor/.test(r.path.summary) && /ladder down/.test(r.path.summary))
  })
}
