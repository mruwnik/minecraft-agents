// Walking over farmland (card fcd996fe). Farmland turns to dirt only when something LANDS on it (a jump or a fall
// of more than half a block); plain walking never tramples, so crops are no longer fenced off from the walk. A crop
// cell is passable at a cost high enough that any crop-free way wins, no move that changes level (or leaps) may land
// on a farmland floor, and a leg that crosses crops walks without sprinting. The pure half lives in src/lib/path.mjs;
// the block fakes here answer getProperties() the way a prismarine Block does.
import test from 'node:test'
import assert from 'node:assert/strict'
import { CROP_STEP, cropStepCost, keepMove, legFlags, farmWalk, explainNoPath } from '../src/lib/path.mjs'

test('a crop cell costs ten steps: a lane three cells longer still wins, a whole bed crossed does not', () => assert.equal(CROP_STEP, 10))

for (const [name, expected] of [
  ['wheat', CROP_STEP], ['carrots', CROP_STEP], ['potatoes', CROP_STEP], ['beetroots', CROP_STEP],
  ['melon_stem', CROP_STEP], ['attached_pumpkin_stem', CROP_STEP], ['sweet_berry_bush', CROP_STEP],
  ['farmland', 0], ['dirt', 0], ['air', 0], ['grass', 0], ['oak_fence_gate', 0]
]) {
  test(`cropStepCost: ${name}`, () => assert.equal(cropStepCost(name), expected))
}

// which moves the planner keeps: from a node at y=64, the move's landing cell and the floor under it
const FROM = { x: 0, y: 64, z: 0 }
for (const [name, move, floor, expected] of [
  ['a level step onto farmland', { x: 1, y: 64, z: 0 }, 'farmland', true],
  ['a level step onto a crop over farmland', { x: 1, y: 64, z: 0 }, 'farmland', true],
  ['a level step onto dirt', { x: 1, y: 64, z: 0 }, 'dirt', true],
  ['a jump up onto farmland', { x: 1, y: 65, z: 0 }, 'farmland', false],
  ['a jump up onto stone', { x: 1, y: 65, z: 0 }, 'stone', true],
  ['a drop down onto farmland', { x: 1, y: 63, z: 0 }, 'farmland', false],
  ['a drop down onto dirt', { x: 1, y: 63, z: 0 }, 'dirt', true],
  ['a parkour leap that lands level on farmland', { x: 3, y: 64, z: 0, parkour: true }, 'farmland', false],
  ['a parkour leap onto grass', { x: 3, y: 64, z: 0, parkour: true }, 'grass_block', true],
  ['a diagonal level step onto farmland', { x: 1, y: 64, z: 1 }, 'farmland', true],
  ['a diagonal jump onto farmland', { x: 1, y: 65, z: 1 }, 'farmland', false],
  ['the floor not loaded', { x: 1, y: 65, z: 0 }, undefined, true]
]) {
  test(`keepMove: ${name}`, () => assert.equal(keepMove(FROM, move, floor), expected))
}

// the flags a leg walks with: no sprint when any node of it stands in a crop or on farmland
const world = cells => (x, y, z) => cells[`${x},${y},${z}`] ?? (y <= 63 ? 'dirt' : 'air')
const lane = [{ x: 1, y: 64, z: 0 }, { x: 2, y: 64, z: 0 }, { x: 3, y: 64, z: 0 }]
for (const [name, nodes, at, expected] of [
  ['a lane: sprint', lane, world({}), { allowSprinting: true }],
  ['a node in wheat: walk', lane, world({ '2,64,0': 'wheat', '2,63,0': 'farmland' }), { allowSprinting: false }],
  ['a node on bare farmland: walk (a jump would still trample it)', lane, world({ '3,63,0': 'farmland' }), { allowSprinting: false }],
  ['the last node only', lane, world({ '3,64,0': 'carrots', '3,63,0': 'farmland' }), { allowSprinting: false }],
  ['crops beside the lane, none on it: sprint', lane, world({ '2,64,1': 'wheat', '2,63,1': 'farmland' }), { allowSprinting: true }],
  ['no path at all: sprint', [], world({}), { allowSprinting: true }],
  ['a node whose cells are not loaded: sprint', lane, () => undefined, { allowSprinting: true }]
]) {
  test(`legFlags: ${name}`, () => assert.deepEqual(legFlags(nodes, at), expected))
}

// the Movements subclass: getNeighbors keeps the base class's moves, prices the ones onto crops and drops the ones
// that would land on farmland from above. The base is a fake whose getBlock answers like a prismarine Block
class FakeMovements {
  constructor (moves, cells) { this.moves = moves; this.cells = cells; this.allowSprinting = true }
  getNeighbors () { return this.moves.map(m => ({ ...m })) }
  getBlock (pos, dx, dy, dz) {
    const name = this.cells[`${pos.x + dx},${pos.y + dy},${pos.z + dz}`] ?? (pos.y + dy <= 63 ? 'dirt' : 'air')
    return { name, getProperties: () => ({}) }
  }
}
const Moves = farmWalk(FakeMovements)
const node = { x: 0, y: 64, z: 0 }
const step = (x, y, z, cost, parkour = false) => ({ x, y, z, cost, parkour })
const neighbours = (moves, cells) => new Moves(moves, cells).getNeighbors(node).map(m => `${m.x},${m.y},${m.z}:${m.cost}`)

test('farmWalk: a lane step keeps its cost, a crop step costs ten more: the crop-free way wins', () => {
  const cells = { '0,64,1': 'wheat', '0,63,1': 'farmland' }
  assert.deepEqual(neighbours([step(1, 64, 0, 1), step(0, 64, 1, 1)], cells), ['1,64,0:1', '0,64,1:11'])
})

test('farmWalk: with crops on every side the crop steps stay, priced: a walk out of a pocket exists', () => {
  const cells = { '1,64,0': 'wheat', '1,63,0': 'farmland', '0,64,1': 'carrots', '0,63,1': 'farmland', '-1,64,0': 'wheat', '-1,63,0': 'farmland', '0,64,-1': 'wheat', '0,63,-1': 'farmland' }
  assert.deepEqual(neighbours([step(1, 64, 0, 1), step(0, 64, 1, 1), step(-1, 64, 0, 1), step(0, 64, -1, 1.5)], cells),
    ['1,64,0:11', '0,64,1:11', '-1,64,0:11', '0,64,-1:11.5'])
})

test('farmWalk: a diagonal onto a crop is priced too (the base class prices only straight steps)', () => {
  const cells = { '1,64,1': 'wheat', '1,63,1': 'farmland' }
  assert.deepEqual(neighbours([step(1, 64, 1, Math.SQRT2)], cells), [`1,64,1:${Math.SQRT2 + CROP_STEP}`])
})

test('farmWalk: a jump up, a drop and a parkour leap onto farmland are dropped; the same onto dirt stay', () => {
  const cells = { '1,64,0': 'farmland', '0,62,1': 'farmland', '3,63,0': 'farmland', '-1,64,0': 'dirt', '0,62,-1': 'dirt' }
  assert.deepEqual(neighbours([
    step(1, 65, 0, 2), // jump up: lands on the farmland at 1,64,0
    step(0, 63, 1, 1), // drop: lands on the farmland at 0,62,1
    step(3, 64, 0, 1, true), // parkour leap: lands on the farmland at 3,63,0
    step(-1, 65, 0, 2), // jump up onto dirt
    step(0, 63, -1, 1) // drop onto dirt
  ], cells), ['-1,65,0:2', '0,63,-1:1'])
})

test('farmWalk: bare farmland at the same level is free, as it always was', () => {
  assert.deepEqual(neighbours([step(1, 64, 0, 1)], { '1,63,0': 'farmland' }), ['1,64,0:1'])
})

// the failure text with the pocket gone: a dig walk keeps its plain error, a shaft its note, the rest the generic line
for (const [name, error, dig, boxed, expected] of [
  ['a dig walk keeps its plain error', 'No path to the goal!', true, false, 'No path to the goal!'],
  ['boxed in: the shaft note', 'No path to the goal!', false, true, /1-wide shaft/],
  ['no path: the generic line', 'No path to the goal!', false, false, /^no walkable path \(walks don't dig or bridge\)/],
  ['the search timeout: its own line', 'Took to long to decide path to goal!', false, false, /ran out of time/],
  ['another error is kept', 'no place called x', false, false, 'no place called x']
]) {
  test(`explainNoPath: ${name}`, () => {
    const got = explainNoPath(error, dig, boxed)
    return expected instanceof RegExp ? assert.match(got, expected) : assert.equal(got, expected)
  })
}

// trample= is gone from farm.maintain: the sweep's walks read as they did before the flag, and the flag is refused
import { fakeApi } from './helpers.mjs'
import farmMaintain from '../library/farm/maintain.mjs'
import { parsePlan, planCells, checkArgs } from '../src/lib.mjs'

const block = (name, properties = {}) => ({ name, properties, solid: name !== 'air' && name !== 'water' })
const PLAN = { name: 'test-field', kind: 'farm', x: 100, y: 70, z: 200, plan: '~cc\n.cT\n#CG' }
const PLACE = { ...PLAN, parsed: parsePlan(PLAN.plan), cells: planCells(PLAN) }
const built = {
  '100,70,200': block('oak_slab', { waterlogged: 'true', type: 'bottom' }),
  '101,70,200': block('farmland'), '102,70,200': block('farmland'), '100,70,201': block('dirt'), '101,70,201': block('farmland'),
  '102,70,201': block('dirt'), '100,70,202': block('dirt'), '101,70,202': block('dirt'), '102,70,202': block('dirt'),
  '101,71,200': block('carrots', { age: 7 }), '102,71,200': block('carrots', { age: 3 }), '101,71,201': block('carrots', { age: 3 }),
  '102,71,201': block('oak_fence'), '102,72,201': block('torch'), '100,71,202': block('oak_fence'), '101,71,202': block('chest'), '102,71,202': block('oak_fence_gate')
}
test('farm.maintain: its walks carry no trample flag, and trample= is no argument of its', async () => {
  const made = fakeApi({ place: PLACE, items: { wheat_seeds: 64, carrot: 64, oak_slab: 8, water_bucket: 1 }, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, 'farm.compost': { fed: 0 } } })
  made.api.block = (x, y, z) => built[`${x},${y},${z}`] ?? null
  await farmMaintain.run(made.api, { place: 'test-field' })
  assert.deepEqual(made.calls.filter(c => /^(goto|farm\.harvest) /.test(c)), ['goto x=101 y=71 z=201 range=3', 'farm.harvest within=8'])
  assert.equal('trample' in farmMaintain.args, false)
  assert.match(checkArgs('farm.maintain', farmMaintain.args, { place: 'f', trample: true }) ?? '', /trample/)
})

// the plan check no longer calls a pocket a fault: a bed with no lane beside it is walked into at a walking pace
import farmPlan from '../library/farm/plan.mjs'
test('farm.plan: a three by three bed with no lane warns about nothing but its lanes', async () => {
  const { api, calls } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { map: '~wwww\n~wwww\n~wwww', x: 0, y: 63, z: 0, check: true })
  assert.deepEqual([calls, /walled in|trample/.test(out.warn ?? '')], [[], false])
})
