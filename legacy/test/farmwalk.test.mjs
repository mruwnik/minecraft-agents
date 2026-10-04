// Walking over farmland (card fcd996fe). Farmland turns to dirt only when something LANDS on it (a jump or a fall
// of more than half a block); plain walking never tramples, so crops are no longer fenced off from the walk. A crop
// cell is passable at a cost high enough that any crop-free way wins, no move that changes level (or leaps) may land
// on a farmland floor, and a leg that crosses crops walks without sprinting. The pure half lives in src/lib/path.mjs;
// the block fakes here answer getProperties() the way a prismarine Block does.
import test from 'node:test'
import assert from 'node:assert/strict'
import { CROP_STEP, TRAMPLE_STEP, cropStepCost, trampleCost, keepMove, legFlags, farmWalk, explainNoPath, noFirstMove } from '../src/lib/path.mjs'

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
  // farmland tramples under a fall of more than half a block; a jump up of one lands with a quarter of a block to fall, and
  // a body that dropped into a one-deep hole ringed by farmland had no other way out (it sat there five minutes, card 94e6dcb1)
  ['a jump up onto farmland (out of a hole)', { x: 1, y: 65, z: 0 }, 'farmland', true],
  ['a jump up onto stone', { x: 1, y: 65, z: 0 }, 'stone', true],
  // a drop of one tramples, and is the only way down for a body perched one above its field (on a log in the rows,
  // 09-26 23:24Z): kept, at TRAMPLE_STEP. A drop of two lands harder and has a way round
  ['a drop of one onto farmland (the way down off a perch)', { x: 1, y: 63, z: 0 }, 'farmland', true],
  ['a drop of two onto farmland', { x: 1, y: 62, z: 0 }, 'farmland', false],
  ['a drop of three onto farmland', { x: 1, y: 61, z: 0 }, 'farmland', false],
  ['a parkour leap down onto farmland', { x: 3, y: 63, z: 0, parkour: true }, 'farmland', false],
  ['a drop down onto dirt', { x: 1, y: 63, z: 0 }, 'dirt', true],
  ['a parkour leap that lands level on farmland', { x: 3, y: 64, z: 0, parkour: true }, 'farmland', false],
  ['a parkour leap onto grass', { x: 3, y: 64, z: 0, parkour: true }, 'grass_block', true],
  ['a diagonal level step onto farmland', { x: 1, y: 64, z: 1 }, 'farmland', true],
  ['a diagonal jump up onto farmland', { x: 1, y: 65, z: 1 }, 'farmland', true],
  ['a parkour leap up onto farmland: a leap lands hard', { x: 3, y: 65, z: 0, parkour: true }, 'farmland', false],
  ['the floor not loaded', { x: 1, y: 65, z: 0 }, undefined, true]
]) {
  test(`keepMove: ${name}`, () => assert.equal(keepMove(FROM, move, floor), expected))
}

// what a kept move adds for the farmland it tramples: only a landing from above does, at TRAMPLE_STEP
for (const [name, move, floor, expected] of [
  ['a drop of one onto farmland', { x: 1, y: 63, z: 0 }, 'farmland', TRAMPLE_STEP],
  ['a drop of one onto dirt', { x: 1, y: 63, z: 0 }, 'dirt', 0],
  ['a level step onto farmland', { x: 1, y: 64, z: 0 }, 'farmland', 0],
  ['a jump up onto farmland', { x: 1, y: 65, z: 0 }, 'farmland', 0],
  ['the floor not loaded', { x: 1, y: 63, z: 0 }, undefined, 0]
]) {
  test(`trampleCost: ${name}`, () => assert.equal(trampleCost(FROM, move, floor), expected))
}
test('a trample costs two crop steps: a last resort, still small enough not to widen the search much', () => assert.equal(TRAMPLE_STEP, 2 * CROP_STEP))

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

test('farmWalk: a drop of two and a parkour leap onto farmland are dropped; a jump up onto it stays (the way out of a hole), and everything onto dirt', () => {
  const cells = { '1,64,0': 'farmland', '0,61,1': 'farmland', '3,63,0': 'farmland', '-1,64,0': 'dirt', '0,62,-1': 'dirt' }
  assert.deepEqual(neighbours([
    step(1, 65, 0, 2), // jump up: lands on the farmland at 1,64,0 from a quarter block up, no trample
    step(0, 62, 1, 2), // drop of two: lands on the farmland at 0,61,1
    step(3, 64, 0, 1, true), // parkour leap: lands on the farmland at 3,63,0
    step(-1, 65, 0, 2), // jump up onto dirt
    step(0, 63, -1, 1) // drop onto dirt
  ], cells), ['1,65,0:2', '-1,65,0:2', '0,63,-1:1'])
})

test('farmWalk: the jump out of a one-deep hole ringed by wheat on farmland stays, priced as a crop step (the trap the human set)', () => {
  const cells = { '1,64,0': 'farmland', '1,65,0': 'wheat', '-1,64,0': 'farmland', '-1,65,0': 'wheat' }
  assert.deepEqual(neighbours([step(1, 65, 0, 2), step(-1, 65, 0, 2)], cells), [`1,65,0:${2 + CROP_STEP}`, `-1,65,0:${2 + CROP_STEP}`])
})

test('farmWalk: perched one above a field (on a log in the rows), the drops of one into the crops stay, priced as a crop and a trample', () => {
  const cells = { '0,63,0': 'birch_log', '1,63,0': 'wheat', '1,62,0': 'farmland', '-1,63,0': 'melon_stem', '-1,62,0': 'farmland', '0,63,1': 'air', '0,62,1': 'farmland' }
  assert.deepEqual(neighbours([step(1, 63, 0, 1), step(-1, 63, 0, 1), step(0, 63, 1, 1)], cells),
    [`1,63,0:${1 + CROP_STEP + TRAMPLE_STEP}`, `-1,63,0:${1 + CROP_STEP + TRAMPLE_STEP}`, `0,63,1:${1 + TRAMPLE_STEP}`])
})

// a search that visits one node and ends `here` had no first move: how many the pathfinder made from the start, how many
// the farmland rule kept (Jizo on the log, 09-26: four drops made, none kept, nodes=0 visited=1)
for (const [name, moves, cells, expected] of [
  ['four drops of two onto farmland: made, none kept', [step(1, 62, 0, 1), step(-1, 62, 0, 1), step(0, 62, 1, 1), step(0, 62, -1, 1)],
    { '1,61,0': 'farmland', '-1,61,0': 'farmland', '0,61,1': 'farmland', '0,61,-1': 'farmland' }, { made: 4, kept: 0 }],
  ['a drop of one onto farmland and a step onto dirt: both kept', [step(1, 63, 0, 1), step(-1, 64, 0, 1)], { '1,62,0': 'farmland' }, { made: 2, kept: 2 }],
  ['no move at all', [], {}, { made: 0, kept: 0 }]
]) {
  test(`farmWalk.firstMoves: ${name}`, () => assert.deepEqual(new Moves(moves, cells).firstMoves(node), expected))
}
for (const [name, first, expected] of [
  ['moves made, none kept: the farmland rule is named', { made: 4, kept: 0 }, 'no first move from here: all 4 moves off this cell land on farmland from a leap or a drop of two or more, which a walk never takes. Dig the block underfoot or step down by hand, or goto with dig=true'],
  ['no move made at all: walled in or nothing to land on', { made: 0, kept: 0 }, 'no first move from here: no cell beside, above or below this one can be walked, jumped or dropped to (walled in, a roof too low to jump, or a drop too deep). Read the four sides (block_at), then dig the block in the way, or goto with dig=true'],
  ['a move kept: the start is not the problem', { made: 3, kept: 1 }, null]
]) {
  test(`noFirstMove: ${name}`, () => assert.equal(noFirstMove(first), expected))
}

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
import { parsePlan, checkArgs } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'

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
  assert.deepEqual(made.calls.filter(c => /^(goto|farm\.harvest) /.test(c)), ['goto x=101 y=71 z=201 range=3', 'farm.harvest place=test-field within=8'])
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

// Standing on a partial block the walk failed from, the goto steps off it sideways (a bed under a low roof, a slab, a chest).
// Farmland is a partial block too, and the body's feet floor into its cell, so the scan for free floor beside it ran at the
// farmland's own level, where the only "floor" was a one-deep hole in the field: stepped_off to that hole on four days
// running (card 94e6dcb1). Worked ground is walked from the cell above by the pathfinder and needs no step off
import { stepsOff } from '../src/lib/path.mjs'
for (const [name, expected] of [['white_bed', true], ['oak_slab', true], ['chest', true], ['farmland', false], ['dirt_path', false]]) {
  test(`stepsOff: ${name} ${expected ? 'is stepped off' : 'is not'}`, () => assert.equal(stepsOff(name), expected))
}
