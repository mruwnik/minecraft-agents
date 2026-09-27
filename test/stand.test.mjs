// Where a farm job stands to work its cell (cards 9608fc11 and 29167296). A pour lands where the eyes really land, so
// the body has to see the TOP face of the block it pours onto: from two cells off on level ground the neighbour's own
// block is in the way, in a lane of top slabs the next slab is, and inside a planted field the crop is. farm.build on
// jizo-melon-patch (09-26) let `pour` pick any cell within 3 and twice answered "cannot see the top", where a driver
// standing one cell off poured at once. The chooser here lists the cells round a target in order of preference and
// keeps the first the pathfinder can stand in, within work range, with a clear line from eye height to the top face.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import { seesTop, standingSpots, standingSpot, workFrom, POUR_RANGE } from '../src/navigation/stand.mjs'
import { WORK_RANGE, dryStandable, loadedAround } from '../src/navigation/walk.mjs'
import { cellOf } from '../src/farm/field.mjs'

// a field the way jizo-melon-patch is built: farmland at y=62 with ripe wheat on it, a channel of waterlogged TOP slabs
// along z=4 (dirt under them at 61), a grass rim one cell round it all, air above, dirt below. An edit of null unloads a cell
const field = (edits = {}) => {
  const world = {}
  for (let x = -1; x <= 8; x++) {
    for (let z = -1; z <= 9; z++) {
      world[`${x},61,${z}`] = 'dirt'
      world[`${x},63,${z}`] = 'air'
      world[`${x},64,${z}`] = 'air'
      world[`${x},65,${z}`] = 'air'
      const rim = x === -1 || x === 8 || z === -1 || z === 9
      if (rim) { world[`${x},62,${z}`] = 'grass_block'; continue }
      if (z === 4) { world[`${x},62,${z}`] = 'oak_slab~#top'; continue }
      world[`${x},62,${z}`] = 'farmland'
      world[`${x},63,${z}`] = 'wheat#7'
    }
  }
  for (const [k, v] of Object.entries(edits)) {
    if (v === null) delete world[k]
    else world[k] = v
  }
  return world
}
// plain ground: grass at 62 everywhere, the way a channel is dug through a meadow
const ground = (edits = {}) => {
  const world = field()
  for (const k of Object.keys(world)) if (k.split(',')[1] === '62') world[k] = 'grass_block'
  for (const k of Object.keys(world)) if (k.split(',')[1] === '63') world[k] = 'air'
  return { ...world, ...edits }
}
const blockAtOf = world => fakeApi({ world }).api.block
const cellAtOf = world => (x, y, z) => cellOf(blockAtOf(world)(x, y, z))
const at = (x, y, z) => ({ x, y, z })

// ---------------------------------------------------------------- the line from the eyes to the top face
// the channel cell at 3,62,4 is dug out (its slab gone): the pour target is the dirt under it, 3,61,4
const HOLE = { '3,62,4': 'air' }
for (const [name, world, from, target, expected] of [
  ['from the slab beside a dug channel cell', field(HOLE), at(2, 63, 4), at(3, 61, 4), true],
  ['from two cells off along the lane: the slab between is in the way', field(HOLE), at(1, 63, 4), at(3, 61, 4), false],
  ['from two cells off when the cell between is dug out too', field({ ...HOLE, '2,62,4': 'air' }), at(1, 63, 4), at(3, 61, 4), true],
  ['from two cells off on level ground: the block between is in the way (the 09-24 case)', ground(HOLE), at(1, 63, 4), at(3, 61, 4), false],
  ['from the cell beside it on level ground', ground(HOLE), at(2, 63, 4), at(3, 61, 4), true],
  ['from the cell diagonally beside it', ground(HOLE), at(2, 63, 5), at(3, 61, 4), true],
  ['from the lane two rows off a bed: the ripe wheat between is in the way', field({ '3,63,2': 'air' }), at(3, 63, 4), at(3, 62, 2), false],
  ['from the lane two rows off a bed: young wheat (a quarter high) is seen over', field({ '3,63,2': 'air', '3,63,3': 'wheat#1' }), at(3, 63, 4), at(3, 62, 2), true],
  ['from straight above, standing in the hole', field(HOLE), at(3, 62, 4), at(3, 61, 4), true],
  ['a cell that is not loaded between: nobody can see through the unknown', field({ ...HOLE, '2,62,4': null, '2,63,4': null }), at(1, 63, 4), at(3, 61, 4), false],
  ['five cells off is beyond the arm, clear line or not', ground({ ...HOLE, '0,62,4': 'air', '1,62,4': 'air', '2,62,4': 'air' }), at(-2, 63, 4), at(3, 61, 4), false],
  ['a fence between reaches a block and a half', field({ ...HOLE, '2,62,4': 'oak_fence', '2,63,4': 'air' }), at(1, 63, 4), at(3, 61, 4), false]
]) {
  test(`seesTop: ${name}`, () => assert.equal(seesTop(blockAtOf(world), from, target), expected))
}

// ---------------------------------------------------------------- where the pathfinder will stand, dry
for (const [name, world, cell, expected] of [
  ['on the slab lane', field(), at(2, 63, 4), true],
  ['on the grass rim', field(), at(-1, 63, 4), true],
  ['on a harvested bed', field({ '3,63,3': 'air' }), at(3, 63, 3), true],
  ['in a crop: never', field(), at(2, 63, 2), false],
  ['in water: the pathfinder swims there, a job does not stand there', field({ '-1,62,4': 'water', '-1,63,4': 'water' }), at(-1, 63, 4), false],
  ['a cell that is not loaded', field(), at(50, 63, 50), false]
]) {
  test(`dryStandable: ${name}`, () => assert.equal(dryStandable(cellAtOf(world), cell), expected))
}

test('loadedAround: every cell within range is loaded, or not', () => {
  assert.equal(loadedAround(cellAtOf(field()), at(3, 63, 4), 2), true)
  assert.equal(loadedAround(cellAtOf(field({ '3,64,5': null })), at(3, 63, 4), 2), false)
  assert.equal(loadedAround(cellAtOf(field()), at(3, 63, 8), 2), false)
})

// ---------------------------------------------------------------- the spot itself
const NONE = (n, x, y, z) => `no cell to stand within ${n} of ${x},${y},${z} that sees its top`
for (const [name, world, target, range, expected] of [
  ['a channel floor deep inside: the slab beside it', field(HOLE), at(3, 61, 4), POUR_RANGE, { at: at(2, 63, 4) }],
  ['a channel floor at the edge: the rim beside it, not the hole next door', field({ '0,62,4': 'air', '1,62,4': 'air' }), at(0, 61, 4), POUR_RANGE, { at: at(-1, 63, 4) }],
  ['a bed to plant with every neighbour a crop: the nearest harvested cell', field({ '3,63,2': 'air', '3,63,3': 'air' }), at(3, 62, 2), WORK_RANGE, { at: at(3, 63, 3) }],
  ['a bed in the first row: the rim', field({ '3,63,0': 'air' }), at(3, 62, 0), WORK_RANGE, { at: at(3, 63, -1) }],
  ['a bed walled in by ripe wheat: no cell at all', field({ '3,63,2': 'air' }), at(3, 62, 2), WORK_RANGE, { why: NONE(WORK_RANGE, 3, 62, 2) }],
  ['the same bed once the wheat round it is cut: the lane sees over the stubble', field({ '3,63,2': 'air', '3,63,3': 'wheat#0' }), at(3, 62, 2), WORK_RANGE, { at: at(3, 63, 4) }],
  ['water beside the target is no place to stand', field({ '0,62,4': 'air', '-1,62,4': 'water', '-1,63,4': 'water' }), at(0, 61, 4), POUR_RANGE, { at: at(1, 63, 4) }],
  ['a hole walled in: the hole itself is never the spot', field({ ...HOLE, '2,62,4': 'oak_log', '2,63,4': 'oak_log', '4,62,4': 'oak_log', '4,63,4': 'oak_log' }), at(3, 61, 4), POUR_RANGE, { why: NONE(POUR_RANGE, 3, 61, 4) }],
  ['a lane cell and a harvested bed both beside the target: the lane', field({ ...HOLE, '3,63,3': 'air' }), at(3, 61, 4), POUR_RANGE, { at: at(2, 63, 4) }],
  ['nothing loaded round the target', {}, at(3, 61, 4), POUR_RANGE, { why: NONE(POUR_RANGE, 3, 61, 4) }]
]) {
  test(`standingSpot: ${name}`, () => assert.deepEqual(standingSpot({ target, blockAt: blockAtOf(world), range }), expected))
}

test('standingSpot: see=false is the nearest cell to stand in, sight or no sight (a walk to the middle of a field)', () => {
  assert.deepEqual(standingSpot({ target: at(3, 63, 2), blockAt: blockAtOf(field()), range: 8, see: false }), { at: at(3, 63, 4) })
})

test('standingSpots: every spot that will do, best first, so a pour that still cannot see has a next one to try', () => {
  const spots = standingSpots({ target: at(3, 61, 4), blockAt: blockAtOf(field(HOLE)), range: POUR_RANGE })
  assert.deepEqual(spots.slice(0, 2), [at(2, 63, 4), at(4, 63, 4)])
})

// ---------------------------------------------------------------- a job done from its spot
const BLIND = 'cannot see the top of 3,61,4 from here (looking at oak_slab at 2,62,4): stand 1-2 blocks away with a clear view down onto it, then retry. Nothing was poured'
const POUR = { do: 'pour', x: 3, y: 61, z: 4, item: 'water_bucket', why: 'the channel at 3,62,4 is dry' }
const jobApi = (world, answers = {}) => fakeApi({ world, answers })

test('workFrom: a pour walks to its spot first, then pours', async () => {
  const { api, calls } = jobApi(field(HOLE))
  await workFrom(api, POUR)
  assert.deepEqual(calls, ['goto x=2 y=63 z=4 range=0', 'pour 3,61,4'])
})

test('workFrom: a pour that still cannot see the top is tried once more from the next spot', async () => {
  let asked = 0
  const { api, calls } = jobApi(field(HOLE), { pour: () => { asked++; if (asked === 1) throw new Error(BLIND); return {} } })
  await workFrom(api, POUR)
  assert.deepEqual(calls, ['goto x=2 y=63 z=4 range=0', 'pour 3,61,4', 'goto x=4 y=63 z=4 range=0', 'pour 3,61,4'])
})

test('workFrom: blind from both spots: the second answer is reported, no third try', async () => {
  const { api, calls } = jobApi(field(HOLE), { pour: () => { throw new Error(BLIND) } })
  await assert.rejects(workFrom(api, POUR), { message: BLIND })
  assert.equal(calls.filter(c => c.startsWith('pour')).length, 2)
})

test('workFrom: no spot in sight of a loaded target: the primitive walks by itself, and a blind answer carries the reason', async () => {
  const world = field({ ...HOLE, '2,62,4': 'oak_log', '2,63,4': 'oak_log', '4,62,4': 'oak_log', '4,63,4': 'oak_log' })
  const { api, calls } = jobApi(world, { pour: () => { throw new Error(BLIND) } })
  await assert.rejects(workFrom(api, POUR), { message: `${NONE(POUR_RANGE, 3, 61, 4)}: ${BLIND}` })
  assert.deepEqual(calls, ['pour 3,61,4'])
})

test('workFrom: any other failure is the primitive\'s own, once', async () => {
  const { api, calls } = jobApi(field(HOLE), { pour: () => { throw new Error('no full bucket: fill x= y= z= at a water source first') } })
  await assert.rejects(workFrom(api, POUR), /no full bucket/)
  assert.deepEqual(calls, ['goto x=2 y=63 z=4 range=0', 'pour 3,61,4'])
})

test('workFrom: a cover stands where it sees the channel floor under the water', async () => {
  const { api, calls } = jobApi(field({ '3,62,4': 'water' }))
  await workFrom(api, { do: 'cover', x: 3, y: 62, z: 4, item: 'oak_slab', why: 'the channel at 3,62,4 is open water' })
  assert.deepEqual(calls, ['goto x=2 y=63 z=4 range=0', 'place item=oak_slab x=3 y=62 z=4 half=top'])
})

test('workFrom: a plant stands where it sees the top of the bed', async () => {
  const { api, calls } = jobApi(field({ '3,63,2': 'air', '3,63,3': 'air' }))
  await workFrom(api, { do: 'plant', x: 3, y: 63, z: 2, item: 'wheat_seeds', why: 'an empty bed' })
  assert.deepEqual(calls, ['goto x=3 y=63 z=3 range=0', 'place item=wheat_seeds x=3 y=63 z=2'])
})

test('workFrom: a slab that did not take is tried once more from the next spot', async () => {
  let asked = 0
  const { api, calls } = jobApi(field({ '3,62,4': 'water' }), { place: () => { asked++; if (asked === 1) throw new Error('placing oak_slab at (3, 62, 4) did not take'); return {} } })
  await workFrom(api, { do: 'cover', x: 3, y: 62, z: 4, item: 'oak_slab', why: 'open water' })
  assert.deepEqual(calls.filter(c => c.startsWith('goto')), ['goto x=2 y=63 z=4 range=0', 'goto x=4 y=63 z=4 range=0'])
})

test('workFrom: a till or a dig is called as before, from wherever the body is', async () => {
  const { api, calls } = jobApi(field())
  await workFrom(api, { do: 'till', x: 3, y: 62, z: 2, why: 'dirt where farmland should be' })
  await workFrom(api, { do: 'clear', x: 3, y: 63, z: 2, why: 'short_grass grew on the bed' })
  assert.deepEqual(calls, ['till 3,62,2', 'dig 3,63,2'])
})

// ---------------------------------------------------------------- the composites use it
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import buildFarm from '../library/farm/build.mjs'

// the plan that `field` is the built form of: eight beds of wheat either side of a channel, anchored at 0,62,0
const PLAN = ['wwwwwwww', 'wwwwwwww', 'wwwwwwww', 'wwwwwwww', '~~~~~~~~', 'wwwwwwww', 'wwwwwwww', 'wwwwwwww', 'wwwwwwww'].join('\n')
const place = () => ({ name: 'test-field', kind: 'farm', x: 0, y: 62, z: 0, plan: PLAN, parsed: parsePlan(PLAN), cells: planCells({ plan: PLAN, x: 0, y: 62, z: 0 }), bill: planBill(parsePlan(PLAN)) })
const farmApi = (world, items = {}) => {
  const made = fakeApi({ world, place: place(), items, answers: { 'farm.harvest': { harvested: {}, replanted: 0 } } })
  made.api.pos = () => ({ x: -5, y: 63, z: 4 })
  return made
}

// farm.maintain used to walk to within 2 of the plan's middle: in a finished field that is a bed walled in by crops on
// every side, the pathfinder has no node to end in, and the walk ran out of time or found no path (Jizo, 09-26) where
// farm.harvest, right after it, walks to the field's edge and works. The sweep now starts at the edge too
test('farm.maintain: the sweep starts at the nearest cell of the field\'s edge, never its middle', async () => {
  const { api, calls } = farmApi(field(), { wheat_seeds: 8 })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls.slice(0, 3), ['goto x=-1 y=63 z=4 range=1', 'kit tools=stone_hoe food=12 place=test-field', 'farm.harvest place=test-field within=15'])
})

test('farm.maintain: with nothing loaded round the plan, a walk to its middle at a range that reaches the rim, then the edge', async () => {
  const { api, calls } = farmApi({}, { wheat_seeds: 8 })
  await maintainFarm.run(api, { place: 'test-field' }).catch(() => {})
  assert.deepEqual(calls.slice(0, 1), ['goto x=3 y=63 z=4 range=7'])
})

test('farm.maintain: a pour is made from the slab beside the dug cell', async () => {
  const { api, calls } = farmApi(field({ '3,62,4': 'air' }), { wheat_seeds: 8, water_bucket: 1, oak_slab: 1 })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls.filter(c => /^goto x=2|^pour/.test(c)), ['goto x=2 y=63 z=4 range=0', 'pour 3,61,4'])
})

test('farm.build: a pour is made from the slab beside the dug cell', async () => {
  const { api, calls } = farmApi(field({ '3,62,4': 'air' }), { wheat_seeds: 8, water_bucket: 1, oak_slab: 1 })
  await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => /^goto x=2|^pour/.test(c)), ['goto x=2 y=63 z=4 range=0', 'pour 3,61,4'])
})
