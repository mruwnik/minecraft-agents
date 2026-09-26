// Out of a 1-wide shaft by hand (card 2b2d1f65). From the bottom of a flee shaft `goto dig=true` aimed at the surface dug
// or scaffolded further DOWN, twice, and `place` at the body's own feet answered "cannot get within reach" (a block cannot
// be placed into the cell the body fills, and no other cell can be walked to from a shaft). What climbed out, by hand: a
// niche dug to the SIDE at head height, a block placed under the feet from it, and so on up, alternating the two columns.
// climbPlan reads that ladder off the world; descendingLeg refuses the pathfinder's way down. Pure geometry, no body.
import test from 'node:test'
import assert from 'node:assert/strict'
import { climbPlan, descendingLeg, descentNote, inPocket, climbBlocks, ownCellRefusal } from '../src/climb.mjs'

const at = (x, y, z) => ({ x, y, z })
const FLUID = new Set(['air', 'water', 'lava', 'cave_air'])
// a world: stone at and below the ground's top, air above, with named cells on top of that; unnamed cells outside the
// loaded box are null, the way an unloaded chunk reads
const world = (cells = {}, top = 65) => (x, y, z) => {
  if (y < 40 || y > 90) return null
  const name = cells[`${x},${y},${z}`] ?? (y <= top ? 'stone' : 'air')
  return { name, solid: !FLUID.has(name) }
}
// a 1-wide shaft at 5,5 from the ground's top (65) down to y=61: five cells of air
const shaft = (extra = {}) => world({ '5,61,5': 'air', '5,62,5': 'air', '5,63,5': 'air', '5,64,5': 'air', '5,65,5': 'air', ...extra })
const FEET = at(5, 61, 5)
const dirt = cell => ({ ...cell, item: 'dirt' })

// the ladder up the shaft: the niche east (x=6), three high so the body can jump out of it, then a block under the feet
// in the other column, and so on; the surface cells above 65 are air already and cost nothing
const LADDER = [
  { to: at(6, 62, 5), dig: [at(6, 62, 5), at(6, 63, 5), at(6, 64, 5)], place: null },
  { to: at(5, 63, 5), dig: [], place: dirt(at(5, 62, 5)) },
  { to: at(6, 64, 5), dig: [at(6, 65, 5)], place: dirt(at(6, 63, 5)) },
  { to: at(5, 65, 5), dig: [], place: dirt(at(5, 64, 5)) },
  { to: at(6, 66, 5), dig: [], place: dirt(at(6, 65, 5)) }
]
const west = level => ({ ...level, to: { ...level.to, x: level.to.x === 6 ? 4 : 5 }, dig: level.dig.map(c => ({ ...c, x: c.x === 6 ? 4 : 5 })), place: level.place && { ...level.place, x: level.place.x === 6 ? 4 : 5 } })

for (const [name, given, expected] of [
  ['a 5-deep shaft with dirt in the pack: east niche, four blocks up, none short',
    { feet: FEET, goalY: 66, blockAt: shaft(), carried: { dirt: 8 } },
    { side: 'east', levels: LADDER, needed: 4, have: 8, short: null, danger: null }],
  ['water east at head height: the niche goes west instead',
    { feet: FEET, goalY: 66, blockAt: shaft({ '6,62,5': 'water' }), carried: { dirt: 8 } },
    { side: 'west', levels: LADDER.map(west), needed: 4, have: 8, short: null, danger: null }],
  ['gravel over the east column: it would pour into the niche, so west',
    { feet: FEET, goalY: 66, blockAt: shaft({ '6,64,5': 'gravel' }), carried: { dirt: 8 } },
    { side: 'west', levels: LADDER.map(west), needed: 4, have: 8, short: null, danger: null }],
  ['a fence post at the mouth of the east column (a field\'s fence): somebody\'s work, so west',
    { feet: FEET, goalY: 66, blockAt: shaft({ '6,66,5': 'birch_fence' }), carried: { dirt: 8 } },
    { side: 'west', levels: LADDER.map(west), needed: 4, have: 8, short: null, danger: null }],
  ['a cobblestone cap over the head is the body\'s own hole-up: dug, not refused',
    { feet: FEET, goalY: 66, blockAt: shaft({ '5,63,5': 'cobblestone' }), carried: { dirt: 8 } },
    { side: 'east', levels: [{ ...LADDER[0], dig: [at(5, 63, 5), ...LADDER[0].dig] }, ...LADDER.slice(1)], needed: 4, have: 8, short: null, danger: null }],
  ['nothing placeable carried: the same ladder, every place without an item, and the shortfall named',
    { feet: FEET, goalY: 66, blockAt: shaft(), carried: {} },
    { side: 'east', levels: LADDER.map(l => ({ ...l, place: l.place && { ...l.place, item: null } })), needed: 4, have: 0, short: 'short by 4 blocks: 4 to place under the feet and nothing placeable carried (dirt or cobblestone)', danger: null }],
  ['two blocks carried for four places: the first two get them, the rest wait',
    { feet: FEET, goalY: 66, blockAt: shaft(), carried: { cobblestone: 1, dirt: 1 } },
    { side: 'east', levels: [LADDER[0], { ...LADDER[1], place: { ...LADDER[1].place, item: 'cobblestone' } }, LADDER[2], { ...LADDER[3], place: { ...LADDER[3].place, item: null } }, { ...LADDER[4], place: { ...LADDER[4].place, item: null } }], needed: 4, have: 2, short: 'short by 2 blocks: 4 to place under the feet, carrying cobblestone:1 dirt:1', danger: null }],
  ['a capped shaft (the hole-up reflex closes it over the head): the cap is the first dig',
    { feet: FEET, goalY: 66, blockAt: shaft({ '5,63,5': 'dirt' }), carried: { dirt: 8 } },
    { side: 'east', levels: [{ ...LADDER[0], dig: [at(5, 63, 5), ...LADDER[0].dig] }, ...LADDER.slice(1)], needed: 4, have: 8, short: null, danger: null }],
  ['the goal at the feet or below: nothing to climb',
    { feet: FEET, goalY: 61, blockAt: shaft(), carried: { dirt: 8 } },
    { side: null, levels: [], needed: 0, have: 8, short: null, danger: null }],
  ['every side unsafe: no ladder, and the dangers named by side',
    { feet: FEET, goalY: 66, blockAt: shaft({ '6,62,5': 'water', '4,63,5': 'lava', '5,64,6': 'gravel', '5,62,4': 'sand' }), carried: { dirt: 8 } },
    { side: null, levels: [], needed: 0, have: 8, short: null, danger: 'no safe side for a niche: east has water at 6,62,5, west has lava at 4,63,5, south has gravel at 5,64,6, north has sand at 5,62,4. Dig by hand where you can see what stands there' }],
  ['fences on every side (a shaft under a pen): every side named as built',
    { feet: FEET, goalY: 66, blockAt: shaft({ '6,66,5': 'oak_fence', '4,66,5': 'oak_fence', '5,66,6': 'oak_fence_gate', '5,66,4': 'cobblestone_wall' }), carried: { dirt: 8 } },
    { side: null, levels: [], needed: 0, have: 8, short: null, danger: 'no safe side for a niche: east has oak_fence at 6,66,5 (built: not mine to dig), west has oak_fence at 4,66,5 (built: not mine to dig), south has oak_fence_gate at 5,66,6 (built: not mine to dig), north has cobblestone_wall at 5,66,4 (built: not mine to dig). Dig by hand where you can see what stands there' }],
  ['a gravel cap over the head: dug from below it falls on the head, so no plan',
    { feet: FEET, goalY: 66, blockAt: shaft({ '5,63,5': 'gravel' }), carried: { dirt: 8 } },
    { side: null, levels: [], needed: 0, have: 8, short: null, danger: 'no safe way up my own column: gravel at 5,63,5 over my head. Dig by hand where you can see what stands there' }],
  ['a column that is not loaded above: no plan either',
    { feet: at(5, 89, 5), goalY: 92, blockAt: world({ '5,89,5': 'air', '5,90,5': 'air' }, 95), carried: { dirt: 8 } },
    { side: null, levels: [], needed: 0, have: 8, short: null, danger: 'no safe way up my own column: a cell I cannot read at 5,91,5 over my head. Dig by hand where you can see what stands there' }]
]) test(`climbPlan: ${name}`, () => assert.deepEqual(climbPlan(given), expected))

// the pathfinder's answer from the bottom of a shaft, for a goal on the surface: a path that ends lower than the feet
// (it dug or scaffolded down, twice, 09-24 and 09-26). Only a goal ABOVE the body makes a way down a refusal
for (const [name, given, expected] of [
  ['the path ends two below the feet for a goal five up: refuse', { from: FEET, path: [at(5, 61, 5), at(5, 60, 5), at(5, 59, 5)], goalY: 66 }, true],
  ['the path climbs: fine', { from: FEET, path: [at(6, 62, 5), at(6, 63, 5)], goalY: 66 }, false],
  ['the path stays level: fine (a way round)', { from: FEET, path: [at(6, 61, 5), at(7, 61, 5)], goalY: 66 }, false],
  ['no path at all: not this refusal', { from: FEET, path: [], goalY: 66 }, false],
  ['the goal is below: going down is the way', { from: FEET, path: [at(5, 60, 5), at(5, 59, 5)], goalY: 55 }, false],
  ['the goal is level: going down a step is the pathfinder\'s business', { from: FEET, path: [at(6, 60, 5)], goalY: 61 }, false]
]) test(`descendingLeg: ${name}`, () => assert.equal(descendingLeg(given), expected))

test('descentNote: names where the path ends', () => {
  assert.equal(descentNote(at(5, 59, 5)), 'the path goes down (to 5,59,5) for a goal above me: climbing instead')
})

// still in the shaft or its niche: fewer than two sides open at feet and head height. On the surface (or at a cave
// junction) the pathfinder takes over. passable(dx, dy, dz) answers about a cell relative to the feet
const open = sides => (dx, dy, dz) => (dx === 0 && dz === 0) || sides.some(([sx, sz]) => sx === dx && sz === dz)
for (const [name, passable, expected] of [
  ['a 1-wide shaft: nothing open', open([]), true],
  ['the niche column beside: one side open', open([[1, 0]]), true],
  ['a niche at head height only: still boxed', (dx, dy, dz) => (dx === 0 && dz === 0) || dy === 1, true],
  ['two sides open: a junction, not a pocket', open([[1, 0], [0, 1]]), false],
  ['the surface: every side open', open([[1, 0], [-1, 0], [0, 1], [0, -1]]), false]
]) test(`inPocket: ${name}`, () => assert.equal(inPocket(passable), expected))

// what in the pack a climb may stand on: full blocks, the plainest first; never a gravity block, a bed, a gate, a door, a
// torch, a sapling, leaves or ground cover (a hole-up once capped itself with leaf_litter)
const solid = name => !/^(torch|oak_sapling|wheat_seeds|bread|leaf_litter|short_grass|iron_sword)$/.test(name)
for (const [name, counts, expected] of [
  ['dirt and cobblestone, cobblestone first', { dirt: 8, cobblestone: 3 }, { cobblestone: 3, dirt: 8 }],
  ['sand, gravel, leaves, a bed, a torch, seeds and bread: none of them', { sand: 4, gravel: 2, oak_leaves: 9, red_bed: 1, torch: 5, wheat_seeds: 3, bread: 2, leaf_litter: 4 }, {}],
  ['other full blocks after the plain ones, by name', { oak_planks: 2, stone: 1, dirt: 1, andesite: 6 }, { dirt: 1, andesite: 6, oak_planks: 2, stone: 1 }],
  ['an empty pack', {}, {}]
]) test(`climbBlocks: ${name}`, () => assert.deepEqual(Object.entries(climbBlocks(counts, solid)), Object.entries(expected)))

// `place` aimed at the cell the body fills, in a 1-wide shaft: no cell beside it to place from, and the reply used to say
// "cannot get within reach" after a 5 s search
for (const [name, given, expected] of [
  ['own feet cell, boxed in: the niche advice', { feet: FEET, target: FEET, boxed: true }, 'no cell beside me to place from: dig a side niche at head height first (climb does this), or goto x= y= z= dig=true which climbs by itself'],
  ['own head cell, boxed in: the same', { feet: FEET, target: at(5, 62, 5), boxed: true }, 'no cell beside me to place from: dig a side niche at head height first (climb does this), or goto x= y= z= dig=true which climbs by itself'],
  ['own cell in the open: the walk sorts it out', { feet: FEET, target: FEET, boxed: false }, null],
  ['the cell beside, boxed in: within reach without a step', { feet: FEET, target: at(6, 61, 5), boxed: true }, null]
]) test(`ownCellRefusal: ${name}`, () => assert.equal(ownCellRefusal(given), expected))

// the composite runs the ladder with dig, place and goto, reading the feet back after every step; the fake body moves
// wherever goto sends it
import climb from '../library/climb.mjs'
import { fakeApi } from './helpers.mjs'

const shaftWorld = (extra = {}) => {
  const cells = {}
  for (let x = 2; x <= 8; x++) for (let z = 2; z <= 8; z++) for (let y = 55; y <= 92; y++) cells[`${x},${y},${z}`] = y <= 65 ? 'stone' : 'air'
  for (let y = 61; y <= 65; y++) cells[`5,${y},5`] = 'air'
  return { ...cells, ...extra }
}
const climber = ({ world = shaftWorld(), items = { dirt: 8 }, from = at(5, 61, 5), moves = true } = {}) => {
  let feet = from
  const { api, calls } = fakeApi({ world, items, answers: { goto: a => { feet = moves ? at(a.x, a.y, a.z) : feet; return {} } } })
  return { api: { ...api, pos: () => feet }, calls }
}

test('climb: y=66 from a 5-deep shaft: the east niche, four dirt, five steps, and the feet on the surface', async () => {
  const { api, calls } = climber()
  const out = await climb.run(api, { y: 66 })
  assert.deepEqual([calls, out], [[
    'dig x=6 y=62 z=5 batch', 'dig x=6 y=63 z=5 batch', 'dig x=6 y=64 z=5 batch', 'goto x=6 y=62 z=5 range=0',
    'place item=dirt x=5 y=62 z=5', 'goto x=5 y=63 z=5 range=0',
    'dig x=6 y=65 z=5 batch', 'place item=dirt x=6 y=63 z=5', 'goto x=6 y=64 z=5 range=0',
    'place item=dirt x=5 y=64 z=5', 'goto x=5 y=65 z=5 range=0',
    'place item=dirt x=6 y=65 z=5', 'goto x=6 y=66 z=5 range=0'
  ], { climbed: 5, side: 'east', from: '5,61,5', to: '6,66,5', dug: 4, placed: 4 }])
})

test('climb: no y=: it stops as soon as the feet stand where two sides are open, the shaft mouth', async () => {
  const { api } = climber()
  const out = await climb.run(api, {})
  assert.deepEqual(out, { climbed: 5, side: 'east', from: '5,61,5', to: '6,66,5', dug: 4, placed: 4 })
})

test('climb: item=cobblestone with none carried: the first place has nothing, and the error says how short', async () => {
  const { api, calls } = climber({ items: { dirt: 8 } })
  await assert.rejects(climb.run(api, { y: 66, item: 'cobblestone' }), { message: 'climbed 1 of 5 from 5,61,5, then out of blocks at 6,62,5: short by 4 blocks: 4 to place under the feet and nothing placeable carried (dirt or cobblestone)' })
  assert.equal(calls.length, 4)
})

test('climb: a step that lands elsewhere ends the climb naming where the body stands', async () => {
  const { api, calls } = climber({ moves: false })
  await assert.rejects(climb.run(api, { y: 66 }), { message: 'climbed 0 of 5 from 5,61,5, then the step up to 6,62,5 did not happen: standing at 5,61,5' })
  assert.equal(calls.length, 4)
})

test('climb: water beside the shaft is named when every side has some', async () => {
  const { api } = climber({ world: shaftWorld({ '6,62,5': 'water', '4,62,5': 'water', '5,62,6': 'water', '5,62,4': 'water' }) })
  await assert.rejects(climb.run(api, { y: 66 }), { message: 'no safe side for a niche: east has water at 6,62,5, west has water at 4,62,5, south has water at 5,62,6, north has water at 5,62,4. Dig by hand where you can see what stands there' })
})
