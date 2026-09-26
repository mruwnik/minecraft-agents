// A crop pocket (card 68f4e331, jizo-melon-patch 09-26): a sweep left the body at 5,63,-86 with live wheat on all four
// sides. A walk steps round crops, so from there nothing can be walked to: farm.maintain, goto and path_to all died at
// once with the generic "no walkable path" line (noPath, visited=12), which sent the farmer looking for a way round that
// harvesting ANY one neighbour would have opened. boxedIn says nothing about it: a crop is no block. The failure now
// names the crop to harvest, on the way to the goal, and the last resort (trample=true).
import test from 'node:test'
import assert from 'node:assert/strict'
import { cropPocket, explainNoPath, mayTrample, mayDig } from '../src/lib/path.mjs'

// cells as src/walk.mjs reads them: { name, solid, crop }, null when the chunk is not loaded
const AIR = { name: 'air', solid: false }
const DIRT = { name: 'dirt', solid: true }
const STONE = { name: 'stone', solid: true }
const WHEAT = { name: 'wheat', solid: false, crop: true }
const CARROTS = { name: 'carrots', solid: false, crop: true }
const FENCE = { name: 'oak_fence', solid: true }
// dirt at and below y=62, air above; the body's feet at y=63
const world = cells => (x, y, z) => `${x},${y},${z}` in cells ? cells[`${x},${y},${z}`] : y <= 62 ? DIRT : AIR
const FEET = { x: 5, y: 63, z: -86 }
// the four sides in STEPS order: east, west, south, north
const beside = (east, west, south, north) => ({ '6,63,-86': east, '4,63,-86': west, '5,63,-85': south, '5,63,-87': north })
const GOAL = { x: -17, y: 62, z: -97 }
// the goal lies west-north-west: the west cell is nearest it, then the north one
const WAY_OUT = 'a walk steps round crops, so nothing can be walked to from here. Harvest one with `dig x=4 y=63 z=-86` and step into its cell, or goto again with trample=true'
const NORTH_OUT = 'a walk steps round crops, so nothing can be walked to from here. Harvest one with `dig x=5 y=63 z=-87` and step into its cell, or goto again with trample=true'

for (const [name, cells, towards, expected] of [
  ['open ground: nothing to say', world({}), GOAL, null],
  ['wheat on every side: the crop nearest the goal is the one to harvest', world(beside(WHEAT, WHEAT, WHEAT, WHEAT)), GOAL,
    `the only cells beside you are crops (wheat:4): ${WAY_OUT}`],
  ['no goal known: the first side', world(beside(WHEAT, WHEAT, WHEAT, WHEAT)), null,
    'the only cells beside you are crops (wheat:4): a walk steps round crops, so nothing can be walked to from here. Harvest one with `dig x=6 y=63 z=-86` and step into its cell, or goto again with trample=true'],
  ['a goal that follows an entity has no coordinates: the first side', world(beside(WHEAT, WHEAT, WHEAT, WHEAT)), { entity: {} },
    'the only cells beside you are crops (wheat:4): a walk steps round crops, so nothing can be walked to from here. Harvest one with `dig x=6 y=63 z=-86` and step into its cell, or goto again with trample=true'],
  ['two crops: both counted, the nearer one named', world(beside(CARROTS, WHEAT, CARROTS, WHEAT)), GOAL,
    `the only cells beside you are crops (carrots:2 wheat:2): ${WAY_OUT}`],
  ['one side open at foot and head: a doorway, not a pocket', world(beside(WHEAT, AIR, WHEAT, WHEAT)), GOAL, null],
  ['one side a step up onto dirt with headroom: a way out', world({ ...beside(WHEAT, DIRT, WHEAT, WHEAT) }), GOAL, null],
  ['one side a fence: walled in by crops and the fence, and the fence is no ledge', world(beside(WHEAT, FENCE, WHEAT, WHEAT)), GOAL,
    `walled in by crops (wheat:3 oak_fence:1): ${NORTH_OUT}`],
  ['one side a wall of stone up to the head: the crops are still the way out', world({ ...beside(WHEAT, STONE, WHEAT, WHEAT), '4,64,-86': STONE }), GOAL,
    `walled in by crops (wheat:3 stone:1): ${NORTH_OUT}`],
  ['one side not loaded: it counts as shut', world({ ...beside(WHEAT, WHEAT, WHEAT, WHEAT), '4,63,-86': null }), GOAL,
    `walled in by crops (wheat:3 unloaded:1): ${NORTH_OUT}`],
  ['a crop with a block over it cannot be stepped into once cut: the next nearest is named', world({ ...beside(WHEAT, WHEAT, WHEAT, WHEAT), '4,64,-86': STONE }), GOAL,
    `the only cells beside you are crops (wheat:4): ${NORTH_OUT}`],
  ['stone on every side: a shaft, which is boxedIn\'s to explain', world(beside(STONE, STONE, STONE, STONE)), GOAL, null],
  ['the body\'s own cell not loaded: nothing to say', () => null, GOAL, null]
]) {
  test(`cropPocket: ${name}`, () => assert.equal(cropPocket(cells, FEET, towards), expected))
}

// the failure text: a pocket is said in place of the generic line, never for a dig walk (it digs the crop and goes),
// and never over the shaft note (solid walls are the shaft's story, and a shaft has no crop beside it)
const POCKET = 'the only cells beside you are crops (wheat:4): a walk steps round crops'
for (const [name, error, dig, boxed, pocket, expected] of [
  ['no path from a pocket: the pocket text', 'No path to the goal!', false, false, POCKET, POCKET],
  ['the search timeout from a pocket: the pocket text too', 'Took to long to decide path to goal!', false, false, POCKET, POCKET],
  ['a walk that ended short from a pocket', 'no path to the goal: the search found nothing to walk from here', false, false, POCKET, POCKET],
  ['a dig walk keeps its plain error', 'No path to the goal!', true, false, POCKET, 'No path to the goal!'],
  ['boxed in and a pocket at once: the shaft note', 'No path to the goal!', false, true, POCKET, explainNoPath('No path to the goal!', false, true)],
  ['no pocket: the generic line as before', 'No path to the goal!', false, false, null, explainNoPath('No path to the goal!', false)],
  ['another error is kept', 'no place called x', false, false, POCKET, 'no place called x']
]) {
  test(`explainNoPath: ${name}`, () => assert.equal(explainNoPath(error, dig, boxed, pocket), expected))
}

// trample=true is the explicit last resort: only a real true, on any walk, the way dig=true is read
for (const [name, action, args, expected] of [
  ['a plain walk steps round crops', 'goto', { x: 1, z: 2 }, false],
  ['trample=true lets it step on them', 'goto', { x: 1, z: 2, trample: true }, true],
  ['only a real true counts', 'goto', { trample: 'yes' }, false],
  ['a sweep asked to trample', 'farm.maintain', { place: 'field', trample: true }, true],
  ['dig=true is not trample', 'goto', { dig: true }, false]
]) {
  test(`mayTrample: ${name}`, () => assert.equal(mayTrample(action, args), expected))
}
test('mayDig and mayTrample read different keys', () => assert.deepEqual([mayDig('goto', { trample: true }), mayTrample('goto', { dig: true })], [false, false]))

// The plan check (item 3 of the card): a crop cell with no walkable cell on any side is a pocket, and a job that ends on
// one is walled in. Said when the plan is checked or saved, before it is built, with the fix. A warning, not a refusal:
// the shape is legal, and a driver who works from the lanes never enters a pocket
import { planPockets, parsePlan } from '../src/lib.mjs'
import { fakeApi } from './helpers.mjs'
import farmPlan from '../library/farm/plan.mjs'

const pocketsOf = (...rows) => planPockets(parsePlan(rows.join('\n')).cells)
const band = (rows, width) => Array.from({ length: rows }, () => 'w'.repeat(width))
const jizo = ['C' + 'w'.repeat(15), ...band(3, 16), '~'.repeat(16), ...band(8, 16), '~'.repeat(16), ...band(4, 16)]
const FIX = 'a job that ends on one is walled in by crops (harvest a neighbour, or trample=true); a . lane every third row leaves every bed a step from one'
for (const [name, got, expected] of [
  ['the middle of a three by three bed', pocketsOf('www', 'www', 'www'), { pockets: `1 crop cell has no walkable cell beside it (1,1): ${FIX}` }],
  ['the melon patch: 140 cells between its channels, the count its census gave', pocketsOf(...jizo),
    { pockets: `140 crop cells have no walkable cell beside them (1,1 2,1 3,1 4,1 and 136 more): ${FIX}` }],
  ['a lane every third row: every bed is a step from one', pocketsOf('wwww', '....', 'wwww', 'wwww', '~~~~', 'wwww'), {}],
  ['two rows between lanes are fine, three are not', pocketsOf('wwww', 'wwww', 'wwww', '....'), { pockets: `2 crop cells have no walkable cell beside them (1,1 2,1): ${FIX}` }],
  ['a crop fenced in on every side', pocketsOf('###', '#w#', '###'), { pockets: `1 crop cell has no walkable cell beside it (1,1): ${FIX}` }],
  ['a gate beside it is a way out', pocketsOf('#G#', '#w#', '###'), {}],
  ['the ground round the plan is walkable', pocketsOf('ww', 'ww'), {}],
  ['a plan with no crops has no pockets', pocketsOf('###', '#.#', '#G#'), {}],
  ['an empty plan is not a complaint', planPockets([]), {}]
]) {
  test(`planPockets: ${name}`, () => assert.deepEqual(got, expected))
}

test('farm.plan: a checked map names its pockets beside its other warnings', async () => {
  const { api, calls } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { map: '~wwww\n~wwww\n~wwww', x: 0, y: 63, z: 0, check: true })
  assert.deepEqual([calls, out.warn], [[], `2 crop cells have no walkable cell beside them (2,1 3,1): ${FIX}`])
})

test('farm.plan: a map with a lane beside every bed warns about nothing', async () => {
  const { api } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { map: '~ww\n~ww\n...', x: 0, y: 63, z: 0, check: true })
  assert.equal(out.warn, undefined)
})
