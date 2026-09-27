// Where a sweep of farm.maintain ends. A days=0 routine parks the body wherever the last job left it, and on 09-26 that
// was the middle of a fully planted pocket: the next leg answered "no walkable path" (a walk steps round crops), and now
// that crops are walkable at a cost it is still the slowest cell to start from. The last walk of a sweep is to the
// nearest cell of the plan's `.` lane, or to the field's edge when the plan has no lane; a body already on a lane, or
// off the plan (at the storage chest), stays where it is (card 46614365).
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import { cellOf, parkSpot } from '../src/farm/field.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const cellsOf = (plan, x = 0, y = 63, z = 0) => planCells({ plan, x, y, z })
const cellAtIn = world => (x, y, z) => cellOf(fakeApi({ world }).api.block(x, y, z))
// two rows of wheat over a lane: crops at z=0, the lane at z=1, air over everything and grass beyond the west end
const column = (x, z, ground, over = 'air') => ({ [`${x},63,${z}`]: ground, [`${x},64,${z}`]: over, [`${x},65,${z}`]: 'air' })
const LANED = { ...column(0, 0, 'farmland', 'wheat#3'), ...column(1, 0, 'farmland', 'wheat#3'), ...column(0, 1, 'dirt'), ...column(1, 1, 'dirt'), ...column(-1, 0, 'grass_block'), ...column(-1, 1, 'grass_block') }

// a two-by-two field, a birch log standing in its south-east bed with room to stand on it, grass west of its north row
const PERCH = {
  ...column(0, 0, 'farmland', 'wheat#3'), ...column(1, 0, 'farmland', 'wheat#3'), ...column(0, 1, 'farmland', 'wheat#3'),
  ...column(1, 1, 'farmland', 'birch_log'), '1,66,1': 'air', ...column(-1, 0, 'grass_block')
}
for (const [name, plan, world, from, expected] of [
  ['over a crop: the nearest lane cell', 'ww\n..', LANED, { x: 1.5, y: 64, z: 0.5 }, { x: 1, y: 64, z: 1, why: 'lane' }],
  ['already on the lane: nowhere to go', 'ww\n..', LANED, { x: 0.5, y: 64, z: 1.5 }, null],
  ['off the plan (at a chest, say): nowhere to go', 'ww\n..', LANED, { x: 9.5, y: 64, z: 9.5 }, null],
  ['no lane in the plan: the nearest cell of the field\'s edge', 'ww', LANED, { x: 0.5, y: 64, z: 0.5 }, { x: 0, y: 64, z: 1, why: 'edge' }],
  ['a lane cell with a block over it is passed over', 'ww\n..', { ...LANED, ...column(1, 1, 'dirt', 'stone') }, { x: 1.5, y: 64, z: 0.5 }, { x: 0, y: 64, z: 1, why: 'lane' }],
  // the top of a log left standing in the rows is standable and nearer than the grass beyond them: the edge is off the
  // plan, never on the clutter, where a body sat one above its field with no way down (Jizo, 09-26 23:24Z)
  ['no lane, a log in the rows: the edge is off the plan, not the log\'s top', 'ww\nww', PERCH, { x: 0.5, y: 64, z: 0.5 }, { x: -1, y: 64, z: 0, why: 'edge' }],
  ['no lane, a bare bed in the rows: the edge is off the plan, not the bed', 'ww\nww', { ...PERCH, ...column(1, 1, 'farmland') }, { x: 0.5, y: 64, z: 0.5 }, { x: -1, y: 64, z: 0, why: 'edge' }],
  ['nothing to stand on anywhere: stay', 'ww', { ...column(0, 0, 'farmland', 'wheat#3'), ...column(1, 0, 'farmland', 'wheat#3') }, { x: 0.5, y: 64, z: 0.5 }, null]
]) {
  test(`parkSpot: ${name}`, () => assert.deepEqual(parkSpot(cellAtIn(world), cellsOf(plan), from), expected))
}

// ---------------------------------------------------------------- at the end of the sweep
const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}
const HARVEST = { 'farm.harvest': { harvested: {}, replanted: 0 } }
// the fake body stands at 0,64,0 and never moves: over the plan's first cell

test('farm.maintain: the sweep\'s last walk is to the lane, and the summary says where it parked', async () => {
  const { api, calls, report } = fakeApi({ place: fakePlace('ww\n..'), world: LANED, items: { wheat_seeds: 4, stone_hoe: 1 }, answers: HARVEST })
  await maintainFarm.run(api, { place: 'test-field' })
  const gotos = calls.filter(c => c.startsWith('goto'))
  assert.deepEqual([gotos[gotos.length - 1], report.parked], ['goto x=0 y=64 z=1 range=0', '0,64,1 (lane)'])
})

test('farm.maintain: a body that ends on the lane already walks nowhere', async () => {
  const { api, calls, report } = fakeApi({ place: fakePlace('..\nww'), world: { ...column(0, 0, 'dirt'), ...column(1, 0, 'dirt'), ...column(0, 1, 'farmland', 'wheat#3'), ...column(1, 1, 'farmland', 'wheat#3') }, items: { wheat_seeds: 4, stone_hoe: 1 }, answers: HARVEST })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([calls.filter(c => c.startsWith('goto') && c.endsWith('range=0')), report.parked], [[], undefined])
})

test('farm.maintain: a parking walk that fails is a note, never stuck', async () => {
  const { api, calls, report } = fakeApi({
    place: fakePlace('ww\n..'), world: LANED, items: { wheat_seeds: 4, stone_hoe: 1 },
    answers: { ...HARVEST, goto: args => args.range === 0 ? new Error('goto: no path to the goal') : {} }
  })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([report.stuck, report.parked, calls.filter(c => c.startsWith('note') && /lane/.test(c))],
    [undefined, undefined, ['note could not end the sweep on the lane cell at 0,64,1: goto: no path to the goal']])
})

test('farm.maintain: cancellation during parking is never downgraded to a note', async () => {
  const { api } = fakeApi({ place: fakePlace('ww\n..'), world: LANED, items: { wheat_seeds: 4 },
    answers: { ...HARVEST, goto: args => args.range === 0 ? new Error('goto: cancelled') : {} } })
  await assert.rejects(maintainFarm.run(api, { place: 'test-field' }), /cancelled/)
})
