// farm.maintain waters its own channels (card 72e49b3d). The dry cells of one carrot patch were reported as
// "dry and I carry no water" sweep after sweep while an empty bucket sat in the field chest: a sweep now fetches its
// own water (the nearest still source within range, the carried bucket filled there) before it lists the jobs, and
// again whenever a pour has emptied the bucket. A channel stays dry for exactly two reasons, and the skipped= line
// says which: no bucket at all (and how to make one), or no water within range.
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells, planBill } from '../src/lib.mjs'
import { NO_BUCKET, noWaterLine, waterShortfall } from '../src/builder.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const NO_WATER = 'no water within 32 blocks'
test('the two reasons, word for word', () => {
  assert.equal(NO_BUCKET, 'no bucket: craft item=bucket (3 iron_ingot)')
  assert.equal(noWaterLine(32), NO_WATER)
  assert.equal(noWaterLine(10), 'no water within 10 blocks')
})

// ---------------------------------------------------------------- waterShortfall
// the pockets change as a bucket is filled or poured: the fake's items are the pockets, and the fake's answers move the water
const pockets = items => ({
  items,
  fill: () => { items.bucket--; items.water_bucket = (items.water_bucket ?? 0) + 1; return {} },
  pour: () => { items.water_bucket--; items.bucket = (items.bucket ?? 0) + 1; return {} }
})
const found = positions => ({ find_blocks: () => ({ positions }) })
const FIND = 'find_blocks block=water maxDistance=32 count=32'

for (const [name, given, expected] of [
  ['a full bucket carried: nothing to do', { items: { water_bucket: 1 }, world: {} }, { why: null, calls: [] }],
  ['no bucket at all: how to make one, and no search', { items: { iron_ingot: 3 }, world: {} }, { why: NO_BUCKET, calls: [] }],
  ['a bucket and no water anywhere near', { items: { bucket: 1 }, world: {}, positions: [] }, { why: NO_WATER, calls: [FIND] }],
  ['the nearest source is filled from, not the first the search lists',
    { items: { bucket: 1 }, world: { '9,63,0': 'water', '3,63,0': 'water' }, positions: [{ x: 9, y: 63, z: 0 }, { x: 3, y: 63, z: 0 }] },
    { why: null, calls: [FIND, 'fill 3,63,0'] }],
  ['flowing water is passed over for a still source',
    { items: { bucket: 1 }, world: { '1,63,0': 'water#3', '4,63,0': 'water' }, positions: [{ x: 1, y: 63, z: 0 }, { x: 4, y: 63, z: 0 }] },
    { why: null, calls: [FIND, 'fill 4,63,0'] }],
  ['only flowing water in range is no water',
    { items: { bucket: 1 }, world: { '1,63,0': 'water#3' }, positions: [{ x: 1, y: 63, z: 0 }] },
    { why: NO_WATER, calls: [FIND] }],
  ['a fill that fails moves on to the next source',
    { items: { bucket: 1 }, world: { '2,63,0': 'water', '6,63,0': 'water' }, positions: [{ x: 2, y: 63, z: 0 }, { x: 6, y: 63, z: 0 }], fillFails: 1 },
    { why: null, calls: [FIND, 'fill 2,63,0', 'fill 6,63,0'] }],
  ['every fill failing is no water',
    { items: { bucket: 1 }, world: { '2,63,0': 'water' }, positions: [{ x: 2, y: 63, z: 0 }], fillFails: 9 },
    { why: NO_WATER, calls: [FIND, 'fill 2,63,0'] }]
]) {
  test(`waterShortfall: ${name}`, async () => {
    const hands = pockets(given.items)
    let fails = given.fillFails ?? 0
    const fill = () => { if (fails-- > 0) throw new Error('fill: cannot see the source'); return hands.fill() }
    const { api, calls } = fakeApi({ world: given.world, items: given.items, answers: { ...found(given.positions ?? []), fill } })
    assert.equal(await waterShortfall(api), expected.why)
    assert.deepEqual(calls, expected.calls)
  })
}

test('waterShortfall: the range is in the reason and in the search', async () => {
  const { api, calls } = fakeApi({ items: { bucket: 1 }, answers: found([]) })
  assert.equal(await waterShortfall(api, 10), 'no water within 10 blocks')
  assert.deepEqual(calls, ['find_blocks block=water maxDistance=10 count=32'])
})

// ---------------------------------------------------------------- the sweep
// a plan at y=63: the body walks at 64 on dirt, a bed of growing wheat at 0,63,0 anchors the plan, and the channel
// cells after it are dry dirt with dirt under them to pour onto. The pond, when there is one, lies east at x=8
const fakePlace = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }), bill: planBill(parsePlan(plan)) })
const field = (plan, edits = {}) => {
  const world = {}
  for (let x = -3; x <= 10; x++) {
    for (let z = -3; z <= 3; z++) {
      world[`${x},62,${z}`] = 'dirt'
      world[`${x},63,${z}`] = 'dirt'
      world[`${x},64,${z}`] = 'air'
      world[`${x},65,${z}`] = 'air'
    }
  }
  return { ...world, '0,63,0': 'farmland', '0,64,0': 'wheat#3', ...edits }
}
const POND = { '8,63,0': 'water', '8,63,1': 'water' }
const pondFound = found([{ x: 8, y: 63, z: 1 }, { x: 8, y: 63, z: 0 }])
const sweep = async ({ plan, world, items, answers = {} }) => {
  const hands = pockets(items)
  const made = fakeApi({ place: fakePlace(plan), world, items, answers: { 'farm.harvest': { harvested: {}, replanted: 0 }, fill: hands.fill, pour: hands.pour, ...answers } })
  const summary = await maintainFarm.run(made.api, { place: 'test-field' })
  return { summary, water: made.calls.filter(c => /^(find_blocks|fill|dig|pour) /.test(c)) }
}

for (const [name, given, expected] of [
  ['no channel in the plan: no search for water',
    { plan: 'w', world: field('w'), items: {} },
    { water: [], poured: 0, skipped: undefined }],
  ['a dry channel and no bucket: the cell is skipped with the bucket recipe, and no search',
    { plan: 'w~', world: field('w~'), items: {} },
    { water: [], poured: 0, skipped: `1,63,0 (${NO_BUCKET})` }],
  ['a dry channel, a bucket and no water: the cell is skipped with the range',
    { plan: 'w~', world: field('w~'), items: { bucket: 1 }, answers: found([]) },
    { water: [FIND], poured: 0, skipped: `1,63,0 (${NO_WATER})` }],
  ['a dry channel, a bucket and a pond: the bucket is filled at the pond, the cell dug open and poured',
    { plan: 'w~', world: field('w~', POND), items: { bucket: 1 }, answers: pondFound },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'pour 1,62,0'], poured: 1, skipped: undefined }],
  ['two dry channels and one bucket (the digs come first, in job order): it is filled again after the first pour empties it',
    { plan: 'w~~', world: field('w~~', POND), items: { bucket: 1 }, answers: pondFound },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'dig 2,63,0', 'pour 1,62,0', FIND, 'fill 8,63,0', 'pour 2,62,0'], poured: 2, skipped: undefined }],
  ['the pond gone after the first pour: the second cell is skipped with the range, the first was poured',
    { plan: 'w~~', world: field('w~~', POND), items: { bucket: 1 }, answers: { find_blocks: (() => { let asked = 0; return () => ({ positions: asked++ === 0 ? [{ x: 8, y: 63, z: 0 }] : [] }) })() } },
    { water: [FIND, 'fill 8,63,0', 'dig 1,63,0', 'dig 2,63,0', 'pour 1,62,0', FIND], poured: 1, skipped: `2,63,0 (${NO_WATER})` }]
]) {
  test(`farm.maintain: ${name}`, async () => {
    const { summary, water } = await sweep(given)
    assert.deepEqual(water, expected.water)
    assert.equal(summary.poured, expected.poured)
    assert.equal(summary.skipped, expected.skipped)
    assert.equal(Boolean(summary.missing?.includes('water_bucket')), false, 'water_bucket is never a missing= item: skipped= says why the channel stays dry')
  })
}
