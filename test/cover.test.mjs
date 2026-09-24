// Covering a channel: a TOP slab over a settled water source, never into flow. The human's rule (09-24): "top slabs
// over water - you can walk better, and it still counts as water. The slab needs to be placed over an actual water
// block - if it's flowing, placing the slab stops the flow." A bottom slab is a half-step down into every channel
// (bodies float and wedge on them, card 1ccb0ea1); a slab dropped into flow is not waterlogged and cuts the flow.
import test from 'node:test'
import assert from 'node:assert/strict'
import { farmJobs, jobCall, planCells, parsePlan, planBill } from '../src/lib.mjs'
import { lowSlabs, lowSlabLine, facesForHalf, FLOW_REASON } from '../src/cover.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import buildFarm from '../library/farm/build.mjs'

const worldOf = table => fakeApi({ world: table }).api.block
const jobsFor = (plan, world, items) => farmJobs({ cells: planCells({ plan, x: 0, y: 63, z: 0 }), worldAt: worldOf(world), items })
const jobLine = j => j.item ? `${j.do} ${j.item} at ${j.x},${j.y},${j.z}` : `${j.do} ${j.x},${j.y},${j.z}`
const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}

// ---------------------------------------------------------------- which cell gets a cover, and what comes first
for (const [name, world, items, expected] of [
  ['a settled source (level 0) is covered', { '0,63,0': 'water' }, { oak_slab: 4 }, ['cover oak_slab at 0,63,0']],
  ['flowing water with a bucket in hand: a source is poured first, then the cover', { '0,63,0': 'water#3', '0,62,0': 'stone' }, { water_bucket: 1, oak_slab: 4 }, ['pour water_bucket at 0,62,0', 'cover oak_slab at 0,63,0']],
  ['flowing water without a bucket is skipped, not capped', { '0,63,0': 'water#3', '0,62,0': 'stone' }, { oak_slab: 4 }, ['skip water_bucket at 0,63,0']],
  ['flow at level 7 (the last block of a spread) is flow all the same', { '0,63,0': 'water#7', '0,62,0': 'stone' }, { oak_slab: 4 }, ['skip water_bucket at 0,63,0']],
  ['a waterlogged top slab is a finished channel', { '0,63,0': 'oak_slab~#top' }, { oak_slab: 4, water_bucket: 1 }, []],
  ['a waterlogged bottom slab is a finished channel too: no churn on old channels', { '0,63,0': 'oak_slab~' }, { oak_slab: 4, water_bucket: 1 }, []]
]) {
  test(`farmJobs: ${name}`, () => assert.deepEqual(jobsFor('~', world, items).map(jobLine), expected))
}

test('farmJobs: the flowing skip names the cell and what has to happen first', () => {
  const [skip] = jobsFor('~', { '0,63,0': 'water#3', '0,62,0': 'stone' }, {})
  assert.equal(skip.why, 'flowing water at 0,63,0: pour a source first, then cover')
  assert.equal(skip.why, FLOW_REASON({ x: 0, y: 63, z: 0 }))
  assert.equal(skip.item, 'water_bucket')
  assert.equal(skip.have, false)
})

test('farmJobs: the pour into a flowing cell says why, since the cell is not dry', () => {
  const [pour] = jobsFor('~', { '0,63,0': 'water#3', '0,62,0': 'stone' }, { water_bucket: 1, oak_slab: 4 })
  assert.match(pour.why, /flowing/)
})

test('jobCall: a cover is a TOP slab', () => {
  const [cover] = jobsFor('~', { '0,63,0': 'water' }, { oak_slab: 4 })
  assert.deepEqual(jobCall(cover), ['place', { item: 'oak_slab', x: 0, y: 63, z: 0, half: 'top' }])
})

// ---------------------------------------------------------------- the face a slab is placed against
// vanilla ignores the cursor on a top or bottom face: a click on the top of the block below always gives a bottom
// slab, a click on the underside of the block above always a top one. Only a side face reads the cursor height. So
// the neighbour a half is placed against must not be the one that fixes the other half
const BELOW = [0, -1, 0]
const ABOVE = [0, 1, 0]
test('facesForHalf: a top slab is placed against a side or the block above, the block below last', () => {
  const faces = facesForHalf('top')
  assert.deepEqual(faces.at(-1), BELOW)
  assert.deepEqual(faces[0], ABOVE)
  assert.equal(faces.length, 6)
})
test('facesForHalf: a bottom slab is placed against the block below first, the block above last', () => {
  const faces = facesForHalf('bottom')
  assert.deepEqual(faces[0], BELOW)
  assert.deepEqual(faces.at(-1), ABOVE)
  assert.equal(faces.length, 6)
})
test('facesForHalf: a plain block keeps the old order, the block below first', () => {
  assert.deepEqual(facesForHalf(undefined), [[0, -1, 0], [0, 1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]])
})

// ---------------------------------------------------------------- old bottom-slab channels: left alone, counted once
const cells = plan => planCells({ plan, x: 0, y: 63, z: 0 })
for (const [name, plan, world, expected] of [
  ['a waterlogged bottom slab in a channel cell is low', '~', { '0,63,0': 'oak_slab~' }, ['0,63,0']],
  ['a waterlogged top slab is not', '~', { '0,63,0': 'oak_slab~#top' }, []],
  ['open water is not a slab', '~', { '0,63,0': 'water' }, []],
  ['a dry bottom slab is a broken channel, not a low one', '~', { '0,63,0': 'oak_slab' }, []],
  ['a cell nobody can see is not counted', '~', {}, []],
  ['only channel cells count: a slab on a path cell is somebody else\'s floor', '.', { '0,63,0': 'oak_slab~' }, []],
  ['two of three', '~~~', { '0,63,0': 'oak_slab~', '1,63,0': 'oak_slab~#top', '2,63,0': 'oak_slab~' }, ['0,63,0', '2,63,0']]
]) {
  test(`lowSlabs: ${name}`, () => assert.deepEqual(lowSlabs(cells(plan), worldOf(world)).map(c => `${c.x},${c.y},${c.z}`), expected))
}

test('lowSlabLine: the count, and how to raise them', () => {
  assert.equal(lowSlabLine(3), '3 (bottom slabs: top slabs walk better; dig and cover again to raise)')
})

// ---------------------------------------------------------------- the composites
test('maintain_farm: old bottom-slab channels are left alone and counted once as lowSlabs', async () => {
  const world = { '0,63,0': 'oak_slab~', '1,63,0': 'oak_slab~#top', '0,62,0': 'stone', '1,62,0': 'stone' }
  const { api, calls } = fakeApi({ place: fakePlace('~~'), world, items: { oak_slab: 4, water_bucket: 1 } })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls.filter(c => c.startsWith('place') || c.startsWith('dig')), [], 'nothing is dug or placed: no churn')
  assert.equal(summary.lowSlabs, '1 (bottom slabs: top slabs walk better; dig and cover again to raise)')
})

test('maintain_farm: a field with no bottom slabs says nothing about them', async () => {
  const world = { '0,63,0': 'oak_slab~#top', '0,62,0': 'stone' }
  const { api } = fakeApi({ place: fakePlace('~'), world, items: { oak_slab: 4 } })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.equal(summary.lowSlabs, undefined)
})

test('maintain_farm: a new cover goes on as a TOP slab', async () => {
  const { api, calls } = fakeApi({ place: fakePlace('~'), world: { '0,63,0': 'water', '0,62,0': 'stone' }, items: { oak_slab: 1 } })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.equal(summary.covered, 1)
  assert.match(calls.join('\n'), /place item=oak_slab x=0 y=63 z=0 half=top/)
})

test('farm.build: a flowing cell with no bucket is left uncapped, with the reason in the report', async () => {
  const world = { '0,63,0': 'water#3', '0,62,0': 'stone' }
  const { api, calls } = fakeApi({ place: fakePlace('~'), world, items: { oak_slab: 4 } })
  const summary = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => c.startsWith('place')), [], 'no slab into flow')
  assert.equal(summary.skipped, '0,63,0 (flowing water at 0,63,0: pour a source first, then cover)')
  assert.equal(summary.missing, 'water_bucket:1')
})

test('farm.build: a flowing cell with a bucket is poured into first, and the cover waits for the source to settle', async () => {
  const world = { '0,63,0': 'water#3', '0,62,0': 'stone' }
  const { api, calls } = fakeApi({
    place: fakePlace('~'), world, items: { oak_slab: 4, water_bucket: 1 },
    // the pour lands: the cell reads as a settled source from then on
    answers: { pour: () => { world['0,63,0'] = 'water'; return {} } }
  })
  const summary = await buildFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls.filter(c => c.startsWith('pour') || c.startsWith('place')), ['pour 0,62,0', 'place item=oak_slab x=0 y=63 z=0 half=top'])
  assert.equal(summary.poured, 1)
  assert.equal(summary.covered, 1)
})
