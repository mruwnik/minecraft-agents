// A bed whose ground is gone. farm.maintain assumed the ground stood and only farm.build partial=true put it back: on
// jizo-melon-patch (09-26) the sweep read 71 beds unreachable, and block_at showed AIR at the farmland level over dirt
// one below, all down the south rows. Now a sweep fills such beds first, dirt from the pockets or the plan's chest,
// then tills and plants them like any other (card 1ac82851; where the holes came from is card 94e6dcb1).
import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells, planBill, PLAN_LEGEND, groundItem } from '../src/lib.mjs'
import { fillShortfall, holeJobs, floorItem } from '../src/lib/fill.mjs'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'

const cellsOf = (plan, x = 0, y = 63, z = 0) => planCells({ plan, x, y, z })
const worldAtIn = world => fakeApi({ world }).api.block
const jobLine = j => `${j.do} ${j.item} at ${j.x},${j.y},${j.z}${j.have ? '' : ' (none carried)'}`

test('farm ground repair and maintenance share the plan legend material rule', () => {
  const grounds = [...new Set(Object.values(PLAN_LEGEND).map(spec => spec.ground))]
  assert.deepEqual(grounds.map(ground => [ground, groundItem({ ground }), floorItem({ ground })]), [
    ['farmland', 'dirt', 'dirt'], ['sand', 'sand', 'sand'], ['dirt', 'dirt', 'dirt'],
    ['water', 'dirt', 'dirt'], ['grass_block', 'dirt', 'dirt']
  ])
})
// a plan cell: its ground at y=63, whatever stands on it, air over that, and dirt underneath (the hole's floor)
const column = (x, z, ground, over = 'air') => ({ [`${x},62,${z}`]: 'dirt', [`${x},63,${z}`]: ground, [`${x},64,${z}`]: over, [`${x},65,${z}`]: 'air' })

for (const [name, plan, world, items, expected] of [
  ['air where farmland should be is filled with dirt', 'w', column(0, 0, 'air'), { dirt: 4 }, ['fill dirt at 0,63,0']],
  ['cave air is air', 'w', column(0, 0, 'cave_air'), { dirt: 4 }, ['fill dirt at 0,63,0']],
  ['short grass in a terrain dip is not ground', 'w', column(0, 0, 'short_grass'), { dirt: 4 }, ['fill dirt at 0,63,0']],
  ['water the plan never asked for is filled too', 'w', column(0, 0, 'water'), { dirt: 4 }, ['fill dirt at 0,63,0']],
  ['a bed under standing water is flooded, not a hole: the water count says it', 'w', column(0, 0, 'water', 'water'), { dirt: 4 }, []],
  ['a hole with water standing in it is left to the water count too', 'w', column(0, 0, 'air', 'water'), { dirt: 4 }, []],
  ['dirt, farmland and grass are ground', 'www', { ...column(0, 0, 'dirt'), ...column(1, 0, 'farmland'), ...column(2, 0, 'grass_block') }, { dirt: 4 }, []],
  ['a cell nobody can see is nobody\'s job', 'w', {}, { dirt: 4 }, []],
  ['a cane bed is filled with sand', 's', column(0, 0, 'air'), { sand: 2 }, ['fill sand at 0,63,0']],
  ['a bamboo bed with dirt', 'B', column(0, 0, 'air'), { dirt: 2 }, ['fill dirt at 0,63,0']],
  // a hole in a lane is the worst kind: the sweep parks the body on the lane, and a one-deep hole there is the trap
  ['a lane cell with no ground is filled: the sweep parks there', '.', column(0, 0, 'air'), { dirt: 4 }, ['fill dirt at 0,63,0']],
  ['the ground under a chest cell and a composter cell too', 'CK', { ...column(0, 0, 'air', 'chest'), ...column(1, 0, 'water') }, { dirt: 4 }, ['fill dirt at 0,63,0', 'fill dirt at 1,63,0']],
  ['a flooded lane is not a hole either', '.', column(0, 0, 'air', 'water'), { dirt: 4 }, []],
  ['a channel is farmJobs\'s to refill, while a fence over a hole gets support', '~#', { ...column(0, 0, 'air'), ...column(1, 0, 'air', 'oak_fence') }, { dirt: 4 }, ['fill dirt at 1,63,0']],
  ['no dirt carried is said on the job', 'w', column(0, 0, 'air'), {}, ['fill dirt at 0,63,0 (none carried)']],
  ['every hole of a plan, in plan order', 'ww\nww', { ...column(0, 0, 'air'), ...column(1, 0, 'farmland'), ...column(0, 1, 'air'), ...column(1, 1, 'water') }, { dirt: 4 }, ['fill dirt at 0,63,0', 'fill dirt at 0,63,1', 'fill dirt at 1,63,1']]
]) {
  test(`holeJobs: ${name}`, () => assert.deepEqual(holeJobs({ cells: cellsOf(plan), worldAt: worldAtIn(world), items }).map(jobLine), expected))
}

test('holeJobs: the job says what it found where', () => {
  const [job] = holeJobs({ cells: cellsOf('w'), worldAt: worldAtIn(column(0, 0, 'water')), items: {} })
  assert.equal(job.why, 'water where farmland should be')
})

const holes = (...items) => items.map((item, i) => ({ do: 'fill', x: i, y: 63, z: 0, item }))
for (const [name, jobs, items, expected] of [
  ['three holes and one dirt: two short', holes('dirt', 'dirt', 'dirt'), { dirt: 1 }, { dirt: 2 }],
  ['enough carried: nothing', holes('dirt', 'dirt'), { dirt: 2 }, {}],
  ['each floor block counted on its own', holes('sand', 'dirt'), {}, { sand: 1, dirt: 1 }],
  ['no holes: nothing', [], {}, {}]
]) {
  test(`fillShortfall: ${name}`, () => assert.deepEqual(fillShortfall(jobs, items), expected))
}

// ---------------------------------------------------------------- the sweep over a field with holes
const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}
// three beds and the plan's chest; the beds at x=0 are holes, the one at 1,0 is farmland
const HOLED = { ...column(0, 0, 'air'), ...column(1, 0, 'farmland'), ...column(0, 1, 'air'), ...column(1, 1, 'dirt', 'chest') }
const PLACED = { dirt: 'dirt', sand: 'sand', wheat_seeds: 'wheat#0' }
// a fake body whose place and till change the world and its pockets, so the sweep's second look sees what it did
const sweepOver = ({ plan = 'ww\nwC', world = { ...HOLED }, items, withdraw, place }) => {
  const answers = {
    'farm.harvest': { harvested: {}, replanted: 0 },
    till: a => { world[`${a.x},${a.y},${a.z}`] = 'farmland'; return {} },
    place: a => {
      const refused = place?.(a)
      if (refused) return refused
      world[`${a.x},${a.y},${a.z}`] = PLACED[a.item]
      items[a.item]--
      return {}
    },
    ...(withdraw ? { withdraw } : {})
  }
  return fakeApi({ place: fakePlace(plan), world, items, answers })
}
const dirtFills = calls => calls.filter(c => c.startsWith('place item=dirt'))
const firstTill = calls => calls.findIndex(c => c.startsWith('till'))

test('farm.maintain reports an unfilled grassy dip without trying to hoe it', async () => {
  const { api, calls, events } = sweepOver({ plan: 'w', world: column(0, 0, 'short_grass'), items: { wheat_seeds: 1, stone_hoe: 1 } })
  const result = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.match(result.missing, /dirt:1/)
  assert.match(result.bare, /unfilled/)
  assert.ok(!calls.some(c => /^(till|place) /.test(c)))
  assert.ok(events.some(e => e.type === 'farm_attention' && e.reasons.bare))
})

test('farm.maintain fills a grassy dip before tilling and sowing', async () => {
  const world = column(0, 0, 'short_grass')
  const { api, calls } = sweepOver({ plan: 'w', world, items: { dirt: 1, wheat_seeds: 1, stone_hoe: 1 } })
  const result = await maintainFarm.run(api, { place: 'test-field', compost: false })
  assert.equal(result.filled, 1)
  assert.equal(result.replanted, 1)
  assert.ok(calls.indexOf('place item=dirt x=0 y=63 z=0') < firstTill(calls))
  assert.equal(world['0,63,0'], 'farmland')
  assert.equal(world['0,64,0'], 'wheat#0')
})

test('farm.maintain: holes are filled from the pockets before a bed is tilled, and counted', async () => {
  const { api, calls, report } = sweepOver({ items: { dirt: 4, wheat_seeds: 8, stone_hoe: 1 } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(dirtFills(calls), ['place item=dirt x=0 y=63 z=0', 'place item=dirt x=0 y=63 z=1'])
  assert.ok(calls.lastIndexOf('place item=dirt x=0 y=63 z=1') < firstTill(calls), `fills before tills: ${calls.join(' | ')}`)
  assert.deepEqual([report.filled, report.tilled, report.replanted, report.bare, report.missing, report.unfinished], [2, 2, 3, undefined, undefined, undefined])
  assert.ok(!calls.some(c => c.startsWith('withdraw')), 'nothing to fetch')
})

test('farm.maintain: the dirt the pockets lack comes from the plan\'s chest first', async () => {
  const items = { dirt: 1, wheat_seeds: 8, stone_hoe: 1 }
  const { api, calls, report } = sweepOver({ items, withdraw: () => { items.dirt = 2; return {} } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.equal(calls.find(c => c.startsWith('withdraw')), 'withdraw items(dirt) x=1 y=64 z=1')
  assert.ok(calls.indexOf('withdraw items(dirt) x=1 y=64 z=1') < calls.indexOf('place item=dirt x=0 y=63 z=0'), 'fetched before filling')
  assert.deepEqual([report.filled, report.bare, report.missing], [2, undefined, undefined])
})

test('farm.maintain: a chest short of dirt gives what it has, and the holes left are missing=, bare= and not tilled', async () => {
  const items = { wheat_seeds: 8, stone_hoe: 1 }
  const { api, calls, report } = sweepOver({ items, withdraw: () => { items.dirt = 1; return new Error('chest has less than asked (have/wanted): dirt:1/2; took what there was') } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(dirtFills(calls), ['place item=dirt x=0 y=63 z=0'])
  assert.deepEqual([report.filled, report.missing, report.bare, report.unfinished], [1, 'dirt:1', '1 (unfilled:1 0,63,1)', undefined])
  assert.ok(!calls.some(c => /^(till|place item=wheat_seeds) x=0 y=6[34] z=1/.test(c)), `the hole is neither tilled nor sown: ${calls.join(' | ')}`)
  assert.equal(calls.filter(c => c.startsWith('place item=wheat_seeds')).length, 2, 'the other beds are sown')
})

test('farm.maintain: no chest in the plan and no dirt: the holes are said, the rest of the field is worked', async () => {
  const { api, calls, report } = sweepOver({ plan: 'ww\nww', world: { ...HOLED, ...column(1, 1, 'farmland') }, items: { wheat_seeds: 8, stone_hoe: 1 } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.ok(!calls.some(c => c.startsWith('withdraw')), 'no chest to ask')
  assert.deepEqual([report.filled, report.missing, report.bare, report.replanted], [undefined, 'dirt:2', '2 (unfilled:2 0,63,0, 0,63,1)', 2])
})

test('farm.maintain: a fill the body cannot make leaves its bed unfilled, and the sweep goes on', async () => {
  const { api, calls, report } = sweepOver({ items: { dirt: 4, wheat_seeds: 8, stone_hoe: 1 }, place: a => a.item === 'dirt' && a.z === 1 ? new Error('nowhere to stand within 4.5 of 0,62,1') : null })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([report.filled, report.bare, report.replanted], [1, '1 (unfilled:1 0,63,1)', 2])
  assert.match(report.stuck, /nowhere to stand within 4.5 of 0,62,1/)
  assert.ok(!calls.some(c => /^till x=0 y=63 z=1/.test(c)), 'the hole is not tilled')
})

test('farm.maintain: a hole in the lane is filled with the rest, before the sweep parks on it', async () => {
  const world = { ...column(0, 0, 'farmland'), ...column(1, 0, 'farmland'), ...column(0, 1, 'air'), ...column(1, 1, 'dirt') }
  const { api, calls, report } = sweepOver({ plan: 'ww\n..', world, items: { dirt: 4, wheat_seeds: 8, stone_hoe: 1 } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(dirtFills(calls), ['place item=dirt x=0 y=63 z=1'])
  assert.deepEqual([report.filled, report.bare, report.parked], [1, undefined, '0,64,1 (lane)'])
})

// the order the sweep returns, which orders the task's line (src/composite.mjs compositeResult); the report itself is
// brought up to date before every checkpoint, and its key order is whichever came first
test('farm.maintain: filled= comes right after bare= in the summary', async () => {
  const { api } = sweepOver({ items: { dirt: 4, wheat_seeds: 8, stone_hoe: 1 } })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(Object.keys(summary).slice(0, 5), ['sweeps', 'harvested', 'replanted', 'filled', 'tilled'])
})
