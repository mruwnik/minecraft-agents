import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, farmJobs, groundJobs } from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { holeJobs } from '../src/lib/fill.mjs'
import { assertFarmRecoverable } from '../src/farm/attention.mjs'
import maintain from '../library/farm/maintain.mjs'
import build from '../library/farm/build.mjs'
import { fakeApi } from './helpers.mjs'

const field = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }) })
const column = (x, ground = 'air') => ({ [`${x},62,0`]: 'dirt', [`${x},63,0`]: ground, [`${x},64,0`]: 'air', [`${x},65,0`]: 'air', [`${x},66,0`]: 'air' })
const key = p => `${p.x},${p.y},${p.z}`

test('maintenance repairs supports for every planned non-water structure and leaves channels to water jobs', () => {
  const plan = field('T#GACKFt~')
  const world = Object.assign({}, ...plan.cells.map(c => column(c.x)))
  const { api } = fakeApi({ world })
  assert.deepEqual(holeJobs({ cells: plan.cells, worldAt: api.block, items: { dirt: 8 } }).map(j => j.x), [0, 1, 2, 3, 4, 5, 6, 7])
})

for (const [name, action] of [['maintain', maintain], ['build', build]]) {
  test(`farm.${name} fills a torch support then places its fence post and light in order`, async () => {
    const plan = field('T')
    const world = column(0)
    const items = { dirt: 1, oak_fence: 1, torch: 1 }
    const { api, calls } = fakeApi({ place: plan, world, items, answers: {
      place: p => {
        if (p.item === 'oak_fence') assert.equal(world['0,63,0'], 'dirt')
        if (p.item === 'torch') assert.equal(world['0,64,0'], 'oak_fence')
        world[key(p)] = p.item
        items[p.item]--
      }
    } })
    const result = await action.run(api, { place: plan.name, compost: false })
    assert.deepEqual(calls.filter(c => c.startsWith('place ')), ['place item=dirt x=0 y=63 z=0', 'place item=oak_fence x=0 y=64 z=0', 'place item=torch x=0 y=65 z=0'])
    assert.equal(result.built, 2)
    assert.equal(world['0,65,0'], 'torch')
  })
}

test('missing support dirt skips both torch levels and other unsupported structures while planting a ready bed', async () => {
  const plan = field('T#w')
  const world = { ...column(0), ...column(1), ...column(2, 'farmland') }
  const { api, calls, events } = fakeApi({ place: plan, world, items: { oak_fence: 2, torch: 1, wheat_seeds: 1 }, answers: {
    place: p => { assert.equal(p.item, 'wheat_seeds'); world[key(p)] = 'wheat#0' }
  } })
  const result = await maintain.run(api, { place: plan.name, compost: false })
  assert.equal(result.replanted, 1)
  assert.match(result.bare, /unfilled:2/)
  assert.match(result.missing, /dirt:2/)
  assert.deepEqual(calls.filter(c => c.startsWith('place ')), ['place item=wheat_seeds x=2 y=64 z=0'])
  assert.ok(events.some(e => e.type === 'farm_attention' && e.reasons.missing === result.missing))
})

test('a fill that reports success but leaves a hole never triggers dependent structure placement', async () => {
  const plan = field('T')
  const { api, calls, events } = fakeApi({ place: plan, world: column(0), items: { dirt: 1, oak_fence: 1, torch: 1 } })
  const result = await maintain.run(api, { place: plan.name, compost: false })
  assert.equal(result.filled, undefined)
  assert.match(result.stuck, /supporting ground is still air/)
  assert.match(result.bare, /unfilled:1/)
  assert.deepEqual(calls.filter(c => c.startsWith('place ')), ['place item=dirt x=0 y=63 z=0'])
  assert.ok(events.some(e => e.type === 'farm_attention'))
})

for (const [name, action] of [['maintain', maintain], ['build', build]]) {
  test(`farm.${name} never places a floating torch when its post placement fails`, async () => {
    const plan = field('T')
    const { api, calls, events } = fakeApi({ place: plan, world: column(0, 'dirt'), items: { oak_fence: 1, torch: 1 }, answers: {
      place: new Error('farm.maintain/place: placed nothing: 1 nothing to place against (first 0,64,0)')
    } })
    const result = await action.run(api, { place: plan.name, compost: false })
    assert.match(result.stuck, /nothing to place against/)
    assert.deepEqual(calls.filter(c => c.startsWith('place ')), ['place item=oak_fence x=0 y=64 z=0'])
    assert.ok(events.some(e => e.type === 'farm_attention'))
  })
}

test('an existing torch and any wooden fence post are preserved by maintenance and levelling', () => {
  const plan = field('T')
  const { api } = fakeApi({ world: { ...column(0, 'dirt'), '0,64,0': 'birch_fence', '0,65,0': 'torch' } })
  assert.deepEqual(farmJobs({ cells: plan.cells, worldAt: api.block, items: {} }), [])
  assert.deepEqual(groundJobs({ cells: plan.cells, worldAt: api.block, solid: api.solid }), [])
})

test('unsupported-placement recovery is specific; unknown failures remain hard', () => {
  assert.doesNotThrow(() => assertFarmRecoverable(new Error('farm.maintain/place: placed nothing: 1 nothing to place against (first 60,69,-111)')))
  const error = new Error('placed nothing: internal inventory state is inconsistent')
  assert.throws(() => assertFarmRecoverable(error), e => e === error)
})
