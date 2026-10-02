import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import { parsePlan} from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import buildFarm from '../library/farm/build.mjs'
import { buildFromPlan, waterShortfall } from '../src/build/plan.mjs'
import getSeeds from '../library/farm/get_seeds.mjs'
import compost from '../library/farm/compost.mjs'
import fields from '../library/farm/fields.mjs'
import findSpot from '../library/farm/find_spot.mjs'

const field = plan => ({ name: 'test-field', kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }) })
const assertAttention = (events, action) => {
  assert.equal(events.length, 1)
  assert.equal(events[0].type, 'farm_attention')
  assert.equal(events[0].action, action)
  assert.ok(Object.keys(events[0].reasons).length)
  assert.ok(events[0].advice)
}

test('farm.build missing materials requests attention before mutation; shared builder default remains strict', async () => {
  const options = { place: field('w'), world: { '0,63,0': 'farmland' } }
  const { api, calls, events } = fakeApi(options)
  const result = await buildFarm.run(api, { place: 'test-field' })
  assert.equal(result.missing, 'wheat_seeds:1')
  assert.ok(!calls.some(c => /^(dig|till|place) /.test(c)))
  assertAttention(events, 'farm.build')
  await assert.rejects(buildFromPlan(fakeApi(options).api, { place: 'test-field' }), /still needs wheat_seeds:1/)
})

test('farm.build partial mode does available sowing and reports what remains', async () => {
  const world = { '0,63,0': 'farmland', '1,63,0': 'farmland' }
  const items = { wheat_seeds: 1 }
  const { api, calls, events } = fakeApi({ place: field('ww'), world, items, answers: {
    place: a => { world[`${a.x},${a.y},${a.z}`] = 'wheat#0'; items.wheat_seeds-- }
  } })
  const result = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.equal(result.planted, 1)
  assert.equal(result.missing, 'wheat_seeds:1')
  assert.equal(calls.filter(c => c.startsWith('place ')).length, 1)
  assertAttention(events, 'farm.build')
})

test('farm.build skips an unfilled grassy dip and plants an independent ready bed', async () => {
  const world = { '0,62,0': 'grass_block', '0,63,0': 'short_grass', '1,63,0': 'farmland' }
  const items = { wheat_seeds: 2, stone_hoe: 1 }
  const { api, calls, events } = fakeApi({ place: field('ww'), world, items, answers: {
    place: a => { world[`${a.x},${a.y},${a.z}`] = 'wheat#0'; items.wheat_seeds-- }
  } })
  api.solid = name => !['air', 'water', 'short_grass'].includes(name)
  const result = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.match(result.missing, /dirt/)
  assert.match(result.blocked, /unfilled bed 0,63,0/)
  assert.equal(result.planted, 1)
  assert.ok(!calls.some(c => c.startsWith('till ')))
  assert.equal(world['0,63,0'], 'short_grass')
  assert.equal(world['1,64,0'], 'wheat#0')
  assertAttention(events, 'farm.build')
})

test('farm.build skips a bed capped with cobblestone and tills and plants the rest', async () => {
  const world = { '0,63,0': 'grass_block', '1,63,0': 'cobblestone' }
  const items = { wheat_seeds: 2, stone_hoe: 1 }
  const { api, calls, events } = fakeApi({ place: field('ww'), world, items, answers: {
    till: a => {
      if (world[`${a.x},${a.y},${a.z}`] === 'cobblestone') throw new Error(`till: tilled nothing: 1 can't turn cobblestone into farmland (first ${a.x},${a.y},${a.z})`)
      world[`${a.x},${a.y},${a.z}`] = 'farmland'
    },
    place: a => { world[`${a.x},${a.y},${a.z}`] = 'wheat#0'; items.wheat_seeds-- }
  } })
  const result = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.equal(result.tilled, 1)
  assert.equal(result.planted, 1)
  assert.match(result.unfinished, /cobblestone where farmland should be/)
  assert.match(result.unfinished, /1,63,0/)
  assert.equal(world['0,63,0'], 'farmland')
  assert.equal(world['0,64,0'], 'wheat#0')
  assert.equal(world['1,63,0'], 'cobblestone')
  assert.equal(world['1,64,0'], undefined)
  assert.equal(calls.filter(c => c.startsWith('till ')).length, 2)
  assertAttention(events, 'farm.build')
})

// A trampled bed (card trampled-retill): farmJobs built the job list before the walk crossed the field, and the
// walk to bed0's own till trampled bed1's farmland back to dirt on the way. Found dirt there, at job time, bed1
// is retilled in place and planted - not left bare for the mock's place to refuse, the way a live field found it
test('farm.build retills a bed trampled back to dirt since the job list was built, and plants it', async () => {
  const world = { '0,63,0': 'dirt', '1,63,0': 'farmland', '4,63,0': 'water' }
  const items = { wheat_seeds: 2, stone_hoe: 1 }
  const { api, calls, events } = fakeApi({ place: field('ww'), world, items, answers: {
    till: a => {
      world[`${a.x},${a.y},${a.z}`] = 'farmland'
      if (a.x === 0) world['1,63,0'] = 'dirt'
    },
    place: a => {
      const ground = world[`${a.x},${a.y - 1},${a.z}`]
      if (ground !== 'farmland') throw new Error(`place: placed nothing: 1 ${ground} is already there (first ${a.x},${a.y},${a.z})`)
      world[`${a.x},${a.y},${a.z}`] = 'wheat#0'; items.wheat_seeds--
    }
  } })
  const result = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.equal(result.tilled, 2)
  assert.equal(result.planted, 2)
  assert.equal(world['1,63,0'], 'farmland')
  assert.equal(world['1,64,0'], 'wheat#0')
  assert.equal(calls.filter(c => c.startsWith('till ')).length, 2)
  assert.equal(result.stuck, undefined)
  assert.equal(result.unfinished, undefined)
  assert.deepEqual(events, [])
})

for (const failure of [new Error('cancelled'), new Error('died at 1,64,1'), new TypeError('unexpected state'), { stopped: 'hurt' }]) {
  test(`farm.build propagates ${failure.message ?? failure.stopped} before further work`, async () => {
    const { api, calls, events } = fakeApi({ place: field('w'), world: { '0,63,0': 'farmland' }, items: { wheat_seeds: 2 }, answers: { goto: failure } })
    await assert.rejects(buildFarm.run(api, { place: 'test-field' }))
    assert.equal(calls.length, 1)
    assert.deepEqual(events, [])
  })
}

test('water provisioning never turns cancellation into an ordinary bucket shortage', async () => {
  const { api } = fakeApi({ items: { bucket: 1 }, answers: { find_blocks: new Error('cancelled') } })
  await assert.rejects(waterShortfall(api), /cancelled/)
})

test('get_seeds reports missing farm storage as attention, with no invented withdrawal', async () => {
  const { api, calls, events } = fakeApi({ place: field('c') })
  const result = await getSeeds.run(api, { crop: 'carrot', count: 8, place: 'test-field' })
  assert.equal(result.got, 0)
  assert.match(result.attention, /no C .*chest/)
  assert.deepEqual(calls, [])
  assertAttention(events, 'farm.get_seeds')
})

test('get_seeds preserves partially withdrawn stock when the chest runs out', async () => {
  const items = { carrot: 2 }
  const { api, events } = fakeApi({ place: field('cC'), items, answers: { withdraw: () => { items.carrot += 3; throw new Error('chest is empty') } } })
  const result = await getSeeds.run(api, { crop: 'carrot', count: 8, place: 'test-field' })
  assert.equal(result.got, 3)
  assert.match(result.short, /5 carrot short/)
  assertAttention(events, 'farm.get_seeds')
})

test('get_seeds exhaustion emits attention while invalid crop remains hard', async () => {
  const { api, events } = fakeApi()
  assert.equal((await getSeeds.run(api, { crop: 'wheat', count: 8 })).got, 0)
  assertAttention(events, 'farm.get_seeds')
  await assert.rejects(getSeeds.run(api, { crop: 'coffee', count: 8 }), /no renewable way/)
})

test('get_seeds does not swallow a nested harvest hand-back or collect afterward', async () => {
  const { api, calls, events } = fakeApi({ answers: {
    find_blocks: { positions: [{ x: 1, y: 64, z: 1 }] },
    'farm.harvest': { stopped: 'hurt', harvested: { sugar_cane: 2 } }
  } })
  await assert.rejects(getSeeds.run(api, { crop: 'sugar_cane', count: 8 }), e => e instanceof CompositeHandBack && e.reason === 'hurt')
  assert.equal(calls.length, 3)
  assert.deepEqual(events, [])
})

test('compost reports only successful feed calls and preserves remaining produce on access failure', async () => {
  let uses = 0
  const items = { wheat: 3 }
  const { api, events } = fakeApi({ world: { '0,64,0': 'composter#0' }, items, answers: { use: () => {
    if (++uses === 2) throw new Error('too far away')
    items.wheat--
  } } })
  const result = await compost.run(api, { x: 0, y: 64, z: 0 })
  assert.equal(result.fed, 'wheat:1')
  assert.equal(items.wheat, 2)
  assert.equal(result.boneMeal, 0)
  assert.match(result.attention, /too far/)
  assertAttention(events, 'farm.compost')
})

test('compost leaves an unloaded target untouched and hard-refuses an explicit wrong block', async () => {
  const { api, calls, events } = fakeApi({ items: { wheat: 3 } })
  assert.match((await compost.run(api, { x: 0, y: 64, z: 0 })).attention, /not loaded/)
  assert.deepEqual(calls, [])
  assertAttention(events, 'farm.compost')
  const wrong = fakeApi({ items: { wheat: 3 }, world: { '0,64,0': 'chest' } })
  await assert.rejects(compost.run(wrong.api, { x: 0, y: 64, z: 0 }), /not a composter/)
  assert.deepEqual(wrong.calls, [])
})

test('compost ordinary partial fill is progress without an attention event', async () => {
  const { api, events } = fakeApi({ items: { wheat: 1 }, world: { '0,64,0': 'composter#1' } })
  const result = await compost.run(api, { x: 0, y: 64, z: 0 })
  assert.equal(result.fed, 'wheat:1')
  assert.ok(result.short)
  assert.deepEqual(events, [])
})

test('compost empties a ready bin before feeding and does not count bone meal as fed seed', async () => {
  const world = { '0,64,0': 'composter#8' }
  const items = { wheat: 1 }
  let uses = 0
  const { api, events } = fakeApi({ items, world, answers: { use: () => {
    uses++
    if (world['0,64,0'] === 'composter#8') world['0,64,0'] = 'composter#0'
    else { items.wheat--; world['0,64,0'] = 'composter#1' }
  } } })
  const result = await compost.run(api, { x: 0, y: 64, z: 0 })
  assert.equal(uses, 2)
  assert.equal(result.boneMeal, 1)
  assert.equal(result.fed, 'wheat:1')
  assert.equal(items.wheat, 0)
  assert.deepEqual(events, [])
})

test('compost cannot feed until inaccessible ready bone meal has been taken', async () => {
  const { api, calls, events } = fakeApi({ items: { wheat: 1 }, world: { '0,64,0': 'composter#8' }, answers: { use: new Error('too far away') } })
  const result = await compost.run(api, { x: 0, y: 64, z: 0 })
  assert.equal(result.fed, undefined)
  assert.equal(result.boneMeal, 0)
  assert.equal(calls.length, 1)
  assertAttention(events, 'farm.compost')
})

test('compost stops immediately on cancellation instead of trying another item', async () => {
  const { api, calls, events } = fakeApi({ items: { wheat: 1, beetroot: 1 }, world: { '0,64,0': 'composter#1' }, answers: { use: new Error('cancelled') } })
  await assert.rejects(compost.run(api, { x: 0, y: 64, z: 0 }), /cancelled/)
  assert.equal(calls.filter(c => c.startsWith('equip ')).length, 1)
  assert.deepEqual(events, [])
})

test('find_spot has actionable attention for an exhausted search and hard validation for bad input', async () => {
  const { api, events } = fakeApi()
  const result = await findSpot.run(api, { range: 2, w: 1, h: 1 })
  assert.match(result.attention, /try a smaller/)
  assertAttention(events, 'farm.find_spot')
  await assert.rejects(findSpot.run(api, { range: NaN }), /range=/)
})

test('fields empty census is quiet and a named missing plan remains invalid', async () => {
  const { api, events } = fakeApi()
  for (let n = 0; n < 2; n++) assert.match((await fields.run(api, {})).text, /no farm plan within/)
  assert.deepEqual(events, [])
  await assert.rejects(fields.run(api, { place: 'unknown' }), /no plan called unknown/)
})
