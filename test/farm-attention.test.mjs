import test from 'node:test'
import assert from 'node:assert/strict'
import { CompositeHandBack } from '../src/composite.mjs'
import { assertFarmRecoverable, farmAct, recoverFarm, reportFarmAttention } from '../src/farm/attention.mjs'
import { parsePlan} from '../src/lib.mjs'
import { planCells } from './plan-fixture.mjs'
import maintain from '../library/farm/maintain.mjs'
import harvest from '../library/farm/harvest.mjs'
import tidy from '../library/farm/tidy.mjs'
import kit from '../library/kit.mjs'
import { fakeApi } from './helpers.mjs'

for (const message of ['cancelled', 'farm.harvest/dig: cancelled', 'health is 4: eat/rest first', 'field is not where its plan says: off by one', "field is Pat's ground and the note on it does not invite work", 'inside protected zone pat-field', 'Unexpected missing schema field', 'empty parser state', 'full stack corruption']) {
  test(`farm recovery propagates ${message}`, () => {
    const error = new Error(message)
    assert.throws(() => recoverFarm(() => 'ignored')(error), e => e === error)
  })
}
for (const message of ['goto: no path to the goal', 'the CHEST is full', 'chest has less than asked (have/wanted): dirt:1/2', 'no hoe', 'place wheat_seeds did not take', 'farm.maintain/dig: cobblestone needs a wooden_pickaxe or better: you carry none, craft one first']) {
  test(`farm recovery permits ${message}`, () => assert.doesNotThrow(() => assertFarmRecoverable(new Error(message))))
}

test('farmAct preserves nested safety hand-back identity and normal completion', async () => {
  for (const stopped of ['health 4', 'food 3 and nothing edible carried', 'night and no bed within 32 blocks', 'twice in a row: goto: no path']) {
    await assert.rejects(farmAct({ act: async () => ({ stopped }) }, 'farm.harvest', {}), e => e instanceof CompositeHandBack && e.reason === stopped)
  }
  for (const stopped of ['done', 'days', 'count', 'until']) assert.equal((await farmAct({ act: async () => ({ stopped }) }, 'farm.harvest', {})).stopped, stopped)
})

const field = { name: 'field', by: 'Tester', kind: 'farm', x: 0, y: 63, z: 0, plan: 'w' }
field.parsed = parsePlan(field.plan)
field.cells = planCells(field)
const world = { '0,63,0': 'farmland', '0,64,0': 'dirt' }

test('a new obstruction after tidy reports attention and does not stop other planting', async () => {
  const plan = { ...field, plan: 'ww', parsed: parsePlan('ww') }
  plan.cells = planCells(plan)
  const world = { '0,63,0': 'farmland', '1,63,0': 'farmland' }
  const items = { wheat_seeds: 2 }
  const { api, calls, events } = fakeApi({ place: plan, world, items, answers: {
    'farm.harvest': {},
    place: p => {
      if (p.x === 0) {
        world['0,64,0'] = 'cobblestone'
        throw new Error('place: placed nothing: 1 cobblestone is already there (first 0,64,0)')
      }
      world[`${p.x},${p.y},${p.z}`] = 'wheat#0'
      items.wheat_seeds--
    }
  } })
  const result = await maintain.run(api, { place: plan.name, compost: false })
  assert.equal(result.sweeps, 1)
  assert.equal(result.replanted, 1)
  assert.equal(world['0,64,0'], 'cobblestone')
  assert.equal(world['1,64,0'], 'wheat#0')
  assert.equal(calls.filter(c => c.startsWith('place ')).length, 2)
  assert.ok(!calls.some(c => c.startsWith('dig ')), 'leave the new obstruction for the next tidy pass')
  assert.ok(events.some(e => e.type === 'farm_attention' && /cobblestone/.test(JSON.stringify(e.reasons))))
})

test('maintenance halts immediately after a nested harvest health stop, before tidying or planting', async () => {
  const { api, calls, events } = fakeApi({ place: field, world, answers: { 'farm.harvest': { harvested: { wheat: 1 }, stopped: 'health 4' } } })
  await assert.rejects(maintain.run(api, { place: field.name }), e => e instanceof CompositeHandBack && e.reason === 'health 4')
  assert.ok(!calls.some(c => /^(zones|dig|till|place|collect|deposit)\b/.test(c)))
  assert.equal(events.filter(e => e.type === 'farm_attention').length, 0)
})

test('maintenance fails closed when protected zones cannot be read', async () => {
  const { api, calls } = fakeApi({ place: field, world, answers: { zones: new Error('zone registry unavailable') } })
  await assert.rejects(maintain.run(api, { place: field.name }), /zone registry unavailable/)
  assert.ok(!calls.some(c => /^(dig|till|place)\b/.test(c)))
})

test('harvest does not swallow cancellation while cutting a crop', async () => {
  const { api, calls } = fakeApi({ world: { '0,64,0': 'wheat#7' }, answers: {
    find_blocks: { positions: [{ x: 0, y: 64, z: 0 }] }, dig: new Error('cancelled')
  } })
  await assert.rejects(harvest.run(api, {}), /cancelled/)
  assert.ok(!calls.some(c => /^(collect|place)\b/.test(c)))
})

test('tidy does not swallow cancellation while clearing a block', async () => {
  const { api, calls } = fakeApi({ places: [field], world, answers: { dig: new Error('cancelled') } })
  await assert.rejects(tidy.run(api, { place: field.name }), /cancelled/)
  assert.ok(!calls.some(c => c.startsWith('collect')))
})

test('kit does not continue to bake food after crafting is cancelled', async () => {
  const { api, calls } = fakeApi({ items: { cobblestone: 4, stick: 4, wheat: 36 }, answers: { craft: new Error('cancelled') } })
  await assert.rejects(kit.run(api, { tools: 'stone_hoe' }), /cancelled/)
  assert.deepEqual(calls, ['craft item=stone_hoe count=2'])
})

test('attention combines unresolved shortages and protected obstructions, but healthy growth is quiet', () => {
  const { api, events } = fakeApi()
  reportFarmAttention(api, { action: 'farm.maintain', place: 'field', summary: { stillGrowing: 10, lowSlabs: 2 } })
  assert.equal(events.length, 0)
  reportFarmAttention(api, { action: 'farm.maintain', place: 'field', summary: { bare: '1 no seed', leftAlone: 'torch@0,64,0', clutter: '1(dirt)' } })
  assert.equal(events.length, 1)
  assert.deepEqual(events[0].reasons, { bare: '1 no seed', leftAlone: 'torch@0,64,0', clutter: '1(dirt)' })
})

for (const storage of ['available', 'full', 'missing']) {
  test(`full inventory with configured ${storage} storage: unload before kit or report and stop`, async () => {
    const items = { wheat: 64, wheat_seeds: 2 }
    const { api, calls, events } = fakeApi({ place: field, world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '5,64,0': storage === 'missing' ? 'air' : 'chest' }, items, answers: {
      deposit: () => { if (storage === 'full') throw new Error('the CHEST is full'); delete items.wheat; return {} }
    } })
    api.freeSlots = () => items.wheat ? 0 : 1
    const running = maintain.run(api, { place: field.name, deposit: '5,64,0' })
    if (storage === 'available') {
      await running
      assert.ok(calls.findIndex(c => c.startsWith('deposit ')) < calls.findIndex(c => c.startsWith('kit ')))
      assert.equal(items.wheat_seeds, 2)
    } else {
      await assert.rejects(running, e => e instanceof CompositeHandBack && /^inventory full/.test(e.reason))
      assert.ok(!calls.some(c => /^(kit|farm.harvest)\b/.test(c)))
      const attention = events.filter(e => e.type === 'farm_attention')
      assert.equal(attention.length, 1)
      assert.equal(attention[0].reasons.inventoryFull, 'configured storage could not free a slot')
    }
  })
}

test('dry cane beds report attention without repeated place failures while other crops are planted', async () => {
  const cane = { ...field, plan: 'ssws' }
  cane.parsed = parsePlan(cane.plan)
  cane.cells = planCells(cane)
  const world = { '0,63,0': 'sand', '1,63,0': 'sand', '2,63,0': 'farmland', '3,63,0': 'sand', '3,63,1': 'oak_slab~' }
  const { api, calls, events } = fakeApi({ place: cane, world, items: { wheat_seeds: 2, sugar_cane: 4 }, answers: {
    place: a => { world[`${a.x},${a.y},${a.z}`] = a.item === 'wheat_seeds' ? 'wheat#0' : a.item; return {} }
  } })
  const result = await maintain.run(api, { place: cane.name })
  assert.equal(result.replanted, 2)
  assert.match(result.bare, /water:2 no adjacent water/)
  assert.deepEqual(calls.filter(c => c.startsWith('place ')), ['place item=wheat_seeds x=2 y=64 z=0', 'place item=sugar_cane x=3 y=64 z=0'])
  assert.equal(events.filter(e => e.type === 'farm_attention').length, 1)
})
