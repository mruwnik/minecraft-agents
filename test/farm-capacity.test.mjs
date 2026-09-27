import test from 'node:test'
import assert from 'node:assert/strict'
import { parsePlan, planCells } from '../src/lib.mjs'
import { seedReserve } from '../src/lib/farm.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import maintain from '../library/farm/maintain.mjs'
import { fakeApi } from './helpers.mjs'

const field = (name, plan) => ({ name, kind: 'farm', x: 0, y: 63, z: 0, plan, parsed: parsePlan(plan), cells: planCells({ plan, x: 0, y: 63, z: 0 }) })
const own = field('test-field', 'w')
const planted = { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '5,64,0': 'chest' }

test('retained-base stalk reserve keeps small plots twice over and caps each species across all plans', () => {
  assert.deepEqual(seedReserve([parsePlan('ssBBB')]), { sugar_cane: 4, bamboo: 6 })
  assert.deepEqual(seedReserve([parsePlan('s'.repeat(24)), parsePlan('s'.repeat(24)), parsePlan('B'.repeat(40)), parsePlan('B'.repeat(10))]), { sugar_cane: 64, bamboo: 64 })
  assert.deepEqual(seedReserve([parsePlan('m'.repeat(40)), parsePlan('k'.repeat(40)), parsePlan('w'.repeat(40))]), { melon_seeds: 80, pumpkin_seeds: 80, wheat_seeds: 80 })
})

test('low capacity unloads excess retained-base stalk stock before kit and harvest, into configured storage only', async () => {
  const cane = field('cane-field', 's'.repeat(40))
  const items = { sugar_cane: 966, wheat_seeds: 2, wheat: 20, stone_hoe: 1 }
  const deposited = []
  const { api, calls, events } = fakeApi({ place: own, world: planted, items, answers: {
    deposit: a => { assert.deepEqual([a.x, a.y, a.z], [5, 64, 0]); deposited.push(a.items); for (const [item, count] of Object.entries(a.items)) items[item] -= count },
    'farm.harvest': () => { assert.equal(items.sugar_cane, 64); return { harvested: {}, replanted: 0 } }
  } })
  api.plan = name => name === cane.name ? cane : own
  api.freeSlots = () => items.sugar_cane > 64 ? 2 : 17
  await maintain.run(api, { place: own.name, deposit: '5,64,0', reserve_for: cane.name, compost: false })
  assert.deepEqual(deposited, [{ sugar_cane: 902, wheat: 20 }])
  const depositAt = calls.findIndex(c => c.startsWith('deposit '))
  assert.ok(depositAt < calls.findIndex(c => c.startsWith('kit ')))
  assert.ok(depositAt < calls.findIndex(c => c.startsWith('farm.harvest ')))
  assert.equal(items.wheat_seeds, 2)
  assert.ok(!calls.some(c => c.startsWith('find_blocks ')), 'does not discover an unconfigured destination')
  assert.deepEqual(events, [])
})

test('mid-harvest inventory hand-back emits attention with honest partial harvest and does not sow or unload afterward', async () => {
  const reason = 'inventory full and no chest to deposit in'
  const { api, calls, events, report } = fakeApi({ place: own, world: planted, items: { wheat_seeds: 2 }, answers: {
    'farm.harvest': { stopped: reason, harvested: { wheat: 7 }, replanted: 0 }
  } })
  await assert.rejects(maintain.run(api, { place: own.name, deposit: '5,64,0' }), e => e instanceof CompositeHandBack && e.reason === reason)
  assert.deepEqual(report.harvested, { wheat: 7 })
  assert.equal(calls.at(-1), 'farm.harvest place=test-field within=8')
  assert.ok(!calls.some(c => /^(place|till|deposit|dig) /.test(c)))
  assert.equal(events.length, 1)
  assert.equal(events[0].type, 'farm_attention')
  assert.match(events[0].reasons.inventoryFull, /make room in configured storage/)
})

test('inventory checkpoint after kit emits attention and preserves the stop before harvest', async () => {
  const { api, calls, events } = fakeApi({ place: own, world: planted, items: { wheat_seeds: 2 } })
  api.checkpoint = async () => { throw new CompositeHandBack('inventory full and no chest to deposit in') }
  await assert.rejects(maintain.run(api, { place: own.name, deposit: false }), e => e instanceof CompositeHandBack)
  assert.ok(!calls.some(c => c.startsWith('farm.harvest ')))
  assert.equal(events[0].type, 'farm_attention')
  assert.match(events[0].reasons.inventoryFull, /inventory full/)
})

test('low capacity without configured storage never chooses a new destination', async () => {
  const { api, calls } = fakeApi({ place: own, world: planted, items: { wheat: 64, wheat_seeds: 2 }, freeSlots: 2 })
  await maintain.run(api, { place: own.name, deposit: false })
  assert.ok(calls.some(c => c.startsWith('farm.harvest ')))
  assert.ok(!calls.some(c => /^(deposit|find_blocks) /.test(c)))
})
