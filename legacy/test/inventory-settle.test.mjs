import test from 'node:test'
import assert from 'node:assert/strict'
import { settleInventory } from '../src/body/inventory-settle.mjs'

test('mounted inventory settles without waiting for suppressed player physics ticks', async () => {
  let elapsed = 0, tickWaits = 0
  const bot = { vehicle: { id: 1 }, waitForTicks: () => { tickWaits++; throw new Error('player physics does not tick aboard') } }
  await settleInventory(bot, () => ({ saddle: 1 }), async ms => { elapsed += ms })
  assert.equal(tickWaits, 0)
  assert.equal(elapsed, 500)
})

test('unmounted settlement still observes native ticks and delayed inventory changes', async () => {
  let waits = 0
  const bot = { vehicle: null, waitForTicks: async ticks => { assert.equal(ticks, 5); waits++ } }
  await settleInventory(bot, () => ({ bread: waits === 1 ? 1 : 2 }), async () => assert.fail('wall clock fallback is only for vehicles'))
  assert.equal(waits, 3)
})

test('vehicle state is checked again after dismount during settlement', async () => {
  let waits = 0, pauses = 0
  const bot = { vehicle: {}, waitForTicks: async () => { waits++ } }
  await settleInventory(bot, () => ({}), async () => { pauses++; bot.vehicle = null })
  assert.equal(pauses, 1)
  assert.equal(waits, 1)
})
