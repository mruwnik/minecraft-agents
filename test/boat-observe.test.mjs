import test from 'node:test'
import assert from 'node:assert/strict'
import { observePassengerBoat } from '../src/boat/observe.mjs'

const uuid = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const passenger = { uuid, name: 'pig', baby: false, width: 0.9, height: 0.9 }
const boat = (id, overrides = {}) => ({ id, passengers: [passenger], leashHolderId: null, ...overrides })
const state = (boats, overrides = {}) => ({ boats, selfId: 5, mounted: null, ...overrides })
const fake = result => {
  const calls = []
  return { calls, api: { act: async (...args) => { calls.push(args); return result } } }
}

test('observePassengerBoat selects the exact boat ID from a reordered observation', async () => {
  const s = state([boat(2), boat(9, { leashHolderId: 5 })])
  const f = fake(s)
  const observed = await observePassengerBoat(f.api, 9, uuid, { lead: 'self' })
  assert.equal(observed.s, s)
  assert.equal(observed.boat, s.boats[1])
  assert.deepEqual(f.calls, [['boat_state', { id: 9 }]])
})

test('observePassengerBoat refuses a missing boat, wrong passenger and non-sole passenger', async () => {
  for (const [result, message] of [
    [state([boat(2)]), /no longer in sight/],
    [state([boat(9, { passengers: [{ ...passenger, uuid: '87b3392e-ae93-4f51-bf07-2f53add88880' }] })]), /not in boat/],
    [state([boat(9, { passengers: [passenger, { ...passenger, uuid: '87b3392e-ae93-4f51-bf07-2f53add88880' }] })]), /exactly one passenger/]
  ]) await assert.rejects(observePassengerBoat(fake(result).api, 9, uuid), message)
})

test('observePassengerBoat refuses mounted boats by default and permits an explicit mounted observation', async () => {
  const s = state([boat(9)], { mounted: 9 })
  await assert.rejects(observePassengerBoat(fake(s).api, 9, uuid), /dismount boat 9/)
  assert.equal((await observePassengerBoat(fake(s).api, 9, uuid, { unmounted: false })).boat.id, 9)
})

test('observePassengerBoat enforces self-held and self-or-none lead policies', async () => {
  for (const leadHolderId of [null, 77]) {
    const api = fake(state([boat(9, { leashHolderId: leadHolderId })])).api
    await assert.rejects(observePassengerBoat(api, 9, uuid, { lead: 'self' }), /needs a lead held by this bot/)
  }
  await assert.rejects(observePassengerBoat(fake(state([boat(9, { leashHolderId: 77 })])).api, 9, uuid, { lead: 'self-or-none' }), /leashed to somebody else/)
  assert.equal((await observePassengerBoat(fake(state([boat(9)])).api, 9, uuid, { lead: 'self-or-none' })).boat.id, 9)
  assert.equal((await observePassengerBoat(fake(state([boat(9, { leashHolderId: 5 })])).api, 9, uuid, { lead: 'self-or-none' })).boat.id, 9)
})

test('observePassengerBoat rejects an unknown lead policy before reading state', async () => {
  const f = fake(state([boat(9)]))
  await assert.rejects(observePassengerBoat(f.api, 9, uuid, { lead: 'foreign' }), /unknown boat lead observation policy/)
  assert.deepEqual(f.calls, [])
})
