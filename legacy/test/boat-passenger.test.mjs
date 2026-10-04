import { test } from 'node:test'
import assert from 'node:assert/strict'
import { BOAT_PASSENGER_MAX_HEIGHT, BOAT_PASSENGER_MAX_WIDTH, boatPassengerProfile, boatPassengerStatus } from '../src/lib.mjs'

const uuid = 'c071f7d4-8b43-4f01-9c2f-92b648d3d143'
const passenger = (name, overrides = {}) => ({ uuid, name, baby: false, width: 0.9, height: 1.4, ...overrides })

test('boat passenger profiles accept only measured adult villagers, cows, sheep and pigs that fit the passage', () => {
  for (const entity of [
    passenger('villager', { width: 0.6, height: 1.95 }),
    passenger('cow'),
    passenger('sheep'),
    passenger('pig')
  ]) {
    const profile = boatPassengerProfile(entity)
    assert.equal(profile.ok, true, entity.name)
    assert.equal(profile.baby, false)
    assert.ok(profile.width <= BOAT_PASSENGER_MAX_WIDTH)
    assert.ok(profile.height <= BOAT_PASSENGER_MAX_HEIGHT)
  }
})

test('boat passenger profiles refuse babies, unknown age, unsupported kinds and missing or oversized hitboxes', () => {
  for (const [entity, pattern] of [
    [passenger('pig', { baby: true }), /babies and unknown age are refused/],
    [passenger('cow', { baby: undefined }), /babies and unknown age are refused/],
    [passenger('horse'), /unsupported boat passenger horse/],
    [passenger('pig', { width: undefined }), /lacks observed hitbox dimensions/],
    [passenger('cow', { width: 1.01 }), /does not fit/],
    [passenger('sheep', { height: 2.01 }), /does not fit/]
  ]) assert.match(boatPassengerProfile(entity).error, pattern)
})

test('boat passenger status requires the exact supported sole passenger profile', () => {
  const state = { boats: [{ id: 8, passengers: [passenger('sheep')] }] }
  assert.equal(boatPassengerStatus(state, 8, uuid), null)
  assert.match(boatPassengerStatus(state, 8, '87b3392e-ae93-4f51-bf07-2f53add88880'), /not in boat/)
  assert.match(boatPassengerStatus({ boats: [] }, 8, uuid), /no longer in sight/)
  assert.match(boatPassengerStatus({ boats: [{ id: 8, passengers: [passenger('sheep'), passenger('pig', { uuid: '87b3392e-ae93-4f51-bf07-2f53add88880' })] }] }, 8, uuid), /exactly one passenger/)
  assert.match(boatPassengerStatus({ boats: [{ id: 8, passengers: [passenger('sheep', { baby: true })] }] }, 8, uuid), /babies and unknown age are refused/)
})
