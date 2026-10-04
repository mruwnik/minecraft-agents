import test from 'node:test'
import assert from 'node:assert/strict'
import { horseState } from '../src/body/riding.mjs'

const bot = {
  registry: {
    entitiesByName: { horse: { metadataKeys: ['flags', 'baby'] } },
    attributesByName: { movementSpeed: { resource: 'minecraft:movement_speed' } }
  }
}
const horse = {
  id: 12, name: 'horse', position: { x: 1, y: 64, z: 2 },
  metadata: [2, false], equipment: [{ name: 'saddle' }],
  attributes: { 'minecraft:movement_speed': { value: 0.25, modifiers: [] } }
}

test('horse identity retains the observed server UUID across transient entity IDs', () => {
  const uuid = 'f1940b6a-4768-4a88-9c14-8f6a91590ded'
  const first = horseState(bot, { ...horse, uuid })
  const reloaded = horseState(bot, { ...horse, id: 91, uuid })
  assert.notEqual(first.id, reloaded.id)
  assert.equal(first.uuid, uuid)
  assert.equal(reloaded.uuid, first.uuid)
})

test('horse identity stays unknown without an observed server UUID', () => {
  assert.equal(horseState(bot, horse).uuid, null)
  assert.equal(horseState(bot, { ...horse, uuid: 12 }).uuid, null)
})
