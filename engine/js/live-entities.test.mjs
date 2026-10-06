// Mineflayer boundary only; which raw entities still count as present.
import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { trackLiveEntities, liveEntities } from './live-entities.mjs'

const drop = (id, count) => ({ id, name: 'item', type: 'other', position: { x: 0, y: 0, z: 0 }, getDroppedItem: () => ({ name: 'stick', count }) })
const botWith = entities => Object.assign(new EventEmitter(), { _client: new EventEmitter(), entities })
const ids = bot => liveEntities(bot).map(e => e.id)

test('an entity no spawn packet described (made by a late packet after its removal) is not listed', () => {
  const bot = botWith({ 1: drop(1, 1), 2: { id: 2, position: { x: 3, y: 20.4, z: 0 } } })
  trackLiveEntities(bot)
  assert.deepEqual(ids(bot), [1])
})

test('a full pickup hides the drop at once, before the removal packet', () => {
  const bot = botWith({ 1: drop(1, 3), 2: drop(2, 1) })
  trackLiveEntities(bot)
  bot._client.emit('collect', { collectedEntityId: 1, collectorEntityId: 9, pickupItemCount: 3 })
  assert.deepEqual(ids(bot), [2])
})

test('a partial pickup leaves the rest of the stack listed', () => {
  const bot = botWith({ 1: drop(1, 5) })
  trackLiveEntities(bot)
  bot._client.emit('collect', { collectedEntityId: 1, collectorEntityId: 9, pickupItemCount: 2 })
  assert.deepEqual(ids(bot), [1])
})

test('a reused id is listed again once the old entity is gone', () => {
  const bot = botWith({ 1: drop(1, 1) })
  trackLiveEntities(bot)
  bot._client.emit('collect', { collectedEntityId: 1, collectorEntityId: 9, pickupItemCount: 1 })
  bot.emit('entityGone', bot.entities[1])
  bot.entities[1] = drop(1, 1)
  assert.deepEqual(ids(bot), [1])
})

test('a mob that died is not listed during its death animation', () => {
  const bot = botWith({ 1: { id: 1, name: 'skeleton', position: { x: 0, y: 0, z: 0 } }, 2: { id: 2, name: 'zombie', position: { x: 1, y: 0, z: 0 } } })
  trackLiveEntities(bot)
  bot._client.emit('entity_status', { entityId: 1, entityStatus: 3 })
  assert.deepEqual(ids(bot), [2])
})

test('a reused id is listed again once the dead mob is gone', () => {
  const bot = botWith({ 1: { id: 1, name: 'skeleton', position: { x: 0, y: 0, z: 0 } } })
  trackLiveEntities(bot)
  bot._client.emit('entity_status', { entityId: 1, entityStatus: 3 })
  bot.emit('entityGone', bot.entities[1])
  assert.deepEqual(ids(bot), [1])
})
