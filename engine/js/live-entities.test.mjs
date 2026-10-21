// Mineflayer boundary only; which raw entities still count as present.
import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { trackLiveEntities, liveEntities, itemSpawn, noteToss, isOwnToss } from './live-entities.mjs'

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

const player = (id, username, x, y, z) => ({ id, name: 'player', type: 'player', username, position: { x, y, z } })
const itemAt = (id, x, y, z) => ({ id, name: 'item', type: 'other', position: { x, y, z } })

test('an item\'s spawn records where and when it appeared and the player nearest to it, no verdict', () => {
  const bot = botWith({ 1: player(1, 'Ann', 10.5, 64, 3.5), 3: player(3, 'Bob', 20.5, 64, 3.5) })
  trackLiveEntities(bot)
  bot.entities[2] = itemAt(2, 10.95, 65.375, 3.5)
  bot.emit('entitySpawn', bot.entities[2])
  const s = itemSpawn(bot, 2)
  assert.equal(typeof s.at, 'number')
  assert.deepEqual(s.pos, { x: 10.95, y: 65.375, z: 3.5 })
  assert.deepEqual(s.player, { name: 'Ann', pos: { x: 10.5, y: 64, z: 3.5 } })
  assert.equal(s.by, undefined)
  assert.equal(s.tosses, undefined)
})

test('an item\'s spawn carries the body\'s recent tosses', () => {
  const bot = botWith({})
  trackLiveEntities(bot)
  noteToss(bot, 'cobblestone', 3)
  bot.entities[2] = itemAt(2, 0, 64, 0)
  bot.emit('entitySpawn', bot.entities[2])
  const [t] = itemSpawn(bot, 2).tosses
  assert.equal(typeof t.at, 'number')
  assert.equal(t.item, 'cobblestone')
  assert.equal(t.count, 3)
  assert.equal(itemSpawn(bot, 2).player, undefined, 'no player near')
})

test('an item that spawned right after the body\'s toss of its kind is its own toss; another kind or no toss is not', () => {
  const bot = botWith({})
  trackLiveEntities(bot)
  bot.entities[2] = itemAt(2, 0, 64, 0)
  bot.emit('entitySpawn', bot.entities[2])
  assert.equal(isOwnToss(bot, 2, 'cobblestone'), false, 'no toss yet')
  noteToss(bot, 'cobblestone', 3)
  bot.entities[3] = itemAt(3, 0, 64, 0)
  bot.emit('entitySpawn', bot.entities[3])
  assert.equal(isOwnToss(bot, 3, 'cobblestone'), true)
  assert.equal(isOwnToss(bot, 3, 'dirt'), false)
  assert.equal(isOwnToss(bot, 99, 'cobblestone'), false, 'unknown entity')
})

test('the spawn record is forgotten with the entity', () => {
  const bot = botWith({})
  trackLiveEntities(bot)
  bot.entities[2] = itemAt(2, 0, 64, 0)
  bot.emit('entitySpawn', bot.entities[2])
  bot.emit('entityGone', bot.entities[2])
  assert.equal(itemSpawn(bot, 2), undefined)
})
