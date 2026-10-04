import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { leashFields, trackLeashes } from './leash.mjs'

const makeBot = () => Object.assign(new EventEmitter(), { entity: { id: 4 }, _client: new EventEmitter() })
const attach = (bot, entityId, vehicleId) => bot._client.emit('attach_entity', { entityId, vehicleId })

test('an entity nobody ever leashed has no leash fields', () => {
  const bot = makeBot()
  trackLeashes(bot)
  assert.deepEqual(leashFields(bot, { id: 5 }), {})
})

test('an attach packet marks the entity leashed, to this body when it is the holder', () => {
  const bot = makeBot()
  trackLeashes(bot)
  attach(bot, 5, 4)
  attach(bot, 6, 9)
  assert.deepEqual(leashFields(bot, { id: 5 }), { leashed: true, leashedToMe: true, leashHolder: 4 })
  assert.deepEqual(leashFields(bot, { id: 6 }), { leashed: true, leashedToMe: false, leashHolder: 9 })
})

test('holder 0 or below (26.1 sends 0 on unleash) clears the mark', () => {
  for (const holder of [0, -1]) {
    const bot = makeBot()
    trackLeashes(bot)
    attach(bot, 5, 4)
    attach(bot, 5, holder)
    assert.deepEqual(leashFields(bot, { id: 5 }), {})
  }
})

test('an entity that left drops its mark, so a reused id does not inherit it', () => {
  const bot = makeBot()
  trackLeashes(bot)
  attach(bot, 5, 4)
  bot.emit('entityGone', { id: 5 })
  assert.deepEqual(leashFields(bot, { id: 5 }), {})
})

test('tracking twice does not double the listeners; a bot never tracked reads as not leashed', () => {
  const bot = makeBot()
  trackLeashes(bot)
  trackLeashes(bot)
  assert.equal(bot._client.listenerCount('attach_entity'), 1)
  assert.deepEqual(leashFields(makeBot(), { id: 5 }), {})
})
