import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import { makeBoatRuntime } from '../src/body/boat.mjs'
const require = createRequire(import.meta.url)
const protocol = require('minecraft-protocol')

function fixture () {
  const bot = new EventEmitter()
  bot._client = new EventEmitter()
  bot.entity = { id: 5, name: 'player' }
  bot.entities = { 5: bot.entity, 8: { id: 8, name: 'horse' }, 9: { id: 9, name: 'cow' }, 10: { id: 10, name: 'oak_boat' } }
  const holders = new Map()
  const runtime = makeBoatRuntime({ getBot: () => bot, getBoatLeashHolder: () => holders, inventoryCounts: () => ({}) })
  runtime.attach()
  const serializer = protocol.createSerializer({ state: 'play', isServer: true, version: '26.1' })
  const deserializer = protocol.createDeserializer({ state: 'play', version: '26.1' })
  const receive = (entityId, vehicleId) => {
    const packet = { name: 'attach_entity', params: { entityId, vehicleId } }
    const decoded = deserializer.parsePacketBuffer(serializer.createPacketBuffer(packet)).data
    bot._client.emit(decoded.name, decoded.params)
  }
  return { bot, holders, runtime, receive }
}

test('real attachment packets confirm horse/cow/boat holders without inventing self ownership', () => {
  const f = fixture()
  f.receive(8, 5); f.receive(9, 77); f.receive(10, 5)
  assert.equal(f.holders.get(8), f.bot.entity.id)
  assert.equal(f.holders.get(9), 77)
  assert.notEqual(f.holders.get(9), f.bot.entity.id)
  assert.equal(f.holders.get(10), f.bot.entity.id)
  f.receive(8, 0) // Actual Paper26.2 ClientboundSetEntityLinkPacket null holder.
  f.receive(10, -1) // Older protocol detach remains accepted.
  assert.equal(f.holders.has(8), false)
  assert.equal(f.holders.has(10), false)
  f.receive(999, 5)
  assert.equal(f.holders.has(999), false, 'an unobserved entity is never claimed')
})

test('removed entity IDs lose attachments; absent holder alone does not claim a free animal', () => {
  const f = fixture()
  f.receive(8, 5)
  f.bot.emit('entityGone', f.bot.entity)
  assert.equal(f.holders.get(8), 5)
  const horse = f.bot.entities[8]
  delete f.bot.entities[8]
  f.bot.emit('entityGone', horse)
  f.bot.entities[8] = { id: 8, name: 'horse' }
  assert.equal(f.holders.has(8), false)
})

test('tracking animal leads does not permit boat actions on animals', async () => {
  const f = fixture()
  f.receive(8, 5)
  await assert.rejects(f.runtime.long.boat_leash({ id: 8 }), /no boat/)
  await assert.rejects(f.runtime.long.boat_unleash({ id: 8 }), /no boat/)
  await assert.rejects(f.runtime.long.boat_recover({ id: 8 }), /no boat/)
})
