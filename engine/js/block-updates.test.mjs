// Why JavaScript: tests block-updates.mjs, which stays JS: it replaces Mineflayer's packet handlers.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import vec3 from 'vec3'
import prismarineRegistry from 'prismarine-registry'
import prismarineWorld from 'prismarine-world'
import mineflayer from 'mineflayer'
import { installFastBlockUpdates, installFastBlockUpdatesOnInject } from './block-updates.mjs'

const { Vec3 } = vec3
const require = createRequire(import.meta.url)
const registry = prismarineRegistry('26.1')
const World = prismarineWorld(registry)
const Chunk = require('prismarine-chunk')(registry)
const id = name => registry.blocksByName[name].defaultState

function fakeBot (beforeInstall = () => {}) {
  const client = new EventEmitter()
  client.on('block_change', () => assert.fail('the stock handler must be removed'))
  const world = new World(null).sync
  world.setColumn(0, 0, new Chunk())
  const bot = Object.assign(new EventEmitter(), { _client: client, registry, world, supportFeature: name => mineflayer.supportFeature(name, '26.1') })
  const updates = []
  world.on('blockUpdate', (oldBlock, newBlock) => updates.push([registry.blocks[oldBlock.type].name, registry.blocks[newBlock.type].name, newBlock.position.toString()]))
  beforeInstall(bot)
  installFastBlockUpdates(bot)
  return { client, world, updates, bot }
}

test('a block_change writes the state and emits blockUpdate with the old and new type', () => {
  const { client, world, updates } = fakeBot()
  client.emit('block_change', { location: { x: 3, y: 70, z: 5 }, type: id('stone') })
  client.emit('block_change', { location: { x: 3, y: 70, z: 5 }, type: id('dirt') })
  assert.equal(world.getBlockStateId(new Vec3(3, 70, 5)), id('dirt'))
  assert.deepEqual(updates, [['air', 'stone', '(3, 70, 5)'], ['stone', 'dirt', '(3, 70, 5)']])
})

test('a listener of one cell gets full Blocks for it; other cells build none', () => {
  const { client, bot, world } = fakeBot()
  const keyed = []
  const listener = (oldBlock, newBlock) => keyed.push([oldBlock.name, newBlock.name, newBlock.boundingBox])
  bot.on('blockUpdate:(3, 70, 5)', listener)
  world.on('blockUpdate:(3, 70, 5)', listener)
  client.emit('block_change', { location: { x: 3, y: 70, z: 5 }, type: id('stone') })
  client.emit('block_change', { location: { x: 4, y: 70, z: 5 }, type: id('stone') })
  bot.off('blockUpdate:(3, 70, 5)', listener)
  world.off('blockUpdate:(3, 70, 5)', listener)
  client.emit('block_change', { location: { x: 3, y: 70, z: 5 }, type: id('dirt') })
  assert.deepEqual(keyed, [['air', 'stone', 'block']])
})

test('a waiter registered before the install is counted, so its removal cannot starve the next waiter', () => {
  const early = () => {}
  const seen = []
  const late = (oldBlock, newBlock) => seen.push(newBlock.name)
  const { client, bot, world } = fakeBot(b => b.on('blockUpdate:(9, 70, 5)', early))
  bot.off('blockUpdate:(9, 70, 5)', early)
  bot.on('blockUpdate:(3, 70, 5)', late)
  world.on('blockUpdate:(3, 70, 5)', late)
  client.emit('block_change', { location: { x: 3, y: 70, z: 5 }, type: id('stone') })
  assert.deepEqual(seen, ['stone'])
})

test('a multi_block_change writes every record, decoded as Mineflayer does', () => {
  const { client, world, updates } = fakeBot()
  const record = (x, y, z, state) => (state << 12) | (x << 8) | (z << 4) | y
  client.emit('multi_block_change', { chunkCoordinates: { x: 0, y: 4, z: 0 }, records: [record(1, 2, 3, id('stone')), record(15, 0, 9, id('dirt'))] })
  assert.equal(world.getBlockStateId(new Vec3(1, 66, 3)), id('stone'))
  assert.equal(world.getBlockStateId(new Vec3(15, 64, 9)), id('dirt'))
  assert.equal(updates.length, 2)
})

test('an update for a column that has not arrived is ignored', () => {
  const { client, updates } = fakeBot()
  client.emit('block_change', { location: { x: 99, y: 70, z: 5 }, type: id('stone') })
  assert.deepEqual(updates, [])
})

test('after Mineflayer injects its plugins only the fast handlers listen for block updates', async () => {
  const bot = mineflayer.createBot({ host: '127.0.0.1', port: 1, username: 'Dup', auth: 'offline', version: '26.1' })
  bot.on('error', () => {})
  installFastBlockUpdatesOnInject(bot)
  await new Promise(resolve => bot.once('inject_allowed', () => setImmediate(resolve)))
  const counts = [bot._client.listenerCount('block_change'), bot._client.listenerCount('multi_block_change')]
  bot.end()
  assert.deepEqual(counts, [1, 1])
})
