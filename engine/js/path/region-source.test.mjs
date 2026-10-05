// Why JavaScript: tests region-source.mjs, which stays JS: Mineflayer boundary (the bot's chunk and block events).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { UNLOADED } from './snapshot.mjs'
import { regionSource } from './region-source.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const ChunkColumn = prismarineChunk(registry)
const id = name => registry.blocksByName[name].defaultState

const rig = () => {
  const columns = new Map([['0,0', new ChunkColumn()]])
  const bot = new EventEmitter()
  bot.game = { minY: -64, height: 384 }
  bot.world = {
    getColumn: (cx, cz) => columns.get(`${cx},${cz}`),
    getColumns: () => [...columns.keys()].map(k => { const [x, z] = k.split(','); return { chunkX: x, chunkZ: z } })
  }
  const source = regionSource(bot)
  const seen = []
  const stop = source.onChange(e => seen.push(e))
  return { bot, columns, source, seen, stop }
}

test('columns lists the loaded columns as numbers', () => {
  assert.deepEqual(rig().source.columns(), [[0, 0]])
})

test('a block update reaches the kept snapshot before the change is told', () => {
  const { bot, source, seen } = rig()
  assert.equal(source.snapshot.stateAt(1, 64, 1), 0)
  const stone = id('stone')
  bot.emit('blockUpdate', { stateId: 0, position: new Vec3(1, 64, 1) }, { stateId: stone, position: new Vec3(1, 64, 1) })
  assert.equal(source.snapshot.stateAt(1, 64, 1), stone)
  assert.deepEqual(seen, [{ type: 'block', x: 1, y: 64, z: 1, old: 0 }])
})

test('an update to the same state is not a change', () => {
  const { bot, seen } = rig()
  bot.emit('blockUpdate', { stateId: 0, position: new Vec3(1, 64, 1) }, { stateId: 0, position: new Vec3(1, 64, 1) })
  assert.deepEqual(seen, [])
})

test('a column loaded or unloaded is read afresh and told', () => {
  const { bot, columns, source, seen } = rig()
  assert.equal(source.snapshot.hasColumn(1, 0), false)
  columns.set('1,0', new ChunkColumn())
  bot.emit('chunkColumnLoad', new Vec3(16, 0, 0))
  assert.equal(source.snapshot.hasColumn(1, 0), true)
  columns.delete('1,0')
  bot.emit('chunkColumnUnload', new Vec3(16, 0, 0))
  assert.equal(source.snapshot.stateAt(17, 64, 0), UNLOADED)
  assert.deepEqual(seen, [{ type: 'load', cx: 1, cz: 0 }, { type: 'unload', cx: 1, cz: 0 }])
})

test('the last unsubscribe takes the listeners off the bot', () => {
  const { bot, stop } = rig()
  assert.equal(bot.listenerCount('blockUpdate'), 1)
  stop()
  assert.equal(bot.listenerCount('blockUpdate'), 0)
  assert.equal(bot.listenerCount('chunkColumnLoad'), 0)
})
