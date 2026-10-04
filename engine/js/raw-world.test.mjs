// Why JavaScript: tests raw-world.mjs, which stays JS: Mineflayer boundary over packed chunk data.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { createRawWorld, sightTable, UNLOADED } from './raw-world.mjs'
import { packNibbles } from './view.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const ChunkColumn = prismarineChunk(registry)
const id = name => registry.blocksByName[name].defaultState
const SECTIONS = 384 >> 4

// light from one-byte-per-cell arrays in vanilla order (as view.test.mjs loads it)
const loadLight = (column, sky, block) => {
  const buffers = cells => Array.from({ length: SECTIONS }, (_, s) => Buffer.from(packNibbles(cells.subarray(s * 4096, (s + 1) * 4096))))
  const mask = [[0, ((1 << SECTIONS) - 1) << 1]]
  column.loadParsedLight(buffers(sky), buffers(block), mask, mask, [[0, 0]], [[0, 0]])
}
const cellIndex = (x, y, z) => ((y + 64) << 8) | (z << 4) | x

const makeBot = () => {
  const column = new ChunkColumn({ minY: -64, worldHeight: 384 })
  column.setBlockStateId(new Vec3(3, 70, 4), id('stone'))
  const sky = new Uint8Array(SECTIONS * 4096)
  const block = new Uint8Array(SECTIONS * 4096)
  sky[cellIndex(3, 71, 4)] = 12
  block[cellIndex(3, 71, 4)] = 5
  loadLight(column, sky, block)
  const bot = new EventEmitter()
  Object.assign(bot, {
    registry,
    version: '26.1',
    game: { minY: -64, height: 384, dimension: 'overworld' },
    time: { timeOfDay: 18000 },
    rainState: 0,
    entity: { position: new Vec3(1.5, 64, 2.5), yaw: 1, pitch: -0.5 },
    world: { getColumn: (cx, cz) => cx === 0 && cz === 0 ? column : undefined }
  })
  return { bot, column }
}

test('stateAt reads the column and answers UNLOADED where no column is', () => {
  const { bot } = makeBot()
  const raw = createRawWorld({ getBot: () => bot })
  assert.deepEqual([raw.stateAt(3, 70, 4), raw.stateAt(3, 71, 4), raw.stateAt(40, 70, 4), raw.stateAt(3, 400, 4)],
    [id('stone'), id('air'), UNLOADED, UNLOADED])
})

test('lightAt packs sky and block light of the cell', () => {
  const { bot } = makeBot()
  const raw = createRawWorld({ getBot: () => bot })
  assert.deepEqual([raw.lightAt(3, 71, 4), raw.lightAt(40, 71, 4)], [12 << 4 | 5, 0])
})

test('lightAt prefers the relight overlay of the section', () => {
  const { bot } = makeBot()
  const sky = new Uint8Array(4096)
  const block = new Uint8Array(4096).fill(9)
  const raw = createRawWorld({ getBot: () => bot, lightOverlay: (cx, cz, s) => cx === 0 && cz === 0 && s === 8 ? { sky, block } : undefined })
  assert.equal(raw.lightAt(3, 71, 4), 9)
})

test('a block update reaches the listeners with the new state and the stale section copy is dropped', () => {
  const { bot, column } = makeBot()
  const raw = createRawWorld({ getBot: () => bot })
  const seen = []
  raw.onBlockChange((x, y, z, state) => seen.push([x, y, z, state]))
  raw.stateAt(3, 70, 4)
  column.setBlockStateId(new Vec3(3, 70, 4), id('dirt'))
  bot.emit('blockUpdate', { position: new Vec3(3, 70, 4), stateId: id('stone') }, { position: new Vec3(3, 70, 4), stateId: id('dirt') })
  assert.deepEqual([seen, raw.stateAt(3, 70, 4)], [[[3, 70, 4, id('dirt')]], id('dirt')])
})

test('the listener follows a reconnected bot', () => {
  const first = makeBot().bot
  const second = makeBot().bot
  let bot = first
  const raw = createRawWorld({ getBot: () => bot })
  const seen = []
  raw.onBlockChange((x, y, z) => seen.push([x, y, z]))
  bot = second
  raw.eye()
  first.emit('blockUpdate', { position: new Vec3(1, 1, 1), stateId: 1 }, { position: new Vec3(1, 1, 1), stateId: 0 })
  second.emit('blockUpdate', { position: new Vec3(2, 2, 2), stateId: 1 }, { position: new Vec3(2, 2, 2), stateId: 0 })
  assert.deepEqual(seen, [[2, 2, 2]])
})

test('eye is the head position and look in mineflayer radians, null offline', () => {
  const { bot } = makeBot()
  const raw = createRawWorld({ getBot: () => bot })
  const offline = createRawWorld({ getBot: () => bot, isOffline: () => true })
  assert.deepEqual([raw.eye(), offline.eye()], [{ x: 1.5, y: 65.62, z: 2.5, yaw: 1, pitch: -0.5, dimension: 'overworld' }, null])
})

test('sightTable blocks sight for full blocks, not for glass, water, grass or torches', () => {
  const table = sightTable(registry)
  assert.deepEqual(['stone', 'diamond_ore', 'oak_leaves', 'glass', 'water', 'short_grass', 'torch', 'air'].map(n => table[id(n)]),
    [1, 1, 1, 0, 0, 0, 0, 0])
})

test('stateInfo names a state with its properties', () => {
  const { bot } = makeBot()
  const raw = createRawWorld({ getBot: () => bot })
  const info = raw.stateInfo(id('oak_log'))
  assert.deepEqual([info.name, info.properties.axis], ['oak_log', 'y'])
})
