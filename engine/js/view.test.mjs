import { test, mock } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { EventEmitter } from 'node:events'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import {
  encodeColumn, decodeColumnFile, restoreColumn, writeAtomic, columnFile,
  poseSnapshot, poseKey, hudSnapshot, createView, poseHzFromEnv, coalescedWriter, STATS_MS, POSE_REFRESH_MS,
  packNibbles, decodeColumnLight, mergeOverlapping, columnLightSection, columnStateSection
} from './view.mjs'
import { lightTable, relightBox } from './light.mjs'
import { decodeLight } from '../../tools/view/web/decode.mjs'

const { Vec3 } = vec3
const VERSION = '26.1'
const registry = prismarineRegistry(VERSION)
const ChunkColumn = prismarineChunk(registry)
const stone = registry.blocksByName.stone.defaultState
const dirt = registry.blocksByName.dirt.defaultState

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), 'view-test-'))

const makeColumn = () => {
  const column = new ChunkColumn()
  column.setBlockStateId(new Vec3(1, 64, 2), stone)
  column.setBlockStateId(new Vec3(15, -64, 15), dirt)
  column.setSkyLight(new Vec3(1, 65, 2), 15)
  column.setBlockLight(new Vec3(1, 64, 2), 7)
  return column
}

const entity = (id, x, z, extra = {}) => ({
  id, type: 'mob', name: 'zombie', kind: 'Hostile mobs', position: new Vec3(x, 64, z), yaw: 1, pitch: 0, height: 1.95, width: 0.6, ...extra
})

const fakeBot = ({ columns = {}, entities = {}, listing = false } = {}) => {
  const bot = new EventEmitter()
  const self = { position: new Vec3(10.123, 64, -3.5), velocity: new Vec3(0, 0, 0), yaw: 0.5, pitch: -0.1, onGround: true, height: 1.8 }
  Object.assign(bot, {
    version: VERSION,
    entity: self,
    entities: { 1: self, ...entities },
    game: { dimension: 'minecraft:overworld' },
    time: { timeOfDay: 6000 },
    rainState: 0,
    health: 20, food: 18, foodSaturation: 4, oxygenLevel: 20,
    experience: { level: 3, points: 50, progress: 0.5 },
    heldItem: { name: 'stone', count: 3, slot: 36 },
    inventory: { slots: [null, { name: 'stone', count: 3, slot: 1 }, null] },
    currentWindow: null,
    world: {
      getColumn: (x, z) => columns[`${x},${z}`] ?? null,
      ...(listing && { getColumns: () => Object.entries(columns).map(([k, column]) => { const [chunkX, chunkZ] = k.split(','); return { chunkX, chunkZ, column } }) })
    }
  })
  self.effects = { 5: { id: 5, amplifier: 1, duration: 100 } }
  bot.registry = { effects: { 5: { name: 'strength' } } }
  return bot
}

const makeView = (bot, overrides = {}) => {
  const dir = tmp()
  const events = []
  const view = createView({ stateDir: dir, agent: 'Bob', world: 'w', onEvent: e => events.push(e), enabled: true, ...overrides })
  view.attach(bot)
  return { view, dir, events }
}

const worldChunks = dir => path.join(dir, 'worlds', 'w', 'chunks')
const readJson = file => JSON.parse(fs.readFileSync(file, 'utf8'))

test('attach marks columns already loaded dirty, so the spawn column is written by the first flush', async () => {
  const columns = { '0,0': makeColumn(), '-1,2': makeColumn() }
  const { view, dir } = makeView(fakeBot({ columns, listing: true }))
  assert.equal(view.pendingCount(), 2)
  await view.flushColumns()
  assert.equal(fs.readdirSync(worldChunks(dir)).length, 2)
})

test('attach to a world without a column listing still works', () => {
  const { view } = makeView(fakeBot({ columns: { '0,0': makeColumn() } }))
  assert.equal(view.pendingCount(), 0)
})

test('column file round-trips sections, light and header', () => {
  const column = makeColumn()
  const raw = encodeColumn({ column, x: -3, z: 12, t: 1234, body: 'Bob', mcVersion: VERSION })
  const decoded = decodeColumnFile(zlib.deflateSync(raw, { level: 1 }))
  assert.equal(decoded.header.v, 1)
  assert.equal(decoded.header.x, -3)
  assert.equal(decoded.header.z, 12)
  assert.equal(decoded.header.body, 'Bob')
  assert.equal(decoded.header.minY, -64)
  assert.equal(decoded.header.worldHeight, 384)
  assert.deepEqual(decoded.header.parts.map(p => p.name), ['sections', 'biomes', 'light'])
  assert.equal(decoded.biomes.length, 0)
  assert.ok(decoded.sections.equals(column.dump()))
  const fresh = new ChunkColumn({ minY: decoded.header.minY, worldHeight: decoded.header.worldHeight })
  restoreColumn(fresh, decoded)
  assert.equal(fresh.getBlockStateId(new Vec3(1, 64, 2)), stone)
  assert.equal(fresh.getBlockStateId(new Vec3(15, -64, 15)), dirt)
  assert.equal(fresh.getBlockStateId(new Vec3(0, 10, 0)), 0)
  assert.equal(fresh.getSkyLight(new Vec3(1, 65, 2)), 15)
  assert.equal(fresh.getBlockLight(new Vec3(1, 64, 2)), 7)
})

test('column file names allow negative chunk coordinates', () => {
  assert.equal(columnFile('/s', 'w', -3, 12), path.join('/s', 'worlds', 'w', 'chunks', '-3.12.bin'))
})

test('writeAtomic leaves the final file and no temp file', async () => {
  const dir = tmp()
  const file = path.join(dir, 'a', 'b.json')
  await writeAtomic(file, 'one')
  await writeAtomic(file, 'two')
  assert.equal(fs.readFileSync(file, 'utf8'), 'two')
  assert.deepEqual(fs.readdirSync(path.dirname(file)), ['b.json'])
})

test('flush writes queued columns, never more than 8 per flush, and coalesces repeats', async () => {
  const columns = Object.fromEntries(Array.from({ length: 20 }, (_, i) => [`${i},0`, makeColumn()]))
  const bot = fakeBot({ columns })
  const { view, dir } = makeView(bot)
  for (let i = 0; i < 20; i++) bot.emit('chunkColumnLoad', new Vec3(i * 16, 0, 0))
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  assert.equal(view.pendingCount(), 20)
  await view.flushColumns()
  assert.equal(fs.readdirSync(worldChunks(dir)).length, 8)
  assert.equal(view.pendingCount(), 12)
  await view.flushColumns()
  await view.flushColumns()
  assert.equal(fs.readdirSync(worldChunks(dir)).length, 20)
  assert.equal(view.pendingCount(), 0)
  const decoded = decodeColumnFile(fs.readFileSync(path.join(worldChunks(dir), '3.0.bin')))
  assert.equal(decoded.header.body, 'Bob')
  assert.equal(decoded.header.mcVersion, VERSION)
})

test('block update marks only its own column dirty, including negative coordinates', async () => {
  const bot = fakeBot({ columns: { '-1,-1': makeColumn(), '0,0': makeColumn() } })
  const { view, dir } = makeView(bot)
  bot.emit('blockUpdate', null, { position: new Vec3(-1, 70, -16) })
  bot.emit('blockUpdate', null, { position: new Vec3(-5, 71, -2) })
  assert.equal(view.pendingCount(), 1)
  await view.flushColumns()
  assert.deepEqual(fs.readdirSync(worldChunks(dir)), ['-1.-1.bin'])
})

test('update_light on the client marks only its own column dirty', async () => {
  const bot = fakeBot({ columns: { '-1,2': makeColumn(), '0,0': makeColumn() } })
  bot._client = new EventEmitter()
  const { view, dir } = makeView(bot)
  bot._client.emit('update_light', { chunkX: -1, chunkZ: 2 })
  assert.equal(view.pendingCount(), 1)
  await view.flushColumns()
  assert.deepEqual(fs.readdirSync(worldChunks(dir)), ['-1.2.bin'])
})

test('detach removes the update_light listener', async () => {
  const bot = fakeBot({ columns: { '0,0': makeColumn() } })
  bot._client = new EventEmitter()
  const { view } = makeView(bot)
  assert.equal(bot._client.listenerCount('update_light'), 1)
  await view.detach()
  assert.equal(bot._client.listenerCount('update_light'), 0)
  bot._client.emit('update_light', { chunkX: 0, chunkZ: 0 })
  assert.equal(view.pendingCount(), 0)
})

test('a bot without _client attaches and detaches without throwing', async () => {
  const bot = fakeBot()
  const { view } = makeView(bot)
  await view.detach()
})

test('a column that is not loaded any more is skipped, not an error', async () => {
  const bot = fakeBot()
  const { view, events } = makeView(bot)
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  assert.equal(view.pendingCount(), 0)
  assert.deepEqual(events, [])
})

test('chunk unload does not delete the file', async () => {
  const bot = fakeBot({ columns: { '0,0': makeColumn() } })
  const { view, dir } = makeView(bot)
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  bot.emit('chunkColumnUnload', new Vec3(0, 0, 0))
  await view.flushColumns()
  assert.deepEqual(fs.readdirSync(worldChunks(dir)), ['0.0.bin'])
})

test('write errors become one view.error per minute and never throw', async () => {
  const bot = fakeBot({ columns: { '0,0': makeColumn(), '1,0': makeColumn() } })
  let clock = 1000
  const { view, dir, events } = makeView(bot, { now: () => clock })
  fs.mkdirSync(path.join(dir, 'worlds', 'w'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'worlds', 'w', 'chunks'), 'a file where the directory should be')
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  bot.emit('chunkColumnLoad', new Vec3(16, 0, 0))
  await view.flushColumns()
  assert.deepEqual(events.map(e => e.kind), ['view.error'])
  assert.equal(events[0].level, 'warn')
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  assert.equal(events.length, 1)
  clock += 61000
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  assert.equal(events.length, 2)
})

test('pose snapshot has the fixed shape, eye height and nearby entities only', () => {
  const bot = fakeBot({ entities: { 2: entity(2, 12, -3), 3: entity(3, 500, 500), 4: entity(4, 11, -3, { type: 'player', username: 'Al', name: 'Al', kind: undefined }) } })
  const pose = poseSnapshot(bot, { world: 'w', now: 99 })
  assert.equal(pose.v, 1)
  assert.equal(pose.t, 99)
  assert.equal(pose.status, 'online')
  assert.equal(pose.world, 'w')
  assert.deepEqual(pose.pos, { x: 10.123, y: 64, z: -3.5 })
  assert.ok(Math.abs(pose.eye.y - 65.62) < 1e-9)
  assert.equal(pose.yaw, 0.5)
  assert.deepEqual(pose.entities.map(e => e.id).sort(), [2, 4])
  const player = pose.entities.find(e => e.id === 4)
  assert.equal(player.username, 'Al')
  assert.equal(player.kind, null)
  assert.equal(player.health, null)
  assert.equal(pose.timeOfDay, 6000)
  assert.equal(pose.rain, 0)
})

test('sneaking lowers the eye', () => {
  const bot = fakeBot()
  bot.entity.height = 1.45
  assert.ok(Math.abs(poseSnapshot(bot, { world: 'w', now: 1 }).eye.y - 65.27) < 1e-9)
})

test('poseKey ignores time and sub-centimetre jitter but sees real changes', () => {
  const bot = fakeBot({ entities: { 2: entity(2, 12, -3) } })
  const key = () => poseKey(poseSnapshot(bot, { world: 'w', now: Math.random() }))
  const base = key()
  bot.entity.position.x += 0.001
  assert.equal(key(), base)
  bot.entity.yaw += 0.5
  const turned = key()
  assert.notEqual(turned, base)
  bot.entities[2].position.x += 1
  assert.notEqual(key(), turned)
  delete bot.entities[2]
  assert.notEqual(key(), turned)
})

test('hud snapshot lists non-empty inventory slots, effects and the held item', () => {
  const hud = hudSnapshot(fakeBot(), 5)
  assert.equal(hud.v, 1)
  assert.equal(hud.t, 5)
  assert.equal(hud.health, 20)
  assert.equal(hud.saturation, 4)
  assert.deepEqual(hud.xp, { level: 3, points: 50, progress: 0.5 })
  assert.deepEqual(hud.held, { name: 'stone', count: 3 })
  assert.deepEqual(hud.inventory, [{ slot: 1, name: 'stone', count: 3 }])
  assert.deepEqual(hud.effects, [{ name: 'strength', amplifier: 1, duration: 100 }])
  assert.equal(hud.window, null)
})

test('pose writes only on change, but at least every 2 seconds', async () => {
  const bot = fakeBot()
  let clock = 10000
  const { view, dir } = makeView(bot, { now: () => clock })
  const file = path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'pose.json')
  await view.tickPose()
  assert.equal(readJson(file).t, 10000)
  clock += 100
  await view.tickPose()
  assert.equal(readJson(file).t, 10000)
  bot.entity.position.x += 1
  await view.tickPose()
  assert.equal(readJson(file).t, 10100)
  clock += 2000
  await view.tickPose()
  assert.equal(readJson(file).t, 12100)
  assert.equal(view.stats().poses, 3)
})

test('hud writes only on change', async () => {
  const bot = fakeBot()
  let clock = 1
  const { view, dir } = makeView(bot, { now: () => clock })
  const file = path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'hud.json')
  await view.tickHud()
  clock = 5000
  await view.tickHud()
  assert.equal(readJson(file).t, 1)
  bot.food = 10
  await view.tickHud()
  assert.equal(readJson(file).food, 10)
})

test('detach writes one offline pose and stops pose writes; attach resumes', async () => {
  const bot = fakeBot()
  let clock = 1
  const { view, dir } = makeView(bot, { now: () => clock })
  const file = path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'pose.json')
  await view.tickPose()
  clock = 500
  await view.detach()
  const off = readJson(file)
  assert.equal(off.status, 'offline')
  assert.deepEqual(off.pos, { x: 10.123, y: 64, z: -3.5 })
  assert.equal(off.yaw, 0.5)
  assert.ok(Array.isArray(off.entities))
  assert.equal(off.t, 500)
  clock = 9999
  await view.tickPose()
  assert.equal(readJson(file).t, 500)
  const next = fakeBot()
  view.attach(next)
  await view.tickPose()
  assert.equal(readJson(file).status, 'online')
})

test('detach before any pose was written writes the short offline record', async () => {
  const bot = fakeBot()
  const { view, dir } = makeView(bot, { now: () => 7 })
  await view.detach()
  assert.deepEqual(readJson(path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'pose.json')),
    { v: 1, t: 7, world: 'w', status: 'offline', mcVersion: VERSION })
})

test("the bot's own end event writes the offline pose", async () => {
  const bot = fakeBot()
  const { view, dir } = makeView(bot)
  await view.tickPose()
  bot.emit('end', 'timeout')
  await view.idle()
  assert.equal(readJson(path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'pose.json')).status, 'offline')
})

test('attaching a new bot unhooks the old one', async () => {
  const first = fakeBot({ columns: { '0,0': makeColumn() } })
  const { view } = makeView(first)
  view.attach(fakeBot())
  first.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  assert.equal(view.pendingCount(), 0)
})

test('stats count columns, compressed bytes and main-thread ms, then reset', async () => {
  const bot = fakeBot({ columns: { '0,0': makeColumn() } })
  const { view, dir } = makeView(bot)
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  const s = view.stats()
  assert.equal(s.columns, 1)
  assert.equal(s.bytes, fs.statSync(path.join(worldChunks(dir), '0.0.bin')).size)
  assert.ok(s.ms > 0)
  assert.deepEqual(view.stats(), { columns: 0, bytes: 0, ms: 0, poses: 0, poseMs: 0, poseBytes: 0, huds: 0, relightMs: 0, relightMaxMs: 0, relightTableMs: 0, relightStatesMs: 0, relightLightMs: 0, relightFloodMs: 0, relightWriteMs: 0, relightBoxes: 0, relightCells: 0, relightCarried: 0 })
})

test('disabled view creates no directories and ignores everything', async () => {
  const dir = tmp()
  const bot = fakeBot({ columns: { '0,0': makeColumn() } })
  const view = createView({ stateDir: dir, agent: 'Bob', world: 'w', onEvent: () => {}, enabled: false })
  view.attach(bot)
  bot.emit('chunkColumnLoad', new Vec3(0, 0, 0))
  await view.flushColumns()
  await view.tickPose()
  await view.detach()
  view.stop()
  assert.deepEqual(fs.readdirSync(dir), [])
})

const posesIn = async (poseHz, seconds, drive) => {
  mock.timers.enable({ apis: ['setInterval', 'Date'] })
  try {
    const bot = fakeBot()
    const { view } = makeView(bot, { poseHz, now: () => Date.now() })
    for (let i = 0; i < seconds * 100; i++) {
      bot.entity.position.x += 1
      drive(bot)
      mock.timers.tick(10)
      await new Promise(resolve => setImmediate(resolve))
    }
    const { poses } = view.stats()
    view.stop()
    return poses
  } finally {
    mock.timers.reset()
  }
}

test('pose rate is capped at the configured hz', async () => {
  assert.ok(Math.abs(await posesIn(10, 2, () => {}) - 20) <= 1)
  assert.ok(Math.abs(await posesIn(20, 2, () => {}) - 40) <= 1)
})

test('hz 0 writes on every physics tick with no interval', async () => {
  let ticks = 0
  const poses = await posesIn(0, 1, bot => { ticks++; bot.emit('physicsTick') })
  assert.equal(poses, ticks)
  assert.equal(ticks, 100)
})

test('default mode writes one pose per changed physics tick, none for an unchanged one', async () => {
  const bot = fakeBot()
  const { view } = makeView(bot)
  for (let i = 0; i < 5; i++) {
    bot.entity.position.x += 1
    bot.emit('physicsTick')
    await view.idle()
  }
  bot.emit('physicsTick')
  await view.idle()
  const { poses } = view.stats()
  view.stop()
  assert.equal(poses, 5)
})

test('with no physics ticks the refresh timer rewrites an unchanged pose after POSE_REFRESH_MS', async () => {
  mock.timers.enable({ apis: ['setInterval', 'Date'] })
  try {
    const bot = fakeBot()
    const { view } = makeView(bot, { now: () => Date.now() })
    await view.tickPose()
    await view.idle()
    assert.equal(view.stats().poses, 1)
    mock.timers.tick(POSE_REFRESH_MS)
    await new Promise(resolve => setImmediate(resolve))
    await view.idle()
    const { poses } = view.stats()
    view.stop()
    assert.equal(poses, 1)
  } finally {
    mock.timers.reset()
  }
})

test('BODY_VIEW_POSE_HZ is parsed, falling back to 0 (every physics tick) on junk', () => {
  assert.equal(poseHzFromEnv({}), 0)
  assert.equal(poseHzFromEnv({ BODY_VIEW_POSE_HZ: '20' }), 20)
  assert.equal(poseHzFromEnv({ BODY_VIEW_POSE_HZ: '0' }), 0)
  assert.equal(poseHzFromEnv({ BODY_VIEW_POSE_HZ: 'x' }), 0)
  assert.equal(poseHzFromEnv({ BODY_VIEW_POSE_HZ: '-3' }), 0)
})

test('stats report pose ms and bytes apart from column ms', async () => {
  const bot = fakeBot()
  const { view, dir } = makeView(bot)
  await view.tickPose()
  const s = view.stats()
  assert.equal(s.poses, 1)
  assert.equal(s.poseBytes, fs.statSync(path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'view', 'pose.json')).size)
  assert.ok(s.poseMs > 0)
  assert.equal(s.ms, 0)
})

test('overlapping atomic writes to one file do not collide on the temp name', async () => {
  const file = path.join(tmp(), 'x.json')
  await Promise.all(Array.from({ length: 30 }, (_, i) => writeAtomic(file, String(i))))
  assert.deepEqual(fs.readdirSync(path.dirname(file)), ['x.json'])
})

test('a coalesced writer keeps one write in flight and the last data wins', async () => {
  const file = path.join(tmp(), 'p.json')
  const errors = []
  const write = coalescedWriter(file, e => errors.push(e))
  await Promise.all(Array.from({ length: 50 }, (_, i) => write(String(i))))
  assert.equal(fs.readFileSync(file, 'utf8'), '49')
  assert.deepEqual(errors, [])
  assert.deepEqual(fs.readdirSync(path.dirname(file)), ['p.json'])
})

test('a coalesced writer reports errors and keeps going', async () => {
  const dir = tmp()
  fs.writeFileSync(path.join(dir, 'blocker'), '')
  const errors = []
  const write = coalescedWriter(path.join(dir, 'blocker', 'f.json'), e => errors.push(e))
  await write('a')
  await write('b')
  assert.equal(errors.length, 2)
})

test('rapid pose ticks while a write is in flight never raise a view.error', async () => {
  const bot = fakeBot()
  const { view, events } = makeView(bot)
  for (let i = 0; i < 40; i++) {
    bot.entity.position.x += 1
    view.tickPose()
  }
  await view.detach()
  await view.idle()
  assert.deepEqual(events, [])
})

const statsEventsAcross = async phases => {
  mock.timers.enable({ apis: ['setInterval'] })
  try {
    const dir = tmp()
    const events = []
    const view = createView({ stateDir: dir, agent: 'Bob', world: 'w', onEvent: e => events.push(e), enabled: true })
    const counts = []
    for (const phase of phases) {
      const before = events.filter(e => e.kind === 'view.stats').length
      await phase(view)
      mock.timers.tick(STATS_MS + 1)
      await new Promise(resolve => setImmediate(resolve))
      counts.push(events.filter(e => e.kind === 'view.stats').length - before)
    }
    view.stop()
    return counts
  } finally {
    mock.timers.reset()
  }
}

test('view.stats is emitted only while a bot is attached', async () => {
  const counts = await statsEventsAcross([
    () => {},                                  // never attached
    view => view.attach(fakeBot()),            // online
    view => view.detach(),                     // offline
    view => view.attach(fakeBot())             // back online
  ])
  assert.deepEqual(counts, [0, 1, 0, 1])
})

// ---- local relight ----

const table = lightTable(registry)
const AIR = registry.blocksByName.air.defaultState
const stateOf = name => registry.blocksByName[name].defaultState
const MIN_Y = 0
const HEIGHT = 128
const SECTIONS = HEIGHT >> 4
const REGION = 48

// light for a column from one-byte-per-cell arrays in vanilla order (y<<8|z<<4|x per section); the scrambling setSkyLight does is avoided
const loadColumnLight = (column, sky, block) => {
  const buffers = cells => Array.from({ length: SECTIONS }, (_, s) => Buffer.from(packNibbles(cells.subarray(s * 4096, (s + 1) * 4096))))
  const mask = [[0, ((1 << SECTIONS) - 1) << 1]]
  column.loadParsedLight(buffers(sky), buffers(block), mask, mask, [[0, 0]], [[0, 0]])
}

const makeWorld = ({ columns = [0, 1, 2], rows = [0, 1, 2], ground = 60 } = {}) => {
  const map = new Map()
  for (const cx of columns) for (const cz of rows) map.set(`${cx},${cz}`, new ChunkColumn({ minY: MIN_Y, worldHeight: HEIGHT }))
  const world = {
    map,
    set: (x, y, z, state) => map.get(`${x >> 4},${z >> 4}`).setBlockStateId({ x: x & 15, y, z: z & 15 }, state),
    get: (x, y, z) => map.get(`${x >> 4},${z >> 4}`).getBlockStateId({ x: x & 15, y, z: z & 15 }),
    fill: (x0, y0, z0, x1, y1, z1, state) => {
      for (let y = y0; y <= y1; y++) for (let z = z0; z <= z1; z++) for (let x = x0; x <= x1; x++) world.set(x, y, z, state)
    }
  }
  world.fill(0, MIN_Y, 0, REGION - 1, ground, REGION - 1, stateOf('stone'))
  return world
}

// the whole region relit at once: its edge is the fixed shell (air = open sky 15, anything else 0), plus the virtual air layer on top
const fullLight = world => {
  const sy = HEIGHT + 1
  const size = [REGION, sy, REGION]
  const cells = REGION * sy * REGION
  const states = new Uint16Array(cells)
  const sky = new Uint8Array(cells)
  const block = new Uint8Array(cells)
  for (let y = 0; y < sy; y++) {
    for (let z = 0; z < REGION; z++) {
      for (let x = 0; x < REGION; x++) {
        const i = (y * REGION + z) * REGION + x
        states[i] = y === HEIGHT ? AIR : world.get(x, y, z)
        const shell = x === 0 || z === 0 || x === REGION - 1 || z === REGION - 1 || y === 0 || y === sy - 1
        sky[i] = shell && states[i] === AIR ? 15 : 0
      }
    }
  }
  return relightBox({ table, states, sky, block, size })
}

const lightWorld = world => {
  const full = fullLight(world)
  for (const [key, column] of world.map) {
    const [cx, cz] = key.split(',').map(Number)
    const sky = new Uint8Array(SECTIONS * 4096)
    const block = new Uint8Array(SECTIONS * 4096)
    for (let y = 0; y < HEIGHT; y++) {
      for (let z = 0; z < 16; z++) {
        for (let x = 0; x < 16; x++) {
          const from = (y * REGION + cz * 16 + z) * REGION + cx * 16 + x
          const to = (y >> 4) * 4096 + ((y & 15) << 8 | z << 4 | x)
          sky[to] = full.sky[from]
          block[to] = full.block[from]
        }
      }
    }
    loadColumnLight(column, sky, block)
  }
}

const worldBot = world => {
  const bot = fakeBot({ columns: Object.fromEntries(world.map) })
  bot.registry = undefined
  return bot
}

// what the dump says now about one column: the file written last, or the column's own light if never written
const dumpedCells = (world, dir, cx, cz) => {
  const file = columnFile(dir, 'w', cx, cz)
  const { light } = fs.existsSync(file)
    ? decodeColumnFile(fs.readFileSync(file))
    : decodeColumnFile(zlib.deflateSync(encodeColumn({ column: world.map.get(`${cx},${cz}`), x: cx, z: cz, t: 0, body: 'Bob', mcVersion: VERSION })))
  return decodeLight(light.buffer, light.meta, SECTIONS)
}

const cellAt = (world, dir, x, y, z) => {
  const cells = dumpedCells(world, dir, x >> 4, z >> 4)
  const v = cells[(y >> 4) * 4096 + ((y & 15) << 8 | (z & 15) << 4 | (x & 15))]
  return { sky: v >> 4, block: v & 15 }
}

// mismatches between the dump and a full relight, over the region without its edge
const mismatches = (world, dir) => {
  const full = fullLight(world)
  const out = []
  for (const [key] of world.map) {
    const [cx, cz] = key.split(',').map(Number)
    const cells = dumpedCells(world, dir, cx, cz)
    for (let y = 1; y < HEIGHT; y++) {
      for (let z = 0; z < 16; z++) {
        for (let x = 0; x < 16; x++) {
          const wx = cx * 16 + x
          const wz = cz * 16 + z
          if (wx === 0 || wz === 0 || wx === REGION - 1 || wz === REGION - 1) continue
          const i = (y * REGION + wz) * REGION + wx
          const got = cells[(y >> 4) * 4096 + ((y & 15) << 8 | z << 4 | x)]
          const want = full.sky[i] << 4 | full.block[i]
          if (got !== want) out.push({ at: [wx, y, wz], got: [got >> 4, got & 15], want: [want >> 4, want & 15] })
        }
      }
    }
  }
  return out
}

// apply block edits as the server would: the column changes, then the bot emits blockUpdate
const applyEdits = (world, bot, edits) => {
  for (const [x, y, z, name] of edits) {
    const oldStateId = world.get(x, y, z)
    const stateId = stateOf(name)
    world.set(x, y, z, stateId)
    bot.emit('blockUpdate', { position: new Vec3(x, y, z), stateId: oldStateId }, { position: new Vec3(x, y, z), stateId })
  }
}

const room = (world, x0, z0, size = 7) => {
  world.fill(x0 - 1, 61, z0 - 1, x0 + size, 66, z0 + size, stateOf('stone'))
  world.fill(x0, 61, z0, x0 + size - 1, 65, z0 + size - 1, AIR)
}

const hill = (world, x0, x1, z0, z1, top) => world.fill(x0, 61, z0, x1, top, z1, stateOf('stone'))

const tunnelSteps = Array.from({ length: 10 }, (_, i) => ({ edits: [[22 + i, 70, 25, 'air'], [22 + i, 71, 25, 'air']] }))

const scenarios = [
  {
    name: 'a torch placed in a dark roofed room lights it, and removing it darkens it again',
    build: world => room(world, 20, 20),
    steps: [
      { edits: [[23, 62, 23, 'torch']], expect: [[23, 62, 23, 0, 14], [22, 62, 23, 0, 13], [23, 62, 25, 0, 12], [19, 62, 23, 0, 0]] },
      { edits: [[23, 62, 23, 'air']], expect: [[23, 62, 23, 0, 0], [22, 62, 23, 0, 0], [24, 63, 24, 0, 0]] }
    ]
  },
  {
    name: 'a hole in the roof lets sky straight down and fall off sideways, and closing it darkens the room',
    build: world => room(world, 20, 20),
    steps: [
      { edits: [[23, 66, 23, 'air']], expect: [[23, 66, 23, 15, 0], [23, 61, 23, 15, 0], [24, 61, 23, 14, 0], [25, 61, 23, 13, 0], [20, 61, 20, 9, 0]] },
      { edits: [[23, 66, 23, 'stone']], expect: [[23, 61, 23, 0, 0], [24, 61, 23, 0, 0]] }
    ]
  },
  {
    name: 'a torch in the open at the border between two columns lights both',
    build: world => room(world, 28, 20, 8),
    steps: [{ edits: [[31, 62, 23, 'torch']], expect: [[31, 62, 23, 0, 14], [32, 62, 23, 0, 13], [30, 62, 23, 0, 13]] }]
  },
  {
    name: 'digging a 1x2 tunnel ten deep into a hillside one block at a time',
    build: world => hill(world, 22, 44, 20, 30, 75),
    steps: tunnelSteps
  }
]

for (const { name, build, steps } of scenarios) {
  test(`relight: ${name} (matches a full recompute after every flush)`, async () => {
    const world = makeWorld()
    build(world)
    lightWorld(world)
    const bot = worldBot(world)
    const { view, dir } = makeView(bot)
    for (const { edits, expect = [] } of steps) {
      applyEdits(world, bot, edits)
      await view.flushColumns()
      await view.idle()
      assert.deepEqual(mismatches(world, dir).slice(0, 5), [])
      for (const [x, y, z, sky, block] of expect) assert.deepEqual(cellAt(world, dir, x, y, z), { sky, block }, `cell ${x},${y},${z}`)
    }
    const stats = view.stats()
    assert.ok(stats.relightBoxes >= 1)
    assert.ok(stats.relightCells > 0)
    assert.equal(stats.relightCarried, 0)
  })
}

test('relight: a change at a column border writes the neighbour column too, and only the columns that changed', async () => {
  const world = makeWorld()
  room(world, 28, 20, 8)
  lightWorld(world)
  const bot = worldBot(world)
  const { view, dir } = makeView(bot)
  applyEdits(world, bot, [[31, 62, 23, 'torch']])
  await view.flushColumns()
  await view.idle()
  assert.deepEqual(fs.readdirSync(worldChunks(dir)).sort(), ['1.1.bin', '2.1.bin'])
})

test('relight: a block update that does not change the state costs nothing', async () => {
  const world = makeWorld()
  room(world, 20, 20)
  lightWorld(world)
  const bot = worldBot(world)
  const { view } = makeView(bot)
  applyEdits(world, bot, [[23, 62, 23, 'air']])
  await view.flushColumns()
  assert.equal(view.stats().relightBoxes, 0)
})

test('relight: chunkColumnLoad and chunkColumnUnload drop the overlay, the server light is written again', async () => {
  const world = makeWorld()
  room(world, 20, 20)
  lightWorld(world)
  const bot = worldBot(world)
  const { view, dir } = makeView(bot)
  applyEdits(world, bot, [[23, 62, 23, 'torch']])
  await view.flushColumns()
  await view.idle()
  assert.equal(cellAt(world, dir, 23, 62, 23).block, 14)
  bot.emit('chunkColumnLoad', new Vec3(16, 0, 16))
  await view.flushColumns()
  await view.idle()
  assert.equal(cellAt(world, dir, 23, 62, 23).block, 0)
  applyEdits(world, bot, [[24, 62, 23, 'torch']])
  await view.flushColumns()
  await view.idle()
  assert.equal(cellAt(world, dir, 24, 62, 23).block, 14)
  bot.emit('chunkColumnUnload', new Vec3(16, 0, 16))
  bot.emit('chunkColumnLoad', new Vec3(16, 0, 16))
  await view.flushColumns()
  await view.idle()
  assert.equal(cellAt(world, dir, 24, 62, 23).block, 0)
  assert.equal(bot.listenerCount('chunkColumnUnload'), 1)
  await view.detach()
  assert.equal(bot.listenerCount('chunkColumnUnload'), 0)
})

test('relight: a new bot attached drops the overlays and queued changes', async () => {
  const world = makeWorld()
  room(world, 20, 20)
  lightWorld(world)
  const bot = worldBot(world)
  const { view, dir } = makeView(bot)
  applyEdits(world, bot, [[23, 62, 23, 'torch']])
  await view.flushColumns()
  await view.idle()
  applyEdits(world, bot, [[24, 62, 23, 'torch']])
  view.attach(worldBot(world))
  await view.flushColumns()
  await view.idle()
  assert.equal(view.stats().relightBoxes, 1)
  bot.emit('chunkColumnLoad', new Vec3(16, 0, 16))
  assert.equal(cellAt(world, dir, 24, 62, 23).block, 0)
})

test('relight: boxes over budget carry their changes to the next flush, and their column waits for its relight', async () => {
  const columns = Object.fromEntries([0, 1, 2, 3, 4, 5, 6, 7].flatMap(cx => [0, 1].map(cz => [`${cx},${cz}`, new ChunkColumn()])))
  const bot = fakeBot({ columns })
  bot.registry = undefined
  const { view, dir } = makeView(bot, { relightBudgetMs: 0 })
  const torch = x => {
    const stone = stateOf('stone')
    columns[`${x >> 4},0`].setBlockStateId({ x: x & 15, y: 64, z: 8 }, stone)
    columns[`${x >> 4},0`].setBlockStateId({ x: x & 15, y: 65, z: 8 }, stateOf('torch'))
    bot.emit('blockUpdate', { position: new Vec3(x, 65, 8), stateId: AIR }, { position: new Vec3(x, 65, 8), stateId: stateOf('torch') })
  }
  torch(8)
  torch(88)
  await view.flushColumns()
  await view.idle()
  assert.equal(fs.existsSync(path.join(worldChunks(dir), '0.0.bin')), true)
  assert.equal(fs.existsSync(path.join(worldChunks(dir), '5.0.bin')), false)
  const first = view.stats()
  assert.equal(first.relightBoxes, 1)
  assert.equal(first.relightCarried, 1)
  await view.flushColumns()
  await view.idle()
  assert.equal(fs.existsSync(path.join(worldChunks(dir), '5.0.bin')), true)
  const second = view.stats()
  assert.equal(second.relightBoxes, 1)
  assert.equal(second.relightCarried, 0)
  const light = decodeColumnFile(fs.readFileSync(path.join(worldChunks(dir), '5.0.bin'))).light
  const cells = decodeLight(light.buffer, light.meta, 24)
  const s = (65 + 64) >> 4
  assert.equal(cells[s * 4096 + (((65 + 64) & 15) << 8 | 8 << 4 | 8)] & 15, 14)
})

test('overlay light in a column file decodes to the overlay, and a column without overlay is unchanged', () => {
  const column = makeColumn()
  const args = { column, x: 0, z: 0, t: 1, body: 'Bob', mcVersion: VERSION }
  const plain = encodeColumn(args)
  assert.deepEqual(encodeColumn({ ...args, overlay: new Map() }), plain)
  assert.deepEqual(encodeColumn({ ...args, overlay: undefined }), plain)
  const section = i => Uint8Array.from({ length: 4096 }, (_, c) => (c * 7 + i) % 16)
  const overlay = new Map([[3, { sky: section(1), block: section(2) }], [0, { sky: section(3), block: section(4) }], [23, { sky: section(5), block: section(6) }]])
  const { light } = decodeColumnFile(zlib.deflateSync(encodeColumn({ ...args, overlay })))
  const cells = decodeLight(light.buffer, light.meta, 24)
  const base = decodeLight(decodeColumnFile(zlib.deflateSync(plain)).light.buffer, decodeColumnFile(zlib.deflateSync(plain)).light.meta, 24)
  for (const [s, { sky, block }] of overlay) {
    assert.deepEqual(Array.from(cells.subarray(s * 4096, (s + 1) * 4096)), Array.from(sky, (v, i) => v << 4 | block[i]))
  }
  assert.deepEqual(Array.from(cells.subarray(4 * 4096, 23 * 4096)), Array.from(base.subarray(4 * 4096, 23 * 4096)))
})

test('the light test helper writes vanilla order: dumpLight and decodeLight give back what was put in', () => {
  const column = new ChunkColumn({ minY: MIN_Y, worldHeight: HEIGHT })
  const sky = Uint8Array.from({ length: SECTIONS * 4096 }, (_, i) => (i * 5 + (i >> 4)) % 16)
  const block = Uint8Array.from({ length: SECTIONS * 4096 }, (_, i) => (i * 3 + (i >> 8)) % 16)
  loadColumnLight(column, sky, block)
  const dumped = column.dumpLight()
  const cells = decodeLight(Buffer.concat([...dumped.skyLight, ...dumped.blockLight].map(b => Buffer.from(b))), {
    skyCount: dumped.skyLight.length, blockCount: dumped.blockLight.length, sectionBytes: 2048,
    skyLightMask: dumped.skyLightMask, blockLightMask: dumped.blockLightMask, emptySkyLightMask: dumped.emptySkyLightMask, emptyBlockLightMask: dumped.emptyBlockLightMask
  }, SECTIONS)
  assert.deepEqual(Array.from(cells), Array.from(sky, (v, i) => v << 4 | block[i]))
  const decoded = decodeColumnLight(dumped, SECTIONS)
  assert.deepEqual(Array.from(decoded.sky), Array.from(sky))
  assert.deepEqual(Array.from(decoded.block), Array.from(block))
})

test('lazy per-section reads equal the dump decode and getBlockStateId', () => {
  const column = new ChunkColumn({ minY: MIN_Y, worldHeight: HEIGHT })
  const sky = Uint8Array.from({ length: SECTIONS * 4096 }, (_, i) => (i * 5 + (i >> 4)) % 16)
  const block = Uint8Array.from({ length: SECTIONS * 4096 }, (_, i) => (i * 3 + (i >> 8)) % 16)
  loadColumnLight(column, sky, block)
  column.setBlockStateId({ x: 3, y: 20, z: 4 }, stateOf('stone'))
  column.setBlockStateId({ x: 4, y: 20, z: 4 }, stateOf('torch'))
  column.setBlockStateId({ x: 5, y: 40, z: 6 }, stateOf('dirt'))
  const decoded = decodeColumnLight(column.dumpLight(), SECTIONS)
  for (let s = 0; s < SECTIONS; s++) {
    const light = columnLightSection(column, s)
    assert.deepEqual(Array.from(light.sky), Array.from(decoded.sky.subarray(s * 4096, (s + 1) * 4096)))
    assert.deepEqual(Array.from(light.block), Array.from(decoded.block.subarray(s * 4096, (s + 1) * 4096)))
    const states = columnStateSection(column, s)
    const expected = Array.from({ length: 4096 }, (_, i) => column.getBlockStateId({ x: i & 15, y: s * 16 + (i >> 8), z: (i >> 4) & 15 }))
    assert.deepEqual(Array.from(states), expected)
  }
})

const box = (x0, x1, extra = {}) => ({ x0, x1, y0: 0, yc0: 0, y1: 10, z0: 0, z1: 10, virtualTop: false, changes: [x0], ...extra })

test('overlapping boxes merge into their bounding box, disjoint ones stay apart', () => {
  const merged = mergeOverlapping([box(0, 10), box(20, 30), box(8, 22)], 100)
  assert.equal(merged.length, 1)
  assert.deepEqual([merged[0].x0, merged[0].x1], [0, 30])
  assert.equal(mergeOverlapping([box(0, 10), box(11, 20)], 100).length, 2)
})

const capCases = [
  ['overlapping boxes within the caps merge', [box(0, 32), box(10, 42)], 1],
  ['overlapping boxes whose union is wider than 48 stay apart', [box(0, 32), box(20, 52)], 2],
  ['overlapping boxes whose union is deeper than 48 on z stay apart', [box(0, 32), box(10, 42, { z0: 20, z1: 52 })], 2],
  ['overlapping boxes whose union is taller than 64 stay apart', [box(0, 32, { y1: 40 }), box(10, 42, { y0: 30, yc0: 30, y1: 70 })], 2],
  ['the downward sky extension does not count towards the height cap', [box(0, 32, { y0: -60, yc0: 0, y1: 40 }), box(10, 42, { y0: 0, yc0: 0, y1: 60 })], 1],
  ['a union over 1.5 times the summed volumes stays apart', [box(0, 30, { z1: 30, y1: 30 }), box(30, 47, { z0: 30, z1: 47, y0: 30, yc0: 30, y1: 47 })], 2]
]

for (const [name, boxes, count] of capCases) {
  test(`merge caps: ${name}`, () => {
    assert.equal(mergeOverlapping(boxes, 1000).length, count)
  })
}

test('merging many overlapping boxes is fast and keeps every change exactly once', () => {
  const boxes = Array.from({ length: 3000 }, (_, i) => box(i % 40, (i % 40) + 32, { z0: i % 17, z1: (i % 17) + 32, changes: [i] }))
  const start = performance.now()
  const merged = mergeOverlapping(boxes, 1000)
  assert.ok(performance.now() - start < 200)
  assert.deepEqual(merged.flatMap(b => b.changes).sort((a, b) => a - b), boxes.map((_, i) => i))
  for (const b of merged) assert.ok(b.x1 - b.x0 + 1 <= 48 && b.z1 - b.z0 + 1 <= 48)
})

test('mergeOverlapping leaves its input boxes unchanged', () => {
  const boxes = [box(0, 10), box(20, 30, { changes: [20, 21] }), box(8, 22, { changes: [8, 9] }), box(100, 110)]
  const before = structuredClone(boxes)
  const merged = mergeOverlapping(boxes, 100)
  assert.ok(merged.length < boxes.length)
  assert.deepEqual(boxes, before)
})

test('a fill-sized burst of one box per cell keeps every change exactly once within the caps', () => {
  const cells = []
  for (let x = 0; x < 81; x++) for (let z = 0; z < 61; z++) for (let y = 0; y < 10; y++) cells.push([x, y, z])
  const boxes = cells.map(([x, y, z], i) => ({ x0: x - 16, x1: x + 16, y0: y - 16, yc0: y - 16, y1: y + 16, z0: z - 16, z1: z + 16, virtualTop: false, changes: [i] }))
  const merged = mergeOverlapping(boxes, 1000)
  const seen = new Uint8Array(cells.length)
  for (const b of merged) for (const c of b.changes) seen[c]++
  assert.equal(merged.reduce((n, b) => n + b.changes.length, 0), cells.length)
  assert.ok(seen.every(n => n === 1))
  for (const b of merged) assert.ok(b.x1 - b.x0 + 1 <= 48 && b.z1 - b.z0 + 1 <= 48 && b.y1 - b.yc0 + 1 <= 64)
})

test('relight: a fill-like burst, a hollow 20x6x20 stone box with a torch inside, relights exactly in capped boxes one flush at a time', async () => {
  const world = makeWorld()
  lightWorld(world)
  const bot = worldBot(world)
  const { view, dir } = makeView(bot, { relightBudgetMs: 0 })
  view.stats()
  const edits = []
  for (let y = 61; y <= 66; y++) {
    for (let z = 14; z <= 33; z++) {
      for (let x = 14; x <= 33; x++) {
        const wall = x === 14 || x === 33 || z === 14 || z === 33 || y === 61 || y === 66
        if (wall) edits.push([x, y, z, 'stone'])
      }
    }
  }
  edits.push([24, 62, 24, 'torch'])
  applyEdits(world, bot, edits)
  const perFlush = []
  for (let i = 0; i < 200; i++) {
    await view.flushColumns()
    await view.idle()
    const s = view.stats()
    perFlush.push({ boxes: s.relightBoxes, cells: s.relightCells })
    if (s.relightCarried === 0) break
  }
  assert.ok(perFlush.length > 1)
  assert.ok(perFlush.length < 200)
  for (const f of perFlush) assert.deepEqual({ boxes: f.boxes, capped: f.cells <= 48 * 48 * 64 }, { boxes: 1, capped: true })
  assert.deepEqual(mismatches(world, dir).slice(0, 5), [])
  assert.deepEqual(cellAt(world, dir, 24, 62, 24), { sky: 0, block: 14 })
  assert.deepEqual(cellAt(world, dir, 20, 63, 20), { sky: 0, block: 5 })
})

test('relight stats: cells are the interior cells relit, and relightMaxMs is the largest single flush', async () => {
  const world = makeWorld()
  room(world, 20, 20)
  lightWorld(world)
  const bot = worldBot(world)
  const { view } = makeView(bot)
  view.stats()
  applyEdits(world, bot, [[23, 62, 23, 'torch']])
  await view.flushColumns()
  applyEdits(world, bot, [[22, 62, 22, 'torch']])
  await view.flushColumns()
  const s = view.stats()
  assert.equal(s.relightCells, 2 * 31 * 31 * 32)
  assert.ok(s.relightMaxMs > 0 && s.relightMaxMs <= s.relightMs)
  assert.ok(s.relightMaxMs >= s.relightMs / 2)
})

test('relight cost: one torch, a mining burst along a tunnel, and changes spread far apart', async () => {
  const report = {}
  const measure = async (label, setup, edits, opts) => {
    for (const run of ['cold', 'warm']) {
      const world = setup()
      lightWorld(world)
      const bot = worldBot(world)
      const { view } = makeView(bot, opts)
      view.stats()
      applyEdits(world, bot, edits)
      await view.flushColumns()
      const s = view.stats()
      const pick = ({ relightMs, relightStatesMs, relightLightMs, relightFloodMs, relightWriteMs, relightBoxes, relightCarried, relightCells }) =>
        ({ ms: relightMs, states: relightStatesMs, light: relightLightMs, flood: relightFloodMs, write: relightWriteMs, boxes: relightBoxes, carried: relightCarried, cells: relightCells })
      report[`${label} ${run}`] = pick(s)
    }
  }
  await measure('one torch', () => { const w = makeWorld(); room(w, 20, 20); return w }, [[23, 62, 23, 'torch']])
  await measure('20 changes along a tunnel', () => { const w = makeWorld(); hill(w, 8, 40, 20, 30, 75); return w },
    Array.from({ length: 20 }, (_, i) => [12 + i, 70, 25, 'air']))
  const wide = () => {
    const w = makeWorld({ columns: Array.from({ length: 52 }, (_, i) => i), rows: [0, 1], ground: -1 })
    return w
  }
  const spread = Array.from({ length: 20 }, (_, i) => [8 + 40 * i, 65, 8, 'torch'])
  const world = wide()
  const columns = Object.fromEntries(world.map)
  const bot = fakeBot({ columns })
  bot.registry = undefined
  const { view } = makeView(bot)
  for (const [x, y, z] of spread) {
    world.set(x, y - 1, z, stateOf('stone'))
    world.set(x, y, z, stateOf('torch'))
    bot.emit('blockUpdate', { position: new Vec3(x, y, z), stateId: AIR }, { position: new Vec3(x, y, z), stateId: stateOf('torch') })
  }
  await view.flushColumns()
  const s = view.stats()
  report['20 spread 40 apart'] = { ms: s.relightMs, states: s.relightStatesMs, light: s.relightLightMs, flood: s.relightFloodMs, write: s.relightWriteMs, boxes: s.relightBoxes, carried: s.relightCarried, cells: s.relightCells }
  console.log('relight cost (flush 1)', JSON.stringify(report, null, 1))
  assert.equal(report['20 changes along a tunnel warm'].boxes, 2)
  assert.equal(report['one torch warm'].boxes, 1)
  assert.equal(report['20 spread 40 apart'].boxes + report['20 spread 40 apart'].carried >= 20, true)
})

// ---- biomes.json ----

const biomesPath = dir => path.join(dir, 'worlds', 'w', 'biomes.json')
const withBiomes = (bot, list) => { bot.registry = { ...bot.registry, biomesArray: list }; return bot }
const entries = [{ id: 1, name: 'plains' }, { id: 0, name: 'badlands', extra: 1 }, { id: 2, name: 'minecraft:desert' }]

test('attach writes the server biome registry, sorted by id, without the namespace', async () => {
  const { view, dir } = makeView(withBiomes(fakeBot(), entries))
  await view.idle()
  assert.deepEqual(readJson(biomesPath(dir)), {
    v: 1, mcVersion: VERSION, biomes: [{ id: 0, name: 'badlands' }, { id: 1, name: 'plains' }, { id: 2, name: 'desert' }]
  })
})

test('a second attach with the same registry does not rewrite biomes.json; a different one does', async () => {
  const { view, dir } = makeView(withBiomes(fakeBot(), entries))
  await view.idle()
  const old = new Date(1000)
  fs.utimesSync(biomesPath(dir), old, old)
  view.attach(withBiomes(fakeBot(), entries))
  await view.idle()
  assert.equal(fs.statSync(biomesPath(dir)).mtimeMs, 1000)
  view.attach(withBiomes(fakeBot(), [...entries, { id: 3, name: 'jungle' }]))
  await view.idle()
  assert.notEqual(fs.statSync(biomesPath(dir)).mtimeMs, 1000)
  assert.equal(readJson(biomesPath(dir)).biomes.length, 4)
})

test('a broken biomes.json is replaced', async () => {
  const dir = tmp()
  fs.mkdirSync(path.dirname(biomesPath(dir)), { recursive: true })
  fs.writeFileSync(biomesPath(dir), '{nope')
  const view = createView({ stateDir: dir, agent: 'Bob', world: 'w', enabled: true })
  view.attach(withBiomes(fakeBot(), entries))
  await view.idle()
  assert.equal(readJson(biomesPath(dir)).biomes.length, 3)
})

test('a bot without a biome registry writes no biomes.json and does not throw', async () => {
  const { view, dir } = makeView(fakeBot())
  await view.idle()
  assert.equal(fs.existsSync(biomesPath(dir)), false)
  const bare = fakeBot()
  delete bare.registry
  view.attach(bare)
  await view.idle()
  assert.equal(fs.existsSync(biomesPath(dir)), false)
})
