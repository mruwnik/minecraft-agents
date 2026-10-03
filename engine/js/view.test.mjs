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
  poseSnapshot, poseKey, hudSnapshot, createView, poseHzFromEnv, coalescedWriter, STATS_MS, POSE_REFRESH_MS
} from './view.mjs'

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

const fakeBot = ({ columns = {}, entities = {} } = {}) => {
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
    world: { getColumn: (x, z) => columns[`${x},${z}`] ?? null }
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
  const file = path.join(dir, 'agents', 'Bob', 'view', 'pose.json')
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
  const file = path.join(dir, 'agents', 'Bob', 'view', 'hud.json')
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
  const file = path.join(dir, 'agents', 'Bob', 'view', 'pose.json')
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
  assert.deepEqual(readJson(path.join(dir, 'agents', 'Bob', 'view', 'pose.json')),
    { v: 1, t: 7, world: 'w', status: 'offline', mcVersion: VERSION })
})

test("the bot's own end event writes the offline pose", async () => {
  const bot = fakeBot()
  const { view, dir } = makeView(bot)
  await view.tickPose()
  bot.emit('end', 'timeout')
  await view.idle()
  assert.equal(readJson(path.join(dir, 'agents', 'Bob', 'view', 'pose.json')).status, 'offline')
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
  assert.deepEqual(view.stats(), { columns: 0, bytes: 0, ms: 0, poses: 0, poseMs: 0, poseBytes: 0, huds: 0 })
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
  assert.equal(s.poseBytes, fs.statSync(path.join(dir, 'agents', 'Bob', 'view', 'pose.json')).size)
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
