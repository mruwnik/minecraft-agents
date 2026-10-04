import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { EventEmitter } from 'node:events'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { Vec3 } from 'vec3'
import { makeEyes, lookKey } from '../src/vision/eyes.mjs'

// ---------------------------------------------------------------- eyes
const registry = prismarineRegistry('26.1')
const STONE = registry.blocksByName.stone.defaultState
const Chunk = prismarineChunk(registry)
// a body standing on a stone floor (y 63) looking north, with `reads` counting the chunk columns it copies
const standingBot = () => {
  const columns = new Map()
  const column = (cx, cz) => {
    const key = `${cx},${cz}`
    if (columns.has(key)) return columns.get(key)
    const c = new Chunk({ minY: -64, worldHeight: 384 })
    for (let x = 0; x < 16; x++) for (let z = 0; z < 16; z++) c.setBlockStateId(new Vec3(x, 63, z), STONE)
    columns.set(key, c)
    return c
  }
  const bot = Object.assign(new EventEmitter(), {
    registry,
    reads: 0,
    game: { minY: -64, height: 384 },
    entity: { position: new Vec3(0.5, 64, 0.5), eyeHeight: 1.62, yaw: 0, pitch: 0 },
    entities: {},
    time: { timeOfDay: 6000 },
    column
  })
  bot.world = { getColumn: (cx, cz) => { bot.reads++; return column(cx, cz) } }
  return bot
}
// a stone wall across the view, one and a half blocks north of the body at (x, 64, 0.5); without an event, as the
// chunk data changes under a body that has not been told yet
const putWall = (bot, x) => [...Array(9).keys()].flatMap(dx => [64, 65, 66, 67].map(y => new Vec3(x + dx - 4, y, -2))).forEach(p => {
  bot.column(p.x >> 4, p.z >> 4).setBlockStateId(new Vec3(p.x & 15, p.y, p.z & 15), STONE)
  bot.wallCells = [...(bot.wallCells ?? []), p]
})
const eyesFor = bot => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'eyes-'))
  return makeEyes(bot, { textureDir: dir, snapshotDir: dir })
}

const lookKeyBase = { eye: { x: 0.5, y: 65.62, z: 0.5 }, yaw: 0.1, pitch: 0, timeOfDay: 6000, entities: [{ name: 'cow', x: 3, y: 65, z: 2 }], world: '1:0', width: 320, height: 180, maxDist: 64, panorama: false, fov: 100 }
for (const [name, change, same] of [
  ['the same scene', {}, true],
  ['a step of under a sixteenth of a block', { eye: { x: 0.52, y: 65.62, z: 0.5 } }, true],
  // 0.004 from the brief lands exactly on this base yaw's rounding boundary (11 vs 12) and flips buckets by
  // floating-point luck; 0.0003 keeps the same "well under half a degree" intent without the boundary coincidence
  ['a turn of under half a degree', { yaw: 0.1 + 0.0003 }, true],
  ['a step of a block', { eye: { x: 1.5, y: 65.62, z: 0.5 } }, false],
  ['a turn', { yaw: 0.3 }, false],
  ['a hundred ticks later', { timeOfDay: 6100 }, false],
  ['an entity that moved a block', { entities: [{ name: 'cow', x: 4, y: 65, z: 2 }] }, false],
  ['an entity that turned a degree', { entities: [{ name: 'cow', x: 3, y: 65, z: 2, yaw: 0.017 }] }, true],
  ['an entity that turned round', { entities: [{ name: 'cow', x: 3, y: 65, z: 2, yaw: Math.PI }] }, false],
  ['a block changed', { world: '1:1' }, false],
  ['another size', { width: 480, height: 270 }, false],
  ['a panorama', { panorama: true }, false]
]) test(`lookKey: ${name} ${same ? 'draws nothing new' : 'is a new picture'}`, () => assert.equal(lookKey({ ...lookKeyBase, ...change }) === lookKey(lookKeyBase), same))

const freshCases = [
  ['a block update', bot => { putWall(bot, 0); bot.wallCells.forEach(position => bot.emit('blockUpdate', null, { position, stateId: STONE })) }],
  ['a chunk that arrives', bot => { putWall(bot, 0); bot.emit('chunkColumnLoad', new Vec3(0, 0, -16)) }],
  ['walking off', bot => { putWall(bot, 30); bot.entity.position.x = 30.5 }]
]
for (const [name, change] of freshCases) {
  test(`look: a wall put up since the last look is seen, after ${name}`, async () => {
    const bot = standingBot()
    const look = eyesFor(bot)
    const before = await look({ file: 'a.png' })
    change(bot)
    const after = await look({ file: 'b.png' })
    assert.deepEqual([before.blocked, Boolean(after.blocked)], [null, true])
  })
}

test('look: reports the body\'s current position as floored block coordinates', async () => {
  const bot = standingBot()
  const look = eyesFor(bot)
  // 0.5 floors to 0 on every axis; Math.round would give 1, so this pins down which one the dashboard gets
  const { at } = await look({ file: 'a.png' })
  assert.deepEqual(at, { x: 0, y: 64, z: 0 })
})

test('look: a body that has not moved does not copy the world again', async () => {
  const bot = standingBot()
  const look = eyesFor(bot)
  await look({ file: 'a.png' })
  const copied = bot.reads
  await look({ file: 'b.png' })
  assert.deepEqual([copied > 0, bot.reads], [true, copied])
})

test("look: the body's own thread runs on while the picture is drawn", async () => {
  const bot = standingBot()
  const look = eyesFor(bot)
  await look({ file: 'a.png' })
  let ticks = 0
  const timer = setInterval(() => ticks++, 1)
  // a changed scene so this look is actually drawn, not answered from the cache under test below
  bot.time.timeOfDay = 6100
  await look({ file: 'b.png' })
  clearInterval(timer)
  assert.ok(ticks > 0)
})

test('look: an unchanged scene answers the last frame without asking the worker to draw again', async () => {
  const bot = standingBot()
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'eyes-'))
  const look = makeEyes(bot, { textureDir: dir, snapshotDir: dir })
  const first = await look({ file: 'a.png' })
  const second = await look({ file: 'b.png' })
  // a cache hit still writes the file (the dashboard reads it from disk), with the bytes the worker drew the first time
  assert.deepEqual(fs.readFileSync(path.join(dir, 'a.png')), fs.readFileSync(path.join(dir, 'b.png')))
  assert.deepEqual([second.view, second.seen], [first.view, first.seen])
  // answering from the cache skips the worker round trip entirely: the draw count stays at the first, real draw
  assert.equal(look.draws, 1)
})

// a cow without a size of its own, three blocks in front of the body and facing away from it
const withCow = () => Object.assign(standingBot(), { entities: { 7: { name: 'cow', type: 'animal', kind: 'Passive mobs', position: new Vec3(0.5, 64, -2.5), yaw: 0 } } })
test('look: marks=true outlines each seen entity in fractions of the picture, the seen strings as they were', async () => {
  const look = eyesFor(withCow())
  const plain = await look({ file: 'a.png' })
  const marked = await look({ file: 'b.png', marks: true })
  const [mark] = marked.marks
  assert.deepEqual([plain.marks, marked.seen, mark.name, mark.kind, mark.dist], [undefined, plain.seen, 'cow', 'animal', Number(plain.seen[0].match(/ (\d+)m @/)[1])])
  assert.ok(mark.box.every(v => v >= 0 && v <= 1) && mark.box[0] < mark.box[2] && mark.box[1] < mark.box[3], String(mark.box))
})

test("look: an entity without a size takes its registry's, so a cow is not drawn a player's height", async () => {
  const { marks: [{ box }] } = await eyesFor(withCow())({ file: 'a.png', marks: true })
  // 1.4 high, its top stays below the eye; at the 1.8 default it would reach above the middle of the picture
  assert.ok(box[1] > 0.5, String(box))
})
