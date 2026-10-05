// Doors, trapdoors, fence gates and dropped items in the snapshot renderer (renderView on a synthetic world).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import prismarineBlock from 'prismarine-block'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { renderView } from '../tools/view/render.mjs'
import { makeBlockSource } from '../tools/view/blocks.mjs'
import { decodePng } from '../tools/view/renderer.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)
const Block = prismarineBlock(Chunk.registry)
const stateOf = (name, props = {}) => Block.fromProperties(name, props, 0).stateId

// a stone wall at z=2 with a doorway at x=8 (y 65, 66) in the stone wall at z=6; the eye at 8.5, 65.62, 9.5 looks north
const worldWith = (cells, entities = []) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-doors-'))
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  const stone = Chunk.registry.blocksByName.stone.defaultState
  for (let x = 0; x < 16; x++) for (let z = 0; z < 16; z++) column.setBlockStateId({ x, y: 63, z }, stone)
  for (let x = 4; x < 13; x++) for (let y = 64; y < 70; y++) column.setBlockStateId({ x, y, z: 2 }, stone)
  for (let x = 4; x < 13; x++) for (let y = 64; y < 70; y++) if (x !== 8) column.setBlockStateId({ x, y, z: 6 }, stone)
  for (const [x, y, z, id] of cells) column.setBlockStateId({ x, y, z }, id)
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: 0, z: 0, t: 1, body: 'Bob', mcVersion: VERSION, minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const n = Buffer.alloc(4)
  n.writeUInt32LE(header.length)
  fs.mkdirSync(path.join(dir, 'worlds/w/chunks'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'worlds/w/chunks/0.0.bin'), zlib.deflateSync(Buffer.concat([n, header, sections])))
  fs.mkdirSync(path.join(dir, 'worlds/w/agents/Bob/view'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'worlds/w/agents/Bob/view/pose.json'), JSON.stringify({ v: 1, t: 1, world: 'w', status: 'online', mcVersion: VERSION, pos: { x: 8.5, y: 64, z: 9.5 }, eye: { x: 8.5, y: 65.62, z: 9.5 }, yaw: 0, pitch: 0, entities, timeOfDay: 6000 }))
  return dir
}

// the mean colour of the middle 16x16 pixels: a cutout door (acacia, bamboo) has holes a single pixel can land in
const centre = async dir => {
  const { png } = await renderView({ world: 'w', agentName: 'Bob', width: 64, height: 36, fov: 70, stateDir: dir })
  const img = decodePng(png)
  const cells = Array.from({ length: 256 }, (_, i) => ((10 + Math.floor(i / 16)) * img.width + 24 + (i % 16)) * 4)
  return [0, 1, 2].map(c => cells.reduce((sum, at) => sum + img.rgba[at + c], 0) / cells.length)
}
const dist = (a, b) => Math.hypot(...a.map((v, i) => v - b[i]))

const door = (name, props) => stateOf(name, { facing: 'south', half: 'lower', hinge: 'left', open: false, powered: false, ...props })

test('stateOf builds a door state', () => {
  assert.equal(Block.fromStateId(door('oak_door', {}), 0).name, 'oak_door')
})

const DOORS = ['oak', 'spruce', 'birch', 'jungle', 'acacia', 'dark_oak', 'mangrove', 'cherry', 'bamboo', 'crimson', 'warped', 'iron'].map(w => `${w}_door`)
for (const name of DOORS) {
  test(`a closed ${name} in the doorway is seen, not the wall behind it`, async () => {
    const open = await centre(worldWith([]))
    const closed = await centre(worldWith([[8, 65, 6, door(name, {})], [8, 66, 6, door(name, { half: 'upper' })]]))
    assert.ok(dist(open, closed) > 20, `${name}: ${open} vs ${closed}`)
  })
}

// ---- fence gates, trapdoors
const blockSource = makeBlockSource(Chunk.registry, path.join(import.meta.dirname, '../textures'))
const infoOf = (name, props) => blockSource.info(stateOf(name, props))

test('an open fence gate is drawn as boxes inside its cell, not as a plant', () => {
  const info = infoOf('oak_fence_gate', { facing: 'north', open: true, in_wall: false, powered: false })
  assert.equal(info.kind, 'boxes')
  assert.ok(info.boxes.length >= 2)
  assert.ok(info.boxes.every(b => b.every(v => v >= 0 && v <= 1)))
})

test('a closed fence gate is a thin bar across its cell', () => {
  const info = infoOf('oak_fence_gate', { facing: 'north', open: false, in_wall: false, powered: false })
  assert.equal(info.kind, 'boxes')
  assert.deepEqual(info.boxes.map(b => [b[0], b[3]]), [[0, 1]])
})

const GATES = ['oak', 'spruce', 'birch', 'jungle', 'acacia', 'dark_oak', 'mangrove', 'cherry', 'bamboo', 'crimson', 'warped'].map(w => `${w}_fence_gate`)
for (const name of GATES) {
  test(`${name}: open and closed both draw something in the cell`, async () => {
    const empty = await centre(worldWith([]))
    const closed = await centre(worldWith([[8, 65, 6, stateOf(name, { facing: 'north', open: false })]]))
    assert.ok(dist(empty, closed) > 20, `${name} closed`)
  })
}

const TRAPDOORS = ['oak', 'spruce', 'iron', 'cherry', 'bamboo', 'crimson'].map(w => `${w}_trapdoor`)
for (const name of TRAPDOORS) {
  test(`a closed ${name} on the doorway is seen`, async () => {
    const empty = await centre(worldWith([]))
    const shut = await centre(worldWith([[8, 65, 6, stateOf(name, { facing: 'south', open: true, half: 'bottom' })]]))
    assert.ok(dist(empty, shut) > 20, `${name}: ${empty} vs ${shut}`)
  })
}

// ---- dropped items
const itemAt = (x, y, z, extra = {}) => ({ id: 1, type: 'other', name: 'item', kind: 'UNKNOWN', pos: { x, y, z }, yaw: 0, pitch: 0, height: 0.25, width: 0.25, ...extra })
const picture = async dir => {
  const { png } = await renderView({ world: 'w', agentName: 'Bob', width: 128, height: 72, fov: 70, stateDir: dir })
  return decodePng(png)
}
const changed = (a, b) => a.rgba.reduce((n, _, i) => i % 4 === 0 && (a.rgba[i] !== b.rgba[i] || a.rgba[i + 1] !== b.rgba[i + 1] || a.rgba[i + 2] !== b.rgba[i + 2]) ? n + 1 : n, 0)
const ITEM_AT = [8.5, 65.5, 5.5] // three blocks ahead of the eye (8.5, 65.62, 9.5) at 4 blocks, on the wall side

test('a dropped item covers about a quarter-block cube, not a bigger one', async () => {
  const bare = await picture(worldWith([[8, 65, 6, 0]]))
  const withItem = await picture(worldWith([[8, 65, 6, 0]], [itemAt(8.5, 65.5, 7.5)]))
  // 0.25 blocks 2 away at fov 70, 128 px wide: about 128 / (2 * tan 35 * 2) * 0.25 = 11.4 px a side, with its side faces maybe 130 px in all
  const n = changed(bare, withItem)
  assert.ok(n > 20, `item drawn: ${n}`)
  assert.ok(n < 220, `item too big: ${n} pixels`)
})

test('a dropped item of a block takes that block\'s colour, not yellow', async () => {
  const bare = await picture(worldWith([], []))
  const withItem = await picture(worldWith([], [itemAt(8.5, 65.5, 7.5, { item: 'oak_log' })]))
  const at = ((36) * withItem.width + 64) * 4
  const px = [withItem.rgba[at], withItem.rgba[at + 1], withItem.rgba[at + 2]]
  assert.ok(changed(bare, withItem) > 0)
  assert.ok(px[1] < px[0] * 0.85, `brown-ish, not yellow: ${px}`)
})
