// Why JavaScript: tests live-snapshot.mjs, which stays JS: Mineflayer boundary; lazy copy over Mineflayer's world.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { UNLOADED } from './snapshot.mjs'
import { liveSnapshot } from './live-snapshot.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const ChunkColumn = prismarineChunk(registry)
const id = name => registry.blocksByName[name].defaultState
const SIZE = { minY: -64, height: 384 }

const rig = () => {
  const column = new ChunkColumn()
  column.setBlockStateId(new Vec3(3, 70, 4), id('stone'))
  column.setBlockStateId(new Vec3(5, 200, 6), id('ladder'))
  const asked = []
  const world = { getColumn: (cx, cz) => { asked.push([cx, cz]); return cx === 0 && cz === 0 ? column : undefined } }
  return { snapshot: liveSnapshot(world, SIZE), asked }
}

test('a block reads its state id', () => {
  assert.equal(rig().snapshot.stateAt(3, 70, 4), id('stone'))
})

test('an absent column reads unloaded and hasColumn is false', () => {
  const { snapshot } = rig()
  assert.equal(snapshot.stateAt(100, 70, 4), UNLOADED)
  assert.equal(snapshot.hasColumn(6, 0), false)
})

test('a present column answers hasColumn', () => {
  assert.equal(rig().snapshot.hasColumn(0, 0), true)
})

test('a section is copied once however often it is read', () => {
  const { snapshot, asked } = rig()
  snapshot.stateAt(3, 70, 4)
  snapshot.stateAt(4, 71, 5)
  snapshot.stateAt(0, 64, 0)
  assert.deepEqual(asked, [[0, 0]])
})

test('a missing column is asked for once', () => {
  const { snapshot, asked } = rig()
  snapshot.stateAt(100, 70, 4)
  snapshot.stateAt(101, 71, 4)
  snapshot.hasColumn(6, 0)
  assert.deepEqual(asked, [[6, 0]])
})

test('sectionHas sees a flagged id in a section nothing has read yet', () => {
  const { snapshot } = rig()
  const flags = new Uint8Array(65536)
  flags[id('ladder')] = 1
  assert.equal(snapshot.sectionHas(flags, 0, (200 + 64) >> 4, 0), true)
  assert.equal(snapshot.sectionHas(flags, 0, (70 + 64) >> 4, 0), false)
})

test('sectionHas of an absent column is false', () => {
  assert.equal(rig().snapshot.sectionHas(new Uint8Array(65536), 9, 0, 9), false)
})

test('y outside the range reads unloaded', () => {
  const { snapshot } = rig()
  assert.equal(snapshot.stateAt(3, -65, 4), UNLOADED)
  assert.equal(snapshot.stateAt(3, 320, 4), UNLOADED)
})

test('setState overrides a copied block', () => {
  const { snapshot } = rig()
  snapshot.stateAt(3, 70, 4)
  snapshot.setState(3, 70, 4, id('dirt'))
  assert.equal(snapshot.stateAt(3, 70, 4), id('dirt'))
})

test('forgetColumn reads a column loaded since again, and one unloaded since as unloaded', () => {
  const column = new ChunkColumn()
  column.setBlockStateId(new Vec3(3, 70, 4), id('stone'))
  let present = false
  const world = { getColumn: (cx, cz) => present && cx === 0 && cz === 0 ? column : undefined }
  const snapshot = liveSnapshot(world, SIZE)
  assert.equal(snapshot.hasColumn(0, 0), false)
  present = true
  snapshot.forgetColumn(0, 0)
  assert.equal(snapshot.stateAt(3, 70, 4), id('stone'))
  present = false
  snapshot.forgetColumn(0, 0)
  assert.equal(snapshot.hasColumn(0, 0), false)
  assert.equal(snapshot.stateAt(3, 70, 4), UNLOADED)
})

// the body's memory of the rig's column: world section y 4 (y 64-79) holds (3,70,4) and (4,70,4); no other section
const memoryRig = () => {
  const column = new ChunkColumn()
  column.setBlockStateId(new Vec3(3, 70, 4), id('stone'))
  column.setBlockStateId(new Vec3(4, 70, 4), id('dirt'))
  column.setBlockStateId(new Vec3(5, 70, 4), id('dirt'))
  column.setBlockStateId(new Vec3(3, 10, 4), id('stone'))
  const world = { getColumn: (cx, cz) => cx === 0 && cz === 0 ? column : undefined }
  const memory = new Uint16Array(4096)
  const at = (x, y, z) => ((y & 15) << 8) | ((z & 15) << 4) | (x & 15)
  memory[at(3, 70, 4)] = id('stone') + 1
  memory[at(4, 70, 4)] = id('stone') + 1 // remembered as stone, dirt now: the live id wins
  const asked = []
  const known = (cx, sy, cz) => { asked.push([cx, sy, cz]); return cx === 0 && sy === 4 && cz === 0 ? memory : null }
  const unknown = { id: id('void_air'), known }
  return { snapshot: liveSnapshot(world, { ...SIZE, unknown }), asked, unknown, world }
}

test('with unknown, a cell the body knows reads its live id', () => {
  const { snapshot } = memoryRig()
  assert.equal(snapshot.stateAt(3, 70, 4), id('stone'))
  assert.equal(snapshot.stateAt(4, 70, 4), id('dirt'))
})

test('with unknown, a cell the body never saw in a known section reads the mask id', () => {
  assert.equal(memoryRig().snapshot.stateAt(5, 70, 4), id('void_air'))
})

test('with unknown, a cell in the blocked set reads the blocked id, others stay masked', () => {
  const { unknown, world } = memoryRig()
  unknown.blockedId = id('stone')
  unknown.blocked = new Set(['5,70,4'])
  const masked = liveSnapshot(world, { ...SIZE, unknown })
  assert.equal(masked.stateAt(5, 70, 4), id('stone'))
  assert.equal(masked.stateAt(6, 70, 4), id('void_air'))
})

test('with unknown, a blocked cell the body has seen since reads as it is', () => {
  const { unknown, world } = memoryRig()
  unknown.blockedId = id('stone')
  unknown.blocked = new Set(['4,70,4', '5,70,4'])
  const masked = liveSnapshot(world, { ...SIZE, unknown })
  assert.equal(masked.stateAt(4, 70, 4), id('dirt'))
  assert.equal(masked.stateAt(5, 70, 4), id('stone'))
})

test('with unknown, the memory is asked by world section y (minY offset), not the snapshot section index', () => {
  const { snapshot, asked } = memoryRig()
  snapshot.stateAt(3, 70, 4)
  assert.deepEqual(asked, [[0, 70 >> 4, 0]])
})

test('with unknown, a section with no memory reads the mask id everywhere', () => {
  const { snapshot } = memoryRig()
  assert.equal(snapshot.stateAt(3, 10, 4), id('void_air'))
  assert.equal(snapshot.stateAt(0, 0, 0), id('void_air'))
})

test('with unknown, sectionHas reads the masked copy', () => {
  const { snapshot } = memoryRig()
  const flags = new Uint8Array(65536)
  flags[id('stone')] = 1
  assert.equal(snapshot.sectionHas(flags, 0, (10 + 64) >> 4, 0), false)
  assert.equal(snapshot.sectionHas(flags, 0, (70 + 64) >> 4, 0), true)
})

test('with unknown, an absent column still reads unloaded', () => {
  assert.equal(memoryRig().snapshot.stateAt(100, 70, 4), UNLOADED)
})

test('the snapshot carries its unknown option; none without one', () => {
  const { snapshot, unknown } = memoryRig()
  assert.equal(snapshot.unknown, unknown)
  assert.equal(rig().snapshot.unknown, undefined)
})
