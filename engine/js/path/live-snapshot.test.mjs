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
