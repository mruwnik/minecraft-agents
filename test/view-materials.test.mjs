// The browser's material table: one material index per block state id, colours averaged from the textures.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { materialTable } from '../tools/view/materials.mjs'

const VERSION = '26.1'
const textureDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const registry = prismarineRegistry(VERSION)
const Block = prismarineBlock(registry)
const table = materialTable(VERSION, textureDir)
const materialOf = new Uint16Array(new Uint8Array(Buffer.from(table.materialOf, 'base64')).buffer)

const stateIds = name => {
  const { minStateId, maxStateId } = registry.blocksByName[name]
  return Array.from({ length: maxStateId - minStateId + 1 }, (_, i) => minStateId + i)
}
const materialFor = (name, id = stateIds(name)[0]) => table.materials[materialOf[id]]

test('table shape', () => {
  assert.equal(table.version, VERSION)
  assert.equal(materialOf.length, table.stateCount)
  assert.ok(materialOf.every(i => i < table.materials.length))
  assert.equal(table.materials[0].name, 'air')
})

for (const name of ['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void']) {
  test(`${name} states are material 0`, () => {
    assert.ok(stateIds(name).every(id => materialOf[id] === 0))
  })
}

test('stone is a grey cube', () => {
  const { kind, top } = materialFor('stone')
  const [r, g, b, a] = top
  assert.equal(kind, 'cube')
  assert.equal(a, 255)
  assert.ok(Math.abs(r - g) < 20 && Math.abs(g - b) < 20 && r > 80 && r < 180)
})

test('grass block top is tinted green and differs from the side', () => {
  const { top, side, bottom } = materialFor('grass_block')
  assert.ok(top[1] > top[0] && top[1] > top[2])
  assert.notDeepEqual(side, top)
  assert.notDeepEqual(bottom, top)
})

test('a poppy is a cross with partial alpha', () => {
  const { kind, top } = materialFor('poppy')
  assert.equal(kind, 'cross')
  assert.ok(top[3] > 0 && top[3] < 255)
})

test('water and lava', () => {
  assert.equal(materialFor('water').kind, 'water')
  assert.equal(materialFor('water').top[3], 160)
  assert.equal(materialFor('lava').kind, 'lava')
  assert.equal(materialFor('lava').top[3], 255)
})

test('an oak slab is partial', () => {
  const id = stateIds('oak_slab').find(i => Block.fromStateId(i, 0).getProperties().type === 'bottom')
  assert.equal(materialFor('oak_slab', id).kind, 'partial')
})

test('a missing texture dir falls back to hashed colours, stably', () => {
  const a = materialTable(VERSION, '/nonexistent')
  const b = materialTable(VERSION, '/nonexistent')
  const stone = t => t.materials[new Uint16Array(new Uint8Array(Buffer.from(t.materialOf, 'base64')).buffer)[stateIds('stone')[0]]]
  assert.deepEqual(stone(a).top, stone(b).top)
  assert.equal(stone(a).top.length, 4)
})
