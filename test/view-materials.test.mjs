// The browser's material table: one material index per block state id, colours averaged from the textures.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { materialTable, textureBytes, CUTOUT, TRANSLUCENT, AXIS_X, AXIS_Z, EMISSIVE, CULL_SAME } from '../tools/view/materials.mjs'

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


test('a missing texture dir falls back to hashed colours, stably', () => {
  const a = materialTable(VERSION, '/nonexistent')
  const b = materialTable(VERSION, '/nonexistent')
  const stone = t => t.materials[new Uint16Array(new Uint8Array(Buffer.from(t.materialOf, 'base64')).buffer)[stateIds('stone')[0]]]
  assert.deepEqual(stone(a).top, stone(b).top)
  assert.equal(stone(a).top.length, 4)
})

const withProps = (name, want) => stateIds(name).find(id => {
  const props = Block.fromStateId(id, 0).getProperties()
  return Object.entries(want).every(([k, v]) => String(props[k]) === String(v))
})
const mat = (name, want = {}) => materialFor(name, withProps(name, want))
const layers = table.textures.names
const layerName = i => layers[i]

test('textures list is consistent with the materials', () => {
  assert.equal(table.textures.size, 16)
  assert.equal(table.textures.levels, 5)
  assert.equal(new Set(layers).size, layers.length)
  const used = new Set(table.materials.flatMap(m => m.tex))
  used.delete(-1)
  assert.equal(used.size, layers.length)
  assert.ok(table.materials.every(m => m.tex.length === 3 && m.tex.every(i => i >= -1 && i < layers.length)))
})

test('stone is an opaque cube sharing one layer', () => {
  const m = mat('stone')
  assert.equal(m.flags, 0)
  assert.deepEqual(m.box, [0, 0, 0, 16, 16, 16])
  assert.ok(m.tex[0] >= 0 && m.tex[0] === m.tex[1] && m.tex[1] === m.tex[2])
})

test('grass block has three layers, the top tinted', () => {
  const { tex, top } = mat('grass_block', { snowy: false })
  assert.equal(new Set(tex).size, 3)
  assert.ok(tex.every(i => i >= 0))
  assert.equal(layerName(tex[0]), 'grass_block_top')
  assert.ok(top[1] > top[0])
})

const cases = [
  ['oak_log', { axis: 'x' }, { has: AXIS_X, not: AXIS_Z | CUTOUT }],
  ['oak_log', { axis: 'z' }, { has: AXIS_Z, not: AXIS_X }],
  ['oak_log', { axis: 'y' }, { has: 0, not: AXIS_X | AXIS_Z }],
  ['oak_leaves', { persistent: true }, { has: CUTOUT, not: TRANSLUCENT | CULL_SAME }],
  ['glass', {}, { has: CUTOUT | CULL_SAME, not: TRANSLUCENT }],
  ['white_stained_glass', {}, { has: TRANSLUCENT | CULL_SAME, not: CUTOUT }],
  ['water', {}, { has: TRANSLUCENT, not: CUTOUT }],
  ['ice', {}, { has: TRANSLUCENT | CULL_SAME, not: CUTOUT }],
  ['glowstone', {}, { has: EMISSIVE, not: TRANSLUCENT }],
  ['campfire', { lit: true }, { has: EMISSIVE, not: 0 }],
  ['campfire', { lit: false }, { has: 0, not: EMISSIVE }],
  ['furnace', { lit: false }, { has: 0, not: EMISSIVE }],
  ['poppy', {}, { has: CUTOUT, not: TRANSLUCENT }],
  ['short_grass', {}, { has: CUTOUT, not: TRANSLUCENT }]
]
for (const [name, props, { has, not }] of cases) {
  test(`flags of ${name} ${JSON.stringify(props)}`, () => {
    const { flags } = mat(name, props)
    assert.equal(flags & has, has)
    assert.equal(flags & not, 0)
  })
}

test('emit', () => {
  assert.equal(mat('glowstone').emit, 15)
  assert.equal(mat('furnace', { lit: false }).emit, 0)
  assert.equal(mat('campfire', { lit: true }).emit, 15)
  assert.equal(mat('campfire', { lit: false }).emit, 0)
  assert.equal(mat('stone').emit, 0)
})

const shapes = [
  ['oak_slab', { type: 'bottom' }, 'box', [0, 0, 0, 16, 8, 16]],
  ['oak_slab', { type: 'top' }, 'box', [0, 8, 0, 16, 16, 16]],
  ['oak_slab', { type: 'double' }, 'cube', [0, 0, 0, 16, 16, 16]],
  ...[1, 2, 3, 4, 5, 6, 7].map(layers => ['snow', { layers }, 'box', [0, 0, 0, 16, layers * 2, 16]]),
  ['snow', { layers: 8 }, 'cube', [0, 0, 0, 16, 16, 16]],
  ['farmland', { moisture: 0 }, 'box', [0, 0, 0, 16, 15, 16]],
  ['dirt_path', {}, 'box', [0, 0, 0, 16, 15, 16]],
  ['white_carpet', {}, 'box', [0, 0, 0, 16, 1, 16]],
  ['rail', { shape: 'north_south' }, 'box', [0, 0, 0, 16, 1, 16]],
  ['powered_rail', { shape: 'north_south', powered: false }, 'box', [0, 0, 0, 16, 1, 16]],
  ['redstone_wire', {}, 'box', [0, 0, 0, 16, 1, 16]],
  ['stone_pressure_plate', { powered: false }, 'box', [0, 0, 0, 16, 1, 16]],
  ['lily_pad', {}, 'box', [0, 0, 0, 16, 1, 16]],
  ['powder_snow', {}, 'cube', [0, 0, 0, 16, 16, 16]],
  ['soul_sand', {}, 'cube', [0, 0, 0, 16, 16, 16]],
  ['mud', {}, 'cube', [0, 0, 0, 16, 16, 16]],
  ['honey_block', {}, 'cube', [0, 0, 0, 16, 16, 16]],
  ['poppy', {}, 'cross', [0, 0, 0, 16, 16, 16]],
  ['short_grass', {}, 'cross', [0, 0, 0, 16, 16, 16]],
  ['water', {}, 'water', [0, 0, 0, 16, 16, 16]],
  ['lava', {}, 'lava', [0, 0, 0, 16, 16, 16]]
]
for (const [name, props, kind, box] of shapes) {
  test(`${name} ${JSON.stringify(props)} is ${kind} ${box}`, () => {
    const m = mat(name, props)
    assert.equal(m.kind, kind)
    assert.deepEqual(m.box, box)
  })
}

test('materials are distinct per descriptor', () => {
  assert.notEqual(withProps('oak_slab', { type: 'bottom', waterlogged: false }), withProps('oak_slab', { type: 'top', waterlogged: false }))
  assert.notEqual(materialOf[withProps('oak_slab', { type: 'bottom', waterlogged: false })], materialOf[withProps('oak_slab', { type: 'top', waterlogged: false })])
})

test('textureBytes builds the table and bytes together', () => {
  const built = textureBytes(VERSION, textureDir)
  assert.equal(built.textures.layers, built.table.textures.names.length)
  assert.equal(built.textures.bytes.length, [16, 8, 4, 2, 1].reduce((s, n) => s + built.textures.layers * n * n * 4, 0))
})
