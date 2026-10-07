// The material table when a client jar is present: cubes with six face layers, models with element lists, the cap, the tint layers.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { textureBytes, SIX_FACES, OVER_CAP, ELEMENT_CAP } from '../tools/view/materials.mjs'
import { findClientJar } from '../tools/view/jar-read.mjs'
import { unpackList, TABLE_WIDTH } from '../tools/view/element-table.mjs'
import { modelLayers } from '../tools/view/web/mob-models.mjs'

const VERSION = '26.1'
const textureDir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const jarPath = findClientJar()
const skip = jarPath === null
const registry = prismarineRegistry(VERSION)
const Block = prismarineBlock(registry)

const build = skip ? null : textureBytes(VERSION, textureDir, { jarPath })
const legacy = textureBytes(VERSION, textureDir, { jarPath: null })
const idsOf = built => new Uint16Array(new Uint8Array(Buffer.from(built.table.materialOf, 'base64')).buffer)

const withProps = (name, want) => {
  const { minStateId, maxStateId } = registry.blocksByName[name]
  return Array.from({ length: maxStateId - minStateId + 1 }, (_, i) => minStateId + i).find(id => {
    const props = Block.fromStateId(id, 0).getProperties()
    return Object.entries(want).every(([k, v]) => String(props[k]) === String(v))
  })
}
const materialOf = (built, name, want = {}) => built.table.materials[idsOf(built)[withProps(name, want)]]
const layerName = (built, layer) => built.table.textures.names[layer]

test('without a jar there are no models, no element table and no six-face cubes', () => {
  assert.equal(legacy.table.elements, undefined)
  assert.equal(legacy.elements, null)
  assert.ok(legacy.table.materials.every(m => m.kind !== 'model' && m.tex6 === undefined))
})

test('bubble_column is the water material, with or without a jar', () => {
  assert.equal(materialOf(legacy, 'bubble_column', { drag: true }).kind, 'water')
  assert.ok(materialOf(legacy, 'bubble_column', { drag: true }).tex.some(l => l >= 0))
})

test('a dispenser is a cube with a texture of its own on each face', { skip }, () => {
  const m = materialOf(build, 'dispenser', { facing: 'north', triggered: false })
  assert.equal(m.kind, 'cube')
  assert.ok((m.flags & SIX_FACES) !== 0)
  const names = m.tex6.map(l => layerName(build, l))
  assert.deepEqual([names[0], names[2]], ['furnace_top', 'dispenser_front'])
  assert.notEqual(names[2], names[3])
  assert.equal(materialOf(build, 'dispenser', { facing: 'up', triggered: false }).tex6.map(l => layerName(build, l))[0], 'dispenser_front_vertical')
})

test('stone is a six-face cube of one layer', { skip }, () => {
  const m = materialOf(build, 'stone')
  assert.equal(m.kind, 'cube')
  assert.equal(new Set(m.tex6).size, 1)
})

test('oak stairs are a model of three elements; the same state shares its element list', { skip }, () => {
  const a = materialOf(build, 'oak_stairs', { facing: 'east', half: 'top', shape: 'inner_left', waterlogged: false })
  assert.equal(a.kind, 'model')
  assert.equal(a.elemCount, 3)
  const elements = unpackList({ data: build.elements, listTexels: build.table.elements.listTexels }, [a.elemOffset, a.elemCount])
  assert.deepEqual(elements.map(e => [e.from, e.to]), [[[0, 8, 0], [16, 16, 16]], [[8, 0, 0], [16, 8, 16]], [[0, 0, 0], [8, 8, 8]]])
})

test('no state exceeds the cap, and a model never lists more elements than it', { skip }, () => {
  assert.ok(build.table.materials.filter(m => m.kind === 'model').every(m => m.elemCount <= ELEMENT_CAP))
})

test('tinted faces are untinted layers plus a tint group; the group colours travel in the table', { skip }, () => {
  const grass = materialOf(build, 'grass_block', { snowy: false })
  assert.equal(grass.kind, 'model')
  assert.deepEqual(grass.tint, ['grass'])
  const names = build.table.textures.names
  assert.ok(names.includes('grass_block_top@ffffff') && !names.some(n => !n.startsWith('entity/') && /@(?!ffffff)/.test(n)))
  const elements = unpackList({ data: build.elements, listTexels: build.table.elements.listTexels }, [grass.elemOffset, grass.elemCount])
  assert.deepEqual(elements.map(e => [e.faces.up?.tint, e.faces.north?.tint]), [['grass', 'none'], [undefined, 'grass']])
  assert.deepEqual(build.table.tints.groups.grass, [124, 189, 107])
  assert.equal(build.table.tints.constants.length, 23)
  assert.deepEqual(materialOf(build, 'leaf_litter', { segment_amount: 1, facing: 'north' }).tint, ['dry_foliage'])
})

test('a tinted cube (oak leaves) has a group per face, a constant tint carries its table index', { skip }, () => {
  const leaves = materialOf(build, 'oak_leaves', { distance: 1, persistent: true, waterlogged: false })
  assert.equal(leaves.kind, 'cube')
  assert.deepEqual(leaves.tint6, [2, 2, 2, 2, 2, 2])
  const birch = materialOf(build, 'birch_leaves', { distance: 1, persistent: true, waterlogged: false })
  assert.deepEqual(birch.tint6.map(t => t & 7), [5, 5, 5, 5, 5, 5])
  assert.deepEqual(build.table.tints.constants[birch.tint6[0] >> 3], [128, 167, 85])
})

test('pink petals and wildflowers fit the element cap', { skip }, () => {
  for (const name of ['pink_petals', 'wildflowers', 'redstone_wire', 'iron_bars']) {
    const { minStateId, maxStateId } = registry.blocksByName[name]
    const mats = Array.from({ length: maxStateId - minStateId + 1 }, (_, i) => build.table.materials[idsOf(build)[minStateId + i]]).filter(Boolean)
    assert.ok(mats.every(m => m.kind !== 'box' || (m.flags & OVER_CAP) === 0), name)
  }
  assert.equal(build.table.elements.overCapStates, 0)
})

test('states with elements outside their voxel are noted', { skip }, () => {
  assert.equal(materialOf(build, 'pink_petals', { flower_amount: 2, facing: 'north' }).outside, true)
  assert.equal(materialOf(build, 'stone').outside, undefined)
})

test('a block entity without an entity model keeps the old material, exactly', { skip }, () => {
  const [a, b] = [materialOf(build, 'conduit', { waterlogged: false }), materialOf(legacy, 'conduit', { waterlogged: false })]
  assert.equal(a.entity, undefined)
  const named = (built, m) => ({ ...m, tex: m.tex.map(l => layerName(built, l)) }) // layer numbers differ between builds, names do not
  assert.deepEqual(named(build, a), named(legacy, b))
})

test('block entities drawn from entity models are models marked entity; their layers are sheet regions', { skip }, () => {
  const chest = materialOf(build, 'chest', { facing: 'north', type: 'single', waterlogged: false })
  assert.equal(chest.kind, 'model')
  assert.equal(chest.entity, true)
  assert.equal(chest.elemCount, 3)
  assert.ok(build.table.textures.names.includes('entity/chest/normal#28,0,14,14'))
  const bell = materialOf(build, 'bell', { attachment: 'floor', facing: 'north', powered: false })
  assert.equal(bell.entity, true)
  assert.ok(bell.elemCount > 2)
  assert.equal(materialOf(legacy, 'chest', { facing: 'north', type: 'single', waterlogged: false }).kind, 'box')
})

test('a lectern with a book and the enchanting table add their book to the jar model; a lectern without one is the plain model', { skip }, () => {
  const plain = materialOf(build, 'lectern', { facing: 'north', has_book: false, powered: false })
  const booked = materialOf(build, 'lectern', { facing: 'north', has_book: true, powered: false })
  const table = materialOf(build, 'enchanting_table')
  assert.equal(plain.entity, undefined)
  assert.equal(booked.entity, true)
  assert.ok(booked.elemCount > plain.elemCount)
  assert.equal(table.entity, true)
  assert.equal(table.outside, undefined)
  assert.equal(booked.outside, undefined)
})

test('entity layers stay within the budget and no sheet region is named twice', { skip }, () => {
  const names = build.table.textures.names.filter(n => n.startsWith('entity/'))
  assert.equal(new Set(names).size, names.length)
  const mobs = new Set(modelLayers())
  const blockEntities = names.filter(n => !mobs.has(n))
  assert.ok(blockEntities.length <= 400, `${blockEntities.length} block entity layers`)
})

test('the element table is whole rows and every list fits in it', { skip }, () => {
  const { rows, ids } = build.table.elements
  assert.equal(build.elements.length, rows * TABLE_WIDTH * 4)
  assert.ok(build.table.materials.filter(m => m.kind === 'model').every(m => m.elemOffset + m.elemCount <= ids))
  assert.ok(ids < 65536)
})

test('layers: every layer a material names exists and the layer count fits the info packing', { skip }, () => {
  const count = build.table.textures.names.length
  assert.ok(count < 4095)
  assert.ok(build.table.materials.flatMap(m => [...(m.tex ?? []), ...(m.tex6 ?? [])]).every(l => l < count))
  assert.equal(build.textures.layers, count)
})

test('the texture array holds the mobs\' model faces', { skip }, () => {
  const { names, alias } = build.table.textures
  const known = new Set(names)
  assert.deepEqual(modelLayers().filter(layer => !known.has(layer) && !(layer in alias)), [])
})

test('layers with the same pixels are one layer, the other names alias it', { skip }, () => {
  const { names, alias } = build.table.textures
  const { bytes, layers } = build.textures
  const level0 = i => Buffer.from(bytes.subarray(i * 1024, (i + 1) * 1024)).toString('base64')
  assert.equal(new Set(names.map((_, i) => level0(i))).size, names.length)
  assert.ok(Object.keys(alias).length > 100)
  assert.ok(Object.entries(alias).every(([name, layer]) => !names.includes(name) && layer >= 0 && layer < names.length))
  assert.ok(build.table.materials.flatMap(m => [...(m.tex ?? []), ...(m.tex6 ?? [])]).every(l => l < names.length))
})
