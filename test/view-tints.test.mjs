// Tint groups of model faces and the stage-1 group and constant colours.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import prismarineRegistry from 'prismarine-registry'
import { tintRef, tintTable, dryFoliageColor, TINT_GROUPS, CONSTANT_COLORS, COLORMAP_BLOCKS, GRASS, FOLIAGE, DRY_FOLIAGE_FALLBACK } from '../tools/view/tints.mjs'

const table = tintTable({ dry: [1, 2, 3] })
const groupColor = ref => ref.group === 'constant' ? CONSTANT_COLORS[ref.index] : table.groups[ref.group]

const cases = [
  ['grass', 'grass_block', {}, 0, 'grass', GRASS],
  ['pink petals stems use tintindex 1', 'pink_petals', {}, 1, 'grass', GRASS],
  ['foliage', 'oak_leaves', {}, 0, 'foliage', FOLIAGE],
  ['water', 'water_cauldron', {}, 0, 'water', [63, 118, 228]],
  ['dry foliage is the given colour', 'leaf_litter', {}, 0, 'dry_foliage', [1, 2, 3]],
  ['birch leaves are a constant', 'birch_leaves', {}, 0, 'constant', [128, 167, 85]],
  ['lily pad is a constant', 'lily_pad', {}, 0, 'constant', [32, 128, 48]],
  ['redstone by power', 'redstone_wire', { power: 15 }, 0, 'constant', [255, 50, 0]],
  ['redstone unpowered', 'redstone_wire', { power: 0 }, 0, 'constant', [76, 0, 0]]
]
for (const [name, block, props, tintindex, group, rgb] of cases) {
  test(`tint: ${name}`, () => {
    const ref = tintRef(block, props, tintindex)
    assert.equal(ref.group, group)
    assert.deepEqual(groupColor(ref), rgb)
  })
}

for (const [name, block, tintindex] of [['a block the game does not tint', 'cherry_leaves', 0], ['no tintindex', 'grass_block', -1]]) {
  test(`tint: ${name} has none`, () => assert.equal(tintRef(block, {}, tintindex), null))
}

test('group numbers are the shader contract: none, grass, foliage, dry_foliage, water, constant', () => {
  assert.deepEqual(TINT_GROUPS, ['none', 'grass', 'foliage', 'dry_foliage', 'water', 'constant'])
})

test('the constant table fits the shader uniform array', () => assert.ok(CONSTANT_COLORS.length <= 32))

test('the dry foliage colormap is read at the vanilla lookup (50, 173) and falls back without one', () => {
  const rgba = new Uint8Array(256 * 256 * 4)
  rgba.set([9, 8, 7, 255], (173 * 256 + 50) * 4)
  assert.deepEqual(dryFoliageColor({ width: 256, rgba }), [9, 8, 7])
  assert.deepEqual(dryFoliageColor(null), DRY_FOLIAGE_FALLBACK)
})

test('every colormap block exists in the 26.1 registry', () => {
  const { blocksByName } = prismarineRegistry('26.1')
  assert.deepEqual(COLORMAP_BLOCKS.filter(name => !blocksByName[name]), [])
})
