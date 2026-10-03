// The tint group of a model face with a `tintindex`, for one block state, and the stage-1 colours the shader multiplies by: a single
// fixed biome (plains) for the colormap groups. minecraft-data's tints.json gives the constant tints by block name, redstone by power
// and water for plains; its plains grass and foliage entries are 0 (the game reads them from the colormap), so those two are the
// vanilla plains colours, as src/vision/renderer.mjs has them. WHICH blocks use which colormap is FROM MEMORY of the game's
// BlockColors and may be stale for 26.1: check it against the game.
import tints from 'minecraft-data/minecraft-data/data/pc/26.1/tints.json' with { type: 'json' }

export const GRASS = [124, 189, 107]
export const FOLIAGE = [89, 174, 48]
export const DRY_FOLIAGE_FALLBACK = [160, 112, 47]
const DRY_TEMPERATURE = 0.8
const DRY_DOWNFALL = 0.4

export const GROUP_BLOCKS = {
  grass: ['grass_block', 'short_grass', 'tall_grass', 'fern', 'large_fern', 'potted_fern', 'sugar_cane', 'bush', 'pink_petals', 'wildflowers'],
  foliage: ['oak_leaves', 'jungle_leaves', 'acacia_leaves', 'dark_oak_leaves', 'mangrove_leaves', 'vine'],
  dry_foliage: ['leaf_litter'],
  water: ['water', 'bubble_column', 'water_cauldron']
}
export const COLORMAP_BLOCKS = Object.values(GROUP_BLOCKS).flat()
// groups whose colour is one fixed biome's, not the block's own
export const APPROXIMATE_GROUPS = Object.keys(GROUP_BLOCKS)

const rgbOf = argb => [(argb >> 16) & 255, (argb >> 8) & 255, argb & 255]
const water = rgbOf(tints.water.data.find(entry => entry.keys.includes('plains')).color)
const groupOf = new Map(Object.entries(GROUP_BLOCKS).flatMap(([group, names]) => names.map(name => [name, group])))

// Tint groups as the shader numbers them. Group 5 (constant) carries an index into the constant-colour table.
export const TINT_GROUPS = ['none', 'grass', 'foliage', 'dry_foliage', 'water', 'constant']
// the constant colours: block names with a fixed tint (tints.json), then redstone wire by power 0..15
const constantEntries = [
  ...tints.constant.data.flatMap(entry => entry.keys.map(key => [key, rgbOf(entry.color)])),
  ...Array.from({ length: 16 }, (_, power) => [`redstone_wire:${power}`, rgbOf(tints.redstone.data.find(entry => entry.keys.includes(power)).color)])
]
export const CONSTANT_COLORS = constantEntries.map(([, rgb]) => rgb)
const constantIndex = new Map(constantEntries.map(([key], i) => [key, i]))

// the dry foliage colormap (256x256 RGBA, as decodePng gives it) at the vanilla lookup for temperature 0.8, downfall 0.4
export const dryFoliageColor = image => {
  if (!image) return DRY_FOLIAGE_FALLBACK
  const x = Math.floor((1 - DRY_TEMPERATURE) * 255)
  const y = Math.floor((1 - DRY_DOWNFALL * DRY_TEMPERATURE) * 255)
  const at = (y * image.width + x) * 4
  return at + 3 < image.rgba.length ? [...image.rgba.subarray(at, at + 3)] : DRY_FOLIAGE_FALLBACK
}

// The stage-1 colour of each biome group (one fixed plains colour) and the constant table, as the page's tint uniforms
export const tintTable = ({ dry = DRY_FOLIAGE_FALLBACK } = {}) => ({
  groups: { none: [255, 255, 255], grass: GRASS, foliage: FOLIAGE, dry_foliage: dry, water, constant: [255, 255, 255] },
  constants: CONSTANT_COLORS
})

// (blockName, props, tintindex) => { group, index } | null: which group a face with this tintindex belongs to, and for 'constant' its table index
export const tintRef = (name, props, tintindex) => {
  if (tintindex < 0) return null
  if (name === 'redstone_wire') return { group: 'constant', index: constantIndex.get(`redstone_wire:${Number(props.power ?? 0)}`) }
  if (constantIndex.has(name)) return { group: 'constant', index: constantIndex.get(name) }
  const group = groupOf.get(name)
  return group ? { group, index: 0 } : null
}
