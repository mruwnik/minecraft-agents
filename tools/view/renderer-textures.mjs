// Why JavaScript: graphics/performance; software raycaster pieces.
// Which texture files draw a block face, and the biome tints and flat colours of blocks.


// ---------------------------------------------------------------- textures
const SHAPED = /_(stairs|slab|wall|fence_gate|fence|pressure_plate|button|trapdoor|carpet|pane)$/
const FACE_SUFFIXES = { top: ['_top', ''], bottom: ['_bottom', '_top', ''], side: ['_side', '', '_front'], cross: ['', '_side'] }
// blocks whose picture is filed under another name
const DRAWN_AS = {
  snow_block: 'snow', magma_block: 'magma', bamboo: 'bamboo_stalk', bamboo_sapling: 'bamboo_stage0', dried_kelp_block: 'dried_kelp',
  ender_chest: 'obsidian', chest: 'oak_planks', trapped_chest: 'oak_planks', redstone_wire: 'redstone_dust_line0', fire: 'fire_0',
  soul_fire: 'soul_fire_0', frosted_ice: 'frosted_ice_0', petrified_oak_slab: 'oak_planks', light_weighted_pressure_plate: 'gold_block',
  heavy_weighted_pressure_plate: 'iron_block', piston_head: 'piston', sticky_piston: 'piston', moving_piston: 'piston',
  campfire: 'campfire_log', soul_campfire: 'soul_campfire_log'
}
// chests are entity-rendered: their real picture is an atlas under entity/chest/, not a plain texture under
// block/, so DRAWN_AS above only gives textureCandidates something plausible for the dashboard's inventory icon.
// The world view (render(), below) uses this dedicated colour instead, so a chest is not just a plank cube.
const BLOCK_COLORS = { chest: [162, 112, 63], trapped_chest: [138, 56, 43], ender_chest: [35, 48, 46] }
export const colorOf = block => BLOCK_COLORS[block]
// a wrapper, treatment or variant of a block that shares its picture; tried after the full name, so smooth_stone keeps its own
const plainName = name => name
  .replace(/^(waxed|infested|potted|smooth)_/, '')
  .replace(/^(water|lava|powder_snow)_cauldron$/, 'cauldron')
  .replace(/^(\w+_)?candle_cake$/, 'cake')
  .replace(/_wood$/, '_log')
  .replace(/_hyphae$/, '_stem')

function candidatesFor (name, face, props) {
  const half = props.half === 'upper' ? '_top' : '_bottom'
  const base = name.replace(SHAPED, '')
  const faced = n => FACE_SUFFIXES[face].map(s => `${n}${s}`)
  return [
    ...(props.age !== undefined ? [`${name}_stage${props.age}`] : []),
    ...(props.half === 'upper' || props.half === 'lower' ? [`${name}${half}`] : []),
    ...faced(name),
    ...(base === name ? [] : [...faced(base), `${base}_planks`, `${base}s`, `${base}_wool`, ...faced(`${base}_block`)])
  ]
}

// Texture file names (without .png) worth trying for a block face, best first. The caller picks the first that exists.
export function textureCandidates (block, face, props = {}) {
  if (block === 'water' || block === 'lava') return [`${block}_still`]
  if (block.endsWith('_bed')) return [block.replace(/_bed$/, '_wool')]
  if (block === 'grass_block' && face === 'bottom') return ['dirt']
  const name = DRAWN_AS[block] ?? block.replace('wall_', '')
  return [...new Set([name, plainName(name)].flatMap(n => candidatesFor(n, face, props)))]
}

// grass and leaves ship grey: the game colours them by biome, and this is a temperate one
const GRASS = [124, 189, 107]
const FOLIAGE = [89, 174, 48]
const TINTS = [
  [/^(grass_block_top|short_grass|tall_grass_(top|bottom)|fern|large_fern_(top|bottom))$/, GRASS],
  [/^birch_leaves$/, [128, 167, 85]],
  [/^spruce_leaves$/, [97, 153, 97]],
  [/^(oak|jungle|acacia|dark_oak|mangrove)_leaves$|^vine$|^lily_pad$/, FOLIAGE],
  [/^water_still$/, [63, 118, 228]]
]
export const tintOf = texture => TINTS.find(([re]) => re.test(texture))?.[1]
