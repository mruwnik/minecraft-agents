// Block names the game replaces when something is placed into their cell (fire, grass, snow layers), so placing
// treats such a cell as free. Shared by primitives.mjs and engine.fake. `grass` is the pre-1.20.3 name of short_grass.
const REPLACEABLE = new Set(['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow',
  'leaf_litter', 'fern', 'large_fern', 'dead_bush', 'vine', 'glow_lichen', 'hanging_roots'])

export const isReplaceable = name => REPLACEABLE.has(name)

// Blocks a right-click works (opens, toggles, uses) instead of placing against: a click on one is refused as a
// placement unless the body sneaks. Keep in step with `usable` in engine/src/jobs/lib/placement.cljs.
const INTERACTABLE = /^(chest|trapped_chest|ender_chest|barrel|furnace|smoker|blast_furnace|crafting_table|hopper|dispenser|dropper|brewing_stand|enchanting_table|anvil|chipped_anvil|damaged_anvil|grindstone|stonecutter|loom|cartography_table|smithing_table|lectern|bell|beacon|lever|note_block|jukebox|cake|composter|flower_pot|repeater|comparator|daylight_detector|respawn_anchor|crafter)$|_(door|trapdoor|fence_gate|bed|button|shulker_box)$|^shulker_box$/

export const isInteractable = name => INTERACTABLE.test(name)
