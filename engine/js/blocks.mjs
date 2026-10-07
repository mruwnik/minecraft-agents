// Why JavaScript: predicates called per block by the Mineflayer-side primitives (prim-dig, prim-move) and engine.fake.
import { readFileSync } from 'node:fs'

// Block names the game replaces when something is placed into their cell (fire, grass, snow layers), so placing
// treats such a cell as free. Shared by primitives.mjs and engine.fake. `grass` is the pre-1.20.3 name of short_grass.
const REPLACEABLE = new Set(['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow',
  'leaf_litter', 'fern', 'large_fern', 'dead_bush', 'vine', 'glow_lichen', 'hanging_roots'])

export const isReplaceable = name => REPLACEABLE.has(name)

// Blocks a right-click works (opens, toggles, uses) instead of placing against: a click on one is refused as a
// placement unless the body sneaks. The table is engine/src/jobs/lib/interactable_blocks.txt, shared with `usable` in placement.cljs.
const INTERACTABLE = new RegExp(readFileSync(new URL('../src/jobs/lib/interactable_blocks.txt', import.meta.url), 'utf8').trim())

export const isInteractable = name => INTERACTABLE.test(name)
