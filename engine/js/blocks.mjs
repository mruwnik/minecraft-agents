// Block names the game replaces when something is placed into their cell (fire, grass, snow layers), so placing
// treats such a cell as free. Shared by primitives.mjs and fake.mjs. `grass` is the pre-1.20.3 name of short_grass.
const REPLACEABLE = new Set(['fire', 'soul_fire', 'short_grass', 'tall_grass', 'grass', 'snow'])

export const isReplaceable = name => REPLACEABLE.has(name)
