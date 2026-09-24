// The map and the merge, shared by tools/dashboard.mjs and the page it serves: the only import is src/lib.mjs, which
// is pure too (no node builtins), so the dashboard server serves it at /src/lib.mjs and this module loads exactly the
// same way for node (test/dashboard.test.mjs, tools/dashboard.mjs) and for the browser (relative import resolves to
// that same route from either location - see the route table in tools/dashboard/lib.mjs).
import { parsePlan, PLAN_LEGEND } from '../../src/lib.mjs'
export { parsePlan }

// polls: { <agent name>: { ok, state, error, at } }. A port that does not answer is a body that is down, which is
// the ordinary state of most folders here, not a failure of the dashboard.
export const mergeBodies = (agents, polls) => agents.map(agent => {
  const poll = polls[agent.name]
  if (!poll) return { ...agent, up: false, error: 'not polled yet', state: null, at: null }
  if (!poll.ok) return { ...agent, up: false, error: poll.error ?? 'no answer', state: null, at: poll.at ?? null }
  return { ...agent, up: true, error: null, state: poll.state, at: poll.at ?? null }
})

const seenAt = (body, who) => {
  const p = body.up ? body.state?.players?.[who] : null
  if (!p || typeof p !== 'object') return null
  return { x: p.x, y: p.y, z: p.z, seenBy: body.name, at: body.at ?? 0 }
}

// Dan shows on the map only while some body can see him; `state` says 'out of sight' for a player it cannot.
export const danSighting = (bodies, who) => bodies
  .map(b => seenAt(b, who))
  .filter(Boolean)
  .sort((a, b) => b.at - a.at)[0] ?? null

export const mapPoints = (bodies, places, zones, dan) => [
  ...bodies.filter(b => b.up && b.state?.pos).map(b => ({ x: b.state.pos.x, z: b.state.pos.z })),
  ...places.map(p => ({ x: p.x, z: p.z })),
  ...zones.flatMap(z => [{ x: z.x1, z: z.z1 }, { x: z.x2, z: z.z2 }]),
  ...(dan ? [{ x: dan.x, z: dan.z }] : [])
]

export const worldBounds = (points, pad = 16) => {
  if (!points.length) return null
  const xs = points.map(p => p.x)
  const zs = points.map(p => p.z)
  return { minX: Math.min(...xs) - pad, maxX: Math.max(...xs) + pad, minZ: Math.min(...zs) - pad, maxZ: Math.max(...zs) + pad }
}

// One scale for both axes (a squashed map lies about distances); the shorter side is centred in the canvas.
export const fitView = (bounds, width, height) => {
  if (!bounds) return null
  const spanX = Math.max(bounds.maxX - bounds.minX, 1)
  const spanZ = Math.max(bounds.maxZ - bounds.minZ, 1)
  const scale = Math.min(width / spanX, height / spanZ)
  return {
    scale,
    originX: bounds.minX - (width / scale - spanX) / 2,
    originZ: bounds.minZ - (height / scale - spanZ) / 2
  }
}

export const project = (view, x, z) => ({ px: (x - view.originX) * view.scale, py: (z - view.originZ) * view.scale })

export const zoneRect = (view, zone) => {
  const a = project(view, Math.min(zone.x1, zone.x2), Math.min(zone.z1, zone.z2))
  const b = project(view, Math.max(zone.x1, zone.x2), Math.max(zone.z1, zone.z2))
  return { px: a.px, py: a.py, w: b.px - a.px, h: b.py - a.py }
}

const overlaps = (a, b) => a.px < b.px + b.w && b.px < a.px + a.w && a.py < b.py + b.h && b.py < a.py + a.h

// Sixty marked places round three huts is a wall of text: keep a label only where nothing already kept stands.
// Greedy and in the order given, so whatever matters most (the bodies) is offered first and always wins its space.
export const fitLabels = boxes => boxes.reduce((kept, box) => kept.some(k => overlaps(k, box)) ? kept : [...kept, box], [])

// a mark well outside the canvas is neither drawn nor allowed to reserve label space
export const onCanvas = ({ px, py }, width, height, margin = 0) =>
  px >= -margin && px <= width + margin && py >= -margin && py <= height + margin

// ---------------------------------------------------------------- farm and pen footprints
// A place with a plan is not a point, it is a rectangle: the plan is anchored at its own north-west corner
// (x,z), rows run south, columns east - exactly what parsePlan already knows how to size.
export const planRects = places => places.flatMap(place => {
  if (!place.plan) return []
  const parsed = parsePlan(place.plan)
  if (parsed.error) return []
  return [{ name: place.name, x: place.x, z: place.z, w: parsed.width, h: parsed.height }]
})

// crops get their own colour so two fields are told apart at a glance; everything else is by kind. One table for
// both colour and label keeps them from drifting apart - a plan's legend (below the grid) is read straight off it.
const CELL_LEGEND = {
  w: { colour: '#d9b25f', label: 'wheat' },
  c: { colour: '#e08b3d', label: 'carrots' },
  p: { colour: '#c9a15f', label: 'potatoes' },
  b: { colour: '#b3435f', label: 'beetroots' },
  s: { colour: '#c9c96a', label: 'sugar cane' },
  m: { colour: '#7fae3a', label: 'melon' },
  k: { colour: '#d67f2e', label: 'pumpkin' },
  B: { colour: '#5c8f4a', label: 'bamboo' },
  '~': { colour: '#4a90d9', label: 'water' },
  '.': { colour: '#6b6558', label: 'path' },
  '#': { colour: '#8b7355', label: 'fence' },
  G: { colour: '#a8895f', label: 'gate' },
  T: { colour: '#e0a030', label: 'torch' },
  C: { colour: '#a0754a', label: 'chest' },
  K: { colour: '#7a5c3a', label: 'composter' },
  F: { colour: '#e0e060', label: 'flower' },
  t: { colour: '#5c8f4a', label: 'sapling' },
  A: { colour: '#a0754a', label: 'crafting table' }
}
const UNKNOWN_CELL = { colour: '#555f6e', label: 'unknown' }

// a character the legend does not know (a stray space, a typo) is drawn grey and labelled as such, not guessed at
export const cellColour = ch => (CELL_LEGEND[ch] ?? UNKNOWN_CELL).colour
export const cellLabel = ch => (CELL_LEGEND[ch] ?? UNKNOWN_CELL).label

// which footprint, if any, a world x,z lands in - the north-west corner is inside, the far edge (x+w, z+h) is not
export const hitPlan = (rects, x, z) =>
  rects.find(r => x >= r.x && x < r.x + r.w && z >= r.z && z < r.z + r.h)?.name ?? null

// ---------------------------------------------------------------- the plan against the world
// A plan's y is the GROUND block; what it puts there (crop, fence, chest, water cover) stands at y+1, and a water
// source lies AT y - as a source block, or as a waterlogged slab laid into it. The world arrives as scan cells
// {x,y,z,name} (plus `waterlogged` where the server asked block_at), and a cell no body had loaded is 'unloaded'.
const isAir = name => /^(air|cave_air|void_air)$/.test(String(name))
const isFlower = name => /^(dandelion|poppy|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|torchflower|wither_rose|[a-z]+_tulip|pink_petals|wildflowers)$/.test(String(name))
// what a body can stand in: air, and the grass, ferns and flowers that grow on open ground (never grass_block, the ground itself)
const isOpen = name => isAir(name) || /^(short_grass|tall_grass|fern|large_fern|dead_bush|snow|leaf_litter|wildflowers|short_dry_grass|tall_dry_grass|bush|firefly_bush)$/.test(String(name)) || isFlower(name)
const isSolid = name => Boolean(name) && !isOpen(name) && !/^(water|lava|unloaded)$/.test(name)
const isTree = name => /_sapling$|_log$|_wood$|_leaves$|^bamboo/.test(String(name))

// true, false, or null when it cannot be told: a slab in a water cell is right only if it is waterlogged, which a
// scan does not say. The server settles those with block_at; the ones it could not remain unsure, not wrong.
const water = ground => {
  if (ground.name === 'water') return true
  if (ground.waterlogged !== undefined) return ground.waterlogged === true
  return isAir(ground.name) ? false : null
}
const JUDGES = {
  crop: (spec, ground, top) => top.name === spec.crop,
  water: (spec, ground) => water(ground),
  path: (spec, ground, top) => isSolid(ground.name) && isOpen(top.name),
  fence: (spec, ground, top) => /_fence$/.test(top.name),
  gate: (spec, ground, top) => /_fence_gate$/.test(top.name),
  torch: (spec, ground, top) => /_fence$|torch$/.test(top.name),
  chest: (spec, ground, top) => /chest$|barrel$/.test(top.name),
  composter: (spec, ground, top) => top.name === 'composter',
  flower: (spec, ground, top) => isFlower(top.name),
  sapling: (spec, ground, top) => isTree(top.name),
  table: (spec, ground, top) => top.name === 'crafting_table'
}

const EXPECTATIONS = {
  water: 'water, or a waterlogged cover',
  path: 'open, on solid ground',
  fence: 'a fence',
  gate: 'a fence gate',
  torch: 'a fence post with a torch',
  chest: 'a chest',
  composter: 'a composter',
  flower: 'a flower',
  sapling: 'a sapling, or the tree it grew into',
  table: 'a crafting table'
}
export const cellExpectation = ch => {
  const spec = PLAN_LEGEND[ch]
  if (!spec) return 'not in the legend'
  if (spec.kind === 'crop') return `${cellLabel(ch)} on ${spec.ground}`
  return EXPECTATIONS[spec.kind] ?? spec.kind
}

const seenBlock = cell => Boolean(cell) && cell.name !== 'unloaded'

export const planDiff = (place, worldCells) => {
  const world = new Map(worldCells.map(c => [`${c.x},${c.y},${c.z}`, c]))
  const at = (x, y, z) => world.get(`${x},${y},${z}`)
  const cells = (parsePlan(place.plan).cells ?? []).map(({ dx, dz, ch }) => {
    const x = place.x + dx
    const z = place.z + dz
    const ground = at(x, place.y, z)
    const top = at(x, place.y + 1, z)
    const seen = seenBlock(ground) && seenBlock(top)
    const judge = JUDGES[PLAN_LEGEND[ch]?.kind]
    const ok = seen && judge ? judge(PLAN_LEGEND[ch], ground, top) : null
    return { dx, dz, x, z, ch, expected: cellExpectation(ch), ground: ground?.name ?? 'unloaded', top: top?.name ?? 'unloaded', ok, seen }
  })
  return {
    cells,
    total: cells.length,
    differ: cells.filter(c => c.ok === false).length,
    unseen: cells.filter(c => !c.seen).length,
    unsure: cells.filter(c => c.seen && c.ok === null).length
  }
}

// what a cell looks like from above: the thing standing on it, or the ground when nothing does
export const worldLabel = ({ ground, top }) => {
  if (!seenBlock({ name: ground }) || !seenBlock({ name: top })) return 'not loaded'
  return isAir(top) ? ground : `${top} on ${ground}`
}

const WORLD_COLOURS = {
  air: '#14171c', unloaded: '#2a2f38', water: '#4a90d9', lava: '#e0641e',
  grass_block: '#5c8f4a', dirt: '#6b5236', coarse_dirt: '#5e4a34', farmland: '#4e3a26', mud: '#4a3f3a', podzol: '#5a4a2a',
  sand: '#d8cf9a', red_sand: '#c2743a', gravel: '#8c8a86', clay: '#9aa1ab', stone: '#7a7f88', cobblestone: '#8a8a8a',
  sandstone: '#d6c99a', snow: '#e8ecf0', snow_block: '#e8ecf0', ice: '#9fd3ff',
  oak_slab: '#a9824e', oak_planks: '#b08a55', oak_log: '#6b4f2a', oak_leaves: '#3f7a2f', oak_sapling: '#5c8f4a',
  oak_fence: '#8b7355', oak_fence_gate: '#a8895f', spruce_fence: '#6b4f33', torch: '#e0a030', wall_torch: '#e0a030',
  chest: '#a0754a', composter: '#7a5c3a', crafting_table: '#a0754a',
  wheat: '#d9b25f', carrots: '#e08b3d', potatoes: '#c9a15f', beetroots: '#b3435f', sugar_cane: '#c9c96a',
  melon_stem: '#7fae3a', pumpkin_stem: '#d67f2e', bamboo: '#5c8f4a', short_grass: '#6fa04f', tall_grass: '#6fa04f', fern: '#5f9a48',
  dandelion: '#e0e060', poppy: '#d64545'
}
const hashName = name => [...String(name)].reduce((h, ch) => (h * 31 + ch.charCodeAt(0)) >>> 0, 7)
// a block the table does not know still gets a colour of its own, the same every time, muted so it never outshines a known one
export const worldColour = name => WORLD_COLOURS[name] ?? `hsl(${hashName(name) % 360} 30% 45%)`
