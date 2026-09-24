// The map and the merge, shared by tools/dashboard.mjs and the page it serves: the only import is src/lib.mjs, which
// is pure too (no node builtins), so the dashboard server serves it at /src/lib.mjs and this module loads exactly the
// same way for node (test/dashboard.test.mjs, tools/dashboard.mjs) and for the browser (relative import resolves to
// that same route from either location - see the route table in tools/dashboard/lib.mjs).
import { parsePlan } from '../../src/lib.mjs'
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
