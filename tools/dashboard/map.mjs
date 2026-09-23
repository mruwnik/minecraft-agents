// The map and the merge, shared by tools/dashboard.mjs and the page it serves: no node imports, so the browser
// loads this module as-is and draws with the same code test/dashboard.test.mjs checks.

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
