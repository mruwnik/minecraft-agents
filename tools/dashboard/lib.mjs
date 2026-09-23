// Pure helpers behind tools/dashboard.mjs: reading agent folders, merging polls, and the top-down map projection.
// Nothing here touches the network or the disk, so test/dashboard.test.mjs drives all of it from tables.
import path from 'node:path'

const parseConfig = text => {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

const describeCharacter = c => c?.name ? `${c.name}${c.source ? ` (${c.source})` : ''}` : null

// entries: [{ name: <folder name>, text: <raw config.json> }]. A folder we cannot read, or one with no apiPort,
// is not an agent we can poll: leave it out rather than show a row that can never come up.
export const parseAgents = entries => entries
  .map(({ name, text }) => ({ name, cfg: parseConfig(text) }))
  .filter(({ cfg }) => Number.isFinite(cfg?.apiPort))
  .map(({ name, cfg }) => ({
    name,
    username: cfg.username ?? name,
    apiPort: cfg.apiPort,
    harness: cfg.harness ?? null,
    character: describeCharacter(cfg.character)
  }))
  .sort((a, b) => a.name.localeCompare(b.name))

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

// `look` answers with a path relative to the body's own home; only a file inside that home's snapshots/ is ours to serve.
export const snapshotFile = (home, file) => {
  if (!file) return null
  const dir = path.resolve(home, 'snapshots')
  const full = path.resolve(home, file)
  return full.startsWith(dir + path.sep) ? full : null
}

const LOOK = /^\/api\/look\/([A-Za-z0-9_]{1,32})$/

export const route = url => {
  const { pathname } = new URL(url, 'http://dashboard')
  if (pathname === '/' || pathname === '/index.html') return { kind: 'page' }
  if (pathname === '/api/state') return { kind: 'state' }
  const look = LOOK.exec(pathname)
  if (look) return { kind: 'look', name: look[1] }
  return { kind: 'unknown' }
}
