// The node-side pure helpers behind tools/dashboard.mjs: reading the agent folders, deciding which file a look may
// hand out, and routing a request. The map itself is in ./map.mjs, which the browser loads too.
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

// `look` answers with a path relative to the body's own home; only a file inside that home's snapshots/ is ours to serve.
export const snapshotFile = (home, file) => {
  if (!file) return null
  const dir = path.resolve(home, 'snapshots')
  const full = path.resolve(home, file)
  return full.startsWith(dir + path.sep) ? full : null
}

const LOOK = /^\/api\/look\/([A-Za-z0-9_]{1,32})$/
// map.mjs imports src/lib.mjs, and lib.mjs re-exports src/cli.mjs - both browser-safe, both need serving at the
// same relative path the browser resolves them to. Matching any flat *.mjs name under src/, rather than hardcoding
// lib.mjs alone, means the page's module graph does not go back to silently failing to load whenever another
// agent gives lib.mjs a new sibling import (an import a static route list would miss with no visible error at all).
const SRCLIB = /^\/src\/([A-Za-z0-9_.-]+\.mjs)$/

export const route = url => {
  const { pathname } = new URL(url, 'http://dashboard')
  if (pathname === '/' || pathname === '/index.html') return { kind: 'page' }
  if (pathname === '/api/state') return { kind: 'state' }
  if (pathname === '/map.mjs') return { kind: 'script' }
  const srclib = SRCLIB.exec(pathname)
  if (srclib) return { kind: 'srclib', name: srclib[1] }
  const look = LOOK.exec(pathname)
  if (look) return { kind: 'look', name: look[1] }
  return { kind: 'unknown' }
}
