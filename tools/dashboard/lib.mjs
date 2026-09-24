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
  if (pathname === '/api/chat') return { kind: 'chat' }
  if (pathname === '/map.mjs') return { kind: 'script' }
  const srclib = SRCLIB.exec(pathname)
  if (srclib) return { kind: 'srclib', name: srclib[1] }
  const look = LOOK.exec(pathname)
  if (look) return { kind: 'look', name: look[1] }
  return { kind: 'unknown' }
}

// ---------------------------------------------------------------- the chat log
// events.jsonl is one JSON object per line. The server reads only the tail of each file, so `torn` says the first
// line may start mid-object: it is dropped rather than parsed (it would fail to parse anyway, but a torn line that
// happens to be valid JSON - a bare number, say - must not slip in as an event).
export const parseEventLines = (text, torn = false) => text
  .split('\n')
  .slice(torn ? 1 : 0)
  .map(parseConfig)
  .filter(e => e && typeof e === 'object')

const TALK = new Set(['chat', 'whisper'])
const isTalk = e => e && typeof e === 'object' && TALK.has(e.type) && typeof e.t === 'string' && typeof e.from === 'string'

// A body logs what OTHERS say: the same chat sits in every online body's file (one seq each), while a whisper is only
// in its recipient's file, so the folder that holds a whisper is who it was for. That makes the identity of a line
// from+message+to: a chat is deduplicated across the files, a whisper to two bodies is two lines. Each body stamps a
// line with its own clock, so the copies differ by a few ms: two lines with the same identity within SAME_LINE_MS
// are one line (kept at its earliest stamp), and the same words said again later are another.
const SAME_LINE_MS = 2000
const identity = m => `${m.from}\u0000${m.to ?? ''}\u0000${m.message}`
const talkLine = (e, agent) => ({ t: e.t, from: e.from, to: e.type === 'whisper' ? agent : null, kind: e.type, message: String(e.message ?? '') })
const byTime = (a, b) => a.t.localeCompare(b.t) || (a.to ?? '').localeCompare(b.to ?? '')

export const mergeChat = (perAgent, limit) => {
  const lastSeen = new Map()
  const lines = perAgent.flatMap(({ agent, lines }) => lines.filter(isTalk).map(e => talkLine(e, agent))).sort(byTime)
  return lines.filter(m => {
    const key = identity(m)
    const at = Date.parse(m.t)
    const before = lastSeen.get(key)
    lastSeen.set(key, at)
    return !(before !== undefined && at - before <= SAME_LINE_MS)
  }).slice(-limit)
}

export const CHAT_LIMIT = 200
export const CHAT_CAP = 1000
export const chatLimit = raw => {
  const n = Number(raw)
  if (!Number.isInteger(n) || n <= 0) return CHAT_LIMIT
  return Math.min(n, CHAT_CAP)
}
