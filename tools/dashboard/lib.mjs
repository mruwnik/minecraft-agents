import { representativeAssignments } from '../../src/blueprint/palette.mjs'
// The node-side pure helpers behind tools/dashboard.mjs: reading the agent folders, deciding which file a look may
// hand out, and routing a request. The map itself is in ./map.mjs, which the browser loads too.
import path from 'node:path'
import prismarineBlock from 'prismarine-block'
import minecraftData from 'minecraft-data'
import { parsePlan, planSpec, parsePlacePlan, planCells, hasPlan } from '../../src/lib/plan.mjs'
import { blockIcon, textureCandidates } from '../../src/vision/renderer.mjs'
import { compileBlueprintStructure, concreteBlueprint } from '../../src/blueprint/compiler.mjs'
import { allocateBlueprintMaterials } from '../../src/blueprint/materials.mjs'
import { readBlueprintManifest } from '../../src/blueprint/manifest.mjs'
import { bill, lint, counts, parseNote } from '../../src/blueprint/format.mjs'

const PreviewBlock = prismarineBlock('26.1')
const previewRegistry = minecraftData('26.1')
// Registry shapes keep slabs, stairs, beds and gates at their actual dimensions.
// Non-colliding decorative blocks need a small visible model of their own.
export const previewCells = bp => bp.layers.flatMap(layer => layer.grid.flatMap((row, z) => [...row].flatMap((token, x) => {
  const alt = bp.legend[token]?.alts?.[0]
  if (!alt || /^(air|cave_air|void_air)$/.test(alt.name)) return []
  let shapes
  try {
    const defaults = PreviewBlock.fromStateId(previewRegistry.blocksByName[alt.name].defaultState).getProperties()
    shapes = PreviewBlock.fromProperties(alt.name, { ...defaults, ...alt.states }, 0).shapes
  } catch { shapes = [[0, 0, 0, 1, 1, 1]] }
  if (!shapes.length) shapes = /torch|candle|lantern/.test(alt.name) ? [[.4, 0, .4, .6, .7, .6]]
    : /water|lava/.test(alt.name) ? [[0, 0, 0, 1, .875, 1]]
      : [[.2, 0, .2, .8, .6, .8]]
  return [{ x, y: layer.y, z, token, name: alt.name, states: alt.states ?? {}, shapes }]
})))

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

// The popup's live look: each frame is asked for the moment the last one is sent, so the page gets them as fast as
// the body draws, but no more than one per minMs: frames past that only spend the body's render worker. A body that
// cannot draw is asked again after retryMs. It ends when the page goes away (`open` turns false).
export async function streamFrames ({ frame, send, open, wait, now = Date.now, minMs, retryMs }) {
  while (open()) {
    const began = now()
    const f = await frame()
    if (!open()) return
    send(f)
    await wait(Math.max(0, (f.error ? retryMs : minMs) - (now() - began)))
  }
}

// One item's icon on the inventory screen. `image(name)` is a decoded texture (block names bare, items as item/<name>) or
// null. Most items have a picture of their own; a compass or clock has only its animation frames, a crossbow its states.
// A block item has none: the game draws a cube of the block, or the flat picture for one you walk through (a flower).
const ITEM_PICTURES = ['', '_00', '_standby']
export const inventoryIcon = (name, image) => {
  const own = ITEM_PICTURES.map(suffix => image(`item/${name}${suffix}`)).find(Boolean)
  if (own) return own
  const face = (side, names = []) => [...names, ...textureCandidates(name, side)].map(image).find(Boolean)
  const side = face('side')
  if (!side) return null
  if (previewRegistry.blocksByName[name]?.boundingBox !== 'block') return side
  return blockIcon(face('top') ?? side, face('side', [`${name}_front`]), side)
}

const ICON = /^\/api\/icon\/([a-z0-9_]{1,64})$/
const LOOK = /^\/api\/look\/([A-Za-z0-9_]{1,32})(\/live)?$/
const SCREEN = /^\/api\/screen\/([A-Za-z0-9_]{1,32})$/
const ACTIONS = /^\/api\/actions\/([A-Za-z0-9_]{1,32})$/
// map.mjs imports src/lib.mjs, and lib.mjs re-exports src/cli.mjs - both browser-safe, both need serving at the
// same relative path the browser resolves them to. Matching any flat *.mjs name under src/, rather than hardcoding
// lib.mjs alone, means the page's module graph does not go back to silently failing to load whenever another
// agent gives lib.mjs a new sibling import (an import a static route list would miss with no visible error at all).
// lib.mjs's split moved most of it under src/lib/: also match one lib/ segment (never src/body/, which the
// browser never loads) so /src/lib/plan.mjs etc still resolve.
const SRCLIB = /^\/src\/((?:(?:lib|boat|villager|build|navigation|tree|structure)\/)?[A-Za-z0-9_.-]+\.mjs)$/

// the blueprint library: one file by its kebab-case name, the same rule the front matter's name field obeys
const BLUEPRINT = /^\/api\/blueprint\/([a-z0-9]+(?:-[a-z0-9]+)*)$/

export const route = url => {
  const { pathname } = new URL(url, 'http://dashboard')
  if (pathname === '/' || pathname === '/index.html') return { kind: 'page' }
  if (pathname === '/blueprints') return { kind: 'blueprints' }
  if (pathname === '/villagers') return { kind: 'villagers' }
  if (pathname === '/villages') return { kind: 'villages' }
  if (pathname === '/blueprint.mjs') return { kind: 'bpscript' }
  if (pathname === '/api/blueprint-preview') return { kind: 'bppreview' }
  if (pathname === '/api/blueprints') return { kind: 'bplist' }
  const blueprint = BLUEPRINT.exec(pathname)
  if (blueprint) return { kind: 'blueprint', name: blueprint[1] }
  if (pathname === '/api/state') return { kind: 'state' }
  if (pathname === '/api/villagers') return { kind: 'villagersApi' }
  if (pathname === '/api/villages') return { kind: 'villagesApi' }
  if (pathname === '/api/chat') return { kind: 'chat' }
  if (pathname === '/api/world') return { kind: 'world' }
  if (pathname === '/map.mjs') return { kind: 'script' }
  const srclib = SRCLIB.exec(pathname)
  if (srclib) return { kind: 'srclib', name: srclib[1] }
  const look = LOOK.exec(pathname)
  if (look) return { kind: look[2] ? 'live' : 'look', name: look[1] }
  const screen = SCREEN.exec(pathname)
  if (screen) return { kind: 'screen', name: screen[1] }
  const actions = ACTIONS.exec(pathname)
  if (actions) return { kind: 'actions', name: actions[1] }
  const icon = ICON.exec(pathname)
  if (icon) return { kind: 'icon', name: icon[1] }
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

// ---------------------------------------------------------------- the action log
// "Action" = what the body was told to do (./mc requests, as job_started/args) and what came of it
// (job outcomes, death/respawn, holing up, chat). One line per entry; everything not listed here is
// either too noisy to show (hurt, fleeing, job_progress, ...) or a duplicate of a line already shown
// (job_queued is always followed ms later by job_started, which is what a queued-behind job becomes).
export const ACTION_LOG_LIMIT = 200

const GIST_CAP = 120
const capGist = s => s.length > GIST_CAP ? `${s.slice(0, GIST_CAP - 1)}…` : s

const argGist = args => Object.entries(args ?? {})
  .map(([k, v]) => `${k}=${typeof v === 'object' && v !== null ? JSON.stringify(v) : v}`)
  .join(' ')

const gainedGist = result => {
  const items = Object.entries(result?.gained ?? {})
  return items.length ? ` ${items.map(([name, n]) => `+${name}×${n}`).join(' ')}` : ''
}

const GIST = {
  job_started: e => capGist([e.name, argGist(e.args)].filter(Boolean).join(' ')),
  job_completed: e => `${e.name} done${e.result?.seconds ? ` in ${e.result.seconds}s` : ''}${gainedGist(e.result)}`,
  job_failed: e => `${e.name}: ${e.error ?? e.result?.error}`,
  job_cancelled: e => `${e.name}: ${e.reason}`,
  job_interrupted: e => `${e.name}: ${e.error}`,
  died: e => capGist(`${e.cause}${e.pos ? ` at ${e.pos.x},${e.pos.y},${e.pos.z}` : ''}`),
  respawned: e => e.note ?? e.doing,
  body_down: e => `exit ${e.exit}`,
  holing_up: e => capGist(e.why),
  holed_up: e => capGist(e.why),
  stuck: e => capGist(e.reason),
  kicked: e => e.reason,
  disconnected: e => e.reason,
  spawned: e => e.dimension,
  jobs_held: e => e.reason,
  eat_failed: e => e.message,
  tool_broke: e => e.item,
  error: e => e.message,
  chat: e => `${e.from}: ${e.message}`,
  whisper: e => `${e.from}: ${e.message}`
}
const BAD = new Set(['job_failed', 'job_interrupted', 'died', 'body_down', 'stuck', 'kicked', 'disconnected', 'eat_failed', 'error'])

export const actionLog = (lines, limit) => lines
  .filter(e => e && GIST[e.type])
  .map(e => ({ t: e.t, type: e.type, gist: GIST[e.type](e) ?? '', bad: BAD.has(e.type) }))
  .slice(-limit)

// ---------------------------------------------------------------- what stands on a plan's footprint
// A body's `scan` answers ASCII (renderScan in src/lib.mjs): a header naming the x range, a ruler, then per level
// "y=N" and one row per z ("<z> <one symbol per x>"), or "y=N all air", and last a legend "s=name g=name". Air is
// '.', every other block gets a symbol of its own per answer, so the legend is read off the answer, not a table.
// The z range is taken from the rows, or from `box` when every level came back all air and there are no rows.
const SCAN_HEAD = /^x (-?\d+)\.\.(-?\d+) across/
const SCAN_LEVEL = /^y=(-?\d+)( all air)?$/
const SCAN_ROW = /^\s*(-?\d+) (\S+)$/
const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, i) => Math.min(a, b) + i)

export const parseScan = (text, box = null) => {
  const lines = String(text ?? '').split('\n')
  const head = SCAN_HEAD.exec(lines[0] ?? '')
  if (!head || lines.length < 3) return []
  const xs = range(Number(head[1]), Number(head[2]))
  const legend = new Map([['.', 'air'], ...[...lines.at(-1).matchAll(/(\S)=(\S+)/g)].map(m => [m[1], m[2]])])
  const levels = []
  lines.slice(2, -1).forEach(line => {
    const level = SCAN_LEVEL.exec(line)
    if (level) { levels.push({ y: Number(level[1]), allAir: Boolean(level[2]), rows: [] }); return }
    const row = SCAN_ROW.exec(line)
    if (row && levels.length) levels.at(-1).rows.push({ z: Number(row[1]), symbols: row[2] })
  })
  const zsSeen = levels.flatMap(l => l.rows.map(r => r.z))
  const zs = box ? range(box.z1, box.z2) : zsSeen.length ? range(Math.min(...zsSeen), Math.max(...zsSeen)) : []
  return levels.flatMap(({ y, allAir, rows }) => allAir
    ? zs.flatMap(z => xs.map(x => ({ x, y, z, name: 'air' })))
    : rows.flatMap(({ z, symbols }) => xs.map((x, i) => ({ x, y, z, name: legend.get(symbols[i]) ?? symbols[i] }))))
}

// the footprint, two levels deep (the ground at y and what stands on it at y+1), in bands of rows a scan accepts
export const scanBoxes = ({ x, y, z, w, h }, cap) => {
  const rows = Math.max(1, Math.floor(cap / (w * 2)))
  return range(0, Math.ceil(h / rows) - 1).map(i => ({
    x1: x, y1: y, z1: z + i * rows, x2: x + w - 1, y2: y + 1, z2: z + Math.min(h, (i + 1) * rows) - 1
  }))
}

// a body only knows the chunks around itself, so the one standing closest is the one to ask
export const nearestBody = (bodies, x, z) => bodies
  .filter(b => b.up && b.state?.pos)
  .map(b => ({ body: b, dist: Math.hypot(b.state.pos.x - x, b.state.pos.z - z) }))
  .sort((a, b) => a.dist - b.dist)[0]?.body ?? null

// scan names a waterlogged slab as a plain slab: the ~ cells whose ground is neither water, air nor unloaded need block_at
export const unsureWater = (place, worldCells) => {
  const ground = new Map(worldCells.map(c => [`${c.x},${c.y},${c.z}`, c]))
  return planCells(place)
    .filter(c => planSpec(c)?.kind === 'water')
    .map(c => ({ x: c.x, y: c.y, z: c.z }))
    .filter(c => {
      const g = ground.get(`${c.x},${c.y},${c.z}`)
      return g && g.waterlogged === undefined && !/^(water|air|cave_air|void_air|unloaded)$/.test(g.name)
    })
}

// ---------------------------------------------------------------- the blueprint library
// One library file as the page shows it: parsed and resolved with its own default parameters (never re-implemented
// here: src/blueprint/format.mjs does all of it), its bill, what lint says, the counts, and every marked place whose note
// says it was built from this blueprint (`current` when the note's hash is this file's, else an older version stands).
// A file that does not parse keeps its name and the parser's errors, so the list shows the library as it is on disk.
export const blueprintBuilds = (name, hash, places) => places.flatMap(p => {
  let note = parseNote(p.note)
  if (/^bp2:/.test(p.note ?? '')) {
    try { const m = readBlueprintManifest(p.note); note = { blueprint: m.source.id, hash: m.sourceHash, facing: m.facing, params: {} } } catch { return [] }
  }
  if (note?.blueprint !== name) return []
  return [{ place: p.name, kind: p.kind, x: p.x, y: p.y, z: p.z, by: p.by, facing: note.facing, params: note.params, current: note.hash === hash }]
})
const lintOf = bp => {
  try { return lint(bp) } catch (error) { return { errors: [`lint failed: ${error.message}`], warnings: [] } }
}
export const blueprintDetail = ({ name, text, hash, document }, places = []) => {
  try {
    const detail = blueprintDocumentDetail(document ?? JSON.parse(text))
    return { ...detail, builds: blueprintBuilds(name, hash, places) }
  } catch (error) { return { name, hash, bp: null, errors: [error.message], builds: blueprintBuilds(name, hash, places), palette: 'unresolved' } }
}

export function blueprintDocumentDetail (document, stock) {
  try {
    const ir = compileBlueprintStructure(document)
    const allocated = stock === undefined ? null : allocateBlueprintMaterials(ir, { stock })
    const assignments = allocated?.assignments ?? representativeAssignments(ir)
    const bp = concreteBlueprint(ir, assignments)
    return { name: document.id, hash: ir.hash, bp, document, palette: allocated ? 'resolved-declared-stock' : 'representative', allocation: allocated, materialObjects: ir.objects.map(({ id, material, block }) => ({ id, material, block })), materials: document.materials, relationships: document.relationships ?? [], preview: previewCells(bp), bill: bill(bp), lint: lintOf(bp), counts: counts(bp), errors: [], builds: [] }
  } catch (error) { return { name: document?.id ?? 'draft', hash: null, bp: null, document, errors: [error.message], builds: [], palette: 'unresolved' } }
}
