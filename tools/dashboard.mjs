// A read-only window on every agent body: where each one is, what it is doing, and what it sees.
//
//   node tools/dashboard.mjs            # http://127.0.0.1:3700
//   PORT=4000 node tools/dashboard.mjs
//
// It polls each body's `state` every 2 s (a quick action: it never takes the task slot, so a body mid-build is not
// disturbed) and serves the page, /api/state, /api/chat (what everyone said, merged from the bodies' event logs)
// and /api/look/<Name> which renders one PNG through that body's eyes. /blueprints is a second page: the blueprint
// library (blueprints/*.md) as a list and, per blueprint, its layers drawn, its bill and what lint says.
// Nothing here drives a body or spends an agent's tokens.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { parseAgents, snapshotFile, route, parseEventLines, mergeChat, chatLimit, parseScan, scanBoxes, nearestBody, unsureWater, blueprintDetail, blueprintBuilds, blueprintDocumentDetail } from './dashboard/lib.mjs'
import { mergeBodies, humanSightings, parsePlan } from './dashboard/map.mjs'
import { scanCap } from '../src/lib.mjs'
import { BLUEPRINT_DIR } from '../src/blueprint/build.mjs'
import { loadBlueprintDocuments } from '../src/blueprint/source.mjs'
import { semanticBlueprintHash } from '../src/blueprint/schema.mjs'

const ROOT = path.resolve(import.meta.dirname, '..')
const AGENTS_DIR = path.join(ROOT, 'state', 'agents')
const PAGE = path.join(import.meta.dirname, 'dashboard', 'index.html')
const MAP_MODULE = path.join(import.meta.dirname, 'dashboard', 'map.mjs')
const BLUEPRINTS_PAGE = path.join(import.meta.dirname, 'dashboard', 'blueprints.html')
const BLUEPRINT_MODULE = path.join(import.meta.dirname, 'dashboard', 'blueprint.mjs')
const SRC_DIR = path.join(ROOT, 'src')
const PORT = Number(process.env.PORT ?? 3700)
const POLL_MS = 2000
const LOOK_FILE = 'dashboard-look.png'
const CHAT_TAIL_BYTES = 64 * 1024
const CHAT_PAGE_LINES = 300   // what the page shows: inlined on load, then polled from /api/chat
const WORLD_TTL_MS = 10000    // how long one look at a place's footprint is handed out again
const WORLD_BLOCK_AT_CAP = 400 // how many slab cells one look may settle with block_at, one call each

const parseJson = text => {
  try {
    return JSON.parse(text)
  } catch {
    return null
  }
}

const readText = file => {
  try {
    return fs.readFileSync(file, 'utf8')
  } catch {
    return ''
  }
}

const readJson = (file, fallback) => parseJson(readText(file)) ?? fallback

// re-read every cycle: a new agent folder appears while this runs, and an agent is cheap to describe
const readAgents = () => {
  const dirs = fs.existsSync(AGENTS_DIR) ? fs.readdirSync(AGENTS_DIR, { withFileTypes: true }).filter(e => e.isDirectory()) : []
  return parseAgents(dirs.map(e => ({ name: e.name, text: readText(path.join(AGENTS_DIR, e.name, 'config.json')) })))
}

// the bodies' own HTTP API (src/bot.mjs): POST /<action> with a JSON body
const ask = (port, action, args, timeoutMs) => new Promise(resolve => {
  const body = JSON.stringify(args)
  const req = http.request({ host: '127.0.0.1', port, path: `/${action}`, method: 'POST', timeout: timeoutMs }, res => {
    const chunks = []
    res.on('data', c => chunks.push(c))
    res.on('end', () => {
      const text = Buffer.concat(chunks).toString('utf8')
      const parsed = parseJson(text)
      resolve(parsed ? { ok: parsed.ok !== false, answer: parsed } : { ok: false, error: `unreadable answer: ${text.slice(0, 80)}` })
    })
  })
  req.on('timeout', () => { req.destroy(new Error(`no answer in ${timeoutMs}ms`)) })
  req.on('error', e => resolve({ ok: false, error: e.message }))
  req.end(body)
})

const polls = {}
let agents = []
// the names the agents play under: any other player some body sees, or any other speaker in chat, is a human
const agentNames = () => [...new Set(agents.flatMap(a => [a.name, a.username]))]

const pollOnce = async () => {
  agents = readAgents()
  await Promise.all(agents.map(async agent => {
    const r = await ask(agent.apiPort, 'state', {}, 1500)
    const at = Date.now()
    if (!r.ok) { polls[agent.name] = { ok: false, error: r.error ?? r.answer?.error ?? 'down', at }; return }
    const { ok, ...state } = r.answer
    polls[agent.name] = { ok: true, state, at }
  }))
}

const snapshot = () => {
  const bodies = mergeBodies(agents, polls)
  return {
    at: Date.now(),
    agents: agentNames(),
    bodies,
    humans: humanSightings(bodies, agentNames()),
    places: readJson(path.join(ROOT, 'state', 'places.json'), []),
    zones: readJson(path.join(ROOT, 'state', 'zones.json'), [])
  }
}

// ---------------------------------------------------------------- the chat log
// Every body's events.jsonl grows to hundreds of KB, and the page asks every 2 s: only the last 64 KB of each file
// is read, and only when the file has grown since the last read (the tail is kept per folder, keyed on its size).
const tails = {}

const readTail = file => {
  const fd = fs.openSync(file, 'r')
  try {
    const size = fs.fstatSync(fd).size
    const start = Math.max(0, size - CHAT_TAIL_BYTES)
    const buf = Buffer.alloc(size - start)
    fs.readSync(fd, buf, 0, buf.length, start)
    return { size, torn: start > 0, text: buf.toString('utf8') }
  } finally {
    fs.closeSync(fd)
  }
}

const eventsTail = agent => {
  const file = path.join(AGENTS_DIR, agent, 'events.jsonl')
  const size = fs.existsSync(file) ? fs.statSync(file).size : -1
  if (size < 0) return []
  if (tails[agent]?.size === size) return tails[agent].lines
  const tail = readTail(file)
  const lines = parseEventLines(tail.text, tail.torn)
  tails[agent] = { size: tail.size, lines }
  return lines
}

// the folders are read afresh (not taken from `agents`): a folder with no config.json still holds the whispers
// its body received, and the log is about who said what, not about which bodies can be polled
const chatLog = limit => {
  const dirs = fs.existsSync(AGENTS_DIR) ? fs.readdirSync(AGENTS_DIR, { withFileTypes: true }).filter(e => e.isDirectory()) : []
  return mergeChat(dirs.map(e => ({ agent: e.name, lines: eventsTail(e.name) })), limit)
}

// ---------------------------------------------------------------- what stands on a plan's footprint
// The dashboard has no world of its own: a running body has, for the chunks around itself. The nearest body that is
// up scans the footprint two levels deep (read-only, a quick action that never moves it), and the ~ cells that came
// back as a slab are asked one by one with block_at, since scan cannot say whether a slab is waterlogged.
const worlds = {}

const lookAtPlace = async name => {
  const place = readJson(path.join(ROOT, 'state', 'places.json'), []).find(p => p.name === name)
  if (!place) return { error: `no place called ${name}` }
  const parsed = parsePlan(place.plan)
  if (parsed.error) return { error: `${name} has no plan to compare the world against` }
  const body = nearestBody(mergeBodies(agents, polls), place.x, place.z)
  if (!body) return { error: 'no body is up to look' }
  const boxes = scanBoxes({ x: place.x, y: place.y, z: place.z, w: parsed.width, h: parsed.height }, scanCap())
  const scans = await Promise.all(boxes.map(box => ask(body.apiPort, 'scan', box, 10000)))
  const failed = scans.find(r => !r.ok)
  if (failed) return { error: `${body.name} could not scan: ${failed.error ?? failed.answer?.error ?? 'no answer'}`, body: body.name }
  const cells = scans.flatMap((r, i) => parseScan(r.answer.map, boxes[i]))
  const unsure = unsureWater(place, cells)
  const checks = await Promise.all(unsure.slice(0, WORLD_BLOCK_AT_CAP).map(c => ask(body.apiPort, 'block_at', c, 5000)))
  const settled = new Map(checks.flatMap((r, i) => r.ok && r.answer.name ? [[`${unsure[i].x},${unsure[i].z}`, r.answer]] : []))
  const known = cells.map(c => {
    const b = c.y === place.y ? settled.get(`${c.x},${c.z}`) : null
    return b ? { ...c, name: b.name, waterlogged: String(b.properties?.waterlogged) === 'true' } : c
  })
  const seen = known.filter(c => c.name !== 'unloaded').length
  const dist = Math.round(Math.hypot(body.state.pos.x - place.x, body.state.pos.z - place.z))
  if (!seen) return { error: `no body near enough to see it (${body.name} is the nearest, ${dist} blocks away)`, body: body.name }
  return { place: name, body: body.name, distance: dist, at: Date.now(), cells: known, unloaded: known.length - seen, unsettled: Math.max(0, unsure.length - WORLD_BLOCK_AT_CAP) }
}

// one look per place per 10 s, shared by every open popup: the page polls while its popup shows the world
const worldFor = name => {
  const cached = worlds[name]
  if (cached && Date.now() - cached.at < WORLD_TTL_MS) return cached.promise
  const promise = lookAtPlace(name).catch(e => ({ error: e.message }))
  worlds[name] = { at: Date.now(), promise }
  return promise
}

// ---------------------------------------------------------------- the blueprint library
// Every file under blueprints/ is re-read on each ask (they are small, and one edited in place should show at once),
// but parsing, costing and lint - lint builds the thing over flat ground - are kept per file hash; only the marked
// places built from a blueprint, which change with the world, are matched afresh.
const details = {}
const blueprintFor = file => {
  const cached = details[file.name]
  if (cached && cached.hash === file.hash) return cached
  details[file.name] = blueprintDetail(file)
  return details[file.name]
}
const library = () => {
  const places = readJson(path.join(ROOT, 'state', 'places.json'), [])
  return { at: Date.now(), blueprints: loadBlueprintDocuments(BLUEPRINT_DIR).map(file => ({ ...file, hash: semanticBlueprintHash(file.document) })).map(file => ({ ...blueprintFor(file), builds: blueprintBuilds(file.name, file.hash, places) })) }
}

const send = (res, code, type, payload, headers = {}) => {
  res.writeHead(code, { 'content-type': type, 'cache-control': 'no-store', ...headers })
  res.end(payload)
}
const sendJson = (res, code, value) => send(res, code, 'application/json', JSON.stringify(value))

// one PNG through that body's eyes. `look` is a quick action in src/bot.mjs: it reads the chunk data the body already
// holds and never turns it, so this cannot interrupt a walk, a build or a composite that is running.
const serveLook = async (res, name, query) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const home = path.join(AGENTS_DIR, agent.name)
  const args = { file: LOOK_FILE, ...(query.get('pano') ? { pano: true } : {}), ...(query.get('dir') ? { dir: query.get('dir') } : {}) }
  const r = await ask(agent.apiPort, 'look', args, 30000)
  if (!r.ok) return sendJson(res, 503, { error: r.error ?? r.answer?.error ?? 'the body did not answer' })
  const file = snapshotFile(home, r.answer.file)
  if (!file || !fs.existsSync(file)) return sendJson(res, 502, { error: `the body rendered ${r.answer.file}, which is not a file I may serve` })
  send(res, 200, 'image/png', fs.readFileSync(file), {
    'x-look-view': encodeURIComponent(r.answer.view ?? ''),
    'x-look-seen': encodeURIComponent(JSON.stringify(r.answer.seen ?? [])),
    'x-look-blocked': encodeURIComponent(r.answer.blocked ?? '')
  })
}

// ?farm=<name> is inlined into the page itself (not left to the /api/state fetch below it) so the popup it opens
// is there on the very first paint - the property a headless screenshot needs, and a plain page load never pays for.
// </script and </head can't slip out of the inline script tag, since the page ships this straight into an attribute-free <script> body.
// The chat log is inlined the same way, for the same reason: the drawer is full on the first paint.
const inline = value => JSON.stringify(value).replace(/</g, '\\u003c')
// The bodies' state is inlined too, so the map is drawn on the first paint; ?view=world|diff with ?farm= inlines
// the world answer as well, so the popup opens already comparing.
const renderPage = async query => {
  const farm = query.get('farm')
  const view = query.get('view')
  const place = farm ? readJson(path.join(ROOT, 'state', 'places.json'), []).find(p => p.name === farm) ?? null : null
  const chat = { at: Date.now(), agents: agentNames(), messages: chatLog(CHAT_PAGE_LINES) }
  const world = place && (view === 'world' || view === 'diff') ? await worldFor(farm) : null
  const preload = `<script>window.__PRELOAD_STATE__=${inline(snapshot())};window.__PRELOAD_PLACE__=${inline(place)};window.__PRELOAD_CHAT__=${inline(chat)};window.__PRELOAD_WORLD__=${inline(world)};window.__PRELOAD_VIEW__=${inline(view)}</script>\n`
  return fs.readFileSync(PAGE, 'utf8').replace('</head>', `${preload}</head>`)
}
// the library is inlined for the same reason: the list and the chosen blueprint (?name=) are drawn on the first paint
const renderBlueprintsPage = () => fs.readFileSync(BLUEPRINTS_PAGE, 'utf8').replace('</head>', `<script>window.__PRELOAD_LIBRARY__=${inline(library())}</script>\n</head>`)

const handlers = {
  page: async (res, query) => send(res, 200, 'text/html; charset=utf-8', await renderPage(query)),
  world: async (res, query) => {
    const name = query.get('place')
    if (!name) return sendJson(res, 400, { error: 'say which place: /api/world?place=<name>' })
    const answer = await worldFor(name)
    return sendJson(res, answer.error ? 404 : 200, answer)
  },
  state: (res) => sendJson(res, 200, snapshot()),
  blueprints: (res) => send(res, 200, 'text/html; charset=utf-8', renderBlueprintsPage()),
  bplist: (res) => sendJson(res, 200, library()),
  blueprint: (res, query, r) => {
    const found = library().blueprints.find(b => b.name === r.name)
    return found ? sendJson(res, 200, found) : sendJson(res, 404, { error: `no blueprint called ${r.name}: /api/blueprints lists them` })
  },
  bpscript: (res) => send(res, 200, 'text/javascript; charset=utf-8', fs.readFileSync(BLUEPRINT_MODULE)),
  chat: (res, query) => sendJson(res, 200, { at: Date.now(), agents: agentNames(), messages: chatLog(chatLimit(query.get('limit'))) }),
  script: (res) => send(res, 200, 'text/javascript; charset=utf-8', fs.readFileSync(MAP_MODULE)),
  srclib: (res, query, r) => send(res, 200, 'text/javascript; charset=utf-8', fs.readFileSync(path.join(SRC_DIR, r.name))),
  unknown: (res) => sendJson(res, 404, { error: 'try /, /blueprints, /api/state, /api/chat?limit=200, /api/world?place=<name>, /api/blueprints, /api/blueprint/<name> or /api/look/<Name>' })
}

http.createServer(async (req, res) => {
  const r = route(req.url)
  if (r.kind === 'bppreview') {
    if (req.method !== 'POST') return sendJson(res, 405, { error: 'POST a structured plan to preview' })
    let body = '', bytes = 0, tooLarge = false
    for await (const chunk of req) { bytes += chunk.length; if (bytes > 2 * 1024 * 1024) { tooLarge = true; body = '' } else if (!tooLarge) body += chunk }
    if (tooLarge) return sendJson(res, 413, { error: 'preview body exceeds 2 MiB' })
    try { const input = JSON.parse(body); const detail = blueprintDocumentDetail(input.plan, input.stock); return sendJson(res, detail.errors.length ? 400 : 200, detail) } catch (error) { return sendJson(res, 400, { error: error.message }) }
  }
  const query = new URL(req.url, 'http://dashboard').searchParams
  if (r.kind === 'look') return serveLook(res, r.name, query).catch(e => sendJson(res, 500, { error: e.message }))
  return Promise.resolve(handlers[r.kind](res, query, r)).catch(e => sendJson(res, 500, { error: e.message }))
}).listen(PORT, '127.0.0.1', async () => {
  await pollOnce()
  setInterval(() => pollOnce().catch(e => console.error('[poll]', e.message)), POLL_MS)
  console.log(`dashboard on http://127.0.0.1:${PORT} (${agents.length} agent folders)`)
})
