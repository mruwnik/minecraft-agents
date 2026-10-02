import { parsePlacePlan } from '../src/lib/plan.mjs'
// A window on every agent body: where each one is, what it is doing, and what it sees.
//
//   node tools/dashboard.mjs            # http://127.0.0.1:3700
//   PORT=4000 node tools/dashboard.mjs
//
// It polls each body's `state` every 2 s (a quick action: it never takes the task slot, so a body mid-build is not
// disturbed) and serves the page, /api/state, /api/chat (what everyone said, merged from the bodies' event logs),
// /api/actions/<Name> (that body's recent action log, same event logs), /api/look/<Name> which renders one PNG
// through that body's eyes, /api/look/<Name>/live which streams them, and POST /api/whisper/<Name> which hands that
// body's driver a typed line as a whisper.
// /blueprints is a second page: the blueprint library (blueprints/*.md) as a list and, per blueprint, its layers
// drawn, its bill and what lint says.
// Nothing here drives a body; only a typed whisper reaches (and so wakes) its driver.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { parseAgents, snapshotFile, streamFrames, inventoryIcon, route, parseEventLines, mergeChat, chatLimit, actionLog, parseScan, scanBoxes, nearestBody, groupWorlds, findPlace, unsureWater, blueprintDetail, blueprintBuilds, blueprintDocumentDetail } from './dashboard/lib.mjs'
import { mergeBodies, parsePlan } from './dashboard/map.mjs'
import { scanCap } from '../src/lib.mjs'
import { decodePng, encodePng, tintOf } from '../src/vision/renderer.mjs'
import { BLUEPRINT_DIR } from '../src/blueprint/build.mjs'
import { loadBlueprintDocuments } from '../src/blueprint/source.mjs'
import { semanticBlueprintHash } from '../src/blueprint/schema.mjs'
import { emptyVillagerRoster, readVillagerRoster } from '../src/villager/roster.mjs'
import { readBlueprintManifest } from '../src/blueprint/manifest.mjs'
import { listVillageInspections } from '../src/villager/inspection.mjs'
import { villageViews, attachVillageStatus } from './dashboard/villages.mjs'

const ROOT = path.resolve(import.meta.dirname, '..')
const AGENTS_DIR = path.join(ROOT, 'state', 'agents')
const WORLDS_DIR = path.join(ROOT, 'state', 'worlds')
const PAGE = path.join(import.meta.dirname, 'dashboard', 'index.html')
const MAP_MODULE = path.join(import.meta.dirname, 'dashboard', 'map.mjs')
const BLUEPRINTS_PAGE = path.join(import.meta.dirname, 'dashboard', 'blueprints.html')
const VILLAGERS_PAGE = path.join(import.meta.dirname, 'dashboard', 'villagers.html')
const VILLAGES_PAGE = path.join(import.meta.dirname, 'dashboard', 'villages.html')
const BLUEPRINT_MODULE = path.join(import.meta.dirname, 'dashboard', 'blueprint.mjs')
const SRC_DIR = path.join(ROOT, 'src')
const TEXTURES = path.join(ROOT, 'textures')
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

// re-read every ask, like the agents: a world appears while this runs. A folder is a world once it holds world.json.
const readWorlds = () => (fs.existsSync(WORLDS_DIR) ? fs.readdirSync(WORLDS_DIR, { withFileTypes: true }) : [])
  .filter(e => e.isDirectory() && fs.existsSync(path.join(WORLDS_DIR, e.name, 'world.json')))
  .map(e => e.name)
  .sort()
  .map(name => ({
    name,
    places: readJson(path.join(WORLDS_DIR, name, 'places.json'), []),
    zones: readJson(path.join(WORLDS_DIR, name, 'zones.json'), [])
  }))
const allPlaces = worlds => worlds.flatMap(w => w.places)

// the bodies' own HTTP API (src/body/api.mjs): POST /<action> with a JSON body
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
  const worlds = readWorlds()
  const villageData = villageSnapshot(worlds), villages = villageData.villages
  return {
    at: Date.now(),
    agents: agentNames(),
    bodies,
    worlds: groupWorlds(worlds.map(w => ({ ...w, places: attachVillageStatus(w.places, villages) })), bodies, agentNames()),
    villageError: villageData.error
  }
}

const villagers = () => {
  try { return readVillagerRoster(path.join(ROOT, 'state', 'villagers.json')) } catch (error) {
    return { ...emptyVillagerRoster(), error: `villager roster unavailable: ${error.message}` }
  }
}

// Village summaries are snapshots written by village.check / village.maintain.
// The dashboard never polls a body or scans world blocks to manufacture one.
const villageSnapshot = (worlds = readWorlds()) => {
  const places = allPlaces(worlds)
  const manifests = [], manifestErrors = []
  for (const place of places) {
    if (!String(place.note ?? '').startsWith('bp2:')) continue
    try { const manifest = readBlueprintManifest(place.note); if (manifest) manifests.push(manifest) } catch (error) { manifestErrors.push({ place: place.name, error: `saved blueprint unavailable: ${error.message}` }) }
  }
  let inspections = [], inspectionError = null
  try { inspections = listVillageInspections() } catch (error) { inspectionError = `saved village inspections unavailable: ${error.message}` }
  return { villages: villageViews({ places, manifests, inspections, manifestErrors, roster: villagers() }), error: inspectionError }
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
const scansByPlace = {}

const lookAtPlace = async name => {
  const found = findPlace(groupWorlds(readWorlds(), mergeBodies(agents, polls), agentNames()), name)
  if (!found) return { error: `no place called ${name}` }
  const { place, world } = found
  const parsed = parsePlacePlan(place)
  if (parsed.error) return { error: `${name} has no plan to compare the world against` }
  const body = nearestBody(world.bodies, place.x, place.z)
  if (!body) return { error: `no body is up in ${world.name} to look` }
  const levels = [...new Set(parsed.cells.map(c => place.y + c.dy))]
  const boxes = levels.flatMap(y => scanBoxes({ x: place.x, y, z: place.z, w: parsed.width, h: parsed.height }, scanCap()))
  const scans = await Promise.all(boxes.map(box => ask(body.apiPort, 'scan', box, 10000)))
  const failed = scans.find(r => !r.ok)
  if (failed) return { error: `${body.name} could not scan: ${failed.error ?? failed.answer?.error ?? 'no answer'}`, body: body.name }
  const cells = scans.flatMap((r, i) => parseScan(r.answer.map, boxes[i]))
  const unsure = unsureWater(place, cells)
  const checks = await Promise.all(unsure.slice(0, WORLD_BLOCK_AT_CAP).map(c => ask(body.apiPort, 'block_at', c, 5000)))
  const settled = new Map(checks.flatMap((r, i) => r.ok && r.answer.name ? [[`${unsure[i].x},${unsure[i].y},${unsure[i].z}`, r.answer]] : []))
  const known = cells.map(c => {
    const b = settled.get(`${c.x},${c.y},${c.z}`)
    return b ? { ...c, name: b.name, waterlogged: String(b.properties?.waterlogged) === 'true' } : c
  })
  const seen = known.filter(c => c.name !== 'unloaded').length
  const dist = Math.round(Math.hypot(body.state.pos.x - place.x, body.state.pos.z - place.z))
  if (!seen) return { error: `no body near enough to see it (${body.name} is the nearest, ${dist} blocks away)`, body: body.name }
  return { place: name, body: body.name, distance: dist, at: Date.now(), cells: known, unloaded: known.length - seen, unsettled: Math.max(0, unsure.length - WORLD_BLOCK_AT_CAP) }
}

// one look per place per 10 s, shared by every open popup: the page polls while its popup shows the world
const worldFor = name => {
  const cached = scansByPlace[name]
  if (cached && Date.now() - cached.at < WORLD_TTL_MS) return cached.promise
  const promise = lookAtPlace(name).catch(e => ({ error: e.message }))
  scansByPlace[name] = { at: Date.now(), promise }
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
  const places = allPlaces(readWorlds())
  return { at: Date.now(), blueprints: loadBlueprintDocuments(BLUEPRINT_DIR).map(file => ({ ...file, hash: semanticBlueprintHash(file.document) })).map(file => ({ ...blueprintFor(file), builds: blueprintBuilds(file.name, file.hash, places) })) }
}

const send = (res, code, type, payload, headers = {}) => {
  res.writeHead(code, { 'content-type': type, 'cache-control': 'no-store', ...headers })
  res.end(payload)
}
const sendJson = (res, code, value) => send(res, code, 'application/json', JSON.stringify(value))

// one PNG through that body's eyes. `look` is a quick action in src/body/actions/sense.mjs: it reads the chunk data the body already
// holds and never turns it, so this cannot interrupt a walk, a build or a composite that is running.
const lookFrame = async (agent, args) => {
  const r = await ask(agent.apiPort, 'look', args, 30000)
  if (!r.ok) return { code: 503, error: r.error ?? r.answer?.error ?? 'the body did not answer' }
  const file = snapshotFile(path.join(AGENTS_DIR, agent.name), r.answer.file)
  if (!file || !fs.existsSync(file)) return { code: 502, error: `the body rendered ${r.answer.file}, which is not a file I may serve` }
  return { png: fs.readFileSync(file), at: r.answer.at ?? null, view: r.answer.view ?? '', seen: r.answer.seen ?? [], marks: r.answer.marks ?? [], blocked: r.answer.blocked ?? '' }
}

const serveLook = async (res, name, query) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const args = { file: LOOK_FILE, ...(query.get('dir') ? { dir: query.get('dir') } : {}) }
  const frame = await lookFrame(agent, args)
  if (frame.error) return sendJson(res, frame.code, { error: frame.error })
  send(res, 200, 'image/png', frame.png, {
    'x-look-view': encodeURIComponent(frame.view),
    'x-look-seen': encodeURIComponent(JSON.stringify(frame.seen)),
    'x-look-blocked': encodeURIComponent(frame.blocked)
  })
}

// one body's screen: HUD numbers, inventory slots and the container it has open. `screen` is a quick action
// (src/body/actions/sense.mjs): it reads the bot's own state, so this never interrupts whatever the body is doing.
const serveScreen = async (res, name) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const r = await ask(agent.apiPort, 'screen', {}, 5000)
  if (!r.ok) return sendJson(res, 503, { error: r.error ?? r.answer?.error ?? 'the body did not answer' })
  return sendJson(res, 200, r.answer)
}

// one body's recent actions, read straight from its events.jsonl tail (the same source the chat log reads). A
// folder with no config.json still counts, same as the chat log: the reader wants what the body did, not whether
// it currently answers.
const serveActions = (res, name) => {
  if (!fs.existsSync(path.join(AGENTS_DIR, name))) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  return sendJson(res, 200, { at: Date.now(), name, entries: actionLog(eventsTail(name)) })
}

// a line typed into the popup goes to the body as the whisper it stands for: the body's `hear` appends it to its own
// events.jsonl (it owns that file and its seq numbers), so the driver reads it exactly as one whispered in game
const WHISPER_FROM = 'dashboard'
const serveWhisper = async (req, res, name) => {
  if (req.method !== 'POST') return sendJson(res, 405, { error: 'POST {"message": "..."} to whisper' })
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  let body = ''
  for await (const chunk of req) body += chunk
  const r = await ask(agent.apiPort, 'hear', { from: WHISPER_FROM, message: parseJson(body)?.message }, 5000)
  if (!r.ok) return sendJson(res, 503, { error: r.error ?? r.answer?.error ?? 'the body did not answer' })
  return sendJson(res, 200, { ok: true })
}

// the popup's live look, as server-sent events of base64 PNGs with their captions. Two thirds the size of a one-shot
// look, about 60 ms of the body's render worker a frame instead of 120, so it keeps up with the 10 a second
// streamFrames allows. Each stream draws to its own file, so two popups on one body never read each other's
// half-written frame.
const LIVE_SIZE = { width: 320, height: 180 }
let liveStreams = 0
const streamLook = async (req, res, name) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const file = `dashboard-live-${++liveStreams}.png`
  const args = { file, marks: true, ...LIVE_SIZE }
  let open = true
  req.on('close', () => { open = false })
  res.writeHead(200, { 'content-type': 'text/event-stream', 'cache-control': 'no-store' })
  await streamFrames({
    frame: () => lookFrame(agent, args),
    send: ({ code, png, ...rest }) => res.write(`data: ${JSON.stringify({ ...rest, ...(png ? { png: png.toString('base64') } : {}) })}\n\n`),
    open: () => open,
    wait: ms => new Promise(resolve => setTimeout(resolve, ms)),
    minMs: 100,
    retryMs: 1000
  })
  fs.rmSync(path.join(AGENTS_DIR, agent.name, 'snapshots', file), { force: true })
}

// textures/ as tools/textures.mjs fills it, tinted as the look pictures are; an animated one is its frames stacked, so
// only the first square is kept
const textureImage = name => {
  const file = path.join(TEXTURES, `${name}.png`)
  if (!fs.existsSync(file)) return null
  const { width, height, rgba } = decodePng(fs.readFileSync(file))
  const tint = tintOf(name)
  const frame = rgba.subarray(0, width * Math.min(width, height) * 4)
  return { width, height: Math.min(width, height), rgba: tint ? frame.map((v, i) => i % 4 === 3 ? v : v * tint[i % 4] / 255) : frame }
}
// the browser keeps an icon: a body's inventory is drawn again every second, and its items seldom change
const serveIcon = (res, name) => {
  const icon = inventoryIcon(name, textureImage)
  if (!icon) return sendJson(res, 404, { error: `no picture for ${name} in textures/ (node tools/textures.mjs fills it)` })
  send(res, 200, 'image/png', encodePng(icon.width, icon.height, icon.rgba), { 'cache-control': 'max-age=3600' })
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
  const place = farm ? findPlace(readWorlds(), farm)?.place ?? null : null
  const chat = { at: Date.now(), agents: agentNames(), messages: chatLog(CHAT_PAGE_LINES) }
  const world = place && (view === 'world' || view === 'diff') ? await worldFor(farm) : null
  const preload = `<script>window.__PRELOAD_STATE__=${inline(snapshot())};window.__PRELOAD_PLACE__=${inline(place)};window.__PRELOAD_CHAT__=${inline(chat)};window.__PRELOAD_WORLD__=${inline(world)};window.__PRELOAD_VIEW__=${inline(view)}</script>\n`
  return fs.readFileSync(PAGE, 'utf8').replace('</head>', `${preload}</head>`)
}
// the library is inlined for the same reason: the list and the chosen blueprint (?name=) are drawn on the first paint
const renderBlueprintsPage = () => fs.readFileSync(BLUEPRINTS_PAGE, 'utf8').replace('</head>', `<script>window.__PRELOAD_LIBRARY__=${inline(library())}</script>\n</head>`)
const renderVillagersPage = () => fs.readFileSync(VILLAGERS_PAGE, 'utf8')
const renderVillagesPage = () => fs.readFileSync(VILLAGES_PAGE, 'utf8')

const handlers = {
  page: async (res, query) => send(res, 200, 'text/html; charset=utf-8', await renderPage(query)),
  world: async (res, query) => {
    const name = query.get('place')
    if (!name) return sendJson(res, 400, { error: 'say which place: /api/world?place=<name>' })
    const answer = await worldFor(name)
    return sendJson(res, answer.error ? 404 : 200, answer)
  },
  villagers: (res) => send(res, 200, 'text/html; charset=utf-8', renderVillagersPage()),
  villagersApi: (res) => sendJson(res, 200, villagers()),
  villages: (res) => send(res, 200, 'text/html; charset=utf-8', renderVillagesPage()),
  villagesApi: (res) => sendJson(res, 200, { ...villageSnapshot(), readOnly: true }),
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
  screen: (res, query, r) => serveScreen(res, r.name),
  actions: (res, query, r) => serveActions(res, r.name),
  icon: (res, query, r) => serveIcon(res, r.name),
  unknown: (res) => sendJson(res, 404, { error: 'try /, /villagers, /villages, /blueprints, /api/state, /api/villagers, /api/villages, /api/chat?limit=200, /api/world?place=<name>, /api/blueprints, /api/blueprint/<name>, /api/look/<Name>, /api/look/<Name>/live, /api/screen/<Name>, /api/actions/<Name>, POST /api/whisper/<Name> or /api/icon/<item>' })
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
  if (r.kind === 'whisper') return serveWhisper(req, res, r.name).catch(e => sendJson(res, 500, { error: e.message }))
  if (r.kind === 'live') return streamLook(req, res, r.name).catch(e => res.headersSent ? res.end() : sendJson(res, 500, { error: e.message }))
  return Promise.resolve(handlers[r.kind](res, query, r)).catch(e => sendJson(res, 500, { error: e.message }))
}).listen(PORT, '127.0.0.1', async () => {
  await pollOnce()
  setInterval(() => pollOnce().catch(e => console.error('[poll]', e.message)), POLL_MS)
  console.log(`dashboard on http://127.0.0.1:${PORT} (${agents.length} agent folders)`)
})
