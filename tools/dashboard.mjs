// A read-only window on every agent body: where each one is, what it is doing, and what it sees.
//
//   node tools/dashboard.mjs            # http://127.0.0.1:3700
//   PORT=4000 node tools/dashboard.mjs
//
// It polls each body's `state` every 2 s (a quick action: it never takes the task slot, so a body mid-build is not
// disturbed) and serves the page, /api/state, and /api/look/<Name> which renders one PNG through that body's eyes.
// Nothing here drives a body or spends an agent's tokens.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { parseAgents, snapshotFile, route } from './dashboard/lib.mjs'
import { mergeBodies, danSighting } from './dashboard/map.mjs'

const ROOT = path.resolve(import.meta.dirname, '..')
const AGENTS_DIR = path.join(ROOT, 'state', 'agents')
const PAGE = path.join(import.meta.dirname, 'dashboard', 'index.html')
const MAP_MODULE = path.join(import.meta.dirname, 'dashboard', 'map.mjs')
const PORT = Number(process.env.PORT ?? 3700)
const DAN = process.env.DAN_NAME ?? 'mruwnik'
const POLL_MS = 2000
const LOOK_FILE = 'dashboard-look.png'

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
    dan: DAN,
    bodies,
    danAt: danSighting(bodies, DAN),
    places: readJson(path.join(ROOT, 'state', 'places.json'), []),
    zones: readJson(path.join(ROOT, 'state', 'zones.json'), [])
  }
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

const handlers = {
  page: (res) => send(res, 200, 'text/html; charset=utf-8', fs.readFileSync(PAGE)),
  state: (res) => sendJson(res, 200, snapshot()),
  script: (res) => send(res, 200, 'text/javascript; charset=utf-8', fs.readFileSync(MAP_MODULE)),
  unknown: (res) => sendJson(res, 404, { error: 'try /, /api/state or /api/look/<Name>' })
}

http.createServer(async (req, res) => {
  const r = route(req.url)
  const query = new URL(req.url, 'http://dashboard').searchParams
  if (r.kind === 'look') return serveLook(res, r.name, query).catch(e => sendJson(res, 500, { error: e.message }))
  return handlers[r.kind](res)
}).listen(PORT, '127.0.0.1', async () => {
  await pollOnce()
  setInterval(() => pollOnce().catch(e => console.error('[poll]', e.message)), POLL_MS)
  console.log(`dashboard on http://127.0.0.1:${PORT} (${agents.length} agent folders)`)
})
