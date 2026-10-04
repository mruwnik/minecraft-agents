// HTTP server for the browser view: static pages, the dumped column files, and a server-sent event stream that
// pushes an agent's pose, hud and changed columns. Reads state/ (the body writes it atomically) and also proxies
// driving commands to a body's control socket.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { createAssetCache, buildAssetsInWorker } from './view-assets.mjs'
import { createDriveProxy } from './drive-proxy.mjs'
import { SEVERITIES, severityOf } from './block-issues.mjs'
import { biomeTable } from './biome-colors.mjs'
import { createBlockScanner, classifyReal, findClientJar } from './block-scan.mjs'
import { bodyDir, listBodies, worldsDir } from '../../engine/js/bodies.mjs'

const NAME = /^[A-Za-z0-9_-]+$/
const VERSION = /^[0-9.]+$/
const COLUMN_FILE = /^(-?\d+)\.(-?\d+)\.bin$/
const WEB_FILE = /^(?!\.)[A-Za-z0-9_.-]+$/
const TYPES = { '.html': 'text/html; charset=utf-8', '.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css; charset=utf-8', '.json': 'application/json; charset=utf-8' }
const PING_MS = 15000
const MAX_STREAM_AGENTS = 32
// a body is addressed as <world>/<name>: a name is unique only within a world
const bodyOfKey = key => {
  const parts = (key ?? '').split('/')
  return parts.length === 2 && parts.every(p => NAME.test(p)) ? { world: parts[0], name: parts[1] } : null
}

const send = (res, status, body, headers = {}) => {
  res.writeHead(status, { 'Cache-Control': 'no-cache', ...headers })
  res.end(body)
}
const VIEWER_BUNDLE = 'cljs/viewer.mjs'
const notFound = res => send(res, 404, 'not found', { 'Content-Type': 'text/plain' })

const statOrNull = async file => {
  try {
    return await fs.promises.stat(file)
  } catch {
    return null
  }
}
const readJson = async file => {
  try {
    return JSON.parse(await fs.promises.readFile(file, 'utf8'))
  } catch {
    return null
  }
}

const sendFile = async (res, file, contentType) => {
  const stat = await statOrNull(file)
  if (!stat?.isFile()) return notFound(res)
  try {
    return send(res, 200, await fs.promises.readFile(file), { 'Content-Type': contentType, 'X-Mtime': String(stat.mtimeMs) })
  } catch {
    return notFound(res)
  }
}

export function createViewServer ({ stateDir, textureDir, webDir, pollMs = 50, columnPollMs = 250, push = 'watch', watchFallbackMs = 250, blockJar, blockSweepMs = 5000, blockWriteMs = 5000 }) {
  const jarPath = blockJar === undefined ? findClientJar() : blockJar
  const scanner = createBlockScanner({ stateDir, textureDir, jar: jarPath, sweepMs: blockSweepMs, writeMs: blockWriteMs })
  const driveProxy = createDriveProxy({ stateDir })
  const agentFile = ({ world, name }, file) => path.join(bodyDir(stateDir, world, name), 'view', file)
  // one build per version (in a worker thread, see view-assets.mjs) serves both the table and the texture bytes
  const buildFor = createAssetCache(version => buildAssetsInWorker(version, textureDir, jarPath))

  // ?debug=1 or 2: the table with `issue` (the worst severity) on every material whose block the view draws wrong (tools/view/block-issues.mjs)
  const debugTables = new Map()
  const debugTableFor = async version => {
    if (debugTables.has(version)) return debugTables.get(version)
    const table = JSON.parse((await buildFor(version)).table)
    const { records } = classifyReal({ version, textureDir, jarPath, table })
    const worst = new Map() // block name -> its worst severity
    const rank = severity => SEVERITIES.indexOf(severity)
    for (const { name, severity } of records) if (!worst.has(name) || rank(severity) < rank(worst.get(name))) worst.set(name, severity)
    const text = JSON.stringify({ ...table, materials: table.materials.map(m => worst.has(m.name) ? { ...m, issue: worst.get(m.name) } : m) })
    debugTables.set(version, text)
    return text
  }

  const sendBlockIssues = async (res, world) => {
    scanner.track(world)
    const held = scanner.peek(world)
    const file = path.join(worldsDir(stateDir), world, 'view-block-issues.json')
    const stored = held ? null : await readJson(file)
    const payload = held ?? stored ?? await scanner.latest(world)
    if (!payload) return notFound(res)
    const doc = await readJson(path.join(worldsDir(stateDir), world, 'biomes.json'))
    send(res, 200, JSON.stringify(withBiomeIssues(world, biomeFallbackReason(doc) ? payload : withoutTintApproximate(payload))), { 'Content-Type': TYPES['.json'] })
  }

  // With the world's biome colours drawn (a usable biomes.json), "drawn with one fixed plains colour" is stale for every block.
  const withoutTintApproximate = payload => ({ ...payload, records: payload.records.filter(r => r.reason !== 'tint-approximate') })

  // Biome problems found while serving /biomes, once per world and name; merged into the /block-issues payload.
  const biomeIssues = new Map()
  const noteBiomeIssue = (world, name, detail) => {
    if (!biomeIssues.has(world)) biomeIssues.set(world, new Map())
    if (biomeIssues.get(world).has(name)) return
    biomeIssues.get(world).set(name, {
      name, reason: 'tint-approximate', severity: severityOf('tint-approximate'), drawnAs: 'fixed plains colour', detail,
      example: { stateId: 0, props: {} }, states: 0, seen: 0, firstSeen: null
    })
  }
  const withBiomeIssues = (world, payload) => {
    const extra = [...(biomeIssues.get(world)?.values() ?? [])]
    if (!extra.length) return payload
    const own = new Set(payload.records.map(r => `${r.name}\0${r.reason}`))
    return { ...payload, records: [...payload.records, ...extra.filter(r => !own.has(`${r.name}\0${r.reason}`))] }
  }

  // biome id -> colours for a world: its biomes.json (written by the body). Ids are never guessed from minecraft-data's order (a
  // server with extra biomes shifts every id above them), so a world without a usable biomes.json gets {fallback: true, colors: null}
  // and the page draws the fixed group colours. Cached per world and file mtime.
  const MAX_BIOMES = 255
  const biomeFallbackReason = doc => !doc?.biomes ? 'no biomes.json for this world' : doc.biomes.length > MAX_BIOMES ? `biomes.json lists ${doc.biomes.length} biomes, more than the ${MAX_BIOMES} ids the view holds` : null
  const biomeCache = new Map()
  const sendBiomes = async (res, world) => {
    const worldDir = path.join(worldsDir(stateDir), world)
    if (!(await statOrNull(worldDir))?.isDirectory()) return notFound(res)
    const file = path.join(worldDir, 'biomes.json')
    const stat = await statOrNull(file)
    const doc = stat ? await readJson(file) : null
    const reasonOf = biomeFallbackReason(doc)
    const answer = body => send(res, 200, JSON.stringify(body), { 'Content-Type': 'application/json' })
    if (reasonOf) {
      noteBiomeIssue(world, 'biome-registry', `${reasonOf}: biome tints are drawn as the fixed plains colours`)
      return answer({ mcVersion: doc?.mcVersion ?? null, source: null, names: [], colors: null, unknown: [], fallback: true, reason: reasonOf })
    }
    const version = doc.mcVersion
    if (!version || !VERSION.test(version)) return notFound(res)
    const key = `${world}\0${stat.mtimeMs}`
    if (!biomeCache.has(key)) {
      const names = []
      doc.biomes.forEach(b => { names[b.id] = b.name })
      const table = biomeTable(version, Array.from(names, n => n ?? ''), { jar: jarPath })
      biomeCache.set(key, JSON.stringify({ mcVersion: version, source: table.source, names: table.names, colors: Array.from(table.colors), unknown: table.unknown, fallback: false }))
    }
    const cached = JSON.parse(biomeCache.get(key))
    for (const name of cached.unknown) noteBiomeIssue(world, `biome:${name}`, `biome ${name} has no colour data: drawn with the plains colours`)
    send(res, 200, biomeCache.get(key), { 'Content-Type': 'application/json' })
  }

  const listAgents = async res => {
    // every body of every world; the world is where its folder is
    const poses = await Promise.all(listBodies(stateDir).map(async body => [body, await readJson(agentFile(body, 'pose.json'))]))
    const agents = poses.filter(([, pose]) => pose).map(([{ world, name }, pose]) => ({ name, world, status: pose.status, t: pose.t }))
    send(res, 200, JSON.stringify(agents), { 'Content-Type': 'application/json' })
  }

  const serveStatic = async (res, name) => {
    const type = TYPES[path.extname(name)]
    if (!type) return notFound(res)
    const file = path.join(webDir, name)
    const stat = await statOrNull(file)
    if (!stat?.isFile()) return name === VIEWER_BUNDLE ? send(res, 503, 'the viewer bundle web/cljs/viewer.mjs is not built: run `node tools/view/build-cljs.mjs` in the repo root', { 'Content-Type': 'text/plain' }) : notFound(res)
    send(res, 200, await fs.promises.readFile(file), { 'Content-Type': type })
  }

  // changed columns around the eye, as events; the first call only records the current mtimes
  // the returned poll checks every column in the window; poll.file(name) checks one (a directory watch event), for columns the poll has seen
  const columnWatcher = (world, emit) => {
    const seen = new Map()
    const check = async (cx, cz) => {
      const key = `${cx}.${cz}`
      const stat = await statOrNull(path.join(worldsDir(stateDir), world, 'chunks', `${key}.bin`))
      const mtime = stat?.mtimeMs ?? null
      const known = seen.has(key)
      const before = seen.get(key)
      seen.set(key, mtime)
      if (known && mtime !== null && mtime !== before) emit('column', { cx, cz, mtime })
    }
    const poll = async (eye, radius) => {
      const ccx = Math.floor(eye.x / 16)
      const ccz = Math.floor(eye.z / 16)
      for (let cx = ccx - radius; cx <= ccx + radius; cx++) {
        for (let cz = ccz - radius; cz <= ccz + radius; cz++) await check(cx, cz)
      }
    }
    poll.file = async name => {
      const match = COLUMN_FILE.exec(name ?? '')
      if (match && seen.has(`${match[1]}.${match[2]}`)) await check(Number(match[1]), Number(match[2]))
    }
    return poll
  }

  // watches one body's ({world, name}) pose, hud and nearby columns (first: its pose already read); calls send(event, data) for every change; returns stop()
  const watchAgent = (body, first, radius, send) => {
    let closed = false
    const emit = (event, data) => closed || send(event, data)
    let pose = first
    const watchColumns = columnWatcher(first.world, emit)

    const watchFile = (file, event, wrap, onValue) => {
      let last = null
      let running = Promise.resolve() // checks run one at a time, so a watch event and a poll never send the same mtime twice
      const check = async () => {
        const stat = await statOrNull(agentFile(body, file))
        if (!stat || stat.mtimeMs === last) return
        const value = await readJson(agentFile(body, file))
        if (!value) return
        last = stat.mtimeMs
        onValue?.(value)
        emit(event, wrap(stat.mtimeMs, value))
      }
      return () => (running = running.then(check))
    }
    const checkPose = watchFile('pose.json', 'pose', (mtime, value) => ({ mtime, sentAt: Date.now(), pose: value }), value => { pose = value })
    const checkHud = watchFile('hud.json', 'hud', (mtime, value) => ({ mtime, hud: value }))

    // a self-rescheduling loop never overlaps itself, and stops for good once the client is gone
    const loop = (ms, step) => {
      const tick = async () => {
        if (closed) return
        await step()
        if (!closed) timers.add(setTimeout(tick, ms))
      }
      tick()
    }
    const timers = new Set()
    // 'watch' reacts to the directory (pose.json is replaced by rename, so the file itself cannot be watched), with a
    // slow poll as the fallback; 'poll' only polls
    let watcher = null
    const startWatch = () => {
      if (watcher || closed || push !== 'watch') return
      try {
        watcher = fs.watch(path.dirname(agentFile(body, 'pose.json')), (_event, file) => {
          if (file === 'hud.json') return checkHud()
          if (file === 'pose.json') return checkPose()
        })
      } catch {
        return // no directory yet: the fallback poll tries again
      }
      watcher.on('error', () => {
        watcher?.close()
        watcher = null
      })
    }
    startWatch()
    loop(push === 'watch' ? watchFallbackMs : pollMs, async () => {
      startWatch()
      await checkPose()
      await checkHud()
    })
    loop(columnPollMs, async () => pose.eye && watchColumns(pose.eye, radius))
    // column files are replaced by rename too: react to the chunks directory, with the poll above as the fallback
    let chunkWatcher = null
    if (push === 'watch') {
      try {
        chunkWatcher = fs.watch(path.join(worldsDir(stateDir), first.world, 'chunks'), (_event, file) => watchColumns.file(file))
        chunkWatcher.on('error', () => chunkWatcher?.close())
      } catch {
        chunkWatcher = null // no directory yet: the poll covers it
      }
    }
    return () => {
      closed = true
      timers.forEach(clearTimeout)
      watcher?.close()
      chunkWatcher?.close()
    }
  }

  const sseHead = res => res.writeHead(200, { 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-cache', Connection: 'keep-alive' })
  const sseWrite = (res, event, data) => res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`)

  const streamAgent = async (req, res, body, radius) => {
    const first = await readJson(agentFile(body, 'pose.json'))
    if (!first) return notFound(res)
    scanner.track(first.world)
    sseHead(res)
    const stop = watchAgent(body, first, radius, (event, data) => sseWrite(res, event, data))
    const ping = setInterval(() => res.write(': ping\n\n'), PING_MS)
    req.on('close', () => {
      clearInterval(ping)
      stop()
    })
  }

  // one stream for several bodies (keys <world>/<name>); every event's data carries `agent`, the key. Bodies without a pose file are left out.
  const streamAgents = async (req, res, keys, radius) => {
    const firsts = await Promise.all(keys.map(async key => [key, await readJson(agentFile(bodyOfKey(key), 'pose.json'))]))
    sseHead(res)
    const stops = firsts.filter(([, first]) => first).map(([key, first]) => {
      scanner.track(first.world)
      return watchAgent(bodyOfKey(key), first, radius, (event, data) => sseWrite(res, event, { agent: key, ...data }))
    })
    const ping = setInterval(() => res.write(': ping\n\n'), PING_MS)
    req.on('close', () => {
      clearInterval(ping)
      stops.forEach(stop => stop())
    })
  }

  const route = async (req, res) => {
    const url = new URL(req.url, 'http://localhost')
    const parts = url.pathname.split('/').slice(1)
    const [head, ...rest] = parts
    const body = rest.length === 2 ? bodyOfKey(rest.join('/')) : null
    if (head === 'drive' && body) return driveProxy(req, res, body)
    if (req.method !== 'GET') return send(res, 405, 'method not allowed')
    if (url.pathname === '/') return serveStatic(res, 'index.html')
    if (head === 'web' && rest.length === 1 && WEB_FILE.test(rest[0])) return serveStatic(res, rest[0])
    if (head === 'web' && rest.length === 2 && rest[0] === 'cljs' && WEB_FILE.test(rest[1])) return serveStatic(res, `cljs/${rest[1]}`) // the :viewer build
    if (url.pathname === '/agents') return listAgents(res)
    if (head === 'pose' && body) {
      const radius = Math.min(32, Math.max(1, Number.parseInt(url.searchParams.get('radius') ?? '8', 10) || 8))
      return streamAgent(req, res, body, radius)
    }
    if (url.pathname === '/poses') {
      const keys = (url.searchParams.get('agents') ?? '').split(',')
      if (keys.length > MAX_STREAM_AGENTS || !keys.every(bodyOfKey)) return send(res, 400, `agents: 1 to ${MAX_STREAM_AGENTS} bodies as <world>/<name>, each of letters, digits, _ and -`, { 'Content-Type': 'text/plain' })
      const radius = Math.min(32, Math.max(1, Number.parseInt(url.searchParams.get('radius') ?? '8', 10) || 8))
      return streamAgents(req, res, keys, radius)
    }
    if (head === 'block-issues' && rest.length === 1 && NAME.test(rest[0])) return sendBlockIssues(res, rest[0])
    if (head === 'hud' && body) return sendFile(res, agentFile(body, 'hud.json'), TYPES['.json'])
    if (head === 'columns' && rest.length === 2 && NAME.test(rest[0]) && COLUMN_FILE.test(rest[1])) {
      return sendFile(res, path.join(worldsDir(stateDir), rest[0], 'chunks', rest[1]), 'application/octet-stream')
    }
    const versionOf = (dir, ext) => head === dir && rest.length === 1 ? new RegExp(`^([0-9.]+)\\.${ext}$`).exec(rest[0])?.[1] : null
    const tableVersion = versionOf('blocks', 'json')
    if (tableVersion && VERSION.test(tableVersion)) {
      const table = ['1', '2'].includes(url.searchParams.get('debug')) ? await debugTableFor(tableVersion) : (await buildFor(tableVersion)).table
      return send(res, 200, table, { 'Content-Type': 'application/json' })
    }
    const biomeWorld = head === 'biomes' && rest.length === 1 ? /^([A-Za-z0-9_-]+)\.json$/.exec(rest[0])?.[1] : null
    if (biomeWorld) return sendBiomes(res, biomeWorld)
    const textureVersion = versionOf('textures', 'bin')
    if (textureVersion && VERSION.test(textureVersion)) return send(res, 200, (await buildFor(textureVersion)).textures, { 'Content-Type': 'application/octet-stream' })
    const elementVersion = versionOf('elements', 'bin')
    if (elementVersion && VERSION.test(elementVersion)) return send(res, 200, (await buildFor(elementVersion)).elements, { 'Content-Type': 'application/octet-stream' })
    notFound(res)
  }

  const server = http.createServer((req, res) => {
    route(req, res).catch(error => {
      if (res.headersSent) return res.end()
      send(res, 500, String(error.message), { 'Content-Type': 'text/plain' })
    })
  })
  server.on('close', scanner.stop)
  return server
}
