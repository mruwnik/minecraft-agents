// HTTP server for the browser view: static pages, the dumped column files, and a server-sent event stream that
// pushes an agent's pose, hud and changed columns. Reads state/ (the body writes it atomically) and also proxies
// driving commands to a body's control socket.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { textureBytes } from './materials.mjs'
import { columnFormat } from './web-format.mjs'
import { createDriveProxy } from './drive-proxy.mjs'
import { SEVERITIES } from './block-issues.mjs'
import minecraftData from 'minecraft-data'
import { biomeTable } from './biome-colors.mjs'
import { createBlockScanner, classifyReal, findClientJar } from './block-scan.mjs'

const NAME = /^[A-Za-z0-9_-]+$/
const VERSION = /^[0-9.]+$/
const COLUMN_FILE = /^(-?\d+)\.(-?\d+)\.bin$/
const TYPES = { '.html': 'text/html; charset=utf-8', '.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css; charset=utf-8', '.json': 'application/json; charset=utf-8' }
const PING_MS = 15000
const MAX_STREAM_AGENTS = 32

const send = (res, status, body, headers = {}) => {
  res.writeHead(status, { 'Cache-Control': 'no-cache', ...headers })
  res.end(body)
}
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
  const agentFile = (name, file) => path.join(stateDir, 'agents', name, 'view', file)
  const builds = new Map()
  // one build per version serves both the table and the texture bytes
  const buildFor = version => {
    if (builds.has(version)) return builds.get(version)
    const { table, textures, elements } = textureBytes(version, textureDir, { jarPath })
    const build = { table: JSON.stringify({ ...table, format: columnFormat(version) }), textures: Buffer.from(textures.bytes), elements: elements ? Buffer.from(elements.buffer, elements.byteOffset, elements.byteLength) : Buffer.alloc(0) }
    builds.set(version, build)
    return build
  }

  // ?debug=1 or 2: the table with `issue` (the worst severity) on every material whose block the view draws wrong (tools/view/block-issues.mjs)
  const debugTables = new Map()
  const debugTableFor = version => {
    if (debugTables.has(version)) return debugTables.get(version)
    const table = JSON.parse(buildFor(version).table)
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
    const file = path.join(stateDir, 'worlds', world, 'view-block-issues.json')
    const stored = held ? null : await readJson(file)
    const payload = held ?? stored ?? await scanner.latest(world)
    if (!payload) return notFound(res)
    send(res, 200, JSON.stringify(payload), { 'Content-Type': TYPES['.json'] })
  }

  // biome id -> colours for a world: its biomes.json (written by the body) or, without one, minecraft-data's order for ?v=; cached per world and file mtime
  const biomeCache = new Map()
  const sendBiomes = async (res, world, requested) => {
    const worldDir = path.join(stateDir, 'worlds', world)
    if (!(await statOrNull(worldDir))?.isDirectory()) return notFound(res)
    const file = path.join(worldDir, 'biomes.json')
    const stat = await statOrNull(file)
    const doc = stat ? await readJson(file) : null
    const fallback = !doc?.biomes
    const version = fallback ? requested : doc.mcVersion
    if (!version || !VERSION.test(version)) return notFound(res)
    const key = `${world}\0${fallback ? `fallback:${version}` : stat.mtimeMs}`
    if (!biomeCache.has(key)) {
      const names = []
      if (fallback) minecraftData(version).biomesArray.forEach(b => { names[b.id] = b.name })
      else doc.biomes.forEach(b => { names[b.id] = b.name })
      const table = biomeTable(version, Array.from(names, n => n ?? ''), { jar: jarPath })
      biomeCache.set(key, JSON.stringify({ mcVersion: version, source: table.source, names: table.names, colors: Array.from(table.colors), unknown: table.unknown, fallback }))
    }
    send(res, 200, biomeCache.get(key), { 'Content-Type': 'application/json' })
  }

  const listAgents = async res => {
    const names = await fs.promises.readdir(path.join(stateDir, 'agents')).catch(() => [])
    const poses = await Promise.all(names.filter(n => NAME.test(n)).map(async name => [name, await readJson(agentFile(name, 'pose.json'))]))
    const agents = poses.filter(([, pose]) => pose).map(([name, pose]) => ({ name, world: pose.world, status: pose.status, t: pose.t }))
    send(res, 200, JSON.stringify(agents), { 'Content-Type': 'application/json' })
  }

  const serveStatic = async (res, name) => {
    const type = TYPES[path.extname(name)]
    if (!type) return notFound(res)
    const file = path.join(webDir, name)
    const stat = await statOrNull(file)
    if (!stat?.isFile()) return notFound(res)
    send(res, 200, await fs.promises.readFile(file), { 'Content-Type': type })
  }

  // changed columns around the eye, as events; the first call only records the current mtimes
  // the returned poll checks every column in the window; poll.file(name) checks one (a directory watch event), for columns the poll has seen
  const columnWatcher = (world, emit) => {
    const seen = new Map()
    const check = async (cx, cz) => {
      const key = `${cx}.${cz}`
      const stat = await statOrNull(path.join(stateDir, 'worlds', world, 'chunks', `${key}.bin`))
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

  // watches one agent's pose, hud and nearby columns (first: its pose already read); calls send(event, data) for every change; returns stop()
  const watchAgent = (name, first, radius, send) => {
    let closed = false
    const emit = (event, data) => closed || send(event, data)
    let pose = first
    const watchColumns = columnWatcher(first.world, emit)

    const watchFile = (file, event, wrap, onValue) => {
      let last = null
      let running = Promise.resolve() // checks run one at a time, so a watch event and a poll never send the same mtime twice
      const check = async () => {
        const stat = await statOrNull(agentFile(name, file))
        if (!stat || stat.mtimeMs === last) return
        const value = await readJson(agentFile(name, file))
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
        watcher = fs.watch(path.dirname(agentFile(name, 'pose.json')), (_event, file) => {
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
        chunkWatcher = fs.watch(path.join(stateDir, 'worlds', first.world, 'chunks'), (_event, file) => watchColumns.file(file))
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

  const streamAgent = async (req, res, name, radius) => {
    const first = await readJson(agentFile(name, 'pose.json'))
    if (!first) return notFound(res)
    scanner.track(first.world)
    sseHead(res)
    const stop = watchAgent(name, first, radius, (event, data) => sseWrite(res, event, data))
    const ping = setInterval(() => res.write(': ping\n\n'), PING_MS)
    req.on('close', () => {
      clearInterval(ping)
      stop()
    })
  }

  // one stream for several agents; every event's data carries `agent`. Agents without a pose file are left out.
  const streamAgents = async (req, res, names, radius) => {
    const firsts = await Promise.all(names.map(async name => [name, await readJson(agentFile(name, 'pose.json'))]))
    sseHead(res)
    const stops = firsts.filter(([, first]) => first).map(([name, first]) => {
      scanner.track(first.world)
      return watchAgent(name, first, radius, (event, data) => sseWrite(res, event, { agent: name, ...data }))
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
    if (head === 'drive' && rest.length === 1 && NAME.test(rest[0])) return driveProxy(req, res, rest[0])
    if (req.method !== 'GET') return send(res, 405, 'method not allowed')
    if (url.pathname === '/') return serveStatic(res, 'index.html')
    if (head === 'web' && rest.length === 1 && /^[A-Za-z0-9_.-]+$/.test(rest[0]) && !rest[0].startsWith('.')) return serveStatic(res, rest[0])
    if (url.pathname === '/agents') return listAgents(res)
    if (head === 'pose' && rest.length === 1 && NAME.test(rest[0])) {
      const radius = Math.min(32, Math.max(1, Number.parseInt(url.searchParams.get('radius') ?? '8', 10) || 8))
      return streamAgent(req, res, rest[0], radius)
    }
    if (url.pathname === '/poses') {
      const names = (url.searchParams.get('agents') ?? '').split(',')
      if (names.length > MAX_STREAM_AGENTS || !names.every(n => NAME.test(n))) return send(res, 400, `agents: 1 to ${MAX_STREAM_AGENTS} names of letters, digits, _ and -`, { 'Content-Type': 'text/plain' })
      const radius = Math.min(32, Math.max(1, Number.parseInt(url.searchParams.get('radius') ?? '8', 10) || 8))
      return streamAgents(req, res, names, radius)
    }
    if (head === 'block-issues' && rest.length === 1 && NAME.test(rest[0])) return sendBlockIssues(res, rest[0])
    if (head === 'hud' && rest.length === 1 && NAME.test(rest[0])) return sendFile(res, agentFile(rest[0], 'hud.json'), TYPES['.json'])
    if (head === 'columns' && rest.length === 2 && NAME.test(rest[0]) && COLUMN_FILE.test(rest[1])) {
      return sendFile(res, path.join(stateDir, 'worlds', rest[0], 'chunks', rest[1]), 'application/octet-stream')
    }
    const versionOf = (dir, ext) => head === dir && rest.length === 1 ? new RegExp(`^([0-9.]+)\\.${ext}$`).exec(rest[0])?.[1] : null
    const tableVersion = versionOf('blocks', 'json')
    if (tableVersion && VERSION.test(tableVersion)) {
      const table = ['1', '2'].includes(url.searchParams.get('debug')) ? debugTableFor(tableVersion) : buildFor(tableVersion).table
      return send(res, 200, table, { 'Content-Type': 'application/json' })
    }
    const biomeWorld = head === 'biomes' && rest.length === 1 ? /^([A-Za-z0-9_-]+)\.json$/.exec(rest[0])?.[1] : null
    if (biomeWorld) return sendBiomes(res, biomeWorld, url.searchParams.get('v'))
    const textureVersion = versionOf('textures', 'bin')
    if (textureVersion && VERSION.test(textureVersion)) return send(res, 200, buildFor(textureVersion).textures, { 'Content-Type': 'application/octet-stream' })
    const elementVersion = versionOf('elements', 'bin')
    if (elementVersion && VERSION.test(elementVersion)) return send(res, 200, buildFor(elementVersion).elements, { 'Content-Type': 'application/octet-stream' })
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
