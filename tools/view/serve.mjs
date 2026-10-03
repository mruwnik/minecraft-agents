// HTTP server for the browser view: static pages, the dumped column files, and a server-sent event stream that
// pushes an agent's pose, hud and changed columns. Reads state/ only; the body writes it atomically.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { materialTable } from './materials.mjs'
import { columnFormat } from './web-format.mjs'

const NAME = /^[A-Za-z0-9_-]+$/
const VERSION = /^[0-9.]+$/
const COLUMN_FILE = /^(-?\d+)\.(-?\d+)\.bin$/
const TYPES = { '.html': 'text/html; charset=utf-8', '.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css; charset=utf-8', '.json': 'application/json; charset=utf-8' }
const PING_MS = 15000

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

export function createViewServer ({ stateDir, textureDir, webDir, pollMs = 50, columnPollMs = 250 }) {
  const agentFile = (name, file) => path.join(stateDir, 'agents', name, 'view', file)
  const tables = new Map()
  const tableFor = version => {
    if (!tables.has(version)) tables.set(version, JSON.stringify({ ...materialTable(version, textureDir), format: columnFormat(version) }))
    return tables.get(version)
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
  const columnWatcher = (world, emit) => {
    const seen = new Map()
    return async (eye, radius) => {
      const ccx = Math.floor(eye.x / 16)
      const ccz = Math.floor(eye.z / 16)
      for (let cx = ccx - radius; cx <= ccx + radius; cx++) {
        for (let cz = ccz - radius; cz <= ccz + radius; cz++) {
          const key = `${cx}.${cz}`
          const stat = await statOrNull(path.join(stateDir, 'worlds', world, 'chunks', `${key}.bin`))
          const mtime = stat?.mtimeMs ?? null
          const known = seen.has(key)
          const before = seen.get(key)
          seen.set(key, mtime)
          if (known && mtime !== null && mtime !== before) emit('column', { cx, cz, mtime })
        }
      }
    }
  }

  const streamAgent = async (req, res, name, radius) => {
    const first = await readJson(agentFile(name, 'pose.json'))
    if (!first) return notFound(res)
    res.writeHead(200, { 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-cache', Connection: 'keep-alive' })
    let closed = false
    const emit = (event, data) => closed || res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`)
    let pose = first
    const watchColumns = columnWatcher(first.world, emit)

    const watchFile = (file, event, wrap, onValue) => {
      let last = null
      return async () => {
        const stat = await statOrNull(agentFile(name, file))
        if (!stat || stat.mtimeMs === last) return
        const value = await readJson(agentFile(name, file))
        if (!value) return
        last = stat.mtimeMs
        onValue?.(value)
        emit(event, wrap(stat.mtimeMs, value))
      }
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
    loop(pollMs, async () => { await checkPose(); await checkHud() })
    loop(columnPollMs, async () => pose.eye && watchColumns(pose.eye, radius))
    const ping = setInterval(() => res.write(': ping\n\n'), PING_MS)
    req.on('close', () => {
      closed = true
      clearInterval(ping)
      timers.forEach(clearTimeout)
    })
  }

  const route = async (req, res) => {
    const url = new URL(req.url, 'http://localhost')
    if (req.method !== 'GET') return send(res, 405, 'method not allowed')
    const parts = url.pathname.split('/').slice(1)
    const [head, ...rest] = parts
    if (url.pathname === '/') return serveStatic(res, 'index.html')
    if (head === 'web' && rest.length === 1 && /^[A-Za-z0-9_.-]+$/.test(rest[0]) && !rest[0].startsWith('.')) return serveStatic(res, rest[0])
    if (url.pathname === '/agents') return listAgents(res)
    if (head === 'pose' && rest.length === 1 && NAME.test(rest[0])) {
      const radius = Math.min(32, Math.max(1, Number.parseInt(url.searchParams.get('radius') ?? '8', 10) || 8))
      return streamAgent(req, res, rest[0], radius)
    }
    if (head === 'hud' && rest.length === 1 && NAME.test(rest[0])) return sendFile(res, agentFile(rest[0], 'hud.json'), TYPES['.json'])
    if (head === 'columns' && rest.length === 2 && NAME.test(rest[0]) && COLUMN_FILE.test(rest[1])) {
      return sendFile(res, path.join(stateDir, 'worlds', rest[0], 'chunks', rest[1]), 'application/octet-stream')
    }
    const version = head === 'blocks' && rest.length === 1 ? /^([0-9.]+)\.json$/.exec(rest[0])?.[1] : null
    if (version && VERSION.test(version)) return send(res, 200, tableFor(version), { 'Content-Type': 'application/json' })
    notFound(res)
  }

  return http.createServer((req, res) => {
    route(req, res).catch(error => {
      if (res.headersSent) return res.end()
      send(res, 500, String(error.message), { 'Content-Type': 'text/plain' })
    })
  })
}
