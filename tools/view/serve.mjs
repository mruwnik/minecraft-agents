// HTTP server for the browser view: static pages, the dumped column files, and a server-sent event stream that
// pushes an agent's pose, hud and changed columns. Reads state/ only; the body writes it atomically.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { textureBytes } from './materials.mjs'
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

export function createViewServer ({ stateDir, textureDir, webDir, pollMs = 50, columnPollMs = 250, push = 'watch', watchFallbackMs = 250 }) {
  const agentFile = (name, file) => path.join(stateDir, 'agents', name, 'view', file)
  const builds = new Map()
  // one build per version serves both the table and the texture bytes
  const buildFor = version => {
    if (builds.has(version)) return builds.get(version)
    const { table, textures } = textureBytes(version, textureDir)
    const build = { table: JSON.stringify({ ...table, format: columnFormat(version) }), textures: Buffer.from(textures.bytes) }
    builds.set(version, build)
    return build
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
    const ping = setInterval(() => res.write(': ping\n\n'), PING_MS)
    req.on('close', () => {
      closed = true
      clearInterval(ping)
      timers.forEach(clearTimeout)
      watcher?.close()
      chunkWatcher?.close()
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
    const versionOf = (dir, ext) => head === dir && rest.length === 1 ? new RegExp(`^([0-9.]+)\\.${ext}$`).exec(rest[0])?.[1] : null
    const tableVersion = versionOf('blocks', 'json')
    if (tableVersion && VERSION.test(tableVersion)) return send(res, 200, buildFor(tableVersion).table, { 'Content-Type': 'application/json' })
    const textureVersion = versionOf('textures', 'bin')
    if (textureVersion && VERSION.test(textureVersion)) return send(res, 200, buildFor(textureVersion).textures, { 'Content-Type': 'application/octet-stream' })
    notFound(res)
  }

  return http.createServer((req, res) => {
    route(req, res).catch(error => {
      if (res.headersSent) return res.end()
      send(res, 500, String(error.message), { 'Content-Type': 'text/plain' })
    })
  })
}
