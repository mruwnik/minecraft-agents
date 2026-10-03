// The control socket: HTTP framing only. Every rule, clock and piece of state lives in ClojureScript
// (engine.takeover over engine.lease); `handle(method, path, json)` returns { status, json }.
import fs from 'node:fs'
import http from 'node:http'

const MAX_SOCKET_PATH = 100
const MAX_BODY = 16 * 1024

const reply = (json, status) => ({ status, json })

const readJson = (req) => new Promise((resolve, reject) => {
  const chunks = []
  let size = 0
  req.on('data', (c) => {
    size += c.length
    if (size > MAX_BODY) return reject(Object.assign(new Error('body too large'), { tooLarge: true }))
    chunks.push(c)
  })
  req.on('end', () => {
    const text = Buffer.concat(chunks).toString('utf8')
    if (text === '') return resolve(null)
    try {
      resolve(JSON.parse(text))
    } catch {
      reject(new Error('bad json'))
    }
  })
  req.on('error', reject)
})

const send = (res, { status, json }) => {
  res.writeHead(status, { 'content-type': 'application/json' })
  res.end(JSON.stringify(json))
}

export function createControl ({ socketPath, handle }) {
  let server = null

  const onRequest = async (req, res) => {
    const parsed = await readJson(req).then(v => ({ v }), e => ({ e }))
    if (parsed.e?.tooLarge) {
      send(res, reply({ ok: false, reason: 'too-large' }, 413))
      req.destroy()
      return
    }
    if (parsed.e) return send(res, reply({ ok: false, reason: 'bad-json' }, 400))
    const path = (req.url ?? '').split('?')[0]
    send(res, await handle(req.method, path, parsed.v))
  }

  const listen = async () => {
    if (Buffer.byteLength(socketPath) > MAX_SOCKET_PATH) {
      throw new Error(`control socket path ${socketPath} is too long for a unix socket (over ${MAX_SOCKET_PATH} bytes)`)
    }
    fs.rmSync(socketPath, { force: true })
    server = http.createServer((req, res) => { onRequest(req, res).catch(() => res.destroy()) })
    await new Promise((resolve, reject) => {
      server.once('error', reject)
      server.listen(socketPath, resolve)
    })
    fs.chmodSync(socketPath, 0o600)
  }

  const close = async () => {
    if (!server) return
    const s = server
    server = null
    await new Promise(resolve => {
      s.close(() => resolve())
      s.closeAllConnections()
    })
    fs.rmSync(socketPath, { force: true })
  }

  return { listen, close }
}
