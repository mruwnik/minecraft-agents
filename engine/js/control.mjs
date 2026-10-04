// The control socket: HTTP framing only. Drive remains JSON for compatibility; world actions
// pass bounded EDN text through to the ClojureScript owner-token handler.
import fs from 'node:fs'
import http from 'node:http'

const MAX_SOCKET_PATH = 100
const MAX_BODY = 16 * 1024

const reply = (json, status) => ({ status, json })
const ednReply = (value, status) => ({ status, contentType: 'application/edn', text: `${value}\n` })

const readText = (req) => new Promise((resolve, reject) => {
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
    resolve(text)
  })
  req.on('error', reject)
})

const send = (res, result) => {
  if (result.text !== undefined) {
    res.writeHead(result.status, { 'content-type': result.contentType ?? 'application/edn' })
    res.end(result.text)
  } else {
    res.writeHead(result.status, { 'content-type': 'application/json' })
    res.end(JSON.stringify(result.json))
  }
}

export function createControl ({ socketPath, handle }) {
  let server = null

  const onRequest = async (req, res) => {
    const path = (req.url ?? '').split('?')[0]
    const edn = path === '/world'
    const parsed = await readText(req).then(v => ({ v }), e => ({ e }))
    if (parsed.e?.tooLarge) {
      send(res, edn ? ednReply('{:ok false :reason :too-large}', 413) : reply({ ok: false, reason: 'too-large' }, 413))
      req.destroy()
      return
    }
    if (parsed.e) return send(res, edn ? ednReply('{:ok false :reason :bad-body}', 400) : reply({ ok: false, reason: 'bad-body' }, 400))
    if (!edn) {
      let body = null
      try { body = parsed.v === '' ? null : JSON.parse(parsed.v) } catch {
        return send(res, reply({ ok: false, reason: 'bad-json' }, 400))
      }
      return send(res, await handle(req.method, path, body, req.headers['content-type'] ?? ''))
    }
    send(res, await handle(req.method, path, parsed.v, req.headers['content-type'] ?? ''))
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
