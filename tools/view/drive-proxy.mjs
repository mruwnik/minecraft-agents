// Why JavaScript: thin adapter; relays view-page driving commands to a body's control socket for the JS view server.
// Relays driving commands from the view page to a body's control socket, guarded against cross-site posts
// and DNS rebinding (the page and its server are loopback only).
import http from 'node:http'
import path from 'node:path'
import { bodyDir } from '../../engine/js/bodies.mjs'

const MAX_BODY = 16 * 1024
const LOCAL_HOST = /^(127\.0\.0\.1|localhost|\[::1\]):\d+$/

const json = (res, status, body) => {
  res.writeHead(status, { 'Content-Type': 'application/json', 'Cache-Control': 'no-cache' })
  res.end(JSON.stringify(body))
}
const forbidden = (res, text) => json(res, 403, { ok: false, reason: 'forbidden', text })

const readBody = (req) => new Promise((resolve, reject) => {
  const chunks = []
  let size = 0
  req.on('data', (c) => {
    size += c.length
    if (size > MAX_BODY) return reject(Object.assign(new Error('body too large'), { tooLarge: true }))
    chunks.push(c)
  })
  req.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')))
  req.on('error', reject)
})

const toSocket = (socketPath, method, payload) => new Promise((resolve, reject) => {
  const headers = payload === null ? {} : { 'Content-Type': 'application/json', 'Content-Length': Buffer.byteLength(payload) }
  const req = http.request({ socketPath, method, path: '/drive', headers }, (res) => {
    const chunks = []
    res.on('data', c => chunks.push(c))
    res.on('end', () => resolve({ status: res.statusCode, text: Buffer.concat(chunks).toString('utf8') }))
  })
  req.on('error', reject)
  req.end(payload ?? undefined)
})

const guard = (req) => {
  const host = req.headers.host ?? ''
  if (!LOCAL_HOST.test(host)) return 'bad Host'
  if (req.headers.origin !== undefined && req.headers.origin !== `http://${host}`) return 'bad Origin'
  if (req.method === 'POST' && !(req.headers['content-type'] ?? '').startsWith('application/json')) return 'Content-Type must be application/json'
  return null
}

export function createDriveProxy ({ stateDir }) {
  return async (req, res, { world, name }) => {
    if (req.method !== 'GET' && req.method !== 'POST') return json(res, 405, { ok: false, reason: 'method-not-allowed' })
    const refused = guard(req)
    if (refused) return forbidden(res, refused)
    const socketPath = path.join(bodyDir(stateDir, world, name), 'engine', 'control.sock')
    let payload = null
    if (req.method === 'POST') {
      const raw = await readBody(req).catch(e => e)
      if (raw instanceof Error) return json(res, raw.tooLarge ? 413 : 400, { ok: false, reason: raw.tooLarge ? 'too-large' : 'bad-request' })
      let msg
      try {
        msg = JSON.parse(raw)
      } catch {
        return json(res, 400, { ok: false, reason: 'bad-json' })
      }
      if (msg === null || typeof msg !== 'object' || Array.isArray(msg)) return json(res, 400, { ok: false, reason: 'bad-request' })
      payload = JSON.stringify({ ...msg, who: msg.who ?? 'view' })
    }
    const reply = await toSocket(socketPath, req.method, payload).catch(() => null)
    if (!reply) return json(res, 503, { ok: false, reason: 'no-body', text: `no running body ${name} in world ${world}` })
    res.writeHead(reply.status, { 'Content-Type': 'application/json', 'Cache-Control': 'no-cache' })
    res.end(reply.text)
  }
}
