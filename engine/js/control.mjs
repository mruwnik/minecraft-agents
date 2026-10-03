import fs from 'node:fs'
import http from 'node:http'

const CONTROLS = ['forward', 'back', 'left', 'right', 'jump', 'sneak', 'sprint']
const LOOK_KEYS = ['yaw', 'pitch', 'dyaw', 'dpitch']
const MAX_SOCKET_PATH = 100
const MAX_BODY = 16 * 1024
const MAX_MS = 10000

const allFalse = () => Object.fromEntries(CONTROLS.map(c => [c, false]))
const reply = (json, status = 200) => ({ status, json })
const refuse = (reason, extra = {}) => reply({ ok: false, reason, ...extra })
const badArgs = (text) => refuse('bad-args', { text })
const isObject = (v) => v !== null && typeof v === 'object' && !Array.isArray(v)

function validateSet (req) {
  const { controls, look, ms } = req
  if (controls !== undefined) {
    if (!isObject(controls)) return 'controls must be an object'
    const bad = Object.entries(controls).find(([k, v]) => !CONTROLS.includes(k) || typeof v !== 'boolean')
    if (bad) return `bad control ${bad[0]}: names are ${CONTROLS.join('/')} with boolean values`
  }
  if (look !== undefined) {
    if (!isObject(look)) return 'look must be {yaw, pitch} or {dyaw, dpitch}'
    const entries = Object.entries(look)
    const bad = entries.find(([k, v]) => !LOOK_KEYS.includes(k) || typeof v !== 'number' || !Number.isFinite(v))
    if (bad || entries.length === 0) return 'look must be {yaw, pitch} or {dyaw, dpitch} with numbers'
  }
  if (ms !== undefined && !(Number.isInteger(ms) && ms >= 1 && ms <= MAX_MS)) return `ms must be an integer 1..${MAX_MS}`
  return null
}

export function createControl ({ socketPath, body, releaseMs = 1000, idleMs = 60000, checkMs = 250, now = Date.now }) {
  let lease = null
  let server = null
  let timer = null

  const manualView = () => lease && {
    who: lease.who,
    why: lease.why,
    since: lease.since,
    controls: { ...lease.controls },
    yaw: lease.yaw,
    pitch: lease.pitch
  }

  const touch = () => {
    lease.lastOp = now()
    lease.deadmanFired = false
  }

  const endTakeover = (reason) => {
    const heldMs = now() - lease.since
    const holder = lease.who
    lease = null
    body.release({ who: holder, reason, heldMs })
  }

  const check = (req) => {
    if (!lease) return refuse('not-taken')
    if (req.who !== lease.who) return refuse('not-driver', { holder: lease.who })
    return null
  }

  const take = (req) => {
    if (typeof req.who !== 'string' || req.who === '') return refuse('bad-args', { text: 'who is required' })
    if (lease && lease.who === req.who) {
      touch()
      return reply({ ok: true, manual: manualView() })
    }
    if (lease) return refuse(`held-by ${lease.who}`)
    const st = body.status()
    if (st.offline) return refuse('offline')
    if (st.settling) return refuse('settling')
    const why = typeof req.why === 'string' ? req.why : ''
    const res = body.take({ who: req.who, why })
    if (!res.ok) return refuse(res.reason)
    lease = {
      who: req.who,
      why,
      since: now(),
      controls: allFalse(),
      deadlines: new Map(),
      yaw: null,
      pitch: null,
      lastOp: now(),
      deadmanFired: false
    }
    return reply({ ok: true, manual: manualView() })
  }

  const set = (req) => {
    const err = validateSet(req)
    if (err) return badArgs(err)
    touch()
    const named = Object.keys(req.controls ?? {})
    const driven = body.drive({ controls: req.controls, look: req.look })
    Object.assign(lease.controls, req.controls)
    named.forEach(c => lease.deadlines.delete(c))
    if (req.ms !== undefined) named.filter(c => req.controls[c]).forEach(c => lease.deadlines.set(c, now() + req.ms))
    lease.yaw = driven.yaw
    lease.pitch = driven.pitch
    return reply({ ok: true, manual: manualView(), pos: driven.pos })
  }

  const stop = () => {
    touch()
    body.stopDriving()
    lease.controls = allFalse()
    lease.deadlines.clear()
    return reply({ ok: true, manual: manualView(), pos: body.status().pos })
  }

  const ping = () => {
    touch()
    return reply({ ok: true, manual: manualView(), pos: body.status().pos })
  }

  const release = (req) => {
    const forced = req.who !== lease.who
    endTakeover(forced ? 'forced' : 'released')
    return reply({ ok: true, manual: null })
  }

  const status = () => {
    const st = body.status()
    return reply({ ok: true, manual: manualView(), pos: st.pos, offline: st.offline, settling: st.settling })
  }

  const withDriver = (fn) => (req) => check(req) ?? fn(req)

  const releaseOp = (req) => {
    if (!lease) return refuse('not-taken')
    if (req.who !== lease.who && req.force !== true) return refuse('not-driver', { holder: lease.who })
    return release(req)
  }

  const ops = {
    take,
    set: withDriver(set),
    stop: withDriver(stop),
    ping: withDriver(ping),
    release: releaseOp
  }

  const handle = async ({ method, path, body: payload }) => {
    if (path !== '/drive') return reply({ ok: false, reason: 'not-found' }, 404)
    if (method === 'GET') return status()
    if (method !== 'POST') return reply({ ok: false, reason: 'not-found' }, 404)
    if (!isObject(payload) || !Object.hasOwn(ops, payload.op)) return reply({ ok: false, reason: 'bad-request', text: 'body must be an object with a known op' }, 400)
    return ops[payload.op](payload)
  }

  const tick = () => {
    if (!lease) return
    if (body.status().offline) {
      endTakeover('offline')
      return
    }
    const t = now()
    const silent = t - lease.lastOp
    if (silent >= idleMs) {
      endTakeover('idle')
      return
    }
    for (const [c, deadline] of [...lease.deadlines]) {
      if (t < deadline) continue
      lease.deadlines.delete(c)
      lease.controls[c] = false
      body.drive({ controls: { [c]: false } })
    }
    const untimedHeld = CONTROLS.some(c => lease.controls[c] && !lease.deadlines.has(c))
    if (!untimedHeld || silent < releaseMs || lease.deadmanFired) return
    lease.deadmanFired = true
    body.stopDriving()
    body.deadman({ who: lease.who, silentMs: silent })
    lease.controls = allFalse()
    lease.deadlines.clear()
  }

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

  const send = (res, { status: code, json }) => {
    res.writeHead(code, { 'content-type': 'application/json' })
    res.end(JSON.stringify(json))
  }

  const onRequest = async (req, res) => {
    const parsed = await readJson(req).then(v => ({ v }), e => ({ e }))
    if (parsed.e?.tooLarge) {
      send(res, reply({ ok: false, reason: 'too-large' }, 413))
      req.destroy()
      return
    }
    if (parsed.e) return send(res, reply({ ok: false, reason: 'bad-json' }, 400))
    const path = (req.url ?? '').split('?')[0]
    send(res, await handle({ method: req.method, path, body: parsed.v }))
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
    timer = setInterval(tick, checkMs)
    timer.unref()
  }

  const close = async () => {
    clearInterval(timer)
    if (lease) endTakeover('shutdown')
    if (!server) return
    const s = server
    server = null
    await new Promise(resolve => {
      s.close(() => resolve())
      s.closeAllConnections()
    })
    fs.rmSync(socketPath, { force: true })
  }

  return { listen, close, state: manualView, handle, tick }
}
