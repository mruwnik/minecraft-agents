import fs from 'node:fs'
import path from 'node:path'
import { Worker } from 'node:worker_threads'

const workerFile = new URL('./thumbs-worker.mjs', import.meta.url)

// Pure: the name of the body to render next, or null. Bodies: {name, interested, poseMtimeMs, renderMtimeMs, renderedAt}.
export function pickNext (bodies, { now, minIntervalMs, budgetLeftMs }) {
  if (budgetLeftMs <= 0) return null
  const due = body =>
    body.interested &&
    body.poseMtimeMs != null &&
    (body.renderMtimeMs == null || body.poseMtimeMs > body.renderMtimeMs) &&
    (body.renderedAt == null || now - body.renderedAt >= minIntervalMs)
  const stale = body => body.renderedAt ?? -Infinity
  return bodies.filter(due).reduce((best, body) => (best && stale(best) <= stale(body) ? best : body), null)?.name ?? null
}

export class RenderError extends Error {
  constructor (message, name) {
    super(message)
    this.name = name
  }
}

// A renderer backed by one worker thread, started on first use; call close() to end it.
export function workerRenderer ({ stateDir, width, height, maxDist }) {
  let worker = null
  let nextId = 0
  const pending = new Map()
  const start = () => {
    worker = new Worker(workerFile)
    worker.unref()
    worker.on('message', ({ id, png, ms, error, errorName }) => {
      const { resolve, reject } = pending.get(id)
      pending.delete(id)
      if (error !== undefined) return reject(new RenderError(error, errorName))
      resolve({ png: Buffer.from(png.buffer, png.byteOffset, png.byteLength), ms })
    })
    const fail = err => {
      pending.forEach(({ reject }) => reject(err))
      pending.clear()
      worker = null
    }
    worker.on('error', fail)
    worker.on('exit', () => fail(new Error('thumbnail worker exited')))
  }
  const render = name => new Promise((resolve, reject) => {
    if (!worker) start()
    const id = nextId++
    pending.set(id, { resolve, reject })
    worker.postMessage({ id, name, stateDir, width, height, maxDist })
  })
  render.close = () => worker?.terminate()
  return render
}

const noRender = err => err.name === 'PoseError'

export function createThumbnailer ({ stateDir, width = 320, height = 180, maxDist = 48, minIntervalMs = 2000, budget = 0.25, windowMs = 10000, interestMs = 60000, tickMs = 250, now = Date.now, renderer }) {
  const render = renderer ?? workerRenderer({ stateDir, width, height, maxDist })
  const cache = new Map() // name -> {png, poseMtimeMs, renderedAt}
  const interest = new Map() // name -> last requested
  const inflight = new Map() // name -> promise
  const history = [] // {at, ms}
  let queue = 0
  let chain = Promise.resolve()
  let lastMs = null
  let totalMs = 0
  let totalRenders = 0

  const poseMtime = name => {
    try {
      return fs.statSync(path.join(stateDir, 'agents', name, 'view', 'pose.json')).mtimeMs
    } catch {
      return null
    }
  }

  const busyInWindow = () => {
    const since = now() - windowMs
    while (history.length && history[0].at <= since) history.shift()
    return history.reduce((sum, h) => sum + h.ms, 0)
  }

  const renderOnce = async name => {
    const poseMtimeMs = poseMtime(name)
    if (poseMtimeMs === null) return null
    try {
      const { png, ms } = await render(name)
      const renderedAt = now()
      history.push({ at: renderedAt, ms })
      lastMs = ms
      totalMs += ms
      totalRenders++
      const entry = { png, poseMtimeMs, renderedAt }
      cache.set(name, entry)
      return entry
    } catch (err) {
      if (noRender(err)) return null
      throw err
    }
  }

  // serialised: one render at a time, and one pending render per body
  const enqueue = name => {
    if (inflight.has(name)) return inflight.get(name)
    queue++
    const run = chain.then(() => renderOnce(name)).finally(() => {
      queue--
      inflight.delete(name)
    })
    chain = run.catch(() => {})
    inflight.set(name, run)
    return run
  }

  const get = async name => {
    interest.set(name, now())
    return cache.get(name) ?? enqueue(name)
  }

  const tick = async () => {
    if (queue > 0) return
    const t = now()
    const bodies = [...interest].map(([name, at]) => ({
      name,
      interested: t - at <= interestMs,
      poseMtimeMs: poseMtime(name),
      renderMtimeMs: cache.get(name)?.poseMtimeMs ?? null,
      renderedAt: cache.get(name)?.renderedAt ?? null
    }))
    const name = pickNext(bodies, { now: t, minIntervalMs, budgetLeftMs: budget * windowMs - busyInWindow() })
    if (name === null) return
    await enqueue(name).catch(() => {})
  }

  const timer = setInterval(() => tick(), tickMs)
  timer.unref()

  const stats = () => {
    const busyMsInWindow = busyInWindow()
    return {
      bodies: interest.size,
      rendersInWindow: history.length,
      busyMsInWindow,
      budgetMs: budget * windowMs,
      lastMs,
      meanMs: totalRenders ? totalMs / totalRenders : null,
      queue
    }
  }

  const close = () => {
    clearInterval(timer)
    render.close?.()
  }

  return { get, stats, close, tick }
}
