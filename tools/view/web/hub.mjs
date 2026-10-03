// Many scenes, one WebGL2 context: the dashboard's body cards. Every scene (scene.mjs) has its own block window (GL world) but
// they share one renderer, one decoder pool and one hidden canvas; a scene is rendered into the hidden canvas at its card's size
// and copied to the card's 2D canvas with drawImage, round-robin at `fps` within a per-frame time budget.
//
//   const hub = createViewHub({ maxScenes: 12, fps: 6, baseUrl: '' })
//   const scene = hub.addScene({ agent, radius: 2, fov: 70, interp: true })
//   scene.attach(canvas2d, { width: 320, height: 180 })   // redrawn while the canvas is visible (IntersectionObserver)
//   scene.snapshot({ width, height }) -> Promise<ImageBitmap>
//   scene.stats() -> { fps, loaded, wanted, poseAge, status, gpuMs? }
//   scene.detach(canvas2d); scene.close(); hub.close()
//
// window.__hub (set by hub-demo.html) exposes hub.stats() for measurement.
import { createDecoder } from './decoder.mjs'
import { createRenderer } from './gl.mjs'
import { createScene, decodePriority } from './scene.mjs'

const FPS_WINDOW_MS = 1000
const FRAME_KEEP = 600
const GPU_SMOOTHING = 0.2 // weight of the newest timer-query result in the running gpuMs
const PLACEHOLDER = '#1a1a1d'
const SNAPSHOT_UPLOAD_ROUNDS = 40
const AGENTS_POLL_MS = 3000 // how often agents whose stream was dropped (offline) are checked for coming back

// ---- scheduling (pure) ----

// The scenes to render now: visible ones whose dueAt has passed, the longest-overdue first (ties keep their order), at most `max`.
export const dueScenes = (entries, now, max = Infinity) => entries
  .filter(e => e.visible && e.dueAt <= now)
  .sort((a, b) => a.dueAt - b.dueAt)
  .slice(0, max)
  .map(e => e.id)

// When a scene rendered at `now` is due again: one period after it was due, so the rate stays exactly `fps` however the
// frames fall; a scene that is more than a period behind is not caught up with, it restarts from now.
export const nextDueAt = (dueAt, now, fps) => {
  const period = 1000 / fps
  const next = dueAt + period
  return next > now ? next : now + period
}

const percentile = (values, p) => {
  if (!values.length) return null
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.min(sorted.length - 1, Math.floor(p * sorted.length))]
}

const keep = (list, value, max) => {
  list.push(value)
  if (list.length > max) list.shift()
}

export const createViewHub = ({ maxScenes = 12, fps = 6, baseUrl = '', frameBudgetMs = 8, debugLevel = 0 } = {}) => {
  const hidden = document.createElement('canvas')
  const renderer = createRenderer(hidden)
  renderer.setDebug(debugLevel > 0)
  const gl = renderer.gl
  const decoder = createDecoder({ priority: decodePriority })
  const timer = gl.getExtension('EXT_disjoint_timer_query_webgl2')
  const entries = new Map() // scene id -> entry
  const frameCosts = [] // main-thread ms of the animation frames that rendered something
  const pendingQueries = [] // {query, entry}
  let closed = false
  let raf = 0

  const observer = typeof IntersectionObserver === 'function'
    ? new IntersectionObserver(changes => {
      for (const { target, isIntersecting } of changes) {
        if (target.__hubTarget) target.__hubTarget.visible = isIntersecting
      }
    })
    : null

  const growHidden = (width, height) => {
    if (hidden.width < width) hidden.width = width
    if (hidden.height < height) hidden.height = height
  }

  // draws the scene's params at width x height into the bottom-left of the hidden canvas, timed by a GPU query when there is one
  const drawInto = (entry, params, width, height) => {
    growHidden(width, height)
    const query = timer ? gl.createQuery() : null
    if (query) gl.beginQuery(timer.TIME_ELAPSED_EXT, query)
    renderer.draw(entry.scene.world, { ...params, width, height })
    if (!query) return
    gl.endQuery(timer.TIME_ELAPSED_EXT)
    pendingQueries.push({ query, entry })
  }

  const pollQueries = () => {
    const disjoint = timer && gl.getParameter(timer.GPU_DISJOINT_EXT)
    while (pendingQueries.length && gl.getQueryParameter(pendingQueries[0].query, gl.QUERY_RESULT_AVAILABLE)) {
      const { query, entry } = pendingQueries.shift()
      const ms = gl.getQueryParameter(query, gl.QUERY_RESULT) / 1e6
      gl.deleteQuery(query)
      if (disjoint) continue
      entry.gpuMs = entry.gpuMs === undefined ? ms : entry.gpuMs + GPU_SMOOTHING * (ms - entry.gpuMs)
    }
  }

  const placeholder = target => {
    const ctx = target.canvas.getContext('2d')
    ctx.fillStyle = PLACEHOLDER
    ctx.fillRect(0, 0, target.width, target.height)
  }

  const renderEntry = (entry, now) => {
    const started = performance.now()
    const targets = [...entry.targets.values()].filter(t => t.visible)
    const params = entry.scene.frame(now)
    for (const target of targets) {
      if (!params) {
        placeholder(target)
        continue
      }
      drawInto(entry, params, target.width, target.height)
      target.canvas.getContext('2d').drawImage(hidden, 0, hidden.height - target.height, target.width, target.height, 0, 0, target.width, target.height)
    }
    entry.scene.drew()
    keep(entry.renders, now, FRAME_KEEP)
    keep(entry.renderMs, performance.now() - started, FRAME_KEEP)
  }

  const tick = now => {
    if (closed) return
    raf = requestAnimationFrame(tick)
    pollQueries()
    const started = performance.now()
    const visible = [...entries.values()].map(e => ({ id: e.scene.id, visible: [...e.targets.values()].some(t => t.visible), dueAt: e.dueAt }))
    let rendered = 0
    for (const id of dueScenes(visible, now)) {
      if (rendered && performance.now() - started > frameBudgetMs) break
      const entry = entries.get(id)
      try {
        renderEntry(entry, now)
      } catch (error) {
        console.error(`render of ${entry.scene.agent} failed:`, error)
      }
      entry.dueAt = nextDueAt(entry.dueAt, now, fps)
      rendered++
    }
    if (rendered) keep(frameCosts, performance.now() - started, FRAME_KEEP)
  }

  // a scene whose agent went offline dropped its stream; reopen it when the agent is online again or wrote a newer pose
  const reviveStreams = async () => {
    const dropped = [...entries.values()].filter(e => !e.scene.streaming())
    if (!dropped.length) return
    const agents = await fetch(`${baseUrl}/agents`).then(r => r.json()).catch(() => [])
    for (const { name, status, t } of agents) {
      const entry = dropped.find(e => e.scene.agent === name)
      if (entry && (status !== 'offline' || t > (entry.scene.pose()?.t ?? 0))) entry.scene.reconnect()
    }
  }
  const revive = setInterval(reviveStreams, AGENTS_POLL_MS)

  const fpsOf = (entry, now) => entry.renders.filter(t => now - t <= FPS_WINDOW_MS).length

  const addScene = ({ agent, radius = 2, fov = 70, interp = true }) => {
    if (entries.size >= maxScenes) throw new Error(`the hub holds at most ${maxScenes} scenes`)
    const scene = createScene({ agent, radius, fov, interp, renderer, decoder, baseUrl, debugLevel, closeWhenOffline: true })
    const entry = { scene, targets: new Map(), dueAt: performance.now() + (entries.size % maxScenes) * 1000 / fps / maxScenes, renders: [], renderMs: [], gpuMs: undefined }
    entries.set(scene.id, entry)

    const attach = (canvas, { width = 320, height = 180 } = {}) => {
      canvas.width = width
      canvas.height = height
      const target = { canvas, width, height, visible: observer === null }
      canvas.__hubTarget = target
      entry.targets.set(canvas, target)
      observer?.observe(canvas)
      placeholder(target)
    }
    const detach = canvas => {
      observer?.unobserve(canvas)
      delete canvas.__hubTarget
      entry.targets.delete(canvas)
    }

    // renders the scene now at width x height; waits no longer than the columns already decoded
    const snapshot = async ({ width = 320, height = 180 } = {}) => {
      const now = performance.now()
      let params = null
      for (let round = 0; round < SNAPSHOT_UPLOAD_ROUNDS; round++) {
        params = scene.frame(now)
        if (!scene.counts().uploads) break
      }
      if (!params) throw new Error(`scene of ${agent} has nothing to draw yet`)
      drawInto(entry, params, width, height)
      const bitmap = await createImageBitmap(hidden, 0, hidden.height - height, width, height)
      scene.drew()
      return bitmap
    }

    const stats = () => ({ fps: fpsOf(entry, performance.now()), ...scene.stats(), ...(entry.gpuMs === undefined ? {} : { gpuMs: Math.round(entry.gpuMs * 100) / 100 }) })

    const close = () => {
      for (const canvas of [...entry.targets.keys()]) detach(canvas)
      entries.delete(scene.id)
      scene.close()
    }
    const ready = () => {
      scene.stats()
      return scene.metrics.ready
    }
    return { id: scene.id, agent, attach, detach, snapshot, stats, ready, close }
  }

  // for measurement: each scene drawn `rounds` times at width x height with gl.finish after every draw; median ms per scene (agent -> ms)
  const probeGpu = ({ width = 320, height = 180, rounds = 7 } = {}) => Object.fromEntries([...entries.values()].map(entry => {
    const params = entry.scene.frame(performance.now())
    if (!params) return [entry.scene.agent, null]
    const times = Array.from({ length: rounds }, () => {
      const started = performance.now()
      drawInto(entry, params, width, height)
      gl.finish()
      return performance.now() - started
    })
    return [entry.scene.agent, Math.round(percentile(times, 0.5) * 100) / 100]
  }))

  // measurement numbers of the whole hub: per-scene stats, main-thread cost of the frames that rendered, GPU memory
  const stats = () => ({
    renderer: renderer.renderer,
    gpuTimer: timer !== null,
    scenes: Object.fromEntries([...entries.values()].map(e => [e.scene.agent, { ...e.scene.stats(), fps: fpsOf(e, performance.now()), renderMsP50: percentile(e.renderMs, 0.5), ...(e.gpuMs === undefined ? {} : { gpuMs: e.gpuMs }) }])),
    frameCostMs: { n: frameCosts.length, p50: percentile(frameCosts, 0.5), p95: percentile(frameCosts, 0.95), max: frameCosts.length ? Math.max(...frameCosts) : null },
    memory: renderer.memory()
  })

  const close = () => {
    closed = true
    clearInterval(revive)
    cancelAnimationFrame(raf)
    for (const entry of [...entries.values()]) {
      for (const canvas of [...entry.targets.keys()]) observer?.unobserve(canvas)
      entry.scene.close()
    }
    entries.clear()
    observer?.disconnect()
    gl.getExtension('WEBGL_lose_context')?.loseContext()
  }

  raf = requestAnimationFrame(tick)
  return { addScene, stats, probeGpu, close }
}
