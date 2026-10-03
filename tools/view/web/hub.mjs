// Many scenes, one WebGL2 context: the dashboard's body cards. Every scene (scene.mjs) has its own block window (GL world) but
// they share one renderer, one decoder pool and one hidden canvas; a scene is rendered into the hidden canvas at its card's size
// and copied to the card's 2D canvas with drawImage, round-robin at `fps` within a per-frame time budget.
//
//   const hub = createViewHub({ maxScenes: 12, fps: 6, baseUrl: '' })
//   const scene = hub.addScene({ agent, radius: 2, fov: 70, interp: true })
//   scene.attach(canvas2d, { width: 320, height: 180, fps })   // redrawn while the canvas is visible (IntersectionObserver);
//     fps: a number (default: the hub's fps) or 'raf' (every animation frame, drawn before the cards and outside their budget)
//   scene.snapshot({ width, height }) -> Promise<ImageBitmap>
//   scene.stats() -> { fps, loaded, wanted, poseAge, status, gpuMs?, attaches: [{ width, height, fps, measuredFps, copyMs, copyMsP95, gpuMs? }] }
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
const STREAM_DEBOUNCE_MS = 500 // scenes added or closed within this window share one reopening of the /poses stream

// ---- scheduling (pure) ----

// The targets to render now: visible ones that are `raf` (every animation frame, first, all of them) or whose dueAt has passed
// (the longest-overdue first, ties keep their order, at most `max` of them).
export const dueScenes = (entries, now, max = Infinity) => {
  const visible = entries.filter(e => e.visible)
  const raf = visible.filter(e => e.raf)
  const rest = visible.filter(e => !e.raf && e.dueAt <= now).sort((a, b) => a.dueAt - b.dueAt).slice(0, max)
  return [...raf, ...rest].map(e => e.id)
}

// When a target rendered at `now` is due again: one period after it was due, so the rate stays exactly `fps` however the
// frames fall; a target that is more than a period behind is not caught up with, it restarts from now. 'raf': next frame.
export const nextDueAt = (dueAt, now, fps) => {
  if (fps === 'raf') return now
  const period = 1000 / fps
  const next = dueAt + period
  return next > now ? next : now + period
}

// One animation frame's renders. Every due raf target renders. The others render while the frame's total cost (raf ones included)
// is under budgetMs, so cards give way to a big view; but a card more than half a period late renders anyway, one per frame, so a
// slow big view slows the cards without starving them. run(id) renders one target and returns its cost in ms. Returns the ids rendered.
export const planFrame = ({ entries, now, budgetMs, run }) => {
  const byId = new Map(entries.map(e => [e.id, e]))
  const rendered = []
  let spent = 0
  let late = 0
  for (const id of dueScenes(entries, now)) {
    const e = byId.get(id)
    if (!e.raf && spent >= budgetMs) {
      const overdue = now - e.dueAt > 500 / e.fps
      if (!overdue || late) continue
      late++
    }
    spent += run(id)
    rendered.push(id)
  }
  return rendered
}

// The last pose and hud event of each agent seen on the shared stream. The stream reopens only when the set of agents changes, so a
// scene added later for an agent already streamed is fed these at once (replay) instead of waiting for a pose that may be far off.
export const eventCache = () => {
  const last = new Map() // `${event}|${agent}` -> data
  return {
    record: (event, data) => {
      if (event === 'pose' || event === 'hud') last.set(`${event}|${data.agent}`, data)
    },
    replay: (agent, feed) => {
      for (const event of ['pose', 'hud']) {
        const data = last.get(`${event}|${agent}`)
        if (data) feed(event, data)
      }
    },
    keepOnly: agents => {
      for (const key of [...last.keys()]) if (!agents.includes(last.get(key).agent)) last.delete(key)
    }
  }
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

const tryRenderer = canvas => {
  try {
    return createRenderer(canvas)
  } catch (error) {
    console.warn(`view hub: no WebGL2 (${error.message}); cards keep what they show`)
    return null
  }
}

// without WebGL2: a hub that reports supported: false and whose scenes do nothing (attach leaves the canvas as it is)
const unsupportedHub = () => ({
  supported: false,
  addScene: ({ agent }) => ({
    id: null,
    agent,
    attach: () => {},
    detach: () => {},
    snapshot: () => Promise.reject(new Error('WebGL2 is not available')),
    stats: () => ({ fps: 0, loaded: 0, wanted: 0, poseAge: null, status: 'unsupported', attaches: [] }),
    ready: () => false,
    close: () => {}
  }),
  stats: () => ({ supported: false, scenes: {} }),
  probeGpu: () => ({}),
  close: () => {}
})

export const createViewHub = (options = {}) => {
  const hidden = document.createElement('canvas')
  const renderer = tryRenderer(hidden)
  return renderer ? createSupportedHub(options, hidden, renderer) : unsupportedHub()
}

const createSupportedHub = ({ maxScenes = 12, fps = 6, baseUrl = '', frameBudgetMs = 8, debugLevel = 0 }, hidden, renderer) => {
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

  // The hidden canvas is as big as the largest attach, resized only when the set of attach sizes changes (a resize reallocates the
  // drawing buffer and clears it); smaller targets are drawn into its bottom-left corner.
  const allTargets = () => [...entries.values()].flatMap(e => [...e.targets.values()])
  const fitHidden = () => {
    const targets = allTargets()
    if (!targets.length) return
    const width = Math.max(...targets.map(t => t.width))
    const height = Math.max(...targets.map(t => t.height))
    if (hidden.width !== width) hidden.width = width
    if (hidden.height !== height) hidden.height = height
  }
  // only a snapshot bigger than every attach grows it past that
  const growHidden = (width, height) => {
    if (hidden.width < width) hidden.width = width
    if (hidden.height < height) hidden.height = height
  }

  // draws the scene's params at width x height into the bottom-left of the hidden canvas, timed by a GPU query when there is one
  const drawInto = (entry, params, width, height, target = null) => {
    growHidden(width, height)
    timedDraw({ gl, timer, draw: () => renderer.draw(entry.scene.world, { ...params, width, height }), entry, target })
  }

  const timedDraw = ({ gl: context, timer: ext, draw, entry, target }) => {
    const query = ext ? context.createQuery() : null
    if (query) context.beginQuery(ext.TIME_ELAPSED_EXT, query)
    draw()
    if (!query) return
    context.endQuery(ext.TIME_ELAPSED_EXT)
    pendingQueries.push({ gl: context, timer: ext, query, entry, target })
  }

  const pollQueries = () => {
    const waiting = []
    for (const item of pendingQueries) {
      const { gl: context, timer: ext, query, entry, target } = item
      if (!context.getQueryParameter(query, context.QUERY_RESULT_AVAILABLE)) {
        waiting.push(item)
        continue
      }
      const disjoint = context.getParameter(ext.GPU_DISJOINT_EXT)
      const ms = context.getQueryParameter(query, context.QUERY_RESULT) / 1e6
      context.deleteQuery(query)
      if (disjoint) continue
      entry.gpuMs = entry.gpuMs === undefined ? ms : entry.gpuMs + GPU_SMOOTHING * (ms - entry.gpuMs)
      if (target) target.gpuMs = target.gpuMs === undefined ? ms : target.gpuMs + GPU_SMOOTHING * (ms - target.gpuMs)
    }
    pendingQueries.length = 0
    pendingQueries.push(...waiting)
  }

  const placeholder = target => {
    const ctx = target.canvas.getContext('2d')
    ctx.fillStyle = PLACEHOLDER
    ctx.fillRect(0, 0, target.width, target.height)
  }

  // WebGL's origin is bottom-left, the 2D canvas's top-left: the corner the viewport drew is the hidden canvas's last `height` rows
  const copyTo = target => {
    const started = performance.now()
    target.canvas.getContext('2d').drawImage(hidden, 0, hidden.height - target.height, target.width, target.height, 0, 0, target.width, target.height)
    keep(target.copyMs, performance.now() - started, FRAME_KEEP)
  }

  // renders one target now (its scene's params are sampled once per animation frame); returns the ms it cost
  const renderTarget = (target, now, frameParams, touched) => {
    const started = performance.now()
    const { entry } = target
    if (!frameParams.has(entry)) frameParams.set(entry, entry.scene.frame(now))
    const params = frameParams.get(entry)
    if (params) {
      drawInto(entry, params, target.width, target.height, target)
      copyTo(target)
    } else placeholder(target)
    touched.add(entry)
    keep(target.renders, now, FRAME_KEEP)
    return performance.now() - started
  }

  const tick = now => {
    if (closed) return
    raf = requestAnimationFrame(tick)
    pollQueries()
    const started = performance.now()
    const targets = allTargets()
    const frameParams = new Map()
    const touched = new Set()
    const rendered = planFrame({
      entries: targets,
      now,
      budgetMs: frameBudgetMs,
      run: id => {
        const target = targets.find(t => t.id === id)
        try {
          return renderTarget(target, now, frameParams, touched)
        } catch (error) {
          console.error(`render of ${target.entry.scene.agent} failed:`, error)
          return 0
        } finally {
          target.dueAt = nextDueAt(target.dueAt, now, target.fps)
        }
      }
    })
    for (const entry of touched) {
      const ms = performance.now() - started
      entry.scene.drew()
      keep(entry.renders, now, FRAME_KEEP)
      entry.renderCount++
      keep(entry.renderMs, ms, FRAME_KEEP)
    }
    if (rendered.length) keep(frameCosts, performance.now() - started, FRAME_KEEP)
  }

  // ONE /poses stream for all scenes (a browser holds only 6 HTTP/1.1 connections per origin); reopened, debounced, when the set
  // of agents or the radius changes. Its events carry `agent` and go to every scene of that agent.
  const replay = eventCache()
  let stream = null // {key, source}
  let streamTimer = null
  const syncStream = () => {
    const agents = [...new Set([...entries.values()].map(e => e.scene.agent))].sort()
    const radius = Math.max(0, ...[...entries.values()].map(e => e.scene.radius))
    const key = `${agents.join(',')}|${radius}`
    if (stream?.key === key || (!agents.length && !stream)) return
    stream?.source.close()
    stream = null
    replay.keepOnly(agents)
    if (!agents.length) return
    const source = new EventSource(`${baseUrl}/poses?agents=${agents.map(encodeURIComponent).join(',')}&radius=${radius}`)
    stream = { key, source }
    for (const event of ['pose', 'hud', 'column']) {
      source.addEventListener(event, e => {
        const data = JSON.parse(e.data)
        replay.record(event, data)
        for (const entry of entries.values()) if (entry.scene.agent === data.agent) entry.scene.feed(event, data)
      })
    }
    source.onerror = () => console.warn('the /poses stream was interrupted; the browser will retry')
  }
  const scheduleStream = () => {
    clearTimeout(streamTimer)
    streamTimer = setTimeout(syncStream, STREAM_DEBOUNCE_MS)
  }

  const fpsOf = (entry, now) => entry.renders.filter(t => now - t <= FPS_WINDOW_MS).length

  const validFps = value => value === 'raf' || (Number.isFinite(value) && value > 0)

  const attachStats = (target, now) => {
    const copyMs = percentile(target.copyMs, 0.5)
    const copyP95 = percentile(target.copyMs, 0.95)
    return {
      width: target.width,
      height: target.height,
      fps: target.fps,
      measuredFps: target.renders.filter(t => now - t <= FPS_WINDOW_MS).length,
      copyMs: copyMs === null ? null : Math.round(copyMs * 1000) / 1000,
      copyMsP95: copyP95 === null ? null : Math.round(copyP95 * 1000) / 1000,
      ...(target.gpuMs === undefined ? {} : { gpuMs: Math.round(target.gpuMs * 100) / 100 })
    }
  }

  let attachCount = 0
  const addScene = ({ agent, radius = 2, fov = 70, interp = true }) => {
    if (entries.size >= maxScenes) throw new Error(`the hub holds at most ${maxScenes} scenes`)
    const scene = createScene({ agent, radius, fov, interp, renderer, decoder, baseUrl, debugLevel, ownStream: false })
    const entry = { scene, targets: new Map(), renders: [], renderCount: 0, renderMs: [], gpuMs: undefined }
    entries.set(scene.id, entry)
    replay.replay(agent, scene.feed)
    scheduleStream()

    const detach = canvas => {
      observer?.unobserve(canvas)
      delete canvas.__hubTarget
      if (entry.targets.delete(canvas)) fitHidden()
    }
    const addTarget = (canvas, { width, height, fps: targetFps }) => {
      if (!validFps(targetFps)) throw new Error(`attach fps must be a positive number or 'raf', got ${targetFps}`)
      detach(canvas)
      canvas.width = width
      canvas.height = height
      const stagger = (attachCount++ % maxScenes) * 1000 / fps / maxScenes
      const target = { id: `${scene.id}:${attachCount}`, entry, canvas, width, height, fps: targetFps, raf: targetFps === 'raf', visible: observer === null, dueAt: performance.now() + stagger, renders: [], copyMs: [], gpuMs: undefined }
      canvas.__hubTarget = target
      entry.targets.set(canvas, target)
      observer?.observe(canvas)
      return target
    }
    const attach = (canvas, { width = 320, height = 180, fps: targetFps = fps } = {}) => {
      const target = addTarget(canvas, { width, height, fps: targetFps })
      fitHidden()
      placeholder(target)
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

    const attachesOf = () => [...entry.targets.values()].map(t => attachStats(t, performance.now()))
    const stats = () => ({ fps: fpsOf(entry, performance.now()), ...scene.stats(), attaches: attachesOf(), ...(entry.gpuMs === undefined ? {} : { gpuMs: Math.round(entry.gpuMs * 100) / 100 }) })

    const close = () => {
      for (const canvas of [...entry.targets.keys()]) detach(canvas)
      entries.delete(scene.id)
      fitHidden()
      scene.close()
      scheduleStream()
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
    scenes: Object.fromEntries([...entries.values()].map(e => [e.scene.agent, { ...e.scene.stats(), fps: fpsOf(e, performance.now()), attaches: [...e.targets.values()].map(t => attachStats(t, performance.now())), renderMsP50: percentile(e.renderMs, 0.5), renders: e.renderCount, ...(e.gpuMs === undefined ? {} : { gpuMs: e.gpuMs }) }])),
    frameCostMs: { n: frameCosts.length, p50: percentile(frameCosts, 0.5), p95: percentile(frameCosts, 0.95), max: frameCosts.length ? Math.max(...frameCosts) : null },
    memory: renderer.memory()
  })

  const close = () => {
    closed = true
    clearTimeout(streamTimer)
    stream?.source.close()
    stream = null
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
  return { supported: true, addScene, stats, probeGpu, close }
}
