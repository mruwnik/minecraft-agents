// One scene of the browser view: follows an agent's pose over server-sent events, keeps a toroidal window of chunk columns
// (fetch, decode on the shared pool, upload into its own GL world) and says where the camera is. It does not draw: the page
// (app.mjs) or the hub (hub.mjs) calls frame(now) and draws the returned params with renderer.draw(scene.world, params).
import { cameraBasis } from './camera.mjs'
import { poseInterpolator } from './interp.mjs'
import { skyDarken, sceneTime } from './shading.mjs'
import { tablesFor } from './tables.mjs'

const MAX_IN_FLIGHT = 8 // column fetches at once, per scene
const MAX_DECODING = 12 // columns fetched and waiting for a decode, per scene
const UPLOAD_BUDGET_MS = 4 // GPU uploads per frame, at least one
const COLUMN_DRAWN_KEEP = 500
const LATENCY_KEEP = 200
const TRACE_KEEP = 4000
const DECODE_KEEP = 2000
const TIMING_KEEP = 2000
const OFFLINE_LATENCY_MS = 60000 // an older file is an offline pose, not a live write

export const mod = (a, n) => ((a % n) + n) % n
export const keyOf = (cx, cz) => `${cx}.${cz}`
export const slotKey = (cx, cz, n) => `${mod(cx, n)}.${mod(cz, n)}`

// The window of (2 radius + 1)^2 columns around the eye's chunk. Columns already owned by their slot are kept (same object);
// the rest get a fresh {cx, cz, dist, status: 'pending'} and take the slot over (owners, slot -> column key, is updated in place).
// Returns the new columns map and the fresh columns, whose slots the caller clears and refills.
export const moveWindow = ({ ccx, ccz, radius, columns: previous, owners }) => {
  const n = 2 * radius + 1
  const columns = new Map()
  const fresh = []
  for (let cx = ccx - radius; cx <= ccx + radius; cx++) {
    for (let cz = ccz - radius; cz <= ccz + radius; cz++) {
      const key = keyOf(cx, cz)
      const slot = slotKey(cx, cz, n)
      const known = previous.get(key)
      if (known && owners.get(slot) === key) {
        columns.set(key, known)
        continue
      }
      owners.set(slot, key)
      const column = { cx, cz, dist: 0, status: 'pending' }
      columns.set(key, column)
      fresh.push(column)
    }
  }
  for (const column of columns.values()) column.dist = Math.hypot(column.cx - ccx, column.cz - ccz)
  return { columns, fresh }
}

// ---- the decoder pool is shared, so its job keys carry the scene id; the pool asks here which job is nearest ----
const live = new Map() // scene id -> () => columns map
let nextSceneId = 1
const decodeKey = (id, key) => `${id}|${key}`
export const decodePriority = jobKey => {
  const at = jobKey.indexOf('|')
  return live.get(jobKey.slice(0, at))?.().get(jobKey.slice(at + 1))?.dist ?? Infinity
}

const entityColor = e => {
  if (e.type === 'player' || e.kind === 'player' || e.username) return [0.2, 0.4, 0.95]
  if (e.type === 'hostile' || e.kind === 'Hostile mobs') return [0.88, 0.14, 0.14]
  if (e.type === 'animal' || e.type === 'passive' || e.kind === 'Passive mobs' || e.kind === 'Animals') return [0.55, 0.38, 0.22]
  if (e.type === 'item' || e.name === 'item' || e.kind === 'Drops') return [1, 0.88, 0.16]
  return [0.5, 0.5, 0.5]
}

const entityBoxes = (entities, origin, eye) => (entities ?? [])
  .map(e => ({ e, d: Math.hypot(e.pos.x - eye.x, e.pos.y - eye.y, e.pos.z - eye.z) }))
  .sort((a, b) => a.d - b.d)
  .slice(0, 64)
  .map(({ e }) => {
    const half = (e.width ?? 0.6) / 2
    const x = e.pos.x - origin.x
    const y = e.pos.y - origin.y
    const z = e.pos.z - origin.z
    return { min: [x - half, y, z - half], max: [x + half, y + (e.height ?? 1.8), z + half], color: entityColor(e) }
  })

const keepTiming = (list, ms) => {
  list.push(Math.round(ms * 10) / 10)
  if (list.length > TIMING_KEEP) list.shift()
}

const keep = (list, value) => {
  list.push(Math.round(value))
  if (list.length > LATENCY_KEEP) list.shift()
}

// options: agent, radius, fov, interp (boolean), renderer, decoder (createDecoder with priority: decodePriority), baseUrl (prefix of
// /pose /columns /blocks ...), debugLevel (0..2, the table's issue marks), maxDist, urlParams (?time / ?rain overrides of shading.mjs),
// finishForLatency (gl.finish before stamping latencies: the single-view page's measurement; leave off for many scenes),
// ownStream (default true: the scene opens /pose/<agent> itself; false: whoever owns a shared /poses stream passes each event of
// this agent to feed(event, data), as the hub does, because a browser allows only 6 HTTP/1.1 connections per origin)
export const createScene = ({ agent, radius = 2, fov = 70, interp: interpOn = true, renderer, decoder, baseUrl = '', debugLevel = 0, maxDist = radius * 16, urlParams = new URLSearchParams(), finishForLatency = false, ownStream = true }) => {
  const id = String(nextSceneId++)
  const N = 2 * radius + 1
  const interp = poseInterpolator()
  const tables = tablesFor(renderer, { decoder, baseUrl, debugLevel })
  const world = renderer.createWorld()
  const metrics = { fps: 0, frames: 0, latencies: [], shownLatencies: [], camTrace: [], underruns: 0, decodeMs: [], lightMs: [], uploadMs: [], columnDrawn: [], retargetMs: [], slowUploads: [], mainDecode: { ms: 0, columns: 0 }, loaded: 0, wanted: 0, ready: false }

  const state = {
    pose: null,
    hud: null,
    table: null, // {materialOf, materials, format}; null until loaded, and for a scene whose version the renderer refused
    dims: null, // {height, minY}
    ccx: null,
    ccz: null,
    owners: new Map(), // slot "sx.sz" -> column key
    columns: new Map(), // wanted column key -> {cx, cz, dist, status: 'pending'|'loaded'|'missing'}
    needs: new Map(), // column key -> sequence of the request that would satisfy it
    inFlight: new Set(), // columns being fetched
    decoding: new Map(), // column key -> sequence of the fetch whose bytes are queued or being decoded
    uploads: new Map(), // column key -> {column, result, mtime} decoded and waiting for the GPU
    eventMtimes: new Map(), // column key -> mtime of the latest column event not yet fetched
    seq: 0,
    pendingMtime: null,
    pendingShown: [], // {mtime, t} of poses not yet displayed
    drawn: null, // the pose sampled for the current frame
    uploaded: [], // {key, mtime} uploaded by the current frame
    uploadCount: 0,
    closed: false,
    source: null
  }
  live.set(id, () => state.columns)

  const fetchColumn = async (worldName, cx, cz) => {
    const res = await fetch(`${baseUrl}/columns/${worldName}/${cx}.${cz}.bin`)
    if (res.status === 404) return null
    if (!res.ok) throw new Error(`column ${cx}.${cz}: HTTP ${res.status}`)
    return new Uint8Array(await res.arrayBuffer())
  }

  const ensureDims = header => {
    if (state.dims) return
    state.dims = { height: header.worldHeight, minY: header.minY }
    world.allocate(N, header.worldHeight)
  }

  // a decoded column waits here for the frame's upload step
  const settle = (key, column, result, mtime) => {
    if (state.owners.get(slotKey(column.cx, column.cz, N)) !== key) return // the slot changed hands while fetching
    if (!result) {
      column.status = 'missing'
      return
    }
    state.uploads.set(key, { column, result, mtime })
  }

  // uploads the nearest decoded columns until the frame's budget is spent; returns the {key, mtime} drawn by this frame
  const uploadStep = () => {
    const started = performance.now()
    const uploaded = []
    const nearest = () => [...state.uploads.entries()].sort(([, a], [, b]) => a.column.dist - b.column.dist)[0]
    for (let next = nearest(); next && (!uploaded.length || performance.now() - started < UPLOAD_BUDGET_MS); next = nearest()) {
      const [key, { column, result, mtime }] = next
      state.uploads.delete(key)
      if (state.columns.get(key) !== column || state.owners.get(slotKey(column.cx, column.cz, N)) !== key) continue
      ensureDims(result.header)
      const uploadStarted = performance.now()
      world.uploadColumn(mod(column.cx, N), mod(column.cz, N), result.mats, result.flags, result.light, result.biomes)
      const uploadMs = performance.now() - uploadStarted
      state.uploadCount++
      keepTiming(metrics.uploadMs, uploadMs)
      if (uploadMs > UPLOAD_BUDGET_MS) metrics.slowUploads.push({ ms: Math.round(uploadMs * 10) / 10, nthInFrame: uploaded.length, queued: state.uploads.size, total: state.uploadCount })
      column.status = 'loaded'
      if (mtime !== undefined) uploaded.push({ key, mtime })
    }
    return uploaded
  }

  const startFetch = async key => {
    const column = state.columns.get(key)
    const seq = state.needs.get(key)
    const mtime = state.eventMtimes.get(key)
    state.eventMtimes.delete(key)
    state.inFlight.add(key)
    const bytes = await fetchColumn(state.pose.world, column.cx, column.cz).catch(error => {
      console.error(`column ${key}:`, error)
      return null
    })
    state.inFlight.delete(key)
    if (state.closed) return
    if (state.needs.get(key) === seq) state.needs.delete(key)
    if (state.columns.get(key) !== column) return pump()
    if (!bytes) {
      settle(key, column, null, mtime)
      return pump()
    }
    state.decoding.set(key, seq)
    pump()
    const result = await decoder.decode(decodeKey(id, key), bytes)
    if (state.closed || state.decoding.get(key) !== seq) return // cancelled, or replaced by a newer fetch of the same column
    state.decoding.delete(key)
    if (result) {
      metrics.decodeMs.push(Math.round(result.ms * 10) / 10)
      keepTiming(metrics.lightMs, result.lightMs)
      metrics.mainDecode.ms += result.mainMs
      metrics.mainDecode.columns++
      if (metrics.decodeMs.length > DECODE_KEEP) metrics.decodeMs.shift()
    }
    if (state.columns.get(key) === column) settle(key, column, result, mtime)
    pump()
  }

  const pump = () => {
    while (!state.closed && state.inFlight.size < MAX_IN_FLIGHT && state.decoding.size < MAX_DECODING && state.table) {
      const next = [...state.needs.keys()]
        .filter(key => !state.inFlight.has(key) && state.columns.has(key))
        .sort((a, b) => state.columns.get(a).dist - state.columns.get(b).dist)[0]
      if (!next) return
      startFetch(next)
    }
  }

  // the window follows the eye's chunk; a slot whose owner changes is zeroed at once and refilled when its fetch lands
  const retarget = (ccx, ccz) => {
    state.ccx = ccx
    state.ccz = ccz
    const { columns, fresh } = moveWindow({ ccx, ccz, radius, columns: state.columns, owners: state.owners })
    for (const column of fresh) {
      if (state.dims) world.clearSlot(mod(column.cx, N), mod(column.cz, N))
      state.needs.set(keyOf(column.cx, column.cz), ++state.seq)
    }
    for (const key of state.needs.keys()) if (!columns.has(key)) state.needs.delete(key)
    for (const key of [...state.decoding.keys()]) {
      if (columns.has(key)) continue
      state.decoding.delete(key)
      decoder.cancel(decodeKey(id, key))
    }
    for (const key of [...state.uploads.keys()]) if (!columns.has(key)) state.uploads.delete(key)
    for (const key of [...state.eventMtimes.keys()]) if (!columns.has(key)) state.eventMtimes.delete(key)
    state.columns = columns
    pump()
  }

  const onColumnEvent = ({ cx, cz, mtime }) => {
    const key = keyOf(cx, cz)
    if (!state.columns.has(key)) return
    state.eventMtimes.set(key, mtime)
    state.needs.set(key, ++state.seq)
    pump()
  }

  // the world's biome colours, once per scene; without them the world keeps the fixed group colours
  const loadBiomes = async worldName => {
    const body = await fetch(`${baseUrl}/biomes/${worldName}.json`).then(r => r.ok ? r.json() : null).catch(() => null)
    if (state.closed) return
    if (!world.setBiomes(body)) console.warn(`biomes: no colour table for world ${worldName}${body?.reason ? ` (${body.reason})` : ''}: fixed tint colours`)
  }

  const onPose = async ({ mtime, pose }) => {
    state.pose = pose
    state.pendingMtime = mtime
    interp.push(pose, Date.now())
    if (pose.eye) state.pendingShown.push({ mtime, t: pose.t })
    if (!pose.eye) return
    if (!state.table) {
      const table = await tables.ensure(pose.mcVersion)
      if (state.closed || !table) return
      state.table = table
      loadBiomes(pose.world)
    }
    const ccx = Math.floor(pose.eye.x / 16)
    const ccz = Math.floor(pose.eye.z / 16)
    if (ccx === state.ccx && ccz === state.ccz) return
    const started = performance.now()
    retarget(ccx, ccz)
    keepTiming(metrics.retargetMs, performance.now() - started)
  }

  // one decoded event of the agent's stream: 'pose', 'hud' or 'column'
  const feed = (event, data) => {
    if (state.closed) return
    if (event === 'pose') return onPose(data).catch(error => console.error('pose:', error))
    if (event === 'hud') return void (state.hud = data.hud)
    if (event === 'column') onColumnEvent(data)
  }

  const connect = () => {
    const source = new EventSource(`${baseUrl}/pose/${encodeURIComponent(agent)}?radius=${radius}`)
    state.source = source
    for (const event of ['pose', 'hud', 'column']) source.addEventListener(event, e => feed(event, JSON.parse(e.data)))
    source.onerror = () => console.warn(`event stream of ${agent} interrupted; the browser will retry`)
  }

  const settled = () => {
    if (!state.pose || !state.dims) return false
    if (state.inFlight.size || state.needs.size || state.decoding.size || state.uploads.size) return false
    return state.columns.size > 0
  }

  const refreshCounts = () => {
    metrics.loaded = [...state.columns.values()].filter(c => c.status === 'loaded').length
    metrics.wanted = state.columns.size
    metrics.ready = settled()
  }

  const recordTrace = (ts, cam) => {
    metrics.camTrace.push({ ts, x: cam.eye.x, y: cam.eye.y, z: cam.eye.z, yaw: cam.yaw })
    if (metrics.camTrace.length > TRACE_KEEP) metrics.camTrace.shift()
  }

  // Uploads what is decoded, samples the pose and returns the params for renderer.draw(world, params) (add width and height), or
  // null when there is nothing to draw yet. camera: {eye, yaw, pitch} replaces the pose's camera (the free camera).
  const frame = (nowMs, { camera = null } = {}) => {
    state.uploaded = uploadStep()
    if (!state.pose?.eye || !state.dims || state.ccx === null) return null
    const shown = (interpOn && interp.sample(Date.now())) || state.pose
    state.drawn = shown
    metrics.underruns = interp.underruns()
    metrics.delay = interp.delay()
    const cam = camera ?? { eye: shown.eye, yaw: shown.yaw, pitch: shown.pitch }
    if (!camera) recordTrace(nowMs, cam)
    const origin = { x: (state.ccx - radius) * 16, y: state.dims.minY, z: (state.ccz - radius) * 16 }
    const { time, rain } = sceneTime({ timeOfDay: shown.timeOfDay ?? state.pose.timeOfDay, rain: shown.rain ?? state.pose.rain }, urlParams)
    return {
      eye: { x: cam.eye.x - origin.x, y: cam.eye.y - origin.y, z: cam.eye.z - origin.z },
      basis: cameraBasis({ yaw: cam.yaw, pitch: cam.pitch, fov }),
      dist: maxDist,
      darken: skyDarken(time, rain),
      slotOff: { x: mod(state.ccx - radius, N) * 16, z: mod(state.ccz - radius, N) * 16 },
      entities: entityBoxes(shown.entities ?? state.pose.entities, origin, cam.eye)
    }
  }

  // columns refetched after a file change count as drawn once the frame that uploaded them has been drawn; a pose counts as
  // displayed once the playback body time has reached its t (at once with interpolation off). Call after the frame was drawn.
  const drew = () => {
    refreshCounts()
    const drawnAt = Date.now()
    for (const { key, mtime } of state.uploaded) metrics.columnDrawn.push({ key, mtime, drawnAt })
    while (metrics.columnDrawn.length > COLUMN_DRAWN_KEEP) metrics.columnDrawn.shift()
    state.uploaded = []
    if (state.pendingMtime === null && !state.pendingShown.length) return
    if (finishForLatency) renderer.finish()
    const wall = Date.now()
    if (state.pendingMtime !== null) {
      const latency = wall - state.pendingMtime
      state.pendingMtime = null
      if (latency <= OFFLINE_LATENCY_MS) keep(metrics.latencies, latency)
    }
    const playhead = interpOn ? interp.playhead(wall) : Infinity
    const due = state.pendingShown.filter(p => p.t <= playhead)
    state.pendingShown = state.pendingShown.filter(p => p.t > playhead)
    for (const { mtime } of due) if (wall - mtime <= OFFLINE_LATENCY_MS) keep(metrics.shownLatencies, wall - mtime)
  }

  const stats = () => {
    refreshCounts()
    const pose = state.pose
    return { loaded: metrics.loaded, wanted: metrics.wanted, poseAge: pose ? Date.now() - pose.t : null, status: pose?.status ?? 'connecting' }
  }

  const close = () => {
    if (state.closed) return
    state.closed = true
    state.source?.close()
    for (const key of state.decoding.keys()) decoder.cancel(decodeKey(id, key))
    live.delete(id)
    world.dispose()
  }

  if (ownStream) connect()
  return {
    id, agent, radius, fov, world, metrics, interp, frame, drew, stats, close,
    pose: () => state.pose,
    hud: () => state.hud,
    counts: () => ({ inFlight: state.inFlight.size, decoding: state.decoding.size, uploads: state.uploads.size, needs: state.needs.size }),
    isReady: () => state.dims !== null && state.ccx !== null,
    feed
  }
}
