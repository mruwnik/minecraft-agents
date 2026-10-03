// The browser view: follows an agent's pose over server-sent events, keeps a toroidal window of chunk columns in the
// GPU, and draws it every frame (see gl.mjs). window.__view exposes numbers for automated measurement.
import { cameraBasis, directionFor } from './camera.mjs'
import { createDecoder } from './decoder.mjs'
import { createRenderer } from './gl.mjs'
import { poseInterpolator } from './interp.mjs'
import { skyDarken, sceneTime } from './shading.mjs'

const MAX_IN_FLIGHT = 8 // column fetches at once
const MAX_DECODING = 12 // columns fetched and waiting for a decode
const UPLOAD_BUDGET_MS = 4 // GPU uploads per frame, at least one
const COLUMN_DRAWN_KEEP = 500
const LATENCY_KEEP = 200
const TRACE_KEEP = 4000
const DECODE_KEEP = 2000
const TIMING_KEEP = 2000
const MOUSE_SENSITIVITY = 0.0022
const FREE_SPEED = 12

const params = new URLSearchParams(location.search)
const numberParam = (name, fallback) => {
  const value = Number(params.get(name))
  return params.has(name) && Number.isFinite(value) && value > 0 ? value : fallback
}
const agentName = params.get('agent')
const radius = Math.min(32, Math.round(numberParam('radius', 8)))
const fov = numberParam('fov', 70)
const maxDist = numberParam('dist', radius * 16)
const fixedWidth = params.has('w') ? Math.round(numberParam('w', 0)) : null
const fixedHeight = params.has('h') ? Math.round(numberParam('h', 0)) : null
const interpOn = params.get('interp') !== '0'
// ?debug=1 checkers blocks the view draws missing or wrong, ?debug=2 also the approximate ones (tools/view/block-issues.mjs)
const debugLevel = { 1: 1, 2: 2 }[params.get('debug')] ?? 0
const debugOn = debugLevel > 0
const ISSUE_LEVELS = ['missing', 'wrong', 'approximate']
const interp = poseInterpolator()
const N = 2 * radius + 1

const canvas = document.getElementById('view')
const overlay = document.getElementById('overlay')
const picker = document.getElementById('agents')
const freeButton = document.getElementById('free')

const mod = (a, n) => ((a % n) + n) % n
const percentile = (values, p) => {
  if (!values.length) return null
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.min(sorted.length - 1, Math.floor(p * sorted.length))]
}

const view = window.__view = { fps: 0, frames: 0, latencies: [], shownLatencies: [], camTrace: [], underruns: 0, decodeMs: [], lightMs: [], uploadMs: [], columnDrawn: [], retargetMs: [], slowUploads: [], mainDecode: { ms: 0, columns: 0 }, loaded: 0, wanted: 0, ready: false, renderer: null }

const state = {
  pose: null,
  poseMtime: null,
  hud: null,
  table: null, // {materialOf, materials, format}
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
  free: null, // {eye, yaw, pitch} while the free camera is on
  pendingMtime: null,
  pendingShown: [], // {mtime, t} of poses not yet displayed
  drawn: null, // the pose sampled for the current frame
  frameTimes: [],
  keys: new Set()
}

const gfx = createRenderer(canvas)
gfx.setDebug(debugOn)
const decoder = createDecoder({
  makeWorker: params.get('worker') === '0' ? null : undefined, // ?worker=0 decodes on the main thread
  priority: key => state.columns.get(key)?.dist ?? Infinity
})
view.renderer = gfx.renderer

// ---- block table and columns ----

const decodeBase64U16 = text => {
  const binary = atob(text)
  const bytes = Uint8Array.from(binary, c => c.charCodeAt(0))
  return new Uint16Array(bytes.buffer)
}

const loadTable = async version => {
  const [res, texRes] = await Promise.all([fetch(`/blocks/${version}.json${debugOn ? `?debug=${debugLevel}` : ''}`), fetch(`/textures/${version}.bin`)])
  if (!res.ok) throw new Error(`blocks table for ${version}: HTTP ${res.status}`)
  if (!texRes.ok) throw new Error(`textures for ${version}: HTTP ${texRes.status}`)
  const [json, bytes] = await Promise.all([res.json(), texRes.arrayBuffer()])
  state.table = { materialOf: decodeBase64U16(json.materialOf), materials: json.materials, format: json.format }
  decoder.setTable({ format: json.format, materialOf: state.table.materialOf })
  gfx.setMaterials(json.materials.map(m => ({ ...m, issue: ISSUE_LEVELS.indexOf(m.issue) >= 0 && ISSUE_LEVELS.indexOf(m.issue) <= debugLevel })))
  gfx.setTextures({ bytes: new Uint8Array(bytes), layers: json.textures.names.length, size: json.textures.size, levels: json.textures.levels })
}

const keepTiming = (list, ms) => {
  list.push(Math.round(ms * 10) / 10)
  if (list.length > TIMING_KEEP) list.shift()
}

const fetchColumn = async (world, cx, cz) => {
  const res = await fetch(`/columns/${world}/${cx}.${cz}.bin`)
  if (res.status === 404) return null
  if (!res.ok) throw new Error(`column ${cx}.${cz}: HTTP ${res.status}`)
  return new Uint8Array(await res.arrayBuffer())
}

const keyOf = (cx, cz) => `${cx}.${cz}`
const slotKey = (cx, cz) => `${mod(cx, N)}.${mod(cz, N)}`

const ensureDims = header => {
  if (state.dims) return
  state.dims = { height: header.worldHeight, minY: header.minY }
  gfx.allocate(N, header.worldHeight)
}

// a decoded column waits here for the frame's upload step
const settle = (key, column, result, mtime) => {
  if (state.owners.get(slotKey(column.cx, column.cz)) !== key) return // the slot changed hands while fetching
  if (!result) {
    column.status = 'missing'
    return
  }
  state.uploads.set(key, { column, result, mtime })
}

// uploads the nearest decoded columns until the frame's budget is spent; returns the {key, mtime} drawn by this frame
let uploadCount = 0
const uploadStep = () => {
  const started = performance.now()
  const uploaded = []
  const nearest = () => [...state.uploads.entries()].sort(([, a], [, b]) => a.column.dist - b.column.dist)[0]
  for (let next = nearest(); next && (!uploaded.length || performance.now() - started < UPLOAD_BUDGET_MS); next = nearest()) {
    const [key, { column, result, mtime }] = next
    state.uploads.delete(key)
    if (state.columns.get(key) !== column || state.owners.get(slotKey(column.cx, column.cz)) !== key) continue
    ensureDims(result.header)
    const uploadStarted = performance.now()
    gfx.uploadColumn(mod(column.cx, N), mod(column.cz, N), result.mats, result.flags, result.light)
    const uploadMs = performance.now() - uploadStarted
    uploadCount++
    keepTiming(view.uploadMs, uploadMs)
    if (uploadMs > UPLOAD_BUDGET_MS) view.slowUploads.push({ ms: Math.round(uploadMs * 10) / 10, nthInFrame: uploaded.length, queued: state.uploads.size, total: uploadCount })
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
  if (state.needs.get(key) === seq) state.needs.delete(key)
  if (state.columns.get(key) !== column) return pump()
  if (!bytes) {
    settle(key, column, null, mtime)
    return pump()
  }
  state.decoding.set(key, seq)
  pump()
  const result = await decoder.decode(key, bytes)
  if (state.decoding.get(key) !== seq) return // cancelled, or replaced by a newer fetch of the same column
  state.decoding.delete(key)
  if (result) {
    view.decodeMs.push(Math.round(result.ms * 10) / 10)
    keepTiming(view.lightMs, result.lightMs)
    view.mainDecode.ms += result.mainMs
    view.mainDecode.columns++
    if (view.decodeMs.length > DECODE_KEEP) view.decodeMs.shift()
  }
  if (state.columns.get(key) === column) settle(key, column, result, mtime)
  pump()
}

const pump = () => {
  while (state.inFlight.size < MAX_IN_FLIGHT && state.decoding.size < MAX_DECODING && state.table) {
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
  const columns = new Map()
  for (let cx = ccx - radius; cx <= ccx + radius; cx++) {
    for (let cz = ccz - radius; cz <= ccz + radius; cz++) {
      const key = keyOf(cx, cz)
      const slot = slotKey(cx, cz)
      const known = state.columns.get(key)
      if (known && state.owners.get(slot) === key) {
        columns.set(key, known)
        continue
      }
      if (state.dims) gfx.clearSlot(mod(cx, N), mod(cz, N))
      state.owners.set(slot, key)
      columns.set(key, { cx, cz, dist: Math.hypot(cx - ccx, cz - ccz), status: 'pending' })
      state.needs.set(key, ++state.seq)
    }
  }
  for (const column of columns.values()) column.dist = Math.hypot(column.cx - ccx, column.cz - ccz)
  for (const key of state.needs.keys()) if (!columns.has(key)) state.needs.delete(key)
  for (const key of [...state.decoding.keys()]) {
    if (columns.has(key)) continue
    state.decoding.delete(key)
    decoder.cancel(key)
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

// ---- pose and hud ----

const onPose = async ({ mtime, pose }) => {
  state.pose = pose
  state.poseMtime = mtime
  state.pendingMtime = mtime
  interp.push(pose, Date.now())
  if (pose.eye) state.pendingShown.push({ mtime, t: pose.t })
  if (!pose.eye) return
  if (!state.table) await loadTable(pose.mcVersion)
  const ccx = Math.floor(pose.eye.x / 16)
  const ccz = Math.floor(pose.eye.z / 16)
  if (ccx === state.ccx && ccz === state.ccz) return
  const started = performance.now()
  retarget(ccx, ccz)
  keepTiming(view.retargetMs, performance.now() - started)
}

const connect = name => {
  const source = new EventSource(`/pose/${encodeURIComponent(name)}?radius=${radius}`)
  const on = (event, handler) => source.addEventListener(event, e => handler(JSON.parse(e.data)))
  on('pose', event => onPose(event).catch(error => console.error('pose:', error)))
  on('hud', ({ hud }) => { state.hud = hud })
  on('column', onColumnEvent)
  source.onerror = () => console.warn('event stream interrupted; the browser will retry')
}

// ---- free camera ----

const startFree = () => {
  const eye = state.pose?.eye
  if (!eye) return
  state.free = { eye: { ...eye }, yaw: state.pose.yaw, pitch: state.pose.pitch }
  canvas.requestPointerLock?.()
}
const stopFree = () => {
  state.free = null
  state.keys.clear()
  if (document.pointerLockElement) document.exitPointerLock()
}
const toggleFree = () => (state.free ? stopFree() : startFree())

addEventListener('keydown', e => {
  if (e.code === 'KeyF' && !e.repeat) return toggleFree()
  state.keys.add(e.code)
})
addEventListener('keyup', e => state.keys.delete(e.code))
freeButton.addEventListener('click', toggleFree)
addEventListener('mousemove', e => {
  if (!state.free || document.pointerLockElement !== canvas) return
  state.free.yaw -= e.movementX * MOUSE_SENSITIVITY
  state.free.pitch = Math.max(-1.55, Math.min(1.55, state.free.pitch - e.movementY * MOUSE_SENSITIVITY))
})
document.addEventListener('pointerlockchange', () => {
  if (state.free && document.pointerLockElement !== canvas) stopFree()
})

const moveFree = dt => {
  const free = state.free
  if (!free) return
  const forward = directionFor(free.yaw, 0)
  const right = directionFor(free.yaw - Math.PI / 2, 0)
  const axis = (plus, minus) => (state.keys.has(plus) ? 1 : 0) - (state.keys.has(minus) ? 1 : 0)
  const f = axis('KeyW', 'KeyS')
  const r = axis('KeyD', 'KeyA')
  const u = axis('Space', 'ShiftLeft')
  free.eye.x += (forward.x * f + right.x * r) * FREE_SPEED * dt
  free.eye.z += (forward.z * f + right.z * r) * FREE_SPEED * dt
  free.eye.y += u * FREE_SPEED * dt
}

// ---- drawing ----

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

const targetSize = () => {
  const dpr = window.devicePixelRatio || 1
  return [fixedWidth ?? Math.max(1, Math.round(canvas.clientWidth * dpr)), fixedHeight ?? Math.max(1, Math.round(canvas.clientHeight * dpr))]
}

const settled = () => {
  if (!state.pose || !state.dims) return false
  if (state.inFlight.size || state.needs.size || state.decoding.size || state.uploads.size) return false
  return state.columns.size > 0
}

const recordTrace = (ts, cam) => {
  view.camTrace.push({ ts, x: cam.eye.x, y: cam.eye.y, z: cam.eye.z, yaw: cam.yaw })
  if (view.camTrace.length > TRACE_KEEP) view.camTrace.shift()
}

// columns refetched after a file change count as drawn once the frame that uploaded them has been drawn
const stampDrawn = uploaded => {
  const drawnAt = Date.now()
  for (const { key, mtime } of uploaded) view.columnDrawn.push({ key, mtime, drawnAt })
  while (view.columnDrawn.length > COLUMN_DRAWN_KEEP) view.columnDrawn.shift()
}

const frame = (now, dt) => {
  const [w, h] = targetSize()
  gfx.resize(w, h)
  const uploaded = uploadStep()
  if (!state.pose?.eye || !state.dims || state.ccx === null) return gfx.clear(0.78, 0.87, 1)
  const shown = (interpOn && interp.sample(Date.now())) || state.pose
  state.drawn = shown
  view.underruns = interp.underruns()
  view.delay = interp.delay()
  const cam = state.free ?? { eye: shown.eye, yaw: shown.yaw, pitch: shown.pitch }
  if (!state.free) recordTrace(now, cam)
  const origin = { x: (state.ccx - radius) * 16, y: state.dims.minY, z: (state.ccz - radius) * 16 }
  const { time, rain } = sceneTime({ timeOfDay: shown.timeOfDay ?? state.pose.timeOfDay, rain: shown.rain ?? state.pose.rain }, params)
  gfx.draw({
    eye: { x: cam.eye.x - origin.x, y: cam.eye.y - origin.y, z: cam.eye.z - origin.z },
    basis: cameraBasis({ yaw: cam.yaw, pitch: cam.pitch, fov }),
    dist: maxDist,
    darken: skyDarken(time, rain),
    slotOff: { x: mod(state.ccx - radius, N) * 16, z: mod(state.ccz - radius, N) * 16 },
    entities: entityBoxes(shown.entities ?? state.pose.entities, origin, cam.eye)
  })
  stampDrawn(uploaded)
}

const keep = (list, value) => {
  list.push(Math.round(value))
  if (list.length > LATENCY_KEEP) list.shift()
}

// a pose is displayed once the playback body time has reached its t (at once with interpolation off)
const recordLatency = () => {
  if (state.pendingMtime === null && !state.pendingShown.length) return
  gfx.finish()
  const wall = Date.now()
  if (state.pendingMtime !== null) {
    const latency = wall - state.pendingMtime
    state.pendingMtime = null
    if (latency <= 60000) keep(view.latencies, latency) // older is an offline pose, not a live write
  }
  const playhead = interpOn ? interp.playhead(wall) : Infinity
  const due = state.pendingShown.filter(p => p.t <= playhead)
  state.pendingShown = state.pendingShown.filter(p => p.t > playhead)
  for (const { mtime } of due) if (wall - mtime <= 60000) keep(view.shownLatencies, wall - mtime)
}

const updateStats = now => {
  state.frameTimes.push(now)
  while (state.frameTimes.length && now - state.frameTimes[0] > 1000) state.frameTimes.shift()
  view.fps = state.frameTimes.length
  view.frames++
  const loaded = [...state.columns.values()].filter(c => c.status === 'loaded').length
  view.loaded = loaded
  view.wanted = state.columns.size
  view.ready = settled()
}

let last = performance.now()
const loop = now => {
  const dt = Math.min(0.1, (now - last) / 1000)
  last = now
  moveFree(dt)
  try {
    frame(now, dt)
    recordLatency()
  } catch (error) {
    console.error('frame failed:', error)
  }
  updateStats(now)
  requestAnimationFrame(loop)
}

// ---- overlay ----

const fmt = (v, digits = 0) => (typeof v === 'number' ? v.toFixed(digits) : '-')
const renderOverlay = () => {
  const pose = state.pose
  const hud = state.hud
  const lat = view.latencies
  const lines = [
    `${agentName ?? '(no agent)'}  ${pose?.status ?? '-'}${state.free ? '  [free camera]' : ''}`,
    `fps ${view.fps}  ${canvas.width}x${canvas.height}  fov ${fov}  dist ${maxDist}`,
    `pose age ${pose ? Date.now() - pose.t : '-'} ms  ${interpOn ? `interp ${fmt(interp.delay())} ms` : 'interp off'}  file->frame ${lat.length ? lat[lat.length - 1] : '-'} ms (p50 ${fmt(percentile(lat, 0.5))})`,
    `columns ${view.loaded}/${view.wanted}  fetching ${state.inFlight.size}  decoding ${state.decoding.size}  to upload ${state.uploads.size}  queued ${state.needs.size}${view.ready ? '  ready' : ''}`,
    hud ? `hp ${fmt(hud.health)}  food ${fmt(hud.food)}  xp ${hud.xp?.level ?? '-'}  held ${hud.held ? `${hud.held.name} x${hud.held.count}` : '-'}` : 'hud -',
    view.renderer
  ]
  overlay.textContent = lines.join('\n')
}

const fillPicker = async () => {
  const agents = await fetch('/agents').then(r => r.json()).catch(() => [])
  picker.replaceChildren(...[{ name: '', world: '' }, ...agents].map(a => {
    const option = document.createElement('option')
    option.value = a.name
    option.textContent = a.name ? `${a.name} (${a.status})` : '- agent -'
    option.selected = a.name === (agentName ?? '')
    return option
  }))
}
picker.addEventListener('change', () => {
  const next = new URLSearchParams(location.search)
  next.set('agent', picker.value)
  location.search = next.toString()
})

const main = () => {
  fillPicker()
  if (agentName) connect(agentName)
  setInterval(renderOverlay, 250)
  requestAnimationFrame(loop)
}
main()
