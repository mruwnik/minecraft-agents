// Why JavaScript: WebGL/browser; the browser view entry, runs in the page.
// The browser view: one scene (scene.mjs) of one agent drawn by the WebGL renderer (gl.mjs) every frame, with an overlay, a free
// camera and the agent picker. window.__view exposes numbers for automated measurement. hub.mjs draws many scenes in one context.
import { isAgentKey } from './cljs/viewer.mjs'
import { directionFor } from './camera.mjs'
import { createDecoder } from './decoder.mjs'
import { createRenderer } from './gl.mjs'
import { createScene, decodePriority } from './scene.mjs'

const MOUSE_SENSITIVITY = 0.0022
const FREE_SPEED = 12

const params = new URLSearchParams(location.search)
const numberParam = (name, fallback) => {
  const value = Number(params.get(name))
  return params.has(name) && Number.isFinite(value) && value > 0 ? value : fallback
}
// the body is <world>/<name>: a name is unique only within a world
const agentParam = params.get('agent')
const agentName = isAgentKey(agentParam) ? agentParam : null
const agentError = agentParam && !agentName ? `bad ?agent=${agentParam}: use <world>/<name>, e.g. ?agent=claude/Bob (pick one below)` : null
const radius = Math.min(32, Math.round(numberParam('radius', 8)))
const fov = numberParam('fov', 70)
const maxDist = numberParam('dist', radius * 16)
const fixedWidth = params.has('w') ? Math.round(numberParam('w', 0)) : null
const fixedHeight = params.has('h') ? Math.round(numberParam('h', 0)) : null
const interpOn = params.get('interp') !== '0'
// ?debug=1 checkers blocks the view draws missing or wrong, ?debug=2 also the approximate ones (tools/view/block-issues.mjs)
const debugLevel = { 1: 1, 2: 2 }[params.get('debug')] ?? 0

const canvas = document.getElementById('view')
const overlay = document.getElementById('overlay')
const picker = document.getElementById('agents')
const freeButton = document.getElementById('free')

const percentile = (values, p) => {
  if (!values.length) return null
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.min(sorted.length - 1, Math.floor(p * sorted.length))]
}

const gfx = createRenderer(canvas)
gfx.setDebug(debugLevel > 0)
const decoder = createDecoder({
  makeWorker: params.get('worker') === '0' ? null : undefined, // ?worker=0 decodes on the main thread
  priority: decodePriority
})
const scene = agentName
  ? createScene({ agent: agentName, radius, fov, interp: interpOn, renderer: gfx, decoder, debugLevel, maxDist, urlParams: params, finishForLatency: true })
  : null
const noMetrics = { fps: 0, frames: 0, latencies: [], shownLatencies: [], camTrace: [], underruns: 0, decodeMs: [], lightMs: [], uploadMs: [], columnDrawn: [], retargetMs: [], slowUploads: [], mainDecode: { ms: 0, columns: 0 }, loaded: 0, wanted: 0, ready: false }
const view = window.__view = scene?.metrics ?? noMetrics
view.renderer = gfx.renderer
window.__viewStats = () => (scene ? { fps: view.fps, ...scene.stats() } : null) // measurement: fps, loaded, wanted, poseAge, status (docs/view-rendering-performance.md)

const state = {
  free: null, // {eye, yaw, pitch} while the free camera is on
  frameTimes: [],
  keys: new Set()
}

// ---- free camera ----

const startFree = () => {
  const pose = scene?.pose()
  if (!pose?.eye) return
  state.free = { eye: { ...pose.eye }, yaw: pose.yaw, pitch: pose.pitch }
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

const targetSize = () => {
  const dpr = window.devicePixelRatio || 1
  return [fixedWidth ?? Math.max(1, Math.round(canvas.clientWidth * dpr)), fixedHeight ?? Math.max(1, Math.round(canvas.clientHeight * dpr))]
}

const frame = now => {
  const [w, h] = targetSize()
  gfx.resize(w, h)
  const drawParams = scene?.frame(now, { camera: state.free })
  if (!drawParams) return gfx.clear(0.78, 0.87, 1)
  gfx.draw(scene.world, drawParams)
}

const updateStats = now => {
  state.frameTimes.push(now)
  while (state.frameTimes.length && now - state.frameTimes[0] > 1000) state.frameTimes.shift()
  view.fps = state.frameTimes.length
  view.frames++
}

let last = performance.now()
const loop = now => {
  const dt = Math.min(0.1, (now - last) / 1000)
  last = now
  moveFree(dt)
  try {
    frame(now)
    scene?.drew()
  } catch (error) {
    console.error('frame failed:', error)
  }
  updateStats(now)
  requestAnimationFrame(loop)
}

// ---- overlay ----

const fmt = (v, digits = 0) => (typeof v === 'number' ? v.toFixed(digits) : '-')
const renderOverlay = () => {
  const pose = scene?.pose()
  const hud = scene?.hud()
  const lat = view.latencies
  const counts = scene?.counts() ?? { inFlight: 0, decoding: 0, uploads: 0, needs: 0 }
  const lines = [
    agentError ?? `${agentName ?? '(no agent)'}  ${pose?.status ?? '-'}${state.free ? '  [free camera]' : ''}`,
    `fps ${view.fps}  ${canvas.width}x${canvas.height}  fov ${fov}  dist ${maxDist}`,
    `pose age ${pose ? Date.now() - pose.t : '-'} ms  ${interpOn ? `interp ${fmt(scene?.interp.delay())} ms` : 'interp off'}  file->frame ${lat.length ? lat[lat.length - 1] : '-'} ms (p50 ${fmt(percentile(lat, 0.5))})`,
    `columns ${view.loaded}/${view.wanted}  fetching ${counts.inFlight}  decoding ${counts.decoding}  to upload ${counts.uploads}  queued ${counts.needs}${view.ready ? '  ready' : ''}`,
    hud ? `hp ${fmt(hud.health)}  food ${fmt(hud.food)}  xp ${hud.xp?.level ?? '-'}  held ${hud.held ? `${hud.held.name} x${hud.held.count}` : '-'}` : 'hud -',
    view.renderer
  ]
  overlay.textContent = lines.join('\n')
}

const fillPicker = async () => {
  const agents = await fetch('/agents').then(r => r.json()).catch(() => [])
  picker.replaceChildren(...[{ name: '', world: '' }, ...agents].map(a => {
    const option = document.createElement('option')
    option.value = a.name ? `${a.world}/${a.name}` : ''
    option.textContent = a.name ? `${a.name} (${a.world}, ${a.status})` : '- agent -'
    option.selected = option.value === (agentName ?? '')
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
  setInterval(renderOverlay, 250)
  requestAnimationFrame(loop)
}
main()
