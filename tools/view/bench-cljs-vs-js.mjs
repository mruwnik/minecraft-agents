// Why JavaScript: a Node bench harness that loads both the cljs build and the old JS; there is no app logic in it.
//
// The viewer's hot logic, cljs against the JS it replaced, on the same inputs: median and p99 per call, no assertion on time.
//   V2 (8d0327d9): hub scheduling (dueScenes, nextDueAt, planFrame, eventCache, stream key), the column window (moveWindow,
//      slotKey) and the scene (pose following, upload selection under budget, frame params): ./bench-old/hub-schedule.mjs and
//      ./bench-old/scene.mjs against view.schedule, view.window and view.scene.
//   V1 (2db6cef8): pose interpolation and drive key rules: ./bench-old/interp.mjs and drive-keys.mjs against view.interp, view.drive.
// The JS originals are verbatim copies under ./bench-old (see their headers); the cljs side is the release build of the :viewer-bench
// shadow-cljs build (advanced optimisations, the :viewer build's compiler options), so build it first:
//   (cd dashboard && flock /tmp/mc-compile.lock npx shadow-cljs release viewer-bench)
//   node tools/view/bench-cljs-vs-js.mjs [--rounds 3] [--json]
// A sample is the mean of `batch` calls (1 for the heavy functions, more for the sub-microsecond ones, where one timer read is
// a large part of a call); p99 is of the samples. Each implementation is warmed up before measuring, and rounds alternate js, cljs.
import { performance } from 'node:perf_hooks'
import { setImmediate as tick } from 'node:timers/promises'
import * as jsHub from './bench-old/hub-schedule.mjs'
import * as jsInterp from './bench-old/interp.mjs'
import * as jsKeys from './bench-old/drive-keys.mjs'
import * as jsScene from './bench-old/scene.mjs'
import * as cljs from '../../dashboard/out/viewer-bench/viewer-bench.mjs'
import { cameraBasis } from './web/camera.mjs'
import { sceneTime, skyDarken } from './web/shading.mjs'

const args = process.argv.slice(2)
const ROUNDS = Number(args[args.indexOf('--rounds') + 1]) || 3
const JSON_OUT = args.includes('--json')

const quantile = (xs, q) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(xs.length * q))]
const US = 1000
let sink = 0 // every call's result is used, so the JIT cannot drop the call
const use = v => { sink = (sink + (v === null || v === undefined ? 1 : 2)) | 0 }

// ---- deterministic inputs ----
const rng = seed => () => {
  seed = (seed * 1664525 + 1013904223) >>> 0
  return seed / 2 ** 32
}

// time `fn(i)` (which makes `batch` calls) `samples` times after `warm` unmeasured runs; per-call microseconds
const measure = (fn, { samples, batch = 1, warm = samples }) => {
  for (let i = 0; i < warm; i++) fn(i)
  const out = new Array(samples)
  for (let i = 0; i < samples; i++) {
    const start = performance.now()
    fn(warm + i)
    out[i] = ((performance.now() - start) * US) / batch
  }
  return out
}

// the same for async steps whose untimed part (letting promises settle) comes between the timed calls: steps(i) returns the µs of
// each timed call it made
const results = [] // {group, name, impl, samples}
const record = (group, name, impl, samples) => {
  const row = results.find(r => r.name === name && r.impl === impl)
  if (row) row.samples.push(...samples)
  else results.push({ group, name, impl, samples })
}

// ---- V2: scheduling ----
const SCENES = 20
const makeTargets = () => {
  const rand = rng(7)
  return Array.from({ length: SCENES }, (_, i) => {
    const fps = [6, 12, 30][i % 3]
    return { id: `s${i}`, visible: i % 7 !== 6, raf: i < 2, fps, dueAt: rand() * (1000 / fps) }
  })
}

const scheduleBench = (impl, api) => {
  const FRAME_MS = 1000 / 60
  let now = 0
  // dueScenes: 20 scenes with staggered due times, advancing one frame at a time; scenes that rendered are rescheduled
  const targets = makeTargets()
  const due = measure(() => { use(api.dueScenes(targets, (now += FRAME_MS))) }, { samples: 4000, batch: 1 })
  record('V2 hub', 'dueScenes (20 scenes)', impl, due)
  const plan = measure(() => { now += FRAME_MS; for (const id of api.planFrame(targets, now, 8)) { const t = targets.find(x => x.id === id); t.dueAt = api.nextDueAt(t.dueAt, now, t.raf ? 'raf' : t.fps) } }, { samples: 4000 })
  record('V2 hub', 'planFrame (20 scenes) + reschedule', impl, plan)
  const fpsList = [6, 'raf', 30, 12]
  record('V2 hub', 'nextDueAt', impl, measure(i => { for (let k = 0; k < 100; k++) use(api.nextDueAt(1000 + k, 1000 + k + (i % 5), fpsList[k & 3])) }, { samples: 4000, batch: 100 }))
}

const cacheBench = (impl, api) => {
  const AGENTS = Array.from({ length: SCENES }, (_, i) => `w/Bot${i}`)
  const events = Array.from({ length: 2000 }, (_, i) => [['pose', 'hud', 'column'][i % 3], { agent: AGENTS[i % SCENES], i }])
  const cache = api.eventCache()
  record('V2 hub', 'eventCache.record (2000 events: pose/hud/column)', impl, measure(i => { for (let k = 0; k < 100; k++) { const [e, d] = events[(i * 100 + k) % 2000]; cache.record(e, d) } }, { samples: 2000, batch: 100 }))
  const sink = []
  record('V2 hub', 'eventCache.replay (one agent)', impl, measure(i => { cache.replay(AGENTS[i % SCENES], (e, d) => sink.push(d)); sink.length = 0 }, { samples: 4000 }))
  record('V2 hub', 'eventCache.keepOnly (20 agents, 10 kept)', impl, measure(i => {
    for (const [e, d] of events.slice(0, 120)) cache.record(e, d)
    cache.keepOnly(i % 2 ? AGENTS.slice(0, 10) : AGENTS)
  }, { samples: 2000 }))
  const scenes = AGENTS.map((agent, i) => ({ agent, radius: 2 + (i % 3) }))
  record('V2 hub', 'streamKey (20 scenes)', impl, measure(() => { use(api.streamKey(scenes)) }, { samples: 2000, batch: 1 }))
}

// ---- V2: the column window ----
const windowBench = (impl, api, radius, name) => {
  let previous = new Map()
  const owners = new Map()
  let ccx = 0
  let ccz = 0
  const samples = measure(i => {
    ccx++
    if (i % 3 === 0) ccz++
    const { columns } = api.moveWindow({ ccx, ccz, radius, columns: previous, owners })
    previous = columns
  }, { samples: 3000 })
  record('V2 window', name, impl, samples)
  record('V2 window', 'slotKey', impl, measure(i => { for (let k = 0; k < 100; k++) use(api.slotKey(i - k, k - 50, 25)) }, { samples: 3000, batch: 100 }))
}

// ---- V2: the scene (pose following, window refill, upload selection under budget, frame params) ----
const fakeWorld = () => ({ allocate () {}, uploadColumn () {}, clearSlot () {}, setBiomes: () => true, dispose () {} })
const decoded = () => ({ header: { worldHeight: 32, minY: -16 }, mats: [], flags: [], light: [], biomes: [], ms: 1, lightMs: 1, mainMs: 0 })
const fakeDecoder = () => ({ decode: () => Promise.resolve(decoded()), cancel () {} })
const fakeResponse = { status: 200, ok: true, arrayBuffer: () => Promise.resolve(new ArrayBuffer(4)), json: () => Promise.resolve(null) }
const fakeFetch = url => Promise.resolve(url.includes('/columns/') ? fakeResponse : { ...fakeResponse, status: 404, ok: false })

const entitiesAround = (x, z) => Array.from({ length: 40 }, (_, i) => ({
  id: i, type: ['player', 'hostile', 'animal', 'item'][i % 4], pos: { x: x + ((i * 37) % 60) - 30, y: 70, z: z + ((i * 53) % 60) - 30 }, yaw: i, width: 0.6, height: 1.8
}))
const poseAt = (i, t) => ({
  mtime: t, pose: { t, status: 'online', world: 'w', mcVersion: '1.21', eye: { x: 8 + i * 0.2, y: 70, z: 8 + i * 0.05 }, pos: { x: 8 + i * 0.2, y: 68, z: 8 + i * 0.05 }, yaw: 1, pitch: 0.1, entities: entitiesAround(8 + i * 0.2, 8 + i * 0.05) }
})

const sceneBench = async (impl, make, radius, label) => {
  const scene = make(radius)
  const POSES = 1200
  const WARM = 400
  const feedUs = []
  const frameUs = []
  const drewUs = []
  let nowMs = Date.now()
  for (let i = 0; i < WARM + POSES; i++) {
    nowMs += 50
    const measured = i >= WARM
    let start = performance.now()
    scene.feed('pose', poseAt(i, nowMs))
    if (measured) feedUs.push((performance.now() - start) * US)
    await tick() // fetches and decodes settle between the timed calls
    await tick()
    for (let f = 0; f < 3; f++) { // the page draws at 60 Hz, the poses come at 20 Hz
      start = performance.now()
      use(scene.frame(performance.now(), {}))
      const mid = performance.now()
      scene.drew()
      const end = performance.now()
      if (measured) { frameUs.push((mid - start) * US); drewUs.push((end - mid) * US) }
    }
  }
  scene.close()
  record('V2 scene', `feed pose, sync part, incl. retarget on a chunk step (${label})`, impl, feedUs)
  record('V2 scene', `frame (${label})`, impl, frameUs)
  record('V2 scene', `drew (${label})`, impl, drewUs)
}

globalThis.fetch = fakeFetch // the original scene.mjs calls the global fetch
const jsMakeScene = radius => jsScene.createScene({ agent: 'w/Bob', radius, interp: true, ownStream: false, renderer: { createWorld: fakeWorld, finish () {} }, decoder: fakeDecoder() })
const cljsMakeScene = radius => cljs.createSceneCore({
  agent: 'w/Bob', radius, interp: true, ownStream: false, world: fakeWorld(), decoder: fakeDecoder(), tables: { ensure: () => Promise.resolve({}) },
  fetch: fakeFetch, cameraBasis, sceneTime, skyDarken
})

// ---- V1: pose interpolation ----
const interpBench = (impl, api) => {
  const rand = rng(11)
  const mk = () => Array.from({ length: 2000 }, (_, i) => ({
    t: 1_000_000 + i * 50, status: 'online', eye: { x: i * 0.2, y: 70, z: i * 0.05 }, pos: { x: i * 0.2, y: 68, z: i * 0.05 }, yaw: 1 + i * 0.01, pitch: 0.1,
    entities: Array.from({ length: 12 }, (_, k) => ({ id: k, pos: { x: k + i * 0.1, y: 70, z: k }, yaw: k * 0.3 }))
  }))
  const jitter = Array.from({ length: 2000 }, () => 20 + rand() * 30)
  const run = () => {
    const poses = mk()
    const interp = api.poseInterpolator()
    const pushUs = []
    const sampleUs = []
    for (let i = 0; i < poses.length; i++) {
      const arrival = poses[i].t + 100 + jitter[i]
      let start = performance.now()
      interp.push(poses[i], arrival)
      pushUs.push((performance.now() - start) * US)
      for (let f = 0; f < 3; f++) { // 60 Hz sampling between 20 Hz arrivals
        const now = arrival + f * (1000 / 60)
        start = performance.now()
        use(interp.sample(now))
        sampleUs.push((performance.now() - start) * US)
      }
    }
    return { pushUs, sampleUs }
  }
  run() // warm-up
  const { pushUs, sampleUs } = run()
  record('V1 interp', 'push (2000 poses, 20 Hz)', impl, pushUs)
  record('V1 interp', 'sample (60 Hz, 12 entities)', impl, sampleUs)
}

// ---- V1: drive keys ----
const keysBench = (impl, api) => {
  const codes = ['KeyW', 'KeyA', 'KeyS', 'KeyD', 'Space', 'ShiftLeft', 'KeyR', 'ArrowLeft', 'ArrowUp', 'KeyQ', 'Escape', 'KeyG']
  const manual = { who: 'bob', why: 'repairs' }
  const dropArgs = { driving: true, reply: { ok: true, offline: false, reason: null, manual }, me: 'bob', startedGen: 3, currentGen: 3 }
  const batchOf = f => i => { for (let k = 0; k < 100; k++) f(i, k) }
  const opts = { samples: 3000, batch: 100 }
  record('V1 keys', 'controlFor (key burst)', impl, measure(batchOf((i, k) => use(api.controlFor(codes[(i + k) % 12]))), opts))
  record('V1 keys', 'lookStepFor', impl, measure(batchOf((i, k) => use(api.lookStepFor(codes[(i + k) % 12]))), opts))
  record('V1 keys', 'mouseLook', impl, measure(batchOf((i, k) => use(api.mouseLook(k - 50, i % 7))), opts))
  const a = { dyaw: 1, dpitch: 2 }
  record('V1 keys', 'mergeLook', impl, measure(batchOf((i, k) => use(api.mergeLook(a, { dyaw: k, dpitch: i }))), opts))
  record('V1 keys', 'shouldDrop', impl, measure(batchOf(() => use(api.shouldDrop(dropArgs))), opts))
  record('V1 keys', 'bannerText', impl, measure(batchOf(() => use(api.bannerText(manual, 'bob'))), opts))
}

const jsApi = { ...jsHub, ...jsInterp, ...jsKeys, ...jsScene }
// the cljs functions take positional arguments where the JS ones took an object; thin adapters, one call each
const cljsApi = {
  ...cljs,
  planFrame: (targets, now, budgetMs) => cljs.planFrame(targets, now, budgetMs, () => 0.4),
  moveWindow: ({ ccx, ccz, radius, columns, owners }) => cljs.moveWindow(ccx, ccz, radius, columns, owners),
  shouldDrop: cljs.shouldDrop,
  streamKey: scenes => cljs.streamKey(scenes.map(s => s.agent), scenes.map(s => s.radius))
}
const jsFns = { ...jsApi, planFrame: (targets, now, budgetMs) => jsHub.planFrame({ entries: targets, now, budgetMs, run: () => 0.4 }), shouldDrop: jsKeys.shouldDrop }

const main = async () => {
  for (let round = 0; round < ROUNDS; round++) {
    for (const [impl, api] of [['js', jsFns], ['cljs', cljsApi]]) {
      scheduleBench(impl, api)
      cacheBench(impl, api)
      windowBench(impl, api, 12, 'moveWindow (radius 12, walk)')
      interpBench(impl, api)
      keysBench(impl, api)
    }
    for (const radius of [4, 12]) {
      await sceneBench('js', jsMakeScene, radius, `radius ${radius}`)
      await sceneBench('cljs', cljsMakeScene, radius, `radius ${radius}`)
    }
  }
  const names = [...new Set(results.map(r => r.name))]
  const rows = names.map(name => {
    const js = results.find(r => r.name === name && r.impl === 'js')
    const cl = results.find(r => r.name === name && r.impl === 'cljs')
    const [p50j, p99j, p50c, p99c] = [quantile(js.samples, 0.5), quantile(js.samples, 0.99), quantile(cl.samples, 0.5), quantile(cl.samples, 0.99)]
    return { group: js.group, name, n: js.samples.length, jsP50: p50j, jsP99: p99j, cljsP50: p50c, cljsP99: p99c, ratioP50: p50c / p50j, ratioP99: p99c / p99j }
  })
  if (JSON_OUT) return console.log(JSON.stringify(rows, null, 2))
  const f = v => (v < 10 ? v.toFixed(2) : v.toFixed(1)).padStart(8)
  console.log(`µs per call, ${ROUNDS} rounds; ratio = cljs / js`)
  console.log(['function'.padEnd(66), 'js p50', 'js p99', 'cljs p50', 'cljs p99', 'x p50', 'x p99'].map((h, i) => i ? h.padStart(8) : h).join(' '))
  for (const r of rows) console.log(`${(r.group + ' ' + r.name).padEnd(66)} ${f(r.jsP50)} ${f(r.jsP99)} ${f(r.cljsP50)} ${f(r.cljsP99)} ${f(r.ratioP50)} ${f(r.ratioP99)}`)
}

await main()
