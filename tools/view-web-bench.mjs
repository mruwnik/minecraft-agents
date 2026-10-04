// Why JavaScript: drives headless Chromium over CDP to measure the WebGL view (browser/GPU).
// Measures the browser view in headless Chromium over CDP.
//   node tools/view-web-bench.mjs <url> [--angle vulkan] [--seconds 5] [--screenshot file.png] [--no-vsync] [--width W --height H] [--trace seconds] [--hub]
// --beside-hub <hub url>: the url is a single view; measures its fps alone, then again with a hub page (own window, same browser) running beside it, and the hub's per-card rates.
// --dashboard: the url is the dashboard (e.g. http://localhost:3701/#/bodies); reads the page's own hub (window.getViewHub()) with the --hub report, plus the page's requestAnimationFrame frame times (fps, p50, p95, max).
// --hub: the url is tools/view/web/hub-demo.html (many scenes in one context); samples window.__hub every second for --seconds and prints per-scene fps (min / median over scenes),
//   main-thread frame cost, GPU time per scene render (timer query, and a gl.finish probe), GPU memory and JS heap instead of the single-view numbers.
// --trace S: after the warm-up, records S seconds of the drawn camera (window.__view.camTrace) and adds `smooth` (with dropped frames), `shownLatency`, `decodeMs`, `uploadMs` and `columnChange` (file mtime to drawn).
// Prints one JSON line {url, angle, renderer, fps, frames, latency, loaded}. No dependencies (global fetch and WebSocket).
import { spawn } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    angle: { type: 'string', default: 'vulkan' },
    seconds: { type: 'string', default: '5' },
    screenshot: { type: 'string' },
    'no-vsync': { type: 'boolean', default: false },
    width: { type: 'string' },
    height: { type: 'string' },
    chromium: { type: 'string', default: '/usr/bin/chromium' },
    timeout: { type: 'string', default: '90' },
    trace: { type: 'string' },
    hub: { type: 'boolean', default: false },
    dashboard: { type: 'boolean', default: false },
    'beside-hub': { type: 'string' }
  }
})
const url = positionals[0]
if (!url) {
  console.error('usage: node tools/view-web-bench.mjs <url> [--angle vulkan] [--seconds 5] [--screenshot file.png] [--no-vsync] [--width W --height H] [--trace seconds]')
  process.exit(2)
}

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

const freePort = () => new Promise((resolve, reject) => {
  const server = net.createServer()
  server.once('error', reject)
  server.listen(0, '127.0.0.1', () => {
    const { port } = server.address()
    server.close(() => resolve(port))
  })
})

// the window size defaults to the page's w and h parameters
const windowSize = () => {
  const params = new URL(url).searchParams
  return [values.width ?? params.get('w') ?? '1280', values.height ?? params.get('h') ?? '720']
}

const waitForTargets = async port => {
  for (let i = 0; i < 100; i++) {
    const targets = await fetch(`http://127.0.0.1:${port}/json`).then(r => r.json()).catch(() => null)
    const page = targets?.find(t => t.type === 'page')
    if (page) return page
    await sleep(100)
  }
  throw new Error('chromium did not expose a page target')
}

const connectCdp = async wsUrl => {
  const socket = new WebSocket(wsUrl)
  await new Promise((resolve, reject) => {
    socket.addEventListener('open', resolve, { once: true })
    socket.addEventListener('error', reject, { once: true })
  })
  let nextId = 1
  const pending = new Map()
  const logs = []
  socket.addEventListener('message', ({ data }) => {
    const message = JSON.parse(data)
    if (message.id) {
      const waiter = pending.get(message.id)
      pending.delete(message.id)
      if (message.error) waiter.reject(new Error(message.error.message))
      else waiter.resolve(message.result)
      return
    }
    if (message.method === 'Runtime.consoleAPICalled' && ['error', 'warning'].includes(message.params.type)) {
      logs.push(`${message.params.type}: ${message.params.args.map(a => a.value ?? a.description).join(' ')}`)
    }
    if (message.method === 'Runtime.exceptionThrown') logs.push(`exception: ${message.params.exceptionDetails.exception?.description ?? message.params.exceptionDetails.text}`)
  })
  const send = (method, params = {}) => new Promise((resolve, reject) => {
    const id = nextId++
    pending.set(id, { resolve, reject })
    socket.send(JSON.stringify({ id, method, params }))
  })
  const evaluate = async expression => {
    const { result, exceptionDetails } = await send('Runtime.evaluate', { expression, returnByValue: true })
    if (exceptionDetails) throw new Error(exceptionDetails.exception?.description ?? exceptionDetails.text)
    return result.value
  }
  return { send, evaluate, logs, close: () => socket.close() }
}

const waitUntil = async (predicate, timeoutMs, what) => {
  const deadline = Date.now() + timeoutMs
  while (Date.now() < deadline) {
    if (await predicate()) return true
    await sleep(200)
  }
  console.error(`timed out waiting for ${what}`)
  return false
}

const percentile = (list, p) => (list.length ? [...list].sort((a, b) => a - b)[Math.min(list.length - 1, Math.floor(p * list.length))] : null)

const MOVING = 1e-4
const TELEPORT = 8 // a camera step above this is a snap (server teleport), counted as a jump and left out of the stats
const DROPPED_FRAME_MS = 25 // a frame later than this after the previous one is dropped at 60 fps
const STUTTER_RUN = 30 // still frames between moving frames up to this many (0.5 s at 60 fps) count as stutter; longer is the body standing still
const round = (v, digits = 4) => (v === null ? null : Math.round(v * 10 ** digits) / 10 ** digits)

// lengths of runs of still steps that have a moving step on both sides
const stutterRuns = steps => {
  const runs = []
  let run = 0
  let seenMoving = false
  for (const s of steps) {
    if (s.d > MOVING) {
      if (seenMoving && run) runs.push(run)
      seenMoving = true
      run = 0
      continue
    }
    run++
  }
  return runs
}

// frame-to-frame camera motion statistics of a trace [{ts, x, y, z}]
const smoothness = trace => {
  const all = trace.slice(1).map((f, i) => ({
    d: Math.hypot(f.x - trace[i].x, f.y - trace[i].y, f.z - trace[i].z),
    dt: f.ts - trace[i].ts
  }))
  const steps = all.filter(s => s.d <= TELEPORT)
  const moving = steps.filter(s => s.d > MOVING).map(s => s.d / s.dt * 1000) // blocks per second, so a dropped frame is not jerk
  const mean = moving.length ? moving.reduce((a, b) => a + b, 0) / moving.length : 0
  const std = moving.length ? Math.sqrt(moving.reduce((a, b) => a + (b - mean) ** 2, 0) / moving.length) : 0
  const dts = all.map(s => s.dt)
  return {
    frames: trace.length,
    moving: moving.length,
    jumps: all.length - steps.length,
    meanSpeed: round(mean),
    stdSpeed: round(std),
    cv: mean ? round(std / mean) : null,
    maxOverMedian: moving.length ? round(Math.max(...moving) / percentile(moving, 0.5)) : null,
    zeroFrames: stutterRuns(steps).filter(r => r <= STUTTER_RUN).reduce((a, b) => a + b, 0),
    dropped: dts.filter(dt => dt > DROPPED_FRAME_MS).length,
    dtMs: { p50: round(percentile(dts, 0.5), 1), p95: round(percentile(dts, 0.95), 1), max: round(dts.length ? Math.max(...dts) : null, 1) }
  }
}

const median = list => percentile(list, 0.5)

// renders per second of every card over the whole run, and its slowest 5-second window (samples one second apart)
const WINDOW_S = 5
const cardRates = samples => {
  const agents = Object.keys(samples.at(-1).stats.scenes)
  const per = Object.fromEntries(agents.map(a => {
    const counts = samples.map(s => s.stats.scenes[a].renders)
    const windows = counts.slice(WINDOW_S).map((c, i) => (c - counts[i]) / WINDOW_S)
    return [a, { run: round((counts.at(-1) - counts[0]) / (counts.length - 1), 2), worstWindow: windows.length ? round(Math.min(...windows), 2) : null }]
  }))
  return { minRun: Math.min(...agents.map(a => per[a].run)), minWindow: Math.min(...agents.map(a => per[a].worstWindow ?? Infinity)), seconds: samples.length - 1, perCard: per }
}

// the hub page: per-second samples of window.__hub.stats(), then the numbers that matter for 11 cards at 6 fps
const hubReport = async cdp => {
  const samples = []
  for (let i = 0; i < Number(values.seconds); i++) {
    await sleep(1000)
    samples.push(await cdp.evaluate('({ stats: __hub.stats(), heap: performance.memory ? performance.memory.usedJSHeapSize : null })'))
  }
  const cards = cardRates(samples)
  const attachSamples = samples.map(s => Object.values(s.stats.scenes).flatMap(scene => scene.attaches ?? []))
  const bigSamples = attachSamples.map(list => list.find(a => a.fps === 'raf')).filter(Boolean)
  const cardCount = attachSamples.at(-1).filter(a => a.fps !== 'raf').length
  const cardFpsMeans = Array.from({ length: cardCount }, (_, i) => attachSamples.reduce((sum, list) => sum + list.filter(a => a.fps !== 'raf')[i].measuredFps, 0) / attachSamples.length)
  const bigReport = bigSamples.length ? {
    size: [bigSamples.at(-1).width, bigSamples.at(-1).height],
    measuredFpsMin: Math.min(...bigSamples.map(a => a.measuredFps)),
    measuredFpsSeries: bigSamples.map(a => a.measuredFps),
    measuredFpsMean: round(bigSamples.reduce((sum, a) => sum + a.measuredFps, 0) / bigSamples.length, 2),
    copyMsP50: bigSamples.at(-1).copyMs,
    copyMsP95: bigSamples.at(-1).copyMsP95,
    gpuMs: bigSamples.at(-1).gpuMs ?? null,
    cardAttachFps: { min: round(Math.min(...cardFpsMeans), 2), median: round(median(cardFpsMeans), 2), n: cardCount }
  } : null
  const gpuProbe = await cdp.evaluate('__hub.probeGpu()')
  const longTasks = await cdp.evaluate('window.__longTasks ?? []')
  const final = samples.at(-1).stats
  const agents = Object.keys(final.scenes)
  const meanFps = agent => samples.reduce((sum, s) => sum + s.stats.scenes[agent].fps, 0) / samples.length
  const fpsList = agents.map(meanFps)
  const ready = agents.filter(a => final.scenes[a].loaded > 0)
  const heaps = samples.map(s => s.heap).filter(h => h !== null)
  const probe = Object.values(gpuProbe).filter(v => v !== null)
  const queried = agents.map(a => final.scenes[a].gpuMs).filter(v => v !== undefined)
  const mb = bytes => round(bytes / 1048576, 1)
  return {
    url,
    renderer: final.renderer,
    scenes: agents.length,
    scenesWithColumns: ready.length,
    cardFps: cards,
    big: bigReport,
    fps: { min: round(Math.min(...fpsList), 2), median: round(median(fpsList), 2), perScene: Object.fromEntries(agents.map((a, i) => [a, round(fpsList[i], 2)])) },
    frameCostMs: { p50: round(final.frameCostMs.p50, 2), p95: round(final.frameCostMs.p95, 2), max: round(final.frameCostMs.max, 2), n: final.frameCostMs.n },
    renderCpuMsP50: round(median(agents.map(a => final.scenes[a].renderMsP50).filter(v => v !== null)), 2),
    gpuMs: { timerQuery: final.gpuTimer ? { median: round(median(queried), 2), max: round(Math.max(...queried), 2), n: queried.length } : null, finishProbe: { median: round(median(probe), 2), max: round(Math.max(...probe), 2), n: probe.length } },
    gpuMemoryMB: { total: mb(final.memory.total), shared: mb(final.memory.shared), perScene: mb(median(final.memory.worlds)), scenes: final.memory.worlds.length },
    jsHeapMB: heaps.length ? { last: mb(heaps.at(-1)), max: mb(Math.max(...heaps)) } : null,
    longTasks: { n: longTasks.length, totalMs: round(longTasks.reduce((x, y) => x + y, 0), 1), maxMs: Math.max(0, ...longTasks) },
    console: cdp.logs.slice(0, 10)
  }
}

// a second window in the same browser (browser-level CDP), so both pages are visible and keep animating
const openWindow = async (port, pageUrl, w, h) => {
  const version = await fetch(`http://127.0.0.1:${port}/json/version`).then(r => r.json())
  const browser = await connectCdp(version.webSocketDebuggerUrl)
  const { targetId } = await browser.send('Target.createTarget', { url: pageUrl, newWindow: true, width: Number(w), height: Number(h) })
  const targets = await fetch(`http://127.0.0.1:${port}/json`).then(r => r.json())
  const target = targets.find(t => t.id === targetId)
  const cdp = await connectCdp(target.webSocketDebuggerUrl)
  await cdp.send('Runtime.enable')
  await cdp.send('Emulation.setFocusEmulationEnabled', { enabled: true })
  return { cdp, browser }
}

const viewFps = async (cdp, seconds) => {
  const first = await cdp.evaluate('({frames: __view.frames, t: performance.now()})')
  await sleep(seconds * 1000)
  const last = await cdp.evaluate('({frames: __view.frames, t: performance.now()})')
  return round((last.frames - first.frames) / ((last.t - first.t) / 1000), 1)
}

const besideReport = async (port, cdp, w, h) => {
  const alone = await viewFps(cdp, Number(values.seconds))
  const { cdp: hubCdp, browser } = await openWindow(port, values['beside-hub'], w, h)
  const hubReady = await waitUntil(() => hubCdp.evaluate('window.__hub?.ready === true'), Number(values.timeout) * 1000, 'window.__hub.ready')
  await sleep(2000)
  const before = await hubCdp.evaluate('__hub.stats()')
  const fpsBeside = await viewFps(cdp, Number(values.seconds))
  const after = await hubCdp.evaluate('__hub.stats()')
  const cardFps = Object.keys(after.scenes).map(a => (after.scenes[a].renders - before.scenes[a].renders) / Number(values.seconds))
  hubCdp.close()
  browser.close()
  return { alone, besideHub11: fpsBeside, hubReady, hubCardFps: { min: round(Math.min(...cardFps), 2), median: round(median(cardFps), 2) }, hubFrameCostMs: after.frameCostMs, console: [...cdp.logs, ...hubCdp.logs].slice(0, 10) }
}

const main = async () => {
  const port = await freePort()
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'view-bench-'))
  const [w, h] = windowSize()
  const flags = [
    '--headless=new', '--disable-background-timer-throttling', '--disable-renderer-backgrounding', '--disable-backgrounding-occluded-windows', `--remote-debugging-port=${port}`, `--user-data-dir=${profile}`, '--enable-gpu', `--use-angle=${values.angle}`,
    '--ignore-gpu-blocklist', '--no-first-run', '--no-default-browser-check', `--window-size=${w},${h}`,
    ...(values['no-vsync'] ? ['--disable-gpu-vsync', '--disable-frame-rate-limit'] : []),
    'about:blank'
  ]
  const chromium = spawn(values.chromium, flags, { stdio: 'ignore' })
  const cleanup = () => {
    chromium.kill('SIGKILL')
    fs.rmSync(profile, { recursive: true, force: true })
  }
  process.on('SIGINT', () => { cleanup(); process.exit(130) })
  try {
    const page = await waitForTargets(port)
    const cdp = await connectCdp(page.webSocketDebuggerUrl)
    await cdp.send('Runtime.enable')
    await cdp.send('Page.enable')
    await cdp.send('Emulation.setDeviceMetricsOverride', { width: Number(w), height: Number(h), deviceScaleFactor: 1, mobile: false })
    // main-thread long tasks (> 50 ms by the browser's definition) from the start of the page, for every page version
    await cdp.send('Page.addScriptToEvaluateOnNewDocument', { source: `window.__longTasks = []; new PerformanceObserver(list => { for (const e of list.getEntries()) window.__longTasks.push(Math.round(e.duration * 10) / 10) }).observe({ entryTypes: ['longtask'] })` })
    if (values.dashboard) await cdp.send('Page.addScriptToEvaluateOnNewDocument', { source: `window.__raf = []; let last = 0; const tick = t => { if (last) window.__raf.push(t - last); last = t; requestAnimationFrame(tick) }; requestAnimationFrame(tick)` })
    await cdp.send('Page.navigate', { url })
    if (values.dashboard) {
      const ready = await waitUntil(() => cdp.evaluate('window.getViewHub?.() != null && document.querySelectorAll("canvas").length > 0'), Number(values.timeout) * 1000, 'dashboard hub and canvases')
      await cdp.evaluate('window.__hub = { stats: () => getViewHub().stats(), probeGpu: o => getViewHub().probeGpu ? getViewHub().probeGpu(o) : {} }; true')
      await sleep(3000)
      await cdp.evaluate('window.__raf.length = 0; window.__longTasks.length = 0; true')
      const report = await hubReport(cdp)
      const raf = await cdp.evaluate('window.__raf')
      const total = raf.reduce((a, b) => a + b, 0)
      const canvases = await cdp.evaluate('document.querySelectorAll("canvas").length')
      if (values.screenshot) fs.writeFileSync(values.screenshot, Buffer.from((await cdp.send('Page.captureScreenshot', { format: 'png' })).data, 'base64'))
      console.log(JSON.stringify({ ready, canvases, page: { fps: round(raf.length / (total / 1000), 1), frameMs: { p50: round(percentile(raf, 0.5), 1), p95: round(percentile(raf, 0.95), 1), max: round(Math.max(0, ...raf), 1) } }, ...report }))
      cdp.close()
      return
    }
    const ready = await waitUntil(() => cdp.evaluate(values.hub ? 'window.__hub?.ready === true' : 'window.__view?.ready === true'), Number(values.timeout) * 1000, values.hub ? 'window.__hub.ready' : 'window.__view.ready')
    await sleep(2000)
    if (values['beside-hub']) {
      console.log(JSON.stringify({ ready, url, ...(await besideReport(port, cdp, w, h)) }))
      cdp.close()
      return
    }
    if (values.hub) {
      const report = await hubReport(cdp)
      if (values.screenshot) fs.writeFileSync(values.screenshot, Buffer.from((await cdp.send('Page.captureScreenshot', { format: 'png' })).data, 'base64'))
      console.log(JSON.stringify({ ready, ...report }))
      cdp.close()
      return
    }
    if (values.trace) await cdp.evaluate('__view.camTrace.length = 0; __view.latencies.length = 0; __view.shownLatencies.length = 0; __view.decodeMs.length = 0; __view.uploadMs.length = 0; (__view.columnDrawn ??= []).length = 0; (__view.retargetMs ??= []).length = 0; __longTasks.length = 0; window.__mainDecode0 = { ...(__view.mainDecode ?? { ms: 0, columns: 0 }) }; (__view.slowUploads ??= []).length = 0; true')
    const first = await cdp.evaluate('({frames: __view.frames, t: performance.now()})')
    await sleep(Number(values.trace ?? values.seconds) * 1000)
    const last = await cdp.evaluate('({shown: __view.shownLatencies, underruns: __view.underruns, delay: __view.delay, decode: __view.decodeMs, upload: __view.uploadMs, drawn: __view.columnDrawn ?? [], retarget: __view.retargetMs ?? [], longTasks: window.__longTasks ?? [], mainDecode: __view.mainDecode ?? null, mainDecode0: window.__mainDecode0 ?? null, slowUploads: __view.slowUploads ?? [], trace: __view.camTrace, frames: __view.frames, t: performance.now(), fps: __view.fps, latencies: __view.latencies, loaded: __view.loaded, wanted: __view.wanted, renderer: __view.renderer, size: [document.getElementById("view").width, document.getElementById("view").height]})')
    if (values.screenshot) {
      const { data } = await cdp.send('Page.captureScreenshot', { format: 'png' })
      fs.writeFileSync(values.screenshot, Buffer.from(data, 'base64'))
    }
    console.log(JSON.stringify({
      url,
      angle: values.angle,
      vsync: !values['no-vsync'],
      ready,
      renderer: last.renderer,
      size: last.size,
      fps: Math.round((last.frames - first.frames) / ((last.t - first.t) / 1000) * 10) / 10,
      fpsOverlay: last.fps,
      frames: last.frames,
      latency: { n: last.latencies.length, p50: percentile(last.latencies, 0.5), p95: percentile(last.latencies, 0.95) },
      ...(values.trace ? {
        smooth: { ...smoothness(last.trace), underruns: last.underruns, delay: last.delay },
        shownLatency: { n: last.shown.length, p50: percentile(last.shown, 0.5), p95: percentile(last.shown, 0.95) },
        decodeMs: { n: last.decode.length, p50: percentile(last.decode, 0.5), max: last.decode.length ? Math.max(...last.decode) : null },
        uploadMs: { n: last.upload.length, p50: percentile(last.upload, 0.5), max: last.upload.length ? Math.max(...last.upload) : null },
        retargetMs: { n: last.retarget.length, p50: percentile(last.retarget, 0.5), max: last.retarget.length ? Math.max(...last.retarget) : null },
        longTasks: { n: last.longTasks.length, totalMs: round(last.longTasks.reduce((x, y) => x + y, 0), 1), maxMs: Math.max(0, ...last.longTasks) },
        mainDecodeMs: last.mainDecode && last.mainDecode0 ? { total: round(last.mainDecode.ms - last.mainDecode0.ms, 1), columns: last.mainDecode.columns - last.mainDecode0.columns } : null,
        slowUploads: last.slowUploads.slice(0, 8),
        columnChange: { n: last.drawn.length, p50: percentile(last.drawn.map(d => d.drawnAt - d.mtime), 0.5), p95: percentile(last.drawn.map(d => d.drawnAt - d.mtime), 0.95) }
      } : {}),
      loaded: `${last.loaded}/${last.wanted}`,
      console: cdp.logs.slice(0, 10)
    }))
    cdp.close()
  } finally {
    cleanup()
  }
}

main().catch(error => {
  console.error(error)
  process.exit(1)
})
