// Measures the browser view in headless Chromium over CDP.
//   node tools/view-web-bench.mjs <url> [--angle vulkan] [--seconds 5] [--screenshot file.png] [--no-vsync] [--width W --height H] [--trace seconds]
// --trace S: after the warm-up, records S seconds of the drawn camera (window.__view.camTrace) and adds `smooth`, `shownLatency` and `decodeMs`.
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
    trace: { type: 'string' }
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
    dtMs: { p50: round(percentile(dts, 0.5), 1), p95: round(percentile(dts, 0.95), 1), max: round(dts.length ? Math.max(...dts) : null, 1) }
  }
}

const main = async () => {
  const port = await freePort()
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'view-bench-'))
  const [w, h] = windowSize()
  const flags = [
    '--headless=new', `--remote-debugging-port=${port}`, `--user-data-dir=${profile}`, '--enable-gpu', `--use-angle=${values.angle}`,
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
    await cdp.send('Page.navigate', { url })
    const ready = await waitUntil(() => cdp.evaluate('window.__view?.ready === true'), Number(values.timeout) * 1000, 'window.__view.ready')
    await sleep(2000)
    if (values.trace) await cdp.evaluate('__view.camTrace.length = 0; __view.latencies.length = 0; __view.shownLatencies.length = 0; true')
    const first = await cdp.evaluate('({frames: __view.frames, t: performance.now()})')
    await sleep(Number(values.trace ?? values.seconds) * 1000)
    const last = await cdp.evaluate('({shown: __view.shownLatencies, underruns: __view.underruns, delay: __view.delay, decode: __view.decodeMs, trace: __view.camTrace, frames: __view.frames, t: performance.now(), fps: __view.fps, latencies: __view.latencies, loaded: __view.loaded, wanted: __view.wanted, renderer: __view.renderer, size: [document.getElementById("view").width, document.getElementById("view").height]})')
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
        decodeMs: { n: last.decode.length, p50: percentile(last.decode, 0.5), max: last.decode.length ? Math.max(...last.decode) : null }
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
