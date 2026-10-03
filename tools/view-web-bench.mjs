// Measures the browser view in headless Chromium over CDP.
//   node tools/view-web-bench.mjs <url> [--angle vulkan] [--seconds 5] [--screenshot file.png] [--no-vsync] [--width W --height H]
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
    timeout: { type: 'string', default: '90' }
  }
})
const url = positionals[0]
if (!url) {
  console.error('usage: node tools/view-web-bench.mjs <url> [--angle vulkan] [--seconds 5] [--screenshot file.png] [--no-vsync] [--width W --height H]')
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
    const first = await cdp.evaluate('({frames: __view.frames, t: performance.now()})')
    await sleep(Number(values.seconds) * 1000)
    const last = await cdp.evaluate('({frames: __view.frames, t: performance.now(), fps: __view.fps, latencies: __view.latencies, loaded: __view.loaded, wanted: __view.wanted, renderer: __view.renderer, size: [document.getElementById("view").width, document.getElementById("view").height]})')
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
