// Headless Chromium over CDP with no dependencies (global fetch and WebSocket); the approach of tools/view-web-bench.mjs.
// withPage({url, width, height}, async page => ...) -> page {evaluate, waitUntil, screenshot, logs}; always cleans up.
import { spawn } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'

const CHROMIUM = '/usr/bin/chromium'
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

export const freePort = () => new Promise((resolve, reject) => {
  const server = net.createServer()
  server.once('error', reject)
  server.listen(0, '127.0.0.1', () => {
    const { port } = server.address()
    server.close(() => resolve(port))
  })
})

const waitForTarget = async port => {
  for (let i = 0; i < 100; i++) {
    const targets = await fetch(`http://127.0.0.1:${port}/json`).then(r => r.json()).catch(() => null)
    const page = targets?.find(t => t.type === 'page')
    if (page) return page
    await sleep(100)
  }
  throw new Error('chromium did not expose a page target')
}

const connect = async wsUrl => {
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
  return { send, logs, close: () => socket.close() }
}

export const withPage = async ({ url, width, height, timeoutMs = 90000 }, fn) => {
  const port = await freePort()
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'view-headless-'))
  const chromium = spawn(CHROMIUM, [
    '--headless=new', `--remote-debugging-port=${port}`, `--user-data-dir=${profile}`, '--enable-gpu', '--use-angle=vulkan',
    '--ignore-gpu-blocklist', '--no-first-run', '--no-default-browser-check', `--window-size=${width},${height}`, 'about:blank'
  ], { stdio: 'ignore' })
  let cdp = null
  try {
    cdp = await connect((await waitForTarget(port)).webSocketDebuggerUrl)
    await cdp.send('Runtime.enable')
    await cdp.send('Page.enable')
    await cdp.send('Emulation.setDeviceMetricsOverride', { width, height, deviceScaleFactor: 1, mobile: false })
    await cdp.send('Page.navigate', { url })
    const evaluate = async expression => {
      const { result, exceptionDetails } = await cdp.send('Runtime.evaluate', { expression, returnByValue: true })
      if (exceptionDetails) throw new Error(exceptionDetails.exception?.description ?? exceptionDetails.text)
      return result.value
    }
    const waitUntil = async (expression, what, ms = timeoutMs) => {
      const deadline = Date.now() + ms
      while (Date.now() < deadline) {
        if (await evaluate(expression)) return
        await sleep(200)
      }
      throw new Error(`timed out waiting for ${what}`)
    }
    const screenshot = async () => Buffer.from((await cdp.send('Page.captureScreenshot', { format: 'png' })).data, 'base64')
    return await fn({ evaluate, waitUntil, screenshot, sleep, logs: cdp.logs })
  } finally {
    cdp?.close()
    chromium.kill('SIGKILL')
    fs.rmSync(profile, { recursive: true, force: true })
  }
}
