// The view server: static pages, column files, agent list and the pose/hud/column event stream.
import { test, before, after } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createViewServer } from '../tools/view/serve.mjs'

const realTextures = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const root = fs.mkdtempSync(path.join(os.tmpdir(), 'view-serve-'))
const stateDir = path.join(root, 'state')
const webDir = path.join(root, 'web')
const viewDir = path.join(stateDir, 'agents', 'Bob', 'view')
const chunkDir = path.join(stateDir, 'worlds', 'w1', 'chunks')
const pose = { v: 1, t: 5, world: 'w1', status: 'online', eye: { x: 8, y: 70, z: 8 }, yaw: 0, pitch: 0 }
const hud = { v: 1, health: 20 }
const columnBytes = Buffer.from([1, 2, 3, 250, 0])
let server
let base

const writeAt = (file, content, seconds) => {
  fs.writeFileSync(file, content)
  fs.utimesSync(file, seconds, seconds)
}
const get = (p, init) => fetch(`${base}${p}`, init)
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

// reads SSE events off a response until `done` says so or the timeout passes
async function collect (response, { ms, done = () => false }) {
  const reader = response.body.getReader()
  const decoder = new TextDecoder()
  const events = []
  let buffer = ''
  const deadline = Date.now() + ms
  while (Date.now() < deadline && !done(events)) {
    const chunk = await Promise.race([reader.read(), sleep(Math.max(1, deadline - Date.now())).then(() => null)])
    if (!chunk || chunk.done) break
    buffer += decoder.decode(chunk.value)
    const parts = buffer.split('\n\n')
    buffer = parts.pop()
    for (const part of parts) {
      const event = /^event: (.*)$/m.exec(part)?.[1]
      const data = /^data: (.*)$/m.exec(part)?.[1]
      if (event) events.push({ event, data: JSON.parse(data) })
    }
  }
  await reader.cancel()
  return events
}

before(async () => {
  fs.mkdirSync(viewDir, { recursive: true })
  fs.mkdirSync(chunkDir, { recursive: true })
  fs.mkdirSync(webDir)
  fs.mkdirSync(path.join(stateDir, 'agents', 'Broken', 'view'), { recursive: true })
  fs.writeFileSync(path.join(stateDir, 'agents', 'Broken', 'view', 'pose.json'), '{nope')
  writeAt(path.join(viewDir, 'pose.json'), JSON.stringify(pose), 1000)
  writeAt(path.join(viewDir, 'hud.json'), JSON.stringify(hud), 1000)
  for (const name of ['0.0', '1.0', '20.20', '-1.-1']) writeAt(path.join(chunkDir, `${name}.bin`), columnBytes, 1000)
  fs.writeFileSync(path.join(webDir, 'index.html'), '<h1>hi</h1>')
  fs.writeFileSync(path.join(webDir, 'x.mjs'), 'export const x = 1')
  fs.writeFileSync(path.join(webDir, 'secret.txt'), 'no')
  server = createViewServer({ stateDir, textureDir: realTextures, webDir, pollMs: 20, columnPollMs: 50 })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  base = `http://127.0.0.1:${server.address().port}`
})

after(() => {
  server.closeAllConnections()
  server.close()
  fs.rmSync(root, { recursive: true, force: true })
})

test('GET /agents lists agents with a readable pose and skips broken ones', async () => {
  const body = await (await get('/agents')).json()
  assert.deepEqual(body, [{ name: 'Bob', world: 'w1', status: 'online', t: 5 }])
})

test('GET /columns returns the exact bytes and the mtime', async () => {
  const response = await get('/columns/w1/0.0.bin')
  assert.equal(response.status, 200)
  assert.equal(response.headers.get('content-type'), 'application/octet-stream')
  assert.equal(response.headers.get('x-mtime'), String(1000 * 1000))
  assert.equal(response.headers.get('cache-control'), 'no-cache')
  assert.deepEqual(Buffer.from(await response.arrayBuffer()), columnBytes)
})

test('GET /columns handles negative coordinates', async () => {
  assert.equal((await get('/columns/w1/-1.-1.bin')).status, 200)
})

for (const [p, status] of [
  ['/columns/w1/9.9.bin', 404],
  ['/columns/w1/..%2F0.0.bin', 404],
  ['/columns/..%2Fw1/0.0.bin', 404],
  ['/columns/a/b/0.0.bin', 404],
  ['/columns/w1/0.0.bin%00', 404],
  ['/hud/..%2FBob', 404],
  ['/hud/a/b', 404],
  ['/hud/Nobody', 404],
  ['/pose/..%2FBob', 404],
  ['/web/..%2Fsecret.txt', 404],
  ['/web/secret.txt', 404],
  ['/web/nothere.mjs', 404],
  ['/blocks/abc.json', 404],
  ['/textures/abc.bin', 404],
  ['/textures/26.1.json', 404],
  ['/textures/a/26.1.bin', 404],
  ['/nothing', 404]
]) {
  test(`GET ${p} is ${status}`, async () => {
    assert.equal((await get(p)).status, status)
  })
}

test('GET /hud returns hud.json', async () => {
  const response = await get('/hud/Bob')
  assert.equal(response.status, 200)
  assert.deepEqual(await response.json(), hud)
})

test('GET / and /web/<file> serve the web dir with content types', async () => {
  const index = await get('/')
  assert.equal(await index.text(), '<h1>hi</h1>')
  assert.match(index.headers.get('content-type'), /text\/html/)
  assert.equal(index.headers.get('cache-control'), 'no-cache')
  const script = await get('/web/x.mjs')
  assert.equal(await script.text(), 'export const x = 1')
  assert.match(script.headers.get('content-type'), /javascript/)
})

test('SSE sends the pose and hud on connect, then a pose event on change', async () => {
  const response = await get('/pose/Bob?radius=2')
  assert.equal(response.headers.get('content-type'), 'text/event-stream')
  setTimeout(() => writeAt(path.join(viewDir, 'pose.json'), JSON.stringify({ ...pose, t: 6 }), 2000), 100)
  const started = Date.now()
  const events = await collect(response, { ms: 1500, done: es => es.filter(e => e.event === 'pose').length >= 2 })
  const poses = events.filter(e => e.event === 'pose')
  assert.equal(poses[0].data.pose.t, 5)
  assert.equal(poses[1].data.pose.t, 6)
  assert.equal(poses[1].data.mtime, 2000 * 1000)
  assert.equal(typeof poses[1].data.sentAt, 'number')
  assert.ok(Date.now() - started < 700)
  assert.deepEqual(events.find(e => e.event === 'hud').data, { mtime: 1000 * 1000, hud })
  writeAt(path.join(viewDir, 'pose.json'), JSON.stringify(pose), 1000)
})

test('SSE sends column events for changed columns inside the radius only', async () => {
  const response = await get('/pose/Bob?radius=2')
  setTimeout(() => {
    writeAt(path.join(chunkDir, '20.20.bin'), columnBytes, 3000)
    writeAt(path.join(chunkDir, '1.0.bin'), columnBytes, 3000)
  }, 200)
  const events = await collect(response, { ms: 900 })
  const columns = events.filter(e => e.event === 'column').map(e => e.data)
  assert.deepEqual(columns, [{ cx: 1, cz: 0, mtime: 3000 * 1000 }])
})

test('SSE for an offline pose without an eye sends it and no column events', async () => {
  fs.mkdirSync(path.join(stateDir, 'agents', 'Off', 'view'), { recursive: true })
  const offline = { v: 1, t: 9, world: 'w1', status: 'offline' }
  fs.writeFileSync(path.join(stateDir, 'agents', 'Off', 'view', 'pose.json'), JSON.stringify(offline))
  const response = await get('/pose/Off')
  setTimeout(() => writeAt(path.join(chunkDir, '0.0.bin'), columnBytes, 4000), 100)
  const events = await collect(response, { ms: 500 })
  assert.deepEqual(events.map(e => e.event), ['pose'])
  assert.deepEqual(events[0].data.pose, offline)
})

test('SSE for an unknown agent is 404', async () => {
  assert.equal((await get('/pose/Nobody')).status, 404)
})

test('GET /blocks/<version>.json serves the material table', async () => {
  const table = await (await get('/blocks/26.1.json')).json()
  assert.equal(table.version, '26.1')
  assert.ok(table.materials.length > 100)
  assert.equal(typeof table.format.maxBitsPerBlock, 'number')
})

test('GET /textures/<version>.bin serves the packed texture layers', async () => {
  const table = await (await get('/blocks/26.1.json')).json()
  assert.ok(table.textures.names.length > 100)
  const response = await get('/textures/26.1.bin')
  assert.equal(response.status, 200)
  assert.equal(response.headers.get('content-type'), 'application/octet-stream')
  const bytes = Buffer.from(await response.arrayBuffer())
  const expected = [0, 1, 2, 3, 4].reduce((sum, level) => sum + table.textures.names.length * (16 >> level) ** 2 * 4, 0)
  assert.equal(bytes.length, expected)
})

// ---- push: 'watch' ----

const watchRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'view-serve-watch-'))
const watchView = path.join(watchRoot, 'agents', 'Wat', 'view')
const atomicWrite = (file, value) => {
  const tmp = `${file}.tmp.${process.pid}`
  fs.writeFileSync(tmp, JSON.stringify(value))
  fs.renameSync(tmp, file)
}
const startWatchServer = async () => {
  fs.mkdirSync(watchView, { recursive: true })
  atomicWrite(path.join(watchView, 'pose.json'), { ...pose, t: 1 })
  atomicWrite(path.join(watchView, 'hud.json'), hud)
  const watched = createViewServer({ stateDir: watchRoot, textureDir: realTextures, webDir, push: 'watch', pollMs: 10000, watchFallbackMs: 10000, columnPollMs: 10000 })
  await new Promise(resolve => watched.listen(0, '127.0.0.1', resolve))
  return { watched, url: `http://127.0.0.1:${watched.address().port}` }
}
const fsEvents = () => process.getActiveResourcesInfo().filter(r => r === 'FSEventWrap').length

const watchCases = [
  { name: 'pose', file: 'pose.json', event: 'pose', value: { ...pose, t: 2 }, read: e => e.data.pose.t, expected: 2 },
  { name: 'hud', file: 'hud.json', event: 'hud', value: { ...hud, health: 11 }, read: e => e.data.hud.health, expected: 11 }
]
for (const { name, file, event, value, read, expected } of watchCases) {
  test(`watch: a rewritten ${name} (tmp + rename) is delivered within 30 ms with the fallback poll off`, async () => {
    const { watched, url } = await startWatchServer()
    const response = await fetch(`${url}/pose/Wat?radius=1`)
    const received = collect(response, { ms: 2000, done: es => es.filter(e => e.event === event).length >= 2 })
    await sleep(100)
    const wrote = Date.now()
    atomicWrite(path.join(watchView, file), value)
    const events = (await received).filter(e => e.event === event)
    const latency = Date.now() - wrote
    watched.closeAllConnections()
    watched.close()
    assert.equal(read(events[1]), expected)
    assert.ok(latency < 30, `delivered after ${latency} ms`)
  })
}

test('watch: no events while the files are unchanged', async () => {
  const { watched, url } = await startWatchServer()
  const response = await fetch(`${url}/pose/Wat?radius=1`)
  const events = await collect(response, { ms: 400 })
  watched.closeAllConnections()
  watched.close()
  assert.deepEqual(events.map(e => e.event).sort(), ['hud', 'pose'])
})

test('watch: the watcher is closed when the client disconnects', async () => {
  const { watched, url } = await startWatchServer()
  await sleep(100)
  const before = fsEvents()
  const response = await fetch(`${url}/pose/Wat?radius=1`)
  await sleep(100)
  const during = fsEvents()
  await response.body.cancel()
  await sleep(100)
  const after = fsEvents()
  await new Promise(resolve => watched.close(resolve))
  assert.deepEqual([during - before, after - before], [1, 0])
})

after(() => fs.rmSync(watchRoot, { recursive: true, force: true }))
