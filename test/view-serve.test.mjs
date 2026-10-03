// The view server: static pages, column files, agent list and the pose/hud/column event stream.
import { test, before, after } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import zlib from 'node:zlib'
import { createViewServer } from '../tools/view/serve.mjs'
import { makeChunkClass } from '../tools/view/columns.mjs'

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
  server = createViewServer({ stateDir, textureDir: realTextures, webDir, pollMs: 20, columnPollMs: 50, blockJar: null, blockSweepMs: 100, blockWriteMs: 100 })
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

test('a column replaced by rename reaches the stream within 100 ms with the poll at 10 s', async () => {
  const watched = createViewServer({ stateDir, textureDir: realTextures, webDir, pollMs: 20, columnPollMs: 10000 })
  await new Promise(resolve => watched.listen(0, '127.0.0.1', resolve))
  const response = await fetch(`http://127.0.0.1:${watched.address().port}/pose/Bob?radius=2`)
  let wroteAt = 0
  setTimeout(() => {
    const tmp = path.join(chunkDir, '1.0.bin.tmp')
    writeAt(tmp, columnBytes, 4000)
    wroteAt = Date.now()
    fs.renameSync(tmp, path.join(chunkDir, '1.0.bin'))
  }, 300)
  const events = await collect(response, { ms: 1500, done: es => es.some(e => e.event === 'column') })
  const seenAt = Date.now()
  watched.closeAllConnections()
  watched.close()
  assert.deepEqual(events.filter(e => e.event === 'column').map(e => e.data), [{ cx: 1, cz: 0, mtime: 4000 * 1000 }])
  assert.ok(seenAt - wroteAt < 100, `took ${seenAt - wroteAt} ms`)
})

// ---- /block-issues: the blocks the view draws wrong, counted in the world's column files ----

const MC_VERSION = '1.21.4'
const Chunk = makeChunkClass(MC_VERSION)
const sand = Chunk.registry.blocksByName.suspicious_sand.defaultState
const gravel = Chunk.registry.blocksByName.suspicious_gravel.defaultState

// the column file format v1 around a column holding `blocks`: [[x, y, z, stateId]...] (local x and z)
const columnFile = ({ cx, cz, body, blocks }) => {
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  blocks.forEach(([x, y, z, id]) => column.setBlockStateId({ x, y, z }, id))
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: cx, z: cz, t: 1, body, mcVersion: MC_VERSION, minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return zlib.deflateSync(Buffer.concat([length, header, sections]))
}

const until = async (read, ms = 4000) => {
  const deadline = Date.now() + ms
  let value = await read()
  while (!value && Date.now() < deadline) {
    await sleep(50)
    value = await read()
  }
  return value
}
const issuesOf = async world => (await get(`/block-issues/${world}`)).json()
const recordOf = (body, name) => body.records.find(r => r.name === name && r.reason === 'no-texture')

test('GET /block-issues counts flagged blocks per name, with the first position and the agent, sorted by seen', async () => {
  const dir = path.join(stateDir, 'worlds', 'w2', 'chunks')
  fs.mkdirSync(dir, { recursive: true })
  fs.writeFileSync(path.join(dir, '2.-1.bin'), columnFile({ cx: 2, cz: -1, body: 'Ann', blocks: [[1, 70, 2, sand], [5, 71, 2, sand], [5, 72, 2, sand], [6, 70, 6, gravel], [7, 70, 6, gravel]] }))
  const body = await issuesOf('w2')
  assert.equal(body.version, 1)
  assert.equal(body.jar, null)
  assert.equal(recordOf(body, 'suspicious_sand').seen, 3)
  assert.deepEqual(recordOf(body, 'suspicious_sand').firstSeen, { world: 'w2', x: 33, y: 70, z: -14, agent: 'Ann' })
  assert.equal(recordOf(body, 'suspicious_gravel').seen, 2)
  const seen = body.records.map(r => r.seen)
  assert.deepEqual(seen, [...seen].sort((a, b) => b - a))
  assert.ok(await until(() => fs.existsSync(path.join(stateDir, 'worlds', 'w2', 'view-block-issues.json'))))
  assert.equal(JSON.parse(fs.readFileSync(path.join(stateDir, 'worlds', 'w2', 'view-block-issues.json'), 'utf8')).records.find(r => r.name === 'suspicious_sand').seen, 3)
})

test('rewriting a column replaces its counts instead of adding to them', async () => {
  const file = path.join(stateDir, 'worlds', 'w2', 'chunks', '2.-1.bin')
  fs.writeFileSync(file, columnFile({ cx: 2, cz: -1, body: 'Ann', blocks: [[3, 80, 3, sand]] }))
  fs.utimesSync(file, 5000, 5000)
  const sandNow = await until(async () => (await issuesOf('w2')).records.find(r => r.name === 'suspicious_sand')?.seen === 1)
  assert.ok(sandNow)
  const body = await issuesOf('w2')
  assert.equal(recordOf(body, 'suspicious_gravel').seen, 0)
  assert.deepEqual(recordOf(body, 'suspicious_sand').firstSeen, { world: 'w2', x: 35, y: 80, z: -13, agent: 'Ann' })
})

test('GET /block-issues for a world without columns is 404', async () => {
  assert.equal((await get('/block-issues/nothere')).status, 404)
  assert.equal((await get('/block-issues/..%2Fw2')).status, 404)
})

test('GET /blocks?debug=1 marks a material with the worst severity of its block, and the plain table has no issue field', async () => {
  const plain = await (await get('/blocks/1.21.4.json')).json()
  const debug = await (await get('/blocks/1.21.4.json?debug=1')).json()
  const issueOf = (table, name) => [...new Set(table.materials.filter(m => m.name === name).map(m => m.issue))]
  assert.deepEqual(issueOf(plain, 'suspicious_sand'), [undefined])
  assert.deepEqual(issueOf(debug, 'suspicious_sand'), ['missing'])
  assert.deepEqual(issueOf(debug, 'stone'), [undefined])
  assert.deepEqual((await (await get('/blocks/1.21.4.json?debug=2')).json()).materials, debug.materials)
})

const biomesFile = path.join(stateDir, 'worlds', 'w1', 'biomes.json')

test('GET /biomes/<world>.json builds colours from the world biomes.json', async () => {
  writeAt(biomesFile, JSON.stringify({ v: 1, mcVersion: '26.1', biomes: [{ id: 0, name: 'plains' }, { id: 1, name: 'swamp' }] }), 2000)
  const body = await (await get('/biomes/w1.json')).json()
  assert.deepEqual(body.names, ['plains', 'swamp'])
  assert.equal(body.colors.length, 24)
  assert.deepEqual(body.colors.slice(12, 15), [0x6A, 0x70, 0x39])
  assert.equal(body.fallback, false)
})

test('GET /biomes/<world>.json rebuilds when biomes.json changes', async () => {
  writeAt(biomesFile, JSON.stringify({ v: 1, mcVersion: '26.1', biomes: [{ id: 0, name: 'swamp' }] }), 3000)
  const body = await (await get('/biomes/w1.json')).json()
  assert.deepEqual(body.names, ['swamp'])
})

test('GET /biomes/<world>.json falls back to minecraft-data order without biomes.json', async () => {
  fs.rmSync(biomesFile)
  const body = await (await get('/biomes/w1.json?v=26.1')).json()
  assert.equal(body.fallback, true)
  assert.ok(body.names.includes('plains'))
  assert.equal(body.colors.length, body.names.length * 12)
})

for (const p of ['/biomes/nope.json', '/biomes/..%2Fw1.json', '/biomes/a.b.json']) {
  test(`GET ${p} is 404`, async () => assert.equal((await get(p)).status, 404))
}

const writeAgent = (name, value, hudValue) => {
  const dir = path.join(stateDir, 'agents', name, 'view')
  fs.mkdirSync(dir, { recursive: true })
  fs.writeFileSync(path.join(dir, 'pose.json'), JSON.stringify(value))
  if (hudValue) fs.writeFileSync(path.join(dir, 'hud.json'), JSON.stringify(hudValue))
}

test('/poses sends every agent\'s pose and hud tagged with its name, one stream', async () => {
  writeAgent('Two', { ...pose, t: 7 }, { v: 1, health: 11 })
  const response = await get('/poses?agents=Bob,Two&radius=2')
  assert.equal(response.headers.get('content-type'), 'text/event-stream')
  const events = await collect(response, { ms: 1500, done: es => es.filter(e => e.event === 'pose').length >= 2 && es.some(e => e.event === 'hud' && e.data.agent === 'Two') })
  const poses = events.filter(e => e.event === 'pose')
  assert.deepEqual(poses.map(e => [e.data.agent, e.data.pose.t]).sort(), [['Bob', 5], ['Two', 7]])
  assert.deepEqual(events.find(e => e.event === 'hud' && e.data.agent === 'Two').data.hud, { v: 1, health: 11 })
})

test('/poses sends a pose change and a column change with the agent', async () => {
  writeAgent('Two', { ...pose, t: 7 })
  const response = await get('/poses?agents=Two&radius=1')
  setTimeout(() => {
    writeAt(path.join(viewDir, 'pose.json'), JSON.stringify(pose), 1000) // not watched: Bob is not in the stream
    writeAt(path.join(stateDir, 'agents', 'Two', 'view', 'pose.json'), JSON.stringify({ ...pose, t: 8 }), 5000)
    writeAt(path.join(chunkDir, '1.0.bin'), columnBytes, 6000)
  }, 150)
  const events = await collect(response, { ms: 1200, done: es => es.some(e => e.event === 'column') && es.filter(e => e.event === 'pose').length >= 2 })
  assert.deepEqual(events.filter(e => e.event === 'pose').map(e => [e.data.agent, e.data.pose.t]), [['Two', 7], ['Two', 8]])
  assert.deepEqual(events.find(e => e.event === 'column').data, { agent: 'Two', cx: 1, cz: 0, mtime: 6000 * 1000 })
})

test('/poses skips agents without a pose and still streams the others', async () => {
  const response = await get('/poses?agents=Nobody,Bob')
  const events = await collect(response, { ms: 600, done: es => es.some(e => e.event === 'pose') })
  assert.deepEqual(events.filter(e => e.event === 'pose').map(e => e.data.agent), ['Bob'])
})

const badQueries = [
  ['no agents', '/poses'],
  ['empty list', '/poses?agents='],
  ['a bad name', '/poses?agents=Bob,..%2FBob'],
  ['a path name', '/poses?agents=Bob,a/b'],
  ['more than 32 agents', `/poses?agents=${Array.from({ length: 33 }, (_, i) => `A${i}`).join(',')}`]
]
for (const [name, url] of badQueries) {
  test(`/poses with ${name} is 400`, async () => assert.equal((await get(url)).status, 400))
}

test('/poses is a GET only and /pose/<Name> still works', async () => {
  assert.equal((await get('/poses?agents=Bob', { method: 'POST' })).status, 405)
  assert.equal((await get('/pose/Bob')).status, 200)
})
