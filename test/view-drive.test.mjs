// The view server's drive proxy: browser requests relayed to a body's control socket.
import { test, before, after } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { createViewServer } from '../tools/view/serve.mjs'
import { createControl } from '../engine/js/control.mjs'

const root = fs.mkdtempSync(path.join(os.tmpdir(), 'vd-'))
const stateDir = path.join(root, 'state')
const webDir = path.join(root, 'web')
const engineDir = path.join(stateDir, 'agents', 'Bob', 'engine')
const calls = []
const body = {
  status: () => ({ offline: false, settling: false, pos: { x: 0, y: 64, z: 0 } }),
  take: (a) => { calls.push(['take', a]); return { ok: true } },
  release: (a) => calls.push(['release', a]),
  drive: (a) => { calls.push(['drive', a]); return { pos: { x: 0, y: 64, z: 0 }, yaw: 0, pitch: 0 } },
  stopDriving: () => calls.push(['stop']),
  deadman: () => calls.push(['deadman'])
}
let control
let server
let base
let host

const post = (p, msg, headers = {}) => fetch(`${base}${p}`, {
  method: 'POST',
  headers: { 'Content-Type': 'application/json', ...headers },
  body: JSON.stringify(msg)
})

before(async () => {
  fs.mkdirSync(engineDir, { recursive: true })
  fs.mkdirSync(webDir)
  control = createControl({ socketPath: path.join(engineDir, 'control.sock'), body })
  await control.listen()
  server = createViewServer({ stateDir, textureDir: webDir, webDir })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  host = `127.0.0.1:${server.address().port}`
  base = `http://${host}`
})

after(async () => {
  await control.close()
  server.closeAllConnections()
  server.close()
  fs.rmSync(root, { recursive: true, force: true })
})

test('take, set, then GET reflects the controls; who defaults to view', async () => {
  const taken = await (await post('/drive/Bob', { op: 'take', why: 'test' })).json()
  assert.equal(taken.manual.who, 'view')
  const set = await post('/drive/Bob', { op: 'set', controls: { forward: true } })
  assert.equal(set.status, 200)
  const state = await (await fetch(`${base}/drive/Bob`)).json()
  assert.equal(state.manual.controls.forward, true)
  assert.equal(state.manual.who, 'view')
  await post('/drive/Bob', { op: 'release' })
  assert.equal((await (await fetch(`${base}/drive/Bob`)).json()).manual, null)
})

test('an explicit who is kept', async () => {
  const taken = await (await post('/drive/Bob', { op: 'take', who: 'claude', why: 'x' })).json()
  assert.equal(taken.manual.who, 'claude')
  await post('/drive/Bob', { op: 'release', who: 'claude' })
})

const forbidden = [
  ['bad Origin', { Origin: 'http://evil.example' }],
  ['text/plain', { 'Content-Type': 'text/plain' }]
]
for (const [name, headers] of forbidden) {
  test(`403 for ${name}`, async () => {
    const res = await post('/drive/Bob', { op: 'take', why: 'x' }, headers)
    assert.equal(res.status, 403)
    assert.equal((await res.json()).reason, 'forbidden')
  })
}

test('403 for a foreign Host header', async () => {
  const status = await new Promise((resolve, reject) => {
    const req = http.request({ host: '127.0.0.1', port: server.address().port, path: '/drive/Bob', method: 'POST', headers: { Host: 'evil.example', 'Content-Type': 'application/json' } }, res => { res.resume(); resolve(res.statusCode) })
    req.on('error', reject)
    req.end('{"op":"take"}')
  })
  assert.equal(status, 403)
})

test('503 when no body runs', async () => {
  const res = await post('/drive/Nobody', { op: 'take', why: 'x' })
  assert.equal(res.status, 503)
  assert.deepEqual(await res.json(), { ok: false, reason: 'no-body', text: 'no running body Nobody' })
})

test('GET with a bad name is 404', async () => {
  assert.equal((await fetch(`${base}/drive/bad.name`)).status, 404)
})

test('POST to anything but /drive/<name> is 405', async () => {
  assert.equal((await post('/agents', {})).status, 405)
})
