// control.mjs is a stateless HTTP adapter over a unix socket: these tests cover the framing only.
// The takeover rules are engine.lease (test/engine/lease_test.cljs), applied by engine.takeover
// (test/engine/takeover_test.cljs, which also runs test/contract/drive-contract.json).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { createControl } from './control.mjs'

const tmpSock = () => path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'ctl-')), 'control.sock')

const request = (socketPath, method, urlPath, payload) => new Promise((resolve, reject) => {
  const req = http.request({ socketPath, method, path: urlPath, headers: { 'content-type': 'application/json' } }, (res) => {
    let data = ''
    res.on('data', (c) => { data += c })
    res.on('end', () => resolve({ status: res.statusCode, type: res.headers['content-type'], text: data }))
  })
  req.on('error', reject)
  req.end(payload)
})

const rig = (socketPath = tmpSock()) => {
  const calls = []
  const handle = (method, p, body) => {
    calls.push([method, p, body])
    return { status: 201, json: { ok: true, echo: body } }
  }
  return { socketPath, calls, control: createControl({ socketPath, handle }) }
}

test('serves the handle reply over a real unix socket with mode 0600, and close removes the socket', async () => {
  const { socketPath, control } = rig()
  fs.writeFileSync(socketPath, 'stale')
  await control.listen()
  try {
    assert.equal(fs.statSync(socketPath).mode & 0o777, 0o600)
    const r = await request(socketPath, 'POST', '/drive', JSON.stringify({ op: 'take' }))
    assert.equal(r.status, 201)
    assert.match(r.type, /application\/json/)
    assert.deepEqual(JSON.parse(r.text), { ok: true, echo: { op: 'take' } })
  } finally {
    await control.close()
  }
  assert.equal(fs.existsSync(socketPath), false)
})

test('method, path (without the query) and parsed body reach handle; an empty body is null', async () => {
  const { socketPath, calls, control } = rig()
  await control.listen()
  try {
    await request(socketPath, 'POST', '/drive?x=1', JSON.stringify({ op: 'ping', n: [1] }))
    await request(socketPath, 'GET', '/drive')
  } finally {
    await control.close()
  }
  assert.deepEqual(calls, [['POST', '/drive', { op: 'ping', n: [1] }], ['GET', '/drive', null]])
})

test('invalid JSON is 400 bad-json and never reaches handle', async () => {
  const { socketPath, calls, control } = rig()
  await control.listen()
  try {
    const bad = await request(socketPath, 'POST', '/drive', '{nope')
    assert.equal(bad.status, 400)
    assert.deepEqual(JSON.parse(bad.text), { ok: false, reason: 'bad-json' })
  } finally {
    await control.close()
  }
  assert.equal(calls.length, 0)
})

test('rejects bodies over 16 KB', async () => {
  const { socketPath, calls, control } = rig()
  await control.listen()
  try {
    const big = JSON.stringify({ op: 'take', who: 'a', why: 'x'.repeat(20000) })
    const r = await request(socketPath, 'POST', '/drive', big).catch(e => ({ status: 'error', text: e.code }))
    assert.notEqual(r.status, 201)
  } finally {
    await control.close()
  }
  assert.equal(calls.length, 0)
})

test('a socket path over 100 bytes rejects listen', async () => {
  const { control } = rig(path.join(os.tmpdir(), 'x'.repeat(120), 'control.sock'))
  await assert.rejects(control.listen(), /too long for a unix socket/)
})

test('close without listen is a no-op', async () => {
  await rig().control.close()
})
