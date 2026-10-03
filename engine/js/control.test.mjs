import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { createControl } from './control.mjs'

const makeRig = (opts = {}) => {
  const calls = []
  const world = { offline: false, settling: false, pos: { x: 1, y: 2, z: 3 } }
  const clock = { t: 1000 }
  const body = {
    status: () => ({ ...world }),
    take: (a) => { calls.push(['take', a]); return { ok: true } },
    release: (a) => { calls.push(['release', a]) },
    drive: (a) => { calls.push(['drive', a]); return { pos: world.pos, yaw: 90, pitch: 10 } },
    stopDriving: () => { calls.push(['stopDriving']) },
    deadman: (a) => { calls.push(['deadman', a]) },
    ...opts.bodyOverrides
  }
  const control = createControl({ socketPath: opts.socketPath ?? '/unused', body, now: () => clock.t, ...opts.config })
  const post = (b) => control.handle({ method: 'POST', path: '/drive', body: b })
  const get = () => control.handle({ method: 'GET', path: '/drive', body: null })
  const names = (n) => calls.filter(c => c[0] === n)
  return { control, calls, world, clock, post, get, names }
}

const taken = async (opts) => {
  const rig = makeRig(opts)
  await rig.post({ op: 'take', who: 'claude', why: 'test' })
  return rig
}

test('take ok pauses the engine and reports the manual state', async () => {
  const { post, names } = makeRig()
  const r = await post({ op: 'take', who: 'claude', why: 'poke' })
  assert.equal(r.status, 200)
  assert.equal(r.json.ok, true)
  assert.equal(r.json.manual.who, 'claude')
  assert.equal(r.json.manual.why, 'poke')
  assert.deepEqual(names('take'), [['take', { who: 'claude', why: 'poke' }]])
})

for (const [label, patch, reason] of [
  ['offline', { offline: true }, 'offline'],
  ['settling', { settling: true }, 'settling']
]) {
  test(`take refused when ${label}`, async () => {
    const { world, post, names } = makeRig()
    Object.assign(world, patch)
    const r = await post({ op: 'take', who: 'claude', why: 'x' })
    assert.deepEqual(r.json, { ok: false, reason })
    assert.equal(names('take').length, 0)
  })
}

test('take refused when held by another', async () => {
  const { post } = await taken()
  const r = await post({ op: 'take', who: 'view', why: 'x' })
  assert.deepEqual(r.json, { ok: false, reason: 'held-by claude' })
})

test('take refused by the engine passes its reason through', async () => {
  const { post, get } = makeRig({ bodyOverrides: { take: () => ({ ok: false, reason: 'nope' }) } })
  const r = await post({ op: 'take', who: 'a', why: 'b' })
  assert.deepEqual(r.json, { ok: false, reason: 'nope' })
  assert.equal((await get()).json.manual, null)
})

test('same who taking again is idempotent', async () => {
  const { post, names } = await taken()
  const r = await post({ op: 'take', who: 'claude', why: 'again' })
  assert.equal(r.json.ok, true)
  assert.equal(names('take').length, 1)
})

test('take without who is bad-args', async () => {
  const { post } = makeRig()
  const r = await post({ op: 'take', why: 'x' })
  assert.equal(r.json.reason, 'bad-args')
})

for (const op of ['set', 'stop', 'ping', 'release']) {
  test(`${op} without a takeover is not-taken`, async () => {
    const { post } = makeRig()
    const r = await post({ op, who: 'claude', controls: { forward: true } })
    assert.deepEqual(r.json, { ok: false, reason: 'not-taken' })
  })

  test(`${op} by a non-driver is not-driver with the holder`, async () => {
    const { post } = await taken()
    const r = await post({ op, who: 'view', controls: { forward: true } })
    assert.deepEqual(r.json, { ok: false, reason: 'not-driver', holder: 'claude' })
  })
}

test('set passes controls and look to body.drive and GET reports them', async () => {
  const { post, get, names } = await taken()
  const r = await post({ op: 'set', who: 'claude', controls: { forward: true, sprint: true }, look: { yaw: 180, pitch: 5 } })
  assert.equal(r.json.ok, true)
  assert.deepEqual(r.json.pos, { x: 1, y: 2, z: 3 })
  assert.deepEqual(names('drive').at(-1)[1], { controls: { forward: true, sprint: true }, look: { yaw: 180, pitch: 5 } })
  const g = (await get()).json
  assert.equal(g.manual.controls.forward, true)
  assert.equal(g.manual.controls.sprint, true)
  assert.equal(g.manual.controls.back, false)
  assert.equal(g.manual.yaw, 90)
  assert.equal(g.manual.pitch, 10)
  assert.deepEqual(g.pos, { x: 1, y: 2, z: 3 })
})

test('set passes relative look through as given', async () => {
  const { post, names } = await taken()
  await post({ op: 'set', who: 'claude', look: { dyaw: 15, dpitch: -3 } })
  assert.deepEqual(names('drive').at(-1)[1].look, { dyaw: 15, dpitch: -3 })
})

test('GET with no takeover reports manual null', async () => {
  const { get } = makeRig()
  const g = (await get()).json
  assert.equal(g.ok, true)
  assert.equal(g.manual, null)
  assert.equal(g.offline, false)
  assert.equal(g.settling, false)
})

for (const [label, args] of [
  ['bad control name', { controls: { fly: true } }],
  ['non-boolean value', { controls: { forward: 1 } }],
  ['controls not an object', { controls: 'forward' }],
  ['ms zero', { controls: { jump: true }, ms: 0 }],
  ['ms too large', { controls: { jump: true }, ms: 10001 }],
  ['ms fractional', { controls: { jump: true }, ms: 1.5 }],
  ['ms string', { controls: { jump: true }, ms: '5' }],
  ['look not numbers', { look: { yaw: 'north', pitch: 0 } }],
  ['look not an object', { look: 5 }]
]) {
  test(`set with ${label} is bad-args`, async () => {
    const { post, names } = await taken()
    const r = await post({ op: 'set', who: 'claude', ...args })
    assert.equal(r.status, 200)
    assert.equal(r.json.ok, false)
    assert.equal(r.json.reason, 'bad-args')
    assert.equal(typeof r.json.text, 'string')
    assert.equal(names('drive').length, 0)
  })
}

test('timed hold releases only the named controls after ms', async () => {
  const { post, control, clock, names, get } = await taken()
  await post({ op: 'set', who: 'claude', controls: { forward: true } })
  await post({ op: 'set', who: 'claude', controls: { jump: true }, ms: 300 })
  clock.t += 299
  control.tick()
  assert.equal((await get()).json.manual.controls.jump, true)
  clock.t += 2
  control.tick()
  assert.deepEqual(names('drive').at(-1)[1], { controls: { jump: false } })
  const c = (await get()).json.manual.controls
  assert.equal(c.jump, false)
  assert.equal(c.forward, true)
})

test('timed hold does not release a control that was set again later', async () => {
  const { post, control, clock, names, get } = await taken()
  await post({ op: 'set', who: 'claude', controls: { jump: true }, ms: 300 })
  clock.t += 100
  await post({ op: 'set', who: 'claude', controls: { jump: true } })
  const before = names('drive').length
  clock.t += 500
  control.tick()
  assert.equal(names('drive').length, before)
  assert.equal((await get()).json.manual.controls.jump, true)
})

test('dead-man releases untimed controls after releaseMs and warns once', async () => {
  const { post, control, clock, names, get } = await taken({ config: { releaseMs: 2000 } })
  await post({ op: 'set', who: 'claude', controls: { forward: true } })
  clock.t += 1999
  control.tick()
  assert.equal(names('stopDriving').length, 0)
  clock.t += 1
  control.tick()
  control.tick()
  clock.t += 500
  control.tick()
  assert.equal(names('stopDriving').length, 1)
  assert.deepEqual(names('deadman'), [['deadman', { who: 'claude', silentMs: 2000 }]])
  assert.equal((await get()).json.manual.controls.forward, false)
})

test('dead-man default releases untimed controls after 1000 ms of silence, not at 999', async () => {
  const { post, control, clock, names } = await taken()
  await post({ op: 'set', who: 'claude', controls: { forward: true } })
  clock.t += 999
  control.tick()
  assert.equal(names('stopDriving').length, 0)
  clock.t += 1
  control.tick()
  assert.equal(names('stopDriving').length, 1)
})

test('dead-man does nothing when no untimed control is held', async () => {
  const { control, clock, names } = await taken()
  clock.t += 5000
  control.tick()
  assert.equal(names('deadman').length, 0)
})

test('dead-man does not fire for timed holds', async () => {
  const { post, control, clock, names } = await taken()
  await post({ op: 'set', who: 'claude', controls: { forward: true }, ms: 10000 })
  clock.t += 3000
  control.tick()
  assert.equal(names('deadman').length, 0)
})

test('ping resets the silence', async () => {
  const { post, control, clock, names } = await taken()
  await post({ op: 'set', who: 'claude', controls: { forward: true } })
  clock.t += 700
  assert.equal((await post({ op: 'ping', who: 'claude' })).json.ok, true)
  clock.t += 700
  control.tick()
  assert.equal(names('deadman').length, 0)
  clock.t += 400
  control.tick()
  assert.equal(names('deadman').length, 1)
})

test('idle ends the takeover with reason idle', async () => {
  const { control, clock, names, get } = await taken({ config: { idleMs: 60000 } })
  clock.t += 59999
  control.tick()
  assert.equal(names('release').length, 0)
  clock.t += 1
  control.tick()
  assert.deepEqual(names('release'), [['release', { who: 'claude', reason: 'idle', heldMs: 60000 }]])
  assert.equal((await get()).json.manual, null)
})

test('going offline during a takeover ends it with reason offline', async () => {
  const { control, world, clock, names, get } = await taken()
  clock.t += 10
  world.offline = true
  control.tick()
  assert.deepEqual(names('release'), [['release', { who: 'claude', reason: 'offline', heldMs: 10 }]])
  assert.equal((await get()).json.manual, null)
})

test('release by the driver is released', async () => {
  const { post, clock, names, get } = await taken()
  clock.t += 42
  const r = await post({ op: 'release', who: 'claude' })
  assert.equal(r.json.ok, true)
  assert.deepEqual(names('release'), [['release', { who: 'claude', reason: 'released', heldMs: 42 }]])
  assert.equal((await get()).json.manual, null)
})

test('release by another without force is refused', async () => {
  const { post, names } = await taken()
  const r = await post({ op: 'release', who: 'view' })
  assert.deepEqual(r.json, { ok: false, reason: 'not-driver', holder: 'claude' })
  assert.equal(names('release').length, 0)
})

test('release by another with force is forced', async () => {
  const { post, names } = await taken()
  const r = await post({ op: 'release', who: 'view', force: true })
  assert.equal(r.json.ok, true)
  assert.equal(names('release')[0][1].reason, 'forced')
  assert.equal(names('release')[0][1].who, 'claude')
})

test('stop clears every control through the body', async () => {
  const { post, names, get } = await taken()
  await post({ op: 'set', who: 'claude', controls: { forward: true } })
  const r = await post({ op: 'stop', who: 'claude' })
  assert.equal(r.json.ok, true)
  assert.equal(names('stopDriving').length, 1)
  assert.equal((await get()).json.manual.controls.forward, false)
})

test('close ends a held takeover with reason shutdown', async () => {
  const { control, names } = await taken()
  await control.close()
  assert.equal(names('release')[0][1].reason, 'shutdown')
})

for (const [label, req, status] of [
  ['unknown op', { method: 'POST', path: '/drive', body: { op: 'dance', who: 'a' } }, 400],
  ['missing body', { method: 'POST', path: '/drive', body: null }, 400],
  ['non-object body', { method: 'POST', path: '/drive', body: [1] }, 400],
  ['unknown path', { method: 'GET', path: '/nope', body: null }, 404],
  ['wrong method', { method: 'DELETE', path: '/drive', body: null }, 404]
]) {
  test(`${label} gives ${status}`, async () => {
    const { control } = makeRig()
    assert.equal((await control.handle(req)).status, status)
  })
}

const request = (socketPath, method, urlPath, payload) => new Promise((resolve, reject) => {
  const req = http.request({ socketPath, method, path: urlPath, headers: { 'content-type': 'application/json' } }, (res) => {
    let data = ''
    res.on('data', (c) => { data += c })
    res.on('end', () => resolve({ status: res.statusCode, type: res.headers['content-type'], text: data }))
  })
  req.on('error', reject)
  req.end(payload)
})

const tmpSock = () => path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'ctl-')), 'control.sock')

test('serves take and GET over a real unix socket with mode 0600', async () => {
  const socketPath = tmpSock()
  fs.writeFileSync(socketPath, 'stale')
  const { control } = makeRig({ socketPath })
  await control.listen()
  try {
    assert.equal(fs.statSync(socketPath).mode & 0o777, 0o600)
    const t = await request(socketPath, 'POST', '/drive', JSON.stringify({ op: 'take', who: 'claude', why: 'x' }))
    assert.equal(t.status, 200)
    assert.match(t.type, /application\/json/)
    assert.equal(JSON.parse(t.text).ok, true)
    const g = await request(socketPath, 'GET', '/drive')
    assert.equal(JSON.parse(g.text).manual.who, 'claude')
    const bad = await request(socketPath, 'POST', '/drive', '{nope')
    assert.equal(bad.status, 400)
    assert.deepEqual(JSON.parse(bad.text), { ok: false, reason: 'bad-json' })
  } finally {
    await control.close()
  }
  assert.equal(fs.existsSync(socketPath), false)
})

test('rejects bodies over 16 KB', async () => {
  const socketPath = tmpSock()
  const { control } = makeRig({ socketPath })
  await control.listen()
  try {
    const big = JSON.stringify({ op: 'take', who: 'a', why: 'x'.repeat(20000) })
    const r = await request(socketPath, 'POST', '/drive', big).catch(e => ({ status: 'error', text: e.code }))
    assert.notEqual(r.status, 200)
  } finally {
    await control.close()
  }
})

test('a socket path over 100 bytes rejects listen', async () => {
  const socketPath = path.join(os.tmpdir(), 'x'.repeat(120), 'control.sock')
  const { control } = makeRig({ socketPath })
  await assert.rejects(control.listen(), /too long for a unix socket/)
})

test('idle defaults to 15 s', async () => {
  const { control, clock, names } = await taken()
  clock.t += 14999
  control.tick()
  assert.equal(names('release').length, 0)
  clock.t += 1
  control.tick()
  assert.equal(names('release')[0][1].reason, 'idle')
})

test('take idleS overrides the idle limit for that takeover', async () => {
  const rig = makeRig()
  await rig.post({ op: 'take', who: 'claude', why: 'x', idleS: 120 })
  rig.clock.t += 119999
  rig.control.tick()
  assert.equal(rig.names('release').length, 0)
  rig.clock.t += 1
  rig.control.tick()
  assert.equal(rig.names('release')[0][1].reason, 'idle')
})

for (const idleS of [0, 0.5, 3601, '5', null, NaN, true]) {
  test(`take with idleS ${String(idleS)} is bad-args and takes nothing`, async () => {
    const { post, names, get } = makeRig()
    const r = await post({ op: 'take', who: 'claude', why: 'x', idleS })
    assert.equal(r.json.reason, 'bad-args')
    assert.equal(names('take').length, 0)
    assert.equal((await get()).json.manual, null)
  })
}

test('the lease view reports idleMs, expiresAt and idleLeftS, counting down from the last op', async () => {
  const rig = makeRig()
  const r = await rig.post({ op: 'take', who: 'claude', why: 'x', idleS: 30 })
  assert.equal(r.json.manual.idleMs, 30000)
  assert.equal(r.json.manual.expiresAt, 31000)
  assert.equal(r.json.manual.idleLeftS, 30)
  rig.clock.t += 12340
  const g = (await rig.get()).json.manual
  assert.equal(g.expiresAt, 31000)
  assert.equal(g.idleLeftS, 17.7)
  const p = (await rig.post({ op: 'ping', who: 'claude' })).json.manual
  assert.equal(p.expiresAt, 43340)
  assert.equal(p.idleLeftS, 30)
})

test('idleLeftS never goes below zero', async () => {
  const { clock, get } = await taken()
  clock.t += 20000
  assert.equal((await get()).json.manual.idleLeftS, 0)
})

test('GET does not touch the lease: it leaves the silence clock and expiry alone', async () => {
  const { control, clock, get, names } = await taken()
  clock.t += 14000
  await get()
  await get()
  assert.equal((await get()).json.manual.expiresAt, 16000)
  clock.t += 1000
  control.tick()
  assert.equal(names('release')[0][1].reason, 'idle')
})
