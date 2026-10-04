import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { requestFor, exitCodeFor, socketPathFor, defaultStateDir } from './drive-lib.mjs'

const post = (body) => ({ method: 'POST', path: '/drive', body })

for (const [argv, expected] of [
  [['Bob', 'take', '--why', 'poking'], post({ op: 'take', who: 'claude', why: 'poking' })],
  [['Bob', 'take'], post({ op: 'take', who: 'claude', why: '' })],
  [['Bob', 'take', '--idle-s', '120', '--why', 'x'], post({ op: 'take', who: 'claude', why: 'x', idleS: 120 })],
  [['Bob', 'hold', 'forward', '500', '--who', 'view'], post({ op: 'set', who: 'view', controls: { forward: true }, ms: 500 })],
  [['Bob', 'hold', 'forward,sprint', '1000'], post({ op: 'set', who: 'claude', controls: { forward: true, sprint: true }, ms: 1000 })],
  [['Bob', 'look', '180', '-10'], post({ op: 'set', who: 'claude', look: { yaw: 180, pitch: -10 } })],
  [['Bob', 'turn', '15'], post({ op: 'set', who: 'claude', look: { dyaw: 15, dpitch: 0 } })],
  [['Bob', 'turn', '-15', '5'], post({ op: 'set', who: 'claude', look: { dyaw: -15, dpitch: 5 } })],
  [['Bob', 'jump'], post({ op: 'set', who: 'claude', controls: { jump: true }, ms: 300 })],
  [['Bob', 'stop'], post({ op: 'stop', who: 'claude' })],
  [['Bob', 'ping'], post({ op: 'ping', who: 'claude' })],
  [['Bob', 'release'], post({ op: 'release', who: 'claude' })],
  [['Bob', 'release', '--force'], post({ op: 'release', who: 'claude', force: true })],
  [['Bob', 'state'], { method: 'GET', path: '/drive', body: null }]
]) {
  test(`requestFor ${argv.join(' ')}`, () => {
    const r = requestFor([...argv, '--world', 'w'])
    assert.equal(r.error, undefined)
    assert.equal(r.agent, 'Bob')
    assert.deepEqual({ method: r.method, path: r.path, body: r.body }, expected)
  })
}

for (const argv of [
  [],
  ['Bob'],
  ['Bob', 'dance'],
  ['Bob', 'hold'],
  ['Bob', 'hold', 'forward'],
  ['Bob', 'hold', 'forward', 'abc'],
  ['Bob', 'hold', 'fly', '100'],
  ['Bob', 'look', '10'],
  ['Bob', 'look', 'a', 'b'],
  ['Bob', 'turn'],
  ['Bob', 'turn', 'x'],
  ['Bob', 'take', '--bogus']
]) {
  test(`requestFor rejects ${JSON.stringify(argv)}`, () => {
    assert.equal(typeof requestFor([...argv, '--world', 'w']).error, 'string')
  })
}

test('requestFor reads --world', () => {
  assert.equal(requestFor(['Bob', 'state', '--world', 'w2']).world, 'w2')
})

test('requestFor without --world is an error naming the flag', () => {
  assert.match(requestFor(['Bob', 'state']).error, /missing --world <world>/)
})

test('requestFor refuses a world that is not a name', () => {
  assert.match(requestFor(['Bob', 'state', '--world', '../x']).error, /world/)
})

test('requestFor reads --state', () => {
  assert.equal(requestFor(['Bob', 'state', '--world', 'w', '--state', '/x/y']).state, '/x/y')
})

test('requestFor defaults state to <repo>/state', () => {
  assert.equal(requestFor(['Bob', 'state', '--world', 'w']).state, defaultStateDir)
  assert.equal(path.basename(defaultStateDir), 'state')
})

test('socketPathFor builds the socket path of a body in its world', () => {
  assert.equal(socketPathFor({ state: '/s', world: 'w', agent: 'Bob' }), '/s/worlds/w/agents/Bob/engine/control.sock')
})

for (const [reply, code] of [
  [{ status: 200, json: { ok: true } }, 0],
  [{ status: 200, json: { ok: false, reason: 'offline' } }, 1],
  [{ status: 400, json: { ok: false } }, 1],
  [{ status: 404, json: {} }, 1],
  [{ status: 200, json: {} }, 1]
]) {
  test(`exitCodeFor ${reply.status} ${JSON.stringify(reply.json)}`, () => {
    assert.equal(exitCodeFor(reply), code)
  })
}

for (const argv of [['Bob', 'take', '--idle-s', 'abc'], ['Bob', 'take', '--idle-s', '']]) {
  test(`requestFor ${argv.join(' ')} is an error`, () => {
    assert.match(requestFor([...argv, '--world', 'w']).error, /idle-s/)
  })
}
