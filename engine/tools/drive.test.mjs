import { test } from 'node:test'
import assert from 'node:assert/strict'
import { execFile, spawnSync } from 'node:child_process'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createControl } from '../js/control.mjs'
import { socketPathFor } from './drive-lib.mjs'

const cli = path.join(path.dirname(fileURLToPath(import.meta.url)), 'drive.mjs')
const run = (args) => new Promise((resolve) => {
  execFile('node', [cli, ...args], (err, stdout, stderr) => resolve({ code: err ? err.code : 0, stdout, stderr }))
})

// Canned replies: the CLI is under test here, not the takeover rules.
const handle = (method, p, body) => {
  if (method === 'GET') return { status: 200, json: { ok: true, manual: { who: 'claude', why: 'cli test' } } }
  if (body?.op === 'take') return { status: 200, json: { ok: true, manual: { who: body.who, why: body.why } } }
  return { status: 200, json: { ok: false, reason: 'not-driver', holder: 'claude' } }
}

const stateDir = () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'drv-'))
  fs.mkdirSync(path.join(dir, 'worlds', 'w', 'agents', 'Bob', 'engine'), { recursive: true })
  return dir
}

test('the CLI takes and reports state against a live control socket', async () => {
  const state = stateDir()
  const control = createControl({ socketPath: socketPathFor({ state, world: 'w', agent: 'Bob' }), handle })
  await control.listen()
  try {
    const taken = await run(['Bob', 'take', '--why', 'cli test', '--world', 'w', '--state', state])
    assert.equal(taken.code, 0)
    assert.equal(JSON.parse(taken.stdout).manual.who, 'claude')
    const st = await run(['Bob', 'state', '--world', 'w', '--state', state])
    assert.equal(st.code, 0)
    assert.equal(JSON.parse(st.stdout).manual.why, 'cli test')
    const refused = await run(['Bob', 'stop', '--who', 'other', '--world', 'w', '--state', state])
    assert.equal(refused.code, 1)
    assert.equal(JSON.parse(refused.stdout).reason, 'not-driver')
  } finally {
    await control.close()
  }
})

test('a missing socket exits 2 with a message', () => {
  const state = stateDir()
  const r = spawnSync('node', [cli, 'Bob', 'state', '--world', 'w', '--state', state], { encoding: 'utf8' })
  assert.equal(r.status, 2)
  assert.match(r.stderr, /no running body Bob \(no control socket at .*control\.sock\)/)
})

test('no --world exits 2 naming the flag', () => {
  const r = spawnSync('node', [cli, 'Bob', 'state', '--state', stateDir()], { encoding: 'utf8' })
  assert.equal(r.status, 2)
  assert.match(r.stderr, /missing --world <world>/)
})

test('bad usage exits 2', () => {
  const r = spawnSync('node', [cli, 'Bob', 'dance'], { encoding: 'utf8' })
  assert.equal(r.status, 2)
  assert.match(r.stderr, /usage/i)
})
