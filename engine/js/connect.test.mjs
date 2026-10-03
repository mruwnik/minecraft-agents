import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { readAgentConfig } from './connect.mjs'

const stateWith = (agent, world) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'engine-connect-'))
  fs.mkdirSync(path.join(dir, 'agents', 'Bob'), { recursive: true })
  fs.mkdirSync(path.join(dir, 'worlds', 'w'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'agents', 'Bob', 'config.json'), JSON.stringify(agent))
  fs.writeFileSync(path.join(dir, 'worlds', 'w', 'world.json'), JSON.stringify(world))
  return dir
}

test('readAgentConfig merges defaults, agent config and world server', () => {
  const dir = stateWith({ username: 'Bob', world: 'w' }, { host: '10.0.0.1', port: 1234 })
  const cfg = readAgentConfig({ stateDir: dir, agent: 'Bob' })
  assert.deepEqual({ ...cfg, version: undefined }, { username: 'Bob', world: 'w', host: '10.0.0.1', port: 1234, auth: 'offline', version: undefined })
  assert.ok(cfg.version)
})

test('readAgentConfig refuses an agent that names no world or no username', () => {
  const noWorld = stateWith({ username: 'Bob' }, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: noWorld, agent: 'Bob' }), /names no world/)
  const noName = stateWith({ world: 'w' }, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: noName, agent: 'Bob' }), /no username/)
})

test('readAgentConfig refuses a missing agent folder', () => {
  const dir = stateWith({ username: 'Bob', world: 'w' }, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: dir, agent: 'Nobody' }), /no .*config\.json/)
})
