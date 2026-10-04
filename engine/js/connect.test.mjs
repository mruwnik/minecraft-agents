import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { readAgentConfig } from './connect.mjs'
import { bodyDir } from './bodies.mjs'

const stateWith = (agent, world) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'engine-connect-'))
  fs.mkdirSync(bodyDir(dir, 'w', 'Bob'), { recursive: true })
  fs.writeFileSync(path.join(bodyDir(dir, 'w', 'Bob'), 'config.json'), JSON.stringify(agent))
  fs.writeFileSync(path.join(dir, 'worlds', 'w', 'world.json'), JSON.stringify(world))
  return dir
}

test('readAgentConfig merges defaults, agent config and world server; the world is the one given', () => {
  const dir = stateWith({ username: 'Bob' }, { host: '10.0.0.1', port: 1234 })
  const cfg = readAgentConfig({ stateDir: dir, world: 'w', agent: 'Bob' })
  assert.deepEqual({ ...cfg, version: undefined }, { username: 'Bob', world: 'w', host: '10.0.0.1', port: 1234, auth: 'offline', version: undefined })
  assert.ok(cfg.version)
})

test('readAgentConfig ignores a world field left in config.json', () => {
  const dir = stateWith({ username: 'Bob', world: 'elsewhere' }, { host: 'h', port: 1 })
  assert.equal(readAgentConfig({ stateDir: dir, world: 'w', agent: 'Bob' }).world, 'w')
})

test('readAgentConfig refuses a missing world, a missing world.json and a config without a username', () => {
  const dir = stateWith({ username: 'Bob' }, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: dir, agent: 'Bob' }), /world/)
  const noName = stateWith({}, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: noName, world: 'w', agent: 'Bob' }), /no username/)
  fs.rmSync(path.join(dir, 'worlds', 'w', 'world.json'))
  assert.throws(() => readAgentConfig({ stateDir: dir, world: 'w', agent: 'Bob' }), /world\.json/)
})

test('readAgentConfig refuses a body that is not in that world', () => {
  const dir = stateWith({ username: 'Bob' }, { host: 'h', port: 1 })
  assert.throws(() => readAgentConfig({ stateDir: dir, world: 'w', agent: 'Nobody' }), /no .*config\.json/)
})
