// A body's identity comes from the config.json in its home folder. Without one, bot.mjs used to fall back to the
// username Claude on port 3777, so a `node src/bot.mjs` typed in the repo root logged in as Claude and kicked the
// real Claude body (duplicate_login, 2026-09-24 17:57Z). Now a missing config refuses to start and says where to run.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { readConfig, readWorld, missingConfig, DEFAULTS } from '../src/config.mjs'

const SERVER = { host: '127.0.0.1', port: 25568 }

// the real layout in miniature: <tmp>/worlds/<world>/agents/Steve inside <tmp>/worlds/<world>, beside its world.json
const setup = ({ config, world = 'main', worlds = { main: SERVER } } = {}) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'config-'))
  const home = path.join(root, 'worlds', world, 'agents', 'Steve')
  fs.mkdirSync(home, { recursive: true })
  for (const [name, server] of Object.entries(worlds)) {
    fs.mkdirSync(path.join(root, 'worlds', name), { recursive: true })
    if (server) fs.writeFileSync(path.join(root, 'worlds', name, 'world.json'), JSON.stringify(server))
  }
  if (config) fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify(config))
  return { root, home }
}

test('a home without config.json refuses to start and names the folder', () => {
  const { home } = setup()
  assert.throws(() => readConfig(home), { message: missingConfig(home) })
})

test('the refusal tells an agent where to run and how to make a body', () => {
  const text = missingConfig('/somewhere')
  assert.match(text, /\/somewhere\/config\.json/)
  assert.match(text, /state\/worlds\/<world>\/agents\/<Name>/)
  assert.match(text, /tools\/new-agent\.mjs/)
})

const merges = [
  ['only a username', { username: 'Steve' }, { ...DEFAULTS, username: 'Steve' }],
  ['a username and a port', { username: 'Steve', apiPort: 3790 }, { ...DEFAULTS, username: 'Steve', apiPort: 3790 }],
  ['extra keys pass through', { username: 'Steve', harness: 'claude-code' }, { ...DEFAULTS, username: 'Steve', harness: 'claude-code' }]
]
for (const [name, written, expected] of merges) {
  test(`config.json with ${name} fills in the defaults`, () => {
    const { root, home } = setup({ config: written })
    const worldDir = path.join(root, 'worlds', 'main')
    assert.deepEqual(readConfig(home), { ...expected, world: 'main', ...SERVER, worldDir })
  })
}

test('host, port and version come from world.json, and worldDir is the world directory', () => {
  const { root, home } = setup({ config: { username: 'Steve' }, world: 'creative', worlds: { creative: { host: 'mc.example', port: 25570, version: '1.21.4' } } })
  const cfg = readConfig(home)
  assert.deepEqual([cfg.host, cfg.port, cfg.version, cfg.worldDir], ['mc.example', 25570, '1.21.4', path.join(root, 'worlds', 'creative')])
})

test('readWorld names the world, its directory and the parsed world.json', () => {
  const { root, home } = setup({ config: { username: 'Steve' } })
  assert.deepEqual(readWorld(home), { name: 'main', dir: path.join(root, 'worlds', 'main'), server: SERVER })
})

test('the defaults name no server: that is the world\'s to say', () => {
  assert.deepEqual(['host' in DEFAULTS, 'port' in DEFAULTS], [false, false])
})

test('the world is where the folder is: a world field left in config.json is ignored', () => {
  const { home } = setup({ config: { username: 'Steve', world: 'elsewhere' } })
  assert.equal(readWorld(home).name, 'main')
  assert.equal(readConfig(home).world, 'main')
})

const missingWorlds = [
  ['a world directory with no world.json', { main: SERVER, bare: null }, 'bare', ['bare', 'main']],
  ['a world nobody set up', { main: SERVER }, 'nowhere', ['main']]
]
for (const [name, worlds, world, listed] of missingWorlds) {
  test(`${name} is refused, naming the world, its world.json and the worlds there are`, () => {
    const { root, home } = setup({ config: { username: 'Steve' }, world, worlds })
    const expected = [`"${world}"`, path.join(root, 'worlds', world, 'world.json'), ...listed]
    assert.throws(() => readConfig(home), err => expected.every(s => err.message.includes(s)))
  })
}

test('a config.json without a username is refused too', () => {
  const { home } = setup({ config: { apiPort: 3790 } })
  assert.throws(() => readConfig(home), /username/)
})

test('the defaults carry no username', () => {
  assert.equal('username' in DEFAULTS, false)
})

const auths = [
  ['defaults to offline', {}, 'offline'],
  ['may be microsoft', { auth: 'microsoft' }, 'microsoft']
]
for (const [name, extra, expected] of auths) {
  test(`auth ${name}`, () => {
    const { home } = setup({ config: { username: 'Steve', ...extra } })
    assert.equal(readConfig(home).auth, expected)
  })
}

test('any other auth is refused and the two choices are named', () => {
  const { home } = setup({ config: { username: 'Steve', auth: 'mojang' } })
  assert.throws(() => readConfig(home), /auth "mojang".*offline.*microsoft/)
})
