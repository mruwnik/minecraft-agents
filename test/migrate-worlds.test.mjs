// One-shot move from the old flat state/ layout to state/worlds/<world>/ (team-lead runs this on the real state;
// these tests only ever touch temp dirs built to the same shape).
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'

const REPO = path.join(import.meta.dirname, '..')
const SCRIPT = path.join(REPO, 'tools', 'migrate-worlds.mjs')
const WORLD_FILES = ['places.json', 'zones.json', 'gates.log', 'clock.json', 'WORLD.md']
const SERVER = { host: '127.0.0.1', port: 25568 }

// the old layout in miniature: shared files flat in <state>/, agents under <state>/agents/<name>/ with host/port
// still in their own config.json (that is what the migration moves out)
const fixture = t => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'migrate-worlds-'))
  t.after(() => fs.rmSync(state, { recursive: true, force: true }))
  for (const name of WORLD_FILES) fs.writeFileSync(path.join(state, name), `${name} content\n`)
  const agentDir = name => path.join(state, 'agents', name)
  fs.mkdirSync(agentDir('Aiel'), { recursive: true })
  fs.writeFileSync(path.join(agentDir('Aiel'), 'config.json'), JSON.stringify({ username: 'Aiel', apiPort: 0, harness: 'claude-code', ...SERVER }, null, 1) + '\n')
  fs.writeFileSync(path.join(agentDir('Aiel'), 'BRIEFING.md'), '# You are Aiel\n\nread `../../WORLD.md` first.\n')
  return { state, agentDir }
}

const run = (world, state, extraArgs = []) =>
  spawnSync(process.execPath, [SCRIPT, world, '--state', state, ...extraArgs], { encoding: 'utf8' })

const readJSON = file => JSON.parse(fs.readFileSync(file, 'utf8'))

test('migrate-worlds: moves shared files, writes world.json, rewrites configs and BRIEFING.md', t => {
  const { state, agentDir } = fixture(t)
  const result = run('main', state)
  assert.equal(result.status, 0, result.stderr)

  const worldDir = path.join(state, 'worlds', 'main')
  for (const name of WORLD_FILES) {
    assert.equal(fs.existsSync(path.join(state, name)), false, `${name} should be gone from state/`)
    assert.equal(fs.readFileSync(path.join(worldDir, name), 'utf8'), `${name} content\n`)
  }
  assert.deepEqual(readJSON(path.join(worldDir, 'world.json')), SERVER)

  const cfg = readJSON(path.join(agentDir('Aiel'), 'config.json'))
  assert.deepEqual(cfg, { username: 'Aiel', apiPort: 0, harness: 'claude-code', world: 'main' })

  const briefing = fs.readFileSync(path.join(agentDir('Aiel'), 'BRIEFING.md'), 'utf8')
  assert.match(briefing, /\.\.\/\.\.\/worlds\/main\/WORLD\.md/)
  assert.doesNotMatch(briefing, /`\.\.\/\.\.\/WORLD\.md`/)

  assert.ok(result.stdout.trim().length > 0, 'reports what it did')
  assert.notEqual(result.stdout.trim(), 'nothing to do')
})

test('migrate-worlds: a config already naming a different world is left alone and reported', t => {
  const { state, agentDir } = fixture(t)
  fs.mkdirSync(agentDir('Egwene'), { recursive: true })
  const other = { username: 'Egwene', apiPort: 0, world: 'other' }
  fs.writeFileSync(path.join(agentDir('Egwene'), 'config.json'), JSON.stringify(other, null, 1) + '\n')

  const result = run('main', state)
  assert.equal(result.status, 0, result.stderr)
  assert.deepEqual(readJSON(path.join(agentDir('Egwene'), 'config.json')), other)
  assert.match(result.stdout, /Egwene/)
  assert.match(result.stdout, /other/)
})

test('migrate-worlds: a second run changes nothing and says so', t => {
  const { state } = fixture(t)
  const first = run('main', state)
  assert.equal(first.status, 0, first.stderr)

  const before = snapshot(state)
  const second = run('main', state)
  assert.equal(second.status, 0, second.stderr)
  assert.equal(second.stdout.trim(), 'nothing to do')
  assert.deepEqual(snapshot(state), before)
})

function snapshot (state) {
  const files = {}
  for (const file of walk(state)) files[path.relative(state, file)] = fs.readFileSync(file, 'utf8')
  return files
}
function * walk (dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name)
    if (entry.isDirectory()) yield * walk(full)
    else yield full
  }
}

test('migrate-worlds: refuses when a file exists at both the old and new place, and moves nothing', t => {
  const { state } = fixture(t)
  const worldDir = path.join(state, 'worlds', 'main')
  fs.mkdirSync(worldDir, { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'places.json'), 'already there\n')

  const before = snapshot(state)
  const result = run('main', state)
  assert.equal(result.status, 2)
  assert.match(result.stderr, /places\.json/)
  assert.deepEqual(snapshot(state), before)
})

test('migrate-worlds: refuses when an agent body is running, and changes nothing', async t => {
  const { state, agentDir } = fixture(t)
  const server = net.createServer()
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  t.after(() => new Promise(resolve => server.close(resolve)))
  const port = server.address().port

  const cfg = readJSON(path.join(agentDir('Aiel'), 'config.json'))
  fs.writeFileSync(path.join(agentDir('Aiel'), 'config.json'), JSON.stringify({ ...cfg, apiPort: port }, null, 1) + '\n')

  const before = snapshot(state)
  const result = run('main', state)
  assert.equal(result.status, 3)
  assert.match(result.stderr, /Aiel/)
  assert.deepEqual(snapshot(state), before)
})
