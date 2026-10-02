// One-shot move from the old flat state/ layout to state/worlds/<world>/ (team-lead runs this on the real state;
// these tests only ever touch temp dirs built to the same shape).
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from '../tools/migrate-worlds.mjs'

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

// a refusal that cannot seed world.json must land before moveWorldFiles moves anything, or a reader in between
// finds the shared files gone with no world.json to replace them
test('migrate-worlds: refuses before moving anything when no config has host/port to seed world.json', t => {
  const { state, agentDir } = fixture(t)
  const { host, port, ...rest } = readJSON(path.join(agentDir('Aiel'), 'config.json'))
  fs.writeFileSync(path.join(agentDir('Aiel'), 'config.json'), JSON.stringify(rest, null, 1) + '\n')

  const before = snapshot(state)
  const result = run('main', state)
  assert.equal(result.status, 2)
  assert.match(result.stderr, /world\.json/)
  assert.deepEqual(snapshot(state), before)
})

// one config's host/port must not become every other agent's world by default: an agent naming a different
// server is a conflict to settle by hand, not something to fold silently into the same world
test('migrate-worlds: refuses when configs disagree on host:port, and changes nothing', t => {
  const { state, agentDir } = fixture(t)
  fs.mkdirSync(agentDir('Egwene'), { recursive: true })
  fs.writeFileSync(path.join(agentDir('Egwene'), 'config.json'), JSON.stringify({ username: 'Egwene', apiPort: 0, host: '10.0.0.5', port: 25565 }, null, 1) + '\n')

  const before = snapshot(state)
  const result = run('main', state)
  assert.equal(result.status, 2)
  assert.match(result.stderr, /127\.0\.0\.1:25568/)
  assert.match(result.stderr, /10\.0\.0\.5:25565/)
  assert.deepEqual(snapshot(state), before)
})

// version belongs to the world, not to one agent's config: readConfig layers {...config, ...world.server} last,
// so a version left behind in a config would sit unused at best and diverge from the seeded one at worst
test('migrate-worlds: a config\'s version is carried into world.json and stripped from the config', t => {
  const { state, agentDir } = fixture(t)
  const cfg = readJSON(path.join(agentDir('Aiel'), 'config.json'))
  fs.writeFileSync(path.join(agentDir('Aiel'), 'config.json'), JSON.stringify({ ...cfg, version: '1.21.4' }, null, 1) + '\n')

  const result = run('main', state)
  assert.equal(result.status, 0, result.stderr)
  assert.deepEqual(readJSON(path.join(state, 'worlds', 'main', 'world.json')), { ...SERVER, version: '1.21.4' })
  assert.equal('version' in readJSON(path.join(agentDir('Aiel'), 'config.json')), false)
})

// port 0 is a legitimate port, not an absent one
test('migrate-worlds: port 0 still counts as a server to seed world.json from', t => {
  const { state, agentDir } = fixture(t)
  const cfg = readJSON(path.join(agentDir('Aiel'), 'config.json'))
  fs.writeFileSync(path.join(agentDir('Aiel'), 'config.json'), JSON.stringify({ ...cfg, port: 0 }, null, 1) + '\n')

  const result = run('main', state)
  assert.equal(result.status, 0, result.stderr)
  assert.deepEqual(readJSON(path.join(state, 'worlds', 'main', 'world.json')), { host: '127.0.0.1', port: 0 })
})

// parsed directly, never by spawning without --state: that would touch the real repo's state/.
const parseRows = [
  ['world only, no --state: defaults to the repo state/', ['main'], { world: 'main', state: path.join(REPO, 'state') }],
  ['world then --state', ['main', '--state', '/tmp/x'], { world: 'main', state: path.resolve('/tmp/x') }],
  ['--state before world', ['--state', '/tmp/x', 'main'], { world: 'main', state: path.resolve('/tmp/x') }]
]
for (const [name, argv, expected] of parseRows) {
  test(`migrate-worlds: parseArgs - ${name}`, () => assert.deepEqual(parseArgs(argv), expected))
}

test('migrate-worlds: --state placed before the world name still migrates (argument order is not significant)', t => {
  const { state } = fixture(t)
  const result = spawnSync(process.execPath, [SCRIPT, '--state', state, 'main'], { encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr)
  assert.deepEqual(readJSON(path.join(state, 'worlds', 'main', 'world.json')), SERVER)
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
  assert.match(result.stderr, /running/)
  assert.deepEqual(snapshot(state), before)
})

// body-lock.mjs exits 3 for a confirmed-running body; any other nonzero exit (a crash on an unparsable
// config.json, say) is no proof the body is down, only that this could not tell, so it must refuse too
test('migrate-worlds: refuses when a body-lock check cannot run at all, not just when it reports running', t => {
  const { state, agentDir } = fixture(t)
  fs.mkdirSync(agentDir('Broken'), { recursive: true })
  fs.writeFileSync(path.join(agentDir('Broken'), 'config.json'), 'not valid json')

  const before = snapshot(state)
  const result = run('main', state)
  assert.equal(result.status, 3)
  assert.match(result.stderr, /Broken/)
  assert.match(result.stderr, /could not check \(exit \d+\)/)
  assert.deepEqual(snapshot(state), before)
})
