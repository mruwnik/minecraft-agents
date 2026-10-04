// tools/move-bodies.mjs moves every body folder from state/agents/<name> into its world, state/worlds/<world>/agents/<name>,
// and a login cache (auth/) to state/accounts/<name>, by rename only. All of it against temp trees.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { planMove, applyMove, reverseMove, runningBodies, bodyProcesses, rewriteWrapper, rewriteBriefing, withoutWorld, socketReport } from '../tools/move-bodies.mjs'

const SCRIPT = path.join(import.meta.dirname, '..', 'tools', 'move-bodies.mjs')
const OLD_MC = '#!/bin/bash\nMC_HOME="$(dirname "$(readlink -f "$0")")" exec node "$(dirname "$(readlink -f "$0")")/../../../tools/mc.mjs" "$@"\n'
const OLD_BRIEFING = 'people (`../../worlds/claude/WORLD.md` says who)\n1. `../../../harness/claude-code.md`\n2. `../../../AGENT_GUIDE.md`\n3. `../../worlds/claude/WORLD.md`\n'

const write = (file, text) => {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  fs.writeFileSync(file, text)
}

// bodies: { Name: { world, auth, extra } }; worlds: names with a world.json
const stateTree = (bodies, worlds = ['claude']) => {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'move-bodies-'))
  for (const w of worlds) write(path.join(state, 'worlds', w, 'world.json'), '{"host":"127.0.0.1","port":1}')
  for (const [name, { world, auth = false, briefing = true }] of Object.entries(bodies)) {
    const dir = path.join(state, 'agents', name)
    write(path.join(dir, 'config.json'), JSON.stringify(world === undefined ? { username: name } : { username: name, apiPort: 3800, world }, null, 1) + '\n')
    write(path.join(dir, 'engine', 'engine.edn'), '{}')
    write(path.join(dir, 'mc'), OLD_MC)
    if (briefing) write(path.join(dir, 'BRIEFING.md'), OLD_BRIEFING)
    if (auth) write(path.join(dir, 'auth', 'token.json'), 'secret')
  }
  return state
}

const noProcesses = { procs: [] }

test('rewriteWrapper points a wrapper five levels up, and leaves a rewritten one alone', () => {
  const once = rewriteWrapper(OLD_MC)
  assert.match(once, /"\)\/\.\.\/\.\.\/\.\.\/\.\.\/\.\.\/legacy\/tools\/mc\.mjs"/)
  assert.equal(rewriteWrapper(once), once)
})

test('rewriteBriefing links the repo five levels up and the world two', () => {
  assert.equal(rewriteBriefing(OLD_BRIEFING), 'people (`../../WORLD.md` says who)\n1. `../../../../../legacy/harness/claude-code.md`\n2. `../../../../../legacy/AGENT_GUIDE.md`\n3. `../../WORLD.md`\n')
  assert.equal(rewriteBriefing(rewriteBriefing(OLD_BRIEFING)), rewriteBriefing(OLD_BRIEFING))
})

test('withoutWorld drops the world key and keeps the rest and its layout', () => {
  assert.equal(withoutWorld('{\n "username": "A",\n "world": "claude",\n "apiPort": 1\n}\n'), '{\n "username": "A",\n "apiPort": 1\n}\n')
  assert.equal(withoutWorld('{\n  "username": "A",\n  "world": "claude"\n}\n'), '{\n  "username": "A"\n}\n')
})

test('the plan moves each body into the world its config names; a folder naming no world is skipped, not guessed', () => {
  const state = stateTree({ Ann: { world: 'claude' }, Bob: { world: 'claude', auth: true }, Nob: {} })
  const plan = planMove(state)
  assert.deepEqual(plan.moves.map(m => [m.name, m.world, m.from, m.to]), [
    ['Ann', 'claude', path.join(state, 'agents', 'Ann'), path.join(state, 'worlds', 'claude', 'agents', 'Ann')],
    ['Bob', 'claude', path.join(state, 'agents', 'Bob'), path.join(state, 'worlds', 'claude', 'agents', 'Bob')]
  ])
  assert.deepEqual(plan.moves.map(m => m.account), [null, { from: path.join(state, 'agents', 'Bob', 'auth'), to: path.join(state, 'accounts', 'Bob') }])
  assert.deepEqual(plan.skipped, [{ name: 'Nob', reason: 'config.json names no world' }])
  assert.deepEqual(plan.problems, [])
})

for (const [label, setup, pattern] of [
  ['the target folder exists', state => fs.mkdirSync(path.join(state, 'worlds', 'claude', 'agents', 'Ann'), { recursive: true }), /Ann: .*exists/],
  ['the account folder exists', state => fs.mkdirSync(path.join(state, 'accounts', 'Bob'), { recursive: true }), /Bob: .*accounts\/Bob exists/],
  ['the world has no folder', state => fs.rmSync(path.join(state, 'worlds', 'claude'), { recursive: true }), /no world folder/],
  ['a socket path would be over 107 bytes', state => {
    fs.mkdirSync(path.join(state, 'agents', 'Ann'), { recursive: true })
    fs.renameSync(path.join(state, 'agents', 'Bob'), path.join(state, 'agents', 'B'.repeat(64)))
  }, /socket path .* is over 107 bytes/]
]) {
  test(`the plan names a problem when ${label}`, () => {
    const state = stateTree({ Ann: { world: 'claude' }, Bob: { world: 'claude', auth: true } })
    setup(state)
    assert.match(planMove(state).problems.join('\n'), pattern)
  })
}

// a process is { pid, argv, cwd } (cwd null when it cannot be read); the state root being moved is /repo/state
const STATE = '/repo/state'
const proc = (pid, cwd, ...argv) => ({ pid, cwd, argv })
for (const [label, p, counts] of [
  ['an engine body from the repo engine with the default state root', proc(1, '/repo/engine', 'node', 'out/body.cjs', '--agent', 'Ann', '--world', 'claude'), true],
  ['an engine body given this state root', proc(2, '/elsewhere/engine', 'node', 'out/body.cjs', '--agent', 'Ann', '--state-dir', '/repo/state'), true],
  ['an engine body given a relative state root that resolves here', proc(3, '/repo/engine', 'node', 'out/body.cjs', '--state-dir', '../state'), true],
  ['an engine body of another checkout', proc(4, '/other/engine', 'node', 'out/body.cjs', '--agent', 'Ann'), false],
  ['an engine body with its own temp state root', proc(5, '/repo/engine', 'node', 'out/body.cjs', '--state-dir', '/tmp/s'), false],
  ['an old bot in a body folder of this state', proc(6, '/repo/state/agents/Ann', 'node', '--max-old-space-size=1536', '../../../src/bot.mjs', '.'), true],
  ['an old bot given a home in this state', proc(7, '/repo', 'node', 'src/bot.mjs', '/repo/state/worlds/claude/agents/Ann'), true],
  ['an old-bot stand-in in a temp tree', proc(8, '/tmp/am/start-body-x/state/worlds/w/agents/y', 'node', '../../../../../src/bot.mjs', '.'), false],
  ['a start-body supervisor for a folder in this state', proc(9, '/repo', '/bin/bash', '/repo/tools/start-body', 'state/agents/Ann'), true],
  ['a start-body stand-in for a temp folder', proc(10, '/x', '/bin/bash', '/w/tools/start-body', '/tmp/am/start-body-y/state/worlds/w/agents/y'), false],
  ['a body process whose cwd cannot be read', proc(11, null, 'node', 'out/body.cjs', '--agent', 'Ann'), true],
  ['a start-body with no folder argument', proc(12, '/x', '/bin/bash', 'tools/start-body'), true],
  ['the dashboard', proc(13, '/repo/dashboard', 'node', 'out/server.cjs'), false],
  ['this script', proc(14, '/repo', 'node', 'tools/move-bodies.mjs'), false]
]) {
  test(`bodyProcesses: ${label} ${counts ? 'counts' : 'does not count'} as a running body`, () => {
    assert.deepEqual(bodyProcesses([p], STATE).map(x => x.pid), counts ? [p.pid] : [])
  })
}

test('runningBodies: a live body.pid, a listening socket and a body process each refuse', async () => {
  const state = stateTree({ Ann: { world: 'claude' }, Bob: { world: 'claude' } })
  assert.deepEqual(await runningBodies(state, noProcesses), [])
  write(path.join(state, 'agents', 'Ann', 'body.pid'), String(process.pid))
  const sock = path.join(state, 'agents', 'Bob', 'engine', 'control.sock')
  const server = net.createServer(s => s.destroy())
  await new Promise(resolve => server.listen(sock, resolve))
  const found = await runningBodies(state, { procs: [proc(9, path.join(state, '..', 'engine'), 'node', 'out/body.cjs', '--agent', 'Zed', '--state-dir', state)] })
  server.close()
  assert.equal(found.length, 3)
  assert.match(found.join('\n'), /Ann: body\.pid names a live process/)
  assert.match(found.join('\n'), /Bob: engine\/control\.sock accepts a connection/)
  assert.match(found.join('\n'), /pid 9 \(cwd .*\): node out\/body\.cjs/)
})

test('runningBodies: a stale body.pid and a dead socket file do not refuse', async () => {
  const state = stateTree({ Ann: { world: 'claude' } })
  write(path.join(state, 'agents', 'Ann', 'body.pid'), '999999999')
  write(path.join(state, 'agents', 'Ann', 'engine', 'events.sock'), '')
  assert.deepEqual(await runningBodies(state, noProcesses), [])
})

test('apply moves folders and the login cache by rename, rewrites links, drops the world key, and writes a manifest', () => {
  const state = stateTree({ Ann: { world: 'claude' }, Bob: { world: 'claude', auth: true } })
  const inode = fs.statSync(path.join(state, 'agents', 'Ann')).ino
  const authInode = fs.statSync(path.join(state, 'agents', 'Bob', 'auth')).ino
  const result = applyMove(planMove(state), path.join(state, 'manifest.json'))
  const ann = path.join(state, 'worlds', 'claude', 'agents', 'Ann')
  assert.deepEqual([result.moved, result.left], [2, []])
  assert.equal(fs.statSync(ann).ino, inode)
  assert.equal(fs.statSync(path.join(state, 'accounts', 'Bob')).ino, authInode)
  assert.equal(fs.existsSync(path.join(state, 'worlds', 'claude', 'agents', 'Bob', 'auth')), false)
  assert.equal(fs.existsSync(path.join(state, 'agents')), false)
  assert.equal(JSON.parse(fs.readFileSync(path.join(ann, 'config.json'), 'utf8')).world, undefined)
  assert.match(fs.readFileSync(path.join(ann, 'mc'), 'utf8'), /\/\.\.\/\.\.\/\.\.\/\.\.\/\.\.\/legacy\/tools\/mc\.mjs/)
  assert.match(fs.readFileSync(path.join(ann, 'BRIEFING.md'), 'utf8'), /`\.\.\/\.\.\/WORLD\.md`/)
  const manifest = JSON.parse(fs.readFileSync(path.join(state, 'manifest.json'), 'utf8'))
  assert.deepEqual(manifest.moves.map(m => [m.from, m.to]), [[path.join(state, 'agents', 'Ann'), ann], [path.join(state, 'agents', 'Bob'), path.join(state, 'worlds', 'claude', 'agents', 'Bob')]])
})

test('reverse puts every folder, login cache and rewritten file back as it was', () => {
  const state = stateTree({ Ann: { world: 'claude' }, Bob: { world: 'claude', auth: true } })
  const before = ['Ann/config.json', 'Ann/mc', 'Ann/BRIEFING.md', 'Bob/config.json', 'Bob/auth/token.json'].map(f => fs.readFileSync(path.join(state, 'agents', f), 'utf8'))
  applyMove(planMove(state), path.join(state, 'manifest.json'))
  const result = reverseMove(path.join(state, 'manifest.json'))
  assert.equal(result.restored, 2)
  assert.deepEqual(['Ann/config.json', 'Ann/mc', 'Ann/BRIEFING.md', 'Bob/config.json', 'Bob/auth/token.json'].map(f => fs.readFileSync(path.join(state, 'agents', f), 'utf8')), before)
  assert.equal(fs.existsSync(path.join(state, 'accounts', 'Bob')), false)
  assert.deepEqual(fs.readdirSync(path.join(state, 'worlds', 'claude', 'agents')), [])
})

test('socketReport gives the longest socket path of the moved bodies', () => {
  const state = stateTree({ Ann: { world: 'claude' }, Kettricken: { world: 'claude' } })
  const report = socketReport(planMove(state))
  assert.equal(report.path, path.join(state, 'worlds', 'claude', 'agents', 'Kettricken', 'engine', 'control.sock'))
  assert.equal(report.bytes, Buffer.byteLength(report.path))
})

const cli = (args, state) => spawnSync(process.execPath, [SCRIPT, '--state', state, ...args], { encoding: 'utf8', env: { ...process.env, MOVE_BODIES_NO_PROC_SCAN: '1' } })

test('the CLI dry run lists every rename and changes nothing', () => {
  const state = stateTree({ Ann: { world: 'claude' }, Nob: {} })
  const r = cli([], state)
  assert.equal(r.status, 1, r.stderr)
  assert.match(r.stdout, /dry run/)
  assert.match(r.stdout, /rename .*agents\/Ann -> .*worlds\/claude\/agents\/Ann/)
  assert.match(r.stdout, /skip Nob: config\.json names no world/)
  assert.equal(fs.existsSync(path.join(state, 'agents', 'Ann')), true)
})

test('the CLI apply ends with counts, exits 1 while something is left in state/agents, 0 when all moved', () => {
  const mixed = stateTree({ Ann: { world: 'claude' }, Nob: {} })
  const r = cli(['--apply'], mixed)
  assert.equal(r.status, 1)
  assert.match(r.stdout, /moved 1, skipped 1, left 1/)
  const clean = stateTree({ Ann: { world: 'claude' } })
  const ok = cli(['--apply'], clean)
  assert.equal(ok.status, 0, ok.stdout + ok.stderr)
  assert.match(ok.stdout, /moved 1, skipped 0, left 0/)
  assert.match(ok.stdout, /manifest .*agents-move-manifest-.*\.json/)
})

test('the CLI refuses to apply while a body runs, and moves nothing', () => {
  const state = stateTree({ Ann: { world: 'claude' } })
  write(path.join(state, 'agents', 'Ann', 'body.pid'), String(process.pid))
  const r = cli(['--apply'], state)
  assert.equal(r.status, 2)
  assert.match(r.stderr, /refusing: .*Ann: body\.pid names a live process/s)
  assert.equal(fs.existsSync(path.join(state, 'agents', 'Ann')), true)
})

test('the CLI refuses to apply with a planning problem, and moves nothing', () => {
  const state = stateTree({ Ann: { world: 'claude' } })
  fs.mkdirSync(path.join(state, 'worlds', 'claude', 'agents', 'Ann'), { recursive: true })
  const r = cli(['--apply'], state)
  assert.equal(r.status, 2)
  assert.equal(fs.existsSync(path.join(state, 'agents', 'Ann', 'config.json')), true)
})
