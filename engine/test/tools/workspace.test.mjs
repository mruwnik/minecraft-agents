// Black-box Node/Unix-socket boundary tests for the compiled CLJS generator.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import http from 'node:http'
import { spawnSync, spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import tools from '../../tools/agent-tools-loader.mjs'
import { readEDN } from '../../tools/observe-lib.mjs'

const repo = fileURLToPath(new URL('../../../', import.meta.url))
const cli = path.join(repo, 'engine/tools/workspace.mjs')
const commands = ['observe', 'jobs', 'triggers', 'say', 'entities', 'drive', 'world', 'map', 'plans', 'blueprints', 'world-changes', 'time']
function fixture (t) {
  const dir = fs.mkdtempSync('/tmp/ws-')
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const workspace = path.join(dir, "agent's workspace")
  const worlds = path.join(dir, 'worlds')
  const args = [workspace, '--body', 'B', '--world', 'w', '--worlds', worlds]
  const generate = (...extra) => spawnSync(process.execPath, [cli, ...args, ...extra], { encoding: 'utf8', cwd: '/tmp' })
  const result = generate()
  assert.ifError(result.error)
  assert.equal(result.status, 0, result.stderr)
  const run = (command, args = []) => spawnSync(path.join(workspace, 'bin', command), args, { cwd: '/', encoding: 'utf8' })
  return { dir, workspace, worlds, generate, run, context: path.join(workspace, 'context.edn') }
}
function runAsync (workspace, command, args = []) {
  return new Promise((resolve, reject) => {
    const child = spawn(path.join(workspace, 'bin', command), args, { cwd: '/' })
    let stdout = '', stderr = ''
    child.stdout.on('data', data => { stdout += data })
    child.stderr.on('data', data => { stderr += data })
    child.on('error', reject)
    child.on('close', status => resolve({ status, stdout, stderr }))
  })
}

test('generator writes EDN bindings and executable tools; reruns preserve agent documents', t => {
  const f = fixture(t)
  const context = readEDN(fs.readFileSync(f.context, 'utf8'))
  assert.equal(context.body, 'B')
  assert.equal(context.world, 'w')
  assert.equal(context.worlds, f.worlds)
  assert.equal(context.repo, path.resolve(repo))
  for (const command of commands) assert.ok(fs.statSync(path.join(f.workspace, 'bin', command)).mode & 0o111)
  for (const file of ['AGENTS.md', 'briefing.md', 'notes/handoff.md']) fs.writeFileSync(path.join(f.workspace, file), `custom ${file}`)
  assert.equal(f.generate().status, 0)
  fs.appendFileSync(path.join(f.workspace, 'bin/observe'), '// old generated content\n')
  assert.equal(f.generate().status, 2)
  assert.equal(f.generate('--update-tools').status, 0)
  for (const file of ['AGENTS.md', 'briefing.md', 'notes/handoff.md']) assert.equal(fs.readFileSync(path.join(f.workspace, file), 'utf8'), `custom ${file}`)
  assert.match(f.generate('--body', 'Other').stderr, /bindings differ/)
})

test('generator refuses unrelated destinations, unknown wrappers and symlinks without partial updates', t => {
  const f = fixture(t)
  const unrelated = path.join(f.dir, 'unrelated')
  fs.mkdirSync(unrelated)
  fs.writeFileSync(path.join(unrelated, 'keep'), 'keep')
  const bad = spawnSync(process.execPath, [cli, unrelated, '--body', 'B', '--world', 'w'], { encoding: 'utf8' })
  assert.equal(bad.status, 2)
  assert.deepEqual(fs.readdirSync(unrelated), ['keep'])
  fs.writeFileSync(path.join(f.workspace, 'bin/say'), 'custom tool')
  assert.match(f.generate('--update-tools').stderr, /unrecognized wrapper/)
  assert.equal(fs.readFileSync(path.join(f.workspace, 'bin/say'), 'utf8'), 'custom tool')
  fs.unlinkSync(path.join(f.workspace, 'bin/say'))
  const outside = path.join(f.dir, 'outside')
  fs.writeFileSync(outside, 'outside')
  fs.symlinkSync(outside, path.join(f.workspace, 'bin/say'))
  assert.match(f.generate('--update-tools').stderr, /non-file/)
  assert.equal(fs.readFileSync(outside, 'utf8'), 'outside')
})

test('all tool routing preserves arguments and binds shared tools without a body', t => {
  const f = fixture(t)
  for (const [index, command] of commands.entries()) {
    const input = ['literal with spaces', '{:note "--world is text"}', '-10']
    const expected = [...(index < 7 ? ['B'] : []), '--world', 'w', '--worlds', f.worlds]
    if (['plans', 'blueprints'].includes(command)) expected.push('--repo', path.resolve(repo))
    if (['map', 'world-changes'].includes(command)) expected.push('--repo-root', path.resolve(repo))
    assert.deepEqual(tools.workspaceRoute(f.context, command, input), [...expected, ...input])
    for (const override of ['--world', '--worlds=/other', '--state', '--body=B', '--agent', '--repo', '--repo-root=/other']) {
      assert.throws(() => tools.workspaceRoute(f.context, command, [override]), /cannot override/)
    }
  }
  assert.deepEqual(tools.workspaceRoute(f.context, 'say', ['--', '--world']), ['B', '--world', 'w', '--worlds', f.worlds, '--', '--world'])
  assert.throws(() => tools.workspaceRoute(f.context, 'unknown', []), /unknown/)
})

test('actual wrappers work from another cwd and give useful help for every tool', t => {
  const f = fixture(t)
  for (const command of commands) {
    const result = f.run(command, ['--help'])
    assert.equal(result.status, 0, `${command}: ${result.stderr}`)
    assert.match(result.stdout, new RegExp(`Workspace ${command}:`))
    assert.doesNotMatch(result.stdout, /undefined/)
    assert.ok(result.stdout.length > 150)
    const rejected = f.run(command, ['--world=other'])
    assert.equal(rejected.status, 2)
    assert.match(rejected.stderr, /cannot override/)
  }
})

test('extensionless wrappers work beneath both CommonJS and ESM package scopes', t => {
  const f = fixture(t)
  for (const type of ['commonjs', 'module']) {
    fs.writeFileSync(path.join(f.dir, 'package.json'), JSON.stringify({ type }))
    const result = f.run('observe', ['--help'])
    assert.equal(result.status, 0, `${type}: ${result.stderr}`)
    assert.match(result.stdout, /Workspace observe:/)
  }
})

test('body wrappers use the bound Unix sockets; quoting reaches chat unchanged', async t => {
  const f = fixture(t)
  const engine = path.join(f.worlds, 'w/agents/B/engine')
  fs.mkdirSync(engine, { recursive: true })
  fs.writeFileSync(path.join(f.worlds, 'w/world.json'), '{}')
  const requests = []
  const server = http.createServer((req, res) => {
    let body = ''
    req.on('data', data => { body += data })
    req.on('end', () => {
      requests.push({ url: req.url, body })
      res.setHeader('content-type', 'application/edn')
      res.end('{:ok true :online true}')
    })
  })
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(path.join(engine, 'events.sock'), resolve) })
  t.after(() => new Promise(resolve => server.close(resolve)))
  const message = 'spaces "quotes" apostrophe \' $() `backticks` and --world text'
  const said = await runAsync(f.workspace, 'say', [message])
  assert.equal(said.status, 0, said.stdout + said.stderr)
  assert.equal(readEDN(requests.at(-1).body).message, message)
  const observed = await runAsync(f.workspace, 'observe', ['status', '--raw'])
  assert.equal(observed.status, 0, observed.stderr)
  assert.equal(requests.at(-1).url, '/snapshot')
  const listed = await runAsync(f.workspace, 'jobs', ['list'])
  assert.equal(listed.status, 0, listed.stderr)
  assert.match(requests.at(-1).url, /^\/jobs/)
})

test('shared wrappers read their world independently of the caller cwd', t => {
  const f = fixture(t)
  const view = path.join(f.worlds, 'w/agents/Observer/view')
  fs.mkdirSync(view, { recursive: true })
  fs.writeFileSync(path.join(f.worlds, 'w/world.json'), '{}')
  fs.writeFileSync(path.join(view, 'pose.json'), JSON.stringify({ world: 'w', status: 'online', dimension: 'overworld', t: Date.now(), timeOfDay: 6000 }))
  const clock = f.run('time', ['clock'])
  assert.equal(clock.status, 0, clock.stdout + clock.stderr)
  assert.equal(readEDN(clock.stdout).by, 'Observer')
  for (const command of ['map', 'plans', 'blueprints']) {
    const result = f.run(command, [command === 'map' ? 'find' : 'list'])
    assert.equal(result.status, 0, command + result.stdout + result.stderr)
  }
  const changes = f.run('world-changes')
  assert.equal(changes.status, 0, changes.stdout + changes.stderr)
})

test('legacy state selects its worlds child and ambiguous selectors fail', t => {
  const f = fixture(t)
  const legacyWorkspace = path.join(f.dir, 'legacy')
  const result = tools.workspaceGenerate([legacyWorkspace, '--body', 'B', '--world', 'w', '--state', f.dir], repo)
  assert.equal(result.ok, true)
  assert.equal(readEDN(fs.readFileSync(path.join(legacyWorkspace, 'context.edn'), 'utf8')).worlds, path.join(f.dir, 'worlds'))
  assert.throws(() => tools.workspaceGenerate([path.join(f.dir, 'ambiguous'), '--body', 'B', '--world', 'w', '--state', f.dir, '--worlds', f.worlds], repo), /choose/)
})

test('generated AOT tools stay within the approximately 500 ms startup budget', t => {
  const f = fixture(t)
  for (const command of ['observe', 'map', 'time']) {
    const samples = []
    for (let i = 0; i < 5; i++) {
      const start = performance.now()
      const result = f.run(command, ['--help'])
      assert.ifError(result.error)
      assert.equal(result.status, 0, result.stderr)
      samples.push(performance.now() - start)
    }
    samples.sort((a, b) => a - b)
    t.diagnostic(`${command}: median ${samples[2].toFixed(1)} ms, range ${samples[0].toFixed(1)}–${samples[4].toFixed(1)} ms`)
    assert.ok(samples[2] < 500, `${command} median startup ${samples[2]} ms exceeds budget`)
  }
})
