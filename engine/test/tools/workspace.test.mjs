// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
// Black-box Node/Unix-socket boundary tests for the compiled CLJS generator.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import http from 'node:http'
import { spawnSync, spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import tools from '../../tools/agent-tools-loader.mjs'
import { readEDN, keyword } from './edn.mjs'

const repo = fileURLToPath(new URL('../../../', import.meta.url))
const cli = path.join(repo, 'engine/tools/workspace.mjs')
const commands = ['observe', 'jobs', 'triggers', 'say', 'entities', 'drive', 'world', 'map', 'plans', 'blueprints', 'world-changes', 'time', 'snapshot']
const bodyTools = new Set(['observe', 'jobs', 'triggers', 'say', 'entities', 'drive', 'world', 'snapshot'])
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
  assert.match(f.generate('--body', 'Other').stdout, /bindings differ/)
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
  assert.match(f.generate('--update-tools').stdout, /unrecognized wrapper/)
  assert.equal(fs.readFileSync(path.join(f.workspace, 'bin/say'), 'utf8'), 'custom tool')
  fs.unlinkSync(path.join(f.workspace, 'bin/say'))
  const outside = path.join(f.dir, 'outside')
  fs.writeFileSync(outside, 'outside')
  fs.symlinkSync(outside, path.join(f.workspace, 'bin/say'))
  assert.match(f.generate('--update-tools').stdout, /non-file/)
  assert.equal(fs.readFileSync(outside, 'utf8'), 'outside')
})

test('all tool routing preserves arguments and binds shared tools without a body', t => {
  const f = fixture(t)
  for (const command of commands) {
    const input = ['literal with spaces', '{:note "--world is text"}', '-10']
    const expected = [...(bodyTools.has(command) ? ['B'] : []), '--world', 'w', '--worlds', f.worlds]
    if (['plans', 'blueprints'].includes(command)) expected.push('--repo', path.resolve(repo))
    if (['map', 'world-changes'].includes(command)) expected.push('--repo-root', path.resolve(repo))
    if (command === 'world-changes') expected.push('--observer', 'B')
    if (command === 'snapshot') expected.push('--workspace', f.workspace)
    if (['drive', 'world'].includes(command)) expected.push('--who', 'B')
    assert.deepEqual(tools.workspaceRoute(f.context, command, input), [...expected, ...input])
    for (const override of ['--world', '--worlds=/other', '--state', '--body=B', '--agent', '--repo', '--repo-root=/other', '--workspace=/other']) {
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
    assert.ok(result.stdout.length > 40, `${command}: ${result.stdout}`)
    const rejected = f.run(command, ['--world=other'])
    assert.equal(rejected.status, 2)
    assert.match(rejected.stdout, /cannot override/)
  }
})

test('player help shows the ./bin form, no body/world plumbing, and says what output means', t => {
  const f = fixture(t)
  const help = command => f.run(command, ['--help']).stdout
  for (const command of commands) {
    const text = help(command)
    assert.doesNotMatch(text, /omit the body|<agent>|<body>|BODY|--world <world>|--world WORLD|--worlds|--state|npm run build-agent-tools/, command)
  }
  assert.match(help('world'), /\.\/bin\/drive take --who NAME --why .* --idle-s N/)
  assert.match(help('drive'), /usage: \.\/bin\/drive <op>/)
  assert.match(help('drive'), /take --who NAME --why/)
  for (const [command, word] of [['entities', /:age-ms/], ['map', /:box/], ['world-changes', /:cursor/], ['observe', /:unknown-job/], ['time', /:time-of-day/]]) {
    assert.match(help(command), word, command)
  }
})

test('a bad request through a wrapper prints EDN on stdout with the ./bin usage, no body/world plumbing', t => {
  const f = fixture(t)
  const result = f.run('observe', ['catalog', 'jobs', 'items'])
  assert.equal(result.status, 2)
  const edn = readEDN(result.stdout)
  assert.equal(edn.ok, false)
  assert.deepEqual(edn.reason, keyword('bad-args'))
  assert.match(edn.message, /job prefix must start with jobs\./)
  assert.match(edn.usage, /\.\/bin\/observe/)
  assert.doesNotMatch(result.stdout, /<agent>|--world <world>|--worlds|--state|observe\.mjs/)
  assert.equal(result.stderr, '')
})

test('a generator error is EDN on stdout, exit 2', t => {
  const f = fixture(t)
  const result = f.generate('--body', 'Other')
  assert.equal(result.status, 2)
  const edn = readEDN(result.stdout)
  assert.equal(edn.ok, false)
  assert.match(edn.message, /bindings differ/)
  assert.equal(result.stderr, '')
})

test('the generated wrapper reports a failed launcher import as EDN on stdout, plumbing cut', t => {
  const f = fixture(t)
  const file = path.join(f.workspace, 'bin', 'observe')
  const broken = 'data:text/javascript,' + encodeURIComponent('throw new Error("boom\\nusage: observe.mjs <agent> --world <world> x")')
  const text = fs.readFileSync(file, 'utf8').replace(/import\("file:[^"]*workspace\.mjs"\)/, `import(${JSON.stringify(broken)})`)
  assert.notEqual(text, fs.readFileSync(file, 'utf8'))
  fs.writeFileSync(file, text)
  const result = f.run('observe', [])
  assert.equal(result.status, 2)
  assert.equal(result.stderr, '')
  assert.match(result.stdout, /:ok false :reason :tool-error/)
  assert.match(result.stdout, /usage: \.\/bin\/observe x/)
  assert.doesNotMatch(result.stdout, /<agent>|--world|\.mjs/)
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
      res.end(body.includes(':submit') ? '{:ok true :job {:id "j1" :status :queued}}' : '{:ok true :online true :generation-id "g1"}')
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
  const submitted = await runAsync(f.workspace, 'world', ['submit', 'wear'])
  assert.equal(submitted.status, 0, submitted.stdout + submitted.stderr)
  const submit = requests.findLast(request => request.url === '/jobs' && request.body.includes(':submit'))
  assert.equal(readEDN(submit.body).by, 'B')
  const triggers = await runAsync(f.workspace, 'triggers', ['list'])
  assert.equal(triggers.status, 0, triggers.stdout + triggers.stderr)
  assert.equal(requests.at(-1).url, '/triggers')

  const control = http.createServer((req, res) => {
    let body = ''
    req.on('data', data => { body += data })
    req.on('end', () => {
      requests.push({ url: req.url, body })
      if (req.url === '/drive') {
        res.setHeader('content-type', 'application/json')
        res.end('{"ok":true}')
      } else {
        res.setHeader('content-type', 'application/edn')
        res.end('{:ok true :world "w" :body "B" :online? true :now 1000 :ttl-ms 60000 :entities []}')
      }
    })
  })
  await new Promise((resolve, reject) => { control.once('error', reject); control.listen(path.join(engine, 'control.sock'), resolve) })
  t.after(() => new Promise(resolve => control.close(resolve)))
  for (const [command, args, endpoint] of [
    ['drive', ['state'], '/drive'],
    ['entities', ['--center', '1,2,3', '--dimension', 'overworld'], '/entities']
  ]) {
    const result = await runAsync(f.workspace, command, args)
    assert.equal(result.status, 0, command + result.stdout + result.stderr)
    assert.equal(requests.at(-1).url, endpoint)
  }
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

test('explicit adoption preserves existing body documents and runtime files', t => {
  const f = fixture(t)
  const body = path.join(f.worlds, 'w/agents/B')
  fs.mkdirSync(path.join(body, 'notes'), { recursive: true })
  const originals = { 'config.json': JSON.stringify({ username: 'B', apiPort: 42 }),
    'AGENTS.md': 'existing instructions', 'BRIEFING.md': 'mission', 'journal.md': 'history',
    'notes/handoff.md': 'handoff', 'bot.log': 'runtime log' }
  for (const [file, text] of Object.entries(originals)) fs.writeFileSync(path.join(body, file), text)
  const args = [body, '--body', 'B', '--world', 'w', '--worlds', f.worlds]
  assert.throws(() => tools.workspaceGenerate(args, repo), /not an empty directory/)
  assert.equal(tools.workspaceGenerate([...args, '--adopt-existing'], repo).ok, true)
  assert.match(fs.readFileSync(path.join(body, 'briefing.md'), 'utf8'), /BRIEFING.md/)
  assert.match(fs.readFileSync(path.join(body, 'WORKSPACE.md'), 'utf8'), /Prefer the bound/)
  assert.equal(tools.workspaceGenerate([...args, '--update-tools'], repo).ok, true)
  for (const [file, text] of Object.entries(originals)) assert.equal(fs.readFileSync(path.join(body, file), 'utf8'), text)
})

test('adoption rejects mismatches, symlinks and conflicting tools before writing', t => {
  const f = fixture(t)
  const body = path.join(f.worlds, 'w/agents/B')
  fs.mkdirSync(body, { recursive: true })
  const args = [body, '--body', 'B', '--world', 'w', '--worlds', f.worlds, '--adopt-existing']
  fs.writeFileSync(path.join(body, 'config.json'), JSON.stringify({ username: 'Other' }))
  assert.throws(() => tools.workspaceGenerate(args, repo), /username differs/)
  assert.throws(() => tools.workspaceGenerate([f.workspace, ...args.slice(1)], repo), /canonical/)
  fs.writeFileSync(path.join(body, 'config.json'), JSON.stringify({ username: 'B' }))
  fs.mkdirSync(path.join(body, 'bin'))
  fs.writeFileSync(path.join(body, 'bin/observe'), 'custom')
  assert.throws(() => tools.workspaceGenerate(args, repo), /unrecognized wrapper/)
  assert.equal(fs.existsSync(path.join(body, 'context.edn')), false)
  fs.unlinkSync(path.join(body, 'bin/observe'))
  fs.symlinkSync(path.join(f.workspace, 'AGENTS.md'), path.join(body, 'AGENTS.md'))
  assert.throws(() => tools.workspaceGenerate(args, repo), /non-file/)
  assert.equal(fs.existsSync(path.join(body, 'context.edn')), false)
  fs.unlinkSync(path.join(body, 'AGENTS.md'))
  const alias = path.join(f.dir, 'alias-worlds')
  fs.symlinkSync(f.worlds, alias)
  assert.throws(() => tools.workspaceGenerate([path.join(alias, 'w/agents/B'), '--body', 'B', '--world', 'w', '--worlds', alias, '--adopt-existing'], repo), /symlinked parents/)
  assert.equal(fs.existsSync(path.join(body, 'context.edn')), false)
})
