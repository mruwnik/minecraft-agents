// NODE_OPTIONS (eg --inspect=...) is meant for the body alone. Every helper tools/start-body runs along the way
// (body-lock, start-gate, check-code, patch-deps, textures, even detach.mjs itself) is a short node process that
// would otherwise inherit it and bind the very same fixed debug port in turn - noise at best, a race with the
// body's own bind at worst. Separately, a supervisor that dies before it ever reaches `wait $body` (killed, or an
// early crash) used to leave a stale body.pid with no trace in events.jsonl. See tools/start-body.
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'

const REPO = path.join(import.meta.dirname, '..')
const REAL_TOOLS = path.join(REPO, 'tools')

// start-body resolves everything as ../../../../../X relative to the agent directory (state/worlds/<world>/agents/<Name>),
// so the fixture needs that same five-levels-deep layout. The real body-lock.mjs, detach.mjs, start-gate.mjs and check-code.mjs are symlinked in
// unchanged (check-code finds no src/library here and exits at once); patch-deps, textures and the body itself
// are tiny stand-ins that record what environment they saw instead of touching node_modules or a real server.
// every fixture made in this file; the after hook stops whatever body each one started, so a failed assertion never
// leaves a stand-in body running
const fixtures = []
const fixtureRoot = () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'start-body-'))
  fixtures.push(path.join(root, 'state', 'worlds', 'w', 'agents', 'y'))
  const toolsDir = path.join(root, 'tools')
  fs.mkdirSync(toolsDir)
  for (const name of ['start-body', 'body-lock.mjs', 'detach.mjs', 'start-gate.mjs', 'check-code.mjs']) {
    fs.symlinkSync(path.join(REAL_TOOLS, name), path.join(toolsDir, name))
  }
  const recordEnv = label => `import fs from 'node:fs'
fs.writeFileSync(new URL('./${label}.recorded.json', import.meta.url), JSON.stringify({ NODE_OPTIONS: process.env.NODE_OPTIONS ?? null }))
console.log('[${label}]')
`
  fs.writeFileSync(path.join(toolsDir, 'patch-deps.mjs'), recordEnv('patch-deps'))
  fs.writeFileSync(path.join(toolsDir, 'textures.mjs'), recordEnv('textures'))
  const srcDir = path.join(root, 'src')
  fs.mkdirSync(srcDir)
  fs.writeFileSync(path.join(srcDir, 'bot.mjs'), `import fs from 'node:fs'
fs.writeFileSync(new URL('./bot.recorded.json', import.meta.url), JSON.stringify({ NODE_OPTIONS: process.env.NODE_OPTIONS ?? null }))
console.log('[bot] up')
setInterval(() => {}, 1000)
`)
  // the world of an agent home is the folder two levels up: state/worlds/w/agents/y is in world "w"
  const worldDir = path.join(root, 'state', 'worlds', 'w')
  const agentDir = path.join(worldDir, 'agents', 'y')
  fs.mkdirSync(agentDir, { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'world.json'), JSON.stringify({ host: '127.0.0.1', port: 25568 }))
  fs.writeFileSync(path.join(agentDir, 'config.json'), JSON.stringify({ apiPort: 0 }))
  return { root, agentDir, toolsDir, srcDir, worldDir }
}

// patch-deps sleeping briefly gives a window to TERM the supervisor while it is still in that synchronous,
// pre-body phase - before this fix, nothing ran in that window would ever get reported.
const slowPatchDeps = toolsDir => {
  fs.writeFileSync(path.join(toolsDir, 'patch-deps.mjs'), `import fs from 'node:fs'
await new Promise(r => setTimeout(r, 2000))
fs.writeFileSync(new URL('./patch-deps.recorded.json', import.meta.url), JSON.stringify({ NODE_OPTIONS: process.env.NODE_OPTIONS ?? null }))
`)
}

const readJSON = file => JSON.parse(fs.readFileSync(file, 'utf8'))
const waitUntil = async (fn, ms = 5000) => {
  const start = Date.now()
  while (!fn()) {
    if (Date.now() - start > ms) throw new Error('timed out waiting')
    await new Promise(r => setTimeout(r, 20))
  }
}
const readPid = dir => Number(fs.readFileSync(path.join(dir, 'body.pid'), 'utf8').trim())
const alive = pid => { try { process.kill(pid, 0); return true } catch { return false } }
// TERM whatever body.pid names (the supervisor's trap takes the body with it; a body's end lets its supervisor exit),
// then whatever it names next, until body.pid is gone or names nothing alive
const stopBody = async dir => {
  for (let i = 0; i < 5; i++) {
    const pid = fs.existsSync(path.join(dir, 'body.pid')) ? readPid(dir) : 0
    if (!(pid > 0) || !alive(pid)) return
    process.kill(pid, 'SIGTERM')
    await waitUntil(() => !alive(pid)).catch(() => {})
  }
}
test.after(async () => {
  for (const dir of fixtures) await stopBody(dir)
})

const eventsOf = dir => fs.existsSync(path.join(dir, 'events.jsonl'))
  ? fs.readFileSync(path.join(dir, 'events.jsonl'), 'utf8').trim().split('\n').filter(Boolean).map(l => JSON.parse(l))
  : []

test('start-body: helpers run without NODE_OPTIONS; only the body gets it', async () => {
  const { root, agentDir, toolsDir, srcDir } = fixtureRoot()
  const inspect = '--inspect=127.0.0.1:0'
  spawn(path.join(toolsDir, 'start-body'), [agentDir, '--now'], {
    env: { ...process.env, NODE_OPTIONS: inspect },
    detached: true,
    stdio: 'ignore'
  }).unref()

  await waitUntil(() => fs.existsSync(path.join(srcDir, 'bot.recorded.json')))

  assert.equal(readJSON(path.join(toolsDir, 'patch-deps.recorded.json')).NODE_OPTIONS, null)
  assert.equal(readJSON(path.join(toolsDir, 'textures.recorded.json')).NODE_OPTIONS, null)
  assert.equal(readJSON(path.join(srcDir, 'bot.recorded.json')).NODE_OPTIONS, inspect)

  let pid
  await waitUntil(() => { pid = readPid(agentDir); return alive(pid) })
  process.kill(pid, 'SIGTERM')
  await waitUntil(() => !alive(pid))
  fs.rmSync(root, { recursive: true, force: true })
})

test('start-body: a supervisor killed before the body starts reports body_down instead of a stale body.pid', async () => {
  const { root, agentDir, toolsDir } = fixtureRoot()
  slowPatchDeps(toolsDir)

  const outer = spawn(path.join(toolsDir, 'start-body'), [agentDir, '--now'], {
    env: { ...process.env },
    detached: true,
    stdio: 'ignore'
  })
  outer.unref()

  // body.pid is written twice before the supervisor does anything: once by the outer process (its own pid), then
  // again once detach.mjs hands back the supervisor's pid. Wait for the second write, not just the first.
  await waitUntil(() => fs.existsSync(path.join(agentDir, 'body.pid')) && readPid(agentDir) !== outer.pid)
  const supervisorPid = readPid(agentDir)
  process.kill(supervisorPid, 'SIGTERM')

  await waitUntil(() => !fs.existsSync(path.join(agentDir, 'body.pid')))
  // a process just exited can sit as a zombie for a moment before its new parent reaps it; give that a beat
  // rather than asserting liveness the instant body.pid disappears.
  await waitUntil(() => !alive(supervisorPid))

  const events = eventsOf(agentDir)
  const down = events.find(e => e.type === 'body_down')
  assert.ok(down, `expected a body_down event, got: ${JSON.stringify(events)}`)
  assert.equal(down.exit, 143)

  fs.rmSync(root, { recursive: true, force: true })
})

// restart-body is run from the repo root as `tools/restart-body state/worlds/<world>/agents/<name>`, and hands that relative path
// on: the supervisor, started from inside the agent directory, must still find it
test('start-body: a relative agent dir and script path bring the body up', async () => {
  const { root, agentDir, srcDir } = fixtureRoot()
  spawn('tools/start-body', [path.relative(root, agentDir), '--now'], { cwd: root, detached: true, stdio: 'ignore' }).unref()

  await waitUntil(() => fs.existsSync(path.join(srcDir, 'bot.recorded.json')))

  const pid = readPid(agentDir)
  assert.ok(alive(pid))
  process.kill(pid)
})

// a fixture body must die with the test, a failed assertion included: the after hook stops what every fixture started
test('start-body: stopBody ends a fixture body and its supervisor', async () => {
  const { agentDir, toolsDir } = fixtureRoot()
  spawn(path.join(toolsDir, 'start-body'), [agentDir, '--now'], { detached: true, stdio: 'ignore' }).unref()
  const isBody = pid => (fs.readFileSync(`/proc/${pid}/cmdline`, { encoding: 'utf8', flag: 'r' }).includes('src/bot.mjs'))
  await waitUntil(() => fs.existsSync(path.join(agentDir, 'body.pid')) && alive(readPid(agentDir)) && isBody(readPid(agentDir)))
  const body = readPid(agentDir)
  await stopBody(agentDir)
  await waitUntil(() => !alive(body))
  assert.equal(alive(body), false)
})

const runSync = (toolsDir, args) => new Promise(resolve => {
  const child = spawn(path.join(toolsDir, 'start-body'), args, { stdio: 'ignore' })
  child.on('close', code => resolve(code))
})

// readWorld's refusal (no world named, or a world nobody made) is start-gate's first check, before the clock: a
// body never starts into a world that is not there, and the body_down it leaves carries readWorld's own message
for (const [name, removed, messagePattern] of [
  ['a world folder without world.json', f => path.join(f.worldDir, 'world.json'), /is in world "w", but there is no/],
  ['a body folder without config.json', f => path.join(f.agentDir, 'config.json'), /no .*config\.json/]
]) {
  test(`start-body: ${name} leaves body_down (exit 5), never starts`, async () => {
    const fixture = fixtureRoot()
    const { root, agentDir, toolsDir, srcDir } = fixture
    fs.rmSync(removed(fixture))

    const code = await runSync(toolsDir, [agentDir])
    assert.equal(code, 5)

    const events = eventsOf(agentDir)
    const down = events.find(e => e.type === 'body_down')
    assert.ok(down, `expected a body_down event, got: ${JSON.stringify(events)}`)
    assert.equal(down.exit, 5)
    assert.match(down.advice, messagePattern)
    assert.equal(fs.existsSync(path.join(srcDir, 'bot.recorded.json')), false)

    fs.rmSync(root, { recursive: true, force: true })
  })
}
