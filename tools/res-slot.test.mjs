// Why JavaScript: node --test file for tools/res-slot.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync, spawn } from 'node:child_process'
import { decide, loadConfig, reservedMb } from './res-slot.mjs'

const base = { freeSlots: 1, availableMb: 10000, needMb: 700, floorMb: 6144, waitedMs: 0, maxWaitMs: 540000 }

test('decide: a free slot and memory above the floor runs', () => {
  assert.equal(decide(base).action, 'run')
})
test('decide: no free slot waits, and says so', () => {
  const d = decide({ ...base, freeSlots: 0 })
  assert.equal(d.action, 'wait'); assert.match(d.why, /no free slot/)
})
test('decide: memory short of floor+need waits', () => {
  const d = decide({ ...base, availableMb: 6800 })
  assert.equal(d.action, 'wait'); assert.match(d.why, /memory/)
})
test('decide: exactly floor+need runs', () => {
  assert.equal(decide({ ...base, availableMb: 6844 }).action, 'run')
})
test('decide: waiting past the limit is busy', () => {
  assert.equal(decide({ ...base, freeSlots: 0, waitedMs: 540000 }).action, 'busy')
})
test('decide: a runnable case never reports busy', () => {
  assert.equal(decide({ ...base, waitedMs: 9e9 }).action, 'run')
})

const run = (args, env) => spawnSync('node', ['tools/res-slot.mjs', ...args], { cwd: new URL('..', import.meta.url).pathname, encoding: 'utf8', env: { ...process.env, ...env } })

test('integration: a held slot makes the next command busy (75), then it runs once released', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify({ floorMb: 0, kinds: { t: { needMb: 1, max: 1 } } }))
  const env = { RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg, RES_SLOT_MAX_WAIT_MS: '1500', RES_SLOT_POLL_MS: '200' }
  const holder = spawn('node', ['tools/res-slot.mjs', 't', '--', 'sleep', '30'], { cwd: new URL('..', import.meta.url).pathname, env: { ...process.env, ...env }, stdio: 'ignore', detached: true })
  try {
    await new Promise((r) => setTimeout(r, 1000))
    const st = run(['status'], env)
    assert.match(st.stdout, /t\.0.*sleep 30/)
    const busy = run(['t', '--', 'echo', 'ran'], env)
    assert.equal(busy.status, 75)
    assert.match(busy.stderr, /res-slot: busy \(.*no free slot.*\), retry later/)
    assert.doesNotMatch(busy.stdout, /ran/)
  } finally { process.kill(-holder.pid, 'SIGKILL') }
  await new Promise((r) => setTimeout(r, 300))
  const ok = run(['t', '--', 'sh', '-c', 'echo ran; exit 3'], env)
  assert.equal(ok.status, 3)
  assert.match(ok.stdout, /ran/)
  fs.rmSync(dir, { recursive: true, force: true })
})

test('waiting: TEST_EVENTS prints one phase line per change of reason; exit 75 says it gave up waiting', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify({ floorMb: 0, kinds: { t: { needMb: 1, max: 1 } } }))
  const env = { RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg, RES_SLOT_MAX_WAIT_MS: '1500', RES_SLOT_POLL_MS: '100', TEST_EVENTS: '1' }
  const holder = spawn('node', ['tools/res-slot.mjs', 't', '--', 'sleep', '30'], { cwd: new URL('..', import.meta.url).pathname, env: { ...process.env, ...env }, stdio: 'ignore', detached: true })
  try {
    await new Promise((r) => setTimeout(r, 1000))
    const busy = run(['t', '--', 'echo', 'ran'], env)
    const phases = busy.stdout.split('\n').filter((l) => l.startsWith('@@test ')).map((l) => JSON.parse(l.slice(7)))
    assert.equal(phases.length, 1)
    assert.equal(phases[0].event, 'phase')
    assert.match(phases[0].name, /^waiting: no free slot.*held by t\.0/)
    assert.equal(busy.status, 75)
    assert.match(busy.stderr, /gave up waiting/)
    assert.equal(run(['t', '--', 'echo', 'x'], { ...env, TEST_EVENTS: '' }).stdout.includes('@@test'), false)
  } finally { process.kill(-holder.pid, 'SIGKILL') }
  fs.rmSync(dir, { recursive: true, force: true })
})

test('config: full runs leave tests slots free for targeted runs', () => {
  const t = loadConfig().kinds.tests
  assert.ok(t.shardMax >= 1 && t.shardMax <= t.max - 2, JSON.stringify(t))
})

test('integration: each run appends kind, need, waited, ran and exit to log.jsonl', (t) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify({ floorMb: 0, kinds: { t: { needMb: 1, max: 1 } } }))
  const r = run(['t', '--need', '5', '--', 'sh', '-c', 'exit 4'], { RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg })
  assert.equal(r.status, 4)
  const entry = fs.readFileSync(path.join(dir, 'log.jsonl'), 'utf8').trim().split('\n').map((l) => JSON.parse(l)).find((e) => !e.start)
  assert.equal(entry.kind, 't'); assert.equal(entry.needMb, 5); assert.equal(entry.code, 4)
  assert.ok(entry.waitedS >= 0 && entry.ranS >= 0); assert.match(entry.cmd, /sh -c exit 4/)
})

test('integration: a start line (pid, kind, cmd) is logged while the command still runs', async (t) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify({ floorMb: 0, kinds: { t: { needMb: 1, max: 1 } } }))
  const p = spawn('node', [path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.mjs'), 't', '--', 'sleep', '3'], { env: { ...process.env, RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg } })
  t.after(() => p.kill())
  await new Promise((r) => setTimeout(r, 1500))
  const lines = fs.readFileSync(path.join(dir, 'log.jsonl'), 'utf8').trim().split('\n').map((l) => JSON.parse(l))
  assert.equal(lines.length, 1)
  assert.equal(lines[0].start, true); assert.equal(lines[0].kind, 't'); assert.match(lines[0].cmd, /sleep 3/)
  assert.ok(Number.isInteger(lines[0].pid) && lines[0].pid > 0)
  assert.doesNotThrow(() => process.kill(lines[0].pid, 0))
})

test('reservedMb: counts only grants still ramping whose owner lives', () => {
  const grants = [{ pid: 1, needMb: 700, t: 1000 }, { pid: 2, needMb: 500, t: 100 }, { pid: 3, needMb: 300, t: 1000 }]
  assert.equal(reservedMb(grants, 1500, 600, (pid) => pid !== 3), 700)
})
test('decide: reserved memory counts as used', () => {
  const d = decide({ ...base, availableMb: 7000, reservedMb: 1000 })
  assert.equal(d.action, 'wait'); assert.match(d.why, /reserved/)
})

const tmpEnv = (cfgObj, extra = {}) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify(cfgObj))
  return { dir, env: { RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg, RES_SLOT_MAX_WAIT_MS: '1500', RES_SLOT_POLL_MS: '200', ...extra } }
}

test('integration: simultaneous waiters do not all pass one MemAvailable reading', async () => {
  const { dir, env } = tmpEnv({ floorMb: 0, kinds: { t: { needMb: 600, max: 3 } } })
  const meminfo = path.join(dir, 'meminfo')
  fs.writeFileSync(meminfo, `MemAvailable: ${1000 * 1024} kB\n`)
  const e = { ...env, RES_SLOT_MEMINFO: meminfo, RES_SLOT_RAMP_MS: '60000' }
  const cwd = new URL('..', import.meta.url).pathname
  const procs = [0, 1, 2].map(() => spawn('node', ['tools/res-slot.mjs', 't', '--', 'sleep', '4'], { cwd, env: { ...process.env, ...e }, stdio: 'ignore' }))
  const codes = await Promise.all(procs.map((p) => new Promise((r) => p.on('close', r))))
  assert.deepEqual(codes.slice().sort(), [0, 75, 75])
  fs.rmSync(dir, { recursive: true, force: true })
})

test('integration: a command that exits 213 itself is not rerun as a taken slot', () => {
  const { dir, env } = tmpEnv({ floorMb: 0, kinds: { t: { needMb: 1, max: 2 } } })
  const out = path.join(dir, 'runs')
  const r = run(['t', '--', 'sh', '-c', `echo x >> ${out}; exit 213`], env)
  assert.equal(r.status, 213)
  assert.equal(fs.readFileSync(out, 'utf8'), 'x\n')
  fs.rmSync(dir, { recursive: true, force: true })
})

test('integration: racing starts for free slots all run (a lost slot race is not a command exit)', async () => {
  const { dir, env } = tmpEnv({ floorMb: 0, kinds: { t: { needMb: 1, max: 4 } } })
  const cwd = new URL('..', import.meta.url).pathname
  for (let round = 0; round < 5; round++) {
    const procs = [0, 1, 2, 3].map(() => spawn('node', ['tools/res-slot.mjs', 't', '--', 'sh', '-c', 'sleep 0.5; exit 7'], { cwd, env: { ...process.env, ...env }, stdio: 'ignore' }))
    const codes = await Promise.all(procs.map((p) => new Promise((r) => p.on('close', r))))
    assert.deepEqual(codes, [7, 7, 7, 7])
  }
  fs.rmSync(dir, { recursive: true, force: true })
})

test('integration: a command exiting 213 with its slot raced still reports 213 and runs once', () => {
  const { dir, env } = tmpEnv({ floorMb: 0, kinds: { t: { needMb: 1, max: 2 } } })
  const out = path.join(dir, 'runs')
  const r = run(['t', '--', 'sh', '-c', `echo x >> ${out}; exit 213`], env)
  assert.equal(r.status, 213)
  assert.equal(fs.readFileSync(out, 'utf8'), 'x\n')
  assert.deepEqual(fs.readdirSync(dir).filter((f) => f.includes('.run.')), [])
  fs.rmSync(dir, { recursive: true, force: true })
})

test('integration: a waiter and a busy exit name the slot holder (pid, age, command)', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'res-slot-test-'))
  const cfg = path.join(dir, 'cfg.json')
  fs.writeFileSync(cfg, JSON.stringify({ floorMb: 0, kinds: { t: { needMb: 1, max: 1 } } }))
  const env = { RES_SLOT_DIR: dir, RES_SLOT_CONFIG: cfg, RES_SLOT_MAX_WAIT_MS: '1500', RES_SLOT_POLL_MS: '200', RES_SLOT_STATUS_MS: '300' }
  const holder = spawn('node', ['tools/res-slot.mjs', 't', '--', 'sleep', '31'], { cwd: new URL('..', import.meta.url).pathname, env: { ...process.env, ...env }, stdio: 'ignore', detached: true })
  try {
    await new Promise((r) => setTimeout(r, 1000))
    const busy = run(['t', '--', 'echo', 'ran'], env)
    assert.match(busy.stderr, /waiting for t .*held by t\.0 pid \d+ \d+s sleep 31/)
    assert.match(busy.stderr, /busy \(.*held by t\.0 pid \d+ \d+s sleep 31/)
  } finally { process.kill(-holder.pid, 'SIGKILL') }
  fs.rmSync(dir, { recursive: true, force: true })
})

test('config: shadow-cljs servers have their own slot kind, with room for several worktrees', () => {
  const res = JSON.parse(fs.readFileSync(path.join(path.dirname(new URL(import.meta.url).pathname), 'res-slot.json'), 'utf8'))
  assert.equal(res.kinds.compile, undefined)
  assert.equal(res.kinds.server.needMb, 2000)
  assert.ok(res.kinds.server.max >= 4)
})
