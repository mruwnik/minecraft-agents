// Why JavaScript: node --test file for tools/res-slot.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync, spawn } from 'node:child_process'
import { decide } from './res-slot.mjs'

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
