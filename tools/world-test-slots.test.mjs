// Why JavaScript: node --test file for tools/world-test-slots.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { slotKinds, slotArgv, bodyName, claimBody, releaseBody } from './world-test-slots.mjs'

test('slotKinds: a run holds the body slot only', () => {
  assert.deepEqual(slotKinds(['a.edn', '--tag', 'x']), ['body'])
})
test('slotKinds: --allow-time takes the time slot first, then the body slot', () => {
  assert.deepEqual(slotKinds(['a.edn', '--allow-time', '--time-log', 'f']), ['time', 'body'])
})
test('slotArgv: nests res-slot commands outermost first and ends with the runner', () => {
  assert.deepEqual(slotArgv(['--allow-time'], '/rs', 'node', 'wt.mjs'),
    ['/rs', 'time', '--', '/rs', 'body', '--', 'node', 'wt.mjs', '--allow-time'])
})
test('slotArgv: without --allow-time only the body slot wraps it', () => {
  assert.deepEqual(slotArgv(['x.edn'], '/rs', 'node', 'wt.mjs'), ['/rs', 'body', '--', 'node', 'wt.mjs', 'x.edn'])
})

test('slotKinds: a kind an outer res-slot already holds (RES_SLOT_HELD) is not taken again', () => {
  assert.deepEqual(slotKinds(['--allow-time'], 'body'), ['time'])
  assert.deepEqual(slotKinds(['a.edn'], 'body'), [])
  assert.deepEqual(slotArgv(['a.edn'], '/rs', 'node', 'wt.mjs', 'body'), ['node', 'wt.mjs', 'a.edn'])
})

test('bodyName: --body NAME, else the runner default', () => {
  assert.equal(bodyName(['a.edn', '--body', 'ProbeX']), 'ProbeX')
  assert.equal(bodyName(['a.edn']), 'ProbeFixture')
})

const lockDir = () => fs.mkdtempSync(path.join(os.tmpdir(), 'wt-body-'))
const isAlive = (pid) => pid === process.pid || pid === 4242

test('claimBody: a live holder of the same body refuses, naming its pid', () => {
  const d = lockDir()
  assert.deepEqual(claimBody(d, 'ProbeX', 4242, isAlive), { ok: true })
  const r = claimBody(d, 'ProbeX', process.pid, isAlive)
  assert.equal(r.ok, false); assert.equal(r.holder, 4242)
  assert.match(r.why, /ProbeX.*pid 4242/)
  fs.rmSync(d, { recursive: true })
})
test('claimBody: a different body does not conflict', () => {
  const d = lockDir()
  claimBody(d, 'ProbeX', 4242, isAlive)
  assert.equal(claimBody(d, 'ProbeY', process.pid, isAlive).ok, true)
  fs.rmSync(d, { recursive: true })
})
test('claimBody: a dead holder is stale and gets replaced', () => {
  const d = lockDir()
  claimBody(d, 'ProbeX', 999999, () => true)
  assert.equal(claimBody(d, 'ProbeX', process.pid, isAlive).ok, true)
  assert.equal(fs.readFileSync(path.join(d, 'world-body.ProbeX.pid'), 'utf8').trim(), String(process.pid))
  fs.rmSync(d, { recursive: true })
})
test('claimBody: a claim replaced while the holder was being checked is not removed', () => {
  const d = lockDir()
  claimBody(d, 'ProbeX', 999999, () => true)
  const racing = (pid) => {
    if (pid !== 999999) return true
    fs.writeFileSync(path.join(d, 'world-body.ProbeX.pid'), '4242') // another runner took over the stale claim
    return false
  }
  const r = claimBody(d, 'ProbeX', process.pid, racing)
  assert.equal(r.ok, false)
  assert.equal(r.holder, 4242)
  assert.equal(fs.readFileSync(path.join(d, 'world-body.ProbeX.pid'), 'utf8').trim(), '4242')
  fs.rmSync(d, { recursive: true })
})
test('releaseBody: removes only its own claim', () => {
  const d = lockDir()
  claimBody(d, 'ProbeX', 4242, isAlive)
  releaseBody(d, 'ProbeX', process.pid)
  assert.equal(claimBody(d, 'ProbeX', process.pid, isAlive).ok, false)
  releaseBody(d, 'ProbeX', 4242)
  assert.equal(claimBody(d, 'ProbeX', process.pid, isAlive).ok, true)
  fs.rmSync(d, { recursive: true })
})
