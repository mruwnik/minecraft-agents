import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createJobShelf } from '../src/job-shelf.mjs'

const tempShelf = () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'job-shelf-'))
  const file = path.join(dir, 'jobs.json')
  return { file, close: () => fs.rmSync(dir, { recursive: true, force: true }) }
}

test('accept snapshots arguments durably, assigns IDs, and claims FIFO', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const args = { x: 1, nested: { item: 'oak' } }
    const one = shelf.accept({ name: 'goto', args })
    args.nested.item = 'diamond'
    const two = shelf.accept({ name: 'farm.harvest', args: { place: 'north' } })
    assert.deepEqual(shelf.get(one.id).args, { x: 1, nested: { item: 'oak' } })
    assert.equal(shelf.claim().id, one.id)
    assert.equal(shelf.claim(), null)
    shelf.finish(one.id, 'completed', { ok: true })
    assert.equal(shelf.claim().id, two.id)
  } finally { t.close() }
})

test('urgent work runs first, then leaves a held normal queue held', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const normal = shelf.accept({ name: 'mine', args: {} })
    shelf.hold('predecessor failed')
    const urgent = shelf.accept({ name: 'say', args: { text: 'urgent' } }, { urgent: true })
    assert.equal(shelf.claim().id, urgent.id)
    shelf.finish(urgent.id, 'completed', { ok: true })
    assert.equal(shelf.claim(), null)
    assert.equal(shelf.get(normal.id).status, 'queued')
    shelf.resume()
    assert.equal(shelf.claim().id, normal.id)
  } finally { t.close() }
})

test('failure holds FIFO; queued cancellation and discard have durable terminal results', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const failing = shelf.accept({ name: 'dig', args: { x: 1 } })
    const canceled = shelf.accept({ name: 'goto', args: { x: 2 } })
    const dropped = shelf.accept({ name: 'place', args: { x: 3 } })
    shelf.claim()
    shelf.finish(failing.id, 'failed', { ok: false }, 'no path')
    assert.equal(shelf.claim(), null)
    assert.equal(shelf.get(canceled.id).status, 'queued')
    assert.equal(shelf.cancelQueued(canceled.id, 'operator canceled').status, 'cancelled')
    assert.equal(shelf.discard()[0].id, dropped.id)
    assert.deepEqual(shelf.get(failing.id).result, { ok: false })
  } finally { t.close() }
})

test('restart interrupts owner, holds accepted FIFO and never replays physical work', () => {
  const t = tempShelf()
  try {
    let shelf = createJobShelf(t.file)
    const active = shelf.accept({ name: 'mine', args: { x: 1 } })
    const pending = shelf.accept({ name: 'goto', args: { x: 2 } })
    shelf.claim()
    const urgent = shelf.accept({ name: 'dig', args: { x: 3 } }, { urgent: true })
    // Simulate process death after the durable running state, before an action result was stored.
    shelf = createJobShelf(t.file)
    assert.equal(shelf.get(active.id).status, 'interrupted')
    assert.equal(shelf.get(pending.id).status, 'queued')
    assert.equal(shelf.get(urgent.id).status, 'queued')
    assert.equal(shelf.claim(), null)
    assert.match(shelf.snapshot().held.reason, /restart/)
    assert.equal(shelf.snapshot().held.blockUrgent, true)
  } finally { t.close() }
})

test('interruptActive frees a wedged owner without a process restart, and a later settlement cannot override it', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const idle = shelf.interruptActive('disconnected: socketClosed')
    assert.equal(idle, null) // nothing to abandon when no job is active
    const stuck = shelf.accept({ name: 'farm.build', args: { place: 'farm' } })
    const pending = shelf.accept({ name: 'goto', args: {} })
    shelf.claim()
    const interrupted = shelf.interruptActive('disconnected: socketClosed')
    assert.equal(interrupted.status, 'interrupted')
    assert.equal(interrupted.error, 'disconnected: socketClosed')
    assert.equal(shelf.snapshot().active, null)
    assert.match(shelf.snapshot().held.reason, /disconnected/)
    assert.equal(shelf.snapshot().held.blockUrgent, true)
    assert.equal(shelf.get(pending.id).status, 'queued')
    // the orphaned executor promise may still settle long after abandonment; it must not resurrect the job
    assert.equal(shelf.finish(stuck.id, 'completed', { ok: true }).status, 'interrupted')
  } finally { t.close() }
})

test('interruptActive keeps an existing hold reason and only adds blockUrgent', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    shelf.accept({ name: 'farm.build', args: { place: 'farm' } })
    shelf.claim()
    shelf.hold('earlier failure needs attention')
    shelf.interruptActive('disconnected: socketClosed')
    assert.equal(shelf.snapshot().held.reason, 'earlier failure needs attention')
    assert.equal(shelf.snapshot().held.blockUrgent, true)
  } finally { t.close() }
})

test('late completion cannot overwrite cancellation and repeated cancellation is harmless', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const job = shelf.accept({ name: 'goto', args: {} })
    shelf.claim()
    shelf.markCancelling(job.id, 'interrupt')
    shelf.finish(job.id, 'cancelled', { ok: false }, 'interrupt')
    assert.equal(shelf.finish(job.id, 'completed', { ok: true }).status, 'cancelled')
    assert.equal(shelf.markCancelling(job.id, 'again'), null)
  } finally { t.close() }
})

test('idle claim does not rewrite the shelf; transient progress is visible immediately and flushes on demand', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const before = fs.readFileSync(t.file, 'utf8')
    assert.equal(shelf.claim(), null)
    assert.equal(fs.readFileSync(t.file, 'utf8'), before)
    const job = shelf.accept({ name: 'farm.harvest', args: {} })
    shelf.claim()
    const running = fs.readFileSync(t.file, 'utf8')
    shelf.patchTransient(job.id, { progress: { harvested: 2 } })
    assert.deepEqual(shelf.get(job.id).progress, { harvested: 2 })
    assert.equal(fs.readFileSync(t.file, 'utf8'), running)
    shelf.flush()
    assert.deepEqual(JSON.parse(fs.readFileSync(t.file, 'utf8')).jobs[0].progress, { harvested: 2 })
  } finally { t.close() }
})

test('verbose and notification policy persists, and a soft hold cannot clear a restoration hold', () => {
  const t = tempShelf()
  try {
    const shelf = createJobShelf(t.file)
    const job = shelf.accept({ name: 'sleep', args: {} }, { verbose: true, notify: false })
    shelf.claim()
    assert.equal(shelf.get(job.id).verbose, true)
    assert.equal(shelf.get(job.id).notify, false)
    shelf.hold('cleanup failed', { blockUrgent: true })
    shelf.hold('ordinary cancellation')
    assert.equal(shelf.snapshot().held.blockUrgent, true)
    assert.match(shelf.snapshot().held.reason, /ordinary cancellation/)
    shelf.finish(job.id, 'failed', { ok: false }, 'failed after recovery was required')
    assert.equal(shelf.snapshot().held.blockUrgent, true)
    assert.equal(shelf.resume().blockUrgent, true) // the scheduler requires recovered=true before calling this
  } finally { t.close() }
})
