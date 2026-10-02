import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createJobShelf } from '../src/job-shelf.mjs'
import { createJobScheduler } from '../src/job-scheduler.mjs'

const setup = (execute, extra = {}) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'job-scheduler-'))
  const events = []
  const terminal = []
  const shelf = createJobShelf(path.join(dir, 'jobs.json'))
  const scheduler = createJobScheduler({
    shelf, execute, emit: (type, data) => events.push({ type, ...data }),
    onTerminal: (job, result, status) => terminal.push({ id: job.id, status }),
    ...extra
  })
  return { scheduler, shelf, events, terminal, close: () => fs.rmSync(dir, { recursive: true, force: true }) }
}
const deferred = () => { let resolve, reject; const promise = new Promise((a, b) => { resolve = a; reject = b }); return { promise, resolve, reject } }
const until = async predicate => {
  const end = Date.now() + 1000
  while (!predicate()) { if (Date.now() > end) throw new Error('condition did not settle'); await new Promise(resolve => setTimeout(resolve, 2)) }
}

test('submission reserves one owner synchronously and runs FIFO only after full cleanup promise settles', async () => {
  const first = deferred(), second = deferred(), started = []
  const t = setup(job => { started.push(job.name); return job.name === 'first' ? first.promise : second.promise })
  try {
    const a = t.scheduler.submit({ name: 'first', args: { depth: 1 }, given: { depth: '1' } })
    const b = t.scheduler.submit({ name: 'second', args: {} })
    assert.equal(t.shelf.snapshot().active, a.id)
    assert.deepEqual(started, []) // execution begins in a microtask, after durable ownership is reserved
    assert.deepEqual(t.events.find(e => e.type === 'job_queued' && e.id === a.id).args, { depth: '1' })
    await until(() => started.length === 1)
    assert.deepEqual(started, ['first'])
    assert.deepEqual(t.events.find(e => e.type === 'job_started' && e.id === a.id).args, { depth: '1' })
    assert.deepEqual(t.events.find(e => e.type === 'job_queued' && e.id === b.id).args, {})
    first.resolve({ ok: true, cleanup: 'settled' })
    await until(() => started.length === 2)
    assert.equal(t.shelf.get(a.id).status, 'completed')
    second.resolve({ ok: true })
    await until(() => t.shelf.get(b.id).status === 'completed')
  } finally { t.close() }
})

test('interrupt waits for old owner cleanup and urgent work does not clear an existing FIFO hold', async () => {
  const old = deferred(), urgent = deferred(), started = [], canceled = []
  const t = setup(job => { started.push(job.name); return job.name === 'old' ? old.promise : urgent.promise })
  try {
    const prior = t.scheduler.submit({ name: 'old', args: {} })
    const pending = t.scheduler.submit({ name: 'pending', args: {} })
    await until(() => started.length === 1)
    t.shelf.hold('earlier failure')
    const replacement = t.scheduler.interrupt({ name: 'urgent', args: {} }, reason => canceled.push(reason))
    assert.equal(replacement.afterCleanup, prior.id)
    assert.deepEqual(canceled, [`interrupted by job ${replacement.job.id}`])
    assert.deepEqual(started, ['old'])
    old.resolve({ ok: false, cancelled: true }) // the executor promise includes cleanup
    await until(() => started.length === 2)
    assert.deepEqual(started, ['old', 'urgent'])
    urgent.resolve({ ok: true })
    await until(() => t.shelf.get(replacement.job.id).status === 'completed')
    await new Promise(resolve => setTimeout(resolve, 5))
    assert.equal(t.shelf.get(pending.id).status, 'queued')
    assert.equal(t.shelf.snapshot().active, null)
    t.scheduler.resume()
    await until(() => t.shelf.get(pending.id).status === 'completed')
  } finally { t.close() }
})

test('interrupt holds pending FIFO until explicitly resumed even when there was no prior failure', async () => {
  const active = deferred(), started = []
  const t = setup(job => { started.push(job.name); return job.name === 'active' ? active.promise : { ok: true } })
  try {
    const owner = t.scheduler.submit({ name: 'active', args: {} })
    const queued = t.scheduler.submit({ name: 'queued', args: {} })
    await until(() => started.length === 1)
    const urgent = t.scheduler.interrupt({ name: 'urgent', args: {} }, () => {})
    active.resolve({ ok: false, cancelled: true })
    await until(() => t.shelf.get(urgent.job.id).status === 'completed')
    assert.deepEqual(started, ['active', 'urgent'])
    assert.equal(t.shelf.get(queued.id).status, 'queued')
    assert.match(t.shelf.snapshot().held.reason, /urgent replacement/)
    t.scheduler.resume()
    await until(() => t.shelf.get(queued.id).status === 'completed')
    assert.equal(t.shelf.get(owner.id).status, 'cancelled')
  } finally { t.close() }
})

test('an ordinary (benign) failure emits a terminal record and does NOT hold FIFO: the queue moves on by itself', async () => {
  const t = setup(async job => job.name === 'bad' ? { ok: false, error: 'no path' } : { ok: true })
  try {
    const bad = t.scheduler.submit({ name: 'bad', args: {} })
    const next = t.scheduler.submit({ name: 'next', args: {} })
    await until(() => t.shelf.get(bad.id).status === 'failed')
    assert.equal(t.shelf.snapshot().held, null)
    assert.ok(t.events.some(event => event.type === 'job_failed' && event.id === bad.id))
    assert.ok(!t.events.some(event => event.type === 'jobs_held'))
    await until(() => t.shelf.get(next.id).status === 'completed')
  } finally { t.close() }
})

test('the same job name failing twice in a row holds FIFO until explicit resume; a success in between resets it', async () => {
  let behavior = 'fail'
  const t = setup(async job => job.name === 'bad' && behavior === 'fail' ? { ok: false, error: 'no path' } : { ok: true })
  try {
    const first = t.scheduler.submit({ name: 'bad', args: {} })
    await until(() => t.shelf.get(first.id).status === 'failed')
    assert.equal(t.shelf.snapshot().held, null) // one failure alone is benign
    const other = t.scheduler.submit({ name: 'other', args: {} }) // a different name does not touch bad's streak
    await until(() => t.shelf.get(other.id).status === 'completed')
    const second = t.scheduler.submit({ name: 'bad', args: {} })
    await until(() => t.shelf.get(second.id).status === 'failed')
    assert.match(t.shelf.snapshot().held.reason, new RegExp(`job ${second.id} failed twice running`))
    assert.equal(t.scheduler.pumping, false)
    behavior = 'ok'
    t.scheduler.resume()
    const third = t.scheduler.submit({ name: 'bad', args: {} })
    await until(() => t.shelf.get(third.id).status === 'completed')
  } finally { t.close() }
})

test('a result that says the body died holds FIFO on the very first failure', async () => {
  const t = setup(async () => ({ ok: false, cancelled: true, error: 'cancelled: died at 10,65,20 (slain by Zombie)' }))
  try {
    const job = t.scheduler.submit({ name: 'farm.maintain', args: {} })
    await until(() => t.shelf.get(job.id).status === 'cancelled')
    assert.match(t.shelf.snapshot().held.reason, new RegExp(`job ${job.id} cancelled: the body died`))
  } finally { t.close() }
})

test('a result that lost carried items holds FIFO even though the job otherwise completed', async () => {
  const t = setup(async () => ({ ok: true, lost: { iron_pickaxe: 1 } }))
  try {
    const job = t.scheduler.submit({ name: 'mine.get', args: {} })
    await until(() => t.shelf.get(job.id).status === 'completed')
    assert.match(t.shelf.snapshot().held.reason, new RegExp(`job ${job.id} completed: lost iron_pickaxe`))
  } finally { t.close() }
})

test('ordinary benign failures (no path, no bed, untillable, refused) never hold the queue on their own', async () => {
  for (const error of ['no path to the goal', 'no bed within 32 blocks', 'untillable: stone', 'placement refused']) {
    const t = setup(async () => ({ ok: false, error }))
    try {
      const job = t.scheduler.submit({ name: 'goto', args: {} })
      await until(() => t.shelf.get(job.id).status === 'failed')
      assert.equal(t.shelf.snapshot().held, null, error)
    } finally { t.close() }
  }
})

test('discard while a restoration is pending is refused with a clear line and its own event, naming resume recovered=true', async () => {
  const old = deferred()
  const t = setup(job => job.name === 'old' ? old.promise : { ok: true })
  try {
    t.scheduler.submit({ name: 'old', args: {} })
    await until(() => t.scheduler.pumping)
    t.scheduler.abandon('disconnected: socketClosed')
    const result = t.scheduler.discard('discarded by request')
    assert.equal(result.blocked, true)
    assert.match(result.held.reason, /disconnected/)
    const refused = t.events.find(event => event.type === 'jobs_discard_refused')
    assert.ok(refused)
    assert.match(refused.note, /resume recovered=true/)
  } finally { t.close() }
})

test('cancel queued jobs produces terminal results; stop clears FIFO and cancels active owner', async () => {
  const active = deferred(), t = setup(job => active.promise)
  try {
    const one = t.scheduler.submit({ name: 'active', args: {} })
    const two = t.scheduler.submit({ name: 'queued', args: {} })
    await until(() => t.scheduler.pumping)
    assert.equal(t.scheduler.cancel(two.id).job.status, 'cancelled')
    const three = t.scheduler.submit({ name: 'queued-too', args: {} })
    const canceled = []
    const stopped = t.scheduler.stop(reason => canceled.push(reason))
    assert.equal(stopped.active, one.id)
    assert.deepEqual(stopped.dropped.map(job => job.id), [three.id])
    assert.deepEqual(canceled, ['stop'])
    active.resolve({ ok: false, cancelled: true })
    await until(() => t.shelf.get(one.id).status === 'cancelled')
    assert.equal(t.shelf.get(three.id).status, 'cancelled')
  } finally { t.close() }
})

test('cancel of running ID marks cancelling and retains owner until cleanup settles', async () => {
  const active = deferred(), started = []
  const t = setup(job => { started.push(job.name); return active.promise })
  try {
    const running = t.scheduler.submit({ name: 'long', args: {} })
    const waiting = t.scheduler.submit({ name: 'next', args: {} })
    await until(() => started.length === 1)
    const response = t.scheduler.cancel(running.id, 'operator request')
    assert.equal(response.cleanup, 'pending')
    assert.equal(t.shelf.get(running.id).status, 'cancelling')
    assert.equal(t.shelf.snapshot().active, running.id)
    active.resolve({ ok: false, cancelled: true })
    await until(() => t.shelf.get(running.id).status === 'cancelled')
    assert.equal(t.shelf.get(waiting.id).status, 'queued')
    assert.equal(t.shelf.snapshot().held.reason.includes('cancelled'), true)
  } finally { t.close() }
})

test('wait returns latest durable state; a rejected executor surfaces as an ordinary failure, held only on the second in a row', async () => {
  const t = setup(async () => { throw new Error('cleanup failed') })
  try {
    const job = t.scheduler.submit({ name: 'repair', args: {} })
    const result = await t.scheduler.wait(job.id, 1000)
    assert.equal(result.status, 'failed')
    assert.equal(result.error, 'cleanup failed')
    assert.equal(t.shelf.snapshot().held, null) // one rejection alone is benign

    const second = t.scheduler.submit({ name: 'repair', args: {} })
    const result2 = await t.scheduler.wait(second.id, 1000)
    assert.equal(result2.status, 'failed')
    assert.match(t.shelf.snapshot().held.reason, new RegExp(`job ${second.id} failed twice running`))
  } finally { t.close() }
})

test('progress and waiting observations persist for later job inspection', async () => {
  const active = deferred(), t = setup(() => active.promise)
  try {
    const job = t.scheduler.submit({ name: 'farm.harvest', args: {} })
    await until(() => t.shelf.get(job.id).status === 'running')
    assert.equal(t.scheduler.report(job.id, 'job_waiting', { reason: 'night' }), true)
    assert.equal(t.shelf.get(job.id).progress.waiting, 'night')
    assert.equal(t.scheduler.report(job.id, 'job_progress', { progress: { harvested: 3 }, waiting: false }), true)
    assert.deepEqual(t.shelf.get(job.id).progress, { harvested: 3 })
    active.resolve({ ok: true })
    await until(() => t.shelf.get(job.id).status === 'completed')
  } finally { t.close() }
})

test('quiet progress is durable but coalesced; verbose opted-in progress emits every update', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'job-scheduler-progress-'))
  const controls = new Map(), events = []
  const shelf = createJobShelf(path.join(dir, 'jobs.json'))
  const scheduler = createJobScheduler({ shelf, execute: job => controls.get(job.name).promise, emit: (type, data) => events.push({ type, ...data }) })
  try {
    const active = deferred(); controls.set('long', active)
    const normal = scheduler.submit({ name: 'long', args: {} })
    await until(() => shelf.get(normal.id).status === 'running')
    scheduler.report(normal.id, 'job_progress', { progress: { round: 1 } })
    scheduler.report(normal.id, 'job_progress', { progress: { round: 2 } })
    assert.deepEqual(shelf.get(normal.id).progress, { round: 2 })
    assert.equal(events.filter(e => e.type === 'job_progress').length, 1)
    active.resolve({ ok: true })
    await until(() => shelf.get(normal.id).status === 'completed')
    assert.equal(events.find(e => e.type === 'job_completed')?.notify, true)
    assert.deepEqual(JSON.parse(fs.readFileSync(path.join(dir, 'jobs.json'), 'utf8')).jobs[0].progress, { round: 2 })

    const activeVerbose = deferred(); controls.set('roll', activeVerbose)
    const verbose = scheduler.submit({ name: 'roll', args: {} }, { verbose: true, notify: false })
    await until(() => shelf.get(verbose.id).status === 'running')
    scheduler.report(verbose.id, 'job_progress', { progress: { offers: 1 } })
    scheduler.report(verbose.id, 'job_progress', { progress: { offers: 2 } })
    assert.equal(events.filter(e => e.type === 'job_progress' && e.id === verbose.id).length, 2)
    assert.equal(events.filter(e => e.type === 'job_progress' && e.id === verbose.id).every(e => e.notify), true)
    activeVerbose.resolve({ ok: true })
    await until(() => shelf.get(verbose.id).status === 'completed')
    assert.equal(events.find(e => e.type === 'job_completed' && e.id === verbose.id)?.notify, false)
  } finally { fs.rmSync(dir, { recursive: true, force: true }) }
})

test('a disconnect that orphans the executor promise wedges the queue forever until abandon frees it', async () => {
  // Reproduces card: a body reconnect (socket EPIPE mid-job) leaves the executor's promise
  // permanently pending, bound to objects the dead connection will never deliver events for again.
  // cancel/stop/discard only ever mark the shelf; none of them can force that promise to settle,
  // so `state.active` stays wedged and every later job sits queued forever.
  const orphaned = deferred() // stands in for a promise tied to a dead socket: it CAN still settle, just very late
  const started = []
  const t = setup(job => { started.push(job.name); return job.name === 'farm.build' ? orphaned.promise : { ok: true } })
  try {
    const stuck = t.scheduler.submit({ name: 'farm.build', args: { place: 'farm' } })
    const next = t.scheduler.submit({ name: 'goto', args: {} })
    await until(() => started.length === 1)

    // what the driver already tried live: cancel, stop (drops the queue), discard, resume - none of it
    // can make the orphaned promise settle, so the owner slot never clears
    t.scheduler.cancel(stuck.id, 'cancelled by request')
    const stopped = t.scheduler.stop(() => {})
    t.scheduler.discard('discarded by request')
    t.scheduler.resume()
    await new Promise(resolve => setTimeout(resolve, 20))
    assert.deepEqual(stopped.dropped.map(job => job.id), [next.id]) // the queued job never gets to run either
    assert.equal(t.shelf.snapshot().active, stuck.id) // still wedged on the orphaned promise

    // the fix: the body's disconnect handler abandons the wedged owner directly
    const result = t.scheduler.abandon('disconnected: socketClosed')
    assert.equal(result.abandoned, true)
    assert.equal(t.shelf.get(stuck.id).status, 'interrupted')
    assert.equal(t.shelf.snapshot().active, null)
    assert.ok(t.events.some(event => event.type === 'job_interrupted' && event.id === stuck.id))

    // freshly submitted work is held for an explicit resume, same as any other restoration hold
    const after = t.scheduler.submit({ name: 'goto', args: {} })
    assert.equal(t.shelf.get(after.id).status, 'queued')
    t.scheduler.resume({ recovered: true })
    await until(() => t.shelf.get(after.id).status === 'completed')
    assert.deepEqual(started, ['farm.build', 'goto'])

    const eventsBeforeLateSettle = t.events.length
    // the orphaned promise finally settles, long after abandonment: it must not resurrect job 322,
    // re-emit a stale job_completed, re-fire onTerminal, or re-freeze a queue the operator already resumed
    orphaned.resolve({ ok: true, built: 'a shed' })
    await new Promise(resolve => setTimeout(resolve, 20))
    assert.equal(t.shelf.get(stuck.id).status, 'interrupted') // not overwritten back to completed
    assert.equal(t.events.length, eventsBeforeLateSettle) // no stale event emitted at all
    assert.deepEqual(t.terminal.map(entry => entry.id), [after.id]) // onTerminal never re-fired for 322
    assert.equal(t.shelf.snapshot().held, null) // the already-resumed queue is not re-frozen
  } finally { t.close() }
})

test('an orphaned executor promise that settles badly after abandonment does not re-hold an already-resumed queue', async () => {
  const pending = [deferred(), deferred()] // one per farm.build dispatch below, each tied to its own dead connection
  const dispatched = [] // the deferred each dispatch actually received, in dispatch order
  const started = []
  const t = setup(job => {
    started.push(job.name)
    if (job.name !== 'farm.build') return { ok: true }
    const next = pending.shift(); dispatched.push(next); return next.promise
  })
  try {
    const stuck = t.scheduler.submit({ name: 'farm.build', args: { place: 'farm' } })
    await until(() => started.length === 1)
    t.scheduler.abandon('disconnected: socketClosed')
    const after = t.scheduler.submit({ name: 'goto', args: {} })
    t.scheduler.resume({ recovered: true })
    await until(() => t.shelf.get(after.id).status === 'completed')

    const eventsBeforeLateSettle = t.events.length
    // the dead connection's attempt finally surfaces as an ok:false result, long after abandonment
    dispatched[0].resolve({ ok: false, error: 'no path to the goal' })
    await new Promise(resolve => setTimeout(resolve, 20))
    assert.equal(t.shelf.get(stuck.id).status, 'interrupted') // not overwritten to failed
    assert.equal(t.events.length, eventsBeforeLateSettle) // no stale job_failed/jobs_held emitted
    assert.deepEqual(t.terminal.map(entry => entry.id), [after.id]) // onFailure/onTerminal never re-fired
    assert.equal(t.shelf.snapshot().held, null) // queue stays resumed, not re-frozen by the late failure

    // a genuine rejection (not just an ok:false result) is guarded the same way
    const stuck2 = t.scheduler.submit({ name: 'farm.build', args: { place: 'farm' } })
    await until(() => started.length === 2)
    t.scheduler.abandon('disconnected: socketClosed')
    const after2 = t.scheduler.submit({ name: 'goto', args: {} })
    t.scheduler.resume({ recovered: true })
    await until(() => t.shelf.get(after2.id).status === 'completed')
    const eventsBeforeSecondLateSettle = t.events.length
    dispatched[1].reject(new Error('socket hang up'))
    await new Promise(resolve => setTimeout(resolve, 20))
    assert.equal(t.shelf.get(stuck2.id).status, 'interrupted')
    assert.equal(t.events.length, eventsBeforeSecondLateSettle)
    assert.equal(t.shelf.snapshot().held, null)
  } finally { t.close() }
})

test('interrupt does not start replacement when predecessor reports restoration pending', async () => {
  const old = deferred(), started = []
  const t = setup(job => { started.push(job.name); return job.name === 'old' ? old.promise : { ok: true } })
  try {
    const prior = t.scheduler.submit({ name: 'old', args: {} })
    const waiting = t.scheduler.submit({ name: 'waiting', args: {} })
    await until(() => started.length === 1)
    const replacement = t.scheduler.interrupt({ name: 'urgent', args: {} }, () => {})
    old.resolve({ ok: false, cancelled: true, restorationPending: 'gate remains open' })
    await until(() => t.shelf.get(prior.id).status === 'failed')
    assert.equal(t.shelf.get(replacement.job.id).status, 'queued')
    assert.deepEqual(started, ['old'])
    assert.ok(t.shelf.snapshot().held)
    assert.equal(t.scheduler.resume().blocked, true)
    const acknowledged = t.scheduler.resume({ recovered: true })
    assert.equal(acknowledged.resumed, true)
    assert.equal(acknowledged.restorationAcknowledged, true)
    await until(() => t.shelf.get(replacement.job.id).status === 'completed')
    assert.equal(t.shelf.get(waiting.id).status, 'queued')
    assert.match(t.shelf.snapshot().held.reason, /restoration acknowledged/)
    assert.ok(t.events.some(event => event.type === 'jobs_restoration_acknowledged' && event.acknowledged === true))
    t.scheduler.resume()
    await until(() => t.shelf.get(waiting.id).status === 'completed')
  } finally { t.close() }
})
