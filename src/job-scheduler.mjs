import { severeFailure, holdReason, recordOutcome } from './job-policy.mjs'

// Single-owner queue controller. It serializes accepted jobs, but the executor may call nested
// body actions directly; only submissions entering this controller are queued.
export function createJobScheduler ({ shelf, execute, emit = () => {}, onTerminal = () => {}, onFailure = () => {}, isReady = () => true }) {
  let pumping = false
  let activePromise = null
  const waiters = new Map()
  let progressTimer = null
  const lastProgressEvent = new Map()
  // (c) of the hold policy: consecutive failures per job NAME, across jobs of other names (job-policy.mjs recordOutcome)
  const failureStreaks = new Map()
  const terminal = new Set(['completed', 'failed', 'cancelled', 'interrupted'])
  const event = (type, job, extra = {}) => emit(`job_${type}`, {
    id: job.id, name: job.name, verbose: Boolean(job.verbose), notify: job.notify !== false, ...extra
  })
  const flushProgress = () => {
    if (progressTimer) clearTimeout(progressTimer)
    progressTimer = null
    shelf.flush?.()
  }
  const scheduleProgressFlush = () => {
    if (progressTimer) return
    progressTimer = setTimeout(() => { progressTimer = null; shelf.flush?.() }, 500)
    progressTimer.unref?.()
  }
  const settle = id => {
    const job = shelf.get(id)
    if (!job || !terminal.has(job.status)) return
    for (const waiter of waiters.get(id) ?? []) {
      clearTimeout(waiter.timer)
      waiter.resolve(job)
    }
    waiters.delete(id)
  }
  const wait = (id, ms = 0) => {
    const job = shelf.get(id)
    if (!job || terminal.has(job.status) || !(ms > 0)) return Promise.resolve(job)
    return new Promise(resolve => {
      const list = waiters.get(Number(id)) ?? []
      const waiter = { resolve, timer: null }
      list.push(waiter); waiters.set(Number(id), list)
      const timer = setTimeout(() => {
        waiter.timer = null
        const pending = waiters.get(Number(id))
        if (pending) {
          const remaining = pending.filter(item => item !== waiter)
          if (remaining.length) waiters.set(Number(id), remaining)
          else waiters.delete(Number(id))
        }
        resolve(shelf.get(id))
      }, ms)
      waiter.timer = timer
      timer.unref?.()
    })
  }
  const report = (id, type, detail = {}) => {
    const job = shelf.get(id)
    if (!job || !['running', 'cancelling'].includes(job.status)) return false
    const progress = { ...(job.progress ?? {}) }
    const previousWaiting = job.progress?.waiting
    if (detail.progress && typeof detail.progress === 'object') Object.assign(progress, detail.progress)
    if (type === 'job_waiting') progress.waiting = detail.reason ?? 'condition'
    else {
      if (detail.waiting === false) delete progress.waiting
    }
    shelf.patchTransient?.(id, { progress })
    if (!shelf.patchTransient) shelf.patch(id, { progress })
    scheduleProgressFlush()
    const now = Date.now()
    const changedWaiting = type === 'job_waiting' ? previousWaiting !== progress.waiting : detail.waiting === false && previousWaiting != null
    const last = lastProgressEvent.get(job.id) ?? 0
    // Persist every latest value, but keep the append-only event log bounded for quiet jobs.
    // Verbose jobs opt into every detail; state transitions always remain visible in the log.
    if (job.verbose || changedWaiting || now - last >= 10000) {
      emit(type, { id: job.id, name: job.name, verbose: Boolean(job.verbose), notify: job.verbose === true, ...detail })
      lastProgressEvent.set(job.id, now)
    }
    return true
  }
  // Bumped whenever a dispatch is forcibly superseded (see `abandon`) without its own promise ever
  // settling. A dispatch only acts on the shelf/emits/calls onTerminal while it is still the current
  // generation: an orphaned promise tied to a dead connection can settle arbitrarily late, and by then
  // a different job may already be running (or the queue already resumed) - without this check its
  // stale result would re-finish a job the shelf already closed out, re-fire onTerminal for it, and
  // `shelf.hold()` could re-freeze a queue an operator already resumed.
  let generation = 0
  const pump = () => {
    if (pumping || !isReady()) return
    const job = shelf.claim()
    if (!job) return
    pumping = true
    const myGeneration = ++generation
    const current = () => myGeneration === generation
    event('started', job, { action: job.name, args: job.given ?? job.args })
    const work = Promise.resolve().then(() => execute(job))
    activePromise = work
    work.then(result => {
      if (!current()) return
      const active = shelf.get(job.id)
      const cancelled = active?.status === 'cancelling' || result?.cancelled === true
      const cleanupPending = Boolean(result?.restorationPending || result?.cleanupPending || result?.cleanupFailed)
      const status = cleanupPending ? 'failed' : cancelled ? 'cancelled' : result?.ok === false ? 'failed' : 'completed'
      flushProgress()
      const finished = shelf.finish(job.id, status, result, result?.error) ?? { id: job.id, name: job.name }
      lastProgressEvent.delete(job.id)
      event(status, finished, { result, ...(result?.error ? { error: result.error } : {}) })
      const streak = recordOutcome(failureStreaks, job.name, status === 'failed')
      if (status === 'failed') onFailure(finished, result)
      if (severeFailure({ result, status, streak, cleanupPending })) {
        const held = shelf.hold(holdReason({ id: job.id, status, result, cleanupPending, streak }), { blockUrgent: cleanupPending })
        emit('jobs_held', { failed: job.id, queued: shelf.list().queued.length, reason: held.reason })
      }
      onTerminal(finished, result, status)
      settle(job.id)
    }, error => {
      if (!current()) return
      flushProgress()
      const result = { ok: false, error: error?.message ?? String(error) }
      const finished = shelf.finish(job.id, 'failed', result, result.error) ?? { id: job.id, name: job.name }
      lastProgressEvent.delete(job.id)
      event('failed', finished, { error: result.error })
      const streak = recordOutcome(failureStreaks, job.name, true)
      onFailure(finished, result)
      if (severeFailure({ result, status: 'failed', streak, cleanupPending: false })) {
        const held = shelf.hold(holdReason({ id: job.id, status: 'failed', result, cleanupPending: false, streak }), { blockUrgent: shelf.get(job.id)?.cancelReason != null })
        emit('jobs_held', { failed: job.id, queued: shelf.list().queued.length, reason: held.reason })
      }
      onTerminal(finished, result, 'failed')
      settle(job.id)
    }).finally(() => {
      if (!current()) return
      pumping = false
      activePromise = null
      const state = shelf.snapshot()
      // An interrupt is allowed through a hold; ordinary FIFO is not.
      if (!state.held || state.urgent.length) queueMicrotask(pump)
    })
  }
  const submit = (input, { urgent = false, verbose = false, notify = true } = {}) => {
    const job = shelf.accept(input, { urgent, verbose, notify })
    event('queued', job, { action: job.name, args: job.given ?? job.args, position: shelf.list().queued.length, urgent })
    pump()
    return job
  }
  const cancel = (id, reason = 'cancelled by request') => {
    const job = shelf.get(id)
    if (!job) return { ok: false, error: `no job ${id}` }
    if (job.status === 'queued') {
      const cancelled = shelf.cancelQueued(id, reason)
      event('cancelled', cancelled, { reason }); settle(id)
      return { ok: true, job: cancelled }
    }
    if (shelf.snapshot().active === Number(id) && ['running', 'cancelling'].includes(job.status)) {
      shelf.markCancelling(id, reason)
      shelf.hold(`job ${id} cancelled; explicitly resume or discard queued jobs`)
      return { ok: true, job: shelf.get(id), cleanup: 'pending' }
    }
    return { ok: true, job }
  }
  const interrupt = (input, cancelOwner = () => {}, options = {}) => {
    const before = shelf.snapshot()
    const prior = before.active
    // Do not let an interrupt accidentally release ordinary FIFO work when the urgent job
    // completes. If there is work behind it, keep that queue held for an explicit resume.
    if (!before.held && before.queue.length) shelf.hold('urgent replacement requested; explicitly resume or discard the pending FIFO queue after it completes')
    const job = shelf.accept(input, { urgent: true, ...options })
    event('queued', job, { action: job.name, args: job.given ?? job.args, position: shelf.list().queued.length, urgent: true })
    if (prior != null) {
      shelf.markCancelling(prior, `interrupted by job ${job.id}`)
      cancelOwner(`interrupted by job ${job.id}`)
    }
    pump()
    return { job, afterCleanup: prior }
  }
  const resume = ({ recovered = false } = {}) => {
    const existing = shelf.snapshot().held
    if (existing?.blockUrgent && !recovered) {
      emit('jobs_resume_refused', { reason: existing.reason, restorationPending: true })
      return { resumed: false, blocked: true, reason: existing.reason }
    }
    let held = shelf.resume()
    const previousHold = existing?.reason
    if (existing?.blockUrgent && recovered && shelf.snapshot().urgent.length) {
      // Acknowledging physical repair unlocks the urgent replacement, but ordinary queued
      // work remains behind an explicit resume, just as it does after any interrupt.
      held = shelf.hold('restoration acknowledged; urgent replacement may run, then explicitly resume or discard the pending FIFO queue')
    }
    if (existing?.blockUrgent && recovered) emit('jobs_restoration_acknowledged', { previousHold, acknowledged: true })
    emit('jobs_resumed', { previousHold, restorationAcknowledged: Boolean(existing?.blockUrgent && recovered) }); pump()
    return { resumed: Boolean(existing), previousHold, restorationAcknowledged: Boolean(existing?.blockUrgent && recovered) }
  }
  const discard = reason => {
    const dropped = shelf.discard(reason ?? 'discarded by request')
    for (const job of dropped) { event('cancelled', job, { reason: job.cancelReason }); settle(job.id) }
    const existing = shelf.snapshot().held
    const blocked = Boolean(existing?.blockUrgent)
    // A quiet one-liner here once left a body standing in the open at nightfall: say plainly what is blocking it
    // and name the way through, and put the same line where `./mc wait` can see it, not only in the direct reply.
    if (blocked) emit('jobs_discard_refused', { reason: existing.reason, note: 'resume recovered=true once the body is safe' })
    const held = blocked ? existing : shelf.resume()
    return { dropped, held, blocked }
  }
  const stop = cancelOwner => {
    const { dropped } = discard('cleared by stop')
    const active = shelf.snapshot().active
    if (active != null) {
      shelf.markCancelling(active, 'stop')
      cancelOwner('stop')
    }
    return { active, dropped }
  }
  // Cancel/stop/discard only ever mark the shelf; none of them can make an executor promise settle.
  // A dead connection orphans that promise forever, so the owner slot needs freeing directly instead
  // of waiting on a `work.then()` that is never going to fire. The in-flight `work` promise is simply
  // left running (not awaited, not rejected): bumping `generation` supersedes its dispatch, so if it
  // ever does settle later, pump()'s handlers see they are no longer current and no-op before touching
  // the shelf, emitting a stale event, or calling onTerminal/onFailure again.
  const abandon = reason => {
    const job = shelf.interruptActive(reason)
    if (!job) return { abandoned: false }
    generation++
    lastProgressEvent.delete(job.id)
    pumping = false
    activePromise = null
    event('interrupted', job, { reason })
    emit('jobs_held', { failed: job.id, queued: shelf.list().queued.length, reason: shelf.snapshot().held?.reason })
    settle(job.id)
    queueMicrotask(pump)
    return { abandoned: true, job }
  }
  return {
    submit, interrupt, cancel, resume, discard, stop, wait, report, pump, abandon,
    get: id => shelf.get(id), list: opts => shelf.list(opts),
    get activePromise () { return activePromise },
    get pumping () { return pumping }
  }
}
