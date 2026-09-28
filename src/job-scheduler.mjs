// Single-owner queue controller. It serializes accepted jobs, but the executor may call nested
// body actions directly; only submissions entering this controller are queued.
export function createJobScheduler ({ shelf, execute, emit = () => {}, onTerminal = () => {}, onFailure = () => {}, isReady = () => true }) {
  let pumping = false
  let activePromise = null
  const waiters = new Map()
  let progressTimer = null
  const lastProgressEvent = new Map()
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
  const pump = () => {
    if (pumping || !isReady()) return
    const job = shelf.claim()
    if (!job) return
    pumping = true
    event('started', job, { action: job.name })
    const work = Promise.resolve().then(() => execute(job))
    activePromise = work
    work.then(result => {
      const current = shelf.get(job.id)
      const cancelled = current?.status === 'cancelling' || result?.cancelled === true
      const cleanupPending = result?.restorationPending || result?.cleanupPending || result?.cleanupFailed
      const status = cleanupPending ? 'failed' : cancelled ? 'cancelled' : result?.ok === false ? 'failed' : 'completed'
      flushProgress()
      const finished = shelf.finish(job.id, status, result, result?.error) ?? { id: job.id, name: job.name }
      lastProgressEvent.delete(job.id)
      event(status, finished, { result, ...(result?.error ? { error: result.error } : {}) })
      if (status === 'failed') {
        const held = shelf.hold(`job ${job.id} failed; inspect the result and explicitly resume, replace, or discard queued jobs`, { blockUrgent: Boolean(cleanupPending) })
        emit('jobs_held', { failed: job.id, queued: shelf.list().queued.length, reason: held.reason })
        onFailure(finished, result)
      }
      onTerminal(finished, result, status)
      settle(job.id)
    }, error => {
      flushProgress()
      const result = { ok: false, error: error?.message ?? String(error) }
      const finished = shelf.finish(job.id, 'failed', result, result.error) ?? { id: job.id, name: job.name }
      lastProgressEvent.delete(job.id)
      event('failed', finished, { error: result.error })
      const held = shelf.hold(`job ${job.id} failed; inspect the result and explicitly resume, replace, or discard queued jobs`, { blockUrgent: shelf.get(job.id)?.cancelReason != null })
      emit('jobs_held', { failed: job.id, queued: shelf.list().queued.length, reason: held.reason })
      onFailure(finished, result)
      onTerminal(finished, result, 'failed')
      settle(job.id)
    }).finally(() => {
      pumping = false
      activePromise = null
      const state = shelf.snapshot()
      // An interrupt is allowed through a hold; ordinary FIFO is not.
      if (!state.held || state.urgent.length) queueMicrotask(pump)
    })
  }
  const submit = (input, { urgent = false, verbose = false, notify = true } = {}) => {
    const job = shelf.accept(input, { urgent, verbose, notify })
    event('queued', job, { action: job.name, position: shelf.list().queued.length, urgent })
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
    event('queued', job, { action: job.name, position: shelf.list().queued.length, urgent: true })
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
    const held = existing?.blockUrgent ? existing : shelf.resume()
    return { dropped, held, blocked: Boolean(existing?.blockUrgent) }
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
  return {
    submit, interrupt, cancel, resume, discard, stop, wait, report, pump,
    get: id => shelf.get(id), list: opts => shelf.list(opts),
    get activePromise () { return activePromise },
    get pumping () { return pumping }
  }
}
