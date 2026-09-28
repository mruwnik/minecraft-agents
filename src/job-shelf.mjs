// Durable, per-body record of accepted work. This module deliberately does not run actions:
// the body scheduler owns execution and calls claim()/finish() around its single owner slot.
import fs from 'node:fs'
import path from 'node:path'

const clone = value => JSON.parse(JSON.stringify(value))

export function createJobShelf (file, { maxJobs = 500 } = {}) {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  let state = { version: 1, nextId: 1, jobs: [], active: null, queue: [], urgent: [], held: null }
  if (fs.existsSync(file)) {
    let saved
    try { saved = JSON.parse(fs.readFileSync(file, 'utf8')) }
    catch (error) { throw new Error(`job shelf is unreadable at ${file}; refusing to replace accepted work: ${error.message}`) }
    if (saved?.version !== 1 || !Array.isArray(saved.jobs) || !Array.isArray(saved.queue) || !Array.isArray(saved.urgent)) {
      throw new Error(`job shelf has an unsupported or incomplete format at ${file}; refusing to replace accepted work`)
    }
    state = saved
    state.nextId = Math.max(Number(state.nextId) || 1, ...state.jobs.map(job => (Number(job.id) || 0) + 1))
  }

  // A process restart must never replay physical work. Preserve the accepted requests for inspection,
  // mark the interrupted owner terminal, and hold any later requests for an explicit resume.
  const owner = state.jobs.find(job => job.id === state.active)
  const interruptedOwner = Boolean(owner && ['running', 'cancelling'].includes(owner.status))
  const pendingOnRestart = state.queue.length > 0 || state.urgent.length > 0
  if (interruptedOwner) {
    owner.status = 'interrupted'
    owner.finishedAt = Date.now()
    owner.error = 'body restarted while this job owned the controls; physical actions were not replayed'
    state.active = null
    state.held ??= { reason: `job ${owner.id} was interrupted by body restart`, at: Date.now() }
  }
  if (pendingOnRestart) state.held ??= { reason: 'body restarted with accepted jobs pending; explicit resume required', at: Date.now() }
  // On restart even urgent requests require an explicit recovery acknowledgment. Their
  // arguments may describe physical work that started or changed before the process died.
  if (interruptedOwner || pendingOnRestart) {
    state.held = { ...state.held, blockUrgent: true }
  }

  const persist = () => {
    if (!dirty) return
    const tmp = `${file}.${process.pid}.${Date.now()}.tmp`
    fs.writeFileSync(tmp, JSON.stringify(state))
    fs.renameSync(tmp, file)
    dirty = false
  }
  let dirty = false
  const find = id => state.jobs.find(job => job.id === Number(id))
  const trim = () => {
    if (state.jobs.length <= maxJobs) return
    const protectedIds = new Set([state.active, ...state.queue, ...state.urgent].filter(id => id != null))
    const removable = state.jobs.filter(job => !protectedIds.has(job.id) && ['completed', 'failed', 'cancelled', 'interrupted'].includes(job.status))
    while (state.jobs.length > maxJobs && removable.length) {
      const job = removable.shift()
      state.jobs.splice(state.jobs.indexOf(job), 1)
    }
  }
  dirty = true
  persist()

  return {
    accept ({ name, args = {}, given = args }, { urgent = false, verbose = false, notify = true } = {}) {
      // Snapshot before returning to the caller. Reject cyclic/proxy/non-JSON values instead of
      // accepting a job whose arguments can mutate after its id has been acknowledged.
      const record = {
        id: state.nextId++, name: String(name), args: clone(args), given: clone(given),
        status: 'queued', acceptedAt: Date.now(), urgent: Boolean(urgent), verbose: Boolean(verbose), notify: notify !== false
      }
      state.jobs.push(record)
      ;(urgent ? state.urgent : state.queue).push(record.id)
      dirty = true; trim(); persist()
      return clone(record)
    },
    get (id) { const job = find(id); return job ? clone(job) : null },
    list ({ after = 0, limit = 100 } = {}) {
      return { active: state.active, held: state.held && clone(state.held), queued: [...state.urgent, ...state.queue],
        jobs: state.jobs.filter(job => job.id > Number(after)).slice(-Math.max(1, Math.min(500, Number(limit) || 100))).map(clone) }
    },
    claim () {
      if (state.active != null) return null
      let id = state.held?.blockUrgent ? null : state.urgent.shift()
      if (id == null && !state.held) id = state.queue.shift()
      if (id == null) return null
      const job = find(id)
      if (!job || job.status !== 'queued') { dirty = true; persist(); return null }
      state.active = id
      job.status = 'running'; job.startedAt = Date.now()
      dirty = true
      persist()
      return clone(job)
    },
    patch (id, values) {
      const job = find(id)
      if (!job) return null
      Object.assign(job, clone(values), { updatedAt: Date.now() })
      dirty = true
      persist(); return clone(job)
    },
    patchTransient (id, values) {
      const job = find(id)
      if (!job) return null
      Object.assign(job, clone(values), { updatedAt: Date.now() })
      dirty = true
      return clone(job)
    },
    flush () { persist() },
    finish (id, status, result, error) {
      const job = find(id)
      if (!job || !['completed', 'failed', 'cancelled', 'interrupted'].includes(status)) return null
      // Late completions from a superseded generation may not overwrite an explicit cancellation.
      if (['cancelled', 'interrupted'].includes(job.status)) return clone(job)
      job.status = status; job.finishedAt = Date.now()
      if (result !== undefined) job.result = clone(result)
      if (error !== undefined) job.error = String(error)
      if (state.active === job.id) state.active = null
      if (status === 'failed' || status === 'interrupted') state.held = {
        reason: `job ${job.id} ${status}`, at: Date.now(), ...(state.held?.blockUrgent ? { blockUrgent: true } : {})
      }
      dirty = true; trim(); persist(); return clone(job)
    },
    markCancelling (id, reason) {
      const job = find(id)
      if (!job || state.active !== job.id || !['running', 'cancelling'].includes(job.status)) return null
      job.status = 'cancelling'; job.cancelReason = String(reason); job.updatedAt = Date.now()
      dirty = true; persist(); return clone(job)
    },
    cancelQueued (id, reason = 'cancelled by request') {
      const job = find(id)
      if (!job || job.status !== 'queued') return null
      state.queue = state.queue.filter(value => value !== job.id)
      state.urgent = state.urgent.filter(value => value !== job.id)
      job.status = 'cancelled'; job.finishedAt = Date.now(); job.cancelReason = String(reason)
      dirty = true; trim(); persist(); return clone(job)
    },
    hold (reason, { blockUrgent = false } = {}) {
      // Only explicit recovered=true resume may clear a restoration hold. Ordinary cancellation,
      // failures, and interrupt policy must not downgrade it while changing its reason.
      blockUrgent = Boolean(blockUrgent || state.held?.blockUrgent)
      state.held = { reason: String(reason), blockUrgent, at: Date.now() }; dirty = true; persist(); return clone(state.held)
    },
    resume () { const was = state.held; if (was) { state.held = null; dirty = true }; persist(); return was ? clone(was) : null },
    discard (reason = 'discarded by request') {
      const ids = [...state.urgent, ...state.queue]
      state.urgent = []; state.queue = []
      for (const id of ids) {
        const job = find(id)
        if (job?.status === 'queued') { job.status = 'cancelled'; job.finishedAt = Date.now(); job.cancelReason = String(reason) }
      }
      if (ids.length) dirty = true
      trim(); persist(); return ids.map(id => find(id)).filter(Boolean).map(clone)
    },
    stopQueue (reason = 'queue cleared by stop') { return this.discard(reason) },
    snapshot () { return clone(state) }
  }
}
