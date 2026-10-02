// Actions in this exact allowlist neither move the body nor change its inventory, controls,
// reflex policy, work authorization, or world state. Everything else is serialized as a job.
export const CONCURRENT_READ_ACTIONS = new Set([
  'help', 'state', 'look', 'block_at', 'find_blocks', 'scan', 'entity', 'inventory', 'screen', 'look_around', 'animals', 'places', 'zones',
  'boat_state', 'cart_state', 'horse_state', 'path_to', 'events', 'chat', 'whisper', 'hear'
])

export const mayRunBesideOwner = (name, { quick, long }) => CONCURRENT_READ_ACTIONS.has(name) && typeof quick?.[name] === 'function' && typeof long?.[name] !== 'function'

// ---------------------------------------------------------------- hold policy (card: 362 holds, 269 resumes in one 25h session)
// A held queue stops every later job until a driver (or, after a reconnect, the body itself) looks at it. Most
// failures - no path, no bed, an untillable cell, a refused place or dig, a clean disconnect - are routine enough
// that stopping the whole queue for them cost more than it protected. Holding is now reserved for the few results
// that genuinely need a look before more work runs: the body died, or the same job failed twice running with nothing
// to explain it. A result's `lost` map is not a hold: composites report everything that left the inventory there,
// blocks they placed included, so it fired on routine building. restorationPending (gates, scaffold, bamboo left mid-repair)
// is its own, pre-existing reason and always holds: the physical world, not the job result, is what is unresolved.

// deathCancel's cancellation message always starts with "died" (src/composite.mjs); wrapped as "cancelled: died ..."
// whenever a task's own cancellation reason is a death. True regardless of the job's final status: a death
// cancels the task that was running, which usually finishes as 'cancelled', never 'failed'.
export const resultIsDeath = result => /^cancelled: died\b/.test(String(result?.error ?? ''))

// (c): the same job NAME failing with nothing of its own in between to clear it. A success for that name resets the
// count to 0; a different name keeps its own count untouched. The caller owns the Map across jobs (one scheduler, one map).
export function recordOutcome (streaks, name, failed) {
  if (!failed) { streaks.delete(name); return 0 }
  const count = (streaks.get(name) ?? 0) + 1
  streaks.set(name, count)
  return count
}

// Whether this terminal result should freeze the queue. `status` is what the job record was just set to
// ('completed'/'failed'/'cancelled'/'interrupted'); `streak` is this job's name's consecutive-failure count,
// already updated by recordOutcome for this very result.
export function severeFailure ({ result, status, streak, cleanupPending }) {
  if (cleanupPending) return true
  if (resultIsDeath(result)) return true
  return status === 'failed' && streak >= 2
}

// The driver-facing reason: says WHICH of the hold conditions fired, not just that one did.
export function holdReason ({ id, status, result, cleanupPending, streak }) {
  const base = `job ${id} ${status}`
  const tail = 'inspect the result and explicitly resume, replace, or discard queued jobs'
  if (cleanupPending) return `${base}; ${tail}`
  if (resultIsDeath(result)) return `${base}: the body died; ${tail}`
  return `${base} twice running; ${tail}`
}
