// What a task carries out with it when it does not finish (card 8946c03a).
//
// A composite builds its report as it goes (api.report: built=, stage=, dug=), and until now the report reached the
// driver only when the composite finished or handed the body back by itself. `./mc stop` answered "FAIL blueprint.build
// 84s error: cancelled: stop" and nothing else, with 25 floor cells dug and the driver reading events.jsonl to learn
// it. The pure rules live here; the runner in src/body/runner.mjs applies them.

// the report the composite built so far rides on whatever error ends it (a cancel, a death, a step that threw).
// What the error already carries wins: a composite mid-restoration says so in its own words
// Nested composites must preserve a safety hand-back instead of treating a
// partial child result as a completed step and continuing to change the world.
export class CompositeHandBack extends Error {
  constructor (reason) { super(reason); this.reason = reason }
}

// A composite may skip one failed waypoint only when the underlying goto produced an ordinary path failure.
// Keep this classifier on the raw error, before explainFailure adds contextual advice such as a protected zone
// or dead end. The standard timeout's leading sentence is also accepted on its own; later advice cannot change it.
export function recoverableGotoFailure (error) {
  if (!error || error instanceof CompositeHandBack || error instanceof TypeError || error instanceof SyntaxError || error instanceof ReferenceError || error.reason) return false
  const message = String(error.message ?? error).trim()
  return /^(?:no path to the goal!?|no path to the goal: the search found nothing to walk from here|took to long to decide path to goal!?)$/i.test(message) ||
    /^no walkable path: no net progress toward this goal for \d+s \(nearest \d+ blocks\); try another checked waypoint$/.test(message) ||
    /^the search ran out of time \((?:up to )?\d+ s\) before it found a way, which is not the same as there being none\./i.test(message)
}

export function navigationTargetKey (target) {
  if (!target || !['x', 'y', 'z'].every(k => Number.isFinite(target[k]))) return null
  return ['x', 'y', 'z'].map(k => Object.is(target[k], -0) ? '0' : String(target[k])).join(',')
}

export const recoverableNavigationTarget = (error, target) =>
  recoverableGotoFailure(error) ? navigationTargetKey(target) : null

export const carryReport = (error, report = {}, notes = []) =>
  Object.assign(error, { report: { ...report, ...(notes.length ? { notes: notes.join('; ') } : {}), ...error.report } })

// the result of a composite that ran to its end or was handed back (src/body/runner.mjs runComposite). A composite that
// reports before every checkpoint, so a hand-back still says what it did (farm.maintain), fixed the report's key order
// at its first report: a finished one's return value orders the line instead (the routine keeps 120 characters of it),
// and wins over what it reported; then what only the report has, then stopped= and the notes
export const compositeResult = (report, { stopped, ...said }, notes = []) =>
  ({ ...said, ...report, ...said, stopped, notes: notes.length ? notes.join('; ') : undefined })

// the FAIL result of a task that did not finish: the runner's own fields, then the report it carried, then why.
// A cancel is said in the canceller's words (stop, superseded by, died at) unless the composite is still restoring
// something, when its own words stand. The report never overrides ok= or error=
export const failedResult = ({ base = {}, report = {}, cancelled = false, why, message, explain = m => m }) => ({
  ...base,
  ...report,
  ok: false,
  error: cancelled && !report.restorationPending ? `cancelled: ${why}` : explain(message)
})

const at = ({ x, y, z }) => `${Math.floor(x)},${Math.floor(y)},${Math.floor(z)}`

// what a task's result says of a death that happened while it ran: the kit lies where the body fell, for five
// minutes (collect there first). Nothing for a death before the task started, or with nothing worth naming carried
export const deathLine = ({ diedAt, startedAt, pos, kit }) =>
  diedAt && startedAt && diedAt >= startedAt && pos && kit ? `${kit} lies at ${at(pos)}` : null

// why a death cancels the running task: the cell, and the cause when one is known
export const deathCancel = ({ pos, cause }) => `died${pos ? ` at ${at(pos)}` : ''}${cause ? ` (${cause})` : ''}`
