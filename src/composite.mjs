// What a task carries out with it when it does not finish (card 8946c03a).
//
// A composite builds its report as it goes (api.report: built=, stage=, dug=), and until now the report reached the
// driver only when the composite finished or handed the body back by itself. `./mc stop` answered "FAIL blueprint.build
// 84s error: cancelled: stop" and nothing else, with 25 floor cells dug and the driver reading events.jsonl to learn
// it. The pure rules live here; the runner in bot.mjs applies them.

// the report the composite built so far rides on whatever error ends it (a cancel, a death, a step that threw).
// What the error already carries wins: a composite mid-restoration says so in its own words
export const carryReport = (error, report = {}, notes = []) =>
  Object.assign(error, { report: { ...report, ...(notes.length ? { notes: notes.join('; ') } : {}), ...error.report } })

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
