// Who owns the body: one job at a time off the durable shelf (src/job-shelf.mjs), run by runLong or as a quick
// action, pumped by the scheduler (src/job-scheduler.mjs); cancelTask is how a reflex, a death or a driver takes it back.
import { settleInventory } from './inventory-settle.mjs'
import path from 'node:path'
import { bedtimeReport, mealTally, scaffoldNote, scaffoldTakeBack, scaffoldBuilt, oversleeping, digFromHere, refuseReason, mayDig } from '../lib.mjs'
import { createJobShelf } from '../job-shelf.mjs'
import { createJobScheduler } from '../job-scheduler.mjs'
import { failedResult, deathLine } from '../composite.mjs'
import { HOME } from './home.mjs'
import { emit } from './events.mjs'
import { inventoryCounts, diffCounts } from './helpers.mjs'
import { Vec3, bot, ready, task, setTask, gen, setGen, pos } from './state.mjs'
import { lastDeath, mealsEaten, basesOwed } from './connection.mjs'
import { gatesPassed, shutGatesBehind, shutTrackedGatesAfterCancel, leaveFenceCell } from './doors.mjs'
import { fighting, setFighting, fightStart, setFightStart, surfacing, diggingOut, flee, endFlee, holingUp } from './reflexes.mjs'
import { explainFailure } from './explain.mjs'
import { straysAt } from './pens.mjs'
import { long, quick } from './actions/tables.mjs'
import { useMoves } from './actions/move.mjs'

export let followTarget = null
export const setFollowTarget = v => { followTarget = v }
// every cell the pathfinder aimed a scaffolding placement at during this task. Chani's cobblestone went that way twice with
// nothing in the reply to say so (#111), so a task now reports what it built beside its drops and takes back what it can reach
export let scaffolded = []
export let bedFailures = 0
export const setBedFailures = v => { bedFailures = v }

// A reflex cancellation (wedged, stalled, holing up, out of air, a death) no longer freezes the queue by itself:
// whether it should is now the job-policy.mjs decision job-scheduler.mjs makes once the cancelled job's own result
// settles (severeFailure: the body died, or the same job failing twice running).
export function cancelTask (why) {
  const active = jobShelf.snapshot().active
  if (active != null) jobShelf.markCancelling(active, why)
  setGen(gen + 1)
  if (task) emit('task_cancelled', { id: task.id, name: task.name, why, ...(task.jobId ? { notify: false } : {}) })
  if (task) lastCancel = { id: task.id, why }
  task?.releaseFollow?.()
  followTarget = null
  setTask(null)
  setFighting(null)
  setFightStart(null)
  bot.pathfinder.setGoal(null)
  bot.pvp.stop()
  try { bot.collectBlock.cancelTask() } catch {}
}

// drowned once while mining at 4 hp; don't start risky work half dead unless told to
export const refusalFor = (name, args) => (['trades', 'trade'].includes(name) && bot.health <= 5 && !args.force
  ? `health is ${Math.round(bot.health)}: eat/rest first, or pass force=true`
  : refuseReason({ name, health: bot.health, force: args.force, sleeping: bot.isSleeping }))

export let taskId = 0
export const setTaskId = v => { taskId = v }
let lastCancel = null
export const jobShelf = createJobShelf(path.join(HOME, 'jobs.json'))
taskId = Math.max(0, ...jobShelf.snapshot().jobs.map(job => Number(job.id) || 0))
for (const recovered of jobShelf.snapshot().jobs.filter(job => job.status === 'interrupted' && !job.recoveryReported)) {
  emit('job_interrupted', { id: recovered.id, name: recovered.name, error: recovered.error })
  jobShelf.patch(recovered.id, { recoveryReported: true })
}
export let scheduler

// One shelf slot owns all body-changing work. Submission persists before this pump claims it;
// nested composite/flow api.act calls still invoke their registered action directly.
// given: the plain arguments, for the log (printing the tracked ones would count as reading them all)
async function runLong (name, args, given = args, queuedAs = null) {
  const refusal = refusalFor(name, args)
  if (refusal) return { ok: false, error: refusal }
  if (task) return { ok: false, error: `body owner invariant violated: ${task.name} (${task.id}) is still active` }
  followTarget = null
  const mine = { id: queuedAs ?? ++taskId, name, gen, started: Date.now() }
  setTask(mine)
  mine.jobId = queuedAs ?? null
  console.log(`[task ${mine.id}] ${name} ${JSON.stringify(given)}`)
  gatesPassed.clear()
  useMoves(mayDig(name, args))
  await leaveFenceCell().catch(() => {})
  if (task !== mine || mine.gen !== gen) return { ok: false, cancelled: true, task: mine.id, error: 'cancelled before action start' }
  scaffolded = []
  // morning, and the server still has me in bed: wake only after the scheduler has durably reserved this job.
  if (bot.isSleeping && name !== 'wake' && oversleeping({ asleep: true, timeOfDay: bot.time.timeOfDay, thundering: bot.thunderState > 0 })) {
    await long.wake().catch(() => {})
    if (task !== mine || mine.gen !== gen) return { ok: false, cancelled: true, task: mine.id, error: 'cancelled while waking' }
  }
  const before = inventoryCounts()
  const mealsAtStart = { ...mealsEaten }
  const finish = (extra) => {
    const seconds = Math.round((Date.now() - mine.started) / 1000)
    // low vitals ride along so the driver needn't poll state
    const vitals = bot.health <= 10 || bot.food <= 8 ? { hp: Math.round(bot.health), food: bot.food } : {}
    const result = { task: mine.id, action: name, seconds, ...mealTally({ ...diffCounts(before, inventoryCounts()), ate: diffCounts(mealsAtStart, mealsEaten).gained }), pos: pos(), ...vitals, ...extra }
    // back to walking, so a later flee or follow doesn't tunnel
    if (task === mine) { setTask(null); useMoves(false); bot.setControlState('sneak', false) }
    return result
  }
  // inventory updates trail the action by a few ticks; wait so gained/lost are accurate
  // ...and until two looks 5 ticks apart agree (1 s at most): after a transfer that failed part-way the server's resync came later still, and its
  // -bamboo:64 turned up in the NEXT command's reply
  const settle = () => settleInventory(bot, inventoryCounts)
  // not after toggle (it is the tool for this); not when another task has taken over
  // bamboo bases the wedge reflex dug to free me: plant them again, whether the task worked or not
  const replantBases = async () => {
    if (!basesOwed.length) return {}
    if (task !== mine) return { restorationPending: `${basesOwed.length} bamboo base${basesOwed.length === 1 ? '' : 's'} still need restoration` }
    const owed = basesOwed.splice(0)
    const { placed = 0 } = await long.place({ blocks: owed }).catch(() => ({}))
    const note = `${placed} of ${owed.length} bases I dug to free myself${placed < owed.length ? `: plant the rest (place item=bamboo at ${owed.map(o => `${o.x},${o.y},${o.z}`).join(' ')})` : ''}`
    return { bambooReplanted: note, ...(placed < owed.length ? { restorationPending: note } : {}) }
  }
  // what the walk climbed on: dig back the pillars still standing within reach, and say where the rest are
  const reclaimScaffold = async () => {
    if (!scaffolded.length) return {}
    const tally = cells => cells.reduce((n, c) => ({ ...n, [c.name]: (n[c.name] ?? 0) + 1 }), {})
    // what it built and what is still there differ: a later leg of the same walk digs its own steps away again
    const standing = scaffoldBuilt(scaffolded, c => bot.blockAt(new Vec3(c.x, c.y, c.z))?.name ?? null)
    const taken = []
    // a walk that failed leaves its pillars too, and must still say so; only one that still has the body may dig them back, and only
    // from where it stands: a dig that walked to a pillar out of reach stood on the pillar and dug it from under itself (card 3fe30fb4)
    const feet = bot.entity.position
    for (const c of task === mine ? scaffoldTakeBack(standing, feet).filter(c => digFromHere(feet, c)) : []) {
      const dug = await long.dig({ x: c.x, y: c.y, z: c.z }).then(() => true, () => false)
      if (dug) taken.push(c)
    }
    const left = standing.filter(c => !taken.includes(c))
    const note = scaffoldNote(tally(scaffolded), tally(taken), left)
    return note ? { scaffold: note, restorationPending: note } : {}
  }
  const tidy = async r => {
    if (task !== mine || name === 'toggle') return { ...r, ...await replantBases(), ...await reclaimScaffold().catch(e => ({ restorationPending: e.message })) }
    r = { ...r, ...await replantBases(), ...await reclaimScaffold().catch(e => ({ restorationPending: e.message })) }
    const { shut: gatesShut, far: gatesLeftOpen } = await shutGatesBehind().catch(() => ({ shut: 0 }))
    // an animal that left the pen at my heels: say so now, not at nightfall when the pen is empty (Kettricken built an airlock over this)
    const out = [...gatesPassed].map(straysAt).filter(Boolean).join(' ')
    const outsideGate = out ? `${out}: outside the pen gate you just used. If it belongs inside it slipped out with you: lead it back now (flock.lead mob= x= y= z= of a cell inside)` : undefined
    return { ...r, ...(gatesShut ? { gatesShut } : {}), ...(gatesLeftOpen ? { gatesLeftOpen } : {}), ...(outsideGate ? { outsideGate } : {}) }
  }
  const work = long[name](args).then(tidy).then(
    r => settle().then(() => finish({ ok: true, ...r })),
    // a cancelled task fails with the pathfinder's vague "goal was changed": say why it was cancelled instead
    e => settle().then(replantBases).then(async b => ({
      ...b,
      ...await reclaimScaffold().catch(error => ({ restorationPending: error.message })),
      ...await shutTrackedGatesAfterCancel()
    })).then(b => {
      // a death while it ran: the kit lies where the body fell, and the driver collects there first
      const carried = deathLine({ diedAt: lastDeath?.at, startedAt: mine.started, pos: lastDeath?.pos, kit: lastDeath?.kit })
      return finish(failedResult({ base: b, report: { ...e.report, ...(carried ? { carried } : {}) }, cancelled: lastCancel?.id === mine.id, why: lastCancel?.why, message: e.message, explain: explainFailure }))
    })
  )
  mine.work = work
  return await work
}

async function executeAcceptedJob (job) {
  const { name, args, given, id } = job
  if (jobShelf.get(id)?.status !== 'running') return { ok: false, cancelled: true, error: 'cancelled before the body action started' }
  if (long[name]) return runLong(name, args, given, id)
  if (!quick[name]) return { ok: false, error: `unknown action ${name}` }
  const mine = { id, name, gen, started: Date.now(), jobId: id, progress: {} }
  setTask(mine)
  try {
    const refusal = refusalFor(name, args)
    if (refusal) return { ok: false, error: refusal }
    useMoves(mayDig(name, args))
    const result = await quick[name](args)
    if (name === 'follow') await new Promise(resolve => { mine.releaseFollow = resolve })
    return { ok: true, action: name, ...result }
  } finally {
    if (task === mine) { setTask(null); useMoves(false); bot.setControlState('sneak', false) }
  }
}

scheduler = createJobScheduler({
  shelf: jobShelf,
  execute: executeAcceptedJob,
  emit,
  isReady: () => ready && !flee && !holingUp && !fighting && !surfacing && !diggingOut,
  onTerminal: (job, result, status) => {
    if (status === 'completed' || status === 'failed') emit('task_done', { ...(result ?? {}), job: job.id, task: job.id, action: job.name, notify: false })
    if (status === 'failed' && job.name === 'sleep' && job.given?.automatic) {
      const report = bedtimeReport(result?.error ?? result?.message)
      if (report && bedFailures++ === 0) emit('bedtime_failed', { error: report })
    }
  }
})
setInterval(() => scheduler?.pump(), 500)

export function submitJob (name, args, given = args, { urgent = false, verbose = false, notify = given?.automatic !== true } = {}) {
  const record = scheduler.submit({ name, args, given }, { urgent, verbose, notify })
  taskId = Math.max(taskId, record.id)
  return record
}

export function stopAllJobs () {
  const { active, dropped } = scheduler.stop(reason => cancelTask(reason))
  followTarget = null; endFlee()
  const held = jobShelf.snapshot().held
  return { ok: true, stopped: active ?? null, dropped: dropped.map(job => job.id), ...(held ? { restorationPending: held.reason } : {}) }
}

function cancelAcceptedJob (id, reason = 'cancelled by request') {
  const job = jobShelf.get(id)
  if (!job) return { ok: false, error: `no job ${id}` }
  const cancelled = scheduler.cancel(id, reason)
  if (cancelled.cleanup === 'pending') cancelTask(reason)
  return cancelled
}

export function jobControl (name, args) {
  if (name === 'jobs') return { ok: true, ...jobShelf.list({ after: args.after ?? 0, limit: args.limit, all: args.all === true }) }
  if (name === 'job') {
    const job = jobShelf.get(args.id)
    return job ? { ok: true, job } : { ok: false, error: `no job ${args.id}` }
  }
  if (name === 'cancel') return cancelAcceptedJob(args.id)
  if (name === 'resume') {
    const result = scheduler.resume({ recovered: args.recovered === true })
    return { ok: !result.blocked, ...result, queued: jobShelf.list().queued.length }
  }
  if (name === 'discard') {
    const { dropped: discarded, held, blocked } = scheduler.discard('discarded by request')
    return {
      ok: !blocked, discarded: discarded.map(job => job.id), clearedHold: Boolean(held && !blocked),
      ...(blocked ? { restorationPending: held.reason, note: 'resume recovered=true once the body is safe; discard is refused while that restoration is pending' } : {})
    }
  }
  return null
}
