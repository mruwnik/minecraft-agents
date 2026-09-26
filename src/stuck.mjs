// The stuck watch (autopilot card): a role on autopilot runs for days with no driver reading its results, so the body
// itself must say when it is going nowhere. bot.mjs takes one sample a second and keeps the last WINDOW_MS of them:
//   { t, pos, taskId, taskName, taskProgress, sleeping, night, health, food, edible, oxygen, holedUp, buried, boxed,
//     frozenWalks, failedWalks (running counts), routine: { lastDayStartedAt, day, failedSteps: [{ day, step }], storageFull: [day], phase } | null }
// stuckVerdict reads the window and names the most urgent condition that holds; nextEpisode turns a run of verdicts
// into one episode, said once (the `stuck` event and the chat line) and shown by `state` while it lasts. Pure.

export const WINDOW_MS = 30 * 60000
export const STILL_MS = 3 * 60000
export const FROZEN_MS = 5 * 60000
export const FROZEN_WALKS = 3
export const FAILED_WALKS = 2
export const HOLE_MS = 5 * 60000
export const OXYGEN_MS = 20000
export const DAY_MS = 30 * 60000
export const LOW_HEALTH = 6
// an episode ends only after the verdict has been clear this long (a one-tick blip is not freedom), and a second
// episode of the same kind within this much of the last one's start is held but not announced again
export const END_MS = 30000
export const REPEAT_MS = 120000
const STILL_DIST = 0.5
const WAITING = ['dusk', 'dawn']

export const addSample = (samples, sample, keepMs = WINDOW_MS) => [...samples, sample].filter(s => sample.t - s.t <= keepMs)

const posText = ({ x, y, z }) => `${Math.floor(x)},${Math.floor(y)},${Math.floor(z)}`
export const stuckLine = (pos, reason) => `stuck at ${posText(pos)}: ${reason}`

const minutes = ms => `${Math.round(ms / 60000)} min`
const near = (a, b) => Math.abs(a.x - b.x) < STILL_DIST && Math.abs(a.y - b.y) < STILL_DIST && Math.abs(a.z - b.z) < STILL_DIST

// the samples of the last `ms` before the newest one, and whether they reach back that far at all: a young window
// (a body up for a minute) says nothing about three minutes
const lastSpan = (samples, ms) => {
  const last = samples[samples.length - 1]
  const window = samples.filter(s => s.t >= last.t - ms)
  return { last, window, covered: samples.some(s => s.t <= last.t - ms) }
}

// every consecutive pair non-increasing, and the end lower than the start
const falling = values => values.every((v, i) => i === 0 || v <= values[i - 1]) && values[values.length - 1] < values[0]

const drowning = samples => {
  const { last, window, covered } = lastSpan(samples, OXYGEN_MS)
  if (!covered || !falling(window.map(s => s.oxygen))) return null
  return { kind: 'oxygen', reason: `oxygen falling ${OXYGEN_MS / 1000} s`, advice: 'the body is drowning: surface (swim up), or dig the block over the head if a roof holds it under', pos: last.pos }
}

const starving = ({ last }) => last.health <= LOW_HEALTH && !last.edible
  ? { kind: 'health', reason: `health ${Math.round(last.health)}, nothing edible carried`, advice: 'fetch food before anything else (WORLD.md says where the shared food is) and do not walk far or fight until health is back', pos: last.pos }
  : null

// holed up or buried in daylight: a night hole is a shelter, a day one is a body that never came out
const entombed = samples => {
  const { last, window, covered } = lastSpan(samples, HOLE_MS)
  if (!covered) return null
  const how = window.every(s => s.buried && !s.night) ? 'buried' : window.every(s => (s.holedUp || s.buried) && !s.night) ? 'holed' : null
  if (!how) return null
  return { kind: how, reason: `${how === 'buried' ? 'buried' : 'holed up'} ${minutes(HOLE_MS)} by day`, advice: 'dig the block over the head (dig at the cell above), climb out and goto daylight; a body that cannot dig needs somebody to open the hole', pos: last.pos }
}

// frozenWalks counts every frozen_walk the body ever said: the count just before the window is the base
const frozen = samples => {
  const { last, window } = lastSpan(samples, FROZEN_MS)
  const before = samples.filter(s => s.t < last.t - FROZEN_MS)
  const base = before.length ? before[before.length - 1].frozenWalks : Math.min(...window.map(s => s.frozenWalks))
  if (last.frozenWalks - base < FROZEN_WALKS) return null
  return { kind: 'frozen', reason: `${FROZEN_WALKS} walks froze in ${minutes(FROZEN_MS)}`, advice: 'read events type=frozen_walk last=3: each names what the legs push into (a fence, a mob, a turned head); clear it or walk round it before the next goto', pos: last.pos }
}

// how far back from the newest sample `holds` is true of every sample without a break, in ms: a run, not a window, so a
// body that stepped away and came back starts its count again, and one condition is measured from when it began
const runBack = (samples, holds) => {
  const last = samples[samples.length - 1]
  let first = last
  for (let i = samples.length - 1; i >= 0 && holds(samples[i], last); i--) first = samples[i]
  return last.t - first.t
}

// Boxed in (no neighbouring cell to step to) for STILL_MS by day, whatever the body's tasks are doing: ClaudeProbe walled
// itself in with `place` calls a second long, and a rule that read the task first saw a new task id every few seconds,
// cleared the verdict and fired three alerts in 15 s (09-26 21:58Z). Never at night: a hole dug by hand and capped over
// at dusk carries no holedUp flag, and a body sitting in one is sheltering, not stuck (Pacer, 09-26 19:20Z)
const boxedRun = (s, last) => s.boxed && !s.night && !s.sleeping && near(s.pos, last.pos)
// or a task that has neither moved the body nor finished a step for STILL_MS (a routine between its days waits on purpose)
const taskRun = (s, last) => s.taskId === last.taskId && s.taskProgress === last.taskProgress && !s.sleeping && near(s.pos, last.pos)
const still = samples => {
  const last = samples[samples.length - 1]
  if (last.sleeping) return null
  if (last.boxed && !last.night && runBack(samples, boxedRun) >= STILL_MS) {
    return { kind: 'boxed', reason: `boxed in for ${minutes(STILL_MS)}`, advice: 'no neighbouring cell to step to: dig or open a way out (a fence gate, the block in the way, the block over the head), or ask in chat for somebody to', pos: last.pos }
  }
  const waiting = last.routine && WAITING.includes(last.routine.phase)
  const working = last.taskId !== null && last.taskId !== undefined && !waiting
  if (!working || runBack(samples, taskRun) < STILL_MS) return null
  return { kind: 'still', reason: `no movement and no progress in ${last.taskName} for ${minutes(STILL_MS)}`, advice: `${last.taskName} is going nowhere: stop it, step two blocks away (goto), start it again, and if the walk will not go read what surrounds the body (look, block_at)`, pos: last.pos }
}

// Walks that found no path from where the body stands, and the body not moving between them. failedWalks counts every
// walk that ended with no path (like frozenWalks); FAILED_WALKS of them inside a run of STILL_MS standing still is a body
// that cannot leave its cell, whatever its tasks say: a routine in a one-deep hole in its field was WAITING between its
// days (exempt on purpose) and its failing step was a day short of the three-day rule, so it sat five minutes with no
// alert (card 94e6dcb1). Any cell counts, day or night: the walks are the evidence, not the standing
const stillRun = (s, last) => !s.sleeping && near(s.pos, last.pos)
const walks = samples => {
  const last = samples[samples.length - 1]
  if (last.sleeping) return null
  const standing = runBack(samples, stillRun)
  if (standing < STILL_MS) return null
  const first = samples.find(s => s.t >= last.t - standing)
  if ((last.failedWalks ?? 0) - (first.failedWalks ?? 0) < FAILED_WALKS) return null
  return { kind: 'walks', reason: `${FAILED_WALKS} walks found no path and no movement in ${minutes(STILL_MS)}`, advice: 'the body cannot leave its cell: read what surrounds it (look, block_at at the four sides). A block low (a hole one deep, sunk into worked ground): pillar_up steps=1 lifts it out; a block high (perched over a field, farmland one below beside it): dig the block underfoot, or goto with dig=true; walled in: goto the same spot with dig=true, or dig the block in the way by hand', pos: last.pos }
}

// the same routine step failing on three consecutive routine days
const consecutive = days => days.some((day, i) => days[i + 1] === day + 1 && days[i + 2] === day + 2)
const failingStep = ({ last }) => {
  if (!last.routine) return null
  const byStep = new Map()
  for (const { day, step } of last.routine.failedSteps ?? []) byStep.set(step, [...(byStep.get(step) ?? []), day])
  const step = [...byStep.entries()].find(([, days]) => consecutive([...new Set(days)].sort((a, b) => a - b)))?.[0]
  if (!step) return null
  return { kind: 'step', reason: `${step} failed 3 days running`, advice: `run ${step} by hand and read its FAIL: the routine keeps failing it every day until what it names is fixed`, pos: last.pos }
}

// the store full two routine days running: the harvest is being carried round until somebody builds or empties a chest
const STORAGE_DAYS = 2
const fullStore = ({ last }) => {
  const days = [...new Set(last.routine?.storageFull ?? [])].sort((a, b) => a - b)
  if (!days.some((day, i) => days[i + 1] === day + 1)) return null
  return { kind: 'storage', reason: `the store was full ${STORAGE_DAYS} days running`, advice: 'the harvest is being carried round: build or empty a chest at the place (or point the routine at another with store=), and the next day stores it', pos: last.pos }
}

const hungDay = ({ last }) => {
  if (!last.routine || last.t - last.routine.lastDayStartedAt < DAY_MS) return null
  return { kind: 'day', reason: `no routine day started in ${minutes(DAY_MS)}`, advice: 'the routine is hung between days (a dusk or dawn that never came, or a step that never returned): stop and start it again', pos: last.pos }
}

// the most urgent condition that holds over the window, or null
export function stuckVerdict (samples) {
  if (!samples.length) return null
  const last = samples[samples.length - 1]
  return drowning(samples) ?? starving({ last }) ?? entombed(samples) ?? frozen(samples) ?? still(samples) ?? walks(samples) ?? failingStep({ last }) ?? fullStore({ last }) ?? hungDay({ last })
}

// One episode per kind of condition. `started` is the moment to write the event and say the chat line, once; `ended` the
// moment to say the body is free (stuck_end), once the verdict has been clear for END_MS. A finished episode is kept,
// with `over`, so a repeat of the same kind within REPEAT_MS of its start is held quietly (state shows it, nothing is said)
export function nextEpisode (episode, verdict, now) {
  const live = episode && !episode.over
  if (!verdict) {
    if (!live || now - episode.lastSeen < END_MS) return { episode, started: false, ended: false }
    return { episode: { ...episode, over: now }, started: false, ended: !episode.quiet }
  }
  if (live && episode.kind === verdict.kind) return { episode: { ...episode, lastSeen: now }, started: false, ended: false }
  const { kind, reason, advice } = verdict
  const quiet = Boolean(episode && episode.kind === kind && now - episode.since < REPEAT_MS)
  return { episode: { kind, reason, advice, since: now, lastSeen: now, quiet }, started: !quiet, ended: false }
}

// what `state` shows while an episode lasts, and nothing once it is over
export const stuckField = episode => episode && !episode.over ? episode.reason : undefined
