// The stuck watch over the body's own progress (src/navigation/stuck.mjs decides; this takes the samples).
import { feetCell, boxedIn, isNight } from '../lib.mjs'
import { addSample, stuckVerdict, nextEpisode, stuckLine } from '../navigation/stuck.mjs'
import { emit } from './events.mjs'
import { edibleCarried } from './runner.mjs'
import { frozenWalks } from '../bot.mjs'
import { Vec3, bot, task, pos } from './state.mjs'
import { diggingOut, holedUp, holingUp } from './reflexes.mjs'
import { jobShelf } from './jobs.mjs'

// the stuck watch (src/navigation/stuck.mjs, autopilot card): one sample a second over a rolling window, one `stuck` event and one
// chat line per episode, stuck=<reason> in `state` while it lasts
let stuckSamples = []
export let stuckNow = null
let failedWalks = 0 // every walk that ended with no path, for the watch's walks verdict (a body that cannot leave its cell)
export const noPathCounted = e => { if (/no path to the goal|no walkable path|took to long to decide/i.test(e.message)) failedWalks++; return e }
export let stepsDone = 0 // composite steps finished: the task progress the watch reads
export const setStepsDone = n => { stepsDone = n }
const stuckSample = () => ({
  t: Date.now(), pos: bot.entity.position.clone(), taskId: task?.id ?? null, taskName: task?.name ?? null, taskProgress: stepsDone,
  waiting: task?.jobId ? jobShelf.get(task.jobId)?.progress?.waiting ?? null : null,
  sleeping: bot.isSleeping, night: isNight(bot.time.timeOfDay), health: bot.health, food: bot.food, edible: edibleCarried(),
  oxygen: bot.oxygenLevel, holedUp: Boolean(holedUp) || holingUp, buried: diggingOut, boxed: trappedIn(), frozenWalks, failedWalks,
  routine: task?.progress?.routine ?? null
})
// boxed in for the watch: amBoxedIn (#128) is about solid shafts and reads the cell over a fence as a ledge to step up
// onto, but a fence or wall is a block and a half tall, so a body fenced into a 1x1 cell has no way out either
const TALL = /_fence$|_wall$|_fence_gate$/
const openGate = b => /_fence_gate$/.test(b?.name ?? '') && String(b?.getProperties?.().open) === 'true'
const trappedIn = () => {
  if (!bot?.entity) return false
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  const at = (dx, dy, dz) => bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz))
  const passable = (dx, dy, dz) => { const b = at(dx, dy, dz); return openGate(b) || b?.boundingBox !== 'block' }
  return boxedIn((dx, dy, dz) => passable(dx, dy, dz) && !(dy === 1 && (dx || dz) && TALL.test(at(dx, 0, dz)?.name ?? '') && !openGate(at(dx, 0, dz))))
}
export function watchStuck () {
  stuckSamples = addSample(stuckSamples, stuckSample())
  const { episode, started, ended } = nextEpisode(stuckNow, stuckVerdict(stuckSamples), Date.now())
  stuckNow = episode
  // free again (the verdict clear for END_MS): said once, in the log only
  if (ended) emit('stuck_end', { pos: pos(), reason: episode.reason, seconds: Math.round((episode.over - episode.since) / 1000) })
  if (!started) return
  emit('stuck', { pos: pos(), reason: episode.reason, advice: episode.advice })
  bot.chat(stuckLine(bot.entity.position, episode.reason))
}
