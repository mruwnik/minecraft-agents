// The bedtime reflex, and the shared clock file the logged-off agents read.
import { automaticBeds, carriedBedSpot, reflexPickups } from '../lib/sleep.mjs'
import fs from 'node:fs'
import path from 'node:path'
import { ownBed, automaticNightPlan, bedtime, feetCell, isNight } from '../lib.mjs'
import { WORLD_DIR, cfg } from './home.mjs'
import { zones, readPlaces, savePlaces, emit } from './events.mjs'
import { inventoryCounts, cellAt, bedsNear } from './helpers.mjs'
import { Vec3, bot, ready, task, pos } from './state.mjs'
import { lastDriven } from './connection.mjs'
import { reflexes, nearbyHostiles, fighting, surfacing, diggingOut, flee, holedUp, setHoledUp, holingUp } from './reflexes.mjs'
import { bedFailures, setBedFailures, jobShelf, scheduler, submitJob } from './jobs.mjs'
import { long } from './actions/tables.mjs'

const villagers = () => Object.values(bot.entities).filter(e => e.name === 'villager').map(e => e.position)
export const automaticSleepBeds = () => automaticBeds(bedsNear(), zones, readPlaces(), cfg.username, villagers())
export const carriedBed = () => bot.inventory.items().find(i => i.name.endsWith('_bed'))
export const carriedBedPlace = () => carriedBedSpot({
  feet: feetCell(bot.entity.position, bot.entity.onGround), cellAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)),
  zones, places: readPlaces(), me: cfg.username, residents: villagers()
})
// each bed the reflex puts down gets its own mark on the shared map, so a restart still knows to pick it up and a
// driver's own <me>-bed mark (and bed) is never touched
const reflexBeds = () => readPlaces().filter(p => p.reflex === true && p.by === cfg.username)
const unmark = name => savePlaces(readPlaces().filter(p => p.name !== name))
export async function placeReflexBed (item, { x, y, z, facing }) {
  const name = `${cfg.username}-bed-${x}_${y}_${z}`
  const mark = () => savePlaces([...readPlaces().filter(p => p.name !== name), { name, kind: 'bed', x, y, z, by: cfg.username, reflex: true, item, note: 'placed by the bedtime reflex' }])
  try {
    await long.place({ item, x, y, z, facing })
  } catch (err) {
    // the bed went down but a later check failed: an unmarked reflex bed would never be picked up
    if (cellAt(x, y, z)?.name === item) mark()
    throw err
  }
  mark()
  emit('bed_placed', { at: `${x},${y},${z}`, item, note: 'no bed of mine nearby: put down the one I carry to sleep in, and pick it up by day' })
}
// through the scheduler, so the pick-up queues behind the driver's work instead of racing it. Waited on rather than
// caught in onTerminal: a job dropped from the queue (stop, discard) never reaches onTerminal
const FOREVER_MS = 2 ** 31 - 1
const pickingUp = new Set()
async function pickUpReflexBed ({ name, x, y, z, item }) {
  pickingUp.add(name)
  const before = inventoryCounts()[item] ?? 0
  await scheduler.wait(submitJob('dig', { x, y, z }, { automatic: true }).id, FOREVER_MS)
  // judged by the world, not the job's status: a dig that failed after breaking the bed still took it down.
  // One left standing stays in pickingUp, so it is not retried this run: every failed job holds the driver's queue
  const cell = cellAt(x, y, z)
  if (!cell || cell.name === item) return
  pickingUp.delete(name)
  unmark(name)
  const pocketed = (inventoryCounts()[item] ?? 0) > before
  emit('bed_picked_up', { at: `${x},${y},${z}`, item, note: pocketed ? 'picked up the bed I put down for the night' : 'took down the bed I put down for the night, but it did not reach my pockets: it may lie on the ground there' })
}
let lastBedTry = 0
export let bedWalkFailed = false // a walk to the own bed failed or came up short tonight: go straight to placement, not retried till the next night
export const setBedWalkFailed = v => { bedWalkFailed = v }
setInterval(() => {
  if (!ready) return
  const now = Date.now()
  // #147: holed up for the night means staying in the hole, not walking out of it to the bed past what put me there
  const night = isNight(bot.time.timeOfDay)
  if (!night) { setHoledUp(null); bedWalkFailed = false }
  for (const { bed, do: step } of reflexPickups({ night, asleep: bot.isSleeping, reflexes, beds: reflexBeds(), cellAt, from: bot.entity.position, inFlight: pickingUp })) {
    if (step === 'unmark') unmark(bed.name)
    else pickUpReflexBed(bed)
  }
  const bedNear = automaticSleepBeds().length > 0
  const hostileNear = nearbyHostiles(8).length > 0
  const carried = night && !bedNear && Boolean(carriedBed()) && Boolean(carriedBedPlace())
  const from = pos()
  // the shared map is only worth reading once it is night: by day there is no bedtime plan to make
  const plan = night
    ? automaticNightPlan({ near: bedNear, bed: ownBed(readPlaces(), cfg.username, { from }), from, carried, walkFailed: bedWalkFailed, hostileNear })
    : { do: 'stop' }
  const bedWalk = plan.do === 'walk'
  const bedCarried = plan.do === 'place'
  const tired = bedtime({
    night, busy: !!task || jobShelf.snapshot().active != null || jobShelf.list().queued.length > 0 || Boolean(jobShelf.snapshot().held) || Boolean(holedUp) || Boolean(flee) || Boolean(holingUp) || Boolean(fighting) || surfacing || diggingOut || Boolean(bot.vehicle), asleep: bot.isSleeping, bedNear, bedCarried, bedWalk,
    hostileNear, reflexes, idleMs: now - lastDriven, sinceTryMs: now - lastBedTry, failures: bedFailures
  })
  if (!night || bot.isSleeping) setBedFailures(0)
  if (!tired) return
  lastBedTry = now
  if (bedFailures === 0) emit('bedtime', {
    note: bedNear ? 'night, no orders, a bed nearby: going to bed by myself'
      : bedWalk ? `night, no orders, my own bed ${plan.distance} blocks off: walking to it and going to bed`
        : 'night, no orders, no bed nearby: placing the bed I carry and going to bed'
  })
  // say so once a night: the driver is told, and the retries (ever further apart) stay quiet
  submitJob('sleep', { timeout: 60, automatic: true }, { automatic: true })
}, 10000)
// shared clock: lets logged-off agents (no bed, rule 3) see when it is day without reconnecting
setInterval(() => {
  if (!ready) return
  // written whole or not at all (own temp file, then rename): every body writes this file and readers caught it empty
  const fresh = path.join(WORLD_DIR, `clock.${cfg.username}.tmp`)
  fs.writeFileSync(fresh, JSON.stringify({ day: !isNight(bot.time.timeOfDay), timeOfDay: bot.time.timeOfDay, by: cfg.username, at: Date.now() }))
  fs.renameSync(fresh, path.join(WORLD_DIR, 'clock.json'))
}, 5000)
