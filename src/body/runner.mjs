// The composite runner ("autopilot"): loads library/ composites, hands each the api it may act through, and stops it
// by the hand-back rules.
import fs from 'node:fs'
import path from 'node:path'
import { breedPlan } from '../villager/breed.mjs'
import { woodenGate } from '../enclosure/blocks.mjs'
import { boatHabitatPlan } from '../boat/habitat.mjs'
import { nightPlan } from '../lib/sleep.mjs'
import { eatAllowed, BANNED_FOOD, workRefusal, parsePlan, parsePlacePlan, hasPlan, planCells, planBill, isNight, mayDig, makeUntil, PAUSES, handBackReason, checkArgs } from '../lib.mjs'
import { carryReport, compositeResult, CompositeHandBack as HandBack, recoverableNavigationTarget, navigationTargetKey } from '../composite.mjs'
import { scaffoldJournal } from '../scaffold/journal.mjs'
import { silkTouchTool } from '../tree/hives.mjs'
import { latestOwnClosedDigHole } from '../survival/recover-hole.mjs'
import { ROOT, HOME, cfg } from './home.mjs'
import { readPlaces, emit, zones } from './events.mjs'
import { carriedFood, fighting, automaticSleepBeds, stepsDone, setStepsDone, flee, holingUp, refusalFor } from '../bot.mjs'
import { Vec3, reportPerformance, bot, ready, task, cancelGuard, ROLLBACK_PLACE, pos } from './state.mjs'
import { explainFailure } from './explain.mjs'
import { penAround, censusOf } from './pens.mjs'
import { long, quick } from './actions/tables.mjs'
import { useMoves } from './actions/move.mjs'
import { bedsNear, inventoryCounts, dropsNear } from './helpers.mjs'

// ---------------------------------------------------------------- the composite runner ("autopilot")
// src/bot.mjs holds primitives; a composite is one file in library/, `export default { doc, args, run }`. The runner loads
// them at body start and registers each in `long`, so to a driver a composite is an ordinary action: a new one cancels
// the old, `state` shows it as doing=, and one that outlasts timeout= reports through task_done like anything else.
// The hand-back rules belong to the RUNNER: conversation is observable while a job runs and does not
// implicitly cancel it; health, food, night, and repeated-step failures still can.
export const LIBRARY_DIR = path.join(ROOT, 'library')
// library/<file>.mjs is the action <file>; library/<folder>/<file>.mjs is <folder>.<file>. A new domain is a new folder.
export const libraryFiles = () => {
  if (!fs.existsSync(LIBRARY_DIR)) return []
  const here = fs.readdirSync(LIBRARY_DIR, { withFileTypes: true })
  const top = here.filter(e => e.isFile() && e.name.endsWith('.mjs')).map(e => e.name)
  const nested = here.filter(e => e.isDirectory()).flatMap(dir =>
    fs.readdirSync(path.join(LIBRARY_DIR, dir.name)).filter(f => f.endsWith('.mjs')).map(f => `${dir.name}/${f}`))
  return [...top, ...nested].sort()
}
export const compositeName = file => file.replace(/\.mjs$/, '').split('/').join('.')
// what ./mc help knows about the composites this body loaded
export const composites = new Map()
// actions the CLI answers by itself, with no body running
export const CLI_ONLY = ['wait', 'dawn', 'clock', 'job', 'jobs', 'cancel', 'resume', 'discard']
// Preserve the runner export used by bot.mjs; the policy itself lives with food logic.
export { BANNED_FOOD }
const worldDay = () => Math.floor(Number(bot.time.age ?? 0) / 24000)
// a body below the hunger floor carrying rotten flesh is not a body with nothing edible: it has a meal it is now
// allowed to eat, and a composite that stopped for "nothing edible carried" was stopping over its own dinner (#139)
export const edibleCarried = () => eatAllowed({ food: bot.food, carried: carriedFood() }).allowed.length > 0
// a saved plan with its cells in world coordinates and what it would cost to build. Every composite that builds,
// tills, plants or fetches from a NAMED place resolves through here - farm.build, pen.build, farm.maintain,
// farm.compost, farm.get_seeds, flock.maintain - and none of them only reads, so this is where they all ask whose
// ground it is (#144). The apiary and flock composites resolve through placeTarget, which apiary.inspect shares, so
// they ask for themselves rather than have a reading action refused.
function planOf (name) {
  const place = readPlaces().find(p => p.name === name)
  if (!place) throw new Error(`no place called ${name}: mark it, then save a map with ./mc plan name=${name} kind=farm x= y= z= map='...'`)
  const refusal = workRefusal(place, cfg.username)
  if (refusal) throw new Error(refusal)
  if (!hasPlan(place)) throw new Error(`${name} is on the map but has no plan: save one with ./mc plan name=${name} map='...'`)
  const parsed = parsePlacePlan(place)
  if (parsed.error) throw new Error(parsed.error)
  return { ...place, parsed, cells: planCells(place), bill: planBill(parsed) }
}

// everything a composite may do to the world, and the only way it may do it
function makeApi (composite, a, alive, jobEvent = () => {}) {
  const ownerTask = task
  let cleanupServiceDirection = null
  const notes = []
  const report = {}
  const failures = new Map()
  let pendingNavigationFailure = null
  const startedDay = worldDay()
  const startedAt = Date.now()
  let sleptTonight = false
  const night = () => isNight(bot.time.timeOfDay)
  const blockAt = (x, y, z) => {
    const b = bot.blockAt(new Vec3(Math.floor(x), Math.floor(y), Math.floor(z)))
    return b ? { name: b.name, properties: b.getProperties?.() ?? {}, solid: b.boundingBox === 'block', shapes: b.shapes } : null
  }
  const failedTwice = () => [...failures.values()].find(f => f.count >= 2)?.why
  const noteFailure = (name, why) => {
    const seen = failures.get(name)
    failures.set(name, { why, count: seen?.why === why ? seen.count + 1 : 1 })
  }
  const act = async (name, args = {}) => {
    pendingNavigationFailure = null
    alive()
    const fn = long[name] ?? quick[name]
    if (!fn) throw new Error(`${composite}: no action called ${name}`)
    const refusal = refusalFor(name, args)
    if (refusal) throw new Error(`${composite}/${name}: ${refusal}`)
    useMoves(mayDig(name, args))
    return Promise.resolve().then(() => fn(args)).then(
      r => { failures.delete(name); setStepsDone(stepsDone + 1); return r ?? {} },
      e => {
        if (e instanceof HandBack || e?.reason) throw e
        const why = `${name}: ${explainFailure(e.message)}`
        noteFailure(name, why)
        const target = name === 'goto' ? recoverableNavigationTarget(e, args) : null
        if (composite === 'forage.search' && target) {
          pendingNavigationFailure = { target, why }
        }
        throw new Error(`${composite}/${why}`)
      })
  }
  const recoverNavigationFailure = target => {
    alive()
    const pending = pendingNavigationFailure
    pendingNavigationFailure = null
    if (composite !== 'forage.search' || !pending || !pending.target ||
        navigationTargetKey(target) !== pending.target) return false
    const recorded = failures.get('goto')
    if (recorded?.why !== pending.why) return false
    failures.delete('goto')
    return true
  }
  const cleanupAct = async (name, args = {}) => {
    const protectedGates = composite === 'villager.breed'
      ? breedPlan(a).gates
      : composite === 'boat.receive' ? boatHabitatPlan(a).gates : []
    const closeProtectedGate = name === 'toggle' && args.open === false && protectedGates.some(gate => {
      if (!['x', 'y', 'z'].every(k => args[k] === gate[k])) return false
      const gateBlock = bot.blockAt(new Vec3(gate.x, gate.y, gate.z))
      return woodenGate(gateBlock?.name) && ['east', 'west'].includes(gateBlock.getProperties?.().facing)
    })
    const lectern = name === 'place' && args.blocks === undefined && args.item === (a.block ?? 'lectern') && ['x', 'y', 'z'].every(k => args[k] === a[k])
    const dx = args.x - a.x, dz = args.z - a.z
    const serviceStand = name === 'goto' && args.y === a.y && args.range === 0 &&
      ((Math.abs(dx) === 3 && dz === 0) || (Math.abs(dz) === 3 && dx === 0))
    const serviceSill = name === 'place' && args.blocks === undefined && args.item === (a.penBlock ?? 'cobblestone') &&
      args.y === a.y && cleanupServiceDirection && [1, 2].some(n =>
        dx === cleanupServiceDirection.x * n && dz === cleanupServiceDirection.z * n)
    if (!closeProtectedGate && (composite !== 'villager.roll' || !(lectern || serviceStand || serviceSill))) throw new Error(`${composite}: cleanup may only restore its own job block/service route or close its own enclosure doorway`)
    const cell = `${a.x},${a.y},${a.z}`
    if (!ready || !bot.entity || bot.health <= 0 || bot.isSleeping) throw new Error(`${composite}: restoration pending at ${cell}: body is offline, dead, or sleeping`)
    if (flee || holingUp || fighting) throw new Error(`${composite}: restoration pending at ${cell}: emergency reflex owns the body`)
    if (task && task !== ownerTask) throw new Error(`${composite}: restoration pending at ${cell}: another task owns the body`)
    const refusal = refusalFor(name, args)
    if (refusal) throw new Error(`${composite}/${name}: ${refusal}`)
    if (closeProtectedGate) { useMoves(false); return long.toggle(args) }
    if (serviceStand) {
      cleanupServiceDirection = { x: dx / 3, z: dz / 3 }
      useMoves(false)
      return long.goto(args)
    }
    return long.place({ ...args, [ROLLBACK_PLACE]: true })
  }
  const waitForCondition = makeUntil({ waitTicks: n => bot.waitForTicks(n), alive, composite })
  const until = async (condition, options = {}) => {
    if (ownerTask?.jobId) jobEvent('job_waiting', { reason: options.what ?? 'condition' })
    try { return await waitForCondition(condition, options) }
    finally { if (ownerTask?.jobId) jobEvent('job_progress', { waiting: false }) }
  }
  // between steps: night with a bed is slept through and the composite never sees it; anything else that needs a person stops the task
  const checkpoint = async (extra = {}) => {
    pendingNavigationFailure = null
    alive()
    if (!night()) sleptTonight = false
    if (night() && bot.vehicle) throw new HandBack('night aboard a vehicle; find a checked landing before walking to a bed')
    // routine bed=: the bed every composite of this task sleeps in, never a nearer one (a creeper chased the body out
    // of an old bed 8 blocks from the one named). Out of range or out of reach is a hand-back, not the nearest bed
    const ownBed = ownerTask?.nightBed ?? null
    if (night() && !sleptTonight && (ownBed || automaticSleepBeds().length)) {
      sleptTonight = true
      if (ownBed) {
        const { bed } = ownBed
        const plan = nightPlan({ near: false, bed, from: pos(), bedRange: ownBed.bedRange })
        if (plan.do !== 'walk') throw new HandBack(`night, and ${plan.why}`)
        await long.goto({ x: bed.x, y: bed.y, z: bed.z, range: 2 }).catch(e => { throw new HandBack(`night, and the walk to ${bed.name} failed: ${e.message}`) })
      }
      // said out loud: an agent that saw `asleep doing=mine.get 174s` with nothing moving stopped it as wedged (Chani, twice)
      const mine = task
      if (mine) mine.paused = 'night'
      emit('task_paused', { id: mine?.id, name: composite, why: PAUSES.night })
      if (mine?.jobId) jobEvent('job_waiting', { reason: 'night', why: PAUSES.night })
      try {
        await long.sleep(ownBed ? { bed: `${ownBed.bed.x},${ownBed.bed.y},${ownBed.bed.z}` } : { automatic: true })
          .catch(e => { if (ownBed) notes.push(`${ownBed.bed.name}: ${e.message}`) })
        await until(() => !bot.isSleeping, { timeout: 900, every: 5, what: 'the night never ended' }).catch(() => {})
      } finally {
        if (mine) delete mine.paused
      }
      alive()
      emit('task_resumed', { id: mine?.id, name: composite })
      if (mine?.jobId) jobEvent('job_progress', { waiting: false })
    }
    const reason = handBackReason({
      spoken: null,
      health: bot.health,
      food: bot.food,
      edible: edibleCarried(),
      failedTwice: failedTwice(),
      invFull: bot.inventory.emptySlotCount() === 0,
      canDeposit: Boolean(extra.canDeposit),
      night: night(),
      // handBackReason only reads bedNear once it is night; the world scan behind it is real wall-clock time a
      // library composite pays on every checkpoint (farm.build: once per block), so skip it by day
      bedNear: night() && bedsNear().length > 0,
      days: a.days,
      elapsedDays: worldDay() - startedDay,
      count: a.count,
      done: extra.done ?? 0,
      // until= is how many minutes of real time this errand may take at most
      until: a.until === undefined ? undefined : startedAt / 60000 + a.until,
      now: Date.now() / 60000
    })
    if (reason) throw new HandBack(reason)
  }
  return {
    notes,
    report,
    api: {
      act,
      cleanupAct,
      until,
      checkpoint,
      performance: reportPerformance,
      scaffolds: scaffoldJournal(path.join(HOME, 'scaffolds.json')),
      forestry: scaffoldJournal(path.join(HOME, 'forestry.json')),
      ownSurvivalHole: () => {
        const file = path.join(HOME, 'events.jsonl')
        return fs.existsSync(file) ? latestOwnClosedDigHole(fs.readFileSync(file, 'utf8'), cfg.username) : null
      },
      acknowledgeFailure: name => { alive(); const had = failures.delete(name); return had },
      scaffoldOccupied: column => Object.values(bot.entities).some(e => e !== bot.entity && e.name !== 'item' && e.position && Math.abs(e.position.x - column.x - 0.5) < 0.8 && Math.abs(e.position.z - column.z - 0.5) < 0.8 && e.position.y >= column.y - 1 && e.position.y <= column.top + 2),
      navigationCapabilities: () => ({
        scaffolding: bot.pathfinder?.movements?.scaffoldingSupported === true,
        climbableVines: bot.pathfinder?.movements?.climbableVinesSupported === true
      }),
      recoverNavigationFailure,
      block: blockAt,
      grounded: () => Boolean(bot.entity?.onGround && !bot.vehicle),
      clock: () => ({ time: bot.time.timeOfDay, night: night(), day: !night(), raining: bot.isRaining, elapsedDays: worldDay() - startedDay }),
      inv: () => inventoryCounts(),
      hasSilkTouch: () => Boolean(silkTouchTool(bot.inventory.items())),
      // who this body is, for a composite that has to tell its own protected zones from somebody else's
      me: () => cfg.username,
      // is this block something you can stand on, and does a pen with animals in it surround me? (mine.get mends its own shaft)
      solid: name => bot.registry.blocksByName[name]?.boundingBox === 'block',
      // a floor to count over exists only once the walk came back enclosed: an open field has no census
      pen: () => {
        const found = penAround(bot.entity.position.floored())
        if (!found) return null
        return { ...found, census: found.enclosed ? censusOf(found.floor) : undefined }
      },
      pos: () => pos(),
      plan: planOf,
      // the shared map itself, for a composite that works over several places at once
      places: () => readPlaces(),
      zones: () => zones,
      // what lies on the ground, how full I am, and a pause between steps: the body's own senses, not actions
      drops: range => dropsNear(range),
      freeSlots: () => bot.inventory.emptySlotCount(),
      pause: async seconds => { await bot.waitForTicks(Math.max(1, Math.round((seconds ?? 0.5) * 20))) },
      note: line => { notes.push(String(line)) },
      // what the task reports even if a hand-back rule cuts it short
      report: partial => Object.assign(report, partial),
      // an event of the composite's own (routine_day, routine_stopped), and what it tells the stuck watch about itself
      emit,
      // routine bed=: the bed every composite of this task sleeps in at its night checkpoint
      nightBed: (bed, bedRange) => { if (ownerTask) ownerTask.nightBed = { bed, bedRange } },
      progress: data => {
        if (!ownerTask) return
        ownerTask.progress = { ...ownerTask.progress, ...data }
        if (ownerTask.jobId) jobEvent('job_progress', { progress: ownerTask.progress })
      }
    }
  }
}

export async function runComposite (name, mod, a, jobEvent) {
  const bad = checkArgs(name, mod.args, a)
  if (bad) throw new Error(bad)
  const alive = cancelGuard()
  const { api, notes, report } = makeApi(name, a, alive, jobEvent)
  const outcome = await mod.run(api, a).then(
    r => ({ stopped: 'done', ...r }),
    // whatever ends it early (stop, a death, a step that threw), the report built so far rides out on the error (src/composite.mjs)
    e => { if (e instanceof HandBack) return { stopped: e.reason }; throw carryReport(e, report, notes) })
  return compositeResult(report, outcome, notes)
}
