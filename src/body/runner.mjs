// The composite runner ("autopilot"): loads library/ composites, hands each the api it may act through, and stops it
// by the hand-back rules.
import fs from 'node:fs'
import path from 'node:path'
import { breedPlan } from '../villager/breed.mjs'
import { woodenGate } from '../enclosure/blocks.mjs'
import { boatHabitatPlan } from '../boat/habitat.mjs'
import { eatAllowed, BANNED_FOOD, workRefusal, parsePlan, planCells, planBill, isNight, mayDig, makeUntil, PAUSES, handBackReason, checkArgs } from '../lib.mjs'
import { carryReport, compositeResult } from '../composite.mjs'
import { ROOT, cfg } from './home.mjs'
import { readPlaces, emit, zones } from './events.mjs'
import { bot, carriedFood, task, Vec3, long, quick, refusalFor, useMoves, setStepsDone, stepsDone, explainFailure, ready, flee, holingUp, fighting, ROLLBACK_PLACE, penAround, censusOf, pos, cancelGuard } from '../bot.mjs'
import { bedsNear, inventoryCounts, dropsNear } from './helpers.mjs'

// ---------------------------------------------------------------- the composite runner ("autopilot")
// src/bot.mjs holds primitives; a composite is one file in library/, `export default { doc, args, run }`. The runner loads
// them at body start and registers each in `long`, so to a driver a composite is an ordinary action: a new one cancels
// the old, `state` shows it as doing=, and one that outlasts timeout= reports through task_done like anything else.
// The hand-back rules (handBackReason in lib.mjs) belong to the RUNNER: a composite cannot opt out of being stopped
// when someone speaks to me, when I am hurt or starving, or when the same step fails twice.
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
export const CLI_ONLY = ['wait', 'dawn', 'clock']
// Preserve the runner export used by bot.mjs; the policy itself lives with food logic.
export { BANNED_FOOD }
// the last thing a person said TO me: a whisper always counts, a chat only when it says my name
let lastSpoken = null
export const setLastSpoken = v => { lastSpoken = v }

class HandBack extends Error {
  constructor (reason) { super(reason); this.reason = reason }
}

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
  if (!place.plan) throw new Error(`${name} is on the map but has no plan: save one with ./mc plan name=${name} map='...'`)
  const parsed = parsePlan(place.plan)
  return { ...place, parsed, cells: planCells(place), bill: planBill(parsed) }
}

// everything a composite may do to the world, and the only way it may do it
function makeApi (composite, a, alive) {
  const ownerTask = task
  let cleanupServiceDirection = null
  const notes = []
  const report = {}
  const failures = new Map()
  const startedDay = worldDay()
  const startedAt = Date.now()
  let sleptTonight = false
  const night = () => isNight(bot.time.timeOfDay)
  const blockAt = (x, y, z) => {
    const b = bot.blockAt(new Vec3(Math.floor(x), Math.floor(y), Math.floor(z)))
    return b ? { name: b.name, properties: b.getProperties?.() ?? {}, solid: b.boundingBox === 'block' } : null
  }
  const failedTwice = () => [...failures.values()].find(f => f.count >= 2)?.why
  const noteFailure = (name, why) => {
    const seen = failures.get(name)
    failures.set(name, { why, count: seen?.why === why ? seen.count + 1 : 1 })
  }
  const act = async (name, args = {}) => {
    alive()
    const fn = long[name] ?? quick[name]
    if (!fn) throw new Error(`${composite}: no action called ${name}`)
    const refusal = refusalFor(name, args)
    if (refusal) throw new Error(`${composite}/${name}: ${refusal}`)
    useMoves(mayDig(name, args))
    return Promise.resolve().then(() => fn(args)).then(
      r => { failures.delete(name); setStepsDone(stepsDone + 1); return r ?? {} },
      e => {
        const why = `${name}: ${explainFailure(e.message)}`
        noteFailure(name, why)
        throw new Error(`${composite}/${why}`)
      })
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
  const until = makeUntil({ waitTicks: n => bot.waitForTicks(n), alive, composite })
  // between steps: night with a bed is slept through and the composite never sees it; anything else that needs a person stops the task
  const checkpoint = async (extra = {}) => {
    alive()
    if (!night()) sleptTonight = false
    if (night() && !sleptTonight && bedsNear().length) {
      sleptTonight = true
      // said out loud: an agent that saw `asleep doing=mine.get 174s` with nothing moving stopped it as wedged (Chani, twice)
      const mine = task
      if (mine) mine.paused = 'night'
      emit('task_paused', { id: mine?.id, name: composite, why: PAUSES.night })
      try {
        await long.sleep({}).catch(() => {})
        await until(() => !bot.isSleeping, { timeout: 900, every: 5, what: 'the night never ended' }).catch(() => {})
      } finally {
        if (mine) delete mine.paused
      }
      alive()
      emit('task_resumed', { id: mine?.id, name: composite })
    }
    const reason = handBackReason({
      spoken: lastSpoken && lastSpoken.at > startedAt ? `${lastSpoken.from}: ${lastSpoken.message}`.slice(0, 90) : null,
      health: bot.health,
      food: bot.food,
      edible: edibleCarried(),
      failedTwice: failedTwice(),
      invFull: bot.inventory.emptySlotCount() === 0,
      canDeposit: Boolean(extra.canDeposit),
      night: night(),
      bedNear: bedsNear().length > 0,
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
      block: blockAt,
      clock: () => ({ time: bot.time.timeOfDay, night: night(), day: !night(), raining: bot.isRaining, elapsedDays: worldDay() - startedDay }),
      inv: () => inventoryCounts(),
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
      progress: data => { if (ownerTask) ownerTask.progress = { ...ownerTask.progress, ...data } }
    }
  }
}

export async function runComposite (name, mod, a) {
  const bad = checkArgs(name, mod.args, a)
  if (bad) throw new Error(bad)
  const alive = cancelGuard()
  const { api, notes, report } = makeApi(name, a, alive)
  const outcome = await mod.run(api, a).then(
    r => ({ stopped: 'done', ...r }),
    // whatever ends it early (stop, a death, a step that threw), the report built so far rides out on the error (src/composite.mjs)
    e => { if (e instanceof HandBack) return { stopped: e.reason }; throw carryReport(e, report, notes) })
  return compositeResult(report, outcome, notes)
}
