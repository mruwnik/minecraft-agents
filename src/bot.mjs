// Claude's Minecraft body.
// Fast reflexes (eating, armour, self-defence) live here; decisions arrive over a
// small localhost HTTP API (see README.md) and everything notable that happens is
// appended to events.jsonl so the planning side can follow along.
import { automaticBeds, carriedBedSpot, reflexPickups } from './lib/sleep.mjs'
import { stalkShape } from './navigation/bamboo.mjs'
import { settleInventory } from './body/inventory-settle.mjs'
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { pathToFileURL } from 'node:url'
import mineflayer from 'mineflayer'
import { installWorldClock } from './world-clock.mjs'
import { installTickEnd } from './tick-end.mjs'
import collectBlock from 'mineflayer-collectblock'
import pvp from 'mineflayer-pvp'
import armorManagerMod from 'mineflayer-armor-manager'
import { loader as autoEat } from 'mineflayer-auto-eat'
import AABB from 'prismarine-physics/lib/aabb.js'
import { restartAdvice } from './restart.mjs'
import { RENAMED, PRIMITIVES, compositeError, gateChange, fencePush, wedgeReplant, staleKey, bedExit, eatJammed, eatFailure, eatRefusal, eatAllowed, eatHold, eatBackoff, mealToDrop, mealFailed, foodSort, noFoodEdge, eatRetryDue, afterTheMeal, deathBy, deathReport, deathUnannounced, deathKit, stackTop, crowdSize, didYouMean, eatBelow, wedgeBreakable, wakeStep, bedtimeReport, droppedWalk, hurtCause, mealTally, FLUIDS, scaffoldNote, scaffoldTakeBack, scaffoldBuilt, isAir, bedChoice, ownBed, automaticNightPlan, bedTrap, idleNudge, deadWalk, oversleeping, staleCode, codeVersion, clampedOffset, nudgeAway, flushCells, airReflex, trackReads, ignoredParams, peacefulTool, chaseBroken, fleeGoal, digFromHere, chargeLeash, breakOffDigs, CHASE_LEASH, fleeUnwinnable, fleeOscillating, fleeRange, fleeIntoCave, holeCells, holeUpVerdict, respawnPlan, FLEE_HOME, FLEE_GIVEUP_MS, NEVER_FIGHT, ENDERMAN_RANGE, brokenSlot, shouldFlee, ARCHERS, rangedThreat, stepOffChoice, bedtime, feetCell, overMemory, arrivalError, inAnyZone, isWedged, within, refuseReason, ignorableMob, explainInterrupt, isStalled, mayDig, explainNoPath, boxedIn, doorwayNode, buriedIn, isNight, loginYield, reconnectDelay, offlineError } from './lib.mjs'
import { makeEyes } from './vision/eyes.mjs'
import { watchWindows } from './body/window-watch.mjs'
import { burrowSite, capChoice, holeUpAborted, mobHit, holeUpBlock, refusalNote, shelterNote, HOLE_STEP, HOLE_DEPTH, HOLE_MELEE } from './survival/holeup.mjs'
import { underRoof, walledIn, nightShelter, nightFleeStep, nightFleeGoal, retarget, fightNotFlee, attackerCount, plugCells, holdNote } from './survival/night.mjs'
import { chatRefusal } from './talk.mjs'
import { thinkBudget, goalDistance, THINK_CAP_MS } from './navigation/walk.mjs'
import { makeSurfaceWalkRuntime } from './navigation/surface-walk.mjs'
import { walkStandstill, walkProgress, WALK_PROGRESS_MS, blockName, frozenWalk, facingOff, aheadCells, serverSide, nearBy, frozenAdvice } from './navigation/stall.mjs'
import { addSample, stuckVerdict, nextEpisode, stuckLine } from './navigation/stuck.mjs'
import { neededArgs } from './needs.mjs'
import { createJobShelf } from './job-shelf.mjs'
import { createJobScheduler } from './job-scheduler.mjs'
import { mayRunBesideOwner } from './job-policy.mjs'
import { airSample, freshAir } from './survival/airlog.mjs'
import { surfaceWay, openingProgress, roofAt, SURFACE_SCAN } from './navigation/surface.mjs'
import { noPathAdvice, inHole, perchedOverField } from './navigation/cave-exit.mjs'
import { legFlags, stepsOff, noFirstMove, clearGoalOnFailure } from './lib/path.mjs'
import { failedResult, deathLine, deathCancel } from './composite.mjs'
import { makeBoatRuntime } from './body/boat.mjs'
import { makeBoatTravelRuntime } from './body/boat-travel.mjs'
import { driveBoat } from './navigation/boat-travel.mjs'
import { makeTravelRuntime } from './body/travel.mjs'
import { makeRidingRuntime } from './body/riding.mjs'
import { driveHorse } from './navigation/horse.mjs'
import { makeVillagerRuntime } from './body/villager.mjs'
import { makeVillagerRosterObserver, saveVillagerObservation } from './villager/roster.mjs'
import { ROOT, HOME, cfg } from './body/home.mjs'
import { authDir, profileFile, loginAdvice } from './auth.mjs'
import { zones, GATES_FILE, readPlaces, savePlaces, emit, sayOnce, sayError } from './body/events.mjs'
import { LIBRARY_DIR, libraryFiles, compositeName, composites, BANNED_FOOD, edibleCarried, runComposite } from './body/runner.mjs'
import { carried, inventoryCounts, diffCounts, findItem, vecOf, cellAt, goNear, findBlocksNear, bedsNear } from './body/helpers.mjs'
import { pathfinder, goals, Vec3, reportPerformance, bot, setBot, mcData, setMcData, ready, setReady, task, setTask, gen, setGen, cancelGuard, pos } from './body/state.mjs'
import { isWoodDoor, doorsIOpened, heldOpen, myClicks, MY_CLICK_MS, othersToggled, doorAt, doorBusy, gatesPassed, doorTick, shutGatesBehind, shutTrackedGatesAfterCancel, fenceEscape, leaveFenceCell } from './body/doors.mjs'
import { straysAt } from './body/pens.mjs'
import { watchesQuick } from './body/watches.mjs'
import { long, quick } from './body/actions/tables.mjs'
import { senseLong, senseQuick } from './body/actions/sense.mjs'
import { mapQuick } from './body/actions/map.mjs'
import { digging, makeMoves, useMoves, moveLong, moveQuick } from './body/actions/move.mjs'
import { handPlacing, blockLong } from './body/actions/block.mjs'
import { compactingInventory, itemLong, itemQuick } from './body/actions/item.mjs'
import { feeding, setFeeding, creatureLong, creatureQuick } from './body/actions/creature.mjs'
import { selfLong, selfQuick } from './body/actions/self.mjs'
import { controlLong, controlQuick } from './body/actions/control.mjs'

// the physics engine's own box comparison lets a hitbox that rounds 1e-14 past a block face walk into the block (see clampedOffset in lib.mjs)
const corners = box => ({ min: [box.minX, box.minY, box.minZ], max: [box.maxX, box.maxY, box.maxZ] })
for (const [axis, method] of ['computeOffsetX', 'computeOffsetY', 'computeOffsetZ'].entries()) {
  AABB.prototype[method] = function (other, offset) { return clampedOffset(corners(this), corners(other), axis, offset) }
}
const armorManager = armorManagerMod.default ?? armorManagerMod
export let boatLeashHolder = new Map()
let eatTimer = null
// item 13 (#109): what a death line needs and cannot work out after the fact. The server's own words, the last wound,
// and the last place the body stood: the respawn point is the world spawn, which tells nobody where the kit fell.
let saidDeath = null
let lastWound = null
let lastStood = null
let lastCarried = null
// the last death: when, where the body fell and what fell with it, for the result of the task it ended (src/composite.mjs)
let lastDeath = null
let diedAt = 0
export let reflexes = true
export const setReflexes = v => { reflexes = v }
export let eyes = null
export let openWindow = () => null
// where containerAt, craftBatch and the furnace actions say they are about to open a window, so the watcher reports
// the block they actually opened rather than a nearby-block guess (helpers.mjs imports this to call it)
export let declareOpening = () => {}
export let followTarget = null
export const setFollowTarget = v => { followTarget = v }
let waitingForServer = false
let yieldUntil = 0 // while someone else is logged in as me, I stay off until then
let reconnectTimer = null
let restoreTimer = null // retries tryAutoRestore every 30s while a reconnect/restart hold is pending and a monster is near
// when the current "eating" began, for the jam backstop: module-level so the health handler can end a meal that was hit (#149c)
let eatingSince = null

// SAFETY (bodies never eat). mineflayer-auto-eat's own reflex is `statusCheck`, and it ends in `catch {}`: every failure
// for days was invisible, and across every body's bot.log there are 140 jam lines and not one word of why. This is the
// same rule -- eat below minHunger, or below minHealth however full I am -- with the failure said out loud as eat_failed.
// Rate-limited like any other error (errorRepeat), so a reflex that fires every physics tick cannot wall the events file.
export const carriedFood = () => foodSort(bot.inventory.items().map(i => i.name), name => Boolean(bot.registry.foodsByName?.[name]), BANNED_FOOD)
const eatFailed = error => sayError(eatFailure(error, carriedFood().edible), { food: bot?.food, health: Math.round(bot?.health ?? 0) }, 'eat_failed')
let eatFailedAt = null
// eat() marks itself eating BEFORE it equips the food, and only clears that inside its own try/finally: an equip that
// throws never reaches the finally, so the plugin stays "eating" for ever and the reflex never fires again. Whatever
// happens here, the next tick may try. (The 15 s jam timer below is now only a backstop.)
export const eatOnce = async opts => {
  try {
    return await bot.autoEat.eat(opts)
  } finally {
    bot.autoEat._eating = false
  }
}
// #149(a): how near a hostile has to be for the hand to belong to the sword rather than to a loaf
const EAT_SAFE_RANGE = 6
let eatFails = 0
// backlog: a body with empty pockets re-said "nothing I carry is food" on every backoff tick for as long as they
// stayed empty, walling the events file the same way the old jam loop did. noFoodEdge fires once, on the tick the
// pockets are first found bare, and stays quiet until something edible or banned is carried again (see noFoodEdge).
let hadNoFood = false
const eatTick = async () => {
  if (!ready || !bot?.autoEat || bot.autoEat.isEating || compactingInventory) return
  if (bot.food >= bot.autoEat.opts.minHunger && bot.health >= bot.autoEat.opts.minHealth) return
  // #149: a hurt body is usually a body in a fight, and minHunger rises as health drops, so this reflex used to walk
  // into the melee with bread in its hand. Mariel died that way at food 17 with a sword in her pack. Silently: 42 eat
  // lines in the minute it took to kill her were the log's whole account of the fight
  if (eatHold({ hostileNear: nearbyHostiles(EAT_SAFE_RANGE).length > 0, fighting: Boolean(fighting), fleeing: Boolean(flee) })) return
  // hurt but full: minHealth sends me to eat however full I am, and the game refuses a meal at food 20. Trying anyway
  // is 20 meals a second that can only time out -- most of the 140 jam lines in the logs are this.
  if (bot.food >= 20) return
  // an empty-handed body asked the plugin 20 times a second and got its own wording back. Ask ourselves first, and say
  // which of the things I DO carry were passed over and why (backlog #134)
  const carried = carriedFood()
  const { fire, hasNone } = noFoodEdge(carried, hadNoFood)
  hadNoFood = hasNone
  if (hasNone) {
    if (fire) sayError(eatRefusal({ food: bot.food, carried }), { food: bot.food, health: Math.round(bot.health) }, 'eat_failed')
    return
  }
  // #149(b): each failure in a row waits twice as long, to half a minute; one meal that works clears the count
  if (!eatRetryDue(eatFailedAt, Date.now(), eatBackoff(eatFails))) return
  const refusal = eatRefusal({ food: bot.food, carried })
  if (refusal) { eatFailedAt = Date.now(); eatFails++; return sayError(refusal, { food: bot.food, health: Math.round(bot.health) }, 'eat_failed') }
  // the plugin keeps a banned list of its own, so a meal only the hunger floor allows is eaten only when the item is
  // put in its hand by name. Below the floor with nothing better carried, the flesh IS the food (backlog #139).
  const { desperate, allowed } = eatAllowed({ food: bot.food, carried })
  const last = desperate ? bot.inventory.items().find(i => allowed.includes(i.name)) : null
  // a meal that worked clears the cooldown: a body at food 4 eats its carrots one after another, it does not wait 5 s
  // between them because the attempt before the first one failed
  // strictErrors is off, so a meal that fails still resolves: the eatFail listener counts it (mealFailed), and only a
  // meal that raised no failure on the way clears the backoff
  const failsBefore = eatFails
  await eatOnce(last ? { food: last } : {}).then(() => { if (eatFails === failsBefore) { eatFailedAt = null; eatFails = 0 } }, error => { eatFailedAt = Date.now(); eatFails++; eatFailed(error) })
}
// #149(c): a meal in flight holds every equip until it ends (afterTheMeal), the sword's included. Cancelling it rejects
// the listener, so the next equip goes through at once. The plugin's own "Eating manually canceled!" is not a failure
function dropMeal (why) {
  if (!bot?.autoEat?.isEating) return false
  bot.autoEat.cancelEat()
  bot.autoEat._eating = false
  eatingSince = null
  console.log(`[auto-eat] dropped a meal: ${why}`)
  return true
}
// ROOT CAUSE of "the body starves with bread in its pockets". The plugin calls a meal finished when the server sends
// entity_status 9 for my own entity, and this server never does: ClaudeProbe ate its way from food 15 to food 20 while
// the plugin logged "Eating timed out with a time of 3000 milliseconds!" for the very same meal. Every meal therefore
// "failed" -- the error vanished into statusCheck's `catch {}`, the hand was held for 3 s a time and handed back to
// whatever it held before, and the reflex started again on the next tick, for ever. So watch what DOES arrive: the food
// number going up, or the food leaving my pockets. It reads the module-level `bot`, so a reconnect needs no new one.
let mealInFlight = null
// every meal this body has finished, by item. A chest transfer reads it before and after: a hungry body eats the bread
// as it arrives, and without this the withdraw could only say the inventory was short, not WHERE it went (#146)
export const mealsEaten = {}
const ateOne = name => { mealsEaten[name] = (mealsEaten[name] ?? 0) + 1 }
const watchTheMeal = (food, timeoutMs) => {
  const meal = new Promise((resolve, reject) => {
    const carried = () => bot.inventory.items().filter(i => i.name === food.name).reduce((n, i) => n + i.count, 0)
    const before = { food: bot.food, carried: carried() }
    const stop = () => { clearTimeout(timer); mealInFlight = null; bot.off('health', fed); bot.off('physicsTick', gone) }
    const fed = () => { if (bot.food > before.food) { stop(); ateOne(food.name); resolve() } }
    const gone = () => { if (carried() < before.carried) { stop(); ateOne(food.name); resolve() } }
    const timer = setTimeout(() => {
      stop()
      reject(new Error(`the meal never showed: food is still ${bot.food} and I still carry ${carried()} ${food.name} ${timeoutMs} ms on`))
    }, timeoutMs)
    bot.on('health', fed)
    bot.on('physicsTick', gone)
    bot.autoEat._rejectionBinding = error => { stop(); reject(error) } // cancelEat() still works
  })
  // the plugin has already put the food in my hand by the time it calls this (customEquip runs first), so from here to
  // the last bite is the window afterTheMeal holds the pathfinder out of
  mealInFlight = meal
  return meal
}

// Two ways of getting about: walking only (the default: digging walks tunnelled through hills and left pillars), and
// digging + scaffolding for `mine` and for walks that ask with dig=true. Both cross planted cells only where there is
// no other way, at a walking pace (src/lib/path.mjs farmWalk and legFlags, card fcd996fe)
export let walkMoves = null
export let digMoves = null
// every cell the pathfinder aimed a scaffolding placement at during this task. Chani's cobblestone went that way twice with
// nothing in the reply to say so (#111), so a task now reports what it built beside its drops and takes back what it can reach
let scaffolded = []

// a device code asked for at runtime means the cached refresh token is gone (months of disuse): a background body
// cannot show it to anyone, and the reconnect loop would ask for a new one every ten seconds, so say so and stop.
// exit 6: tools/start-body writes the body_down line for any non-zero exit
const loginNeeded = () => {
  emit('login_needed', { advice: loginAdvice(HOME) })
  process.exit(6)
}

function connect () {
  setReady(false)
  yieldUntil = 0
  setBot(mineflayer.createBot({
    host: cfg.host, port: cfg.port, username: cfg.username, version: cfg.version, auth: cfg.auth,
    ...(cfg.auth === 'microsoft' && { profilesFolder: authDir(HOME), onMsaCode: loginNeeded })
  }))
  installWorldClock(bot)
  installTickEnd(bot)
  villagerRoster.attach(bot)
  bot.loadPlugin(pathfinder)
  bot.loadPlugin(collectBlock.plugin)
  bot.loadPlugin(pvp.plugin)
  bot.loadPlugin(armorManager)
  bot.loadPlugin(autoEat)
  boatLeashHolder = new Map()
  boatRuntime.attach()
  // 9 physicsTick listeners stand by design and a walk or a wait adds two for a moment: the warning at 11 was noise, not a leak ([listeners] stayed at 9 for hours on every body)
  bot.setMaxListeners(30)

  // minecraft-data's one fixed bamboo shape misses the server's per-block offset (navigation/bamboo.mjs); the blocks
  // plugin's bot.blockAt exists by login, before spawn, so wrapping it there covers every respawn and dimension change
  bot.once('login', () => {
    const blockAt = bot.blockAt.bind(bot)
    bot.blockAt = (point, extraInfos) => {
      const block = blockAt(point, extraInfos)
      if (block?.name === 'bamboo' && block.position) block.shapes = [stalkShape(block.position.x, block.position.z)]
      return block
    }
  })

  bot.once('spawn', () => {
    setMcData(bot.registry)
    eyes = makeEyes(bot, { textureDir: path.join(ROOT, 'textures'), snapshotDir: path.join(HOME, 'snapshots') })
    const windows = watchWindows(bot, { now: Date.now })
    openWindow = windows.open
    declareOpening = windows.opening
    walkMoves = makeMoves(false)
    digMoves = makeMoves(true)
    // The pathfinder towers and bridges with the blocks I carry and says nothing about it (Chani's cobblestone, twice).
    // It aims at one cell several times a tick and its own place call rejects over blocks the server did put down, so the
    // clicks are only candidates: what it actually built is read off the world when the task ends (scaffoldBuilt).
    // Anything the `place` primitive puts down on purpose is not scaffolding: handPlacing tells the two apart.
    const placeBlock = bot.placeBlock.bind(bot)
    bot.placeBlock = (ref, face) => {
      const held = bot.heldItem?.name
      const cell = !handPlacing && ref?.position ? new Vec3(ref.position.x + face.x, ref.position.y + face.y, ref.position.z + face.z) : null
      // an aim at a cell that already holds that block is a click that changed nothing: only air the walk filled counts
      const wasOpen = cell ? isAir(bot.blockAt(cell)?.name ?? 'air') : false
      const done = placeBlock(ref, face)
      // the click says nothing: it rejects over blocks the server did put down, and the cell is still air when it settles.
      // Look a moment later instead, and count a cell once however many times the walk aimed at it
      if (cell && held && wasOpen) {
        done.catch(() => {}).finally(() => setTimeout(() => {
          if (bot.blockAt(cell)?.name !== held) return
          if (scaffolded.some(c => c.x === cell.x && c.y === cell.y && c.z === cell.z)) return
          scaffolded.push({ x: cell.x, y: cell.y, z: cell.z, name: held })
        }, 250))
      }
      return done
    }
    // `mine` runs through collectBlock, which otherwise installs stock movements of its own (no zones, doors are walls) and leaves them on
    bot.collectBlock.movements = digMoves
    // mineflayer-pvp also brings its own default Movements (digging, towers, no zones) and installs them on every attack: a fight then
    // tunnelled through builds, and a fight with a mob it could not reach grew a 3D dig search until the body died at 4 GB
    bot.pvp.movements = walkMoves
    // mineflayer-tool's getFromChest calls itself for ever when no chest holds a tool (we never register chests): the heap fills and the
    // body dies without a trace. Without it the plugin throws a plain NoItem instead
    const equipForBlock = bot.tool.equipForBlock.bind(bot.tool)
    // ...and it takes whatever digs fastest, which for melons, pumpkins and leaves is the sword: it wore out there and left the body unarmed
    bot.tool.equipForBlock = async (block, options = {}, cb) => {
      const tool = peacefulTool([...bot.inventory.items(), null].map(item => ({ name: item?.name ?? null, item, time: bot.tool.getDigTime(block, item ?? undefined), harvests: block.canHarvest(item?.type ?? null) })))
      if (!tool) return equipForBlock(block, { ...options, getFromChest: false }, cb)
      if (tool.item) await bot.equip(tool.item, 'hand')
      else if (bot.heldItem) await bot.unequip('hand')
      cb?.()
    }
    useMoves(false)
    // unbounded by default: a goal that cannot be reached (no scaffold blocks left in a shaft) kept one search growing until the body died at
    // 4 GB. This allows a detour of 160 cost units beyond the straight line, then gives up with noPath.
    bot.pathfinder.searchRadius = 160
    // the plugin's goto resolves when the search returns an empty path (nothing walkable from here, as in a shaft): check the goal ourselves
    // and a walk that fails takes its goal with it: the plugin's goto keeps it, and the body walked on with no task (clearGoalOnFailure)
    const plainWalk = clearGoalOnFailure(bot.pathfinder, bot.pathfinder.goto.bind(bot.pathfinder))
    // a near goal that 1.5 s of search has not found is walled in: say so then, not after the plugin's 5 s (card 1ccb0ea1). Every search of
    // this walk, replans included, reads thinkTimeout; reflex walks (setGoal) get the default back once the walk is over
    const walk = goal => {
      lastWalkGoal = goal
      const leg = { goal }
      activeWalk = leg
      bot.pathfinder.thinkTimeout = thinkBudget(goalDistance(goal, bot.entity.position))
      return plainWalk(goal).finally(() => {
        if (activeWalk === leg) { activeWalk = null; nearest = null }
        bot.pathfinder.thinkTimeout = THINK_CAP_MS; sprintFor([])
      })
    }
    const arrived = goal => {
      const feet = feetCell(bot.entity.position, bot.entity.onGround)
      return goal.isEnd(bot.entity.position.floored()) || goal.isEnd(new Vec3(feet.x, feet.y, feet.z))
    }
    // see stepOffChoice: only when I stand in a block that is not a full one high (a bed, a slab), never on worked ground (stepsOff:
    // from farmland the free floor beside the feet was a hole in the field, and the body stood in it five minutes, card 94e6dcb1)
    const stepOff = async () => {
      const here = bot.entity.position.floored()
      if (bot.blockAt(here)?.boundingBox !== 'block' || !stepsOff(bot.blockAt(here).name)) return false
      const sides = [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => here.offset(dx, 0, dz))
      const box = p => bot.blockAt(p)?.boundingBox
      // low: not a full block high, like the one I stand in (the other half of my bed, the next slab)
      const low = p => (bot.blockAt(p)?.shapes ?? []).length > 0 && Math.max(...bot.blockAt(p).shapes.map(shape => shape[4])) < 1
      const side = sides[stepOffChoice(sides.map(p => ({ feet: box(p), head: box(p.offset(0, 1, 0)), ground: box(p.offset(0, -1, 0)), low: low(p), door: isWoodDoor(bot.blockAt(p) ?? {}) })))]
      if (!side) return false
      const exit = bot.blockAt(side)
      if (isWoodDoor(exit) && !exit.getProperties().open) { await bot.activateBlock(exit).catch(() => {}); doorsIOpened.add(String(exit.position)); await bot.waitForTicks(3) }
      // told once per half minute: a walk off a bed takes three tries and used to write three events (24 in Aviendha's log)
      if (Date.now() - lastSteppedOff > 30000) emit('stepped_off', { from: bot.blockAt(here).name, to: `${side.x},${side.y},${side.z}` })
      lastSteppedOff = Date.now()
      await bot.lookAt(side.offset(0.5, 1.6, 0.5), true)
      bot.setControlState('forward', true)
      await bot.waitForTicks(8)
      bot.setControlState('forward', false)
      await bot.waitForTicks(4)
      return true
    }
    bot.pathfinder.goto = async goal => {
      // standing on a chest or a bed the first search may also THROW noPath, not just come back empty: the step off is tried for both
      // the watchdog ended a walk that had no path (see deadWalk): "goal was changed" would send the driver looking for a reflex
      const failure = await walk(goal).then(() => null, e => Date.now() - walkEndedAt < 2000 ? new Error(walkEndedBy) : e)
      if (!failure && arrived(goal)) return
      // up to 3 steps: along a bed in a 1-wide room the first one only reaches the bed's other half
      let stepped = false
      for (let steps = 0; steps < 3 && await stepOff(); steps++) stepped = true
      const here = bot.entity.position.floored()
      const trap = failure && bedTrap(bot.blockAt(here)?.name, bot.blockAt(here.offset(0, 2, 0))?.boundingBox)
      if (trap) throw new Error(trap)
      if (stepped) await walk(goal)
      else if (failure) throw noPathCounted(failure)
      const error = arrivalError(arrived(goal))
      if (error) throw noPathCounted(new Error(error))
    }
    // path_update hands out the live path before the pathfinder starts walking it: fix doorway waypoints in place (see doorwayNode)
    bot.on('path_update', r => r.path.forEach(n => Object.assign(n, doorwayNode(n, doorAt(n)))))
    bot.on('goal_updated', goal => {
      goalSetAt = Date.now()
      // A fresh walk starts a new clock; setGoal(theSameGoal) is the watchdog's
      // one retry, so it must keep the original episode and eventual timeout.
      if (!goal || stillFrom?.goal !== goal) stillFrom = null
    })
    bot.on('path_reset', reason => { pathResets.push({ reason, at: Date.now() }); if (pathResets.length > 200) pathResets.shift() })
    bot.on('path_update', r => { livePath = r.path })
    // a leg with a crop or farmland node in it walks without sprinting: the executor sprint-jumps a straight line it cannot
    // walk in one go, and a landing is what tramples farmland (card fcd996fe). Read per tick off the movements in use
    const sprintFor = leg => { for (const moves of [walkMoves, digMoves]) moves.allowSprinting = legFlags(leg, (x, y, z) => bot.blockAt(new Vec3(x, y, z), false)?.name).allowSprinting }
    bot.on('path_update', r => sprintFor(r.path))
    bot.on('goal_reached', () => { stillFrom = null; sprintFor([]) })
    // registered after the pathfinder's own tick, so a key pressed here survives until the next physics step (see idleNudge)
    // ...and released here, before it: what is pressed when my turn comes is then the pathfinder's own choice for this tick
    bot.prependListener('physicsTick', () => {
      if (!nudging) return
      bot.setControlState('forward', false)
      bot.setControlState('jump', false)
    })
    bot.on('physicsTick', () => {
      idleTicks = keysDown().length || !bot.pathfinder.goal ? 0 : idleTicks + 1 // only time spent idle on a walk counts
      const busy = doorBusy || Boolean(bot.targetDigBlock) || bot.pathfinder.isMining() || bot.pathfinder.isBuilding() || !bot.entity.onGround
      const nudge = idleNudge({ hasGoal: Boolean(task && bot.pathfinder.goal), busy, idleTicks, node: livePath[0], pos: bot.entity.position, collided: bot.entity.isCollidedHorizontally })
      if (nudge && !nudging) console.log('[idle nudge]', JSON.stringify(stallEvidence()))
      // slid along a fence into ITS cell in mid-walk (my sleep walk, 21:05Z): the next node lies beyond the fence, and nudging towards it presses me into the
      // post. Back into the cell I really stand in, then plan again from there (setMovements replans without failing the walk; setGoal would)
      const { target: out, replan } = fencePush(bot.pathfinder.goal ? fencePressed : null, nudge ? fenceEscape() : null, Boolean(nudge), bot.entity.position)
      fencePressed = out
      nudging = Boolean(nudge || out) // my own key presses are taken back at the start of the next tick
      if (replan) bot.pathfinder.setMovements(bot.pathfinder.movements)
      if (!nudge && !out) return
      const push = out ? { dx: out.x + 0.5 - bot.entity.position.x, dz: out.z + 0.5 - bot.entity.position.z, jump: false } : nudge
      bot.look(Math.atan2(-push.dx, -push.dz), 0)
      bot.setControlState('forward', true)
      bot.setControlState('jump', push.jump)
    })
    const reportedSlowPaths = new WeakSet()
    bot.on('path_update', r => {
      lastPath = { status: r.status, at: Date.now(), nodes: r.path.slice(0, 4).map(n => `${n.x},${n.y},${n.z}`), visited: r.visitedNodes, searchMs: Math.round(r.time) }
      if (r.time > 1000 && (!r.context || !reportedSlowPaths.has(r.context))) {
        if (r.context) reportedSlowPaths.add(r.context)
        reportPerformance('pathfinding', r.time, { status: r.status, visited: r.visitedNodes, generated: r.generatedNodes, pathNodes: r.path.length, at: pos(), task: task?.name })
      }
    })
    // The server silently resets us every tick if our hitbox touches a block exactly, so keep a hair's gap.
    bot.physics.playerHalfWidth = cfg.halfWidth ?? 0.3001
    // remember my last few outgoing positions: printed with each server reset, they show what the server refused
    if (!bot._client.remembersClaims) {
      bot._client.remembersClaims = true
      const write = bot._client.write.bind(bot._client)
      bot._client.write = (name, params) => {
        if (params && 'x' in params && params.flags) claimed = [...claimed.slice(-3), `${params.x},${params.y},${params.z} ground=${params.flags.onGround}`]
        return write(name, params)
      }
    }
    // strictErrors:false so a meal that goes wrong emits eatFail with the real error instead of throwing it into the
    // plugin's own `catch {}` (see eatTick): with the default every failure was invisible.
    bot.autoEat.setOpts({ priority: 'foodPoints', minHunger: 15, bannedFood: BANNED_FOOD, strictErrors: false })
    bot.autoEat.statusCheck = eatTick // the plugin's own reflex swallows every error; ours says them. Set BEFORE enableAuto: that registers it
    bot.autoEat.buildEatingListener = watchTheMeal // the signal it waits for never comes on this server: see watchTheMeal
    // and a meal that is interrupted is no meal: the pathfinder puts a tool in my hand before every dig and works every
    // gate by hand, once per tick while it is stuck, which is how Chani's body stood at health 7 with five carrots in
    // its pockets. Wrapping the bot's own two calls catches the plugin's as well, and the plugin's own equip runs
    // before any of this is armed. See afterTheMeal.
    if (!bot.mealComesFirst) {
      bot.mealComesFirst = true
      bot.equip = afterTheMeal(() => mealInFlight, bot.equip.bind(bot))
      bot.activateBlock = afterTheMeal(() => mealInFlight, bot.activateBlock.bind(bot))
    }
    // every click of my own hand, whoever made it (a walk, doorTick, the gate check at a task's end, the pathfinder):
    // the gates.log listener asks this whether a gate that just moved was my doing
    if (!bot.clicksRecorded) {
      bot.clicksRecorded = true
      const click = bot.activateBlock
      bot.activateBlock = (block, ...rest) => { if (block?.position) myClicks.set(String(block.position), Date.now()); return click(block, ...rest) }
    }
    bot.autoEat.enableAuto()
    bot.autoEat.on('eatFail', error => {
      if (!mealFailed(error)) return
      eatFailedAt = Date.now()
      eatFails++
      eatFailed(error)
    })
    // see eatJammed: the plugin can stay "eating" for ever. ONE timer for the process: started at every spawn and never
    // stopped, the old ones outlive their connection and read the module-level `bot`, which by then is a new one whose
    // plugins are not loaded yet -- that is the `uncaught: ... (reading 'isEating')` every few seconds that filled
    // Perrin's and Mariel's events files after the 09-22 20:53 restart. Cleared here, and guarded for the same reason.
    clearInterval(eatTimer)
    eatingSince = null
    eatTimer = setInterval(() => {
      if (!bot?.autoEat || !ready) { eatingSince = null; return }
      if (!bot.autoEat.isEating) { eatingSince = null; return }
      eatingSince ??= Date.now()
      if (!eatJammed(Date.now() - eatingSince)) return
      console.log(`[auto-eat] "eating" for ${Math.round((Date.now() - eatingSince) / 1000)} s at food=${bot.food}: jammed, reset`)
      bot.autoEat._eating = false
      eatingSince = null
    }, 5000)
    setReady(true)
    waitingForServer = false
    scheduler?.pump()
    emit('spawned', { pos: pos(), dimension: bot.game.dimension, ...codeHere })
    // give the spawn chunks a beat to load before trusting a hostile scan (mirrors the respawn plan's own wait)
    if (jobShelf.snapshot().held?.blockUrgent) bot.waitForTicks(20).then(tryAutoRestore, () => {})
  })

  // incoming conversation is recorded for the active job; it never implicitly cancels that job
  bot.on('chat', (username, message) => {
    if (username === bot.username) return
    lastDriven = Date.now()
    emit('chat', { from: username, message })
  })
  bot.on('whisper', (username, message) => {
    if (username === bot.username) return
    lastDriven = Date.now()
    emit('whisper', { from: username, message })
  })
  bot.on('playerJoined', p => { if (ready && p.username !== bot.username) emit('player_joined', { player: p.username }) })
  bot.on('playerLeft', p => { if (p.username !== bot.username) emit('player_left', { player: p.username }) })
  // The server announces the death in a system message ("Claude was slain by Zombie"), which beats every guess at the
  // cause. A chat or command the server refuses is answered the same way, and nobody else would ever see it.
  bot.on('message', (msg, position) => {
    const said = deathBy(String(msg), bot.username)
    if (said) saidDeath = { said, at: Date.now() }
    const refusal = position === 'system' && chatRefusal(String(msg))
    if (refusal) emit('chat_refused', { text: refusal })
  })
  const died = pos => {
    diedAt = Date.now()
    lives++
    followTarget = null
    // #147(b): a death ends the run. The phase survived it, and the walk back took Perrin's respawned body straight
    // into the skeleton and the zombie that had just killed him (03:01:10Z)
    if (flee) { const wasDigging = flee.wasDigging; flee = null; useMoves(wasDigging) }
    lastFleeReturn = null
    fleeGaveUp = null
    holedUp = null
    bot.pathfinder.setGoal(null)
    const said = saidDeath && Date.now() - saidDeath.at < 5000 ? saidDeath.said : null
    // from the snapshot, not from the world: by the time a death is handled the server has already emptied the
    // inventory, so a live read says the body died carrying nothing (03:14Z, the first died line with a cause on it)
    const kit = deathKit(lastCarried ?? {})
    const line = deathReport({ pos, said, wound: lastWound, now: Date.now() })
    emit('died', { ...line, ...(kit ? { carried: kit } : {}) })
    lastDeath = { at: diedAt, pos, kit }
    // a death ends the task it interrupted: its FAIL line says so and where the kit lies (card 8946c03a)
    if (task) cancelTask(deathCancel({ pos, cause: line.cause }))
    saidDeath = null
  }
  // a beat, so the death message and the health packet can land in either order: they arrive in the same read, and
  // setImmediate runs after both handlers have had it. Long enough to catch the server's words, short enough that
  // nothing else can happen first
  bot.on('death', () => { const where = pos(); setImmediate(() => died(where)) })
  // a dug bamboo base never regrows. Whatever dug it on the way (the wedge reflex, a dig=true walk out of a thicket), it is planted again when the task ends;
  // not when the driver asked for the dig itself
  // who opened that gate? Every body that sees a fence gate change state adds a line to the shared gates.log (an open gate empties a pen, and no log could say whose it was)
  bot.on('blockUpdate', (before, after) => {
    if (!after?.name?.endsWith('_fence_gate')) return
    const state = b => ({ name: b?.name, open: String(b?.getProperties().open) === 'true' })
    const players = Object.values(bot.players).filter(p => p.entity).map(p => ({ name: p.username, dist: p.entity.position.distanceTo(after.position) }))
    const moving = Boolean(bot.pathfinder.goal) || bot.pathfinder.isMoving()
    const line = gateChange(after.position, state(before), state(after), players, { me: bot.username, moving, clicking: doorBusy || Date.now() - (myClicks.get(String(after.position)) ?? -Infinity) < MY_CLICK_MS })
    // whose gate it is now: only my own walk or click makes it mine to shut; anyone else's change takes it off my list
    if (line?.mine && !heldOpen.has(String(after.position))) doorsIOpened.add(String(after.position))
    if (line?.byOther) { doorsIOpened.delete(String(after.position)); othersToggled.set(String(after.position), Date.now()) }
    if (line) fs.appendFile(GATES_FILE, JSON.stringify({ t: new Date().toISOString(), seenBy: cfg.username, task: task?.name, ...line }) + '\n', () => {})
  })
  bot.on('diggingCompleted', block => {
    if (['dig', 'mine'].includes(task?.name)) return
    basesOwed.push(...wedgeReplant([{ name: block.name, below: bot.blockAt(block.position.offset(0, -1, 0))?.name, at: [block.position.x, block.position.y, block.position.z] }]))
  })
  // A death mineflayer did not announce is still a death, and the respawn always arrives: that is what the 09-22 file
  // looks like (a jump to the world spawn and nothing else). The body is at the spawn point by now, so the line is
  // written from where it last stood.
  bot.on('respawn', () => {
    if (deathUnannounced({ diedAt, now: Date.now() })) died(lastStood)
    // #147(c): a body that has just died does not walk anywhere while it is night or its killer is still standing there.
    // The respawn event comes before the new position and its chunks: read the world a second later, not the grave
    bot.pathfinder.setGoal(null)
    bot.waitForTicks(20).then(() => {
      if (!bot.entity) return
      const plan = respawnPlan({
        night: isNight(bot.time.timeOfDay),
        bedNear: !bedChoice(bedsNear(), zones, cfg.username).error,
        killerNear: nearbyHostiles(8).length > 0
      })
      emit('respawned', { ...(plan.why ? { doing: plan.do, note: plan.why } : {}) })
      if (plan.do === 'burrow') holeUp(plan.why)
      // #147(c): a just-respawned body sleeps in a near or carried bed, never walks one of its own
      if (plan.do === 'sleep') submitJob('sleep', { timeout: 60, automatic: true, walk: false }, { automatic: true })
    }, () => emit('respawned'))
  })
  bot.on('sleep', () => emit('sleeping'))
  bot.on('wake', () => emit('woke_up'))
  bot.on('rain', () => emit('weather', { raining: bot.isRaining }))

  // what each slot held a moment ago: when the server says "your main hand item broke", the item is already gone
  const SLOT_INDEX = { head: 5, torso: 6, legs: 7, feet: 8, 'off-hand': 45 }
  const held = slot => (slot === 'hand' ? bot.heldItem : bot.inventory.slots[SLOT_INDEX[slot]])?.name
  let worn = {}
  bot.on('physicsTick', () => { for (const slot of ['hand', 'off-hand', 'head', 'torso', 'legs', 'feet']) worn[slot] = held(slot) ?? worn[slot] })
  bot._client.on('entity_status', packet => {
    const slot = packet.entityId === bot.entity?.id && brokenSlot(packet.entityStatus)
    if (!slot) return
    const item = worn[slot]
    worn = { ...worn, [slot]: undefined }
    // the broken item is still in the inventory when this packet comes: count the spares once it is gone
    bot.waitForTicks(10).then(() => emit('tool_broke', { item, slot, spare: bot.inventory.items().filter(i => i.name === item).length }))
  })

  let lastHealth = 0
  // for hurtCause: how far I dropped before I last landed, and when a creeper was last close
  let peakY = null
  let lastFall = { blocks: 0, at: 0 }
  let creeperSeenAt = 0
  bot.on('physicsTick', () => {
    if (!bot.entity) { peakY = null; return }
    const y = bot.entity.position.y
    if (!bot.entity.onGround) { peakY = Math.max(peakY ?? y, y); return }
    if (peakY !== null && peakY - y >= 1) lastFall = { blocks: Math.round(peakY - y), at: Date.now() }
    peakY = null
  })
  setInterval(() => {
    if (!ready) return
    if (nearbyHostiles(8).some(e => e.name === 'creeper')) creeperSeenAt = Date.now()
    // where the kit would fall: read here rather than at the respawn, which has already moved the body to the world spawn
    if (bot.entity?.onGround) lastStood = pos()
    lastCarried = carried()
  }, 500)
  bot.on('health', () => {
    bot.autoEat.setOpts({ minHunger: eatBelow(bot.health) })
    if (bot.health < lastHealth - 0.5) {
      lastHurt = Date.now()
      const nearby = nearbyHostiles(8).map(e => e.name)
      // #149(c): hit mid-meal by something with teeth: the meal goes, the sword comes back. Starving damage keeps the meal
      if (nearby.length) dropMeal(`hit by ${nearby[0]} mid-meal`)
      const cause = hurtCause({ lost: lastHealth - bot.health, nearby, sinceCreeperMs: Date.now() - creeperSeenAt, fell: Date.now() - lastFall.at < 1500 ? lastFall.blocks : 0, fledFrom: lastReflex?.kind === 'fleeing' ? lastReflex.mob : null, sinceFledMs: Date.now() - (lastReflex?.at ?? 0), food: bot.food, oxygen: bot.oxygenLevel ?? 20 })
      lastWound = { cause, nearby, at: Date.now() }
      // a hit the water or a fall gave is no mob in reach (17:46Z: drowning at health 5, the hole-up refusal said a hostile hit me)
      if (mobHit(cause)) lastMobHurt = Date.now()
      emit('hurt', { health: Math.round(bot.health), food: bot.food, nearby, ...(cause ? { cause } : {}) })
    }
    lastHealth = bot.health
  })

  let wasNight = null
  bot.on('time', () => {
    const night = isNight(bot.time.timeOfDay)
    if (wasNight !== null && night !== wasNight) emit(night ? 'night_fell' : 'dawn')
    wasNight = night
  })

  let claimed = []
  // logged at most once a second: a wedged body is reset 20 times a second (482 lines in Jizo's log for one bamboo grove)
  let lastResetLogged = 0
  bot.on('forcedMove', () => {
    resets++
    // the LAST packet, rate limit or not: the oxygen evidence (card 962beec2) reads its age
    lastServerPos = { x: bot.entity.position.x, y: bot.entity.position.y, z: bot.entity.position.z, at: Date.now() }
    if (Date.now() - lastResetLogged < 1000) return
    lastResetLogged = Date.now()
    console.log('  claimed before the reset:', claimed.join(' | '))
    console.log('forcedMove (server reset position) ->', JSON.stringify(pos()), 'exact', bot.entity.position.x, bot.entity.position.y, bot.entity.position.z, 'onGround', bot.entity.onGround)
  })

  if (cfg.debugWindows) {
    const short = o => JSON.stringify(o, (k, v) => (k === 'components' || k === 'removedComponents') ? undefined : v).slice(0, 400)
    for (const n of ['set_slot', 'window_items', 'open_window', 'close_window', 'craft_recipe_response', 'set_player_inventory', 'set_cursor_item']) {
      bot._client.on(n, p => console.log('<-', n, short(p)))
    }
    const w0 = bot._client.write.bind(bot._client)
    bot._client.write = (name, params) => {
      if (/window|craft|container/.test(name)) console.log('->', name, short(params))
      return w0(name, params)
    }
  }

  let tick = 0
  bot.on('physicsTick', () => { if (++tick % 10 === 0 && ready && reflexes) reflexTick() })
  // card 962beec2: every change of the air number is an `oxygen` event with the evidence that says whether the server
  // holds the body somewhere wet (reflexes or not: the number is read, never acted on here)
  let airMemory = freshAir
  bot.on('physicsTick', () => {
    if (tick % 5 !== 0 || !ready) return
    const me = bot.entity.position
    const { memory, event } = airSample({ memory: airMemory, oxygen: bot.oxygenLevel, health: bot.health, client: me, server: lastServerPos, now: Date.now(), head: blockName(bot.blockAt(me.offset(0, 1.62, 0))), inWater: bot.entity.isInWater })
    airMemory = memory
    if (event) emit('oxygen', { ...event, ...(lastAirMeta ? { meta: lastAirMeta } : {}) })
  })
  // the raw air metadata, by entity id: a number that swings 8 -> 20 -> 8 within a second is no drain, so WHICH entity
  // the server meant, and what else rode in the packet, is the question (card 962beec2)
  let lastAirMeta = null
  bot._client.on('entity_metadata', p => {
    const air = (p.metadata ?? []).find(m => m.key === 1)
    if (!air) return
    const own = Boolean(bot.entity) && p.entityId === bot.entity.id
    lastAirMeta = { entityId: p.entityId, own, who: bot.entities[p.entityId]?.username ?? bot.entities[p.entityId]?.name ?? null, air: air.value, packet: p.metadata.map(m => `${m.key}:${m.type}=${JSON.stringify(m.value)}`).join(' ') }
    if (own) console.log('[air_meta]', JSON.stringify(lastAirMeta))
  })
  // card 962beec2, hypothesis 2: a move of the body's OWN entity by the entity packets (not the `position` packet, so
  // no forcedMove and no resend) puts the body where the server holds it without a word. Said, and kept as a server
  // position for the oxygen event and the state line
  for (const kind of ['entity_teleport', 'sync_entity_position', 'rel_entity_move', 'entity_move_look']) {
    bot._client.on(kind, p => {
      if (!bot.entity || p.entityId !== bot.entity.id) return
      const at = p.x !== undefined ? { x: p.x, y: p.y, z: p.z } : null
      if (at) lastServerPos = { ...at, at: Date.now() }
      emit('server_moved_me', { packet: kind, ...(at ? { to: [at.x, at.y, at.z].map(n => Math.round(n * 100) / 100).join(',') } : { delta: [p.dX, p.dY, p.dZ].join(',') }), client: bot.entity.position.toArray().map(n => Math.round(n * 100) / 100).join(',') })
    })
  }
  bot.on('physicsTick', () => { if (tick % 2 === 0 && ready) doorTick() })
  let lastSurfaceTrace = 0
  // Pathfinding and idle motion can reset controls between reflexTick calls.
  // Keep the emergency swim pressed on every physics tick until breathing.
  bot.on('physicsTick', () => {
    if (!ready) return
    if (swimStepTarget) {
      // The bounded swim action owns horizontal steering. Keep the upward
      // stroke even if the air reflex changes state during this physics tick.
      bot.setControlState('jump', true)
      bot.setControlState('forward', true)
      return
    }
    if (!reflexes || !surfacing || !surfaceWayNow) return
    if (bot.pathfinder.isMoving()) bot.pathfinder.setGoal(null)
    bot.setControlState('jump', true)
    // forward only for a sideways swim, aimed at its opening: in water it moves along the yaw whatever the pitch
    bot.setControlState('forward', surfaceWayNow.way === 'sideways')
    if (Date.now() - lastSurfaceTrace >= 2000) {
      lastSurfaceTrace = Date.now()
      const p = bot.entity.position
      emit('surfacing_debug', { way: surfaceWayNow.way, exact: p.toArray().map(n => Math.round(n * 100) / 100).join(','), vy: Math.round(bot.entity.velocity.y * 1000) / 1000, oxygen: bot.oxygenLevel, inWater: bot.entity.isInWater, headInWater: bot.blockAt(p.offset(0, 1.62, 0))?.name === 'water', jump: bot.getControlState('jump'), forward: bot.getControlState('forward') })
    }
  })

  // a body refused at login is kicked again on every retry, every ten seconds, for ever: a throwaway body of mine that
  // was not on the whitelist would have written one line all night (#119). The first is said, a reason that CHANGES is
  // said at once, and the rest are counted - which a gate like `disconnected`'s would have thrown away
  bot.on('kicked', reason => {
    const say = sayOnce('kicked', typeof reason === 'string' ? reason : JSON.stringify(reason))
    if (say) emit('kicked', { reason: say })
    const yielded = loginYield(reason, Date.now())
    if (!yielded) return
    yieldUntil = Date.parse(yielded.until)
    emit('yielded', yielded)
  })
  // while the server is down we retry quietly: only the first failure is worth an event
  bot.on('error', err => { if (ready || !waitingForServer) sayError(err.message || err.code || String(err)) })
  bot.on('end', reason => {
    if (ready || !waitingForServer) emit('disconnected', { reason })
    waitingForServer = !ready
    setReady(false)
    // Whatever primitive/composite was mid-await is now bound to a socket that will never deliver its
    // event again: cancel/stop/discard only ever mark the shelf, none of them can make that promise
    // settle, and without this the owner slot stays wedged until the process itself restarts (card:
    // farm.build job 322 stuck at active= for hours after a creeper-interrupted reconnect).
    cancelTask(`disconnected: ${reason}`)
    scheduler?.abandon(`disconnected: ${reason}`)
    clearTimeout(restoreTimer); restoreTimer = null
    reconnectTimer = setTimeout(connect, reconnectDelay(yieldUntil, Date.now()))
  })
}
export const isHostile = e => e.type === 'hostile' || e.kind === 'Hostile mobs'
export function nearbyHostiles (range) {
  if (!bot?.entity) return []
  return Object.values(bot.entities).filter(e => e !== bot.entity && isHostile(e) && e.position.distanceTo(bot.entity.position) <= range)
}

// A reconnect (disconnect, server outage, or a process restart) that interrupted the active job always leaves the
// queue held with blockUrgent (job-shelf.mjs: the interrupted job's own outcome is unknown). A driver used to have
// to notice and send `./mc resume recovered=true`; one who didn't left a body standing in the open at nightfall.
// Once the world is loaded, the body now does that itself, but only when no monster stands within 8 blocks - a
// hostile scan right after a reconnect is exactly the situation that killed it last time. Not clear: wait and retry.
function tryAutoRestore () {
  clearTimeout(restoreTimer); restoreTimer = null
  if (!ready) return
  if (!jobShelf.snapshot().held?.blockUrgent) return
  if (nearbyHostiles(8).length > 0) {
    emit('queue_waiting', { reason: 'hostile' })
    restoreTimer = setTimeout(tryAutoRestore, 30000)
    return
  }
  scheduler.resume({ recovered: true })
}
// the shared code is loaded once, at start: tell the driver when it has changed since, once per batch of edits
const codeLoaded = Date.now()
// and WHICH code that was, read from git once at start and said in the join line. A body started between two saves of
// a shared tree runs half of somebody's change and throws something that is in nobody's diff (#140). Never fatal: a
// body with no git, or no repo, joins anyway and says it does not know.
export const codeHere = (() => {
  const run = args => execFileSync('git', args, { cwd: ROOT, encoding: 'utf8', timeout: 5000, stdio: ['ignore', 'pipe', 'ignore'] })
  try {
    return codeVersion({
      head: run(['rev-parse', '--short', 'HEAD']).trim(),
      changed: run(['status', '--porcelain']).split('\n').filter(Boolean).map(line => line.slice(3).trim())
    })
  } catch {
    return codeVersion({ head: null })
  }
})()
let staleTold = ''
setInterval(() => {
  if (!ready) return
  // was a hard-coded list of src/ files: a split into src/lib/ or src/body/ modules would announce nothing for
  // an edit to any of them, so this now walks src/ itself, the same way tools/check-code.mjs's codeFiles() does
  const files = [...fs.readdirSync(path.join(ROOT, 'src'), { recursive: true }).map(f => path.join('src', f)).filter(f => f.endsWith('.mjs')), ...libraryFiles().map(f => `library/${f}`)]
  const mtimes = Object.fromEntries(files.map(f => [f, fs.statSync(path.join(ROOT, f), { throwIfNoEntry: false })?.mtimeMs]))
  const stale = staleCode(codeLoaded, mtimes, Date.now())
  if (!stale || staleKey(stale, mtimes) === staleTold) return
  staleTold = staleKey(stale, mtimes)
  emit('code_updated', { files: stale.join(' '), advice: restartAdvice({ day: !isNight(bot.time.timeOfDay), timeOfDay: bot.time.timeOfDay, at: Date.now() }) })
}, 60000)
let lastSteppedOff = 0

export let fighting = null
export const setFighting = v => { fighting = v }
// where the body stood when the current fight began, and the leash that measures from it (#105)
export let fightStart = null
export const setFightStart = v => { fightStart = v }
let chaseHeldUntil = 0
let chaseLeash = CHASE_LEASH
export let surfacing = false
let swimStepTarget = null
// what the surfacing reflex is doing now (src/navigation/surface.mjs: up, sideways to an opening, or a pocket dug in the ceiling),
// judged again every reflex tick as the body moves
let surfaceWayNow = null
// openings a sideways swim pressed towards for 2 s without getting nearer: walls, not ways
let surfaceTried = []
let swimTracks = {}
let pocketDigging = false
// the blocks straight over the head, as names, so the reflex can tell deep water from a roof it must swim out from under
const columnAbove = (pos, height = 8) =>
  Array.from({ length: height }, (_, i) => bot.blockAt(new Vec3(Math.floor(pos.x), Math.floor(pos.y) + 2 + i, Math.floor(pos.z)))?.name ?? 'air')
// the first block over the head that is not water, within reach, with how long the best carried tool takes to break it
// from here (Infinity: never): the pocket the reflex digs when no open water is near
const ceilingOver = pos => {
  const i = roofAt(columnAbove(pos, 3))
  if (i < 0) return null
  const block = bot.blockAt(new Vec3(Math.floor(pos.x), Math.floor(pos.y) + 2 + i, Math.floor(pos.z)))
  if (!block) return null
  const ms = block.hardness >= 0 ? Math.min(...[null, ...bot.inventory.items()].map(item => block.digTime(item?.type ?? null, false, bot.entity.isInWater, !bot.entity.onGround, [], {}))) : Infinity
  return { x: block.position.x, y: block.position.y, z: block.position.z, name: block.name, digTicks: Math.ceil(ms / 50), block }
}
// water cells with air over them round the body: where a sideways swim can surface
const openingsNear = pos => findBlocksNear({ point: pos, matching: bot.registry.blocksByName.water.id, useExtraInfo: b => isAir(bot.blockAt(b.position.offset(0, 1, 0))?.name), maxDistance: SURFACE_SCAN + 2, count: 64 })
const cellKey = ({ x, y, z }) => `${x},${y},${z}`
const wayLabel = way => way ? `${way.way}${way.to ? ` to ${cellKey(way.to)}` : ''}${way.at ? ` at ${cellKey(way.at)}` : ''}` : ''
// one reflex tick of surfacing: judge the way out from where the body is now, say it when it changes, and steer.
// A body drowned at -129.3,33.2,-138.3 with air two blocks off (card a164bbfd): the way was judged once, at the start
function steerSurfacing (me) {
  if (pocketDigging) return
  const ceiling = ceilingOver(me)
  const way = surfaceWay({ column: columnAbove(me), openings: openingsNear(me), me, ceiling, oxygen: bot.oxygenLevel, health: bot.health, tried: surfaceTried })
  if (wayLabel(way) !== wayLabel(surfaceWayNow)) {
    emit('surfacing', { oxygen: bot.oxygenLevel, way: way.way, ...(way.to && { to: cellKey(way.to), dist: way.dist }), ...(way.at && { at: cellKey(way.at), ticks: way.ticks }), ...(way.note && { note: way.note }) })
  }
  surfaceWayNow = way
  if (way.way === 'sideways') {
    if (!swimStepTarget) bot.lookAt(new Vec3(way.to.x + 0.5, me.y + 1.62, way.to.z + 0.5), true).catch(() => {})
    const progress = openingProgress(swimTracks, way, Date.now())
    swimTracks = progress.tracks
    if (progress.failed) surfaceTried = [...new Set([...surfaceTried, progress.failed])]
  }
  if (way.way === 'pocket') digPocket(ceiling.block)
}
// a block dug out of the ceiling stays air (water never flows up): the body rises into it and breathes
function digPocket (block) {
  pocketDigging = true
  bot.tool.equipForBlock(block).catch(() => {})
    .then(() => bot.dig(block, true))
    .catch(e => emit('surfacing', { way: 'pocket', error: String(e?.message ?? e) }))
    .finally(() => { pocketDigging = false })
}
// the eight cells around the body (four sides, feet and head), as names: a room is one with at most a doorway open
const sidesAround = pos => [[1, 0], [-1, 0], [0, 1], [0, -1]].flatMap(([dx, dz]) => [0, 1].map(dy => bot.blockAt(pos.floored().offset(dx, dy, dz))?.name ?? 'air'))
// card 0f110bb5 (1): under a roof or in a walled room at night the body chases and charges nothing; it holds where it is
const inShelter = me => nightShelter({ night: isNight(bot.time.timeOfDay), roofed: underRoof(columnAbove(me)), walled: walledIn(sidesAround(me)) })
let floating = false
export let diggingOut = false
let resets = 0
let lastNudge = 0
let basesOwed = []
let windowStart = null
// wedged watchdog: every 3 s compare resets against progress; a wedged body can only be freed by digging
setInterval(async () => {
  if (!ready) return
  const here = bot.entity.position.clone()
  const sample = { resets, moved: windowStart ? here.distanceTo(windowStart) : 0 }
  resets = 0
  windowStart = here
  if (!task || !bot.pathfinder.isMoving() || !isWedged(sample)) return
  // the blocks the hitbox lies flush against. Leaves (the usual culprit, at head height under a tree) and bamboo cost nothing to break: do it and walk on
  const against = flushCells(here).map(c => bot.blockAt(vecOf(c))).filter(b => b?.boundingBox === 'block')
  const named = against.map(b => `${b.name} at x=${b.position.x} y=${b.position.y} z=${b.position.z}`).join(', ')
  if (against.length && against.every(b => wedgeBreakable(b.name) && !inAnyZone(zones, b.position))) {
    const bases = wedgeReplant(against.map(b => ({ name: b.name, below: bot.blockAt(b.position.offset(0, -1, 0))?.name, at: [b.position.x, b.position.y, b.position.z] })))
    const failed = await against.reduce((done, b) => done.then(() => bot.dig(b)), Promise.resolve()).then(() => null, e => e)
    if (!failed) return emit('unwedged', { dug: named, ...(bases.length ? { replantOwed: bases.length } : {}) })
  }
  // anything else: step 2 cm off it once (the pinned state is a body the server put EXACTLY flush); only a second wedge here ends the task
  if (against.length && Date.now() - lastNudge > 15000) {
    lastNudge = Date.now()
    const clear = nudgeAway(here)
    bot.entity.position.set(clear.x, clear.y, clear.z)
    return emit('unwedged', { nudgedOff: named })
  }
  emit('wedged', { pos: pos(), against: named || 'nothing found', advice: `server keeps resetting my position: dig the block I am pressed against (${named || 'look around with scan'}), then retry` })
  cancelTask(`wedged against ${named || 'a block'}: dig it (dig x= y= z=), then retry`)
}, 3000)
// memory safety net: whatever is eating the heap is almost certainly a path search (a task's, or a reflex's: fighting or fleeing towards
// something unreachable). Losing the walk beats losing the body
setInterval(() => {
  const heapMb = Math.round(process.memoryUsage().heapUsed / 1e6)
  if (!ready || !overMemory(heapMb)) return
  emit('error', { message: `memory at ${heapMb} MB: dropping every walk`, task: task?.name, fighting: !!bot.pvp.target, goal: bot.pathfinder.goal?.constructor.name, path: lastPath })
  bot.pvp.stop()
  bot.pathfinder.setGoal(null)
  if (task) cancelTask(`the path search ran out of memory (${heapMb} MB): the goal is probably unreachable from here; go in shorter legs`)
}, 5000)
// is the MaxListeners warning (11 physicsTick listeners) a plateau or a leak? One line every 10 minutes in bot.log settles it
setInterval(() => { if (ready) console.log(`[listeners] physicsTick=${bot.listenerCount('physicsTick')} heapMb=${Math.round(process.memoryUsage().heapUsed / 1e6)}`) }, 600000)
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
// bedtime reflex (see bedtime in lib.mjs)
export let lastDriven = Date.now()
export const setLastDriven = v => { lastDriven = v }
let lastBedTry = 0
let bedFailures = 0
export let bedWalkFailed = false // a walk to the own bed failed or came up short tonight: go straight to placement, not retried till the next night
export const setBedWalkFailed = v => { bedWalkFailed = v }
setInterval(() => {
  if (!ready) return
  const now = Date.now()
  // #147: holed up for the night means staying in the hole, not walking out of it to the bed past what put me there
  const night = isNight(bot.time.timeOfDay)
  if (!night) { holedUp = null; bedWalkFailed = false }
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
  if (!night || bot.isSleeping) bedFailures = 0
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
  const fresh = path.join(ROOT, 'state', `clock.${cfg.username}.tmp`)
  fs.writeFileSync(fresh, JSON.stringify({ day: !isNight(bot.time.timeOfDay), timeOfDay: bot.time.timeOfDay, by: cfg.username, at: Date.now() }))
  fs.renameSync(fresh, path.join(ROOT, 'state', 'clock.json'))
}, 5000)
// stall watchdog: a task with somewhere to walk that neither moves nor digs is hung; fail it loudly instead of forever
let stillFrom = null
// the stuck watch (src/navigation/stuck.mjs, autopilot card): one sample a second over a rolling window, one `stuck` event and one
// chat line per episode, stuck=<reason> in `state` while it lasts
let stuckSamples = []
export let stuckNow = null
let frozenWalks = 0 // every frozen_walk said, for the watch's five-minute window
let failedWalks = 0 // every walk that ended with no path, for the watch's walks verdict (a body that cannot leave its cell)
const noPathCounted = e => { if (/no path to the goal|no walkable path|took to long to decide/i.test(e.message)) failedWalks++; return e }
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
function watchStuck () {
  stuckSamples = addSample(stuckSamples, stuckSample())
  const { episode, started, ended } = nextEpisode(stuckNow, stuckVerdict(stuckSamples), Date.now())
  stuckNow = episode
  // free again (the verdict clear for END_MS): said once, in the log only
  if (ended) emit('stuck_end', { pos: pos(), reason: episode.reason, seconds: Math.round((episode.over - episode.since) / 1000) })
  if (!started) return
  emit('stuck', { pos: pos(), reason: episode.reason, advice: episode.advice })
  bot.chat(stuckLine(bot.entity.position, episode.reason))
}
let kickedFor = null // the stand-still (a stillFrom) whose walk I already restarted once
// bot.controlState has no enumerable keys (getters): Object.entries on it is always empty, which made every stall report say keys=[] until 09-19
const keysDown = () => ['forward', 'back', 'left', 'right', 'jump', 'sprint', 'sneak'].filter(k => bot.getControlState(k))
export let lastPath = null
export let lastServerPos = null // where the server last PUT the body (it only speaks up when it disagrees with the client)
let frozenFor = null // the stillFrom a frozen walk was already reported for
export let lastFrozen = null // the last frozen walk: when, where and what the evidence blamed, for a task to own the failure (escort)
let livePath = [] // the pathfinder's own array: [0] is always the node it is heading for
let idleTicks = 0
let nudging = false
let fencePressed = null // the idle nudge is walking me out of a fence's cell: plan again once I am out
let walkEndedAt = 0
// the goal of the last walk: the no-path advice says how far up it was
let lastWalkGoal = null
let walkEndedBy = null
let goalSetAt = 0 // a path result from before this goal says nothing about this walk
let activeWalk = null // the actual awaited goto promise, including composite sub-actions
let nearest = null // meaningful nearest-goal progress for that walking leg
// why the pathfinder threw its path away, lately: a stall with a found path and idle keys looks like a storm of these
const pathResets = []
// what the legs were up to when a walk hung: evidence for the doorstep stall nobody has explained yet
const stallEvidence = () => ({
  moving: bot.pathfinder.isMoving(),
  keys: keysDown(),
  onGround: bot.entity.onGround,
  exact: bot.entity.position.toArray().map(n => Math.round(n * 100) / 100),
  path: lastPath && { ...lastPath, agoMs: Date.now() - lastPath.at, at: undefined },
  resets: Object.entries(pathResets.filter(r => Date.now() - r.at < 12000).reduce((n, r) => ({ ...n, [r.reason]: (n[r.reason] ?? 0) + 1 }), {})).map(([reason, n]) => `${reason}:${n}`).join(' '),
  doorBusy,
  // card 962beec2: a walk that presses forward with the position frozen has a valid path and nothing to see at the
  // feet. What the legs push into, where the head faces against the path, who is pressed against the body and where
  // the server last put it is what tells the cases apart
  server: serverSide(lastServerPos, Date.now()),
  facing: facingOff({ yaw: bot.entity.yaw, from: bot.entity.position, nodes: lastPath?.nodes ?? [] }),
  ahead: aheadCells(bot.entity.position, bot.entity.yaw).map(c => `${blockName(bot.blockAt(new Vec3(c.x, c.y, c.z)))}@${c.x},${c.y},${c.z}`),
  near: nearBy(Object.values(bot.entities), bot.entity)
})
setInterval(() => {
  if (!ready) return
  watchStuck()
  const here = bot.entity.position.clone()
  stillFrom = walkStandstill(stillFrom, { pos: here, now: Date.now(), task: task?.id, goal: bot.pathfinder.goal })
  const sample = { hasGoal: Boolean(task && bot.pathfinder.goal), moved: Math.hypot(here.x - stillFrom.pos.x, here.z - stillFrom.pos.z), digging: Boolean(bot.targetDigBlock), seconds: (Date.now() - stillFrom.at) / 1000, path: lastPath && lastPath.at >= goalSetAt ? lastPath : null }
  // forward held and the body not moving for 2 s: said once per standstill, with the evidence that names the cause, long
  // before the 12 s alarm cancels the task (the alarm still does)
  if (frozenWalk({ keys: keysDown(), moved: sample.moved, seconds: sample.seconds }) && frozenFor !== stillFrom) {
    frozenFor = stillFrom
    const evidence = stallEvidence()
    lastFrozen = { at: Date.now(), pos: pos(), advice: frozenAdvice(evidence) }
    frozenWalks++
    emit('frozen_walk', { pos: lastFrozen.pos, evidence, advice: lastFrozen.advice })
  }
  // Every awaited static walk owns this watchdog, including forage.search's
  // sub-action. Water bobbing and repeated replans cannot renew its allowance.
  const goal = bot.pathfinder.goal
  nearest = walkProgress(nearest, { leg: task && activeWalk?.goal === goal ? activeWalk : null, goal, pos: here, now: Date.now(),
    busy: Boolean(bot.targetDigBlock) || bot.pathfinder.isMining() || bot.pathfinder.isBuilding() })
  if (nearest?.stalled) {
    const best = Math.round(nearest.best)
    walkEndedAt = Date.now()
    walkEndedBy = `no walkable path: no net progress toward this goal for ${WALK_PROGRESS_MS / 1000}s (nearest ${best} blocks); try another checked waypoint`
    emit('walk_no_progress', { pos: pos(), best, seconds: WALK_PROGRESS_MS / 1000, evidence: stallEvidence() })
    nearest = null
    stillFrom = null
    // setGoal settles native goto before its caller can recover and start the
    // next leg. Never race a detached timeout against a still-running walk.
    bot.pathfinder.setGoal(null)
    return
  }
  if (deadWalk(sample) && Date.now() - walkEndedAt > 4000) {
    walkEndedAt = Date.now()
    // its search was still going (partial, no step yet): out of time, not out of ways (the alley by Perrin's sheep pen gate)
    walkEndedBy = 'Took to long to decide path to goal!'
    console.log('[dead walk ended]', JSON.stringify(stallEvidence()))
    stillFrom = null // the task was told at once and may go on to its next target: the 12 s stall clock starts again
    bot.pathfinder.setGoal(null)
    return
  }
  // the pathfinder walked its path to the end, the goal is not met, and it stands there without searching again: search once more
  // for it, and if that does not get it going either, end the walk now rather than at the 12 s alarm
  if (droppedWalk({ ...sample, moving: bot.pathfinder.isMoving(), pathAgeMs: lastPath ? Date.now() - lastPath.at : null })) {
    const again = kickedFor !== stillFrom
    console.log(again ? '[walk kicked]' : '[dropped walk ended]', JSON.stringify(stallEvidence()))
    if (again) { kickedFor = stillFrom; bot.pathfinder.setGoal(bot.pathfinder.goal, bot.pathfinder.goal.entity !== undefined); return }
    walkEndedAt = Date.now()
    walkEndedBy = arrivalError(false)
    stillFrom = null
    bot.pathfinder.setGoal(null)
    return
  }
  if (!isStalled(sample)) return
  stillFrom = null
  const onBed = /_bed$/.test(bot.blockAt(bot.entity.position.floored())?.name ?? '') ? bedExit(bedExits(bot.entity.position.floored())) : null
  emit('stalled', { pos: pos(), evidence: stallEvidence(), advice: onBed ?? 'the walk never got going: step a couple of blocks away (goto), then retry' })
  cancelTask('stalled: no movement for 12s; step a couple of blocks away, then retry')
}, 1000)
// #138: one flee run at a time, with a phase and a way home. `fleeingUntil` was a timer that every tick re-armed
// while a threat stood near, which is how a body chased once kept running until something else stopped it.
export let flee = null // { mob, entity, home, phase, held, wasDigging, still: { pos, at } }
let lastFleeReturn = null // the mob and time of the last walk back, for the oscillation guard
let fleeGaveUp = null // the mob and time of the last run that handed control back, so it does not restart itself

// One run of the flee reflex, start to finish (#138). Everything that moves the body is here; what to do next is
// decided by fleeStep in lib.mjs, which knows nothing about bodies.
const fleeAt = () => flee ? { from: flee.mob, pos: pos(), home: `${Math.round(flee.home.x)},${Math.round(flee.home.y)},${Math.round(flee.home.z)}` } : {}
function startFlee (entity, me, note, extra = {}) {
  // a run this body already gave up on does not start itself again the moment the legs are free: that is the loop
  if (fleeOscillating({ mob: entity.name, last: fleeGaveUp, now: Date.now(), within: FLEE_GIVEUP_MS })) return false
  const held = fleeOscillating({ mob: entity.name, last: lastFleeReturn, now: Date.now() })
  fighting = null
  fightStart = null
  bot.pvp.stop()
  // home survives a re-trigger: the work is still where the FIRST run left it, not where the second one started
  flee = { mob: entity.name, entity, home: flee?.home ?? me.clone(), phase: 'away', held, wasDigging: flee?.wasDigging ?? digging, still: { pos: me.clone(), at: Date.now() } }
  // digging and bridging are the escape, not a detour: a body cornered against a wall has to be able to cut its way out
  useMoves(true)
  fleeTowards(me, entity)
  lastReflex = { kind: 'fleeing', mob: entity.name, at: Date.now() }
  emit('fleeing', { from: entity.name, ...(note ? { note } : {}), ...extra })
  emit('flee_started', { ...fleeAt(), ...(held ? { held: 'it came back within a minute of the last walk back: this run does not come home' } : {}) })
  return true
}

// a fixed point straight away from the mob (see fleeGoal): a goal that moved with the mob never let the run sprint
// at night the point is a bed or a placed torch that lies away from the mob, within twenty blocks of where the run began;
// with none, the straight point is cut at that bound (card 0f110bb5 (2): a 52-block run into dark hills is where the zombie came back)
function fleeTowards (me, entity) {
  const night = isNight(bot.time.timeOfDay)
  const refuges = night ? [...bedsNear().map(p => ({ x: p.x, y: p.y, z: p.z, kind: 'bed' })), ...torchesNear().map(p => ({ x: p.x, y: p.y, z: p.z, kind: 'torch' }))] : []
  flee.goal = nightFleeGoal({ night, me, home: flee.home, mob: entity.position, dist: fleeRange(entity.name) + 4, refuges })
  bot.pathfinder.setGoal(new goals.GoalNearXZ(flee.goal.x, flee.goal.z, 2), false)
}
const torchesNear = () => findBlocksNear({ matching: ['torch', 'wall_torch', 'soul_torch', 'soul_wall_torch', 'lantern'].map(n => bot.registry.blocksByName[n]?.id).filter(Boolean), maxDistance: 24, count: 8 })

function endFlee () {
  if (!flee) return
  const wasDigging = flee.wasDigging
  flee = null
  useMoves(wasDigging)
  if (!task && !followTarget) bot.pathfinder.setGoal(null)
  else resumeFollow()
}

function stepFlee (me) {
  const entity = flee.entity
  const alive = entity && entity.isValid !== false && entity.position
  // a creeper that turns up mid-run is a different problem: it does not chase, it arrives and goes off
  const creeper = nearbyHostiles(6).find(e => e.name === 'creeper')
  if (creeper && flee.mob !== 'creeper') return startFlee(creeper, me)
  if (me.distanceTo(flee.still.pos) >= 1.5) flee.still = { pos: me.clone(), at: Date.now() }
  // #147(d): my own body fled a creeper into the cave under my test pits and died there. A run heading down or into the
  // dark is running INTO what it is running from, so it stops and hands back instead of finding the cave
  const here = bot.blockAt(me.floored())
  // (3) the threat is whatever last hurt me: both runs said "from: zombie" while a spider took the body from 14 to 2 (15:15Z).
  // Armed and hit by something that cannot be outrun, the body turns and fights it instead
  const other = retarget({ fleeing: flee.mob, hurtBy: lastWound?.nearby ?? [], hurtMsAgo: Date.now() - (lastWound?.at ?? 0) })
  const otherEntity = other ? nearbyHostiles(8).find(e => e.name === other) : null
  if (otherEntity) {
    const armed = bot.inventory.items().some(i => /_sword$|_axe$/.test(i.name))
    if (fightNotFlee({ armed, mob: other })) { endFlee(); startFight(otherEntity, `hit by a ${other} mid-run: it cannot be outrun, so I fight it`); return }
    return startFlee(otherEntity, me, `hit by a ${other} mid-run: it is the threat now`)
  }
  const step = nightFleeStep({
    night: isNight(bot.time.timeOfDay),
    phase: flee.phase,
    threatDist: alive ? entity.position.distanceTo(me) : Infinity,
    homeDist: me.distanceTo(flee.home),
    stillMs: Date.now() - flee.still.at,
    held: flee.held,
    bound: fleeRange(flee.mob),
    underground: fleeIntoCave({ startY: flee.home.y, y: me.y, skyLight: here?.skyLight ?? 15, light: here?.light ?? 15 })
  })
  if (step.event === 'flee_started') { if (!startFlee(entity, me)) endFlee(); return }
  // there, and it is still coming: the next point away from where it is now
  if (!step.event && flee.phase === 'away' && alive && flee.goal && Math.hypot(me.x - flee.goal.x, me.z - flee.goal.z) < 3) return fleeTowards(me, entity)
  if (!step.event) return
  emit(step.event, { ...fleeAt(), ...(step.note ? { advice: step.note } : {}) })
  if (step.phase === 'back') {
    // the walk back is what the oscillation guard counts from: a threat that follows me home gets one run, not forever
    lastFleeReturn = { mob: flee.mob, at: Date.now() }
    flee.phase = 'back'
    flee.still = { pos: me.clone(), at: Date.now() }
    useMoves(flee.wasDigging)
    bot.pathfinder.setGoal(new goals.GoalNear(flee.home.x, flee.home.y, flee.home.z, FLEE_HOME), false)
    return
  }
  if (step.event === 'flee_stuck' || step.event === 'flee_held') fleeGaveUp = { mob: flee.mob, at: Date.now() }
  endFlee()
  // #147: a run that gave up where it stood is the body out of ideas, and 40 s of waiting for an agent is what killed Perrin
  if (step.event === 'flee_stuck') holeUp(step.note)
}

// #147. The body's own last resort, the escape the reflex memory already knew: go under the ground and close the hole.
// Nothing here decides WHETHER (holeUpVerdict, holeUpBlock) or WHERE (burrowSite); this digs, places and says where the body went.
const HOLE_AGAIN_MS = 300000
const HOLE_STEP_MS = 3000 // a creeper refusal moves the body once per this
const HOLE_WALK_MS = 4000 // the step to a sounder cell gets this long, not a search
let holedUp = null // { at, why }: one hole per emergency, or the reflex digs a fresh one every tick it is still hungry
export let holingUp = false
let lives = 0 // deaths so far: a hole-up dug by a body that has since died must stop, not cap a hole at the respawn point
let holeRefusedAt = 0
let holeSteppedAt = 0
let lastHoleAt = 0 // holedUp is cleared by day (it gates the bedtime walk), so the once-per-emergency guard keeps its own clock
// a cell closed by placing a block against any solid neighbour of it: inside a 1-wide shaft there is nowhere to walk to
async function fillCell (p, item) {
  const there = bot.blockAt(p)
  if (there && there.boundingBox === 'block') return true
  if (!item) return false
  for (const [dx, dy, dz] of [[0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0]]) {
    const ref = bot.blockAt(p.offset(dx, dy, dz))
    if (!ref || ref.boundingBox !== 'block') continue
    const held = findItem(item)
    if (!held) return false
    const done = await bot.equip(held, 'hand').then(() => bot.placeBlock(ref, new Vec3(-dx, -dy, -dz))).then(() => true, () => false)
    if (done) return true
  }
  return false
}
// the cap and the pillar: full solid blocks only (one hole was capped with leaf_litter, the first block-shaped item in the pack)
const capItems = () => bot.inventory.items().filter(i => capChoice([{ name: i.name, boundingBox: bot.registry.blocksByName?.[i.name]?.boundingBox }]))
export const capBlock = () => capItems()[0] ?? null
const capCount = () => capItems().reduce((n, i) => n + i.count, 0)
// what the site probe reads for one cell: the column below it and the four cells beside each depth (a channel one cell to the
// side poured into the shaft as it was dug, 15:02Z)
const siteReading = cell => ({
  below: [1, 2, 3].map(dy => bot.blockAt(cell.offset(0, -dy, 0))?.name ?? null),
  beside: [1, 2, 3].map(dy => [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => bot.blockAt(cell.offset(dx, -dy, dz))?.name ?? null))
})
const passable = b => Boolean(b) && b.boundingBox !== 'block'
const standable = cell => passable(bot.blockAt(cell)) && passable(bot.blockAt(cell.offset(0, 1, 0))) && bot.blockAt(cell.offset(0, -1, 0))?.boundingBox === 'block'
const sitesAround = start => {
  const out = []
  for (let dx = -HOLE_STEP; dx <= HOLE_STEP; dx++) {
    for (let dz = -HOLE_STEP; dz <= HOLE_STEP; dz++) {
      if (dx === 0 && dz === 0) continue
      const cell = start.offset(dx, 0, dz)
      out.push({ dx, dz, standable: standable(cell), ...siteReading(cell) })
    }
  }
  return out
}
// WHETHER now. Returns true when a hole-up is under way (or the body is already down), false when it was refused, so the
// reflex tick that asked falls through to the fight it should have instead: the verdict returned early every tick and the
// fight reflex never ran while an armed body stood refused (17:46Z)
function holeUp (why) {
  if (holingUp || bot.isSleeping || !bot.entity) return true
  if (Date.now() - lastHoleAt < HOLE_AGAIN_MS) return true
  const me = bot.entity.position
  const nearest = nearbyHostiles(16).map(e => ({ entity: e, dist: e.position.distanceTo(me) })).sort((a, b) => a.dist - b.dist)[0]
  const armed = bot.inventory.items().some(i => /_sword$|_axe$/.test(i.name))
  const hurtMsAgo = Date.now() - lastMobHurt
  const way = holeUpBlock({ armed, hostile: nearest?.entity.name ?? null, hostileDist: nearest?.dist ?? Infinity, hurtMsAgo })
  if (!way) { digIn(why); return true }
  if (Date.now() - holeRefusedAt > 10000) emit('holing_up', { why, way, note: refusalNote({ way, hostile: nearest?.entity.name, hostileDist: nearest?.dist ?? Infinity, hurtMsAgo }) })
  holeRefusedAt = Date.now()
  // a creeper in reach is never fought and the run it came from has just given up: refused and standing still was the gap
  if (way === 'step') stepAway(nearest.entity, me)
  return false
}
function stepAway (entity, me) {
  if (Date.now() - holeSteppedAt < HOLE_STEP_MS) return
  holeSteppedAt = Date.now()
  const goal = fleeGoal(me, entity.position, HOLE_MELEE + 2)
  bot.pathfinder.setGoal(new goals.GoalNearXZ(goal.x, goal.z, 1), false)
  setTimeout(() => { if (!task && !followTarget && !flee) bot.pathfinder.setGoal(null) }, HOLE_STEP_MS)
}
// the step to a sounder cell: a short walk with a deadline, never a search
async function stepTo (cell) {
  await within(HOLE_WALK_MS, bot.pathfinder.goto(new goals.GoalBlock(cell.x, cell.y, cell.z)), 'stepping to sounder ground').catch(() => {})
  bot.pathfinder.setGoal(null)
}
// the column under the START cell, not under wherever the body is: one that landed on a rim slid about as it dug and dug a
// cell under each place it slid to, none of them a shaft (18:15Z). After each cell the body steps down into it
async function digDown (start, stopped) {
  for (const k of [1, 2, 3]) {
    const under = bot.blockAt(start.offset(0, -k, 0))
    if (!under || under.boundingBox !== 'block') return
    await bot.tool.equipForBlock(under).catch(() => {})
    await bot.dig(under).catch(() => {})
    await bot.waitForTicks(8)
    if (stopped()) return
    if (bot.entity.position.y > start.y - k + 0.5) await within(1500, bot.pathfinder.goto(new goals.GoalBlock(start.x, start.y - k, start.z)), 'stepping into the shaft').catch(() => {})
    bot.pathfinder.setGoal(null)
    if (stopped()) return
  }
}
// up instead of down: jump, and click the block under the feet while the body is in the air above it
export async function pillarUp (stopped, steps = 3, explicitItem = null) {
  for (let step = 0; step < steps; step++) {
    if (stopped()) return
    const item = explicitItem ? findItem(explicitItem) : capBlock()
    const feet = bot.entity.position.floored()
    const under = bot.blockAt(feet.offset(0, -1, 0))
    if (!item || !under || under.boundingBox !== 'block') return
    await bot.equip(item, 'hand').catch(() => {})
    await bot.lookAt(feet.offset(0.5, -1, 0.5), true).catch(() => {})
    bot.setControlState('jump', true)
    for (let t = 0; t < 8 && bot.entity.position.y - feet.y < 0.9; t++) await bot.waitForTicks(1)
    if (stopped()) { bot.setControlState('jump', false); return }
    const placed = await bot.placeBlock(under, new Vec3(0, 1, 0)).then(() => true, () => false)
    bot.setControlState('jump', false)
    await bot.waitForTicks(6)
    if (!placed || stopped()) return
  }
}
async function digIn (why) {
  holingUp = true
  const life = lives
  holedUp = { at: Date.now(), why }
  lastHoleAt = Date.now()
  // died while digging (13:40Z): the respawned body stood in a village bed and said it had holed up there; and one that
  // respawned INTO its bed went on capping from there (17:27Z)
  const stopped = () => holeUpAborted({ life, lives, asleep: bot.isSleeping })
  try {
    if (task) cancelTask(`holing up by myself: ${why}`)
    bot.pathfinder.setGoal(null)
    let start = bot.entity.position.floored()
    // the site is read BEFORE anything is dug: the column, and what stands beside it, which pours in. A bad site is a step
    // to a sounder cell or a pillar up before it is a wall around the spot
    const plan = burrowSite({ here: siteReading(start), around: sitesAround(start), blocks: capCount() })
    emit('holing_up', { why, way: plan.way, ...(plan.step ? { step: `${start.x + plan.step.dx},${start.y},${start.z + plan.step.dz}` } : {}), ...(plan.why ? { floor: plan.why } : {}) })
    if (plan.step) {
      await stepTo(start.offset(plan.step.dx, 0, plan.step.dz))
      if (stopped()) return
      start = bot.entity.position.floored()
    }
    const surface = { x: start.x, y: start.y, z: start.z }
    if (plan.way === 'dig') await digDown(start, stopped)
    if (plan.way === 'pillar') await pillarUp(stopped)
    // the body is still falling down its own shaft for a tick or two, and the cap goes over the head it ends up with.
    // A respawned body carries nothing, but it lands on what it dug: the cap is chosen now, not before the first swing
    await bot.waitForTicks(10)
    if (stopped()) return
    const block = capBlock()
    const feet = bot.entity.position.floored()
    // a shaft that filled with water is not capped: under a cap in water the body drowns. It is reported as wet instead
    const wet = bot.entity.isInWater || FLUIDS.has(bot.blockAt(feet)?.name ?? '')
    // the ring and the roof, however the hole was made: a shaft dug in a cave stands open to it on the sides
    const cells = plan.way === 'pillar' || wet ? [] : holeCells()
    const closed = []
    for (const [dx, dy, dz] of cells) {
      if (stopped()) return
      closed.push(await fillCell(feet.offset(dx, dy, dz), block?.name))
    }
    const open = plan.way === 'pillar' ? feet.y - surface.y < HOLE_DEPTH : wet || closed.some(done => !done)
    emit('holed_up', {
      why,
      way: plan.way,
      open,
      ...(wet ? { wet } : {}),
      at: `${feet.x},${feet.y},${feet.z}`,
      note: shelterNote({ way: plan.way, open, surface, wet })
    })
  } catch (e) {
    if (stopped()) return
    emit('holed_up', { why, error: String(e?.message ?? e), note: 'the hole-up itself failed: dig me out or tell me what to do' })
  } finally {
    holingUp = false
  }
}
let lastMobHurt = 0 // the last hit a mob could have given: what the hole-up refusal counts, not a fall or the water
let lastHurt = 0
// mineflayer's bot.wake() sends action id 2, which since 1.21.6 means stop_sprinting: the server never hears it
let lastLeftBed = 0
let oversleptSince = null
// at most every 2 s: the reflex tick asks again until the body is really up
export const leaveBed = () => {
  if (Date.now() - lastLeftBed < 2000) return
  lastLeftBed = Date.now()
  bot._client.write('entity_action', { entityId: bot.entity.id, actionId: 'leave_bed', jumpBoost: 0 })
}

function reflexTick () {
  const me = bot.entity.position
  // #149(a,c): the hand belongs to the sword while anything hostile is in reach, a fight or a run is on
  const drop = mealToDrop({ eating: Boolean(bot.autoEat?.isEating), hostileNear: nearbyHostiles(EAT_SAFE_RANGE).length > 0, fighting: Boolean(fighting), fleeing: Boolean(flee) })
  if (drop) dropMeal(drop)
  // running out of air: swim up until we can breathe again
  // (oxygenLevel alone misfires on dry land through ViaBackwards, so also require our head to be in water)
  const headInWater = bot.blockAt(me.offset(0, 1.62, 0))?.name === 'water'
  const air = airReflex({ headInWater, inWater: bot.entity.isInWater, oxygen: bot.oxygenLevel, surfacing })
  if (air === 'start') {
    surfacing = true
    surfaceWayNow = null
    surfaceTried = []
    swimTracks = {}
    // a running task steers the body every tick and wins over one press of jump: Jizo drowned that way, mid-harvest
    if (task && !swimStepTarget) cancelTask('out of air: swimming up to breathe. Work from dry land, then retry')
    bot.pathfinder.setGoal(null)
  }
  // the way out is judged again every half second (src/navigation/surface.mjs): a sideways swim ends under open water, where up is
  // the answer, and an opening not reached in 2 s is given up for the next. Forward is never pressed blind: in water it
  // moves along the yaw whatever the pitch, and that carried a body two blocks under a rock ceiling, where it drowned
  if (air === 'start' || air === 'hold') steerSurfacing(me)
  if (air === 'stop') {
    surfacing = false
    surfaceWayNow = null
    bot.pathfinder.setGoal(null)
    if (!swimStepTarget) {
      bot.setControlState('jump', false)
      bot.setControlState('forward', false)
    }
  }
  // an idle body sinks like a stone, then yo-yos between drowning and surfacing: tread water until the driver moves it
  const afloat = bot.entity.isInWater && !task && !surfacing && !bot.pathfinder.isMoving()
  if ((afloat || floating) && !swimStepTarget) bot.setControlState('jump', afloat)
  floating = afloat
  // buried (gravel or sand fell on us): dig our head free before we suffocate
  const burying = buriedIn([bot.blockAt(me.offset(0, 1.62, 0))])
  if (burying && !diggingOut) {
    diggingOut = true
    emit('buried', { in: burying.name })
    bot.dig(burying, true).catch(() => {}).finally(() => { diggingOut = false })
  }
  // in bed the server ignores our legs, so a reflex that moves us would desync the body: get up properly, act next tick
  if (bot.isSleeping) {
    const late = oversleeping({ asleep: true, timeOfDay: bot.time.timeOfDay, thundering: bot.thunderState > 0 })
    oversleptSince = late ? oversleptSince ?? Date.now() : null
    const step = wakeStep({ oversleeping: late, forMs: late ? Date.now() - oversleptSince : 0 })
    if (nearbyHostiles(7).length || step === 'ask') leaveBed()
    if (step === 'declare') {
      console.log('[stale sleep] in bed by day and the server does not answer leave_bed: awake by my own word')
      oversleptSince = null
      bot.isSleeping = false
      bot.emit('wake')
    }
    return
  }
  // #147: a body digging itself in owns the legs and the hand until the hole is capped: no fight, no run pulls it out
  if (holingUp) return
  // a run already going owns the legs until it ends: one run at a time, and it ends itself (#138)
  if (flee) { stepFlee(me); return }
  // #147: boxed in with nowhere to run, or too hurt to heal with nothing to eat: the answer is the ground, not the legs
  const holeWhy = holeUpVerdict({ hasFood: edibleCarried(), health: bot.health })
  // in the water the problem is air, not shelter (17:46Z: a body drowning at health 5 was told to hole up); and a refused
  // hole-up falls through to the fight or the run below instead of ending the tick
  const drowning = surfacing || bot.entity.isInWater
  if (holeWhy && !drowning && holeUp(holeWhy.why)) return
  // #97, #live-death: an enderman killed Ganesha's body at its own door, and one teleported in and killed another
  // 3s after a 48-block scan saw nothing. Nothing here wins that fight, so one within ENDERMAN_RANGE is backed away
  // from before the reflex below can think of fighting it, well before a teleport can put it at arm's reach
  const unwinnable = fleeUnwinnable(nearbyHostiles(ENDERMAN_RANGE).map(e => ({ name: e.name, dist: e.position.distanceTo(me), entity: e })))
  // a run refused because this body already gave up on that mob falls THROUGH to the fight reflex: a body that cannot
  // run and will not fight is a body standing still while something kills it
  if (unwinnable && startFlee(unwinnable.entity, me, 'not a fight this body can win: breaking line of sight')) return
  const creeper = nearbyHostiles(6).find(e => e.name === 'creeper')
  if (creeper && startFlee(creeper, me)) return
  // unarmed or badly hurt: don't brawl, run. (Two deaths on night one taught me this.)
  const armed = bot.inventory.items().some(i => /_sword$|_axe$/.test(i.name))
  const where = { day: !isNight(bot.time.timeOfDay), skyLight: bot.blockAt(me)?.skyLight ?? 0 }
  const chaser = nearbyHostiles(7).filter(e => !ignorableMob(e.name, where))
    .sort((a, b) => a.position.distanceTo(me) - b.position.distanceTo(me))[0]
  const archer = nearbyHostiles(24).filter(e => ARCHERS.has(e.name)).sort((a, b) => a.position.distanceTo(me) - b.position.distanceTo(me))[0]
  const ranged = rangedThreat({ hurtMsAgo: Date.now() - lastHurt, fighting: Boolean(fighting), armed, health: bot.health, archerNear: Boolean(archer), inWater: bot.entity.isInWater, meleeNear: nearbyHostiles(5).some(e => !ARCHERS.has(e.name)) })
  const sheltered = inShelter(me)
  if (ranged === 'charge' && sheltered) { holdAgainst(archer, me); return }
  if (ranged === 'charge') {
    fighting = archer
    fightStart = me.clone()
    // the charge is the point: the ground it crosses to reach the archer is owed to it on top of the leash
    chaseLeash = chargeLeash(fightStart, archer.position)
    equipBestWeapon().finally(() => bot.pvp.attack(archer))
    lastReflex = { kind: 'fighting', mob: archer.name, at: Date.now() }
    emit('fighting', { mob: archer.name, health: Math.round(bot.health), note: 'it shot me from afar: charging' })
    return
  }
  const crowd = crowdSize(nearbyHostiles(24).filter(e => !ignorableMob(e.name, where)).map(e => ({ name: e.name, dist: e.position.distanceTo(me) })))
  const hurtBy = Date.now() - (lastWound?.at ?? 0) <= 3000 ? lastWound?.nearby ?? [] : []
  const attackers = attackerCount({ crowd, seen: nearbyHostiles(5).map(e => e.name), hurtBy, hurtMsAgo: Date.now() - (lastWound?.at ?? 0) })
  const outmatched = shouldFlee({ armed, health: bot.health, attackers: Math.max(attackers, 1), armorPieces: [5, 6, 7, 8].filter(slot => bot.inventory.slots[slot]).length })
  // (3) what an armed body cannot outrun it fights: the run from a spider is what killed the body at health 2
  if (chaser && !fighting && fightNotFlee({ armed, mob: chaser.name })) { startFight(chaser, `a ${chaser.name} cannot be outrun: fighting it, not running`); return }
  // (1) at night under cover the body does not run out into the dark either, unless the mob is in the room with it
  const inTheRoom = chaser && chaser.position.distanceTo(me) <= 2.5
  // an archer that just hit me counts like a chaser: mid-charge rangedThreat is silent, and a patrol shot Jizo from 20 to 0 that way
  if (((chaser || (archer && Date.now() - lastHurt < 5000)) && outmatched) || ranged === 'flee') {
    if (sheltered && !inTheRoom) { holdAgainst(chaser ?? archer, me, Boolean(chaser)); return }
    if (startFlee(chaser ?? archer, me, undefined, { health: Math.round(bot.health), armed })) return
  }
  if (fighting && (!fighting.isValid || fighting.position.distanceTo(me) > (ARCHERS.has(fighting.name) ? 28 : 12))) {
    fighting = null
    fightStart = null
    bot.pvp.stop()
    resumeFollow()
  }
  // pvp walks the body after the mob, so the mob never gets far from it: the leash is measured from where the fight
  // began instead, and a fight that pulls the body down a hole is broken off before it becomes the cave it died in
  const overLeash = fighting && chaseBroken(fightStart, me, { leash: chaseLeash, mobDist: fighting.isValid !== false && fighting.position ? fighting.position.distanceTo(me) : Infinity })
  if (overLeash) {
    const mob = fighting.name
    const back = fightStart
    // the body dug its way down into this: walking back up a shaft it cannot climb left it standing there while a
    // zombie killed it (02:26Z), so a break-off that was a drop digs and bridges its way out, and gets longer to do it
    const climbing = breakOffDigs(back, me)
    const wasDigging = digging
    fighting = null
    fightStart = null
    chaseHeldUntil = Date.now() + (climbing ? 15000 : 10000)
    bot.pvp.stop()
    lastReflex = { kind: 'leashed', mob, at: Date.now() }
    emit('leashed', { mob, note: overLeash, back: `${Math.round(back.x)},${Math.round(back.y)},${Math.round(back.z)}` })
    if (climbing) useMoves(true)
    bot.pathfinder.setGoal(new goals.GoalNear(back.x, back.y, back.z, 2), false)
    setTimeout(() => {
      if (climbing) useMoves(wasDigging)
      if (!task && !followTarget) bot.pathfinder.setGoal(null); else resumeFollow()
    }, climbing ? 15000 : 6000)
    return
  }
  // and it does not simply pick the same fight up again the moment it stops: the walk back has to happen first
  if (!fighting && Date.now() >= chaseHeldUntil) {
    const threat = nearbyHostiles(4.5).filter(e => e.name !== 'creeper' && !NEVER_FIGHT.has(e.name))
      .sort((a, b) => a.position.distanceTo(me) - b.position.distanceTo(me))[0]
    if (threat && sheltered) { swingAt(threat, me); return }
    if (threat) startFight(threat)
  }
}
// a fight the reflex starts: pvp walks the body after the mob from here on, on the leash measured from this spot
function startFight (threat, note) {
  fighting = threat
  fightStart = bot.entity.position.clone()
  chaseLeash = CHASE_LEASH
  equipBestWeapon().finally(() => bot.pvp.attack(threat))
  lastReflex = { kind: 'fighting', mob: threat.name, at: Date.now() }
  emit('fighting', { mob: threat.name, health: Math.round(bot.health), ...(note ? { note } : {}) })
}
// card 0f110bb5 (1). From shelter at night nothing is followed: a mob in reach is swung at from where the body stands, and an
// archer is not charged; the gap it shoots through gets a block if the pack has one, and the driver is told either way
let heldAt = 0
let lastSwing = 0
function swingAt (entity, me) {
  if (Date.now() - heldAt > 10000) { heldAt = Date.now(); emit('holding', { mob: entity.name, health: Math.round(bot.health), note: holdNote({ mob: entity.name, melee: true }) }) }
  if (Date.now() - lastSwing < 600 || entity.position.distanceTo(me) > 3.5) return
  lastSwing = Date.now()
  const swing = () => bot.lookAt(entity.position.offset(0, (entity.height ?? 1.8) * 0.8, 0), true).catch(() => {}).then(() => bot.attack(entity))
  if (/_sword$|_axe$/.test(bot.heldItem?.name ?? '')) swing(); else equipBestWeapon().finally(swing)
}
// door: the mob is a chaser outside the room, not an archer (the note says which; a spider was called a shooter, 09-26)
async function holdAgainst (entity, me, door = false) {
  if (!entity || Date.now() - heldAt < 10000) return
  heldAt = Date.now()
  bot.pathfinder.setGoal(null)
  const cell = plugCells(me, entity.position).map(({ dx, dy, dz }) => me.floored().offset(dx, dy, dz)).find(p => bot.blockAt(p)?.boundingBox !== 'block')
  const plugged = cell ? await fillCell(cell, capBlock()?.name) : false
  emit('holding', { mob: entity.name, health: Math.round(bot.health), ...(plugged ? { plugged: `${cell.x},${cell.y},${cell.z}` } : {}), note: holdNote({ mob: entity.name, plugged, door }) })
}

export async function equipBestWeapon () {
  dropMeal('a fight is starting: the sword goes in my hand')
  const order = ['netherite_sword', 'diamond_sword', 'iron_sword', 'stone_sword', 'golden_sword', 'wooden_sword',
    'netherite_axe', 'diamond_axe', 'iron_axe', 'stone_axe', 'wooden_axe']
  const item = order.map(n => bot.inventory.items().find(i => i.name === n)).find(Boolean)
  if (item) await bot.equip(item, 'hand').catch(() => {})
}

export function resumeFollow () {
  if (!followTarget) return
  const p = bot.players[followTarget]?.entity
  if (p) bot.pathfinder.setGoal(new goals.GoalFollow(p, 3), true)
}
export const surfaceWalkRuntime = makeSurfaceWalkRuntime({
  getBot: () => bot, Vec3, goals, makeMoves, cancelGuard,
  reportPerformance: (...args) => reportPerformance(...args),
  report: data => emit('surface_walk', data),
  dangerous: p => isNight(bot.time.timeOfDay) || Object.values(bot.entities).some(e => e.isValid && isHostile(e) && e.position.distanceTo(p ? new Vec3(p.x, p.y, p.z) : bot.entity.position) < 12)
})
export const boatRuntime = makeBoatRuntime({
  getBot: () => bot, getBoatLeashHolder: () => boatLeashHolder,
  Vec3, vecOf, goNear, findItem, inventoryCounts, pos, columnAbove, cancelGuard,
  getSwimStepTarget: () => swimStepTarget, setSwimStepTarget: value => { swimStepTarget = value }
})
const boatTravelRuntime = makeBoatTravelRuntime({
  getBot: () => bot, Vec3, cancelGuard, edibleCarried, driveBoat,
  readBoatState: a => boatRuntime.quick.boat_state(a),
  getLeashHolder: id => boatLeashHolder.get(id),
  reportPerformance: (...args) => reportPerformance(...args),
  report: progress => emit('boat_progress', progress)
})
const villagerRosterFile = path.join(ROOT, 'state', 'villagers.json')
const villagerRoster = makeVillagerRosterObserver({ file: villagerRosterFile, by: cfg.username })
const travelRuntime = makeTravelRuntime({ getBot: () => bot, Vec3, cancelGuard, edibleCarried, reportPerformance: (...args) => reportPerformance(...args) })
const ridingRuntime = makeRidingRuntime({
  getBot: () => bot, Vec3, cancelGuard, edibleCarried, driveHorse, surfaceWalk: surfaceWalkRuntime,
  reportPerformance: (...args) => reportPerformance(...args),
  goNear: async (entity, check) => { check(); await goNear(entity.position, 2.5); check() },
  report: progress => emit('riding_progress', progress)
})
export const villagerRuntime = makeVillagerRuntime({
  getBot: () => bot, Vec3, goNear, findItem, inventoryCounts, cancelGuard, emit,
  by: cfg.username,
  recordVillagerObservation: input => saveVillagerObservation(villagerRosterFile, input),
  getFeeding: () => feeding, setFeeding: value => { setFeeding(value) },
  currentVehicleId: boatRuntime.currentVehicleId
})
Object.assign(long, boatRuntime.long, boatTravelRuntime.long, travelRuntime.long, ridingRuntime.long, villagerRuntime.long, senseLong, moveLong, blockLong, itemLong, creatureLong, selfLong, controlLong)
Object.assign(quick, boatRuntime.quick, boatTravelRuntime.quick, travelRuntime.quick, ridingRuntime.quick, villagerRuntime.quick, watchesQuick, senseQuick, mapQuick, moveQuick, itemQuick, creatureQuick, selfQuick, controlQuick)

// A reflex cancellation (wedged, stalled, holing up, out of air, a death) no longer freezes the queue by itself:
// whether it should is now the job-policy.mjs decision job-scheduler.mjs makes once the cancelled job's own result
// settles (severeFailure: the body died, or the same job failing twice running).
function cancelTask (why) {
  const active = jobShelf.snapshot().active
  if (active != null) jobShelf.markCancelling(active, why)
  setGen(gen + 1)
  if (task) emit('task_cancelled', { id: task.id, name: task.name, why, ...(task.jobId ? { notify: false } : {}) })
  if (task) lastCancel = { id: task.id, why }
  task?.releaseFollow?.()
  followTarget = null
  setTask(null)
  fighting = null
  fightStart = null
  bot.pathfinder.setGoal(null)
  bot.pvp.stop()
  try { bot.collectBlock.cancelTask() } catch {}
}

// drowned once while mining at 4 hp; don't start risky work half dead unless told to
export const refusalFor = (name, args) => (['trades', 'trade'].includes(name) && bot.health <= 5 && !args.force
  ? `health is ${Math.round(bot.health)}: eat/rest first, or pass force=true`
  : refuseReason({ name, health: bot.health, force: args.force, sleeping: bot.isSleeping }))

let taskId = 0
let lastCancel = null
let lastReflex = null
export const jobShelf = createJobShelf(path.join(HOME, 'jobs.json'))
taskId = Math.max(0, ...jobShelf.snapshot().jobs.map(job => Number(job.id) || 0))
for (const recovered of jobShelf.snapshot().jobs.filter(job => job.status === 'interrupted' && !job.recoveryReported)) {
  emit('job_interrupted', { id: recovered.id, name: recovered.name, error: recovered.error })
  jobShelf.patch(recovered.id, { recoveryReported: true })
}
export let scheduler
const recentReflex = () => lastReflex && { ...lastReflex, agoMs: Date.now() - lastReflex.at }
// #128: every goto out of a 1x1 natural shaft fails in a second with "no walkable path", a goto one block away
// included. True, and useless: read once from the body's own cell, the answer is about the block it is ON
const passableAboutFeet = (through = () => false) => {
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return (dx, dy, dz) => { const block = bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz)); return block?.boundingBox !== 'block' || through(block) }
}
export const amBoxedIn = () => Boolean(bot?.entity) && boxedIn(passableAboutFeet())
// bamboo the pathfinder reads as walls (a fence-like thicket), though the server's offset stalks leave the body room to walk out between
export const amBoxedByBamboo = () => amBoxedIn() && !boxedIn(passableAboutFeet(block => block.name === 'bamboo'))
// a hole one block deep (card 94e6dcb1): the walk out of it is a jump, and a failed one reads as a distant obstacle
const amInHole = () => Boolean(bot?.entity) && inHole(passableAboutFeet())
// one block above a field, on a log in the rows (Jizo, 09-26 23:24Z): the way down is a drop onto farmland
const amPerched = () => {
  if (!bot?.entity) return false
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return perchedOverField((dx, dy, dz) => bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz))?.name)
}
// what the body can read off itself when a walk finds no path (src/navigation/cave-exit.mjs): no sky over the head and the goal up
// on the surface, water in or beside its cell (a dig walk breaks nothing beside a liquid), a protected zone round it
const noPathEvidence = () => {
  if (!bot?.entity) return {}
  const me = bot.entity.position
  const feet = feetCell(me, bot.entity.onGround)
  const beside = [[0, 0, 0], [0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]
  const wet = bot.entity.isInWater || beside.some(([dx, dy, dz]) => FLUIDS.has(bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz))?.name ?? ''))
  const goalY = typeof lastWalkGoal?.y === 'number' ? lastWalkGoal.y : null
  return { underground: bot.blockAt(me.offset(0, 1, 0))?.skyLight === 0, goalDy: goalY === null ? null : goalY - feet.y, wet, zoned: inAnyZone(zones, new Vec3(feet.x, feet.y, feet.z)) }
}
// the search's start with no move the walk keeps (path_to: noPath nodes=0 visited=1), named: src/lib/path.mjs noFirstMove
export const firstMoveNote = moves => {
  if (!bot?.entity || !moves?.firstMoves) return null
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return noFirstMove(moves.firstMoves({ ...feet, remainingBlocks: moves.countScaffoldingItems() }))
}
export const explainFailure = message => {
  const boxed = amBoxedIn()
  // appended, never instead: the sweeps' dig retry and the stuck count read the no-path words (src/farm/leg.mjs PATH_FAILURE)
  const stuckHere = /no path to the goal|no walkable path/i.test(message) ? firstMoveNote(bot.pathfinder.movements) : null
  const advice = noPathAdvice({ text: explainNoPath(explainInterrupt(message, recentReflex()), digging, boxed), dig: digging, boxed, holed: amInHole(), perched: amPerched(), ...noPathEvidence() })
  return stuckHere ? `${advice}. ${stuckHere}` : advice
}

// the cells around a bed (both halves), for bedExit
export function bedExits (bed) {
  const sides = [[1, 0], [-1, 0], [0, 1], [0, -1]]
  const isBed = p => /_bed$/.test(bot.blockAt(p)?.name ?? '')
  const halves = [bed, ...sides.map(([dx, dz]) => bed.offset(dx, 0, dz)).filter(isBed)]
  const open = p => { const b = bot.blockAt(p); return !b || b.boundingBox === 'empty' || isWoodDoor(b) }
  return halves.flatMap(h => sides.map(([dx, dz]) => h.offset(dx, 0, dz))).filter(p => !isBed(p))
    .map(p => ({ at: `${p.x},${p.y},${p.z}`, free: open(p) && open(p.offset(0, 1, 0)), lintel: bot.blockAt(p.offset(0, 2, 0))?.boundingBox === 'block' }))
}

// One shelf slot owns all body-changing work. Submission persists before this pump claims it;
// nested composite/flow api.act calls still invoke their registered action directly.
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

function submitJob (name, args, given = args, { urgent = false, verbose = false, notify = given?.automatic !== true } = {}) {
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

function jobControl (name, args) {
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

for (const file of libraryFiles()) {
  const name = compositeName(file)
  const mod = await import(pathToFileURL(path.join(LIBRARY_DIR, file)).href).then(m => m.default, e => ({ loadError: e.message }))
  const problem = mod?.loadError
    ? `library/${file}: ${mod.loadError}`
    : (long[name] || quick[name]) ? `library/${file}: ${name} is already a primitive, rename the file` : compositeError(name, mod)
  if (problem) { console.log(`[library] ${problem}`); emit('error', { message: problem }); continue }
  // instant: it only reads (or writes the map), so it goes in the quick table and never cancels a task that is running
  const table = mod.instant ? quick : long
  table[name] = args => runComposite(name, mod, args, (type, detail) => {
    if (task?.jobId) scheduler?.report(task.jobId, type, detail)
  })
  composites.set(name, mod)
  console.log(`[library] ${name}: ${mod.doc}`)
}

// an action nobody can look up may as well not exist: say so at start rather than let ./mc help quietly skip it
const undocumented = [...Object.keys(long), ...Object.keys(quick)].filter(name => !PRIMITIVES[name] && !composites.has(name))
if (undocumented.length) console.log(`[help] no catalogue line for: ${undocumented.join(' ')} (add one to PRIMITIVES in lib.mjs)`)
// a renamed action must point at one that exists, or the error sends the driver after a ghost
const served = name => Boolean(long[name] || quick[name])
const ghosts = Object.entries(RENAMED).filter(([was, now]) => !served(now) || served(was))
if (ghosts.length) console.log(`[help] RENAMED is stale: ${ghosts.map(([was, now]) => `${was} -> ${now}`).join(' ')} (lib.mjs)`)

// ---------------------------------------------------------------- HTTP API
http.createServer((req, res) => {
  let body = '', bodyBytes = 0, bodyTooLarge = false
  req.on('data', c => {
    bodyBytes += c.length
    if (bodyBytes > 2 * 1024 * 1024) { bodyTooLarge = true; body = ''; return }
    if (!bodyTooLarge) body += c
  })
  req.on('end', async () => {
    if (bodyTooLarge) {
      res.writeHead(413, { 'content-type': 'application/json' })
      res.end(JSON.stringify({ ok: false, error: 'request body exceeds 2 MiB' }))
      return
    }
    const name = new URL(req.url, 'http://x').pathname.slice(1)
    let out
    try {
      // a primitive's required argument is named before it runs (find_blocks name=oak_log used to answer "unknown block
      // name: undefined"), and the spelling a driver reaches for is folded into the one the action reads (src/needs.mjs)
      const { args: filled, error: lacking } = neededArgs(name, body ? JSON.parse(body) : {})
      const tracked = trackReads(filled)
      const args = tracked.args
      // help is answered even before the body is connected: a driver reads it first of all
      if (name === '' || name === 'help') out = { ok: true, ...quick.help(args) }
      else if (args.queue === false && args.interrupt !== true) out = { ok: false, error: 'queue=false is no longer supported; jobs queue by default. Use interrupt=true to cancel the current job safely before urgent work' }
      else if (!ready && !['events', 'job', 'jobs', 'cancel', 'resume', 'discard', 'stop'].includes(name)) out = { ok: false, error: offlineError(yieldUntil, Date.now()) }
      else if (name === 'resume' && !ready && yieldUntil > Date.now()) { clearTimeout(reconnectTimer); connect(); out = { ok: true, reconnecting: true } }
      else if (lacking) out = { ok: false, error: lacking }
      else if (['job', 'jobs', 'cancel', 'resume', 'discard'].includes(name)) out = jobControl(name, args)
      else if (name === 'stop') out = stopAllJobs()
      else if (name === 'quit') {
        const stopping = stopAllJobs()
        if (stopping.restorationPending) {
          out = { ok: false, cleanup: 'failed', error: stopping.restorationPending, note: 'body remains online for recovery' }
        } else if (stopping.stopped != null) {
          const settled = await scheduler.wait(stopping.stopped, 120000)
          if (!settled || !['completed', 'failed', 'cancelled', 'interrupted'].includes(settled.status)) {
            out = { ok: false, cleanup: 'pending', job: stopping.stopped, error: 'the active job has not finished cancellation cleanup; body remains online' }
          } else if (settled.status === 'failed') out = { ok: false, cleanup: 'failed', job: stopping.stopped, error: settled.error ?? settled.result?.error ?? 'job cleanup failed; body remains online for recovery' }
          else out = { ok: true, ...quick.quit(args) }
        } else out = { ok: true, ...quick.quit(args) }
      }
      else if (mayRunBesideOwner(name, { quick, long })) { out = { ok: true, ...(await quick[name](args)) } }
      else if (long[name] || quick[name]) {
        const { queue: _queue, interrupt = false, sync = false, verbose = false, wait, waitMs, ...rest } = args
        const waitForResult = sync || (name !== 'smelt' && wait === true)
        const waitDuration = Number.isFinite(Number(waitMs)) ? Math.max(0, Math.min(120000, Number(waitMs))) : 120000
        const actionArgs = name === 'smelt' && wait !== undefined ? { ...rest, wait } : rest
        lastDriven = Date.now()
        if (interrupt) {
          const { job: accepted, afterCleanup } = scheduler.interrupt({ name, args: actionArgs, given: tracked.given }, reason => cancelTask(reason), { verbose, notify: tracked.given?.automatic !== true })
          taskId = Math.max(taskId, accepted.id)
          out = { ok: true, status: 'queued', job: accepted.id, urgent: true, afterCleanup, held: jobShelf.snapshot().held?.reason }
          if (waitForResult) {
            const finished = await scheduler.wait(accepted.id, waitDuration)
            if (finished && ['completed', 'failed', 'cancelled', 'interrupted'].includes(finished.status)) out = { ok: finished.status === 'completed', job: accepted.id, ...finished.result }
            else out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, urgent: true, afterCleanup, held: jobShelf.snapshot().held?.reason }
          }
        } else {
          const accepted = submitJob(name, actionArgs, tracked.given, { verbose, notify: tracked.given?.automatic !== true })
          out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, held: jobShelf.snapshot().held?.reason }
          if (waitForResult) {
            const finished = await scheduler.wait(accepted.id, waitDuration)
            if (finished && ['completed', 'failed', 'cancelled', 'interrupted'].includes(finished.status)) out = { ok: finished.status === 'completed', job: accepted.id, ...finished.result }
            else out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, held: jobShelf.snapshot().held?.reason }
          }
        }
      }
      else out = { ok: false, error: didYouMean(name, [...Object.keys(long), ...Object.keys(quick)]) }
      const ignored = ignoredParams(tracked.unread(), out.ok, String(quick[name] ?? long[name] ?? ''))
      if (ignored && out.status !== 'running') out = { ...out, ignored }
    } catch (e) {
      out = { ok: false, error: e.message }
    }
    res.writeHead(200, { 'content-type': 'application/json' })
    res.end(JSON.stringify(out))
  })
}).listen(cfg.apiPort, '127.0.0.1', () => console.log(`control API on http://127.0.0.1:${cfg.apiPort}`))

process.on('uncaughtException', e => sayError(`uncaught: ${e.message}`, { at: stackTop(e.stack) }))
process.on('unhandledRejection', e => sayError(`unhandled: ${e?.message ?? e}`))

if (cfg.auth === 'microsoft' && !fs.existsSync(profileFile(HOME))) loginNeeded()
connect()
