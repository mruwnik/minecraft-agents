import { automaticBeds, carriedBedSpot, reflexPickups } from './lib/sleep.mjs'
import { scaffoldSide } from './scaffold/side.mjs'
import { centerStand } from './navigation/center-stand.mjs'
import { stalkShape, groveExit, steer } from './navigation/bamboo.mjs'
import { forestHiveClaim, hiveSmokeCampfire, silkTouchTool } from './tree/hives.mjs'
import { resolveLegend, hasPlan, parsePlacePlan, parseStructurePlan, legacyPlanStructure } from './lib/plan.mjs'
import { controlTrace } from './body/control-trace.mjs'
import { settleInventory } from './body/inventory-settle.mjs'
// Claude's Minecraft body.
// Fast reflexes (eating, armour, self-defence) live here; decisions arrive over a
// small localhost HTTP API (see README.md) and everything notable that happens is
// appended to events.jsonl so the planning side can follow along.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { pathToFileURL } from 'node:url'
import mineflayer from 'mineflayer'
import { installWorldClock } from './world-clock.mjs'
import { installTickEnd } from './tick-end.mjs'
import pf from 'mineflayer-pathfinder'
import collectBlock from 'mineflayer-collectblock'
import pvp from 'mineflayer-pvp'
import armorManagerMod from 'mineflayer-armor-manager'
import { loader as autoEat } from 'mineflayer-auto-eat'
import vec3 from 'vec3'
import AABB from 'prismarine-physics/lib/aabb.js'
import { restartAdvice } from './restart.mjs'
import { createSlowScanReporter, timedScan } from './performance.mjs'
import { isGreeting } from './chatter.mjs'
import { inventoryCompactPair } from './inventory/compact.mjs'
import { HOLE_HURT_MS, openGateWalk, markMove, planStands, doingText, tillWarning, parsePlan, planCells, planErrors, RENAMED, helpText, argsUsage, docText, PRIMITIVES, compositeError, leadTargetError, blindGates, enchantNames, itemsArg, enchantChoice, fencedIn, gateChange, fencePush, realCell, besideNames, noFooting, pitAdvice, chatText, wedgeReplant, thicketCost, leadPick, herdPassed, gatesByReach, holesLeft, penShaftRefusal, staleKey, bedExit, gateStepCost, eatJammed, eatFailure, uneatenMeal, eatRefusal, eatAllowed, eatHold, eatBackoff, mealToDrop, mealFailed, foodSort, noFoodEdge, penStance, stanceNote, eatRetryDue, afterTheMeal, errorRepeat, deathBy, deathReport, deathUnannounced, deathKit, outOfSight, herdOrder, ledReport, tagalongs, ledExtra, waterWary, stackTop, isBaby, progressed, crowdSize, dryCells, cropNames, openNow, strays, shutNow, didYouMean, scanCap, eatBelow, withDefaultItem, foodAway, gateLeak, smeltWait, giveReport, wedgeBreakable, wakeStep, bedtimeReport, deepestCell, unpenned, penCensus, droppedWalk, hurtCause, scanWhere, craftRoom, craftReport, GATE_OTHERS_NEAR, holeUpRefusal, mealTally, routeSummary, circling, CIRCLING_MS, coordsError, digRefusal, fluidsLeft, FLUIDS, scaffoldNote, scaffoldTakeBack, scaffoldBuilt, isAir, bedChoice, ownBed, nightPlan, automaticNightPlan, BED_RANGE, bedTrap, idleNudge, isGroundCover, looksBuilt, mineTargets, craftShortfall, placeObstacle, deadWalk, fillOutcome, penLeak, gatesLeftOpen, oversleeping, staleCode, codeVersion, mapRefusal, leadVerdict, clampedOffset, nudgeAway, creatureFood, CREATURE_FOOD, breedingFood, BREEDING_FOOD, flushCells, airReflex, openAbove, surfacingStalled, furnaceReport, trackReads, ignoredParams, depositWanted, peacefulTool, chaseVerdict, chaseBroken, fleeGoal, DIG_REACH, digFromHere, digPlan, chargeLeash, breakOffDigs, CHASE_LEASH, attackRefusal, fleeUnwinnable, fleeStep, fleeOscillating, fleeRange, fleeIntoCave, holeCells, holeUpVerdict, burrowPlan, holedUpNote, respawnPlan, FLEE_HOME, FLEE_GIVEUP_MS, NEVER_FIGHT, ENDERMAN_RANGE, brokenSlot, placeOutcome, placeMissed, strayFluid, equipSlot, inventorySlots, armorPoints, shouldFlee, ARCHERS, rangedThreat, plansFromOwnCell, missingTool, stepOffChoice, bedtime, feetCell, overMemory, placeAgainst, arrivalError, renderScan, inAnyZone, describePlaces, describePlace, markFields, matchPlaces, compact, pickFuel, isWedged, matchesProps, checkWatch, within, refuseReason, canPlaceFromHere, ignorableMob, explainInterrupt, isStalled, mayDig, explainNoPath, boxedIn, doorwayNode, buriedIn, nextSheep, occupiedBy, isNight, withdrawPlan, agentNames, splitPlayers, lateMeal, givePlan, shortNote, tooFarToGive, lyingFrom, GIVE_REACH, chestFree, leashable, leashPlan, leashedLine, loginYield, reconnectDelay, offlineError } from './lib.mjs'
import { makeEyes, YAWS } from './vision/eyes.mjs'
import { watchWindows } from './body/window-watch.mjs'
import { burrowSite, capChoice, holeUpAborted, mobHit, holeUpBlock, refusalNote, shelterNote, HOLE_STEP, HOLE_DEPTH, HOLE_MELEE } from './survival/holeup.mjs'
import { underRoof, walledIn, nightShelter, nightFleeStep, nightFleeGoal, retarget, fightNotFlee, attackerCount, plugCells, holdNote } from './survival/night.mjs'
import { addressedTo, whisperHint, offlineWhisper, splitSay, sayLimit, chatRefusal, heardWhisper } from './talk.mjs'
import { WORK_RANGE, noStanding, loadedAround, thinkBudget, goalDistance, THINK_CAP_MS, rimGoal } from './navigation/walk.mjs'
import { configureTerrainMoves, scaffoldingAvailable, climbableVinesAvailable } from './navigation/terrain-moves.mjs'
import { makeSurfaceWalkRuntime } from './navigation/surface-walk.mjs'
import { walkStandstill, walkProgress, WALK_PROGRESS_MS, blockName, frozenWalk, facingOff, aheadCells, serverSide, nearBy, frozenAdvice } from './navigation/stall.mjs'
import { addSample, stuckVerdict, nextEpisode, stuckField, stuckLine } from './navigation/stuck.mjs'
import { neededArgs } from './needs.mjs'
import { createJobShelf } from './job-shelf.mjs'
import { createJobScheduler } from './job-scheduler.mjs'
import { mayRunBesideOwner } from './job-policy.mjs'
import { airSample, freshAir, serverPosNote } from './survival/airlog.mjs'
import { surfaceWay, openingProgress, roofAt, SURFACE_SCAN } from './navigation/surface.mjs'
import { digLegs } from './navigation/dig-legs.mjs'
import { noPathAdvice, inHole, perchedOverField } from './navigation/cave-exit.mjs'
import { farmWalk, legFlags, stepsOff, noFirstMove, clearGoalOnFailure } from './lib/path.mjs'
import { spareTest } from './farm/leg.mjs'
import { climbShaft, climbBlocks, inPocket, descendingLeg, descentNote, ownCellRefusal } from './navigation/climb.mjs'
import { failedResult, deathLine, deathCancel } from './composite.mjs'
import { placeFaces } from './build/cover.mjs'
import { slabMergeRefusal } from './build/slab-merge.mjs'
import { executeFlow, executeLegacySteps, parseFlowEDN, resolveFlowAction } from './flow.mjs'
import { fetchFailure, stalledSince, fencedRefusal, wedgedIn, wedgedRefusal } from './fetch.mjs'
import { makeBoatRuntime } from './body/boat.mjs'
import { makeBoatTravelRuntime } from './body/boat-travel.mjs'
import { driveBoat } from './navigation/boat-travel.mjs'
import { makeTravelRuntime } from './body/travel.mjs'
import { makeRidingRuntime, horseState } from './body/riding.mjs'
import { driveHorse } from './navigation/horse.mjs'
import { makeVillagerRuntime } from './body/villager.mjs'
import { makeVillagerRosterObserver, saveVillagerObservation } from './villager/roster.mjs'
import { ROOT, HOME, cfg } from './body/home.mjs'
import { authDir, profileFile, loginAdvice } from './auth.mjs'
import { zones, saveZones, GATES_FILE, readPlaces, savePlaces, recent, emit, sayOnce, sayError } from './body/events.mjs'
import { LIBRARY_DIR, libraryFiles, compositeName, composites, CLI_ONLY, BANNED_FOOD, edibleCarried, runComposite } from './body/runner.mjs'
import { matcher, countsOf, chestTransfer, carried, inventoryCounts, inventoryQuiet, diffCounts, findItem, vecOf, dropsNear, sweepDrops, walkToDig, cellAt, goNear, findBlocksNear, findBlockByName, bedsNear, craftBatch, containerAt, leashHolderOf, onMyLeads, leadsCarried, leashCandidate, leashOne, unleashOne, leadWalk } from './body/helpers.mjs'

// the physics engine's own box comparison lets a hitbox that rounds 1e-14 past a block face walk into the block (see clampedOffset in lib.mjs)
const corners = box => ({ min: [box.minX, box.minY, box.minZ], max: [box.maxX, box.maxY, box.maxZ] })
for (const [axis, method] of ['computeOffsetX', 'computeOffsetY', 'computeOffsetZ'].entries()) {
  AABB.prototype[method] = function (other, offset) { return clampedOffset(corners(this), corners(other), axis, offset) }
}

export const { pathfinder, Movements, goals } = pf
export const { Vec3 } = vec3
export const reportPerformance = createSlowScanReporter({ emit })
const armorManager = armorManagerMod.default ?? armorManagerMod

// ---------------------------------------------------------------- bot lifecycle
export let bot = null
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
export let mcData = null
export let ready = false
let reflexes = true
let eyes = null
let openWindow = () => null
// where containerAt, craftBatch and the furnace actions say they are about to open a window, so the watcher reports
// the block they actually opened rather than a nearby-block guess (helpers.mjs imports this to call it)
export let declareOpening = () => {}
let followTarget = null
export let task = null // { id, name, gen, started }
let gen = 0
// a long action calls `const alive = cancelGuard()` when it starts and `alive()` in every loop: once it has been cancelled
// or superseded it must stop, or it keeps fighting the next command for the body
export const cancelGuard = () => { const mine = gen; return () => { if (gen !== mine) throw new Error('cancelled') } }
// A composite may finish restoring one job block after cancellation. This private token cannot be supplied by a CLI caller.
export const ROLLBACK_PLACE = Symbol('rollback-place')
let waitingForServer = false
let yieldUntil = 0 // while someone else is logged in as me, I stay off until then
let reconnectTimer = null
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
const eatOnce = async opts => {
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
let walkMoves = null
let digMoves = null
export let digging = false
// every cell the pathfinder aimed a scaffolding placement at during this task. Chani's cobblestone went that way twice with
// nothing in the reply to say so (#111), so a task now reports what it built beside its drops and takes back what it can reach
let scaffolded = []
// >0 while the `place` primitive is putting a block down on purpose: what lands then is a build, not scaffolding
let handPlacing = 0
// whether a dig walk must leave a block whole, whatever it is: the cells and the ground of the plan a farm sweep walks
// inside (goto spare= floor=, src/farm/leg.mjs spareTest). Set for one walk and cleared after it; looksBuilt keeps
// guarding everything else
const NONE_SPARED = () => false
let spared = NONE_SPARED
const FarmMovements = farmWalk(Movements)
export function makeMoves (dig) {
  const moves = new FarmMovements(bot)
  moves.allowParkour = true
  moves.canOpenDoors = true
  for (const block of Object.values(bot.registry.blocksByName)) if (plansFromOwnCell(block.name)) moves.emptyBlocks.add(block.id)
  // the pathfinder's list of gates it may open is older than cherry, mangrove, bamboo, pale oak, crimson and warped: to it those were walls
  // (Vivenna's and Aviendha's cherry gates: lead said "no way", walks went over the fence by parkour or not at all)
  for (const block of Object.values(bot.registry.blocksByName)) if (block.name.endsWith('_fence_gate')) moves.openable.add(block.id)
  for (const block of Object.values(bot.registry.blocksByName)) if (noFooting(block.name)) moves.fences.add(block.id)
  moves.canDig = dig
  Object.assign(moves, waterWary(dig))
  moves.allow1by1towers = dig
  if (!dig) moves.scafoldingBlocks = []
  // The pathfinder only knows fence gates; to it a door is a wall to smash. Call wooden doors walkable and let doorTick work the handle.
  const getBlock = moves.getBlock.bind(moves)
  moves.getBlock = (...at) => {
    const b = getBlock(...at)
    if (isWoodDoor(b)) return Object.assign(b, { safe: true, physical: false, height: at[0].y + at[2] })
    // and an OPEN gate is air to walk through, not the wall prismarine-block makes of it (see openGateWalk)
    const open = b?.name?.endsWith('_fence_gate') ? openGateWalk({ name: b.name, open: b.getProperties().open }) : null
    return open ? Object.assign(b, open) : b
  }
  const zoneCost = block => inAnyZone(zones, block.position) ? 100 : 0
  moves.exclusionAreasBreak.push(zoneCost)
  // nor anything that looks built, protected or not
  moves.exclusionAreasBreak.push(block => looksBuilt(block.name) ? 100 : 0)
  moves.exclusionAreasBreak.push(block => block.position && spared(block.position, block.name) ? 100 : 0)
  // Dig walks can place emergency footing as well as break obstructions; keep
  // that placement inside the same caller-authorized cells and bounds.
  moves.exclusionAreasPlace.push(block => block.position && spared(block.position, block.name) ? 100 : 0)
  moves.exclusionAreasPlace.push(zoneCost)
  moves.exclusionAreasStep.push(block => gateStepCost(block.name))
  moves.exclusionAreasStep.push(block => thicketCost(besideNames(block.position, (x, y, z) => bot.blockAt(new Vec3(x, y, z), false)?.name)))
  // collectBlock switches both of these off on the movements it is given; with them off a tunnel under gravel buried and killed me
  for (const guard of ['dontMineUnderFallingBlock', 'dontCreateFlow']) Object.defineProperty(moves, guard, { get: () => true, set () {} })
  configureTerrainMoves(moves, { blockAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)), scaffolding: scaffoldingAvailable(), climbableVines: climbableVinesAvailable() })
  return moves
}
export function useMoves (dig) {
  digging = dig
  bot.pathfinder.setMovements(dig ? digMoves : walkMoves)
}

// a device code asked for at runtime means the cached refresh token is gone (months of disuse): a background body
// cannot show it to anyone, and the reconnect loop would ask for a new one every ten seconds, so say so and stop.
// exit 6: tools/start-body writes the body_down line for any non-zero exit
const loginNeeded = () => {
  emit('login_needed', { advice: loginAdvice(HOME) })
  process.exit(6)
}

function connect () {
  ready = false
  yieldUntil = 0
  bot = mineflayer.createBot({
    host: cfg.host, port: cfg.port, username: cfg.username, version: cfg.version, auth: cfg.auth,
    ...(cfg.auth === 'microsoft' && { profilesFolder: authDir(HOME), onMsaCode: loginNeeded })
  })
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
    mcData = bot.registry
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
    ready = true
    waitingForServer = false
    scheduler?.pump()
    emit('spawned', { pos: pos(), dimension: bot.game.dimension, ...codeHere })
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
    ready = false
    // Whatever primitive/composite was mid-await is now bound to a socket that will never deliver its
    // event again: cancel/stop/discard only ever mark the shelf, none of them can make that promise
    // settle, and without this the owner slot stays wedged until the process itself restarts (card:
    // farm.build job 322 stuck at active= for hours after a creeper-interrupted reconnect).
    cancelTask(`disconnected: ${reason}`, { holdQueue: false })
    scheduler?.abandon(`disconnected: ${reason}`)
    reconnectTimer = setTimeout(connect, reconnectDelay(yieldUntil, Date.now()))
  })
}

// ---------------------------------------------------------------- reflexes
export const pos = () => bot?.entity ? roundVec(bot.entity.position) : null
const roundVec = v => ({ x: Math.round(v.x * 10) / 10, y: Math.round(v.y * 10) / 10, z: Math.round(v.z * 10) / 10 })
const isHostile = e => e.type === 'hostile' || e.kind === 'Hostile mobs'
function nearbyHostiles (range) {
  if (!bot?.entity) return []
  return Object.values(bot.entities).filter(e => e !== bot.entity && isHostile(e) && e.position.distanceTo(bot.entity.position) <= range)
}

export const isWoodDoor = b => Boolean(b?.name?.endsWith('_door')) && b.name !== 'iron_door'
const doorsIOpened = new Set()
const heldOpen = new Set() // gates opened with `toggle`: they stay open until toggled shut
const myClicks = new Map() // block -> when my own hand last clicked it
const MY_CLICK_MS = 1500 // a gate that moves this soon after my own click on it moved because of me
const othersToggled = new Map() // gate -> when a change that was not my doing last moved it: hands off for a minute
// everyone on the server but this body: bot.players is the tab list, so it holds players out of sight too
const onlinePlayers = () => Object.keys(bot.players).filter(n => n !== bot.username)
const otherPlayerNear = at => Object.values(bot.players).some(p => p.entity && p.username !== bot.username && p.entity.position.distanceTo(at) <= GATE_OTHERS_NEAR)
const doorAt = n => {
  const b = bot.blockAt(new Vec3(Math.floor(n.x), Math.floor(n.y), Math.floor(n.z)))
  return isWoodDoor(b) ? { x: b.position.x, y: b.position.y, z: b.position.z, half: b.getProperties().half } : null
}
// the shared code is loaded once, at start: tell the driver when it has changed since, once per batch of edits
const codeLoaded = Date.now()
// and WHICH code that was, read from git once at start and said in the join line. A body started between two saves of
// a shared tree runs half of somebody's change and throws something that is in nobody's diff (#140). Never fatal: a
// body with no git, or no repo, joins anyway and says it does not know.
const codeHere = (() => {
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
let doorBusy = false
let lastSteppedOff = 0
const gatesPassed = new Set() // fence gates this task walked through: their pens get a look for strays when it ends
let leading = false // animals are following me: doors and gates stay open behind me until they have caught up
let following = [] // the animals a lead is bringing along: a gate stays open until they are through it (herdPassed), then shuts at once
export const setLeading = v => { leading = v }
export const setFollowing = v => { following = v }
let luring = false // a lead is on, from its first step towards the animal: the food stays in my hand, gates or no gates
let feeding = false // feed is holding food out to an animal: it stays in my hand
let compactingInventory = false
// open wooden doors as we walk up to them, and shut the ones we opened once we're through
async function doorTick () {
  if (doorBusy) return
  const doorIds = mcData.blocksArray.filter(isWoodDoor).map(b => b.id)
  // fence gates: the pathfinder opens them itself but never shuts them, and an open gate empties a pen. A gate is mine to
  // shut only when my own walk or click opened it (the gates.log listener decides that): "any open gate I pass" shut the
  // gate the human had just opened, 16 ms after, again and again (Perrin's idle body, 13:30Z)
  const gateIds = mcData.blocksArray.filter(b => b.name.endsWith('_fence_gate')).map(b => b.id)
  const doors = findBlocksNear({ matching: [...doorIds, ...gateIds], maxDistance: 5, count: 8 }).map(p => bot.blockAt(p)).filter(b => (b.getProperties().half ?? 'lower') === 'lower')
  if (foodAway({ held: bot.heldItem?.name, luring, feeding, gateNear: doors.some(d => d.name.endsWith('_fence_gate')), eating: Boolean(bot.autoEat?.isEating) })) {
    console.log('[food away] tempting food in hand at a gate: put away, or the animals follow me out')
    doorBusy = true
    await bot.unequip('hand').catch(() => {})
    doorBusy = false
  }
  const todo = doors.find(d => {
    const near = d.position.offset(0.5, 0, 0.5).distanceTo(bot.entity.position) < 1.6
    const open = d.getProperties().open
    // doors too, not only gates: a door found open and walked through stayed open behind me all night (my hut; Miles's cottage let a zombie in)
    const otherNear = otherPlayerNear(d.position)
    if (near && open && isWoodDoor(d) && !otherNear && !heldOpen.has(String(d.position))) doorsIOpened.add(String(d.position))
    if (near && open && d.name.endsWith('_fence_gate')) gatesPassed.add(String(d.position))
    // moving = the walk still has a goal: the pathfinder stands still while it works a gate, and "stopped" then shut the gate in my own face, for ever
    const feet = bot.entity.position.floored()
    const inDoorway = feet.x === d.position.x && feet.z === d.position.z
    const moving = Boolean(bot.pathfinder.goal) || bot.pathfinder.isMoving()
    return openNow({ near, open, door: isWoodDoor(d), moving, inDoorway }) || shutNow({ near, open, mine: doorsIOpened.has(String(d.position)), held: heldOpen.has(String(d.position)), leading: leading && !herdPassed(bot.entity.position.toArray(), d.position.offset(0.5, 0, 0.5).toArray(), following.filter(e => e.isValid).map(e => e.position.toArray())), moving, inDoorway, reflexes, otherNear, otherToggledMsAgo: Date.now() - (othersToggled.get(String(d.position)) ?? -Infinity) })
  })
  if (!todo) return
  doorBusy = true
  await bot.activateBlock(todo).catch(() => {})
  await bot.waitForTicks(3).catch(() => {})
  // believe the gate, not the click: a click that failed (out of reach at a sprint) used to strike the gate off my list, and it stayed open
  if (bot.blockAt(todo.position)?.getProperties().open) doorsIOpened.add(String(todo.position))
  else doorsIOpened.delete(String(todo.position))
  doorBusy = false
}

export let fighting = null
// where the body stood when the current fight began, and the leash that measures from it (#105)
let fightStart = null
let chaseHeldUntil = 0
let chaseLeash = CHASE_LEASH
let surfacing = false
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
let diggingOut = false
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
const carriedBed = () => bot.inventory.items().find(i => i.name.endsWith('_bed'))
const carriedBedPlace = () => carriedBedSpot({
  feet: feetCell(bot.entity.position, bot.entity.onGround), cellAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)),
  zones, places: readPlaces(), me: cfg.username, residents: villagers()
})
// each bed the reflex puts down gets its own mark on the shared map, so a restart still knows to pick it up and a
// driver's own <me>-bed mark (and bed) is never touched
const reflexBeds = () => readPlaces().filter(p => p.reflex === true && p.by === cfg.username)
const unmark = name => savePlaces(readPlaces().filter(p => p.name !== name))
async function placeReflexBed (item, { x, y, z, facing }) {
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
let lastDriven = Date.now()
let lastBedTry = 0
let bedFailures = 0
let bedWalkFailed = false // a walk to the own bed failed or came up short tonight: go straight to placement, not retried till the next night
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
let stuckNow = null
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
let lastServerPos = null // where the server last PUT the body (it only speaks up when it disagrees with the client)
let frozenFor = null // the stillFrom a frozen walk was already reported for
let lastFrozen = null // the last frozen walk: when, where and what the evidence blamed, for a task to own the failure (escort)
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
const capBlock = () => capItems()[0] ?? null
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
async function pillarUp (stopped, steps = 3, explicitItem = null) {
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
const leaveBed = () => {
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
  // #97: an enderman killed Ganesha's body at its own door in five seconds. Nothing here wins that fight, so one that
  // comes within arm's reach is backed away from exactly as a creeper is, before the reflex below can think of fighting it
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

async function equipBestWeapon () {
  dropMeal('a fight is starting: the sword goes in my hand')
  const order = ['netherite_sword', 'diamond_sword', 'iron_sword', 'stone_sword', 'golden_sword', 'wooden_sword',
    'netherite_axe', 'diamond_axe', 'iron_axe', 'stone_axe', 'wooden_axe']
  const item = order.map(n => bot.inventory.items().find(i => i.name === n)).find(Boolean)
  if (item) await bot.equip(item, 'hand').catch(() => {})
}

function resumeFollow () {
  if (!followTarget) return
  const p = bot.players[followTarget]?.entity
  if (p) bot.pathfinder.setGoal(new goals.GoalFollow(p, 3), true)
}

// ---------------------------------------------------------------- actions
// "long" actions take over the body; starting a new one cancels the previous.
// using a tool on the ground: hoe -> farmland, shovel -> dirt_path. One block (x y z) or many (blocks=[{x,y,z},...])
const GROUND_WORK = {
  till: { tool: '_hoe', from: ['dirt', 'grass_block', 'dirt_path'], to: 'farmland', verb: 'tilled', missing: 'no hoe: craft item=wooden_hoe (2 planks + 2 sticks)' },
  path: { tool: '_shovel', from: ['dirt', 'grass_block', 'coarse_dirt', 'podzol'], to: 'dirt_path', verb: 'paved', missing: 'no shovel: craft item=wooden_shovel (1 plank + 2 sticks)' }
}
async function workGround (a, work) {
  const tool = bot.inventory.items().find(i => i.name.endsWith(work.tool))
  if (!tool) throw new Error(work.missing)
  let done = 0
  const skipped = []
  const worked = []
  const alive = cancelGuard()
  // one bad cell (stone in the row, a block on top) must not throw away the rest of the batch, nor the count of what was done
  for (const b of a.blocks ?? [a]) {
    alive()
    const p = vecOf(b)
    const skip = why => skipped.push({ at: `${p.x},${p.y},${p.z}`, why })
    const unreachable = await goNear(p, WORK_RANGE).then(() => null, e => e)
    if (unreachable) { skip(`cannot get within reach: ${unreachable.message}`); continue }
    const block = bot.blockAt(p)
    if (block?.name === work.to) continue
    if (!work.from.includes(block?.name)) { skip(`can't turn ${block?.name ?? 'nothing'} into ${work.to}`); continue }
    const cover = bot.blockAt(p.offset(0, 1, 0))
    if (isGroundCover(cover?.name ?? '')) await bot.dig(cover)
    await bot.equip(tool, 'hand')
    await bot.activateBlock(bot.blockAt(p))
    await bot.waitForTicks(5)
    const now = bot.blockAt(p)?.name
    if (now !== work.to) {
      const above = bot.blockAt(p.offset(0, 1, 0))?.name
      // a crop cannot stand without farmland under it: if one grew back here while the click was still landing, the
      // ground already IS farmland and this cell is done, not failed - a cached local read just still says otherwise
      if (work.to === 'farmland' && cropNames.includes(above)) { done++; worked.push(p); continue }
      skip(`still ${now}: ${above && above !== 'air' ? above : 'nothing'} is on top of it`)
      continue
    }
    done++
    worked.push(p)
  }
  const outcome = placeOutcome(done, skipped, work.verb)
  if (outcome.error) throw new Error(outcome.error)
  if (work.to !== 'farmland' || !worked.length) return outcome
  // dry, unplanted farmland is grass again within minutes: two agents took that for a till that lied (BUGS.md 09-19)
  const waters = findBlocksNear({ point: worked[0], matching: mcData.blocksByName.water.id, maxDistance: 24, count: 200 }).map(w => w.toArray())
  const dry = dryCells(worked.map(w => w.toArray()), waters)
  // wet or not, farmland with nothing planted in it does not last: the warning always comes
  return { ...outcome, [dry.length ? 'dry' : 'advice']: tillWarning(dry.length, worked.length) }
}

// the walk itself: goto sets the cells spared from digging round it
async function gotoWalk (a) {
  const wriggled = await wriggleOut()
  let walked = { legs: 1 }
  if (a.place) {
    const p = readPlaces().find(q => q.name === a.place)
    if (!p) throw new Error(`no place called ${a.place}; see ./mc places`)
    walked = await walkLegs({ x: p.x, y: p.y, z: p.z }, a.range ?? 2, a.into === true)
  } else if (a.player) {
    const e = bot.players[a.player]?.entity
    if (!e) throw new Error(`can't see ${a.player}`)
    await bot.pathfinder.goto(new goals.GoalFollow(e, a.range ?? 2))
  } else if (coordsError(a, a.y !== undefined)) {
    throw new Error(coordsError(a, a.y !== undefined))
  } else if (a.y === undefined) {
    await bot.pathfinder.goto(new goals.GoalNearXZ(a.x, a.z, a.range ?? 1))
    // an x/z goal is met at any depth, and a walk that may not dig likes caves: say so rather than let the driver assume the surface
    if (bot.blockAt(bot.entity.position.offset(0, 1, 0))?.skyLight === 0) return { pos: pos(), ...(wriggled && { note: wriggled }), underground: 'no sky above you: an x/z goal is met at any depth. For a spot on the surface pass y= as well' }
  } else {
    walked = await walkLegs({ x: a.x, y: a.y, z: a.z }, a.range ?? 1, a.into === true)
  }
  const note = [wriggled, walked.note].filter(Boolean).join('; ')
  return { pos: pos(), ...(walked.legs > 1 && { legs: walked.legs }), ...(note && { note }) }
}
const surfaceWalkRuntime = makeSurfaceWalkRuntime({
  getBot: () => bot, Vec3, goals, makeMoves, cancelGuard,
  reportPerformance: (...args) => reportPerformance(...args),
  report: data => emit('surface_walk', data),
  dangerous: p => isNight(bot.time.timeOfDay) || Object.values(bot.entities).some(e => e.isValid && isHostile(e) && e.position.distanceTo(p ? new Vec3(p.x, p.y, p.z) : bot.entity.position) < 12)
})
const boatRuntime = makeBoatRuntime({
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
const villagerRuntime = makeVillagerRuntime({
  getBot: () => bot, Vec3, goNear, findItem, inventoryCounts, cancelGuard, emit,
  by: cfg.username,
  recordVillagerObservation: input => saveVillagerObservation(villagerRosterFile, input),
  getFeeding: () => feeding, setFeeding: value => { feeding = value },
  currentVehicleId: boatRuntime.currentVehicleId
})

export const long = {
  ...boatRuntime.long,
  ...boatTravelRuntime.long,
  ...travelRuntime.long,
  ...ridingRuntime.long,
  ...villagerRuntime.long,
  async goto (a) {
    if (bot.vehicle) throw new Error('cannot walk while mounted; use the vehicle controller or confirm a safe dismount first')
    if (a.surface !== undefined) return surfaceWalkRuntime.walk(a)
    spared = spareTest(a)
    try { return await gotoWalk(a) } finally { spared = NONE_SPARED }
  },

  async dig (a) {
    const p = vecOf(a)
    const checkSafeHive = (block, smokeAt) => {
      const place = readPlaces().find(saved => saved.name === a.place)
      if (!a.safe_hive || !forestHiveClaim(place, cfg.username, p, block?.name)) throw new Error('safe hive destruction requires a known hive inside this body\'s owned forest plan')
      const smoke = hiveSmokeCampfire((x, y, z) => {
        const b = bot.blockAt(new Vec3(x, y, z))
        return b ? { name: b.name, properties: b.getProperties?.() ?? {} } : null
      }, { x: p.x, y: p.y, z: p.z })
      if (!smoke || smoke.x !== smokeAt?.x || smoke.y !== smokeAt?.y || smoke.z !== smokeAt?.z) throw new Error('safe hive destruction refused: exact campfire smoke column is not verifiably lit and clear')
    }
    if (a.safe_hive === true) checkSafeHive(bot.blockAt(p), a.smoke)
    if (a.silk_touch === true) {
      const place = readPlaces().find(saved => saved.name === a.place)
      const initial = bot.blockAt(p)
      if (!forestHiveClaim(place, cfg.username, p, initial?.name)) throw new Error('Silk Touch hive pickup requires a known nest inside an owned forest plan')
      if (!silkTouchTool(bot.inventory.items())) throw new Error('Silk Touch tool required to move this hive with bees intact')
    }
    const refusal = digRefusal(bot.blockAt(p)?.name, [1, 2, 3].map(dy => bot.blockAt(p.offset(0, dy, 0))?.name), a.wet === true)
    if (refusal) throw new Error(refusal)
    // everything the cell can say from here is said before the body moves (card 150b3ee1: 148 s walking to a cell that was air).
    // The arm reaches 4.5 from the eyes: walking to within 3 of every cell was a path search every few crops of a harvest
    const read = () => {
      const block = bot.blockAt(p)
      const needed = block && missingTool(block.harvestTools, bot.inventory.items().map(i => i.type), id => bot.registry.items[id].name)
      return { block, name: block?.name, needed }
    }
    const here = read()
    const byHand = a.by_hand === true
    const walk = digPlan({ ...here, near: digFromHere(bot.entity.position, p), byHand }) === 'walk'
    if (walk) await walkToDig(p)
    const cell = walk ? read() : here
    const step = digPlan({ ...cell, near: true, byHand })
    if (step === 'air') return { already: 'air' }
    if (step === 'tool') throw new Error(`${cell.name} needs a ${cell.needed} or better: you carry none, craft one first (or by_hand=true breaks it for no drop)`)
    const block = cell.block
    if (a.safe_hive === true) checkSafeHive(block, a.smoke)
    if (a.silk_touch === true) {
      const place = readPlaces().find(saved => saved.name === a.place)
      if (!forestHiveClaim(place, cfg.username, p, block?.name)) throw new Error('Silk Touch hive target changed or left the owned forest plan')
      const tool = silkTouchTool(bot.inventory.items())
      if (!tool) throw new Error('Silk Touch tool required to move this hive with bees intact')
      await bot.equip(tool, 'hand')
      if (!silkTouchTool([bot.heldItem])) throw new Error('Silk Touch tool was not equipped; hive retained')
    } else await bot.tool.equipForBlock(block)
    await bot.dig(block)
    if (block.name === 'lectern') villagerRuntime.invalidateOffers()
    // batch=true: one cell of a sweep (farm.harvest). No wait for the drop and no chase after it: one collect follows the sweep
    if (a.batch) return { dug: block.name, at: `${p.x},${p.y},${p.z}` }
    // the drop of a gate or fence stays where it fell (Ganesha dug a gate and crafted a new one; my three fences lay behind the wall): fetch it, or say where it lies
    await bot.waitForTicks(8)
    const lying = () => Object.values(bot.entities).filter(e => e.name === 'item' && e.position.distanceTo(p.offset(0.5, 0.5, 0.5)) <= 2.5)
    if (lying().length) await sweepDrops(5).catch(() => {})
    const left = lying()[0]?.position.floored()
    // the @x,y,z of the reply is where the body stands: name the cell that was dug, so nobody works from the wrong one
    return { dug: block.name, at: `${p.x},${p.y},${p.z}`, ...(left ? { dropLeft: `its drop still lies at ${left.x},${left.y},${left.z}: go nearer, then collect` } : {}) }
  },

  // clear {x1..z2, keep:[names]}: dig out a box from the top down (demolition, site levelling). Beds and
  // containers are always kept. Only for what is yours to remove.
  async clear (a) {
    const lo = new Vec3(Math.min(a.x1, a.x2), Math.min(a.y1, a.y2), Math.min(a.z1, a.z2))
    const hi = new Vec3(Math.max(a.x1, a.x2), Math.max(a.y1, a.y2), Math.max(a.z1, a.z2))
    if ((hi.x - lo.x + 1) * (hi.y - lo.y + 1) * (hi.z - lo.z + 1) > 400) throw new Error('box too big: 400 blocks at most')
    const keep = name => /_bed$|chest$|furnace$|crafting_table$|barrel$/.test(name) || (a.keep ?? []).includes(name)
    let dug = 0
    const fluids = {}
    const alive = cancelGuard()
    for (let y = hi.y; y >= lo.y; y--) {
      for (let x = lo.x; x <= hi.x; x++) {
        for (let z = lo.z; z <= hi.z; z++) {
          alive()
          const block = bot.blockAt(new Vec3(x, y, z))
          if (!block || block.name === 'air' || keep(block.name)) continue
          // a fluid never finishes breaking: one water cell used to hang the whole sweep (#110)
          if (FLUIDS.has(block.name)) { fluids[block.name] = (fluids[block.name] ?? 0) + 1; continue }
          await goNear(block.position, 3)
          await bot.tool.equipForBlock(block)
          await bot.dig(block)
          dug++
        }
      }
    }
    const wet = fluidsLeft(fluids)
    return { dug, ...(wet ? { fluid: wet } : {}) }
  },

  // till {x,y,z}: turn dirt or grass into farmland with any hoe you carry (for mending or extending a farm)
  async till (a) { return workGround(a, GROUND_WORK.till) },
  async path (a) { return workGround(a, GROUND_WORK.path) },
  // bone meal on crops, saplings or grass (grass grows flowers around it). One block (x y z) or many (blocks=[{x,y,z},...])
  async fertilize (a) {
    const carried = () => bot.inventory.items().filter(i => i.name === 'bone_meal').reduce((n, i) => n + i.count, 0)
    const before = carried()
    if (!before) throw new Error('no bone_meal: craft item=bone_meal (1 bone gives 3)')
    const alive = cancelGuard()
    for (const b of a.blocks ?? [a]) {
      alive()
      const meal = bot.inventory.items().find(i => i.name === 'bone_meal')
      if (!meal) break
      await goNear(vecOf(b), 3)
      await bot.equip(meal, 'hand')
      await bot.activateBlock(bot.blockAt(vecOf(b)))
      await bot.waitForTicks(5)
    }
    return { used: before - carried() }
  },

  async place (a) {
    // `place` is the block verb; the shared map is `places`. Asking this one for a marked place by name used to read as
    // "place a block called starter-pen" and fail on a missing item=, so it is sent next door instead.
    if (a.name !== undefined && a.item === undefined && a.block === undefined) {
      throw new Error(`place puts a block down; to look up the place called ${a.name} on the shared map use places name=${a.name}`)
    }
    // block= is what mine and scan call it, and agents guess it here too (Arren: "no undefined in inventory")
    const blocks = withDefaultItem(a.blocks ?? [a], a.item ?? a.block)
    const unnamed = blocks.find(b => !b.item)
    if (unnamed) throw new Error(`place needs item=<name> for every block (none given for ${unnamed.x},${unnamed.y},${unnamed.z})`)
    let placed = 0
    // what really stands in each cell afterwards: the reply's @x,y,z is where the BODY is, and a driver read it as the
    // block he had just placed (AhuraMazda dug someone else's pressure plate that way)
    const done = []
    const alive = a[ROLLBACK_PLACE] ? () => {} : cancelGuard()
    // a cell that cannot be reached or has nothing to attach to yet is skipped and tried once more at the end (its neighbours may exist by then)
    class Skip extends Error {}
    const already = new Set()
    const placeOne = async b => {
      const p = vecOf(b)
      const existing = bot.blockAt(p)
      // a slab placed against a cell that already holds a bottom slab merges into a double block, no gap for water
      // left underneath: check before occupiedBy even, since the merge risk is real whatever occupiedBy would say
      // (jizo-melon-patch, 09-26; see src/build/slab-merge.mjs for the full story)
      const merge = slabMergeRefusal({ x: p.x, y: p.y, z: p.z }, existing, b.item)
      if (merge) throw new Skip(merge)
      const state = occupiedBy(existing, b.item)
      const verifyDirection = () => {
        const stood = bot.blockAt(p)
        if (stood?.name !== b.item) return false
        const props = stood.getProperties?.() ?? {}
        if (b.facing && props.facing !== b.facing) return false
        if (b.item.endsWith('_bed')) {
          if (props.part !== 'foot') return false
          const offsets = { north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0] }
          const direction = offsets[props.facing]
          if (!direction) return false
          const head = bot.blockAt(p.offset(...direction))
          const headProps = head?.getProperties?.() ?? {}
          return head?.name === b.item && headProps.part === 'head' && headProps.facing === props.facing
        }
        return true
      }
      if (state === 'skip') {
        if ((b.item.endsWith('_bed') || b.item.endsWith('_fence_gate')) && !verifyDirection()) throw new Skip('existing bed halves or gate facing do not match the requested placement')
        already.add(`${b.x},${b.y},${b.z}`); return
      }
      // on lumpy ground part of a wall is often terrain already: skip that cell and build the rest (Aviendha's pen, 09-19)
      if (state === 'blocked') throw new Skip(`${existing.name} is already there`)
      if (state === 'clear') { await goNear(p, 3); await bot.dig(existing) }
      // only for what is not a block (occupiedBy dealt with those): a crop, a flower, or the wrong ground for a seed
      const obstacle = state === 'free' && existing ? placeObstacle(b.item ?? a.item, existing.name, bot.blockAt(p.offset(0, -1, 0))?.name) : null
      if (obstacle) throw new Skip(obstacle)
      // walking is only needed when the block is out of reach or inside our own body; route searches on rough ground can time out
      if (!canPlaceFromHere(bot.entity.position, p)) {
        // the body's own cell in a 1-wide shaft: no cell beside it to place from, and the search below took 5 s to say "cannot get
        // within reach" (card 2b2d1f65). A niche to the side first, which climb digs
        const own = ownCellRefusal({ feet: feetCell(bot.entity.position, bot.entity.onGround), target: { x: p.x, y: p.y, z: p.z }, boxed: amBoxedIn() })
        if (own) throw new Skip(own)
        // the goal is a head within DIG_REACH of a face of the cell: from one up and four across that is 4.3, so a lane every eight rows
        // serves a field. Judged before the search: a cell walled in by crops has no such node, and A* took 5 s to say so (card 1ccb0ea1)
        const nowhere = noStanding(cellAt, p, WORK_RANGE)
        if (nowhere) throw new Skip(nowhere)
        const unreachable = await bot.pathfinder.goto(new goals.GoalPlaceBlock(p, bot.world, { range: DIG_REACH })).then(() => null, e => e)
        if (unreachable) throw new Skip('cannot get within reach')
      }
      await bot.equip(findItem(b.item ?? a.item), 'hand')
      const before = bot.blockAt(p)?.name
      // the neighbour to click decides a slab's half before the cursor does: the top of the block below always gives a
      // bottom slab, so a top slab (a channel cover) is placed against a side or the block above (see cover.mjs)
      // against= is the one neighbour to click: a ladder or a wall torch takes its facing from the face it goes on
      const faces = placeFaces(b).map(f => new Vec3(...f))
      const against = placeAgainst(faces.map(f => bot.blockAt(p.plus(f))))
      if (!against) throw new Skip('nothing to place against')
      const face = faces[against.index]
      // a click on a bed, chest or door uses it instead of placing: sneak when there is nothing plainer to click
      bot.setControlState('sneak', against.sneak)
      if (against.sneak) await bot.waitForTicks(2)
      if (b.facing || b.half) {
        // stairs, logs' cousins, furnaces, doors: the block takes its direction from where the player looks.
        // facing=south means "looking south while placing" (a stair then climbs towards the south)
        if (b.facing && YAWS[b.facing] === undefined) throw new Error('facing must be north, south, east or west')
        if (b.facing) await bot.look(YAWS[b.facing] * Math.PI / 180, 0, true)
        await bot.waitForTicks(3) // the new rotation only reaches the server with the next position packet
        handPlacing++
        await bot._genericPlace(bot.blockAt(p.plus(face)), face.scaled(-1), { forceLook: 'ignore', half: b.half ?? 'bottom' }).finally(() => { handPlacing-- })
        await bot.waitForTicks(4)
        const directionalPartial = b.item.endsWith('_bed') || b.item.endsWith('_fence_gate')
        if (directionalPartial ? !verifyDirection() : bot.blockAt(p)?.boundingBox !== 'block') throw new Error(`placing ${b.item ?? a.item} at ${p} did not take with the requested facing and parts`)
      } else {
        // "the block is still air": out of the server's reach, or our own body is in the cell
        handPlacing++
        const refused = await bot.placeBlock(bot.blockAt(p.plus(face)), face.scaled(-1)).then(() => null, e => e).finally(() => { handPlacing-- })
        // believe the world, not the click, both ways round: a fence that joins its neighbours comes back as another state than the one asked for and
        // reads as refused though it stands (Ganesha: placed=0 for three fences); and a click the server quietly drops resolves as if it had worked,
        // which is how a sweep once reported a bed planted and left it bare (09-22). So always look at the cell afterwards.
        await bot.waitForTicks(3)
        const missed = placeMissed(before, bot.blockAt(p)?.name)
        if (missed) { bot.setControlState('sneak', false); throw new Skip(refused ? 'the server refused it (out of reach, or you stand in it)' : missed) }
      }
      bot.setControlState('sneak', false)
      placed++
      const stands = bot.blockAt(p)?.name
      if (stands && !isAir(stands)) done.push({ x: p.x, y: p.y, z: p.z, name: stands })
      if (stands === 'lectern') villagerRuntime.invalidateOffers()
    }
    const attempt = async list => {
      const skipped = []
      for (const b of list) {
        alive()
        const failure = await placeOne(b).then(() => null, e => e)
        if (failure && !(failure instanceof Skip)) throw failure
        if (failure) skipped.push({ b, why: failure.message })
      }
      return skipped
    }
    const secondTry = await attempt((await attempt(blocks)).map(s => s.b))
    const outcome = placeOutcome(placed, secondTry.map(s => ({ at: `${s.b.x},${s.b.y},${s.b.z}`, why: s.why })), 'placed', already.size, done)
    if (outcome.error) throw new Error(outcome.error)
    return outcome
  },

  async craft (a) {
    const item = mcData.itemsByName[a.item]
    if (!item) throw new Error(`unknown item ${a.item}`)
    const count = a.count ?? 1
    let table = null
    if (!bot.recipesFor(item.id, null, 1, null).length) {
      const tp = findBlockByName('crafting_table', 32)[0]
      if (tp) { await goNear(tp, 2); table = bot.blockAt(tp) }
    }
    const recipe = bot.recipesFor(item.id, null, 1, table)[0]
    if (!recipe) {
      const all = bot.recipesAll(item.id, null, table ?? true)
      if (!all.length) throw new Error(`no recipe for ${a.item}`)
      const needs = r => Object.fromEntries(r.delta.filter(d => d.count < 0).map(d => [mcData.items[d.id].name, -d.count]))
      const short = craftShortfall(all.map(needs), inventoryCounts())
      const noTable = all[0].requiresTable && !table
      throw new Error(`can't craft ${a.item}: ${[noTable && 'needs a crafting table within 32 blocks', short && `you are short of ${short}`].filter(Boolean).join('; ') || 'you seem to carry everything (counts out of step? open a chest or retry)'}`)
    }
    // Crafting clicks race the server's state updates through ViaBackwards, so a craft can be silently
    // rejected and leave our local inventory wrong. Craft one batch at a time, let the server's resync land
    // (reopening the table forces one), and retry until the item count has really gone up.
    const have = () => inventoryCounts()[a.item] ?? 0
    const start = have()
    const before = inventoryCounts()
    const ingredients = recipe.delta.filter(d => d.count < 0).map(d => mcData.items[d.id].name)
    // A fixed wait after the click was the whole of #133. The server's answer can land after it, so an accepted batch
    // read as a failure and the loop crafted it AGAIN: that is how a shears craft ate two iron ingots and still said
    // nothing was made. Wait for the count to MOVE, up to five seconds, and stop waiting the moment it does.
    const settleTo = async target => { for (let i = 0; i < 25 && have() < target; i++) await bot.waitForTicks(4) }
    const recipeSettled = async () => { await inventoryQuiet(); return bot.recipesFor(item.id, null, 1, table)[0] }
    // what the ingredients really cost, read back at the end: the difference between a free retry and a real loss
    const spent = () => Object.fromEntries(ingredients.map(name => [name, Math.max(0, (before[name] ?? 0) - (inventoryCounts()[name] ?? 0))]))
    // before calling ingredients lost, look on the ground: Chani's 16 planks were lying by the table the whole time
    const giveUp = async why => {
      await inventoryQuiet()
      const onGround = () => dropsNear(6).filter(d => ingredients.includes(d.item))
      const fell = onGround().map(d => d.item)
      if (fell.length) await sweepDrops(6)
      const lying = onGround().map(d => `${d.item}@${d.x},${d.y},${d.z}`)
      throw new Error(craftReport({ item: a.item, count, made: have() - start, spent: spent(), why, fell, lying }).error)
    }
    for (let attempt = 1; have() < start + count; attempt++) {
      const full = craftRoom({ freeSlots: bot.inventory.emptySlotCount(), stacks: bot.inventory.items().filter(i => i.name === a.item).map(i => i.count), stackSize: item.stackSize, batch: recipe.result.count, item: a.item, made: have() - start, count })
      if (full) throw new Error(full)
      if (attempt > Math.ceil(count / recipe.result.count) + 5) await giveUp('the server kept rejecting the craft')
      // the last batch's grid and cursor may still be on their way back to the pockets: a recipe the pockets cannot
      // fill is asked for again once they are quiet, before the ingredients are called gone
      const r = bot.recipesFor(item.id, null, 1, table)[0] ?? await recipeSettled()
      if (!r) await giveUp('the ingredients ran out')
      const target = have() + r.result.count
      await craftBatch(r, table)
      await settleTo(target)
      await inventoryQuiet()
    }
    return craftReport({ item: a.item, count, made: have() - start })
  },

  async smelt (a) {
    const block = await containerAt(a, ['furnace', 'blast_furnace', 'smoker'])
    const furnace = await bot.openFurnace(block)
    try {
      const count = a.count ?? 1
      if (a.fuel) {
        const f = findItem(a.fuel)
        // only what the job burns: the rest of the stack is of more use in my pockets than in a furnace
        const needed = a.fuelCount ?? pickFuel([{ name: f.name, count: f.count }], count)?.count ?? count
        await furnace.putFuel(f.type, null, Math.min(needed, f.count))
      }
      if (a.item) { const input = findItem(a.item); await furnace.putInput(input.type, null, Math.min(count, input.count)) }
      await bot.waitForTicks(5)
      const waiting = furnace.inputItem()?.count ?? 0
      if (!waiting) throw new Error('nothing in the furnace to smelt')
      // no fuel named and none burning: feed it from the inventory, or say so instead of waiting for nothing
      if (!a.fuel && !furnace.fuelItem() && !(furnace.fuel > 0)) {
        const pick = pickFuel(bot.inventory.items().map(i => ({ name: i.name, count: i.count })), waiting)
        if (!pick) throw new Error('no fuel: carry coal, charcoal, planks or logs')
        await furnace.putFuel(findItem(pick.name).type, null, pick.count)
      }
      const deadline = Date.now() + (a.wait ?? count * 11 + 5) * 1000
      const verdict = () => smeltWait({ got: furnace.outputItem()?.count ?? 0, wanted: count, night: isNight(bot.time.timeOfDay), timedOut: Date.now() >= deadline })
      while (verdict() === 'wait') await new Promise(r => setTimeout(r, 1000))
      const night = verdict() === 'night'
      if (furnace.outputItem()) await furnace.takeOutput()
      // the night only passes when EVERYBODY sleeps, and a furnace needs nobody watching it
      if (night) return { stopped: `night fell with ${furnace.inputItem()?.count ?? 0} still to cook: the furnace cooks on without you. Sleep now (the others cannot skip the night while you are up), then furnace_take x=${block.position.x} y=${block.position.y} z=${block.position.z}` }
    } finally { furnace.close() }
    return {}
  },

  // enchant item=<name> [x= y= z= of the table] [slot=1-3, default: the dearest I can pay]: lapis comes from my inventory (slot n needs n lapis and xp level >= its offer)
  async enchant (a) {
    if (!a.item) throw new Error('enchant needs item= (what to enchant, from your inventory)')
    if (!bot.inventory.items().some(i => i.name === a.item && !i.enchants?.length)) throw new Error(`you carry no unenchanted ${a.item}`)
    const block = await containerAt(a, ['enchanting_table'])
    const table = await bot.openEnchantmentTable(block)
    try {
      // from the table's own window: its slot numbers are not my inventory's ("invalid operation")
      const item = table.items().find(i => i.name === a.item && !i.enchants?.length)
      const lapis = table.items().find(i => i.name === 'lapis_lazuli')
      await table.putTargetItem(item)
      if (lapis) await table.putLapis(lapis)
      // the offers arrive a moment after the item lies in the table
      for (let i = 0; i < 20 && !table.enchantments.some(e => e.level > 0); i++) await bot.waitForTicks(2)
      const { choice, error } = enchantChoice(table.enchantments, bot.experience.level, lapis?.count ?? 0, a.slot)
      if (error) { await table.takeTargetItem().catch(() => {}); throw new Error(error) }
      const cost = table.enchantments[choice].level
      await table.enchant(choice)
      const done = await table.takeTargetItem()
      return { enchanted: done.name, slot: choice + 1, asked: cost, got: enchantNames(done.enchants, id => bot.registry.enchantments?.[id]?.name), xpLevel: bot.experience.level }
    } finally { table.close() }
  },

  async furnace_take (a) {
    const block = await containerAt(a, ['furnace', 'blast_furnace', 'smoker'])
    const furnace = await bot.openFurnace(block)
    try {
      await bot.waitForTicks(5)
      if (furnace.outputItem()) await furnace.takeOutput()
      return furnaceReport({ input: furnace.inputItem(), fuel: furnace.fuelItem(), burning: furnace.fuel > 0 })
    } finally { furnace.close() }
  },

  // A Java scaffold extends upward when its SIDE is used with scaffolding.
  // Keep the body on dry ground: clicking the top from a deck instead extends
  // sideways, and sneaking to override that would start descending mid-click.
  async scaffold_extend (a) {
    if (![a.x, a.y, a.z, a.base_y].every(Number.isInteger) || a.y <= a.base_y || a.y - a.base_y > 48) throw new Error('scaffold_extend needs integer x/y/z/base_y with target 1..48 above its base')
    const target = vecOf(a)
    const base = new Vec3(a.x, a.base_y, a.z)
    const validate = () => {
      for (let y = a.base_y; y < a.y; y++) {
        const block = bot.blockAt(new Vec3(a.x, y, a.z))
        if (block?.name !== 'scaffolding' || Number(block.getProperties?.().distance ?? 0) !== 0) throw new Error('scaffold_extend requires a continuous supported vertical column')
      }
      if (![0, 1, 2].every(dy => isAir(bot.blockAt(target.offset(0, dy, 0))?.name))) throw new Error('scaffold_extend requires clear loaded target and headroom')
      const refusal = refusalFor('place', { x: a.x, y: a.y, z: a.z, item: 'scaffolding' })
      if (refusal) throw new Error(refusal)
      if (!digFromHere(bot.entity.position, base)) throw new Error('scaffold_extend: stand beside the base within reach')
      if (bot.entity.position.floored().x === a.x && bot.entity.position.floored().z === a.z) throw new Error('scaffold_extend: stand beside the column, not inside it')
    }
    validate()
    const alive = cancelGuard()
    const count = () => inventoryCounts().scaffolding ?? 0
    const before = count()
    if (!before) throw new Error('no scaffolding carried')
    await bot.equip(findItem('scaffolding'), 'hand')
    alive()
    validate()
    const dx = bot.entity.position.x - a.x - 0.5
    const dz = bot.entity.position.z - a.z - 0.5
    const face = Math.abs(dx) >= Math.abs(dz) ? new Vec3(Math.sign(dx), 0, 0) : new Vec3(0, 0, Math.sign(dz))
    bot.setControlState('sneak', false)
    handPlacing++
    try { await bot.activateBlock(bot.blockAt(base), face) } finally { handPlacing-- }
    await bot.waitForTicks(5)
    alive()
    if (bot.blockAt(target)?.name !== 'scaffolding' || count() >= before) throw new Error(`placing scaffolding did not take at ${a.x},${a.y},${a.z}`)
    return { placed: 1, at: `${a.x},${a.y},${a.z}` }
  },

  async center_work_stand (a) {
    return centerStand({ bot, Vec3, target: { x: a.x, y: a.y, z: a.z }, support: a.support, alive: cancelGuard() })
  },

  async scaffold_side (a) {
    handPlacing++
    try { return await scaffoldSide(a, {bot, Vec3, refusalFor, cancelGuard, inventoryCounts, findItem}) } finally { handPlacing-- }
  },

  async pillar_up (a) {
    const steps = a.steps ?? 1
    if (!Number.isInteger(steps) || steps < 1 || steps > 4) throw new Error('pillar_up steps must be 1..4')
    if (bot.vehicle || !bot.entity.onGround) throw new Error('pillar_up requires grounded feet and no vehicle')
    const start = bot.entity.position.clone()
    const cell = start.floored()
    const support = bot.blockAt(cell.offset(0, -1, 0))
    if (support?.boundingBox !== 'block') throw new Error('pillar_up requires a full solid support')
    const item = a.item ? findItem(a.item) : capBlock()
    if (!item || bot.registry.blocksByName[item.name]?.boundingBox !== 'block' || item.count < steps) throw new Error('pillar_up needs enough carried full building blocks')
    for (let y = cell.y; y <= cell.y + steps + 2; y++) {
      const b = bot.blockAt(new Vec3(cell.x, y, cell.z))
      if (!b || !isAir(b.name)) throw new Error(`pillar_up needs clear loaded headroom at ${cell.x},${y},${cell.z}`)
    }
    const occupied = Object.values(bot.entities).find(e => e !== bot.entity && e.name !== 'item' && e.position && Math.abs(e.position.x - start.x) < 0.8 && Math.abs(e.position.z - start.z) < 0.8 && Math.abs(e.position.y - start.y) < steps + 2)
    if (occupied) throw new Error(`pillar_up column is near entity ${occupied.id}`)
    // These are ordinary placements, checked before the first jump as well as
    // on each fresh cancellation check; the emergency helper keeps its defaults.
    const checkColumn = () => {
      for (let n = 0; n < steps; n++) {
        const placement = { x: cell.x, y: cell.y + n, z: cell.z, item: item.name }
        const refusal = refusalFor('place', placement)
        if (refusal) throw new Error(refusal)
      }
    }
    checkColumn()
    const alive = cancelGuard()
    bot.pathfinder.setGoal(null)
    // Explicit pillars belong to their caller's cleanup journal, not to the
    // pathfinder's end-of-task reclaim pass (which may run after replanting).
    handPlacing++
    try {
      await pillarUp(() => { alive(); checkColumn(); return false }, steps, item.name)
    } finally { handPlacing--; bot.setControlState('jump', false) }
    for (let tick = 0; tick < 20 && !bot.entity.onGround; tick++) {
      alive()
      await bot.waitForTicks(1)
    }
    alive()
    const raised = bot.entity.position.y - start.y
    const completed = Array.from({ length: steps }, (_, n) => bot.blockAt(cell.offset(0, n, 0)))
    const finalFeet = bot.entity.position.floored()
    if (raised < steps - 0.2 || !bot.entity.onGround || finalFeet.x !== cell.x || finalFeet.z !== cell.z || finalFeet.y !== cell.y + steps || completed.some(b => b?.name !== item.name || b.boundingBox !== 'block')) throw new Error(`pillar_up stopped after ${raised.toFixed(2)} blocks without a confirmed complete grounded pillar; inspect footing before retry`)
    return { from: roundVec(start), to: pos(), raised: Math.round(raised * 100) / 100, item: item.name }
  },

  async inventory_compact (a) {
    if (!mcData.itemsByName[a.item]) throw new Error('inventory_compact needs a known item=')
    const maxMoves = a.maxMoves ?? 72
    if (!Number.isInteger(maxMoves) || maxMoves < 1 || maxMoves > 72) throw new Error('maxMoves= must be 1..72')
    if (bot.currentWindow || bot.inventory.selectedItem) throw new Error('close the current window and empty the cursor before inventory_compact')
    if (bot.autoEat?.isEating) throw new Error('inventory_compact must wait for the bot to finish its meal')
    bot.pathfinder.setGoal(null)
    const total = () => inventoryCounts()[a.item] ?? 0
    const stacks = () => bot.inventory.items().filter(i => i.name === a.item).length
    const before = { count: total(), stacks: stacks(), freeSlots: bot.inventory.emptySlotCount() }
    let moves = 0, source = null
    compactingInventory = true
    try {
      while (moves < maxMoves) {
        if (!ready || bot.health <= 0 || flee || fighting || holingUp) throw new Error('inventory_compact interrupted by body safety')
        if (bot.currentWindow || bot.inventory.selectedItem) throw new Error('inventory window or cursor changed during compaction')
        const pair = inventoryCompactPair(bot.inventory.items(), a.item)
        if (!pair) break
        source = pair.source
        const sourceCount = bot.inventory.slots[source].count
        const destinationCount = bot.inventory.slots[pair.destination].count
        await bot.moveSlotItem(source, pair.destination)
        await bot.waitForTicks(3)
        if (bot.inventory.selectedItem || total() !== before.count ||
            (bot.inventory.slots[source]?.count ?? 0) !== sourceCount - pair.moved ||
            (bot.inventory.slots[pair.destination]?.count ?? 0) !== destinationCount + pair.moved) {
          throw new Error('inventory merge was not confirmed; inspect inventory before retrying')
        }
        source = null
        moves++
      }
      return { item: a.item, moves, count: total(), beforeStacks: before.stacks, afterStacks: stacks(), freedSlots: bot.inventory.emptySlotCount() - before.freeSlots }
    } finally {
      try {
        const cursor = bot.inventory.selectedItem
        if (cursor && source !== null && !bot.currentWindow) {
          const current = bot.inventory.slots[source]
          if (!current || (compatibleInventoryStacks(current, cursor) && current.count + cursor.count <= current.stackSize)) await bot.clickWindow(source, 0, 0)
        }
        if (bot.inventory.selectedItem) throw new Error('inventory_compact cursor restoration pending; inspect inventory before another action')
      } finally { compactingInventory = false }
    }
  },

  async deposit (a) {
    const wanted = depositWanted(a, bot.inventory.items().map(i => ({ name: i.name, count: i.count })))
    if (wanted.error) throw new Error(wanted.error)
    // same plan as withdraw, the other way round: what I carry is the source
    const { plan, corrected, eaten } = await chestTransfer(a, 'deposit', chest => withdrawPlan(wanted, countsOf(chest.items())))
    if (plan.short.length) throw new Error(`you carry less than asked (have/wanted): ${plan.short.join(' ')}; deposited what there was`)
    return { ...(corrected ? { corrected } : {}), ...(eaten ? { eaten } : {}) }
  },

  async withdraw (a) {
    const { plan, corrected, eaten } = await chestTransfer(a, 'withdraw', chest => withdrawPlan(itemsArg(a), countsOf(chest.containerItems())))
    if (plan.short.length) throw new Error(`chest has less than asked (have/wanted): ${plan.short.join(' ')}; took what there was`)
    return { ...(corrected ? { corrected } : {}), ...(eaten ? { eaten } : {}) }
  },

  async chest_contents (a) {
    const chest = await bot.openContainer(await containerAt(a))
    const items = {}
    for (const i of chest.containerItems()) items[i.name] = (items[i.name] || 0) + i.count
    // free= and slots= say the chest's room up front, so a deposit plan can choose a chest before walking there
    const free = chestFree(chest.slots, chest.inventoryStart)
    chest.close()
    return { items, free, slots: chest.inventoryStart }
  },

  async give (a) {
    const e = bot.players[a.player]?.entity
    if (!e) throw new Error(`can't see ${a.player}`)
    // within arm's reach first: a toss flies about three blocks, and from three off it lay where the player never came
    // (card 8c7b6652); the goal is half a block inside the reach so a diagonal cell still counts
    await bot.pathfinder.goto(new goals.GoalFollow(e, GIVE_REACH - 0.5))
    // a fleeing or walking player is gone again by the time we toss: keep the items rather than litter
    const tooFar = tooFarToGive(a.player, bot.entity.position.distanceTo(e.position))
    if (tooFar) throw new Error(tooFar)
    await bot.lookAt(e.position.offset(0, 1.2, 0))
    const item = findItem(a.item)
    const drops = () => Object.values(bot.entities).filter(d => d.name === 'item' && d.position.distanceTo(bot.entity.position) <= 8)
    const before = new Set(drops().map(d => d.id))
    const had = inventoryCounts()[item.name] ?? 0
    // every stack until the count is met (bot.toss crosses stacks): capped at the first stack, count=101 gave 64 and said taken=yes
    const { give: tossed } = givePlan({ count: a.count, carried: had })
    await bot.toss(item.type, null, tossed)
    // did it arrive? Watch my own drop: gone within 5 s = picked up (the toss itself said ok even when nobody got the bread)
    const mine = () => drops().filter(d => !before.has(d.id))
    await bot.waitForTicks(10)
    for (let i = 0; i < 18 && mine().length; i++) await bot.waitForTicks(5)
    // my own drop is mine again after 2 s: across a fence it falls at my feet and I pick it up myself, which looked like taken
    const cameBack = Math.max(0, (inventoryCounts()[item.name] ?? 0) - (had - tossed))
    // a drop still lying says how far it is from the player: "has not picked it up" read as a full inventory when it was distance
    const theirs = bot.players[a.player]?.entity?.position ?? null
    const short = shortNote({ item: item.name, asked: a.count ?? had, carried: had })
    return { ...giveReport(a.player, mine().map(d => lyingFrom(d.position, theirs, a.player)), cameBack), ...(short ? { short } : {}) }
  },

  // farming in one call: every ripe crop within `within` blocks is dug and replanted with its own seed, then the drops are picked up.
  // Works inside protected zones on purpose: crops are there to be harvested, and what it breaks it replants.
  // wool without killing: shears on up to `count` sheep nearby, then picks the wool up
  async shear (a) {
    const shears = bot.inventory.items().find(i => i.name === 'shears')
    if (!shears) throw new Error('no shears: craft item=shears (2 iron ingots)')
    const shorn = new Set()
    const alive = cancelGuard()
    // the sheep's colour byte (metadata 18 on this protocol, 17 on older ones; seen with `entity name=sheep`): bit 0x10 means already shorn
    const bare = e => [17, 18].some(i => (e.metadata?.[i] ?? 0) & 0x10)
    const sheepInSight = () => Object.values(bot.entities).filter(e => e.name === 'sheep' && !bare(e)).map(e => ({ id: e.id, entity: e, dist: e.position.distanceTo(bot.entity.position) }))
    while (shorn.size < (a.count ?? 4)) {
      alive()
      const sheep = nextSheep(sheepInSight(), shorn, a.within ?? 40)
      if (!sheep) break
      await bot.pathfinder.goto(new goals.GoalFollow(sheep.entity, 2))
      await bot.equip(shears, 'hand')
      await bot.useOn(sheep.entity)
      shorn.add(sheep.id)
      await bot.waitForTicks(10)
    }
    if (!shorn.size) throw new Error(`no sheep with wool within ${a.within ?? 40} blocks`)
    await sweepDrops(8)
    return { tried: shorn.size }
  },

  // give one animal the food it breeds on: walk to it, hold the food out, put it away again
  async feed (a) {
    if (!CREATURE_FOOD[a.mob]) throw new Error(`cannot feed ${a.mob}: one of ${Object.keys(CREATURE_FOOD).join(', ')}`)
    const foodName = creatureFood(a.mob, bot.inventory.items().map(i => i.name))
    if (!foodName) throw new Error(`a ${a.mob} eats ${CREATURE_FOOD[a.mob].join(' or ')}: you carry none`)
    const near = e => e.position.distanceTo(bot.entity.position)
    const animal = a.id === undefined
      ? Object.values(bot.entities).filter(e => e.name === a.mob && !isBaby(e.metadata)).sort((x, y) => near(x) - near(y))[0]
      : bot.entities[a.id]
    if (!animal?.isValid) throw new Error(a.id === undefined ? `no grown ${a.mob} about` : `the ${a.mob} with id ${a.id} is gone (despawned, unloaded or already led off)`)
    const carried = () => bot.inventory.items().filter(i => i.name === foodName).reduce((n, i) => n + i.count, 0)
    const before = carried()
    feeding = true
    try {
      await bot.pathfinder.goto(new goals.GoalFollow(animal, 2))
      const food = bot.inventory.items().find(i => i.name === foodName)
      if (!food) throw new Error(`the ${foodName} is gone from my hands`)
      await bot.equip(food, 'hand')
      await bot.useOn(animal)
      await bot.waitForTicks(10)
    } finally {
      feeding = false
      // food left in my hand walks the herd out through the gate at my heels (Vivenna lost a cow that way two mornings running)
      await bot.unequip('hand').catch(() => {})
    }
    // the game takes the food only from a grown one that is ready: `fed` says whether this one really ate
    return { fed: before - carried(), with: foodName, id: animal.id }
  },

  // bucket work: fill it from a water (or lava) source block, pour it out on top of a block
  async fill (a) {
    const bucket = bot.inventory.items().find(i => i.name === 'bucket')
    if (!bucket) throw new Error('no empty bucket: craft item=bucket (3 iron ingots)')
    const at = vecOf(a)
    const source = bot.blockAt(at)
    if (!['water', 'lava'].includes(source?.name)) throw new Error(`${source?.name ?? 'nothing'} at ${a.x},${a.y},${a.z} is not water or lava`)
    if (source.metadata !== 0) throw new Error(`the ${source.name} at ${a.x},${a.y},${a.z} is flowing: a bucket only fills from a still source block`)
    await goNear(at, 3)
    await bot.equip(bucket, 'hand')
    await bot.lookAt(at.offset(0.5, 0.5, 0.5), true)
    bot.activateItem()
    await bot.waitForTicks(10)
    const outcome = fillOutcome(bot.heldItem?.name)
    if (outcome.error) throw new Error(outcome.error)
    return outcome
  },
  async pour (a) {
    const bucket = bot.inventory.items().find(i => /^(water|lava)_bucket$/.test(i.name))
    if (!bucket) throw new Error('no full bucket: fill x= y= z= at a water source first')
    const at = vecOf(a)
    if (bot.blockAt(at)?.boundingBox !== 'block') throw new Error(`pour needs the solid block to pour ONTO: ${bot.blockAt(at)?.name ?? 'nothing'} at ${a.x},${a.y},${a.z} is not one`)
    await goNear(at, 3)
    await bot.equip(bucket, 'hand')
    await bot.lookAt(at.offset(0.5, 1, 0.5), true)
    // the server pours where my eyes really land: if something else is in the line of sight, the water ends up there (Kettricken's flooded crop)
    const seen = bot.blockAtCursor(5)
    if (!seen || !seen.position.equals(at) || seen.face !== 1) throw new Error(`cannot see the top of ${a.x},${a.y},${a.z} from here (looking at ${seen ? `${seen.name} at ${compact(roundVec(seen.position))}` : 'nothing'}): stand 1-2 blocks away with a clear view down onto it, then retry. Nothing was poured`)
    // look at the world before the click, so what appears that nobody asked for can be told from what was always there
    const fluid = bucket.name.replace('_bucket', '')
    const around = () => {
      const me = bot.entity.position.floored()
      const cells = []
      for (let dx = -2; dx <= 2; dx++) for (let dz = -2; dz <= 2; dz++) for (let dy = -1; dy <= 2; dy++) {
        const p = me.offset(dx, dy, dz)
        const block = bot.blockAt(p)
        if (block) cells.push({ x: p.x, y: p.y, z: p.z, name: block.name, level: block.getProperties?.().level })
      }
      return cells
    }
    const before = new Set(around().filter(c => c.name === fluid).map(c => `${c.x},${c.y},${c.z}`))
    bot.activateItem()
    await bot.waitForTicks(10)
    const above = bot.blockAt(at.offset(0, 1, 0))?.name
    if (above === fluid) return { holding: bot.heldItem?.name, above }
    // it emptied SOMEWHERE: a miss pours at my own eye level and floods everything downhill, so take it straight back
    const stray = strayFluid(before, around(), fluid)
    if (stray) {
      await bot.lookAt(new Vec3(stray.x + 0.5, stray.y + 0.5, stray.z + 0.5), true)
      bot.activateItem()
      await bot.waitForTicks(10)
    }
    const back = stray ? `it landed at ${stray.x},${stray.y},${stray.z} instead and I have scooped it back` : 'and I cannot see where it went'
    throw new Error(`no ${fluid} at ${a.x},${a.y + 1},${a.z} after pouring, ${back}: stand 1-2 blocks away on the same level as the block, with a clear view down onto its top, and pour again`)
  },

  // work a gate, door, trapdoor, lever or button by hand: the pathfinder opens gates on its way but never closes them behind me
  // right-click a block with whatever is in my hand: feeding a composter, ringing a bell, using a cake. `toggle` is the
  // one for doors, gates, trapdoors, levers and buttons, which have an open/shut state to aim at
  async use (a) {
    if (a.empty_hand === true && a.item) throw new Error('use: choose item= or empty_hand=true, not both')
    const alive = cancelGuard()
    const at = vecOf(a)
    if (!bot.blockAt(at) || bot.blockAt(at).name === 'air') throw new Error(`nothing at ${a.x},${a.y},${a.z} to use`)
    await goNear(at, 3)
    alive()
    if (a.empty_hand === true) await bot.unequip('hand')
    else if (a.item) await bot.equip(findItem(a.item), 'hand')
    alive()
    const block = bot.blockAt(at)
    const was = compact(block.getProperties?.() ?? {})
    await bot.activateBlock(block)
    await bot.waitForTicks(a.ticks ?? 6)
    const after = bot.blockAt(at)
    return { block: after?.name, was: was || undefined, now: compact(after?.getProperties?.() ?? {}) || undefined, holding: bot.heldItem?.name }
  },

  async toggle (a) {
    const at = vecOf(a)
    const block = bot.blockAt(at)
    if (!/_gate$|_door$|_trapdoor$|^lever$|_button$/.test(block?.name ?? '')) throw new Error(`${block?.name ?? 'nothing'} at ${a.x},${a.y},${a.z} is not a gate, door, trapdoor, lever or button`)
    const isOpen = () => { const props = bot.blockAt(at).getProperties(); return props.open ?? props.powered }
    if (a.open !== undefined && isOpen() === a.open) {
      if (a.open) heldOpen.add(String(at))
      else heldOpen.delete(String(at))
      return { block: block.name, now: isOpen() ? 'open' : 'closed', unchanged: true }
    }
    await goNear(at, 3)
    // worked by hand from here on: the gate reflex keeps off it. It may have opened it for me on my way here and be about to shut it,
    // and two clicks at once leave it the wrong way round (asked shut, left open): so look again after every click, up to 3 times
    doorsIOpened.delete(String(at))
    // held open from BEFORE the click: standing right beside the gate (within the reflex's 1.6) the reflex took the freshly opened gate for one I walked through
    // and shut it 60 ms later, three times (Kettricken 22:00Z; gates.log showed open-shut-open-shut)
    if (a.open !== false) heldOpen.add(String(at))
    for (let tries = 0; tries < 3 && (a.open === undefined ? tries === 0 : isOpen() !== a.open); tries++) {
      await bot.activateBlock(bot.blockAt(at))
      await bot.waitForTicks(tries ? 12 : 5)
    }
    if (a.open !== undefined && isOpen() !== a.open) throw new Error(`${block.name} is still ${isOpen() ? 'open' : 'closed'} after 3 tries: is someone standing in it?`)
    if (isOpen()) heldOpen.add(String(at))
    else heldOpen.delete(String(at))
    return { block: block.name, now: isOpen() ? 'open' : 'closed' }
  },

  // walk animals to a spot with their food in my hand: they follow from 10 blocks and are slower than I am, so stop for stragglers.
  // flock.lead decides where this goes, shuts a gate that stands open there and counts the pen afterwards; this is the walk itself
  async escort (a) {
    // on leads when carried (card 43a32481): pulled after me, the animals need see no food and a gate only has to open
    if (leashable(a.mob) && (leadsCarried() > 0 || ['horse', 'donkey', 'mule'].includes(a.mob) && onMyLeads().some(e => e.name === a.mob))) return leadWalk(a)
    const foodName = breedingFood(a.mob, bot.inventory.items().map(i => i.name))
    if (!BREEDING_FOOD[a.mob]) throw new Error(`cannot lead ${a.mob}: one of ${Object.keys(BREEDING_FOOD).join(', ')}`)
    if (!foodName) throw new Error(`a ${a.mob} follows ${BREEDING_FOOD[a.mob].join(' or ')}: you carry none`)
    const to = a
    if (to.x === undefined || to.y === undefined || to.z === undefined) throw new Error('escort needs x= y= z= (flock.lead takes place= too)')
    const near = e => e.position.distanceTo(bot.entity.position)
    // leading INTO a pen: the ones already in it stay where they are (the nearest cow was the one in the pen, 09-19)
    const pen = penAround(new Vec3(to.x, to.y, to.z).floored())
    const floor = pen?.enclosed ? pen.floor : null
    // The food in my hand is visible to every animal of its kind that can see me, not only to the ones I pick, so a
    // lead for two can walk a queue of six in and `with=2` says nothing about the other four (Perrin, item 17). Who
    // was standing at the goal BEFORE the walk has to be read before the walk - and only counts when the goal was in
    // sight then: from far enough off the pen's own animals are not loaded yet, and counting those as followers would
    // be a lie told confidently
    const toVec = new Vec3(to.x, to.y, to.z)
    const atGoal = e => floor ? unpenned(floor, [e], x => x.position).length === 0 : e.position.distanceTo(toVec) <= 4
    const standingThere = () => Object.values(bot.entities).filter(e => e.name === a.mob && e.isValid && atGoal(e)).map(e => e.id)
    const alreadyThere = bot.blockAt(toVec) ? standingThere() : null
    const free = () => unpenned(floor, Object.values(bot.entities).filter(e => e.name === a.mob), e => e.position)
    const inRange = free().filter(e => near(e) <= (a.within ?? 32)).sort((x, y) => near(x) - near(y))
    if (!inRange.length) throw new Error(`no ${a.mob} within ${a.within ?? 32} blocks${floor ? ' (not counting those already in the pen)' : ''}`)
    // one that stands in a pen is somebody's (my lead went to Aviendha's base for her cow). Only the nearest few are checked: a pen check in open country is a long walk
    const candidates = inRange.slice(0, 6).map(e => ({ id: e.id, at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`, penned: Boolean(penAround(e.position.floored())?.enclosed), grown: !isBaby(e.metadata), wedged: wedgedIn(bot.blockAt(e.position.floored()), e.position.y) }))
    // one wedged in a fence post cannot walk (card fc47bf28: two cows floored to the post's own cell): a free one first, and the wedge is the refusal only when nothing else is in range
    const walkable = candidates.filter(c => !c.wedged)
    const picked = leadPick(walkable.length ? walkable : candidates, a.penned === true, a.mob)
    if (picked.error) throw new Error(`no ${a.mob} to lead: ${picked.error}`)
    const first = inRange.find(e => e.id === picked.id)
    const alive = cancelGuard()
    luring = true
    try {
      await bot.equip(bot.inventory.items().find(i => i.name === foodName), 'hand')
      // one that stands in a pen: INTO the pen, to its own cell. Two blocks from it is also a spot outside the fence, and from there I walked off without ever
      // opening the gate (my sheep, with=0 twice: it stood at the shut gate and watched the wheat go)
      const pick = candidates.find(c => c.id === picked.id)
      const wedged = wedgedRefusal({ mob: a.mob, at: pick.at, block: pick.wedged })
      if (wedged) return { arrived: false, with: 0, why: wedged, pos: pos() }
      if (pick.penned) {
        // a pen the body is not in is the end of the lead, said before any walk: a walk into it follows partial paths
        // round the fence until the 12 s stall alarm cancels the task, and the fetch loop below would otherwise walk
        // three times to the nearest cell outside the fence and blame the animal (card fc47bf28)
        const fenced = fencedRefusal({ mob: a.mob, at: pick.at, pen: penAround(first.position.floored()), feet: pos() })
        if (fenced) return { arrived: false, with: 0, why: fenced, pos: pos() }
        await goNear(first.position.floored(), 0).catch(() => {})
      } else await bot.pathfinder.goto(new goals.GoalFollow(first, 2))
      alive()
      // the ones that come along are the ones close to me now, where they can see the food - the GROWN ones first, or a
      // lead for a breeding pair comes home with two calves and a herd that cannot breed (Perrin, from 24 cows)
      const herd = herdOrder(free().filter(e => near(e) <= 8).sort((x, y) => near(x) - near(y))
        .map(e => Object.assign(e, { grown: !isBaby(e.metadata) }))).slice(0, a.count ?? 2)
      const stroll = makeMoves(false)
      stroll.allowSprinting = false
      stroll.allowParkour = false
      bot.pathfinder.setMovements(stroll)
      // 1, not 2: two blocks from a spot inside a pen can be outside its fence
      const goal = new goals.GoalNear(to.x, to.y, to.z, a.range ?? 1)
      let holding = false
      let heldSince = 0
      let walking = false
      let walkingSince = 0
      let fetchesSinceProgress = 0
      let bestToGo = Infinity
      // a frozen walk from here on is one of the fetches: three of them beside a wheat field's fence (card fc47bf28)
      // ended "the cow will not follow" while the body itself had never moved
      const fetchingSince = Date.now()
      following = herd
      leading = true
      while (!goal.isEnd(bot.entity.position.floored())) {
        alive()
        const toGo = bot.entity.position.distanceTo(new Vec3(to.x, to.y, to.z))
        if (toGo < bestToGo - 8) { bestToGo = toGo; fetchesSinceProgress = 0 }
        const noPath = walking && lastPath?.status === 'noPath' && lastPath.at > walkingSince
        const verdict = leadVerdict({ distances: herd.filter(e => e.isValid).map(near), holding, heldFor: holding ? (Date.now() - heldSince) / 1000 : 0, fetchesSinceProgress, noPath })
        if (verdict === 'noway') { const along = herd.filter(e => e.isValid && near(e) <= 5); bot.pathfinder.setGoal(null); return { arrived: false, with: along.length, brought: ledReport(a.mob, along), toGo: Math.round(toGo), why: `no route on foot from here to ${to.x},${to.y},${to.z}. One of: the spot is not free floor to stand on; the gate is in a corner or something stands outside it (pen.check names such gates: blindGates=); a gap, drop or fence somewhere between here and there. The animals are with you: walk the way yourself (goto), fix what blocks it, then lead again`, pos: pos() } }
        if (verdict === 'giveup') { bot.pathfinder.setGoal(null); return { arrived: false, with: 0, why: fetchFailure({ mob: a.mob, frozen: stalledSince(lastFrozen, fetchingSince) }), pos: pos() } }
        if (verdict === 'lost') { bot.pathfinder.setGoal(null); return { arrived: false, with: 0, why: `the ${a.mob} are gone (despawned or unloaded)`, pos: pos() } }
        if (verdict === 'fetch') {
          bot.pathfinder.setGoal(null)
          walking = false
          fetchesSinceProgress++
          const straggler = herd.filter(e => e.isValid).sort((x, y) => near(y) - near(x))[0]
          // a fixed spot, not GoalFollow: a jostling animal makes the pathfinder replan every tick and never take a step
          await goNear(straggler.position.floored(), 2)
        }
        if (verdict === 'hold' && !holding) heldSince = Date.now()
        holding = verdict === 'hold'
        if (holding && walking) { bot.pathfinder.setGoal(null); walking = false }
        if (verdict === 'go' && !walking) { bot.pathfinder.setGoal(goal); walking = true; walkingSince = Date.now() }
        await bot.waitForTicks(5)
      }
      bot.pathfinder.setGoal(null)
      // into a pen: on to the cell furthest from them, or they stop 2.5 blocks behind me, in the gateway, and the gate shuts in their face
      const inPen = e => floor ? unpenned(floor, [e], x => x.position).length === 0 : near(e) <= 4
      const deepest = floor ? new Vec3(...deepestCell(floor, herd.find(e => e.isValid)?.position ?? bot.entity.position)) : null
      if (deepest) await goNear(deepest, 0).catch(() => {})
      // I am the faster one: give them time to catch up before counting who came along
      const waitForHerd = async ms => {
        const until = Date.now() + ms
        while (Date.now() < until && herd.some(e => e.isValid && !inPen(e))) { alive(); await bot.waitForTicks(5) }
      }
      await waitForHerd(deepest ? 8000 : 20000)
      // one that followed me along the OUTSIDE of the fence never finds the gate by itself, and the gate stood open for 20 s while I waited (a sheep of the human's
      // at 11,67,-117): go and get it once, the way back leads it through the gate
      const straggler = deepest && herd.find(e => e.isValid && !inPen(e))
      if (straggler) {
        await goNear(straggler.position.floored(), 2).catch(() => {})
        await goNear(deepest, 0).catch(() => {})
        await waitForHerd(12000)
      }
      // food out of sight, or the whole herd walks out again at my heels
      await bot.unequip('hand')
      const arrivals = herd.filter(e => e.isValid && inPen(e))
      const came = arrivals.length
      leading = false
      // counted BEFORE any walk to a gate: Ganesha's body reported from 170 blocks away, the cow long out of sight.
      // From the cells walked, not by eye: a cow beside the fence counted as inside. The gate we came through may still
      // stand open (the runner shuts it after this), and an open gate makes the whole pen read as open country
      const census = floor ? censusOf(floor) : {}
      const animals = herd.filter(e => e.isValid).map(e => `${a.mob}@${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`).join(' ')
      // one that never came may be unable to: say so, rather than let the driver lead it again and again (Ganesha, 6 times)
      const passable = v => bot.blockAt(v)?.boundingBox !== 'block'
      const riseAt = (feet, dx, dz) => [0, 1, 2, 3].find(up => passable(feet.offset(dx, up, dz)) && passable(feet.offset(dx, up + 1, dz))) ?? Infinity
      const stuck = herd.filter(e => e.isValid && !inPen(e)).map(e => e.position.floored())
        .map(feet => pitAdvice(a.mob, `${feet.x},${feet.y},${feet.z}`, [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => riseAt(feet, dx, dz)))).find(Boolean)
      // `with=3` was true and useless: nothing in it said that two of the three were calves and the pen now holds
      // nothing that can breed. brought= says which, and the note says when a calf came for want of anything else
      // and the ones that came uninvited, which no count of the ones I asked for could show
      const extra = alreadyThere ? ledExtra(a.mob, tagalongs(herd.map(e => e.id), alreadyThere, standingThere())) : {}
      return { arrived: true, with: came, brought: ledReport(a.mob, arrivals), animals, stuck, ...(picked.note ? { note: picked.note } : {}), ...extra, ...census, pos: pos() }
    } finally {
      leading = false
      luring = false
      following = []
      // also when cancelled or given up: food left in my hand drags every animal in sight after me
      await bot.unequip('hand').catch(() => {})
    }
  },

  // leads (card 43a32481): a lead in the hand used on an animal ties it to me, and it is pulled after me from then on
  async leash (a) {
    const named = a.id === undefined ? null : bot.entities[a.id]
    if (a.id !== undefined && !named) throw new Error(`nothing here with id ${a.id}: it is dead, or out of sight. animals gives the ids that are still there`)
    if (!named && !a.mob) throw new Error('leash needs mob= or id=')
    const mob = named ? named.name : a.mob
    const m = matcher(mob)
    const near = e => e.position.distanceTo(bot.entity.position)
    const candidates = (named ? [named] : Object.values(bot.entities).filter(e => e !== bot.entity && e.isValid && m(e.name ?? '') && near(e) <= (a.within ?? 16)))
      .filter(e => !leashHolderOf(e)).map(leashCandidate)
    const plan = leashPlan({ mob, leads: leadsCarried(), count: a.count ?? 1, candidates, allowPenned: named ? true : a.penned === true })
    if (plan.error) throw new Error(plan.error)
    for (const id of plan.take) await leashOne(bot.entities[id])
    return { leashed: leashedLine(onMyLeads().map(e => ({ name: e.name, id: e.id, ...e.position }))), leads: leadsCarried(), ...(plan.note ? { note: plan.note } : {}) }
  },
  // the leads come off: on the animals, each lead drops and is picked up; with a fence post at x= y= z=, every animal
  // on my leads is tied to a knot there and the leads stay on the knot
  async unleash (a) {
    const held = onMyLeads()
    if (!held.length) throw new Error('nothing is on my leads')
    if (a.x !== undefined) {
      const post = bot.blockAt(vecOf(a))
      if (!post || !/_fence$|_wall$/.test(post.name)) throw new Error(`${a.x},${a.y},${a.z} is ${post?.name ?? 'nothing'}, not a fence post or wall to tie a lead to`)
      await goNear(post.position, 2)
      await bot.unequip('hand')
      await bot.activateBlock(post)
      for (let i = 0; i < 20 && onMyLeads().length; i++) await bot.waitForTicks(1)
      const still = onMyLeads()
      if (still.length) throw new Error(`${still.length} of ${held.length} still on my leads after the knot: stand closer to ${a.x},${a.y},${a.z} and try again`)
      return { tied: held.length, at: `${a.x},${a.y},${a.z}`, note: 'the leads stay on the knot; break the knot (attack it, or right-click it empty-handed) to free them and drop the leads' }
    }
    const before = leadsCarried()
    for (const e of held) await unleashOne(e)
    await sweepDrops(8).catch(() => {})
    const lying = before + held.length - leadsCarried()
    return { unleashed: held.length, leads: leadsCarried(), ...(lying > 0 ? { leadsLying: `${lying} lead${lying === 1 ? '' : 's'} dropped and not picked up: collect` } : {}) }
  },

  async attack (a) {
    const m = matcher(a.mob)
    const named = a.id === undefined ? null : bot.entities[a.id]
    // by id (from animals) when the caller means ONE animal and not simply the nearest of its kind: culling the wrong
    // cow, or somebody else's from outside the fence, cannot be undone
    if (a.id !== undefined && !named) throw new Error(`nothing here with id ${a.id}: it is dead, or out of sight. animals gives the ids that are still there`)
    if (named && !m(named.name ?? '')) throw new Error(`id ${a.id} is a ${named.name}, not a ${a.mob}`)
    const target = named ?? Object.values(bot.entities)
      .filter(e => e !== bot.entity && e.type !== 'player' && m(e.name ?? ''))
      .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position))[0]
    if (!target) throw new Error(`no ${a.mob} in sight`)
    // #97: pvp aims at the target's head to swing, and an enderman's head is exactly what must not be aimed at
    const refused = attackRefusal(target.name)
    if (refused) throw new Error(refused)
    await equipBestWeapon()
    const start = bot.entity.position.clone()
    bot.pvp.attack(target)
    const verdict = await new Promise(resolve => {
      const iv = setInterval(() => {
        const v = chaseVerdict({ targetValid: target.isValid, hunting: !!bot.pvp.target, strayed: bot.entity.position.distanceTo(start), leash: a.leash ?? 24 })
        if (!v) return
        clearInterval(iv)
        resolve(v)
      }, 250)
    })
    if (verdict.gaveUp) bot.pvp.stop()
    return verdict
  },

  // several actions in one call; stops at the first failure and reports how far it got
  async run (a) {
    const program = typeof a.steps === 'string'
      ? parseFlowEDN(a.steps)
      : a.steps
    const observations = ['state', 'entity', 'block_at', 'boat_state', 'inventory', 'look_around', 'scan', 'animals', 'places', 'zones']
    const host = {
      actions: [...new Set([...Object.keys(long), ...Object.keys(quick)])],
      observations,
      alive: cancelGuard(),
      waitTicks: n => bot.waitForTicks(n),
      observe: async (name, args) => {
        const read = quick[name]
        if (!observations.includes(name) || typeof read !== 'function') throw new Error(`run: observation ${name} is unavailable`)
        return read(args)
      },
      onProgress: detail => {
        if (!task) return
        task.progress = { ...(task.progress ?? {}), ...detail }
        if (task.jobId) scheduler?.report(task.jobId, detail.waiting ? 'job_waiting' : 'job_progress', {
          ...detail, ...(detail.waiting ? {} : { waiting: false }), progress: task.progress
        })
      },
      act: async (name, args, legacyStep) => {
        const fn = resolveFlowAction(name, long, quick)
        const where = legacyStep ? `step ${legacyStep.index}/${legacyStep.total} (${name})` : `flow/${name}`
        if (!fn) throw new Error(legacyStep ? `${where}: unknown action` : `flow: no action called ${name}`)
        const refusal = refusalFor(name, args)
        if (refusal) throw new Error(`${where}: ${refusal}`)
        useMoves(mayDig(name, args))
        return Promise.resolve().then(() => fn(args)).catch(e => { throw new Error(`${where}: ${explainFailure(e.message)}`) })
      }
    }
    if (Array.isArray(program) && ['action', 'seq', 'when', 'any'].includes(program[0])) return executeFlow(program, host)
    return executeLegacySteps(program, host)
  },

  async sleep (a) {
    if (bot.vehicle) throw new Error('confirm a safe dismount before walking to a bed')
    const alive = cancelGuard()
    // a taken bed is passed over for the next one I may use (a shared bedroom: "the bed is occupied" was the end of the night)
    const occupied = new Set()
    // no bed within 32 is not the end of the night when one of my own is on the shared map within bed_range (default 200,
    // card bebf3a5f): walk there once (bed=<place>, else my nearest kind=bed mark; src/lib/sleep.mjs ownBed) and look again
    let walked = false
    let placed = false
    for (;;) {
      const { bed: p, error } = bedChoice(a.automatic ? automaticSleepBeds() : bedsNear(), zones, cfg.username, a.any === true && !a.automatic, occupied)
      if (error && a.automatic && !placed) {
        // walked there already and bedChoice still has nothing for me: that walk counted as failed, so tonight goes straight to placement
        if (walked) bedWalkFailed = true
        const from = pos()
        const plan = automaticNightPlan({
          near: false, bed: ownBed(readPlaces(), cfg.username, { from }), from,
          carried: Boolean(carriedBed()) && Boolean(carriedBedPlace()), walkFailed: bedWalkFailed || a.walk === false, hostileNear: nearbyHostiles(8).length > 0
        })
        if (plan.do === 'walk') {
          walked = true
          try {
            await goNear(plan.to, 2)
          } catch (e) {
            bedWalkFailed = true
            alive()
            if (/goal was changed|path was stopped/i.test(e.message)) throw e
          }
          continue
        }
        if (plan.do === 'place') {
          const item = carriedBed()
          const spot = item && nearbyHostiles(8).length === 0 ? carriedBedPlace() : null
          if (spot) {
            alive()
            placed = true
            await placeReflexBed(item.name, spot)
            continue
          }
        }
        throw new Error(plan.why ? `${error} (${plan.why})` : error)
      }
      if (error && !a.automatic && !walked && /^no bed within 32/.test(error)) {
        const from = pos()
        const plan = nightPlan({ near: false, bed: ownBed(readPlaces(), cfg.username, { bed: a.bed, from }), from, bedRange: a.bed_range ?? BED_RANGE })
        if (plan.do !== 'walk') throw new Error(`${error} (${plan.why})`)
        walked = true
        await goNear(plan.to, 2)
        continue
      }
      if (error) throw new Error(error)
      await goNear(p, 2)
      const taken = await bot.sleep(bot.blockAt(p)).then(() => false, e => { if (!/occupied/.test(e.message)) throw e; return true })
      if (!taken) return { trap: bedExit(bedExits(p)) ?? undefined }
      occupied.add(`${p.x},${p.y},${p.z}`)
    }
  }
}

// "quick" actions answer immediately and don't interrupt whatever the body is doing.
// ---------------------------------------------------------------- watches: "tell me when ..."
// A watch is checked every 5 s and writes one `watch_hit` event when its condition becomes true, so waiting costs
// the driver nothing. Kinds: block (with optional `where` properties), mob (any entity or player name), item (in
// my inventory). Centre is a fixed x,y,z or, without one, wherever I am.
const WATCH_FILE = path.join(HOME, 'watches.json')
let watches = fs.existsSync(WATCH_FILE) ? JSON.parse(fs.readFileSync(WATCH_FILE, 'utf8')) : []
const saveWatches = () => fs.writeFileSync(WATCH_FILE, JSON.stringify(watches, null, 1) + '\n')
const watchTarget = w => w.block ? `block ${w.block}` : w.mob ? `mob ${w.mob}` : `item ${w.item}`
const describeWatch = w => `${w.name}: ${w.atMost ? 'at most' : 'at least'} ${w.count ?? 1} ${watchTarget(w)}${w.where ? ' ' + compact(w.where) : ''}${w.item ? '' : ` within ${w.within ?? 16}${w.x === undefined ? ' of me' : ` of ${w.x},${w.y},${w.z}`}`}${w.repeat ? ' (repeats)' : ''}`

function countForWatch (w) {
  if (w.item) return { seen: inventoryCounts()[w.item] ?? 0 }
  const centre = w.x === undefined ? bot.entity.position : new Vec3(w.x, w.y, w.z)
  const within = w.within ?? 16
  if (w.mob) {
    const m = matcher(w.mob)
    const hits = Object.values(bot.entities).filter(e => e !== bot.entity && e.position && m(e.username ?? e.name ?? '') && e.position.distanceTo(centre) <= within)
    return { seen: hits.length, at: hits[0] && roundVec(hits[0].position) }
  }
  const m = matcher(w.block)
  const ids = Object.values(mcData.blocksByName).filter(b => m(b.name)).map(b => b.id)
  const hits = findBlocksNear({ matching: ids, maxDistance: within, count: 512, point: centre }).filter(p => matchesProps(bot.blockAt(p)?.getProperties(), w.where))
  return { seen: hits.length, at: hits[0] && roundVec(hits[0]) }
}

setInterval(() => {
  if (!ready || !watches.length) return
  const before = JSON.stringify(watches)
  watches = watches.flatMap(w => {
    const { seen, at } = countForWatch(w)
    const { fire, met } = checkWatch(w, seen)
    if (fire) emit('watch_hit', { name: w.name, seen, what: watchTarget(w), at })
    return fire && !w.repeat ? [] : [{ ...w, met }]
  })
  if (JSON.stringify(watches) !== before) saveWatches()
}, 5000)

export const quick = {
  ...boatRuntime.quick,
  ...boatTravelRuntime.quick,
  ...travelRuntime.quick,
  ...ridingRuntime.quick,
  ...villagerRuntime.quick,
  // the catalogue every driver starts from: each action with its arguments, and for a composite what hands the body back.
  // It is built from the dispatch tables themselves, so it cannot drift from what this body can actually do.
  help (a) {
    const served = name => Boolean(long[name] || quick[name]) || CLI_ONLY.includes(name)
    const entries = [
      ...Object.entries(PRIMITIVES).filter(([name]) => served(name)).map(([name, p]) => ({ name, ...p })),
      ...[...composites].map(([name, mod]) => ({ name, args: argsUsage(mod.args), doc: docText(mod.doc), stops: mod.stops ?? 'the usual hand-backs' }))
    ]
    const catalogue = helpText(a.topic, entries)
    return { text: `${catalogue}\n\nLong and body-changing actions queue by default and return a job ID. Add wait=true for a bounded synchronous reply; use interrupt=true to replace the current owner after cleanup. Inspect with job/jobs, cancel one ID, resume or discard a held queue, and stop to cancel all work.` }
  },

  // stop this body for good (logging off for the night, or done playing): answers first, then leaves the server and exits
  quit: () => {
    emit('quit', {})
    setTimeout(() => { bot.quit('quit'); process.exit(0) }, 200)
    return { note: 'body stopped: ./start brings it back' }
  },
  // debugging aid: what the pathfinder makes of a walk from here, without walking it. stroll=true: with lead's movements (no sprint, no parkour)
  path_to: (a) => {
    if (a.surface !== undefined) return surfaceWalkRuntime.preview(a)
    const fresh = makeMoves(a.dig === true)
    // live=true: plan with the movements the walks really use, and name every setting where they differ from a fresh set
    const moves = a.live ? bot.pathfinder.movements : fresh
    const differs = a.live ? Object.keys(fresh).filter(k => ['number', 'boolean', 'string'].includes(typeof fresh[k]) && fresh[k] !== moves[k]).map(k => `${k}:${moves[k]}`).join(' ') : ''
    if (a.stroll) { moves.allowSprinting = false; moves.allowParkour = false }
    // the same judgement a walk makes before it searches: a goal on the floor of a pit walks to its rim; range 0 at a ground block, or a cell walled in by crops, is a refusal, not a 5 s timeout
    const rim = rimGoal(cellAt, a, a.range ?? 0, { into: a.into === true, from: feetCell(bot.entity.position, bot.entity.onGround) })
    const aim = rim ?? { x: a.x, y: a.y, z: a.z, range: a.range ?? 0 }
    const nowhere = noStanding(cellAt, aim, aim.range)
    if (nowhere) return { status: 'refused', ms: 0, why: nowhere }
    const began = Date.now()
    const budget = thinkBudget(goalDistance(aim, bot.entity.position))
    let r = bot.pathfinder.getPathTo(moves, new goals.GoalNear(aim.x, aim.y, aim.z, aim.range), budget)
    // one call searches for a single 40 ms slice: go on the way a walk does, until it is done or the time a walk this long gets is over
    while (r.status === 'partial' && r.context && Date.now() - began < budget) r = Object.assign(r.context.compute(), { context: r.context })
    const last = r.path[r.path.length - 1]
    reportPerformance('path_to', Date.now() - began, { status: r.status, visited: r.visitedNodes, generated: r.generatedNodes, goal: aim, budget_ms: budget })
    const stuckHere = r.status === 'noPath' && r.visitedNodes <= 1 ? firstMoveNote(moves) : null
    return { status: r.status, ms: Date.now() - began, nodes: r.path.length, cost: Math.round(r.cost), visited: r.visitedNodes, ends: last ? `${last.x},${last.y},${last.z}` : 'here', ...(stuckHere && { why: stuckHere }), ...(rim && { note: rim.note }), gates: r.path.filter(n => n.toPlace?.some(t => t.useOne)).length, ...(a.route ? routeSummary(r.path) : {}), ...(a.live ? { differs: differs || 'nothing' } : {}) }
  },
  // debugging aid: the raw metadata of the nearest entities with this name (how does the server mark a shorn sheep?)
  entity: (a) => ({
    found: Object.values(bot.entities).filter(e => e !== bot.entity && matcher(a.name)(e.name ?? '') && (a.hostile !== true || isHostile(e)))
      .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position)).slice(0, a.count ?? 2)
      .map(e => ({ id: e.id, name: e.name, width: e.width, height: e.height, hostile: isHostile(e), ...(a.uuid ? { uuid: e.uuid, vehicleId: boatRuntime.currentVehicleId(e) } : {}), ...(['villager', 'cow', 'sheep', 'pig'].includes(e.name) ? { baby: isBaby(e.metadata), adult: !isBaby(e.metadata) } : {}), dist: Math.round(e.position.distanceTo(bot.entity.position)), at: e.position.floored().toArray().join(','), exact: e.position.toArray().map(n => Math.round(n * 100) / 100).join(','), metadata: JSON.stringify(e.metadata), attributes: e.attributes, equipment: (e.equipment ?? []).flatMap((item, slot) => item ? [{ slot, name: item.name, count: item.count }] : []) }))
  }),
  watch: (a) => {
    if (!a.name || [a.block, a.mob, a.item].filter(Boolean).length !== 1) throw new Error('watch needs name= and exactly one of block=, mob=, item=')
    if (a.block && !Object.keys(mcData.blocksByName).some(matcher(a.block))) throw new Error(`unknown block name: ${a.block}`)
    const { name, block, mob, item, where, count, atMost, within, x, y, z, repeat } = a
    watches = [...watches.filter(w => w.name !== name), { name, block, mob, item, where, count, atMost, within, x, y, z, repeat }]
    saveWatches()
    return { watching: describeWatch(a), now: countForWatch(a).seen }
  },
  unwatch: (a) => { watches = watches.filter(w => w.name !== a.name); saveWatches(); return { watches: watches.length } },
  watches: () => ({ text: watches.map(describeWatch).join('\n') || 'no watches' }),
  state () {
    const others = Object.values(bot.players).filter(p => p.username !== bot.username)
    const playersSeen = Object.fromEntries(others.map(p => [p.username, p.entity ? roundVec(p.entity.position) : 'out of sight']))
    const { humans } = splitPlayers(playersSeen, agentNames(path.join(ROOT, 'state')))
    const chattiness = cfg.chat?.chattiness ?? 1
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      oxygen: bot.oxygenLevel,
      inWater: bot.entity.isInWater,
      exact: bot.entity.position.toArray().map(n => Math.round(n * 100) / 100).join(','),
      // where the server last put the body, when that is off the client's position (card 962beec2)
      ...serverPosNote({ client: bot.entity.position, server: lastServerPos, now: Date.now() }),
      time: `${isNight(bot.time.timeOfDay) ? 'night' : 'day'} ${bot.time.timeOfDay}`,
      pos: pos(),
      dimension: bot.game.dimension === 'overworld' ? null : bot.game.dimension,
      raining: bot.isRaining,
      holding: bot.heldItem?.name,
      asleep: bot.isSleeping,
      doing: task && doingText({ name: task.name, seconds: Math.round((Date.now() - task.started) / 1000), paused: task.paused }),
      queued: jobShelf.list().queued.map(id => { const j = jobShelf.get(id); return j ? `${j.name} (${j.id})` : String(id) }).join(', ') || undefined,
      queueHeld: jobShelf.snapshot().held?.reason,
      stuck: stuckField(stuckNow),
      following: followTarget,
      reflexesOff: !reflexes,
      // which code this is, so `am I running the fix?` is answered by the line every driver already reads (#140)
      code: codeHere.code,
      dirty: codeHere.dirty,
      players: playersSeen,
      // only the humans some body can currently see right now (not agent bodies, and not 'out of sight' ones)
      humans: humans.filter(name => playersSeen[name] !== 'out of sight').join(','),
      // only shown below 1 (today's behaviour, unfiltered): card 2e032c4a
      ...(chattiness < 1 && { chattiness })
    }
  },

  look_around (a) {
    const range = a.range ?? 32
    const me = bot.entity.position
    const groups = {}
    for (const e of Object.values(bot.entities)) {
      if (e === bot.entity || !e.position) continue
      const d = e.position.distanceTo(me)
      if (d > range) continue
      const name = e.type === 'player' ? `player:${e.username}` : (e.name ?? e.type)
      const g = groups[name] ??= { count: 0, nearest: Infinity }
      g.count++
      if (d < g.nearest) { g.nearest = Math.round(d); g.at = roundVec(e.position) }
    }
    const interesting = a.blocks ?? ['*_ore', '*_log', 'chest', 'barrel', 'crafting_table', 'furnace', '*_bed', 'water', 'lava', 'spawner', '*_door', 'farmland']
    const blocks = {}
    for (const pattern of interesting) {
      const found = findBlockByName(pattern, a.blockRange ?? 24, 64)
      for (const p of found) {
        const name = bot.blockAt(p).name
        const b = blocks[name] ??= { count: 0, nearest: p, dist: Infinity }
        b.count++
        const d = p.distanceTo(me)
        if (d < b.dist) { b.dist = Math.round(d); b.nearest = p }
      }
    }
    const below = bot.blockAt(me.offset(0, -1, 0))
    const line = (count, dist, at) => `${count}x ${dist}m @${compact(at)}`
    const nearestBlocks = Object.entries(blocks).sort((p, q) => p[1].dist - q[1].dist).slice(0, a.limit ?? 10)
    // mob=cow: where every one of them is (a herd count does not say who is outside the fence)
    const each = a.mob && Object.values(bot.entities).filter(e => e.name === a.mob && e.position.distanceTo(me) <= range)
      .sort((p, q) => p.position.distanceTo(me) - q.position.distanceTo(me)).slice(0, 24).map(e => compact(roundVec(e.position))).join(' ')
    if (a.mob) return { pos: pos(), mob: a.mob, each: each || 'none in range' }
    return {
      pos: pos(),
      on: below?.name,
      entities: Object.fromEntries(Object.entries(groups).map(([n, g]) => [n, line(g.count, g.nearest, g.at)])),
      blocks: Object.fromEntries(nearestBlocks.map(([n, b]) => [n, line(b.count, b.dist, b.nearest)])),
      places: describePlaces(readPlaces(), me, { limit: 5, maxDist: 64, notes: false }),
      // #97: the one entity in this list you must not aim at. It is named here because the count alone reads like any other mob
      ...(Object.keys(groups).some(n => NEVER_FIGHT.has(n)) ? { careful: `${Object.keys(groups).filter(n => NEVER_FIGHT.has(n)).join(' and ')} in sight: do not attack or aim at one, my body loses that fight in seconds. Keep a block between you and walk away` } : {})
    }
  },

  inventory () {
    const slot = n => bot.inventory.slots[bot.getEquipmentDestSlot(n)]?.name ?? null
    return {
      items: inventoryCounts(),
      freeSlots: bot.inventory.emptySlotCount(),
      armor: { head: slot('head'), torso: slot('torso'), legs: slot('legs'), feet: slot('feet'), offhand: slot('off-hand') }
    }
  },

  // the dashboard's: what the player's screen shows. Every stack by slot would be dozens of tokens a driver pays for on
  // each ./mc inventory, so they live here and not there
  screen () {
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      oxygen: bot.oxygenLevel,
      armor: armorPoints(bot.entity.attributes),
      ...inventorySlots(bot.inventory.slots, bot.quickBarSlot),
      window: openWindow()
    }
  },

  // the farm animals about me, one line each: a driver's eye cannot tell a lamb from a sheep, nor which side of a fence one stands on
  animals (a) {
    const me = bot.entity.position
    // which pen counts as "in": the one around me, or the one around a cell I name, so a pen can be counted from outside it
    const pen = penAround(a.x === undefined ? me.floored() : vecOf(a))
    const floor = pen?.enclosed ? pen.floor : null
    const wanted = a.mob ? matcher(a.mob) : () => true
    const near = e => e.position.distanceTo(me)
    return {
      found: Object.values(bot.entities)
        .filter(e => e.name && (CREATURE_FOOD[e.name] || ['horse', 'donkey', 'mule'].includes(e.name)) && wanted(e.name) && near(e) <= (a.within ?? 24))
        .sort((x, y) => near(x) - near(y))
        .map(e => ({
          mob: e.name,
          id: e.id,
          at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`,
          dist: Math.round(near(e)),
          grown: ['horse', 'donkey', 'mule'].includes(e.name) ? horseState(bot, e)?.baby === false : !isBaby(e.metadata),
          inMyPen: Boolean(floor) && unpenned(floor, [e], x => x.position).length === 0
        }))
    }
  },

  // x= y= z= anchors the search on a cell instead of on me: a place's hives are the same list from wherever I stand
  find_blocks (a) {
    const point = a.x === undefined ? bot.entity.position : vecOf(a)
    const positions = timedScan(reportPerformance, 'find_blocks', () => findBlockByName(a.block, a.maxDistance ?? 64, a.count ?? 10, point),
      { block: a.block, range: a.maxDistance ?? 64, count: a.count ?? 10, at: { x: point.x, y: point.y, z: point.z } })
    return { positions }
  },

  // will this pen hold? Walks the way an animal can from a spot inside (default: where I stand) and says where it gets out
  'pen.check' (a) {
    const feet = a.x === undefined ? bot.entity.position.floored() : vecOf(a)
    // a cell in a chunk this body was never sent reads as nothing at all, and nothing at all used to come back as "not
    // a spot to stand on": Perrin's check on a pen 200 blocks off blamed his coordinates for a world I had not seen
    const blind = outOfSight(bot.blockAt(feet), feet, bot.entity.position)
    if (blind) throw new Error(`pen.check: ${blind}`)
    const found = penAround(feet, a.radius)
    if (!found) throw new Error(`${feet.x},${feet.y},${feet.z} is not a spot to stand on: give the x y z of a free floor cell INSIDE the pen (y = where feet would be), or stand in it`)
    // every verdict here is a verdict about ONE cell, and until now the reply never said which (backlog #125)
    const from = `${feet.x},${feet.y},${feet.z}`
    if (!found.enclosed) {
      return {
        pen: 'LEAKS',
        from,
        side: stanceNote(from, penStance({ start: [feet.x, feet.y, feet.z], topsAt: found.topsAt, radius: a.radius ?? 24 })),
        via: found.via,
        advice: 'an animal can walk out: via= is where (x,height,z): one spot = a gap or open gate on level ground; three = the step it climbs, the barrier top it crosses, where it lands. A fence or wall must stand 2 above EVERY block next to it, inside and out, corner to corner included. Fix it and check again'
      }
    }
    const census = { ...censusOf(found.floor), ...blindGateAdvice(found.floor, found.topsAt) }
    if (found.cells >= 16) return { pen: 'holds', from, cells: found.cells, ...census }
    return { pen: 'holds', from, cells: found.cells, ...census, advice: 'but it is small: an animal led in stops 2.5 blocks from you, so under 16 cells it stops in the gateway' }
  },

  block_at (a) {
    const b = bot.blockAt(vecOf(a))
    return b ? { name: blockName(b), properties: b.getProperties?.() } : { name: null }
  },

  // render what the bot sees to a PNG (see eyes.mjs): look | look pano=true | look dir=north | look x= y= z=
  look (a) { return eyes(a) },

  // shared points of interest: mark name= kind=<base|mine|farm|village|danger|resource|...> note= [x= y= z=, default: here]
  // map= saves an ASCII plan with the place (see farm.plan, which is what validates one). Marking a place again keeps
  // the plan, the OWNER and anything else already saved under that name: only what you pass is replaced (backlog #141).
  mark (a) {
    if (!a.name) throw new Error('mark needs name= (and ideally kind= and note=)')
    const saved = readPlaces().find(p => p.name === a.name)
    // moving somebody else's place, re-planning it or calling it something else overwrites THEIR record of it.
    // Adding to its note is how agents leave each other word and stays open (markFields keeps the owner through it)
    const rewrites = a.structure !== undefined || a.legend !== undefined || a.map !== undefined || a.x !== undefined || (a.kind !== undefined && a.kind !== saved?.kind)
    const refusal = rewrites ? mapRefusal(saved, bot.username) : null
    if (refusal) throw new Error(refusal)
    // a note-only mark used to move the place to my feet (BUGS.md 09-24 12:42Z): markMove keeps the anchor, says a move
    // out loud, and refuses one off a plan that still stands where it was marked
    const stands = hasPlan(saved) ? planStands(planCells(saved), (x, y, z) => bot.blockAt(new Vec3(x, y, z))) : false
    const where = markMove({ saved, args: a, here: bot.entity.position, stands })
    if (where.error) throw new Error(where.error)
    const at = where.at
    if (a.map !== undefined && a.structure !== undefined) throw new Error('choose structure= or legacy map= import, not both')
    if (a.legend !== undefined && a.map === undefined) throw new Error('legend= is only accepted with legacy map= import; update structure.legend instead')
    const structure = a.structure !== undefined ? parseStructurePlan(a.structure).structure : a.map !== undefined ? legacyPlanStructure(a.map, a.legend) : saved?.structure
    const parsed = a.structure !== undefined ? parseStructurePlan(a.structure) : structure ? parseStructurePlan(structure) : null
    const errors = parsed ? planErrors(parsed) : []
    if (errors.length) throw new Error(errors.join('; '))
    const fields = markFields({ saved, by: bot.username, note: a.note })
    if (fields.error) throw new Error(fields.error)
    const place = { ...saved, name: String(a.name), kind: a.kind ?? saved?.kind ?? 'place', x: Math.floor(at.x), y: Math.floor(at.y), z: Math.floor(at.z), by: fields.by, note: fields.note, structure }
    delete place.plan
    delete place.legend
    savePlaces([...readPlaces().filter(p => p.name !== place.name), place])
    return { marked: place.name, at: `${place.x},${place.y},${place.z}`, moved: where.moved, plan: parsed ? `${parsed.width}x${parsed.maxY - parsed.minY + 1}x${parsed.height}` : undefined }
  },
  // deleting an entry off the shared map is never leaving word: whoever marked it is the only one who can take it off
  unmark (a) {
    const refusal = mapRefusal(readPlaces().find(p => p.name === a.name), bot.username)
    if (refusal) throw new Error(refusal)
    savePlaces(readPlaces().filter(p => p.name !== a.name))
    return {}
  },
  places (a) {
    const all = readPlaces()
    const from = bot.entity.position
    if (a.name !== undefined) {
      const one = describePlace(all, String(a.name), from)
      if (!one) throw new Error(`no place called ${a.name}: search for it with places q=${String(a.name).slice(0, 12)}`)
      return one
    }
    // places.json is shared by every body and grows without limit: a list that silently stopped at 12 sent agents to
    // read the file. The filters are the search, and the tail says what they did not see
    const search = { q: a.q, by: a.by, kind: a.kind, within: a.within }
    const found = matchPlaces(all, from, search)
    const lines = describePlaces(all, from, { ...search, limit: a.limit })
    const asked = compact(Object.fromEntries(Object.entries(search).filter(([, v]) => v !== undefined)), false)
    if (!lines.length) return { text: all.length ? `no place matches ${asked || 'that'}: ${all.length} are marked, try a shorter q= or drop within=` : 'no places marked yet' }
    const more = found.length - lines.length
    return { text: [...lines, more > 0 ? `... and ${more} more of ${all.length} marked: narrow it with q= by= kind= within=, or raise limit=` : ''].filter(Boolean).join('\n') }
  },

  zones () { return { zones } },
  protect (a) {
    const zone = Object.fromEntries(['name', 'x1', 'y1', 'z1', 'x2', 'y2', 'z2'].map(k => [k, a[k]]))
    if (Object.values(zone).some(v => v === undefined)) throw new Error('protect needs name,x1,y1,z1,x2,y2,z2')
    zones.splice(0, zones.length, ...zones.filter(z => z.name !== zone.name), zone)
    saveZones()
    return { zones: zones.length }
  },
  unprotect (a) {
    zones.splice(0, zones.length, ...zones.filter(z => z.name !== a.name))
    saveZones()
    return { zones: zones.length }
  },

  // ASCII slices of the box between two corners; one call instead of hundreds of block_at round trips
  scan (a) {
    const volume = ['x', 'y', 'z'].reduce((n, k) => n * (Math.abs(a[k + '2'] - a[k + '1']) + 1), 1)
    if (!(volume <= scanCap(a.where))) throw new Error(`scan needs x1,y1,z1,x2,y2,z2 spanning at most ${scanCap(a.where)} blocks (got ${volume})${a.where ? '' : '; to find one kind of block in a bigger box add where=<name>, for a wider view use look'}`)
    const nameAt = (x, y, z) => blockName(bot.blockAt(new Vec3(x, y, z)))
    // where= answers with coordinates only: the picture is the dear part, and whoever asks where wants to act, not to look
    return a.where ? { where: scanWhere(nameAt, a, a.where) } : { map: renderScan(nameAt, a) }
  },

  // chat goes to everyone: a message that opens with an online player's name still goes, with a hint to whisper next time (src/talk.mjs)
  // a long text goes out in numbered pieces under the server's line limit instead of being cut off (src/talk.mjs)
  chat (a) {
    const said = chatText(a, Infinity)
    if (said.error) throw new Error(said.error)
    const chattiness = cfg.chat?.chattiness ?? 1
    if (chattiness < 0.2 && isGreeting(said.text)) throw new Error(`chattiness ${chattiness}: greetings and acks are not sent; whisper if it matters`)
    const parts = splitSay(said.text, sayLimit())
    for (const part of parts) bot.chat(part)
    const to = addressedTo(said.text, onlinePlayers())
    return { ...(parts.length > 1 && { parts: parts.length }), ...(to && { hint: whisperHint(to) }) }
  },
  // a whisper to someone offline is /tell into the void: the server's "No player was found" never reaches the driver
  whisper (a) {
    const said = chatText(a, Infinity)
    if (said.error) throw new Error(said.error)
    const offline = offlineWhisper(a.player, onlinePlayers())
    if (offline) throw new Error(offline)
    const parts = splitSay(said.text, sayLimit(a.player))
    for (const part of parts) bot.whisper(a.player, part)
    return parts.length > 1 ? { parts: parts.length } : {}
  },
  // the dashboard's: a line typed into an agent's popup, recorded as the whisper it stands for (bot.on('whisper') above)
  hear (a) {
    const said = heardWhisper(a)
    lastDriven = Date.now()
    emit('whisper', said)
    return {}
  },

  async equip (a) {
    const item = findItem(a.item)
    const destination = a.destination ?? equipSlot(item.name)
    await bot.equip(item, destination)
    return { on: destination }
  },
  async toss (a) { const i = findItem(a.item); await bot.toss(i.type, null, Math.min(a.count ?? i.count, i.count)); return {} },
  async look_at (a) { await bot.lookAt(new Vec3(a.x, a.y, a.z)); return {} },
  // The reflex should beat you to this (see eat_failed when it cannot), and a body that will not eat has to be drivable
  // by hand. It is also the only way to read what mineflayer-auto-eat really answers: its own reflex swallowed every word.
  async eat (a) {
    const carried = carriedFood()
    const refusal = eatRefusal({ food: bot.food, item: a.item, carried, anyway: a.anyway })
    if (refusal) throw new Error(refusal)
    const { allowed } = eatAllowed({ food: bot.food, carried, anyway: a.anyway })
    const edible = bot.inventory.items().filter(i => allowed.includes(i.name))
    const before = bot.food
    const countsBefore = inventoryCounts()
    // sanitizeOpts writes its choice back into this object, so an eat with no item= still says what it ate. With
    // nothing on the ordinary list the choice is made here instead: the plugin would refuse what the floor allowed.
    const pick = a.item ? edible.find(i => i.name === a.item) : (carried.edible.length ? null : edible[0])
    const opts = pick ? { food: pick } : {}
    // with strictErrors off a failed meal resolves and emits eatFail instead of throwing: catch both, or `ate` would lie
    const attempt = async () => {
      let failure = null
      const onFail = error => { failure ??= error }
      bot.autoEat.on('eatFail', onFail)
      try {
        await eatOnce(opts)
      } catch (error) {
        failure ??= error
      } finally {
        bot.autoEat.off('eatFail', onFail)
      }
      return failure
    }
    // a meal "never showed" right after a craft: the plugin asked for bread from a slot the server had just moved, and
    // the second eat worked (card c13b704d). Once the pockets have settled, judge it again and try once more
    const pocket = () => inventoryCounts()[opts.food?.name] ?? 0
    const judge = (failure, retried) => lateMeal({ failure, before: { food: before, carried: countsBefore[opts.food?.name] ?? 0 }, after: { food: bot.food, carried: pocket() }, retried })
    let failure = await attempt()
    let late = null
    if (failure) { await inventoryQuiet(); late = judge(failure, false) }
    if (late === 'retry') {
      failure = await attempt()
      late = failure ? (await inventoryQuiet(), judge(failure, true)) : 'retried'
    }
    if (failure && late !== 'ate') throw new Error(eatFailure(failure, carriedFood().edible))
    await bot.waitForTicks(5) // the food number comes in the update_health after the meal, not with it
    const eaten = opts.food?.name ?? null
    const uneaten = late === 'ate' ? null : uneatenMeal({ item: eaten, before: countsBefore[eaten] ?? 0, after: inventoryCounts()[eaten] ?? 0 })
    if (uneaten) throw new Error(uneaten)
    const note = late === 'ate' ? 'the meal showed once the pockets settled' : late === 'retried' ? 'the first try asked for a slot the server had just moved; the second ate' : null
    return { ate: eaten, gained: bot.food - before, food: bot.food, health: Math.round(bot.health), ...(note ? { note } : {}) }
  },

  async wake () {
    if (!bot.isSleeping) throw new Error('already awake')
    const woke = new Promise(resolve => bot.once('wake', resolve))
    leaveBed()
    await within(3000, woke, 'waking up')
    return {}
  },

  follow (a) {
    if (!bot.players[a.player]?.entity) throw new Error(`can't see ${a.player} right now`)
    followTarget = a.player
    resumeFollow()
    return { following: a.player }
  },

  // raw movement for debugging: hold a control (forward/back/left/right/jump/sprint) for ms
  async control (a) {
    const from = pos()
    const finishTrace = a.trace ? controlTrace(bot, { duration: a.ms ?? 1000 }) : null
    const alive = cancelGuard()
    try {
      bot.setControlState(a.state ?? 'forward', true)
      const until = Date.now() + (a.ms ?? 1000)
      while (Date.now() < until) { alive(); await bot.waitForTicks(1) }
      return { from, to: pos(), onGround: bot.entity.onGround, velocity: roundVec(bot.entity.velocity), ...(finishTrace ? { trace: finishTrace() } : {}) }
    } finally {
      finishTrace?.()
      bot.setControlState(a.state ?? 'forward', false)
    }
  },

  // stop is an explicit all-work cancellation; normal chat never calls it implicitly.
  stop () { return stopAllJobs() },
  // without on= it only tells: a bare `reflexes` "to look" used to switch them all off, silently
  reflexes (a) {
    if (a.on === undefined) return { reflexes, note: 'unchanged: reflexes on=true|false switches them' }
    reflexes = !!a.on
    if (!reflexes) { bot.pvp.stop(); fighting = null; fightStart = null }
    return { reflexes }
  },
  // recent history without reading the log: events [type=chat] [last=10] (the last 500, earlier runs included)
  events (a) {
    const lines = recent.filter(e => !a.type || e.type === a.type).slice(-(a.last ?? 10))
      .map(({ seq, t, type, ...rest }) => `${t.slice(11, 19)} ${type} ${compact(rest)}`.trim())
    return { text: lines.join('\n') || 'nothing yet' }
  }
}

function cancelTask (why, { holdQueue = true } = {}) {
  const active = jobShelf.snapshot().active
  if (active != null) {
    jobShelf.markCancelling(active, why)
    if (holdQueue) jobShelf.hold(`job ${active} cancelled by ${why}; explicitly resume or discard queued jobs`)
  }
  gen++
  if (task) emit('task_cancelled', { id: task.id, name: task.name, why, ...(task.jobId ? { notify: false } : {}) })
  if (task) lastCancel = { id: task.id, why }
  task?.releaseFollow?.()
  followTarget = null
  task = null
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
const jobShelf = createJobShelf(path.join(HOME, 'jobs.json'))
taskId = Math.max(0, ...jobShelf.snapshot().jobs.map(job => Number(job.id) || 0))
for (const recovered of jobShelf.snapshot().jobs.filter(job => job.status === 'interrupted' && !job.recoveryReported)) {
  emit('job_interrupted', { id: recovered.id, name: recovered.name, error: recovered.error })
  jobShelf.patch(recovered.id, { recoveryReported: true })
}
let scheduler
const recentReflex = () => lastReflex && { ...lastReflex, agoMs: Date.now() - lastReflex.at }
// #128: every goto out of a 1x1 natural shaft fails in a second with "no walkable path", a goto one block away
// included. True, and useless: read once from the body's own cell, the answer is about the block it is ON
const passableAboutFeet = (through = () => false) => {
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  return (dx, dy, dz) => { const block = bot.blockAt(new Vec3(feet.x + dx, feet.y + dy, feet.z + dz)); return block?.boundingBox !== 'block' || through(block) }
}
const amBoxedIn = () => Boolean(bot?.entity) && boxedIn(passableAboutFeet())
// bamboo the pathfinder reads as walls (a fence-like thicket), though the server's offset stalks leave the body room to walk out between
const amBoxedByBamboo = () => amBoxedIn() && !boxedIn(passableAboutFeet(block => block.name === 'bamboo'))
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
const firstMoveNote = moves => {
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
// the path a walk would take, searched the way path_to searches it: one 40 ms slice at a time until it is done or the budget is out
function searchPath (aim, range) {
  const began = Date.now()
  const budget = thinkBudget(goalDistance(aim, bot.entity.position))
  let r = bot.pathfinder.getPathTo(bot.pathfinder.movements, new goals.GoalNear(aim.x, aim.y, aim.z, range), budget)
  while (r.status === 'partial' && r.context && Date.now() - began < budget) r = Object.assign(r.context.compute(), { context: r.context })
  return r.path
}
// one jump into the cell beside and one up, with the legs (a shaft is where the pathfinder found nothing, so it is not asked first)
async function stepUp (cell) {
  const there = () => { const feet = feetCell(bot.entity.position, bot.entity.onGround); return feet.x === cell.x && feet.y === cell.y && feet.z === cell.z }
  await bot.lookAt(new Vec3(cell.x + 0.5, cell.y + 1.62, cell.z + 0.5), true).catch(() => {})
  bot.setControlState('forward', true)
  bot.setControlState('jump', true)
  try {
    for (let t = 0; t < 30 && !(there() && bot.entity.onGround); t++) await bot.waitForTicks(1)
  } finally {
    bot.setControlState('forward', false)
    bot.setControlState('jump', false)
  }
  await bot.waitForTicks(4)
  if (!there()) await within(3000, bot.pathfinder.goto(new goals.GoalBlock(cell.x, cell.y, cell.z)), 'stepping up').catch(() => {})
  bot.pathfinder.setGoal(null)
}
// From the bottom of a 1-wide shaft a dig walk aimed at the surface dug or scaffolded further DOWN (card 2b2d1f65): from a cell
// boxed in on four sides the pathfinder's best partial path goes the one way it can dig. A goal above the body is climbed first
// when the body is boxed in or the search's path ends lower than the feet: by hand (src/navigation/climb.mjs), a niche to the side at
// head height, a block under the feet, a step up, until the shaft opens on two sides; the legs take it from there
async function climbFirst (to) {
  const feet = feetCell(bot.entity.position, bot.entity.onGround)
  if (to.y <= feet.y) return null
  const boxed = amBoxedIn()
  const path = boxed ? [] : searchPath(digLegs(bot.entity.position, to)[0], 1)
  if (!boxed && !descendingLeg({ from: feet, path, goalY: to.y })) return null
  const why = boxed ? 'in a 1-wide shaft with the goal above me: climbing first' : descentNote(path[path.length - 1])
  const passable = (x, y, z) => { const cell = cellAt(x, y, z); return Boolean(cell) && !cell.solid && cell.name !== 'lava' }
  const out = await climbShaft({
    feetAt: () => feetCell(bot.entity.position, bot.entity.onGround),
    goalY: to.y,
    blockAt: cellAt,
    carried: climbBlocks(inventoryCounts(), name => bot.registry.blocksByName[name]?.boundingBox === 'block'),
    dig: cell => long.dig({ x: cell.x, y: cell.y, z: cell.z, batch: true, by_hand: true }),
    place: block => long.place({ item: block.item, x: block.x, y: block.y, z: block.z }),
    step: stepUp,
    until: now => !inPocket((dx, dy, dz) => passable(now.x + dx, now.y + dy, now.z + dz))
  }).catch(e => { throw new Error(`${why}; ${e.message}`) })
  return `${why}; climbed ${out.climbed} (${out.side} niche, ${out.placed} placed, ${out.dug} dug) to ${out.to.x},${out.to.y},${out.to.z}`
}
// walked, never dug: a dug base never regrows, and the free space between the stalks always leads out of a grove that is not sealed
async function wriggleOut () {
  if (!amBoxedByBamboo()) return null
  const alive = cancelGuard()
  const { y } = feetCell(bot.entity.position, bot.entity.onGround)
  const at = (x, dy, z) => bot.blockAt(new Vec3(x, y + dy, z))
  const bambooAt = (x, z) => [0, 1].some(dy => at(x, dy, z)?.name === 'bamboo')
  const clear = (x, dy, z) => { const block = at(x, dy, z); return Boolean(block) && (block.name === 'bamboo' || block.boundingBox === 'empty') }
  const openAt = (x, z) => clear(x, 0, z) && clear(x, 1, z) && at(x, -1, z)?.boundingBox === 'block'
  const waypoints = groveExit({ from: bot.entity.position, bambooAt, openAt })
  if (!waypoints) return null
  const reached = async waypoint => {
    for (let t = 0; t < 40; t++) {
      const { yaw, sneak, arrived } = steer(bot.entity.position, waypoint)
      if (arrived) return true
      alive()
      await bot.look(yaw, 0, true)
      bot.setControlState('forward', true)
      bot.setControlState('sneak', sneak)
      await bot.waitForTicks(1)
    }
    return steer(bot.entity.position, waypoint).arrived
  }
  try {
    for (const waypoint of waypoints) if (!await reached(waypoint)) return null
  } finally {
    bot.setControlState('forward', false)
    bot.setControlState('sneak', false)
  }
  const out = waypoints.at(-1)
  return `wriggled out of the bamboo to ${out.x.toFixed(2)},${out.z.toFixed(2)}`
}
// a dig walk goes in legs of 6 (src/navigation/dig-legs.mjs): a straight line of 20 through rock is more search than the 5 s budget
// holds, and legs of 5-8 arrived all afternoon where 10+ timed out (card 5e16aff9). A plain walk keeps its one goal
async function walkLegs (to, range, into = false) {
  const notes = []
  const climbed = digging ? await climbFirst(to) : null
  if (climbed) notes.push(climbed)
  const legs = digging ? digLegs(bot.entity.position, to) : [to]
  for (const [i, leg] of legs.entries()) {
    const last = i === legs.length - 1
    // a leg on (or in mid-air over) the floor of a pit walks to the pit's rim instead (src/navigation/walk.mjs rimGoal, card 3fe30fb4)
    const rim = rimGoal(cellAt, leg, last ? range : 1, { into, from: feetCell(bot.entity.position, bot.entity.onGround) })
    if (rim) notes.push(rim.note)
    const aim = rim ?? { x: leg.x, y: leg.y, z: leg.z, range: last ? range : 1 }
    // the judgement path_to and goNear make before the search, which this walk alone did not: a goal with no cell to stand in
    // within its range (the middle of a planted field: farm.maintain's first walk, card 29167296) is refused in a millisecond
    // with the reason, not after A* has run its budget out ("ran out of time") or its radius ("no walkable path"). Only over
    // loaded cells: a far goal is walked towards and judged by the pathfinder as its chunks arrive; a dig walk makes its own room
    const nowhere = !digging && loadedAround(cellAt, aim, aim.range) ? noStanding(cellAt, aim, aim.range) : null
    if (nowhere) throw new Error(nowhere)
    await bot.pathfinder.goto(new goals.GoalNear(aim.x, aim.y, aim.z, aim.range))
      .catch(e => { throw new Error(last && legs.length === 1 ? e.message : `leg ${i + 1} of ${legs.length}, to ${aim.x},${aim.y},${aim.z}: ${e.message}`) })
  }
  return { legs: legs.length, ...(notes.length && { note: notes.join('; ') }) }
}
// given: the plain arguments, for the log (printing the tracked ones would count as reading them all)
// the gate reflex only reaches 5 blocks and can miss at a sprint: whatever I opened and is still open when a task ends gets shut now.
// An open gate empties a pen (the human's sheep after lead, Kettricken's after flock.breed, Miles' after shear and goto)
// gates on the ring of this pen (floor: "x,y,z" keys) that nothing can walk through: see blindGates. topsAt is penAround's own column
// reader, the one penLeak walks by: heights an animal can stand at, so a step up outside a gate reads as the step it is (Chani's report, 2026-09-24)
function blindGateAdvice (floor, topsAt) {
  const cells = floor.map(k => k.split(',').map(Number))
  const floorAt = (x, z) => cells.filter(c => c[0] === x && c[2] === z).map(c => c[1])
  const around = [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => [dx, dz]))
  const gates = [...new Map(cells.flatMap(([x, y, z]) => around.map(([dx, dz]) => new Vec3(x + dx, Math.floor(y), z + dz)))
    .filter(p => bot.blockAt(p)?.name.endsWith('_fence_gate')).map(p => [String(p), p])).values()]
  const blind = blindGates(gates, floorAt, topsAt)
  return blind.length ? { blindGates: `${blind.map(b => `${b.at} (${b.why})`).join('; ')}. Nothing can walk through such a gate: put it in the middle of a wall with ground straight across at the pen floor's own level (one step up or down is fine)` } : {}
}

// the pen around a floor cell, walked the way an animal can (see penLeak); null when that cell is no spot to stand on
export function penAround (feet, radius) {
  const columns = new Map()
  const rims = new Map()
  const thin = shape => shape[3] - shape[0] < 0.8 || shape[5] - shape[2] < 0.8
  const shapesAt = (x, y, z) => bot.blockAt(new Vec3(x, y, z))?.shapes ?? []
  // the heights an animal could stand at in a column: the top of anything solid with 1.4 of free room above it
  const topsAt = (x, z) => {
    const known = columns.get(`${x},${z}`)
    if (known) return known
    const tops = []
    for (let y = feet.y - 6; y <= feet.y + 4; y++) {
      const shapes = shapesAt(x, y, z)
      if (!shapes.length) continue
      const top = y + Math.max(...shapes.map(shape => shape[4]))
      const free = [Math.floor(top), Math.floor(top + 1.4)].every(c => c <= y || !shapesAt(x, c, z).length)
      if (free) tops.push(top)
      // a full block carrying a fence, wall, pane or door: the post leaves a ledge an animal can stand on (Vivenna's second leak, see penLeak)
      else if (top === y + 1 && shapesAt(x, y + 1, z).length && [y + 1, y + 2].every(c => shapesAt(x, c, z).every(thin))) rims.set(`${x},${z}`, [...(rims.get(`${x},${z}`) ?? []), top])
    }
    columns.set(`${x},${z}`, tops)
    return tops
  }
  const rimsAt = (x, z) => { topsAt(x, z); return rims.get(`${x},${z}`) ?? [] }
  if (!topsAt(feet.x, feet.z).includes(feet.y)) return null
  const found = penLeak({ start: [feet.x, feet.y, feet.z], topsAt, rimsAt, radius, withFloor: true })
  // rock all round is no pen (Kettricken's cave pocket): `fenced` says a fence or wall stands beside the floor
  // topsAt goes back with it so a caller can ask penStance which side of the fence the walk started on (backlog #125)
  return found.enclosed ? { ...found, topsAt, fenced: fencedIn(found.floor, topsAt) } : { ...found, topsAt }
}
// after walking through a pen gate: farm animals standing outside it, within 6 blocks. The pen is whichever side of the gate is enclosed
function straysAt (gateKey) {
  const gate = new Vec3(...gateKey.match(/-?\d+/g).map(Number))
  const pen = [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => penAround(gate.offset(dx, 0, dz))).find(found => found?.enclosed)
  if (!pen) return null
  const animals = Object.values(bot.entities).filter(e => BREEDING_FOOD[e.name]).map(e => ({ name: e.name, x: e.position.x, y: e.position.y, z: e.position.z }))
  return strays(pen.floor, animals, [gate.x, gate.y, gate.z])
}
// who is in a pen and who stands outside it, within 16 blocks of its floor: counted from the cells just walked, not judged by eye
export function censusOf (floor) {
  const cells = floor.map(k => k.split(',').map(Number))
  const close = e => cells.some(([x, , z]) => Math.abs(e.position.x - x) <= 16 && Math.abs(e.position.z - z) <= 16)
  return penCensus(floor, Object.values(bot.entities).filter(e => BREEDING_FOOD[e.name] && close(e)).map(e => ({ name: e.name, x: e.position.x, y: e.position.y, z: e.position.z })))
}

// the cells around a bed (both halves), for bedExit
function bedExits (bed) {
  const sides = [[1, 0], [-1, 0], [0, 1], [0, -1]]
  const isBed = p => /_bed$/.test(bot.blockAt(p)?.name ?? '')
  const halves = [bed, ...sides.map(([dx, dz]) => bed.offset(dx, 0, dz)).filter(isBed)]
  const open = p => { const b = bot.blockAt(p); return !b || b.boundingBox === 'empty' || isWoodDoor(b) }
  return halves.flatMap(h => sides.map(([dx, dz]) => h.offset(dx, 0, dz))).filter(p => !isBed(p))
    .map(p => ({ at: `${p.x},${p.y},${p.z}`, free: open(p) && open(p.offset(0, 1, 0)), lintel: bot.blockAt(p.offset(0, 2, 0))?.boundingBox === 'block' }))
}
async function shutGatesBehind () {
  const open = gatesLeftOpen(doorsIOpened, heldOpen, key => bot.blockAt(new Vec3(...key.match(/-?\d+/g).map(Number)))?.getProperties().open)
  const { near, far } = gatesByReach(open, bot.entity.position.toArray())
  for (const [x, y, z] of near) await long.toggle({ x, y, z, open: false })
  // a far one is named, not walked to: the body once crossed the map for two gates and left the cow it had just brought home
  return { shut: near.length, far: far && `${far}: still open and more than 32 blocks back: go and shut them (toggle x= y= z= open=false)` }
}
async function shutTrackedGatesAfterCancel () {
  const tracked = [...doorsIOpened].filter(key => !heldOpen.has(key))
  if (!tracked.length) return {}
  const open = []
  const unknown = []
  for (const key of tracked) {
    const block = bot.blockAt(new Vec3(...key.match(/-?\d+/g).map(Number)))
    if (!block) unknown.push(key)
    else if (block.getProperties?.().open === true) open.push(key)
    else doorsIOpened.delete(key)
  }
  if (!open.length && !unknown.length) return {}
  const safeToWalk = ready && bot.entity && bot.health > 0 && !bot.isSleeping && !bot.vehicle &&
    !flee && !holingUp && !fighting && !surfacing && !diggingOut
  if (!safeToWalk) return { restorationPending: `job was cancelled with tracked gate state unresolved (${[...open, ...unknown].join('; ')}); close it after the body is safe` }
  let result = {}
  if (open.length) {
    try { result = await shutGatesBehind() }
    catch (error) { return { restorationPending: `could not close a tracked gate after cancellation: ${error.message}` } }
  }
  const unresolved = [
    ...(result.far ? [result.far] : []),
    ...(unknown.length ? [`gate state is unloaded at ${unknown.join('; ')}`] : [])
  ]
  return { ...result, ...(unresolved.length ? { restorationPending: unresolved.join('; ') } : {}) }
}
// pressed against a fence, a wall or a shut gate my centre lies inside ITS cell, and every plan starts on the wrong side of it (see realCell): three steps
// back into the cell I really stand in, before any task plans a walk
// the cell I really stand in when my centre lies in a fence's cell, else null
function fenceEscape () {
  const here = bot.entity.position
  const mine = bot.blockAt(here.floored())
  if (mine?.boundingBox !== 'block' || !bot.pathfinder.movements?.fences.has(mine.type)) return null
  return realCell(here, (x, y, z) => [0, 1].every(up => bot.blockAt(new Vec3(x, y + up, z))?.boundingBox === 'empty'))
}
async function leaveFenceCell () {
  const here = bot.entity.position
  const mine = bot.blockAt(here.floored())
  const cell = fenceEscape()
  if (!cell) return
  await bot.lookAt(new Vec3(cell.x + 0.5, here.y + 1.6, cell.z + 0.5), true)
  bot.setControlState('forward', true)
  for (let i = 0; i < 12 && (Math.floor(bot.entity.position.x) !== cell.x || Math.floor(bot.entity.position.z) !== cell.z); i++) await bot.waitForTicks(1)
  bot.setControlState('forward', false)
  console.log(`[left fence cell] ${mine.name} -> ${cell.x},${cell.y},${cell.z}`)
}

// One shelf slot owns all body-changing work. Submission persists before this pump claims it;
// nested composite/flow api.act calls still invoke their registered action directly.
async function runLong (name, args, given = args, queuedAs = null) {
  const refusal = refusalFor(name, args)
  if (refusal) return { ok: false, error: refusal }
  if (task) return { ok: false, error: `body owner invariant violated: ${task.name} (${task.id}) is still active` }
  followTarget = null
  const mine = { id: queuedAs ?? ++taskId, name, gen, started: Date.now() }
  task = mine
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
    if (task === mine) { task = null; useMoves(false); bot.setControlState('sneak', false) }
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
  task = mine
  try {
    const refusal = refusalFor(name, args)
    if (refusal) return { ok: false, error: refusal }
    useMoves(mayDig(name, args))
    const result = await quick[name](args)
    if (name === 'follow') await new Promise(resolve => { mine.releaseFollow = resolve })
    return { ok: true, action: name, ...result }
  } finally {
    if (task === mine) { task = null; useMoves(false); bot.setControlState('sneak', false) }
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

function stopAllJobs () {
  const { active, dropped } = scheduler.stop(reason => cancelTask(reason, { holdQueue: false }))
  followTarget = null; endFlee()
  const held = jobShelf.snapshot().held
  return { ok: true, stopped: active ?? null, dropped: dropped.map(job => job.id), ...(held ? { restorationPending: held.reason } : {}) }
}

function cancelAcceptedJob (id, reason = 'cancelled by request') {
  const job = jobShelf.get(id)
  if (!job) return { ok: false, error: `no job ${id}` }
  const cancelled = scheduler.cancel(id, reason)
  if (cancelled.cleanup === 'pending') cancelTask(reason, { holdQueue: false })
  return cancelled
}

function jobControl (name, args) {
  if (name === 'jobs') return { ok: true, ...jobShelf.list({ after: args.after ?? 0, limit: args.limit ?? 100 }) }
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
    return { ok: !blocked, discarded: discarded.map(job => job.id), clearedHold: Boolean(held && !blocked), restorationPending: blocked ? held.reason : undefined }
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
          const { job: accepted, afterCleanup } = scheduler.interrupt({ name, args: actionArgs, given: tracked.given }, reason => cancelTask(reason, { holdQueue: false }), { verbose, notify: tracked.given?.automatic !== true })
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
