// Claude's Minecraft body.
// Fast reflexes (eating, armour, self-defence) live here; decisions arrive over a
// small localhost HTTP API (see README.md) and everything notable that happens is
// appended to events.jsonl so the planning side can follow along.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'
import mineflayer from 'mineflayer'
import pf from 'mineflayer-pathfinder'
import collectBlock from 'mineflayer-collectblock'
import pvp from 'mineflayer-pvp'
import armorManagerMod from 'mineflayer-armor-manager'
import { loader as autoEat } from 'mineflayer-auto-eat'
import vec3 from 'vec3'
import AABB from 'prismarine-physics/lib/aabb.js'
import { tillWarning, parsePlan, planCells, planErrors, planBill, RENAMED, helpText, argsUsage, docText, PRIMITIVES, checkArgs, handBackReason, compositeError, leadTargetError, blindGates, enchantNames, itemsArg, enchantChoice, fencedIn, gateChange, fencePush, realCell, besideNames, noFooting, pitAdvice, chatText, wedgeReplant, thicketCost, leadPick, herdPassed, gatesByReach, holesLeft, penShaftRefusal, fullSide, staleKey, bedExit, gateStepCost, eatJammed, eatFailure, eatRefusal, eatRetryDue, errorRepeat, deathBy, deathReport, deathUnannounced, deathKit, outOfSight, herdOrder, ledReport, tagalongs, ledExtra, waterWary, stackTop, isBaby, progressed, crowdSize, dryCells, openNow, strays, shutNow, didYouMean, scanCap, eatBelow, withDefaultItem, foodAway, gateLeak, smeltWait, giveReport, wedgeBreakable, wakeStep, bedtimeReport, deepestCell, unpenned, penCensus, droppedWalk, hurtCause, scanWhere, craftRoom, coordsError, nextDrop, digRefusal, fluidsLeft, FLUIDS, scaffoldNote, scaffoldTakeBack, scaffoldBuilt, isAir, bedChoice, bedTrap, idleNudge, isGroundCover, looksBuilt, mineTargets, craftShortfall, placeObstacle, deadWalk, parseEventTail, fillOutcome, penLeak, transferFix, gatesLeftOpen, oversleeping, staleCode, leadVerdict, clampedOffset, nudgeAway, creatureFood, CREATURE_FOOD, breedingFood, BREEDING_FOOD, flushCells, airReflex, openAbove, surfacingStalled, breaksUnderfoot, furnaceReport, trackReads, ignoredParams, depositWanted, peacefulTool, chaseVerdict, chaseBroken, chargeLeash, breakOffDigs, CHASE_LEASH, attackRefusal, fleeUnwinnable, NEVER_FIGHT, ENDERMAN_RANGE, brokenSlot, placeOutcome, placeMissed, strayFluid, equipSlot, shouldFlee, ARCHERS, rangedThreat, plansFromOwnCell, missingTool, stepOffChoice, bedtime, feetCell, overMemory, placeAgainst, leftLying, arrivalError, renderScan, inAnyZone, describePlaces, describePlace, matchPlaces, compact, pickFuel, isWedged, matchesProps, checkWatch, within, refuseReason, canPlaceFromHere, ignorableMob, explainInterrupt, isStalled, mayDig, explainNoPath, doorwayNode, buriedIn, nextSheep, occupiedBy, isNight, withdrawPlan } from './lib.mjs'
import { makeEyes, YAWS } from './eyes.mjs'

// the physics engine's own box comparison lets a hitbox that rounds 1e-14 past a block face walk into the block (see clampedOffset in lib.mjs)
const corners = box => ({ min: [box.minX, box.minY, box.minZ], max: [box.maxX, box.maxY, box.maxZ] })
for (const [axis, method] of ['computeOffsetX', 'computeOffsetY', 'computeOffsetZ'].entries()) {
  AABB.prototype[method] = function (other, offset) { return clampedOffset(corners(this), corners(other), axis, offset) }
}

const { pathfinder, Movements, goals } = pf
const { Vec3 } = vec3
const armorManager = armorManagerMod.default ?? armorManagerMod

const DIR = path.dirname(fileURLToPath(import.meta.url))
// the bot folder itself: src/ is the code, and library/ state/ textures/ beside it are shared by every body
const ROOT = path.resolve(DIR, '..')
fs.mkdirSync(path.join(ROOT, 'state'), { recursive: true })
// `node src/bot.mjs <home>` runs another body: its config.json, events.jsonl and snapshots/ live in <home>,
// while state/zones.json and textures/ stay here, shared by every bot.
const HOME = path.resolve(process.argv[2] ?? ROOT)
const cfg = {
  host: 'localhost',
  port: 25565,
  username: 'Claude',
  version: '26.1', // newest protocol mineflayer speaks; ViaBackwards bridges to the 26.2 server
  apiPort: 3777,
  ...JSON.parse(fs.existsSync(path.join(HOME, 'config.json')) ? fs.readFileSync(path.join(HOME, 'config.json')) : '{}')
}

// ---------------------------------------------------------------- events
const EVENTS_FILE = path.join(HOME, 'events.jsonl')
// Boxes the pathfinder must not dig through or scaffold in (it happily tunnels through walls otherwise).
const ZONES_FILE = path.join(ROOT, 'state', 'zones.json')
const readZones = () => fs.existsSync(ZONES_FILE) ? JSON.parse(fs.readFileSync(ZONES_FILE, 'utf8')) : []
const zones = readZones()
// another bot may protect something while we run
fs.watchFile(ZONES_FILE, { interval: 2000 }, () => zones.splice(0, zones.length, ...readZones()))
const saveZones = () => fs.writeFileSync(ZONES_FILE, JSON.stringify(zones, null, 1))
// Points of interest shared by every agent (./mc mark / places / unmark, and goto place=<name>).
const PLACES_FILE = path.join(ROOT, 'state', 'places.json')
const GATES_FILE = path.join(ROOT, 'state', 'gates.log')
const readPlaces = () => fs.existsSync(PLACES_FILE) ? JSON.parse(fs.readFileSync(PLACES_FILE, 'utf8')) : []
// every body shares this file: write beside it and rename, so a reader never catches it half written
const savePlaces = places => {
  const tmp = `${PLACES_FILE}.${process.pid}.tmp`
  fs.writeFileSync(tmp, JSON.stringify(places, null, 1))
  fs.renameSync(tmp, PLACES_FILE)
}
// what `events` shows: starts from the tail of the file, so a restart does not wipe the history
function readEventTail () {
  if (!fs.existsSync(EVENTS_FILE)) return []
  const size = fs.statSync(EVENTS_FILE).size
  const tail = Buffer.alloc(Math.min(size, 200000))
  const file = fs.openSync(EVENTS_FILE, 'r')
  fs.readSync(file, tail, 0, tail.length, size - tail.length)
  fs.closeSync(file)
  return parseEventTail(tail.toString('utf8'), 500)
}
const recent = readEventTail()
let seq = 0
function emit (type, data = {}) {
  const ev = { seq: ++seq, t: new Date().toISOString(), type, ...data }
  recent.push(ev)
  if (recent.length > 500) recent.shift()
  fs.appendFileSync(EVENTS_FILE, JSON.stringify(ev) + '\n')
  console.log(`[${type}]`, JSON.stringify(data))
}
// Errors go through here rather than straight to emit: a fault that repeats (a timer left running over a reconnect,
// above all) writes the same line every few seconds until the events file is a wall. See errorRepeat.
let errorSeen = null
function sayError (message, extra = {}, type = 'error') {
  const { say, seen } = errorRepeat(errorSeen, message, Date.now())
  errorSeen = seen
  if (say) emit(type, { ...extra, message: say })
}

// ---------------------------------------------------------------- bot lifecycle
let bot = null
let eatTimer = null
// item 13 (#109): what a death line needs and cannot work out after the fact. The server's own words, the last wound,
// and the last place the body stood: the respawn point is the world spawn, which tells nobody where the kit fell.
let saidDeath = null
let lastWound = null
let lastStood = null
let lastCarried = null
let diedAt = 0
let mcData = null
let ready = false
let reflexes = true
let eyes = null
let followTarget = null
let task = null // { id, name, gen, started }
let gen = 0
// a long action calls `const alive = cancelGuard()` when it starts and `alive()` in every loop: once it has been cancelled
// or superseded it must stop, or it keeps fighting the next command for the body
const cancelGuard = () => { const mine = gen; return () => { if (gen !== mine) throw new Error('cancelled') } }
let waitingForServer = false

// SAFETY (bodies never eat). mineflayer-auto-eat's own reflex is `statusCheck`, and it ends in `catch {}`: every failure
// for days was invisible, and across every body's bot.log there are 140 jam lines and not one word of why. This is the
// same rule -- eat below minHunger, or below minHealth however full I am -- with the failure said out loud as eat_failed.
// Rate-limited like any other error (errorRepeat), so a reflex that fires every physics tick cannot wall the events file.
const eatFailed = error => sayError(eatFailure(error), { food: bot?.food, health: Math.round(bot?.health ?? 0) }, 'eat_failed')
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
const eatTick = async () => {
  if (!ready || !bot?.autoEat || bot.autoEat.isEating) return
  if (bot.food >= bot.autoEat.opts.minHunger && bot.health >= bot.autoEat.opts.minHealth) return
  // hurt but full: minHealth sends me to eat however full I am, and the game refuses a meal at food 20. Trying anyway
  // is 20 meals a second that can only time out -- most of the 140 jam lines in the logs are this.
  if (bot.food >= 20) return
  if (!eatRetryDue(eatFailedAt, Date.now())) return
  await eatOnce({}).catch(error => { eatFailedAt = Date.now(); eatFailed(error) })
}
// ROOT CAUSE of "the body starves with bread in its pockets". The plugin calls a meal finished when the server sends
// entity_status 9 for my own entity, and this server never does: ClaudeProbe ate its way from food 15 to food 20 while
// the plugin logged "Eating timed out with a time of 3000 milliseconds!" for the very same meal. Every meal therefore
// "failed" -- the error vanished into statusCheck's `catch {}`, the hand was held for 3 s a time and handed back to
// whatever it held before, and the reflex started again on the next tick, for ever. So watch what DOES arrive: the food
// number going up, or the food leaving my pockets. It reads the module-level `bot`, so a reconnect needs no new one.
const watchTheMeal = (food, timeoutMs) => new Promise((resolve, reject) => {
  const carried = () => bot.inventory.items().filter(i => i.name === food.name).reduce((n, i) => n + i.count, 0)
  const before = { food: bot.food, carried: carried() }
  const stop = () => { clearTimeout(timer); bot.off('health', fed); bot.off('physicsTick', gone) }
  const fed = () => { if (bot.food > before.food) { stop(); resolve() } }
  const gone = () => { if (carried() < before.carried) { stop(); resolve() } }
  const timer = setTimeout(() => {
    stop()
    reject(new Error(`the meal never showed: food is still ${bot.food} and I still carry ${carried()} ${food.name} ${timeoutMs} ms on`))
  }, timeoutMs)
  bot.on('health', fed)
  bot.on('physicsTick', gone)
  bot.autoEat._rejectionBinding = error => { stop(); reject(error) } // cancelEat() still works
})

// Two ways of getting about: walking only (the default: digging walks tunnelled through hills and left pillars), and
// digging + scaffolding for `mine` and for walks that ask with dig=true.
let walkMoves = null
let digMoves = null
let digging = false
// every cell the pathfinder aimed a scaffolding placement at during this task. Chani's cobblestone went that way twice with
// nothing in the reply to say so (#111), so a task now reports what it built beside its drops and takes back what it can reach
let scaffolded = []
// >0 while the `place` primitive is putting a block down on purpose: what lands then is a build, not scaffolding
let handPlacing = 0
function makeMoves (dig) {
  const moves = new Movements(bot)
  moves.allowParkour = true
  moves.canOpenDoors = true
  for (const block of Object.values(bot.registry.blocksByName)) if (plansFromOwnCell(block.name)) moves.emptyBlocks.add(block.id)
  // the pathfinder's list of gates it may open is older than cherry, mangrove, bamboo, pale oak, crimson and warped: to it those were walls
  // (Vivenna's and Aviendha's cherry gates: lead said "no way", walks went over the fence by parkour or not at all)
  for (const block of Object.values(bot.registry.blocksByName)) if (block.name.endsWith('_fence_gate')) moves.openable.add(block.id)
  for (const block of Object.values(bot.registry.blocksByName)) if (noFooting(block.name)) moves.fences.add(block.id)
  // a walk straight across a planted field broke the crops on the way (goto is not a licence to trample): route round them.
  // Farmland itself stays walkable, so an empty bed is still a path.
  for (const block of Object.values(bot.registry.blocksByName)) if (breaksUnderfoot(block.name)) moves.blocksToAvoid.add(block.id)
  moves.canDig = dig
  Object.assign(moves, waterWary(dig))
  moves.allow1by1towers = dig
  if (!dig) moves.scafoldingBlocks = []
  // The pathfinder only knows fence gates; to it a door is a wall to smash. Call wooden doors walkable and let doorTick work the handle.
  const getBlock = moves.getBlock.bind(moves)
  moves.getBlock = (...at) => {
    const b = getBlock(...at)
    return isWoodDoor(b) ? Object.assign(b, { safe: true, physical: false, height: at[0].y + at[2] }) : b
  }
  const zoneCost = block => inAnyZone(zones, block.position) ? 100 : 0
  moves.exclusionAreasBreak.push(zoneCost)
  // nor anything that looks built, protected or not
  moves.exclusionAreasBreak.push(block => looksBuilt(block.name) ? 100 : 0)
  moves.exclusionAreasPlace.push(zoneCost)
  moves.exclusionAreasStep.push(block => gateStepCost(block.name))
  moves.exclusionAreasStep.push(block => thicketCost(besideNames(block.position, (x, y, z) => bot.blockAt(new Vec3(x, y, z), false)?.name)))
  // collectBlock switches both of these off on the movements it is given; with them off a tunnel under gravel buried and killed me
  for (const guard of ['dontMineUnderFallingBlock', 'dontCreateFlow']) Object.defineProperty(moves, guard, { get: () => true, set () {} })
  return moves
}
function useMoves (dig) {
  digging = dig
  bot.pathfinder.setMovements(dig ? digMoves : walkMoves)
}

function connect () {
  ready = false
  bot = mineflayer.createBot({
    host: cfg.host, port: cfg.port, username: cfg.username, version: cfg.version, auth: 'offline'
  })
  bot.loadPlugin(pathfinder)
  bot.loadPlugin(collectBlock.plugin)
  bot.loadPlugin(pvp.plugin)
  bot.loadPlugin(armorManager)
  bot.loadPlugin(autoEat)
  // 9 physicsTick listeners stand by design and a walk or a wait adds two for a moment: the warning at 11 was noise, not a leak ([listeners] stayed at 9 for hours on every body)
  bot.setMaxListeners(30)

  bot.once('spawn', () => {
    mcData = bot.registry
    eyes = makeEyes(bot, { textureDir: path.join(ROOT, 'textures'), snapshotDir: path.join(HOME, 'snapshots') })
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
    const walk = bot.pathfinder.goto.bind(bot.pathfinder)
    const arrived = goal => {
      const feet = feetCell(bot.entity.position, bot.entity.onGround)
      return goal.isEnd(bot.entity.position.floored()) || goal.isEnd(new Vec3(feet.x, feet.y, feet.z))
    }
    // see stepOffChoice: only when I stand in a block that is not a full one high (a bed, a slab)
    const stepOff = async () => {
      const here = bot.entity.position.floored()
      if (bot.blockAt(here)?.boundingBox !== 'block') return false
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
      const failure = await walk(goal).then(() => null, e => Date.now() - walkEndedAt < 2000 ? new Error(arrivalError(false)) : e)
      if (!failure && arrived(goal)) return
      // up to 3 steps: along a bed in a 1-wide room the first one only reaches the bed's other half
      let stepped = false
      for (let steps = 0; steps < 3 && await stepOff(); steps++) stepped = true
      const here = bot.entity.position.floored()
      const trap = failure && bedTrap(bot.blockAt(here)?.name, bot.blockAt(here.offset(0, 2, 0))?.boundingBox)
      if (trap) throw new Error(trap)
      if (stepped) await walk(goal)
      else if (failure) throw failure
      const error = arrivalError(arrived(goal))
      if (error) throw new Error(error)
    }
    // path_update hands out the live path before the pathfinder starts walking it: fix doorway waypoints in place (see doorwayNode)
    bot.on('path_update', r => r.path.forEach(n => Object.assign(n, doorwayNode(n, doorAt(n)))))
    bot.on('goal_updated', () => { goalSetAt = Date.now() })
    bot.on('path_reset', reason => { pathResets.push({ reason, at: Date.now() }); if (pathResets.length > 200) pathResets.shift() })
    bot.on('path_update', r => { livePath = r.path })
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
    bot.on('path_update', r => { lastPath = { status: r.status, at: Date.now(), nodes: r.path.slice(0, 4).map(n => `${n.x},${n.y},${n.z}`) } })
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
    bot.autoEat.enableAuto()
    bot.autoEat.on('eatFail', error => eatFailed(error))
    // see eatJammed: the plugin can stay "eating" for ever. ONE timer for the process: started at every spawn and never
    // stopped, the old ones outlive their connection and read the module-level `bot`, which by then is a new one whose
    // plugins are not loaded yet -- that is the `uncaught: ... (reading 'isEating')` every few seconds that filled
    // Perrin's and Mariel's events files after the 09-22 20:53 restart. Cleared here, and guarded for the same reason.
    clearInterval(eatTimer)
    let eatingSince = null
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
    emit('spawned', { pos: pos(), dimension: bot.game.dimension })
  })

  // a running composite hands control back when a person addresses me: a whisper always, a chat that says my name
  bot.on('chat', (username, message) => {
    if (username === bot.username) return
    if (new RegExp(cfg.username, 'i').test(message)) lastSpoken = { from: username, message, at: Date.now() }
    emit('chat', { from: username, message })
  })
  bot.on('whisper', (username, message) => {
    if (username === bot.username) return
    lastSpoken = { from: username, message, at: Date.now() }
    emit('whisper', { from: username, message })
  })
  bot.on('playerJoined', p => { if (ready && p.username !== bot.username) emit('player_joined', { player: p.username }) })
  bot.on('playerLeft', p => { if (p.username !== bot.username) emit('player_left', { player: p.username }) })
  // The server announces the death in a system message ("Claude was slain by Zombie"), which beats every guess at the
  // cause. Nothing else here reads system messages, so this only looks for my own death line.
  bot.on('message', msg => {
    const said = deathBy(String(msg), bot.username)
    if (said) saidDeath = { said, at: Date.now() }
  })
  const died = pos => {
    diedAt = Date.now()
    followTarget = null
    const said = saidDeath && Date.now() - saidDeath.at < 5000 ? saidDeath.said : null
    // from the snapshot, not from the world: by the time a death is handled the server has already emptied the
    // inventory, so a live read says the body died carrying nothing (03:14Z, the first died line with a cause on it)
    const kit = deathKit(lastCarried ?? {})
    emit('died', { ...deathReport({ pos, said, wound: lastWound, now: Date.now() }), ...(kit ? { carried: kit } : {}) })
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
    const line = gateChange(after.position, state(before), state(after), players)
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
    emit('respawned')
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
      const cause = hurtCause({ lost: lastHealth - bot.health, nearby, sinceCreeperMs: Date.now() - creeperSeenAt, fell: Date.now() - lastFall.at < 1500 ? lastFall.blocks : 0, fledFrom: lastReflex?.kind === 'fleeing' ? lastReflex.mob : null, sinceFledMs: Date.now() - (lastReflex?.at ?? 0), food: bot.food, oxygen: bot.oxygenLevel ?? 20 })
      lastWound = { cause, nearby, at: Date.now() }
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
  bot.on('physicsTick', () => { if (tick % 2 === 0 && ready) doorTick() })

  bot.on('kicked', reason => emit('kicked', { reason: typeof reason === 'string' ? reason : JSON.stringify(reason) }))
  // while the server is down we retry quietly: only the first failure is worth an event
  bot.on('error', err => { if (ready || !waitingForServer) sayError(err.message || err.code || String(err)) })
  bot.on('end', reason => {
    if (ready || !waitingForServer) emit('disconnected', { reason })
    waitingForServer = !ready
    ready = false
    setTimeout(connect, 10000)
  })
}

// ---------------------------------------------------------------- reflexes
const pos = () => bot?.entity ? roundVec(bot.entity.position) : null
const roundVec = v => ({ x: Math.round(v.x * 10) / 10, y: Math.round(v.y * 10) / 10, z: Math.round(v.z * 10) / 10 })
const isHostile = e => e.type === 'hostile' || e.kind === 'Hostile mobs'
function nearbyHostiles (range) {
  if (!bot?.entity) return []
  return Object.values(bot.entities).filter(e => e !== bot.entity && isHostile(e) && e.position.distanceTo(bot.entity.position) <= range)
}

const isWoodDoor = b => Boolean(b?.name?.endsWith('_door')) && b.name !== 'iron_door'
const doorsIOpened = new Set()
const heldOpen = new Set() // gates opened with `toggle`: they stay open until toggled shut
const doorAt = n => {
  const b = bot.blockAt(new Vec3(Math.floor(n.x), Math.floor(n.y), Math.floor(n.z)))
  return isWoodDoor(b) ? { x: b.position.x, y: b.position.y, z: b.position.z, half: b.getProperties().half } : null
}
// the shared code is loaded once, at start: tell the driver when it has changed since, once per batch of edits
const codeLoaded = Date.now()
let staleTold = ''
setInterval(() => {
  if (!ready) return
  const files = ['src/bot.mjs', 'src/lib.mjs', 'src/eyes.mjs', 'src/vision.mjs', 'src/builder.mjs', 'src/pens.mjs', ...libraryFiles().map(f => `library/${f}`)]
  const mtimes = Object.fromEntries(files.map(f => [f, fs.statSync(path.join(ROOT, f), { throwIfNoEntry: false })?.mtimeMs]))
  const stale = staleCode(codeLoaded, mtimes, Date.now())
  if (!stale || staleKey(stale, mtimes) === staleTold) return
  staleTold = staleKey(stale, mtimes)
  emit('code_updated', { files: stale.join(' '), advice: 'the shared code has fixes you are not running. No hurry: restart (./mc quit, then ./start in the background) next time you are idle somewhere safe, or at once if a tool misbehaves' })
}, 60000)
let doorBusy = false
let lastSteppedOff = 0
const gatesPassed = new Set() // fence gates this task walked through: their pens get a look for strays when it ends
let leading = false // animals are following me: doors and gates stay open behind me until they have caught up
let following = [] // the animals a lead is bringing along: a gate stays open until they are through it (herdPassed), then shuts at once
let luring = false // a lead is on, from its first step towards the animal: the food stays in my hand, gates or no gates
let feeding = false // feed is holding food out to an animal: it stays in my hand
// open wooden doors as we walk up to them, and shut the ones we opened once we're through
async function doorTick () {
  if (doorBusy) return
  const doorIds = mcData.blocksArray.filter(isWoodDoor).map(b => b.id)
  // fence gates: the pathfinder opens them itself but never shuts them, and an open gate empties a pen. Any open gate I pass counts as mine to shut
  const gateIds = mcData.blocksArray.filter(b => b.name.endsWith('_fence_gate')).map(b => b.id)
  const doors = bot.findBlocks({ matching: [...doorIds, ...gateIds], maxDistance: 5, count: 8 }).map(p => bot.blockAt(p)).filter(b => (b.getProperties().half ?? 'lower') === 'lower')
  if (foodAway({ held: bot.heldItem?.name, luring, feeding, gateNear: doors.some(d => d.name.endsWith('_fence_gate')) })) {
    console.log('[food away] tempting food in hand at a gate: put away, or the animals follow me out')
    doorBusy = true
    await bot.unequip('hand').catch(() => {})
    doorBusy = false
  }
  const todo = doors.find(d => {
    const near = d.position.offset(0.5, 0, 0.5).distanceTo(bot.entity.position) < 1.6
    const open = d.getProperties().open
    // doors too, not only gates: a door found open and walked through stayed open behind me all night (my hut; Miles's cottage let a zombie in)
    if (near && open && !heldOpen.has(String(d.position))) doorsIOpened.add(String(d.position))
    if (near && open && d.name.endsWith('_fence_gate')) gatesPassed.add(String(d.position))
    // moving = the walk still has a goal: the pathfinder stands still while it works a gate, and "stopped" then shut the gate in my own face, for ever
    const feet = bot.entity.position.floored()
    const inDoorway = feet.x === d.position.x && feet.z === d.position.z
    const moving = Boolean(bot.pathfinder.goal) || bot.pathfinder.isMoving()
    return openNow({ near, open, door: isWoodDoor(d), moving, inDoorway }) || shutNow({ near, open, mine: doorsIOpened.has(String(d.position)), leading: leading && !herdPassed(bot.entity.position.toArray(), d.position.offset(0.5, 0, 0.5).toArray(), following.filter(e => e.isValid).map(e => e.position.toArray())), moving, inDoorway })
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

let fighting = null
// where the body stood when the current fight began, and the leash that measures from it (#105)
let fightStart = null
let chaseHeldUntil = 0
let chaseLeash = CHASE_LEASH
let surfacing = false
let swimmingUp = false
let surfaceStart = { at: 0, y: 0 }
let surfaceGoal = null
// the blocks straight over the head, as names, so the reflex can tell deep water from a roof it must swim out from under
const columnAbove = (pos, height = 8) =>
  Array.from({ length: height }, (_, i) => bot.blockAt(new Vec3(Math.floor(pos.x), Math.floor(pos.y) + 2 + i, Math.floor(pos.z)))?.name ?? 'air')
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
// bedtime reflex (see bedtime in lib.mjs)
let lastDriven = Date.now()
let lastBedTry = 0
let bedFailures = 0
setInterval(() => {
  if (!ready) return
  const now = Date.now()
  const tired = bedtime({
    night: isNight(bot.time.timeOfDay), busy: !!task, asleep: bot.isSleeping, bedNear: !bedChoice(bedsNear(), zones, cfg.username).error,
    hostileNear: nearbyHostiles(8).length > 0, reflexes, idleMs: now - lastDriven, sinceTryMs: now - lastBedTry, failures: bedFailures
  })
  if (!isNight(bot.time.timeOfDay) || bot.isSleeping) bedFailures = 0
  if (!tired) return
  lastBedTry = now
  if (bedFailures === 0) emit('bedtime', { note: 'night, no orders, a bed nearby: going to bed by myself' })
  // say so once a night: the driver is told, and the retries (ever further apart) stay quiet
  runLong('sleep', { timeout: 60 }).then(r => {
    const report = r.ok ? null : bedtimeReport(r.error)
    if (report && bedFailures++ === 0) emit('bedtime_failed', { error: report })
  })
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
let kickedFor = null // the stand-still (a stillFrom) whose walk I already restarted once
// bot.controlState has no enumerable keys (getters): Object.entries on it is always empty, which made every stall report say keys=[] until 09-19
const keysDown = () => ['forward', 'back', 'left', 'right', 'jump', 'sprint', 'sneak'].filter(k => bot.getControlState(k))
let lastPath = null
let livePath = [] // the pathfinder's own array: [0] is always the node it is heading for
let idleTicks = 0
let nudging = false
let fencePressed = null // the idle nudge is walking me out of a fence's cell: plan again once I am out
let walkEndedAt = 0
let goalSetAt = 0 // a path result from before this goal says nothing about this walk
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
  doorBusy
})
setInterval(() => {
  if (!ready) return
  const here = bot.entity.position.clone()
  if (!task || !stillFrom || progressed(stillFrom.pos, here) || stillFrom.task !== task.id) stillFrom = { pos: here, at: Date.now(), task: task?.id }
  const sample = { hasGoal: Boolean(task && bot.pathfinder.goal), moved: Math.hypot(here.x - stillFrom.pos.x, here.z - stillFrom.pos.z), digging: Boolean(bot.targetDigBlock), seconds: (Date.now() - stillFrom.at) / 1000, path: lastPath && lastPath.at >= goalSetAt ? lastPath : null }
  if (deadWalk(sample) && Date.now() - walkEndedAt > 4000) {
    walkEndedAt = Date.now()
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
let fleeingUntil = 0
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
  // running out of air: swim up until we can breathe again
  // (oxygenLevel alone misfires on dry land through ViaBackwards, so also require our head to be in water)
  const headInWater = bot.blockAt(me.offset(0, 1.62, 0))?.name === 'water'
  const air = airReflex({ headInWater, inWater: bot.entity.isInWater, oxygen: bot.oxygenLevel, surfacing })
  if (air === 'start') {
    surfacing = true
    surfaceStart = { at: Date.now(), y: me.y }
    // a running task steers the body every tick and wins over one press of jump: Jizo drowned that way, mid-harvest
    if (task) cancelTask('out of air: swimming up to breathe. Work from dry land, then retry')
    // Straight up is the fastest way out and needs no path at all; it is only useless under a roof (Jizo drowned beneath
    // their own farmland, built out over the lake). Chani drowned the other way: the walk to open water three blocks off
    // never arrived, and while the pathfinder called itself moving, jump was never pressed.
    swimmingUp = openAbove(columnAbove(me))
    surfaceGoal = swimmingUp ? null : bot.findBlocks({ matching: bot.registry.blocksByName.water.id, useExtraInfo: b => bot.blockAt(b.position.offset(0, 1, 0))?.name === 'air', maxDistance: 16, count: 1 })[0] ?? null
    if (surfaceGoal) { bot.pathfinder.setMovements(walkMoves); bot.pathfinder.setGoal(new goals.GoalBlock(surfaceGoal.x, surfaceGoal.y, surfaceGoal.z)) }
    else swimmingUp = true
    emit('surfacing', { oxygen: bot.oxygenLevel, to: surfaceGoal ? `${surfaceGoal.x},${surfaceGoal.y},${surfaceGoal.z}` : 'straight up' })
  }
  // the walk had its chance: drop it and swim there by hand, or we drown watching the pathfinder say it is moving.
  // Swimming is jump + forward at whatever we are looking at, so it works under an overhang too, where bare jump does not.
  if (air === 'hold' && surfacingStalled({ startedAt: surfaceStart.at, startY: surfaceStart.y, now: Date.now(), y: me.y, swimming: swimmingUp })) {
    swimmingUp = true
    bot.pathfinder.setGoal(null)
    emit('surfacing', { oxygen: bot.oxygenLevel, to: surfaceGoal ? `${surfaceGoal.x},${surfaceGoal.y},${surfaceGoal.z} by hand` : 'straight up', gaveUpOn: 'the walk lifted me nowhere in 2s' })
  }
  if ((air === 'start' || air === 'hold') && (swimmingUp || !bot.pathfinder.isMoving())) {
    bot.setControlState('jump', true)
    // no goal means straight up, and forward would only carry us under the next roof
    if (swimmingUp && surfaceGoal) { bot.lookAt(surfaceGoal.offset(0.5, 0.5, 0.5), true).catch(() => {}); bot.setControlState('forward', true) }
  }
  if (air === 'stop') {
    surfacing = false
    swimmingUp = false
    surfaceGoal = null
    bot.pathfinder.setGoal(null)
    bot.setControlState('jump', false)
    bot.setControlState('forward', false)
  }
  // an idle body sinks like a stone, then yo-yos between drowning and surfacing: tread water until the driver moves it
  const afloat = bot.entity.isInWater && !task && !surfacing && !bot.pathfinder.isMoving()
  if (afloat || floating) bot.setControlState('jump', afloat)
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
  // #97: an enderman killed Ganesha's body at its own door in five seconds. Nothing here wins that fight, so one that
  // comes within arm's reach is backed away from exactly as a creeper is, before the reflex below can think of fighting it
  const unwinnable = fleeUnwinnable(nearbyHostiles(ENDERMAN_RANGE).map(e => ({ name: e.name, dist: e.position.distanceTo(me), entity: e })))
  if (unwinnable && Date.now() > fleeingUntil) {
    fleeingUntil = Date.now() + 4000
    fighting = null
    fightStart = null
    bot.pvp.stop()
    bot.pathfinder.setGoal(new goals.GoalInvert(new goals.GoalFollow(unwinnable.entity, 16)), true)
    lastReflex = { kind: 'fleeing', mob: unwinnable.name, at: Date.now() }
    emit('fleeing', { from: unwinnable.name, note: 'not a fight this body can win: breaking line of sight' })
    setTimeout(() => { if (!task && !followTarget) bot.pathfinder.setGoal(null); else resumeFollow() }, 4000)
    return
  }
  const creeper = nearbyHostiles(6).find(e => e.name === 'creeper')
  if (creeper && Date.now() > fleeingUntil) {
    fleeingUntil = Date.now() + 4000
    bot.pvp.stop()
    bot.pathfinder.setGoal(new goals.GoalInvert(new goals.GoalFollow(creeper, 10)), true)
    lastReflex = { kind: 'fleeing', mob: 'creeper', at: Date.now() }
    emit('fleeing', { from: 'creeper' })
    setTimeout(() => { if (!task && !followTarget) bot.pathfinder.setGoal(null); else resumeFollow() }, 4000)
    return
  }
  if (Date.now() < fleeingUntil) return
  // unarmed or badly hurt: don't brawl, run. (Two deaths on night one taught me this.)
  const armed = bot.inventory.items().some(i => /_sword$|_axe$/.test(i.name))
  const where = { day: !isNight(bot.time.timeOfDay), skyLight: bot.blockAt(me)?.skyLight ?? 0 }
  const chaser = nearbyHostiles(7).filter(e => !ignorableMob(e.name, where))
    .sort((a, b) => a.position.distanceTo(me) - b.position.distanceTo(me))[0]
  const archer = nearbyHostiles(24).filter(e => ARCHERS.has(e.name)).sort((a, b) => a.position.distanceTo(me) - b.position.distanceTo(me))[0]
  const ranged = rangedThreat({ hurtMsAgo: Date.now() - lastHurt, fighting: Boolean(fighting), armed, health: bot.health, archerNear: Boolean(archer), inWater: bot.entity.isInWater, meleeNear: nearbyHostiles(5).some(e => !ARCHERS.has(e.name)) })
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
  const outmatched = shouldFlee({ armed, health: bot.health, attackers: Math.max(crowd, 1), armorPieces: [5, 6, 7, 8].filter(slot => bot.inventory.slots[slot]).length })
  // an archer that just hit me counts like a chaser: mid-charge rangedThreat is silent, and a patrol shot Jizo from 20 to 0 that way
  if (((chaser || (archer && Date.now() - lastHurt < 5000)) && outmatched) || ranged === 'flee') {
    const from = chaser ?? archer
    fleeingUntil = Date.now() + 3000
    fighting = null
    fightStart = null
    bot.pvp.stop()
    bot.pathfinder.setGoal(new goals.GoalInvert(new goals.GoalFollow(from, ARCHERS.has(from.name) ? 28 : 16)), true)
    lastReflex = { kind: 'fleeing', mob: from.name, at: Date.now() }
    emit('fleeing', { from: from.name, health: Math.round(bot.health), armed })
    return
  }
  if (fighting && (!fighting.isValid || fighting.position.distanceTo(me) > (ARCHERS.has(fighting.name) ? 28 : 12))) {
    fighting = null
    fightStart = null
    bot.pvp.stop()
    resumeFollow()
  }
  // pvp walks the body after the mob, so the mob never gets far from it: the leash is measured from where the fight
  // began instead, and a fight that pulls the body down a hole is broken off before it becomes the cave it died in
  const overLeash = fighting && chaseBroken(fightStart, me, { leash: chaseLeash })
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
    if (threat) {
      fighting = threat
      fightStart = me.clone()
      chaseLeash = CHASE_LEASH
      equipBestWeapon().finally(() => bot.pvp.attack(threat))
      lastReflex = { kind: 'fighting', mob: threat.name, at: Date.now() }
      emit('fighting', { mob: threat.name, health: Math.round(bot.health) })
    }
  }
}

async function equipBestWeapon () {
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

// ---------------------------------------------------------------- helpers
function matcher (names) {
  const list = (Array.isArray(names) ? names : [names]).map(n => new RegExp('^' + String(n).replace(/\*/g, '.*') + '$'))
  return name => list.some(r => r.test(name))
}
const countsOf = items => items.reduce((out, i) => ({ ...out, [i.name]: (out[i.name] ?? 0) + i.count }), {})

// Move items between me and a chest and make sure it really happened. Through ViaBackwards a transfer is sometimes lost, lands late, or
// takes a whole stack along (asked 8 wheat, got 24; asked to deposit 30 planks, 36 went), and my own counts only tell the truth right
// after a window opens, when the server sends every slot afresh. So: move, shut, open again, compare with the plan, put right; 3 rounds at most
async function chestTransfer (a, way, planFor) {
  const undo = way === 'withdraw' ? 'deposit' : 'withdraw'
  const run = async (chest, dir, list) => {
    for (const t of list) {
      await chest[dir](mcData.itemsByName[t.name].id, null, t.count)
      await bot.waitForTicks(6)
    }
  }
  const first = await bot.openContainer(await containerAt(a))
  const before = countsOf(first.items())
  const corrections = []
  let settled = false
  let plan
  try {
    plan = planFor(first)
    await run(first, way, plan.take).catch(e => { throw new Error(fullSide(e.message, way)) })
  } finally { first.close() }
  for (let round = 0; round < 3; round++) {
    await bot.waitForTicks(10)
    const chest = await bot.openContainer(await containerAt(a))
    try {
      const now = countsOf(chest.items())
      const fix = way === 'withdraw' ? transferFix(plan.take, before, now) : transferFix(plan.take, now, before)
      settled = !fix.back.length && !fix.more.length
      if (settled) break
      corrections.push(...[...fix.back, ...fix.more].map(t => `${t.name}:${t.count}`))
      await run(chest, undo, fix.back)
      await run(chest, way, fix.more)
    } finally { chest.close() }
  }
  if (!settled) throw new Error(`the ${way} keeps going wrong (off by ${corrections.join(' ')}): compare chest_contents and inventory before you go on`)
  return { plan, corrected: corrections.length ? `the first try was off by ${corrections.join(' ')}: put right` : undefined }
}

// everything that drops: what inventoryCounts sees plus the armour being worn and the off hand, which live in window
// slots of their own. A died line that leaves out the helmet you were wearing is the line that loses it.
const WORN = [5, 6, 7, 8, 45]
const carried = () => WORN.reduce((out, slot) => {
  const item = bot.inventory?.slots?.[slot]
  return item ? { ...out, [item.name]: (out[item.name] || 0) + item.count } : out
}, bot.inventory ? inventoryCounts() : {})

function inventoryCounts () {
  const out = {}
  for (const i of bot.inventory.items()) out[i.name] = (out[i.name] || 0) + i.count
  return out
}
function diffCounts (before, after) {
  const gained = {}; const lost = {}
  for (const k of new Set([...Object.keys(before), ...Object.keys(after)])) {
    const d = (after[k] || 0) - (before[k] || 0)
    if (d > 0) gained[k] = d
    if (d < 0) lost[k] = -d
  }
  return { gained, lost }
}
function findItem (name) {
  const m = matcher(name)
  const item = bot.inventory.items().find(i => m(i.name))
  if (!item) throw new Error(`no ${name} in inventory`)
  return item
}
const vecOf = a => {
  const error = coordsError(a)
  if (error) throw new Error(error)
  return new Vec3(Math.floor(a.x), Math.floor(a.y), Math.floor(a.z))
}
// Every dropped item lying about: what it is, where it lies, whether it is deep in water and whether it is outside the
// pen I stand in. Entity tracking is the body's own knowledge, so this is what the collect action is built on (through
// api.drops) and what dig and shear use to fetch what they just knocked loose.
function dropsNear (range = 16) {
  const me = bot.entity.position
  const pen = penAround(me.floored())
  const inPen = e => !pen?.fenced || unpenned(pen.floor, [e], x => x.position).length === 0
  const water = p => bot.blockAt(p)?.name === 'water'
  return Object.values(bot.entities)
    .filter(e => e.name === 'item' && e.position && e.position.distanceTo(me) <= range)
    .map(e => ({
      id: e.id,
      item: mcData.items[e.getDroppedItem?.()?.type]?.name ?? 'item',
      x: Math.floor(e.position.x),
      y: Math.floor(e.position.y),
      z: Math.floor(e.position.z),
      dist: e.position.distanceTo(me),
      deep: water(e.position) && water(e.position.offset(0, -1, 0)),
      outsidePen: !inPen(e)
    }))
    .sort((p, q) => p.dist - q.dist)
}

// what dig, harvest and shear do with what they knocked loose: a few steps and no fuss. The collect ACTION is the
// composite in library/collect.mjs; this is the reflex that belongs to digging something up in the first place.
async function sweepDrops (range = 8) {
  const tried = new Set()
  for (let i = 0; i < 8; i++) {
    const drop = nextDrop(dropsNear(range).filter(d => !d.outsidePen), tried, false)
    if (!drop) break
    tried.add(drop.id)
    await bot.pathfinder.goto(new goals.GoalNear(drop.x, drop.y, drop.z, 0)).catch(() => {})
    await bot.waitForTicks(10)
    if (!bot.inventory.emptySlotCount()) break
  }
  const left = dropsNear(range).filter(d => !d.outsidePen)
  return leftLying(bot.inventory.emptySlotCount(), left.map(d => d.item), left.filter(d => d.deep).map(d => d.item))
}

async function goNear (v, range = 2) {
  // already there: don't ask the pathfinder, which can fail from a perch (pillar top, ledge) even though nothing needs walking
  if (bot.entity.position.distanceTo(new Vec3(v.x + 0.5, v.y, v.z + 0.5)) <= range) return
  await bot.pathfinder.goto(new goals.GoalNear(v.x, v.y, v.z, range))
}
function findBlockByName (names, maxDistance = 48, count = 1) {
  const m = matcher(names)
  const ids = Object.values(mcData.blocksByName).filter(b => m(b.name)).map(b => b.id)
  if (!ids.length) throw new Error(`unknown block name: ${names}`)
  return bot.findBlocks({ matching: ids, maxDistance, count })
}
const bedsNear = () => findBlockByName('*_bed', 32, 16).sort((p, q) => p.distanceTo(bot.entity.position) - q.distanceTo(bot.entity.position))
// One batch of a recipe. bot.craft clicks the table itself and starts filling the grid as soon as anything answers, so
// after a walk in the click's own look could still be turning while the recipe ran against no window at all and the
// server rejected every batch (bug #91, and #74's "12/15 made"). Open the window here, wait for it, then craft into it.
async function craftBatch (recipe, table) {
  if (!table) return within(15000, bot.craft(recipe, 1, null), 'crafting').catch(() => {})
  await goNear(table.position, 2)
  bot.pathfinder.setGoal(null)
  await bot.lookAt(table.position.offset(0.5, 0.5, 0.5), true)
  const window = await within(6000, bot.openBlock(table), 'opening the crafting table').catch(() => null)
  if (!window) return
  if (!String(window.type ?? '').startsWith('minecraft:crafting')) {
    bot.closeWindow(window)
    throw new Error(`${table.name} at ${table.position.x},${table.position.y},${table.position.z} opened a ${window.type}, not a crafting grid`)
  }
  // the plugin opens the table itself, and a second click with the window already open reopens it under a new id
  // mid-recipe: stub that click out and hand its `once('windowOpen')` the window we are already holding
  const activate = bot.activateBlock
  bot.activateBlock = async () => {}
  try {
    const crafting = bot.craft(recipe, 1, table)
    bot.emit('windowOpen', window)
    await within(15000, crafting, 'crafting').catch(() => {})
  } finally {
    bot.activateBlock = activate
    if (bot.currentWindow) bot.closeWindow(bot.currentWindow)
  }
}

async function containerAt (a, names = ['chest', 'barrel', 'trapped_chest']) {
  const p = a.x !== undefined ? vecOf(a) : findBlockByName(names, 32)[0]
  if (!p) throw new Error('no container found nearby')
  await goNear(p, 2)
  return bot.blockAt(p)
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
    const unreachable = await goNear(p, 3).then(() => null, e => e)
    if (unreachable) { skip('cannot get within reach'); continue }
    const block = bot.blockAt(p)
    if (block?.name === work.to) continue
    if (!work.from.includes(block?.name)) { skip(`can't turn ${block?.name ?? 'nothing'} into ${work.to}`); continue }
    const cover = bot.blockAt(p.offset(0, 1, 0))
    if (isGroundCover(cover?.name ?? '')) await bot.dig(cover)
    await bot.equip(tool, 'hand')
    await bot.activateBlock(bot.blockAt(p))
    await bot.waitForTicks(5)
    const now = bot.blockAt(p)?.name
    if (now !== work.to) { skip(`still ${now}: is there a block on top of it?`); continue }
    done++
    worked.push(p)
  }
  const outcome = placeOutcome(done, skipped, work.verb)
  if (outcome.error) throw new Error(outcome.error)
  if (work.to !== 'farmland' || !worked.length) return outcome
  // dry, unplanted farmland is grass again within minutes: two agents took that for a till that lied (BUGS.md 09-19)
  const waters = bot.findBlocks({ point: worked[0], matching: mcData.blocksByName.water.id, maxDistance: 24, count: 200 }).map(w => w.toArray())
  const dry = dryCells(worked.map(w => w.toArray()), waters)
  // wet or not, farmland with nothing planted in it does not last: the warning always comes
  return { ...outcome, [dry.length ? 'dry' : 'advice']: tillWarning(dry.length, worked.length) }
}

const long = {
  async goto (a) {
    if (a.place) {
      const p = readPlaces().find(q => q.name === a.place)
      if (!p) throw new Error(`no place called ${a.place}; see ./mc places`)
      await bot.pathfinder.goto(new goals.GoalNear(p.x, p.y, p.z, a.range ?? 2))
    } else if (a.player) {
      const e = bot.players[a.player]?.entity
      if (!e) throw new Error(`can't see ${a.player}`)
      await bot.pathfinder.goto(new goals.GoalFollow(e, a.range ?? 2))
    } else if (coordsError(a, a.y !== undefined)) {
      throw new Error(coordsError(a, a.y !== undefined))
    } else if (a.y === undefined) {
      await bot.pathfinder.goto(new goals.GoalNearXZ(a.x, a.z, a.range ?? 1))
      // an x/z goal is met at any depth, and a walk that may not dig likes caves: say so rather than let the driver assume the surface
      if (bot.blockAt(bot.entity.position.offset(0, 1, 0))?.skyLight === 0) return { pos: pos(), underground: 'no sky above you: an x/z goal is met at any depth. For a spot on the surface pass y= as well' }
    } else {
      await bot.pathfinder.goto(new goals.GoalNear(a.x, a.y, a.z, a.range ?? 1))
    }
    return { pos: pos() }
  },

  async dig (a) {
    const p = vecOf(a)
    const refusal = digRefusal(bot.blockAt(p)?.name, [1, 2, 3].map(dy => bot.blockAt(p.offset(0, dy, 0))?.name), a.wet === true)
    if (refusal) throw new Error(refusal)
    await goNear(p, 3)
    const block = bot.blockAt(p)
    if (!block || block.name === 'air') return { already: 'air' }
    const needed = missingTool(block.harvestTools, bot.inventory.items().map(i => i.type), id => bot.registry.items[id].name)
    if (needed) throw new Error(`${block.name} needs a ${needed} or better: you carry none, craft one first`)
    await bot.tool.equipForBlock(block)
    await bot.dig(block)
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
    const alive = cancelGuard()
    // a cell that cannot be reached or has nothing to attach to yet is skipped and tried once more at the end (its neighbours may exist by then)
    class Skip extends Error {}
    const already = new Set()
    const placeOne = async b => {
      const p = vecOf(b)
      const existing = bot.blockAt(p)
      const state = occupiedBy(existing, b.item)
      if (state === 'skip') { already.add(`${b.x},${b.y},${b.z}`); return }
      // on lumpy ground part of a wall is often terrain already: skip that cell and build the rest (Aviendha's pen, 09-19)
      if (state === 'blocked') throw new Skip(`${existing.name} is already there`)
      if (state === 'clear') { await goNear(p, 3); await bot.dig(existing) }
      // only for what is not a block (occupiedBy dealt with those): a crop, a flower, or the wrong ground for a seed
      const obstacle = state === 'free' && existing ? placeObstacle(b.item ?? a.item, existing.name, bot.blockAt(p.offset(0, -1, 0))?.name) : null
      if (obstacle) throw new Skip(obstacle)
      // walking is only needed when the block is out of reach or inside our own body; route searches on rough ground can time out
      if (!canPlaceFromHere(bot.entity.position, p)) {
        const unreachable = await bot.pathfinder.goto(new goals.GoalPlaceBlock(p, bot.world, { range: 4 })).then(() => null, e => e)
        if (unreachable) throw new Skip('cannot get within reach')
      }
      await bot.equip(findItem(b.item ?? a.item), 'hand')
      const before = bot.blockAt(p)?.name
      const faces = [[0, -1, 0], [0, 1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]].map(f => new Vec3(...f))
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
        if (bot.blockAt(p)?.boundingBox !== 'block') throw new Error(`placing ${b.item ?? a.item} at ${p} did not take`)
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
    for (let attempt = 1; have() < start + count; attempt++) {
      const full = craftRoom({ freeSlots: bot.inventory.emptySlotCount(), stacks: bot.inventory.items().filter(i => i.name === a.item).map(i => i.count), stackSize: item.stackSize, batch: recipe.result.count, item: a.item, made: have() - start, count })
      if (full) throw new Error(full)
      if (attempt > Math.ceil(count / recipe.result.count) + 5) throw new Error(`server kept rejecting the craft (${have() - start}/${count} made)`)
      const r = bot.recipesFor(item.id, null, 1, table)[0]
      if (!r) throw new Error(`ran out of ingredients (${have() - start}/${count} made)`)
      await craftBatch(r, table)
      await bot.waitForTicks(8)
    }
    return { crafted: a.item }
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

  async deposit (a) {
    const wanted = depositWanted(a, bot.inventory.items().map(i => ({ name: i.name, count: i.count })))
    if (wanted.error) throw new Error(wanted.error)
    // same plan as withdraw, the other way round: what I carry is the source
    const { plan, corrected } = await chestTransfer(a, 'deposit', chest => withdrawPlan(wanted, countsOf(chest.items())))
    if (plan.short.length) throw new Error(`you carry less than asked (have/wanted): ${plan.short.join(' ')}; deposited what there was`)
    return corrected ? { corrected } : {}
  },

  async withdraw (a) {
    const { plan, corrected } = await chestTransfer(a, 'withdraw', chest => withdrawPlan(itemsArg(a), countsOf(chest.containerItems())))
    if (plan.short.length) throw new Error(`chest has less than asked (have/wanted): ${plan.short.join(' ')}; took what there was`)
    return corrected ? { corrected } : {}
  },

  async chest_contents (a) {
    const chest = await bot.openContainer(await containerAt(a))
    const items = {}
    for (const i of chest.containerItems()) items[i.name] = (items[i.name] || 0) + i.count
    chest.close()
    return { items }
  },

  async give (a) {
    const e = bot.players[a.player]?.entity
    if (!e) throw new Error(`can't see ${a.player}`)
    await bot.pathfinder.goto(new goals.GoalFollow(e, 2))
    // a fleeing or walking player is gone again by the time we toss: keep the items rather than litter
    const dist = bot.entity.position.distanceTo(e.position)
    if (dist > 3.5) throw new Error(`${a.player} moved away (${Math.round(dist)}m): nothing given. Ask them to stand still, or use a chest`)
    await bot.lookAt(e.position.offset(0, 1.2, 0))
    const item = findItem(a.item)
    const drops = () => Object.values(bot.entities).filter(d => d.name === 'item' && d.position.distanceTo(bot.entity.position) <= 8)
    const before = new Set(drops().map(d => d.id))
    const had = inventoryCounts()[item.name] ?? 0
    const tossed = Math.min(a.count ?? item.count, item.count)
    await bot.toss(item.type, null, tossed)
    // did it arrive? Watch my own drop: gone within 5 s = picked up (the toss itself said ok even when nobody got the bread)
    const mine = () => drops().filter(d => !before.has(d.id))
    await bot.waitForTicks(10)
    for (let i = 0; i < 18 && mine().length; i++) await bot.waitForTicks(5)
    // my own drop is mine again after 2 s: across a fence it falls at my feet and I pick it up myself, which looked like taken
    const cameBack = Math.max(0, (inventoryCounts()[item.name] ?? 0) - (had - tossed))
    return giveReport(a.player, mine().map(d => `${Math.floor(d.position.x)},${Math.floor(d.position.y)},${Math.floor(d.position.z)}`), cameBack)
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
    const at = vecOf(a)
    if (!bot.blockAt(at) || bot.blockAt(at).name === 'air') throw new Error(`nothing at ${a.x},${a.y},${a.z} to use`)
    await goNear(at, 3)
    if (a.item) await bot.equip(findItem(a.item), 'hand')
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
    const candidates = inRange.slice(0, 6).map(e => ({ id: e.id, at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`, penned: Boolean(penAround(e.position.floored())?.enclosed), grown: !isBaby(e.metadata) }))
    const picked = leadPick(candidates, a.penned === true, a.mob)
    if (picked.error) throw new Error(`no ${a.mob} to lead: ${picked.error}`)
    const first = inRange.find(e => e.id === picked.id)
    const alive = cancelGuard()
    luring = true
    try {
      await bot.equip(bot.inventory.items().find(i => i.name === foodName), 'hand')
      // one that stands in a pen: INTO the pen, to its own cell. Two blocks from it is also a spot outside the fence, and from there I walked off without ever
      // opening the gate (my sheep, with=0 twice: it stood at the shut gate and watched the wheat go)
      if (candidates.find(c => c.id === picked.id).penned) await goNear(first.position.floored(), 0).catch(() => {})
      else await bot.pathfinder.goto(new goals.GoalFollow(first, 2))
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
      following = herd
      leading = true
      while (!goal.isEnd(bot.entity.position.floored())) {
        alive()
        const toGo = bot.entity.position.distanceTo(new Vec3(to.x, to.y, to.z))
        if (toGo < bestToGo - 8) { bestToGo = toGo; fetchesSinceProgress = 0 }
        const noPath = walking && lastPath?.status === 'noPath' && lastPath.at > walkingSince
        const verdict = leadVerdict({ distances: herd.filter(e => e.isValid).map(near), holding, heldFor: holding ? (Date.now() - heldSince) / 1000 : 0, fetchesSinceProgress, noPath })
        if (verdict === 'noway') { const along = herd.filter(e => e.isValid && near(e) <= 5); bot.pathfinder.setGoal(null); return { arrived: false, with: along.length, brought: ledReport(a.mob, along), toGo: Math.round(toGo), why: `no route on foot from here to ${to.x},${to.y},${to.z}. One of: the spot is not free floor to stand on; the gate is in a corner or something stands outside it (pen.check names such gates: blindGates=); a gap, drop or fence somewhere between here and there. The animals are with you: walk the way yourself (goto), fix what blocks it, then lead again`, pos: pos() } }
        if (verdict === 'giveup') { bot.pathfinder.setGoal(null); return { arrived: false, with: 0, why: `the ${a.mob} will not follow (fetched it 3 times, got no nearer): is there a fence or water between you? Get them out in the open first, or lead fewer`, pos: pos() } }
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
      // one that followed me along the OUTSIDE of the fence never finds the gate by itself, and the gate stood open for 20 s while I waited (a sheep of Dan's
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
    const results = []
    const alive = cancelGuard()
    for (const [i, { action, ...args }] of a.steps.entries()) {
      alive()
      const fn = action === 'run' ? null : long[action] ?? quick[action]
      const where = `step ${i + 1}/${a.steps.length} (${action})`
      if (!fn) throw new Error(`${where}: unknown action`)
      const refusal = refusalFor(action, args)
      if (refusal) throw new Error(`${where}: ${refusal}`)
      useMoves(mayDig(action, args))
      const r = await Promise.resolve().then(() => fn(args)).catch(e => { throw new Error(`${where}: ${explainFailure(e.message)}`) })
      results.push({ action, ...r })
    }
    return { results }
  },

  async sleep (a) {
    // a taken bed is passed over for the next one I may use (a shared bedroom: "the bed is occupied" was the end of the night)
    const occupied = new Set()
    for (;;) {
      const { bed: p, error } = bedChoice(bedsNear(), zones, cfg.username, a.any === true, occupied)
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
  const hits = bot.findBlocks({ matching: ids, maxDistance: within, count: 512, point: centre }).filter(p => matchesProps(bot.blockAt(p)?.getProperties(), w.where))
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

const quick = {
  // the catalogue every driver starts from: each action with its arguments, and for a composite what hands the body back.
  // It is built from the dispatch tables themselves, so it cannot drift from what this body can actually do.
  help (a) {
    const served = name => Boolean(long[name] || quick[name]) || CLI_ONLY.includes(name)
    const entries = [
      ...Object.entries(PRIMITIVES).filter(([name]) => served(name)).map(([name, p]) => ({ name, ...p })),
      ...[...composites].map(([name, mod]) => ({ name, args: argsUsage(mod.args), doc: docText(mod.doc), stops: mod.stops ?? 'the usual hand-backs' }))
    ]
    return { text: helpText(a.topic, entries) }
  },

  // stop this body for good (logging off for the night, or done playing): answers first, then leaves the server and exits
  quit: () => {
    emit('quit', {})
    setTimeout(() => { bot.quit('quit'); process.exit(0) }, 200)
    return { note: 'body stopped: ./start (in the background) brings it back' }
  },
  // debugging aid: what the pathfinder makes of a walk from here, without walking it. stroll=true: with lead's movements (no sprint, no parkour)
  path_to: (a) => {
    const moves = makeMoves(a.dig === true)
    if (a.stroll) { moves.allowSprinting = false; moves.allowParkour = false }
    const began = Date.now()
    let r = bot.pathfinder.getPathTo(moves, new goals.GoalNear(a.x, a.y, a.z, a.range ?? 0), 5000)
    // one call searches for a single 40 ms slice: go on the way a walk does, until it is done or the 5 s a walk gets are over
    while (r.status === 'partial' && r.context && Date.now() - began < 5000) r = Object.assign(r.context.compute(), { context: r.context })
    const last = r.path[r.path.length - 1]
    return { status: r.status, ms: Date.now() - began, nodes: r.path.length, cost: Math.round(r.cost), visited: r.visitedNodes, ends: last ? `${last.x},${last.y},${last.z}` : 'here', gates: r.path.filter(n => n.toPlace?.some(t => t.useOne)).length }
  },
  // debugging aid: the raw metadata of the nearest entities with this name (how does the server mark a shorn sheep?)
  entity: (a) => ({
    found: Object.values(bot.entities).filter(e => e !== bot.entity && matcher(a.name)(e.name ?? ''))
      .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position)).slice(0, a.count ?? 2)
      .map(e => ({ id: e.id, dist: Math.round(e.position.distanceTo(bot.entity.position)), at: e.position.floored().toArray().join(','), exact: e.position.toArray().map(n => Math.round(n * 100) / 100).join(','), metadata: JSON.stringify(e.metadata) }))
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
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      time: `${isNight(bot.time.timeOfDay) ? 'night' : 'day'} ${bot.time.timeOfDay}`,
      pos: pos(),
      dimension: bot.game.dimension === 'overworld' ? null : bot.game.dimension,
      raining: bot.isRaining,
      holding: bot.heldItem?.name,
      asleep: bot.isSleeping,
      doing: task && `${task.name} ${Math.round((Date.now() - task.started) / 1000)}s`,
      following: followTarget,
      reflexesOff: !reflexes,
      players: Object.fromEntries(others.map(p => [p.username, p.entity ? roundVec(p.entity.position) : 'out of sight']))
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
        .filter(e => e.name && CREATURE_FOOD[e.name] && wanted(e.name) && near(e) <= (a.within ?? 24))
        .sort((x, y) => near(x) - near(y))
        .map(e => ({
          mob: e.name,
          id: e.id,
          at: `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`,
          dist: Math.round(near(e)),
          grown: !isBaby(e.metadata),
          inMyPen: Boolean(floor) && unpenned(floor, [e], x => x.position).length === 0
        }))
    }
  },

  find_blocks (a) {
    return { positions: findBlockByName(a.block, a.maxDistance ?? 64, a.count ?? 10) }
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
    if (!found.enclosed) return { pen: 'LEAKS', via: found.via, advice: 'an animal can walk out: via= is where (x,height,z): one spot = a gap or open gate on level ground; three = the step it climbs, the barrier top it crosses, where it lands. A fence or wall must stand 2 above EVERY block next to it, inside and out, corner to corner included. Fix it and check again' }
    const census = { ...censusOf(found.floor), ...blindGateAdvice(found.floor) }
    if (found.cells >= 16) return { pen: 'holds', cells: found.cells, ...census }
    return { pen: 'holds', cells: found.cells, ...census, advice: 'but it is small: an animal led in stops 2.5 blocks from you, so under 16 cells it stops in the gateway' }
  },

  block_at (a) {
    const b = bot.blockAt(vecOf(a))
    return b ? { name: b.name, properties: b.getProperties?.() } : { name: null }
  },

  // render what the bot sees to a PNG (see eyes.mjs): look | look pano=true | look dir=north | look x= y= z=
  look (a) { return eyes(a) },

  // shared points of interest: mark name= kind=<base|mine|farm|village|danger|resource|...> note= [x= y= z=, default: here]
  // map= saves an ASCII plan with the place (see farm.plan, which is what validates one). Marking a place again keeps
  // the plan and anything else already saved under that name: only what you pass is replaced.
  mark (a) {
    if (!a.name) throw new Error('mark needs name= (and ideally kind= and note=)')
    const saved = readPlaces().find(p => p.name === a.name)
    const at = a.x === undefined ? bot.entity.position : a
    const plan = a.map === undefined ? saved?.plan : parsePlan(a.map).rows?.join('\n')
    const errors = a.map === undefined ? [] : planErrors(parsePlan(a.map))
    if (errors.length) throw new Error(errors.join('; '))
    const place = { ...saved, name: String(a.name), kind: a.kind ?? saved?.kind ?? 'place', x: Math.floor(at.x), y: Math.floor(at.y), z: Math.floor(at.z), by: bot.username, note: String(a.note ?? saved?.note ?? '').slice(0, 80), plan }
    savePlaces([...readPlaces().filter(p => p.name !== place.name), place])
    return { marked: place.name, plan: plan ? `${plan.split('\n')[0].length}x${plan.split('\n').length}` : undefined }
  },
  unmark (a) {
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
    const nameAt = (x, y, z) => bot.blockAt(new Vec3(x, y, z))?.name ?? 'unloaded'
    // where= answers with coordinates only: the picture is the dear part, and whoever asks where wants to act, not to look
    return a.where ? { where: scanWhere(nameAt, a, a.where) } : { map: renderScan(nameAt, a) }
  },

  chat (a) {
    const said = chatText(a, 250)
    if (said.error) throw new Error(said.error)
    bot.chat(said.text)
    return {}
  },
  whisper (a) {
    const said = chatText(a, 230)
    if (said.error) throw new Error(said.error)
    bot.whisper(a.player, said.text)
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
    const edible = bot.inventory.items().filter(i => bot.registry.foodsByName?.[i.name] && !BANNED_FOOD.includes(i.name))
    const refusal = eatRefusal({ food: bot.food, item: a.item, edible: [...new Set(edible.map(i => i.name))] })
    if (refusal) throw new Error(refusal)
    const before = bot.food
    // sanitizeOpts writes its choice back into this object, so an eat with no item= still says what it ate
    const opts = a.item ? { food: edible.find(i => i.name === a.item) } : {}
    // with strictErrors off a failed meal resolves and emits eatFail instead of throwing: catch both, or `ate` would lie
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
    if (failure) throw new Error(eatFailure(failure))
    await bot.waitForTicks(5) // the food number comes in the update_health after the meal, not with it
    return { ate: opts.food?.name ?? null, gained: bot.food - before, food: bot.food, health: Math.round(bot.health) }
  },

  async wake () {
    if (!bot.isSleeping) throw new Error('already awake')
    const woke = new Promise(resolve => bot.once('wake', resolve))
    leaveBed()
    await within(3000, woke, 'waking up')
    return {}
  },

  follow (a) {
    cancelTask('follow')
    followTarget = a.player
    if (!bot.players[a.player]?.entity) throw new Error(`can't see ${a.player} right now`)
    resumeFollow()
    return { following: a.player }
  },

  // raw movement for debugging: hold a control (forward/back/left/right/jump/sprint) for ms
  async control (a) {
    const from = pos()
    bot.setControlState(a.state ?? 'forward', true)
    await new Promise(r => setTimeout(r, a.ms ?? 1000))
    bot.setControlState(a.state ?? 'forward', false)
    return { from, to: pos(), onGround: bot.entity.onGround, velocity: roundVec(bot.entity.velocity) }
  },

  stop () { cancelTask('stop'); followTarget = null; return {} },
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

function cancelTask (why) {
  gen++
  if (task) emit('task_cancelled', { id: task.id, name: task.name, why })
  if (task) lastCancel = { id: task.id, why }
  task = null
  fighting = null
  fightStart = null
  bot.pathfinder.setGoal(null)
  bot.pvp.stop()
  try { bot.collectBlock.cancelTask() } catch {}
}

// drowned once while mining at 4 hp; don't start risky work half dead unless told to
const refusalFor = (name, args) => refuseReason({ name, health: bot.health, force: args.force, sleeping: bot.isSleeping })

let taskId = 0
let lastCancel = null
let lastReflex = null
const recentReflex = () => lastReflex && { ...lastReflex, agoMs: Date.now() - lastReflex.at }
const explainFailure = message => explainNoPath(explainInterrupt(message, recentReflex()), digging)
// given: the plain arguments, for the log (printing the tracked ones would count as reading them all)
// the gate reflex only reaches 5 blocks and can miss at a sprint: whatever I opened and is still open when a task ends gets shut now.
// An open gate empties a pen (Dan's sheep after lead, Kettricken's after flock.breed, Miles' after shear and goto)
// gates on the ring of this pen (floor: "x,y,z" keys) that nothing can walk through: see blindGates
function blindGateAdvice (floor) {
  const cells = floor.map(k => k.split(',').map(Number))
  const inFloor = (x, z) => cells.some(c => c[0] === x && c[2] === z)
  const around = [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => [dx, dz]))
  const gates = [...new Map(cells.flatMap(([x, y, z]) => around.map(([dx, dz]) => new Vec3(x + dx, Math.floor(y), z + dz)))
    .filter(p => bot.blockAt(p)?.name.endsWith('_fence_gate')).map(p => [String(p), p])).values()]
  const blind = gates.flatMap(g => blindGates([g], inFloor, (x, z) => bot.blockAt(new Vec3(x, g.y, z))?.boundingBox === 'block'))
  return blind.length ? { blindGates: `${blind.join(' ')}: nothing can walk through (a gate in a CORNER of the fence, or something stands right outside it). Put the gate in the middle of a wall, open ground straight across` } : {}
}

// the pen around a floor cell, walked the way an animal can (see penLeak); null when that cell is no spot to stand on
function penAround (feet, radius) {
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
  return found.enclosed ? { ...found, fenced: fencedIn(found.floor, topsAt) } : found
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
function censusOf (floor) {
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

async function runLong (name, args, given = args) {
  // morning, and the server still has me in bed: get up rather than refuse (Arren added a `wake` to every morning after "you are asleep: wake first")
  if (bot.isSleeping && name !== 'wake' && oversleeping({ asleep: true, timeOfDay: bot.time.timeOfDay, thundering: bot.thunderState > 0 })) await long.wake().catch(() => {})
  const refusal = refusalFor(name, args)
  if (refusal) return { ok: false, error: refusal }
  cancelTask(`superseded by ${name}`)
  followTarget = null
  const mine = { id: ++taskId, name, gen, started: Date.now() }
  task = mine
  console.log(`[task ${mine.id}] ${name} ${JSON.stringify(given)}`)
  gatesPassed.clear()
  useMoves(mayDig(name, args))
  await leaveFenceCell().catch(() => {})
  scaffolded = []
  const before = inventoryCounts()
  const finish = (extra) => {
    const seconds = Math.round((Date.now() - mine.started) / 1000)
    // low vitals ride along so the driver needn't poll state
    const vitals = bot.health <= 10 || bot.food <= 8 ? { hp: Math.round(bot.health), food: bot.food } : {}
    const result = { task: mine.id, action: name, seconds, ...diffCounts(before, inventoryCounts()), pos: pos(), ...vitals, ...extra }
    // back to walking, so a later flee or follow doesn't tunnel
    if (task === mine) { task = null; useMoves(false); bot.setControlState('sneak', false) }
    return result
  }
  // inventory updates trail the action by a few ticks; wait so gained/lost are accurate
  // ...and until two looks 5 ticks apart agree (1 s at most): after a transfer that failed part-way the server's resync came later still, and its
  // -bamboo:64 turned up in the NEXT command's reply
  const settle = async () => {
    for (let i = 0, last = null; i < 4; i++) {
      await bot.waitForTicks(5).catch(() => {})
      const now = JSON.stringify(inventoryCounts())
      if (now === last) return
      last = now
    }
  }
  // not after toggle (it is the tool for this); not when another task has taken over
  // bamboo bases the wedge reflex dug to free me: plant them again, whether the task worked or not
  const replantBases = async () => {
    if (task !== mine || !basesOwed.length) return {}
    const owed = basesOwed.splice(0)
    const { placed = 0 } = await long.place({ blocks: owed }).catch(() => ({}))
    return { bambooReplanted: `${placed} of ${owed.length} bases I dug to free myself${placed < owed.length ? `: plant the rest (place item=bamboo at ${owed.map(o => `${o.x},${o.y},${o.z}`).join(' ')})` : ''}` }
  }
  // what the walk climbed on: dig back the pillars still standing within reach, and say where the rest are
  const reclaimScaffold = async () => {
    if (!scaffolded.length) return {}
    const tally = cells => cells.reduce((n, c) => ({ ...n, [c.name]: (n[c.name] ?? 0) + 1 }), {})
    // what it built and what is still there differ: a later leg of the same walk digs its own steps away again
    const standing = scaffoldBuilt(scaffolded, c => bot.blockAt(new Vec3(c.x, c.y, c.z))?.name ?? null)
    const taken = []
    // a walk that failed leaves its pillars too, and must still say so; only one that still has the body may dig them back
    for (const c of task === mine ? scaffoldTakeBack(standing, bot.entity.position) : []) {
      const dug = await long.dig({ x: c.x, y: c.y, z: c.z }).then(() => true, () => false)
      if (dug) taken.push(c)
    }
    const left = standing.filter(c => !taken.includes(c))
    const note = scaffoldNote(tally(scaffolded), tally(taken), left)
    return note ? { scaffold: note } : {}
  }
  const tidy = async r => {
    if (task !== mine || name === 'toggle') return { ...r, ...await replantBases(), ...await reclaimScaffold().catch(() => ({})) }
    r = { ...r, ...await replantBases(), ...await reclaimScaffold().catch(() => ({})) }
    const { shut: gatesShut, far: gatesLeftOpen } = await shutGatesBehind().catch(() => ({ shut: 0 }))
    // an animal that left the pen at my heels: say so now, not at nightfall when the pen is empty (Kettricken built an airlock over this)
    const out = [...gatesPassed].map(straysAt).filter(Boolean).join(' ')
    const outsideGate = out ? `${out}: outside the pen gate you just used. If it belongs inside it slipped out with you: lead it back now (flock.lead mob= x= y= z= of a cell inside)` : undefined
    return { ...r, ...(gatesShut ? { gatesShut } : {}), ...(gatesLeftOpen ? { gatesLeftOpen } : {}), ...(outsideGate ? { outsideGate } : {}) }
  }
  const work = long[name](args).then(tidy).then(
    r => settle().then(() => finish({ ok: true, ...r })),
    // a cancelled task fails with the pathfinder's vague "goal was changed": say why it was cancelled instead
    e => settle().then(replantBases).then(async b => ({ ...b, ...await reclaimScaffold().catch(() => ({})) })).then(b => finish({ ...b, ok: false, error: lastCancel?.id === mine.id ? `cancelled: ${lastCancel.why}` : explainFailure(e.message) }))
  )
  const timeout = (args.timeout ?? 60) * 1000
  const timedOut = Symbol('timeout')
  const first = await Promise.race([work, new Promise(r => setTimeout(() => r(timedOut), timeout))])
  if (first !== timedOut) return first
  // still going: report completion through the event stream instead
  work.then(r => { if (mine.gen === gen) emit('task_done', r) })
  return { ok: true, status: 'running', task: mine.id, note: 'still going: run ./mc wait (blocking, Bash timeout 600000 ms) to get its task_done. Do not end your turn to wait' }
}

// ---------------------------------------------------------------- the composite runner ("autopilot")
// src/bot.mjs holds primitives; a composite is one file in library/, `export default { doc, args, run }`. The runner loads
// them at body start and registers each in `long`, so to a driver a composite is an ordinary action: a new one cancels
// the old, `state` shows it as doing=, and one that outlasts timeout= reports through task_done like anything else.
// The hand-back rules (handBackReason in lib.mjs) belong to the RUNNER: a composite cannot opt out of being stopped
// when someone speaks to me, when I am hurt or starving, or when the same step fails twice.
const LIBRARY_DIR = path.join(ROOT, 'library')
// library/<file>.mjs is the action <file>; library/<folder>/<file>.mjs is <folder>.<file>. A new domain is a new folder.
const libraryFiles = () => {
  if (!fs.existsSync(LIBRARY_DIR)) return []
  const here = fs.readdirSync(LIBRARY_DIR, { withFileTypes: true })
  const top = here.filter(e => e.isFile() && e.name.endsWith('.mjs')).map(e => e.name)
  const nested = here.filter(e => e.isDirectory()).flatMap(dir =>
    fs.readdirSync(path.join(LIBRARY_DIR, dir.name)).filter(f => f.endsWith('.mjs')).map(f => `${dir.name}/${f}`))
  return [...top, ...nested].sort()
}
const compositeName = file => file.replace(/\.mjs$/, '').split('/').join('.')
// what ./mc help knows about the composites this body loaded
const composites = new Map()
// actions the CLI answers by itself, with no body running
const CLI_ONLY = ['wait', 'dawn', 'clock']
// what auto-eat will never touch, and so what does not count as food I carry
const BANNED_FOOD = ['rotten_flesh', 'spider_eye', 'poisonous_potato', 'pufferfish', 'chicken']
// the last thing a person said TO me: a whisper always counts, a chat only when it says my name
let lastSpoken = null

class HandBack extends Error {
  constructor (reason) { super(reason); this.reason = reason }
}

const worldDay = () => Math.floor(Number(bot.time.age ?? 0) / 24000)
const edibleCarried = () => bot.inventory.items().some(i => bot.registry.foodsByName?.[i.name] && !BANNED_FOOD.includes(i.name))
// a saved plan with its cells in world coordinates and what it would cost to build
function planOf (name) {
  const place = readPlaces().find(p => p.name === name)
  if (!place) throw new Error(`no place called ${name}: mark it, then save a map with ./mc plan name=${name} kind=farm x= y= z= map='...'`)
  if (!place.plan) throw new Error(`${name} is on the map but has no plan: save one with ./mc plan name=${name} map='...'`)
  const parsed = parsePlan(place.plan)
  return { ...place, parsed, cells: planCells(place), bill: planBill(parsed) }
}

// everything a composite may do to the world, and the only way it may do it
function makeApi (composite, a, alive) {
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
      r => { failures.delete(name); return r ?? {} },
      e => {
        const why = `${name}: ${explainFailure(e.message)}`
        noteFailure(name, why)
        throw new Error(`${composite}/${why}`)
      })
  }
  const until = async (pred, opts = {}) => {
    const deadline = Date.now() + (opts.timeout ?? 60) * 1000
    for (;;) {
      alive()
      if (await pred()) return true
      if (Date.now() >= deadline) throw new Error(`${composite}: waited ${opts.timeout ?? 60}s and ${opts.what ?? 'it never happened'}`)
      await bot.waitForTicks(Math.max(1, Math.round((opts.every ?? 1) * 20)))
    }
  }
  // between steps: night with a bed is slept through and the composite never sees it; anything else that needs a person stops the task
  const checkpoint = async (extra = {}) => {
    alive()
    if (!night()) sleptTonight = false
    if (night() && !sleptTonight && bedsNear().length) {
      sleptTonight = true
      await long.sleep({}).catch(() => {})
      await until(() => !bot.isSleeping, { timeout: 900, every: 5, what: 'the night never ended' }).catch(() => {})
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
      // what lies on the ground, how full I am, and a pause between steps: the body's own senses, not actions
      drops: range => dropsNear(range),
      freeSlots: () => bot.inventory.emptySlotCount(),
      pause: async seconds => { await bot.waitForTicks(Math.max(1, Math.round((seconds ?? 0.5) * 20))) },
      note: line => { notes.push(String(line)) },
      // what the task reports even if a hand-back rule cuts it short
      report: partial => Object.assign(report, partial)
    }
  }
}

async function runComposite (name, mod, a) {
  const bad = checkArgs(name, mod.args, a)
  if (bad) throw new Error(bad)
  const alive = cancelGuard()
  const { api, notes, report } = makeApi(name, a, alive)
  const outcome = await mod.run(api, a).then(
    r => ({ stopped: 'done', ...r }),
    e => { if (e instanceof HandBack) return { stopped: e.reason }; throw e })
  return { ...report, ...outcome, notes: notes.length ? notes.join('; ') : undefined }
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
  table[name] = args => runComposite(name, mod, args)
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
  let body = ''
  req.on('data', c => { body += c })
  req.on('end', async () => {
    const name = new URL(req.url, 'http://x').pathname.slice(1)
    let out
    try {
      const tracked = trackReads(body ? JSON.parse(body) : {})
      const args = tracked.args
      // help is answered even before the body is connected: a driver reads it first of all
      if (name === '' || name === 'help') out = { ok: true, ...quick.help(args) }
      else if (!ready && name !== 'events') out = { ok: false, error: 'bot is not connected to the server (retrying every 10s)' }
      else if (quick[name]) { if (name === 'wake') lastDriven = Date.now(); out = { ok: true, ...(await quick[name](args)) } }
      else if (long[name]) { lastDriven = Date.now(); out = await runLong(name, args, tracked.given) }
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

connect()
