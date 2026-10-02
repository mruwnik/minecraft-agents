// What the body does unasked to stay alive: the reflex tick (connection.mjs runs it every 10 physics ticks) eats
// out of a fight, swims up for air, digs out when buried, gets out of bed, runs, fights or holes up.
import { mealToDrop, crowdSize, wakeStep, FLUIDS, isAir, oversleeping, airReflex, chaseBroken, fleeGoal, chargeLeash, breakOffDigs, CHASE_LEASH, fleeUnwinnable, fleeOscillating, fleeRange, fleeIntoCave, holeCells, holeUpVerdict, FLEE_HOME, FLEE_GIVEUP_MS, NEVER_FIGHT, ENDERMAN_RANGE, shouldFlee, ARCHERS, rangedThreat, within, ignorableMob, buriedIn, isNight } from '../lib.mjs'
import { burrowSite, capChoice, holeUpAborted, holeUpBlock, refusalNote, shelterNote, HOLE_STEP, HOLE_DEPTH, HOLE_MELEE } from '../survival/holeup.mjs'
import { underRoof, walledIn, nightShelter, nightFleeStep, nightFleeGoal, retarget, fightNotFlee, attackerCount, plugCells, holdNote } from '../survival/night.mjs'
import { surfaceWay, openingProgress, roofAt, SURFACE_SCAN } from '../navigation/surface.mjs'
import { emit } from './events.mjs'
import { edibleCarried } from './runner.mjs'
import { findItem, findBlocksNear, bedsNear } from './helpers.mjs'
import { goals, Vec3, bot, task, pos } from './state.mjs'
import { lastWound, EAT_SAFE_RANGE, dropMeal, lives, lastMobHurt, lastHurt } from './connection.mjs'
import { swimStepTarget } from './runtimes.mjs'
import { followTarget, cancelTask } from './jobs.mjs'
import { digging, useMoves } from './actions/move.mjs'

export let reflexes = true
export const setReflexes = v => { reflexes = v }
export const isHostile = e => e.type === 'hostile' || e.kind === 'Hostile mobs'
export function nearbyHostiles (range) {
  if (!bot?.entity) return []
  return Object.values(bot.entities).filter(e => e !== bot.entity && isHostile(e) && e.position.distanceTo(bot.entity.position) <= range)
}

export let fighting = null
export const setFighting = v => { fighting = v }
// where the body stood when the current fight began, and the leash that measures from it (#105)
export let fightStart = null
export const setFightStart = v => { fightStart = v }
let chaseHeldUntil = 0
let chaseLeash = CHASE_LEASH
export let surfacing = false
// what the surfacing reflex is doing now (src/navigation/surface.mjs: up, sideways to an opening, or a pocket dug in the ceiling),
// judged again every reflex tick as the body moves
export let surfaceWayNow = null
// openings a sideways swim pressed towards for 2 s without getting nearer: walls, not ways
let surfaceTried = []
let swimTracks = {}
let pocketDigging = false
// the blocks straight over the head, as names, so the reflex can tell deep water from a roof it must swim out from under
export const columnAbove = (pos, height = 8) =>
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
// #138: one flee run at a time, with a phase and a way home. `fleeingUntil` was a timer that every tick re-armed
// while a threat stood near, which is how a body chased once kept running until something else stopped it.
export let flee = null // { mob, entity, home, phase, held, wasDigging, still: { pos, at } }
export const setFlee = v => { flee = v }
export let lastFleeReturn = null // the mob and time of the last walk back, for the oscillation guard
export const setLastFleeReturn = v => { lastFleeReturn = v }
export let fleeGaveUp = null // the mob and time of the last run that handed control back, so it does not restart itself
export const setFleeGaveUp = v => { fleeGaveUp = v }

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

export function endFlee () {
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
export let holedUp = null // { at, why }: one hole per emergency, or the reflex digs a fresh one every tick it is still hungry
export const setHoledUp = v => { holedUp = v }
export let holingUp = false
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
export function holeUp (why) {
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
// mineflayer's bot.wake() sends action id 2, which since 1.21.6 means stop_sprinting: the server never hears it
let lastLeftBed = 0
let oversleptSince = null
// at most every 2 s: the reflex tick asks again until the body is really up
export const leaveBed = () => {
  if (Date.now() - lastLeftBed < 2000) return
  lastLeftBed = Date.now()
  bot._client.write('entity_action', { entityId: bot.entity.id, actionId: 'leave_bed', jumpBoost: 0 })
}

export function reflexTick () {
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
export let lastReflex = null
