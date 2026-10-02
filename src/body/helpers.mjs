// What the primitives share: matching names, counting and moving what I carry, finding blocks and drops, walking near
// things, crafting in batches, and leading animals on leads.
import vec3 from 'vec3'
import { dropGoal } from '../drop.mjs'
import { fullSide, transferOutcome, compact, settleVerdict, coordsError, unpenned, nextDrop, feetCell, leftLying, DIG_WALK_MS, digUnreached, FLUIDS, breaksUnderfoot, within, gridLeftovers, isBaby, leashPlan, leashVerdict, leadBroke, deepestCell, ledReport } from '../lib.mjs'
import { rimGoal, walkRefusal } from '../navigation/walk.mjs'
import { configureEscortMoves, equine, escortAtDestination, runSurfaceEscort } from '../navigation/escort.mjs'
import { searchSections, enough } from '../blocksearch.mjs'
import { boatLeashHolder, declareOpening, mealsEaten, digging, makeMoves, isWoodDoor, setLeading, setFollowing, lastPath, penAround, censusOf } from '../bot.mjs'
import { goals, Vec3, reportPerformance, bot, mcData, cancelGuard, pos } from './state.mjs'

// ---------------------------------------------------------------- helpers
export function matcher (names) {
  const list = (Array.isArray(names) ? names : [names]).map(n => new RegExp('^' + String(n).replace(/\*/g, '.*') + '$'))
  return name => list.some(r => r.test(name))
}
export const countsOf = items => items.reduce((out, i) => ({ ...out, [i.name]: (out[i.name] ?? 0) + i.count }), {})

// Move items between me and a chest and make sure it really happened. Through ViaBackwards a transfer is sometimes lost, lands late, or
// takes a whole stack along (asked 8 wheat, got 24; asked to deposit 30 planks, 36 went), and my own counts only tell the truth right
// after a window opens, when the server sends every slot afresh. So: move, shut, open again, compare with the plan, put right; 3 rounds at most
export async function chestTransfer (a, way, planFor) {
  const undo = way === 'withdraw' ? 'deposit' : 'withdraw'
  const run = async (chest, dir, list) => {
    for (const t of list) {
      await chest[dir](mcData.itemsByName[t.name].id, null, t.count)
      await bot.waitForTicks(6)
    }
  }
  const first = await bot.openContainer(await containerAt(a))
  // the chest is the world and the verdict: an inventory delta judged a withdraw that WORKED to be a failure three
  // rounds running, because auto-eat ate three of the eight loaves as they arrived (#146). The inventory is still
  // read, but only to say where the difference went
  const before = countsOf(first.items())
  const chestBefore = countsOf(first.containerItems())
  const mealsBefore = { ...mealsEaten }
  const corrections = []
  let settled = false
  let outcome = null
  let plan
  try {
    plan = planFor(first)
    await run(first, way, plan.take).catch(e => { throw new Error(fullSide(e.message, way)) })
  } finally { first.close() }
  for (let round = 0; round < 3; round++) {
    await bot.waitForTicks(10)
    const chest = await bot.openContainer(await containerAt(a))
    try {
      outcome = transferOutcome({
        way,
        take: plan.take,
        chestBefore,
        chestAfter: countsOf(chest.containerItems()),
        invBefore: before,
        invAfter: countsOf(chest.items()),
        eaten: diffCounts(mealsBefore, mealsEaten).gained
      })
      settled = outcome.settled
      if (settled) break
      corrections.push(...[...outcome.back, ...outcome.more].map(t => `${t.name}:${t.count}`))
      await run(chest, undo, outcome.back)
      await run(chest, way, outcome.more)
    } finally { chest.close() }
  }
  if (!settled) throw new Error(`the ${way} keeps going wrong (off by ${corrections.join(' ')}): compare chest_contents and inventory before you go on`)
  return { plan, eaten: outcome?.eaten && compact(outcome.eaten), corrected: corrections.length ? `the first try was off by ${corrections.join(' ')}: put right` : undefined }
}

// everything that drops: what inventoryCounts sees plus the armour being worn and the off hand, which live in window
// slots of their own. A died line that leaves out the helmet you were wearing is the line that loses it.
const WORN = [5, 6, 7, 8, 45]
export const carried = () => WORN.reduce((out, slot) => {
  const item = bot.inventory?.slots?.[slot]
  return item ? { ...out, [item.name]: (out[item.name] || 0) + item.count } : out
}, bot.inventory ? inventoryCounts() : {})

export function inventoryCounts () {
  return countsOf(bot.inventory.items())
}
// the pockets are still moving after a crafting window closes (the grid and the cursor come back one set_slot at a
// time): a count read the moment a batch resolves showed the whole wheat stack spent (card c13b704d). Wait for the
// slots to go quiet before counting anything; the verdict is 'settled' or 'timeout' (src/lib/settle.mjs)
export async function inventoryQuiet () {
  const startedAt = Date.now()
  let lastChangeAt = null
  const moved = () => { lastChangeAt = Date.now() }
  bot.inventory.on('updateSlot', moved)
  try {
    for (;;) {
      const verdict = settleVerdict({ lastChangeAt, startedAt, now: Date.now() })
      if (verdict !== 'wait') return verdict
      await bot.waitForTicks(1)
    }
  } finally { bot.inventory.off('updateSlot', moved) }
}
export function diffCounts (before, after) {
  const gained = {}; const lost = {}
  for (const k of new Set([...Object.keys(before), ...Object.keys(after)])) {
    const d = (after[k] || 0) - (before[k] || 0)
    if (d > 0) gained[k] = d
    if (d < 0) lost[k] = -d
  }
  return { gained, lost }
}
export function findItem (name) {
  const m = matcher(name)
  const item = bot.inventory.items().find(i => m(i.name))
  if (!item) throw new Error(`no ${name} in inventory`)
  return item
}
export const vecOf = a => {
  const error = coordsError(a)
  if (error) throw new Error(error)
  return new Vec3(Math.floor(a.x), Math.floor(a.y), Math.floor(a.z))
}
// Every dropped item lying about: what it is, where it lies, whether it is deep in water and whether it is outside the
// pen I stand in. Entity tracking is the body's own knowledge, so this is what the collect action is built on (through
// api.drops) and what dig and shear use to fetch what they just knocked loose.
export function dropsNear (range = 16) {
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
export async function sweepDrops (range = 8) {
  const tried = new Set()
  for (let i = 0; i < 8; i++) {
    const drop = nextDrop(dropsNear(range).filter(d => !d.outsidePen), tried, false)
    if (!drop) break
    tried.add(drop.id)
    const goal = dropGoal(drop, cellAt)
    // a drop on the floor of a pit is left lying (the result names it), not followed down: a dig of a pit's own wall from the rim
    // dropped its dirt on the floor and this sweep jumped in after it (src/navigation/walk.mjs rimGoal, card 3fe30fb4)
    if (rimGoal(cellAt, goal, goal.range, { from: feetCell(bot.entity.position, bot.entity.onGround) })) continue
    await bot.pathfinder.goto(new goals.GoalNear(goal.x, goal.y, goal.z, goal.range)).catch(() => {})
    await bot.waitForTicks(10)
    if (!bot.inventory.emptySlotCount()) break
  }
  const left = dropsNear(range).filter(d => !d.outsidePen)
  return leftLying(bot.inventory.emptySlotCount(), left.map(d => d.item), left.filter(d => d.deep).map(d => d.item))
}

// a dig's walk: one that has not arrived in DIG_WALK_MS is stopped and fails naming the cell, instead of wandering (148 s, card 150b3ee1)
export async function walkToDig (p) {
  let late = false
  const timer = setTimeout(() => { late = true; bot.pathfinder.setGoal(null) }, DIG_WALK_MS)
  const walked = await goNear(p, 3).then(() => null, error => error).finally(() => clearTimeout(timer))
  if (late) throw new Error(digUnreached(p))
  if (walked) throw walked
}
// a cell the way walk.mjs judges it: solid is what the pathfinder cannot walk into (a door and an open gate it can), crop is what it steps round
export const cellAt = (x, y, z) => {
  const b = bot.blockAt(new Vec3(x, y, z), false)
  if (!b) return null
  const scaffold = b.name === 'scaffolding' && bot.pathfinder?.movements?.scaffoldingSupported === true
  const walkable = scaffold || isWoodDoor(b) || (b.name.endsWith('_fence_gate') && b.getProperties().open)
  return { name: b.name, solid: b.boundingBox === 'block' && !walkable, shapes: b.shapes, properties: b.getProperties?.() ?? {}, liquid: FLUIDS.has(b.name), crop: breaksUnderfoot(b.name),
    scaffoldingSupported: bot.pathfinder?.movements?.scaffoldingSupported === true, climbableVinesSupported: bot.pathfinder?.movements?.climbableVinesSupported === true, ...(scaffold ? { climbable: true } : {}) }
}
export async function goNear (v, range = 2) {
  // already there: don't ask the pathfinder, which can fail from a perch (pillar top, ledge) even though nothing needs walking
  if (bot.entity.position.distanceTo(new Vec3(v.x + 0.5, v.y, v.z + 0.5)) <= range) return
  // a cell on the floor of a pit is worked from the pit's rim (src/navigation/walk.mjs rimGoal, card 3fe30fb4): a dig of a pit's own floor jumped
  // in when the rim was 3.16 off with range 3, and the walk that takes a scaffold pillar back stood on the pillar and dug it from under itself
  const rim = rimGoal(cellAt, v, range, { from: feetCell(bot.entity.position, bot.entity.onGround) })
  const aim = rim ?? { x: v.x, y: v.y, z: v.z, range }
  // no cell to stand in within range (a farmland cell walled in by crops): refused now, not after a 5 s search of 16k nodes (card 1ccb0ea1).
  // A dig walk makes its own room: mine.get's walk to ore under dirt was refused here as "mid-air, or inside a block" (Hollis, 09-26)
  const nowhere = walkRefusal(cellAt, aim, { dig: digging })
  if (nowhere) throw new Error(nowhere)
  await bot.pathfinder.goto(new goals.GoalNear(aim.x, aim.y, aim.z, aim.range))
}
// Every matching block within maxDistance of point (where I stand, by default), nearest first. Not bot.findBlocks: that
// walks chunk sections in an octahedron of apothem ceil((range + 8) / 16), which at range 16 never reads the section one
// over, one down and one across, so a body two blocks across a chunk line from what it looks for got one campfire in
// four (mariel-apiary, 2026-09-26 21:00Z; card 4552230d). searchSections lists every section the range touches; the
// palette short cut (skip a section whose palette holds none of the ids) is mineflayer's own.
export function findBlocksNear ({ matching, maxDistance = 16, count = 1, point = bot.entity.position, useExtraInfo = false }) {
  const ids = new Set([].concat(matching))
  const at = vec3(point).floored()
  const inPalette = section => !section.palette || section.palette.some(id => ids.has(bot.registry.blocksByStateId[id]?.id))
  const found = []
  for (const s of searchSections(at, maxDistance, { minY: bot.game.minY, height: bot.game.height })) {
    if (enough(found.map(f => f.dist), count, s.near)) break
    const section = bot.world.getColumn(s.x, s.z)?.sections[s.y - (bot.game.minY >> 4)]
    if (!section || (useExtraInfo !== true && !inPalette(section))) continue
    const corner = vec3(s.x * 16, s.y * 16, s.z * 16)
    for (let x = 0; x < 16; x++) {
      for (let y = 0; y < 16; y++) {
        for (let z = 0; z < 16; z++) {
          const p = corner.offset(x, y, z)
          const dist = p.distanceTo(at)
          if (dist > maxDistance) continue
          const block = bot.blockAt(p, Boolean(useExtraInfo))
          if (!block || !ids.has(block.type)) continue
          if (typeof useExtraInfo === 'function' && !useExtraInfo(block)) continue
          found.push({ p, dist })
        }
      }
    }
  }
  return found.sort((a, b) => a.dist - b.dist).slice(0, count).map(f => f.p)
}
export function findBlockByName (names, maxDistance = 48, count = 1, point = bot.entity.position) {
  const m = matcher(names)
  const ids = Object.values(mcData.blocksByName).filter(b => m(b.name)).map(b => b.id)
  if (!ids.length) throw new Error(`unknown block name: ${names}`)
  return findBlocksNear({ matching: ids, maxDistance, count, point })
}
export const bedsNear = () => findBlockByName('*_bed', 32, 16).sort((p, q) => p.distanceTo(bot.entity.position) - q.distanceTo(bot.entity.position))
// One batch of a recipe. bot.craft clicks the table itself and starts filling the grid as soon as anything answers, so
// after a walk in the click's own look could still be turning while the recipe ran against no window at all and the
// server rejected every batch (bug #91, and #74's "12/15 made"). Open the window here, wait for it, then craft into it.
export async function craftBatch (recipe, table) {
  if (!table) return craftInPockets(recipe)
  await goNear(table.position, 2)
  bot.pathfinder.setGoal(null)
  await bot.lookAt(table.position.offset(0.5, 0.5, 0.5), true)
  declareOpening({ x: table.position.x, y: table.position.y, z: table.position.z })
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

// The 2x2 grid is the inventory window itself, and mineflayer neither syncs nor closes it after a craft (it does both
// for a table). A put-back click the server rejected left my 26 bamboo on the cursor for one stick, and the tally called
// them lost. Read the grid back from the server; if anything is stuck there, closing the inventory makes the server
// return the grid and the cursor to the pockets
async function craftInPockets (recipe) {
  await within(15000, bot.craft(recipe, 1, null), 'crafting').catch(() => {})
  const readBack = () => within(3000, bot._syncWindow(bot.inventory), 'reading the crafting grid back').catch(() => {})
  await readBack()
  const stuck = gridLeftovers({ grid: bot.inventory.slots.slice(1, 5), cursor: bot.inventory.selectedItem })
  if (!Object.keys(stuck).length) return
  console.log(`[craft] handed back from the 2x2 grid and the cursor: ${JSON.stringify(stuck)}`)
  bot.closeWindow(bot.inventory)
  await readBack()
}

export async function containerAt (a, names = ['chest', 'barrel', 'trapped_chest']) {
  const p = a.x !== undefined ? vecOf(a) : findBlockByName(names, 32)[0]
  if (!p) throw new Error('no container found nearby')
  await goNear(p, 2)
  const block = bot.blockAt(p)
  // the block every caller here opens next: declared so the dashboard's screen reports where the window really
  // came from, not a nearby-block guess (the previous scan mixed up a chest and a barrel within reach of each other)
  declareOpening({ x: block.position.x, y: block.position.y, z: block.position.z })
  return block
}

// ---- leads (card 43a32481). attach_entity is the leash packet too, so boatLeashHolder maps every leashed entity,
// animal or boat, to its holder. A lead in the hand used on an animal ties it to me; from then on it is pulled after
// me, needs to see no food, and a gate only has to open. A lead breaks at 10 blocks, so the walk holds when one is
// pulled tight. Right-clicking an animal on my lead with an empty hand drops its lead as an item, to pick up.
export const leashHolderOf = e => boatLeashHolder.get(e.id)
export const onMyLeads = () => Object.values(bot.entities).filter(e => e.isValid && e !== bot.entity && leashHolderOf(e) === bot.entity.id)
const cellOf = e => `${Math.floor(e.position.x)},${Math.floor(e.position.y)},${Math.floor(e.position.z)}`
export const leadsCarried = () => inventoryCounts().lead ?? 0
export const leashCandidate = e => ({ id: e.id, dist: e.position.distanceTo(bot.entity.position), grown: !isBaby(e.metadata), penned: Boolean(penAround(e.position.floored())?.enclosed) })
export async function leashOne (e, { approach = true, check = () => {} } = {}) {
  check()
  if (approach) await goNear(e.position.floored(), 2)
  else if (e.position.distanceTo(bot.entity.position) > 3) throw new Error('stand within three blocks of the horse before attaching its lead')
  check()
  await bot.equip(findItem('lead'), 'hand')
  check()
  await bot.activateEntity(e)
  for (let i = 0; i < 20 && leashHolderOf(e) !== bot.entity.id; i++) { check(); await bot.waitForTicks(1) }
  check()
  if (leashHolderOf(e) !== bot.entity.id) throw new Error(`the ${e.name} at ${cellOf(e)} took no lead (leads carried: ${leadsCarried()}): stand beside it and try again`)
  Object.assign(e, { grown: !isBaby(e.metadata) })
}
export async function unleashOne (e) {
  await goNear(e.position.floored(), 2)
  // an empty hand: wheat in it would feed the cow instead
  await bot.unequip('hand')
  await bot.activateEntity(e)
  for (let i = 0; i < 20 && leashHolderOf(e) === bot.entity.id; i++) await bot.waitForTicks(1)
  return leashHolderOf(e) !== bot.entity.id
}
// escort on leads: leash the plan's animals, walk with them pulled along, hold when a lead is tight, take the leads
// off at the goal (into a pen: on its far cell) and pick them up
export async function leadWalk (a) {
  const to = a
  if (to.x === undefined || to.y === undefined || to.z === undefined) throw new Error('escort needs x= y= z= (flock.lead takes place= too)')
  const near = e => e.position.distanceTo(bot.entity.position)
  const toVec = new Vec3(to.x, to.y, to.z)
  const pen = penAround(toVec.floored())
  const floor = pen?.enclosed ? pen.floor : null
  const inPen = e => floor ? unpenned(floor, [e], x => x.position).length === 0 : escortAtDestination(e.position, toVec)
  const surfaceEscort = equine(a.mob)
  // the ones already at the goal are not fetched, and one that stands in another pen only with penned=true
  const candidates = Object.values(bot.entities)
    .filter(e => e.name === a.mob && e.isValid && !leashHolderOf(e) && near(e) <= (a.within ?? 32) && !inPen(e))
    .map(leashCandidate)
  const resuming = surfaceEscort ? onMyLeads().filter(e => e.name === a.mob && near(e) <= (a.within ?? 32)) : []
  const plan = resuming.length ? { take: resuming.slice(0, a.count ?? 2).map(e => e.id) }
    : leashPlan({ mob: a.mob, leads: leadsCarried(), count: a.count ?? 2, candidates, allowPenned: a.penned === true })
  if (plan.error) throw new Error(plan.error)
  const alive = cancelGuard()
  const initialHealth = bot.health
  const danger = () => Object.values(bot.entities).some(e => e.isValid && (e.type === 'hostile' || e.kind === 'Hostile mobs') && near(e) < 12)
  const safeAttachment = () => {
    alive()
    if (bot.health < 16 || bot.health < initialHealth || danger()) throw new Error('horse escort refused: nearby danger or damage; reach a safe surface checkpoint first')
  }
  const held = []
  const mine = () => held.filter(e => e.isValid && leashHolderOf(e) === bot.entity.id)
  let surfaceMoves
  if (surfaceEscort) {
    const selected = plan.take.map(id => bot.entities[id])
    safeAttachment()
    if (selected.some(e => leashHolderOf(e) !== bot.entity.id && near(e) > 3)) throw new Error('horse escort requires standing within three blocks before attaching a lead; approach a safe surface checkpoint explicitly')
    surfaceMoves = configureEscortMoves(makeMoves(false), {
      blockAt: (x, y, z) => bot.blockAt(new Vec3(x, y, z)), from: bot.entity.position.clone(), to: toVec,
      width: Math.max(1.4, ...selected.map(e => e.width ?? 1.4)), height: Math.max(1.6, ...selected.map(e => e.height ?? 1.6)),
      maxY: (bot.game.minY ?? -64) + (bot.game.height ?? 384), reportPerformance
    })
    bot.pathfinder.setMovements(surfaceMoves)
  }
  setLeading(true)
  try {
    for (const id of plan.take) { alive(); const e = bot.entities[id]; if (leashHolderOf(e) !== bot.entity.id) await leashOne(e, surfaceEscort ? { approach: false, check: safeAttachment } : undefined); held.push(e) }
    setFollowing(held)
    if (surfaceEscort) {
      let walkingSince = 0
      const surfaceGoal = new goals.GoalNear(to.x, to.y, to.z, a.range ?? 1)
      const result = await runSurfaceEscort({
        position: () => bot.entity.position, animals: mine, destination: toVec, arrived: inPen, check: alive,
        leaderArrived: p => surfaceGoal.isEnd(p.floored()),
        pause: () => bot.waitForTicks(5), health: () => bot.health, grounded: () => bot.entity.onGround,
        threatened: danger,
        refresh: () => surfaceMoves.refreshEscortTerrain(), corridor: () => surfaceMoves.escortCorridor(),
        start: () => { walkingSince = Date.now(); bot.pathfinder.setGoal(surfaceGoal) },
        stop: () => bot.pathfinder.setGoal(null),
        pathFailed: () => lastPath?.status === 'noPath' && lastPath.at >= walkingSince
      })
      // Leave the tether intact at a handback. Unleashing/collecting used to
      // start another unrestricted walk, even after damage or cancellation.
      return { ...result, with: mine().length, animals: held.filter(e => e.isValid).map(e => `${e.name}@${cellOf(e)}`).join(' '),
        toGo: Math.round(bot.entity.position.distanceTo(toVec)), leads: leadsCarried(), pos: pos(),
        note: 'Horse-width surface escort; leads remain attached. Release them explicitly when safely beside the animal.' }
    }
    const stroll = makeMoves(false)
    stroll.allowSprinting = false
    stroll.allowParkour = false
    bot.pathfinder.setMovements(stroll)
    const goal = new goals.GoalNear(to.x, to.y, to.z, a.range ?? 1)
    let walking = false
    let walkingSince = 0
    let heldSince = 0
    while (!goal.isEnd(bot.entity.position.floored())) {
      alive()
      const noPath = walking && lastPath?.status === 'noPath' && lastPath.at > walkingSince
      const verdict = leashVerdict({ distances: mine().map(near), held: held.length, noPath })
      if (verdict === 'broke' || verdict === 'lost') {
        bot.pathfinder.setGoal(null)
        const loose = held.filter(e => e.isValid && leashHolderOf(e) !== bot.entity.id).map(e => e.position.floored())
        await sweepDrops(8).catch(() => {})
        return { arrived: false, with: mine().length, why: leadBroke(a.mob, loose), leads: leadsCarried(), pos: pos() }
      }
      if (verdict === 'noway') { bot.pathfinder.setGoal(null); return { arrived: false, with: mine().length, toGo: Math.round(bot.entity.position.distanceTo(toVec)), why: `no route on foot from here to ${to.x},${to.y},${to.z} with them on leads. One of: the spot is not free floor to stand on; the gate is in a corner or something stands outside it (pen.check names such gates: blindGates=); a gap, drop or fence somewhere between here and there. Walk the way yourself (goto), fix what blocks it, then lead again`, pos: pos() } }
      // a lead pulled tight: stand and let the pull bring them in; one held tight for 20 s is stuck behind something, so go and get it
      if (verdict === 'hold') {
        if (walking) { bot.pathfinder.setGoal(null); walking = false; heldSince = Date.now() }
        if (heldSince && Date.now() - heldSince > 20000) { await goNear(mine().sort((x, y) => near(y) - near(x))[0].position.floored(), 2).catch(() => {}); heldSince = Date.now() }
      }
      if (verdict === 'go' && !walking) { bot.pathfinder.setGoal(goal); walking = true; walkingSince = Date.now(); heldSince = 0 }
      await bot.waitForTicks(5)
    }
    bot.pathfinder.setGoal(null)
    // into a pen: on to the cell furthest from the gate, so the pull brings them right in before the leads come off
    const deepest = floor ? new Vec3(...deepestCell(floor, held.find(e => e.isValid)?.position ?? bot.entity.position)) : null
    if (deepest) await goNear(deepest, 0).catch(() => {})
    const until = Date.now() + 8000
    while (Date.now() < until && mine().some(e => !inPen(e))) { alive(); await bot.waitForTicks(5) }
    // the leads come off here and drop as items: picked up at once, they may be what the walk borrowed
    const before = leadsCarried()
    for (const e of mine()) await unleashOne(e)
    await sweepDrops(8).catch(() => {})
    const lying = before + held.length - leadsCarried()
    const arrivals = held.filter(e => e.isValid && inPen(e))
    const census = floor ? censusOf(floor) : {}
    const animals = held.filter(e => e.isValid).map(e => `${a.mob}@${cellOf(e)}`).join(' ')
    return { arrived: true, with: arrivals.length, brought: ledReport(a.mob, arrivals), byLead: held.length, animals, leads: leadsCarried(), ...(lying > 0 ? { leadsLying: `${lying} lead${lying === 1 ? '' : 's'} dropped and not picked up: collect` } : {}), ...(plan.note ? { note: plan.note } : {}), ...census, pos: pos() }
  } finally {
    setLeading(false)
    setFollowing([])
    // cancelled or given up: a lead still on an animal drags it after me wherever I go next
    const still = surfaceEscort ? [] : mine()
    if (surfaceEscort) bot.pathfinder.setGoal(null)
    for (const e of still) await unleashOne(e).catch(() => {})
    if (still.length) await sweepDrops(8).catch(() => {})
  }
}
