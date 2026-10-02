// Blocks and the ground (help section block), and the scaffold primitives.
import { scaffoldSide } from '../../scaffold/side.mjs'
import { centerStand } from '../../navigation/center-stand.mjs'
import { forestHiveClaim, hiveSmokeCampfire, silkTouchTool } from '../../tree/hives.mjs'
import { tillWarning, dryCells, cropNames, withDefaultItem, digRefusal, fluidsLeft, FLUIDS, isAir, isGroundCover, placeObstacle, fillOutcome, DIG_REACH, digFromHere, digPlan, placeOutcome, placeMissed, strayFluid, missingTool, feetCell, placeAgainst, compact, canPlaceFromHere, occupiedBy } from '../../lib.mjs'
import { YAWS } from '../../vision/eyes.mjs'
import { WORK_RANGE, noStanding } from '../../navigation/walk.mjs'
import { ownCellRefusal } from '../../navigation/climb.mjs'
import { placeFaces } from '../../build/cover.mjs'
import { slabMergeRefusal } from '../../build/slab-merge.mjs'
import { cfg } from '../home.mjs'
import { readPlaces } from '../events.mjs'
import { inventoryCounts, findItem, vecOf, sweepDrops, walkToDig, cellAt, goNear, findBlocksNear } from '../helpers.mjs'
import { villagerRuntime, refusalFor } from '../../bot.mjs'
import { goals, Vec3, bot, mcData, cancelGuard, ROLLBACK_PLACE, pos, roundVec } from '../state.mjs'
import { doorsIOpened, heldOpen } from '../doors.mjs'
import { capBlock, pillarUp } from '../reflexes.mjs'
import { amBoxedIn } from '../explain.mjs'

// >0 while the `place` primitive is putting a block down on purpose: what lands then is a build, not scaffolding
export let handPlacing = 0

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

export const blockLong = {
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
  }
}
