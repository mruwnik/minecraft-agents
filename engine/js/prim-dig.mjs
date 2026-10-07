// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Dig, place and collect for the primitives.

import { isReplaceable, isInteractable } from './blocks.mjs'
import { stateProperties } from './use-on.mjs'
import { liveEntities, liveEntity } from './live-entities.mjs'
import vec3 from 'vec3'
import blockFor from 'prismarine-block'
import pf from 'mineflayer-pathfinder'
import { REACH, DROP_RADIUS, DROP_WAIT_S, DIG_MARGIN_S, POLL_MS, sleepMs, xyz, dist, center, isAir, isNum, isPos, cell, vec, cutError, BUCKET_WAIT_S, need, entityKind, droppedItem, gained } from './prim-base.mjs'

const { Vec3 } = vec3
const { goals } = pf

const blockClasses = new WeakMap()

export function createDig (env) {
  const { act, isOwner, here, eye, inventory, countsNow, timeScale, lookNow, walk } = env
  const dropsNear = p => liveEntities(env.bot)
    .filter(e => entityKind(e) === 'item' && dist(center(p), e.position) <= DROP_RADIUS)
    .map(e => ({ id: e.id, ...droppedItem(env.bot, e), pos: xyz(e.position) }))
  // an item already lying near the cell is not this dig's drop, unless its stack grew (a drop that merged into it)
  const lyingBefore = p => new Map(dropsNear(p).map(d => [d.id, d.count]))
  // a grown stack is reported with the growth only (new count minus the count before the dig)
  const newDrops = (p, before) => dropsNear(p)
    .filter(d => !(before.get(d.id) >= d.count))
    .map(d => ({ ...d, count: d.count - (before.get(d.id) ?? 0) }))

  // the bound of a dig: the time the server needs with the held tool (env.bot.digTime) plus a margin and the drop wait
  const digBoundS = p => {
    const block = env.bot.blockAt(vec(p))
    const ms = block && !isAir(block.name) && block.diggable ? env.bot.digTime?.(block) : 0
    return (Number.isFinite(ms) ? ms / 1000 : 0) + DIG_MARGIN_S + DROP_WAIT_S
  }

  // block.digTime counts 50 ms a tick; at another server tick rate the shim's clock scales it (bot.digTime is wrapped already)
  const atRate = ms => env.bot.physicsClock?.digMs(ms) ?? ms

  // the expected dig time in ms of the block at pos with the named tool (the held one when item is omitted or held);
  // 0 for air and a block that cannot be dug. Enchantments and effects are left out, so a faster dig is never cut short.
  const digTime = (pos, itemName) => {
    const block = env.bot.blockAt(vec(cell(pos)))
    if (!block || isAir(block.name) || !block.diggable) return 0
    if (!itemName || itemName === env.bot.heldItem?.name) return env.bot.digTime?.(block) ?? 0
    const type = env.bot.registry?.itemsByName?.[itemName]?.id
    return atRate(block.digTime?.(type, env.bot.game?.gameMode === 'creative', env.bot.entity.isInWater, !env.bot.entity.onGround, [], {}) ?? 0)
  }

  // the time in ms to break a block of this name with the named item (the bare hand when omitted); Infinity for a block
  // that cannot be dug, 0 for an unknown name. No world lookup: a block is a kind here, not a cell.
  const blockClass = registry => (blockClasses.get(registry) ?? blockClasses.set(registry, blockFor(registry)).get(registry))
  const clearTime = (blockName, itemName) => {
    const registry = env.bot.registry
    const kind = registry?.blocksByName?.[blockName]
    if (!kind) return 0
    const block = blockClass(registry).fromStateId(kind.defaultState, 0)
    if (!block.diggable) return Infinity
    const type = itemName ? registry.itemsByName?.[itemName]?.id : null
    return atRate(block.digTime(type ?? null, false, false, false, [], {}))
  }

  // the item names minecraft-data lists as able to harvest a block (its drops are lost otherwise); null when the
  // block lists none, i.e. any tool or the hand harvests it
  const harvestTools = blockName => {
    const ids = env.bot.registry?.blocksByName?.[blockName]?.harvestTools
    if (!ids) return null
    return Object.keys(ids).map(id => env.bot.registry.items?.[id]?.name).filter(Boolean)
  }

  const dig = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'dig needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: digBoundS(p) }, async ctx => {
      const block = env.bot.blockAt(vec(p))
      if (!block || isAir(block.name)) return { status: 'missing', reason: `nothing to dig at ${p.x} ${p.y} ${p.z} (${block ? 'air' : 'not loaded'})` }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      if (!block.diggable) return { status: 'cannot' }
      ctx.onAbort(() => env.bot.stopDigging())
      ctx.alive()
      const before = lyingBefore(p)
      await env.bot.dig(block, true)
      ctx.alive()
      const deadline = Date.now() + DROP_WAIT_S * 1000 * timeScale
      let drops = newDrops(p, before)
      while (drops.length === 0 && Date.now() < deadline) {
        await sleepMs(POLL_MS * timeScale)
        ctx.alive()
        drops = newDrops(p, before)
      }
      return { status: 'dug', block: block.name, drops }
    })
  }

  // A player clicks a plain block when there is one; a door, chest or table would open instead of taking the block.
  const supportCandidates = p => [[0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0]]
    .map(([dx, dy, dz]) => ({ ref: env.bot.blockAt(new Vec3(p.x + dx, p.y + dy, p.z + dz)), face: new Vec3(-dx, -dy, -dz) }))
    .filter(({ ref }) => ref && !isAir(ref.name) && ref.boundingBox === 'block')
  const supportFor = p => {
    const candidates = supportCandidates(p)
    return candidates.find(({ ref }) => !isInteractable(ref.name)) ?? candidates[0]
  }

  const isBucket = name => name === 'bucket' || name.endsWith('_bucket')
  const isLiquid = name => name === 'water' || name === 'lava'
  const isBoat = name => /_(boat|raft)$/.test(name)

  // A boat is used on the water, not placed as a block: look at the water cell's surface and activate the item; it is
  // down when the carried count dropped or a boat entity stands within 2 blocks of the cell.
  const useBoat = async (ctx, item, p) => {
    const there = env.bot.blockAt(vec(p))
    if (!there || there.name !== 'water') return { status: 'occupied', block: there?.name }
    if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
    const count = () => inventory().filter(i => i.name === item.name).reduce((sum, i) => sum + i.count, 0)
    const before = count()
    ctx.alive()
    await env.bot.equip(item, 'hand')
    ctx.alive()
    await lookNow(() => env.bot.lookAt(vec({ x: p.x + 0.5, y: p.y + 0.9, z: p.z + 0.5 }), true))
    ctx.alive()
    await env.bot.activateItem()
    const down = () => count() < before || Object.values(env.bot.entities).some(e => isBoat(e.name) && Math.hypot(e.position.x - (p.x + 0.5), e.position.z - (p.z + 0.5)) < 2 && Math.abs(e.position.y - p.y) < 2)
    const deadline = Date.now() + BUCKET_WAIT_S * 1000 * timeScale
    while (!down() && Date.now() < deadline) {
      await sleepMs(POLL_MS * timeScale)
      ctx.alive()
    }
    return down() ? { status: 'placed', block: item.name } : { status: 'failed', reason: 'unchanged' }
  }

  // A bucket is used, not placed: look at the block the liquid goes on (or at the liquid to scoop) and activate the
  // item, then check that the cell p changed within a short bound.
  const useBucket = async (ctx, item, p) => {
    const scoop = item.name === 'bucket'
    const there = env.bot.blockAt(vec(p))
    if (scoop && !(there && isLiquid(there.name))) return { status: 'missing' }
    if (!scoop && there && !isAir(there.name) && !isReplaceable(there.name)) return { status: 'occupied', block: there.name }
    const aim = scoop ? there : supportFor(p)?.ref
    if (!aim) return { status: 'no-support' }
    if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
    const count = name => inventory().filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)
    const filledName = `${there?.name}_bucket`
    const [filledBefore, heldBefore] = [count(filledName), count(item.name)]
    ctx.alive()
    await env.bot.equip(item, 'hand')
    ctx.alive()
    await lookNow(() => env.bot.lookAt(aim.position.offset(0.5, 0.5, 0.5), true))
    ctx.alive()
    await env.bot.activateItem()
    // The block update can arrive after the window while the inventory already changed, so either one counts: the
    // cell flipped, or the held bucket turned into the filled one (scoop) / the emptied one (pour).
    const cellChanged = () => { const now = env.bot.blockAt(vec(p)); return scoop ? !(now && isLiquid(now.name)) : Boolean(now && isLiquid(now.name)) }
    const itemChanged = () => scoop ? count(filledName) > filledBefore : count(item.name) < heldBefore
    const changed = () => cellChanged() || itemChanged()
    const deadline = Date.now() + BUCKET_WAIT_S * 1000 * timeScale
    while (!changed() && Date.now() < deadline) {
      await sleepMs(POLL_MS * timeScale)
      ctx.alive()
    }
    if (!changed()) return { status: 'failed', reason: 'unchanged' }
    return { status: 'placed', block: scoop ? 'bucket' : item.name.replace('_bucket', '') }
  }

  // A click chosen by the caller (jobs.lib.placement): sneak if asked, hold the look if one is given (else look at the
  // clicked point), click the face of `against` that points into the cell at `cursor`, and never look again on the way.
  const clickSupport = (click, p) => {
    const ref = env.bot.blockAt(vec(click.against))
    if (!ref || isAir(ref.name) || isLiquid(ref.name)) return null
    return { ref, face: new Vec3(p.x - click.against.x, p.y - click.against.y, p.z - click.against.z) }
  }
  const clickPlace = async (ctx, { ref, face }, click) => {
    const point = ref.position.offset(click.cursor.x, click.cursor.y, click.cursor.z)
    const sneak = on => env.bot.setControlState('sneak', on)
    if (click.sneak) { ctx.onAbort(() => sneak(false)); sneak(true) }
    try {
      await lookNow(() => isNum(click.yaw) || isNum(click.pitch) ? env.bot.look(click.yaw ?? env.bot.entity.yaw, click.pitch ?? env.bot.entity.pitch, true) : env.bot.lookAt(point, true))
      ctx.alive()
      await env.bot._placeBlockWithOptions(ref, face, { forceLook: 'ignore', delta: vec(click.cursor), swingArm: 'right' })
    } finally {
      if (click.sneak) sneak(false)
    }
  }
  // Sneaking skips the block's own use, so the click places; released again whatever the placement does.
  const placeSneaking = async (ctx, { ref, face }, sneak) => {
    if (!sneak) return env.bot.placeBlock(ref, face)
    ctx.onAbort(() => env.bot.setControlState('sneak', false))
    env.bot.setControlState('sneak', true)
    try {
      await env.bot.placeBlock(ref, face)
    } finally {
      env.bot.setControlState('sneak', false)
    }
  }
  const isClick = (c, p) => Boolean(c) && isPos(c.against) && isPos(c.cursor) &&
    Math.abs(c.against.x - p.x) + Math.abs(c.against.y - p.y) + Math.abs(c.against.z - p.z) === 1 &&
    [c.cursor.x, c.cursor.y, c.cursor.z].every(v => v >= 0 && v <= 1)

  // What a refused place (the server left the cell as it was) can be told by: the face clicked, where the body stood and
  // looked, and every entity whose box touches the cell (an entity in the cell makes the server refuse).
  const refusalFacts = (p, { ref, face }) => ({
    against: xyz(ref.position),
    face: xyz(face),
    stand: xyz(here()),
    look: { yaw: env.bot.entity.yaw, pitch: env.bot.entity.pitch },
    cell: env.bot.blockAt(vec(p))?.name,
    entities: liveEntities(env.bot)
      .filter(e => Math.abs(e.position.x - (p.x + 0.5)) < 1.2 && Math.abs(e.position.z - (p.z + 0.5)) < 1.2 && e.position.y > p.y - 2 && e.position.y < p.y + 1.5)
      .map(e => ({ id: e.id, name: e.name, kind: entityKind(e), pos: xyz(e.position) }))
  })

  const place = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string', 'place needs pos {x, y, z} and item')
    need(a.click == null || isClick(a.click, cell(a.pos)), 'place click needs against {x, y, z} beside pos and cursor {x, y, z} within 0..1')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      if (isBucket(a.item)) {
        const bucket = inventory().find(i => i.name === a.item)
        return bucket ? useBucket(ctx, bucket, p) : { status: 'no-item' }
      }
      if (isBoat(a.item)) {
        const boat = inventory().find(i => i.name === a.item)
        return boat ? useBoat(ctx, boat, p) : { status: 'no-item' }
      }
      const there = env.bot.blockAt(vec(p))
      if (there && !isAir(there.name) && !isReplaceable(there.name) && there.name !== 'water' && there.name !== 'lava') return { status: 'occupied', block: there.name }
      const item = inventory().find(i => i.name === a.item)
      if (!item) return { status: 'no-item' }
      const support = a.click ? clickSupport(a.click, p) : supportFor(p)
      if (!support) return { status: 'no-support' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      ctx.alive()
      await env.bot.equip(item, 'hand')
      ctx.alive()
      try {
        if (a.click) await clickPlace(ctx, support, a.click)
        else await placeSneaking(ctx, support, isInteractable(support.ref.name))
      } catch (err) {
        if (!/^Server refused/.test(err?.message ?? '')) throw err
        const refusal = refusalFacts(p, support)
        const near = refusal.entities.map(e => `${e.name ?? e.kind}#${e.id}`).join(', ') || 'none'
        const at = v => `${v.x},${v.y},${v.z}`
        return { status: 'failed', reason: `${err.message} (against ${at(refusal.against)} face ${at(refusal.face)}, body at ${at(refusal.stand)}, entities near the cell: ${near})`, refusal }
      }
      const now = env.bot.blockAt(vec(p))
      return { status: 'placed', block: a.item, placed: { name: now?.name, properties: now ? stateProperties(now) : {} } }
    })
  }

  // The server picks an item up when it lies within about 1.4 blocks (horizontally) of the body; a walk that ends within
  // a cell of the item can stop 1.5 away, and an item can slide while the body waits. So the body judges the reach
  // itself, re-reads the item's position, and walks into the item's own cell (MAX_APPROACHES walks in all).
  const PICKUP_REACH = 1
  const PICKUP_WAIT_S = 3 // an item in reach that is still there after this long is not going to be picked up (full inventory)
  const MAX_APPROACHES = 3
  const inPickupReach = e => Math.hypot(e.position.x - here().x, e.position.z - here().z) <= PICKUP_REACH && Math.abs(e.position.y - here().y) <= 1

  const collect = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'collect needs an entity id')
    const { id, timeoutS = 10 } = a
    const before = countsNow()
    const result = (status, reason) => ({ status, ...(reason && { reason }), gained: gained(before, countsNow()) })
    return act(token, { boundS: Math.min(timeoutS, 20), onTimeout: () => result('timeout') }, async ctx => {
      const target = liveEntity(env.bot, id)
      if (!target || entityKind(target) !== 'item') return { status: 'gone' }
      let approaches = 0
      let inReachSince = null
      while (liveEntity(env.bot, id)) {
        const item = liveEntity(env.bot, id)
        if (inPickupReach(item)) {
          inReachSince ??= Date.now()
          if (Date.now() - inReachSince > PICKUP_WAIT_S * 1000 * timeScale) return result('unreachable', 'not-picked-up')
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
          continue
        }
        inReachSince = null
        if (approaches >= MAX_APPROACHES) return result('unreachable', 'out-of-reach')
        const p = item.position
        const { reached } = await walk(ctx, new goals.GoalNear(p.x, p.y, p.z, approaches === 0 ? 1 : 0), { stall: false })
        approaches++
        if (!reached && liveEntity(env.bot, id)) return result('unreachable')
      }
      const got = gained(before, countsNow())
      return got.length ? { status: 'collected', gained: got } : { status: 'gone', gained: [] }
    })
  }
  return { dig, place, collect, digTime, clearTime, harvestTools }
}
