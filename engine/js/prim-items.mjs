// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Containers and held items for the primitives: inspectContainer, transfer, equip, toss, craft, furnace, enchant, eat, interact, trade, unequip.

import { interactWith } from './interact.mjs'
import { emptyHand } from './unequip.mjs'
import { furnaceVisit } from './furnace.mjs'
import { enchantVisit } from './enchant.mjs'
import { craftItem } from './craft.mjs'
import { tradeWith } from './trade.mjs'
import { REACH, ATTACK_REACH, SETTLE_QUIET_MS, SETTLE_CAP_MS, CONTAINER, DESTS, sleepMs, dist, center, isNum, isPos, cell, vec, cutError, need } from './prim-base.mjs'

export function createItems (env) {
  const { act, isOwner, eye, inventory, timeScale } = env
  const withWindow = async (ctx, block, use) => {
    const win = await env.bot.openContainer(block)
    ctx.onAbort(() => env.bot.closeWindow(win))
    try {
      ctx.alive()
      return await use(win)
    } finally {
      env.bot.closeWindow(win)
    }
  }
  const containerCount = (win, name) => win.containerItems().filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)
  // mineflayer clicks in a burst on one stale stateId and the server answers each with a full window_items resync;
  // closing before those land leaves the inventory view short. Wait until the window's slot updates go quiet (or the cap).
  const settleWindow = async (ctx, win) => {
    const quiet = Math.max(1, SETTLE_QUIET_MS * timeScale)
    const deadline = Date.now() + SETTLE_CAP_MS * timeScale
    let last = Date.now()
    const touch = () => { last = Date.now() }
    win.on('updateSlot', touch)
    try {
      while (Date.now() - last < quiet && Date.now() < deadline) await sleepMs(Math.min(quiet, Math.max(1, deadline - Date.now())) / 4 + 1)
    } finally {
      win.off('updateSlot', touch)
    }
    ctx.alive()
  }
  const containerAt = p => {
    const block = env.bot.blockAt(vec(p))
    return block && CONTAINER.test(block.name) ? block : null
  }
  const slots = items => items.map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  const inspectContainer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'inspectContainer needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      return withWindow(ctx, block, async win => ({ status: 'ok', items: slots(win.containerItems()), size: win.inventoryStart, free: win.slots.slice(0, win.inventoryStart).filter(i => !i).length }))
    })
  }

  const transfer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string' && ['deposit', 'withdraw'].includes(a.direction), 'transfer needs pos, item and direction deposit|withdraw')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const type = env.bot.registry.itemsByName[a.item]?.id
      const clicked = await withWindow(ctx, block, async win => {
        const source = a.direction === 'deposit' ? inventory() : win.containerItems()
        const available = source.filter(i => i.name === a.item).reduce((sum, i) => sum + i.count, 0)
        const count = Math.min(a.count ?? available, available)
        if (count <= 0 || type === undefined) return { status: 'no-item', moved: 0 }
        const before = containerCount(win, a.item)
        ctx.alive()
        const failure = await (a.direction === 'deposit' ? win.deposit(type, null, count) : win.withdraw(type, null, count)).then(() => null, err => err)
        ctx.alive()
        if (failure && !/full|room|space/i.test(failure.message)) throw failure
        await settleWindow(ctx, win)
        return { before, failure }
      })
      if (clicked.status) return clicked
      // The first open after a login can leave mineflayer's view stale (the click burst is stamped with an old stateId and
      // the server's resyncs stop at an intermediate state); a fresh open carries the true slots.
      const after = await withWindow(ctx, block, async win => containerCount(win, a.item))
      const change = after - clicked.before
      const moved = Math.max(0, a.direction === 'deposit' ? change : -change)
      if (moved > 0) return { status: 'ok', moved }
      return clicked.failure ? { status: 'full', moved: 0 } : { status: 'ok', moved: 0 }
    })
  }

  const equip = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const { item: name, dest = 'hand' } = a
    need(typeof name === 'string' && DESTS.includes(dest), 'equip needs item and dest hand|off-hand|head|torso|legs|feet')
    return act(token, { boundS: 2 }, async ctx => {
      const item = inventory().find(i => i.name === name)
      if (!item) return { status: 'no-item' }
      await env.bot.equip(item, dest)
      return { status: 'equipped' }
    })
  }

  // With `slot`, throws exactly that slot's whole stack (env.bot.toss would take from whichever slot it finds first).
  // Throws carried items in the direction the body looks (it does not look anywhere itself); the stacks of the item
  // are summed, so a count may span several slots.
  // With `watchS` (up to 1.5) it then waits that long, or until all of it is taken, and adds `takenBy`: {collector uuid
  // count} of the tossed item picked up by anyone (a receipt of who took it).
  const watchPickups = async (ctx, a, count, throwIt) => {
    const takenBy = {}
    const taken = () => Object.values(takenBy).reduce((sum, n) => sum + n, 0)
    const onCollect = (collector, collected) => {
      const stack = collected?.getDroppedItem?.()
      if (stack?.name !== a.item || !collector?.uuid) return
      takenBy[collector.uuid] = (takenBy[collector.uuid] ?? 0) + stack.count
    }
    env.bot.on('playerCollect', onCollect)
    try {
      await throwIt()
      const deadline = Date.now() + Math.min(a.watchS, 1.5) * 1000 * timeScale
      while (taken() < count && Date.now() < deadline) {
        await sleepMs(Math.max(1, Math.min(20, deadline - Date.now())))
        ctx.alive()
      }
    } finally {
      env.bot.removeListener('playerCollect', onCollect)
    }
    return takenBy
  }
  const toss = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.item === 'string', 'toss needs item, an item name')
    need(a.slot === undefined || a.slot === null || isNum(a.slot), 'toss slot must be a number')
    need(a.watchS === undefined || (isNum(a.watchS) && a.watchS >= 0), 'toss watchS must be a number of seconds')
    const tossed = async (ctx, count, throwIt) => {
      if (!a.watchS) {
        await throwIt()
        ctx.alive()
        return { status: 'tossed', count }
      }
      const takenBy = await watchPickups(ctx, a, count, throwIt)
      return { status: 'tossed', count, takenBy }
    }
    return act(token, { boundS: a.watchS ? 2 + Math.min(a.watchS, 1.5) : 2 }, async ctx => {
      if (isNum(a.slot)) {
        const stack = env.bot.inventory.slots[a.slot]
        if (!stack || stack.name !== a.item) return { status: 'no-item', count: 0 }
        return tossed(ctx, stack.count, () => env.bot.tossStack(stack))
      }
      const total = inventory().filter(i => i.name === a.item).reduce((sum, i) => sum + i.count, 0)
      const count = Math.min(a.count ?? total, total)
      const type = env.bot.registry.itemsByName[a.item]?.id
      if (count <= 0 || type === undefined) return { status: 'no-item', count: 0 }
      return tossed(ctx, count, () => env.bot.toss(type, null, count))
    })
  }

  const craft = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.item === 'string' && a.item !== '', 'craft needs item, an item name')
    need(a.count === undefined || a.count === null || (Number.isInteger(a.count) && a.count > 0), 'craft count must be a positive integer')
    need(!a.table || (isNum(a.table.x) && isNum(a.table.y) && isNum(a.table.z)), 'craft table must have numeric x, y, z')
    return act(token, { boundS: Math.min(60, 4 + 6 * (a.count ?? 1)) }, ctx => craftItem(env.bot, ctx, a, { timeScale }))
  }

  const furnaceLoad = part => part === undefined || (typeof part?.item === 'string' && (part.count === undefined || (Number.isInteger(part.count) && part.count > 0)))
  const furnace = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'furnace needs pos {x, y, z}')
    need(['read', 'load', 'take'].includes(a.op), 'furnace op must be read, load or take')
    need(a.op !== 'load' || ((a.input || a.fuel) && furnaceLoad(a.input) && furnaceLoad(a.fuel)), 'furnace load needs input and/or fuel as {item, count?}')
    return act(token, { boundS: 5 }, ctx => furnaceVisit(env.bot, ctx, { ...a, pos: cell(a.pos) }, { reach: REACH, distanceTo: p => dist(eye(), center(p)), settle: win => settleWindow(ctx, win) }))
  }

  const enchant = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'enchant needs pos {x, y, z}')
    need(['offers', 'enchant'].includes(a.op), 'enchant op must be offers or enchant')
    need(typeof a.item === 'string' && a.item !== '', 'enchant needs item, the name of what to enchant')
    need(a.op !== 'enchant' || [0, 1, 2].includes(a.choice), 'enchant needs choice 0, 1 or 2')
    need(a.levelCost === undefined || Number.isInteger(a.levelCost), 'enchant levelCost must be an integer')
    return act(token, { boundS: 15 }, ctx => enchantVisit(env.bot, ctx, { ...a, pos: cell(a.pos) }, { reach: REACH, distanceTo: p => dist(eye(), center(p)), timeScale }))
  }
  const bestFood = () => inventory()
    .filter(i => env.bot.registry.foodsByName?.[i.name])
    .sort((a, b) => env.bot.registry.foodsByName[b.name].foodPoints - env.bot.registry.foodsByName[a.name].foodPoints)[0]

  const eat = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const startFood = env.bot.food
    return act(token, { boundS: 5, onTimeout: () => env.bot.food > startFood ? { status: 'ate', item: a.item ?? null, food: env.bot.food } : { status: 'timeout' } }, async ctx => {
      const item = a.item ? inventory().find(i => i.name === a.item) : bestFood()
      if (!item) return { status: 'no-food' }
      if (env.bot.food >= 20) return { status: 'full' }
      ctx.onAbort(() => env.bot.deactivateItem())
      await env.bot.equip(item, 'hand')
      ctx.alive()
      await env.bot.consume()
      return { status: 'ate', item: item.name, food: env.bot.food }
    })
  }

  const interact = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'interact needs an entity id')
    need(a.item == null || typeof a.item === 'string', 'interact item must be an item name')
    return act(token, { boundS: 2 }, ctx => interactWith(env.bot, ctx, a, { timeScale, reach: ATTACK_REACH }))
  }

  const trade = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.villager === 'string' && a.villager !== '', 'trade needs a villager uuid')
    need(a.op === 'offers' || a.op === 'buy', 'trade op must be offers or buy')
    need(a.op !== 'buy' || (Number.isInteger(a.offer) && a.offer >= 0), 'trade buy needs an offer index of 0 or more')
    need(a.times == null || (Number.isInteger(a.times) && a.times >= 1), 'trade times must be a positive integer')
    return act(token, { boundS: 8 }, ctx => tradeWith(env.bot, ctx, a, { timeScale, reach: ATTACK_REACH }))
  }

  const unequip = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.dest == null || a.dest === 'hand', 'unequip only empties the hand')
    return act(token, { boundS: 2 }, ctx => emptyHand(env.bot, ctx))
  }
  return { inspectContainer, transfer, equip, toss, craft, furnace, enchant, eat, interact, trade, unequip }
}
