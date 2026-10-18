// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Containers and held items for the primitives: inspectContainer, transfer, equip, toss, craft, furnace, enchant, eat, fish, interact, trade, unequip.

import { interactWith } from './interact.mjs'
import { emptyHand, emptyOffHand } from './unequip.mjs'
import { furnaceVisit } from './furnace.mjs'
import { enchantVisit } from './enchant.mjs'
import { craftItem } from './craft.mjs'
import { tradeWith } from './trade.mjs'
import { CONTAINER, DESTS, sleepMs, dist, center, isNum, isPos, cell, vec, cutError, need } from './prim-base.mjs'
import { setting } from './settings.mjs'
import { noteToss } from './live-entities.mjs'

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
    const quiet = Math.max(1, setting('prim/settle-quiet-ms') * timeScale)
    const deadline = Date.now() + setting('prim/settle-cap-ms') * timeScale
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
  // A container is a block at `pos` or a chest minecart entity `entity` (an id); the second is where the position is the entity's.
  const holderOf = a => {
    if (a.entity === undefined) return { block: containerAt(cell(a.pos)), at: center(cell(a.pos)) }
    const e = env.bot.entities[a.entity]
    return { block: e?.name === 'chest_minecart' && e.position ? e : null, at: e?.position }
  }
  const holderArgs = a => isPos(a.pos) || Number.isInteger(a.entity)
  const slots = items => items.map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  const inspectContainer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(holderArgs(a), 'inspectContainer needs pos {x, y, z} or entity id')
    return act(token, { boundS: 5 }, async ctx => {
      const { block, at } = holderOf(a)
      if (!block) return { status: 'missing' }
      if (dist(eye(), at) > setting('prim/reach')) return { status: 'unreachable' }
      return withWindow(ctx, block, async win => ({ status: 'ok', items: slots(win.containerItems()), size: win.inventoryStart, free: win.slots.slice(0, win.inventoryStart).filter(i => !i).length }))
    })
  }

  const transfer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(holderArgs(a) && typeof a.item === 'string' && ['deposit', 'withdraw'].includes(a.direction), 'transfer needs pos or entity, item and direction deposit|withdraw')
    return act(token, { boundS: 5 }, async ctx => {
      const { block, at } = holderOf(a)
      if (!block) return { status: 'missing' }
      if (dist(eye(), at) > setting('prim/reach')) return { status: 'unreachable' }
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
      const fresh = await withWindow(ctx, block, async win => ({ count: containerCount(win, a.item), items: slots(win.containerItems()) }))
      const change = fresh.count - clicked.before
      const moved = Math.max(0, a.direction === 'deposit' ? change : -change)
      if (moved > 0) return { status: 'ok', moved, items: fresh.items }
      return { status: clicked.failure ? 'full' : 'ok', moved: 0, items: fresh.items }
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
  // With `watchS` (up to 5: a thrown item cannot be taken for 2 s) it then waits that long, or until all of it is taken, and adds `takenBy`: {collector uuid
  // count} of the tossed item picked up by anyone (a receipt of who took it).
  const maxWatchS = 5
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
      const deadline = Date.now() + Math.min(a.watchS, maxWatchS) * 1000 * timeScale
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
    const tossed = async (ctx, count, drop) => {
      const throwIt = () => { noteToss(env.bot, a.item, count); return drop() }
      if (!a.watchS) {
        await throwIt()
        ctx.alive()
        return { status: 'tossed', count }
      }
      const takenBy = await watchPickups(ctx, a, count, throwIt)
      return { status: 'tossed', count, takenBy }
    }
    return act(token, { boundS: a.watchS ? 2 + Math.min(a.watchS, maxWatchS) : 2 }, async ctx => {
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
    return act(token, { boundS: Math.min(setting('prim/craft-max-s'), 4 + 6 * (a.count ?? 1)) }, ctx => craftItem(env.bot, ctx, a, { timeScale }))
  }

  const furnaceLoad = part => part === undefined || (typeof part?.item === 'string' && (part.count === undefined || (Number.isInteger(part.count) && part.count > 0)))
  const furnace = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'furnace needs pos {x, y, z}')
    need(['read', 'load', 'take'].includes(a.op), 'furnace op must be read, load or take')
    need(a.op !== 'load' || ((a.input || a.fuel) && furnaceLoad(a.input) && furnaceLoad(a.fuel)), 'furnace load needs input and/or fuel as {item, count?}')
    return act(token, { boundS: 5 }, ctx => furnaceVisit(env.bot, ctx, { ...a, pos: cell(a.pos) }, { reach: setting('prim/reach'), distanceTo: p => dist(eye(), center(p)), settle: win => settleWindow(ctx, win) }))
  }

  const enchant = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'enchant needs pos {x, y, z}')
    need(['offers', 'enchant'].includes(a.op), 'enchant op must be offers or enchant')
    need(typeof a.item === 'string' && a.item !== '', 'enchant needs item, the name of what to enchant')
    need(a.op !== 'enchant' || [0, 1, 2].includes(a.choice), 'enchant needs choice 0, 1 or 2')
    need(a.levelCost === undefined || Number.isInteger(a.levelCost), 'enchant levelCost must be an integer')
    return act(token, { boundS: 15 }, ctx => enchantVisit(env.bot, ctx, { ...a, pos: cell(a.pos) }, { reach: setting('prim/reach'), distanceTo: p => dist(eye(), center(p)), timeScale }))
  }
  const eat = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.item === 'string' && a.item !== '', 'eat needs item, the name of what to eat')
    const startFood = env.bot.food
    return act(token, { boundS: 5, onTimeout: () => env.bot.food > startFood ? { status: 'ate', item: a.item, food: env.bot.food } : { status: 'timeout' } }, async ctx => {
      const item = inventory().find(i => i.name === a.item)
      if (!item) return { status: 'no-food' }
      if (env.bot.food >= 20 && !a.always) return { status: 'full' }
      ctx.onAbort(() => env.bot.deactivateItem())
      await env.bot.equip(item, 'hand')
      ctx.alive()
      await env.bot.consume()
      return { status: 'ate', item: item.name, food: env.bot.food }
    })
  }

  // Cast at the water cell a.pos (looked at from where the body stands), wait for the bite up to a.timeoutS, reel in
  // (mineflayer's bot.fish casts, waits and reels); the catch is the inventory gain after the reel.
  const fish = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'fish needs pos {x, y, z}, the water cell to cast at')
    need(a.timeoutS == null || (isNum(a.timeoutS) && a.timeoutS > 0), 'fish timeoutS must be a positive number')
    const p = cell(a.pos)
    const waitS = a.timeoutS ?? setting('prim/fish-wait-s')
    const count = () => { const m = {}; for (const i of inventory()) m[i.name] = (m[i.name] ?? 0) + i.count; return m }
    return act(token, { boundS: waitS + 5 }, async ctx => {
      const rod = inventory().find(i => i.name === 'fishing_rod')
      if (!rod) return { status: 'no-rod' }
      const there = env.bot.blockAt(vec(p))
      if (!there || there.name !== 'water') return { status: 'no-water' }
      if (dist(eye(), center(p)) > setting('prim/fish-reach')) return { status: 'unreachable' }
      const before = count()
      ctx.onAbort(() => env.bot.deactivateItem())
      await env.bot.equip(rod, 'hand')
      ctx.alive()
      await env.bot.lookAt(vec({ x: p.x + 0.5, y: p.y + 0.9, z: p.z + 0.5 }), true)
      ctx.alive()
      let timer
      const bite = env.bot.fish().then(() => 'bite', () => 'failed')
      const timeout = new Promise(resolve => { timer = setTimeout(() => resolve('timeout'), waitS * 1000 * timeScale) })
      const outcome = await Promise.race([bite, timeout])
      clearTimeout(timer)
      if (outcome === 'timeout') {
        env.bot.activateItem()
        return { status: 'timeout' }
      }
      if (outcome === 'failed') return { status: 'timeout' }
      const deadline = Date.now() + setting('prim/fish-pickup-ms') * timeScale
      const gained = () => Object.entries(count()).find(([name, n]) => name !== 'fishing_rod' && n > (before[name] ?? 0))
      while (!gained() && Date.now() < deadline) {
        await sleepMs(setting('prim/poll-ms') * timeScale)
        ctx.alive()
      }
      const got = gained()
      return got ? { status: 'caught', item: got[0], count: got[1] - (before[got[0]] ?? 0) } : { status: 'nothing' }
    })
  }

  const interact = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'interact needs an entity id')
    need(a.item == null || typeof a.item === 'string', 'interact item must be an item name')
    return act(token, { boundS: 2 }, ctx => interactWith(env.bot, ctx, a, { timeScale, reach: setting('prim/attack-reach') }))
  }

  const trade = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(typeof a.villager === 'string' && a.villager !== '', 'trade needs a villager uuid')
    need(a.op === 'offers' || a.op === 'buy', 'trade op must be offers or buy')
    need(a.op !== 'buy' || (Number.isInteger(a.offer) && a.offer >= 0), 'trade buy needs an offer index of 0 or more')
    need(a.times == null || (Number.isInteger(a.times) && a.times >= 1), 'trade times must be a positive integer')
    return act(token, { boundS: 8 }, ctx => tradeWith(env.bot, ctx, a, { timeScale, reach: setting('prim/attack-reach') }))
  }

  const unequip = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(a.dest == null || a.dest === 'hand' || a.dest === 'off-hand', 'unequip only empties the hand or the off-hand')
    return act(token, { boundS: 2 }, ctx => (a.dest === 'off-hand' ? emptyOffHand : emptyHand)(env.bot, ctx))
  }
  return { inspectContainer, transfer, equip, toss, craft, furnace, enchant, eat, fish, interact, trade, unequip }
}
