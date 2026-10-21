// Why JavaScript: Mineflayer boundary; block use/activate calls and the safety guards before the click.
// useOn: right-click a block with an item (the held stack when none is named, or 'none' for an empty hand) and report what changed; with `air` use the item toward the spot pos instead (a throw).
import vec3 from 'vec3'
import { setting } from './settings.mjs'
import { spawnSetMessage } from './prim-base.mjs'

const { Vec3 } = vec3

const FACES = { up: [0, 1, 0], down: [0, -1, 0], north: [0, 0, -1], south: [0, 0, 1], west: [-1, 0, 0], east: [1, 0, 0] }
const BED = /_bed$/
const ANCHOR = /^respawn_anchor$/
// A bed explodes outside the overworld; an unknown dimension is taken as the overworld.
const bedSafe = bot => /overworld$/.test(bot.game?.dimension ?? 'overworld')
// The generic window guard is a last resort: a window mineflayer misparses can desync the inventory, so refuse window-opening blocks before the click.
const CONTAINER = /chest$|barrel$|shulker_box$|furnace$|smoker$|hopper$|dispenser$|dropper$|brewing_stand$|crafting_table$|anvil$|enchanting_table$|grindstone$|loom$|stonecutter$|cartography_table$|smithing_table$|lectern$|beacon$|^crafter$|command_block$|^structure_block$|^jigsaw$|^vault$/
const HAZARDS = new Set(['flint_and_steel', 'fire_charge', 'lava_bucket'])
// A click the block does not take would place or pour the held item against it, so these are put away first.
// Animal foods a lure holds: several share a name with their crop block (wheat, carrots), so the block-name test must not stow them. Mirror of jobs.lib.animals/breeding-food.
const LURE_FOODS = new Set(['wheat', 'wheat_seeds', 'melon_seeds', 'pumpkin_seeds', 'beetroot_seeds', 'torchflower_seeds', 'carrot', 'potato', 'beetroot', 'golden_carrot'])
const PLACING = /_bucket$|_spawn_egg$|_boat$|_raft$|minecart/
// Items a player uses in the air by looking at a spot and right-clicking; anything else (food, bows, blocks) would start another action.
const THROWN = /^(egg|snowball|ender_pearl|experience_bottle|wind_charge|splash_potion|lingering_potion|trident)$/
const isAir = name => name === 'air' || name.endsWith('_air')
const isNum = n => typeof n === 'number' && Number.isFinite(n)
const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
// Mineflayer hands integer block states back as strings ('8'); report them as numbers.
export const stateProperties = block => Object.fromEntries(Object.entries(block.getProperties?.() ?? {}).map(([k, v]) => [k, typeof v === 'string' && /^-?\d+$/.test(v) ? Number(v) : v]))
const snapshot = block => ({ name: block.name, properties: stateProperties(block) })
const same = (a, b) => a.name === b.name && JSON.stringify(a.properties) === JSON.stringify(b.properties)

export function createUseOn ({ act, getBot, inventory, eye, lookNow, timeScale, isOwner, cutError, badArgs }) {
  const carried = name => inventory().filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)

  // Empties the hand without ever tossing: a free hotbar slot is selected, else the held stack is moved into a free
  // inventory slot. True when the hand is empty afterwards, false when there is no room.
  const emptyHand = async (ctx, bot) => {
    if (!bot.heldItem) return true
    const slot = [...Array(9).keys()].find(i => !bot.inventory.slots[36 + i])
    if (slot !== undefined) {
      bot.setQuickBarSlot(slot)
      return true
    }
    if (bot.inventory.firstEmptyInventorySlot() === null) return false
    await bot.unequip('hand')
    ctx.alive()
    return true
  }

  const placing = (bot, name) => name && !LURE_FOODS.has(name) && (HAZARDS.has(name) || PLACING.test(name) || bot.registry?.blocksByName?.[name])

  // Air use: look at the spot pos (any point) and activate the held item; used when the carried count dropped.
  const throwAt = (token, { pos, item }) => {
    if (!pos || !isNum(pos.x) || !isNum(pos.y) || !isNum(pos.z)) throw badArgs('useOn needs pos {x, y, z}')
    if (typeof item !== 'string') throw badArgs('useOn in the air needs an item name')
    return act(token, { boundS: 5 }, async ctx => {
      const bot = getBot()
      if (!THROWN.test(item)) return { status: 'cannot', reason: 'not-throwable', consumed: 0 }
      const stack = inventory().find(i => i.name === item)
      if (!stack) return { status: 'no-item', consumed: 0 }
      const before = carried(item)
      await bot.equip(stack, 'hand')
      ctx.alive()
      await lookNow(() => bot.lookAt(new Vec3(pos.x, pos.y, pos.z), true))
      ctx.alive()
      await bot.activateItem()
      const deadline = Date.now() + setting('use-on/wait-ms') * timeScale
      while (carried(item) >= before && Date.now() < deadline) {
        await sleepMs(setting('use-on/poll-ms') * timeScale)
        ctx.alive()
      }
      const consumed = Math.max(0, before - carried(item))
      return { status: consumed > 0 ? 'used' : 'unchanged', consumed }
    })
  }

  return async function useOn (token, a = {}) {
    if (!isOwner(token)) throw cutError()
    const { pos, face = 'up' } = a
    if (a.air) return throwAt(token, a)
    // item undefined: keep whatever is in hand; 'none': the empty hand.
    const keepHand = a.item === undefined
    const item = a.item === 'none' ? undefined : a.item
    if (!pos || !isNum(pos.x) || !isNum(pos.y) || !isNum(pos.z)) throw badArgs('useOn needs pos {x, y, z}')
    if (a.item !== undefined && typeof a.item !== 'string') throw badArgs('useOn item must be a string')
    if (!Object.hasOwn(FACES, face)) throw badArgs(`useOn face must be one of ${Object.keys(FACES).join(', ')}`)
    const p = new Vec3(Math.floor(pos.x), Math.floor(pos.y), Math.floor(pos.z))
    const d = new Vec3(...FACES[face])
    return act(token, { boundS: 5 }, async ctx => {
      const bot = getBot()
      const block = bot.blockAt(p)
      if (!block || isAir(block.name)) return { status: 'missing' }
      const before = snapshot(block)
      const refuse = (status, extra = {}) => ({ status, ...extra, before, after: before, consumed: 0 })
      if (ANCHOR.test(block.name) || (BED.test(block.name) && !bedSafe(bot))) return refuse('cannot', { reason: 'bed' })
      if (CONTAINER.test(block.name)) return refuse('cannot', { reason: 'container' })
      if (HAZARDS.has(item)) return refuse('cannot', { reason: 'hazard' })
      if (item !== undefined && bot.registry?.blocksByName?.[item] && block.name !== 'composter') return refuse('cannot', { reason: 'use-place' })
      const stack = item === undefined ? null : inventory().find(i => i.name === item)
      if (item !== undefined && !stack) return refuse('no-item')
      const distance = Math.hypot(eye().x - (p.x + 0.5), eye().y - (p.y + 0.5), eye().z - (p.z + 0.5))
      if (distance > setting('use-on/reach')) return refuse('unreachable', { reason: 'too-far', distance: Math.round(distance * 100) / 100 })
      // noLine comes from engine.senses: the eye's ray reaches no part of the target before another block's shape.
      if (a.noLine) return refuse('unreachable', { reason: 'no-line' })
      if (stack) {
        await bot.equip(stack, 'hand')
        ctx.alive()
      } else if ((!keepHand || placing(bot, bot.heldItem?.name)) && !await emptyHand(ctx, bot)) return refuse('no-room')
      const countBefore = item === undefined ? 0 : carried(item)
      await lookNow(() => bot.lookAt(new Vec3(p.x + 0.5 + 0.5 * d.x, p.y + 0.5 + 0.5 * d.y, p.z + 0.5 + 0.5 * d.z), true))
      ctx.alive()
      let opened = null
      const onOpen = w => { opened ??= w }
      const now = () => { const b = bot.blockAt(p); return b ? snapshot(b) : { name: 'air', properties: {} } }
      // A bed used by day answers with the "respawn point set" chat line instead of changing.
      let spawnSet = false
      const onMessage = msg => { spawnSet ||= spawnSetMessage(msg) }
      const changed = () => spawnSet || !same(now(), before) || (item !== undefined && carried(item) !== countBefore)
      const windowOpened = () => { opened ??= bot.currentWindow ?? null; return opened !== null }
      // activateBlock resolves once the packet is written, so the server's open_window can arrive during the waits.
      bot.on('windowOpen', onOpen)
      bot.on('message', onMessage)
      try {
        await bot.activateBlock(bot.blockAt(p), d, new Vec3(0.5 + 0.5 * d.x, 0.5 + 0.5 * d.y, 0.5 + 0.5 * d.z))
        ctx.alive()
        const deadline = Date.now() + setting('use-on/wait-ms') * timeScale
        while (!windowOpened() && !changed() && Date.now() < deadline) {
          await sleepMs(setting('use-on/poll-ms') * timeScale)
          ctx.alive()
        }
        // The block update can arrive before the inventory slot update; give the count a moment to follow.
        const countMoved = () => carried(item) !== countBefore
        // Tools with durability never drop in count, so only other items are worth the wait.
        const countCanDrop = !(bot.registry?.itemsByName?.[item]?.maxDurability > 0)
        const consumeDeadline = Date.now() + setting('use-on/consume-wait-ms') * timeScale
        while (!windowOpened() && item !== undefined && countCanDrop && !same(now(), before) && !countMoved() && Date.now() < consumeDeadline) {
          await sleepMs(setting('use-on/poll-ms') * timeScale)
          ctx.alive()
        }
        if (windowOpened()) {
          bot.closeWindow(opened)
          return { status: 'cannot', reason: 'window', before, after: now(), consumed: item === undefined ? 0 : Math.max(0, countBefore - carried(item)) }
        }
        return { status: changed() ? 'used' : 'unchanged', ...(spawnSet ? { spawnSet } : {}), before, after: now(), consumed: item === undefined ? 0 : Math.max(0, countBefore - carried(item)) }
      } finally {
        bot.removeListener('windowOpen', onOpen)
        bot.removeListener('message', onMessage)
      }
    })
  }
}
