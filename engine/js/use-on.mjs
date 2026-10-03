// useOn: right-click a block with an item (or an empty hand) and report what changed.
import vec3 from 'vec3'

const { Vec3 } = vec3

const REACH = 4.5
const POLL_MS = 50
const WAIT_MS = 1000
const CONSUME_WAIT_MS = 300 // extra wait for the carried count to follow a block change (slot updates can lag)
const FACES = { up: [0, 1, 0], down: [0, -1, 0], north: [0, 0, -1], south: [0, 0, 1], west: [-1, 0, 0], east: [1, 0, 0] }
const BED = /_bed$|^respawn_anchor$/
// The generic window guard is a last resort: a window mineflayer misparses can desync the inventory, so refuse window-opening blocks before the click.
const CONTAINER = /chest$|barrel$|shulker_box$|furnace$|smoker$|hopper$|dispenser$|dropper$|brewing_stand$|crafting_table$|anvil$|enchanting_table$|grindstone$|loom$|stonecutter$|cartography_table$|smithing_table$|lectern$|beacon$|^crafter$|command_block$|^structure_block$|^jigsaw$|^vault$/
const HAZARDS = new Set(['flint_and_steel', 'fire_charge', 'lava_bucket'])
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

  return async function useOn (token, a = {}) {
    if (!isOwner(token)) throw cutError()
    const { pos, item, face = 'up' } = a
    if (!pos || !isNum(pos.x) || !isNum(pos.y) || !isNum(pos.z)) throw badArgs('useOn needs pos {x, y, z}')
    if (item !== undefined && typeof item !== 'string') throw badArgs('useOn item must be a string')
    if (!Object.hasOwn(FACES, face)) throw badArgs(`useOn face must be one of ${Object.keys(FACES).join(', ')}`)
    const p = new Vec3(Math.floor(pos.x), Math.floor(pos.y), Math.floor(pos.z))
    const d = new Vec3(...FACES[face])
    return act(token, { boundS: 5 }, async ctx => {
      const bot = getBot()
      const block = bot.blockAt(p)
      if (!block || isAir(block.name)) return { status: 'missing' }
      const before = snapshot(block)
      const refuse = (status, extra = {}) => ({ status, ...extra, before, after: before, consumed: 0 })
      if (BED.test(block.name)) return refuse('cannot', { reason: 'bed' })
      if (CONTAINER.test(block.name)) return refuse('cannot', { reason: 'container' })
      if (HAZARDS.has(item)) return refuse('cannot', { reason: 'hazard' })
      if (item !== undefined && bot.registry?.blocksByName?.[item] && block.name !== 'composter') return refuse('cannot', { reason: 'use-place' })
      const stack = item === undefined ? null : inventory().find(i => i.name === item)
      if (item !== undefined && !stack) return refuse('no-item')
      const distance = Math.hypot(eye().x - (p.x + 0.5), eye().y - (p.y + 0.5), eye().z - (p.z + 0.5))
      if (distance > REACH) return refuse('unreachable', { reason: 'too-far', distance: Math.round(distance * 100) / 100 })
      if (stack) {
        await bot.equip(stack, 'hand')
        ctx.alive()
      } else if (!await emptyHand(ctx, bot)) return refuse('no-room')
      const countBefore = item === undefined ? 0 : carried(item)
      await lookNow(() => bot.lookAt(new Vec3(p.x + 0.5 + 0.5 * d.x, p.y + 0.5 + 0.5 * d.y, p.z + 0.5 + 0.5 * d.z), true))
      ctx.alive()
      let opened = null
      const onOpen = w => { opened ??= w }
      const now = () => { const b = bot.blockAt(p); return b ? snapshot(b) : { name: 'air', properties: {} } }
      const changed = () => !same(now(), before) || (item !== undefined && carried(item) !== countBefore)
      const windowOpened = () => { opened ??= bot.currentWindow ?? null; return opened !== null }
      // activateBlock resolves once the packet is written, so the server's open_window can arrive during the waits.
      bot.on('windowOpen', onOpen)
      try {
        await bot.activateBlock(bot.blockAt(p), d, new Vec3(0.5 + 0.5 * d.x, 0.5 + 0.5 * d.y, 0.5 + 0.5 * d.z))
        ctx.alive()
        const deadline = Date.now() + WAIT_MS * timeScale
        while (!windowOpened() && !changed() && Date.now() < deadline) {
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
        }
        // The block update can arrive before the inventory slot update; give the count a moment to follow.
        const countMoved = () => carried(item) !== countBefore
        // Tools with durability never drop in count, so only other items are worth the wait.
        const countCanDrop = !(bot.registry?.itemsByName?.[item]?.maxDurability > 0)
        const consumeDeadline = Date.now() + CONSUME_WAIT_MS * timeScale
        while (!windowOpened() && item !== undefined && countCanDrop && !same(now(), before) && !countMoved() && Date.now() < consumeDeadline) {
          await sleepMs(POLL_MS * timeScale)
          ctx.alive()
        }
        if (windowOpened()) {
          bot.closeWindow(opened)
          return { status: 'cannot', reason: 'window', before, after: now(), consumed: item === undefined ? 0 : Math.max(0, countBefore - carried(item)) }
        }
        return { status: changed() ? 'used' : 'unchanged', before, after: now(), consumed: item === undefined ? 0 : Math.max(0, countBefore - carried(item)) }
      } finally {
        bot.removeListener('windowOpen', onOpen)
      }
    })
  }
}
