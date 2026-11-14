// Why JavaScript: Mineflayer boundary; drives the anvil window.
// One visit to an anvil: put a carried item and an enchanted book holding `enchant` in, read the level cost the server
// sets, take the result when the body can pay it, close. It never walks and decides nothing; refusals come back as data
// and the result is measured from what is carried and the body's level after the close. Mineflayer's anvil.combine is
// not used: it prices the pair with prismarine-item, which cannot read a book's stored_enchantments and refuses every
// book. See engine/README.md, primitive `anvil`.
import vec3 from 'vec3'
import { setting } from './settings.mjs'
import { enchantsOf } from './enchants.mjs'

const { Vec3 } = vec3

const ANVIL = /^(chipped_|damaged_)?anvil$/
const LEFT = 0
const RIGHT = 1
const RESULT = 2
const BOOK = 'enchanted_book'
const TOO_EXPENSIVE = 40 // the game refuses a survival anvil cost of 40 levels or more

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
const holds = (bot, item, enchant) => enchantsOf(bot.registry, item).some(e => e.name === enchant)
const pocketsOf = win => win.slots.slice(win.inventoryStart, win.inventoryEnd).map((item, i) => item && { item, slot: win.inventoryStart + i }).filter(Boolean)
const level = bot => bot.experience?.level ?? 0
const done = (bot, a) => bot.inventory.items().filter(i => i.name === a.item && holds(bot, i, a.enchant))

// The window, or the reason it did not open; a window that arrives late is closed so it cannot stay open
async function openWindow (bot, block, timeScale) {
  const opened = bot.openBlock(block)
  let timer
  const timeout = new Promise(resolve => { timer = setTimeout(() => resolve(null), setting('anvil/open-wait-ms') * timeScale) })
  try {
    const win = await Promise.race([opened, timeout])
    if (!win) {
      opened.then(late => bot.closeWindow(late), () => {})
      return { reason: 'window-did-not-open' }
    }
    if (!/anvil/.test(String(win.type))) {
      bot.closeWindow(win)
      return { reason: 'not-an-anvil-window' }
    }
    return { win }
  } catch (err) {
    return { reason: String(err.message ?? err).slice(0, 120) }
  } finally {
    clearTimeout(timer)
  }
}

// The level cost the server sends as window property 0 once both inputs lie in the anvil
function watchCost (bot, win) {
  const seen = { cost: null }
  const on = p => { if (p.windowId === win.id && p.property === 0) seen.cost = p.value }
  bot._client.on('craft_progress_bar', on)
  return { seen, stop: () => bot._client.removeListener('craft_progress_bar', on) }
}

// Waits for the result and its cost: true once both are there, false when none came (the pair does not combine)
async function resultArrives (win, seen, timeScale) {
  const deadline = Date.now() + setting('anvil/result-wait-ms') * timeScale
  while (!(win.slots[RESULT] && seen.cost > 0)) {
    if (Date.now() >= deadline) return false
    await sleepMs(setting('anvil/poll-ms') * timeScale)
  }
  return true
}

async function levelMoves (bot, from, timeScale) {
  const deadline = Date.now() + setting('anvil/level-wait-ms') * timeScale
  while (level(bot) === from && Date.now() < deadline) await sleepMs(setting('anvil/poll-ms') * timeScale)
}

async function combine (bot, ctx, win, a, timeScale, before) {
  // the server may send the cost before the move's promise settles
  const cost = watchCost(bot, win)
  try {
    const item = pocketsOf(win).find(p => p.item.name === a.item && !holds(bot, p.item, a.enchant))
    await bot.moveSlotItem(item.slot, LEFT)
    const book = pocketsOf(win).find(p => p.item.name === BOOK && holds(bot, p.item, a.enchant))
    await bot.moveSlotItem(book.slot, RIGHT)
    ctx.alive()
    if (!await resultArrives(win, cost.seen, timeScale)) return { status: 'cannot', reason: 'not-combinable' }
    const need = cost.seen.cost
    if (need >= TOO_EXPENSIVE) return { status: 'cannot', reason: 'too-expensive', need }
    if (before.level < need) return { status: 'no-levels', need, have: before.level }
    let timer
    const taken = await Promise.race([bot.putAway(RESULT).then(() => true),
      new Promise(resolve => { timer = setTimeout(() => resolve(false), setting('anvil/take-wait-ms') * timeScale) })])
      .catch(err => ({ error: String(err.message ?? err).slice(0, 120) }))
      .finally(() => clearTimeout(timer))
    if (taken === true) await levelMoves(bot, before.level, timeScale)
    return { measure: { levelCost: need, error: taken?.error ?? (taken ? null : 'take-stalled') } }
  } finally {
    cost.stop()
  }
}

// the server's inventory updates land after the close
async function settle (bot, ctx, timeScale) {
  let last = Date.now()
  const touch = () => { last = Date.now() }
  bot.inventory.on('updateSlot', touch)
  try {
    const deadline = Date.now() + setting('anvil/settle-max-ms') * timeScale
    for (;;) {
      ctx.alive()
      const now = Date.now()
      if (now - last >= setting('anvil/quiet-ms') * timeScale || now >= deadline) return
      await sleepMs(setting('anvil/poll-ms') * timeScale)
    }
  } finally {
    bot.inventory.removeListener('updateSlot', touch)
  }
}

function measured (bot, a, before, { levelCost, error }) {
  const levelsSpent = before.level - level(bot)
  const now = done(bot, a)
  if (now.length > before.done) return { status: 'combined', item: a.item, enchant: a.enchant, enchants: enchantsOf(bot.registry, now[now.length - 1]), levelCost, levelsSpent, xpLevel: level(bot) }
  return { status: 'failed', reason: error ?? 'not-confirmed', levelCost, levelsSpent }
}

export async function anvilVisit (bot, ctx, a, { distanceTo, reach, timeScale = 1 }) {
  const block = bot.blockAt(new Vec3(a.pos.x, a.pos.y, a.pos.z))
  if (!block || block.name === 'air') return { status: 'missing' }
  if (!ANVIL.test(block.name)) return { status: 'cannot', reason: 'not-an-anvil' }
  const distance = distanceTo(a.pos)
  if (distance > reach) return { status: 'unreachable', reason: 'too-far', distance: Math.round(distance * 100) / 100 }

  const items = bot.inventory.items()
  const mine = items.filter(i => i.name === a.item)
  if (mine.length === 0) return { status: 'no-item', item: a.item }
  if (mine.every(i => holds(bot, i, a.enchant))) return { status: 'cannot', reason: 'already-has', item: a.item }
  if (!items.some(i => i.name === BOOK && holds(bot, i, a.enchant))) return { status: 'no-book', enchant: a.enchant }

  const before = { level: level(bot), done: done(bot, a).length }
  const { win, reason } = await openWindow(bot, block, timeScale)
  if (!win) return { status: 'failed', reason }
  ctx.onAbort(() => bot.closeWindow(win))
  let outcome
  try {
    ctx.alive()
    outcome = await combine(bot, ctx, win, a, timeScale, before)
  } finally {
    bot.closeWindow(win)
  }
  await settle(bot, ctx, timeScale)
  return outcome.measure ? measured(bot, a, before, outcome.measure) : outcome
}
