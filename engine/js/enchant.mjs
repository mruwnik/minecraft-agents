// Why JavaScript: Mineflayer boundary; drives the enchanting-table window.
// One visit to an enchanting table: open, read the three offers or enchant with one of them, close. It never walks and
// decides nothing (which offer is worth it); refusals come back as data and the result is measured from what is carried
// and the body's level after the window is closed. See engine/README.md, primitive `enchant`.
import vec3 from 'vec3'

const { Vec3 } = vec3

const OPEN_WAIT_MS = 3000
const OFFERS_WAIT_MS = 3000
const ENCHANT_WAIT_MS = 4000
const LEVEL_WAIT_MS = 2000
const QUIET_MS = 150
const SETTLE_MAX_MS = 2000
const POLL_MS = 25
const ITEM_SLOT = 0
const LAPIS_SLOT = 1
const LAPIS = 'lapis_lazuli'
const RESULT = { book: 'enchanted_book' } // a book comes out as another item

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))

// The window, or null when it did not open in time; a window that arrives later is closed so it cannot stay open.
// A throw from the open (a window of another kind) is the reason the visit failed.
async function openWindow (bot, block, timeScale) {
  const opened = bot.openEnchantmentTable(block)
  let timer
  const timeout = new Promise(resolve => { timer = setTimeout(() => resolve(null), OPEN_WAIT_MS * timeScale) })
  try {
    const win = await Promise.race([opened, timeout])
    if (win) return { win }
    opened.then(late => late.close(), () => {})
    return { reason: 'window-did-not-open' }
  } catch (err) {
    return { reason: String(err.message ?? err).slice(0, 120) }
  } finally {
    clearTimeout(timer)
  }
}

// What is carried, as the open window sees it: bot.inventory is not updated while a window is open
const pocketsOf = win => win.slots.slice(win.inventoryStart, win.inventoryEnd).map((item, i) => item && { item, slot: win.inventoryStart + i }).filter(Boolean)
const countIn = (stacks, name) => stacks.filter(s => s.name === name).reduce((sum, s) => sum + s.count, 0)
const plain = item => (item.enchants ?? []).length === 0
const enchantsOf = item => (item.enchants ?? []).map(e => ({ name: e.name, level: e.lvl ?? e.level }))
const signature = item => JSON.stringify([item.name, enchantsOf(item).map(e => `${e.name}:${e.level}`).sort()])

const hintOf = (bot, e) => (e.expected.enchant >= 0 ? { enchant: bot.registry?.enchantments?.[e.expected.enchant]?.name ?? `enchantment#${e.expected.enchant}`, level: e.expected.level } : null)
const offersOf = (bot, win) => win.enchantments.map((e, index) => ({ index, levelCost: e.level, lapisCost: index + 1, hint: hintOf(bot, e) }))

// the offers arrive as window properties a moment after the item lies in the table
async function offersArrive (win, timeScale) {
  const deadline = Date.now() + OFFERS_WAIT_MS * timeScale
  while (!win.enchantments.every(e => e.level >= 0)) {
    if (Date.now() >= deadline) return false
    await sleepMs(POLL_MS * timeScale)
  }
  return true
}

// Resolves wait() once the body's level differs from `from` (or after the bound)
function watchLevel (bot, from) {
  let on
  const moved = new Promise(resolve => {
    on = () => { if ((bot.experience?.level ?? 0) !== from) resolve() }
    bot.on('experience', on)
    on()
  })
  return {
    async wait (ms) {
      let timer
      await Promise.race([moved, new Promise(resolve => { timer = setTimeout(resolve, ms) })])
      clearTimeout(timer)
    },
    stop () { bot.removeListener('experience', on) }
  }
}

// Listens for the server's updates that land after the close; wait() returns once they have been quiet for a moment
function watchQuiet (bot, timeScale) {
  let last = Date.now()
  const touch = () => { last = Date.now() }
  bot.inventory.on('updateSlot', touch)
  bot.on('experience', touch)
  return {
    async wait (ctx) {
      last = Date.now()
      const deadline = last + SETTLE_MAX_MS * timeScale
      for (;;) {
        ctx.alive()
        const now = Date.now()
        if (now - last >= QUIET_MS * timeScale || now >= deadline) return
        await sleepMs(POLL_MS * timeScale)
      }
    },
    stop () {
      bot.inventory.removeListener('updateSlot', touch)
      bot.removeListener('experience', touch)
    }
  }
}

const snapshot = (bot, name) => {
  const items = bot.inventory.items()
  return { lapis: countIn(items, LAPIS), carried: countIn(items, name), enchanted: items.filter(i => i.name === (RESULT[name] ?? name) && !plain(i)).map(signature), level: bot.experience?.level ?? 0 }
}

// the enchanted copies that were not there before the visit
const gained = (bot, name, before) => {
  const made = RESULT[name] ?? name
  const left = [...before.enchanted]
  return bot.inventory.items().filter(i => i.name === made && !plain(i)).filter(i => {
    const at = left.indexOf(signature(i))
    if (at < 0) return true
    left.splice(at, 1)
    return false
  })
}

// Put the item and, when wanted, the lapis in the table. Slot numbers are the window's own, not the inventory's.
async function stock (bot, win, a, wantLapis) {
  const pockets = pocketsOf(win)
  const item = pockets.find(p => p.item.name === a.item && plain(p.item))
  await bot.moveSlotItem(item.slot, ITEM_SLOT)
  if (!wantLapis) return
  const lapis = pocketsOf(win).find(p => p.item.name === LAPIS)
  await bot.moveSlotItem(lapis.slot, LAPIS_SLOT)
}

async function read (bot, ctx, win, a, timeScale, xpLevel) {
  await stock(bot, win, a, false)
  ctx.alive()
  if (!await offersArrive(win, timeScale)) return { status: 'failed', reason: 'offers-did-not-arrive' }
  return { status: 'ok', item: a.item, xpLevel, lapis: countIn(pocketsOf(win).map(p => p.item), LAPIS), offers: offersOf(bot, win) }
}

// refusals that need only the offers: [] when the choice can be bought
const refusalFor = (offers, a, xpLevel) => {
  const levels = offers.map(o => o.levelCost)
  if (levels.every(l => l <= 0)) return { status: 'cannot', reason: 'not-enchantable' }
  const chosen = offers[a.choice]
  if (chosen.levelCost <= 0) return { status: 'cannot', reason: 'no-such-offer', offers: levels }
  if (a.levelCost !== undefined && a.levelCost !== chosen.levelCost) return { status: 'cannot', reason: 'offer-changed', offers: levels }
  const need = Math.max(chosen.levelCost, chosen.lapisCost)
  if (xpLevel < need) return { status: 'no-levels', need, have: xpLevel }
  return null
}

async function enchant (bot, ctx, win, a, timeScale, xpLevel) {
  const lapisHave = countIn(pocketsOf(win).map(p => p.item), LAPIS)
  if (lapisHave < a.choice + 1) return { status: 'no-lapis', have: lapisHave, need: a.choice + 1 }
  await stock(bot, win, a, true)
  ctx.alive()
  if (!await offersArrive(win, timeScale)) return { status: 'failed', reason: 'offers-did-not-arrive' }
  const refusal = refusalFor(offersOf(bot, win), a, xpLevel)
  if (refusal) return refusal
  // win.enchant can wait for a slot update that never comes while the server did the work: stop waiting after a bound,
  // close and measure whatever happened
  let timer
  let stalled = false
  let error = null
  const levelMoved = watchLevel(bot, xpLevel)
  try {
    const call = Promise.resolve(win.enchant(a.choice))
    call.catch(() => {})
    const bound = new Promise(resolve => { timer = setTimeout(() => resolve('stalled'), ENCHANT_WAIT_MS * timeScale) })
    stalled = (await Promise.race([call, bound])) === 'stalled'
  } catch (err) {
    error = err
  } finally {
    clearTimeout(timer)
  }
  ctx.alive()
  // the server's new level can arrive after the window call has returned
  if (!stalled && !error) await levelMoved.wait(LEVEL_WAIT_MS * timeScale)
  levelMoved.stop()
  return { measure: { stalled, error } }
}

function measured (bot, a, before, { stalled, error }) {
  const lapisSpent = before.lapis - countIn(bot.inventory.items(), LAPIS)
  const levelsSpent = before.level - (bot.experience?.level ?? 0)
  const [done] = gained(bot, a.item, before)
  if (done) return { status: 'enchanted', item: a.item, choice: a.choice, enchants: enchantsOf(done), lapisSpent, levelsSpent, xpLevel: bot.experience?.level ?? 0, ...(stalled && { stalled: true }) }
  const gone = countIn(bot.inventory.items(), a.item) < before.carried
  if (gone && bot.inventory.emptySlotCount() === 0) return { status: 'full' }
  const reason = error ? String(error.message ?? error).slice(0, 120) : stalled ? 'enchant-stalled' : gone ? 'item-not-returned' : 'not-confirmed'
  return { status: 'failed', reason, lapisSpent, levelsSpent }
}

export async function enchantVisit (bot, ctx, a, { distanceTo, reach, timeScale = 1 }) {
  const block = bot.blockAt(new Vec3(a.pos.x, a.pos.y, a.pos.z))
  if (!block || block.name === 'air') return { status: 'missing' }
  if (block.name !== 'enchanting_table') return { status: 'cannot', reason: 'not-a-table' }
  const distance = distanceTo(a.pos)
  if (distance > reach) return { status: 'unreachable', reason: 'too-far', distance: Math.round(distance * 100) / 100 }

  const carried = bot.inventory.items().filter(i => i.name === a.item)
  if (carried.length === 0) return { status: 'no-item', item: a.item }
  if (!carried.some(plain)) return { status: 'cannot', reason: 'already-enchanted', item: a.item }

  const before = snapshot(bot, a.item)
  const { win, reason } = await openWindow(bot, block, timeScale)
  if (!win) return { status: 'failed', reason }
  ctx.onAbort(() => win.close())
  const quiet = watchQuiet(bot, timeScale)
  let outcome
  try {
    try {
      ctx.alive()
      outcome = await (a.op === 'offers' ? read : enchant)(bot, ctx, win, a, timeScale, before.level)
    } finally {
      win.close()
    }
    await quiet.wait(ctx)
  } finally {
    quiet.stop()
  }
  return outcome.measure ? measured(bot, a, before, outcome.measure) : outcome
}
