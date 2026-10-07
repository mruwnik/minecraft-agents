// Why JavaScript: Mineflayer boundary; decodes and drives the villager trade window.
// Trading with a villager: open the window, read the offers or buy, close. One short visit per call; it never walks
// and never judges whether a trade is worth it.
import vec3 from 'vec3'
import { mobFields } from './interact.mjs'
import { professionOf, villagerData } from './villager.mjs'

export { professionOf }

const OPEN_WAIT_MS = 3000
const QUIET_MS = 150
const SETTLE_MAX_MS = 2000
const POLL_MS = 25
const TRADE_WAIT_MS = 4000
const DEFAULT_STACK = 64

const middle = e => vec3(e.position.x, e.position.y + (e.height ?? 1) / 2, e.position.z)

// the reason a villager has nothing to trade, or null
const noOffersWhy = (profession, baby) => baby ? 'baby' : ['unemployed', 'nitwit'].includes(profession) ? profession : null

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))

const closeWindow = bot => { if (bot.currentWindow) bot.closeWindow(bot.currentWindow) }

// The window, or null when it did not open in time; a window that arrives later is closed so it cannot stay open
async function openWindow (bot, target, timeScale) {
  const opened = bot.openVillager(target)
  let timer
  const timeout = new Promise(resolve => { timer = setTimeout(() => resolve(null), OPEN_WAIT_MS * timeScale) })
  try {
    const win = await Promise.race([opened, timeout])
    if (win) return win
    opened.then(late => bot.closeWindow(late), () => {})
    return null
  } finally {
    clearTimeout(timer)
  }
}

// cost[0] is the adjusted price (realPrice), not the base count the stack carries
const offerRow = (t, index) => {
  const left = Math.max(0, t.maximumNbTradeUses - t.nbTradeUses)
  return {
    index,
    cost: [
      { item: t.inputItem1.name, count: t.realPrice },
      ...(t.hasItem2 ? [{ item: t.inputItem2.name, count: t.inputItem2.count }] : [])
    ],
    gives: { item: t.outputItem.name, count: t.outputItem.count },
    uses: t.nbTradeUses,
    maxUses: t.maximumNbTradeUses,
    left,
    disabled: t.tradeDisabled === true || left === 0
  }
}

const tally = bot => bot.inventory.items().reduce((have, i) => ({ ...have, [i.name]: (have[i.name] ?? 0) + i.count }), {})

// net change per name: [rose, fell]
const changes = (before, after) => {
  const net = Object.fromEntries([...new Set([...Object.keys(before), ...Object.keys(after)])].map(k => [k, (after[k] ?? 0) - (before[k] ?? 0)]))
  const only = keep => Object.fromEntries(Object.entries(net).filter(([, d]) => keep(d)).map(([k, d]) => [k, Math.abs(d)]))
  return [only(d => d > 0), only(d => d < 0)]
}

// How many of the result fit: free slots plus the unfilled part of the stacks already carried
const fits = (bot, name, count) => {
  const stackSize = bot.registry?.itemsByName?.[name]?.stackSize ?? DEFAULT_STACK
  const partial = bot.inventory.items().filter(i => i.name === name).reduce((room, i) => room + Math.max(0, stackSize - i.count), 0)
  return Math.floor((bot.inventory.emptySlotCount() * stackSize + partial) / count)
}

// Closes the window and waits until the inventory has been quiet: the server's slot updates land after the close
async function closeAndSettle (bot, ctx, timeScale) {
  let last = Date.now()
  const touch = () => { last = Date.now() }
  const deadline = last + SETTLE_MAX_MS * timeScale
  bot.inventory.on('updateSlot', touch)
  try {
    closeWindow(bot)
    for (;;) {
      ctx.alive()
      const now = Date.now()
      if (now - last >= QUIET_MS * timeScale || now >= deadline) return
      await sleepMs(POLL_MS * timeScale)
    }
  } finally {
    bot.inventory.removeListener('updateSlot', touch)
  }
}

async function buy (bot, ctx, win, a, timeScale) {
  const row = win.trades.map(offerRow)[a.offer]
  if (!row) return { status: 'cannot', reason: 'no-such-offer', offers: win.trades.length }
  if (row.disabled) return { status: 'cannot', reason: 'sold-out' }
  const before = tally(bot)
  const short = Object.fromEntries(row.cost.map(c => [c.item, Math.max(0, c.count - (before[c.item] ?? 0))]).filter(([, n]) => n > 0))
  if (Object.keys(short).length > 0) return { status: 'no-item', short }
  const room = fits(bot, row.gives.item, row.gives.count)
  if (room === 0) return { status: 'full' }
  const times = a.times ?? 1
  const payable = Math.min(...row.cost.map(c => Math.floor(before[c.item] / c.count)))
  const limits = [['sold-out', row.left], ['payment', payable], ['room', room]]
  const n = Math.min(times, ...limits.map(([, v]) => v))

  // bot.trade can wait for a slot update that never comes (an adjusted price) while the server has done the trade:
  // stop waiting after a bound, close and measure whatever happened
  let error = null
  let stalled = false
  let windowClosed = false
  let timer
  // one click per call so a window that closed underneath (villager gone, server close) ends the run
  const clickAll = async () => {
    for (let i = 0; i < n; i++) {
      if (bot.currentWindow !== win) { windowClosed = true; return }
      await bot.trade(win, a.offer, 1)
    }
  }
  try {
    const call = clickAll()
    call.catch(() => {})
    const bound = new Promise(resolve => { timer = setTimeout(() => resolve('stalled'), TRADE_WAIT_MS * timeScale) })
    stalled = (await Promise.race([call, bound])) === 'stalled'
  } catch (err) {
    error = err
  } finally {
    clearTimeout(timer)
  }
  ctx.alive()
  // the changes land when the window closes, so measure only after that and a quiet inventory
  await closeAndSettle(bot, ctx, timeScale)
  const [gained, paid] = changes(before, tally(bot))
  const done = Math.floor((gained[row.gives.item] ?? 0) / row.gives.count)
  if (done === 0) return { status: 'failed', reason: error ? String(error.message ?? error).slice(0, 120) : stalled ? 'trade-stalled' : 'not-confirmed', ...(!error && { paid, gained }) }
  const stopped = done >= times ? null : stalled ? 'stalled' : windowClosed ? 'window-closed' : (limits.find(([, v]) => v === n && n < times)?.[0] ?? null)
  return { status: 'bought', times: done, requested: times, gained, paid, stopped, ...(stalled && { stalled: true }) }
}

export async function tradeWith (bot, ctx, a, { timeScale = 1, reach }) {
  const target = Object.values(bot.entities).find(e => e.uuid === a.villager)
  if (!target) return { status: 'gone' }
  if (target.name !== 'villager') return { status: 'cannot', reason: 'not-villager', name: target.name }
  const eye = bot.entity.position.offset(0, bot.entity.height ?? 1.62, 0)
  const distance = eye.distanceTo(middle(target))
  if (distance > reach) return { status: 'out-of-reach', reason: 'too-far', distance }

  const { villagerProfession, level } = villagerData(bot, target)
  const profession = professionOf(villagerProfession)
  const why = noOffersWhy(profession, mobFields(bot, target).baby === true)
  if (why) return { status: 'cannot', reason: 'no-offers', why, profession, level }

  const win = await openWindow(bot, target, timeScale)
  if (!win) return { status: 'failed', reason: 'window-did-not-open' }
  ctx.onAbort(() => closeWindow(bot))
  try {
    ctx.alive()
    if (win.trades.length === 0) return { status: 'cannot', reason: 'no-offers', why: 'none', profession, level }
    if (a.op === 'offers') return { status: 'ok', uuid: a.villager, profession, level, offers: win.trades.map(offerRow) }
    return await buy(bot, ctx, win, a, timeScale)
  } finally {
    closeWindow(bot)
  }
}
