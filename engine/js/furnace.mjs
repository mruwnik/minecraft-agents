// One visit to a furnace, blast furnace or smoker: open, act, settle, close. It never waits for the cooking and decides
// nothing (what to load, when to come back); refusals come back as data. See engine/README.md, primitive `furnace`.
import vec3 from 'vec3'

const { Vec3 } = vec3

const FURNACE = /^(furnace|blast_furnace|smoker)$/
const SLOTS = { input: 0, fuel: 1, output: 2 }

const stackOf = s => (s ? { name: s.name, count: s.count } : null)
const barOf = (fraction, total) => Math.round((fraction ?? 0) * total)

// The server sends the bars as window properties: both totals (1 burn, 3 cook) once when the window opens, the running
// values (0 burn left, 2 cook done) only when they change, so every tick while it burns. mineflayer starts listening
// after the open has resolved and misses the totals, so the packets are collected here from before the open.
const BAR = { 0: 'burnLeft', 1: 'burnTotal', 2: 'cookDone', 3: 'cookTotal' }

const collectBars = bot => {
  const seen = []
  const onBar = packet => seen.push(packet)
  bot._client.on('craft_progress_bar', onBar)
  return { barsOf: win => Object.fromEntries(seen.filter(p => p.windowId === win.id).map(p => [BAR[p.property], p.value])), stop: () => bot._client.removeListener('craft_progress_bar', onBar) }
}

// the window as data; a bar the server never sent is null. An unchanging value is never sent: a total with no running
// value means 0 (nothing burns, nothing cooks).
const stateOf = (win, block, bars) => ({
  kind: block.name,
  input: stackOf(win.slots[SLOTS.input]),
  fuel: stackOf(win.slots[SLOTS.fuel]),
  output: stackOf(win.slots[SLOTS.output]),
  lit: Boolean(block.getProperties?.().lit) || (bars.burnLeft ?? 0) > 0, // the block state lags the bar by a tick
  burn: bars.burnTotal == null ? null : { left: bars.burnLeft ?? 0, total: bars.burnTotal },
  cook: bars.cookTotal == null ? null : { done: bars.cookDone ?? 0, total: bars.cookTotal }
})

// What is carried, as the open window sees it. bot.inventory is not updated while the window is open (it catches up
// after the close), so every count and every free slot in a visit is read from the window's own inventory part.
const pocketsOf = win => win.slots.slice(win.inventoryStart, win.inventoryEnd).filter(Boolean)
const carriedCount = (win, name) => pocketsOf(win).filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)

// what the pockets can still take of a stack: the room left in partial stacks of it, and a whole stack when a slot is free
const roomFor = (bot, win, name) => {
  const size = bot.registry.itemsByName[name]?.stackSize ?? 64
  const partial = pocketsOf(win).filter(i => i.name === name).reduce((sum, i) => sum + Math.max(0, size - i.count), 0)
  return partial + (pocketsOf(win).length < win.inventoryEnd - win.inventoryStart ? size : 0)
}

const putters = win => ({ input: win.putInput, fuel: win.putFuel })
const takers = win => ({ input: win.takeInput, fuel: win.takeFuel, output: win.takeOutput })

// refusals that need no click: an item not carried, a slot that holds something else
const loadRefusal = (win, wanted) => {
  for (const [slot, { item }] of wanted) {
    if (carriedCount(win, item) === 0) return { status: 'no-item', slot, item }
    const held = win.slots[SLOTS[slot]]
    if (held && held.name !== item) return { status: 'busy', slot, holds: stackOf(held) }
  }
  return null
}

async function load (bot, ctx, win, block, a, { settle, barsOf }) {
  const wanted = ['input', 'fuel'].filter(slot => a[slot]).map(slot => [slot, a[slot]])
  const refusal = loadRefusal(win, wanted)
  if (refusal) return refusal
  const moved = {}
  for (const [slot, { item, count }] of wanted) {
    const type = bot.registry.itemsByName[item].id
    const held = win.slots[SLOTS[slot]]?.count ?? 0
    const had = carriedCount(win, item)
    await putters(win)[slot].call(win, type, null, Math.min(count ?? Infinity, had))
    ctx.alive()
    await settle(win)
    // the pockets say what went in: a single fuel item is burnt at once and the slot never shows it
    moved[slot] = had - carriedCount(win, item)
    if (moved[slot] > 0) continue
    const full = held >= (bot.registry.itemsByName[item].stackSize ?? 64)
    return { status: 'rejected', slot, item, reason: full ? 'slot-full' : 'not-accepted' }
  }
  return { status: 'ok', moved, ...stateOf(win, block, barsOf(win)) }
}

async function take (bot, ctx, win, block, a, { settle, barsOf }) {
  const parts = ['output', 'input', 'fuel'].filter(slot => slot === 'output' ? a.output !== false : a[slot])
  const before = Object.fromEntries(parts.map(slot => [slot, stackOf(win.slots[SLOTS[slot]])]).filter(([, s]) => s))
  for (const [slot, held] of Object.entries(before)) {
    if (roomFor(bot, win, held.name) <= 0) continue
    await takers(win)[slot].call(win)
    ctx.alive()
  }
  await settle(win) // also when nothing was clicked: the bars arrive a moment after the open
  const taken = Object.entries(before)
    .map(([slot, held]) => ({ part: slot, name: held.name, count: held.count - (win.slots[SLOTS[slot]]?.count ?? 0) }))
    .filter(t => t.count > 0)
  const stuck = Object.entries(before).some(([slot]) => win.slots[SLOTS[slot]])
  return { status: stuck ? 'full' : 'ok', taken, ...stateOf(win, block, barsOf(win)) }
}

const OPS = {
  read: async (bot, ctx, win, block, a, { settle, barsOf }) => {
    await settle(win)
    return { status: 'ok', ...stateOf(win, block, barsOf(win)) }
  },
  load,
  take
}

export async function furnaceVisit (bot, ctx, a, { distanceTo, reach, settle }) {
  const block = bot.blockAt(new Vec3(a.pos.x, a.pos.y, a.pos.z))
  if (!block || block.name === 'air') return { status: 'missing' }
  if (!FURNACE.test(block.name)) return { status: 'cannot', reason: 'not-a-furnace' }
  const distance = distanceTo(a.pos)
  if (distance > reach) return { status: 'unreachable', reason: 'too-far', distance: Math.round(distance * 100) / 100 }
  const bars = collectBars(bot)
  try {
    const win = await bot.openFurnace(block)
    // win.close(), not bot.closeWindow(win): only the former emits 'close', which is when mineflayer's openFurnace removes
    // the listener it adds to the client for every window it opens
    ctx.onAbort(() => win.close())
    try {
      ctx.alive()
      return await OPS[a.op](bot, ctx, win, block, a, { settle, barsOf: bars.barsOf })
    } finally {
      win.close()
    }
  } finally {
    bars.stop()
  }
}
