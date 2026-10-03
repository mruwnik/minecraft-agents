// Crafting for a body: one batch at a time, never walks. See engine/README.md "Primitives: implementation notes".
// The traps this knows about: a server that silently drops a craft (the count never rises), a full inventory
// (the result would be lost), items left in the 2x2 grid, and recipes that differ only in the wood they use.

import vec3 from 'vec3'

const { Vec3 } = vec3

const total = counts => Object.values(counts).reduce((s, n) => s + n, 0)

const missingFor = (recipe, have) => Object.fromEntries(
  Object.entries(recipe).map(([name, n]) => [name, n - (have[name] ?? 0)]).filter(([, n]) => n > 0)
)

// the missing ingredients of whichever recipe is closest to being satisfied
export const craftShortfall = (recipes, have) => recipes
  .map(r => missingFor(r, have))
  .reduce((best, m) => (best === null || total(m) < total(best) ? m : best), null) ?? {}

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

const carriedCounts = bot => bot.inventory.items().reduce((acc, i) => ({ ...acc, [i.name]: (acc[i.name] ?? 0) + i.count }), {})

const ingredientsOf = (bot, recipe) => Object.fromEntries(
  recipe.delta.filter(d => d.count < 0).map(d => [bot.registry.items[d.id].name, -d.count])
)

const eyeDistance = (bot, block) => {
  const p = bot.entity.position
  const c = block.position
  return Math.hypot(c.x + 0.5 - p.x, c.y + 0.5 - (p.y + bot.entity.height), c.z + 0.5 - p.z)
}

const isTable = block => block?.name === 'crafting_table'

const findTable = (bot, reach) => bot.findBlocks({ matching: isTable, maxDistance: reach + 1, count: 8 })
  .map(pos => bot.blockAt(pos))
  .find(b => isTable(b) && eyeDistance(bot, b) <= reach) ?? null

// { table } (a block or null) or { error } (an unreachable result)
const resolveTable = (bot, a, reach) => {
  if (!a.table) return { table: findTable(bot, reach) }
  const block = bot.blockAt(new Vec3(a.table.x, a.table.y, a.table.z))
  if (!isTable(block)) return { error: { status: 'unreachable', reason: 'not-a-table' } }
  if (eyeDistance(bot, block) > reach) return { error: { status: 'unreachable', reason: 'too-far' } }
  return { table: block }
}

const shortOf = (bot, id, table) => craftShortfall(
  bot.recipesAll(id, null, table ?? null).map(r => ingredientsOf(bot, r)),
  carriedCounts(bot)
)

const hasRoom = (bot, item, perBatch) => {
  if (bot.inventory.emptySlotCount() > 0) return true
  const stackSize = bot.registry.itemsByName[item].stackSize ?? 64
  return bot.inventory.items().some(i => i.name === item && i.count + perBatch <= stackSize)
}

// poll until the carried count has risen to `target`; false when it never did
async function waitForCount (bot, ctx, item, target, timeScale) {
  const deadline = Date.now() + 5000 * timeScale
  for (;;) {
    ctx.alive()
    if ((carriedCounts(bot)[item] ?? 0) >= target) return true
    if (Date.now() >= deadline) return false
    await sleep(50 * timeScale)
  }
}

// wait until the inventory has been quiet (no updateSlot) for 150 ms, at most 2 s: the server hands the cursor
// stack back to the pockets some hundreds of ms after a craft
async function settle (bot, ctx, timeScale) {
  const inventory = bot.inventory
  if (typeof inventory.on !== 'function') return
  let last = Date.now()
  const touch = () => { last = Date.now() }
  const deadline = last + 2000 * timeScale
  inventory.on('updateSlot', touch)
  try {
    for (;;) {
      ctx.alive()
      const now = Date.now()
      if (now - last >= 150 * timeScale || now >= deadline) return
      await sleep(25 * timeScale)
    }
  } finally {
    inventory.removeListener('updateSlot', touch)
  }
}

// hand back whatever a 2x2 craft left in the grid or on the cursor
async function settleGrid (bot, timeScale) {
  let timer
  const bound = new Promise(resolve => { timer = setTimeout(resolve, 3000 * timeScale) })
  await Promise.race([Promise.resolve().then(() => bot._syncWindow?.(bot.inventory)).catch(() => {}), bound])
  clearTimeout(timer)
  const grid = [1, 2, 3, 4].some(i => bot.inventory.slots[i])
  if (grid || bot.inventory.selectedItem) bot.closeWindow(bot.inventory)
}

const usedSince = (bot, before, names) => Object.fromEntries(
  [...names].map(n => [n, Math.max(0, (before[n] ?? 0) - (carriedCounts(bot)[n] ?? 0))]).filter(([, n]) => n > 0)
)

export async function craftItem (bot, ctx, a, { timeScale = 1, reach = 4.5 } = {}) {
  const item = a.item
  const count = a.count ?? 1
  const info = bot.registry.itemsByName[item]
  if (!info) return { status: 'cannot', reason: 'unknown-item' }
  const id = info.id
  const all = bot.recipesAll(id, null, true)
  if (all.length === 0) return { status: 'cannot', reason: 'no-recipe' }

  const found = resolveTable(bot, a, reach)
  if (found.error) return found.error
  const table = found.table
  if (!table && bot.recipesAll(id, null, null).length === 0) return { status: 'unreachable', reason: 'no-table' }

  ctx.onAbort(() => { if (bot.currentWindow) bot.closeWindow(bot.currentWindow) })

  const before = carriedCounts(bot)
  const start = before[item] ?? 0
  const names = new Set()
  const maxAttempts = Math.ceil(count / all[0].result.count) + 3
  let reason = null
  let short = null
  let full = false

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    const made = (carriedCounts(bot)[item] ?? 0) - start
    if (made >= count) break
    const r = bot.recipesFor(id, null, 1, table)[0]
    if (!r) {
      await settle(bot, ctx, timeScale)
      if (bot.recipesFor(id, null, 1, table)[0]) continue
      short = shortOf(bot, id, table)
      break
    }
    if (!hasRoom(bot, item, r.result.count)) { full = true; break }
    Object.keys(ingredientsOf(bot, r)).forEach(n => names.add(n))
    const now = carriedCounts(bot)[item] ?? 0
    try {
      await bot.craft(r, 1, table)
    } catch (err) {
      reason = err.message
    }
    const landed = await waitForCount(bot, ctx, item, now + r.result.count, timeScale)
    if (!landed) reason = reason ?? 'server rejected the craft'
    if (landed) reason = null
    await settle(bot, ctx, timeScale)
    if (!table) await settleGrid(bot, timeScale)
  }

  await settle(bot, ctx, timeScale)
  const made = (carriedCounts(bot)[item] ?? 0) - start
  const used = usedSince(bot, before, names)
  if (made >= count) return { status: 'crafted', item, made, used }
  if (short) return made > 0 ? { status: 'partial', item, made, used, reason: 'no-item', short } : { status: 'no-item', short }
  if (full) return made > 0 ? { status: 'partial', item, made, used, reason: 'full' } : { status: 'full', made, used }
  const why = reason ?? 'server rejected the craft'
  return made > 0 ? { status: 'partial', item, made, used, reason: why } : { status: 'failed', item, made: 0, used, reason: why }
}
