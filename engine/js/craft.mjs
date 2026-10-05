// Crafting for a body: one batch at a time, never walks. See engine/README.md "Primitives: implementation notes".
// The traps this knows about: a server that silently drops a craft (the count never rises), a full inventory
// (the result would be lost), items left in the 2x2 grid, and recipes that differ only in the wood they use.

import vec3 from 'vec3'

const { Vec3 } = vec3

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

const posOf = block => ({ x: block.position.x, y: block.position.y, z: block.position.z })

// { table } (a block or null) or { error } (an unreachable or out-of-reach result)
const resolveTable = (bot, a, reach) => {
  if (!a.table) return { table: findTable(bot, reach) }
  const block = bot.blockAt(new Vec3(a.table.x, a.table.y, a.table.z))
  if (!isTable(block)) return { error: { status: 'unreachable', reason: 'not-a-table' } }
  if (eyeDistance(bot, block) > reach) return { error: { status: 'out-of-reach', reason: 'too-far', table: posOf(block) } }
  return { table: block }
}

// no table in reach: the nearest known one (out of reach), or none
const missingTable = bot => {
  const pos = bot.findBlocks({ matching: isTable, maxDistance: 32, count: 1 })[0]
  return pos
    ? { status: 'out-of-reach', reason: 'too-far', table: { x: pos.x, y: pos.y, z: pos.z } }
    : { status: 'unreachable', reason: 'no-table' }
}

// every candidate recipe ({name: n} of ingredients) and what is carried: engine.craft chooses what to report as missing
const shortOf = (bot, id, table) => ({
  recipes: bot.recipesAll(id, null, table ?? null).map(r => ingredientsOf(bot, r)),
  have: carriedCounts(bot)
})

// The result goes into an empty slot (mineflayer does not join it to a stack), then gets merged. At a table that means
// two empty slots for a result with no stack of its own yet (one for the result, one the new stack keeps) and one when it
// joins a stack: with less mineflayer tosses the leftover ingredients on the ground
const hasRoom = (bot, item, perBatch, table) => {
  const free = bot.inventory.emptySlotCount()
  const stackSize = bot.registry.itemsByName[item].stackSize ?? 64
  const joins = bot.inventory.items().some(i => i.name === item && i.count + perBatch <= stackSize)
  return free >= (table && !joins ? 2 : 1) || (!table && joins)
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

// ask the server to resend the whole inventory window (fixes a stale local view), bounded, errors ignored; then settle
async function resync (bot, ctx, timeScale) {
  await syncWindow(bot, timeScale)
  await settle(bot, ctx, timeScale)
}

async function syncWindow (bot, timeScale) {
  let timer
  const bound = new Promise(resolve => { timer = setTimeout(resolve, 3000 * timeScale) })
  await Promise.race([Promise.resolve().then(() => bot._syncWindow?.(bot.inventory)).catch(() => {}), bound])
  clearTimeout(timer)
}

// hand back whatever a 2x2 craft left in the grid or on the cursor
async function settleGrid (bot, timeScale) {
  await syncWindow(bot, timeScale)
  const grid = [1, 2, 3, 4].some(i => bot.inventory.slots[i])
  if (grid || bot.inventory.selectedItem) bot.closeWindow(bot.inventory)
}

const usedSince = (bot, before, names) => Object.fromEntries(
  [...names].map(n => [n, Math.max(0, (before[n] ?? 0) - (carriedCounts(bot)[n] ?? 0))]).filter(([, n]) => n > 0)
)

const made0 = (bot, item, start) => (carriedCounts(bot)[item] ?? 0) - start

const closedWindow = bot => !bot.currentWindow || bot.currentWindow === bot.inventory

// a click that reports whether it worked: merging is cosmetic, so a failed click just stops it
async function click (bot, slot) {
  try {
    await bot.clickWindow(slot, 0, 0)
    return true
  } catch {
    return false
  }
}

const partialStacks = (bot, item) => {
  const stackSize = bot.registry.itemsByName[item].stackSize ?? 64
  return bot.inventory.items().filter(i => i.name === item && i.count < stackSize).sort((a, b) => a.count - b.count)
}

// move the smallest non-full stack onto the largest; true when both clicks worked
async function mergeOnce (bot, ctx, item) {
  const stacks = partialStacks(bot, item)
  if (stacks.length < 2) return false
  const from = stacks[0].slot
  const picked = await click(bot, from)
  ctx.alive()
  if (!picked) return false
  const dropped = await click(bot, stacks.at(-1).slot)
  ctx.alive()
  if (!dropped || bot.inventory.selectedItem) {
    await click(bot, from)
    ctx.alive()
  }
  return dropped
}

// the server-sent result stacks do not join mineflayer's own, so a craft can leave 35+1+1+1: put them together.
// At most 8 merges; returns how many happened.
async function mergeStacks (bot, ctx, item) {
  let merged = 0
  while (merged < 8 && await mergeOnce(bot, ctx, item)) merged++
  return merged
}

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
  if (!table && bot.recipesAll(id, null, null).length === 0) return missingTable(bot)

  ctx.onAbort(() => { if (bot.currentWindow) bot.closeWindow(bot.currentWindow) })

  // a cut mid-batch can leave a cursor stack or an open window and a local view the server no longer shares
  if (bot.inventory.selectedItem || (bot.currentWindow && bot.currentWindow !== bot.inventory)) {
    if (bot.currentWindow) bot.closeWindow(bot.currentWindow)
  }
  await resync(bot, ctx, timeScale)

  const before = carriedCounts(bot)
  const start = before[item] ?? 0
  const names = new Set()
  const maxAttempts = Math.ceil(count / all[0].result.count) + 3
  let reason = null
  let shortage = null
  let full = false

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    const made = (carriedCounts(bot)[item] ?? 0) - start
    if (made >= count) break
    const r = bot.recipesFor(id, null, 1, table)[0]
    if (!r) {
      await resync(bot, ctx, timeScale)
      if (bot.recipesFor(id, null, 1, table)[0]) continue
      shortage = shortOf(bot, id, table)
      break
    }
    if (!hasRoom(bot, item, r.result.count, table)) { full = true; break }
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
    if (!landed) await resync(bot, ctx, timeScale)
    await settle(bot, ctx, timeScale)
    if (!table) await settleGrid(bot, timeScale)
    // the result lands in a stack of its own: with little room merge as we go, or the leftover ingredients have nowhere to return to
    if (bot.inventory.emptySlotCount() < 3 && closedWindow(bot) && partialStacks(bot, item).length > 1) await mergeStacks(bot, ctx, item)
  }

  await settle(bot, ctx, timeScale)
  if (made0(bot, item, start) > 0 && closedWindow(bot)) {
    // merge clicks name slots by the local view: make it the server's first, or a click lands on another item's stack
    await syncWindow(bot, timeScale)
    if (await mergeStacks(bot, ctx, item) > 0) await settle(bot, ctx, timeScale)
  }
  const made = (carriedCounts(bot)[item] ?? 0) - start
  const used = usedSince(bot, before, names)
  if (made >= count) return { status: 'crafted', item, made, used }
  if (shortage) return made > 0 ? { status: 'partial', item, made, used, reason: 'no-item', ...shortage } : { status: 'no-item', ...shortage }
  if (full) return made > 0 ? { status: 'partial', item, made, used, reason: 'full' } : { status: 'full', made, used }
  const why = reason ?? 'server rejected the craft'
  return made > 0 ? { status: 'partial', item, made, used, reason: why } : { status: 'failed', item, made: 0, used, reason: why }
}
