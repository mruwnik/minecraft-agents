// The things I carry (help section item).
import { compatibleInventoryStacks, inventoryCompactPair } from '../../inventory/compact.mjs'
import { enchantNames, itemsArg, enchantChoice, smeltWait, giveReport, craftRoom, craftReport, craftShortfall, furnaceReport, depositWanted, equipSlot, pickFuel, isNight, withdrawPlan, givePlan, shortNote, tooFarToGive, lyingFrom, GIVE_REACH } from '../../lib.mjs'
import { countsOf, chestTransfer, inventoryCounts, inventoryQuiet, findItem, dropsNear, sweepDrops, goNear, findBlockByName, craftBatch, containerAt } from '../helpers.mjs'
import { goals, bot, mcData, ready } from '../state.mjs'
import { fighting, flee, holingUp } from '../reflexes.mjs'

export let compactingInventory = false

export const itemLong = {
  async craft (a) {
    const item = mcData.itemsByName[a.item]
    if (!item) throw new Error(`unknown item ${a.item}`)
    const count = a.count ?? 1
    let table = null
    if (!bot.recipesFor(item.id, null, 1, null).length) {
      const tp = findBlockByName('crafting_table', 32)[0]
      if (tp) { await goNear(tp, 2); table = bot.blockAt(tp) }
    }
    const recipe = bot.recipesFor(item.id, null, 1, table)[0]
    if (!recipe) {
      const all = bot.recipesAll(item.id, null, table ?? true)
      if (!all.length) throw new Error(`no recipe for ${a.item}`)
      const needs = r => Object.fromEntries(r.delta.filter(d => d.count < 0).map(d => [mcData.items[d.id].name, -d.count]))
      const short = craftShortfall(all.map(needs), inventoryCounts())
      const noTable = all[0].requiresTable && !table
      throw new Error(`can't craft ${a.item}: ${[noTable && 'needs a crafting table within 32 blocks', short && `you are short of ${short}`].filter(Boolean).join('; ') || 'you seem to carry everything (counts out of step? open a chest or retry)'}`)
    }
    // Crafting clicks race the server's state updates through ViaBackwards, so a craft can be silently
    // rejected and leave our local inventory wrong. Craft one batch at a time, let the server's resync land
    // (reopening the table forces one), and retry until the item count has really gone up.
    const have = () => inventoryCounts()[a.item] ?? 0
    const start = have()
    const before = inventoryCounts()
    const ingredients = recipe.delta.filter(d => d.count < 0).map(d => mcData.items[d.id].name)
    // A fixed wait after the click was the whole of #133. The server's answer can land after it, so an accepted batch
    // read as a failure and the loop crafted it AGAIN: that is how a shears craft ate two iron ingots and still said
    // nothing was made. Wait for the count to MOVE, up to five seconds, and stop waiting the moment it does.
    const settleTo = async target => { for (let i = 0; i < 25 && have() < target; i++) await bot.waitForTicks(4) }
    const recipeSettled = async () => { await inventoryQuiet(); return bot.recipesFor(item.id, null, 1, table)[0] }
    // what the ingredients really cost, read back at the end: the difference between a free retry and a real loss
    const spent = () => Object.fromEntries(ingredients.map(name => [name, Math.max(0, (before[name] ?? 0) - (inventoryCounts()[name] ?? 0))]))
    // before calling ingredients lost, look on the ground: Chani's 16 planks were lying by the table the whole time
    const giveUp = async why => {
      await inventoryQuiet()
      const onGround = () => dropsNear(6).filter(d => ingredients.includes(d.item))
      const fell = onGround().map(d => d.item)
      if (fell.length) await sweepDrops(6)
      const lying = onGround().map(d => `${d.item}@${d.x},${d.y},${d.z}`)
      throw new Error(craftReport({ item: a.item, count, made: have() - start, spent: spent(), why, fell, lying }).error)
    }
    for (let attempt = 1; have() < start + count; attempt++) {
      const full = craftRoom({ freeSlots: bot.inventory.emptySlotCount(), stacks: bot.inventory.items().filter(i => i.name === a.item).map(i => i.count), stackSize: item.stackSize, batch: recipe.result.count, item: a.item, made: have() - start, count })
      if (full) throw new Error(full)
      if (attempt > Math.ceil(count / recipe.result.count) + 5) await giveUp('the server kept rejecting the craft')
      // the last batch's grid and cursor may still be on their way back to the pockets: a recipe the pockets cannot
      // fill is asked for again once they are quiet, before the ingredients are called gone
      const r = bot.recipesFor(item.id, null, 1, table)[0] ?? await recipeSettled()
      if (!r) await giveUp('the ingredients ran out')
      const target = have() + r.result.count
      await craftBatch(r, table)
      await settleTo(target)
      await inventoryQuiet()
    }
    return craftReport({ item: a.item, count, made: have() - start })
  },

  async smelt (a) {
    const block = await containerAt(a, ['furnace', 'blast_furnace', 'smoker'])
    const furnace = await bot.openFurnace(block)
    try {
      const count = a.count ?? 1
      if (a.fuel) {
        const f = findItem(a.fuel)
        // only what the job burns: the rest of the stack is of more use in my pockets than in a furnace
        const needed = a.fuelCount ?? pickFuel([{ name: f.name, count: f.count }], count)?.count ?? count
        await furnace.putFuel(f.type, null, Math.min(needed, f.count))
      }
      if (a.item) { const input = findItem(a.item); await furnace.putInput(input.type, null, Math.min(count, input.count)) }
      await bot.waitForTicks(5)
      const waiting = furnace.inputItem()?.count ?? 0
      if (!waiting) throw new Error('nothing in the furnace to smelt')
      // no fuel named and none burning: feed it from the inventory, or say so instead of waiting for nothing
      if (!a.fuel && !furnace.fuelItem() && !(furnace.fuel > 0)) {
        const pick = pickFuel(bot.inventory.items().map(i => ({ name: i.name, count: i.count })), waiting)
        if (!pick) throw new Error('no fuel: carry coal, charcoal, planks or logs')
        await furnace.putFuel(findItem(pick.name).type, null, pick.count)
      }
      const deadline = Date.now() + (a.wait ?? count * 11 + 5) * 1000
      const verdict = () => smeltWait({ got: furnace.outputItem()?.count ?? 0, wanted: count, night: isNight(bot.time.timeOfDay), timedOut: Date.now() >= deadline })
      while (verdict() === 'wait') await new Promise(r => setTimeout(r, 1000))
      const night = verdict() === 'night'
      if (furnace.outputItem()) await furnace.takeOutput()
      // the night only passes when EVERYBODY sleeps, and a furnace needs nobody watching it
      if (night) return { stopped: `night fell with ${furnace.inputItem()?.count ?? 0} still to cook: the furnace cooks on without you. Sleep now (the others cannot skip the night while you are up), then furnace_take x=${block.position.x} y=${block.position.y} z=${block.position.z}` }
    } finally { furnace.close() }
    return {}
  },

  // enchant item=<name> [x= y= z= of the table] [slot=1-3, default: the dearest I can pay]: lapis comes from my inventory (slot n needs n lapis and xp level >= its offer)
  async enchant (a) {
    if (!a.item) throw new Error('enchant needs item= (what to enchant, from your inventory)')
    if (!bot.inventory.items().some(i => i.name === a.item && !i.enchants?.length)) throw new Error(`you carry no unenchanted ${a.item}`)
    const block = await containerAt(a, ['enchanting_table'])
    const table = await bot.openEnchantmentTable(block)
    try {
      // from the table's own window: its slot numbers are not my inventory's ("invalid operation")
      const item = table.items().find(i => i.name === a.item && !i.enchants?.length)
      const lapis = table.items().find(i => i.name === 'lapis_lazuli')
      await table.putTargetItem(item)
      if (lapis) await table.putLapis(lapis)
      // the offers arrive a moment after the item lies in the table
      for (let i = 0; i < 20 && !table.enchantments.some(e => e.level > 0); i++) await bot.waitForTicks(2)
      const { choice, error } = enchantChoice(table.enchantments, bot.experience.level, lapis?.count ?? 0, a.slot)
      if (error) { await table.takeTargetItem().catch(() => {}); throw new Error(error) }
      const cost = table.enchantments[choice].level
      await table.enchant(choice)
      const done = await table.takeTargetItem()
      return { enchanted: done.name, slot: choice + 1, asked: cost, got: enchantNames(done.enchants, id => bot.registry.enchantments?.[id]?.name), xpLevel: bot.experience.level }
    } finally { table.close() }
  },

  async furnace_take (a) {
    const block = await containerAt(a, ['furnace', 'blast_furnace', 'smoker'])
    const furnace = await bot.openFurnace(block)
    try {
      await bot.waitForTicks(5)
      if (furnace.outputItem()) await furnace.takeOutput()
      return furnaceReport({ input: furnace.inputItem(), fuel: furnace.fuelItem(), burning: furnace.fuel > 0 })
    } finally { furnace.close() }
  },

  async inventory_compact (a) {
    if (!mcData.itemsByName[a.item]) throw new Error('inventory_compact needs a known item=')
    const maxMoves = a.maxMoves ?? 72
    if (!Number.isInteger(maxMoves) || maxMoves < 1 || maxMoves > 72) throw new Error('maxMoves= must be 1..72')
    if (bot.currentWindow || bot.inventory.selectedItem) throw new Error('close the current window and empty the cursor before inventory_compact')
    if (bot.autoEat?.isEating) throw new Error('inventory_compact must wait for the bot to finish its meal')
    bot.pathfinder.setGoal(null)
    const total = () => inventoryCounts()[a.item] ?? 0
    const stacks = () => bot.inventory.items().filter(i => i.name === a.item).length
    const before = { count: total(), stacks: stacks(), freeSlots: bot.inventory.emptySlotCount() }
    let moves = 0, source = null
    compactingInventory = true
    try {
      while (moves < maxMoves) {
        if (!ready || bot.health <= 0 || flee || fighting || holingUp) throw new Error('inventory_compact interrupted by body safety')
        if (bot.currentWindow || bot.inventory.selectedItem) throw new Error('inventory window or cursor changed during compaction')
        const pair = inventoryCompactPair(bot.inventory.items(), a.item)
        if (!pair) break
        source = pair.source
        const sourceCount = bot.inventory.slots[source].count
        const destinationCount = bot.inventory.slots[pair.destination].count
        await bot.moveSlotItem(source, pair.destination)
        await bot.waitForTicks(3)
        if (bot.inventory.selectedItem || total() !== before.count ||
            (bot.inventory.slots[source]?.count ?? 0) !== sourceCount - pair.moved ||
            (bot.inventory.slots[pair.destination]?.count ?? 0) !== destinationCount + pair.moved) {
          throw new Error('inventory merge was not confirmed; inspect inventory before retrying')
        }
        source = null
        moves++
      }
      return { item: a.item, moves, count: total(), beforeStacks: before.stacks, afterStacks: stacks(), freedSlots: bot.inventory.emptySlotCount() - before.freeSlots }
    } finally {
      try {
        const cursor = bot.inventory.selectedItem
        if (cursor && source !== null && !bot.currentWindow) {
          const current = bot.inventory.slots[source]
          if (!current || (compatibleInventoryStacks(current, cursor) && current.count + cursor.count <= current.stackSize)) await bot.clickWindow(source, 0, 0)
        }
        if (bot.inventory.selectedItem) throw new Error('inventory_compact cursor restoration pending; inspect inventory before another action')
      } finally { compactingInventory = false }
    }
  },

  async deposit (a) {
    const wanted = depositWanted(a, bot.inventory.items().map(i => ({ name: i.name, count: i.count })))
    if (wanted.error) throw new Error(wanted.error)
    // same plan as withdraw, the other way round: what I carry is the source
    const { plan, corrected, eaten } = await chestTransfer(a, 'deposit', chest => withdrawPlan(wanted, countsOf(chest.items())))
    if (plan.short.length) throw new Error(`you carry less than asked (have/wanted): ${plan.short.join(' ')}; deposited what there was`)
    return { ...(corrected ? { corrected } : {}), ...(eaten ? { eaten } : {}) }
  },

  async withdraw (a) {
    const { plan, corrected, eaten } = await chestTransfer(a, 'withdraw', chest => withdrawPlan(itemsArg(a), countsOf(chest.containerItems())))
    if (plan.short.length) throw new Error(`chest has less than asked (have/wanted): ${plan.short.join(' ')}; took what there was`)
    return { ...(corrected ? { corrected } : {}), ...(eaten ? { eaten } : {}) }
  },

  async give (a) {
    const e = bot.players[a.player]?.entity
    if (!e) throw new Error(`can't see ${a.player}`)
    // within arm's reach first: a toss flies about three blocks, and from three off it lay where the player never came
    // (card 8c7b6652); the goal is half a block inside the reach so a diagonal cell still counts
    await bot.pathfinder.goto(new goals.GoalFollow(e, GIVE_REACH - 0.5))
    // a fleeing or walking player is gone again by the time we toss: keep the items rather than litter
    const tooFar = tooFarToGive(a.player, bot.entity.position.distanceTo(e.position))
    if (tooFar) throw new Error(tooFar)
    await bot.lookAt(e.position.offset(0, 1.2, 0))
    const item = findItem(a.item)
    const drops = () => Object.values(bot.entities).filter(d => d.name === 'item' && d.position.distanceTo(bot.entity.position) <= 8)
    const before = new Set(drops().map(d => d.id))
    const had = inventoryCounts()[item.name] ?? 0
    // every stack until the count is met (bot.toss crosses stacks): capped at the first stack, count=101 gave 64 and said taken=yes
    const { give: tossed } = givePlan({ count: a.count, carried: had })
    await bot.toss(item.type, null, tossed)
    // did it arrive? Watch my own drop: gone within 5 s = picked up (the toss itself said ok even when nobody got the bread)
    const mine = () => drops().filter(d => !before.has(d.id))
    await bot.waitForTicks(10)
    for (let i = 0; i < 18 && mine().length; i++) await bot.waitForTicks(5)
    // my own drop is mine again after 2 s: across a fence it falls at my feet and I pick it up myself, which looked like taken
    const cameBack = Math.max(0, (inventoryCounts()[item.name] ?? 0) - (had - tossed))
    // a drop still lying says how far it is from the player: "has not picked it up" read as a full inventory when it was distance
    const theirs = bot.players[a.player]?.entity?.position ?? null
    const short = shortNote({ item: item.name, asked: a.count ?? had, carried: had })
    return { ...giveReport(a.player, mine().map(d => lyingFrom(d.position, theirs, a.player)), cameBack), ...(short ? { short } : {}) }
  }
}

export const itemQuick = {
  async equip (a) {
    const item = findItem(a.item)
    const destination = a.destination ?? equipSlot(item.name)
    await bot.equip(item, destination)
    return { on: destination }
  },
  async toss (a) { const i = findItem(a.item); await bot.toss(i.type, null, Math.min(a.count ?? i.count, i.count)); return {} }
}
