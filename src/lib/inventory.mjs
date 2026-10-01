// Inventory bookkeeping: broken tools, which slot to equip into, and what a hand is missing for a job.

// positive count deficits shared by build bills; existing callers give this the public names billShortfall or shortfall.
export const itemShortfall = (bill, have = {}) =>
  Object.fromEntries(Object.entries(bill).map(([item, n]) => [item, n - (have[item] ?? 0)]).filter(([, n]) => n > 0))

// the server tells a player which of its items just broke with an entity status (47 main hand .. 52 boots). Nothing else does: my axe
// wore out unnoticed and the body, unarmed without knowing it, ran from a zombie it should have fought
const BROKEN = { 47: 'hand', 48: 'off-hand', 49: 'head', 50: 'torso', 51: 'legs', 52: 'feet' }
export const brokenSlot = status => BROKEN[status] ?? null

// where an item goes when `equip` is given no destination: a helmet "equipped" into the hand answered ok and protected nothing
const SLOTS = [[/_helmet$|^carved_pumpkin$/, 'head'], [/_chestplate$|^elytra$/, 'torso'], [/_leggings$/, 'legs'], [/_boots$/, 'feet'], [/^shield$/, 'off-hand']]
export const equipSlot = itemName => SLOTS.find(([pattern]) => pattern.test(itemName))?.[1] ?? 'hand'

// the inventory screen as the dashboard draws it, by mineflayer's player window slot (5-8 armour, 9-35 main, 36-44 hotbar,
// 45 offhand). 0-4 is the 2x2 crafting grid, which the game empties back into the inventory when the screen closes
export const inventorySlots = (slots, quickBarSlot) => ({
  slots: slots.flatMap((item, slot) => item && slot >= 5 ? [{ slot, name: item.name, count: item.count }] : []),
  selected: quickBarSlot
})

// the weakest tool that can harvest a block, when none of the carried item types can; null when the block needs no tool or one is carried.
// mineflayer-tool recurses for ever (until the heap is gone) when asked to equip for a block nothing carried can harvest
export function missingTool (harvestTools, carriedTypes, nameOf) {
  if (!harvestTools) return null
  if (carriedTypes.some(type => harvestTools[type])) return null
  return nameOf(Number(Object.keys(harvestTools)[0]))
}

// tools: [{name (null = bare hand), time, harvests}]. The fastest that is not a weapon; undefined when only a weapon can harvest the block
const isWeapon = name => /_sword$|^trident$|^mace$/.test(name ?? '')
export const peacefulTool = tools => tools.filter(t => !isWeapon(t.name) && t.harvests).sort((a, b) => a.time - b.time)[0]

// lying: where what I tossed still lies 5 s later ('x,y,z'); a thrown item can be picked up after 2 s
export const giveReport = (player, lying, cameBack = 0) => cameBack > 0
  ? { cameBack: `${cameBack} came back to you: NOT given. Something stands between you (a fence, a wall, a gate): go and stand on the same side as ${player}, within 2 blocks, and give again` }
  : lying.length
  ? { lying: `${lying.join(' ')}: ${player} has not picked it up (full inventory, walked off, or it fell out of their reach). Tell them where it lies, or take it back with collect` }
  : { taken: 'yes' }
