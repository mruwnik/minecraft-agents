// Why JavaScript: Mineflayer boundary; calls bot.unequip.
// Emptying the main hand. mineflayer's unequip('hand') tosses the stack on the ground when nothing has room,
// so room is checked first and a full inventory is reported instead.
const HOTBAR_START = 36
const HOTBAR_SIZE = 9

const hotbarHasFree = bot => Array.from({ length: HOTBAR_SIZE }, (_, i) => bot.inventory.slots[HOTBAR_START + i]).some(s => !s)

export const hasRoom = bot => hotbarHasFree(bot) || bot.inventory.firstEmptyInventorySlot() != null

export async function emptyHand (bot, ctx) {
  const held = bot.heldItem
  if (!held) return { status: 'empty' }
  if (!hasRoom(bot)) return { status: 'full' }
  await bot.unequip('hand')
  ctx.alive()
  if (bot.heldItem) return { status: 'failed', reason: 'still-held' }
  return { status: 'ok', item: held.name }
}

// An unfilled stack of the item in the main slots or the hotbar (9..44): mineflayer's putAway merges into it.
const hasMerge = (bot, item) => Array.from({ length: 36 }, (_, i) => bot.inventory.slots[9 + i])
  .some(s => s && s.name === item.name && s.count < s.stackSize)

// The off hand (slot 45) or an armour slot (head 5, torso 6, legs 7, feet 8) back into the inventory: room is a free
// slot or an unfilled stack of it. What did not fit stays in the slot (full, kept: its count).
const SLOT_INDEX = { head: 5, torso: 6, legs: 7, feet: 8, 'off-hand': 45 }

export const emptiable = dest => dest in SLOT_INDEX

export async function emptySlot (bot, ctx, dest) {
  const held = bot.inventory.slots[SLOT_INDEX[dest]]
  if (!held) return { status: 'empty' }
  if (!hasRoom(bot) && !hasMerge(bot, held)) return { status: 'full' }
  await bot.unequip(dest)
  ctx.alive()
  const left = bot.inventory.slots[SLOT_INDEX[dest]]
  if (left && left.name === held.name && left.count < held.count) return { status: 'full', kept: left.count }
  if (left) return { status: 'failed', reason: 'still-held' }
  return { status: 'ok', item: held.name }
}
