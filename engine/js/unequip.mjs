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
