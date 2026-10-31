// Why JavaScript: wraps Mineflayer's equip, whose result depends on its client-side view of the inventory window.
// Puts a named item in a hand or armour slot and checks the outcome. Mineflayer's equip swaps by slot numbers from the
// client's view of the window; when that view is stale the server refuses the swap and the hand holds something else,
// so a swap is followed by a resync and a check, and a miss gets one more try.

import { syncWindow } from './craft.mjs'

const SLOTS = { head: 5, torso: 6, legs: 7, feet: 8, 'off-hand': 45 }

const inHotbar = slot => slot >= 36 && slot <= 44
const at = (bot, dest) => (dest === 'hand' ? bot.heldItem : bot.inventory.slots[SLOTS[dest]])

// { status: 'equipped' } | { status: 'no-item' } | { held } (no status: the caller words the failure) (held: the name the hand or slot has instead)
export async function hold (bot, name, dest, timeScale = 1) {
  for (let attempt = 0; attempt < 2; attempt++) {
    const item = bot.inventory.items().find(i => i.name === name)
    if (!item) return { status: 'no-item' }
    const swaps = dest !== 'hand' || !inHotbar(item.slot)
    await bot.equip(item, dest)
    if (swaps) await syncWindow(bot, timeScale)
    if (at(bot, dest)?.name === name) return { status: 'equipped' }
    if (!swaps) await syncWindow(bot, timeScale)
  }
  return { held: at(bot, dest)?.name ?? null }
}
