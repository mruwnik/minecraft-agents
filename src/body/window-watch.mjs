// The container the body has open, for the dashboard's screen. Kept a moment after it closes: withdraw, deposit and
// smelt open and close a chest inside one call, often under a second, so a one-second poll would never see one otherwise
export const LINGER_MS = 5000

const BLOCKS = {
  'minecraft:generic_9x3': /^(chest|trapped_chest|barrel|[a-z_]*shulker_box)$/,
  'minecraft:generic_9x6': /^(chest|trapped_chest)$/,
  'minecraft:furnace': /^furnace$/,
  'minecraft:blast_furnace': /^blast_furnace$/,
  'minecraft:smoker': /^smoker$/,
  'minecraft:hopper': /^hopper$/,
  'minecraft:generic_3x3': /^(dispenser|dropper)$/,
  'minecraft:crafting': /^crafting_table$/
}
export const containerBlocks = type => BLOCKS[type] ?? null

// the window's title as the game shows it: a chat component, usually {"translate":"container.chest"}
const titleOf = title => {
  try {
    const chat = typeof title === 'string' ? JSON.parse(title) : title
    return chat?.text || chat?.translate?.replace(/^container\./, '') || String(title)
  } catch { return String(title) }
}
const stacks = (slots, size) => slots.slice(0, size).flatMap((item, slot) => item ? [{ slot, name: item.name, count: item.count }] : [])

export function watchWindows (bot, { nearest, now = Date.now }) {
  let current = null
  bot.on('windowOpen', window => {
    const size = window.inventoryStart
    current = { type: window.type, title: titleOf(window.title), at: nearest(window.type), size, open: true, closedAt: null, slots: stacks(window.slots, size) }
    window.on('updateSlot', slot => { if (slot < size) current.slots = stacks(window.slots, size) })
  })
  bot.on('windowClose', () => { if (current) current = { ...current, open: false, closedAt: now() } })
  return () => {
    if (!current) return null
    if (current.open || now() - current.closedAt < LINGER_MS) return current
    current = null
    return null
  }
}
