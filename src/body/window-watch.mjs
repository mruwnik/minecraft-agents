// The container the body has open, for the dashboard's screen. Kept a moment after it closes: withdraw, deposit and
// smelt open and close a chest inside one call, often under a second, so a one-second poll would never see one otherwise
export const LINGER_MS = 5000

// the window's title as the game shows it: a chat component, usually {"translate":"container.chest"}
const titleOf = title => {
  try {
    const chat = typeof title === 'string' ? JSON.parse(title) : title
    return chat?.text || chat?.translate?.replace(/^container\./, '') || String(title)
  } catch { return String(title) }
}
const stacks = (slots, size) => slots.slice(0, size).flatMap((item, slot) => item ? [{ slot, name: item.name, count: item.count }] : [])

// at: the block the opener (containerAt, craftBatch, the furnace actions) declared with `opening` just before it
// opened the window - never a nearby-block guess, which once reported a barrel for a chest the body actually opened
// when both stood within range. Consumed by the next windowOpen and cleared, so a window nobody declared for (a
// villager trade) always answers null rather than reusing a stale position.
export function watchWindows (bot, { now = Date.now } = {}) {
  let declared = null
  let current = null
  bot.on('windowOpen', window => {
    const size = window.inventoryStart
    current = { type: window.type, title: titleOf(window.title), at: declared, size, open: true, closedAt: null, slots: stacks(window.slots, size) }
    declared = null
    window.on('updateSlot', slot => { if (slot < size) current.slots = stacks(window.slots, size) })
  })
  bot.on('windowClose', () => { if (current) current = { ...current, open: false, closedAt: now() } })
  return {
    open: () => {
      if (!current) return null
      if (current.open || now() - current.closedAt < LINGER_MS) return current
      current = null
      return null
    },
    opening: pos => { declared = pos ?? null }
  }
}
