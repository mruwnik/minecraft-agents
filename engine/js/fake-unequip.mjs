// The fake's `unequip` primitive: empty the main hand.
const SLOTS = 36

export const fakeUnequip = s => async () => {
  const held = s.self.held
  if (!held) return { status: 'empty' }
  if (s.inventory.length >= SLOTS) return { status: 'full' }
  s.self.held = null
  return { status: 'ok', item: held }
}
