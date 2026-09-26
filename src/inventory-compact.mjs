import { isDeepStrictEqual } from 'node:util'

// Item.equal currently omits modern components. Never merge merely because
// two stacks share a name: custom food, damage and other data must match.
export function compatibleInventoryStacks (a, b) {
  return !!a && !!b && Number.isInteger(a.type) && a.type === b.type && a.metadata === b.metadata &&
    isDeepStrictEqual(a.nbt, b.nbt) &&
    isDeepStrictEqual(a.componentMap ?? a.components ?? [], b.componentMap ?? b.components ?? []) &&
    isDeepStrictEqual(a.removedComponents ?? [], b.removedComponents ?? [])
}

export function inventoryCompactPair (items, name) {
  const stacks = items.filter(i => i.name === name && Number.isInteger(i.slot) && Number.isInteger(i.count) && i.count > 0 && i.stackSize > 1)
  for (const destination of stacks.slice().sort((a, b) => b.count - a.count || a.slot - b.slot)) {
    const free = destination.stackSize - destination.count
    if (free <= 0) continue
    const sources = stacks.filter(i => i.slot !== destination.slot && i.count <= destination.count && compatibleInventoryStacks(i, destination))
      .sort((a, b) => a.count - b.count || a.slot - b.slot)
    const source = sources.find(i => i.count <= free) ?? sources[0]
    if (source) return { source: source.slot, destination: destination.slot, moved: Math.min(source.count, free) }
  }
  return null
}
