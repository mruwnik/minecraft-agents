// Why JavaScript: Mineflayer boundary; reads enchantments off prismarine items.
// The enchantments an item carries, [{name, level}]: a book keeps them in the stored_enchantments component, which
// prismarine-item's enchants getter does not read.
export const enchantsOf = (registry, item) => {
  const stored = item.componentMap?.get?.('stored_enchantments')?.data
  if (stored) {
    const raw = Array.isArray(stored) ? stored : stored.enchantments ?? []
    return raw.map(e => ({ name: e.name ?? registry?.enchantments?.[e.id]?.name ?? `enchantment#${e.id}`, level: e.level ?? e.lvl }))
  }
  try {
    return (item.enchants ?? []).map(e => ({ name: e.name, level: e.lvl ?? e.level }))
  } catch {
    return []
  }
}
