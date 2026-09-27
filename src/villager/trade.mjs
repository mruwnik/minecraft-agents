// Villager trading: parsing what is wanted, pricing an offer, and judging whether a roll is worth it.

export function enchantNames (raw, nameOf) {
  const list = Array.isArray(raw) ? raw.map(e => ({ name: e.name, level: e.lvl })) : raw?.enchantments?.map(e => ({ name: nameOf(e.id) ?? `enchantment#${e.id}`, level: e.level }))
  return list?.length ? list.map(e => `${e.name} ${e.level}`).join(', ') : undefined
}

export const VILLAGER_ENCHANTS = 'aqua_affinity bane_of_arthropods binding_curse blast_protection breach channeling density depth_strider efficiency feather_falling fire_aspect fire_protection flame fortune frost_walker impaling infinity knockback looting loyalty luck_of_the_sea lure mending multishot piercing power projectile_protection protection punch quick_charge respiration riptide sharpness silk_touch smite soul_speed sweeping_edge swift_sneak thorns unbreaking vanishing_curse wind_burst'.split(' ')
const editDistance = (a, b) => {
  let row = [...Array(b.length + 1)].map((_, i) => i)
  for (let i = 1; i <= a.length; i++) {
    const next = [i]
    for (let j = 1; j <= b.length; j++) next[j] = Math.min(next[j - 1] + 1, row[j] + 1, row[j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1))
    row = next
  }
  return row[b.length]
}

export function parseWant (text) {
  if (!text || typeof text !== 'string') return 'want= needs an enchantment, for example mending or sharpness:5'
  const parts = text.split(',').map(s => s.trim())
  const result = []
  for (const part of parts) {
    const match = /^([a-z_ -]+?)(?::([1-9]\d*)(\+)?)?$/i.exec(part)
    if (!match) return `invalid want=${part}: use enchant[:level[+]]`
    const enchant = match[1].toLowerCase().replace(/[ -]+/g, '_')
    if (!VILLAGER_ENCHANTS.includes(enchant)) return `unknown enchantment ${enchant}; did you mean ${[...VILLAGER_ENCHANTS].sort((a, b) => editDistance(enchant, a) - editDistance(enchant, b))[0]}?`
    result.push({ enchant, level: match[2] ? Number(match[2]) : null, atLeast: Boolean(match[3]) })
  }
  return result
}

const offerInputs = offer => [offer?.inputItem1, offer?.inputItem2].filter(i => i?.name && i.count > 0)
export const offerCost = offer => Object.fromEntries(offerInputs(offer).map(i => [i.name, (offerInputs(offer).filter(j => j.name === i.name).reduce((n, j) => n + (j === offer.inputItem1 ? (offer.realPrice ?? j.count) : j.count), 0))]))

export function bookOffer (offer, index = 1) {
  if (offer?.outputItem?.name !== 'enchanted_book') return null
  const raw = offer.outputItem.enchants
  const list = Array.isArray(raw) ? raw : raw?.enchantments ?? []
  const first = list[0]
  if (!first) return null
  const enchant = String(first.name ?? first.id ?? '').replace(/^minecraft:/, '').toLowerCase()
  const level = Number(first.lvl ?? first.level)
  const price = offerCost(offer).emerald ?? 0
  return enchant && Number.isFinite(level) ? { enchant, level, price, index } : null
}

export function rollVerdict (offers, wants, maxPrice = 64) {
  const books = offers.map((offer, n) => offer.tradeDisabled ? null : bookOffer(offer, n + 1)).filter(Boolean)
  const same = books.filter(b => wants.some(w => w.enchant === b.enchant))
  const matches = same.filter(b => b.price <= maxPrice && wants.some(w => w.enchant === b.enchant && (w.level == null || (w.atLeast ? b.level >= w.level : b.level === w.level))))
  const nearest = b => Math.min(...wants.filter(w => w.enchant === b.enchant).map(w => Math.max(0, w.level == null ? 0 : w.atLeast ? w.level - b.level : Math.abs(w.level - b.level)) * 100 + Math.max(0, b.price - maxPrice)))
  return { found: matches.sort((a, b) => a.price - b.price)[0] ?? null, best: same.sort((a, b) => nearest(a) - nearest(b) || a.price - b.price)[0] ?? null }
}

// Spend paper or another cheap input before emeralds or books when one trade will lock the profession.
export function cheapestLockOffer (offers, carried) {
  const cost = offer => offerCost(offer)
  const affordable = offer => !offer.tradeDisabled && !(Number.isFinite(offer.maximumNbTradeUses) && Number(offer.nbTradeUses ?? 0) >= offer.maximumNbTradeUses) && Object.entries(cost(offer)).every(([item, n]) => (carried[item] ?? 0) >= n)
  const score = offer => { const c = cost(offer); return (c.emerald ?? 0) * 10000 + (c.book ?? 0) * 1000 + Object.values(c).reduce((n, x) => n + x, 0) }
  return offers.filter(affordable).sort((a, b) => score(a) - score(b))[0] ?? null
}

export function matchesVillagerOutput (offer, { output, wants, enchant, level, atLeast = false } = {}, { includeDisabled = false } = {}) {
  if (!offer || (!includeDisabled && offer.tradeDisabled)) return false
  if (wants?.length) {
    const book = bookOffer(offer)
    return Boolean(book && wants.some(w => w.enchant === book.enchant && (w.level == null || (w.atLeast ? book.level >= w.level : book.level === w.level))))
  }
  if (output && offer.outputItem?.name?.replace(/^minecraft:/, '') !== output.replace(/^minecraft:/, '')) return false
  if (enchant) {
    const found = (Array.isArray(offer.outputItem?.enchants) ? offer.outputItem.enchants : offer.outputItem?.enchants?.enchantments ?? []).some(e => String(e.name ?? e.id ?? '').replace(/^minecraft:/, '').toLowerCase() === enchant && (level == null || (atLeast ? Number(e.lvl ?? e.level) >= level : Number(e.lvl ?? e.level) === level)))
    if (!found) return false
  }
  return Boolean(offer.outputItem?.name)
}

export function tradeLine (offer, i) {
  const item = n => `${n.count} ${n.name}`
  const inputs = offerInputs(offer).map((n, j) => item(j === 0 ? { ...n, count: offer.realPrice ?? n.count } : n)).join(' + ')
  const output = offer.outputItem ? item(offer.outputItem) : 'nothing'
  const book = bookOffer(offer, i)
  return `${i}) ${inputs} -> ${output}${book ? ` ${book.enchant} ${book.level}` : ''} (uses ${offer.nbTradeUses ?? 0}/${offer.maximumNbTradeUses ?? '?'})${offer.tradeDisabled ? ' disabled' : ''}`
}

export function rollRefusal ({ villagers, allowCrowd = false, profession, adult, nitwit, day, carried, placed = false, block = 'lectern', spareBlocks = 0, buy = false, maxPrice = 64 }) {
  if (villagers !== 1 && !(allowCrowd && villagers > 0)) return `${villagers} villagers within 8 blocks: isolate exactly one before rolling, or use pen=true with id= for the one to attract`
  if (!adult) return 'this villager is a baby and cannot take a profession'
  if (nitwit) return 'this villager is a nitwit and cannot take a profession'
  const job = JOB_BLOCK_PROFESSION[block]
  if (!job) return `unsupported job block ${block}`
  if (profession && profession !== 'unemployed' && profession !== job) return `it is a ${profession}: remove its job block or use a fresh villager`
  if (!day) return 'villagers take jobs while awake by day: try again after dawn'
  if (!placed && !carried?.[block]) return `need one ${block} to place the job block`
  if (buy && !((carried.emerald ?? 0) >= maxPrice && (carried.book ?? 0) >= 1) && (carried.paper ?? 0) < 64) return `buy=true needs ${maxPrice} emerald and 1 book, or 64 paper for the cheaper paper trade, before rolling`
  if (spareBlocks) return `${spareBlocks} other ${block} within 16 blocks: the villager may claim the wrong one`
  return null
}

export const JOB_BLOCK_PROFESSION = Object.freeze({
  blast_furnace: 'armorer', smoker: 'butcher', cartography_table: 'cartographer', brewing_stand: 'cleric',
  composter: 'farmer', barrel: 'fisherman', fletching_table: 'fletcher', cauldron: 'leatherworker',
  lectern: 'librarian', stonecutter: 'mason', loom: 'shepherd', smithing_table: 'toolsmith', grindstone: 'weaponsmith'
})

// Two walkable cells lead from the far doorway to the lectern. Paper assigns a job
// when the villager is within two blocks of the POI center, so every outside cell
// close enough to claim it is walled off. The only route into that radius is the
// doorway; a one-high sight tunnel lets the bot tend the lectern from outside.
export function villagerPenPlan (cell, villager) {
  const dx = villager.x - cell.x, dz = villager.z - cell.z
  const dir = Math.abs(dx) >= Math.abs(dz) ? { x: Math.sign(dx) || 1, z: 0 } : { x: 0, z: Math.sign(dz) || 1 }
  const side = { x: -dir.z, z: dir.x }
  const at = (d, t, y = cell.y) => ({ x: cell.x + d * dir.x + t * side.x, y, z: cell.z + d * dir.z + t * side.z })
  const center = at(1, 0)
  const interior = [center, at(2, 0)]
  const gate = at(3, 0)
  const outside = at(4, 0)
  const walls = []
  for (const y of [cell.y, cell.y + 1, cell.y + 2]) for (let d = 0; d <= 3; d++) for (const t of [-1, 0, 1]) {
    if (t === 0 && (d === 1 || d === 2 || d === 3)) continue
    if (d === 0 && t === 0 && y === cell.y) continue
    walls.push(at(d, t, y))
  }
  const guardBases = [[-2, -1], [-2, 1], [-1, -2], [-1, -1], [-1, 1], [-1, 2], [0, -2], [0, 2], [1, -2], [1, 2]].map(([d, t]) => at(d, t))
  const guard = guardBases.flatMap(p => [p, { ...p, y: p.y + 1 }, { ...p, y: p.y + 2 }])
  const service = [at(-1, 0), at(-1, 0, cell.y + 2), at(-2, 0), at(-2, 0, cell.y + 2)]
  const serviceStand = at(-3, 0)
  return { center, interior, gate, outside, walls, guard, service, serviceStand, dir, floor: [...interior, gate, serviceStand, ...walls.filter(p => p.y === cell.y), ...guardBases, at(-1, 0), at(-2, 0)] }
}
