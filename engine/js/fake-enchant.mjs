// The fake's `enchant` primitive: an enchanting table's three offers and enchanting with one of them. Mirrors
// js/enchant.mjs. A table's offers come from its bookshelves (none: 2/3/5 levels, fifteen: 10/20/30) unless a spec gives
// them: `enchantTables: {"x,y,z": {shelves, offers: [l, l, l], hints: [[name, level] | null, ...], busy}}`.
const REACH = 4.5
const SLOTS = 36
const LAPIS = 'lapis_lazuli'
const RESULT = { book: 'enchanted_book' }
const ENCHANTABLE = /_(sword|pickaxe|axe|shovel|hoe|helmet|chestplate|leggings|boots)$|^(bow|crossbow|fishing_rod|trident|shears|book|elytra|shield|mace)$/
const GIVES = [[/_sword$|_axe$/, 'sharpness'], [/_(pickaxe|shovel|hoe)$/, 'efficiency'], [/_(helmet|chestplate|leggings|boots)$/, 'protection'], [/^bow$/, 'power']]

const key = ({ x, y, z }) => `${x},${y},${z}`
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const badArgs = message => Object.assign(new Error(message), { code: 'bad-args', badArgs: true })
const plain = i => (i.enchants ?? []).length === 0
const sig = i => JSON.stringify([i.name, (i.enchants ?? []).map(e => `${e.name}:${e.level}`).sort()])

function check (a) {
  if (!a || typeof a.pos?.x !== 'number') throw badArgs('enchant needs pos {x, y, z}')
  if (!['offers', 'enchant'].includes(a.op)) throw badArgs('enchant op must be offers or enchant')
  if (typeof a.item !== 'string' || a.item === '') throw badArgs('enchant needs item, a name')
  if (a.op === 'enchant' && !(Number.isInteger(a.choice) && a.choice >= 0 && a.choice <= 2)) throw badArgs('enchant needs choice 0, 1 or 2')
  if (a.levelCost !== undefined && !Number.isInteger(a.levelCost)) throw badArgs('enchant levelCost must be an integer')
}

const carried = (s, name) => s.inventory.filter(i => i.name === name).reduce((n, i) => n + i.count, 0)

function take (s, name, count) {
  let owed = count
  for (const stack of s.inventory.filter(i => i.name === name)) {
    const n = Math.min(owed, stack.count)
    stack.count -= n
    owed -= n
  }
  s.inventory = s.inventory.filter(i => i.count > 0)
}

const levelsFor = (table, item) => {
  if (!ENCHANTABLE.test(item)) return [0, 0, 0]
  if (table.offers) return table.offers
  const top = Math.max(5, 2 * (table.shelves ?? 0))
  return [Math.max(1, Math.round(top / 3)), Math.round(top * 2 / 3), top]
}

const hintsFor = (table, item, levels) => levels.map((cost, i) => {
  if (cost <= 0) return null
  if (table.hints) return table.hints[i] ? { enchant: table.hints[i][0], level: table.hints[i][1] } : null
  const name = GIVES.find(([re]) => re.test(item))?.[1] ?? 'unbreaking'
  return { enchant: name, level: Math.max(1, Math.ceil(cost / 8)) }
})

const enchantsFor = (item, cost) => {
  const name = GIVES.find(([re]) => re.test(item))?.[1] ?? 'unbreaking'
  return [{ name, level: Math.max(1, Math.ceil(cost / 8)) }, ...(cost >= 15 ? [{ name: 'unbreaking', level: Math.ceil(cost / 10) }] : [])]
}

function enchantOne (s, a, cost) {
  const stack = s.inventory.find(i => i.name === a.item && plain(i))
  const made = { name: RESULT[a.item] ?? a.item, count: 1, enchants: enchantsFor(a.item, cost) }
  stack.count--
  if (stack.count === 0) s.inventory.splice(s.inventory.indexOf(stack), 1)
  s.inventory.push(made)
  return made
}

export const fakeEnchant = (s, spec = {}) => {
  const tables = spec.enchantTables ?? {}
  return async (token, a) => {
    check(a)
    const block = s.blocks.get(key(a.pos))
    if (!block || block === 'air') return { status: 'missing' }
    if (block !== 'enchanting_table') return { status: 'cannot', reason: 'not-a-table' }
    const distance = dist(s.self.pos, a.pos)
    if (distance > REACH) return { status: 'unreachable', reason: 'too-far', distance: Math.round(distance * 100) / 100 }
    const mine = s.inventory.filter(i => i.name === a.item)
    if (mine.length === 0) return { status: 'no-item', item: a.item }
    if (!mine.some(plain)) return { status: 'cannot', reason: 'already-enchanted', item: a.item }
    const table = tables[key(a.pos)] ?? {}
    if (table.busy) return { status: 'failed', reason: 'window-did-not-open' }
    const xpLevel = s.self.experience.level
    const levels = levelsFor(table, a.item)
    const hints = hintsFor(table, a.item, levels)
    const offers = levels.map((levelCost, index) => ({ index, levelCost, lapisCost: index + 1, hint: hints[index] }))
    if (a.op === 'offers') return { status: 'ok', item: a.item, xpLevel, lapis: carried(s, LAPIS), offers }
    if (carried(s, LAPIS) < a.choice + 1) return { status: 'no-lapis', have: carried(s, LAPIS), need: a.choice + 1 }
    if (levels.every(l => l <= 0)) return { status: 'cannot', reason: 'not-enchantable' }
    const chosen = offers[a.choice]
    if (chosen.levelCost <= 0) return { status: 'cannot', reason: 'no-such-offer', offers: levels }
    if (a.levelCost !== undefined && a.levelCost !== chosen.levelCost) return { status: 'cannot', reason: 'offer-changed', offers: levels }
    const need = Math.max(chosen.levelCost, chosen.lapisCost)
    if (xpLevel < need) return { status: 'no-levels', need, have: xpLevel }
    const done = enchantOne(s, a, chosen.levelCost)
    take(s, LAPIS, chosen.lapisCost)
    s.self.experience.level = xpLevel - chosen.lapisCost
    return { status: 'enchanted', item: a.item, choice: a.choice, enchants: done.enchants, lapisSpent: chosen.lapisCost, levelsSpent: chosen.lapisCost, xpLevel: s.self.experience.level }
  }
}
