// Where a harvest goes, and what to do when it does not fit. Pure: the composites (farm.maintain, flock.maintain,
// apiary.maintain) resolve deposit= here and src/storage.mjs walks the chests. Until 09-26 the surplus went to the
// plan's one C chest, a full chest was a stuck= line and the body carried the harvest round for another day; now the
// plan's other chests (or a marked storage place's) take the overflow in a fixed order, and what nobody could take is
// said as storage_full=, which the stuck watch turns into an alert after two days (card 63e91e8f).

// the blocks a harvest is stored in, in the order find_blocks is asked for them
export const STORAGE_BLOCKS = ['chest', 'barrel', 'trapped_chest']
export const STORAGE_REACH = 12 // blocks round a storage mark its chests may stand
export const NEAREST_REACH = 32 // a round with no plan and deposit=true: the nearest chest within this

// items that stack by 16, not 64 (the produce a farm, pen or apiary makes; a bucket or tool never reaches a deposit)
const STACK_16 = new Set(['egg', 'honey_bottle', 'ender_pearl', 'snowball', 'sign', 'oak_sign', 'bucket', 'water_bucket'])
export const stackOf = name => STACK_16.has(name) ? 16 : 64

// deposit=: false stores nothing; left out or true means the plan's own chests; x,y,z is a chest cell (tried first, the
// plan's chests after it); any other word is a marked place (kind=storage by convention) whose chests are found there
export function depositTarget (arg, places) {
  if (arg === false) return null
  if (arg === undefined || arg === true) return { kind: 'plan' }
  const text = String(arg)
  const nums = text.split(',').map(Number)
  if (nums.length === 3 && nums.every(Number.isFinite)) return { kind: 'cell', x: nums[0], y: nums[1], z: nums[2] }
  const place = places.find(p => p.name === text)
  if (!place) return { error: `deposit=${text} is neither x,y,z nor a marked place: places lists them (mark name=<name> kind=storage beside the chests)` }
  return { kind: 'place', name: place.name, x: Math.floor(place.x), y: Math.floor(place.y), z: Math.floor(place.z) }
}

// every chest a plan marks, in the plan's own order (row by row), one above the ground the plan's y names
export const planChests = cells => cells.filter(c => c.ch === 'C').map(c => ({ x: c.x, y: c.y + 1, z: c.z }))

// a fixed order for the chests found round a mark: nearest the mark first, ties by x, then z, then y. The same chests
// are tried in the same order every day, so a reader knows which one fills first
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
export const chestOrder = (positions, from) => positions
  .map(p => ({ x: p.x, y: p.y, z: p.z }))
  .sort((a, b) => dist(a, from) - dist(b, from) || a.x - b.x || a.z - b.z || a.y - b.y)

const sameCell = (a, b) => a.x === b.x && a.y === b.y && a.z === b.z

// how much of `count` of one item a chest takes: the part-filled stack of it first, then whole free slots. A chest whose
// room is not known (no `free`) is asked for everything: the deposit itself says what fitted
const fit = ({ free, items }, name, count) => {
  if (free === undefined) return { n: count, slots: 0 }
  const stack = stackOf(name)
  const rem = (items?.[name] ?? 0) % stack
  const partial = rem ? stack - rem : 0
  const n = Math.min(count, partial + free * stack)
  return { n, slots: Math.max(0, Math.ceil((n - partial) / stack)) }
}

const positive = counts => Object.fromEntries(Object.entries(counts).filter(([, n]) => n > 0))

// Which chest takes what: the chests in the order given, each filled as far as its room is known before the next is
// asked. `drops` is one entry per chest that takes anything; `left` is what no chest can take
export function depositPlan (surplus, chests) {
  let left = positive(surplus)
  const drops = []
  for (const chest of chests) {
    if (!Object.keys(left).length) break
    let free = chest.free
    const items = {}
    for (const [name, count] of Object.entries(left)) {
      const { n, slots } = fit({ free, items: chest.items }, name, count)
      if (n <= 0) continue
      items[name] = n
      if (free !== undefined) free -= slots
    }
    left = positive(Object.fromEntries(Object.entries(left).map(([name, n]) => [name, n - (items[name] ?? 0)])))
    if (Object.keys(items).length) drops.push({ x: chest.x, y: chest.y, z: chest.z, items })
  }
  return { drops, left }
}

// what a deposit really moved: the pockets before and after are the verdict, never the deposit's answer (a full chest
// takes what fits and then fails; a meal eaten meanwhile is not a deposit either, so never more than was asked)
export const wentIn = (asked, before, after) => positive(Object.fromEntries(Object.entries(asked)
  .map(([name, n]) => [name, Math.min(n, (before[name] ?? 0) - (after[name] ?? 0))])))

export const minusCounts = (a, b) => positive(Object.fromEntries(Object.entries(a).map(([name, n]) => [name, n - (b[name] ?? 0)])))

// deposited=wheat:64@12,63,-80 wheat:16@13,63,-80: what went into which chest
export const depositLine = drops => drops
  .flatMap(d => Object.entries(d.items).map(([name, n]) => `${name}:${n}@${d.x},${d.y},${d.z}`))
  .join(' ') || null

// storage_full=wheat:40 carried: what no chest could take, still in the pockets
export const storageFullLine = (left, why = null) => Object.keys(left).length
  ? `${Object.entries(left).map(([name, n]) => `${name}:${n}`).join(' ')} carried${why ? ` (${why})` : ''}`
  : null

// the primitive's word for a chest that took what fitted and no more (src/lib/chests.mjs fullSide)
export const CHEST_FULL = /CHEST is full/

// the chests deposit= means, once found: the target's own first, then the plan's other chests
export const orderedChests = (target, cells, found = []) => {
  const own = target.kind === 'cell' ? [{ x: target.x, y: target.y, z: target.z }] : target.kind === 'plan' ? [] : found
  const rest = target.kind === 'place' || target.kind === 'nearest' ? [] : planChests(cells).filter(c => !own.some(o => sameCell(o, c)))
  return [...own, ...rest]
}
