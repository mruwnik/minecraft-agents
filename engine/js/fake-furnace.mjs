// The fake's `furnace` primitive and its cooking: furnace, blast furnace and smoker with the real rates (a furnace takes
// 200 ticks an item, the other two 100), fuel that burns for its real time and one stack of input, fuel and output.
// Time passes only when world.advance(ticks) says so. Results have the shape of engine/js/furnace.mjs.
const KINDS = /^(furnace|blast_furnace|smoker)$/
const REACH = 4.5
const SLOT_MAX = 64
const INVENTORY_SLOTS = 36

const ORES = { raw_iron: 'iron_ingot', raw_gold: 'gold_ingot', raw_copper: 'copper_ingot', iron_ore: 'iron_ingot', gold_ore: 'gold_ingot', copper_ore: 'copper_ingot' }
const FOODS = { beef: 'cooked_beef', porkchop: 'cooked_porkchop', chicken: 'cooked_chicken', mutton: 'cooked_mutton', rabbit: 'cooked_rabbit', cod: 'cooked_cod', salmon: 'cooked_salmon', potato: 'baked_potato', kelp: 'dried_kelp' }
const BLOCKS = { cobblestone: 'stone', sand: 'glass', clay_ball: 'brick', oak_log: 'charcoal' }
const RECIPES = {
  furnace: { ...ORES, ...FOODS, ...BLOCKS },
  blast_furnace: ORES,
  smoker: FOODS
}
const COOK_TICKS = { furnace: 200, blast_furnace: 100, smoker: 100 }
// a blast furnace and a smoker burn fuel twice as fast as they cook twice as fast: an item costs the same fuel in all three
const BURN_RATE = { furnace: 1, blast_furnace: 2, smoker: 2 }
const FUEL_TICKS = { coal: 1600, charcoal: 1600, coal_block: 16000, blaze_rod: 2400, oak_planks: 300, oak_log: 300, stick: 100 }

const key = ({ x, y, z }) => `${x},${y},${z}`
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const stackOf = i => (i ? { name: i.name, count: i.count } : null)
const emptyFurnace = () => ({ input: null, fuel: null, output: null, burn: 0, burnTotal: 0, cook: 0 })

const setLit = (s, k, lit) => s.states.set(k, { ...(s.states.get(k) ?? {}), lit })

// one tick of vanilla's furnace: burn down, light from the fuel slot when there is something to cook, cook, finish an item
function tick (s, k, kind, f) {
  const recipe = RECIPES[kind][f.input?.name]
  const room = recipe && (!f.output || (f.output.name === recipe && f.output.count < SLOT_MAX))
  if (f.burn > 0) f.burn--
  if (f.burn === 0 && room && f.fuel) {
    f.burn = f.burnTotal = FUEL_TICKS[f.fuel.name] / BURN_RATE[kind]
    f.fuel.count--
    if (f.fuel.count === 0) f.fuel = null
  }
  if (f.burn > 0 && room) {
    f.cook++
    if (f.cook >= COOK_TICKS[kind]) {
      f.cook = 0
      f.output = { name: recipe, count: (f.output?.count ?? 0) + 1 }
      f.input.count--
      if (f.input.count === 0) f.input = null
    }
  } else if (!room) {
    f.cook = 0
  }
  setLit(s, k, f.burn > 0)
}

export function advanceFurnaces (s, ticks) {
  for (const [k, f] of s.furnaces) {
    const kind = s.blocks.get(k)
    if (!KINDS.test(kind)) { s.furnaces.delete(k); continue }
    for (let i = 0; i < ticks; i++) tick(s, k, kind, f)
  }
}

const slotsOf = (kind, f) => ({
  kind,
  input: stackOf(f.input),
  fuel: stackOf(f.fuel),
  output: stackOf(f.output),
  lit: f.burn > 0,
  burn: { left: f.burn, total: f.burnTotal },
  cook: { done: f.cook, total: COOK_TICKS[kind] }
})

const carried = (s, name) => s.inventory.filter(i => i.name === name).reduce((sum, i) => sum + i.count, 0)
const hasRoom = (s, name) => s.inventory.some(i => i.name === name) || s.inventory.length < INVENTORY_SLOTS

function removeFromPockets (s, name, count) {
  for (let left = count; left > 0;) {
    const stack = s.inventory.find(i => i.name === name)
    const n = Math.min(left, stack.count)
    stack.count -= n
    left -= n
    if (stack.count === 0) s.inventory.splice(s.inventory.indexOf(stack), 1)
  }
}

function addToPockets (s, name, count) {
  const stack = s.inventory.find(i => i.name === name)
  if (stack) stack.count += count
  else s.inventory.push({ name, count })
}

// as on the server: the input slot takes anything (a smoker holds iron and never cooks it), the fuel slot only fuel
const accepted = (kind, slot, item) => slot === 'input' || item in FUEL_TICKS

function load (s, kind, f, a) {
  const wanted = ['input', 'fuel'].filter(slot => a[slot]).map(slot => [slot, a[slot]])
  for (const [slot, { item }] of wanted) {
    if (carried(s, item) === 0) return { status: 'no-item', slot, item }
    if (f[slot] && f[slot].name !== item) return { status: 'busy', slot, holds: stackOf(f[slot]) }
  }
  const moved = {}
  for (const [slot, { item, count }] of wanted) {
    const held = f[slot]?.count ?? 0
    const n = accepted(kind, slot, item) ? Math.min(count ?? Infinity, carried(s, item), SLOT_MAX - held) : 0
    if (n === 0) return { status: 'rejected', slot, item, reason: held >= SLOT_MAX && accepted(kind, slot, item) ? 'slot-full' : 'not-accepted' }
    removeFromPockets(s, item, n)
    f[slot] = { name: item, count: held + n }
    moved[slot] = n
  }
  return { status: 'ok', moved, ...slotsOf(kind, f) }
}

function take (s, kind, f, a) {
  const parts = ['output', 'input', 'fuel'].filter(slot => (slot === 'output' ? a.output !== false : a[slot]))
  const taken = []
  let stuck = false
  for (const part of parts.filter(p => f[p])) {
    const held = f[part]
    if (!hasRoom(s, held.name)) { stuck = true; continue }
    addToPockets(s, held.name, held.count)
    taken.push({ part, name: held.name, count: held.count })
    f[part] = null
  }
  return { status: stuck ? 'full' : 'ok', taken, ...slotsOf(kind, f) }
}

export const fakeFurnace = (s, spec = {}) => {
  s.furnaces = new Map(Object.entries(spec.furnaces ?? {}).map(([k, f]) => [k, { ...emptyFurnace(), ...structuredClone(f) }]))
  for (const [k, name] of s.blocks) if (KINDS.test(name)) setLit(s, k, false)
  return async (token, a) => {
    const block = s.blocks.get(key(a.pos))
    if (!block || block === 'air') return { status: 'missing' }
    if (!KINDS.test(block)) return { status: 'cannot', reason: 'not-a-furnace' }
    const distance = dist(s.self.pos, a.pos)
    if (distance > REACH) return { status: 'unreachable', reason: 'too-far', distance: Math.round(distance * 100) / 100 }
    const k = key(a.pos)
    if (!s.furnaces.has(k)) s.furnaces.set(k, emptyFurnace())
    const f = s.furnaces.get(k)
    if (a.op === 'read') return { status: 'ok', ...slotsOf(block, f) }
    return (a.op === 'load' ? load : take)(s, block, f, a)
  }
}
