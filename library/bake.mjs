// The second half of a farmer's day: farm.maintain empties the harvest into the store chest, and bake turns its wheat
// into bread at the crafting table beside it and puts the bread back, so the store's bread count is the goal a
// routine's until= reads.
const RANGE = 32
// three stacks of wheat in, one of bread out: room in any working inventory
const PER_ROUND = 192
const cellOf = text => {
  const n = String(text).split(',').map(Number)
  return n.length === 3 && n.every(Number.isInteger) ? { x: n[0], y: n[1], z: n[2] } : null
}

export default {
  doc: 'bake store=x,y,z [keep=16]: take the wheat out of the store chest, craft every 3 into bread at the nearest crafting table within 32 blocks of the store, put the bread back and carry keep= loaves (the rest deposited, a shortfall taken from the store); says baked=, deposited=, store_bread= and bread_carried=',
  stops: 'no whole bread left in the wheat, or no crafting table within 32 blocks of the store (said as attention=, nothing taken out)',
  args: { store: 'string!', keep: 'number' },

  async run (api, a) {
    const store = cellOf(a.store)
    if (!store) throw new Error(`bake: store=${a.store} is not x,y,z`)
    const at = `${store.x},${store.y},${store.z}`
    const keep = a.keep ?? 16
    const { positions = [] } = await api.act('find_blocks', { block: 'crafting_table', maxDistance: RANGE, count: 1, ...store })
    const table = positions[0]
    if (!table) return { attention: `no crafting table within ${RANGE} blocks of the store at ${at}: place one beside it`, store: at }
    const carried = name => api.inv()[name] ?? 0
    const { items = {} } = await api.act('chest_contents', store)
    let stock = items.wheat ?? 0
    let storeBread = items.bread ?? 0
    let baked = 0
    let deposited = 0
    for (;;) {
      // only whole loaves' worth comes out: one or two left over stay in the store for tomorrow's harvest
      const reach = Math.min(stock, PER_ROUND - carried('wheat'))
      const take = Math.max(0, Math.floor((carried('wheat') + reach) / 3) * 3 - carried('wheat'))
      if (take) { await api.act('withdraw', { items: { wheat: take }, ...store }); stock -= take }
      const batches = Math.floor(carried('wheat') / 3)
      if (!batches) break
      await api.act('goto', { x: table.x, y: table.y, z: table.z, range: 2 })
      const before = carried('bread')
      await api.act('craft', { item: 'bread', count: batches })
      baked += carried('bread') - before
      const spare = carried('bread') - keep
      if (spare > 0) {
        await api.act('deposit', { items: { bread: spare }, ...store })
        deposited += spare
        storeBread += spare
      }
      await api.checkpoint()
    }
    const short = Math.min(keep - carried('bread'), storeBread)
    if (short > 0) {
      await api.act('withdraw', { items: { bread: short }, ...store })
      storeBread -= short
    }
    return { baked, deposited, store_bread: storeBread, bread_carried: carried('bread'), store: at }
  }
}
