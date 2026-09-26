// Chests and furnaces: fuel choice, withdraw/deposit planning, and furnace/smelt status reporting.

// items smelted per unit of fuel, best first
const FUELS = [[/^(coal|charcoal)$/, 8], [/_(planks|log|wood)$/, 1.5], [/^stick$/, 0.5]]

// Which fuel to put in a furnace to smelt `count` items, from inventory `items` [{name,count}]: {name,count} or null
export function pickFuel (items, count) {
  for (const [re, perUnit] of FUELS) {
    const item = items.find(i => re.test(i.name))
    if (item) return { name: item.name, count: Math.min(item.count, Math.ceil(count / perUnit)) }
  }
  return null
}

// items= comes as [{name, count}] or, shorter, as {name: count}
const itemList = wanted => Array.isArray(wanted) ? wanted : Object.entries(wanted).map(([name, count]) => ({ name, count }))

// the chests' items= or, as every other action says it, item= (count=)
export const itemsArg = a => a.items ?? (a.item ? [{ name: a.item, count: a.count }] : undefined)

export function withdrawPlan (wanted, inChest) {
  if (!wanted) throw new Error(`withdraw needs items='{"coal":4}' or item=coal count=4`)
  // count 'all': whatever is there, and none of it is no shortfall
  const rows = itemList(wanted).map(w => ({ name: w.name, want: w.count === 'all' ? inChest[w.name] ?? 0 : w.count ?? inChest[w.name] ?? 1, have: inChest[w.name] ?? 0 }))
  return {
    take: rows.filter(r => r.have > 0).map(r => ({ name: r.name, count: Math.min(r.want, r.have) })),
    short: rows.filter(r => r.have < r.want).map(r => `${r.name}:${r.have}/${r.want}`)
  }
}

// what a deposit puts in: the named items, or everything only when asked for outright
export function depositWanted (a, carried) {
  if (itemsArg(a)) return itemsArg(a)
  if (a.all) return carried
  return { error: `deposit needs items='{"dirt":4}' (or all=true for everything you carry, tools included)` }
}

// what furnace_take says about what is left inside: a furnace with input and no fire never finishes, and used to look just like one that cooks
export function furnaceReport ({ input, fuel, burning }) {
  const stillCooking = input?.count ?? 0
  if (!stillCooking || burning) return { stillCooking }
  if (!fuel) return { stillCooking, stuck: `${input.name} is waiting but the fire is out and the fuel slot is empty: smelt fuel=coal count=${stillCooking} x= y= z= adds fuel only (no item= needed; charcoal, planks or logs work too)` }
  return { stillCooking, stuck: `${input.name} is waiting and ${fuel.name} is in the fuel slot, but nothing burns: that input cannot be smelted here, or the output slot holds something else. chest_contents shows the slots` }
}

// a smelt that waits by the furnace: 'night' = stop watching it, everybody else is waiting for me to go to bed
export const smeltWait = ({ got, wanted, night, timedOut }) => got >= wanted || timedOut ? 'done' : night ? 'night' : 'wait'

// chest transfers go wrong through ViaBackwards: one is lost, or a whole stack comes along (asked 8 wheat, got 24). Compare what arrived
// with the plan: `back` is what to return, `more` what to ask for once again
export function transferFix (plan, before, after) {
  const rows = plan.map(t => ({ name: t.name, off: (after[t.name] ?? 0) - (before[t.name] ?? 0) - t.count }))
  return {
    back: rows.filter(r => r.off > 0).map(r => ({ name: r.name, count: r.off })),
    more: rows.filter(r => r.off < 0).map(r => ({ name: r.name, count: -r.off }))
  }
}

// What a transfer actually did. The CHEST is the world, and what left it (or landed in it) is the verdict - never the
// inventory delta. A hungry body ate three of the eight loaves as they arrived, so the inventory was short by three
// through no fault of the transfer, and `withdraw` reported FAIL three rounds running while the chest had already
// given up all eight (#146). The meal is worth SAYING, so an agent that reads `eaten=bread:3` knows where its food
// went and does not withdraw again - but only where it explains the gap, and never for more than the meal took.
export function transferOutcome ({ way, take, chestBefore, chestAfter, invBefore = {}, invAfter = {}, eaten = {} }) {
  const fix = way === 'withdraw' ? transferFix(take, chestAfter, chestBefore) : transferFix(take, chestBefore, chestAfter)
  // what the inventory SHOULD have done: up by what was asked on the way in, down by it on the way out
  const wanted = way === 'withdraw' ? 1 : -1
  const missed = take
    .map(t => ({ name: t.name, short: (wanted * t.count) - ((invAfter[t.name] ?? 0) - (invBefore[t.name] ?? 0)) }))
    .filter(r => r.short > 0 && (eaten[r.name] ?? 0) > 0)
    .map(r => [r.name, Math.min(r.short, eaten[r.name])])
  return { ...fix, settled: !fix.back.length && !fix.more.length, eaten: missed.length ? Object.fromEntries(missed) : undefined }
}

// mineflayer says "destination full" for a full chest AND for full pockets: say which side
export const fullSide = (message, way) => !/destination full|inventory is full/i.test(message)
  ? message
  : way === 'withdraw'
    ? 'YOUR INVENTORY is full: what fitted was taken (the + above). deposit or toss something, then withdraw the rest'
    : 'the CHEST is full: what fitted went in (the - above). Put the rest in another chest, or take out what does not belong here'
