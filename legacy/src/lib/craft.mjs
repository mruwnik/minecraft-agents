// Crafting: what a recipe is short of, room in the grid, and what is left over after a craft.

// Which of an enchanting table's three offers to take: offers[n].level is the xp level slot n+1 asks for (-1: none), and it also needs n+1 lapis.
// `wanted` is the slot number 1-3; without it the dearest I can pay for
export function enchantChoice (offers, xp, lapis, wanted) {
  const listed = offers.map((o, n) => `${n + 1}=level ${o.level}`).join(', ')
  if (offers.every(o => !(o.level > 0))) return { error: 'the table offers nothing for this item: it cannot be enchanted here (already enchanted, or not enchantable)' }
  const affordable = n => offers[n]?.level > 0 && offers[n].level <= xp && lapis >= n + 1
  if (wanted !== undefined) return affordable(wanted - 1) ? { choice: wanted - 1 } : { error: `slot ${wanted} needs xp level ${offers[wanted - 1]?.level} and ${wanted} lapis_lazuli: you have level ${xp} and ${lapis} lapis. Offers: ${listed}` }
  const best = [2, 1, 0].find(affordable)
  return best === undefined ? { error: `nothing affordable: you have xp level ${xp} and ${lapis} lapis_lazuli. Offers: ${listed} (slot n also needs n lapis). Gain xp by mining ore, smelting, breeding or fighting` } : { choice: best }
}

// what a craft is really short of. mineflayer names one arbitrary member of an item tag: "needs cherry_planks:2 stick:1" to someone
// holding 5 oak planks and no stick (four agents chased that). Take the recipe nearest to what is carried, tell only what is missing,
// and call an ingredient that other recipes swap for a cousin by its family name
export function craftShortfall (recipes, have) {
  const missing = r => Object.entries(r).map(([name, n]) => [name, n - (have[name] ?? 0)]).filter(([, n]) => n > 0)
  const total = r => missing(r).reduce((sum, [, n]) => sum + n, 0)
  const best = recipes.reduce((a, b) => total(b) < total(a) ? b : a)
  const swapsFor = name => [...new Set(recipes.filter(r => !(name in r)).flatMap(r => Object.keys(r).filter(n => !(n in best))))]
  const label = (name, n) => {
    const cousins = swapsFor(name)
    const family = name.split('_').pop()
    if (!cousins.length) return `${name}:${n}`
    return name.includes('_') && cousins.every(c => c.split('_').pop() === family) ? `any ${family}:${n}` : `${name}:${n} (or ${cousins.slice(0, 2).join(', ')})`
  }
  return missing(best).map(([name, n]) => label(name, n)).join(' ')
}

// stacks: the sizes of the stacks of this item I already carry; batch: how many one craft makes
// made/count: asked again before every batch, because the stack that had room at the start fills up on the way
export const craftRoom = ({ freeSlots, stacks, stackSize, batch, item, made = 0, count }) => freeSlots > 0 || stacks.some(n => n + batch <= stackSize)
  ? null
  : `${made ? `${made}/${count} made, then ` : ''}your inventory is full and ${item} has nowhere to go (the craft would eat the ingredients and drop or lose the result): toss or deposit something first`

// A craft counts its result from the LOCAL inventory, and the server's answer to the click can land long after the
// loop has decided the batch failed. So "0/1 made" was said twice over things that had gone very differently: a
// stone_axe craft that consumed nothing, where a plain retry made the axe; and a shears craft that ate two iron
// ingots and never gave them back (backlog #133). One is free to retry and one is a real loss, and a message that
// cannot tell them apart leaves the driver to find out by counting their own pockets. So the ingredients are read
// back too, and what the message says about them is what the inventory actually shows.
// fell: ingredients found lying within reach after the failure (and swept up); lying: those still on the ground after
// the sweep, as name@x,y,z. Chani's "real" loss of 16 planks was a stack on the ground that farm.maintain picked up later
// What a 2x2 craft left behind where the pockets count never looks: the four grid cells and the cursor. A put-back click
// the server rejected leaves the ingredient stack there (my stick craft: "-bamboo:26" for one stick, the 24 back later)
export const gridLeftovers = ({ grid, cursor }) => [...grid, cursor].filter(Boolean)
  .reduce((acc, { name, count }) => ({ ...acc, [name]: (acc[name] ?? 0) + count }), {})

export function craftReport ({ item, count, made, spent = {}, why, fell = [], lying = [] }) {
  if (made >= count) return { crafted: item, made }
  const used = Object.entries(spent).filter(([, n]) => n > 0).map(([name, n]) => `${name}:${n}`).join(' ')
  if (made <= 0 && fell.length) {
    const names = [...new Set(fell)].join(', ')
    if (lying.length && used) return { error: `${why}: no ${item} made at all; the ingredients (${used}) are not in my pockets but on the ground: ${lying.join(' ')}. Collect them (./mc collect) before retrying` }
    if (!used) return { error: `${why}: no ${item} made at all; the ${names} fell out of the crafting grid onto the ground and I picked them back up, so nothing is lost: retry once` }
    return { error: `${why}: no ${item} made at all; the ${names} fell out of the crafting grid and I picked up what lay within reach, but ${used} is still missing: that part of the loss is real. Check your inventory before retrying` }
  }
  // ingredients consumed alongside a result are not lost, they ARE the result: only a craft that made nothing at all
  // can claim its ingredients went for nothing, and saying otherwise sends the driver hunting a loss that never was
  if (made > 0) return { error: `${why}: only ${made} of ${count} ${item} made${used ? `, and ${used} went into them` : ''}` }
  return {
    error: `${why}: no ${item} made at all and ${used
      ? `the ingredients are gone (${used}): the server took them and gave nothing back, so this loss is real. Check your inventory before retrying`
      : 'nothing was consumed, so nothing is lost: retry once, the second call usually works'}`
  }
}
