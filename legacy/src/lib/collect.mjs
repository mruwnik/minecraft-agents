// Picking up drops: retrying until a count is met, and what is still lying on the ground.

// Call attempt(stillWanted) until `count` is gathered. attempt resolves to how many it got, or rejects (counted as
// none). Two empty rounds in a row, or maxRounds, end it: {got, rounds, gaveUp?: why}
export async function retryUntilCount (attempt, count, maxRounds = 6) {
  let got = 0
  let empty = 0
  let lastError = 'nothing gained'
  for (let rounds = 1; rounds <= maxRounds; rounds++) {
    const gained = await attempt(count - got).catch(e => { lastError = e.message; return 0 })
    got += gained
    empty = gained ? 0 : empty + 1
    if (got >= count) return { got, rounds }
    if (empty >= 2) return { got, rounds, gaveUp: lastError }
  }
  return { got, rounds: maxRounds, gaveUp: 'round limit' }
}

// `picked` used to count the drops the body WALKED TO, and walking to a drop is not picking it up: at a carrot no cell
// could stand beside, collect answered picked=1 twice over while the carrot lay in the dirt the whole time, and
// farm.harvest counted the same carrot as harvested because it had dug it (backlog #142). A drop is picked when it is
// no longer on the ground; nothing else counts. The ones still lying are the half of the answer worth having, so they
// are named where they lie, and split by which of the two things went wrong: the walk never arrived, or it arrived and
// the drop stayed put. Those need different fixes from the driver, so they must not share one sentence.
const SHOW = 3
const stillLyingList = drops => `${drops.length} still lying (${[...drops.slice(0, SHOW).map(d => `${d.item} at ${d.x},${d.y},${d.z}`), ...(drops.length > SHOW ? [`and ${drops.length - SHOW} more`] : [])].join(', ')})`
export function collectTally (tried, lying) {
  const left = new Set(lying.map(drop => drop.id))
  const stuck = tried.filter(drop => left.has(drop.id))
  const noWalk = stuck.filter(drop => !drop.reached)
  const noLift = stuck.filter(drop => drop.reached)
  return {
    picked: tried.length - stuck.length,
    ...(noWalk.length ? { couldNotReach: `${stillLyingList(noWalk)}: I could not walk there. Nothing can stand beside that cell: clear a way in, or stand above it and dig down` } : {}),
    ...(noLift.length ? { stillLying: `${stillLyingList(noLift)}: I stood on it and it did not come. Deposit or toss something, or it is stuck inside a block` } : {})
  }
}

// collect with a full inventory walks to drops it cannot pick up: tell the driver why they stayed
export const leftLying = (freeSlots, left, inDeepWater = []) => ({
  ...(freeSlots > 0 || !left.length ? {} : { inventoryFull: `left lying: ${[...new Set(left)].join(' ')}. Deposit or toss something first` }),
  ...(inDeepWater.length ? { inWater: `left in deep water: ${[...new Set(inDeepWater)].join(' ')}. Fetch it from a boat or the shore, or collect wet=true and watch your air` } : {})
})

// drops: {id, dist, deep}, deep = water with water under it, a swim. Each drop gets one try
export const nextDrop = (drops, tried, allowWet) =>
  drops.filter(d => !tried.has(d.id) && (allowWet || !d.deep)).sort((a, b) => a.dist - b.dist)[0]
