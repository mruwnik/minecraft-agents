// What a lead says when it gives up fetching an animal. The old answer blamed the animal every time ("will not
// follow: is there a fence or water between you?"), and three times in one afternoon the evidence said the BODY had
// never moved: flock.lead beside a wheat field's fence stalled pressing forward at the same cell with a found path,
// then fetched three times and gave up (card fc47bf28). The body's frozen_walk event (src/stall.mjs) already names
// what froze it; a stall seen since the fetches began makes the failure the walk's own, and the answer says so.

// the frozen walk that belongs to these fetches: one seen before they began is some earlier walk's
export const stalledSince = (frozen, sinceMs) => frozen && frozen.at >= sinceMs ? frozen : null

export const fetchFailure = ({ mob, frozen }) => {
  if (!frozen) return `the ${mob} will not follow (fetched it 3 times, got no nearer): is there a fence or water between you? Get them out in the open first, or lead fewer`
  const where = `${frozen.pos.x},${frozen.pos.y},${frozen.pos.z}`
  return `I could not walk to the ${mob} (stalled at ${where} pressing forward)${frozen.advice ? `: ${frozen.advice}` : ''}`
}

// An animal that stands fenced in is not fetched at all. The walk into its pen used to fail quietly and the fetch loop
// then walked three times to the nearest reachable cell outside the fence and blamed the animal. `entered` is whether
// the walk to its cell arrived; `pen` is penAround's answer for that cell (enclosed pens only; a leaking one is open
// country to a walk). The pen check names the gates, so the answer points at it rather than repeat its work.
// the body's own feet on the pen floor (x and z of a floor cell, within a step of its height: a slab floor is half a block up)
const standsIn = (pen, feet) => pen.floor.some(key => {
  const [x, y, z] = key.split(',').map(Number)
  return x === Math.floor(feet.x) && z === Math.floor(feet.z) && Math.abs(y - feet.y) <= 1
})
// an animal fenced in, judged before any walk: a walk into an enclosed pen never fails cleanly (the pathfinder
// follows partial paths round the fence until the 12 s stall alarm cancels the task), so the pen and the body's
// feet decide. pen is penAround's answer for the animal's cell (null when that cell is no spot to stand on)
export const fencedRefusal = ({ mob, at, pen, feet }) => {
  if (!pen?.enclosed || standsIn(pen, feet)) return null
  const [x, y, z] = String(at).split(',')
  return `the ${mob} at ${at} stands fenced in (a ${pen.cells}-cell pen) and I found no way in: open a gate or a fence post beside it (pen.check x=${x} y=${y} z=${z} names its gates), or lead from inside`
}

// the block an animal stands IN, when its collision rises above the feet: the fence post of card fc47bf28's two cows
// (both floored to the fence's own cell, so any centre there overlaps the post's box). A slab or carpet under the
// feet lifts them to its own top, so it is what the animal stands ON and no wedge. Ground cover has no box at all
export const wedgedIn = (block, feetY) => {
  const shapes = block?.shapes ?? []
  if (!shapes.length) return null
  const top = Math.max(...shapes.map(shape => shape[4]))
  return top > feetY - Math.floor(feetY) + 0.05 ? block.name : null
}
// an animal that cannot walk cannot be led: said before the walk, never after three fetches
export const wedgedRefusal = ({ mob, at, block }) => {
  if (!block) return null
  const [x, y, z] = String(at).split(',')
  return `the ${mob} at ${at} stands wedged in a ${block} and cannot walk: free it (dig x=${x} y=${y} z=${z}, if that ${block.replace(/^.*_/, '')} is yours to break) or lead another`
}
