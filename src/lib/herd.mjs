// Leading and herding: picking who to lead, whether the herd kept up, and a pen's census of strays.

import { within } from './world.mjs'
import { invited } from './places.mjs'

// leading animals with food in hand: they follow from up to 10 blocks and are slower than I am. distances = how far each one still with me is
// heldFor: seconds I have stood waiting. An animal that does not come (a fence between us) would keep me waiting for ever: go and get it,
// and after 3 fetches that brought me no nearer the goal, give up and tell the driver
export function leadVerdict ({ distances, holding, heldFor = 0, fetchesSinceProgress = 0, noPath = false }) {
  if (!distances.length) return 'lost'
  const farthest = Math.max(...distances)
  const fetch = farthest > 9 || (holding && heldFor >= 12 && farthest > 4)
  if (fetch) return fetchesSinceProgress >= 3 ? 'giveup' : 'fetch'
  if (farthest > (holding ? 4 : 6)) return 'hold'
  return noPath ? 'noway' : 'go'
}
// why an animal that was shown food stayed put. rises: for its four neighbour cells, how far up the first free standing room is (Infinity: none within reach)
export function pitAdvice (mob, at, rises) {
  if (rises.some(r => r <= 1)) return null
  if (rises.every(r => r === Infinity)) return `the ${mob} at ${at} is walled in on all four sides: open a side (dig), then lead again`
  return `the ${mob} at ${at} stands in a pit (every way out is ${Math.min(...rises)}+ blocks up, it jumps 1): give it a step (place a block beside it, or dig the rim down), then lead again`
}

// a lead must end on a spot to stand on: a marker set on the fence line ends the walk OUTSIDE the pen
export const leadTargetError = (to, block) => block?.solid ? `flock.lead: ${to.x},${to.y},${to.z} is inside a ${block.name}, not a spot to stand on: give a free floor cell INSIDE the pen (or mark the place again there)` : null

// floor: the "x,y,z" cells pen.check walked; animals: {name,x,y,z}. In = standing in a column of the pen, whatever the height
const onFloor = floor => {
  const columns = new Set(floor.map(k => k.split(',').filter((_, i) => i !== 1).join(',')))
  return p => columns.has(`${Math.floor(p.x)},${Math.floor(p.z)}`)
}
// the animals that are not already standing in the pen (floor = penLeak's floor cells, null when the goal is no pen)
export const unpenned = (floor, animals, posOf) => floor ? animals.filter(a => !onFloor(floor)(posOf(a))) : animals
// a pen that leaks (penLeak's via) through nothing but an open gate: where that gate is, so lead can shut it and see the pen
export function gateLeak (via, blockAt) {
  const spots = via.split(' ')
  if (spots.length !== 1) return null
  const [x, y, z] = spots[0].split(',').map(n => Math.floor(Number(n)))
  const block = blockAt(x, y, z)
  return block?.open && /_fence_gate$/.test(block.name) ? [x, y, z] : null
}
// where to stand in a pen so that animals following 2.5 blocks behind end up inside it: the floor cell furthest from them
export function deepestCell (floor, from) {
  const far = ([x, , z]) => (x + 0.5 - from.x) ** 2 + (z + 0.5 - from.z) ** 2
  return floor.map(k => k.split(',').map(Number)).sort((a, b) => far(b) - far(a))[0]
}
// farm animals that are NOT on the pen floor but within a few blocks of its gate: the ones that slipped out with whoever just walked through
export function strays (floor, animals, [gx, , gz], within = 6) {
  const out = animals.filter(a => !onFloor(floor)(a) && Math.hypot(a.x - gx - 0.5, a.z - gz - 0.5) <= within)
  return out.length ? out.map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ') : null
}
export function penCensus (floor, animals) {
  const isIn = onFloor(floor)
  const counts = animals.filter(isIn).reduce((n, a) => ({ ...n, [a.name]: (n[a.name] ?? 0) + 1 }), {})
  const inside = Object.entries(counts).map(([name, n]) => `${name}:${n}`).join(' ')
  const outside = animals.filter(a => !isIn(a)).map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ')
  return { ...(inside ? { inside } : {}), ...(outside ? { outside } : {}) }
}

// which animal a lead goes for: nearest first, but one standing in a pen belongs to somebody (my lead went for Aviendha's cow, 60 blocks off)
// and a calf is next year's herd, not this year's breeding pair. Perrin's `flock.lead count=2` out of a 24-cow herd
// delivered one adult and two calves, silently, and the breed that followed did nothing: the grown one is taken even
// when a calf stands nearer, and a calf is only taken when there was no grown one to take, which is said out loud.
export function leadPick (candidates, allowPenned, mob = 'animal') {
  const free = candidates.filter(c => allowPenned || !c.penned)
  const grown = free.find(c => c.grown !== false)
  if (grown) return { id: grown.id }
  const calf = free[0]
  if (calf) return { id: calf.id, note: `the only ${mob} in range is a calf (at ${calf.at}): it will not breed, and it stays with its herd until it grows` }
  return { error: candidates.length ? `the only ones in range stand in a pen (nearest at ${candidates[0].at}): they are somebody's. penned=true takes one anyway: only from the starter pen or a pen of your own` : 'none in range' }
}

// and which of the ones standing round me come along. Grown first, distance order kept within each: a calf only fills a
// place no grown animal was there to take. An age I could not read is not a reason to leave an animal behind.
export const herdOrder = herd => [...herd.filter(a => a.grown !== false), ...herd.filter(a => a.grown === false)]

// what the reply says came. `with=3` was true and useless: nothing in the line said the pen now holds one cow and two
// calves, so the driver bred an empty pair and saw nothing wrong.
export const ledReport = (mob, came) => {
  const calves = came.filter(a => a.grown === false).length
  return calves
    ? `${mob}:${came.length} (${came.length - calves} grown, ${calves} ${calves === 1 ? 'calf' : 'calves'}: a calf will not breed)`
    : `${mob}:${came.length}`
}

// A lead is walked with the food in my hand, and food in the hand is visible to every animal of its kind that can see
// me, not only to the ones that were picked. So a lead for two out of a big herd walks a queue in, and `with=2` was
// true and said nothing about the four others now standing in the pen (Perrin, item 17). Shedding them is not on
// offer: the food is what the walk is MADE of, and an animal that follows food cannot be told to stop. They are
// counted instead - by id, because one sheep is not told from another by looks or by where it stands. The ones that
// were in the pen before I arrived are not followers, and neither are the ones I asked for.
export function tagalongs (invited, before, now) {
  const known = new Set([...invited, ...before])
  return now.filter(id => !known.has(id)).length
}

export const ledExtra = (mob, extra) => extra
  ? {
      extra,
      extraNote: `${extra} more ${mob} followed the food in uninvited: ${extra === 1 ? 'it is' : 'they are'} in there too. Lead ${extra === 1 ? 'it' : 'them'} out, or feed the pen for ${extra === 1 ? 'one' : extra} more`
    }
  : {}
