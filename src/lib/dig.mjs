// Digging: reach and plan, refusals (falls, fluids, buried), and counting what a mining pass found.

import { isAir, looksBuilt, FLUIDS } from './world.mjs'
// can I dig this cell from where I stand: the arm reaches 4.5 from the eyes. dig used to walk to within 3 of every cell
export const DIG_REACH = 4.5
export const digFromHere = (feet, cell, reach = DIG_REACH) =>
  Math.hypot(cell.x + 0.5 - feet.x, cell.y + 0.5 - (feet.y + 1.62), cell.z + 0.5 - feet.z) <= reach

// what a dig does before it moves (card 150b3ee1): a dig at a cell that was already air walked for 148 s before it looked.
// name: the cell as read from here (undefined: its chunk is not loaded). needed: a tool the body lacks for it. near: digFromHere
export const digPlan = ({ name, needed, near }) => {
  if (name === undefined) return near ? 'air' : 'walk'
  if (isAir(name)) return 'air'
  if (needed) return 'tool'
  return near ? 'dig' : 'walk'
}
// a dig's walk that has not arrived by then gives up, and says so
export const DIG_WALK_MS = 30000
export const digUnreached = p =>
  `could not reach ${p.x},${p.y},${p.z} to dig it in ${DIG_WALK_MS / 1000}s (no standing spot within reach, or a drop or mob in the way): goto a spot beside it, then dig again`

// suffocating (died once under gravel that fell while a digging walk tunnelled beneath it): the block to dig free is the one
// round our head; a solid block at the feet alone does no harm
// only blocks that fall: a door or a slab at head height is no burial, and digging it would wreck someone's house
const FALLS = /^(gravel|sand|red_sand|suspicious_sand|suspicious_gravel|.*_concrete_powder)$/
export const buriedIn = ([head]) => head?.boundingBox === 'block' && FALLS.test(head.name) ? head : undefined

// how many of the wanted blocks a round of mining really brought in: the blocks that are gone, if anything at all was picked up
// (drops from tree tops are often still falling when we count, so the items gained are no upper limit).
// Counting every gained item called a round that only dug dirt on the way a success
export const minedCount = (gone, gained) => gained > 0 ? gone : 0

// the collect plugin's way of saying "inventory full" (it wants a chest to empty into, and I give it none)
export const mineFailure = message => /no defined chest locations/i.test(message) ? 'your inventory is full: deposit or drop something, then mine again' : message

// which of the blocks found `mine` goes for. Never inside a protected zone; and not in or beside water unless asked: the walk to a
// submerged block ends in a pocket under water, the air reflex cancels the task, and three bodies drowned on 09-19 fetching sand
export function mineTargets ({ nearby, wanted, inZone, wet, allowWet, what, placed = () => false }) {
  if (!nearby.length) return { error: `no more ${what}` }
  const free = nearby.filter(p => !inZone(p))
  if (!free.length) return { error: `the only ${what} is inside protected zones (someone's build): go further away and retry` }
  const grown = free.filter(p => !placed(p))
  if (!grown.length) return { error: `the only ${what} is placed wood (someone's build: posts, beams), not trees: go where trees grow and retry` }
  const dry = grown.filter(p => allowWet || !wet(p))
  if (!dry.length) return { error: `the only ${what} lies in or next to water, where mining bodies drown: dig it block by block from the shore (dig x= y= z=), or pass wet=true if you take the risk` }
  const skippedWet = grown.length - dry.length
  const skippedPlaced = free.length - grown.length
  return { found: dry.slice(0, wanted), ...(skippedWet ? { skippedWet } : {}), ...(skippedPlaced ? { skippedPlaced } : {}) }
}

// A log is a tree's when leaves grow within 2 of it or of the top of its log column, and nothing built touches the
// column. A house post (stairs on top, planks against it) is not: mine.get *_log cut a cherry village corner post at
// 61,68,-184 and put it back on the wrong axis. `nameAt` takes {x,y,z} and gives a block name
const cube = r => [...Array(2 * r + 1).keys()].map(i => i - r).flatMap(dx => [...Array(2 * r + 1).keys()].map(i => i - r).flatMap(dy => [...Array(2 * r + 1).keys()].map(i => i - r).map(dz => [dx, dy, dz])))
export function isTreeLog (p, nameAt) {
  const isLog = at => /_log$/.test(nameAt(at) ?? '')
  const up = (c, dy) => ({ x: c.x, y: c.y + dy, z: c.z })
  const reach = dir => { const out = []; for (let c = up(p, dir); out.length < 16 && isLog(c); c = up(c, dir)) out.push(c); return out }
  const above = reach(1)
  const column = [...reach(-1), p, ...above]
  const top = above.at(-1) ?? p
  const around = (c, r) => cube(r).map(([dx, dy, dz]) => nameAt({ x: c.x + dx, y: c.y + dy, z: c.z + dz }) ?? 'air')
  const built = column.some(c => around(c, 1).some(looksBuilt))
  const leaves = [p, top].some(c => around(c, 2).some(n => /_leaves$/.test(n)))
  return leaves && !built
}
const fluidCure = 'Scoop the source with fill x= y= z= (an empty bucket), or fill the cell in with place item=dirt'

// target: the block to dig. above: the names of the 3 blocks over it. Water beside it is fine (a trench by a pond); water over it means a dive
export const digRefusal = (target, above, allowWet) => {
  if (FLUIDS.has(target)) {
    const burns = target === 'lava' || target === 'flowing_lava'
    return `${target} is a fluid, not a block: digging it never finishes (one such dig ran 167 seconds before it was cancelled)${burns ? ', and it burns whatever reaches into it' : ''}. ${fluidCure}`
  }
  return !allowWet && above.includes('water')
    ? 'that block is under water: the body would dive for it and run out of air. Work from the shore (dig down beside it, or drain it with sand or dirt first), or pass wet=true if it is shallow and you watch your air'
    : null
}

// clear sweeps a box: one fluid cell in it would hang the whole sweep, so they are skipped and named. counts: {water: 3}
export const fluidsLeft = counts => {
  const named = Object.entries(counts).sort((a, b) => b[1] - a[1]).map(([name, n]) => `${name} x${n}`)
  return named.length ? `${named.join(', ')} left in the box: a fluid cannot be dug. ${fluidCure}` : null
}

// mine started inside a stocked pen: it digs down from where the body stands and the shaft stays (mine in the starter pen, 09-19)
export const penShaftRefusal = (inside, force) => inside && !force
  ? `you stand in a pen with animals in it (${inside}): mine digs its way down from where you stand and would leave a shaft for them to fall into. Walk out through the gate first, then mine (force=true if you really mean it, and fill the hole after)`
  : null

// the ground mine broke open around its start and left open: what to put back, with what. solid(name) says whether a block was ground at all
const FILLERS = ['dirt', 'cobblestone', 'cobbled_deepslate', 'stone', 'andesite', 'diorite', 'granite', 'netherrack']
export function holesLeft (before, after, carried, solid) {
  const item = FILLERS.find(f => carried.includes(f))
  if (!item) return []
  return Object.keys(before).filter(k => solid(before[k]) && !solid(after[k])).map(k => { const [x, y, z] = k.split(',').map(Number); return { x, y, z, item } })
}
