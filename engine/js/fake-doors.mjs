// Doors, gates and trapdoors in the fake: the block's `open` state (the fake's s.states) decides whether a body passes. An open
// one has no collision for the fake steer, a shut one is a wall; the planner's snapshot reads the state; a click on a door flips
// both of its halves, as the server does.

const OPENABLE = /_(fence_gate|door|trapdoor)$/
const DOOR = /_door$/
// the block properties the planner's state ids carry
const PATH_PROPS = ['open', 'half', 'facing', 'hinge', 'face', 'powered']
// what opens a door from a distance: the planner looks for these beside an iron door
const ACTIVATOR = /_button$|^lever$/
const key = (x, y, z) => `${x},${y},${z}`

export const isOpenable = name => OPENABLE.test(name ?? '')

// does the block at the cell k (x,y,z key) let a body through: an open door, gate or trapdoor
export const isOpen = (s, k) => isOpenable(s.blocks.get(k)) && s.states.get(k)?.open === true

// an open trapdoor over a ladder of its own facing is climbable, as on the server (and in prismarine-physics once patched)
export const climbsThrough = (s, x, y, z) => {
  const [here, below] = [key(x, y, z), key(x, y - 1, z)]
  return isOpen(s, here) && /_trapdoor$/.test(s.blocks.get(here)) && s.blocks.get(below) === 'ladder' && s.states.get(here)?.facing === s.states.get(below)?.facing
}

// the properties of the block at k for a planner state id, {} for a block that is neither openable, a button or lever, nor
// a ladder (a trapdoor over it is judged against its facing)
export const pathProps = (s, k) => {
  const name = s.blocks.get(k)
  if (!isOpenable(name) && !ACTIVATOR.test(name ?? '') && name !== 'ladder') return {}
  const states = s.states.get(k) ?? {}
  return Object.fromEntries(PATH_PROPS.filter(p => states[p] !== undefined).map(p => [p, states[p]]))
}

// flip `open` of the block at pos and, for a door, of its other half
export function flipOpen (s, { x, y, z }) {
  const k = key(x, y, z)
  const open = !s.states.get(k)?.open
  const cells = [k]
  if (DOOR.test(s.blocks.get(k))) {
    const other = s.states.get(k)?.half === 'upper' ? key(x, y - 1, z) : key(x, y + 1, z)
    if (s.blocks.get(other) === s.blocks.get(k)) cells.push(other)
  }
  cells.forEach(c => s.states.set(c, { ...s.states.get(c), open }))
}
