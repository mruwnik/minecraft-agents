// Holing up: the site checks and verdict for burrowing into the ground to wait something out.

// #147. Perrin starved into a skeleton: 16 bread gone in 35 minutes of leading, then "starving: eat" every 4 s from health 20
// to 1 while the flee reflex worked exactly as designed, handed control back twice, and no agent answered in the 40 s it bought.
// A body that cannot eat and cannot win has one move left, the one the reflex memory already knew: go under the ground and
// close the hole. It does that itself now. These verdicts decide WHETHER and WHAT; nothing here touches a block.
export const HOLE_DEPTH = 3
export const HOLE_HEALTH = 6
// what must not be under the feet before digging down: the cave, the lava and the water this is meant to avoid
export const HOLE_UNSAFE = new Set(['air', 'cave_air', 'void_air', 'water', 'flowing_water', 'lava', 'flowing_lava', 'bubble_column'])
// A hole takes seconds to dig and the hands are busy the whole time: with a hostile in reach that is standing still to be
// killed (my body, 13:40Z: dug under two zombies and a skeleton and died at the bottom). An armed body fights instead;
// an unarmed one has nothing better than the ground
export const HOLE_MELEE = 4
// and where the mob stands at that instant is not the measure: my body (17:26Z) fled a zombie 3 blocks, stopped in the dark with it
// 4-8 blocks behind, dug, and it was back on it 1.5 s later; dead at the bottom with a stone sword. A hit within these last seconds
// is a hostile in reach whether or not it is seen
export const HOLE_HURT_MS = 3000
export const holeUpRefusal = ({ armed, hostileDist, hurtMsAgo = Infinity }) =>
  armed && (hostileDist <= HOLE_MELEE || hurtMsAgo <= HOLE_HURT_MS) ? 'fight' : null

export function holeUpVerdict ({ food = 20, hasFood = true, health = 20, night = false, mobNear = false, stuck = false } = {}) {
  if (stuck) return { why: 'the run is boxed in and there is nowhere to run to' }
  // a full pack is auto-eat's business, however low the food bar is
  if (hasFood) return null
  if (food <= 0 && (night || mobNear)) return { why: `food 0 and nothing at all to eat${night ? ' at night' : ' with a mob in reach'}: standing here is starving to death` }
  // (e) a body with no food cannot heal: at 6 health waiting does not get better, day or night
  if (health <= HOLE_HEALTH) return { why: `health ${health} and nothing to eat: it cannot heal, so waiting in the open only ends one way` }
  return null
}
// Read the three cells under the feet BEFORE digging: down into lava, water or a cave is the death this is meant to avoid.
// When the floor cannot be trusted, wall in where the body stands instead; with nothing to place, say the shelter is open
export function burrowPlan ({ below = [], cap = false } = {}) {
  const floor = below.slice(0, HOLE_DEPTH)
  // an unloaded cell reads as null, which is not "safe": it is "I cannot see the floor", and blind is how a body digs into lava
  const found = floor.findIndex(name => !name || HOLE_UNSAFE.has(String(name)))
  const bad = floor.length < HOLE_DEPTH ? 'a floor I could not read' : (found < 0 ? null : (floor[found] ?? 'a cell I cannot read'))
  if (!bad) return { way: 'dig', open: !cap }
  return { way: 'wall', open: !cap, why: `${bad} under my feet: digging down from here would drop me into it, so I am walling myself in where I stand` }
}
export const holedUpNote = ({ way, open, surface }) =>
  `${way === 'dig' ? `dug ${HOLE_DEPTH} straight down` : 'walled myself in where I stood'} and ${open ? 'had nothing to close it with: the hole is OPEN, so something can still reach me' : 'closed it over'}. ` +
  `The way out when you want me back: goto x=${surface.x} y=${surface.y} z=${surface.z} dig=true`
// the cells a hole-up fills around the body's feet: the four sides at feet and head height, and the one over the head.
// A shaft dug in a cave had air on two sides and a cap that kept nothing out (13:17Z); a filled cell that is already
// solid costs nothing
export const holeCells = () => [[1, 0], [-1, 0], [0, 1], [0, -1]].flatMap(([dx, dz]) => [[dx, 0, dz], [dx, 1, dz]]).concat([[0, 2, 0]])
