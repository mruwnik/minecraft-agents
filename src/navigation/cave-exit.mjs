// Why a walk found no path in a second, said so the driver can act. The pathfinder answers "No path to the goal!"
// alike for every dead search, and from a cave pool three of them preceded a drowning (card a164bbfd); a goto dig=true
// from a 1-wide shaft got the same (card 5e16aff9). The causes are readable off the body: no sky over the head with
// the goal up on the surface (a walk neither digs nor climbs); water touching the cell (mineflayer-pathfinder's
// safeToBreak refuses any block beside a liquid, dontCreateFlow, so from a pool a dig walk digs nothing); a protected
// zone or built blocks round it (exclusionAreasBreak 100, never broken); a 1-wide shaft. Pure: bot.mjs hands the evidence in.
import { breaksUnderfoot } from '../lib/path.mjs'

const NO_PATH = /no path to the goal|no walkable path|took to long to decide/i
// a goal fewer blocks up than this is a ledge, not the surface: the plain text serves
const SURFACE_DY = 3
const PILLAR = 'pillar up: place a block at your feet, again and again'
const WATER_RULE = 'a dig walk breaks nothing that touches water'

const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]
// A hole one block deep: every cell beside the feet at feet height is a block and the cells over those are open, with
// headroom to jump. A body that dropped into one in the middle of a field stood there five minutes while every walk
// said "no walkable path" (card 94e6dcb1): the answer is about the cell it is in. `passable(dx, dy, dz)` is relative
// to the feet, like boxedIn's. A shaft (walls at head height too) is boxedIn's case, not this one
export const inHole = passable =>
  passable(0, 2, 0) && SIDES.every(([dx, dz]) => !passable(dx, 0, dz) && passable(dx, 1, dz) && passable(dx, 2, dz))

// One block ABOVE a field: farm.maintain parked a body on a log in its melon rows and every walk said "no walkable path"
// (Jizo, 09-26 23:24Z). Beside the feet, farmland two below with the cells over it open (air or a crop): the way down is a
// drop onto farmland, the one move a walk takes last. `nameAt(dx, dy, dz)` names the block about the feet
const openCell = name => ['air', 'cave_air'].includes(name) || breaksUnderfoot(name)
export const perchedOverField = nameAt =>
  SIDES.some(([dx, dz]) => nameAt(dx, -2, dz) === 'farmland' && openCell(nameAt(dx, -1, dz)) && openCell(nameAt(dx, 0, dz)))

export function noPathAdvice ({ text, dig, underground = false, goalDy = null, wet = false, zoned = false, boxed = false, holed = false, perched = false }) {
  if (!NO_PATH.test(text)) return text
  if (holed) return 'no path: you stand in a hole one block deep (every cell beside you at feet height is a block). If the hole is what stops the walk, pillar_up steps=1 (a block placed under your feet) lifts you out, or goto the same spot with dig=true, then walk on; if not, the goal is what has no way to it: path_to x= y= z= from up there names the gap'
  if (perched) return 'no path: you stand one block above a field (farmland one below beside you). If getting down is what stops the walk, dig the block underfoot, or goto with dig=true, then walk on; if not, the goal is what has no way to it: path_to x= y= z= from down there names the gap'
  if (dig && wet) return `no path even with dig=true: ${WATER_RULE} (the way would flood), so from a pool nothing can be dug. Swim to the pool's edge, walk two blocks clear of the water, then goto dig=true again; or ${PILLAR}`
  if (dig && zoned) return 'no path even with dig=true: a dig walk breaks nothing inside a protected zone or that looks built (cobblestone, planks, fences...). Dig by hand: dig x= y= z= the wall at head height, step up, again (a staircase), or pillar up: place a block at your feet'
  if (dig && boxed) return `no path even with dig=true: you stand in a 1-wide shaft. Pillar up: place a block at your feet, again and again (dirt or cobblestone in the pocket also lets the walk tower by itself), or dig a staircase by hand: dig the wall at head height, step up, again`
  if (dig || !underground || goalDy === null || goalDy < SURFACE_DY) return text
  const first = wet ? `You are in water: swim to the pool's edge and walk two blocks clear of it first (${WATER_RULE}), then ` : ''
  return `no path: you are underground (no sky over your head) and the goal is ${Math.round(goalDy)} blocks up; a walk neither digs nor climbs. ${first}goto the same spot with dig=true (it digs a staircase and climbs in legs of 6), or ${PILLAR}`
}
