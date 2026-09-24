// Why a walk found no path in a second, said so the driver can act. The pathfinder answers "No path to the goal!"
// alike for every dead search, and from a cave pool three of them preceded a drowning (card a164bbfd); a goto dig=true
// from a 1-wide shaft got the same (card 5e16aff9). The causes are readable off the body: no sky over the head with
// the goal up on the surface (a walk neither digs nor climbs); water touching the cell (mineflayer-pathfinder's
// safeToBreak refuses any block beside a liquid, dontCreateFlow, so from a pool a dig walk digs nothing); a protected
// zone or built blocks round it (exclusionAreasBreak 100, never broken); a 1-wide shaft. Pure: bot.mjs hands the evidence in.
const NO_PATH = /no path to the goal|no walkable path|took to long to decide/i
// a goal fewer blocks up than this is a ledge, not the surface: the plain text serves
const SURFACE_DY = 3
const PILLAR = 'pillar up: place a block at your feet, again and again'
const WATER_RULE = 'a dig walk breaks nothing that touches water'

export function noPathAdvice ({ text, dig, underground = false, goalDy = null, wet = false, zoned = false, boxed = false }) {
  if (!NO_PATH.test(text)) return text
  if (dig && wet) return `no path even with dig=true: ${WATER_RULE} (the way would flood), so from a pool nothing can be dug. Swim to the pool's edge, walk two blocks clear of the water, then goto dig=true again; or ${PILLAR}`
  if (dig && zoned) return 'no path even with dig=true: a dig walk breaks nothing inside a protected zone or that looks built (cobblestone, planks, fences...). Dig by hand: dig x= y= z= the wall at head height, step up, again (a staircase), or pillar up: place a block at your feet'
  if (dig && boxed) return `no path even with dig=true: you stand in a 1-wide shaft. Pillar up: place a block at your feet, again and again (dirt or cobblestone in the pocket also lets the walk tower by itself), or dig a staircase by hand: dig the wall at head height, step up, again`
  if (dig || !underground || goalDy === null || goalDy < SURFACE_DY) return text
  const first = wet ? `You are in water: swim to the pool's edge and walk two blocks clear of it first (${WATER_RULE}), then ` : ''
  return `no path: you are underground (no sky over your head) and the goal is ${Math.round(goalDy)} blocks up; a walk neither digs nor climbs. ${first}goto the same spot with dig=true (it digs a staircase and climbs in legs of 6), or ${PILLAR}`
}
