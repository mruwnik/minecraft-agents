// Night reflexes (card 0f110bb5). Pure: nothing here reads a body or moves one.
//
// A body restarted at night inside a hut (15:14Z) chased a spider out through the door 18 s later, charged a skeleton that
// shot it, fled a zombie 52 blocks east into dark hills (the zombie came back within the minute), and died at health 2 to a
// spider while both runs still said "from: zombie". Three rules: under a roof at night nothing is chased or charged; a
// night run is bounded and heads for lit, known ground; the threat is whatever last hurt me, and what an armed body
// cannot outrun it fights.
import { fleeStep, fleeGoal, NEVER_FIGHT } from './lib.mjs'

const OPEN = new Set(['air', 'cave_air', 'void_air', 'water', 'flowing_water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const COVER = /^(short_grass|tall_grass|fern|large_fern|dead_bush|snow|leaf_litter)$/
// a roof is anything solid-ish straight over the head; water over the head is deep water, not shelter
export const underRoof = above => above.some(name => !OPEN.has(String(name)))
// a room: at most one side open (a doorway at feet and head height) of the eight cells around the body
export const walledIn = sides => sides.length === 8 && sides.filter(name => OPEN.has(String(name)) || COVER.test(String(name))).length <= 2
export const nightShelter = ({ night = false, roofed = false, walled = false }) => night && (roofed || walled)

// the run at night: fleeStep's rules, and a bound from where the run began. Past it the body is in the open dark with the
// mob behind it and no way home, which is where the 15:15Z body died: it stops and goes to ground (flee_stuck) instead
export const NIGHT_FLEE_MAX = 20
export const REFUGE_MIN = 4
export const NIGHT_BOUND_NOTE = `the run is ${NIGHT_FLEE_MAX} blocks from home in the dark: further is where bodies die at night, so I stopped. Dig down and cap the hole, fight it, or wait for dawn`
export function nightFleeStep ({ night = false, ...args }) {
  const step = fleeStep(args)
  if (!night || args.phase !== 'away' || step.event) return step
  return args.homeDist >= NIGHT_FLEE_MAX ? { phase: null, event: 'flee_stuck', note: NIGHT_BOUND_NOTE } : step
}

const away = (from, to, mob) => (to.x - from.x) * (mob.x - from.x) + (to.z - from.z) * (mob.z - from.z) <= 0
const flat = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)
// where a night run heads: the nearest lit or known cell (home, a bed, a placed torch) that lies away from the mob and within
// the bound of home; with none, straight away from the mob as by day, but never past the bound from home
export function nightFleeGoal ({ night = false, me, home, mob, dist, refuges = [] }) {
  const straight = fleeGoal(me, mob, dist)
  if (!night) return straight
  // a refuge beside me is no run at all: the goal has to move the body, or the run stands still and calls itself stuck
  const refuge = refuges.filter(r => flat(r, home) <= NIGHT_FLEE_MAX && flat(r, me) <= NIGHT_FLEE_MAX && flat(r, me) >= REFUGE_MIN && away(me, r, mob)).sort((a, b) => flat(a, me) - flat(b, me))[0]
  if (refuge) return { x: refuge.x, z: refuge.z, kind: refuge.kind }
  const off = flat(straight, home)
  if (off <= NIGHT_FLEE_MAX) return straight
  const scale = NIGHT_FLEE_MAX / off
  return { x: Math.round(home.x + (straight.x - home.x) * scale), z: Math.round(home.z + (straight.z - home.z) * scale) }
}

// the threat is whatever last hurt me: a hit within these seconds by something other than the mob the run is from
export const RETARGET_MS = 3000
export const retarget = ({ fleeing = null, hurtBy = [], hurtMsAgo = Infinity }) =>
  hurtMsAgo <= RETARGET_MS && hurtBy[0] && hurtBy[0] !== fleeing ? hurtBy[0] : null

// what cannot be outrun: climbers, leapers and flyers. An armed body turns and fights these instead of running
export const NO_OUTRUN = new Set(['spider', 'cave_spider', 'phantom', 'vex', 'blaze', 'ravager', 'hoglin'])
export const fightNotFlee = ({ armed = false, mob }) => armed && NO_OUTRUN.has(mob) && !NEVER_FIGHT.has(mob) && mob !== 'creeper'

// shouldFlee's attacker count: the mob that just hit me is in the fight even when the knockback put it past the crowd's radius
export const attackerCount = ({ crowd = 0, seen = [], hurtBy = [], hurtMsAgo = Infinity }) =>
  crowd + (hurtMsAgo <= RETARGET_MS && hurtBy[0] && !seen.includes(hurtBy[0]) ? 1 : 0)

// an archer through a gap: the cells between me and it along its dominant direction, head height first (the arrow comes at
// the head), then the feet. The body plugs the first that is open
export function plugCells (me, mob) {
  const [dx, dz] = [mob.x - me.x, mob.z - me.z]
  const step = Math.abs(dx) >= Math.abs(dz) ? { dx: Math.sign(dx), dz: 0 } : { dx: 0, dz: Math.sign(dz) }
  return [{ ...step, dy: 1 }, { ...step, dy: 0 }].map(({ dx, dy, dz }) => ({ dx, dy, dz }))
}

// three holds: a mob in reach is swung at (melee), a mob outside the door is waited for (door), an archer is not charged
export function holdNote ({ mob, plugged = false, melee = false, door = false }) {
  if (melee) return `a ${mob} in reach at night: swinging at it from inside, not chasing it out of the door`
  if (door) return `a ${mob} outside at night: not chasing it out of the door into the dark${plugged ? '; I plugged the way in' : ''}. It is swung at if it comes in reach`
  return `a ${mob} shooting at me at night: not charging it into the dark. ${plugged ? 'I plugged the gap it shoots through' : 'Nothing to plug the gap with: step out of its line of sight, or place a block between us'}`
}
