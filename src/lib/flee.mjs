// Fleeing a threat: whether to run, where to, when it is stuck or unwinnable, and what a body faces (archers,
// enderman range) that changes the answer.

import { within, range } from './world.mjs'
import { NEVER_FIGHT, ENDERMAN_RANGE } from './fight.mjs'
// hostile-looking mobs the flee/fight reflexes should leave alone: endermen always, spiders in bright daylight (neutral then)
export const ignorableMob = (name, { day, skyLight }) => name === 'enderman' || (name === 'spider' && day && skyLight >= 12)

// fight or run. Running at 8 health was too late for a body without armour: two zombies took 20 health in 12 s, and it died fleeing.
// So run earlier the less armour it wears and the more there are of them (never above 16: a fresh body may always try)
export function shouldFlee (s) {
  if (!s.armed) return true
  return s.health <= Math.min(8 + (4 - s.armorPieces) + 3 * (s.attackers - 1), 16)
}

// archers shoot from beyond the 7 blocks the other reflexes watch: a body that stood still was shot dead without ever reacting.
// Just hurt, not in a fight, an archer in sight: charge it (it cannot be outwaited), or run when unarmed or badly hurt
export const ARCHERS = new Set(['skeleton', 'stray', 'bogged', 'pillager'])
// #138. The flee reflex had no end and no way home. `GoalInvert(GoalFollow(mob, 16))` runs for as long as the mob is
// followed, `fleeingUntil` was re-armed every tick a threat sat within 7 blocks, and `stop` cleared neither, so a body
// chased once drifted until something else stopped it: hundreds of blocks from its work, and one of them starved on
// the way. A run has a phase now. It goes AWAY until the threat is `bound` blocks off, then BACK to the cell the run
// started from, and both halves can end badly: a run that covers no ground is boxed in and says so, and a second run
// from the same mob within a minute of the last walk back does not walk back at all, because the walk back is what
// was feeding the loop. Either way the agent is told and gets the legs, because the body has run out of ideas and the
// agent has not: it can dig down, wall the hole behind it, fight, or wait for dawn.
export const FLEE_AWAY = 16
export const FLEE_ARCHER_AWAY = 28
export const FLEE_NEAR = 6
export const FLEE_STUCK_MS = 6000
export const FLEE_HOME = 2
export const FLEE_HOLD_MS = 60000
export const FLEE_GIVEUP_MS = 15000
export const FLEE_STUCK_NOTE = 'the run has covered no ground in 6 seconds: I am boxed in. Dig straight down and wall the hole behind me, or turn and fight'
export const FLEE_HELD_NOTE = 'the same threat drove me off again within a minute of the last walk back, so I am not walking back this time. Dig down, fight it, or wait for dawn: your call'
export const fleeRange = name => ARCHERS.has(name) ? FLEE_ARCHER_AWAY : FLEE_AWAY
// where a run heads: a fixed point `dist` straight away from the mob. A goal that moves with the mob reset the path at every one of its
// steps and the body never sprinted (Perrin: 10.6 blocks in 7.7 s, slower than the zombie that killed him)
export const fleeGoal = (me, mob, dist) => {
  const [dx, dz] = [me.x - mob.x, me.z - mob.z]
  const len = Math.hypot(dx, dz)
  const [ux, uz] = len > 0.01 ? [dx / len, dz / len] : [1, 0]
  return { x: Math.round(me.x + ux * dist), z: Math.round(me.z + uz * dist) }
}
// the same mob, too soon: used for the oscillation guard (a minute after a walk back) and, with FLEE_GIVEUP_MS, to
// keep a run the body already gave up on from starting itself again the moment the legs come back
export const fleeOscillating = ({ mob, last, now, within = FLEE_HOLD_MS }) => Boolean(last && last.mob === mob && now - last.at < within)

// One tick of the run. `stillMs` is how long since the body last covered ground, `homeDist` how far it is from where
// the run began, `bound` how far off this threat has to be to count as left behind. Nothing here moves anything.
export function fleeStep ({ phase = null, threatDist = Infinity, homeDist = 0, stillMs = 0, held = false, bound = FLEE_AWAY, underground = false }) {
  if (phase === 'away') {
    // clear before stuck: a body walled in whose threat has wandered off did not fail, it finished
    if (threatDist >= bound) return held ? { phase: null, event: 'flee_held', note: FLEE_HELD_NOTE } : { phase: 'back', event: 'flee_clear' }
    // #147(d): running downhill into the dark is running into the cave, not away from the mob
    if (underground) return { phase: null, event: 'flee_stuck', note: FLEE_CAVE_NOTE }
    if (stillMs >= FLEE_STUCK_MS) return { phase: null, event: 'flee_stuck', note: FLEE_STUCK_NOTE }
    return { phase: 'away' }
  }
  if (phase === 'back') {
    // the threat beats arriving: being home with it on my heels is not being safe
    if (threatDist <= FLEE_NEAR) return { phase: 'away', event: 'flee_started' }
    if (homeDist <= FLEE_HOME) return { phase: null, event: 'flee_returned' }
    if (stillMs >= FLEE_STUCK_MS) return { phase: null, event: 'flee_stuck', note: FLEE_STUCK_NOTE }
    return { phase: 'back' }
  }
  return { phase: null }
}

// (d) my own body, 2026-09-24 11:26Z: it fled a creeper at 10 hp, went down into the cave under my test pits and a zombie
// killed it there. A run heading underground or into an unlit cell is running INTO the thing it is running from. A lit room
// is shelter, so darkness counts only where the sky does not reach: both dark together is a cave mouth
export const FLEE_DROP = 4
export const FLEE_DARK = 4
// Darkness alone is no dive: my body met an enderman on a cave floor lit through its own shaft and holed up on the
// first tick of the run, twice (13:17Z, 13:20Z). Dark counts only on the way DOWN, two below where the run began
export const FLEE_DARK_DROP = 2
export const fleeIntoCave = ({ startY = 0, y = 0, skyLight = 15, light = 15 } = {}) =>
  startY - y >= FLEE_DROP || (startY - y >= FLEE_DARK_DROP && skyLight <= FLEE_DARK && light <= FLEE_DARK)
export const FLEE_CAVE_NOTE = 'the only way clear of it led underground or into the dark, and that is where bodies die: I stopped instead of running into a cave. Dig down and cap the hole, fight it, or wait for dawn: `goto x= y= z= dig=true` brings me back up'

// (b) and (c). Perrin's flee_returned walked his respawned body straight back into the skeleton and the zombie that had just
// killed it. A body that has just died does not walk anywhere while it is night or the killer is still standing there
// The #147 addendum: Mariel respawned on her bed beside the two zombies that had just killed her, fled unarmed, was boxed
// in and died, three times in five minutes. At night a respawn beside a hostile goes under the ground (zombies burn at
// sunrise: wait them out, do not outrun them); by day it only stays put
export function respawnPlan ({ night = false, bedNear = false, killerNear = false } = {}) {
  if (night && killerNear) return { do: 'burrow', why: 'respawned at night beside a hostile: digging down three and capping the hole until it is gone or the sun burns it' }
  if (killerNear) return { do: 'stay', why: 'whatever killed me is still here: staying put, not walking back to it' }
  if (night && bedNear) return { do: 'sleep', why: 'respawned at night with a bed in reach: sleeping it off rather than walking home in the dark' }
  if (night) return { do: 'stay', why: 'respawned at night with no bed: staying where I am until dawn rather than walking back through it' }
  return { do: 'free' }
}

export const crowdSize = hostiles => hostiles.filter(h => h.dist <= 5 || (ARCHERS.has(h.name) && h.dist <= 24)).length
export function rangedThreat (s) {
  if (s.hurtMsAgo > 5000 || s.fighting || s.meleeNear || !s.archerNear) return null
  return s.armed && s.health >= 14 && !s.inWater ? 'charge' : 'flee'
}
// a neutral enderman teleports, so waiting for it to be adjacent before moving leaves no time to put ground or a
// wall between it and the body; one within ENDERMAN_RANGE is moved away from now, aimed at never, provoked or not
export const fleeUnwinnable = (mobs, range = ENDERMAN_RANGE) =>
  mobs.filter(m => NEVER_FIGHT.has(m.name) && m.dist <= range).sort((a, b) => a.dist - b.dist)[0] ?? null
