// Melee combat judgements: whether to chase, when a chase breaks off, and refusing an attack that cannot land.

// how a hunt (attack mob=) stands: null = keep fighting. A target that runs or falls away is let go past the leash
export function chaseVerdict (c) {
  if (!c.targetValid) return { killed: true }
  if (!c.hunting) return { killed: false, gaveUp: 'the fight was broken off (the body fled, ate or was interrupted): it is still alive. Look around, then attack again or keep away' }
  if (c.strayed <= c.leash) return null
  return { killed: false, gaveUp: `it led me ${Math.round(c.strayed)} blocks away (leash=${c.leash}): let it go, or attack again from here` }
}

// #105: the fight REFLEX had no leash of its own. mineflayer-pvp walks the body after its target, and the target stays
// beside the body, so any distance measured mob-to-body stays small however far the pair travels: a spider walked the
// body into a cave and it died down there. This leash is measured from where the fight STARTED, and the drop is checked
// before the walk, because falling out of the daylight is what kills, not the distance.
export const CHASE_LEASH = 8
export const CHASE_DROP = 3
// a fight that starts by crossing ground (rangedThreat 'charge' runs at a skeleton that shot from 20 blocks) is owed
// that ground on top of its leash: measured from where the body stood, a flat 8 aborts the charge and walks it back
// into the arrows.
export const chargeLeash = (start, mob, { leash = CHASE_LEASH } = {}) =>
  leash + Math.round(Math.hypot(mob.x - start.x, mob.z - start.z))

// a break-off that was a DROP has to dig and bridge its way back: the body dug its way down and the same ground is in
// the way going up. Walking it instead left the body standing in the hole while a zombie killed it (2026-09-23).
export const breakOffDigs = (start, here) => start.y - here.y > 0

// mobDist: a mob within CHASE_REACH is a fight that came to me. Breaking off then turns my back on it (Perrin, 14:41Z: four hits on the
// walk back, dead in daylight), so the leash waits until it backs off, up to twice its length, and so does a drop (my body, 14:51Z: three hits climbing out)
export const CHASE_REACH = 4
export const chaseBroken = (start, here, { leash = CHASE_LEASH, drop = CHASE_DROP, mobDist = Infinity } = {}) => {
  if (!start) return null
  const fell = start.y - here.y
  if (fell > drop && (mobDist > CHASE_REACH || fell > 2 * drop)) return `the fight pulled me ${Math.round(fell)} blocks down (from y=${Math.round(start.y)}): broken off before it becomes a cave, and I am walking back`
  const away = Math.hypot(here.x - start.x, here.z - start.z)
  if (away > leash && (mobDist > CHASE_REACH || away > 2 * leash)) return `the fight pulled me ${Math.round(away)} blocks from where it started (leash=${leash}): broken off, and I am walking back`
  return null
}

// #97: an enderman killed Ganesha's body at its own cabin in five seconds, because the fight reflex treated it as one
// more mob to beat. Nothing this body carries wins that fight, and aiming at its head is what starts it: these are never
// attacked, never chased, and one that comes within arm's reach is backed away from the way a creeper is.
export const NEVER_FIGHT = new Set(['enderman', 'warden'])
// #live-death 2026-10-02: a dawn hostile scan at 48 blocks showed nothing, and 3 seconds after one teleported in the
// body was dead; waiting until it was at arm's length (5) left no time to move. 16 moves the body away while there
// is still ground between them, instead of waiting for the teleport that closes it.
export const ENDERMAN_RANGE = 16
export const attackRefusal = name => NEVER_FIGHT.has(name)
  ? `${name}: not a fight this body can win (one killed Ganesha's body in five seconds at its own door, #97). Aiming at its head is what provokes it, so I will not aim at one either. Break the line of sight - a block, a door, deep water - and walk away`
  : null
