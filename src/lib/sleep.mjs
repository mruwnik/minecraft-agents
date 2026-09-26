// Sleeping: whether it is night, oversleeping, bed choice, and the report once the body wakes.

import { inAnyZone } from './world.mjs'
// night is when beds work; state, the shared clock, night_fell and the reflexes must all agree on it
export const isNight = tick => tick > 12542 && tick < 23460

// when others skip the night the server's "leave bed" sometimes never reaches the body, and it lies in bed all day. In bed in broad
// daylight (past the first 15 s of the day, no thunderstorm) means: get up myself
export const oversleeping = s => s.asleep && !s.thundering && s.timeOfDay > 300 && s.timeOfDay < 12000

// in bed in broad daylight: 'ask' the server to let me up; when that changes nothing for 6 s the server has me up already and only my own flag is stale: 'declare' myself awake
export const wakeStep = ({ oversleeping, forMs }) => !oversleeping ? null : forMs >= 6000 ? 'declare' : 'ask'

// on: the block I stand in, above: the bounding box two cells up. A bed is 0.56 high, so a ceiling 2 above the floor leaves 1.44: nobody fits
export const bedTrap = (on, above) => /_bed$/.test(on ?? '') && above === 'block'
  ? 'you are standing ON a bed under a low ceiling (1.4 blocks of headroom, nobody fits, so no walk can start): dig the bed, walk out, place it back. For good: leave one free floor cell beside the bed, or raise the ceiling over it by one'
  : null

// beds: nearest first. A bed in another agent's zone is theirs (zones are named owner-something; starter-* is shared, and so is
// a *-village: protected from digging, not owned, its beds are for any body caught out at night) and a bed holds one sleeper
export function bedChoice (beds, zones, me, any = false, occupied = new Set()) {
  if (!beds.length) return { error: 'no bed within 32 blocks' }
  const shared = name => new RegExp(`^(${me.toLowerCase()}|starter)-|-village$`).test(name)
  const theirs = bed => zones.find(z => inAnyZone([z], bed) && !shared(z.name))
  const free = beds.filter(b => !occupied.has(`${b.x},${b.y},${b.z}`))
  const bed = any ? free[0] : free.find(b => !theirs(b))
  if (bed) return { bed }
  const spare = 'Place your own (spare beds are in the starter chest at 112,70,-138)'
  if (any || beds.some(b => !theirs(b))) return { error: `every bed you may use within 32 blocks is occupied: a bed holds one sleeper. ${spare}` }
  return { error: `the only bed within 32 blocks is in ${theirs(beds[0]).name}: that is their bed, and a bed holds one sleeper. ${spare}, or sleep any=true if they invited you` }
}
// the bedtime reflex failed with this error: what to tell the driver (null: nothing, the driver's own order took over)
export const bedtimeReport = error => /^cancelled: superseded/.test(error)
  ? null
  : /monsters nearby/.test(error) ? `${error}: the server lets nobody sleep with a monster within 8 blocks of the bed. Kill it (attack mob=<its name>) and sleep again, or wait it out indoors; walls and light around the bed keep them off` : error

// will this bed let me go in the morning? The server wakes a sleeper ON the bed when it finds no better spot, 0.56 above the floor, and from there
// a cell with a block 2 above its floor cannot be entered (Aviendha, every morning). exits: the cells around the bed, { at: 'x,y,z', free, lintel }
export function bedExit (exits) {
  if (!exits.length || exits.some(e => e.free && !e.lintel)) return null
  const low = exits.find(e => e.free)
  if (!low) return 'this bed has no free cell beside it: you will wake up standing ON it with nowhere to step. Clear one cell next to it (feet and head, and the block above those)'
  const [x, y, z] = low.at.split(',').map(Number)
  return `this bed is a trap: you wake up standing ON it (0.56 high) and the only free cell beside it (${low.at}) has a block 2 above its floor, which leaves 1.44: nobody fits and no walk will start. Dig the block at ${x},${y + 2},${z} (or move the bed next to a cell with 3 of headroom). In the morning, if stuck: dig the bed, walk out, place it back`
}
