// The way out of the water when the air runs low, judged before the body swims. A body drowned at -129.3,33.2,-138.3
// (card a164bbfd) under a rock ceiling with an air cell two blocks away: the reflex read an open column over its head,
// then pressed forward as well as jump, and in water forward moves along the yaw whatever the pitch (prismarine-physics
// applyHeading), so it drifted two blocks sideways under the roof and pushed against stone until it died. Pure: bot.mjs
// reads the column, the openings and the ceiling off the world and hands them in.
import { openAbove } from './lib.mjs'

// water plants and a bubble column are still water: a body swims through them
const WATERY = new Set(['water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const BREATHABLE = new Set(['air', 'cave_air', 'void_air'])

// how far round the body the reflex looks for water with air over it: a longer swim than this on 8 air is a gamble
export const SURFACE_SCAN = 6
// no nearer than this for two seconds and the swim is pressing into a wall: that opening is given up
const SWIM_STALL_MS = 2000
const NEARER = 0.3
const TICKS_PER_AIR = 15
const DROWN_HIT_TICKS = 20
const DROWN_HIT = 2
// two drowning hits kept in hand, and one second to swim up into the pocket once it is dug
const POCKET_MARGIN_TICKS = 2 * DROWN_HIT_TICKS + 20

const NO_WAY = `no open water within ${SURFACE_SCAN} and nothing over my head I can dig in time: swimming up anyway`

// ticks before a body with this much air and health is dead: 15 ticks an air point, then two hearts a second
export const airBudgetTicks = ({ oxygen, health }) => oxygen * TICKS_PER_AIR + Math.ceil(health / DROWN_HIT) * DROWN_HIT_TICKS

// the index in the column (from just over the head) of the first block that is neither water nor air: the ceiling, or -1 under the sky
export const roofAt = names => names.findIndex(name => !WATERY.has(name) && !BREATHABLE.has(name))

const key = ({ x, y, z }) => `${x},${y},${z}`
const flat = (me, to) => Math.round(Math.hypot(to.x + 0.5 - me.x, to.z + 0.5 - me.z) * 10) / 10

// { way: 'up' } when the column over the head is water to the sky; { way: 'sideways', to, dist } for the nearest water
// cell with air over it within SURFACE_SCAN, skipping the ones already tried; { way: 'pocket', at, ticks } to dig the
// ceiling when the tool in the pocket breaks it inside the air that is left; else up with the note, since anything
// beats treading water. column: block names from just over the head upwards; openings: water cells with air above;
// ceiling: the first block over the head that is not water, with digTicks for the best carried tool here (Infinity: never)
export function surfaceWay ({ column, openings = [], me, ceiling = null, oxygen = 20, health = 20, tried = [] }) {
  if (openAbove(column)) return { way: 'up' }
  const skip = new Set(tried)
  const near = openings.filter(cell => !skip.has(key(cell)))
    .map(cell => ({ to: cell, dist: flat(me, cell) }))
    .filter(({ dist }) => dist <= SURFACE_SCAN)
    .sort((a, b) => a.dist - b.dist)
  if (near.length) return { way: 'sideways', to: near[0].to, dist: near[0].dist }
  const ticks = ceiling?.digTicks ?? Infinity
  if (ticks + POCKET_MARGIN_TICKS <= airBudgetTicks({ oxygen, health })) return { way: 'pocket', at: { x: ceiling.x, y: ceiling.y, z: ceiling.z }, ticks }
  return { way: 'up', note: NO_WAY }
}

// the nearest a sideways swim has come to its opening and when: no nearer in two seconds means a wall, not a slow swim
export function swimProgress (track, dist, now) {
  if (!track) return { best: dist, at: now, stalled: false }
  if (dist < track.best - NEARER) return { best: dist, at: now, stalled: false }
  return { best: track.best, at: track.at, stalled: now - track.at >= SWIM_STALL_MS }
}
