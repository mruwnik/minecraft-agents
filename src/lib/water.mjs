// Breathing and surfacing: the air reflex, what counts as breathable, and a stalled surface attempt.

// running out of air. 'start' takes the body away from its task (a task that keeps steering drowned Jizo), 'hold' keeps swimming up.
// oxygen alone misfires on dry land through ViaBackwards, so starting also needs the head under water
export function airReflex (s) {
  if (!s.surfacing) return s.headInWater && s.oxygen <= 8 ? 'start' : null
  // Older protocol bridges can report a full oxygen bar while the head is
  // still submerged. Keep swimming until we can actually breathe.
  return !s.headInWater ? 'stop' : 'hold'
}

// swimming is faster than any walk, so the reflex only looks for open water sideways when the column over the head is
// roofed. `names` are the blocks from just over the head upwards; water plants and a bubble column are still water.
const WATERY = new Set(['water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const BREATHABLE = new Set(['air', 'cave_air', 'void_air'])
export function openAbove (names) {
  const blocker = names.find(name => !WATERY.has(name))
  return blocker === undefined || BREATHABLE.has(blocker)
}

// Chani drowned at 126,53,-52 walking to a surface three blocks away: the pathfinder said it was moving, so the reflex
// never pressed jump. Give the walk two seconds to lift the body, then drop the path and swim straight up.
export function surfacingStalled ({ startedAt, now, startY, y, swimming }) {
  if (swimming) return false
  return now - startedAt >= 2000 && y < startY + 0.5
}
