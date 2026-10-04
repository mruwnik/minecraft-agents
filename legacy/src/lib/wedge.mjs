// A body wedged against geometry: detecting it, nudging free, and what edge/corner offset caused it.

// The server silently puts the body back every tick when its hitbox touches a block just so (see the hitbox
// note in README). One window of {resets, moved blocks}: many resets and no progress means it is wedged.
export const isWedged = ({ resets, moved }) => resets >= 30 && moved < 0.5

// the cells a standing body's hitbox (0.6 wide) lies flush against, at feet and head level: the candidates for what wedges it
export function flushCells (pos) {
  const span = v => [...new Set([Math.floor(v - 0.299), Math.floor(v + 0.299)])]
  const side = v => { const f = v - Math.floor(v); return f <= 0.305 ? Math.floor(v) - 1 : f >= 0.695 ? Math.floor(v) + 1 : null }
  const levels = [Math.floor(pos.y), Math.floor(pos.y) + 1]
  const [sx, sz] = [side(pos.x), side(pos.z)]
  return [
    ...(sx === null ? [] : span(pos.z).flatMap(z => levels.map(y => ({ x: sx, y, z })))),
    ...(sz === null ? [] : span(pos.x).flatMap(x => levels.map(y => ({ x, y, z: sz }))))
  ]
}

// a position 2 cm clear of whatever the hitbox lies flush against (see flushCells): out of the pinned state without digging
export function nudgeAway (pos) {
  const clear = v => { const f = v - Math.floor(v); return f <= 0.305 ? Math.floor(v) + 0.32 : f >= 0.695 ? Math.floor(v) + 0.68 : v }
  return { x: clear(pos.x), y: pos.y, z: clear(pos.z) }
}

// prismarine-physics stops a move only at a box that lies strictly ahead, and counts any overlap on the other axes. A hitbox edge that rounds
// 1e-14 past a block face (-128.3001 + 0.3001 = -127.99999999999999) then counts as "already inside": the body walks into the block and the
// server puts it back every tick (the step-up wedge). Same comparison, with a tolerance. Boxes are { min: [x,y,z], max: [x,y,z] }
const EDGE = 1e-7
export function clampedOffset (block, player, axis, offset) {
  const beside = [0, 1, 2].filter(a => a !== axis).some(a => player.max[a] <= block.min[a] + EDGE || player.min[a] >= block.max[a] - EDGE)
  if (beside) return offset
  if (offset > 0 && player.max[axis] <= block.min[axis] + EDGE) return Math.min(Math.max(block.min[axis] - player.max[axis], 0), offset)
  if (offset < 0 && player.min[axis] >= block.max[axis] - EDGE) return Math.max(Math.min(block.max[axis] - player.min[axis], 0), offset)
  return offset
}

// what the wedge reflex may break by itself to get the body free: it grows back and nobody built it
// blocks the pathfinder takes for a full cube but that carry nobody: it must neither walk through them nor plan to stand on them (like a fence)
export const noFooting = name => name === 'bamboo'
export const wedgeBreakable = name => /_leaves$|^bamboo$/.test(name)

// Walking me out of a fence's cell (see realCell): `pressed` is the cell I am being walked into, or null. Start when the idle nudge finds me in a fence's cell; from then
// on keep going whatever the nudge says, until I stand within 0.3 of that cell's middle: only there does a new plan start from the right side of the fence
export function fencePush (pressed, inFence, nudge, pos) {
  const target = pressed ?? (nudge && inFence ? inFence : null)
  if (!target) return { target: null, replan: false }
  const there = Math.abs(pos.x - target.x - 0.5) <= 0.3 && Math.abs(pos.z - target.z - 0.5) <= 0.3
  return there ? { target: null, replan: true } : { target, replan: false }
}
