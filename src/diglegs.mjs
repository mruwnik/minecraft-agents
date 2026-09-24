// A dig walk of 10+ blocks through rock runs out of its 5 s search or circles for 40 s, while legs of 5-8 blocks arrive
// in 10-30 s each (cards 5e16aff9 and a164bbfd): every dig move costs a search node or several, and a straight line
// through rock 20 long is more nodes than the budget holds. So goto dig=true walks its own straight line in legs.
export const DIG_LEG = 6

// the cells to walk to in turn from `from` to `to`, each leg at most DIG_LEG long on the straight line, the last being the goal
export function digLegs (from, to, leg = DIG_LEG) {
  const d = { x: to.x - from.x, y: to.y - from.y, z: to.z - from.z }
  const n = Math.ceil(Math.hypot(d.x, d.y, d.z) / leg)
  const between = Array.from({ length: Math.max(n - 1, 0) }, (_, i) => (i + 1) / n)
    .map(t => ({ x: Math.round(from.x + d.x * t), y: Math.round(from.y + d.y * t), z: Math.round(from.z + d.z * t) }))
  return [...between, { x: to.x, y: to.y, z: to.z }]
}
