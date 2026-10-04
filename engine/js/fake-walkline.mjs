// How far an animal gets walking a line of points over the fake's blocks at its feet: it stops before the first
// cell it cannot enter, or once within `stop` of the last point. Shared by the lead's drag and the food's tempt.
const STEP = 0.1
const OPEN_AIR = /^(air|cave_air|void_air|short_grass|tall_grass|fern)$/

const cellKey = ({ x, y, z }) => `${Math.floor(x)},${Math.floor(y)},${Math.floor(z)}`
const flat = (a, b) => Math.hypot(a.x - b.x, a.z - b.z)

// True when nothing stands in the cell at pos: open air only (a dragged animal jams in a gate, open or not).
export const openAir = s => pos => OPEN_AIR.test(s.blocks.get(cellKey(pos)) ?? 'air')

// Open air or an open fence gate: what an animal walking by its own path can enter.
export const walkable = s => pos => {
  const k = cellKey(pos)
  return openAir(s)(pos) || (/_fence_gate$/.test(s.blocks.get(k) ?? '') && s.states.get(k)?.open === true)
}

// Walk points[0] -> ... -> the last point. { pos, clear }: where the walk ended, and whether it got within `stop` of
// the last point without meeting a cell `enters` refuses (the start cell is never judged).
export function walkLine (points, enters, stop = 0) {
  const target = points.at(-1)
  const start = cellKey(points[0])
  let at = { ...points[0] }
  if (flat(at, target) <= stop) return { pos: at, clear: true }
  for (let i = 1; i < points.length; i++) {
    const from = points[i - 1]
    const to = points[i]
    const steps = Math.max(1, Math.ceil(flat(from, to) / STEP))
    for (let n = 1; n <= steps; n++) {
      const next = { ...at, x: from.x + (to.x - from.x) * n / steps, z: from.z + (to.z - from.z) * n / steps }
      if (cellKey(next) !== start && !enters(next)) return { pos: at, clear: false }
      if (flat(next, target) <= stop) {
        const d = flat(at, target)
        const k = stop / d
        return { pos: d > stop ? { ...at, x: target.x + (at.x - target.x) * k, z: target.z + (at.z - target.z) * k } : next, clear: true }
      }
      at = next
    }
  }
  return { pos: at, clear: true }
}
