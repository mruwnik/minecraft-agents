// Which chunk sections a block search has to read. mineflayer's findBlocks walks sections in an octahedron of apothem
// ceil((range + 8) / 16) around the body's section, and at range 16 that is 2: the section one over, one down and one
// across is three steps away and is never read, though its cells can be nine blocks off. mariel-apiary, 2026-09-26
// 21:00Z: a body at -1.4,64,49.6 asked for campfires within 16 and got one of four (card 4552230d). So: every section
// whose nearest cell lies within the range, nearest first, and the search stops when no later section can hold a
// nearer block than the ones already in hand. Pure; the body reads the sections.
export const SECTION = 16

const sectionOf = v => Math.floor(v / SECTION)
const floored = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })
// from a cell to the nearest cell of a section, axis by axis
const gap = (v, s) => { const lo = s * SECTION; const hi = lo + SECTION - 1; return v < lo ? lo - v : v > hi ? v - hi : 0 }
const nearest = (at, s) => Math.hypot(gap(at.x, s.x), gap(at.y, s.y), gap(at.z, s.z))

export function searchSections (point, maxDistance, { minY = -64, height = 384 } = {}) {
  const at = floored(point)
  const lowest = sectionOf(minY)
  const highest = sectionOf(minY + height - 1)
  const found = []
  for (let x = sectionOf(at.x - maxDistance); x <= sectionOf(at.x + maxDistance); x++) {
    for (let z = sectionOf(at.z - maxDistance); z <= sectionOf(at.z + maxDistance); z++) {
      for (let y = Math.max(lowest, sectionOf(at.y - maxDistance)); y <= Math.min(highest, sectionOf(at.y + maxDistance)); y++) {
        const near = nearest(at, { x, y, z })
        if (near <= maxDistance) found.push({ x, y, z, near })
      }
    }
  }
  return found.sort((a, b) => a.near - b.near)
}

// `count` blocks are in hand (their distances) and the next section starts beyond the farthest of the count nearest
export function enough (distances, count, near) {
  if (distances.length < count) return false
  const kept = [...distances].sort((a, b) => a - b)[count - 1]
  return kept < near
}
