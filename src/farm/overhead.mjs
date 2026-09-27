// Tidy deliberately clears only the two cells above the ground. A tree can leave
// its upper trunk and canopy over a usable bed, so report those separately rather
// than silently treating the cleared base as a cleared tree. This never digs.
import { PLAN_LEGEND, planSpec } from '../lib/plan.mjs'

export const OVERHEAD_TREE_HEIGHT = 32
const TREE_WOOD = /_(?:log|wood)$/
const TREE_LEAVES = /_leaves$/

export function overheadTreeBlocks (cells, worldAt) {
  const found = []
  for (const cell of cells) {
    const spec = planSpec(cell)
    // Planned saplings, structures and ground outside the plan are not farm beds.
    if (spec?.kind !== 'crop' && spec?.kind !== 'path') continue
    for (let height = 3; height <= OVERHEAD_TREE_HEIGHT; height++) {
      const y = cell.y + height
      const name = worldAt(cell.x, y, cell.z)?.name
      if (!name || (!TREE_WOOD.test(name) && !TREE_LEAVES.test(name))) continue
      found.push({ name, x: cell.x, y, z: cell.z, height, kind: TREE_LEAVES.test(name) ? 'leaves' : 'wood' })
    }
  }
  return found
}

export function overheadTreeLine (blocks) {
  if (!blocks.length) return undefined
  const wood = blocks.filter(b => b.kind === 'wood')
  const leaves = blocks.filter(b => b.kind === 'leaves')
  const span = key => blocks.reduce(([min, max], b) => [Math.min(min, b[key]), Math.max(max, b[key])], [Infinity, -Infinity]).join('..')
  const samples = [...wood.slice(0, 2), ...leaves.slice(0, 2)]
  const at = samples.map(b => `${b.name}@${b.x},${b.y},${b.z}`).join(' ')
  const extra = blocks.length > samples.length ? ` and ${blocks.length - samples.length} more` : ''
  return `${wood.length} log/wood, ${leaves.length} leaves over beds/paths at y=${span('y')} (${span('height')} above ground; scan +3..${OVERHEAD_TREE_HEIGHT}): ${at}${extra}; inspect the remaining trunk/canopy and choose safe tree removal or revise the plan; upper blocks were left intact`
}
