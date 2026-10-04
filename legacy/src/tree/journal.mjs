import { isAir } from '../lib/world.mjs'
import { planSpec } from '../lib/plan.mjs'
import { harmlessPlant, treeSpec } from './inspect.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const sameRoot = (a, b) => a && b && ['x', 'y', 'z'].every(k => a[k] === b[k])
const nameIn = (name, profile) => [...profile.wood, ...(profile.roots ?? [])].includes(name)

// A record is trusted only for the one mapped planting site and only for
// matching species wood inside that profile's inspected envelope.
export function validateForestryRecord (record, tree, worldAt) {
  if (!record || record.phase !== 'harvesting' || !sameRoot(record.root, tree.root) ||
      record.species !== tree.species || record.form !== tree.form || !Array.isArray(record.wood) ||
      !record.wood.length || !Array.isArray(record.leaves)) return { ok: false, reason: 'no matching active forestry harvest record' }
  const withinEnvelope = p => p.x >= tree.root.x - tree.profile.radius &&
    p.x <= tree.root.x + tree.profile.radius + tree.profile.width - 1 &&
    p.z >= tree.root.z - tree.profile.radius &&
    p.z <= tree.root.z + tree.profile.radius + tree.profile.width - 1
  const seen = new Set()
  for (const p of record.wood) {
    if (!p || !Number.isSafeInteger(p.x) || !Number.isSafeInteger(p.y) || !Number.isSafeInteger(p.z) ||
        !nameIn(p.name, tree.profile) || p.y < tree.root.y + 1 || p.y > tree.root.y + tree.profile.height ||
        !withinEnvelope(p) || seen.has(key(p))) return { ok: false, reason: 'forestry record contains wood outside this tree profile' }
    seen.add(key(p))
  }
  for (const p of record.leaves) {
    if (!p || !Number.isSafeInteger(p.x) || !Number.isSafeInteger(p.y) || !Number.isSafeInteger(p.z) ||
        !tree.profile.leaves.includes(p.name) || p.y < tree.root.y + 1 || p.y > tree.root.y + tree.profile.height ||
        !withinEnvelope(p)) return { ok: false, reason: 'forestry record contains leaves outside this tree profile' }
  }
  // Do not let a stale record authorize new wood or silently overwrite a
  // block placed where an original log used to be.
  for (const p of record.wood) {
    const current = worldAt(p.x, p.y, p.z)
    if (!current || (!isAir(current.name) && current.name !== p.name)) return { ok: false, reason: `recorded tree cell changed at ${key(p)}` }
  }
  const recorded = new Map(record.wood.map(p => [key(p), p.name]))
  for (const p of tree.wood) if (recorded.get(key(p)) !== p.name) return { ok: false, reason: `unrecorded wood at ${key(p)} is outside the saved harvest` }
  // Also refuse disconnected matching logs elsewhere in the scan envelope.
  for (let dx = -tree.profile.radius; dx <= tree.profile.radius + tree.profile.width - 1; dx++) {
    for (let dz = -tree.profile.radius; dz <= tree.profile.radius + tree.profile.width - 1; dz++) {
      for (let dy = 1; dy <= tree.profile.height; dy++) {
        const p = { x: tree.root.x + dx, y: tree.root.y + dy, z: tree.root.z + dz }
        const b = worldAt(p.x, p.y, p.z)
        if (b && nameIn(b.name, tree.profile) && recorded.get(key(p)) !== b.name) return { ok: false, reason: `unrecorded wood at ${key(p)} is outside the saved harvest` }
      }
    }
  }
  const blocks = new Map(tree.blocks.map(p => [key(p), p]))
  const wood = new Map(tree.wood.map(p => [key(p), p]))
  for (const p of record.wood) {
    const b = worldAt(p.x, p.y, p.z)
    if (b?.name === p.name) {
      const item = { ...p, properties: b.properties ?? {} }
      blocks.set(key(p), item)
      wood.set(key(p), item)
    }
  }
  const resumed = { ...tree, blocks: [...blocks.values()], wood: [...wood.values()] }
  if (resumed.wood.length) resumed.state = 'mature'
  const occupiedOutsideRecord = () => {
    const height = tree.profile.height, radius = tree.profile.radius, width = tree.profile.width
    for (let dx = -radius; dx <= radius + width - 1; dx++) for (let dz = -radius; dz <= radius + width - 1; dz++) for (let dy = 2; dy <= height; dy++) {
      const p = { x: tree.root.x + dx, y: tree.root.y + dy, z: tree.root.z + dz }
      if (recorded.has(key(p))) continue
      const b = worldAt(p.x, p.y, p.z)
      const decaying = b && tree.profile.leaves.includes(b.name) && b.name.endsWith('_leaves') && b.properties?.persistent !== true && b.properties?.persistent !== 'true'
      if (b && !isAir(b.name) && !harmlessPlant(b.name) && !decaying) return true
    }
    return false
  }
  resumed.attention = resumed.attention.filter(s =>
    s !== 'wood has no attributable canopy: possible build or incomplete tree; inspect manually' &&
    !s.startsWith('off-trunk wood without nearby matching canopy may be a build:') &&
    !(/^\d+ occupied cells in conservative growth envelope/.test(s) && !occupiedOutsideRecord()))
  return { ok: true, tree: resumed }
}

export const forestryRecordMatchesSite = (record, tree) => Boolean(record && sameRoot(record.root, tree.root) &&
  record.species === tree.species && record.form === tree.form)

export function legacyPlannedStumpRefusal (tree, cells) {
  if (!tree || tree.state !== 'mature' || tree.leaves.length || !tree.wood.length || tree.wood.length > (tree.form === 'single' ? 3 : tree.footprint.length * 2)) return 'not a mature canopy-free stump within the small-stump log limit'
  const planned = cells.find(c => c.x === tree.root.x && c.y === tree.root.y && c.z === tree.root.z)
  const mapped = planSpec(planned), spec = treeSpec(planned)
  if (!spec || !['tree', 'sapling'].includes(mapped?.kind)) return `no mapped tree planting cell at root ${key(tree.root)}`
  if (spec.species !== tree.species) return `mapped site species ${spec.species} does not match observed ${tree.species}`
  if (spec.form && spec.form !== 'auto' && spec.form !== tree.form) return `mapped site form ${spec.form} does not match observed ${tree.form}`
  const stack = new Map()
  for (const p of tree.wood) {
    const rootedSingle = tree.form === 'single' && p.x === tree.root.x && p.z === tree.root.z
    const maxY = tree.root.y + (tree.form === 'single' ? 3 : 2)
    if (!tree.footprint.some(f => f.x === p.x && f.z === p.z) || (tree.form === 'single' && !rootedSingle) || p.y < tree.root.y + 1 || p.y > maxY || !tree.profile.wood.includes(p.name)) return `observed wood at ${key(p)} is outside the first ${tree.form === 'single' ? 'three rooted' : 'two footprint'} trunk blocks`
    const k = `${p.x},${p.z}`
    const ys = stack.get(k) ?? new Set()
    ys.add(p.y); stack.set(k, ys)
  }
  const maxStack = tree.form === 'single' ? 3 : 2
  const broken = [...stack.entries()].find(([, ys]) => ys.size > maxStack || [...ys].some(y => !ys.has(y - 1) && y !== tree.root.y + 1))
  if (broken) return `observed trunk column ${broken[0]} is not a contiguous one- to ${maxStack}-log stump`
  // The only acceptable diagnostics here are the missing canopy signal. A
  // boundary, protected block, neighboring trunk, or adjoining build remains
  // an explicit stop even on a mapped site.
  const other = tree.attention.find(s => s !== 'wood has no attributable canopy: possible build or incomplete tree; inspect manually')
  if (other) return `inspection also found: ${other}`
  return null
}

export const isLegacyPlannedStump = (tree, cells) => legacyPlannedStumpRefusal(tree, cells) === null
