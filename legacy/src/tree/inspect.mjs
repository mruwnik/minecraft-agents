import { treeProfile, TREE_PROFILES, treeFootprint, treePlantProfile } from './profiles.mjs'
import { planSpec } from '../lib/plan.mjs'
import { isAir, isGroundCover } from '../lib/world.mjs'
import { digFromHere } from '../lib/dig.mjs'
import { standingSpots } from '../navigation/stand.mjs'

export const key = p => `${p.x},${p.y},${p.z}`
export const treeSpec = cell => {
  const s = planSpec(cell)
  return s?.kind === 'tree' ? s : s?.kind === 'sapling' && treePlantProfile(s.item) ? { ...s, species: treePlantProfile(s.item).species, form: 'auto' } : null
}
export const treeRoot = a => {
  if (!['x', 'y', 'z'].every(k => Number.isSafeInteger(a[k]))) throw new Error('tree coordinates must be finite integers at ground level: x= y= z=')
  return { x: a.x, y: a.y, z: a.z }
}
const NATURAL = /^(?:air|cave_air|void_air|vine|weeping_vines|weeping_vines_plant|twisting_vines|twisting_vines_plant|pale_hanging_moss|resin_clump)$/
export const treePart = name => /_(?:log|wood|leaves|hyphae)$/.test(name) || ['crimson_stem', 'warped_stem', 'mangrove_roots', 'muddy_mangrove_roots', 'nether_wart_block', 'warped_wart_block', 'shroomlight'].includes(name)
const FLOWERS = new Set(['dandelion', 'poppy', 'blue_orchid', 'allium', 'azure_bluet', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'torchflower', 'pink_petals', 'wildflowers'])
export const harmlessPlant = name => isGroundCover(name) || FLOWERS.has(name) || name === 'bush'
const PROTECTED = new Set(['bee_nest', 'beehive', 'creaking_heart'])
const neighbors = p => [-1, 0, 1].flatMap(dx => [-1, 0, 1].flatMap(dy => [-1, 0, 1].filter(dz => dx || dy || dz).map(dz => ({ x: p.x + dx, y: p.y + dy, z: p.z + dz }))))
export function inspectTree (worldAt, root, species, form = 'auto') {
  const base = worldAt(root.x, root.y + 1, root.z)
  if (!species) {
    species = treePlantProfile(base?.name)?.species
    if (!species) species = Object.values(TREE_PROFILES).find(p => p.wood.includes(base?.name) || p.roots?.includes(base?.name))?.species
  }
  if (!species) return { root, state: base ? 'unknown' : 'unloaded', attention: [`cannot identify tree at ${key(root)}; specify species and load the planting site`], blocks: [], wood: [], leaves: [] }
  if (form === 'auto' && ['spruce', 'jungle'].includes(species)) {
    const p = TREE_PROFILES[species]
    form = [[0, 0], [1, 0], [0, 1], [1, 1]].every(([dx, dz]) => [p.plant, ...p.wood].includes(worldAt(root.x + dx, root.y + 1, root.z + dz)?.name)) ? 'large' : 'single'
  }
  const p = treeProfile(species, form)
  const footprint = treeFootprint(root, p)
  const attention = []
  const footprintKeys = new Set(footprint.map(key))
  const candidates = new Map()
  const protectedBlocks = []
  let unloaded = 0
  for (let dx = -p.radius; dx <= p.radius + p.width - 1; dx++) for (let dz = -p.radius; dz <= p.radius + p.width - 1; dz++) for (let dy = 1; dy <= p.height; dy++) {
    const pos = { x: root.x + dx, y: root.y + dy, z: root.z + dz }
    const b = worldAt(pos.x, pos.y, pos.z)
    if (!b) { unloaded++; continue }
    if (PROTECTED.has(b.name)) protectedBlocks.push({ ...pos, name: b.name })
    if ([...p.wood, ...p.leaves, ...(p.roots ?? [])].includes(b.name)) candidates.set(key(pos), { ...pos, name: b.name, properties: b.properties ?? {} })
  }
  const woodNames = new Set([...p.wood, ...(p.roots ?? [])])
  const queue = footprint.map(pos => candidates.get(key(pos))).filter(Boolean)
  const reached = new Map()
  while (queue.length) {
    const b = queue.pop()
    if (reached.has(key(b))) continue
    reached.set(key(b), b)
    for (const n of neighbors(b)) if (candidates.has(key(n)) && !reached.has(key(n))) queue.push(candidates.get(key(n)))
  }
  const blocks = [...reached.values()]
  const wood = blocks.filter(b => woodNames.has(b.name))
  const leaves = blocks.filter(b => !woodNames.has(b.name))
  const planted = footprint.every(pos => [p.plant, ...(p.species === 'azalea' ? ['flowering_azalea'] : [])].includes(worldAt(pos.x, pos.y, pos.z)?.name))
  const empty = footprint.every(pos => { const b = worldAt(pos.x, pos.y, pos.z); return b && (isAir(b.name) || isGroundCover(b.name)) })
  const compatible = footprint.every(pos => { const b = worldAt(pos.x, pos.y, pos.z); return b && (isAir(b.name) || isGroundCover(b.name) || b.name === p.plant || (p.species === 'azalea' && b.name === 'flowering_azalea')) })
  const state = planted || (compatible && !empty) ? 'sapling' : wood.length ? 'mature' : empty ? 'empty' : 'blocked'
  if (unloaded) attention.push(`${unloaded} unloaded cells in tree envelope; load the whole site before work`)
  if (protectedBlocks.length) attention.push(`protected ${protectedBlocks.map(b => `${b.name}@${key(b)}`).join(' ')}; retain this tree`)
  if (wood.length && !leaves.length) attention.push('wood has no attributable canopy: possible build or incomplete tree; inspect manually')
  if (blocks.some(b => b.properties.persistent === true || b.properties.persistent === 'true')) attention.push('persistent placed leaves indicate a build; inspect manually')
  if (wood.some(b => !footprintKeys.has(key(b)) && !woodNames.has(worldAt(b.x, b.y - 1, b.z)?.name) && p.soil.includes(worldAt(b.x, b.y - 1, b.z)?.name) && !p.roots?.includes(b.name))) attention.push('connected neighboring trunk outside planting footprint; separate ownership is ambiguous')
  if (blocks.some(b => b.y === root.y + p.height || b.x === root.x - p.radius || b.x === root.x + p.radius + p.width - 1 || b.z === root.z - p.radius || b.z === root.z + p.radius + p.width - 1)) attention.push('tree reaches scan boundary; whole tree cannot be safely attributed')
  const built = new Map()
  for (const b of wood) for (const n of neighbors(b)) {
    if (n.y <= root.y || reached.has(key(n))) continue
    const other = worldAt(n.x, n.y, n.z)
    if (other && !NATURAL.test(other.name) && !treePart(other.name) && !harmlessPlant(other.name) && !footprintKeys.has(key(n)) && !PROTECTED.has(other.name)) built.set(key(n), { ...n, name: other.name })
  }
  const unsupported = wood.filter(b => !p.roots?.includes(b.name) && !footprint.some(f => f.x === b.x && f.z === b.z) && !leaves.some(l => Math.hypot(l.x - b.x, l.y - b.y, l.z - b.z) <= 6))
  if (unsupported.length) attention.push(`off-trunk wood without nearby matching canopy may be a build: ${unsupported.slice(0, 8).map(key).join(' ')}`)
  if (built.size) attention.push(`blocks adjoining trunk need inspection: ${[...built.values()].slice(0, 8).map(b => `${b.name}@${key(b)}`).join(' ')}`)
  return { root, species: p.species, form: p.form, profile: p, state, footprint, blocks, wood, leaves, protected: protectedBlocks, attention }
}

export function checkTree (worldAt, root, species, form, cells = []) {
  const tree = inspectTree(worldAt, root, species, form)
  if (!tree.profile) return tree
  const { profile: p } = tree
  const attention = [...tree.attention]
  for (const pos of tree.footprint) {
    const ground = worldAt(pos.x, root.y, pos.z)
    if (!ground || !p.soil.includes(ground.name)) attention.push(`needs ${p.soil.join('/')} at ${pos.x},${root.y},${pos.z}; found ${ground?.name ?? 'unloaded'}`)
    const cell = cells.find(c => c.x === pos.x && c.z === pos.z && !(c.x === root.x && c.z === root.z))
    if (cell && planSpec(cell)?.kind !== 'reserved') attention.push(`planting footprint conflicts with planned ${cell.ch} at ${key(cell)}`)
  }
  if (['empty', 'sapling'].includes(tree.state)) {
    let blocked = 0
    for (let dx = -p.radius; dx <= p.radius + p.width - 1; dx++) for (let dz = -p.radius; dz <= p.radius + p.width - 1; dz++) for (let dy = 2; dy <= p.height; dy++) {
      const b = worldAt(root.x + dx, root.y + dy, root.z + dz)
      const decaying = b && p.leaves.includes(b.name) && b.name.endsWith('_leaves') && b.properties?.persistent !== true && b.properties?.persistent !== 'true'
      if (b && !isAir(b.name) && !harmlessPlant(b.name) && !decaying) blocked++
    }
    if (blocked) attention.push(`${blocked} occupied cells in conservative growth envelope (radius ${p.radius}, height ${p.height}); choose wider spacing or clear authorized obstructions`)
  }
  for (const cell of cells) {
    const other = treeSpec(cell)
    if (!other || (cell.x === root.x && cell.z === root.z)) continue
    const q = treeProfile(other.species, other.form)
    if (Math.abs(cell.x - root.x) <= p.radius + q.radius && Math.abs(cell.z - root.z) <= p.radius + q.radius) attention.push(`growth envelope overlaps tree at ${key(cell)}; widen spacing`)
  }
  return { ...tree, attention: [...new Set(attention)] }
}

export function harvestStands (tree, worldAt) {
  const removed = new Set([...tree.wood,...tree.leaves].map(key))
  const at = (x, y, z) => {
    const b = worldAt(x, y, z)
    if (!b) return null
    // Never rely on a leaf, root or log that harvesting will remove as a floor.
    if (removed.has(`${x},${y},${z}`)) return { ...b, solid: false }
    return { ...b, liquid: ['water', 'lava'].includes(b.name), crop: /sapling|propagule/.test(b.name) }
  }
  return new Map(tree.blocks.map(b => [key(b), standingSpots({ target: b, blockAt: at, range: 4, see: false }).filter(s => digFromHere({ x: s.x + .5, y: s.y, z: s.z + .5 }, b) && !removed.has(key(s)) && !removed.has(`${s.x},${s.y + 1},${s.z}`) && !removed.has(`${s.x},${s.y - 1},${s.z}`) && !['composter', 'water', 'lava'].includes(worldAt(s.x, s.y - 1, s.z)?.name))]))
}
