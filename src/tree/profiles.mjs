// Explicit planting forms. Envelopes are conservative operating space, not
// Minecraft's minimum growth algorithm. Inspect never assumes an unloaded cell is air.
const profile = (species, forms, radius, height, extra = {}) => ({ species, forms, radius, height, plant: `${species}_sapling`, wood: [`${species}_log`], leaves: [`${species}_leaves`], soil: ['dirt', 'grass_block', 'coarse_dirt', 'podzol', 'rooted_dirt', 'moss_block'], ...extra })
export const TREE_PROFILES = {
  oak: profile('oak', ['single'], 5, 18),
  birch: profile('birch', ['single'], 3, 14),
  spruce: profile('spruce', ['single', 'large'], 6, 36),
  jungle: profile('jungle', ['single', 'large'], 7, 40),
  acacia: profile('acacia', ['single'], 7, 16),
  dark_oak: profile('dark_oak', ['large'], 6, 16),
  pale_oak: profile('pale_oak', ['large'], 6, 16),
  cherry: profile('cherry', ['single'], 8, 18),
  mangrove: profile('mangrove', ['single'], 8, 28, { plant: 'mangrove_propagule', roots: ['mangrove_roots', 'muddy_mangrove_roots'], soil: ['dirt', 'grass_block', 'coarse_dirt', 'podzol', 'rooted_dirt', 'moss_block', 'mud', 'clay'] }),
  azalea: profile('azalea', ['single'], 5, 14, { plant: 'azalea', wood: ['oak_log'], leaves: ['azalea_leaves', 'flowering_azalea_leaves'], requiresMeal: true }),
  crimson: profile('crimson', ['single'], 5, 30, { plant: 'crimson_fungus', wood: ['crimson_stem'], leaves: ['nether_wart_block', 'shroomlight'], soil: ['crimson_nylium'], requiresMeal: true }),
  warped: profile('warped', ['single'], 5, 30, { plant: 'warped_fungus', wood: ['warped_stem'], leaves: ['warped_wart_block', 'shroomlight'], soil: ['warped_nylium'], requiresMeal: true })
}
export function treeProfile (species, form = 'auto') {
  const p = TREE_PROFILES[String(species ?? '').replace(/^minecraft:/, '')]
  if (!p) throw new Error(`unknown tree species ${species}; supported: ${Object.keys(TREE_PROFILES).join(', ')}`)
  if (form === 'auto') form = p.forms[0]
  if (!p.forms.includes(form)) throw new Error(`invalid ${p.species} tree form ${form}; expected ${p.forms.join(' or ')}`)
  return { ...p, form, width: form === 'large' ? 2 : 1 }
}
export const treePlantProfile = item => Object.values(TREE_PROFILES).find(p => p.plant === item || (p.species === 'azalea' && item === 'flowering_azalea'))
export const treeFootprint = (root, p) => Array.from({ length: p.width ** 2 }, (_, i) => ({ x: root.x + i % p.width, y: root.y + 1, z: root.z + Math.floor(i / p.width) }))
