// The family table a blueprint's material parameters look roles up in: `{wood:log}` with wood=crimson is
// crimson_stem, `{stone:stairs}` with stone=stone_bricks is stone_brick_stairs. Names are irregular enough that a
// table beats a rule, and one table shared by every blueprint beats one guessed per file. A role a value lacks (stone
// has no wall) is refused with the values that do have it, so the fix is in the sentence.

export const WOODS = ['oak', 'spruce', 'birch', 'jungle', 'acacia', 'dark_oak', 'mangrove', 'cherry', 'pale_oak', 'bamboo', 'crimson', 'warped']
export const WOOD_ROLES = ['planks', 'log', 'stripped_log', 'slab', 'stairs', 'fence', 'fence_gate', 'door', 'trapdoor', 'button', 'pressure_plate', 'sign']
// the stones a house is likely to be built of; any block the registry knows works as `block`, these also have cousins
export const STONES = ['cobblestone', 'mossy_cobblestone', 'stone', 'stone_bricks', 'mossy_stone_bricks', 'bricks', 'deepslate_bricks',
  'cobbled_deepslate', 'polished_deepslate', 'andesite', 'polished_andesite', 'granite', 'polished_granite', 'diorite', 'polished_diorite',
  'sandstone', 'red_sandstone', 'blackstone', 'polished_blackstone', 'polished_blackstone_bricks', 'mud_bricks', 'tuff', 'tuff_bricks',
  'nether_bricks', 'end_stone_bricks', 'quartz_block', 'smooth_stone', 'prismarine']
export const STONE_ROLES = ['block', 'slab', 'stairs', 'wall']

const LOG_OF = { crimson: 'crimson_stem', warped: 'warped_stem', bamboo: 'bamboo_block' }
const STRIPPED_OF = { crimson: 'stripped_crimson_stem', warped: 'stripped_warped_stem', bamboo: 'stripped_bamboo_block' }

// the stem a stone's cousins are named from: stone_bricks -> stone_brick_slab, quartz_block -> quartz_stairs
const stoneStem = value => value.replace(/_block$/, '').replace(/bricks$/, 'brick')

// the candidate name for a role, before the registry is asked whether it exists
export const familyName = (family, value, role) => {
  if (family === 'wood') {
    if (role === 'log') return LOG_OF[value] ?? `${value}_log`
    if (role === 'stripped_log') return STRIPPED_OF[value] ?? `stripped_${value}_log`
    return `${value}_${role}`
  }
  if (family === 'stone') return role === 'block' ? value : `${stoneStem(value)}_${role}`
  return null
}

export const FAMILIES = { wood: { values: WOODS, roles: WOOD_ROLES }, stone: { values: STONES, roles: STONE_ROLES } }

// the block a role resolves to for this value, or null when the registry has no such block
export const familyBlock = (family, value, role, registry) => {
  const name = familyName(family, value, role)
  return name && registry.blocksByName[name] ? name : null
}

// null when the role resolves, else the refusal naming the values that have it
export const familyRefusal = (family, value, role, registry) => {
  const known = FAMILIES[family]
  if (!known) return `${family} is not a material family: use ${Object.keys(FAMILIES).join(' or ')}`
  if (!known.roles.includes(role)) return `${family} has no role ${role}: use ${known.roles.join(', ')}`
  if (familyBlock(family, value, role, registry)) return null
  const have = known.values.filter(v => familyBlock(family, v, role, registry))
  return `${value} has no ${role}: use ${have.slice(0, 4).join(', ')}${have.length > 4 ? ' or another that has one' : ''} for ${family}=`
}
