// The blueprint library page's pure half, shared by the page and the tests: what a list row says, the cells of one
// layer with their colours and labels, the legend, the bill as rows with a bar each, and what lint said as lines.
// Like map.mjs it has no node imports (the browser loads it at /blueprint.mjs): its inputs are the plain data the
// server hands out from /api/blueprints - a parsed, resolved blueprint (src/blueprint/format.mjs), its bill and its lint.
// Colours follow the material: every role of one wood or stone is a shade of that material's hue, read off the same
// family table the parser resolves {wood:planks} through, so a hut's planks, logs, stairs and fence read as one thing.
import { WOODS, WOOD_ROLES, STONES, STONE_ROLES, familyName } from '../../src/build/materials.mjs'
import { worldColour } from './map.mjs'

const isAir = name => /^(air|cave_air|void_air)$/.test(String(name))
const primary = spec => spec.alts[0]

// ---------------------------------------------------------------- the palette

const DYES = ['white', 'light_gray', 'gray', 'black', 'brown', 'red', 'orange', 'yellow', 'lime', 'green', 'cyan', 'light_blue', 'blue', 'purple', 'magenta', 'pink']
const DYE_ROLES = ['bed', 'wool', 'carpet', 'concrete', 'concrete_powder', 'terracotta', 'glazed_terracotta', 'stained_glass', 'stained_glass_pane', 'banner', 'candle', 'shulker_box']
const EXTRA_WOOD = { wood: v => `${v}_wood`, stripped_wood: v => `stripped_${v}_wood` }

// block name -> {family, value, role}, built once from the tables src/build/materials.mjs resolves roles through
const FAMILY_OF = new Map()
WOODS.forEach(value => WOOD_ROLES.forEach(role => FAMILY_OF.set(familyName('wood', value, role), { family: 'wood', value, role })))
WOODS.forEach(value => Object.entries(EXTRA_WOOD).forEach(([role, name]) => FAMILY_OF.set(name(value), { family: 'wood', value, role })))
STONES.forEach(value => STONE_ROLES.forEach(role => FAMILY_OF.set(familyName('stone', value, role), { family: 'stone', value, role })))
DYES.forEach(value => DYE_ROLES.forEach(role => FAMILY_OF.set(`${value}_${role}`, { family: 'dye', value, role })))
export const familyOf = name => FAMILY_OF.get(String(name)) ?? null

const WOOD_TINT = {
  oak: '#b08a55', spruce: '#755433', birch: '#d7c99a', jungle: '#a67a4f', acacia: '#b5652e', dark_oak: '#4d3620',
  mangrove: '#7a3a34', cherry: '#e2a5b3', pale_oak: '#d9d0c0', bamboo: '#d1c26a', crimson: '#7d3c5a', warped: '#2f8c85'
}
// planks are the material's own tint; logs darker, stripped logs lighter, the thin roles a little dimmer
const WOOD_SHADE = { planks: 1, log: 0.72, wood: 0.72, stripped_log: 1.15, stripped_wood: 1.15, slab: 0.93, stairs: 0.93, fence: 0.84, fence_gate: 0.88, door: 0.8, trapdoor: 0.9, button: 1, pressure_plate: 1, sign: 1 }
const STONE_TINT = {
  cobblestone: '#8a8a8a', mossy_cobblestone: '#7a8a6a', stone: '#7a7f88', stone_bricks: '#6f747c', mossy_stone_bricks: '#6a7a68', bricks: '#9a5a4a',
  deepslate_bricks: '#3e4247', cobbled_deepslate: '#474b50', polished_deepslate: '#3a3d42', andesite: '#8c8e8a', polished_andesite: '#9a9c98',
  granite: '#9a6a5a', polished_granite: '#a87868', diorite: '#c9c9c6', polished_diorite: '#d6d6d3', sandstone: '#d6c99a', red_sandstone: '#b8653a',
  blackstone: '#2b2b2e', polished_blackstone: '#333338', polished_blackstone_bricks: '#3a3a40', mud_bricks: '#8a6a4e', tuff: '#6c6e66', tuff_bricks: '#767870',
  nether_bricks: '#4a2a30', end_stone_bricks: '#d8d9a6', quartz_block: '#e8e4dc', smooth_stone: '#9a9ea6', prismarine: '#5f9a90'
}
const STONE_SHADE = { block: 1, slab: 0.93, stairs: 0.93, wall: 0.86 }
const DYE_TINT = {
  white: '#e9ecec', light_gray: '#8e8e86', gray: '#3e4447', black: '#1d1d21', brown: '#835432', red: '#b02e26', orange: '#f9801d', yellow: '#fed83d',
  lime: '#80c71f', green: '#5e7c16', cyan: '#169c9c', light_blue: '#3ab3da', blue: '#3c44aa', purple: '#8932b8', magenta: '#c74ebd', pink: '#f38baa'
}
// blocks with a look of their own: lights warm, glass pale, the workstations and containers the browns they are
const FIXED = {
  torch: '#e0a030', wall_torch: '#e0a030', soul_torch: '#5fc7c7', lantern: '#e0a030', soul_lantern: '#5fc7c7', jack_o_lantern: '#e08a2e', campfire: '#e0641e',
  glowstone: '#f2d16b', sea_lantern: '#bfe6e0', glass: '#a9d6e8', glass_pane: '#a9d6e8', ladder: '#a67a4f', chest: '#a0754a', trapped_chest: '#a0754a',
  barrel: '#8a6a44', crafting_table: '#8f6b3f', furnace: '#6d7076', smoker: '#5c4a3a', blast_furnace: '#4f5560', composter: '#7a5c3a', bookshelf: '#a0754a',
  iron_bars: '#8c949c', iron_block: '#d8d8d8', iron_door: '#c8c8c8', iron_trapdoor: '#c8c8c8', hay_block: '#c9a542', bell: '#e0b842', anvil: '#3e4247',
  '@solid': '#4b5462'
}
const hex2 = n => Math.max(0, Math.min(255, Math.round(n))).toString(16).padStart(2, '0')
const shade = (hex, k) => `#${[1, 3, 5].map(i => hex2(parseInt(hex.slice(i, i + 2), 16) * k)).join('')}`

// null for air (drawn as nothing); a block nobody listed still gets map.mjs's stable muted colour
export const blockColour = name => {
  if (isAir(name)) return null
  if (FIXED[name]) return FIXED[name]
  const f = familyOf(name)
  if (f?.family === 'wood') return shade(WOOD_TINT[f.value], WOOD_SHADE[f.role] ?? 1)
  if (f?.family === 'stone') return shade(STONE_TINT[f.value], STONE_SHADE[f.role] ?? 1)
  if (f?.family === 'dye') return DYE_TINT[f.value]
  return worldColour(name)
}

// a waterlogged block is mostly water to the eye: a covered channel of slabs reads as the water it carries
const WATER = '#4a90d9'
const mix = (a, b, t) => `#${[1, 3, 5].map(i => hex2(parseInt(a.slice(i, i + 2), 16) * (1 - t) + parseInt(b.slice(i, i + 2), 16) * t)).join('')}`
export const altColour = alt => {
  const base = blockColour(alt.name)
  if (base === null || alt.states?.waterlogged !== 'true' || !base.startsWith('#')) return base
  return mix(base, WATER, 0.7)
}

// ---------------------------------------------------------------- cells, legend, hover

const statesText = states => {
  const pairs = Object.entries(states ?? {})
  return pairs.length ? `[${pairs.map(([k, v]) => `${k}=${v}`).join(',')}]` : ''
}
// the block a token stands for, written the way the legend line wrote it; @solid in words
export const cellLabel = spec => {
  const alt = primary(spec)
  return alt.name === '@solid' ? 'any solid block' : `${alt.name}${statesText(alt.states)}`
}

// every cell of layer y that is part of the blueprint (a _ cell is not), west to east then north to south, with the
// block it asks for, its colour and its label; air cells are kept (a room is drawn as the space it leaves)
export const layerCells = (bp, y) => {
  const layer = bp.layers.find(l => l.y === y)
  if (!layer) return []
  return layer.grid.flatMap((row, dz) => [...row].flatMap((token, dx) => {
    if (token === '_') return []
    const spec = bp.legend[token]
    const name = primary(spec).name
    return [{ dx, dz, y, token, name, label: cellLabel(spec), colour: altColour(primary(spec)), air: isAir(name) }]
  }))
}

// what the page says under the cursor: the cell's offset from the anchor (x east, z south), then the block
export const hoverText = cell => `x+${cell.dx} y${cell.y} z+${cell.dz} · ${cell.label} (${cell.token})`

// the tokens the layers actually use, first met from the lowest layer up, with how many cells each fills; air and _
// are not blocks to fetch and are left out
export const legendRows = bp => {
  const rows = new Map()
  const layers = [...bp.layers].sort((a, b) => a.y - b.y)
  layers.forEach(layer => layerCells(bp, layer.y).forEach(cell => {
    if (cell.air) return
    const row = rows.get(cell.token) ?? { token: cell.token, name: cell.name, label: cell.label, colour: cell.colour, tags: bp.legend[cell.token].tags ?? [], count: 0 }
    row.count += 1
    rows.set(cell.token, row)
  }))
  return [...rows.values()]
}

// ---------------------------------------------------------------- the bill and the list

// items most needed first (ties by name), each with its share of the largest count, for a bar beside the number
export const billRows = items => {
  const rows = Object.entries(items ?? {}).map(([item, count]) => ({ item, count }))
  const max = Math.max(0, ...rows.map(r => r.count))
  return rows.sort((a, b) => b.count - a.count || a.item.localeCompare(b.item)).map(r => ({ ...r, share: r.count / max }))
}

export const blockCount = bill => Object.values(bill?.total ?? {}).reduce((a, b) => a + b, 0)
const height = bp => bp.layers.at(-1).y - bp.layers[0].y + 1
export const footprint = bp => `${bp.width}x${bp.depth}x${height(bp)}`

const statusOf = detail => {
  if (!detail.bp) return 'does not parse'
  if (detail.lint.errors.length) return 'build refuses'
  const n = detail.lint.warnings.length
  return n ? `${n} warning${n === 1 ? '' : 's'}` : 'ok'
}

// one row of the list: a file that does not parse still gets one, so the list shows the library as it is on disk
export const blueprintRow = detail => {
  const { name, bp } = detail
  const builds = detail.builds?.length ?? 0
  if (!bp) return { name, title: '', kind: '', footprint: '', layers: 0, blocks: 0, status: 'does not parse', builds }
  return { name, title: bp.title, kind: bp.tags.join(', '), footprint: footprint(bp), layers: bp.layers.length, blocks: blockCount(detail.bill), status: statusOf(detail), builds }
}

// what the parser and lint said, as lines with a level: parse (the file is not a blueprint), error (build refuses),
// warning, or one ok line when there is nothing to say
export const lintLines = detail => {
  if (!detail.bp) return detail.errors.map(text => ({ level: 'parse', text }))
  const lines = [...detail.lint.errors.map(text => ({ level: 'error', text })), ...detail.lint.warnings.map(text => ({ level: 'warning', text }))]
  return lines.length ? lines : [{ level: 'ok', text: 'lint has nothing to say: build accepts it' }]
}
