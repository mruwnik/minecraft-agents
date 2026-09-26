// The blueprint format (docs/superpowers/specs/2026-09-26-blueprint-format-design.md), the pure half: a Markdown file
// is parsed into layers of one-character tokens over a legend of setblock-style blocks, resolved with material
// parameters through src/materials.mjs, turned by facing=, costed, cut into stages that fit the inventory, and judged
// against a world (what stands in the way, what is still missing, where the body stands for each job). Nothing here
// walks, digs, reads a file or writes one: src/blueprint-build.mjs does the walking and library/blueprint/* the I/O.
import { createHash } from 'node:crypto'
import minecraftData from 'minecraft-data'
import { familyRefusal, familyBlock, FAMILIES } from './materials.mjs'

// the client's registry: what this body can name and place (the server is one protocol ahead; see SERVER_ONLY)
export const REGISTRY = minecraftData('26.1')
// the 28 blocks the 26.2 server has and the 26.1 client cannot name (the sulfur and cinnabar families and the spike)
const SERVER_ONLY = new Set(['sulfur', 'potent_sulfur', 'polished_sulfur', 'sulfur_bricks', 'chiseled_sulfur', 'cinnabar', 'polished_cinnabar', 'cinnabar_bricks', 'chiseled_cinnabar', 'sulfur_spike']
  .flatMap(n => [n, `${n}_slab`, `${n}_stairs`, `${n}_wall`]))

export const TAGS = ['shelter', 'storage', 'farm', 'pen', 'lookout', 'workshop', 'decorative', 'bridge']
export const DIRS = ['north', 'east', 'south', 'west']
const STEP = { north: { dx: 0, dz: -1 }, east: { dx: 1, dz: 0 }, south: { dx: 0, dz: 1 }, west: { dx: -1, dz: 0 } }
const OPPOSITE = { north: 'south', south: 'north', east: 'west', west: 'east' }
const FRONT_KEYS = { name: true, title: true, description: true, tags: true, front: true, foundation: true, clearance: false, params: false, source: false, license: false, difficulty: false, notes: false, by: false }
const FOUNDATIONS = ['flat', 'any', 'dug']
const LIMITS = { side: 64, minY: -4, maxY: 47, cells: 16384 }
// the eyes are 1.62 over the feet and the arm reaches 4.5 from them (src/stand.mjs, bot.mjs DIG_REACH)
export const EYE = 1.62
export const REACH = 4.5
// the margin of free slots a build never fills: the drops of a dig, a tool swap
export const CARRY_MARGIN = 2
// a pillar for a layer no floor reaches: one per this much perimeter, of the scaffold= parameter
export const PILLAR_PER = 8
export const DEFAULT_SCAFFOLD = 'dirt'
export const DEFAULT_FILL = 'dirt'

const key = (x, y, z) => `${x},${y},${z}`
const cellName = (dx, dz) => `${dx},${dz}`

// ---------------------------------------------------------------- what the registry says about a block

export const blockInfo = (name, registry = REGISTRY) => registry.blocksByName[name] ?? null
const PARTIAL = /(_slab|_stairs|_fence|_wall|_fence_gate|_pane|_trapdoor|_bars|_door|_bed|chest)$|^(chain|end_rod|iron_bars|lantern|soul_lantern|bell|grindstone|anvil|chipped_anvil|damaged_anvil|lectern|composter|hopper|cauldron|water_cauldron|enchanting_table|brewing_stand|stonecutter|daylight_detector|bamboo|cactus|campfire|soul_campfire|scaffolding|conduit|lightning_rod)$/
const DOORLIKE = /(_door|_fence_gate|_trapdoor)$/
const CONTAINER = /^(chest|trapped_chest|barrel|ender_chest|hopper|dispenser|dropper|furnace|blast_furnace|smoker|brewing_stand|shulker_box|.*_shulker_box|decorated_pot|chiseled_bookshelf|lectern|jukebox|crafter)$/
const WORKSTATION = /^(crafting_table|furnace|blast_furnace|smoker|smithing_table|anvil|grindstone|stonecutter|loom|cartography_table|fletching_table|composter|brewing_stand|enchanting_table|lectern|barrel|cauldron|bell)$/
const GRAVITY = /^(sand|red_sand|gravel|suspicious_sand|suspicious_gravel|.*_concrete_powder|anvil|chipped_anvil|damaged_anvil|dragon_egg|scaffolding)$/
const CROP = /^(wheat|carrots|potatoes|beetroots|melon_stem|pumpkin_stem|attached_melon_stem|attached_pumpkin_stem|torchflower_crop|pitcher_crop)$/
const WALL_HUNG = /^(wall_torch|soul_wall_torch|redstone_wall_torch|ladder|tripwire_hook|.*_wall_sign|.*_wall_hanging_sign|.*_wall_banner|.*_wall_fan|.*_wall_skull|.*_wall_head|vine|glow_lichen|sculk_vein)$/
const FRONT_FACING = /^(chest|trapped_chest|ender_chest|furnace|blast_furnace|smoker|barrel|dispenser|dropper|observer|lectern|loom|stonecutter|grindstone|beehive|bee_nest|jack_o_lantern|carved_pumpkin|end_portal_frame|hopper|crafter|vault|trial_spawner|chiseled_bookshelf|decorated_pot|bell|campfire|soul_campfire|anvil|chipped_anvil|damaged_anvil)$/
const LOOK_TOWARD = /(_stairs|_door|_bed|_fence_gate|_trapdoor)$/
export const isAir = name => /^(air|cave_air|void_air)$/.test(String(name))
export const FLUIDS = new Set(['water', 'lava', 'flowing_water', 'flowing_lava', 'bubble_column'])
// blocks somebody put there, as src/lib/world.mjs judges them: never dug without clear=true
const BUILT = /(^|_)(cobblestone|planks|fence|gate|wall|door|trapdoor|bed|stairs|slab|glass|pane|wool|carpet|torch|lantern|chest|barrel|furnace|smoker|table|farmland|bricks|ladder|sign|banner|rail|hopper|composter|campfire|anvil|bookshelf|concrete|terracotta)$|^(wheat|carrots|potatoes|beetroots|melon_stem|pumpkin_stem|cocoa|sugar_cane|bamboo|hay_block)$/
const looksBuilt = name => BUILT.test(name)

// a full cube: what @solid accepts, what a torch stands on, what light stops at
export const fullBlock = (name, registry = REGISTRY) => {
  const info = blockInfo(name, registry)
  return Boolean(info) && info.boundingBox === 'block' && !PARTIAL.test(name) && !FLUIDS.has(name)
}
const hasBox = (name, registry = REGISTRY) => blockInfo(name, registry)?.boundingBox === 'block'

// the class a block is placed in: digs go first, full blocks in support order, then what stands alone, then what hangs on
// something, then fluids (section 3, order of work)
export const classOf = (alt, registry = REGISTRY) => {
  const name = alt.name
  if (name === '@solid') return 'full'
  if (isAir(name)) return 'air'
  if (FLUIDS.has(name)) return 'fluid'
  if (alt.states.waterlogged === 'true') return 'fluid'
  if (DOORLIKE.test(name) && !/_trapdoor$/.test(name)) return 'attach'
  if (/_bed$/.test(name) || GRAVITY.test(name) || /_carpet$|^snow$|^moss_carpet$|^lantern$|^soul_lantern$/.test(name)) return 'attach'
  if (!hasBox(name, registry)) return 'attach'
  if (PARTIAL.test(name)) return 'partial'
  return 'full'
}

// the item that places a block, when it is not the block's own name
const ITEM_OF = {
  wall_torch: 'torch', soul_wall_torch: 'soul_torch', redstone_wall_torch: 'redstone_torch', wheat: 'wheat_seeds', carrots: 'carrot', potatoes: 'potato',
  beetroots: 'beetroot_seeds', melon_stem: 'melon_seeds', pumpkin_stem: 'pumpkin_seeds', attached_melon_stem: 'melon_seeds', attached_pumpkin_stem: 'pumpkin_seeds',
  cocoa: 'cocoa_beans', water: 'water_bucket', lava: 'lava_bucket', bamboo_sapling: 'bamboo', sweet_berry_bush: 'sweet_berries', torchflower_crop: 'torchflower_seeds',
  pitcher_crop: 'pitcher_pod', tripwire: 'string', redstone_wire: 'redstone', nether_wart: 'nether_wart', cave_vines: 'glow_berries', kelp_plant: 'kelp',
  tall_seagrass: 'seagrass', fire: null, farmland: null, dirt_path: null, frosted_ice: null, powered_rail: 'powered_rail'
}
// what a cell needs a tool for rather than an item
const TOOL_OF = { farmland: 'hoe', dirt_path: 'shovel', fire: 'flint_and_steel' }
export const itemOf = name => {
  if (name in ITEM_OF) return ITEM_OF[name]
  if (name === '@solid' || isAir(name)) return null
  if (/_wall_/.test(name)) return name.replace('_wall_', '_')
  return name
}

// two-part blocks: the lower door, the bed's foot and the tall plant's bottom are the job; the other half is placed by
// the game and only checked. secondPart says where the other half is and what it should read
export const secondPart = alt => {
  const { name, states } = alt
  if (/_door$/.test(name) && states.half === 'lower') return { dx: 0, dy: 1, dz: 0, states: { ...states, half: 'upper' } }
  if (/_bed$/.test(name) && states.part === 'foot' && STEP[states.facing]) return { ...STEP[states.facing], dy: 0, states: { ...states, part: 'head' } }
  if (states.half === 'lower' && !/_door$/.test(name) && !/_stairs|_trapdoor/.test(name)) return { dx: 0, dy: 1, dz: 0, states: { ...states, half: 'upper' } }
  return null
}
export const isSecondPart = alt => (/_door$/.test(alt.name) && alt.states.half === 'upper') || (/_bed$/.test(alt.name) && alt.states.part === 'head') || (!/_door$|_stairs$|_trapdoor$/.test(alt.name) && alt.states.half === 'upper')

// the cell an attachable hangs on, relative to its own: under a torch, a crop or a bed; behind a wall torch or a ladder
export const supportOf = alt => {
  const { name, states } = alt
  if (WALL_HUNG.test(name) && STEP[states.facing]) return { ...STEP[OPPOSITE[states.facing]], dy: 0, word: OPPOSITE[states.facing] }
  if (states.face === 'wall' && STEP[states.facing]) return { ...STEP[OPPOSITE[states.facing]], dy: 0, word: OPPOSITE[states.facing] }
  if (states.face === 'ceiling' || states.hanging === 'true') return { dx: 0, dy: 1, dz: 0, word: 'above' }
  return { dx: 0, dy: -1, dz: 0, word: 'below' }
}

// states the game derives from neighbours or changes at play, written for the reader and never compared or placed
const RUNTIME = new Set(['powered', 'open', 'occupied', 'lit', 'moisture', 'level', 'age', 'stage', 'distance', 'persistent', 'snowy', 'in_wall', 'triggered', 'signal_fire', 'instrument', 'note', 'hinge', 'eye', 'enabled', 'extended', 'has_record', 'has_book', 'attached', 'disarmed', 'unstable', 'inverted', 'bloom', 'can_summon', 'shrieking', 'cracked', 'dusted', 'sculk_sensor_phase', 'flower_amount', 'segment_amount', 'tip', 'thickness', 'berries', 'leaves'])
const CONNECTIONS = new Set(['north', 'south', 'east', 'west', 'up', 'down'])
export const derivedState = (name, state) => {
  if (RUNTIME.has(state) || CONNECTIONS.has(state)) return true
  if (state === 'shape') return true
  if (state === 'type' && /chest$/.test(name)) return true
  return false
}
export const stateOf = block => block?.getProperties?.() ?? block?.properties ?? {}

// does what stands in a cell satisfy one of the legend's alternatives? The block by name (@solid: any full block, air:
// any air), then every written state that is not derived
export const matchesCell = (block, alts, registry = REGISTRY) => {
  if (!block) return false
  const state = stateOf(block)
  return alts.some(alt => {
    const named = alt.name === '@solid' ? fullBlock(block.name, registry) : alt.name === 'air' ? isAir(block.name) : block.name === alt.name
    if (!named) return false
    return Object.entries(alt.states).every(([k, v]) => derivedState(alt.name, k) || String(state[k]) === String(v))
  })
}

// ---------------------------------------------------------------- what place has to be told for a state

// the argument place takes for this block's state: the way to look, the half, or (not yet possible) the face to click
export const placement = alt => {
  const { name, states } = alt
  if (WALL_HUNG.test(name) && STEP[states.facing]) return { against: { ...STEP[OPPOSITE[states.facing]], dy: 0 } }
  if (states.face === 'wall' && STEP[states.facing]) return { against: { ...STEP[OPPOSITE[states.facing]], dy: 0 } }
  if (states.hanging === 'true') return { against: { dx: 0, dy: 1, dz: 0 } }
  if (states.axis === 'x') return { against: { dx: 1, dy: 0, dz: 0 } }
  if (states.axis === 'z') return { against: { dx: 0, dy: 0, dz: 1 } }
  const out = {}
  if (LOOK_TOWARD.test(name) && states.facing) out.facing = states.facing
  else if (FRONT_FACING.test(name) && OPPOSITE[states.facing]) out.facing = OPPOSITE[states.facing]
  if (/_stairs$|_trapdoor$/.test(name) && states.half) out.half = states.half
  if (/_slab$/.test(name) && states.type && states.type !== 'double') out.half = states.type
  return out
}
// the label lint uses for a token place cannot do yet: the block, with its axis when that is what needs the click face
export const placeGap = alt => {
  if (!placement(alt).against) return null
  return alt.states.axis ? `${alt.name}[axis=${alt.states.axis}]` : alt.name
}
export const gapWarning = (token, alt) => `${token}: ${placeGap(alt)} needs place against=, not available yet: build refuses this blueprint until it is`

// ---------------------------------------------------------------- parsing

const levenshtein = (a, b) => {
  const row = Array.from({ length: b.length + 1 }, (_, i) => i)
  for (let i = 1; i <= a.length; i++) {
    let prev = row[0]
    row[0] = i
    for (let j = 1; j <= b.length; j++) {
      const tmp = row[j]
      row[j] = Math.min(row[j] + 1, row[j - 1] + 1, prev + (a[i - 1] === b[j - 1] ? 0 : 1))
      prev = tmp
    }
  }
  return row[b.length]
}
const closest = (word, options) => {
  const [best] = options.map(o => [o, levenshtein(word, o)]).sort((a, b) => a[1] - b[1])
  return best && best[1] <= 3 ? best[0] : null
}
const didYouMean = (word, options) => {
  const near = closest(word, options)
  return near ? ` (did you mean ${near}?)` : ''
}

const parseStates = text => Object.fromEntries((text ?? '').split(',').map(s => s.trim()).filter(Boolean).map(s => {
  const [k, v] = s.split('=')
  return [k.trim(), (v ?? '').trim()]
}))

// one alternative of a legend line: name, name[states] or @solid
const parseAlt = text => {
  const m = /^(@solid|[a-z0-9_{}:]+)(?:\[([^\]]*)\])?$/.exec(text)
  return m ? { name: m[1], states: parseStates(m[2]) } : null
}

const parseFrontMatter = lines => {
  const out = { errors: [] }
  if (lines[0] !== '---') return { errors: ['the file must start with a --- front matter block'] }
  const end = lines.indexOf('---', 1)
  if (end < 0) return { errors: ['the front matter never closes (a second --- line)'] }
  const fields = {}
  for (const line of lines.slice(1, end)) {
    if (!line.trim()) continue
    const at = line.indexOf(':')
    if (at < 0) { out.errors.push(`front matter: "${line}" is not key: value`); continue }
    const k = line.slice(0, at).trim()
    if (!(k in FRONT_KEYS)) { out.errors.push(`front matter: ${k} is not a key${didYouMean(k, Object.keys(FRONT_KEYS))}`); continue }
    fields[k] = line.slice(at + 1).trim()
  }
  for (const [k, required] of Object.entries(FRONT_KEYS)) if (required && fields[k] === undefined) out.errors.push(`front matter: ${k} is required`)
  const tags = (fields.tags ?? '').split(',').map(t => t.trim()).filter(Boolean)
  for (const t of tags) if (!TAGS.includes(t)) out.errors.push(`front matter: ${t} is not a tag (${TAGS.join(', ')})`)
  if (fields.front !== undefined && !DIRS.includes(fields.front)) out.errors.push('front matter: front must be north, south, east or west')
  if (fields.foundation !== undefined && !FOUNDATIONS.includes(fields.foundation)) out.errors.push(`front matter: foundation must be ${FOUNDATIONS.join(', ')}`)
  const clearance = fields.clearance === undefined ? 0 : Number(fields.clearance)
  if (!Number.isInteger(clearance) || clearance < 0) out.errors.push('front matter: clearance must be a whole number of air layers')
  const params = {}
  for (const pair of (fields.params ?? '').split(',').map(s => s.trim()).filter(Boolean)) {
    const [k, v] = pair.split('=').map(s => s.trim())
    if (!k || !v) { out.errors.push(`front matter: params entry "${pair}" is not name=default`); continue }
    params[k] = v
  }
  if (fields.name !== undefined && !/^[a-z0-9]+(-[a-z0-9]+)*$/.test(fields.name)) out.errors.push(`front matter: name ${fields.name} is not kebab-case`)
  return { ...out, name: fields.name, title: fields.title, description: fields.description, tags, front: fields.front, foundation: fields.foundation, clearance, params, source: fields.source, license: fields.license, difficulty: fields.difficulty, notes: fields.notes, by: fields.by, end }
}

// the fenced blocks: every ```legend, and every ```layer under its ## y heading; prose between them is ignored
const parseBlocks = lines => {
  const errors = []
  const legendLines = []
  const layerBlocks = []
  let heading = null
  let fence = null
  for (const line of lines) {
    const open = /^```(\w*)\s*$/.exec(line)
    if (fence && open) {
      if (fence.kind === 'layer') layerBlocks.push({ heading, rows: fence.rows })
      if (fence.kind === 'legend') legendLines.push(...fence.rows)
      fence = null
      continue
    }
    if (fence) { fence.rows.push(line.replace(/\s+$/, '')); continue }
    if (open) { fence = { kind: open[1], rows: [] }; continue }
    const h = /^## y(-?\d+)(?:\.\.y?(-?\d+))?\s*$/.exec(line)
    if (h) heading = { from: Number(h[1]), to: Number(h[2] ?? h[1]), text: line.slice(3).trim() }
    else if (/^#{1,6}\s/.test(line)) heading = null
  }
  if (fence) errors.push(`a \`\`\`${fence.kind} block never closes`)
  return { errors, legendLines, layerBlocks }
}

const parseLegend = lines => {
  const errors = []
  const legend = { '.': { token: '.', alts: [{ name: 'air', states: {} }], tags: [] } }
  for (const raw of lines) {
    const line = raw.trim()
    if (!line) continue
    const m = /^(\S)\s+(\S+)((?:\s+#[\w-]+)*)$/.exec(line)
    if (!m) { errors.push(`legend: "${line}" is not "<token> <block>[states] #tags" with a one-character token`); continue }
    const [, token, spec, tagText] = m
    if (token === '_') { errors.push('legend: _ is reserved (do not care) and cannot be redefined'); continue }
    const alts = spec.split('|').map(parseAlt)
    const bad = spec.split('|')[alts.indexOf(null)]
    if (alts.includes(null)) { errors.push(`${token}: ${bad} is not a block name, name[state=value,...] or @solid`); continue }
    legend[token] = { token, alts, tags: tagText.split(/\s+/).filter(Boolean).map(t => t.slice(1)) }
  }
  return { errors, legend }
}

// {wood} is the parameter's value, {wood:planks} the role looked up in the family table; a template that names no
// declared parameter or a role the value lacks is a refusal naming the token
const TEMPLATE = /\{(\w+)(?::(\w+))?\}/g
export const substituteName = (name, params, registry = REGISTRY) => {
  return name.replace(TEMPLATE, (whole, param, role) => {
    if (params[param] === undefined) throw new Error(`${whole} names a parameter the front matter does not declare (params: ${param}=...)`)
    if (!role) return params[param]
    const refusal = familyRefusal(param, params[param], role, registry)
    if (refusal) throw new Error(refusal)
    return familyBlock(param, params[param], role, registry)
  })
}

// the block and its states against the registry: the name, then every written state and value
const blockErrors = (token, alt, registry) => {
  const { name, states } = alt
  if (name === '@solid' || name === 'air') return []
  if (SERVER_ONLY.has(name)) return [`${token}: this body cannot place ${name} (server-only block)`]
  const info = blockInfo(name, registry)
  if (!info) return [`${token}: ${name} is not a block this body knows`]
  const known = Object.fromEntries((info.states ?? []).map(s => [s.name, s]))
  return Object.entries(states).flatMap(([k, v]) => {
    const state = known[k]
    if (!state) return [`${token}: ${name} has no state ${k}${didYouMean(k, Object.keys(known))}`]
    const values = state.type === 'bool' ? ['true', 'false'] : state.type === 'int' ? (state.values ?? []).map(String) : state.values
    return values.includes(v) ? [] : [`${token}: ${name} ${k} has no value ${v} (${values.join(', ')})`]
  })
}

const parseLayers = (blocks, legend) => {
  const errors = []
  const layers = new Map()
  for (const { heading, rows } of blocks) {
    if (!heading) { errors.push('a ```layer block has no ## y<n> heading over it'); continue }
    const grid = rows.filter(r => r.length)
    for (let y = heading.from; y <= heading.to; y++) {
      const seen = layers.get(y)
      if (seen) { errors.push(`y${y} is given twice (${seen.heading} and ${heading.text})`); continue }
      layers.set(y, { y, grid, heading: heading.text })
    }
  }
  const sorted = [...layers.values()].sort((a, b) => a.y - b.y)
  if (!sorted.length) return { errors: [...errors, 'no ## y<n> layer found'], layers: [] }
  const [first] = blocks.filter(b => b.heading)
  const width = first.rows.filter(r => r.length)[0]?.length ?? 0
  const depth = first.rows.filter(r => r.length).length
  for (const layer of sorted) {
    const w = layer.grid[0]?.length ?? 0
    const ragged = layer.grid.findIndex(r => r.length !== w)
    if (ragged >= 0) { errors.push(`y${layer.y} row ${ragged} is ${layer.grid[ragged].length} wide, the others ${w}`); continue }
    if (w !== width || layer.grid.length !== depth) { errors.push(`y${layer.y} is ${w}x${layer.grid.length} but the blueprint is ${width}x${depth}`); continue }
    layer.grid.forEach((row, dz) => [...row].forEach((ch, dx) => {
      if (ch !== '_' && !legend[ch]) errors.push(`y${layer.y} row ${dz} col ${dx}: ${ch} is not in the legend`)
    }))
    if (layer.y < LIMITS.minY || layer.y > LIMITS.maxY) errors.push(`y${layer.y} is outside y${LIMITS.minY}..y${LIMITS.maxY}`)
  }
  if (width > LIMITS.side || depth > LIMITS.side) errors.push(`the footprint is ${width}x${depth}; the most is ${LIMITS.side}x${LIMITS.side}`)
  const cells = sorted.reduce((n, l) => n + l.grid.join('').replace(/_/g, '').length, 0)
  if (cells > LIMITS.cells) errors.push(`${cells} cells are not _; the most a blueprint may have is ${LIMITS.cells}`)
  return { errors, layers: sorted.map(({ y, grid }) => ({ y, grid })), width, depth }
}

// the whole file: front matter, legend, layers, every block checked against the registry with the default parameters.
// errors is empty when the file is a blueprint; a non-empty list refuses it, every line naming the fix
export function parseBlueprint (text, registry = REGISTRY) {
  const lines = String(text).split(/\r?\n/)
  const front = parseFrontMatter(lines)
  if (front.end === undefined) return { errors: front.errors }
  const blocks = parseBlocks(lines.slice(front.end + 1))
  const legend = parseLegend(blocks.legendLines)
  const layers = parseLayers(blocks.layerBlocks, legend.legend)
  const errors = [...front.errors, ...blocks.errors, ...legend.errors, ...layers.errors]
  for (const { token, alts } of Object.values(legend.legend)) {
    for (const alt of alts) {
      const named = (() => { try { return { name: substituteName(alt.name, front.params, registry) } } catch (e) { return { error: `${token}: ${e.message}` } } })()
      if (named.error) { errors.push(named.error); continue }
      errors.push(...blockErrors(token, { ...alt, states: alt.states, name: named.name }, registry))
    }
  }
  const { end, errors: _e, ...meta } = front
  return { ...meta, legend: legend.legend, layers: layers.layers, width: layers.width, depth: layers.depth, turns: 0, errors }
}

// ---------------------------------------------------------------- resolving and turning

// the legend with every template filled from the call's parameters over the file's defaults
export function resolve (bp, params = {}, registry = REGISTRY) {
  for (const k of Object.keys(params)) {
    if (bp.params[k] === undefined) throw new Error(`${k}= is not a parameter of ${bp.name} (it has ${Object.keys(bp.params).join(', ') || 'none'})`)
  }
  const merged = { ...bp.params, ...params }
  const legend = {}
  for (const [token, spec] of Object.entries(bp.legend)) {
    const alts = spec.alts.map(alt => {
      const name = (() => { try { return substituteName(alt.name, merged, registry) } catch (e) { throw new Error(`${token}: ${e.message}`) } })()
      if (name !== '@solid' && !isAir(name) && !blockInfo(name, registry)) throw new Error(`${token}: ${name} is not a block this body knows`)
      return { name, states: { ...alt.states } }
    })
    legend[token] = { ...spec, alts }
  }
  return { ...bp, params: merged, legend, resolved: true }
}

export const turnsFor = (front, facing) => facing === undefined ? 0 : ((DIRS.indexOf(facing) - DIRS.indexOf(front)) % 4 + 4) % 4

const TURN_FACING = { north: 'east', east: 'south', south: 'west', west: 'north' }
const TURN_AXIS = { x: 'z', z: 'x', y: 'y' }
const TURN_RAIL = { north_south: 'east_west', east_west: 'north_south', ascending_north: 'ascending_east', ascending_east: 'ascending_south', ascending_south: 'ascending_west', ascending_west: 'ascending_north', south_east: 'south_west', south_west: 'north_west', north_west: 'north_east', north_east: 'south_east' }
const turnStates = (name, states) => Object.fromEntries(Object.entries(states).map(([k, v]) => {
  if (k === 'facing' && TURN_FACING[v]) return [k, TURN_FACING[v]]
  if (k === 'axis' && TURN_AXIS[v]) return [k, TURN_AXIS[v]]
  if (k === 'rotation') return [k, String((Number(v) + 4) % 16)]
  if (k === 'shape' && /rail$/.test(name) && TURN_RAIL[v]) return [k, TURN_RAIL[v]]
  return [k, v]
}))
const turnGrid = (grid, width, depth) => Array.from({ length: width }, (_, row) => Array.from({ length: depth }, (_, col) => grid[depth - 1 - col][row]).join(''))

// one quarter turn clockwise seen from above, `turns` times: the grids and every directional state
export function rotate (bp, turns) {
  let out = bp
  for (let i = 0; i < ((turns % 4) + 4) % 4; i++) {
    const legend = Object.fromEntries(Object.entries(out.legend).map(([token, spec]) => [token, { ...spec, alts: spec.alts.map(alt => ({ ...alt, states: turnStates(alt.name, alt.states) })) }]))
    out = { ...out, legend, layers: out.layers.map(l => ({ ...l, grid: turnGrid(l.grid, out.width, out.depth) })), width: out.depth, depth: out.width, front: TURN_FACING[out.front], turns: (out.turns ?? 0) + 1 }
  }
  return out
}

// every cell that is not _, with its legend entry: dx east, dy up from y0, dz south
export const blueprintCells = bp => bp.layers.flatMap(({ y, grid }) => grid.flatMap((row, dz) => [...row].flatMap((token, dx) => token === '_' ? [] : [{ dx, dy: y, dz, token, spec: bp.legend[token] }])))
const cellMap = bp => new Map(blueprintCells(bp).map(c => [key(c.dx, c.dy, c.dz), c]))
const layerMap = bp => new Map(bp.layers.map(l => [l.y, l]))
const tokenAt = (bp, dx, dy, dz) => {
  const layer = layerMap(bp).get(dy)
  if (!layer || dx < 0 || dz < 0 || dx >= bp.width || dz >= bp.depth) return undefined
  return layer.grid[dz][dx]
}

// ---------------------------------------------------------------- the bill

const primary = spec => spec.alts[0]
const addItem = (bill, item, n = 1) => { if (item) bill[item] = (bill[item] ?? 0) + n }
// what one cell asks for, as items: the block's item, plus a bucket when it holds water; two-part blocks once
const cellItems = (spec, bill, once) => {
  const alt = primary(spec)
  if (isSecondPart(alt)) return
  const item = itemOf(alt.name)
  if (item === 'water_bucket' || item === 'lava_bucket') { if (!once.has(item)) { once.add(item); addItem(bill, item) } return }
  addItem(bill, item)
  if (alt.states.waterlogged === 'true' && !once.has('water_bucket')) { once.add('water_bucket'); addItem(bill, 'water_bucket') }
}

export function bill (bp) {
  const total = {}
  const once = new Set()
  const tools = new Set()
  const layers = bp.layers.map(({ y, grid }) => {
    const items = {}
    const layerOnce = new Set()
    for (const row of grid) {
      for (const token of row) {
        if (token === '_') continue
        const spec = bp.legend[token]
        cellItems(spec, items, layerOnce)
        cellItems(spec, total, once)
        const tool = TOOL_OF[primary(spec).name]
        if (tool) tools.add(tool)
      }
    }
    return { y, items }
  })
  const scaffold = scaffoldBill(bp)
  return { total, layers, tools: [...tools], ...(scaffold ? { scaffold } : {}) }
}

// the pillars a build needs for layers no floor reaches (section 4): one per PILLAR_PER cells of perimeter, as tall as
// the layer, in the scaffold= material. Computed the way lint reaches every cell, over flat ground
export function scaffoldBill (bp) {
  const { jobs } = orderJobs(jobsFor(bp, ORIGIN, flatGround(-1)), bp, ORIGIN, flatGround(-1))
  const tallest = Math.max(-1, ...jobs.filter(j => j.stand?.scaffold).map(j => j.y))
  if (tallest < 0) return null
  const pillars = Math.ceil(2 * (bp.width + bp.depth) / PILLAR_PER)
  return { [bp.params.scaffold ?? DEFAULT_SCAFFOLD]: pillars * tallest }
}
const ORIGIN = { x: 0, y: 0, z: 0 }

export const stackSize = (item, registry = REGISTRY) => registry.itemsByName[item]?.stackSize ?? 64
export const stackSlots = (items, registry = REGISTRY) => Object.entries(items).reduce((n, [item, count]) => n + Math.ceil(count / stackSize(item, registry)), 0)

export function counts (bp, registry = REGISTRY) {
  const out = { doors: 0, beds: 0, containers: 0, workstations: 0, lights: 0 }
  for (const { spec } of blueprintCells(bp)) {
    const alt = primary(spec)
    if (isSecondPart(alt)) continue
    if (/_door$/.test(alt.name)) out.doors++
    if (/_bed$/.test(alt.name)) out.beds++
    if (CONTAINER.test(alt.name)) out.containers++
    if (WORKSTATION.test(alt.name)) out.workstations++
    if ((blockInfo(alt.name, registry)?.emitLight ?? 0) > 0) out.lights++
  }
  return out
}

// ---------------------------------------------------------------- enclosed, lit, spawn-safe

const SIX = [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]]

// the interior air no path reaches from outside the bounding box (doors and gates count as walls, _ as open), the
// light in it from the blueprint's own sources, and the cells a mob could stand in at light 0
export function enclosure (bp, registry = REGISTRY) {
  const cells = cellMap(bp)
  const minY = bp.layers[0].y
  const maxY = bp.layers.at(-1).y
  const inside = (dx, dy, dz) => dx >= 0 && dx < bp.width && dz >= 0 && dz < bp.depth && dy >= minY && dy <= maxY
  const specAt = (dx, dy, dz) => cells.get(key(dx, dy, dz))?.spec ?? null
  const open = (dx, dy, dz) => { const spec = specAt(dx, dy, dz); return !spec || isAir(primary(spec).name) }
  const opaque = (dx, dy, dz) => { const spec = specAt(dx, dy, dz); return Boolean(spec) && (fullBlock(primary(spec).name, registry) || /_door$/.test(primary(spec).name)) }
  const reached = new Set()
  const queue = []
  for (let dx = 0; dx < bp.width; dx++) for (let dz = 0; dz < bp.depth; dz++) for (let dy = minY; dy <= maxY; dy++) {
    const edge = dx === 0 || dz === 0 || dx === bp.width - 1 || dz === bp.depth - 1 || dy === maxY
    if (edge && open(dx, dy, dz)) queue.push([dx, dy, dz])
  }
  while (queue.length) {
    const [dx, dy, dz] = queue.pop()
    const k = key(dx, dy, dz)
    if (reached.has(k) || !inside(dx, dy, dz) || !open(dx, dy, dz)) continue
    reached.add(k)
    for (const [sx, sy, sz] of SIX) queue.push([dx + sx, dy + sy, dz + sz])
  }
  const enclosed = []
  for (let dx = 0; dx < bp.width; dx++) for (let dz = 0; dz < bp.depth; dz++) for (let dy = minY; dy <= maxY; dy++) {
    if (open(dx, dy, dz) && specAt(dx, dy, dz) && !reached.has(key(dx, dy, dz))) enclosed.push({ dx, dy, dz })
  }
  // light: every source at its level, one less per cell, stopped by full blocks and doors
  const light = new Map()
  const sources = blueprintCells(bp).map(c => ({ ...c, level: blockInfo(primary(c.spec).name, registry)?.emitLight ?? 0 })).filter(c => c.level > 0)
  const lit = [...sources.map(c => [c.dx, c.dy, c.dz, c.level])]
  while (lit.length) {
    const [dx, dy, dz, level] = lit.shift()
    const k = key(dx, dy, dz)
    if ((light.get(k) ?? 0) >= level || !inside(dx, dy, dz)) continue
    light.set(k, level)
    if (level <= 1) continue
    for (const [sx, sy, sz] of SIX) if (!opaque(dx + sx, dy + sy, dz + sz)) lit.push([dx + sx, dy + sy, dz + sz, level - 1])
  }
  const floorUnder = (dx, dy, dz) => dy - 1 < minY ? true : (specAt(dx, dy - 1, dz) ? fullBlock(primary(specAt(dx, dy - 1, dz)).name, registry) : false)
  const dark = enclosed.filter(c => floorUnder(c.dx, c.dy, c.dz) && (light.get(key(c.dx, c.dy, c.dz)) ?? 0) === 0)
  const room = enclosed.length
    ? `y${Math.min(...enclosed.map(c => c.dy))} ${Math.min(...enclosed.map(c => c.dx))}..${Math.max(...enclosed.map(c => c.dx))},${Math.min(...enclosed.map(c => c.dz))}..${Math.max(...enclosed.map(c => c.dz))}`
    : null
  return { enclosed: enclosed.length, room, dark, lit: enclosed.length > 0 && dark.length === 0, spawnSafe: enclosed.length > 0 && dark.length === 0 }
}

// ---------------------------------------------------------------- lint

const at = (dy, dx, dz) => `y${dy} ${dx},${dz}`
const describeToken = (bp, dx, dy, dz) => {
  const token = tokenAt(bp, dx, dy, dz)
  if (token === undefined) return 'outside the blueprint'
  if (token === '_') return '_'
  const name = primary(bp.legend[token]).name
  return isAir(name) ? '. (air)' : `${token} (${name})`
}
const isCropName = name => CROP.test(name)
const holdsWaterAlt = alt => alt.name === 'water' || alt.states.waterlogged === 'true'
const few = (cells, n = 4) => `${cells.slice(0, n).map(c => cellName(c.dx, c.dz)).join(' ')}${cells.length > n ? ` and ${cells.length - n} more` : ''}`

export function lint (bp, registry = REGISTRY) {
  const errors = []
  const warnings = []
  const cells = blueprintCells(bp)
  const minY = bp.layers[0].y
  const below = (dx, dy, dz) => tokenAt(bp, dx, dy - 1, dz)
  for (const { dx, dy, dz, spec, token } of cells) {
    const alt = primary(spec)
    const second = secondPart(alt)
    const kind = /_door$/.test(alt.name) ? 'door' : /_bed$/.test(alt.name) ? 'bed' : 'plant'
    if (second) {
      const there = tokenAt(bp, dx + second.dx, dy + second.dy, dz + second.dz)
      const mate = there && there !== '_' ? primary(bp.legend[there]) : null
      const ok = mate && mate.name === alt.name && Object.entries(second.states).every(([k, v]) => derivedState(alt.name, k) || mate.states[k] === v)
      if (!ok && kind === 'bed') errors.push(`the bed foot at ${at(dy, dx, dz)} faces ${alt.states.facing} but ${cellName(dx + second.dx, dz + second.dz)} is not its head`)
      if (!ok && kind !== 'bed') errors.push(`the ${kind} at ${at(dy, dx, dz)} has no upper half over it: ${at(dy + 1, dx, dz)} is ${describeToken(bp, dx, dy + 1, dz)}`)
    }
    if (isSecondPart(alt)) {
      const under = kind === 'bed' ? [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([sx, sz]) => tokenAt(bp, dx + sx, dy, dz + sz)) : [below(dx, dy, dz)]
      const mates = under.filter(t => t && t !== '_').map(t => primary(bp.legend[t])).filter(m => m.name === alt.name && !isSecondPart(m))
      if (!mates.length) errors.push(kind === 'bed' ? `the bed head at ${at(dy, dx, dz)} has no foot beside it` : `the ${kind} upper half at ${at(dy, dx, dz)} has no lower half under it`)
    }
    if (kind === 'door' && alt.states.half === 'lower') {
      const floor = below(dx, dy, dz)
      const grounded = floor === undefined ? dy === minY && dy <= 0 : floor !== '_' && !isAir(primary(bp.legend[floor]).name)
      if (!grounded) errors.push(`the door at ${at(dy, dx, dz)} stands on nothing: ${at(dy - 1, dx, dz)} is ${describeToken(bp, dx, dy - 1, dz)}`)
    }
    if (classOf(alt, registry) === 'attach' && !isSecondPart(alt) && kind !== 'door') {
      const s = supportOf(alt)
      const there = tokenAt(bp, dx + s.dx, dy + s.dy, dz + s.dz)
      const name = alt.name
      const where = `${cellName(dx + s.dx, dz + s.dz)} ${s.word === 'below' || s.word === 'above' ? `${s.word} it` : `${s.word} of it`}`
      if (there === undefined && !(s.dy === -1 && dy === minY && dy <= 0)) warnings.push(`the ${name} at ${at(dy, dx, dz)} stands on ${where}, which is outside the blueprint: whatever is there must hold it`)
      else if (there === '_') warnings.push(`the ${name} at ${at(dy, dx, dz)} stands on ${where}, which is _: whatever is there must hold it`)
      else if (there !== undefined && isAir(primary(bp.legend[there]).name)) errors.push(`the ${name} at ${at(dy, dx, dz)} needs a block at ${where}; that cell is . (air)`)
    }
    if (isCropName(alt.name)) {
      const floor = below(dx, dy, dz)
      const soil = floor && floor !== '_' && primary(bp.legend[floor]).name === 'farmland'
      if (!soil) errors.push(`the ${alt.name} at ${at(dy, dx, dz)} needs farmland under it; ${at(dy - 1, dx, dz)} is ${floor === undefined ? 'outside the blueprint (the ground)' : describeToken(bp, dx, dy - 1, dz)}`)
    }
  }
  for (const { token, alts } of Object.values(bp.legend)) if (placeGap(alts[0])) warnings.push(gapWarning(token, alts[0]))
  // farmland stays wet within 4 of water level with it or one above
  const waters = cells.filter(c => holdsWaterAlt(primary(c.spec)))
  const dry = cells.filter(c => primary(c.spec).name === 'farmland' && !waters.some(w => Math.abs(w.dx - c.dx) <= 4 && Math.abs(w.dz - c.dz) <= 4 && (w.dy === c.dy || w.dy === c.dy + 1)))
  if (dry.length) errors.push(`${dry.length} cell${dry.length === 1 ? ' is' : 's are'} farmland with no water within 4 blocks (${few(dry)}): move the channel or shorten the row`)
  // a crop needs a cell to stand in within reach: an air cell of its own layer over a floor, or anything outside the footprint
  const crops = cells.filter(c => isCropName(primary(c.spec).name))
  if (crops.length) {
    const laneAt = (dx, dz, dy) => {
      if (dx < 0 || dz < 0 || dx >= bp.width || dz >= bp.depth) return true
      const t = tokenAt(bp, dx, dy, dz)
      return t !== undefined && t !== '_' && isAir(primary(bp.legend[t]).name)
    }
    const offsets = [-4, -3, -2, -1, 0, 1, 2, 3, 4]
    const served = c => offsets.some(i => offsets.some(j => i * i + j * j + 1 <= 4.2 * 4.2 && (i || j) && laneAt(c.dx + i, c.dz + j, c.dy)))
    const stranded = crops.filter(c => !served(c))
    if (stranded.length) {
      const subject = stranded.length === 1 ? '1 crop cell has nothing to stand on within 4 of it' : `${stranded.length} crop cells have nothing to stand on within 4 of them`
      warnings.push(`${subject} (${few(stranded)}): lay a . path or a covered channel through the rows, eight rows apart at most, or every job there answers nowhere to stand`)
    }
  }
  // an enclosed room with a cell a mob could stand in at light 0
  const room = enclosure(bp, registry)
  if (room.dark.length) {
    const line = `the room at ${room.room} has ${room.dark.length} ${room.dark.length === 1 ? 'cell' : 'cells'} at light 0: add a light source`
    if (bp.tags.includes('shelter')) errors.push(`${line} (a shelter must be spawn-safe)`)
    else warnings.push(line)
  }
  // every cell must have somewhere to stand while it is placed, on flat ground
  const { unreachable } = orderJobs(jobsFor(bp, ORIGIN, flatGround(-1)), bp, ORIGIN, flatGround(-1))
  for (const j of unreachable) errors.push(`y${j.y} ${j.x},${j.z} has no cell to stand on within reach, even with a scaffold`)
  return { errors, warnings }
}

// ---------------------------------------------------------------- jobs

// flat open ground: dirt with a grass top at `top`, air above; what lint builds on and what the tests build on
export const flatGround = top => (x, y, z) => ({ name: y === top ? 'grass_block' : y < top ? 'dirt' : 'air', getProperties: () => ({}) })

const isSoil = name => /^(dirt|grass_block|dirt_path)$/.test(name)
const isKept = name => CONTAINER.test(name) || /_bed$/.test(name)

// what one mismatching cell needs: a dig when something stands there, then the block. `natural` says whether the dig
// is terrain (dug without asking) or something built (an obstacle: check names it, build refuses it without clear=)
const cellJobs = (cell, at, block, registry) => {
  const { dx, dy, dz, spec, token } = cell
  const alt = primary(spec)
  const x = at.x + dx; const y = at.y + dy; const z = at.z + dz
  const base = { x, y, z, token, tags: spec.tags, layer: dy }
  const jobs = []
  const standing = block.name
  const wantsAir = isAir(alt.name)
  if (!isAir(standing) && !FLUIDS.has(standing) && !(alt.name === 'farmland' && isSoil(standing))) {
    const natural = standing === alt.name || !looksBuilt(standing)
    jobs.push({ ...base, do: 'dig', was: standing, natural, keep: isKept(standing), class: 'dig' })
  }
  if (wantsAir) {
    if (FLUIDS.has(standing)) jobs.push({ ...base, do: 'dig', was: standing, natural: false, keep: false, class: 'dig', fluid: true })
    return jobs
  }
  if (alt.name === '@solid') return [...jobs, { ...base, do: 'place', item: 'cobblestone', block: { name: 'cobblestone', states: {} }, class: 'full' }]
  const cls = classOf(alt, registry)
  const how = placement(alt)
  if (alt.name === 'farmland') {
    const till = { ...base, do: 'till', tool: 'hoe', block: { name: 'farmland', states: {} }, class: 'full' }
    if (isSoil(standing)) return [till]
    return [...jobs, { ...base, do: 'place', item: 'dirt', block: { name: 'dirt', states: {} }, class: 'full' }, till]
  }
  if (alt.name === 'water' || alt.name === 'lava') return [...jobs, { ...base, do: 'pour', item: itemOf(alt.name), block: { name: alt.name, states: {} }, class: 'fluid' }]
  const job = { ...base, do: 'place', item: itemOf(alt.name), block: { name: alt.name, states: alt.states }, class: cls, ...how, ...(secondPart(alt) ? { second: secondPart(alt) } : {}) }
  if (alt.states.waterlogged === 'true') return [...jobs, { ...base, do: 'pour', item: 'water_bucket', block: { name: 'water', states: {} }, class: 'fluid' }, { ...job, do: 'cover', class: 'fluid' }]
  return [...jobs, job]
}

// every job the world still needs for this blueprint at this anchor: a cell whose block and written states already
// match is no job at all, so the world is the only progress record. Cells whose chunk is not loaded are left out here
// (siteCheck names them). Unordered: orderJobs puts them in the order of work
export function jobsFor (bp, at, worldAt, registry = REGISTRY) {
  return blueprintCells(bp).flatMap(cell => {
    const alt = primary(cell.spec)
    if (isSecondPart(alt)) return []
    const block = worldAt(at.x + cell.dx, at.y + cell.dy, at.z + cell.dz)
    if (!block || matchesCell(block, cell.spec.alts, registry)) return []
    return cellJobs(cell, at, block, registry)
  })
}

// ---------------------------------------------------------------- the predicted world, standing cells, order

// the real world with the jobs done so far laid over it
const predicted = worldAt => {
  const over = new Map()
  const get = (x, y, z) => over.has(key(x, y, z)) ? over.get(key(x, y, z)) : worldAt(x, y, z)
  const set = (x, y, z, name, states = {}) => over.set(key(x, y, z), { name, getProperties: () => states })
  const apply = job => {
    if (job.do === 'dig') return set(job.x, job.y, job.z, 'air')
    if (job.do === 'till') return set(job.x, job.y, job.z, 'farmland')
    if (job.do === 'pour') return set(job.x, job.y, job.z, 'water', { level: '0' })
    set(job.x, job.y, job.z, job.block.name, job.block.states)
    if (job.second) set(job.x + job.second.dx, job.y + job.second.dy, job.z + job.second.dz, job.block.name, job.second.states)
  }
  return { get, set, apply }
}

// can a body pass through this cell (feet or head): anything without a box, a door, a gate, a carpet, water
const climbable = block => Boolean(block) && /^(ladder|vine|scaffolding)$/.test(block.name)
const passable = (block, registry) => Boolean(block) && (!hasBox(block.name, registry) || DOORLIKE.test(block.name) || /_carpet$/.test(block.name) || climbable(block)) && block.name !== 'lava'
const floorAt = (world, x, y, z, registry) => {
  const under = world.get(x, y - 1, z)
  return Boolean(under) && !passable(under, registry) && !FLUIDS.has(under.name)
}
const standableAt = (world, x, y, z, registry) => {
  const feet = world.get(x, y, z)
  if (!passable(feet, registry) || FLUIDS.has(feet?.name) || !passable(world.get(x, y + 1, z), registry)) return false
  return climbable(feet) || floorAt(world, x, y, z, registry)
}

// the cells a body can walk to from the ring round the footprint: four-way steps of at most one up or down, and
// ladders climbed straight up or down. Bounded to the site and its ring, at every height the build reaches
const reachableFrom = (world, starts, box, registry) => {
  const seen = new Set()
  const queue = [...starts]
  const inBox = (x, y, z) => x >= box.x1 && x <= box.x2 && z >= box.z1 && z <= box.z2 && y >= box.y1 && y <= box.y2
  while (queue.length) {
    const { x, y, z } = queue.pop()
    const k = key(x, y, z)
    if (seen.has(k) || !inBox(x, y, z) || !standableAt(world, x, y, z, registry)) continue
    seen.add(k)
    for (const [sx, sz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) for (const dy of [0, 1, -1]) queue.push({ x: x + sx, y: y + dy, z: z + sz })
    if (climbable(world.get(x, y, z))) queue.push({ x, y: y + 1, z }, { x, y: y - 1, z })
  }
  return seen
}

const eyeDistance = (stand, target) => Math.hypot(target.x + 0.5 - (stand.x + 0.5), target.y + 0.5 - (stand.y + EYE), target.z + 0.5 - (stand.z + 0.5))
// every cell within arm's reach of a target that is not the target's own column at its height or just under it
const withinReach = target => {
  const out = []
  for (let dx = -4; dx <= 4; dx++) for (let dy = -5; dy <= 4; dy++) for (let dz = -4; dz <= 4; dz++) {
    const stand = { x: target.x + dx, y: target.y + dy, z: target.z + dz }
    if (dx === 0 && dz === 0 && (dy === 0 || dy === -1)) continue
    if (eyeDistance(stand, target) <= REACH) out.push({ ...stand, d: Math.hypot(dx, dz) })
  }
  return out.sort((a, b) => a.d - b.d || a.y - b.y || a.x - b.x || a.z - b.z)
}

const site = (bp, at) => {
  const minY = bp.layers[0].y
  const maxY = bp.layers.at(-1).y
  return { x1: at.x - 1, x2: at.x + bp.width, z1: at.z - 1, z2: at.z + bp.depth, y1: at.y + minY - 1, y2: at.y + maxY + 2, minY, maxY }
}
const outsideFootprint = (bp, at, c) => c.x < at.x || c.x >= at.x + bp.width || c.z < at.z || c.z >= at.z + bp.depth
// the ring of cells just outside the footprint, at the height the body stands on the ground round it
const ringCells = (bp, at, world, registry) => {
  const box = site(bp, at)
  const out = []
  for (let x = box.x1; x <= box.x2; x++) for (let z = box.z1; z <= box.z2; z++) {
    if (!outsideFootprint(bp, at, { x, z })) continue
    for (const dy of [0, 1, -1, 2, -2]) if (standableAt(world, x, at.y + dy, z, registry)) { out.push({ x, y: at.y + dy, z }); break }
  }
  return out
}

// support order for the full blocks of a layer: out from the cells that already touch something solid
const supportOrder = (jobs, world, registry) => {
  const solidAt = (x, y, z) => { const b = world.get(x, y, z); return Boolean(b) && hasBox(b.name, registry) }
  const left = [...jobs]
  const out = []
  const placedKeys = new Set()
  const touching = j => SIX.some(([sx, sy, sz]) => placedKeys.has(key(j.x + sx, j.y + sy, j.z + sz)) || solidAt(j.x + sx, j.y + sy, j.z + sz))
  while (left.length) {
    const wave = left.filter(touching)
    const take = wave.length ? wave : [left[0]]
    for (const j of take) { out.push(j); placedKeys.add(key(j.x, j.y, j.z)); left.splice(left.indexOf(j), 1) }
  }
  return out
}

const CLASS_ORDER = ['dig', 'full', 'partial', 'attach', 'fluid']
const byCell = (a, b) => a.z - b.z || a.x - b.x

// the jobs in the order of work with a standing cell for each (section 3): layers bottom-up, classes in CLASS_ORDER,
// full blocks in support order, attachables after their support, and no job that fills the body's cell or cuts it off
// from the ring. A job that would cut it off waits until only such jobs are left, and is done from outside. A cell no
// standing cell reaches gets a pillar spot outside the footprint (scaffold) or is listed as unreachable
export function orderJobs (jobs, bp, at, worldAt, registry = REGISTRY) {
  const world = predicted(worldAt)
  const box = site(bp, at)
  const ordered = []
  const unreachable = []
  const ring = ringCells(bp, at, world, registry)
  const layers = [...new Set(jobs.map(j => j.y))].sort((a, b) => a - b)
  const attachAfter = list => {
    // an attachable goes after the job of the cell it hangs on, when that cell is in the same list
    const keys = new Map(list.map((j, i) => [key(j.x, j.y, j.z), i]))
    return [...list].sort((a, b) => {
      const sa = a.class === 'attach' ? supportOf(a.block) : null
      const sb = b.class === 'attach' ? supportOf(b.block) : null
      const aAfterB = sa && keys.get(key(a.x + sa.dx, a.y + sa.dy, a.z + sa.dz)) === keys.get(key(b.x, b.y, b.z))
      const bAfterA = sb && keys.get(key(b.x + sb.dx, b.y + sb.dy, b.z + sb.dz)) === keys.get(key(a.x, a.y, a.z))
      return aAfterB ? 1 : bAfterA ? -1 : 0
    })
  }
  const chooseStand = (job, reachable, fromOutside) => {
    const candidates = withinReach(job).filter(c => standableAt(world, c.x, c.y, c.z, registry) && reachable.has(key(c.x, c.y, c.z)))
      .filter(c => !fromOutside || outsideFootprint(bp, at, c))
    if (job.do === 'dig' || job.class === 'attach' || job.class === 'fluid' || fromOutside) return candidates[0] ?? null
    // a solid block may not cut the body off: the spot must still be reachable once the block stands
    for (const c of candidates.slice(0, 4)) {
      world.apply(job)
      const still = reachableFrom(world, ring, box, registry).has(key(c.x, c.y, c.z))
      world.set(job.x, job.y, job.z, worldAt(job.x, job.y, job.z)?.name ?? 'air')
      if (still) return c
    }
    return candidates.length ? undefined : null
  }
  const pillarStand = job => {
    const spots = withinReach(job).filter(c => outsideFootprint(bp, at, c) && c.x >= box.x1 && c.x <= box.x2 && c.z >= box.z1 && c.z <= box.z2 && c.y <= job.y && c.y > at.y)
    return spots[0] ? { x: spots[0].x, y: spots[0].y, z: spots[0].z, scaffold: spots[0].y - at.y } : null
  }
  for (const y of layers) {
    const ofLayer = jobs.filter(j => j.y === y)
    const classes = CLASS_ORDER.map(cls => ofLayer.filter(j => j.class === cls).sort(byCell))
    const full = supportOrder(classes[1], world, registry)
    const attach = attachAfter(classes[3])
    let pending = [...classes[0], ...full, ...classes[2], ...attach, ...classes[4]]
    let deferred = []
    let fromOutside = false
    while (pending.length) {
      const job = pending.shift()
      const reachable = reachableFrom(world, ring, box, registry)
      const stand = chooseStand(job, reachable, fromOutside)
      if (stand === undefined) { deferred.push(job); continue }
      if (stand === null && !fromOutside && !pending.length) { deferred.push(job); continue }
      if (stand === null) {
        const pillar = pillarStand(job)
        if (!pillar) { unreachable.push(job); continue }
        ordered.push({ ...job, stand: pillar })
        world.apply(job)
        continue
      }
      ordered.push({ ...job, stand: { x: stand.x, y: stand.y, z: stand.z } })
      world.apply(job)
      if (!pending.length && deferred.length && !fromOutside) { pending = deferred; deferred = []; fromOutside = true }
    }
    // what could not be done from inside is done from the ring, once nothing else of the layer is left
    if (deferred.length) {
      for (const job of deferred) {
        const reachable = reachableFrom(world, ring, box, registry)
        const stand = chooseStand(job, reachable, true) ?? pillarStand(job)
        if (!stand) { unreachable.push(job); continue }
        ordered.push({ ...job, stand: { x: stand.x, y: stand.y, z: stand.z, ...(stand.scaffold ? { scaffold: stand.scaffold } : {}) } })
        world.apply(job)
      }
    }
  }
  return { jobs: ordered, unreachable }
}

// ---------------------------------------------------------------- stages

// what a list of jobs will use up: one bucket for any amount of water, scaffold blocks for the tallest pillar
export function jobsBill (jobs, scaffoldItem = DEFAULT_SCAFFOLD) {
  const bill = {}
  for (const j of jobs) {
    if (!j.item) continue
    if (j.item === 'water_bucket' || j.item === 'lava_bucket') { bill[j.item] = 1; continue }
    addItem(bill, j.item)
  }
  const pillar = Math.max(0, ...jobs.map(j => j.stand?.scaffold ?? 0))
  if (pillar) addItem(bill, scaffoldItem, pillar)
  return bill
}

// the remaining jobs cut into stages: each the longest run of whole layers whose bill fits `carry` slots, a single
// layer that does not fit cut inside its own order (section 4)
export function stages (jobs, carry, registry = REGISTRY, scaffoldItem = DEFAULT_SCAFFOLD) {
  const fits = list => stackSlots(jobsBill(list, scaffoldItem), registry) <= Math.max(carry, 1)
  const layers = [...new Set(jobs.map(j => j.y))].sort((a, b) => a - b).map(y => jobs.filter(j => j.y === y))
  const out = []
  let current = []
  const close = () => { if (current.length) out.push(current); current = [] }
  for (const layer of layers) {
    if (fits([...current, ...layer])) { current.push(...layer); continue }
    close()
    if (fits(layer)) { current = [...layer]; continue }
    for (const job of layer) {
      if (!fits([...current, job])) close()
      current.push(job)
    }
    close()
  }
  close()
  // from/to are the blueprint's layer numbers (y-1..y1), the way the file and the stage table name them
  return out.map((list, i) => ({ n: i + 1, of: out.length, from: list[0].layer ?? list[0].y, to: list.at(-1).layer ?? list.at(-1).y, jobs: list, bill: jobsBill(list, scaffoldItem) }))
}

const billLine = items => Object.entries(items).map(([k, n]) => `${k}:${n}`).join(' ')
export const shortfall = (bill, have = {}) => Object.fromEntries(Object.entries(bill).map(([k, n]) => [k, n - (have[k] ?? 0)]).filter(([, n]) => n > 0))
// one row of the stage table check and show print
export const stageLine = (stage, have = {}) => {
  const short = shortfall(stage.bill, have)
  const span = stage.from === stage.to ? `y${stage.from}` : `y${stage.from}..y${stage.to}`
  return `stage ${stage.n}/${stage.of} ${span} carry=${billLine(stage.bill)} ${Object.keys(short).length ? `short=${billLine(short)}` : 'have=all'}`
}

// ---------------------------------------------------------------- the site

const possessive = place => `${place.name} (${place.by}'s)`
// the footprint of a marked place on the map: a plan's rows, a build's recorded size, else its one cell
const placeBox = place => {
  if (place.plan) {
    const rows = place.plan.split('\n')
    return { x1: place.x, x2: place.x + rows[0].length - 1, z1: place.z, z2: place.z + rows.length - 1 }
  }
  if (place.w && place.d) return { x1: place.x, x2: place.x + place.w - 1, z1: place.z, z2: place.z + place.d - 1 }
  return { x1: place.x, x2: place.x, z1: place.z, z2: place.z }
}
const boxesOverlap = (a, b) => a.x1 <= b.x2 && b.x1 <= a.x2 && a.z1 <= b.z2 && b.z1 <= a.z2
const between = (v, a, b) => v >= Math.min(a, b) && v <= Math.max(a, b)

// everything build would refuse over, read off the world and the shared map without touching a block: obstacles by
// block and cell, foundation cells that are not solid, the clearance over the roof, zones and other agents' places,
// unloaded chunks. `refusal` is the first sentence build would stop with, or null
export function siteCheck (bp, at, worldAt, { zones = [], places = [], me = '', clear = false } = {}, registry = REGISTRY) {
  const cells = blueprintCells(bp)
  const unloaded = []
  const seen = new Set()
  for (const c of cells) {
    const x = at.x + c.dx; const y = at.y + c.dy; const z = at.z + c.dz
    if (!worldAt(x, y, z) && !seen.has(key(x, y, z))) { seen.add(key(x, y, z)); unloaded.push({ x, y, z }) }
  }
  const box = { x1: at.x, x2: at.x + bp.width - 1, z1: at.z, z2: at.z + bp.depth - 1 }
  const minY = bp.layers[0].y
  const maxY = bp.layers.at(-1).y
  const zoneHits = zones.filter(zn => cells.some(c => between(at.x + c.dx, zn.x1, zn.x2) && between(at.y + c.dy, zn.y1, zn.y2) && between(at.z + c.dz, zn.z1, zn.z2)))
  const placeHits = places.filter(p => String(p.by ?? '').toLowerCase() !== String(me).toLowerCase() && boxesOverlap(box, placeBox(p)))
  const overlaps = [...zoneHits.map(zn => `${zn.name} (a protected zone)`), ...placeHits.map(possessive)]
  const digs = jobsFor(bp, at, worldAt, registry).filter(j => j.do === 'dig' && !j.natural)
  const obstacles = digs.map(j => ({ name: j.was, x: j.x, y: j.y, z: j.z, token: j.token, keep: j.keep, fluid: Boolean(j.fluid) }))
  // the ground under the lowest layer's solid cells
  const foundation = []
  const fill = bp.params.fill ?? DEFAULT_FILL
  for (const c of cells.filter(c => c.dy === minY && !isAir(primary(c.spec).name))) {
    const x = at.x + c.dx; const y = at.y + minY - 1; const z = at.z + c.dz
    const under = worldAt(x, y, z)
    if (!under || hasBox(under.name, registry)) continue
    foundation.push({ name: under.name, x, y, z, ...(bp.foundation === 'any' ? { fill } : {}) })
  }
  const clearance = []
  for (let dy = 1; dy <= bp.clearance; dy++) {
    for (let dx = 0; dx < bp.width; dx++) for (let dz = 0; dz < bp.depth; dz++) {
      if (tokenAt(bp, dx, maxY, dz) === '_') continue
      const x = at.x + dx; const y = at.y + maxY + dy; const z = at.z + dz
      const there = worldAt(x, y, z)
      if (there && !isAir(there.name) && !/^(short_grass|tall_grass|fern|large_fern|snow|leaf_litter|.*_leaves)$/.test(there.name)) clearance.push({ name: there.name, x, y, z })
    }
  }
  const kept = obstacles.find(o => o.keep)
  const refusal = unloaded.length ? unloadedRefusal(unloaded[0])
    : overlaps.length ? `${bp.name} at ${at.x},${at.y},${at.z} overlaps ${overlaps[0]}: pick another anchor`
    : kept ? `${kept.name} at ${kept.x},${kept.y},${kept.z} is in the way of ${kept.token} and a ${CONTAINER.test(kept.name) ? 'container' : 'bed'} is never dug: move it, or move the anchor`
    : obstacles.length && obstacles[0].fluid ? `${obstacles[0].name} at ${obstacles[0].x},${obstacles[0].y},${obstacles[0].z} is in the way of ${obstacles[0].token}: fill it (a bucket takes the source), or move the anchor`
    : obstacles.length && !clear ? `${obstacles[0].name} at ${obstacles[0].x},${obstacles[0].y},${obstacles[0].z} is in the way of ${obstacles[0].token}: dig it, or run again with clear=true`
    : foundation.length && bp.foundation !== 'any' ? `the foundation at ${foundation[0].x},${foundation[0].y},${foundation[0].z} is ${foundation[0].name}: fill it, choose foundation=any, or move the anchor`
    : clearance.length ? `${clearance[0].name} at ${clearance[0].x},${clearance[0].y},${clearance[0].z} is inside the clearance over the roof: dig it, or move the anchor`
    : null
  return { obstacles, foundation, clearance, unloaded, overlaps, refusal }
}
export const unloadedRefusal = c => `the chunk under ${c.x},${c.y},${c.z} is not loaded: stand within 60 blocks of the site and check again`

// ---------------------------------------------------------------- the place note, the hash, the rendering

export const blueprintHash = text => createHash('sha1').update(String(text)).digest('hex').slice(0, 8)
// what a build's place carries in its note until mark has fields for it: the blueprint, the facing, the file's hash,
// then the parameters that differ from the defaults. A note holds 80 characters
export const buildNote = ({ blueprint, facing, params = {}, hash }) =>
  [`bp=${blueprint}`, `f=${facing}`, `h=${hash}`, ...Object.entries(params).map(([k, v]) => `${k}=${v}`)].join(' ')
export const parseNote = note => {
  const words = String(note ?? '').split(/\s+/).filter(Boolean)
  if (!words[0]?.startsWith('bp=')) return null
  const out = { blueprint: '', facing: undefined, params: {}, hash: '' }
  for (const w of words) {
    const [k, v] = w.split('=')
    if (k === 'bp') out.blueprint = v
    else if (k === 'f') out.facing = v
    else if (k === 'h') out.hash = v
    else if (v !== undefined) out.params[k] = v
  }
  return out
}

export const renderLayer = (bp, y) => {
  const layer = bp.layers.find(l => l.y === y)
  return layer ? [`y${y}`, ...layer.grid].join('\n') : `no layer y${y}`
}

// ---------------------------------------------------------------- farms as blueprints

// today's farm map (lib.mjs PLAN_LEGEND), one character a column, as the three-layer blueprint of section 2
const FARM_COLUMNS = {
  w: ['farmland #ground', 'wheat[age=0] #crop'],
  c: ['farmland #ground', 'carrots #crop'],
  p: ['farmland #ground', 'potatoes #crop'],
  b: ['farmland #ground', 'beetroots #crop'],
  m: ['farmland #ground', 'melon_stem #crop'],
  k: ['farmland #ground', 'pumpkin_stem #crop'],
  s: ['sand #ground', 'sugar_cane #crop'],
  B: ['dirt #ground', 'bamboo_sapling #crop'],
  '~': ['oak_slab[type=top,waterlogged=true] #ground #cover #lane', 'air #lane'],
  '.': ['dirt|grass_block #ground', 'air #lane'],
  '#': ['dirt|@solid', 'oak_fence'],
  G: ['dirt|@solid', 'oak_fence_gate'],
  T: ['dirt|@solid', 'oak_fence', 'torch'],
  C: ['dirt|@solid', 'chest'],
  K: ['dirt|@solid', 'composter'],
  A: ['dirt|@solid', 'crafting_table'],
  F: ['grass_block', 'dandelion #lane'],
  t: ['dirt', 'oak_sapling #lane']
}
export function farmPlanToBlueprint (map, { name = 'farm', front = 'south' } = {}) {
  const rows = String(map).split('\n').map(r => r.trim()).filter(Boolean)
  const tokens = new Map()
  const tokenFor = spec => {
    if (!tokens.has(spec)) tokens.set(spec, String.fromCharCode(97 + tokens.size + (tokens.size >= 26 ? 39 : 0)))
    return tokens.get(spec)
  }
  const grid = dy => rows.map(row => [...row].map(ch => {
    const column = FARM_COLUMNS[ch]
    const spec = column?.[dy + 1]
    return spec ? tokenFor(spec) : '_'
  }).join(''))
  const grids = [-1, 0, 1].map(dy => `## y${dy}\n\n\`\`\`layer\n${grid(dy).join('\n')}\n\`\`\``).join('\n\n')
  const legend = [...tokens.entries()].map(([spec, token]) => `${token}  ${spec}`).join('\n')
  const text = `---\nname: ${name}\ntitle: ${name}\ndescription: a farm plan as a blueprint\ntags: farm\nfront: ${front}\nfoundation: flat\n---\n\n\`\`\`legend\n${legend}\n\`\`\`\n\n${grids}\n`
  return parseBlueprint(text)
}
