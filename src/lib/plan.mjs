import { readStructureLayers } from '../structure/layers.mjs'
// Plans: the legend, parsing a plan into cells, its errors, lane reachability, bill, and one-line summary.

import { treePlantProfile, treeProfile } from '../tree/profiles.mjs'
import { compact } from '../cli.mjs'
import { WORK_RANGE } from '../navigation/walk.mjs'
import { STEPS } from './world.mjs'
// ---------------------------------------------------------------- composite actions: plans
// A plan is an ASCII map of a farm or pen, one character per block, anchored at its NORTH-WEST corner: rows run south (z),
// columns east (x). `y` is the crop/floor level, so the ground under a cell is y-1: a crop stands at y on farmland at y-1,
// water lies AT y-1, a fence stands at y on dirt. The plan is the truth of what SHOULD be there; the world is what is.
// `ground` is what must lie at y-1; `seed` is the item a crop is planted from; `item` the block a structure is placed from.
export const PLAN_LEGEND = {
  '*': { kind: 'crop', crop: 'crop', ground: 'farmland', generic: true },
  w: { kind: 'crop', crop: 'wheat', seed: 'wheat_seeds', ground: 'farmland' },
  c: { kind: 'crop', crop: 'carrots', seed: 'carrot', ground: 'farmland' },
  p: { kind: 'crop', crop: 'potatoes', seed: 'potato', ground: 'farmland' },
  b: { kind: 'crop', crop: 'beetroots', seed: 'beetroot_seeds', ground: 'farmland' },
  s: { kind: 'crop', crop: 'sugar_cane', seed: 'sugar_cane', ground: 'sand' },
  m: { kind: 'crop', crop: 'melon_stem', seed: 'melon_seeds', ground: 'farmland' },
  k: { kind: 'crop', crop: 'pumpkin_stem', seed: 'pumpkin_seeds', ground: 'farmland' },
  B: { kind: 'crop', crop: 'bamboo', seed: 'bamboo', ground: 'dirt' },
  // a channel is built COVERED: a slab laid in the source keeps the water (and the farmland wet) but leaves a floor to
  // walk on. Open water in a field is a trap - the body wades in, `dig` refuses every block beside it, and the
  // pathfinder will not cross it, which is how Chani ended up walled into her own plan
  '~': { kind: 'water', ground: 'water', cover: 'oak_slab' },
  '.': { kind: 'path', ground: 'dirt' },
  '#': { kind: 'fence', item: 'oak_fence', ground: 'dirt' },
  G: { kind: 'gate', item: 'oak_fence_gate', ground: 'dirt' },
  T: { kind: 'torch', item: 'oak_fence', ground: 'dirt' },
  C: { kind: 'chest', item: 'chest', ground: 'dirt' },
  K: { kind: 'composter', item: 'composter', ground: 'dirt' },
  F: { kind: 'flower', item: 'dandelion', ground: 'grass_block' },
  t: { kind: 'sapling', item: 'oak_sapling', ground: 'dirt' },
  A: { kind: 'table', item: 'crafting_table', ground: 'dirt' }
}
// Custom legends overlay presets, and are resolved once on cells. Strings mean
// literal block identity; predicates such as generic crop require an explicit spec.
export const planItemMatches = (spec, name, family) => spec?.literal ? spec.item === name : family(spec?.item, name)
export const planSpec = cell => cell?.spec ?? PLAN_LEGEND[cell?.ch]
export function resolveLegend (value = {}) {
  if (typeof value === 'string') { try { value = JSON.parse(value) } catch { throw new Error('legend must be a JSON object') } }
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('legend must be an object')
  const out = {}
  const blockId = value => {
    if (typeof value !== 'string' || !/^(minecraft:)?[a-z][a-z0-9_]*$/.test(value)) throw new Error(`invalid block ID ${JSON.stringify(value)}`)
    return value.replace(/^minecraft:/, '')
  }
  for (const [ch, input] of Object.entries(value)) {
    if ([...ch].length !== 1 || /\s/.test(ch)) throw new Error('legend keys must be single non-space characters')
    let spec
    if (typeof input === 'string') {
      const item = blockId(input)
      const tree = treePlantProfile(item)
      const known = Object.values(PLAN_LEGEND).find(s => s.crop === item || (s.item === item && s.kind !== 'torch'))
      spec = tree ? { kind: 'tree', species: tree.species, form: 'auto', item, ground: tree.soil[0] }
        : known ? { ...known } : /_fence_gate$/.test(item) ? { kind: 'gate', item, ground: 'dirt' } : /_fence$/.test(item) ? { kind: 'fence', item, ground: 'dirt' } : ['barrel', 'trapped_chest'].includes(item) ? { kind: 'chest', item, ground: 'dirt' } : item === 'water' ? { ...PLAN_LEGEND['~'] }
          : { kind: /(?:_flower|_tulip)$/.test(item) || ['poppy', 'dandelion', 'blue_orchid', 'allium', 'azure_bluet', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'torchflower', 'wither_rose'].includes(item) ? 'flower' : 'block', item, ground: 'dirt' }
    } else {
      if (!input || typeof input !== 'object' || Array.isArray(input)) throw new Error(`invalid legend definition for ${ch}`)
      spec = { ...input }
      if (spec.ground_offset !== undefined && (spec.kind !== 'tree' || !Number.isSafeInteger(spec.ground_offset))) throw new Error('ground_offset must be a safe integer on a tree definition')
      if (spec.kind === 'tree') {
        const tree = treeProfile(spec.species, spec.form ?? 'auto')
        spec = { ...spec, species: tree.species, form: spec.form ?? 'auto', item: tree.plant, ground: tree.soil[0] }
      } else if (spec.kind === 'crop' && spec.generic === true) spec = { ...PLAN_LEGEND['*'] }
      else if (spec.kind === 'crop') {
        const known = Object.values(PLAN_LEGEND).find(s => s.kind === 'crop' && !s.generic && s.crop === blockId(spec.crop))
        if (!known) throw new Error(`unknown crop ${spec.crop}`)
        spec = { ...known }
      } else {
        if (!['block', 'flower', 'sapling', 'fence', 'gate', 'chest', 'composter', 'table', 'path', 'water', 'reserved', 'air', 'ground', 'torch'].includes(spec.kind)) throw new Error(`invalid legend kind ${spec.kind}`)
        if (spec.kind === 'path' || spec.kind === 'ground') spec.ground = blockId(spec.ground ?? 'dirt')
        else if (spec.kind === 'water') spec = { ...PLAN_LEGEND['~'], ...spec }
        else if (!['reserved', 'air'].includes(spec.kind)) spec.item = blockId(spec.item)
        if (spec.ground) spec.ground = blockId(spec.ground)
      }
    }
    if (typeof input === 'object' && input.literal !== undefined && typeof input.literal !== 'boolean') throw new Error('literal must be boolean')
    out[ch] = { ...spec, literal: typeof input === 'object' ? input.literal ?? true : true }
  }
  return out
}
// Generic beds accept crops sown on farmland. Cane and bamboo need their own
// ground; a fruit is harvested, never mistaken for a planted stem.
export const FARMLAND_CROPS = Object.values(PLAN_LEGEND).filter(s => s.kind === 'crop' && s.ground === 'farmland' && !s.generic)
export const planCropMatches = (spec, name) => spec?.kind === 'crop' && (spec.generic
  ? FARMLAND_CROPS.some(s => planCropMatches(s, name))
  : name === spec.crop || (spec.crop.endsWith('_stem') && name === `attached_${spec.crop}`))
export const GENERIC_SEED = 'crop_seed'
// Block to restore when a planned ground cell is missing. Sand supports cane;
// all other planned grounds are repaired with dirt.
const GROUND_ITEM = { farmland: 'dirt', grass_block: 'dirt', water: 'dirt' }
export const groundItem = spec => GROUND_ITEM[spec.ground] ?? spec.ground ?? 'dirt'
const PLAN_MAX = 64

export const hasPlan = place => Boolean(place?.plan || place?.structure)
export const parsePlacePlan = place => place?.structure ? parseStructurePlan(place.structure) : { error: 'legacy 2D plan: import it with farm.plan map= or migrate the saved map to structure.layers first' }
export function parseStructurePlan (structure) {
  try {
    if (typeof structure === 'string') structure = JSON.parse(structure)
    if (!structure || typeof structure !== 'object' || Array.isArray(structure) || Object.keys(structure).some(k => !['legend', 'layers'].includes(k))) throw new Error('maintenance structure supports legend and layers only; use blueprint.build for materials, objects, spaces or construction recipes')
    const legend = {}
    for (const [ch, input] of Object.entries(structure.legend ?? {})) {
      if (ch === '.' || ch === '_') throw new Error('layered legend reserves . for explicit air and _ for unconstrained space')
      if (typeof input === 'object' && input !== null && !Array.isArray(input)) {
        if (input.ground_offset !== undefined) throw new Error('layered tree elevations come from layer.y; ground_offset is only for legacy 2D maps')
        if (input.require) {
          if (Object.keys(input).length !== 1 || !['air', 'preserve'].includes(input.require)) throw new Error(`invalid space requirement for ${ch}`)
          legend[ch] = { kind: input.require === 'air' ? 'air' : 'reserved' }
          continue
        }
        if (input.kind) { legend[ch] = input; continue }
        if (Object.keys(input).some(k => !['block', 'type'].includes(k)) || !input.block || !['block', 'crop', 'water', 'farmland', undefined].includes(input.type)) throw new Error(`unsupported maintenance recipe for ${ch}; materials and construction state belong to blueprint.build`)
        if (input.type === 'crop' && !FARMLAND_CROPS.some(s => s.crop === input.block)) throw new Error(`invalid crop block ${input.block}`)
        if (['water', 'farmland'].includes(input.type) && input.block !== input.type) throw new Error(`recipe ${input.type} requires block=${input.type}`)
        legend[ch] = input.block
      } else legend[ch] = input
      if (legend[ch] === 'air' || legend[ch] === 'minecraft:air') legend[ch] = { kind: 'air' }
      if (legend[ch] === 'farmland' || legend[ch] === 'minecraft:farmland') legend[ch] = { kind: 'ground', ground: 'farmland' }
      if (legend[ch] === 'water' || legend[ch] === 'minecraft:water') legend[ch] = { kind: 'water', ground: 'water', cover: null }
    }
    const custom = resolveLegend(legend)
    const geometry = readStructureLayers(structure.layers)
    if (!geometry.cells.length) throw new Error('the structure has no constrained cells')
    const cells = geometry.cells.flatMap(c => {
      const spec = c.token === '.' ? { kind: 'air' } : custom[c.token]
      if (!spec) throw new Error(`unknown layered token ${c.token} at ${c.x},${c.y},${c.z}`)
      const groundLevel = ['water', 'path', 'ground', 'reserved'].includes(spec.kind)
      return [{ dx: c.x, dy: c.y - (groundLevel ? 0 : 1), dz: c.z, blockY: c.y, ch: c.token, spec, layered: true }]
    })
    if (!cells.length) throw new Error('the structure has no maintained cells')
    return { rows: structure.layers[0].rows, width: geometry.width, height: geometry.depth, cells, layered: true, structure, minY: Math.min(...cells.map(c => c.blockY)), maxY: Math.max(...cells.map(c => c.blockY)) }
  } catch (error) { return { error: error.message } }
}

// Explicit one-way importer. Legacy map symbols are ground anchors; layered
// coordinates name actual blocks. Preserve maintenance recipes, not just pictures.
export function legacyPlanStructure (map, legend) {
  const parsed = parsePlan(map, legend)
  if (parsed.error) throw new Error(parsed.error)
  const layers = new Map(), outLegend = {}, tokens = new Map()
  let next = 0xe000
  for (const cell of parsed.cells) {
    const original = planSpec(cell)
    if (!original) throw new Error(`unknown legacy token ${cell.ch}`)
    const spec = { ...original, literal: Boolean(original.literal) }
    const offset = spec.kind === 'tree' ? spec.ground_offset ?? 0 : 0
    delete spec.ground_offset
    let ch = tokens.get(cell.ch)
    if (!ch) {
      ch = ['.', '_'].includes(cell.ch) ? String.fromCodePoint(next++) : cell.ch
      while (Object.hasOwn(outLegend, ch)) ch = String.fromCodePoint(next++)
      tokens.set(cell.ch, ch)
      outLegend[ch] = spec
    }
    const y = offset + (['water', 'path', 'ground', 'reserved'].includes(spec.kind) ? 0 : 1)
    if (!layers.has(y)) layers.set(y, Array.from({ length: parsed.height }, () => Array(parsed.width).fill('_')))
    layers.get(y)[cell.dz][cell.dx] = ch
  }
  const structure = { legend: outLegend, layers: [...layers].sort(([a], [b]) => a - b).map(([y, rows]) => ({ y, rows: rows.map(row => row.join('')) })) }
  const checked = parseStructurePlan(structure)
  if (checked.error) throw new Error(checked.error)
  return structure
}
export function migratePlan (place) {
  if (!hasPlan(place)) return { ...place }
  const structure = place.structure ?? legacyPlanStructure(place.plan, place.legend)
  const parsed = parseStructurePlan(structure)
  if (parsed.error) throw new Error(`${place.name ?? 'plan'}: ${parsed.error}`)
  const { plan, legend, ...rest } = place
  return { ...rest, structure }
}

// the ASCII map as rows and cells; a space is a hole in the plan, not a cell
export function parsePlan (map, legend, callbackArray) {
  if (map && typeof map === 'object') return parseStructurePlan(map.structure ?? map)
  if (Array.isArray(callbackArray) && typeof legend === 'number') legend = undefined
  let custom
  try { custom = legend === undefined ? null : resolveLegend(legend) } catch (e) { return { error: e.message } }
  const rows = String(map ?? '').replace(/\t/g, ' ').split('\n')
  const first = rows.findIndex(r => r.trim())
  if (first < 0) return { error: 'the map has no cells: give rows of legend characters, one character per block' }
  const last = rows.length - [...rows].reverse().findIndex(r => r.trim())
  const kept = rows.slice(first, last).map(r => r.replace(/\s+$/, ''))
  const width = Math.max(...kept.map(r => [...r].length))
  if (width > PLAN_MAX || kept.length > PLAN_MAX) return { error: `a plan is at most ${PLAN_MAX}x${PLAN_MAX} blocks (got ${width}x${kept.length})` }
  return { rows: kept, width, height: kept.length, cells: kept.flatMap((row, dz) => [...row].flatMap((ch, dx) => ch === ' ' ? [] : [{ dx, dz, ch, ...(custom?.[ch] ? { spec: custom[ch] } : {}) }])) }
}

// Every cell in world coordinates: x east of the anchor, z south of it, y the GROUND block — the farmland, pen floor
// or path the plan describes, the level `till` asks for. What the plan puts on it (crop, fence, gate, torch, chest,
// composter, flower, sapling) stands at y+1; a water source lies AT y, with its cover at y+1.
export const planCells = place => {
  if (!hasPlan(place)) return []
  const parsed = parsePlacePlan(place)
  if (parsed.error) throw new Error(parsed.error)
  return parsed.cells.map(c => ({ ...c, x: place.x + c.dx, y: place.y + (c.dy ?? 0) + (planSpec(c)?.kind === 'tree' ? planSpec(c).ground_offset ?? 0 : 0), z: place.z + c.dz }))
}

// A different layer owns its declared cell and any required supporting ground.
// Low clearing for one bed must never consume the floor/crop of a stacked bed.
export function planClaimAt (cells, pos, except) {
  return cells.some(c => {
    if (c === except || c.x !== pos.x || c.z !== pos.z) return false
    const s = planSpec(c)
    if (!s || s.kind === 'air') return false
    if (s.ground && pos.y === c.y) return true
    const atY = ['water', 'path', 'ground', 'reserved'].includes(s.kind) ? c.y : c.y + 1
    return pos.y === atY || (s.kind === 'torch' && pos.y === c.y + 2)
  })
}
export const jobGroundKey = job => `${job.x},${job.groundY ?? (job.do === 'plant' || job.do === 'place' ? job.y - 1 : job.do === 'pour' ? job.y + 1 : job.y)},${job.z}`

// farmland stays wet within 4 blocks of a water source, level with it or one above it: a plan that breaks that rule
// turns back into dirt within minutes of being built. A gate in a corner is one nothing can ever walk through (see blindGates)
const BARRIER_KINDS = new Set(['fence', 'gate'])
export function planErrors (parsed) {
  if (parsed.error) return [parsed.error]
  const unknown = parsed.cells.filter(c => !planSpec(c))
  if (unknown.length) return unknown.map(c => `${c.ch} at ${c.dx},${c.dz} is not in the legend (${Object.keys(PLAN_LEGEND).join(' ')})`)
  const waters = parsed.cells.filter(c => planSpec(c).kind === 'water')
  const dry = parsed.cells.filter(c => planSpec(c).ground === 'farmland' &&
    !waters.some(w => Math.abs(w.dx - c.dx) <= 4 && Math.abs(w.dz - c.dz) <= 4 && (w.dy ?? 0) >= (c.dy ?? 0) && (w.dy ?? 0) <= (c.dy ?? 0) + 1))
  const at = (dx, dz, dy = 0) => parsed.cells.find(c => c.dx === dx && c.dz === dz && (c.dy ?? 0) === dy)
  const kindAt = (dx, dz, dy = 0) => planSpec(at(dx, dz, dy))?.kind ?? null
  const walkable = kind => kind && !BARRIER_KINDS.has(kind)
  const blind = parsed.cells.filter(c => planSpec(c).kind === 'gate').filter(g =>
    ![[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) =>
      walkable(kindAt(g.dx + dx, g.dz + dz, g.dy ?? 0)) && !BARRIER_KINDS.has(kindAt(g.dx - dx, g.dz - dz, g.dy ?? 0))))
  // one complaint for the whole dry patch: a plan that forgot its channel used to answer with a line per cell
  const dryLine = dry.length
    ? [`${dry.length} cell${dry.length === 1 ? ' is' : 's are'} farmland with no water within 4 blocks (${dry.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${dry.length > 4 ? ` and ${dry.length - 4} more` : ''}): move the channel or shorten the row`]
    : []
  const trees = parsed.cells.filter(c => planSpec(c)?.kind === 'tree')
  const footprints = trees.flatMap(c => {
    const p = treeProfile(planSpec(c).species, planSpec(c).form)
    if (p.width === 1) return []
    return [[1, 0], [0, 1], [1, 1]].flatMap(([dx, dz]) => {
      const other = at(c.dx + dx, c.dz + dz, c.dy ?? 0)
      return other && planSpec(other)?.kind !== 'reserved' ? [`tree at ${c.dx},${c.dz} needs reserved or unmapped 2x2 footprint; conflicts with ${other.ch} at ${other.dx},${other.dz}`] : []
    })
  })
  return [
    ...footprints,
    ...dryLine,
    ...blind.map(g => `the gate at ${g.dx},${g.dz} is in a corner: nothing can walk through it. Put it in the middle of a wall`)
  ]
}

// A walk into a field crosses planted cells only where it must (at ten steps a cell, at a walking pace), so every crop
// a walk can get beside is reachable; but a job stands on dry footing within WORK_RANGE of the cell it works and never
// in a planted cell (noStanding refuses those): one up and four across is 4.12, so a lane four blocks from a bed serves
// it and a covered channel every eight rows serves a whole field. A crop with nothing to stand on within that reach is
// a crop no job can be done on, however the walk went: every till, plant and pour there answers "nowhere to stand".
// Chani's carrot patch sandwiched its water row between two carrot rows and left nothing but crops between the gate
// and the far row, and she took her own plan's fault for a tool bug (BUGS.md 09-23); jizo-melon-patch's census called
// 140 cells stranded for want of a cell BESIDE them, on a field a sweep had just harvested end to end from its channels
// (09-26). A plan can be told this before it is built, and a field that already stands can be asked. Stood on: a path,
// a covered channel, a gate, a flower, a sapling - the ones a body passes without breaking - and anything outside the
// plan, because the plan says nothing about it and the ground around a farm is where a walk starts from. Reached over:
// crops, which are seen over and walked through but never stood in. In the way of the arm: fences, gate panels seen from outside, torch posts, chests,
// composters, tables - anything taller than a crop on the straight line between the lane cell and the bed.
const LANE_KINDS = new Set(['path', 'water', 'gate', 'flower', 'sapling'])
// the arm works a bed from a lane cell one up and beside it: dx²+dz²+1 <= WORK_RANGE², four across at most
const LANE_REACH2 = WORK_RANGE * WORK_RANGE - 1
const LANE_REACH = Math.floor(Math.sqrt(LANE_REACH2))
// seen over from a lane: crops, and every lane kind but a gate, whose panel is a fence's height when it is not the cell stood in
const SEEN_OVER = new Set(['crop', 'path', 'water', 'flower', 'sapling'])
const tallKind = kind => Boolean(kind) && !SEEN_OVER.has(kind)
// nothing taller than a crop on the line between two cell centres, sampled every quarter block
const clearBetween = (kindAt, from, to) => {
  const steps = Math.ceil(Math.hypot(to.dx - from.dx, to.dz - from.dz) * 4)
  return Array.from({ length: Math.max(steps - 1, 0) }, (_, i) => (i + 1) / steps)
    .map(t => [Math.floor(from.dx + 0.5 + (to.dx - from.dx) * t), Math.floor(from.dz + 0.5 + (to.dz - from.dz) * t)])
    .filter(([dx, dz]) => !(dx === from.dx && dz === from.dz) && !(dx === to.dx && dz === to.dz))
    .every(([dx, dz]) => !tallKind(kindAt(dx, dz)))
}
const key = (dx, dz) => `${dx},${dz}`
const planMap = cells => new Map(cells.map(c => [key(c.dx, c.dz), c]))
// a cell of the plan a body stands on (LANE_KINDS), or ground outside the plan, which the plan says nothing about
const standableIn = map => (dx, dz) => {
  const cell = map.get(key(dx, dz))
  return !cell || LANE_KINDS.has(planSpec(cell)?.kind)
}
const named = cells => `${cells.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${cells.length > 4 ? ` and ${cells.length - 4} more` : ''}`
export function planLane (cells) {
  const levels = [...new Set((cells ?? []).map(c => c.dy ?? c.y ?? 0))]
  if (levels.length > 1) {
    const notes = levels.map(y => planLane(cells.filter(c => (c.dy ?? c.y ?? 0) === y)).noLane).filter(Boolean)
    return notes.length ? { noLane: notes.join('; ') } : {}
  }
  const crops = (cells ?? []).filter(c => planSpec(c)?.kind === 'crop')
  if (!crops.length) return {}
  const map = planMap(cells)
  const kindAt = (dx, dz) => planSpec(map.get(key(dx, dz)))?.kind ?? null
  const bounds = ['dx', 'dz'].map(k => [Math.min(...cells.map(c => c[k])) - 1, Math.max(...cells.map(c => c[k])) + 1])
  const inside = (dx, dz) => dx >= bounds[0][0] && dx <= bounds[0][1] && dz >= bounds[1][0] && dz <= bounds[1][1]
  const standable = standableIn(map)
  // flood in from the ring around the plan, which is all ground the plan never claimed
  const reached = new Set()
  const queue = []
  for (let dx = bounds[0][0]; dx <= bounds[0][1]; dx++) for (const dz of bounds[1]) queue.push([dx, dz])
  for (let dz = bounds[1][0]; dz <= bounds[1][1]; dz++) for (const dx of bounds[0]) queue.push([dx, dz])
  while (queue.length) {
    const [dx, dz] = queue.pop()
    if (reached.has(key(dx, dz)) || !inside(dx, dz) || !standable(dx, dz)) continue
    reached.add(key(dx, dz))
    for (const [sx, sz] of STEPS) queue.push([dx + sx, dz + sz])
  }
  const lanes = [...reached].map(k => k.split(',').map(Number)).map(([dx, dz]) => ({ dx, dz }))
  const served = crop => lanes.some(lane =>
    (lane.dx - crop.dx) ** 2 + (lane.dz - crop.dz) ** 2 <= LANE_REACH2 && clearBetween(kindAt, lane, crop))
  const stranded = crops.filter(c => !served(c))
  if (!stranded.length) return {}
  const subject = stranded.length === 1
    ? `1 crop cell has nothing to stand on within ${LANE_REACH} of it`
    : `${stranded.length} crop cells have nothing to stand on within ${LANE_REACH} of them`
  return { noLane: `${subject} (${named(stranded)}): lay a . path or a ~ channel through the rows, eight rows apart at most, or every job there answers nowhere to stand (a walk crosses the rows where it must, but a job never stands in one)` }
}

// what it takes to build this plan from nothing: one water bucket does the whole field, a torch cell needs its post too
export function planBill (parsed) {
  const bill = {}
  const add = (item, n = 1) => { bill[item] = (bill[item] ?? 0) + n }
  for (const entry of parsed.cells ?? []) {
    const cell = planSpec(entry)
    if (!cell) continue
    if (cell.kind === 'crop') add(cell.generic ? GENERIC_SEED : cell.seed)
    if (cell.cover) add(cell.cover)
    if (cell.kind === 'torch') add('torch')
    if (cell.item) add(cell.item, cell.kind === 'tree' ? treeProfile(cell.species, cell.form).width ** 2 : 1)
  }
  if ((parsed.cells ?? []).some(c => planSpec(c)?.kind === 'water')) add('water_bucket')
  return bill
}

// one line: how big it is and what is in it
export function planSummary (parsed) {
  if (parsed.error) return parsed.error
  const counts = {}
  for (const entry of parsed.cells) {
    const ch = entry.ch
    const cell = planSpec(entry)
    const label = cell?.kind === 'crop' ? cell.crop.replace(/s$/, '') : cell?.kind ?? ch
    counts[label] = (counts[label] ?? 0) + 1
  }
  return `${parsed.width}x${parsed.layered ? `${parsed.maxY - parsed.minY + 1}x` : ""}${parsed.height} ${compact(counts)}`
}

// Where the block a plan marks with one character really stands: the plan's y is the ground it sits on, so a chest,
// composter or torch is at y+1. Every composite that walks to one asks for it this way.
export function planStructure (cells, ch) {
  const expected = PLAN_LEGEND[ch]
  const cell = cells.find(c => expected ? planSpec(c)?.kind === expected.kind : c.ch === ch)
  return cell ? { x: cell.x, y: cell.y + 1, z: cell.z } : null
}
