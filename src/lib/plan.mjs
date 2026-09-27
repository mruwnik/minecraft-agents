// Plans: the legend, parsing a plan into cells, its errors, lane reachability, bill, and one-line summary.

import { compact } from '../cli.mjs'
import { WORK_RANGE } from '../navigation/walk.mjs'
import { STEPS } from './world.mjs'
// ---------------------------------------------------------------- composite actions: plans
// A plan is an ASCII map of a farm or pen, one character per block, anchored at its NORTH-WEST corner: rows run south (z),
// columns east (x). `y` is the crop/floor level, so the ground under a cell is y-1: a crop stands at y on farmland at y-1,
// water lies AT y-1, a fence stands at y on dirt. The plan is the truth of what SHOULD be there; the world is what is.
// `ground` is what must lie at y-1; `seed` is the item a crop is planted from; `item` the block a structure is placed from.
export const PLAN_LEGEND = {
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
// Block to restore when a planned ground cell is missing. Sand supports cane;
// all other planned grounds are repaired with dirt.
const GROUND_ITEM = { sand: 'sand' }
export const groundItem = spec => GROUND_ITEM[spec.ground] ?? 'dirt'
const PLAN_MAX = 64

// the ASCII map as rows and cells; a space is a hole in the plan, not a cell
export function parsePlan (map) {
  const rows = String(map ?? '').replace(/\t/g, ' ').split('\n')
  const first = rows.findIndex(r => r.trim())
  if (first < 0) return { error: 'the map has no cells: give rows of legend characters, one character per block' }
  const last = rows.length - [...rows].reverse().findIndex(r => r.trim())
  const kept = rows.slice(first, last).map(r => r.replace(/\s+$/, ''))
  const width = Math.max(...kept.map(r => r.length))
  if (width > PLAN_MAX || kept.length > PLAN_MAX) return { error: `a plan is at most ${PLAN_MAX}x${PLAN_MAX} blocks (got ${width}x${kept.length})` }
  return { rows: kept, width, height: kept.length, cells: kept.flatMap((row, dz) => [...row].flatMap((ch, dx) => ch === ' ' ? [] : [{ dx, dz, ch }])) }
}

// Every cell in world coordinates: x east of the anchor, z south of it, y the GROUND block — the farmland, pen floor
// or path the plan describes, the level `till` asks for. What the plan puts on it (crop, fence, gate, torch, chest,
// composter, flower, sapling) stands at y+1; a water source lies AT y, with its cover at y+1.
export const planCells = place => (parsePlan(place.plan).cells ?? []).map(c => ({ ...c, x: place.x + c.dx, y: place.y, z: place.z + c.dz }))

// farmland stays wet within 4 blocks of a water source, level with it or one above it: a plan that breaks that rule
// turns back into dirt within minutes of being built. A gate in a corner is one nothing can ever walk through (see blindGates)
const BARRIER_KINDS = new Set(['fence', 'gate'])
export function planErrors (parsed) {
  if (parsed.error) return [parsed.error]
  const unknown = parsed.cells.filter(c => !PLAN_LEGEND[c.ch])
  if (unknown.length) return unknown.map(c => `${c.ch} at ${c.dx},${c.dz} is not in the legend (${Object.keys(PLAN_LEGEND).join(' ')})`)
  const waters = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'water')
  const dry = parsed.cells.filter(c => PLAN_LEGEND[c.ch].ground === 'farmland' &&
    !waters.some(w => Math.abs(w.dx - c.dx) <= 4 && Math.abs(w.dz - c.dz) <= 4))
  const at = (dx, dz) => parsed.cells.find(c => c.dx === dx && c.dz === dz)
  const kindAt = (dx, dz) => PLAN_LEGEND[at(dx, dz)?.ch]?.kind ?? null
  const walkable = kind => kind && !BARRIER_KINDS.has(kind)
  const blind = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'gate').filter(g =>
    ![[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) =>
      walkable(kindAt(g.dx + dx, g.dz + dz)) && !BARRIER_KINDS.has(kindAt(g.dx - dx, g.dz - dz))))
  // one complaint for the whole dry patch: a plan that forgot its channel used to answer with a line per cell
  const dryLine = dry.length
    ? [`${dry.length} cell${dry.length === 1 ? ' is' : 's are'} farmland with no water within 4 blocks (${dry.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${dry.length > 4 ? ` and ${dry.length - 4} more` : ''}): move the channel or shorten the row`]
    : []
  return [
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
  return !cell || LANE_KINDS.has(PLAN_LEGEND[cell.ch]?.kind)
}
const named = cells => `${cells.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${cells.length > 4 ? ` and ${cells.length - 4} more` : ''}`
export function planLane (cells) {
  const crops = (cells ?? []).filter(c => PLAN_LEGEND[c.ch]?.kind === 'crop')
  if (!crops.length) return {}
  const map = planMap(cells)
  const kindAt = (dx, dz) => PLAN_LEGEND[map.get(key(dx, dz))?.ch]?.kind ?? null
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
  for (const { ch } of parsed.cells ?? []) {
    const cell = PLAN_LEGEND[ch]
    if (!cell) continue
    if (cell.kind === 'crop') add(cell.seed)
    if (cell.cover) add(cell.cover)
    if (cell.kind === 'torch') add('torch')
    if (cell.item) add(cell.item)
  }
  if ((parsed.cells ?? []).some(c => PLAN_LEGEND[c.ch]?.kind === 'water')) add('water_bucket')
  return bill
}

// one line: how big it is and what is in it
export function planSummary (parsed) {
  if (parsed.error) return parsed.error
  const counts = {}
  for (const { ch } of parsed.cells) {
    const cell = PLAN_LEGEND[ch]
    const label = cell?.kind === 'crop' ? cell.crop.replace(/s$/, '') : cell?.kind ?? ch
    counts[label] = (counts[label] ?? 0) + 1
  }
  return `${parsed.width}x${parsed.height} ${compact(counts)}`
}

// Where the block a plan marks with one character really stands: the plan's y is the ground it sits on, so a chest,
// composter or torch is at y+1. Every composite that walks to one asks for it this way.
export function planStructure (cells, ch) {
  const cell = cells.find(c => c.ch === ch)
  return cell ? { x: cell.x, y: cell.y + 1, z: cell.z } : null
}
