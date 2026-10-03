// Compare a plan (a place with a `structure`, see src/lib/plan.mjs) with the blocks the bodies dumped.
//
// One status per constrained cell, judged on the block AT the cell (world y = place.y + layer.y; layers hold real
// block coordinates, so the cell's block is read directly, never y+1). Ground under a crop is not judged.
//
//   spec kind   | block at the cell that matches                                  | doubt
//   ------------+-----------------------------------------------------------------+---------------------------------
//   crop        | planCropMatches: the crop block (stem or attached_ stem);       | the farmland under it is not
//               | generic '*' accepts any farmland crop; cane = sugar_cane        | checked; a fruit never counts
//   water       | water (source or flowing: not told apart), or spec.cover: the   | cover is a waterlogged slab in
//               | covered channel's block IS the slab (seen live: oak_slab)       | the water cell, seen in the dump
//   path        | any solid-ish block: not air, not a liquid (the old bot never  | dirt_path vs dirt vs grass all
//               | checks path cells, it is just a floor)                          | pass; this is the loosest rule
//   ground      | exactly spec.ground (e.g. farmland)                             |
//   fence       | non-literal: any *_fence; literal: exactly spec.item            |
//   torch       | the post (any *_fence) or a torch/wall_torch/soul_torch there   | live plan had a torch in the T cell
//   gate        | any *_fence_gate (literal: spec.item)                           |
//   chest       | chest or trapped_chest (literal: spec.item)                     | barrel not accepted
//   composter, table | spec.item exactly                                          |
//   flower      | any flower (literal: spec.item)                                 | flower list is hand-made
//   sapling     | any *_sapling (literal: spec.item)                              |
//   tree        | the sapling, <species>_sapling or <species>_log (grown)         | leaves/wood not accepted
//   block       | spec.item exactly                                               | non-literal families unknown
//   air         | air, cave_air, void_air                                         |
//   reserved    | anything (the plan only says "leave it alone")                  |
//   '_' cell    | not constrained: status 'free', not counted                     |
//
// compareCell: actual null -> 'unknown' (no column dumped); match; air where something is expected -> 'missing';
// any other block -> 'wrong'. An air spec met by a non-air block is 'wrong'.
import fs from 'node:fs'
import path from 'node:path'
import { decodeColumnFile, columnCache, makeChunkClass } from '../../tools/view/columns.mjs'
import { planCropMatches, planSpec, parsePlacePlan } from '../../src/lib/plan.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air'])
const LIQUID = new Set(['water', 'lava'])
const FLOWERS = new Set(['dandelion', 'poppy', 'blue_orchid', 'allium', 'azure_bluet', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'torchflower', 'wither_rose', 'sunflower', 'lilac', 'rose_bush', 'peony', 'pink_petals'])
const isFlower = name => FLOWERS.has(name) || /_tulip$/.test(name)

const exact = spec => ({ label: spec.item, test: name => name === spec.item })
const family = (spec, test) => spec.literal ? exact(spec) : { label: spec.item, test }

const RULES = {
  crop: spec => ({ label: spec.generic ? 'any crop' : spec.crop, test: name => planCropMatches(spec, name) }),
  water: spec => ({ label: 'water', test: name => name === 'water' || name === spec.cover }),
  path: () => ({ label: 'any floor', test: name => !AIR.has(name) && !LIQUID.has(name) }),
  ground: spec => ({ label: spec.ground, test: name => name === spec.ground }),
  fence: spec => family(spec, name => /_fence$/.test(name)),
  torch: spec => ({ label: spec.item, test: name => /_fence$/.test(name) || /torch$/.test(name) }),
  gate: spec => family(spec, name => /_fence_gate$/.test(name)),
  chest: spec => family(spec, name => name === 'chest' || name === 'trapped_chest'),
  composter: exact,
  table: exact,
  flower: spec => family(spec, isFlower),
  sapling: spec => family(spec, name => /_sapling$/.test(name)),
  tree: spec => ({ label: `${spec.species} tree`, test: name => [spec.item, `${spec.species}_sapling`, `${spec.species}_log`].includes(name) }),
  block: exact,
  air: () => ({ label: 'air', test: name => AIR.has(name) }),
  reserved: () => ({ label: 'anything', test: () => true })
}

// {label, test(name) -> bool}: what the actual block must be for this cell spec
export function expectedAt (spec) {
  const rule = RULES[spec?.kind]
  return rule ? rule(spec) : { label: `?${spec?.kind}`, test: () => false }
}

export function compareCell (spec, actualName) {
  if (actualName == null) return 'unknown'
  if (expectedAt(spec).test(actualName)) return 'match'
  return AIR.has(actualName) ? 'missing' : 'wrong'
}

const blockYs = cells => cells.map(c => c.blockY)

export function listPlans (places) {
  return (places ?? []).flatMap(place => {
    if (!place?.structure) return []
    const parsed = parsePlacePlan(place)
    if (parsed.error) return []
    const ys = blockYs(parsed.cells).map(y => place.y + y)
    const bounds = {
      x1: place.x, y1: Math.min(...ys), z1: place.z,
      x2: place.x + parsed.width - 1, y2: Math.max(...ys), z2: place.z + parsed.height - 1
    }
    return [{ name: place.name, kind: place.kind, by: place.by, note: place.note, x: place.x, y: place.y, z: place.z, bounds, cells: parsed.cells.length, layers: place.structure.layers.length }]
  })
}

export function comparePlan ({ place, blockAt }) {
  const parsed = parsePlacePlan(place)
  if (parsed.error) throw new Error(parsed.error)
  const specs = new Map(parsed.cells.map(c => [`${c.dx},${c.blockY},${c.dz}`, c]))
  const counts = { match: 0, missing: 0, wrong: 0, unknown: 0 }
  const layers = place.structure.layers.map(layer => ({
    y: layer.y,
    rows: layer.rows.map((row, dz) => [...row].map((ch, dx) => {
      const cell = specs.get(`${dx},${layer.y},${dz}`)
      if (!cell) return { ch, status: 'free', expected: null, actual: null }
      const spec = planSpec(cell)
      const actual = blockAt(place.x + dx, place.y + layer.y, place.z + dz)
      const status = compareCell(spec, actual)
      counts[status]++
      return { ch, status, expected: expectedAt(spec).label, actual }
    }))
  }))
  const total = parsed.cells.length
  return { name: place.name, total, ...counts, percent: total ? Math.round(counts.match / total * 100) : 0, layers }
}

// blockAt over the dumped columns: state/worlds/<world>/chunks/<cx>.<cz>.bin, each read on first use and again when its
// mtime changes. The game version comes from the first column's header.
export function createWorldBlocks ({ stateDir, world }) {
  const dir = path.join(stateDir, 'worlds', world, 'chunks')
  let columns = null
  const columnsFor = file => {
    if (columns) return columns
    const header = decodeColumnFile(fs.readFileSync(file)).header
    columns = columnCache(makeChunkClass(header.mcVersion))
    return columns
  }
  const blockAt = (x, y, z) => {
    const file = path.join(dir, `${x >> 4}.${z >> 4}.bin`)
    if (!fs.existsSync(file)) return null
    const column = columnsFor(file).get(file)
    if (!column) return null
    try { return column.getBlock({ x: x & 15, y, z: z & 15 })?.name ?? null } catch { return null }
  }
  return { blockAt, close: () => { columns = null } }
}
