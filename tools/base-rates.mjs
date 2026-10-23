// Why JavaScript: binary data; reads the worldgen JSON out of the server jar (a zip in a zip).
// Writes engine/src/jobs/lib/cost/base_rates.json, the worldgen numbers jobs.lib.cost.base-rates reads: per ore its
// placements (veins per chunk, vein size, height range and shape, the chance a block of the vein beside air is left out)
// and the biomes that carry each, and the blocks each ore replaces. The jar is needed
// only here, when the game version changes:
//   node tools/base-rates.mjs /path/to/mojang_<version>.jar
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { zipEntries, entryContent } from './view/jar-read.mjs'

const OUT = path.join(import.meta.dirname, '../engine/src/jobs/lib/cost/base_rates.json')
const ORES = ['coal', 'iron', 'copper', 'gold', 'redstone', 'lapis', 'diamond', 'emerald']
const bare = id => id.replace(/^minecraft:/, '')

// the ore a configured feature places (iron for iron_ore and deepslate_iron_ore), or undefined
const oreOf = configured => {
  if (!configured || bare(configured.type) !== 'ore') return undefined
  const names = new Set(configured.config.targets.map(t => bare(t.state.Name).replace(/^deepslate_/, '')))
  const [name] = names
  return names.size === 1 && name.endsWith('_ore') ? name.slice(0, -'_ore'.length) : undefined
}

const anchorY = (anchor, { min_y: minY, height }) => {
  if ('absolute' in anchor) return anchor.absolute
  if ('above_bottom' in anchor) return minY + anchor.above_bottom
  if ('below_top' in anchor) return minY + height - 1 - anchor.below_top
  throw new Error(`height anchor ${JSON.stringify(anchor)}`)
}

// mean veins a chunk gets from one count step
const meanCount = count => {
  if (typeof count === 'number') return count
  if (bare(count.type) === 'uniform') return (count.min_inclusive + count.max_inclusive) / 2
  throw new Error(`count ${JSON.stringify(count)}`)
}

// Each placement step's part of the row; a step that is not here would make the numbers wrong without a sign, so it throws.
const STEPS = {
  count: step => ({ count: meanCount(step.count) }),
  rarity_filter: step => ({ count: 1 / step.chance }),
  in_square: () => ({}),
  biome: () => ({}),
  height_range: ({ height }, dimension) => {
    const shape = bare(height.type)
    if (!['uniform', 'trapezoid'].includes(shape) || height.plateau) throw new Error(`height ${JSON.stringify(height)}`)
    return { shape, min: anchorY(height.min_inclusive, dimension), max: anchorY(height.max_inclusive, dimension) }
  }
}

const placementOf = (feature, placed, { size, discard_chance_on_air_exposure: air = 0 }, dimension) => {
  const parts = placed.placement.map(step => {
    const price = STEPS[bare(step.type)]
    if (!price) throw new Error(`${feature}: placement step ${bare(step.type)} is not priced`)
    return price(step, dimension)
  })
  const { count = 1, shape, min, max } = Object.assign({}, ...parts)
  return { feature, count, size, shape, min, max, air }
}

// the blocks a configured ore's targets replace (a tag's values, nested tags resolved)
const hostsOf = (made, tags) => {
  const blocks = name => (tags[name]?.values ?? []).flatMap(v => v.startsWith('#') ? blocks(bare(v.slice(1))) : [bare(v)])
  return made.config.targets.flatMap(({ target }) => target.tag ? blocks(bare(target.tag)) : target.block ? [bare(target.block)] : [])
}

// worldgen: {version, dimension (the overworld's dimension_type), placed, configured, biomes, tags (block tags)}, the last
// four as {name: parsed JSON}. A placement lists its biomes only when some biome with ores lacks it.
export function oreRates (worldgen, ores = ORES) {
  const { version, dimension, placed, configured, biomes, tags = {} } = worldgen
  const rows = Object.keys(placed).sort().flatMap(feature => {
    const made = configured[bare(placed[feature].feature)]
    const ore = oreOf(made)
    return ores.includes(ore) ? [{ ore, hosts: hostsOf(made, tags), ...placementOf(feature, placed[feature], made.config, dimension) }] : []
  })
  const carriers = feature => Object.keys(biomes).sort().filter(b => biomes[b].features.flat().map(bare).includes(feature))
  const withOres = [...new Set(rows.flatMap(r => carriers(r.feature)))].sort()
  const rowOf = ({ ore, hosts, ...row }) => {
    const inBiomes = carriers(row.feature)
    return inBiomes.length === withOres.length ? row : { ...row, biomes: inBiomes }
  }
  return {
    version,
    biomes: withOres,
    ores: Object.fromEntries(ores.map(ore => [ore, rows.filter(r => r.ore === ore).map(rowOf)])),
    hosts: Object.fromEntries(ores.map(ore => [ore, [...new Set(rows.filter(r => r.ore === ore).flatMap(r => r.hosts))].sort()]))
  }
}

const SERVER_JAR = /^META-INF\/versions\/([^/]+)\/server-[^/]+\.jar$/
const jsonUnder = (jar, dir) => Object.fromEntries(zipEntries(jar)
  .filter(e => e.name.startsWith(dir) && e.name.endsWith('.json'))
  .map(e => [e.name.slice(dir.length, -'.json'.length), JSON.parse(entryContent(jar, e))]))

// the worldgen JSON of a server jar as Mojang ships it: the real server jar lies inside the bundler jar
export function readWorldgen (bundle) {
  const inner = zipEntries(bundle).find(e => SERVER_JAR.test(e.name))
  if (!inner) throw new Error('no META-INF/versions/<version>/server-<version>.jar inside: not a server bundler jar')
  const jar = entryContent(bundle, inner)
  return {
    version: inner.name.match(SERVER_JAR)[1],
    dimension: jsonUnder(jar, 'data/minecraft/dimension_type/').overworld,
    placed: jsonUnder(jar, 'data/minecraft/worldgen/placed_feature/'),
    configured: jsonUnder(jar, 'data/minecraft/worldgen/configured_feature/'),
    biomes: jsonUnder(jar, 'data/minecraft/worldgen/biome/'),
    tags: jsonUnder(jar, 'data/minecraft/tags/block/')
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const jar = process.argv[2]
  if (!jar) throw new Error('usage: node tools/base-rates.mjs <server jar>')
  const rates = oreRates(readWorldgen(fs.readFileSync(jar)))
  fs.writeFileSync(OUT, JSON.stringify(rates, null, 2) + '\n')
  console.log(`[base-rates] ${OUT}: ${Object.values(rates.ores).flat().length} placements of ${ORES.length} ores, ${rates.biomes.length} biomes, version ${rates.version}`)
}
