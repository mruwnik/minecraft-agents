// Why JavaScript: GPU data; biome tint colours packed for the WebGL shader tables.
// Per-biome tint colours for the browser view: per biome name (map to ids through the world's own biome registry: biomeTable) four RGB
// triples: grass, foliage, dry foliage, water. From the client jar's colormaps and worldgen biome json by vanilla's rules, else
// from minecraft-data's tints.json (source 'fallback').
// Simplifications: the swamp grass modifier is vanilla's noise between #4C763C and #6A7039, here fixed at #6A7039; a colormap index
// outside the image uses the plains colour (vanilla's own fallback is magenta, which would only show a bug).
import fs from 'node:fs'
import minecraftData from 'minecraft-data'
import tints from 'minecraft-data/minecraft-data/data/pc/26.1/tints.json' with { type: 'json' }
import { decodePng } from './renderer.mjs'
import { zipEntries, entryContent, findClientJar } from './jar-read.mjs'

export const PLAINS_GRASS = [124, 189, 107]
export const PLAINS_FOLIAGE = [89, 174, 48]
export const PLAINS_DRY = [160, 112, 47]
const DEFAULT_WATER = 0x3F76E4
const SWAMP_GRASS = 0x6A7039

const rgb = n => [(n >> 16) & 255, (n >> 8) & 255, n & 255]
const parseHex = s => parseInt(s.slice(1), 16)
const clamp01 = v => Math.min(1, Math.max(0, v))
const darkForest = c => (((c & 0xFEFEFE) + 0x28340A) >> 1)

const lookup = (image, fallback, temperature, downfall) => {
  const t = clamp01(temperature)
  const d = clamp01(downfall) * t
  const x = Math.floor((1 - t) * 255)
  const y = Math.floor((1 - d) * 255)
  const at = (y * image.width + x) * 4
  return x < image.width && y < image.height ? [...image.rgba.subarray(at, at + 3)] : fallback
}

const BIOME_DIR = 'data/minecraft/worldgen/biome/'

const fromJar = jar => {
  const buf = fs.readFileSync(jar)
  const entries = new Map(zipEntries(buf).map(e => [e.name, e]))
  const read = name => entryContent(buf, entries.get(name))
  const map = name => decodePng(read(`assets/minecraft/textures/colormap/${name}.png`))
  const [grass, foliage, dry] = ['grass', 'foliage', 'dry_foliage'].map(map)
  const names = [...entries.keys()].filter(n => n.startsWith(BIOME_DIR) && n.endsWith('.json')).map(n => n.slice(BIOME_DIR.length, -5))
  return new Map(names.map(name => {
    const { temperature, downfall, effects = {} } = JSON.parse(read(`${BIOME_DIR}${name}.json`).toString('utf8'))
    const pick = (hex, image, fallback) => hex ? rgb(parseHex(hex)) : lookup(image, fallback, temperature, downfall)
    const base = pick(effects.grass_color, grass, PLAINS_GRASS)
    const packed = (base[0] << 16) | (base[1] << 8) | base[2]
    const modified = { dark_forest: rgb(darkForest(packed)), swamp: rgb(SWAMP_GRASS) }[effects.grass_color_modifier] ?? base
    return [name, Uint8Array.from([
      ...modified,
      ...pick(effects.foliage_color, foliage, PLAINS_FOLIAGE),
      ...pick(effects.dry_foliage_color, dry, PLAINS_DRY),
      ...rgb(effects.water_color ? parseHex(effects.water_color) : DEFAULT_WATER)
    ])]
  }))
}

const fallbackColors = names => {
  const waterOf = new Map(tints.water.data.flatMap(e => e.keys.map(k => [k, rgb(e.color)])))
  const colorOf = (group, name, plains) => {
    const entry = tints[group]?.data.find(e => e.keys.includes(name))
    return entry?.color ? rgb(entry.color) : plains
  }
  return new Map(names.map(name => [name, Uint8Array.from([
    ...colorOf('grass', name, PLAINS_GRASS),
    ...colorOf('foliage', name, PLAINS_FOLIAGE),
    ...PLAINS_DRY,
    ...(waterOf.get(name) ?? rgb(DEFAULT_WATER))
  ])]))
}

const cache = new Map()

// opts.jar: undefined finds the client jar; null or a missing path means no jar (fallback). Cached per version and jar.
// byName covers every biome json in the jar plus minecraft-data's names (jar wins), so names the data lacks (sulfur_caves) work.
export const biomeColorsByName = (version, { jar } = {}) => {
  const file = jar === undefined ? findClientJar() : jar
  const usable = file && fs.existsSync(file) ? file : null
  const key = `${version}\0${usable}`
  if (cache.has(key)) return cache.get(key)
  const dataNames = minecraftData(version).biomesArray.map(b => b.name)
  const result = {
    source: usable ? 'jar' : 'fallback',
    byName: new Map([...fallbackColors(dataNames), ...(usable ? fromJar(usable) : [])])
  }
  cache.set(key, result)
  return result
}

// Row i of colors (12 bytes) is the colours of names[i]; names without data get plains' colours and are listed in unknown.
export const biomeTable = (version, names, opts = {}) => {
  const { byName, source } = biomeColorsByName(version, opts)
  const plains = byName.get('plains')
  const unknown = names.filter(n => !byName.has(n))
  const colors = new Uint8Array(names.length * 12)
  names.forEach((n, i) => colors.set(byName.get(n) ?? plains, i * 12))
  return { names, colors, unknown, source }
}

// minecraft-data id order.
export const biomeColors = (version, opts = {}) => {
  const ids = minecraftData(version).biomesArray.map(b => b.name)
  return { ...biomeTable(version, ids, opts), ids }
}
