// The material table the browser renders with: every block state id maps to a material index, and a material is a
// block name plus a shape kind with one average colour per face direction (top, side, bottom).
import fs from 'node:fs'
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { textureSet, SIZE, LEVELS } from './textures.mjs'
import { decodePng, textureCandidates, tintOf, colorOf } from '../../src/vision/renderer.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FACES = [['top', 'top'], ['side', 'side'], ['bottom', 'bottom']]
const FULL = [0, 0, 0, 16, 16, 16]

export const CUTOUT = 1
export const TRANSLUCENT = 2
export const AXIS_X = 4
export const AXIS_Z = 8
export const EMISSIVE = 16
export const CULL_SAME = 32 // no face between two blocks of this material

// collision shapes are not the visual shape: these blocks are drawn differently from how they collide
const FULL_CUBES = new Set(['powder_snow', 'soul_sand', 'mud', 'honey_block'])
const FLAT = /(rail|_pressure_plate)$|^(redstone_wire|lily_pad)$/
const CULL_SAME_NAMES = /glass$|glass_pane$|^ice$|^frosted_ice$|^slime_block$|^honey_block$/
const TRANSLUCENT_NAMES = /^(water|ice|frosted_ice|slime_block|honey_block)$|stained_glass(_pane)?$/

const isFull = box => box.every((v, i) => v === FULL[i])

const boxOf = (block, props) => {
  if (block.name === 'snow') return [0, 0, 0, 16, Number(props.layers) * 2, 16]
  if (FULL_CUBES.has(block.name)) return FULL
  if (FLAT.test(block.name)) return [0, 0, 0, 16, 1, 16]
  if (!block.shapes.length) return null
  const union = block.shapes.reduce((u, sh) => u.map((v, i) => i < 3 ? Math.min(v, sh[i]) : Math.max(v, sh[i])))
  return union.map(v => Math.round(Math.min(1, Math.max(0, v)) * 16))
}

const shapeOf = (block, props) => {
  if (block.name === 'water' || block.name === 'lava') return { kind: block.name, box: FULL }
  const box = boxOf(block, props)
  if (!box) return { kind: 'cross', box: FULL }
  return { kind: isFull(box) ? 'cube' : 'box', box }
}

const hashColor = name => {
  let h = 2166136261
  for (const c of name) h = Math.imul(h ^ c.charCodeAt(0), 16777619) >>> 0
  return [64 + (h & 127), 64 + ((h >> 8) & 127), 64 + ((h >> 16) & 127)]
}

// alpha-weighted mean colour over visible pixels, and the share of pixels that are visible
export function averageColor ({ rgba }) {
  const sum = [0, 0, 0]
  let weight = 0
  let visible = 0
  for (let i = 0; i < rgba.length; i += 4) {
    const a = rgba[i + 3]
    if (a === 0) continue
    visible++
    weight += a
    for (let c = 0; c < 3; c++) sum[c] += rgba[i + c] * a
  }
  if (!weight) return { rgb: null, coverage: 0 }
  return { rgb: sum.map(v => v / weight), coverage: visible / (rgba.length / 4) }
}

const alphaFor = (kind, coverage) => {
  if (kind === 'water') return 160
  if (kind === 'cross') return Math.max(1, Math.round(coverage * 255))
  return 255
}

const hasCutout = ({ width, rgba }) => {
  for (let i = 3; i < width * width * 4; i += 4) if (rgba[i] < 128) return true
  return false
}

const flagsOf = (block, props, kind, cutout, emitLight) => {
  const translucent = TRANSLUCENT_NAMES.test(block.name)
  return (translucent ? TRANSLUCENT : cutout && kind !== 'water' && kind !== 'lava' ? CUTOUT : 0) |
    (CULL_SAME_NAMES.test(block.name) ? CULL_SAME : 0) | (props.axis === 'x' ? AXIS_X : 0) | (props.axis === 'z' ? AXIS_Z : 0) |
    (emitLight > 0 && props.lit !== false ? EMISSIVE : 0)
}

// one build gives the table and the texture bytes, so layer indices always agree
export function textureBytes (version, textureDir) {
  const registry = prismarineRegistry(version)
  const Block = prismarineBlock(registry)
  const images = new Map()
  const image = name => {
    if (images.has(name)) return images.get(name)
    const file = path.join(textureDir, `${name}.png`)
    const found = fs.existsSync(file) ? decodePng(fs.readFileSync(file)) : null
    images.set(name, found)
    return found
  }
  const layerNames = []
  const layerIndex = new Map()
  const layerFor = name => {
    if (!name) return -1
    if (!layerIndex.has(name)) layerIndex.set(name, layerNames.push(name) - 1)
    return layerIndex.get(name)
  }
  const faceTexture = (block, kind, face, props) => textureCandidates(block.name, kind === 'cross' ? 'cross' : face, props).find(n => image(n))
  const faceColor = (block, kind, textureName) => {
    const fallback = () => [...(colorOf(block.name) ?? hashColor(block.name)), alphaFor(kind, 1)]
    if (!textureName) return fallback()
    const { rgb, coverage } = averageColor(image(textureName))
    if (!rgb) return fallback()
    const tint = tintOf(textureName)
    const tinted = rgb.map((v, c) => tint ? v * tint[c] / 255 : v)
    return [...tinted.map(Math.round), alphaFor(kind, coverage)]
  }
  const describe = block => {
    const props = block.getProperties()
    const { kind, box } = shapeOf(block, props)
    const names = FACES.map(([, face]) => faceTexture(block, kind, face, props))
    const colors = FACES.map(([k], i) => [k, faceColor(block, kind, names[i])])
    const cutout = names.some(n => n && hasCutout(image(n)))
    const emitLight = props.lit === false ? 0 : registry.blocksByName[block.name].emitLight
    const flags = flagsOf(block, props, kind, cutout, emitLight)
    return { name: block.name, kind, tex: names.map(layerFor), box, flags, emit: emitLight, ...Object.fromEntries(colors) }
  }

  const materials = [{ name: 'air', kind: 'cube', tex: [-1, -1, -1], box: FULL, flags: 0, emit: 0, top: [0, 0, 0, 0], side: [0, 0, 0, 0], bottom: [0, 0, 0, 0] }]
  const byKey = new Map()
  const materialIndex = block => {
    const material = describe(block)
    const key = JSON.stringify(material)
    if (byKey.has(key)) return byKey.get(key)
    materials.push(material)
    byKey.set(key, materials.length - 1)
    return materials.length - 1
  }

  const stateCount = Math.max(...registry.blocksArray.map(b => b.maxStateId)) + 1
  const materialOf = new Uint16Array(stateCount)
  for (let id = 0; id < stateCount; id++) {
    const block = Block.fromStateId(id, 0)
    materialOf[id] = AIR.has(block.name) ? 0 : materialIndex(block)
  }
  const table = {
    version,
    stateCount,
    materialOf: Buffer.from(materialOf.buffer).toString('base64'),
    materials,
    textures: { size: SIZE, levels: LEVELS, names: layerNames }
  }
  return { table, textures: textureSet(textureDir, layerNames) }
}

export const materialTable = (version, textureDir) => textureBytes(version, textureDir).table
