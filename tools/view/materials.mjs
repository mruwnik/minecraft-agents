// The material table the browser renders with: every block state id maps to a material index, and a material is a
// block name plus a shape kind with one average colour per face direction (top, side, bottom).
import fs from 'node:fs'
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { decodePng, textureCandidates, tintOf, colorOf } from '../../src/vision/renderer.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FULL_CUBE = JSON.stringify([[0, 0, 0, 1, 1, 1]])
const FACES = [['top', 'top'], ['side', 'side'], ['bottom', 'bottom']]

const kindOf = block => {
  if (block.name === 'water' || block.name === 'lava') return block.name
  if (!block.shapes.length) return 'cross'
  return JSON.stringify(block.shapes) === FULL_CUBE ? 'cube' : 'partial'
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

export function materialTable (version, textureDir) {
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
  const faceColor = (block, kind, face) => {
    const props = block.getProperties()
    const textureName = textureCandidates(block.name, face, props).find(n => image(n))
    const fallback = () => [...(colorOf(block.name) ?? hashColor(block.name)), alphaFor(kind, 1)]
    if (!textureName) return fallback()
    const { rgb, coverage } = averageColor(image(textureName))
    if (!rgb) return fallback()
    const tint = tintOf(textureName)
    const tinted = rgb.map((v, c) => tint ? v * tint[c] / 255 : v)
    return [...tinted.map(Math.round), alphaFor(kind, coverage)]
  }

  const materials = [{ name: 'air', kind: 'cube', top: [0, 0, 0, 0], side: [0, 0, 0, 0], bottom: [0, 0, 0, 0] }]
  const byKey = new Map()
  const materialIndex = block => {
    const kind = kindOf(block)
    const key = `${block.name}|${kind}`
    if (byKey.has(key)) return byKey.get(key)
    const material = { name: block.name, kind, ...Object.fromEntries(FACES.map(([k, face]) => [k, faceColor(block, kind, face)])) }
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
  return {
    version,
    stateCount,
    materialOf: Buffer.from(materialOf.buffer).toString('base64'),
    materials
  }
}
