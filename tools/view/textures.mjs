// The browser's texture array: 16x16 RGBA layers with a mip chain, packed level-major (all layers of level 0, then 1...).
import fs from 'node:fs'
import path from 'node:path'
import { decodePng, tintOf } from '../../src/vision/renderer.mjs'

export const SIZE = 16
export const LEVELS = 5

// the chunks of a PNG by type (the first of each)
const pngChunks = buf => {
  const chunks = {}
  for (let at = 8; at + 8 <= buf.length;) {
    const length = buf.readUInt32BE(at)
    chunks[buf.toString('latin1', at + 4, at + 8)] ??= buf.subarray(at + 8, at + 8 + length)
    at += 12 + length
  }
  return chunks
}

// decodePng, plus the tRNS colour key it ignores on grey and RGB textures (leaf_litter is grey with a key: its gaps are transparent)
export const decodeTexture = buf => {
  const image = decodePng(buf)
  const { IHDR, tRNS } = pngChunks(buf)
  const colorType = IHDR[9]
  if (!tRNS || (colorType !== 0 && colorType !== 2) || IHDR[8] !== 8) return image
  const key = colorType === 0 ? [tRNS[1], tRNS[1], tRNS[1]] : [tRNS[1], tRNS[3], tRNS[5]]
  const rgba = image.rgba.slice()
  for (let i = 0; i < rgba.length; i += 4) if (key.every((v, c) => rgba[i + c] === v)) rgba[i + 3] = 0
  return { ...image, rgba }
}

// alpha-weighted mean colour of the visible texels, or black for a texture with none
const meanColor = rgba => {
  const sum = [0, 0, 0]
  let weight = 0
  for (let i = 0; i < rgba.length; i += 4) {
    weight += rgba[i + 3]
    for (let c = 0; c < 3; c++) sum[c] += rgba[i + c] * rgba[i + 3]
  }
  return weight ? sum.map(v => Math.round(v / weight)) : [0, 0, 0]
}

// 2x2 boxes: colour alpha-weighted, alpha plain; a block with no alpha takes the fallback colour
const halve = (rgba, size, fallback) => {
  const half = size / 2
  const out = new Uint8Array(half * half * 4)
  for (let y = 0; y < half; y++) {
    for (let x = 0; x < half; x++) {
      const sum = [0, 0, 0]
      let weight = 0
      for (const [dx, dy] of [[0, 0], [1, 0], [0, 1], [1, 1]]) {
        const at = ((2 * y + dy) * size + 2 * x + dx) * 4
        weight += rgba[at + 3]
        for (let c = 0; c < 3; c++) sum[c] += rgba[at + c] * rgba[at + 3]
      }
      const o = (y * half + x) * 4
      out.set(weight ? sum.map(v => Math.round(v / weight)) : fallback, o)
      out[o + 3] = Math.round(weight / 4)
    }
  }
  return out
}

const scaled = (alpha, scale) => Math.min(255, Math.round(alpha * scale))

const coverage = (rgba, scale) => {
  let n = 0
  for (let i = 3; i < rgba.length; i += 4) if (scaled(rgba[i], scale) >= 128) n++
  return n / (rgba.length / 4)
}

// the alpha scale in [0, 8] that brings the share of texels at or above 0.5 closest to the target
const coverageScale = (rgba, target) => {
  let lo = 0
  let hi = 8
  for (let i = 0; i < 30; i++) {
    const mid = (lo + hi) / 2
    if (coverage(rgba, mid) >= target) hi = mid
    else lo = mid
  }
  return Math.abs(coverage(rgba, lo) - target) < Math.abs(coverage(rgba, hi) - target) ? lo : hi
}

const scaleAlpha = (rgba, scale) => {
  const out = rgba.slice()
  for (let i = 3; i < out.length; i += 4) out[i] = scaled(out[i], scale)
  return out
}

const isBinary = rgba => {
  let opaque = 0
  for (let i = 3; i < rgba.length; i += 4) {
    if (rgba[i] !== 0 && rgba[i] !== 255) return false
    if (rgba[i] === 255) opaque++
  }
  return opaque !== rgba.length / 4
}

// 16x16 RGBA in, [16, 8, 4, 2, 1] out. Transparent texels carry the mean colour so filtering never pulls in black, and
// binary alpha keeps its coverage down the chain, because the shader alpha-tests at 0.5.
export function mipChain (rgba) {
  const mean = meanColor(rgba)
  const base = rgba.slice()
  for (let i = 0; i < base.length; i += 4) if (!base[i + 3]) base.set(mean, i)
  const chain = [base]
  for (let level = 1; level < LEVELS; level++) chain.push(halve(chain[level - 1], SIZE >> (level - 1), mean))
  if (!isBinary(base)) return chain
  const target = coverage(base, 1)
  return chain.map((level, i) => i === 0 ? level : scaleAlpha(level, coverageScale(level, target)))
}

// first frame of an animated strip; a 32 wide texture is box-downsampled
const firstFrame = ({ width, rgba }) => {
  if (width === SIZE) return rgba.subarray(0, SIZE * SIZE * 4)
  return halve(rgba.subarray(0, width * width * 4), width, meanColor(rgba))
}

const hexColor = hex => [0, 2, 4].map(i => parseInt(hex.slice(i, i + 2), 16))

const tinted = (rgba, tint) => {
  if (!tint) return rgba
  for (let i = 0; i < rgba.length; i += 4) for (let c = 0; c < 3; c++) rgba[i + c] = Math.round(rgba[i + c] * tint[c] / 255)
  return rgba
}

// A region (x, y, w, h in sheet pixels) of a decoded sheet resampled, nearest, to one 16x16 layer
export const cropRegion = ({ width, height, rgba }, [x, y, w, h]) => {
  const out = new Uint8Array(SIZE * SIZE * 4)
  for (let j = 0; j < SIZE; j++) {
    for (let i = 0; i < SIZE; i++) {
      const sx = Math.min(width - 1, x + Math.floor((i + 0.5) * w / SIZE))
      const sy = Math.min(height - 1, y + Math.floor((j + 0.5) * h / SIZE))
      out.set(rgba.subarray((sy * width + sx) * 4, (sy * width + sx) * 4 + 4), (j * SIZE + i) * 4)
    }
  }
  return out
}

// `entity/<sheet>#x,y,w,h[@rrggbb]`: a region of an entity sheet in the jar (materials.mjs hands the sheet reader over), see block-entity-models.mjs
export const parseEntityLayer = layerName => {
  const [, sheet, region, hex] = /^(entity\/[^#]+)#([\d,]+)(?:@(\w+))?$/.exec(layerName)
  return { sheet, region: region.split(',').map(Number), hex }
}
export const isEntityLayer = layerName => layerName.startsWith('entity/')

// `name`: the texture tinted as the renderer's grass/leaf/water list says (tintOf). `name@rrggbb`: tinted by exactly that colour.
const layerOf = (textureDir, layerName, sheet) => {
  if (isEntityLayer(layerName)) {
    const { sheet: sheetName, region, hex } = parseEntityLayer(layerName)
    return mipChain(tinted(cropRegion(sheet(sheetName), region), hex ? hexColor(hex) : null))
  }
  const [name, hex] = layerName.split('@')
  const rgba = firstFrame(decodeTexture(fs.readFileSync(path.join(textureDir, `${name}.png`)))).slice()
  return mipChain(tinted(rgba, hex ? hexColor(hex) : tintOf(name)))
}

// `sheet(name)` reads an entity sheet (decoded) by its `entity/...` path
export function textureSet (textureDir, names, { sheet } = {}) {
  const layers = names.map(name => layerOf(textureDir, name, sheet))
  const parts = Array.from({ length: LEVELS }, (_, level) => layers.map(chain => chain[level])).flat()
  const bytes = new Uint8Array(parts.reduce((n, p) => n + p.length, 0))
  parts.reduce((at, p) => { bytes.set(p, at); return at + p.length }, 0)
  return { size: SIZE, levels: LEVELS, layers: names.length, bytes }
}
