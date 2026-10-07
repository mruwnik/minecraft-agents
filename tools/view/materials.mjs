// Why JavaScript: GPU data; the material table the WebGL renderer uses.
// The material table the browser renders with: every block state id maps to a material index, and a material is a
// block name plus a shape kind with one average colour per face direction (top, side, bottom).
import fs from 'node:fs'
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { textureSet, layerOf, decodeTexture, SIZE, LEVELS, isEntityLayer } from './textures.mjs'
import { decodePng, textureCandidates, tintOf, colorOf } from './renderer.mjs'
import { findClientJar, zipEntries, entryContent, openJar } from './jar-read.mjs'
import { loadModels } from './block-models.mjs'
import { bakeAll, classify } from './block-bake.mjs'
import { blockEntityElements, ADDS_TO_MODEL } from './block-entity-models.mjs'
import { modelLayers } from './web/mob-models.mjs'
import { tintRef, tintTable, dryFoliageColor, APPROXIMATE_GROUPS, TINT_GROUPS } from './tints.mjs'
import { packElementTable, FACE_DIRS, TABLE_WIDTH } from './element-table.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FACES = [['top', 'top'], ['side', 'side'], ['bottom', 'bottom']]
const FULL = [0, 0, 0, 16, 16, 16]

export const CUTOUT = 1
export const TRANSLUCENT = 2
export const AXIS_X = 4
export const AXIS_Z = 8
export const EMISSIVE = 16
export const CULL_SAME = 32 // no face between two blocks of this material
export const SIX_FACES = 128 // a cube from the jar: tex6 / rot6 are its six faces (up, down, north, south, east, west); tex is unused
export const OVER_CAP = 256 // a model state with more than ELEMENT_CAP elements, drawn as the bounding box of its elements

// elements the shader loops over for one model voxel
export const ELEMENT_CAP = 24
const MAX_LAYER = 4094 // layer + 1 and a quarter-turn count share 16 bits in the info texture
const WATER_LIKE = new Set(['water', 'lava', 'bubble_column'])
const FULL_UV = [0, 0, 16, 16]

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
  if (block.name === 'bubble_column') return { kind: 'water', box: FULL } // water in the game
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

const hex = rgb => rgb.map(v => v.toString(16).padStart(2, '0')).join('')
const isCube = element => !element.rotation && element.from.every(v => v === 0) && element.to.every(v => v === 16) && FACE_DIRS.every(dir => element.faces[dir]?.uv.every((v, i) => v === FULL_UV[i]))
const unionBox = elements => {
  const lo = [0, 1, 2].map(i => Math.min(...elements.map(e => e.from[i])))
  const hi = [0, 1, 2].map(i => Math.max(...elements.map(e => e.to[i])))
  return [...lo, ...hi].map(v => Math.round(Math.min(16, Math.max(0, v))))
}

// the jar's blockstates and models baked per state id, and the dry foliage colour from its colormap
const readJar = (jarPath, registry) => {
  const models = loadModels(jarPath)
  const buf = fs.readFileSync(jarPath)
  const colormap = zipEntries(buf).find(e => e.name === 'assets/minecraft/textures/colormap/dry_foliage.png')
  const dry = dryFoliageColor(colormap ? decodePng(entryContent(buf, colormap)) : null)
  return { baked: bakeAll(registry, models).states, tints: tintTable({ dry }) }
}

// One build gives the table, the texture bytes and the element table, so layer indices always agree.
// With a client jar (jarPath, default findClientJar()) a state the jar models is drawn from its blockstate and models: a plain cube
// with six face layers, or kind `model` with a list of elements in the element table. With jarPath null every block is drawn as before.
export function textureBytes (version, textureDir, { jarPath = findClientJar() } = {}) {
  const registry = prismarineRegistry(version)
  const Block = prismarineBlock(registry)
  const images = new Map()
  const image = name => {
    if (images.has(name)) return images.get(name)
    const file = path.join(textureDir, `${name}.png`)
    const found = fs.existsSync(file) ? decodeTexture(fs.readFileSync(file)) : null
    images.set(name, found)
    return found
  }
  const layerNames = []
  const layerIndex = new Map()
  const chains = new Map()
  const byPixels = new Map()
  const alias = {}
  // a name whose pixels (the first mip level) another layer has already is that layer, listed in `alias`
  const layerFor = name => {
    if (!name) return -1
    if (layerIndex.has(name)) return layerIndex.get(name)
    const chain = layerOf(textureDir, name, sheet)
    const key = Buffer.from(chain[0]).toString('base64')
    if (!byPixels.has(key)) {
      byPixels.set(key, layerNames.push(name) - 1)
      chains.set(name, chain)
    } else alias[name] = byPixels.get(key)
    layerIndex.set(name, byPixels.get(key))
    return layerIndex.get(name)
  }
  const faceTexture = (block, kind, face, props) => textureCandidates(block.name === 'bubble_column' ? 'water' : block.name, kind === 'cross' ? 'cross' : face, props).find(n => image(n))
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

  const jar = jarPath ? readJar(jarPath, registry) : null
  // entity sheets (block-entity-models.mjs) are read from the jar, decoded once
  const jarFiles = jarPath ? openJar(jarPath) : null
  const sheets = new Map()
  const sheetPath = name => `assets/minecraft/textures/${name}.png`
  const sheet = name => {
    if (!sheets.has(name)) sheets.set(name, jarFiles.has(sheetPath(name)) ? decodeTexture(jarFiles.read(sheetPath(name))) : null)
    return sheets.get(name)
  }
  // blocks with a state that has geometry: their empty states are drawn as nothing; a block with none is a block entity
  const hasGeometry = new Set(jar ? [...jar.baked].filter(([, b]) => b.elements.length > 0).map(([id]) => Block.fromStateId(id, 0).name) : [])
  const lists = []
  const listIndex = new Map()
  const stats = { overCapStates: 0 }
  // textures are untinted layers (the shader multiplies by the face's tint group); a texture the renderer's name list would tint gets an explicit white
  const faceLayer = (block, props, face) => {
    if (face.texture && isEntityLayer(face.texture)) return { layer: sheet(face.texture.split('#')[0]) ? layerFor(face.texture) : -1, tint: 'none', tintIndex: 0, texture: face.texture, entity: true }
    if (!face.texture || !image(face.texture)) return { layer: -1, tint: 'none', tintIndex: 0, texture: face.texture }
    const ref = tintRef(block.name, props, face.tintindex)
    return { layer: layerFor(tintOf(face.texture) ? `${face.texture}@ffffff` : face.texture), tint: ref?.group ?? 'none', tintIndex: ref?.index ?? 0, texture: face.texture }
  }
  // null: the blockstate selects nothing for this state (a wall with no post and no sides), and the game draws nothing
  const bakedMaterial = (block, props, jarBaked, base) => {
    if (jarBaked.elements.length === 0 && /multipart \[\]$|no matching variant$/.test(jarBaked.source)) return null
    if (WATER_LIKE.has(block.name)) return base
    // a block entity the game draws in code: its hand-written elements stand in for (or, the bell, join) the jar model
    const entity = jarBaked.elements.length === 0 || ADDS_TO_MODEL.has(block.name) ? blockEntityElements(block.name, props) : null
    const baked = entity ? { ...jarBaked, elements: [...jarBaked.elements, ...entity] } : jarBaked
    if (classify(baked) === 'empty') return hasGeometry.has(block.name) ? null : base
    const resolved = baked.elements.map(element => Object.fromEntries(FACE_DIRS.filter(dir => element.faces[dir]).map(dir => [dir, faceLayer(block, props, element.faces[dir])])))
    const used = resolved.flatMap(faces => Object.values(faces))
    const groups = APPROXIMATE_GROUPS.filter(g => used.some(f => f.tint === g))
    const missing = [...new Set(used.filter(f => f.layer < 0).map(f => f.texture ?? '?'))]
    const outside = baked.elements.some(e => e.from.some(v => v < 0) || e.to.some(v => v > 16))
    const notes = { ...(groups.length ? { tint: groups } : {}), ...(missing.length ? { noTexture: missing } : {}), ...(outside ? { outside: true } : {}) }
    const { top, side, bottom } = base
    const common = { name: block.name, emit: base.emit, ...(entity ? { entity: true } : {}), ...notes }
    const colors = { top, side, bottom }
    const flags = kind => flagsOf(block, props, kind, used.some(f => f.layer >= 0 && !f.entity && hasCutout(image(f.texture))), base.emit) & ~(AXIS_X | AXIS_Z)
    if (baked.elements.length > ELEMENT_CAP) {
      stats.overCapStates++
      return { ...common, ...colors, kind: 'box', tex: base.tex, box: unionBox(baked.elements), flags: base.flags | OVER_CAP }
    }
    if (classify(baked) === 'cube' && isCube(baked.elements[0]) && used.length === 6) {
      const faces = FACE_DIRS.map(dir => baked.elements[0].faces[dir])
      const turns = faces.map(f => f.rotation / 90)
      const tint6 = FACE_DIRS.map(dir => TINT_GROUPS.indexOf(resolved[0][dir].tint) + 8 * resolved[0][dir].tintIndex)
      return { ...common, ...colors, kind: 'cube', tex: [-1, -1, -1], tex6: FACE_DIRS.map((_, i) => resolved[0][FACE_DIRS[i]].layer), ...(turns.some(Boolean) ? { rot6: turns } : {}), ...(tint6.some(Boolean) ? { tint6 } : {}), box: FULL, flags: flags('cube') | SIX_FACES }
    }
    const tableElements = baked.elements.map((element, i) => ({
      from: element.from,
      to: element.to,
      rotation: element.rotation,
      shade: element.shade,
      faces: Object.fromEntries(Object.entries(resolved[i]).filter(([, f]) => f.layer >= 0).map(([dir, f]) => [dir, { layer: f.layer, uv: element.faces[dir].uv, rotation: element.faces[dir].rotation, tint: f.tint, ...(f.tintIndex ? { tintIndex: f.tintIndex } : {}), cullface: element.faces[dir].cullface }]))
    }))
    const key = JSON.stringify(tableElements)
    if (!listIndex.has(key)) listIndex.set(key, lists.push(tableElements) - 1)
    return { ...common, kind: 'model', tex: [-1, -1, -1], box: unionBox(baked.elements), flags: flags('model'), list: listIndex.get(key) }
  }

  const materials = [{ name: 'air', kind: 'cube', tex: [-1, -1, -1], box: FULL, flags: 0, emit: 0, top: [0, 0, 0, 0], side: [0, 0, 0, 0], bottom: [0, 0, 0, 0] }]
  const byKey = new Map()
  const materialIndex = (block, id) => {
    const baked = jar?.baked.get(id)
    const material = baked ? bakedMaterial(block, block.getProperties(), baked, describe(block)) : describe(block)
    if (material === null) return 0
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
    materialOf[id] = AIR.has(block.name) ? 0 : materialIndex(block, id)
  }
  // the mobs' model faces (web/mob-models.mjs) are layers too: the browser view textures them from this array
  if (jarFiles) for (const name of modelLayers()) if (sheet(name.split('#')[0])) layerFor(name)
  if (layerNames.length > MAX_LAYER) throw new Error(`${layerNames.length} texture layers: the info texture packs at most ${MAX_LAYER}`)
  const packed = lists.length ? packElementTable(lists) : null
  materials.forEach(material => {
    if (material.list === undefined) return
    ;[material.elemOffset, material.elemCount] = packed.ranges[material.list]
    delete material.list
  })
  const table = {
    version,
    stateCount,
    materialOf: Buffer.from(materialOf.buffer).toString('base64'),
    materials,
    textures: { size: SIZE, levels: LEVELS, names: layerNames, alias },
    ...(jar ? { tints: jar.tints } : {}),
    ...(packed ? { elements: { width: TABLE_WIDTH, rows: packed.rows, listTexels: packed.listTexels, count: packed.elementCount, ids: packed.idCount, overCapStates: stats.overCapStates } } : {})
  }
  return { table, textures: textureSet(textureDir, layerNames, { sheet, chains }), elements: packed?.data ?? null }
}

export const materialTable = (version, textureDir, options) => textureBytes(version, textureDir, options).table
