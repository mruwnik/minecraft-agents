// Why JavaScript: WebGL/GPU; WebGL2 ray marcher for the block window.
// WebGL2 side of the browser view: the block window as a 3D texture, a two-level DDA ray marcher in a full-screen
// fragment shader, entity boxes. Coordinates in the shader are relative to the window origin (a chunk-aligned corner).
import { LABEL_MARGIN, MAX_LABELS, labelWidth, placeLabels, projectorFor } from './mobs.mjs'
import { MAX_ENTITIES, ELEMENT_CAP, VERTEX, FRAGMENT } from './gl-shader.mjs'

export { MAX_ENTITIES, ELEMENT_CAP }
export const MAX_PART_ROWS = 1024 // parts of all the mobs drawn with models at once; the shader loops at most 24 parts a mob
const PART_UNIT = 10 // texture unit of the parts table
export const KINDS = { cube: 0, box: 1, cross: 2, water: 3, lava: 4, model: 5 }
const MATERIAL_COLUMNS = 4 // top, side, bottom, kind
const ISSUE_FLAG = 64 // the table's `issue` as a material flag, read by debugColor in the shader
const INFO_TEXELS = 8 // per material: layers+kind, flags+emit, box min, box max, up/down/north/south, east/west/elemOffset/elemCount, tints up..south, tints east/west
const TINT_GROUP_NAMES = ['none', 'grass', 'foliage', 'dry_foliage', 'water', 'constant'] // tools/view/tints.mjs TINT_GROUPS

const compile = (gl, type, source) => {
  const shader = gl.createShader(type)
  gl.shaderSource(shader, source)
  gl.compileShader(shader)
  if (gl.getShaderParameter(shader, gl.COMPILE_STATUS)) return shader
  throw new Error(`shader compile failed: ${gl.getShaderInfoLog(shader)}`)
}

const link = gl => {
  const program = gl.createProgram()
  gl.attachShader(program, compile(gl, gl.VERTEX_SHADER, VERTEX))
  gl.attachShader(program, compile(gl, gl.FRAGMENT_SHADER, FRAGMENT))
  gl.linkProgram(program)
  if (gl.getProgramParameter(program, gl.LINK_STATUS)) return program
  throw new Error(`program link failed: ${gl.getProgramInfoLog(program)}`)
}

const nearestTexture = (gl, target, unit) => {
  const texture = gl.createTexture()
  gl.activeTexture(gl.TEXTURE0 + unit)
  gl.bindTexture(target, texture)
  gl.texParameteri(target, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
  gl.texParameteri(target, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
  gl.texParameteri(target, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
  gl.texParameteri(target, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
  return texture
}

// The uniforms and the parts table for the entities with models (`model`: {parts: [{box, layers}], right: {x, z}, origin: [x, y, z]}, web/mob-models.mjs):
// rot per entity as (right x, right z, first row, part count) and org its origin, both zero for a plain box, and `rows` the table, 16 floats a part
// (see uParts). A model whose faces are not all layers of the texture array, or that no longer fits the table, is drawn as its plain box.
export const modelUniforms = (entities, layerIndex) => {
  const rot = new Float32Array(MAX_ENTITIES * 4)
  const org = new Float32Array(MAX_ENTITIES * 3)
  const rows = []
  entities.slice(0, MAX_ENTITIES).forEach((e, i) => {
    const layers = e.model?.parts.map(p => p.layers.map(l => layerIndex.get(l) ?? -1))
    if (!layers || layers.some(l => l.includes(-1)) || rows.length / 16 + layers.length > MAX_PART_ROWS) return
    rot.set([e.model.right.x, e.model.right.z, rows.length / 16, layers.length], i * 4)
    org.set(e.model.origin, i * 3)
    e.model.parts.forEach((p, k) => rows.push(...p.box.slice(0, 3), 0, ...p.box.slice(3), p.paint, ...layers[k], 0, 0))
  })
  return { rot, org, rows: Float32Array.from(rows) }
}

// RGBA8 texture, 4 columns by one row per material: top, side, bottom colour, then the kind in red
export const materialPixels = materials => {
  const data = new Uint8Array(materials.length * MATERIAL_COLUMNS * 4)
  materials.forEach((material, row) => {
    const faces = [material.top, material.side, material.bottom]
    faces.forEach((rgba, column) => data.set((rgba ?? [0, 0, 0, 0]).map(Math.round), (row * MATERIAL_COLUMNS + column) * 4))
    data[(row * MATERIAL_COLUMNS + 3) * 4] = KINDS[material.kind] ?? 0
  })
  return data
}

// RGBA16UI, INFO_TEXELS texels per material: (top+1, side+1, bottom+1, kind), (flags, emit), box min, box max (1/16 units),
// (up, down, north, south), (east, west, elemOffset, elemCount), (tint up..south), (tint east, west). A tint is group + 8 * constant index. A six-face cube (flag 128) has tex6 layers; each face is
// (layer + 1) | quarter turns << 12. A model has elemOffset / elemCount into the element table.
export const materialInfo = materials => {
  const data = new Uint16Array(materials.length * INFO_TEXELS * 4)
  const faceCode = (m, i) => (m.tex6 ? (m.tex6[i] + 1) | ((m.rot6?.[i] ?? 0) << 12) : 0)
  const tints = m => [...(m.tint6 ?? [0, 0, 0, 0, 0, 0]), 0, 0]
  materials.forEach((m, row) => {
    const [x0, y0, z0, x1, y1, z1] = m.box ?? [0, 0, 0, 16, 16, 16]
    const faces = [0, 1, 2, 3, 4, 5].map(i => faceCode(m, i))
    data.set([...(m.tex ?? [-1, -1, -1]).map(l => l + 1), KINDS[m.kind] ?? 0, (m.flags ?? 0) | (m.issue ? ISSUE_FLAG : 0), m.emit ?? 0, 0, 0, x0, y0, z0, 0, x1, y1, z1, 0, ...faces.slice(0, 4), faces[4], faces[5], m.elemOffset ?? 0, m.elemCount ?? 0, ...tints(m).slice(0, 4), tints(m)[4], tints(m)[5], 0, 0], row * INFO_TEXELS * 4)
  })
  return data
}

// the level-major texture bytes as one subarray per mip level
export const textureLevels = (bytes, layers, size, levels) => {
  const sizes = Array.from({ length: levels }, (_, l) => (size >> l) ** 2 * 4 * layers)
  return sizes.map((n, l) => bytes.subarray(sizes.slice(0, l).reduce((a, b) => a + b, 0), sizes.slice(0, l + 1).reduce((a, b) => a + b, 0)))
}

// ---- name labels over mobs (the shader draws the plate; the text comes from a small atlas, one row per label)
const LABEL_ATLAS_WIDTH = 256
const LABEL_ATLAS_ROW = 32

// The entity boxes {min, max, name, label?, kind?} (relative to the window origin, like eye) as the labels placeLabels gives for them.
export const boxLabels = ({ boxes, eye, basis, width, height, dist }) => placeLabels({
  eye,
  project: projectorFor(basis, width, height),
  width,
  height,
  entities: boxes
    .filter(b => b.name !== undefined || b.label !== undefined)
    .map(b => ({ name: b.name, label: b.label, kind: b.kind, x: (b.min[0] + b.max[0]) / 2, y: b.min[1], z: (b.min[2] + b.max[2]) / 2, width: b.max[0] - b.min[0], height: b.max[1] - b.min[1] }))
    .filter(e => Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z) <= dist)
})

// The shader's label uniforms for placeLabels' labels in a picture `height` pixels tall: rects as x0, y0, x1, y1 from the bottom
// left, and depths, both padded to MAX_LABELS.
export const labelUniforms = (labels, height) => {
  const rects = new Float32Array(MAX_LABELS * 4)
  const depths = new Float32Array(MAX_LABELS)
  const ats = new Float32Array(MAX_LABELS * 3)
  const dists = new Float32Array(MAX_LABELS)
  labels.slice(0, MAX_LABELS).forEach((l, i) => {
    const w = labelWidth(l.text, l.h)
    rects.set([l.px - w / 2, height - l.py, l.px + w / 2, height - l.py + l.h], i * 4)
    depths[i] = l.depth
    ats.set(l.at, i * 3)
    dists[i] = l.dist
  })
  return { count: Math.min(labels.length, MAX_LABELS), rects, depths, ats, dists }
}

// RGBA text for the shader: row i holds label i's text in white (alpha is the ink), drawn so that the row, stretched over the
// label's plate, shows the letters in their natural shape. Needs a 2D canvas, so browser only.
const labelAtlas = labels => {
  const canvas = typeof OffscreenCanvas === 'function' ? new OffscreenCanvas(LABEL_ATLAS_WIDTH, LABEL_ATLAS_ROW * MAX_LABELS) : Object.assign(document.createElement('canvas'), { width: LABEL_ATLAS_WIDTH, height: LABEL_ATLAS_ROW * MAX_LABELS })
  const ctx = canvas.getContext('2d')
  ctx.fillStyle = '#fff'
  ctx.textBaseline = 'middle'
  ctx.textAlign = 'center'
  ctx.font = `bold ${Math.round(LABEL_ATLAS_ROW * 0.72)}px monospace`
  labels.slice(0, MAX_LABELS).forEach((l, i) => {
    // the row is stretched over the plate, so the text is squeezed here to fill the plate's text cells and come out in natural shape
    const text = l.text.toUpperCase()
    const cells = text.length * 6 - 1
    ctx.save()
    ctx.translate(LABEL_ATLAS_WIDTH / 2, (i + 0.5) * LABEL_ATLAS_ROW)
    ctx.scale(LABEL_ATLAS_WIDTH * cells / (cells + 2 * LABEL_MARGIN) / ctx.measureText(text).width, 1)
    ctx.fillText(text, 0, 1)
    ctx.restore()
  })
  return ctx.getImageData(0, 0, LABEL_ATLAS_WIDTH, LABEL_ATLAS_ROW * MAX_LABELS)
}

const TABLE_ROW_BYTES = MATERIAL_COLUMNS * 4 + INFO_TEXELS * 8 // RGBA8 colours + RGBA16UI info per material

// One renderer owns the GL context, the program and the tables every world shares (materials, textures, elements, tints,
// debug); a world (createWorld) owns the per-scene block window. canvasOrGl: a canvas (the renderer makes the context) or an
// existing WebGL2 context (the renderer then draws into whatever framebuffer is bound, and never resizes the canvas itself).
export function createRenderer (canvasOrGl) {
  const gl = typeof canvasOrGl.getContext === 'function'
    ? canvasOrGl.getContext('webgl2', { antialias: false, alpha: false, powerPreference: 'high-performance' })
    : canvasOrGl
  if (!gl) throw new Error('WebGL2 is not available')
  const canvas = gl.canvas
  const debugInfo = gl.getExtension('WEBGL_debug_renderer_info')
  const renderer = debugInfo ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)
  const program = link(gl)
  const uniform = Object.fromEntries(['uBlocks', 'uCoarse', 'uMats', 'uInfo', 'uElems', 'uElemBase', 'uTintGroups', 'uTintConst', 'uTex', 'uLodMax', 'uRes', 'uEye', 'uFwd', 'uRight', 'uUp', 'uHalf', 'uSize', 'uSlotOff', 'uDist', 'uDarken', 'uDebug', 'uLightTex', 'uBiomes', 'uBiomeColors', 'uHasBiomeColors', 'uEntCount', 'uEntMin', 'uEntMax', 'uEntCol', 'uEntRot', 'uEntOrg', 'uParts', 'uLabelCount', 'uLabelRect', 'uLabelDepth', 'uLabelAt', 'uLabelDist', 'uLabels']
    .map(name => [name, gl.getUniformLocation(program, name)]))
  gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1)
  gl.bindVertexArray(gl.createVertexArray())
  gl.useProgram(program)

  const mats = nearestTexture(gl, gl.TEXTURE_2D, 2)
  const info = nearestTexture(gl, gl.TEXTURE_2D, 3)
  const tex = nearestTexture(gl, gl.TEXTURE_2D_ARRAY, 4)
  const elems = nearestTexture(gl, gl.TEXTURE_2D, 6)
  const labelTexture = nearestTexture(gl, gl.TEXTURE_2D, 9)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
  const partsTexture = nearestTexture(gl, gl.TEXTURE_2D, PART_UNIT)
  let labelAtlasKey = null // the texts the atlas holds, so it is redrawn and uploaded only when they change
  let elemBase = 0
  let tintGroups = new Float32Array(6 * 3).fill(1)
  let tintConst = new Float32Array(32 * 3).fill(1)
  gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MIN_FILTER, gl.NEAREST_MIPMAP_LINEAR)
  let lodMax = 0
  let debug = false
  let tableVersion = null
  const sharedBytes = { materials: 0, elements: 0, textures: 0 }
  const worlds = new Set()

  // The tables are per Minecraft version and one renderer holds one set: every scene of a renderer must use the same version.
  // Returns false (and logs) when a different version was claimed before.
  const claimTable = version => {
    if (tableVersion === null) tableVersion = version
    if (tableVersion === version) return true
    console.error(`this renderer holds the tables of ${tableVersion}; a scene wants ${version} and is not drawn`)
    return false
  }

  const setMaterials = materials => {
    gl.activeTexture(gl.TEXTURE2)
    gl.bindTexture(gl.TEXTURE_2D, mats)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, MATERIAL_COLUMNS, materials.length, 0, gl.RGBA, gl.UNSIGNED_BYTE, materialPixels(materials))
    gl.activeTexture(gl.TEXTURE3)
    gl.bindTexture(gl.TEXTURE_2D, info)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA16UI, INFO_TEXELS, materials.length, 0, gl.RGBA_INTEGER, gl.UNSIGNED_SHORT, materialInfo(materials))
    sharedBytes.materials = materials.length * TABLE_ROW_BYTES
  }

  // the element table: RGBA32F, width texels per row (tools/view/element-table.mjs); a table with no elements leaves one zero row
  const setElements = ({ data, width, rows, listTexels }) => {
    gl.activeTexture(gl.TEXTURE6)
    gl.bindTexture(gl.TEXTURE_2D, elems)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, rows, 0, gl.RGBA, gl.FLOAT, data)
    elemBase = listTexels
    sharedBytes.elements = width * rows * 16
  }
  // the stage-1 tint colours: groups by TINT_GROUP_NAMES, constants by index (both 0..255 RGB)
  const setTints = ({ groups, constants }) => {
    tintGroups = Float32Array.from(TINT_GROUP_NAMES.flatMap(name => groups[name].map(v => v / 255)))
    tintConst = new Float32Array(32 * 3).fill(1)
    tintConst.set(constants.flat().map(v => v / 255))
  }
  gl.activeTexture(gl.TEXTURE6)
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 1024, 1, 0, gl.RGBA, gl.FLOAT, new Float32Array(1024 * 4))

  // once per page: texStorage is immutable
  let layerIndex = new Map() // texture layer name -> layer, for the mobs' model faces
  const setTextures = ({ bytes, layers, size, levels, names = [] }) => {
    layerIndex = new Map(names.map((name, i) => [name, i]))
    gl.activeTexture(gl.TEXTURE4)
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, tex)
    gl.texStorage3D(gl.TEXTURE_2D_ARRAY, levels, gl.RGBA8, size, size, layers)
    textureLevels(bytes, layers, size, levels).forEach((data, l) => {
      const n = size >> l
      gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, l, 0, 0, 0, n, n, layers, gl.RGBA, gl.UNSIGNED_BYTE, data)
    })
    lodMax = levels - 1
    sharedBytes.textures = bytes.byteLength
  }

  // ---- one world: the block window of one scene (its own 3D textures; texture units 0, 1 and 5 are bound to them while drawing) ----
  const createWorld = () => {
    const blocks = nearestTexture(gl, gl.TEXTURE_3D, 0)
    const coarse = nearestTexture(gl, gl.TEXTURE_3D, 1)
    const lightTex = nearestTexture(gl, gl.TEXTURE_3D, 5)
    // Per-world biome data (read by tintFor):
    // biomes is R8UI, (n*4, height/4, n*4), one texel per 4x4x4 blocks, filled by uploadColumn's optional biomes argument;
    // biomeColors is RGBA8 4 x 256 (biome id -> colours), per world because scenes may show different worlds.
    const biomes = nearestTexture(gl, gl.TEXTURE_3D, 7)
    const biomeColors = nearestTexture(gl, gl.TEXTURE_2D, 8)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, 4, 256, 0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(4 * 256 * 4))
    let size = null
    let hasColors = false

    const bind = () => {
      gl.activeTexture(gl.TEXTURE0)
      gl.bindTexture(gl.TEXTURE_3D, blocks)
      gl.activeTexture(gl.TEXTURE1)
      gl.bindTexture(gl.TEXTURE_3D, coarse)
      gl.activeTexture(gl.TEXTURE5)
      gl.bindTexture(gl.TEXTURE_3D, lightTex)
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.activeTexture(gl.TEXTURE8)
      gl.bindTexture(gl.TEXTURE_2D, biomeColors)
    }

    // N columns a side, height blocks tall; texStorage contents start zeroed
    const allocate = (n, height) => {
      const sections = height >> 4
      gl.activeTexture(gl.TEXTURE0)
      gl.bindTexture(gl.TEXTURE_3D, blocks)
      gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R16UI, n * 16, height, n * 16)
      gl.activeTexture(gl.TEXTURE1)
      gl.bindTexture(gl.TEXTURE_3D, coarse)
      gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n, sections, n)
      gl.activeTexture(gl.TEXTURE5)
      gl.bindTexture(gl.TEXTURE_3D, lightTex)
      gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n * 16, height, n * 16)
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n * 4, height >> 2, n * 4)
      size = { n, height, sections, noFlags: new Uint8Array(sections) }
      world.bytes = n * n * 256 * height * 3 + n * n * sections + n * n * 16 * (height >> 2) + 4 * 256 * 4
    }

    // mats is 16*height*16 material indices, x fastest then y then z; flags one byte per section, bottom first; light is
    // sky << 4 | block per cell in the same order
    // slot flags: 0 no column (mats and light are stale and never read), 1 a section with blocks, 2 an all-air section
    const putFlags = (sx, sz, flags) => {
      gl.activeTexture(gl.TEXTURE1)
      gl.bindTexture(gl.TEXTURE_3D, coarse)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx, 0, sz, 1, size.sections, 1, gl.RED_INTEGER, gl.UNSIGNED_BYTE, flags)
    }
    // biomes (optional): 4 x height/4 x 4 bytes, x fastest then y then z
    const uploadColumn = (sx, sz, mats16, flags, light, biomeIds) => {
      gl.activeTexture(gl.TEXTURE0)
      gl.bindTexture(gl.TEXTURE_3D, blocks)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, size.height, 16, gl.RED_INTEGER, gl.UNSIGNED_SHORT, mats16)
      putFlags(sx, sz, flags.map(f => (f ? 1 : 2)))
      gl.activeTexture(gl.TEXTURE5)
      gl.bindTexture(gl.TEXTURE_3D, lightTex)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, size.height, 16, gl.RED_INTEGER, gl.UNSIGNED_BYTE, light)
      if (!biomeIds) return
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 4, 0, sz * 4, 4, size.height >> 2, 4, gl.RED_INTEGER, gl.UNSIGNED_BYTE, biomeIds)
    }
    // colours: 4 x 256 RGBA8 bytes (Uint8Array of 4096)
    const setBiomeColors = colors => {
      gl.activeTexture(gl.TEXTURE8)
      gl.bindTexture(gl.TEXTURE_2D, biomeColors)
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, 4, 256, 0, gl.RGBA, gl.UNSIGNED_BYTE, colors)
      hasColors = true
    }
    // body: GET /biomes/<world>.json. Rows are biome ids, 4 texels each (grass, foliage, dry foliage, water); ids without data and
    // row 255 take the plains row, or the fixed group colours when there is no plains. Without colours the world keeps the fixed colours.
    const setBiomes = body => {
      if (!body?.colors) return false
      const count = body.colors.length / 12
      const fixed = [1, 2, 3, 4].flatMap(g => [...tintGroups.slice(g * 3, g * 3 + 3)].map(v => Math.round(v * 255)))
      const plainsAt = body.names.indexOf('plains')
      const plains = plainsAt >= 0 ? body.colors.slice(plainsAt * 12, plainsAt * 12 + 12) : fixed
      const bytes = new Uint8Array(4 * 256 * 4)
      for (let id = 0; id < 256; id++) {
        const row = id < count && id < 255 && body.names[id] ? body.colors.slice(id * 12, id * 12 + 12) : plains
        for (let t = 0; t < 4; t++) bytes.set([row[t * 3], row[t * 3 + 1], row[t * 3 + 2], 255], (id * 4 + t) * 4)
      }
      setBiomeColors(bytes)
      return true
    }
    // an unloaded slot reads as air with open sky (so loaded neighbours are not shaded black): only its flags are zeroed,
    // the stale blocks and light stay in the textures and every read checks the flag first
    const clearSlot = (sx, sz) => putFlags(sx, sz, size.noFlags)

    const dispose = () => {
      for (const t of [blocks, coarse, lightTex, biomes, biomeColors]) gl.deleteTexture(t)
      size = null
      world.bytes = 0
      worlds.delete(world)
    }

    const world = { allocate, uploadColumn, clearSlot, setBiomeColors, setBiomes, hasBiomeColors: () => hasColors, biomes, biomeColors, dispose, bind, isAllocated: () => size !== null, size: () => size, bytes: 0 }
    worlds.add(world)
    return world
  }

  // sets the canvas size (when this renderer was given a canvas) and the viewport
  const resize = (w, h) => {
    if (canvas && canvas.width !== w) canvas.width = w
    if (canvas && canvas.height !== h) canvas.height = h
    gl.viewport(0, 0, w, h)
  }

  // eye is relative to the window origin; entities are {min, max, color} in the same space; width/height (default: the canvas size)
  // are the viewport drawn at the framebuffer's bottom-left
  const draw = (world, { eye, basis, dist, darken, slotOff, entities, width = canvas.width, height = canvas.height }) => {
    const size = world.size()
    const labels = boxLabels({ boxes: entities, eye, basis, width, height, dist })
    const labelKey = labels.map(l => l.text).join('\n')
    if (labelKey !== labelAtlasKey) {
      labelAtlasKey = labelKey
      gl.activeTexture(gl.TEXTURE9)
      gl.bindTexture(gl.TEXTURE_2D, labelTexture)
      const atlas = labelAtlas(labels)
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, atlas.width, atlas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, atlas)
    }
    const shown = labelUniforms(labels, height)
    const count = Math.min(entities.length, MAX_ENTITIES)
    const models = modelUniforms(entities, layerIndex)
    gl.activeTexture(gl.TEXTURE0 + PART_UNIT)
    gl.bindTexture(gl.TEXTURE_2D, partsTexture)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 4, Math.max(1, models.rows.length / 16), 0, gl.RGBA, gl.FLOAT, models.rows.length ? models.rows : new Float32Array(16))
    const flat = key => new Float32Array(MAX_ENTITIES * 3).map((_, i) => (entities[Math.floor(i / 3)]?.[key]?.[i % 3]) ?? 0)
    world.bind()
    gl.viewport(0, 0, width, height)
    gl.uniform1i(uniform.uBlocks, 0)
    gl.uniform1i(uniform.uCoarse, 1)
    gl.uniform1i(uniform.uMats, 2)
    gl.uniform1i(uniform.uInfo, 3)
    gl.uniform1i(uniform.uTex, 4)
    gl.uniform1i(uniform.uElems, 6)
    gl.uniform1i(uniform.uElemBase, elemBase)
    gl.uniform3fv(uniform.uTintGroups, tintGroups)
    gl.uniform3fv(uniform.uTintConst, tintConst)
    gl.uniform1i(uniform.uLightTex, 5)
    gl.uniform1i(uniform.uBiomes, 7)
    gl.uniform1i(uniform.uBiomeColors, 8)
    gl.uniform1i(uniform.uHasBiomeColors, world.hasBiomeColors() ? 1 : 0)
    gl.uniform1f(uniform.uLodMax, lodMax)
    gl.uniform2f(uniform.uRes, width, height)
    gl.uniform3f(uniform.uEye, eye.x, eye.y, eye.z)
    gl.uniform3f(uniform.uFwd, basis.forward.x, basis.forward.y, basis.forward.z)
    gl.uniform3f(uniform.uRight, basis.right.x, basis.right.y, basis.right.z)
    gl.uniform3f(uniform.uUp, basis.up.x, basis.up.y, basis.up.z)
    gl.uniform1f(uniform.uHalf, basis.half)
    gl.uniform3i(uniform.uSize, size.n * 16, size.height, size.n * 16)
    gl.uniform2i(uniform.uSlotOff, slotOff.x, slotOff.z)
    gl.uniform1f(uniform.uDist, dist)
    gl.uniform1f(uniform.uDarken, darken)
    gl.uniform1i(uniform.uDebug, debug ? 1 : 0)
    gl.uniform1i(uniform.uEntCount, count)
    gl.uniform3fv(uniform.uEntMin, flat('min'))
    gl.uniform3fv(uniform.uEntMax, flat('max'))
    gl.uniform3fv(uniform.uEntCol, flat('color'))
    gl.uniform4fv(uniform.uEntRot, models.rot)
    gl.uniform3fv(uniform.uEntOrg, models.org)
    gl.uniform1i(uniform.uParts, PART_UNIT)
    gl.uniform1i(uniform.uLabels, 9)
    gl.uniform1i(uniform.uLabelCount, shown.count)
    gl.uniform4fv(uniform.uLabelRect, shown.rects)
    gl.uniform1fv(uniform.uLabelDepth, shown.depths)
    gl.uniform3fv(uniform.uLabelAt, shown.ats)
    gl.uniform1fv(uniform.uLabelDist, shown.dists)
    gl.drawArrays(gl.TRIANGLES, 0, 3)
  }

  const clear = (r, g, b) => {
    gl.clearColor(r, g, b, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)
  }

  const setDebug = on => { debug = on }
  // bytes of GPU texture memory: the shared tables and each live world (texture storage only; no driver padding or mip overhead guessed)
  const memory = () => {
    const shared = Object.values(sharedBytes).reduce((a, b) => a + b, 0)
    const perWorld = [...worlds].map(w => w.bytes)
    return { shared, sharedParts: { ...sharedBytes }, worlds: perWorld, total: shared + perWorld.reduce((a, b) => a + b, 0) }
  }
  return { gl, renderer, setDebug, setMaterials, setTextures, setElements, setTints, claimTable, createWorld, resize, draw, clear, memory, finish: () => gl.finish() }
}
