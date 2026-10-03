// Bakes a block state into model elements the way the game does: the blockstate JSON picks a variant (or the multipart parts that
// apply), the model JSON gives elements (parents followed, texture variables resolved), and the blockstate's x/y rotation turns
// the elements, their faces and their uv. Pure: it takes the blockstate and model maps tools/view/block-models.mjs loads.
//
// A baked element: { from, to, rotation, shade, faces } in 0..16 block space after the model rotation. `rotation` is the element's own
// rotation ({ origin, axis, angle, rescale }) or null. A face is { texture, uv, tintindex (-1 for none), cullface (or null),
// rotation (0/90/180/270) }, keyed by the direction it ends up facing.
// `shade` is false for elements the game draws without directional shading (plants). Not baked: `light_emission`, `ambientocclusion`, `display`; weighted variant lists take their first model.

const DIRS = { up: [0, 1, 0], down: [0, -1, 0], north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0] }
const FACE_ORDER = Object.keys(DIRS)

// each face's corners as (top-left, top-right, bottom-right, bottom-left) of its uv square, as +-1 points about the block centre
const CORNERS = {
  up: [[-1, 1, -1], [1, 1, -1], [1, 1, 1], [-1, 1, 1]],
  down: [[-1, -1, 1], [1, -1, 1], [1, -1, -1], [-1, -1, -1]],
  north: [[1, 1, -1], [-1, 1, -1], [-1, -1, -1], [1, -1, -1]],
  south: [[-1, 1, 1], [1, 1, 1], [1, -1, 1], [-1, -1, 1]],
  west: [[-1, 1, -1], [-1, 1, 1], [-1, -1, 1], [-1, -1, -1]],
  east: [[1, 1, 1], [1, 1, -1], [1, -1, -1], [1, -1, 1]]
}

const stripNamespace = id => id.replace(/^minecraft:/, '')
const asList = value => value === undefined ? [] : [value].flat()
const same = (a, b) => a.length === b.length && a.every((v, i) => v === b[i])
const clean = v => v + 0 // -0 to 0

// ---------------------------------------------------------------- rotation

// one quarter turn about x takes up to north to down to south; one about y takes north to east to south to west
const quarterX = ([x, y, z]) => [x, z, -y]
const quarterY = ([x, y, z]) => [-z, y, x]
const times = (n, turn, v) => n <= 0 ? v : times(n - 1, turn, turn(v))
const turnVector = ({ x, y }, v) => times(y / 90, quarterY, times(x / 90, quarterX, v)).map(clean) // x first, then y
const round = v => Math.round(v * 1e4) / 1e4 // 0.1 - 8 + 8 is not 0.1
const turnPoint = (turns, p) => turnVector(turns, p.map(v => v - 8)).map(v => round(v + 8))
const turnsOf = apply => ({ x: ((apply.x ?? 0) % 360 + 360) % 360, y: ((apply.y ?? 0) % 360 + 360) % 360 })

const directionOf = v => FACE_ORDER.find(d => same(DIRS[d], v))

// the position index (0..3) in the turned face's corner order of the corner that started at index 0, and the face it ended up on
const faceMove = (turns, dir) => {
  const to = directionOf(turnVector(turns, DIRS[dir]))
  const corner = turnVector(turns, CORNERS[dir][0])
  return { to, shift: CORNERS[to].findIndex(c => same(c, corner)) }
}

const turnElementRotation = (rotation, turns) => {
  if (!rotation) return null
  const axisVector = turnVector(turns, rotation.axis === 'x' ? [1, 0, 0] : rotation.axis === 'y' ? [0, 1, 0] : [0, 0, 1])
  const at = axisVector.findIndex(v => v !== 0)
  return {
    origin: turnPoint(turns, rotation.origin ?? [8, 8, 8]),
    axis: 'xyz'[at],
    angle: clean(rotation.angle * axisVector[at]),
    rescale: rotation.rescale ?? false
  }
}

// ---------------------------------------------------------------- uv

// vanilla's default uv: the element's from/to projected on the face
const defaultUv = (dir, [x1, y1, z1], [x2, y2, z2]) => ({
  up: [x1, z1, x2, z2],
  down: [x1, 16 - z2, x2, 16 - z1],
  north: [16 - x2, 16 - y2, 16 - x1, 16 - y1],
  south: [x1, 16 - y2, x2, 16 - y1],
  west: [z1, 16 - y2, z2, 16 - y1],
  east: [16 - z2, 16 - y2, 16 - z1, 16 - y1]
})[dir]

const cornerUvs = ([u0, v0, u1, v1]) => [[u0, v0], [u1, v0], [u1, v1], [u0, v1]]
const clockwise = ([u, v]) => [16 - v, u]

// the (uv, rotation) pair that gives these corner uvs (top-left first); a rect with u0 <= u1 and v0 <= v1 wins over a reversed one
const fromCorners = corners => {
  const fits = [0, 1, 2, 3].map(k => ({ k, base: [0, 1, 2, 3].map(i => corners[(i + k) % 4]) }))
    .filter(({ base }) => base[0][1] === base[1][1] && base[0][0] === base[3][0] && base[2][0] === base[1][0] && base[2][1] === base[3][1])
  const ordered = ({ base }) => base[0][0] <= base[2][0] && base[0][1] <= base[2][1]
  const { k, base } = fits.find(ordered) ?? fits[0]
  return { uv: [...base[0], ...base[2]], rotation: k * 90 }
}

// the turned face's uv and texture rotation. Without uvlock the texture turns with the block: same uv, the face rotation grows by the
// quarter turns the face's corners moved. With uvlock the uv is turned back about the face centre so the texture stays world-aligned.
const turnFaceUv = (uv, rotation, shift, uvlock) => {
  if (!shift) return { uv, rotation }
  if (!uvlock) return { uv, rotation: (rotation + 90 * shift) % 360 }
  const base = cornerUvs(uv)
  const spin = point => times(shift, clockwise, point)
  return fromCorners([0, 1, 2, 3].map(j => spin(base[(((j - shift - rotation / 90) % 4) + 8) % 4])))
}

// ---------------------------------------------------------------- models

// "#var" through the merged textures, to the texture's file name (no namespace, no block/ directory); null when it never resolves
const textureName = (textures, reference) => {
  let name = reference
  for (let hops = 0; hops < 16; hops++) {
    if (name && typeof name === 'object') name = name.sprite // 26.1 models: { sprite, force_translucent }
    if (typeof name !== 'string') break
    const bare = !name.includes(':') && !name.includes('/') && !name.startsWith('#') && name in textures // heavy_core names its texture "all"
    if (!name.startsWith('#') && !bare) break
    name = textures[bare ? name : name.slice(1)]
  }
  return typeof name === 'string' && !name.startsWith('#') ? stripNamespace(name).replace(/^block\//, '') : null
}

const resolveModel = (models, id) => {
  const textures = {}
  let elements = null
  for (let name = stripNamespace(id), hops = 0; name && hops < 32; name = stripNamespace(models.get(name)?.parent ?? ''), hops++) {
    const model = models.get(name)
    if (!model) break
    for (const [key, value] of Object.entries(model.textures ?? {})) textures[key] ??= value
    elements ??= model.elements ?? null
  }
  return { elements: elements ?? [], textures }
}

const bakeFace = (face, dir, element, textures, turns, uvlock) => {
  const { to, shift } = faceMove(turns, dir)
  const { uv, rotation } = turnFaceUv(face.uv ?? defaultUv(dir, element.from, element.to), face.rotation ?? 0, shift, uvlock)
  const cullface = face.cullface ? directionOf(turnVector(turns, DIRS[face.cullface] ?? [0, 0, 0])) ?? null : null
  return [to, { texture: textureName(textures, face.texture), uv, tintindex: face.tintindex ?? -1, cullface, rotation }]
}

const bakeElement = (element, textures, turns, uvlock) => {
  const corners = [turnPoint(turns, element.from), turnPoint(turns, element.to)]
  const faces = Object.entries(element.faces ?? {}).filter(([dir]) => DIRS[dir]).map(([dir, face]) => bakeFace(face, dir, element, textures, turns, uvlock))
  return {
    from: [0, 1, 2].map(i => Math.min(corners[0][i], corners[1][i])),
    to: [0, 1, 2].map(i => Math.max(corners[0][i], corners[1][i])),
    rotation: turnElementRotation(element.rotation, turns),
    shade: element.shade ?? true,
    faces: Object.fromEntries(faces.sort(([a], [b]) => FACE_ORDER.indexOf(a) - FACE_ORDER.indexOf(b)))
  }
}

export const bakeApply = (apply, models) => {
  const { elements, textures } = resolveModel(models, apply.model)
  return elements.map(element => bakeElement(element, textures, turnsOf(apply), apply.uvlock ?? false))
}

// ---------------------------------------------------------------- blockstates

const pairMatches = props => pair => {
  const at = pair.indexOf('=')
  return String(props[pair.slice(0, at)]) === pair.slice(at + 1)
}

const whenMatches = (when, props) => {
  if (!when) return true
  if (when.OR) return asList(when.OR).some(c => whenMatches(c, props))
  if (when.AND) return asList(when.AND).every(c => whenMatches(c, props))
  return Object.entries(when).every(([key, value]) => String(value).split('|').includes(String(props[key])))
}

// what the blockstate uses for these props: a source label (naming the variant or the parts) and the model applications
const select = (name, blockstate, props) => {
  if (blockstate.multipart) {
    const parts = blockstate.multipart.map((part, i) => ({ part, i })).filter(({ part }) => whenMatches(part.when, props))
    return { source: `${name} multipart [${parts.map(({ i }) => i).join(',')}]`, applies: parts.map(({ part }) => asList(part.apply)[0]).filter(Boolean) }
  }
  const key = Object.keys(blockstate.variants ?? {}).find(k => k === '' || k.split(',').every(pairMatches(props)))
  if (key === undefined) return { source: `${name} no matching variant`, applies: [] }
  return { source: `${name} variant "${key}"`, applies: asList(blockstate.variants[key]).slice(0, 1) }
}

// per models map: baked elements by source label, so states that use the same variant bake once
const caches = new WeakMap()
const cacheFor = models => {
  if (!caches.has(models)) caches.set(models, new Map())
  return caches.get(models)
}

export function bakeState ({ name, props }, { blockstates, models }) {
  const blockstate = blockstates.get(name)
  if (!blockstate) return null
  const { source, applies } = select(name, blockstate, props)
  const cache = cacheFor(models)
  if (!cache.has(source)) cache.set(source, applies.flatMap(apply => bakeApply(apply, models)))
  return { elements: cache.get(source), source }
}

// 'cube': one unrotated element over the whole block; 'empty': no elements (block entities, particle-only models); else 'model'
export function classify (baked) {
  if (baked.elements.length === 0) return 'empty'
  const [element] = baked.elements
  const full = baked.elements.length === 1 && !element.rotation && same(element.from, [0, 0, 0]) && same(element.to, [16, 16, 16])
  return full ? 'cube' : 'model'
}

// ---------------------------------------------------------------- the registry

const propertyValues = state => state.type === 'bool' ? [true, false] : state.values

// state id to property values, the last property varying fastest; a bool's first value is true
const propsOf = (block, id) => {
  const props = {}
  let offset = id - block.minStateId
  for (let i = block.states.length - 1; i >= 0; i--) {
    const state = block.states[i]
    props[state.name] = propertyValues(state)[offset % state.num_values]
    offset = Math.floor(offset / state.num_values)
  }
  return props
}

// Map(stateId -> baked or null) over every state of the registry, and counts: states, baked (has a blockstate), withElements,
// distinctKeys (distinct element lists by JSON), totalElements (summed over those distinct lists), maxElements, noBlockstate
export function bakeAll (registry, jar) {
  const states = new Map()
  const distinct = new Map()
  const counts = { states: 0, baked: 0, withElements: 0, maxElements: 0, noBlockstate: 0 }
  for (const block of Object.values(registry.blocksByName)) {
    for (let id = block.minStateId; id <= block.maxStateId; id++) {
      const baked = bakeState({ name: block.name, props: propsOf(block, id) }, jar)
      states.set(id, baked)
      counts.states++
      if (!baked) {
        counts.noBlockstate++
        continue
      }
      counts.baked++
      if (!baked.elements.length) continue
      counts.withElements++
      counts.maxElements = Math.max(counts.maxElements, baked.elements.length)
      distinct.set(JSON.stringify(baked.elements), baked.elements.length)
    }
  }
  const summary = {
    states: counts.states,
    baked: counts.baked,
    withElements: counts.withElements,
    distinctKeys: distinct.size,
    totalElements: [...distinct.values()].reduce((a, b) => a + b, 0),
    maxElements: counts.maxElements,
    noBlockstate: counts.noBlockstate
  }
  return { states, summary }
}
