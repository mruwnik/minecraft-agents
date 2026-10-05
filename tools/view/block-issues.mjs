// Why JavaScript: view build pipeline; compares the JS material table against block names, shares code with the block scan worker.
// Which blocks the browser view cannot draw properly. Pure: it compares what the view's material table does with a block
// (a kind, one texture per face, a box) against the block's real visual model from the client jar, and merges the result
// with what was seen in the world. Nothing is fixed here; the output is a durable list for whoever fixes them later.
import tints from 'minecraft-data/minecraft-data/data/pc/26.1/tints.json' with { type: 'json' }
import { tintOf } from './renderer.mjs'
import { COLORMAP_BLOCKS } from './tints.mjs'
import { OVER_CAP, ELEMENT_CAP } from './materials.mjs'

export { COLORMAP_BLOCKS }

export const REASONS = ['unknown-state', 'no-texture', 'shape-mismatch', 'shape-approximated', 'tint-missing', 'state-ignored', 'no-model-data', 'block-entity', 'model-over-cap', 'tint-approximate', 'element-outside-voxel']

// missing: the block vanishes or is a hash colour; wrong: it reads as a different thing; approximate: right place and colour,
// simplified. Each reason has a default; a record may carry a milder one where the case is milder (see blockRecords).
export const SEVERITIES = ['missing', 'wrong', 'approximate']
const DEFAULT_SEVERITY = {
  'unknown-state': 'missing',
  'no-texture': 'missing',
  'shape-mismatch': 'wrong',
  'tint-missing': 'wrong',
  'block-entity': 'wrong',
  'shape-approximated': 'approximate',
  'state-ignored': 'approximate',
  'no-model-data': 'approximate', // nothing is known to be wrong, it is unchecked
  'model-over-cap': 'approximate', // the state has more elements than the view draws: its bounding box stands in
  'element-outside-voxel': 'approximate', // parts of the model lie outside its own voxel; the view draws only what is inside
  'tint-approximate': 'approximate' // a biome colour drawn as one fixed plains colour
}
export const severityOf = reason => DEFAULT_SEVERITY[reason] ?? 'approximate'

// Blocks whose model has geometry but whose items the game adds with a block-entity renderer (the lectern's and the enchanting table's books are drawn: block-entity-models.mjs)
// Blocks the game colours with a registered colour provider: constant tints by name (minecraft-data's tints.json) and the colormap
// groups in tools/view/tints.mjs, which says where its list is FROM MEMORY. A `tintindex` in a model only says a face asks for a tint.
const FROM_DATA = new Set([...tints.constant.data.flatMap(entry => entry.keys), 'redstone_wire'])
const TINTED_BLOCKS = new Set([...FROM_DATA, ...COLORMAP_BLOCKS])
// bubble_column is water in the game; moving_piston is never drawn as a block
const WATER_LIKE = new Set(['bubble_column'])
const NEVER_DRAWN = new Set(['moving_piston'])
const PARTLY_ENTITY = new Set(['campfire', 'soul_campfire'])

const CROSS_PARENTS = new Set(['block/cross', 'block/tinted_cross', 'block/flower_pot_cross'])
const FULL_FROM = [0, 0, 0]
const FULL_TO = [16, 16, 16]
const NO_JAR_NOTE = 'no client jar found (set $MC_CLIENT_JAR or install a release): only blocks without a texture can be reported'

const stripNamespace = id => id.replace(/^minecraft:/, '')
const same = (a, b) => a.length === b.length && a.every((v, i) => v === b[i])
const asList = value => value === undefined ? [] : [value].flat()

// ---------------------------------------------------------------- models

// A model with its parent chain followed: `chain` is the model's own id then its ancestors, `elements` those of the nearest
// ancestor that has any, `textures` merged with the child's winning.
// Not handled: `display`, element `rotation` (a rotated element is only ever "not a plain cube"), `ambientocclusion`, and
// models the game builds in code (builtin/entity, as chests and signs have): those resolve to no elements.
export const resolveModel = (models, id) => {
  const chain = []
  const textures = {}
  let elements = null
  for (let name = stripNamespace(id); name && chain.length < 32; name = stripNamespace(models.get(name)?.parent ?? '')) {
    chain.push(name)
    const model = models.get(name)
    if (!model) break
    for (const [key, value] of Object.entries(model.textures ?? {})) textures[key] ??= value
    elements ??= model.elements ?? null
  }
  return { chain, elements: elements ?? [], textures }
}

const isFullElement = element => !element.rotation && same(element.from, FULL_FROM) && same(element.to, FULL_TO)

// 'cross' (the parents the renderer's cross stands for), 'cube' (one plain 0..16 box), else 'complex'
export const modelKind = resolved => {
  if (resolved.chain.some(name => CROSS_PARENTS.has(name))) return 'cross'
  if (resolved.elements.length === 1 && isFullElement(resolved.elements[0])) return 'cube'
  return 'complex'
}

const textureOf = (textures, reference) => {
  let name = reference
  for (let hops = 0; typeof name === 'string' && name.startsWith('#') && hops < 16; hops++) name = textures[name.slice(1)]
  return typeof name === 'string' && !name.startsWith('#') ? stripNamespace(name).replace(/^block\//, '') : null
}

// textures of faces that ask for a biome tint
export const tintedTextures = resolved => [...new Set(resolved.elements
  .flatMap(element => Object.values(element.faces ?? {}))
  .filter(face => face.tintindex !== undefined)
  .map(face => textureOf(resolved.textures, face.texture))
  .filter(Boolean))]

// ---------------------------------------------------------------- blockstates

const applyModels = apply => asList(apply).map(a => a.model).filter(Boolean).map(stripNamespace)

const conditionProperties = when => {
  if (!when) return []
  return Object.entries(when).flatMap(([key, value]) => key === 'OR' || key === 'AND' ? asList(value).flatMap(conditionProperties) : [key])
}

// the model ids a blockstate can use, and the properties it varies on
export const blockstateUses = blockstate => {
  const variants = Object.entries(blockstate.variants ?? {})
  const multipart = blockstate.multipart ?? []
  return {
    models: [...new Set([...variants.flatMap(([, v]) => applyModels(v)), ...multipart.flatMap(part => applyModels(part.apply))])],
    properties: [...new Set([
      ...variants.flatMap(([key]) => key.split(',').filter(Boolean).map(pair => pair.split('=')[0])),
      ...multipart.flatMap(part => conditionProperties(part.when))
    ])]
  }
}

// ---------------------------------------------------------------- states

// state id to property values, the last property varying fastest; a bool's first value is true
const propertyValues = state => state.type === 'bool' ? [true, false] : state.values
const indicesOf = (block, id) => {
  const indices = []
  let offset = id - block.minStateId
  for (let i = block.states.length - 1; i >= 0; i--) {
    indices[i] = offset % block.states[i].num_values
    offset = Math.floor(offset / block.states[i].num_values)
  }
  return indices
}
const propsOf = (block, id) => Object.fromEntries(indicesOf(block, id).map((v, i) => [block.states[i].name, propertyValues(block.states[i])[v]]))

const range = block => Array.from({ length: block.maxStateId - block.minStateId + 1 }, (_, i) => block.minStateId + i)

// ---------------------------------------------------------------- classification

const describeBox = box => `box [${box.join(',')}]`
const drawnAs = material => material.kind === 'box' ? describeBox(material.box) : material.kind
const hasMissingFace = material => material.tex.some(layer => layer < 0)
// what the view draws from the jar's own model: a six-face cube or an element model (a bounding box when over the element cap)
const isOverCap = material => ((material.flags ?? 0) & OVER_CAP) !== 0
const isModelled = material => material.kind === 'model' || material.tex6 !== undefined
const fromJar = material => isModelled(material) || isOverCap(material)

const sameBox = (element, box) => !element.rotation && same([...element.from, ...element.to].map(v => Math.round(v)), box)
// a model the drawn box stands for exactly: one unrotated element with the same from/to
const matchesBox = (model, box) => model.elements.length === 1 && sameBox(model.elements[0], box)
// upright planes (crops), torches: a cross is a fair picture of them
const isUprightPlanes = model => model.chain.includes('block/crop') || model.chain.some(name => /torch/.test(name)) ||
  (model.elements.length > 0 && model.elements.every(e => (e.from[0] === e.to[0] || e.from[2] === e.to[2]) && e.to[1] - e.from[1] >= 8))

// a drawn material against the block's models: null when it is a fair picture, else the reason and its severity
const shapeVerdict = (resolved, material) => {
  if (material.kind === 'water' || material.kind === 'lava') return null
  const kinds = resolved.map(modelKind)
  const all = kind => kinds.every(k => k === kind)
  if (material.kind === 'cross') {
    if (all('cross')) return null
    return { reason: 'shape-mismatch', severity: resolved.every(m => modelKind(m) === 'cross' || isUprightPlanes(m)) ? 'approximate' : undefined }
  }
  if (all('cross')) return { reason: 'shape-mismatch' }
  if (material.kind === 'cube') return kinds.includes('cube') ? null : { reason: 'shape-approximated' }
  if (resolved.some(m => matchesBox(m, material.box))) return null
  return { reason: all('cube') ? 'shape-mismatch' : 'shape-approximated' }
}

// extra: severity (a milder one than the reason's default), model (id), parents (its chain), ignored (properties)
// drawnAs comes from the first affected state's material; drawnVariants lists every distinct picture when there are several
const makeRecord = (block, reason, ids, materialAt, detail, extra = {}, resolved = []) => {
  const drawn = ids.map(id => drawnAs(materialAt(id)))
  const variants = Object.entries(drawn.reduce((count, d) => ({ ...count, [d]: (count[d] ?? 0) + 1 }), {}))
  const model = extra.model ? resolved.find(m => m.id === extra.model) : null
  return {
    name: block.name,
    reason,
    severity: extra.severity ?? severityOf(reason),
    drawnAs: reason === 'no-texture' ? 'hash colour' : drawn[0],
    ...(variants.length > 1 && reason !== 'no-texture' ? { drawnVariants: Object.fromEntries(variants) } : {}),
    detail,
    ...(model ? { model: model.id, parents: model.chain } : {}),
    ...(extra.ignored ? { ignored: extra.ignored } : {}),
    example: { stateId: ids[0], props: propsOf(block, ids[0]) },
    states: ids.length,
    statesTotal: extra.total
  }
}

// a property is honoured when two states differing only in it get different materials
const honouredProperties = (block, ids, materialOf) => {
  const seen = block.states.map(() => new Map())
  const honoured = new Set()
  for (const id of ids) {
    const indices = indicesOf(block, id)
    block.states.forEach((state, p) => {
      if (honoured.has(state.name)) return
      const key = indices.map((v, i) => i === p ? '*' : v).join(',')
      const before = seen[p].get(key)
      if (before === undefined) return seen[p].set(key, materialOf[id])
      if (before !== materialOf[id]) honoured.add(state.name)
    })
  }
  return honoured
}

const blockRecords = ({ block, materials, materialOf, models }) => {
  const ids = range(block).filter(id => materialOf[id] !== 0)
  if (!ids.length) return []
  const materialAt = id => materials[materialOf[id]]
  const total = ids.length
  const record = (reason, affected, detail, extra, resolved) => makeRecord(block, reason, affected, materialAt, detail, { total, ...extra }, resolved)
  const records = []
  if (NEVER_DRAWN.has(block.name)) return records
  if (WATER_LIKE.has(block.name)) return materialAt(ids[0]).kind === 'water' ? [] : [record('shape-mismatch', ids, `drawn as ${drawnAs(materialAt(ids[0]))}; the game draws it as water`)]

  const blockstate = models?.blockstates.get(block.name)
  const uses = blockstate ? blockstateUses(blockstate) : null
  const resolved = uses ? uses.models.map(id => ({ id, ...resolveModel(models.models, id) })) : []

  const isBare = material => {
    if (material.kind === 'water' || material.kind === 'lava' || isOverCap(material)) return false
    return isModelled(material) ? (material.noTexture?.length ?? 0) > 0 : hasMissingFace(material)
  }
  // no geometry at all (particle-only, builtin/entity): the game draws it with a block-entity renderer, which is the record that matters
  const entityOnly = resolved.length > 0 && resolved.every(m => m.elements.length === 0)
  const bare = entityOnly ? [] : ids.filter(id => isBare(materialAt(id)))
  if (bare.length) {
    const first = materialAt(bare[0])
    const faces = ['top', 'side', 'bottom'].filter((_, i) => first.tex[i] < 0)
    const named = [...new Set(resolved.flatMap(m => Object.values(m.textures)).map(t => textureOf({}, t)).filter(Boolean))]
    const detail = isModelled(first) ? `the model names a texture the texture folder lacks: ${first.noTexture.join(', ')}`
      : named.length ? `texture lookup by name failed; the model uses ${named.join(', ')}` : `no texture found for the ${faces.join(', ')} face`
    records.push(record('no-texture', bare, detail))
  }
  if (!models || block.name === 'water' || block.name === 'lava') return records
  if (!blockstate) return [...records, record('no-model-data', ids, 'no blockstate for this block in the client jar (newer than the jar?)')]

  const describe = model => model ? `model ${model.id} (${model.chain.join(' > ')}), ${model.elements.length} element${model.elements.length === 1 ? '' : 's'}` : 'no model'

  // states the view draws from its own entity models (block-entity-models.mjs) are not reported here
  const undrawn = entityOnly ? ids.filter(id => !materialAt(id).entity) : ids
  if ((entityOnly && undrawn.length) || PARTLY_ENTITY.has(block.name)) {
    const detail = entityOnly ? `the model has no elements, the game draws it with a block-entity renderer: ${describe(resolved[0])}` : 'the model has geometry, but its items are drawn by a block-entity renderer'
    records.push(record('block-entity', undrawn, detail, { model: resolved[0]?.id, severity: entityOnly ? undefined : 'approximate' }, resolved))
  }
  if (entityOnly) return records

  const unmodelled = ids.filter(id => !fromJar(materialAt(id)))
  const verdicts = unmodelled.map(id => [id, shapeVerdict(resolved, materialAt(id))])
  for (const reason of ['shape-mismatch', 'shape-approximated']) {
    const hits = verdicts.filter(([, v]) => v?.reason === reason)
    if (!hits.length) continue
    const affected = hits.map(([id]) => id)
    const severities = new Set(hits.map(([, v]) => v.severity ?? severityOf(reason)))
    const severity = SEVERITIES.find(level => severities.has(level)) // the worst of the affected states
    const offender = resolved.find(m => modelKind(m) !== (materialAt(affected[0]).kind === 'cross' ? 'cross' : 'cube')) ?? resolved[0]
    records.push(record(reason, affected, `drawn as ${drawnAs(materialAt(affected[0]))}; ${describe(offender)}`, { model: offender?.id, severity }, resolved))
  }

  const tinted = !TINTED_BLOCKS.has(block.name) || !unmodelled.length ? [] : [...new Set(resolved.flatMap(tintedTextures))].filter(texture => !tintOf(texture))
  // an overlay left untinted (the grass block's side) leaves the base drawn right
  if (tinted.length) records.push(record('tint-missing', unmodelled, `faces ask for a biome tint, none for: ${tinted.join(', ')}`, { severity: tinted.every(t => t.includes('overlay')) ? 'approximate' : undefined }))

  const capped = ids.filter(id => isOverCap(materialAt(id)))
  if (capped.length) records.push(record('model-over-cap', capped, `a state has more than ${ELEMENT_CAP} elements; drawn as the bounding box of them`))
  const outside = ids.filter(id => materialAt(id).outside)
  if (outside.length) records.push(record('element-outside-voxel', outside, 'some elements reach outside 0..16 of the block; the view draws only the part inside the voxel'))
  const biome = ids.filter(id => materialAt(id).tint?.length)
  if (biome.length) records.push(record('tint-approximate', biome, `faces use a biome colour (${[...new Set(biome.flatMap(id => materialAt(id).tint))].join(', ')}); drawn with one fixed plains colour`))

  const honoured = honouredProperties(block, unmodelled, materialOf)
  const known = new Set(block.states.map(s => s.name))
  const ignored = unmodelled.length ? uses.properties.filter(p => known.has(p) && !honoured.has(p)).sort() : []
  if (ignored.length) records.push(record('state-ignored', unmodelled, `the blockstate depends on ${ignored.join(', ')}; the view draws every value the same`, { ignored }))
  return records
}

// registry: { blocksByName: { name: { name, minStateId, maxStateId, states: [{ name, type, num_values, values }] } } }
// materials / materialOf: the view's table (materialOf[stateId] is an index into materials; 0 is air)
// models: loadModels(jar) or null when there is no jar
export function classifyStatic ({ registry, materials, materialOf, models }) {
  const records = Object.values(registry.blocksByName).flatMap(block => blockRecords({ block, materials, materialOf, models }))
  if (models) return records
  return [...records, { name: '*', reason: 'no-model-data', severity: 'approximate', drawnAs: '-', detail: NO_JAR_NOTE, example: { stateId: 0, props: {} }, states: 0 }]
}

// ---------------------------------------------------------------- merging with the world

const keyOf = record => `${record.name}\0${record.reason}`

// seen: Map(block name or 'state:<id>' -> { count, first: { world, x, y, z, agent } }). A block with several reasons shows
// its one count on each of its records, so count records, not seen totals, when adding up.
export function mergeSeen (staticRecords, seen) {
  const unknown = [...seen.keys()].filter(name => name.startsWith('state:')).map(name => {
    const [lo, hi] = name.slice(6).split('-').map(Number)
    return {
      name,
      reason: 'unknown-state',
      drawnAs: 'air',
      detail: 'state ids the view table does not know (outside its range, or in no block): the page draws them as air',
      example: { stateId: lo, props: {} },
      states: (hi ?? lo) - lo + 1
    }
  })
  const unique = new Map()
  for (const record of [...staticRecords, ...unknown]) if (!unique.has(keyOf(record))) unique.set(keyOf(record), record)
  return [...unique.values()]
    .map(record => ({
      ...record,
      severity: record.severity ?? severityOf(record.reason),
      seen: seen.get(record.name)?.count ?? 0,
      firstSeen: seen.get(record.name)?.first ?? null,
      ...(seen.get(record.name)?.firsts ? { positions: seen.get(record.name).firsts } : {})
    }))
    .sort((a, b) => SEVERITIES.indexOf(a.severity) - SEVERITIES.indexOf(b.severity) || b.seen - a.seen || (a.name < b.name ? -1 : a.name > b.name ? 1 : 0) || (a.reason < b.reason ? -1 : a.reason > b.reason ? 1 : 0))
}
