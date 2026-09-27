import { populationWorkspaces } from '../villager/population.mjs'
import { JOB_BLOCK_PROFESSION } from '../villager/trade.mjs'
import { readStructureLayers } from '../structure/layers.mjs'
import { validateBlueprintDocument, semanticBlueprintHash, BLUEPRINT_LIMITS } from './schema.mjs'
import { REGISTRY, derivedState } from './format.mjs'
import { materialCandidates } from './materials.mjs'

const key = p => p.join(',')
const fail = (path, message) => { throw new Error(`blueprint ${path}: ${message}`) }
const STEP = { north: [0, -1], east: [1, 0], south: [0, 1], west: [-1, 0] }
const TYPES = ['block', 'bed', 'door', 'fence_gate', 'trapdoor', 'farmland', 'crop', 'water']
const CONSTRUCTION = new Set(['facing', 'axis', 'half', 'type', 'part', 'hanging', 'face', 'waterlogged'])
export function compileBlueprintStructure (source, registry = REGISTRY) {
  const document = validateBlueprintDocument(source)
  const [width, height, depth] = document.dimensions
  for (const [slot, material] of Object.entries(document.materials ?? {})) for (const name of [...(material.candidates ?? []), ...(material.preferences ?? []), ...(material.acceptExisting ?? [])]) if (!registry.blocksByName[name]) fail(`materials.${slot}`, `unknown block ${name}`)
  const cells = new Map(), objects = []
  const inside = p => Array.isArray(p) && p.length === 3 && p.every(Number.isInteger) && p[0] >= 0 && p[0] < width && p[2] >= 0 && p[2] < depth && p[1] >= -4 && p[1] < height
  const put = (at, cell, path) => {
    if (!inside(at)) fail(path, `cell ${key(at)} is outside dimensions`)
    if (cells.has(key(at))) fail(path, `overlapping cell ${key(at)}`)
    if (cells.size >= BLUEPRINT_LIMITS.cells) fail(path, 'more than 16384 constrained cells')
    cells.set(key(at), { at: [...at], ...cell })
  }
  const add = (spec, at, id, path) => {
    if (!spec || typeof spec !== 'object' || Array.isArray(spec)) fail(path, 'must be an object specification')
    if (spec.require) {
      if (Object.keys(spec).some(k => k !== 'require')) fail(path, 'space requirement cannot also declare a block')
      if (!['air', 'preserve'].includes(spec.require)) fail(path, 'unsupported requirement')
      if (spec.require === 'air') put(at, { require: 'air' }, path)
      return
    }
    for (const k of Object.keys(spec)) if (!['id', 'at', 'type', 'material', 'block', 'constructionState', 'initialState', 'tags'].includes(k)) fail(`${path}.${k}`, 'unsupported field')
    const type = spec.type ?? 'block'
    if (typeof id !== 'string' || !/^[a-zA-Z0-9][a-zA-Z0-9_-]{0,127}$/.test(id) || ['constructor', 'prototype', '__proto__'].includes(id)) fail(`${path}.id`, 'invalid object identifier')
    if (spec.tags !== undefined && (!Array.isArray(spec.tags) || !spec.tags.every(t => typeof t === 'string'))) fail(`${path}.tags`, 'must be a string array')
    if (!TYPES.includes(type)) fail(`${path}.type`, 'unsupported construction operation')
    if (Boolean(spec.material) === Boolean(spec.block)) fail(path, 'declare exactly one material slot or exact block')
    if (spec.material && !document.materials?.[spec.material]) fail(`${path}.material`, 'unknown slot')
    if (spec.block && !registry.blocksByName[spec.block]) fail(`${path}.block`, 'unknown block')
    const slotKind = document.materials?.[spec.material]?.kind
    if ((slotKind === 'bed' || /_bed$/.test(spec.block ?? '')) && type !== 'bed') fail(path, 'bed requires an atomic type=bed object')
    if ((slotKind === 'door' || /_door$/.test(spec.block ?? '')) && type !== 'door') fail(path, 'door requires an atomic type=door object')
    if (['farmland', 'water'].includes(spec.block) && type !== spec.block) fail(path, `use the explicit ${spec.block} recipe type`)
    if (['farmland', 'water'].includes(type) && spec.block !== type) fail(path, `${type} recipe requires exact block=${type}`)
    if (type === 'crop' && !['wheat', 'carrots', 'potatoes', 'beetroots'].includes(spec.block)) fail(path, 'unsupported crop recipe')
    const states = Object.fromEntries(Object.entries(spec.constructionState ?? {}).map(([k, v]) => [k, String(v)]))
    for (const k of Object.keys(states)) if (!CONSTRUCTION.has(k) || (spec.block && derivedState(spec.block, k))) fail(`${path}.constructionState.${k}`, 'state is not established by a supported placement recipe')
    const initialState = { ...(spec.initialState ?? {}) }
    if (document.guarantees?.includes('closed_entrances') && ['door', 'fence_gate', 'trapdoor'].includes(type)) {
      if (initialState.open === true) fail(path, 'open entrance contradicts closed_entrances guarantee')
      initialState.open = false
    }
    if (Object.keys(initialState).some(k => k !== 'open') || (Object.keys(initialState).length && !['door', 'fence_gate', 'trapdoor'].includes(type)) || (initialState.open !== undefined && typeof initialState.open !== 'boolean')) fail(`${path}.initialState`, 'only door/gate open:boolean is supported')
    if (['bed', 'door'].includes(type) && !STEP[states.facing]) fail(`${path}.constructionState.facing`, 'multipart object needs cardinal facing')
    if (type === 'bed' && states.part !== undefined && states.part !== 'foot') fail(path, 'bed must be authored at its foot')
    if (type === 'door' && states.half !== undefined && states.half !== 'lower') fail(path, 'door must be authored at its lower half')
    if (states.type === 'double' && /_slab$/.test(spec.block ?? '') || states.type === 'double' && document.materials?.[spec.material]?.kind === 'slab') fail(path, 'double slabs require an unsupported merge recipe')
    if (type === 'bed') states.part = 'foot'
    if (type === 'door') states.half = 'lower'
    const footprint = [{ at, states }]
    if (type === 'bed') { const [dx, dz] = STEP[states.facing]; footprint.push({ at: [at[0] + dx, at[1], at[2] + dz], states: { ...states, part: 'head' } }) }
    if (type === 'door') footprint.push({ at: [at[0], at[1] + 1, at[2]], states: { ...states, half: 'upper' } })
    const object = { id, type, material: spec.material, block: spec.block, states, initialState, tags: spec.tags ?? [], footprint }
    objects.push(object)
    for (const cell of footprint) put(cell.at, { objectId: id, states: cell.states }, path)
  }
  const geometry = readStructureLayers(document.structure.layers ?? [], { width, depth, minY: -4, maxY: height - 1, maxCells: BLUEPRINT_LIMITS.cells, label: 'blueprint structure' })
  for (const { x, y, z, token, layer } of geometry.cells) {
    const spec = token === '.' ? { require: 'air' } : document.structure.legend?.[token]
    if (!spec) fail(`structure.layers[${layer}]`, `unknown token ${token}`)
    add(spec, [x, y, z], `cell-${x}-${y}-${z}`, `structure.layers[${layer}]`)
  }
  for (const [i, spec] of (document.structure.objects ?? []).entries()) {
    if (!inside(spec.at)) fail(`structure.objects[${i}].at`, 'invalid coordinates')
    const id = spec.id ?? `object-${i}`
    if (objects.some(o => o.id === id)) fail(`structure.objects[${i}].id`, 'duplicate object id')
    add(spec, spec.at, id, `structure.objects[${i}]`)
  }
  for (const [i, space] of (document.structure.spaces ?? []).entries()) {
    if (Object.keys(space).some(k => !['box', 'require'].includes(k))) fail(`structure.spaces[${i}]`, 'unsupported space field')
    if (!Array.isArray(space.box) || space.box.length !== 2 || !space.box.every(inside) || space.box[0].some((n, a) => n > space.box[1][a]) || space.require !== 'air_except_declared_objects') fail(`structure.spaces[${i}]`, 'needs ordered in-bounds box and air_except_declared_objects')
    const [a, b] = space.box
    for (let y = a[1]; y <= b[1]; y++) for (let z = a[2]; z <= b[2]; z++) for (let x = a[0]; x <= b[0]; x++) {
      const at = [x, y, z], existing = cells.get(key(at))
      if (existing?.objectId || existing?.require === 'air') continue
      put(at, { require: 'air' }, `structure.spaces[${i}]`)
    }
  }
  for(const w of populationWorkspaces(document.population ?? {})){
    const cell=cells.get(key(w.at)),object=objects.find(o=>o.id===cell?.objectId)
    if(!object?.block||JOB_BLOCK_PROFESSION[object.block]!==w.profession)fail(`population.workspace.${w.id}`,'must point to an exact authored workstation block for its profession')
  }
  if (!cells.size) fail('structure', 'must constrain at least one world cell')
  return { document, hash: semanticBlueprintHash(document), width, height, depth, objects, cells: [...cells.values()] }
}

// Existing executor input: every object is frozen to one concrete variant before conversion.
export function concreteBlueprint (ir, assignments, registry = REGISTRY) {
  const legend = {}, indexed = new Map(), layers = new Map()
  for (const cell of ir.cells) {
    const object = cell.objectId && ir.objects.find(o => o.id === cell.objectId)
    const name = object ? assignments[object.id] ?? object.block : 'air'
    if (!name || !registry.blocksByName[name]) fail(`allocation.${cell.objectId}`, 'missing or unknown concrete block')
    const alt = { name, states: cell.states ?? {} }
    if (object) {
      if (object.block && name !== object.block) fail(`allocation.${object.id}`, 'exact block pin cannot be substituted')
      if (object.material) {
        const slot = ir.document.materials[object.material]
        if (!materialCandidates({ ...slot, candidates: [name] }, registry).includes(name) || (slot.candidates && !slot.candidates.includes(name) && !slot.acceptExisting?.includes(name))) fail(`allocation.${object.id}`, 'selected material violates hard capabilities or candidates')
      }
      const expected = object.type === 'bed' ? /_bed$/ : object.type === 'door' ? /_door$/ : object.type === 'fence_gate' ? /_fence_gate$/ : object.type === 'trapdoor' ? /_trapdoor$/ : null
      if (expected && !expected.test(name)) fail(`allocation.${object.id}`, 'selected block has wrong object kind')
      for (const [state, value] of Object.entries(alt.states)) {
        const known = registry.blocksByName[name].states?.find(s => s.name === state)
        const values = known?.type === 'bool' ? ['true', 'false'] : known?.values?.map(String)
        if (!values?.includes(value)) fail(`allocation.${object.id}.${state}`, 'unsupported concrete state')
      }
    }
    const signature = JSON.stringify([alt, object?.tags ?? []])
    if (!indexed.has(signature)) {
      const token = String.fromCharCode(0x100 + indexed.size)
      indexed.set(signature, token); legend[token] = { token, alts: object?.type === 'fence_gate' && STEP[alt.states.facing] ? [alt, { name, states: { ...alt.states, facing: { east: 'west', west: 'east', north: 'south', south: 'north' }[alt.states.facing] } }] : [alt], tags: object?.tags ?? [] }
    }
    const [x, y, z] = cell.at
    if (!layers.has(y)) layers.set(y, Array.from({ length: ir.depth }, () => Array(ir.width).fill('_')))
    layers.get(y)[z][x] = indexed.get(signature)
  }
  return { name: ir.document.id, title: ir.document.title ?? ir.document.id, description: ir.document.description ?? '', tags: ir.document.tags ?? [], front: ir.document.front ?? 'south', foundation: ir.document.site?.foundation === 'any' ? 'any' : 'flat', clearance: ir.document.site?.clearance ?? 1, params: {}, width: ir.width, depth: ir.depth, legend, layers: [...layers].sort(([a], [b]) => a - b).map(([y, rows]) => ({ y, grid: rows.map(row => row.join('')) })), errors: [], resolved: true, turns: 0, v2: { hash: ir.hash, objects: ir.objects } }
}
