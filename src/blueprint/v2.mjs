import { representativeAssignments } from './palette.mjs'
import { rotateBlueprintPosition } from './transform.mjs'
import { compileBlueprintStructure, concreteBlueprint } from './compiler.mjs'
import { allocateBlueprintMaterials, materialCandidates } from './materials.mjs'
import { readBlueprintSource, loadBlueprintDocuments } from './source.mjs'
import { manifestId, readBlueprintManifest, writeBlueprintManifest } from './manifest.mjs'
import { BLUEPRINT_DIR, planJobs, buildBlueprint, checkBlueprint, supplyOf } from './build.mjs'
import { rotate, turnsFor, DIRS, lint, bill, matchesCell, renderLayer, shortfall, parseNote } from './format.mjs'
import { establishBlueprintOperations, verifyBlueprintGuarantees } from './verify.mjs'
import { workRefusal, mapRefusal } from '../lib.mjs'

const validateArguments = a => {
  const allowed = new Set(['name', 'plan', 'file', 'origin', 'place', 'x', 'y', 'z', 'facing', 'supply', 'clear', 'partial', 'until', 'layer'])
  for (const k of Object.keys(a)) if (!allowed.has(k) && a[k] !== undefined) throw new Error(`blueprint v2 does not accept ${k}=; edit material constraints/preferences in plan=`)
}
const concrete = (ir, assignments, facing) => rotate(concreteBlueprint(ir, assignments), turnsFor(ir.document.front ?? 'south', facing))
export const existingObjects = (ir, at, facing, blockAt, { strictMultipart = true } = {}) => {
  const reused = {}, turns = turnsFor(ir.document.front ?? 'south', facing)
  for (const obj of ir.objects) {
    const [ox, oy, oz] = rotateBlueprintPosition(obj.footprint[0].at, ir.width, ir.depth, turns)
    const name = blockAt(at.x + ox, at.y + oy, at.z + oz)?.name
    const names = obj.block ? [obj.block] : [...materialCandidates(ir.document.materials[obj.material]), ...(ir.document.materials[obj.material].acceptExisting ?? [])]
    if (!names.includes(name)) continue
    const valid = obj.footprint.every(cell => {
      const [x, y, z] = rotateBlueprintPosition(cell.at, ir.width, ir.depth, turns)
      const alts = [{ name, states: cell.states }]
      if (name.endsWith('_fence_gate')) alts.push({ name, states: { ...cell.states, facing: ({ east: 'west', west: 'east', north: 'south', south: 'north' })[cell.states.facing] } })
      const tiny = rotate({ width: 1, depth: 1, front: 'south', layers: [], legend: { A: { alts } } }, turns)
      return matchesCell(blockAt(at.x + x, at.y + y, at.z + z), tiny.legend.A.alts)
    })
    if (valid) reused[obj.id] = name
    else if (strictMultipart && obj.footprint.length > 1) throw new Error(`blueprint ${obj.id}: partial or mismatched multipart object at ${ox},${oy},${oz}; repair/remove it safely before resuming`)
  }
  return reused
}
const stockAt = async (api, a, readOnly = false) => {
  const stock = { ...api.inv() }, supply = supplyOf(api, a.supply)
  if (supply) {
    const pos = api.pos?.()
    if (readOnly && (!pos || Math.hypot(pos.x - supply.x - 0.5, pos.y - supply.y - 0.5, pos.z - supply.z - 0.5) > 3)) throw new Error('blueprint check cannot observe the declared supply from here; approach it before checking (no movement performed)')
    const { items = {} } = await api.act('chest_contents', { x: supply.x, y: supply.y, z: supply.z })
    for (const [name, count] of Object.entries(items)) stock[name] = (stock[name] ?? 0) + count
  }
  return stock
}
const fundedConstruction = (api, a, ir, found, stock) => {
  const planned = planJobs(api, found, { clear: a.clear === true })
  if (planned.site.refusal) throw new Error(planned.site.refusal)
  if (ir.document.site?.removal === 'none' && planned.jobs.some(job => job.do === 'dig')) throw new Error('blueprint site.removal=none forbids the required clearing')
  if ((ir.document.site?.removal ?? 'natural_only') === 'natural_only' && planned.jobs.some(job => job.do === 'dig' && !job.natural)) throw new Error('blueprint site.removal=natural_only forbids removing existing construction')
  if (planned.unreachable.length) throw new Error(`blueprint has ${planned.unreachable.length} unreachable jobs; allocation is not constructible`)
  const construction = {}, total = {}, tools = new Set()
  for (const job of planned.jobs) {
    if (job.item) total[job.item] = job.item.endsWith('_bucket') ? 1 : (total[job.item] ?? 0) + 1
    if (job.tool) tools.add(job.tool)
    for (const cell of job.stand?.scaffoldCells ?? []) construction[`${cell.x},${cell.y},${cell.z}`] = found.bp.params.scaffold ?? 'dirt'
  }
  for (const tool of tools) if (!Object.keys(stock).some(name => stock[name] > 0 && name.endsWith(`_${tool}`))) throw new Error(`blueprint requires a carried or declared-supply ${tool}`)
  for (const item of Object.values(construction)) total[item] = (total[item] ?? 0) + 1
  const deficit = shortfall(total, stock)
  if (Object.keys(deficit).length) throw new Error(`blueprint placement plus scaffolds needs ${Object.entries(deficit).map(([item, count]) => `${item}:${count}`).join(' ')} more; no build started`)
  return { construction, bill: total, tools: [...tools] }
}
const prepareManifest = async (api, a, { write = false, stateDir } = {}) => {
  validateArguments(a)
  const dimension = (await api.act('state', {})).dimension ?? 'overworld'
  const saved = a.place === undefined ? null : api.places().find(p => p.name === a.place)
  const old = saved && readBlueprintManifest(saved.note, stateDir)
  const refusal = saved && (old ? workRefusal(saved, api.me()) : mapRefusal(saved, api.me()))
  if (refusal) throw new Error(refusal)
  const legacy = saved && parseNote(saved.note)
  if (legacy && (!['x', 'y', 'z'].every(k => a[k] === saved[k]) || a.name !== legacy.blueprint || a.facing !== legacy.facing)) {
    throw new Error(`legacy build ${saved.name} has no saved source/allocation; its mark is unchanged. Explicitly review and migrate with name=${legacy.blueprint} x=${saved.x} y=${saved.y} z=${saved.z} facing=${legacy.facing} place=${saved.name}; old material parameters are not v2 stock constraints`)
  }
  if (old) {
    if (old.dimension !== dimension) throw new Error('blueprint resume is in a different world dimension')
    if (old.capabilityVersion !== 'minecraft-26.1-placement-v2-1') throw new Error('blueprint manifest capability version is unsupported')
    if (old.at.x !== saved.x || old.at.y !== saved.y || old.at.z !== saved.z || old.place !== saved.name) throw new Error('blueprint manifest anchor/place differs from shared map')
    if (a.plan !== undefined || a.name !== undefined) {
      const candidate = compileBlueprintStructure(readBlueprintSource(a, BLUEPRINT_DIR).document)
      if (candidate.hash !== old.sourceHash) throw new Error('blueprint source differs from the saved allocation; choose a new place to start a different build')
    }
    if ((a.facing !== undefined && a.facing !== old.facing) || ['x', 'y', 'z'].some(k => a[k] !== undefined && a[k] !== old.at[k])) throw new Error('blueprint resume cannot change anchor or facing')
    const ir = compileBlueprintStructure(old.source)
    existingObjects(ir, old.at, old.facing, api.block)
    if (ir.hash !== old.sourceHash) throw new Error('blueprint manifest source hash is invalid')
    const bp = concrete(ir, old.allocation.assignments, old.facing), found = { bp, at: old.at, hash: old.sourceHash, facing: old.facing, params: {}, saved, resume: true, lint: lint(bp) }
    const manifest = { ...old, ...fundedConstruction(api, a, ir, found, await stockAt(api, a, !write)) }
    if (write) writeBlueprintManifest(manifest, stateDir)
    return { manifest, found }
  }
  if (!['x', 'y', 'z'].every(k => Number.isInteger(a[k]))) throw new Error('blueprint needs integer x= y= z= anchor')
  const source = readBlueprintSource(a, BLUEPRINT_DIR), ir = compileBlueprintStructure(source.document)
  const facing = a.facing ?? ir.document.front ?? 'south'
  if (!DIRS.includes(facing)) throw new Error('blueprint facing must be cardinal')
  const at = { x: a.x, y: a.y, z: a.z }, stock = await stockAt(api, a, !write)
  const allocation = allocateBlueprintMaterials(ir, { stock, existing: existingObjects(ir, at, facing, api.block) })
  const bp = concrete(ir, allocation.assignments, facing)
  const found = { bp, at, hash: ir.hash, facing, params: {}, saved, resume: false, lint: lint(bp) }
  const manifest = { id: manifestId(a.place ?? ir.document.id, at), place: a.place, source: ir.document, origin: source.origin, sourceHash: ir.hash, facing, at, dimension, capabilityVersion: 'minecraft-26.1-placement-v2-1', allocation, ...fundedConstruction(api, a, ir, found, stock), by: api.me() }
  if (write) writeBlueprintManifest(manifest, stateDir)
  return { manifest, found }
}
export async function checkBlueprintV2 (api, a, io = {}) {
  const { manifest, found } = await prepareManifest(api, a, io)
  const result = await checkBlueprint(api, a, { found })
  return { ...result, sourceHash: manifest.sourceHash, allocation: manifest.allocation, bill: manifest.bill, planned: true }
}
export async function buildBlueprintV2 (api, a, io = {}) {
  if (!a.place) throw new Error('blueprint.build needs place= for durable resume')
  const { manifest, found } = await prepareManifest(api, a, { ...io, write: true })
  const result = await buildBlueprint(api, a, { found, note: `bp2:${manifest.id}`, deferFinalMark: true })
  if (!result.left && !result.stopped && !result.night && !result.stuck && !result.missing && !result.unreachable) {
    await establishBlueprintOperations(api, manifest)
    verifyBlueprintGuarantees(api, manifest)
    const saved = api.places().find(p => p.name === a.place)
    if (!saved || String(saved.by ?? '').toLowerCase() === String(api.me()).toLowerCase()) await api.act('mark', { name: a.place, kind: found.bp.tags[0] ?? 'build', ...manifest.at, note: `bp2:${manifest.id}` })
    return { ...result, sourceHash: manifest.sourceHash, initialStateVerified: true }
  }
  return result
}
export function showBlueprintV2 (api, a) {
  validateArguments(a)
  const { document } = readBlueprintSource(a, BLUEPRINT_DIR), ir = compileBlueprintStructure(document)
  const assignments = representativeAssignments(ir)
  const facing = a.facing ?? document.front ?? 'south', bp = concrete(ir, assignments, facing)
  return { text: [`${document.title ?? document.id}: ${ir.width}x${ir.depth}`, 'Illustrative material palette; not allocated from inventory.', `sourceHash=${ir.hash}`, ...bp.layers.filter(l => a.layer === undefined || l.y === a.layer).map(l => renderLayer(bp, l.y))].join('\n'), schemaVersion: 2, sourceHash: ir.hash, palette: 'representative', materials: document.materials, relationships: document.relationships ?? [], bill: bill(bp).total }
}

export function listBlueprintsV2 ({ tag, q } = {}) {
  return loadBlueprintDocuments(BLUEPRINT_DIR).filter(({ name, document }) => (tag === undefined || document.tags?.includes(tag)) && (q === undefined || `${name} ${document.title ?? ''} ${document.description ?? ''}`.toLowerCase().includes(String(q).toLowerCase()))).map(({ name, document }) => `${name} ${document.dimensions.join('x')} tags=${(document.tags ?? []).join(',')} ${document.title ?? name}`).join('\n') || 'no blueprint matches'
}
