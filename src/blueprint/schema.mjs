// Versioned authored structure. Compilation and allocation are separate from execution.
import { createHash } from 'node:crypto'

export const BLUEPRINT_SCHEMA_VERSION = 2
export const BLUEPRINT_LIMITS = Object.freeze({ bytes: 1024 * 1024, side: 64, height: 52, cells: 16384 })
const object = value => value !== null && typeof value === 'object' && !Array.isArray(value)
const fail = (path, message) => { throw new Error(`blueprint ${path}: ${message}`) }
const fields = (value, allowed, path) => {
  if (!object(value)) fail(path, 'must be an object')
  for (const key of Object.keys(value)) if (!allowed.includes(key)) fail(`${path}.${key}`, 'unsupported field')
}
const canonical = value => Array.isArray(value) ? value.map(canonical) : object(value) ? Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])])) : value
export const canonicalBlueprint = value => JSON.stringify(canonical(value))
export const semanticBlueprintHash = value => {
  const { title, description, metadata, ...semantic } = value
  return createHash('sha256').update(canonicalBlueprint(semantic)).digest('hex')
}
export const MATERIAL_KINDS = ['full_cube', 'log', 'slab', 'stairs', 'door', 'bed', 'fence', 'fence_gate', 'trapdoor', 'pane', 'glass_block', 'exact']
export function validateBlueprintDocument (input) {
  if (Buffer.byteLength(JSON.stringify(input) ?? '') > BLUEPRINT_LIMITS.bytes) fail('$', 'document exceeds 1 MiB')
  fields(input, ['schemaVersion', 'id', 'title', 'description', 'metadata', 'dimensions', 'anchor', 'front', 'tags', 'materials', 'relationships', 'structure', 'site', 'guarantees'], '$')
  if (input.schemaVersion !== 2) fail('schemaVersion', 'must be 2')
  if (!/^[a-z0-9][a-z0-9-]{0,63}$/.test(input.id ?? '')) fail('id', 'must be a lowercase catalog identifier')
  for (const key of ['title', 'description']) if (input[key] !== undefined && typeof input[key] !== 'string') fail(key, 'must be a string')
  if (input.tags !== undefined && (!Array.isArray(input.tags) || !input.tags.every(t => typeof t === 'string'))) fail('tags', 'must be a string array')
  if (!Array.isArray(input.dimensions) || input.dimensions.length !== 3 || !input.dimensions.every((n, i) => Number.isInteger(n) && n >= 1 && n <= (i === 1 ? 52 : 64))) fail('dimensions', 'must be [width,height,depth] within 64x52x64')
  if (input.anchor !== undefined && input.anchor !== 'northwest-floor') fail('anchor', 'only northwest-floor is supported')
  if (!['north', 'east', 'south', 'west'].includes(input.front ?? 'south')) fail('front', 'must be a cardinal direction')
  fields(input.materials ?? {}, Object.keys(input.materials ?? {}), 'materials')
  if (Object.keys(input.materials ?? {}).length > 256) fail('materials', 'at most 256 slots')
  for (const [slot, material] of Object.entries(input.materials ?? {})) {
    if (!/^[a-zA-Z0-9][a-zA-Z0-9_-]{0,127}$/.test(slot) || ['constructor', 'prototype', '__proto__'].includes(slot)) fail(`materials.${slot}`, 'invalid material slot identifier')
    fields(material, ['kind', 'requires', 'candidates', 'preferences', 'familyClass', 'acceptExisting'], `materials.${slot}`)
    if (!MATERIAL_KINDS.includes(material.kind)) fail(`materials.${slot}.kind`, 'unsupported kind')
    if (material.kind === 'exact' && material.candidates === undefined) fail(`materials.${slot}`, 'exact material requires explicit candidates')
    if (material.candidates !== undefined && (!Array.isArray(material.candidates) || !material.candidates.length || !material.candidates.every(n => typeof n === 'string'))) fail(`materials.${slot}.candidates`, 'must be a nonempty block-name array')
    fields(material.requires ?? {}, ['gravity', 'contactHazard', 'interactive', 'transparent', 'handOperable'], `materials.${slot}.requires`)
    for (const [trait, value] of Object.entries(material.requires ?? {})) if (typeof value !== 'boolean') fail(`materials.${slot}.requires.${trait}`, 'must be boolean')
    if (material.familyClass !== undefined && !['wood', 'stone'].includes(material.familyClass)) fail(`materials.${slot}.familyClass`, 'supported family classes: wood, stone')
    if (material.preferences !== undefined && (!Array.isArray(material.preferences) || !material.preferences.every(n => typeof n === 'string'))) fail(`materials.${slot}.preferences`, 'must be a block-name array')
    if (material.acceptExisting !== undefined && (!Array.isArray(material.acceptExisting) || !material.acceptExisting.every(n => typeof n === 'string'))) fail(`materials.${slot}.acceptExisting`, 'must be a block-name array')
  }
  if (!Array.isArray(input.relationships ?? [])) fail('relationships', 'must be an array')
  if ((input.relationships ?? []).length > 32) fail('relationships', 'at most 32 relationships')
  for (const [i, relation] of (input.relationships ?? []).entries()) {
    fields(relation, ['members', 'relation', 'strength'], `relationships[${i}]`)
    if (!Array.isArray(relation.members) || !relation.members.length || relation.members.some(slot => !input.materials?.[slot])) fail(`relationships[${i}].members`, 'must reference material slots')
    if (!['material', 'family', 'color'].includes(relation.relation) || !['required', 'preferred'].includes(relation.strength)) fail(`relationships[${i}]`, 'unsupported relation or strength')
  }
  fields(input.structure, ['legend', 'layers', 'objects', 'spaces'], 'structure')
  fields(input.structure.legend ?? {}, Object.keys(input.structure.legend ?? {}), 'structure.legend')
  if (Object.keys(input.structure.legend ?? {}).some(token => [...token].length !== 1 || ['_', '.'].includes(token))) fail('structure.legend', 'tokens must be single characters; . and _ are reserved')
  if (!Array.isArray(input.structure.layers ?? []) || !Array.isArray(input.structure.objects ?? []) || !Array.isArray(input.structure.spaces ?? [])) fail('structure', 'layers, objects and spaces must be arrays')
  fields(input.site ?? {}, ['foundation', 'clearance', 'existing', 'removal'], 'site')
  if (!['supported', 'flat', 'any'].includes(input.site?.foundation ?? 'supported')) fail('site.foundation', 'unsupported foundation policy')
  if (!['reuse_compatible'].includes(input.site?.existing ?? 'reuse_compatible')) fail('site.existing', 'unsupported existing policy')
  if (!['natural_only', 'none'].includes(input.site?.removal ?? 'natural_only')) fail('site.removal', 'unsupported removal policy')
  if (input.guarantees !== undefined && (!Array.isArray(input.guarantees) || input.guarantees.some(g => !['closed_entrances', 'source_water'].includes(g)))) fail('guarantees', 'supported guarantees: closed_entrances, source_water')
  if (input.site?.clearance !== undefined && (!Number.isInteger(input.site.clearance) || input.site.clearance < 0 || input.site.clearance > 8)) fail('site.clearance', 'must be an integer 0..8')
  return structuredClone(input)
}
