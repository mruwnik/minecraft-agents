// Finite capabilities and global inventory allocation. No crafting is implied by stock.
import prismarineBlock from 'prismarine-block'
import { REGISTRY, fullBlock, itemOf } from './format.mjs'
import { WOODS, WOOD_ROLES, STONES, STONE_ROLES, familyName } from '../build/materials.mjs'
const COLORS = ['white', 'orange', 'magenta', 'light_blue', 'yellow', 'lime', 'pink', 'gray', 'light_gray', 'cyan', 'purple', 'blue', 'brown', 'green', 'red', 'black']
const Block = prismarineBlock('26.1')
const cube = (name, registry) => {
  if (!fullBlock(name, registry)) return false
  const shapes = Block.fromStateId(registry.blocksByName[name].defaultState).shapes
  return shapes.length === 1 && shapes[0].every((n, i) => n === (i < 3 ? 0 : 1))
}
const HAZARD = /^(magma_block|cactus|campfire|soul_campfire|lava|fire|soul_fire|sweet_berry_bush|powder_snow|wither_rose)$/
const GRAVITY = /^(sand|red_sand|gravel|.*_concrete_powder|anvil|chipped_anvil|damaged_anvil|dragon_egg|scaffolding)$/
const INTERACTIVE = /(_door|_trapdoor|_fence_gate|_bed|chest|barrel|furnace|smoker|hopper|dispenser|dropper|anvil|lever|table|loom|stonecutter|grindstone|composter|cauldron|lectern|brewing_stand|enchanting_table|note_block|jukebox|beacon|crafter|chiseled_bookshelf|decorated_pot|brewing_stand|bell)$/
export const materialFamily = name => WOODS.find(wood => WOOD_ROLES.some(role => familyName('wood', wood, role) === name)) ?? STONES.find(stone => STONE_ROLES.some(role => familyName('stone', stone, role) === name)) ?? null
export const materialColor = name => COLORS.find(color => name.startsWith(`${color}_`)) ?? null
export function materialCapabilities (name, registry = REGISTRY) {
  const block = registry.blocksByName[name]
  if (!block) return null
  const kind = /_bed$/.test(name) ? 'bed' : /_fence_gate$/.test(name) ? 'fence_gate' : /_trapdoor$/.test(name) ? 'trapdoor' : /_door$/.test(name) ? 'door' : /_stairs$/.test(name) ? 'stairs' : /_slab$/.test(name) ? 'slab' : /_fence$/.test(name) ? 'fence' : /_pane$/.test(name) ? 'pane' : /^(glass|.*_stained_glass)$/.test(name) ? 'glass_block' : /(_log|_stem|bamboo_block)$/.test(name) ? 'log' : cube(name, registry) ? 'full_cube' : 'exact'
  return { kind, gravity: GRAVITY.test(name), contactHazard: HAZARD.test(name), interactive: INTERACTIVE.test(name), transparent: /glass|pane/.test(name), handOperable: /(_fence_gate|_door|_trapdoor)$/.test(name) && !/^iron_/.test(name), family: materialFamily(name), color: materialColor(name) }
}
const CANDIDATE_CACHE = new WeakMap()
export function materialCandidates (slot, registry = REGISTRY) {
  if (!CANDIDATE_CACHE.has(registry)) CANDIDATE_CACHE.set(registry, new Map())
  const cache = CANDIDATE_CACHE.get(registry), cacheKey = JSON.stringify(slot)
  if (cache.has(cacheKey)) return [...cache.get(cacheKey)]
  const names = slot.candidates ?? Object.keys(registry.blocksByName)
  const result = names.filter(name => {
    const c = materialCapabilities(name, registry)
    return c && (slot.kind === 'exact' || slot.kind === 'full_cube' && cube(name, registry) || c.kind === slot.kind) && (!slot.familyClass || (slot.familyClass === 'wood' ? WOODS.includes(c.family) : STONES.includes(c.family))) && Object.entries(slot.requires ?? {}).every(([trait, wanted]) => c[trait] === wanted)
  }).sort((a, b) => {
    const prefs = slot.preferences ?? []
    const rank = n => prefs.includes(n) ? prefs.indexOf(n) : prefs.length
    return rank(a) - rank(b) || a.localeCompare(b)
  })
  cache.set(cacheKey, result)
  return [...result]
}
export function allocateBlueprintMaterials (ir, { stock = {}, existing = {}, registry = REGISTRY, maxSearch = 10000 } = {}) {
  for (const [name, count] of Object.entries(stock)) if (!Number.isInteger(count) || count < 0) throw new Error(`blueprint stock.${name}: must be a nonnegative integer`)
  const candidates = new Map(), reused = {}, objects = ir.objects
  for (const obj of objects) {
    const names = obj.block ? [obj.block] : materialCandidates(ir.document.materials[obj.material], registry)
    if (!names.length) throw new Error(`blueprint material ${obj.material}: no supported candidates`)
    candidates.set(obj.id, names)
    const observed = existing[obj.id]
    if (observed !== undefined) {
      const accepted = names.includes(observed) || (!obj.block && ir.document.materials[obj.material].acceptExisting?.includes(observed) && materialCandidates({ ...ir.document.materials[obj.material], candidates: [observed] }, registry).includes(observed))
      if (!accepted) throw new Error(`blueprint existing ${obj.id}: ${observed} violates its requirement`)
      reused[obj.id] = observed
    }
  }
  const relations = ir.document.relationships ?? []
  const identity = (name, relation) => relation === 'material' ? name : relation === 'family' ? materialFamily(name) : materialColor(name)
  const groups = relations.filter(r => r.strength === 'required').map(r => {
    const members = objects.filter(o => r.members.includes(o.material))
    if (!members.length) return null
    let values = null
    for (const obj of members) {
      const available = new Set((reused[obj.id] ? [reused[obj.id]] : candidates.get(obj.id)).map(n => identity(n, r.relation)).filter(Boolean))
      values = values === null ? available : new Set([...values].filter(v => available.has(v)))
    }
    if (!values?.size) throw new Error(`blueprint required ${r.relation} relationship conflicts with existing members or capabilities`)
    return { relation: r, members, values: [...values].sort() }
  }).filter(Boolean)
  const preferenceScore = (members, relation, value) => members.reduce((sum, obj) => {
    const names = candidates.get(obj.id), rank = names.findIndex(name => identity(name, relation) === value)
    return sum + (rank < 0 ? names.length : rank)
  }, 0)
  for (const group of groups) group.values.sort((a, b) => preferenceScore(group.members, group.relation.relation, a) - preferenceScore(group.members, group.relation.relation, b) || a.localeCompare(b))
  let searched = 0, result = null
  const attempt = bindings => {
    // Capacitated augmenting paths share one stock ledger across every slot.
    const owners = new Map(), assignments = { ...reused }
    const transforms = objects.filter(o => !reused[o.id] && (o.type === 'farmland' || o.type === 'water'))
    for (const obj of transforms) assignments[obj.id] = obj.type === 'farmland' ? 'farmland' : 'water'
    const pending = objects.filter(o => !reused[o.id] && !transforms.includes(o))
    const choices = obj => candidates.get(obj.id).filter(n => groups.every((g, i) => !g.members.includes(obj) || identity(n, g.relation.relation) === bindings[i]))
    const assignedItem = name => itemOf(name)
    const place = (obj, visitedObjects, visitedItems) => {
      if (visitedObjects.has(obj.id)) return false
      visitedObjects.add(obj.id)
      for (const name of choices(obj)) {
        const item = assignedItem(name)
        if (!item) throw new Error(`blueprint ${obj.id}: resource-transform recipe ${name} requires explicit executor support`)
        if (visitedItems.has(item)) continue
        visitedItems.add(item)
        const held = owners.get(item) ?? []
        if (held.length < (stock[item] ?? 0)) { held.push(obj); owners.set(item, held); assignments[obj.id] = name; return true }
        for (let i = 0; i < held.length; i++) {
          const previous = held[i]
          if (place(previous, new Set(visitedObjects), new Set(visitedItems))) { held[i] = obj; assignments[obj.id] = name; return true }
        }
      }
      return false
    }
    pending.sort((a, b) => choices(a).length - choices(b).length || a.id.localeCompare(b.id))
    for (const obj of pending) if (!place(obj, new Set(), new Set())) return null
    const bill = {}
    for (const obj of pending) { const item = assignedItem(assignments[obj.id]); bill[item] = (bill[item] ?? 0) + 1 }
    return { assignments, reused, bill }
  }
  const search = (i, bindings) => {
    if (++searched > maxSearch) throw new Error('blueprint allocation unresolved: bounded relationship search exhausted')
    if (i === groups.length) { result = attempt(bindings); return Boolean(result) }
    return groups[i].values.some(value => search(i + 1, [...bindings, value]))
  }
  // Preferred uniform groups are tried as complete funded alternatives; if
  // they cannot fit the ledger, preserve feasible mixing rather than refusing.
  const preferred = relations.filter(r => r.strength === 'preferred')
  const originalGroups = groups.length
  for (const relation of preferred) {
    const members = objects.filter(o => relation.members.includes(o.material))
    let values = null
    for (const obj of members) {
      const options = new Set((reused[obj.id] ? [reused[obj.id]] : candidates.get(obj.id)).map(n => identity(n, relation.relation)).filter(Boolean))
      values = values === null ? options : new Set([...values].filter(v => options.has(v)))
    }
    if (values?.size) groups.push({ relation, members, values: [...values].sort((a, b) => preferenceScore(members, relation.relation, a) - preferenceScore(members, relation.relation, b) || a.localeCompare(b)) })
  }
  try { search(0, []) } catch (error) {
    if (!error.message.includes('bounded relationship search exhausted')) throw error
    // A preferred search limit is not a proof that the hard problem fails.
  }
  if (!result && groups.length > originalGroups) {
    groups.splice(originalGroups); searched = 0; search(0, [])
  }
  if (!result) throw new Error('blueprint materials are not funded by declared stock; add compatible items or relax required relationships')
  return { ...result, schemaVersion: 2, sourceHash: ir.hash, searched }
}
