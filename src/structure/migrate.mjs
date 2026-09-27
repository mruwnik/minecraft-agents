import { parsePlan, parsePlacePlan, planCells, planSpec, planBill, migratePlan, hasPlan } from '../lib/plan.mjs'
const sorted = value => Array.isArray(value) ? value.map(sorted) : value && typeof value === 'object' ? Object.fromEntries(Object.keys(value).sort().filter(k => value[k] !== undefined).map(k => [k, sorted(value[k])])) : value
const meaning = spec => {
  const out = { ...spec, literal: Boolean(spec.literal) }
  delete out.ground_offset
  return out
}
const tuple = cell => JSON.stringify(sorted({ at: [cell.x, cell.y, cell.z], spec: meaning(planSpec(cell)) }))
export function migrateSavedPlans (places) {
  if (!Array.isArray(places)) throw new Error('shared map must be an array')
  const reports = []
  const migrated = places.map(place => {
    if (!hasPlan(place)) return structuredClone(place)
    const next = migratePlan(place)
    const legacy = typeof place.plan === 'string' && !place.structure
    let before
    let oldBill
    if (legacy) {
      const parsed = parsePlan(place.plan, place.legend)
      if (parsed.error) throw new Error(`${place.name}: ${parsed.error}`)
      before = parsed.cells.map(c => ({ ...c, x: place.x + c.dx, y: place.y + (planSpec(c)?.kind === 'tree' ? planSpec(c).ground_offset ?? 0 : 0), z: place.z + c.dz }))
      oldBill = planBill(parsed)
    } else {
      before = planCells(place)
      oldBill = planBill(parsePlacePlan(place))
    }
    const after = planCells(next)
    if (JSON.stringify(before.map(tuple).sort()) !== JSON.stringify(after.map(tuple).sort())) throw new Error(`${place.name}: migration changed cell coordinates or maintenance meaning`)
    if (JSON.stringify(sorted(oldBill)) !== JSON.stringify(sorted(planBill(parsePlacePlan(next))))) throw new Error(`${place.name}: migration changed material bill`)
    reports.push({ name: place.name, kind: place.kind, cells: after.length, layers: next.structure.layers.length, migrated: legacy, equivalent: true })
    return next
  })
  return { places: migrated, reports }
}
