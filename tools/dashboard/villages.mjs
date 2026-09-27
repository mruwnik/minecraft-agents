import { compileBlueprintStructure } from '../../src/blueprint/compiler.mjs'
import { turnsFor } from '../../src/blueprint/format.mjs'

const locationKey = (name, at) => `${name}\0${at?.x},${at?.y},${at?.z}`

export function attachVillageStatus(places, villages) {
  const byName = new Map(villages.map(v => [v.name, v]))
  const names = new Set(places.map(p => p.name))
  return [
    ...places.map(p => byName.has(p.name) ? { ...p, village: byName.get(p.name) } : p),
    ...villages.filter(v => !names.has(v.name) && [v.x, v.y, v.z].every(Number.isFinite)).map(v => ({ name: v.name, kind: v.kind ?? 'village', x: v.x, y: v.y, z: v.z, by: v.by, village: v }))
  ]
}

// Pure projection for the dashboard. Inputs are saved places/manifests,
// inspection snapshots and the shared UUID roster; this never polls a body.
export function villageViews({ places = [], manifests = [], inspections = [], manifestErrors = [], roster = { villagers: {} }, now = Date.now(), freshFor = 120000 } = {}) {
  const manifestByPlace = new Map(manifests.map(m => [m.place, m]))
  const inspectionsByLocation = new Map(inspections.map(i => [locationKey(i.place, i.at), i]))
  const placeByName = new Map(places.map(p => [p.name, p]))
  const candidates = new Map()
  for (const place of places) {
    const manifest = manifestByPlace.get(place.name)
    const manifestError = manifestErrors.find(e => e.place === place.name)?.error ?? null
    const inspection = manifest
      ? inspectionsByLocation.get(locationKey(place.name, manifest.at))
      : inspections.filter(i => i.place === place.name).sort((a, b) => Date.parse(b.observedAt ?? 0) - Date.parse(a.observedAt ?? 0))[0]
    const source = inspection?.source ?? manifest?.source
    const population = inspection?.population ?? source?.population ?? null
    if (place.kind !== 'village' && !inspection && !population && !manifestError) continue
    candidates.set(locationKey(place.name, manifest?.at ?? place), { place, manifest, inspection, source, population, manifestError })
  }
  // Keep explicit saved inspections visible if their map place was later
  // removed. Their old anchor is still useful as a historical marker.
  for (const inspection of inspections) {
    const key = locationKey(inspection.place, inspection.at)
    if (candidates.has(key) || placeByName.has(inspection.place)) continue
    const place = placeByName.get(inspection.place) ?? { name: inspection.place, kind: 'village', ...inspection.at, by: null }
    const manifest = manifestByPlace.get(inspection.place)
    candidates.set(key, { place, manifest, inspection, source: inspection.source, population: inspection.population ?? inspection.source?.population ?? null, manifestError: null })
  }

  return [...candidates.values()].map(({ place, manifest, inspection, source, population, manifestError }) => {
    const observedAt = inspection?.observedAt ?? null
    const observedMs = observedAt ? Date.parse(observedAt) : NaN
    const fresh = Number.isFinite(observedMs) && observedMs <= now && now - observedMs <= freshFor
    let state = 'not-inspected'
    if (manifestError) state = 'unknown'
    else if (!population) state = 'unplanned'
    else if (inspection && !fresh) state = 'stale'
    else if (fresh) state = inspection.report?.satisfied ? 'satisfied' : inspection.report?.populationStatus === 'violated' ? 'below-target' : 'unknown'

    let bounds = null
    if (source) {
      try {
        const ir = compileBlueprintStructure(source)
        const turns = turnsFor(source.front ?? 'south', manifest?.facing ?? source.front ?? 'south')
        bounds = { x: place.x ?? manifest?.at?.x ?? inspection?.at?.x, y: place.y ?? manifest?.at?.y ?? inspection?.at?.y, z: place.z ?? manifest?.at?.z ?? inspection?.at?.z, width: turns % 2 ? ir.depth : ir.width, depth: turns % 2 ? ir.width : ir.depth, height: ir.height }
      } catch { /* invalid/old blueprints remain point markers */ }
    }

    const report = inspection?.report ?? null
    const roles = (population?.roles ?? []).map(role => {
      const found = report?.roles?.find(r => r.id === role.id)
      return { id: role.id, profession: role.profession, required: role.count ?? 1, observed: fresh ? (found?.uuids?.length ?? null) : null, lastObserved: found?.uuids?.length ?? null, status: fresh ? (found?.status ?? 'unknown') : 'unknown', workstation: role.workstation ?? null, trade: role.trade ?? null, stock: (found?.stock ?? []).map(s => ({ uuid: s.uuid, status: fresh ? s.status : 'unknown', lastStatus: s.status, observedAt: s.observedAt ?? null, restock: s.restock ?? 'not observed' })) }
    })
    const declaredWorkspaces = [...(population?.workspaces ?? []), ...(population?.roles ?? []).filter(r => r.workstation).map(r => ({ id: r.id, at: r.workstation, profession: r.profession, trade: r.trade }))]
    const workspaces = declaredWorkspaces.map(workspace => {
      const found = report?.workspaces?.find(w => w.id === workspace.id)
      return {
        id: workspace.id, localAt: workspace.at, profession: workspace.profession, trade: workspace.trade ?? null,
        at: found?.at ?? null, block: fresh ? (found?.block ?? null) : null,
        status: fresh ? (found?.status ?? 'unknown') : 'unknown', lastStatus: found?.status ?? 'unknown',
        uuids: fresh ? (found?.uuids ?? []) : [], lastUuids: found?.uuids ?? [],
        associations: (found?.associations ?? []).map(a => ({ uuid: a.uuid, status: fresh ? a.status : 'unknown', lastStatus: a.status, reason: a.reason ?? null, source: a.source ?? null, basis: a.basis ?? null, observedAt: a.observedAt ?? null }))
      }
    })
    const explicitMemberIds = [...(report?.assigned ?? []), ...roles.flatMap(r => report?.roles?.find(v => v.id === r.id)?.uuids ?? [])]
    const insideLastSeen = bounds ? Object.values(roster.villagers ?? {}).filter(r => {
      const p = r.lastPosition
      return p && p.x >= bounds.x && p.x < bounds.x + bounds.width && p.z >= bounds.z && p.z < bounds.z + bounds.depth && p.y >= bounds.y - 0.5 && p.y < bounds.y + bounds.height
    }).map(r => r.uuid) : []
    const memberIds = [...new Set([...explicitMemberIds, ...insideLastSeen])]
    const members = memberIds.map(uuid => {
      const record = roster.villagers?.[uuid]
      const seen = record?.lastSeenAt ? Date.parse(record.lastSeenAt) : NaN
      return record ? { uuid, profession: record.profession ?? 'unknown', age: record.age ?? 'unknown', lastSeenAt: record.lastSeenAt ?? null, lastPosition: record.lastPosition ?? null, sightingFresh: Number.isFinite(seen) && seen <= now && now - seen <= freshFor, lockVerifiedAt: record.lockEvidence?.at ?? null, offersObservedAt: record.offers?.observedAt ?? null, offerCount: record.offers?.items?.length ?? null, purchases: record.purchases?.length ?? 0 } : { uuid, profession: 'unknown', age: 'unknown', lastSeenAt: null, lastPosition: null, sightingFresh: false, lockVerifiedAt: null, offersObservedAt: null, offerCount: null, purchases: 0 }
    })
    return {
      name: place.name, kind: place.kind ?? 'village', x: place.x ?? manifest?.at?.x ?? inspection?.at?.x,
      y: place.y ?? manifest?.at?.y ?? inspection?.at?.y, z: place.z ?? manifest?.at?.z ?? inspection?.at?.z,
      by: place.by ?? manifest?.by ?? null, note: place.note ?? '', bounds, blueprintId: source?.id ?? null, population, state, fresh, observedAt, error: manifestError,
      observed: fresh ? (report?.population ?? null) : null,
      lastObservedPopulation: report?.population ?? null,
      unknownResidents: fresh ? (report?.unknown?.length ?? 0) : null,
      surplus: fresh ? (report?.surplus ?? null) : null,
      roles,
      workspaces,
      housing: fresh ? { state: report?.structure?.complete ? 'complete' : report?.structure?.unknown?.length ? 'unknown' : 'incomplete', usableBeds: report?.structure?.usableBeds ?? null, residentBedCapacity: report?.structure?.residentBedCapacity ?? null, requiredBeds: report?.requiredBeds ?? null, residentBeds: report?.structure?.residentBeds ?? [], missingCells: report?.structure?.missing?.length ?? null, unknownCells: report?.structure?.unknown?.length ?? null, shelter: report?.shelter ?? { status: 'unknown', issues: ['shelter proof not recorded'] } } : { state: 'unknown', usableBeds: null, residentBedCapacity: null, requiredBeds: null, residentBeds: [], missingCells: null, unknownCells: null, shelter: { status: 'unknown', issues: [] } },
      restock: 'unknown; trade uses are last observed on merchant windows', members
    }
  }).sort((a, b) => a.name.localeCompare(b.name))
}
