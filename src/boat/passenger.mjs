// Only entities whose adult hitbox fits the one-cell house entrance and
// three-cell passage are eligible for the small boat-transfer workflow.
import { entityUuid, observedAge } from '../lib/entity-observation.mjs'
export { entityUuid } from '../lib/entity-observation.mjs'

const PASSENGERS = Object.freeze({
  villager: { label: 'adult villager' },
  cow: { label: 'adult cow' },
  sheep: { label: 'adult sheep' },
  pig: { label: 'adult pig' }
})
export const BOAT_PASSENGER_MAX_WIDTH = 0.95
export const BOAT_PASSENGER_MAX_HEIGHT = 2

export function boatPassengerProfile (entity) {
  if (!entity || !PASSENGERS[entity.name]) return { ok: false, error: `unsupported boat passenger ${entity?.name ?? 'unknown'}; supported adults are villagers, cows, sheep and pigs` }
  const baby = observedAge(entity)
  if (baby !== false) return { ok: false, error: `${entity.name} ${entity.uuid ?? ''} must be positively observed as an adult; babies and unknown age are refused` }
  if (!Number.isFinite(entity.width) || !Number.isFinite(entity.height) || entity.width <= 0 || entity.height <= 0) return { ok: false, error: `${entity.name} ${entity.uuid ?? ''} lacks observed hitbox dimensions` }
  if (entity.width > BOAT_PASSENGER_MAX_WIDTH || entity.height > BOAT_PASSENGER_MAX_HEIGHT) return { ok: false, error: `${entity.name} ${entity.uuid ?? ''} hitbox ${entity.width}x${entity.height} does not fit the one-wide, two-high gate` }
  return { ok: true, name: entity.name, label: PASSENGERS[entity.name].label, baby: false, width: entity.width, height: entity.height }
}

export function boatPassengerStatus (state, boatId, uuid) {
  const boat = state?.boats?.find(b => b.id === boatId)
  if (!boat) return `boat ${boatId} is no longer in sight`
  if (boat.passengers?.length !== 1) return `boat ${boatId} must carry exactly one passenger; observed ${boat.passengers?.length ?? 0}`
  const passenger = boat.passengers?.find(p => p.uuid === uuid)
  if (!passenger) return `passenger ${uuid} is not in boat ${boatId}`
  const profile = boatPassengerProfile(passenger)
  if (!profile.ok) return profile.error
  return null
}
