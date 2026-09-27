import { entityUuid, observationMetadata, observedAge } from '../lib/entity-observation.mjs'
export { entityUuid, observationMetadata, observedAge } from '../lib/entity-observation.mjs'
const PROFESSIONS = 'unemployed armorer butcher cartographer cleric farmer fisherman fletcher leatherworker librarian mason nitwit shepherd toolsmith weaponsmith'.split(' ')
export function villagerObservation (entity) {
  const metadata = observationMetadata(entity)
  const raw = metadata?.[19] ?? metadata?.[18]
  const profession = typeof entity?.metadata === 'string' && metadata === undefined ? 'unknown' : typeof raw?.profession === 'string' ? raw.profession.replace(/^minecraft:/, '') : PROFESSIONS[raw?.villagerProfession ?? raw?.profession ?? raw?.[1] ?? 0] ?? 'unknown'
  const xyz = typeof (entity?.exact ?? entity?.at) === 'string' ? (entity.exact ?? entity.at).split(',').map(Number) : null
  const position = entity?.position ?? (xyz?.length === 3 && xyz.every(Number.isFinite) ? { x: xyz[0], y: xyz[1], z: xyz[2] } : undefined)
  const baby = observedAge(entity)
  return { uuid: entityUuid(entity?.uuid) ? entity.uuid : undefined, position, metadata, baby, profession, level: raw?.level ?? raw?.[2] ?? 1, sleeping: entity?.sleeping === true || metadata?.[6] === 2, nitwit: profession === 'nitwit' }
}
