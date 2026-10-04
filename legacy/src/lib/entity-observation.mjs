// Decode wire/client observations without deciding whether unknown age is safe.
export const entityUuid = value => typeof value === 'string' && /^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(value)
export function observationMetadata (entity) {
  const value = entity?.metadata
  if (typeof value !== 'string') return value
  try { return JSON.parse(value) } catch { return undefined }
}
export function observedAge (entity) {
  if (typeof entity?.baby === 'boolean') return entity.baby
  const value = observationMetadata(entity)?.[16]
  return typeof value === 'boolean' ? value : undefined
}
