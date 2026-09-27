import { boatPassengerStatus } from './passenger.mjs'

// Read a fresh exact passenger boat; command-specific terrain and pacing stay with the caller.
export async function observePassengerBoat (api, boatId, uuid, { unmounted = true, lead = 'any' } = {}) {
  if (!['any', 'self', 'self-or-none'].includes(lead)) throw new Error('unknown boat lead observation policy')
  const s = await api.act('boat_state', { id: boatId })
  const issue = boatPassengerStatus(s, boatId, uuid)
  if (issue) throw new Error(issue)
  const boat = s.boats.find(b => b.id === boatId)
  if (unmounted && s.mounted === boatId) throw new Error(`dismount boat ${boatId} before towing it`)
  if (lead === 'self' && boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} needs a lead held by this bot`)
  if (lead === 'self-or-none' && boat.leashHolderId !== null && boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} is leashed to somebody else`)
  return { s, boat }
}
