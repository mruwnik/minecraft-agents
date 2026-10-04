// The fake's vehicles: `self.vehicle` (an entity id) in the spec puts the body aboard; entities carry `passengers`
// (ids) and `vehicle` (an id) as given. `mount` follows vehicle.mjs's refusals and seats the body within 3 blocks;
// `dismount` puts it at the vehicle's `dismountAt` (else one block +x beside it). `mountFails` / `dismountFails` on the
// spec make them time out. The body's id in passenger lists is FAKE_BODY_ID.
import { mountRefusal, MOUNT_REACH } from './vehicle.mjs'

export const FAKE_BODY_ID = -1

const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const cellKey = ({ x, y, z }) => `${Math.floor(x)},${Math.floor(y)},${Math.floor(z)}`

const describe = e => ({ id: e.id, uuid: e.uuid ?? null, name: e.name ?? null })

export const fakeVehicleOf = s => {
  if (s.self.vehicle == null) return null
  const e = s.entities.find(x => x.id === s.self.vehicle)
  return e ? describe(e) : { id: s.self.vehicle, uuid: null, name: null }
}

const dropRider = (e, id) => {
  const left = (e.passengers ?? []).filter(p => p !== id)
  if (left.length > 0) e.passengers = left
  else delete e.passengers
}

export function fakeMount (s) {
  return async (token, { id } = {}) => {
    if (s.self.vehicle != null) return { status: 'already-mounted', vehicle: fakeVehicleOf(s) }
    const e = s.entities.find(x => x.id === id)
    if (!e) return { status: 'gone' }
    const refused = mountRefusal(e.name, (e.passengers ?? []).length)
    if (refused) return { status: refused }
    if (dist(s.self.pos, e.pos) > MOUNT_REACH) return { status: 'out-of-reach' }
    if (s.mountFails) return { status: 'timeout' }
    s.self.held = null
    s.self.vehicle = e.id
    e.passengers = [...(e.passengers ?? []), FAKE_BODY_ID]
    s.self.pos = { ...e.pos }
    return { status: 'mounted', vehicle: describe(e) }
  }
}

export function fakeDismount (s) {
  return async (token, { yaw } = {}) => {
    if (s.self.vehicle == null) return { status: 'not-mounted' }
    if (s.dismountFails) return { status: 'timeout', mounted: true }
    const e = s.entities.find(x => x.id === s.self.vehicle)
    const from = e?.pos ?? s.self.pos
    const pos = e?.dismountAt ? { ...e.dismountAt } : { x: from.x + 1, y: from.y, z: from.z }
    if (e) dropRider(e, FAKE_BODY_ID)
    s.self.vehicle = null
    s.dismountYaw = yaw ?? null
    s.self.pos = pos
    const here = s.blocks.get(cellKey(pos)) ?? 'air'
    s.self.inWater = here === 'water'
    s.self.inLava = here === 'lava'
    return { status: 'dismounted', pos: { ...pos } }
  }
}
