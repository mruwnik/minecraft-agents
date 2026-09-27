import { boatPassengerProfile, boatPassengerStatus, entityUuid } from '../../src/lib.mjs'

const point = e => {
  const [x, y, z] = e.exact.split(',').map(Number)
  return { x, y, z }
}
const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)

export default {
  doc: 'boat.board uuid= [id=] [boat= | item= x= y= z= [centerX= centerZ= aimY=]] [timeout=60]: capture one observed adult villager, cow, sheep or pig in an empty boat for leash transport',
  stops: 'the named supported adult is the only boat passenger, or boarding times out',
  args: { uuid: 'string!', id: 'number', boat: 'number', item: 'string', x: 'number', y: 'number', z: 'number', centerX: 'number', centerZ: 'number', aimY: 'number', timeout: 'number' },

  async run (api, a) {
    if (!entityUuid(a.uuid)) throw new Error('uuid= must be the observed passenger UUID')
    const timeout = a.timeout ?? 60
    if (!Number.isFinite(timeout) || timeout < 3 || timeout > 180) throw new Error('timeout= must be 3..180 seconds')
    const entities = (await api.act('entity', { name: '*', count: 100, uuid: true })).found
    const target = entities.find(e => e.uuid === a.uuid)
    if (!target || (a.id !== undefined && target.id !== a.id)) throw new Error(`passenger ${a.uuid} is not observed with the requested ID`)
    const profile = boatPassengerProfile(target)
    if (!profile.ok) throw new Error(profile.error)

    let boatId = a.boat
    if (boatId === undefined) {
      if (![a.x, a.y, a.z].every(Number.isInteger)) throw new Error('new boat needs integer x= y= z= launch cell')
      const centerGiven = a.centerX !== undefined || a.centerZ !== undefined
      if (centerGiven && (![a.centerX, a.centerZ].every(Number.isFinite) || Math.abs(a.centerX - (a.x + 0.5)) > 0.8 || Math.abs(a.centerZ - (a.z + 0.5)) > 0.8)) throw new Error('centerX= and centerZ= must both be numbers within 0.8 block of the launch cell center')
      if (a.aimY !== undefined && (!Number.isFinite(a.aimY) || a.aimY < a.y + 0.5 || a.aimY > a.y + 1)) throw new Error('aimY= must be within the launch water layer')
      if (distance(point(target), a) > 8) throw new Error(`boat launch at ${a.x},${a.y},${a.z} is more than 8 blocks from passenger ${a.uuid}`)
      const placed = await api.act('boat_place', { item: a.item ?? 'oak_boat', x: a.x, y: a.y, z: a.z, ...(centerGiven ? { centerX: a.centerX, centerZ: a.centerZ } : {}), ...(a.aimY !== undefined ? { aimY: a.aimY } : {}) })
      boatId = placed.boat?.id
      if (!Number.isInteger(boatId)) throw new Error('boat placement returned no entity ID; inspect boat_state before retrying')
    }
    const state = async () => api.act('boat_state', { id: boatId })
    let last = await state()
    if (!last.boats?.length) throw new Error(`boat ${boatId} is not in sight`)
    const unwanted = s => s.boats[0].passengers?.some(p => p.uuid !== a.uuid)
    if (unwanted(last)) throw new Error(`boat ${boatId} already carries somebody other than passenger ${a.uuid}`)
    if (last.mounted === boatId) throw new Error(`bot is riding boat ${boatId}; dismount before towing it`)
    api.report({ boat: boatId, passengerUuid: a.uuid, kind: profile.name, boarding: 'waiting' })
    await api.until(async () => {
      last = await state()
      if (!last.boats?.length) throw new Error(`boat ${boatId} disappeared while waiting for passenger ${a.uuid}`)
      return last.boats[0].passengers?.some(p => p.uuid === a.uuid && p.name === profile.name)
    }, { timeout, every: 0.5, what: `passenger ${a.uuid} did not board boat ${boatId}` })
    if (unwanted(last)) throw new Error(`another passenger entered boat ${boatId}`)
    const issue = boatPassengerStatus(last, boatId, a.uuid)
    if (issue) throw new Error(issue)
    api.report({ boarding: 'complete' })
    return { boat: boatId, ...(profile.name === 'villager' ? { villagerUuid: a.uuid } : { passengerUuid: a.uuid, kind: profile.name }), boarded: true }
  }
}
