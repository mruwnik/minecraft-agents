import board from './board.mjs'
import { inAnyZone, villagerBoatRoute, villagerDockPlan, workRefusal } from '../../src/lib.mjs'
import { boatPassengerProfile, boatPassengerStatus, entityUuid } from '../../src/boat/passenger.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const point = e => {
  const [x, y, z] = e.exact.split(',').map(Number)
  return { x, y, z }
}
const inside = (plan, p) => Math.abs(p.x - (plan.cell.x + 0.5)) < 1.5 && Math.abs(p.z - (plan.cell.z + 0.5)) < 1.5

export default {
  doc: 'boat.undock uuid= x= y= z= riverX= riverZ= [boat=] [item=oak_boat] [centerX= centerZ= aimY=] [block=cobblestone] [timeout=90]: reboard the same supported adult villager, cow or sheep inside a closed boat dock, leash its boat, and open the river gate; pigs are refused because they fit through the temporary one-high service opening',
  stops: 'the exact supported passenger is seated in a leashed boat with the dock gate open, or it remains inside the closed dock',
  args: { uuid: 'string!', x: 'number!', y: 'number!', z: 'number!', riverX: 'number!', riverZ: 'number!', boat: 'number', item: 'string', centerX: 'number', centerZ: 'number', aimY: 'number', block: 'string', timeout: 'number' },

  async run (api, a) {
    if (!entityUuid(a.uuid)) throw new Error('uuid= must be the observed passenger UUID')
    if (![a.x, a.y, a.z, a.riverX, a.riverZ].every(Number.isInteger)) throw new Error('dock coordinates must be integers')
    const plan = villagerDockPlan({ x: a.x, y: a.y, z: a.z }, { x: a.riverX, z: a.riverZ })
    const block = a.block ?? 'cobblestone'
    const item = a.item ?? 'oak_boat'
    const centerGiven = a.centerX !== undefined || a.centerZ !== undefined
    if (centerGiven && (![a.centerX, a.centerZ].every(Number.isFinite) || Math.abs(a.centerX - (a.x + 0.5)) > 0.8 || Math.abs(a.centerZ - (a.z + 0.5)) > 0.8)) throw new Error('centerX= and centerZ= must both be numbers within 0.8 block of the dock cell center')
    if (centerGiven && a.boat !== undefined) throw new Error('centerX= centerZ= apply only when placing a new boat')
    if (a.aimY !== undefined && (!Number.isFinite(a.aimY) || a.aimY < a.y - 0.5 || a.aimY > a.y)) throw new Error('aimY= must be within the dock launch water layer')
    if (a.aimY !== undefined && a.boat !== undefined) throw new Error('aimY= applies only when placing a new boat')
    if (a.boat === undefined) {
      const launch = { x: centerGiven ? a.centerX : a.x + 0.5, y: a.y - 1, z: centerGiven ? a.centerZ : a.z + 0.5 }
      const hull = villagerBoatRoute({ from: { ...launch, y: launch.y + 0.5 }, to: launch, blockAt: (x, y, z) => api.block(x, y, z), margin: 0 })
      if (hull.error) throw new Error(`dock boat launch blocked at ${hull.at.x},${hull.at.y},${hull.at.z}: ${hull.error}`)
    }
    // The one-high service slot may already be open after an interrupted
    // attempt. Its solid sill and the wall above still contain an adult.
    const allClosed = [...plan.walls, ...plan.gate.filter(p => key(p) !== key(plan.service))]
    const missing = allClosed.find(p => !api.block(p.x, p.y, p.z)?.solid)
    if (missing) throw new Error(`dock is not closed at ${key(missing)}; secure it before boarding`)
    const slot = api.block(plan.service.x, plan.service.y, plan.service.z)
    if (!slot || (!slot.solid && !['air', 'water', 'cave_air'].includes(slot.name))) throw new Error(`dock service slot at ${key(plan.service)} is obstructed or unloaded`)
    if (!api.block(plan.serviceFoundation.x, plan.serviceFoundation.y, plan.serviceFoundation.z)?.solid) throw new Error(`dock service sill is missing at ${key(plan.serviceFoundation)}`)
    const stand = plan.serviceStand
    if (!api.block(stand.x, stand.y - 1, stand.z)?.solid || api.block(stand.x, stand.y, stand.z)?.name !== 'water' || api.block(stand.x, stand.y + 1, stand.z)?.solid) throw new Error(`dock service stand at ${key(stand)} needs shallow water over a solid floor with head clearance`)
    const foreign = api.zones().find(z => [...plan.gate, plan.service].some(p => inAnyZone([z], p)) && !new RegExp(`^(${api.me().toLowerCase()}|starter)-`).test(z.name.toLowerCase()))
    if (foreign) throw new Error(`dock crosses protected zone ${foreign.name}`)
    const owner = api.places().filter(p => Math.hypot(p.x - a.x, p.z - a.z) <= (p.radius ?? 8) + 3).map(p => workRefusal(p, api.me())).find(Boolean)
    if (owner) throw new Error(owner)
    if (a.boat === undefined && (api.inv()[item] ?? 0) < 1) throw new Error(`undocking needs an empty ${item}`)
    if ((api.inv().lead ?? 0) < 1) throw new Error('undocking needs one lead before the gate opens')
    if ((api.inv()[block] ?? 0) < 5) throw new Error(`undocking needs five spare ${block} to reseal the gate if interrupted`)
    const entities = (await api.act('entity', { name: '*', count: 100, uuid: true })).found
    const target = entities.find(e => e.uuid === a.uuid)
    if (!target || !inside(plan, point(target))) throw new Error(`passenger ${a.uuid} is not inside the closed dock`)
    const profile = boatPassengerProfile(target)
    if (!profile.ok) throw new Error(profile.error)
    if (profile.height <= 1) throw new Error('this undock path opens a one-high service slot; a passenger that fits that opening could escape')
    if (target.vehicleId != null && a.boat === undefined) throw new Error(`passenger ${a.uuid} is already riding vehicle ${target.vehicleId}; pass boat= to resume`)
    const other = entities.find(e => e.uuid !== a.uuid && e.uuid && !['item', 'experience_orb'].includes(e.name) && !/(^|_)boat$/.test(e.name ?? '') && inside(plan, point(e)))
    if (other) throw new Error(`dock holds another entity ${other.uuid ?? other.id}`)

    let boatId = a.boat ?? null
    const check = async () => {
      const s = await api.act('boat_state', { id: boatId })
      const issue = boatPassengerStatus(s, boatId, a.uuid)
      if (issue) throw new Error(issue)
      const boat = s.boats.find(b => b.id === boatId)
      if (boat.passengers.length !== 1) throw new Error(`boat ${boatId} carries another passenger`)
      return { s, boat }
    }
    // Open the one-high slot before placing the boat: the modern use-item
    // eye ray otherwise hits the closed gate and never reaches the launch
    // water. Its solid sill and overhead wall still contain the villager.
    let openedService = false
    try {
      await api.act('goto', { ...plan.serviceStand, range: 0 })
      if (api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) {
        await api.act('dig', plan.service)
        openedService = true
      }
      if (api.block(plan.service.x, plan.service.y, plan.service.z)?.solid) throw new Error(`dock service slot did not open at ${key(plan.service)}`)
      const boarded = await board.run(api, {
        uuid: a.uuid,
        ...(a.boat === undefined ? { item, x: a.x, y: a.y - 1, z: a.z, ...(centerGiven ? { centerX: a.centerX, centerZ: a.centerZ } : {}), ...(a.aimY !== undefined ? { aimY: a.aimY } : {}) } : { boat: a.boat }),
        timeout: a.timeout ?? 90
      })
      boatId = boarded.boat
      let { s, boat } = await check()
      if (boat.leashHolderId !== s.selfId) await api.act('boat_leash', { id: boatId })
      ;({ s, boat } = await check())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} was not leashed to this bot`)
      for (const p of [...plan.gate].filter(p => key(p) !== key(plan.service)).sort((a, b) => b.y - a.y)) {
        ;({ s, boat } = await check())
        if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} lost its lead while opening the gate`)
        if (api.block(p.x, p.y, p.z)?.solid) await api.act('dig', p)
      }
      ;({ s, boat } = await check())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} lost its lead before the water channel opened`)
      // Removing this last underwater sill gives the 1.375-wide boat its
      // complete two-cell channel only after the same villager is aboard.
      await api.act('dig', plan.serviceFoundation)
      const stuck = [...plan.gate, plan.serviceFoundation].find(p => api.block(p.x, p.y, p.z)?.solid)
      if (stuck) throw new Error(`dock gate did not open at ${key(stuck)}`)
      ;({ s, boat } = await check())
      if (boat.leashHolderId !== s.selfId) throw new Error(`boat ${boatId} lost its lead while opening the gate`)
      const identity = profile.name === 'villager' ? { villagerUuid: a.uuid } : { passengerUuid: a.uuid, kind: profile.name }
      api.report({ dock: 'open', boat: boatId, ...identity })
      return { boat: boatId, ...identity, dock: key(plan.cell), passengerSeated: true, leashHeld: true, gateOpen: true, secure: false }
    } catch (error) {
      const closure = [plan.serviceFoundation, ...plan.gate].filter(p => !api.block(p.x, p.y, p.z)?.solid)
      for (const p of closure) {
        try { await api.act('place', { ...p, item: block }) } catch (_) {}
      }
      const pending = [plan.serviceFoundation, ...plan.gate].filter(p => !api.block(p.x, p.y, p.z)?.solid).map(key)
      api.report({ dock: 'undock pending', boat: boatId, ...(profile.name === 'villager' ? { villagerUuid: a.uuid } : { passengerUuid: a.uuid, kind: profile.name }), serviceOpen: openedService, closurePending: pending.join(' '), error: error.message })
      if (pending.length) throw new Error(`${error.message}; dock closure pending at ${pending.join(' ')}`)
      throw error
    }
  }
}
