// Boat interaction, passenger identity and lead/swim transport runtime.
// The body supplies live bindings; importing this module never starts a client.
import { boatPassengerProfile } from '../boat/passenger.mjs'
import { isAir, isBaby, openAbove } from '../lib.mjs'

export function makeBoatRuntime (deps) {
  const { Vec3, vecOf, goNear, findItem, inventoryCounts, pos, columnAbove,
    getSwimStepTarget, setSwimStepTarget } = deps
  let bot, boatLeashHolder
  const sync = () => { bot = deps.getBot(); boatLeashHolder = deps.getBoatLeashHolder() }
  function isBoat (entity) { return /(^|_)boat$/.test(entity?.name ?? '') }
  const carriedBoatCount = () => Object.entries(inventoryCounts()).filter(([name]) => /^\w+_boat$/.test(name)).reduce((n, [, count]) => n + count, 0)
  function boatById (id) {
    const boat = bot.entities[id]
    if (!isBoat(boat)) throw new Error(`no boat with id ${id} is in sight`)
    return boat
  }
  function currentVehicleId (entity) {
    const vehicle = entity?.vehicle
    // Mineflayer may retain this pointer after the server destroys a boat.
    return vehicle && vehicle.isValid && bot.entities[vehicle.id] === vehicle ? vehicle.id : null
  }
  function boatPassengers (boat) {
    const seated = boat.passengers ?? Object.values(bot.entities).filter(e => currentVehicleId(e) === boat.id)
    return seated.map(e => ({ id: e.id, uuid: e.uuid, name: e.name, width: e.width, height: e.height, baby: isBaby(e.metadata), at: e.position.floored().toArray().join(',') }))
  }
  function boatView (boat) {
    const passengers = boatPassengers(boat)
    return { id: boat.id, uuid: boat.uuid, exact: boat.position.toArray().map(n => Math.round(n * 100) / 100).join(','), controller: passengers[0] ? { id: passengers[0].id, uuid: passengers[0].uuid, name: passengers[0].name } : null, passengers, leashHolderId: boatLeashHolder.get(boat.id) ?? null }
  }
  async function reachBoatForUse (boat) {
    // A dock can leave the bot outside a one-block service slot. If already in
    // reach, don't pathfind through its closed wall just to interact.
    const point = boat.position.offset(0, 0.35, 0)
    const eye = () => bot.entity.position.offset(0, bot.entity.eyeHeight, 0)
    const from = eye()
    const distance = from.distanceTo(point)
    if (distance > 3.5) throw new Error(`boat ${boat.id} is ${distance.toFixed(1)} blocks away; move to its service slot`)
    if (bot.world.raycast(from, point.minus(from).normalize(), distance - 0.2)) throw new Error(`solid block obstructs boat ${boat.id} interaction`)
  }
  async function equipBoatBreakingAxe () {
    // A lead or empty hand can leave a boat intact after three attacks. Avoid
    // swords here: their sweep can strike the villager beside the boat.
    const order = ['netherite_axe', 'diamond_axe', 'iron_axe', 'stone_axe', 'golden_axe', 'wooden_axe']
    const axe = order.map(name => bot.inventory.items().find(item => item.name === name)).find(Boolean)
    if (!axe) throw new Error('breaking a boat safely needs an axe; equip one before releasing or recovering it')
    await bot.equip(axe, 'hand')
  }

  const long = {
    async boat_place (a) {
      const item = a.item ?? 'oak_boat'
      if (!/^\w+_(?:chest_)?boat$/.test(item)) throw new Error('boat_place needs a boat item, such as oak_boat or oak_chest_boat')
      const p = vecOf(a)
      const centerGiven = a.centerX !== undefined || a.centerZ !== undefined
      if (centerGiven && (![a.centerX, a.centerZ].every(Number.isFinite) || Math.abs(a.centerX - (p.x + 0.5)) > 0.8 || Math.abs(a.centerZ - (p.z + 0.5)) > 0.8)) throw new Error('boat_place centerX= and centerZ= must both be within 0.8 block of the launch cell center')
      const center = p.offset(centerGiven ? a.centerX - p.x : 0.5, 0.5, centerGiven ? a.centerZ - p.z : 0.5)
      const at = bot.blockAt(p)
      const below = bot.blockAt(p.offset(0, -1, 0))
      const water = at?.name === 'water'
      if (!water && !(isAir(at?.name) && below?.boundingBox === 'block')) throw new Error(`boat_place needs water at ${p} or air over solid ground (found ${at?.name ?? 'unloaded'})`)
      if (a.aimY !== undefined && (!water || !Number.isFinite(a.aimY) || a.aimY < p.y + 0.5 || a.aimY > p.y + 1)) throw new Error('boat_place aimY= needs a water height within the launch cell')
      // For land, aim just inside the support's top surface. A ray aimed half a
      // block above it continues past the requested center before hitting ground.
      const aimY = a.aimY ?? (water ? p.y + 1 : p.y - 0.01)
      if (Object.values(bot.entities).some(e => isBoat(e) && e.position.distanceTo(center) < 2)) throw new Error('a boat already occupies this launch cell')
      await goNear(p, 3)
      await bot.equip(findItem(item), 'hand')
      const before = inventoryCounts()[item] ?? 0
      const beforeIds = new Set(Object.values(bot.entities).filter(isBoat).map(e => e.id))
      // Minecraft places boats with use_item and an eye raycast. Mineflayer's
      // placeEntity omits the modern use_item rotation/sequence and can silently
      // leave the boat in hand, so use its normal item activation packet here.
      await bot.lookAt(new Vec3(center.x, aimY, center.z), true)
      const eye = bot.entity.position.offset(0, bot.entity.eyeHeight, 0)
      const aimedAt = bot.blockAtCursor(5)
      bot.activateItem()
      await bot.waitForTicks(5)
      bot.deactivateItem()
      const candidates = Object.values(bot.entities).filter(e => isBoat(e) && !beforeIds.has(e.id) && e.position.distanceTo(center) < 3)
      const boat = candidates.length === 1 ? candidates[0] : null
      if (!isBoat(boat) || (inventoryCounts()[item] ?? 0) >= before) throw new Error(`boat placement at ${p} was not confirmed: eye=${eye.toArray().map(n => Math.round(n * 100) / 100).join(',')} aim=${center.x},${aimY},${center.z} sight=${aimedAt ? `${aimedAt.name}@${aimedAt.position}` : 'none'} itemBefore=${before} itemNow=${inventoryCounts()[item] ?? 0}; inspect nearby boats before retrying`)
      return { boat: boatView(boat), item, at: `${p.x},${p.y},${p.z}` }
    },
    async boat_mount (a) {
      const boat = boatById(a.id)
      if (bot.vehicle?.id === boat.id) return { mounted: boatView(boat) }
      if (bot.vehicle) throw new Error(`already riding vehicle ${bot.vehicle.id}: dismount first`)
      await goNear(boat.position, 2.5)
      bot.mount(boat)
      for (let i = 0; i < 20 && bot.vehicle?.id !== boat.id; i++) await bot.waitForTicks(1)
      if (bot.vehicle?.id !== boat.id) throw new Error(`boat ${boat.id} did not accept the mount`)
      if (boat.passengers?.[0]?.id !== bot.entity.id) throw new Error(`mounted boat ${boat.id} but another passenger controls it; do not steer`)
      return { mounted: boatView(boat) }
    },
    async boat_dismount (a) {
      if (!bot.vehicle || !isBoat(bot.vehicle)) throw new Error('not riding a boat')
      if (a.id !== undefined && bot.vehicle.id !== a.id) throw new Error(`riding boat ${bot.vehicle.id}, not ${a.id}`)
      const id = bot.vehicle.id
      bot.dismount()
      for (let i = 0; i < 20 && bot.vehicle; i++) await bot.waitForTicks(1)
      if (bot.vehicle) throw new Error(`still riding boat ${id}: dismount was not confirmed`)
      return { dismounted: id, at: pos() }
    },
    async boat_leash (a) {
      const boat = boatById(a.id)
      if (boatLeashHolder.get(boat.id) === bot.entity.id) return { leashed: boatView(boat) }
      if (boatLeashHolder.has(boat.id)) throw new Error(`boat ${boat.id} is leashed to another entity`)
      if (!(inventoryCounts().lead > 0)) throw new Error('boat_leash needs one lead')
      await reachBoatForUse(boat)
      await bot.equip(findItem('lead'), 'hand')
      bot.useOn(boat)
      for (let i = 0; i < 20 && boatLeashHolder.get(boat.id) !== bot.entity.id; i++) await bot.waitForTicks(1)
      if (boatLeashHolder.get(boat.id) !== bot.entity.id) throw new Error(`boat ${boat.id} leash was not confirmed: inspect boat_state before retrying`)
      return { leashed: boatView(boat) }
    },
    async boat_unleash (a) {
      const boat = boatById(a.id)
      if (boatLeashHolder.get(boat.id) !== bot.entity.id) throw new Error(`boat ${boat.id} is not leashed to this body`)
      await reachBoatForUse(boat)
      await bot.unequip('hand')
      bot.useOn(boat)
      for (let i = 0; i < 20 && boatLeashHolder.has(boat.id); i++) await bot.waitForTicks(1)
      if (boatLeashHolder.has(boat.id)) throw new Error(`boat ${boat.id} leash did not detach`)
      return { unleashed: boatView(boat) }
    },
    async boat_release (a) {
      if (!Number.isInteger(a.id) || !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(a.passengerUuid ?? '')) throw new Error('boat_release needs boat id= and passengerUuid=')
      if (bot.vehicle) throw new Error('dismount before releasing the boat passenger')
      const boat = boatById(a.id)
      const seated = boatPassengers(boat)
      if (seated.length !== 1 || seated[0].uuid !== a.passengerUuid) throw new Error(`boat ${boat.id} must contain only passenger ${a.passengerUuid}; seated=${JSON.stringify(seated)}`)
      const passenger = Object.values(bot.entities).find(e => e.uuid === a.passengerUuid)
      if (!passenger) throw new Error(`passenger ${a.passengerUuid} is not visible`)
      const profile = boatPassengerProfile(passenger)
      if (!profile.ok) throw new Error(profile.error)
      if (seated[0].name !== passenger.name || seated[0].name !== profile.name || seated[0].width !== passenger.width || seated[0].height !== passenger.height) throw new Error(`boat ${boat.id} passenger identity or hitbox changed; refresh boat_state before release`)
      const edge = boat.position.floored()
      const dry = [-2, -1, 0, 1, 2].some(dx => [-2, -1, 0, 1, 2].some(dz => [0, 1].some(dy => {
        const ground = bot.blockAt(edge.offset(dx, dy, dz))
        const above = ground && bot.blockAt(ground.position.offset(0, 1, 0))
        return ground?.boundingBox === 'block' && isAir(above?.name) && ground.position.y >= Math.floor(boat.position.y)
      })))
      if (!dry) throw new Error('boat_release needs a dry landing within two blocks of the boat')
      await reachBoatForUse(boat)
      await equipBoatBreakingAxe()
      for (let hit = 0; hit < 3 && bot.entities[boat.id]; hit++) {
        bot.attack(boat)
        await bot.waitForTicks(7)
      }
      if (bot.entities[boat.id]) throw new Error(`boat ${a.id} did not break after three hits; passenger still aboard`)
      for (let i = 0; i < 10 && currentVehicleId(passenger) !== null; i++) await bot.waitForTicks(1)
      if (!passenger.isValid || currentVehicleId(passenger) !== null) throw new Error(`boat broke but passenger ${a.passengerUuid} was not confirmed safely on foot`)
      return { released: a.passengerUuid, passenger: profile.name, onFoot: { uuid: passenger.uuid, exact: passenger.position.toArray().map(n => Math.round(n * 100) / 100).join(','), vehicleId: null }, boatBroken: a.id }
    },
    async boat_recover (a) {
      const boat = boatById(a.id)
      if (boatPassengers(boat).length) throw new Error(`boat ${a.id} still has passengers`)
      if (boatLeashHolder.has(boat.id)) throw new Error(`boat ${a.id} is leashed; detach before recovering it`)
      const at = boat.position.clone()
      const before = carriedBoatCount()
      await reachBoatForUse(boat)
      await equipBoatBreakingAxe()
      for (let hit = 0; hit < 3 && bot.entities[boat.id]; hit++) {
        bot.attack(boat)
        await bot.waitForTicks(7)
      }
      if (bot.entities[boat.id]) throw new Error(`empty boat ${boat.id} did not break after three hits`)
      for (let i = 0; i < 20; i++) {
        const now = carriedBoatCount()
        if (now > before) return { recovered: boat.id, itemCount: now - before, at: pos() }
        await bot.waitForTicks(1)
      }
      return { broken: boat.id, itemPending: at.toArray().map(n => Math.round(n * 100) / 100).join(','), advice: 'boat item did not reach this safe stance; collect it separately' }
    },
  }
  const quick = {
    boat_state (a) {
      const boats = Object.values(bot.entities).filter(isBoat).filter(e => a.id === undefined || e.id === a.id)
        .sort((x, y) => x.position.distanceTo(bot.entity.position) - y.position.distanceTo(bot.entity.position))
        .slice(0, 20).map(boatView)
      return { selfId: bot.entity.id, selfUuid: bot.entity.uuid, mounted: bot.vehicle?.id ?? null, boats }
    },
    async boat_swim (a) {
      if (![a.x, a.y, a.z].every(Number.isFinite)) throw new Error('boat_swim needs finite x= y= z=')
      const ms = a.ms ?? 700
      if (!Number.isInteger(ms) || ms < 100 || ms > 1000) throw new Error('boat_swim ms= must be 100..1000')
      if (getSwimStepTarget()) throw new Error('another boat_swim is still steering')
      const belowFeet = ['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(bot.blockAt(bot.entity.position.offset(0, -0.5, 0))?.name)
      if (!bot.entity.isInWater && !belowFeet) throw new Error('boat_swim needs the bot in or just above water')
      if (!openAbove(columnAbove(bot.entity.position))) throw new Error('boat_swim needs open water overhead')
      const from = bot.entity.position.clone()
      const target = new Vec3(a.x, Math.max(a.y + 1.5, from.y + 1.6), a.z)
      if (Math.hypot(target.x - from.x, target.z - from.z) < 0.5) throw new Error('boat_swim target is too close')
      bot.pathfinder.setGoal(null)
      setSwimStepTarget(target)
      try {
        await bot.lookAt(target, true)
        bot.setControlState('jump', true)
        bot.setControlState('forward', true)
        await new Promise(resolve => setTimeout(resolve, ms))
        return { from: from.toArray().map(n => Math.round(n * 100) / 100).join(','), to: bot.entity.position.toArray().map(n => Math.round(n * 100) / 100).join(','), oxygen: bot.oxygenLevel }
      } finally {
        setSwimStepTarget(null)
        bot.setControlState('forward', false)
        bot.setControlState('jump', false)
      }
    }
  }
  const bind = table => Object.fromEntries(Object.entries(table).map(([name, fn]) => [name, a => { sync(); return fn(a) }]))
  return {
    long: bind(long), quick: bind(quick),
    currentVehicleId: entity => { sync(); return currentVehicleId(entity) },
    attach: () => {
      sync()
      bot._client.on('set_passengers', ({ entityId, passengers }) => {
        const vehicle = bot.entities[entityId]
        if (!vehicle) return
        // The packet replaces the complete passenger list. Mineflayer only
        // updates the listed riders, leaving omitted former riders behind.
        const wanted = new Set(passengers)
        for (const old of vehicle.passengers ?? []) {
          if (!wanted.has(old.id) && old.vehicle === vehicle) old.vehicle = null
        }
        vehicle.passengers = passengers.map(id => bot.entities[id]).filter(Boolean)
        for (const rider of vehicle.passengers) rider.vehicle = vehicle
      })
      bot._client.on('attach_entity', packet => {
        const entity = bot.entities[packet.entityId]
        if (!isBoat(entity)) return
        if (packet.vehicleId <= 0) boatLeashHolder.delete(entity.id)
        else boatLeashHolder.set(entity.id, packet.vehicleId)
      })
    }
  }
}
