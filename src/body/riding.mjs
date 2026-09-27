import { createRequire } from 'node:module'
import { CompositeHandBack } from '../composite.mjs'
import { dryTravelExit } from '../navigation/travel.mjs'
import { observedHorseSpeed } from '../navigation/horse.mjs'
import { isNight } from '../lib/sleep.mjs'

const HORSES = new Set(['horse', 'donkey', 'mule'])
const minecraftData = createRequire(import.meta.url)('minecraft-data')
export function horseInventoryAction (bot) {
  const fields = bot.version && minecraftData(bot.version)?.protocol?.play?.toServer?.types?.packet_entity_action?.[1]
  const action = fields?.find(field => field.name === 'actionId')?.type
  // New protocols removed sneak actions from this enum and shifted inventory
  // opening. Use the negotiated mapper name instead of its old numeric slot.
  if (Array.isArray(action) && action[0] === 'mapper') {
    if (!Object.values(action[1].mappings).includes('open_vehicle_inventory')) throw new Error('protocol does not expose horse inventory opening')
    return 'open_vehicle_inventory'
  }
  return 6
}
const point = p => ({ x: p.x, y: p.y, z: p.z })
const distance = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)

// AbstractHorse.DATA_ID_FLAGS: tame=2, saddle=4. Resolve indices from the
// negotiated registry rather than guessing across protocol versions.
// https://github.com/mahtomedi/minecraft/blob/main/src/main/java/net/minecraft/world/entity/animal/horse/AbstractHorse.java
export function horseState (bot, entity) {
  if (!entity || !HORSES.has(entity.name)) return null
  const keys = bot.registry?.entitiesByName?.[entity.name]?.metadataKeys
  const flagsIndex = keys?.indexOf('flags'), babyIndex = keys?.indexOf('baby')
  const flags = flagsIndex >= 0 ? entity.metadata?.[flagsIndex] : undefined
  const baby = babyIndex >= 0 ? entity.metadata?.[babyIndex] : undefined
  const movementSpeed = observedHorseSpeed(bot, entity)
  return { id: entity.id, name: entity.name, at: point(entity.position),
    tamed: Number.isInteger(flags) ? Boolean(flags & 2) : null,
    saddled: Number.isInteger(flags) ? Boolean(flags & 4) : null,
    // Entity metadata is sparse: an adult's false AgeableMob baby value is
    // omitted from the initial server snapshot. A received horse flags byte
    // proves metadata readiness; true baby values are explicitly transmitted.
    // Keep entities without that snapshot unknown rather than assuming adult.
    baby: typeof baby === 'boolean' ? baby : babyIndex >= 0 && Number.isInteger(flags) ? false : null,
    movementSpeed: Number.isFinite(movementSpeed) ? movementSpeed : null,
    mounted: bot.vehicle?.id === entity.id,
    passengers: (entity.passengers ?? []).map(e => e.id) }
}

export function makeRidingRuntime ({ getBot, cancelGuard, allowEntity = () => true, report = () => {}, reportPerformance = () => {}, edibleCarried = () => false, now = () => performance.now(), goNear,
  Vec3, pause = ms => new Promise(resolve => setTimeout(resolve, ms)), driveHorse }) {
  const target = (bot, id, args) => {
    if (!Number.isInteger(id)) throw new Error('choose an explicit horse, donkey or mule id')
    const entity = bot.entities[id]
    if (!entity || entity.isValid === false || !HORSES.has(entity.name)) throw new Error('tame/ride supports only a visible horse, donkey or mule')
    if (!allowEntity(entity, args)) throw new Error('animal access is protected')
    const state = horseState(bot, entity)
    if (state.baby !== false || state.tamed === null) throw new Error('adult status and horse flags must be confirmed by the server')
    return entity
  }
  const watch = (bot, entity, args, { departure = true } = {}) => {
    const alive = cancelGuard()
    const check = () => {
      alive()
      if (departure && (bot.health <= 8 || bot.oxygenLevel <= 5 || bot.food <= 6 && !edibleCarried() || isNight(bot.time?.timeOfDay))) throw new CompositeHandBack('horse action paused for health, oxygen, hunger or night; traveler remains mounted if already seated')
      if (bot.entities[entity.id] !== entity || entity.isValid === false || !allowEntity(entity, args)) throw new Error('horse identity or permitted access changed')
      if (bot.vehicle && bot.vehicle.id !== entity.id) throw new Error('already riding another vehicle')
      if ((entity.passengers ?? []).some(e => e.id !== bot.entity.id)) throw new Error('horse is occupied by another passenger')
      if (bot.vehicle?.id === entity.id && !(entity.passengers ?? []).some(e => e.id === bot.entity.id)) throw new Error('horse passenger identity was not confirmed')
    }
    // Repair Mineflayer stale self vehicle pointer only from the server full list.
    const packet = p => {
      if (p.entityId !== entity.id || p.passengers.includes(bot.entity.id)) return
      entity.passengers = (entity.passengers ?? []).filter(e => p.passengers.includes(e.id))
      if (bot.entity.vehicle === entity) bot.entity.vehicle = null
      if (bot.vehicle === entity) { bot.vehicle = null; bot.emit('dismount', entity) }
    }
    bot._client.on('set_passengers', packet)
    return { check, dispose: () => bot._client.removeListener('set_passengers', packet) }
  }
  const wait = async (check, predicate, ticks = 40) => {
    for (let i = 0; i < ticks; i++) { check(); if (predicate()) return true; await pause(50) }
    check(); return predicate()
  }
  const board = async (bot, entity, check) => {
    check()
    if (bot.vehicle?.id === entity.id) return
    if (distance(bot.entity.position, entity.position) > 3 && goNear) { await goNear(entity, check); check() }
    if (distance(bot.entity.position, entity.position) > 3) throw new Error('walk within three blocks of the animal before mounting')
    bot.pathfinder?.setGoal(null)
    await bot.unequip('hand')
    check()
    bot.mount(entity)
    if (!await wait(check, () => bot.vehicle?.id === entity.id)) throw new Error('horse boarding was not confirmed by the server')
  }
  const neutral = bot => {
    if (!bot.vehicle) return
    try {
      if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: {} })
      else bot._client.write('steer_vehicle', { sideways: 0, forward: 0, jump: 0 })
    } catch { /* Preserve cancellation/disconnect failure. Never dismount here. */ }
  }
  const saddle = async (bot, entity, check) => {
    if (horseState(bot, entity).saddled) return
    if (!horseState(bot, entity).tamed) throw new Error('tame the horse before saddling')
    if (!bot.inventory.items().some(i => i.name === 'saddle')) throw new Error('carry a saddle before riding; check the starter chest')
    await board(bot, entity, check)
    if (bot.currentWindow) throw new Error('close the existing inventory window before saddling')
    let windowId = null
    const opened = p => { if (p.entityId === entity.id) windowId = p.windowId }
    bot._client.on('open_horse_window', opened)
    try {
      bot._client.write('entity_action', { entityId: bot.entity.id, actionId: horseInventoryAction(bot), jumpBoost: 0 })
      if (!await wait(check, () => windowId !== null && bot.currentWindow?.id === windowId)) throw new Error('horse inventory was not confirmed')
      const window = bot.currentWindow
      const slot = window.slots.findIndex((item, i) => i >= window.inventoryStart && item?.name === 'saddle')
      if (slot < 0) throw new Error('server horse inventory contains no carried saddle')
      if (window.slots[0]) throw new Error('horse saddle slot is occupied; inspect before replacing equipment')
      await bot.transfer({ window, itemType: window.slots[slot].type, metadata: null, count: 1,
        sourceStart: window.inventoryStart, sourceEnd: window.inventoryEnd, destStart: 0, destEnd: 1 })
      check()
      if (!await wait(check, () => horseState(bot, entity).saddled)) throw new Error('saddle transfer was not confirmed by horse metadata')
    } finally {
      bot._client.removeListener('open_horse_window', opened)
      if (windowId !== null && bot.currentWindow?.id === windowId) bot.closeWindow(bot.currentWindow)
    }
  }
  return {
    quick: { horse_state: a => {
      const bot = getBot()
      if (a.id !== undefined) return { goalTravel: Boolean(driveHorse), mounted: bot.vehicle?.id ?? null, horse: horseState(bot, bot.entities[a.id]) }
      return { goalTravel: Boolean(driveHorse), mounted: bot.vehicle?.id ?? null, horses: Object.values(bot.entities).filter(e => HORSES.has(e.name) && e.isValid !== false && distance(bot.entity.position, e.position) <= 32).sort((a, b) => distance(bot.entity.position, a.position) - distance(bot.entity.position, b.position)).slice(0, 16).map(e => horseState(bot, e)) }
    } },
    long: {
      async tame (a) {
        const bot = getBot(), entity = target(bot, a.id, a), guard = watch(bot, entity, a)
        const attempts = a.attempts ?? 12, seconds = a.seconds ?? 120
        if (!Number.isInteger(attempts) || attempts < 1 || attempts > 30 || !Number.isFinite(seconds) || seconds < 1 || seconds > 300) { guard.dispose(); throw new Error('tame attempts must be 1..30 and seconds 1..300') }
        const deadline = now() + seconds * 1000
        let tried = 0
        try {
          guard.check()
          if (horseState(bot, entity).tamed) return { ...horseState(bot, entity), attempts: 0 }
          while (tried < attempts && now() < deadline) {
            await board(bot, entity, guard.check)
            tried++
            report({ action: 'tame', id: entity.id, attempt: tried, status: 'mounted; waiting for tame flag or bucking' })
            while (bot.vehicle?.id === entity.id && !horseState(bot, entity).tamed && now() < deadline) {
              guard.check(); await pause(100)
            }
            guard.check()
            if (horseState(bot, entity).tamed) return { ...horseState(bot, entity), attempts: tried }
            report({ action: 'tame', id: entity.id, attempt: tried, status: bot.vehicle ? 'still untamed' : 'bucked; retrying' })
            if (!bot.vehicle) { await pause(250) }
          }
          return { ...horseState(bot, entity), attempts: tried, complete: false, reason: 'taming budget exhausted; inspect horse_state before continuing' }
        } finally { neutral(bot); guard.dispose() }
      },
      async horse_saddle (a) {
        const bot = getBot(), entity = target(bot, a.id, a), guard = watch(bot, entity, a)
        try { guard.check(); await saddle(bot, entity, guard.check); return horseState(bot, entity) } finally { neutral(bot); guard.dispose() }
      },
      async horse_dismount (a) {
        const bot = getBot(), id = a.id ?? bot.vehicle?.id
        const entity = target(bot, id, a), guard = watch(bot, entity, a, { departure: false })
        if (bot.vehicle?.id !== id) { guard.dispose(); throw new Error('not riding the requested horse') }
        const blockAt = (x, y, z) => {
          const block = bot.blockAt(Vec3 ? new Vec3(x, y, z) : { x, y, z })
          return block && { name: block.name, solid: block.boundingBox === 'block', properties: block.getProperties?.() ?? {} }
        }
        let requested = false, landing = null
        const positioned = () => { if (requested) landing = point(bot.entity.position) }
        bot.on('forcedMove', positioned)
        try {
          guard.check(); neutral(bot)
          const before = point(entity.position)
          await pause(250); guard.check()
          if (distance(before, entity.position) > 0.05 || Math.hypot(entity.velocity?.x ?? 0, entity.velocity?.y ?? 0, entity.velocity?.z ?? 0) > 0.08) throw new Error('wait until the horse is stationary before dismounting')
          const feet = { x: Math.floor(entity.position.x), y: Math.floor(entity.position.y), z: Math.floor(entity.position.z) }
          const safe = [[1,0],[-1,0],[0,1],[0,-1],[1,1],[-1,-1],[1,-1],[-1,1]].some(([dx,dz]) => dryTravelExit(blockAt, { x: feet.x + dx, y: feet.y, z: feet.z + dz }))
          if (!safe) throw new Error('no checked dry landing beside the horse; remain mounted')
          requested = true
          if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: { shift: true } })
          else bot.dismount()
          if (!await wait(guard.check, () => !bot.vehicle && landing !== null)) throw new Error('horse dismount needs both server passenger removal and server landing position')
          const actual = Object.fromEntries(['x','y','z'].map(k => [k, Math.floor(landing[k])]))
          if (!dryTravelExit(blockAt, actual) || distance(landing, entity.position) > 4) throw new Error('server dismount landing is outside the checked dry area; inspect before walking')
          return { dismounted: id, at: landing }
        } finally {
          bot.removeListener('forcedMove', positioned)
          if (requested && bot.supportFeature?.('newPlayerInputPacket')) { try { bot._client.write('player_input', { inputs: {} }) } catch {} }
          else neutral(bot)
          guard.dispose()
        }
      },
      async ride (a) {
        const hasGoal = ['x', 'y', 'z'].some(k => a[k] !== undefined)
        if (hasGoal && (!['x', 'y', 'z'].every(k => Number.isFinite(a[k])) || !driveHorse)) throw new Error('horse goal travel needs a verified horse physics controller; ride id= boards a saddled horse without moving it')
        const bot = getBot(), entity = target(bot, a.id, a), guard = watch(bot, entity, a)
        try {
          guard.check()
          if (!horseState(bot, entity).tamed) throw new Error('tame the horse before riding')
          if (hasGoal && driveHorse.validate) await driveHorse.validate({ bot, entity, goal: point(a), check: guard.check, pause, report, reportPerformance })
          await saddle(bot, entity, guard.check)
          await board(bot, entity, guard.check)
          const trip = hasGoal ? await driveHorse({ bot, entity, goal: point(a), check: guard.check, pause, report, reportPerformance }) : null
          return { ...horseState(bot, entity), ...(trip ? { trip } : {}) }
        } finally { neutral(bot); guard.dispose() }
      }
    }
  }
}
