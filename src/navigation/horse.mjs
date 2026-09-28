import { createRequire } from 'node:module'
import { createTerrainGeometry, terrainProfile } from './terrain.mjs'
import { planHorseSteps, driveHorseSteps } from './horse-steps.mjs'

const require = createRequire(import.meta.url)
const { Physics, PlayerState } = require('prismarine-physics')
const { Vec3 } = require('vec3')
const { getAttributeValue } = require('prismarine-physics/lib/attribute')
const HALF = 0.7, HEIGHT = 1.6, RIDER_CLEARANCE = 3.5
const controls = forward => ({ forward, back: false, left: false, right: false, jump: false, sprint: false, sneak: false })
const SEAT = { horse: 1.44375, donkey: 1.1125, mule: 1.2125 }
export class HorseRouteError extends Error {}
export const horseDestination = p => ({ x: p.x + (Number.isInteger(p.x) ? 0.5 : 0), y: p.y, z: p.z + (Number.isInteger(p.z) ? 0.5 : 0) })

// Initial mounted travel deliberately uses a wider, taller corridor than the
// pedestrian planner. No construction, jumping, slopes, swimming or squeezing.
export function checkedHorseRoute (blockAt, from, target, { centerGoal = true } = {}) {
  const to = centerGoal ? horseDestination(target) : target
  if (![from, to].every(p => ['x', 'y', 'z'].every(k => Number.isFinite(p?.[k])))) throw new HorseRouteError('horse route needs finite coordinates')
  const distance = Math.hypot(to.x - from.x, to.z - from.z)
  if (Math.abs(to.y - from.y) > 0.001 || !Number.isInteger(from.y) || distance > 128) throw new HorseRouteError('horse travel currently needs a flat loaded corridor no longer than 128 blocks')
  const cache = new Map()
  const at = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!cache.has(key)) cache.set(key, blockAt(x, y, z))
    return cache.get(key)
  }
  const geometry = createTerrainGeometry(at, { openDoors: false, dry: true, avoidCrops: true })
  const validAt = (x, z, previousX = x, previousZ = z) => {
    const y = from.y
    const lowX = Math.min(x, previousX) - HALF, highX = Math.max(x, previousX) + HALF
    const lowZ = Math.min(z, previousZ) - HALF, highZ = Math.max(z, previousZ) + HALF
    if (!geometry.clearBox([lowX, y, lowZ, highX, y + RIDER_CLEARANCE, highZ])) throw new HorseRouteError('horse and rider need a clear, dry 1.4-wide, 3.5-high corridor')
    for (let bx = Math.floor(lowX); bx <= Math.floor(highX - 1e-7); bx++) for (let bz = Math.floor(lowZ); bz <= Math.floor(highZ - 1e-7); bz++) {
      const floor = terrainProfile(at(bx, y - 1, bz))
      if (!floor.loaded || floor.hazardous || floor.liquid || floor.crop || /ice|slime|honey|soul_sand|farmland|leaves/.test(floor.name ?? '') ||
        !floor.support.some(s => s[0] === 0 && s[1] === 0 && s[2] === 0 && s[3] === 1 && s[4] === 1 && s[5] === 1)) throw new HorseRouteError('horse corridor needs continuous ordinary full-block footing under its whole width')
    }
  }
  // Swept boxes also cover thin corner obstructions between diagonal samples.
  const samples = Math.max(1, Math.ceil(distance / 0.25))
  let previousX = from.x, previousZ = from.z
  for (let i = 0; i <= samples; i++) {
    const x = from.x + (to.x - from.x) * i / samples, z = from.z + (to.z - from.z) * i / samples
    validAt(x, z, previousX, previousZ)
    previousX = x; previousZ = z
  }
  return { from: { ...from }, to, distance, reads: cache.size }
}

// minecraft-data's 1.21.5/26.1 protocol mapper emits generic.movement_speed,
// while its attribute registry calls the SAME attribute minecraft:movement_speed.
// Mineflayer retains the decoded wire key. Resolve only these verified aliases;
// absent values must never become an assumed average horse speed.
export function horseMovementAttribute (bot, entity) {
  const resource = bot.registry?.attributesByName?.movementSpeed?.resource
  const attribute = entity.attributes?.[resource] ?? entity.attributes?.['generic.movement_speed']
  if (!resource || !attribute || !Number.isFinite(attribute.value) || !Array.isArray(attribute.modifiers) ||
      attribute.modifiers.some(m => !Number.isFinite(m.amount) || ![0, 1, 2].includes(m.operation))) return null
  return attribute
}

export function observedHorseSpeed (bot, entity) {
  const attribute = horseMovementAttribute(bot, entity)
  const value = attribute && getAttributeValue(attribute)
  return Number.isFinite(value) ? value : null
}

export function horseSpeed (bot, entity) {
  const value = observedHorseSpeed(bot, entity)
  if (!Number.isFinite(value) || value <= 0 || value > 0.5) throw new HorseRouteError('horse movement_speed attribute must be confirmed and within the supported range')
  return value
}

const physicsAttributes = (bot, entity) => {
  const attribute = horseMovementAttribute(bot, entity)
  return { ...entity.attributes, [bot.registry.attributesByName.movementSpeed.resource]: {
    ...attribute, modifiers: attribute.modifiers.map(modifier => ({ ...modifier }))
  } }
}

function checkPose (bot, entity) {
  const index = bot.registry.entitiesByName[entity.name]?.metadataKeys?.indexOf('flags')
  if (index >= 0 && (entity.metadata?.[index] & 32)) throw new HorseRouteError('wait for the horse to finish rearing before ground travel')
  if (entity.leashHolder) throw new HorseRouteError('release the horse lead before mounted travel')
}

function input (bot, forward) {
  if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: forward ? { forward: true } : {} })
  else bot._client.write('steer_vehicle', { sideways: 0, forward: forward ? 1 : 0, jump: 0 })
}

// Horses are client-simulated vehicles. Unlike minecarts, input packets alone
// do not move them. Every outgoing position below comes from native collision
// physics at 20 Hz; server corrections are authoritative and abort the trip.
export async function driveHorse ({ bot, entity, goal, terrain = 'flat', check, pause, report = () => {}, reportPerformance = () => {} }) {
  if (!['horse', 'donkey', 'mule'].includes(entity.name)) throw new HorseRouteError('unsupported mounted animal')
  const speed = horseSpeed(bot, entity)
  checkPose(bot, entity)
  if (terrain === 'steps') return driveHorseSteps({ bot, entity, goal, speed, attributes: physicsAttributes(bot, entity), check, checkPose: () => {
    checkPose(bot, entity)
    if (horseSpeed(bot, entity) !== speed) throw new HorseRouteError('horse movement speed changed during the checked step leg')
  }, pause, report, reportPerformance })
  if (terrain !== 'flat') throw new HorseRouteError('horse terrain must be flat or steps')
  const blockAt = (x, y, z) => bot.blockAt(new Vec3(x, y, z))
  const validateRoute = (from, target, options, phase) => {
    const began = performance.now()
    try { return checkedHorseRoute(blockAt, from, target, options) } finally { reportPerformance('horse.route', performance.now() - began, { id: entity.id, phase }) }
  }
  const began = performance.now()
  const route = validateRoute(entity.position, goal, undefined, 'departure')
  report({ action: 'ride', status: 'route checked', distance: route.distance, planningMs: performance.now() - began, reads: route.reads })
  const world = { getBlock: p => bot.blockAt(p.floored()) }
  const physics = Physics(bot.registry, world)
  physics.playerHalfWidth = HALF; physics.playerHeight = HEIGHT; physics.stepHeight = 0
  const shadow = { version: bot.version, entity: { ...entity, attributes: physicsAttributes(bot, entity), velocity: entity.velocity?.clone() ?? new Vec3(0, 0, 0), onGround: true, effects: entity.effects ?? {} }, inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0 }
  const state = new PlayerState(shadow, controls(false))
  if (Math.hypot(state.vel.x, state.vel.z) > 0.05 || Math.abs(state.vel.y) > 0.1) throw new HorseRouteError('wait for the horse to stand still before starting a checked ride')
  state.vel.set(0, -physics.gravity * physics.airdrag, 0)
  const yaw = Math.atan2(route.from.x - route.to.x, route.from.z - route.to.z)
  const notchYaw = 180 - yaw * 180 / Math.PI
  state.yaw = yaw; state.pitch = 0
  let correction = null, ticks = 0, settled = 0, braking = route.distance <= 0.45, completed = false
  const riderPose = () => {
    // Vanilla standing equine passenger attachment minus the player's 0.6
    // vehicle attachment. This is client prediction while mounted; the server
    // forcedMove on dismount remains authoritative for resuming foot travel.
    bot.entity.position?.set(entity.position.x, entity.position.y + SEAT[entity.name] - 0.6, entity.position.z)
    bot.entity.yaw = entity.yaw; bot.entity.pitch = 0
  }
  const corrected = packet => {
    correction = packet
    if (['x', 'y', 'z'].every(k => Number.isFinite(packet[k]))) {
      entity.position.set(packet.x, packet.y, packet.z)
      entity.velocity?.set(0, 0, 0)
      if (Number.isFinite(packet.yaw)) entity.yaw = Math.PI - packet.yaw * Math.PI / 180
      riderPose()
      // Vanilla acknowledges the corrected vehicle pose; do not continue the
      // prediction that the server rejected or synthesize a rider teleport.
      bot._client.write('vehicle_move', { ...packet, onGround: true })
    }
  }
  const collidedEntity = (from, to) => Object.values(bot.entities).some(other => {
    if (other === entity || other === bot.entity || other.isValid === false || !other.position || ['item', 'experience_orb', 'arrow'].includes(other.name)) return false
    const width = (other.width ?? 0.6) / 2, height = other.height ?? 1.8, p = other.position
    return p.y < state.pos.y + RIDER_CLEARANCE && p.y + height > state.pos.y &&
      p.x + width > Math.min(from.x, to.x) - HALF && p.x - width < Math.max(from.x, to.x) + HALF &&
      p.z + width > Math.min(from.z, to.z) - HALF && p.z - width < Math.max(from.z, to.z) + HALF
  })
  const sendPose = forward => {
    input(bot, forward)
    bot._client.write('look', { yaw: notchYaw, pitch: 0, onGround: true, flags: { onGround: true, hasHorizontalCollision: false } })
    bot._client.write('vehicle_move', { x: state.pos.x, y: state.pos.y, z: state.pos.z, yaw: notchYaw, pitch: 0, onGround: true })
    entity.position.set(state.pos.x, state.pos.y, state.pos.z)
    entity.velocity?.set(state.vel.x, state.vel.y, state.vel.z)
    entity.yaw = yaw
    riderPose()
  }
  bot._client.on('vehicle_move', corrected)
  try {
    const maxTicks = Math.ceil((route.distance / Math.max(1, speed * 30) + 10) * 20)
    for (; ticks < maxTicks; ticks++) {
      check()
      if (bot.vehicle?.id !== entity.id) throw new HorseRouteError('horse rider identity was lost; ride stopped')
      if (correction) throw new HorseRouteError('server corrected horse movement; inspect the authoritative position before continuing')
      checkPose(bot, entity)
      horseSpeed(bot, entity)
      state.attributes = physicsAttributes(bot, entity)
      const remaining = Math.hypot(route.to.x - state.pos.x, route.to.z - state.pos.z)
      const velocity = Math.hypot(state.vel.x, state.vel.z)
      // Ordinary dry ground friction is 0.6 * 0.91. Coast to the goal rather
      // than snapping the horse onto a destination or reversing at full speed.
      braking ||= remaining <= velocity / (1 - 0.546) + 0.3
      state.control = controls(!braking)
      const previous = state.pos.clone()
      physics.simulatePlayer(state, world)
      if (!state.onGround || Math.abs(state.pos.y - route.from.y) > 0.001 || state.isInWater || state.isInLava || state.isCollidedHorizontally) throw new HorseRouteError('horse physics encountered an unsupported surface or obstruction')
      // Re-read current terrain every tick, including stopping room. A changed
      // corridor must never inherit approval from the initial cached scan.
      const coast = state.pos.offset(-Math.sin(yaw) * Math.max(0.5, Math.hypot(state.vel.x, state.vel.z) / (1 - 0.546)), 0, -Math.cos(yaw) * Math.max(0.5, Math.hypot(state.vel.x, state.vel.z) / (1 - 0.546)))
      validateRoute(previous, { x: coast.x, y: coast.y, z: coast.z }, { centerGoal: false }, 'clearance')
      if (collidedEntity(previous, coast)) throw new HorseRouteError('another entity entered the horse corridor; ride stopped')
      sendPose(!braking)
      settled = braking && Math.hypot(state.vel.x, state.vel.z) < 0.003 ? settled + 1 : 0
      await pause(50)
      if (settled >= 3) {
        check()
        if (correction) throw new HorseRouteError('server corrected horse movement; inspect before continuing')
        if (state.pos.distanceTo(new Vec3(route.to.x, route.to.y, route.to.z)) > 0.8) throw new HorseRouteError('horse stopped short of the checked destination')
        completed = true
        return { arrived: true, mounted: true, at: { x: state.pos.x, y: state.pos.y, z: state.pos.z }, ticks: ticks + 1, distance: route.distance, prediction: 'native mounted physics; no server correction received' }
      }
    }
    throw new HorseRouteError('horse did not reach its checked destination within the movement budget')
  } finally {
    // Cancelled input still leaves physical momentum. Coast under neutral
    // controls for at most 0.75 s, using the same collision engine, instead of
    // leaving a stale velocity that makes every later ride/dismount fail.
    if (!completed && !correction && bot.vehicle?.id === entity.id && entity.isValid !== false) {
      state.pos.set(entity.position.x, entity.position.y, entity.position.z)
      if (entity.velocity) state.vel.set(entity.velocity.x, entity.velocity.y, entity.velocity.z)
      state.control = controls(false)
      for (let coastTick = 0; coastTick < 15 && Math.hypot(state.vel.x, state.vel.z) > 0.003; coastTick++) {
        if (correction || bot.vehicle?.id !== entity.id || entity.isValid === false || bot.health <= 0) break
        try {
          const previous = state.pos.clone()
          physics.simulatePlayer(state, world)
          if (!state.onGround || Math.abs(state.pos.y - route.from.y) > 0.001 || state.isInWater || state.isInLava) break
          validateRoute(previous, state.pos, { centerGoal: false }, 'coasting')
          if (collidedEntity(previous, state.pos)) break
          sendPose(false)
          await pause(50)
        } catch { break }
      }
      if (Math.hypot(entity.velocity?.x ?? 0, entity.velocity?.z ?? 0) > 0.003) report({ action: 'ride', status: 'neutral input; safe coast could not finish, inspect vehicle state before further travel' })
    }
    bot._client.removeListener('vehicle_move', corrected)
    try { input(bot, false) } catch { /* Preserve cancellation or disconnect. */ }
  }
}

driveHorse.validate = ({ bot, entity, goal, terrain = 'flat', check, reportPerformance = () => {} }) => {
  check()
  const speed = horseSpeed(bot, entity)
  checkPose(bot, entity)
  const began = performance.now()
  try {
    if (terrain === 'steps') return planHorseSteps({ bot, entity, goal, speed, attributes: physicsAttributes(bot, entity) }).summary
    if (terrain !== 'flat') throw new HorseRouteError('horse terrain must be flat or steps')
    return checkedHorseRoute((x, y, z) => bot.blockAt(new Vec3(x, y, z)), entity.position, goal)
  } finally { reportPerformance('horse.route', performance.now() - began, { id: entity.id, phase: 'preflight', terrain }) }
}
