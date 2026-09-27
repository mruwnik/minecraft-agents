import { createRequire } from 'node:module'
import { createTerrainGeometry } from './terrain.mjs'
const require = createRequire(import.meta.url)
const { Vec3 } = require('vec3')
export class BoatRouteError extends Error {}
const HALF = 1.375 / 2, HEIGHT = 0.5625, DRAG = Math.fround(0.9), GRAVITY = 0.04
const WATER_HEIGHT = Math.fround(8 / 9), DEG = Math.PI / 180
const point = p => ({ x: p.x, y: p.y, z: p.z })
const properties = b => b?.properties ?? b?.getProperties?.() ?? {}
const sourceWater = b => b?.name === 'water' && Number(properties(b).level) === 0
const wrap = angle => ((angle + 180) % 360 + 360) % 360 - 180

// Deliberately bounded initial boat support: ordinary hulls on a continuous,
// level source-water surface. Flow, bubbles, land, ice and rafts have different
// physics and must not be accepted by this water-only integrator.
export function checkedBoatRoute (blockAt, from, to) {
  if (![from, to].every(p => ['x', 'y', 'z'].every(k => Number.isFinite(p?.[k])))) throw new BoatRouteError('boat route needs finite coordinates')
  const distance = Math.hypot(to.x - from.x, to.z - from.z), waterY = Math.floor(from.y)
  if (distance > 128 || Math.floor(to.y) !== waterY || Math.abs(to.y - from.y) > 0.5) throw new BoatRouteError('boat route needs one loaded level water surface and at most 128 blocks per leg')
  const cache = new Map(), at = (x, y, z) => {
    const key = `${x},${y},${z}`
    if (!cache.has(key)) cache.set(key, blockAt(x, y, z))
    return cache.get(key)
  }
  const geometry = createTerrainGeometry(at, { openDoors: false, dry: false, avoidCrops: true })
  const samples = Math.max(1, Math.ceil(distance / 0.25))
  let previous = from
  for (let i = 0; i <= samples; i++) {
    const p = { x: from.x + (to.x - from.x) * i / samples, y: from.y + (to.y - from.y) * i / samples, z: from.z + (to.z - from.z) * i / samples }
    const box = [Math.min(p.x, previous.x) - HALF, Math.min(p.y, previous.y), Math.min(p.z, previous.z) - HALF,
      Math.max(p.x, previous.x) + HALF, Math.max(p.y, previous.y) + 2.2, Math.max(p.z, previous.z) + HALF]
    if (p.y < waterY + 0.15 || p.y >= waterY + WATER_HEIGHT || !geometry.clearBox(box)) throw new BoatRouteError('boat and rider need a clear loaded 1.375-wide water corridor')
    for (let x = Math.floor(box[0]); x <= Math.floor(box[3] - 1e-7); x++) for (let z = Math.floor(box[2]); z <= Math.floor(box[5] - 1e-7); z++) {
      if (!sourceWater(at(x, waterY, z)) || geometry.get(x, waterY + 1, z).liquid) throw new BoatRouteError('the whole boat hull needs level source water without currents or submerged passage')
    }
    previous = p
  }
  return { from: point(from), to: point(to), distance, waterY, waterLevel: waterY + WATER_HEIGHT, reads: cache.size }
}

export function boatSurface (blockAt, x, z, nearY) {
  if (![x, z, nearY].every(Number.isFinite)) return null
  x = Math.floor(x); z = Math.floor(z)
  for (const dy of [0, -1, 1, -2, 2]) {
    const waterY = Math.floor(nearY) + dy
    if (!sourceWater(blockAt(x, waterY, z))) continue
    const p = { x: x + 0.5, y: waterY + WATER_HEIGHT - HEIGHT * 0.65, z: z + 0.5 }
    try { checkedBoatRoute(blockAt, p, p); return p } catch (error) { if (!(error instanceof BoatRouteError)) throw error }
  }
  return null
}

// AbstractBoat.floatBoat/controlBoat, IN_WATER branch (local Paper 26.2
// bytecode): .9 horizontal/rotation drag; .04 gravity; buoyancy depth/height
// times gravity/.65 then .75 vertical damping; forward .04, turn-only .005.
// This step is valid only after the route checker excludes other statuses.
export function stepBoatWater (state, input, waterLevel) {
  const s = { ...state }
  s.vx *= DRAG; s.vz *= DRAG; s.rotation = Math.fround(s.rotation * DRAG)
  s.vy -= GRAVITY
  const submerged = (waterLevel - s.y) / HEIGHT
  if (submerged > 0) s.vy = (s.vy + submerged * GRAVITY / 0.65) * 0.75
  if (input.left) s.rotation = Math.fround(s.rotation - 1)
  if (input.right) s.rotation = Math.fround(s.rotation + 1)
  s.yaw = Math.fround(s.yaw + s.rotation)
  let acceleration = 0
  if (Boolean(input.left) !== Boolean(input.right) && !input.forward && !input.back) acceleration = Math.fround(0.005)
  if (input.forward) acceleration = Math.fround(acceleration + Math.fround(0.04))
  if (input.back) acceleration = Math.fround(acceleration - Math.fround(0.005))
  s.vx += Math.sin(-s.yaw * DEG) * acceleration
  s.vz += Math.cos(s.yaw * DEG) * acceleration
  s.x += s.vx; s.y += s.vy; s.z += s.vz
  return s
}

const still = state => Math.hypot(state.vx, state.vz) < 0.003 && Math.abs(state.rotation) < 0.1 && Math.abs(state.vy) < 0.01
function steering (state, target) {
  const desired = Math.atan2(state.x - target.x, target.z - state.z) / DEG
  const error = wrap(desired - state.yaw)
  const predicted = error - state.rotation * DRAG / (1 - DRAG)
  return { left: predicted < -1.5, right: predicted > 1.5, forward: Math.abs(error) < 5 && Math.abs(state.rotation) < 1 }
}
const supportedBoat = entity => /(?:^boat$|_boat$)/.test(entity?.name ?? '') && !/chest|bamboo/.test(entity.name) &&
  Math.abs((entity.width ?? 1.375) - 1.375) < 0.001 && Math.abs((entity.height ?? HEIGHT) - HEIGHT) < 0.001
function initialState (entity) {
  if (!supportedBoat(entity) || !Number.isFinite(entity.yaw)) throw new BoatRouteError('self-travel currently supports ordinary wooden boats with a known heading, not chest boats or rafts')
  const v = entity.velocity ?? { x: 0, y: 0, z: 0 }
  if (![v.x, v.y, v.z].every(Number.isFinite) || Math.hypot(v.x, v.z) > 0.03 || Math.abs(v.y) > 0.08) throw new BoatRouteError('wait for the boat to settle before starting a checked leg')
  return { ...point(entity.position), vx: v.x, vy: v.y, vz: v.z, yaw: Math.fround(180 - entity.yaw / DEG), rotation: 0 }
}

export function planBoatLeg (blockAt, entity, goal) {
  const start = initialState(entity), route = checkedBoatRoute(blockAt, start, goal)
  let state = start, braking = route.distance <= 0.04, settled = 0, precision = false
  const steps = [], limit = Math.ceil(route.distance / 0.15 + 500)
  for (let tick = 0; tick < limit; tick++) {
    const remaining = Math.hypot(goal.x - state.x, goal.z - state.z)
    const speed = Math.hypot(state.vx, state.vz)
    const stopping = speed * DRAG / (1 - DRAG)
    braking ||= remaining <= stopping + 0.025
    precision ||= remaining < 2 && speed < 0.06
    let input = braking ? {} : steering(state, goal)
    if (!braking && remaining <= stopping + 1.25 && speed >= 0.06) input = {}
    else if (!braking && precision) {
      // One paddle supplies vanilla's smaller .005 acceleration. Alternate
      // corrective strokes near the goal instead of a .04 full-thrust pulse
      // whose eventual coasting distance advances in roughly .4m increments.
      input.forward = false
      if (!input.left && !input.right) { input.left = state.rotation >= 0; input.right = !input.left }
    }
    const next = stepBoatWater(state, input, route.waterLevel)
    checkedBoatRoute(blockAt, state, next)
    // Reserve the complete neutral stopping path before applying any thrust.
    const coast = { x: next.x + next.vx * DRAG / (1 - DRAG), y: next.y, z: next.z + next.vz * DRAG / (1 - DRAG) }
    checkedBoatRoute(blockAt, next, coast)
    steps.push({ state: next, input }); state = next
    settled = braking && still(state) ? settled + 1 : 0
    if (settled >= 3) {
      if (Math.hypot(goal.x - state.x, goal.z - state.z) > 0.12) throw new BoatRouteError('boat cannot settle near this destination with the checked steering path')
      return { ...route, steps }
    }
  }
  throw new BoatRouteError('boat steering could not produce a bounded arrival; choose a nearer open-water waypoint')
}

const writeInput = (bot, input) => {
  if (bot.supportFeature?.('newPlayerInputPacket')) bot._client.write('player_input', { inputs: { ...input } })
  else bot._client.write('steer_vehicle', { sideways: input.left ? 1 : input.right ? -1 : 0, forward: input.forward ? 1 : input.back ? -1 : 0, jump: 0 })
  bot._client.write('steer_boat', { leftPaddle: Boolean(input.right && !input.left || input.forward), rightPaddle: Boolean(input.left && !input.right || input.forward) })
}

export async function driveBoat ({ bot, entity, goal, check, pause, report = () => {}, reportPerformance = () => {} }) {
  const blockAt = (x, y, z) => bot.blockAt(new Vec3(x, y, z))
  const begun = performance.now()
  let plan
  try { plan = planBoatLeg(blockAt, entity, goal) } finally { reportPerformance('boat.route', performance.now() - begun, { id: entity.id, phase: 'departure' }) }
  report({ action: 'boat_drive', status: 'water route checked', distance: plan.distance, ticks: plan.steps.length })
  let state = initialState(entity), correction = false, completed = false
  const riderPose = () => {
    // Vanilla ordinary boat attachment is height/3, minus the player's .6
    // vehicle attachment. Only predicted mounted pose; dismount needs server
    // passenger removal and a server position before any walking is allowed.
    bot.entity.position.set(entity.position.x, entity.position.y + HEIGHT / 3 - 0.6, entity.position.z)
    bot.entity.yaw = entity.yaw
  }
  const corrected = packet => {
    correction = true
    if (['x', 'y', 'z'].every(k => Number.isFinite(packet[k]))) {
      entity.position.set(packet.x, packet.y, packet.z)
      if (Number.isFinite(packet.yaw)) entity.yaw = (180 - packet.yaw) * DEG
      entity.velocity?.set(0, 0, 0); riderPose()
      bot._client.write('vehicle_move', { ...packet, onGround: false })
    }
  }
  const send = (next, input) => {
    writeInput(bot, input)
    bot._client.write('vehicle_move', { x: next.x, y: next.y, z: next.z, yaw: next.yaw, pitch: 0, onGround: false })
    entity.position.set(next.x, next.y, next.z); entity.velocity?.set(next.vx, next.vy, next.vz)
    entity.yaw = (180 - next.yaw) * DEG
    riderPose(); state = next
  }
  const checkStep = next => {
    const start = performance.now()
    try {
      checkedBoatRoute(blockAt, state, next)
      const coast = { x: next.x + next.vx * DRAG / (1 - DRAG), y: next.y, z: next.z + next.vz * DRAG / (1 - DRAG) }
      checkedBoatRoute(blockAt, next, coast)
      for (const other of Object.values(bot.entities)) {
        if (other === entity || other === bot.entity || other.isValid === false || !other.position || ['item', 'experience_orb', 'arrow'].includes(other.name)) continue
        const p = other.position, w = (other.width ?? 0.6) / 2
        if (p.y + (other.height ?? 1.8) > state.y && p.y < state.y + 2.2 && p.x + w > Math.min(state.x, coast.x) - HALF && p.x - w < Math.max(state.x, coast.x) + HALF && p.z + w > Math.min(state.z, coast.z) - HALF && p.z - w < Math.max(state.z, coast.z) + HALF) throw new BoatRouteError('another entity entered the checked boat corridor')
      }
    } finally { reportPerformance('boat.route', performance.now() - start, { id: entity.id, phase: 'clearance' }) }
  }
  bot._client.on('vehicle_move', corrected)
  try {
    for (const step of plan.steps) {
      check()
      if (Math.hypot(entity.position.x - state.x, entity.position.y - state.y, entity.position.z - state.z) > 0.05) correction = true
      if (correction || bot.vehicle?.id !== entity.id) throw new BoatRouteError('boat movement was corrected or the controlling seat was lost; inspect the authoritative state')
      checkStep(step.state); send(step.state, step.input)
      await pause(50)
    }
    check()
    if (correction || bot.vehicle?.id !== entity.id) throw new BoatRouteError('boat arrival was not confirmed; inspect state')
    completed = true
    return { arrived: true, mounted: true, at: point(entity.position), distance: plan.distance, ticks: plan.steps.length, prediction: 'vanilla source-water boat physics; no server correction received' }
  } finally {
    // Neutral paddles do not erase momentum. Continue only the already checked
    // natural coast, bounded to 3 seconds, and retain the seat on cancellation.
    if (!completed && !correction) for (let tick = 0; tick < 60 && !still(state); tick++) {
      if (correction || bot.vehicle?.id !== entity.id || entity.isValid === false || bot.health <= 0) break
      try {
        const next = stepBoatWater(state, {}, plan.waterLevel)
        checkStep(next); send(next, {}); await pause(50)
      } catch { break }
    }
    bot._client.removeListener('vehicle_move', corrected)
    try { writeInput(bot, {}) } catch { /* Keep the original disconnect/cancellation. */ }
    if (!completed && !still(state)) report({ action: 'boat_drive', status: 'neutral paddles; safe coast could not finish, remain aboard and inspect state' })
  }
}

driveBoat.validate = ({ bot, entity, goal, check, reportPerformance = () => {} }) => {
  check()
  const start = performance.now()
  try { return planBoatLeg((x, y, z) => bot.blockAt(new Vec3(x, y, z)), entity, goal) } finally { reportPerformance('boat.route', performance.now() - start, { id: entity.id, phase: 'preflight' }) }
}
