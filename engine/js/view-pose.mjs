// Why JavaScript: binary/graphics; the view dump's column, light and pose encoding.
// View dump pose and hud snapshots of the body and what it sees.
import vec3 from 'vec3'
import { liveEntities } from './live-entities.mjs'
import { clearLine } from './sight.mjs'
import { mobFields, sheepDye } from './interact.mjs'
import { VIEW_VERSION } from './view-column.mjs'
import constants from '../src/engine/constants.json' with { type: 'json' }

export const ENTITY_RANGE = 48

const round2 = n => Math.round(n * 100) / 100
const round4 = n => Math.round(n * 10000) / 10000
// pose.json is rewritten about 10 times a second: short numbers keep it within one or two 4 KB pages
export const poseJson = pose => JSON.stringify(pose, (k, v) => typeof v === 'number' ? round4(v) : v)
const xyz = v => ({ x: v.x, y: v.y, z: v.z })

// ---- pose and hud ----

const eyeHeight = entity => entity.height ? entity.height - 0.18 : constants.eyeHeight

// the name of the item a drop entity holds, or null when the client library cannot read it (it reads one fixed metadata index)
const droppedName = (bot, e) => {
  if (e.name !== 'item') return null
  try {
    const stack = e.getDroppedItem?.()
    return stack?.name ?? bot.registry?.items?.[stack?.type ?? stack?.itemId]?.name ?? null
  } catch { return null }
}

const entityView = (bot, e, seen, item = droppedName(bot, e), baby = mobFields(bot, e).baby === true, dye = sheepDye(bot, e)) => ({
  id: e.id,
  type: e.type ?? null,
  name: e.name ?? null,
  kind: e.kind ?? null,
  ...(e.username ? { username: e.username } : {}),
  ...(item ? { item } : {}),
  ...(baby ? { baby } : {}),
  ...(dye === null ? {} : { dye }),
  seen,
  pos: xyz(e.position),
  yaw: e.yaw ?? 0,
  pitch: e.pitch ?? 0,
  height: e.height ?? null,
  width: e.width ?? null,
  health: e.health ?? null
})

const { Vec3 } = vec3

// whether the body's eye has a clear line to the entity (feet, middle or head): a player does not see through walls. A bot that
// cannot read blocks sees everything. `blockAt` answers whole cells and is memoised by the caller for one scan: the lines all start at the eye.
const inSight = (blockAt, eye, e) => {
  if (!blockAt) return true
  const h = e.height ?? 1
  return [0.1, h / 2, h * 0.9].some(up => clearLine(blockAt, eye, { x: e.position.x, y: e.position.y + up, z: e.position.z }))
}

// one scan's block reads: a cell is read once however many lines cross it, and one Vec3 serves every read. Keys are offsets from the
// eye's cell (entities are within ENTITY_RANGE, so each offset fits in 8 bits).
const scanBlockAt = (bot, eye) => {
  if (typeof bot.blockAt !== 'function') return null
  const [ox, oy, oz] = [Math.floor(eye.x) - 128, Math.floor(eye.y) - 128, Math.floor(eye.z) - 128]
  const cells = new Map()
  const at = new Vec3(0, 0, 0)
  return c => {
    const key = ((c.x - ox) * 256 + (c.y - oy)) * 256 + (c.z - oz)
    if (cells.has(key)) return cells.get(key)
    const block = bot.blockAt(at.set(c.x, c.y, c.z))
    cells.set(key, block)
    return block
  }
}

const near = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z) <= ENTITY_RANGE

export const poseEntities = bot => {
  const self = bot.entity
  const p = self.position
  const eye = { x: p.x, y: p.y + eyeHeight(self), z: p.z }
  const blockAt = scanBlockAt(bot, eye)
  return liveEntities(bot).filter(e => e !== self && e.position && near(e.position, p)).map(e => entityView(bot, e, inSight(blockAt, eye, e)))
}

// withEntities false leaves the list empty: the caller's write gates need only the body part, and the entity scan is the costly bit
export function poseSnapshot (bot, { world, now, withEntities = true }) {
  const self = bot.entity
  const p = self.position
  const eye = { x: p.x, y: p.y + eyeHeight(self), z: p.z }
  return {
    v: VIEW_VERSION,
    t: now,
    world,
    status: 'online',
    dimension: bot.game?.dimension ?? null,
    mcVersion: bot.version,
    pos: xyz(p),
    eye,
    yaw: self.yaw,
    pitch: self.pitch,
    velocity: xyz(self.velocity ?? { x: 0, y: 0, z: 0 }),
    onGround: Boolean(self.onGround),
    entities: withEntities ? poseEntities(bot) : [],
    timeOfDay: bot.time?.timeOfDay ?? null,
    rain: bot.rainState ?? 0,
    held: bot.heldItem?.name ?? null,
    offHand: bot.inventory?.slots?.[45]?.name ?? null
  }
}

// a string that changes when the pose does: time ignored, numbers rounded to two decimals
export const bodyKey = pose => poseKey({ ...pose, entities: undefined, timeOfDay: undefined, rain: undefined })
export const poseKey = pose => JSON.stringify(pose, (k, v) => k === 't' ? undefined : typeof v === 'number' ? round2(v) : v)

// the last full pose marked offline (so a renderer still has a position), or the short record if none was ever written
export const offlinePose = ({ world, now, mcVersion, last }) => last
  ? { ...last, t: now, status: 'offline' }
  : { v: VIEW_VERSION, t: now, world, status: 'offline', mcVersion }

// durability left and max for a damageable item, nothing for the rest
const durability = (bot, item) => {
  const max = bot.registry?.itemsByName?.[item.name]?.maxDurability
  return max > 0 ? { durability: max - (item.durabilityUsed ?? 0), maxDurability: max } : {}
}

export function hudSnapshot (bot, now) {
  const slots = bot.inventory?.slots ?? []
  const effects = Object.values(bot.entity?.effects ?? {})
  return {
    v: VIEW_VERSION,
    t: now,
    health: bot.health ?? null,
    food: bot.food ?? null,
    saturation: bot.foodSaturation ?? null,
    oxygen: bot.oxygenLevel ?? null,
    xp: { level: bot.experience?.level ?? 0, points: bot.experience?.points ?? 0, progress: bot.experience?.progress ?? 0 },
    effects: effects.map(e => ({ name: bot.registry?.effects?.[e.id]?.name ?? String(e.id), amplifier: e.amplifier, duration: e.duration })),
    held: bot.heldItem ? { name: bot.heldItem.name, count: bot.heldItem.count } : null,
    inventory: slots.map((item, slot) => item ? { slot, name: item.name, count: item.count, ...durability(bot, item) } : null).filter(Boolean),
    window: bot.currentWindow?.type ?? null
  }
}

export const hudKey = hud => JSON.stringify(hud, (k, v) => k === 't' ? undefined : v)
