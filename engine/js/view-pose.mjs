// Why JavaScript: binary/graphics; the view dump's column, light and pose encoding.
// View dump pose and hud snapshots of the body and what it sees.
import { liveEntities } from './live-entities.mjs'
import { mobFields, sheepDye } from './interact.mjs'
import { VIEW_VERSION } from './view-column.mjs'

export const ENTITY_RANGE = 48

const round2 = n => Math.round(n * 100) / 100
const round4 = n => Math.round(n * 10000) / 10000
// pose.json is rewritten about 10 times a second: short numbers keep it within one or two 4 KB pages
export const poseJson = pose => JSON.stringify(pose, (k, v) => typeof v === 'number' ? round4(v) : v)
const xyz = v => ({ x: v.x, y: v.y, z: v.z })

// ---- pose and hud ----

const eyeHeight = entity => entity.height ? entity.height - 0.18 : 1.62

// the name of the item a drop entity holds, or null when the client library cannot read it (it reads one fixed metadata index)
const droppedName = (bot, e) => {
  if (e.name !== 'item') return null
  try {
    const stack = e.getDroppedItem?.()
    return stack?.name ?? bot.registry?.items?.[stack?.type ?? stack?.itemId]?.name ?? null
  } catch { return null }
}

const entityView = (bot, e, item = droppedName(bot, e), baby = mobFields(bot, e).baby === true, dye = sheepDye(bot, e)) => ({
  id: e.id,
  type: e.type ?? null,
  name: e.name ?? null,
  kind: e.kind ?? null,
  ...(e.username ? { username: e.username } : {}),
  ...(item ? { item } : {}),
  ...(baby ? { baby } : {}),
  ...(dye === null ? {} : { dye }),
  pos: xyz(e.position),
  yaw: e.yaw ?? 0,
  pitch: e.pitch ?? 0,
  height: e.height ?? null,
  width: e.width ?? null,
  health: e.health ?? null
})

const near = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z) <= ENTITY_RANGE

export function poseSnapshot (bot, { world, now }) {
  const self = bot.entity
  const p = self.position
  return {
    v: VIEW_VERSION,
    t: now,
    world,
    status: 'online',
    dimension: bot.game?.dimension ?? null,
    mcVersion: bot.version,
    pos: xyz(p),
    eye: { x: p.x, y: p.y + eyeHeight(self), z: p.z },
    yaw: self.yaw,
    pitch: self.pitch,
    velocity: xyz(self.velocity ?? { x: 0, y: 0, z: 0 }),
    onGround: Boolean(self.onGround),
    entities: liveEntities(bot).filter(e => e !== self && e.position && near(e.position, p)).map(e => entityView(bot, e)),
    timeOfDay: bot.time?.timeOfDay ?? null,
    rain: bot.rainState ?? 0
  }
}

// a string that changes when the pose does: time ignored, numbers rounded to two decimals
export const bodyKey = pose => poseKey({ ...pose, entities: undefined, timeOfDay: undefined, rain: undefined })
export const poseKey = pose => JSON.stringify(pose, (k, v) => k === 't' ? undefined : typeof v === 'number' ? round2(v) : v)

// the last full pose marked offline (so a renderer still has a position), or the short record if none was ever written
export const offlinePose = ({ world, now, mcVersion, last }) => last
  ? { ...last, t: now, status: 'offline' }
  : { v: VIEW_VERSION, t: now, world, status: 'offline', mcVersion }

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
    inventory: slots.map((item, slot) => item ? { slot, name: item.name, count: item.count } : null).filter(Boolean),
    window: bot.currentWindow?.type ?? null
  }
}

export const hudKey = hud => JSON.stringify(hud, (k, v) => k === 't' ? undefined : v)
