// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Sensing for the primitives: self, entities, blockAt and the settling state of a body.

import { lineClear, rayClear, blocksSight } from './sight.mjs'
import { stateProperties } from './use-on.mjs'
import { mobFields } from './interact.mjs'
import { professionOf, villagerData } from './villager.mjs'
import { leashFields } from './leash.mjs'
import { liveEntities } from './live-entities.mjs'
import { vehicleFields, selfVehicle } from './vehicle.mjs'
import { weatherOf, DEFAULT_RADIUS, HIT_RANGE, xyz, dist, cell, vec, entityKind, burning, lyingDown, droppedItem } from './prim-base.mjs'

export function createSense (env) {
  const { here, eye, inventory, isOffline, equipment, timeScale, settleMs } = env
  // ---- sensing ----

  // Settling: connected but the senses are not trustworthy yet (entities arrive after the chunks, there is no signal
  // for "all sent"). Starts at login, on every adopted reconnect, on respawn and on a teleport; ends settleMs after
  // the column under the body is loaded. readyAt null means the column was not loaded when it was last looked at.
  let readyAt = null

  const feetIn = name => env.bot.blockAt(vec(cell(here())))?.name === name

  // "FireResistance" (registry) or "minecraft:fire_resistance" -> "fire_resistance"
  const effectName = id => {
    const name = env.bot.registry?.effects?.[id]?.name
    if (!name) return String(id)
    return name.replace(/^minecraft:/, '').replace(/([a-z0-9])([A-Z])/g, '$1_$2').toLowerCase()
  }
  const effects = () => Object.values(env.bot.entity.effects ?? {})
    .map(e => ({ name: effectName(e.id), amplifier: e.amplifier, duration: e.duration }))

  // false while the column under the body is not loaded: mineflayer's physics then skips its tick and the body hangs.
  const columnLoaded = () => Boolean(env.bot.blockAt(env.bot.entity.position))

  const settleFromNow = () => { readyAt = columnLoaded() ? Date.now() + settleMs * timeScale : null }
  const isSettling = () => {
    if (isOffline()) return false
    if (!columnLoaded()) { readyAt = null; return true }
    readyAt ??= Date.now() + settleMs * timeScale
    return Date.now() < readyAt
  }

  const self = () => isOffline() ? { status: 'offline' } : readSelf()

  // Position, health, food, inventory and equipment self() read just before the body went away (a log-out or a dropped connection), for status and inventory
  // views while it is offline; null when online or when nothing could be read.
  let lastSelf = null
  const rememberSelf = () => {
    try {
      const { pos, health, food, equipment, inventory } = readSelf()
      lastSelf = { pos, health, food, equipment, inventory }
    } catch { lastSelf = null }
  }
  const lastKnown = () => isOffline() ? lastSelf : null

  const readSelf = () => {
    const timeOfDay = env.bot.time.timeOfDay
    return {
      username: env.bot.username,
      pos: here(),
      health: env.bot.health,
      food: env.bot.food,
      foodSaturation: env.bot.foodSaturation,
      oxygen: env.bot.oxygenLevel ?? 20,
      onFire: burning(env.bot, env.bot.entity),
      inWater: env.bot.entity.isInWater ?? feetIn('water'),
      inLava: env.bot.entity.isInLava ?? feetIn('lava'),
      onGround: Boolean(env.bot.entity.onGround),
      chunkLoaded: columnLoaded(),
      settling: isSettling(),
      isSleeping: Boolean(env.bot.isSleeping),
      vehicle: selfVehicle(env.bot),
      effects: effects(),
      experience: { level: env.bot.experience?.level ?? 0, points: env.bot.experience?.points ?? 0, progress: env.bot.experience?.progress ?? 0 },
      dimension: env.bot.game?.dimension,
      timeOfDay,
      isDay: timeOfDay < 12542 || timeOfDay > 23460,
      // the other players in the server's player list (what the tab list shows a player)
      players: Object.keys(env.bot.players ?? {}).filter(name => name !== env.bot.username),
      ...weatherOf(env.bot),
      held: env.bot.heldItem?.name ?? null,
      equipment: equipment(),
      inventory: inventory().map(i => {
        const max = env.bot.registry?.itemsByName?.[i.name]?.maxDurability
        return { name: i.name, count: i.count, slot: i.slot, ...(max > 0 && { durability: max - (i.durabilityUsed ?? 0), maxDurability: max }) }
      })
    }
  }

  // An unloaded cell never blocks, so a threat is not hidden by a gap in the map.
  const cellBlocksSight = p => blocksSight(env.bot.blockAt(vec(p)))
  // eye to the middle of the entity; the walk is bounded by that segment, which the caller keeps within its radius
  const canSee = e => lineClear(eye(), { x: e.position.x, y: e.position.y + (e.height ?? 1.8) / 2, z: e.position.z }, cellBlocksSight)

  // collision boxes of a cell, for melee: a block's shapes when it is solid; an unloaded cell has none
  const shapesAt = p => {
    const block = env.bot.blockAt(vec(p))
    return block?.boundingBox === 'block' ? block.shapes ?? [] : []
  }
  const canHit = e => [0.2, (e.height ?? 1.8) / 2, (e.height ?? 1.8) - 0.1].some(dy =>
    rayClear(eye(), { x: e.position.x, y: e.position.y + dy, z: e.position.z }, shapesAt))

  const entities = ({ radius = DEFAULT_RADIUS, kind, names, ids, max = 32 } = {}) => {
    if (isOffline()) return []
    const me = here()
    return liveEntities(env.bot)
      .filter(e => e !== env.bot.entity && e.position)
      .map(e => ({ e, distance: dist(me, e.position), kind: entityKind(e) }))
      .filter(({ e, distance, kind: k }) => distance <= radius && (!kind || k === kind) && (!names || names.includes(e.name ?? e.username)) && (!ids || ids.includes(e.id)))
      // like a player: a passive mob or a villager behind a wall is not listed. Hostiles, items and players stay
      // listed with `visible` (players show through walls in the game, nametags); sleeping needs sight
      .filter(({ e, kind: k }) => k === 'hostile' || k === 'item' || k === 'player' || canSee(e))
      .sort((a, b) => a.distance - b.distance)
      .slice(0, max)
      .map(({ e, distance, kind: k }) => ({
        id: e.id,
        name: e.name ?? e.username,
        kind: k,
        pos: xyz(e.position),
        distance,
        ...(k !== 'item' && k !== 'player' && mobFields(env.bot, e)),
        ...(e.name === 'villager' && { profession: professionOf(villagerData(env.bot, e).villagerProfession) }),
        ...(k !== 'item' && k !== 'player' && leashFields(env.bot, e)),
        ...(k !== 'item' && vehicleFields(env.bot, e)),
        ...((k === 'hostile' || k === 'item' || k === 'player') && { visible: canSee(e) }),
        ...(k !== 'item' && distance <= HIT_RANGE && { hittable: canHit(e) }),
        ...(k === 'item' && { item: droppedItem(env.bot, e) }),
        ...(k === 'player' && { username: e.username, sleeping: lyingDown(env.bot, e) && canSee(e) }),
        ...(e.name === 'creeper' && { creeper: true })
      }))
  }

  // a collision shape that fills the whole cell: what a head can be stuck in (slabs, farmland, crops, carpets are not)
  const fullCube = block => block.boundingBox === 'block' && (block.shapes ?? []).some(([x0, y0, z0, x1, y1, z1]) => x0 <= 0 && y0 <= 0 && z0 <= 0 && x1 >= 1 && y1 >= 1 && z1 >= 1)

  // {name, pos}, plus the crop `age` as a number when the block has one, plus all its state `properties` when it has any,
  // plus `fullCube: true` when its collision shape fills the cell
  const blockInfo = (block, withProps = true) => {
    const properties = stateProperties(block)
    const age = properties.age
    return { name: block.name, pos: xyz(block.position), ...(age !== undefined && { age: Number(age) }), ...(withProps && Object.keys(properties).length > 0 && { properties }), ...(fullCube(block) && { fullCube: true }) }
  }

  const blockAt = pos => {
    if (isOffline()) return null
    const block = env.bot.blockAt(vec(pos))
    return block ? blockInfo(block) : null
  }
  return { self, entities, blockAt, isSettling, settleFromNow, rememberSelf, lastKnown, columnLoaded }
}
