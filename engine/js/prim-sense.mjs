// Why JavaScript: Mineflayer boundary; the one adapter that calls Mineflayer and the pathfinder, with tick-bound policy that lives inside their event loops.
// Sensing for the primitives: self, entities, blockAt and the settling state of a body. Raw values only: engine.senses
// derives isDay, raining, thundering, visible, hittable and sleeping and drops what a player would not see.

import { fullCube } from './block-shape.mjs'
import { stateProperties } from './use-on.mjs'
import { mobFields } from './interact.mjs'
import { professionOf, villagerData } from './villager.mjs'
import { leashFields } from './leash.mjs'
import { liveEntities } from './live-entities.mjs'
import { vehicleFields, selfVehicle } from './vehicle.mjs'
import { DEFAULT_RADIUS, xyz, dist, cell, vec, entityKind, burning, lyingDown, droppedItem } from './prim-base.mjs'

export function createSense (env) {
  const { here, inventory, isOffline, equipment, timeScale, settleMs } = env
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
      timeOfDay: env.bot.time.timeOfDay,
      // the world age in game ticks (stands still while the tick is frozen)
      worldAge: env.bot.time.age ?? null,
      // the raw levels; engine.senses derives isDay, raining and thundering
      rainState: env.bot.rainState ?? 0,
      thunderState: env.bot.thunderState ?? 0,
      // the other players in the server's player list (what the tab list shows a player)
      players: Object.keys(env.bot.players ?? {}).filter(name => name !== env.bot.username),
      held: env.bot.heldItem?.name ?? null,
      equipment: equipment(),
      inventory: inventory().map(i => {
        const max = env.bot.registry?.itemsByName?.[i.name]?.maxDurability
        return { name: i.name, count: i.count, slot: i.slot, ...(max > 0 && { durability: max - (i.durabilityUsed ?? 0), maxDurability: max }) }
      })
    }
  }

  const entities = ({ radius = DEFAULT_RADIUS, kind, names, ids, max = 32 } = {}) => {
    if (isOffline()) return []
    const me = here()
    return liveEntities(env.bot)
      .filter(e => e !== env.bot.entity && e.position)
      .map(e => ({ e, distance: dist(me, e.position), kind: entityKind(e) }))
      .filter(({ e, distance, kind: k }) => distance <= radius && (!kind || k === kind) && (!names || names.includes(e.name ?? e.username)) && (!ids || ids.includes(e.id)))
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
        ...(e.height !== undefined && { height: e.height }),
        ...(k === 'item' && { item: droppedItem(env.bot, e) }),
        ...(k === 'player' && { username: e.username, lyingDown: lyingDown(env.bot, e) }),
        ...(e.name === 'creeper' && { creeper: true })
      }))
  }

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
