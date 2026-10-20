// Why JavaScript: Mineflayer boundary; reads packed chunk palettes and light nibbles straight from the bot's columns.
// The raw world the perception layer (engine.perception) looks at: block state ids, light, the eye, block-change
// events. Nothing here decides what the body sees; it only answers "what is really there". The bot is read through
// getBot at each call, so a reconnect is followed (the block-change listener moves to the new bot on the next call).
import prismarineBlock from 'prismarine-block'
import vec3 from 'vec3'
import { sectionIds } from './path/snapshot.mjs'
import { blocksSight } from './sight.mjs'
import { fullCube, shapeTop } from './block-shape.mjs'
import { columnLightSection, hasSkyLight } from './view.mjs'
import { setting } from './settings.mjs'
import constants from '../src/engine/constants.json' with { type: 'json' }

const { Vec3 } = vec3
export const UNLOADED = -1
const EYE_HEIGHT = constants.eyeHeight
const hereY = bot => bot?.entity?.position?.y ?? 0

// Uint8Array over state ids: 1 where the state blocks sight.
export function sightTable (registry) {
  const size = registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0) + 1
  const out = new Uint8Array(size)
  for (const block of registry.blocksArray) {
    if (!blocksSight(block)) continue
    out.fill(1, block.minStateId, block.maxStateId + 1)
  }
  return out
}

// id -> {name, properties, boundingBox ('block' or 'empty'), fullCube, top (shapeTop), shapes (collision boxes)}, cached per id.
export function stateInfo (registry) {
  const Block = prismarineBlock(registry)
  const cache = new Map()
  return id => {
    let info = cache.get(id)
    if (info) return info
    const block = Block.fromStateId(id, 0)
    info = { name: block.name, properties: block.getProperties(), boundingBox: block.boundingBox, fullCube: fullCube(block), top: shapeTop(block), shapes: block.boundingBox === 'block' ? block.shapes ?? [] : [] }
    cache.set(id, info)
    return info
  }
}

// section key: column (16 bits each, as path/snapshot.mjs) and world section index (6 bits)
const keyOf = (cx, cz, s) => ((cx & 0xFFFF) * 65536 + (cz & 0xFFFF)) * 64 + s

export function createRawWorld ({ getBot, isOffline = () => false, lightOverlay = () => undefined, now = Date.now, lightTtlMs = setting('raw-world/light-ttl-ms') }) {
  const states = new Map() // key -> Uint16Array | null (no column)
  const lights = new Map() // key -> {sky, block, at} | null
  const listeners = new Set()
  let hooked = null
  let unhook = () => {}
  let tables = null // {registry, sight, info}
  let lastStateKey = -1
  let lastStates = null
  let lastLightKey = -1
  let lightCalls = 0
  let lastLight = null
  let epoch = 0 // counts block updates and chunk loads/unloads within EPOCH_RANGE of the body: "what it can see may differ"
  const near = (bot, x, y, z, range) => {
    const e = bot?.entity?.position
    return !e || (Math.abs(x - e.x) <= range && Math.abs(z - e.z) <= range && Math.abs(y - e.y) <= range)
  }

  const forget = () => {
    states.clear()
    lights.clear()
    lastStateKey = -1
    lastLightKey = -1
  }

  // a column's sections (world section indices 0..63) leave the caches; the arg is mineflayer's corner Vec3 in blocks
  const forgetColumn = corner => {
    if (near(getBot(), corner.x + 8, hereY(getBot()), corner.z + 8, setting('raw-world/epoch-range') + 8)) epoch++
    const cx = corner.x >> 4
    const cz = corner.z >> 4
    for (let s = 0; s < 64; s++) {
      const key = keyOf(cx, cz, s)
      states.delete(key)
      lights.delete(key)
    }
    lastStateKey = -1
    lastLightKey = -1
  }

  const onUpdate = (oldBlock, newBlock) => {
    const p = (newBlock ?? oldBlock)?.position
    if (!p) return
    if (near(getBot(), p.x, p.y, p.z, setting('raw-world/epoch-range'))) epoch++
    const bot = getBot()
    const s = (p.y - (bot.game?.minY ?? -64)) >> 4
    const key = keyOf(p.x >> 4, p.z >> 4, s)
    states.delete(key)
    lights.delete(key)
    if (key === lastStateKey) lastStateKey = -1
    if (key === lastLightKey) lastLightKey = -1
    const id = newBlock?.stateId ?? 0
    if (oldBlock?.stateId === id) return
    for (const fn of listeners) fn(p.x, p.y, p.z, id)
  }

  const follow = () => {
    const bot = getBot()
    if (bot === hooked) return bot
    unhook()
    forget()
    hooked = bot
    tables = null
    if (!bot?.on) { unhook = () => {}; return bot }
    bot.on('blockUpdate', onUpdate)
    bot.on('chunkColumnLoad', forgetColumn)
    bot.on('chunkColumnUnload', forgetColumn)
    unhook = () => {
      bot.removeListener('blockUpdate', onUpdate)
      bot.removeListener('chunkColumnLoad', forgetColumn)
      bot.removeListener('chunkColumnUnload', forgetColumn)
    }
    return bot
  }

  const minYOf = bot => bot.game?.minY ?? -64
  const heightOf = bot => bot.game?.height ?? 384

  const sectionStates = (bot, cx, cz, s, key) => {
    if (states.size > setting('raw-world/cache-max')) forget()
    const container = bot.world?.getColumn?.(cx, cz)?.sections?.[s]?.data
    const ids = container ? sectionIds(container) : null
    states.set(key, ids)
    return ids
  }

  const stateAt = (x, y, z) => {
    const bot = follow()
    if (!bot) return UNLOADED
    const ry = y - minYOf(bot)
    if (ry < 0 || ry >= heightOf(bot)) return UNLOADED
    const s = ry >> 4
    const key = keyOf(x >> 4, z >> 4, s)
    if (key !== lastStateKey) {
      lastStates = states.has(key) ? states.get(key) : sectionStates(bot, x >> 4, z >> 4, s, key)
      lastStateKey = key
    }
    if (lastStates === null) return UNLOADED
    return lastStates[((ry & 15) << 8) | ((z & 15) << 4) | (x & 15)]
  }

  const sectionLight = (bot, cx, cz, s, key) => {
    if (lights.size > setting('raw-world/cache-max')) forget()
    const column = bot.world?.getColumn?.(cx, cz)
    const light = column ? (lightOverlay(cx, cz, s) ?? columnLightSection(column, s, hasSkyLight(bot.game?.dimension))) : null
    const entry = light ? { sky: light.sky, block: light.block, at: now() } : null
    lights.set(key, entry)
    return entry
  }

  const lightOf = (entry, ry, x, z) => {
    if (entry === null) return 0
    const i = ((ry & 15) << 8) | ((z & 15) << 4) | (x & 15)
    return (entry.sky[i] << 4) | entry.block[i]
  }

  // sky << 4 | block of the cell, 0 when not loaded
  const lightAt = (x, y, z) => {
    const bot = follow()
    if (!bot) return 0
    const ry = y - minYOf(bot)
    if (ry < 0 || ry >= heightOf(bot)) return ry < 0 ? 0 : 0xF0
    const s = ry >> 4
    const key = keyOf(x >> 4, z >> 4, s)
    // the clock is read when the section changes or every 256th call, not per cell
    if (key !== lastLightKey || (++lightCalls & 255) === 0) {
      const t = now()
      if (key === lastLightKey && lastLight && t - lastLight.at <= lightTtlMs) return lightOf(lastLight, ry, x, z)
      const cached = lights.get(key)
      lastLight = cached && t - cached.at <= lightTtlMs ? cached : sectionLight(bot, x >> 4, z >> 4, s, key)
      lastLightKey = key
    }
    return lightOf(lastLight, ry, x, z)
  }

  const tablesOf = bot => {
    if (tables?.registry === bot.registry) return tables
    tables = { registry: bot.registry, sight: sightTable(bot.registry), info: stateInfo(bot.registry) }
    return tables
  }

  return {
    stateAt,
    lightAt,
    // {x, y, z, yaw, pitch, dimension}: the eye, mineflayer radians (yaw 0 faces -z, pitch up is positive); null offline
    eye: () => {
      const bot = follow()
      const e = bot?.entity
      if (!e?.position || isOffline()) return null
      return { x: e.position.x, y: e.position.y + EYE_HEIGHT, z: e.position.z, yaw: e.yaw ?? 0, pitch: e.pitch ?? 0, dimension: bot.game?.dimension ?? 'overworld' }
    },
    // collision boxes of the cell, local coordinates (0..1 across it): a solid block's shapes (a full box when it lists none), none for an unloaded or non-solid cell
    shapesAt: (x, y, z) => {
      const block = follow()?.blockAt?.(new Vec3(x, y, z))
      if (block?.boundingBox !== 'block') return []
      return block.shapes?.length ? block.shapes : [[0, 0, 0, 1, 1, 1]]
    },
    // the biome's name (plains) of the cell, null when its column is not loaded
    biomeAt: (x, y, z) => {
      const bot = follow()
      const [fx, fy, fz] = [x, y, z].map(Math.floor)
      const column = bot?.world?.getColumn?.(fx >> 4, fz >> 4)
      return column ? bot.registry?.biomes?.[column.getBiome(new Vec3(fx & 15, fy, fz & 15))]?.name ?? null : null
    },
    // the name of the item in the off hand (slot 45), or null
    offHand: () => follow()?.inventory?.slots?.[45]?.name ?? null,
    // the name of the held (main hand) item, or null
    heldItem: () => follow()?.heldItem?.name ?? null,
    sky: () => {
      const bot = follow()
      return { timeOfDay: bot?.time?.timeOfDay ?? 6000, rain: bot?.rainState ?? 0, thunder: bot?.thunderState ?? 0 }
    },
    // changes whenever a block, chunk column or the hooked bot changes (light-only changes are not counted)
    epoch: () => { follow(); return epoch },
    version: () => follow()?.version ?? null,
    // null while the bot has no registry yet (the perception waits and asks again)
    sightTable: () => { const bot = follow(); return bot?.registry ? tablesOf(bot).sight : null },
    stateInfo: id => tablesOf(follow()).info(id),
    // fn(x, y, z, stateId) for every changed cell; returns the unsubscribe
    onBlockChange: fn => {
      follow()
      listeners.add(fn)
      return () => listeners.delete(fn)
    },
    close: () => { unhook(); unhook = () => {}; hooked = null; listeners.clear(); forget() }
  }
}
