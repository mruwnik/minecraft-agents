// Why JavaScript: binary/graphics; column encoding, light overlay and atomic writers for the view dumps.
// View dump: what the body knows about the world, written to disk for an external renderer. Format: docs/view-format.md.
// The body pays no rendering cost here: it copies mineflayer's packed chunk data, deflates off the main thread and writes
// files. The one cell walk is the bounded local relight of the light overlay (light.mjs).
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { promisify } from 'node:util'
import prismarineRegistry from 'prismarine-registry'
import { lightTable, relightBox } from './light.mjs'
import { bodyDir, worldsDir } from './bodies.mjs'
import { liveEntities } from './live-entities.mjs'

const deflate = promisify(zlib.deflate)

export const VIEW_VERSION = 1
export const FLUSH_MS = 500
export const MAX_COLUMNS_PER_FLUSH = 8
export const POSE_HZ = 0
export const POSE_REFRESH_MS = 2000
// a pose whose only change is the surroundings (mobs, time, rain) is written at most this often; the body's own move is capped by POSE_BODY_MS
export const POSE_SURROUND_MS = 500
// the body's own small moves are written at most this often (the viewer interpolates); a jump past POSE_JUMP blocks or a dimension change goes out at once
export const POSE_BODY_MS = 100
export const POSE_JUMP = 2.5
export const HUD_MS = 1000
export const STATS_MS = 60000
export const ERROR_EVERY_MS = 60000
export const ENTITY_RANGE = 48
export const LIGHT_SECTION_BYTES = 2048
export const RELIGHT_BUDGET_MS = 10
export const RELIGHT_REACH = 16
const SECTION_VOLUME = 4096

const round2 = n => Math.round(n * 100) / 100
const round4 = n => Math.round(n * 10000) / 10000
// pose.json is rewritten about 10 times a second: short numbers keep it within one or two 4 KB pages
const poseJson = pose => JSON.stringify(pose, (k, v) => typeof v === 'number' ? round4(v) : v)
const xyz = v => ({ x: v.x, y: v.y, z: v.z })

// ---- files ----

export const columnFile = (stateDir, world, cx, cz) => path.join(worldsDir(stateDir), world, 'chunks', `${cx}.${cz}.bin`)
export const biomesFile = (stateDir, world) => path.join(worldsDir(stateDir), world, 'biomes.json')
export const poseFile = (stateDir, world, agent) => path.join(bodyDir(stateDir, world, agent), 'view', 'pose.json')
export const hudFile = (stateDir, world, agent) => path.join(bodyDir(stateDir, world, agent), 'view', 'hud.json')

// write to <file>.tmp.<pid> then rename, so a reader never sees half a file
let writeCounter = 0
export async function writeAtomic (file, data) {
  const tmp = `${file}.tmp.${process.pid}.${writeCounter++}`
  const write = () => fs.promises.writeFile(tmp, data)
  await write().catch(async err => {
    if (err.code !== 'ENOENT') throw err
    await fs.promises.mkdir(path.dirname(file), { recursive: true })
    await write()
  })
  await fs.promises.rename(tmp, file)
}

// One write in flight per file. A write asked for meanwhile replaces any waiting one, so the last data always lands
// and the intermediate ones are dropped. Resolves when data (or something newer) has been written; never rejects
// (errors go to onError).
export function coalescedWriter (file, onError = () => {}) {
  let running = null
  let waiting = null
  const drain = async () => {
    while (waiting) {
      const { data, done } = waiting
      waiting = null
      await writeAtomic(file, data).catch(onError)
      done()
    }
    running = null
  }
  return data => {
    const done = new Promise(resolve => {
      const superseded = waiting
      waiting = { data, done: () => { resolve(); superseded?.done() } }
    })
    running ??= drain()
    return done
  }
}

// ---- light: masks, nibbles, and the local relight overlay ----

// bit i of a long-array mask of [hi, lo] int32 pairs
const maskBit = (mask, i) => ((mask?.[i >> 6]?.[(i & 63) >= 32 ? 0 : 1] ?? 0) >>> (i & 31)) & 1
const copyMask = mask => (mask ?? []).map(pair => [...pair])
function setMaskBit (mask, i, on) {
  while (mask.length <= (i >> 6)) mask.push([0, 0])
  const at = (i & 63) >= 32 ? 0 : 1
  const bit = 1 << (i & 31)
  mask[i >> 6][at] = on ? mask[i >> 6][at] | bit : mask[i >> 6][at] & ~bit
}

// light section index -> its nibble buffer, for the set bits of the mask (the dump lists buffers in section order)
const lightBuffers = (buffers, mask, numSections) => {
  const out = new Map()
  let next = 0
  for (let l = 0; l < numSections + 2; l++) {
    if (maskBit(mask, l) && next < buffers.length) out.set(l, buffers[next++])
  }
  return out
}

// one byte per cell (vanilla cell order y<<8|z<<4|x) -> 2048 bytes, even cell in the low nibble
export function packNibbles (cells) {
  const out = new Uint8Array(LIGHT_SECTION_BYTES)
  for (let i = 0; i < SECTION_VOLUME; i += 2) out[i >> 1] = cells[i] | cells[i + 1] << 4
  return out
}

const unpackNibbles = (buffer, into, offset) => {
  for (let i = 0; i < SECTION_VOLUME; i++) into[offset + i] = (buffer[i >> 1] >> ((i & 1) * 4)) & 15
}

// a column's dumped light as {sky, block}: one byte per cell for every world section, section s at s * 4096.
// Mirrors tools/view/web/decodeLight: sky with no data and not flagged empty is open (15), block with no data is 0.
export function decodeColumnLight (light, numSections) {
  const sky = new Uint8Array(numSections * SECTION_VOLUME)
  const block = new Uint8Array(numSections * SECTION_VOLUME)
  const skyBuffers = lightBuffers(light.skyLight, light.skyLightMask, numSections)
  const blockBuffers = lightBuffers(light.blockLight, light.blockLightMask, numSections)
  for (let s = 0; s < numSections; s++) {
    const skyBuffer = skyBuffers.get(s + 1)
    if (skyBuffer) unpackNibbles(skyBuffer, sky, s * SECTION_VOLUME)
    else if (!maskBit(light.emptySkyLightMask, s + 1)) sky.fill(15, s * SECTION_VOLUME, (s + 1) * SECTION_VOLUME)
    const blockBuffer = blockBuffers.get(s + 1)
    if (blockBuffer) unpackNibbles(blockBuffer, block, s * SECTION_VOLUME)
  }
  return { sky, block }
}

// One light section of the column as {sky, block}, one byte per cell in vanilla order, read straight from prismarine's
// BitArray (what dumpLight serialises, without serialising): each pair of Uint32 words holds 8 vanilla bytes, the
// second word of the pair first, both big-endian. Same defaults as decodeColumnLight for sections with no data.
const unpackWords = (words, into) => {
  for (let p = 0, at = 0; p < words.length; p += 2) {
    for (let w = 1; w >= 0; w--) {
      const word = words[p + w]
      for (let shift = 24; shift >= 0; shift -= 8) {
        const byte = (word >>> shift) & 255
        into[at++] = byte & 15
        into[at++] = byte >> 4
      }
    }
  }
}

// Dimensions with no sky send no sky light at all (no data and no empty flag), which must not read as open sky.
export const hasSkyLight = dimension => !/^(minecraft:)?(the_nether|the_end)$/.test(dimension ?? '')

export function columnLightSection (column, s, hasSky = true) {
  const l = s + 1
  const sky = new Uint8Array(SECTION_VOLUME)
  const block = new Uint8Array(SECTION_VOLUME)
  const skyData = column.skyLightSections[l]
  if (skyData && column.skyLightMask.get(l)) unpackWords(skyData.data, sky)
  else if (hasSky && !column.emptySkyLightMask.get(l)) sky.fill(15)
  const blockData = column.blockLightSections[l]
  if (blockData && column.blockLightMask.get(l)) unpackWords(blockData.data, block)
  return { sky, block }
}

// one section's block state ids as a Uint16Array, read once from the palette container
export function columnStateSection (column, s) {
  const out = new Uint16Array(SECTION_VOLUME)
  const container = column.sections[s]?.data
  if (!container) return out
  if (container.data === undefined) return out.fill(container.value)
  const { data, palette } = container
  if (palette) for (let i = 0; i < SECTION_VOLUME; i++) out[i] = palette[data.get(i)]
  else for (let i = 0; i < SECTION_VOLUME; i++) out[i] = data.get(i)
  return out
}

// the dump with the overlay's sections in place of the column's: Map<world section, {sky, block}> of one byte per cell
export function overlayLight (light, overlay, numSections) {
  const skyBuffers = lightBuffers(light.skyLight, light.skyLightMask, numSections)
  const blockBuffers = lightBuffers(light.blockLight, light.blockLightMask, numSections)
  const skyLightMask = copyMask(light.skyLightMask)
  const blockLightMask = copyMask(light.blockLightMask)
  const emptySkyLightMask = copyMask(light.emptySkyLightMask)
  const emptyBlockLightMask = copyMask(light.emptyBlockLightMask)
  for (const [s, cells] of overlay) {
    const l = s + 1
    skyBuffers.set(l, packNibbles(cells.sky))
    blockBuffers.set(l, packNibbles(cells.block))
    setMaskBit(skyLightMask, l, 1)
    setMaskBit(blockLightMask, l, 1)
    setMaskBit(emptySkyLightMask, l, 0)
    setMaskBit(emptyBlockLightMask, l, 0)
  }
  const inOrder = buffers => [...buffers.keys()].sort((a, b) => a - b).map(l => buffers.get(l))
  return { skyLight: inOrder(skyBuffers), blockLight: inOrder(blockBuffers), skyLightMask, blockLightMask, emptySkyLightMask, emptyBlockLightMask }
}

// the light part: sky buffers then block buffers, each LIGHT_SECTION_BYTES; the meta says how to restore them
const encodeLight = (column, overlay) => {
  if (!column.dumpLight) return { buffer: Buffer.alloc(0), meta: {} }
  const dumped = column.dumpLight()
  const light = overlay?.size ? overlayLight(dumped, overlay, column.numSections ?? column.worldHeight >> 4) : dumped
  return {
    buffer: Buffer.concat([...light.skyLight, ...light.blockLight].map(b => Buffer.from(b))),
    meta: {
      skyCount: light.skyLight.length,
      blockCount: light.blockLight.length,
      sectionBytes: LIGHT_SECTION_BYTES,
      skyLightMask: light.skyLightMask,
      blockLightMask: light.blockLightMask,
      emptySkyLightMask: light.emptySkyLightMask,
      emptyBlockLightMask: light.emptyBlockLightMask
    }
  }
}

// ---- chunk column encoding ----

// the uncompressed file content: uint32le header length, JSON header, then the parts
export function encodeColumn ({ column, x, z, t, body, mcVersion, overlay }) {
  const sections = column.dump()
  const biomes = column.dumpBiomes?.() ?? Buffer.alloc(0)
  const light = encodeLight(column, overlay)
  const header = Buffer.from(JSON.stringify({
    v: VIEW_VERSION, x, z, t, body, mcVersion, minY: column.minY, worldHeight: column.worldHeight,
    parts: [
      { name: 'sections', len: sections.length },
      { name: 'biomes', len: biomes.length },
      { name: 'light', len: light.buffer.length, meta: light.meta }
    ]
  }), 'utf8')
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return Buffer.concat([length, header, sections, biomes, light.buffer])
}

// the reader's side: the file as written (deflated) -> {header, sections, biomes, light: {meta, buffer}}
export function decodeColumnFile (file) {
  const raw = zlib.inflateSync(file)
  const n = raw.readUInt32LE(0)
  const header = JSON.parse(raw.subarray(4, 4 + n).toString('utf8'))
  let at = 4 + n
  const parts = Object.fromEntries(header.parts.map(p => {
    const buffer = raw.subarray(at, at + p.len)
    at += p.len
    return [p.name, { buffer, meta: p.meta }]
  }))
  return { header, sections: parts.sections.buffer, biomes: parts.biomes.buffer, light: parts.light }
}

// load a decoded file into a fresh prismarine ChunkColumn built with {minY, worldHeight} from the header
export function restoreColumn (column, { sections, light }) {
  column.load(sections)
  const { meta, buffer } = light
  if (!meta.sectionBytes) return column
  const slice = (from, count) => Array.from({ length: count }, (_, i) => buffer.subarray((from + i) * meta.sectionBytes, (from + i + 1) * meta.sectionBytes))
  column.loadParsedLight(slice(0, meta.skyCount), slice(meta.skyCount, meta.blockCount),
    meta.skyLightMask, meta.blockLightMask, meta.emptySkyLightMask, meta.emptyBlockLightMask)
  return column
}

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

const entityView = (bot, e, item = droppedName(bot, e)) => ({
  id: e.id,
  type: e.type ?? null,
  name: e.name ?? null,
  kind: e.kind ?? null,
  ...(e.username ? { username: e.username } : {}),
  ...(item ? { item } : {}),
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

// ---- the writer ----

const zeroStats = () => ({
  columns: 0, bytes: 0, ms: 0, poses: 0, poseMs: 0, poseBytes: 0, huds: 0,
  relightMs: 0, relightMaxMs: 0, relightTableMs: 0, relightStatesMs: 0, relightLightMs: 0, relightFloodMs: 0, relightWriteMs: 0, relightBoxes: 0, relightCells: 0, relightCarried: 0
})

const noView = Object.freeze({
  attach: () => {}, detach: async () => {}, stop: () => {}, markCell: () => {}, flushColumns: async () => {}, tickPose: async () => {}, tickHud: async () => {},
  idle: async () => {}, pendingCount: () => 0, stats: () => zeroStats()
})

// BODY_VIEW_POSE_HZ: pose writes per second at most; 0 = every physics tick (the default), with a POSE_REFRESH_MS timer for when none arrive
export function poseHzFromEnv (env = process.env) {
  const hz = Number(env.BODY_VIEW_POSE_HZ ?? POSE_HZ)
  return Number.isFinite(hz) && hz >= 0 ? hz : POSE_HZ
}


// ---- local relight ----

const lightTables = new Map()
const tableFor = target => {
  const cached = lightTables.get(target.version)
  if (cached) return cached
  const registry = target.registry?.blocksArray ? target.registry : prismarineRegistry(target.version)
  const table = { ...lightTable(registry), stone: registry.blocksByName.stone.defaultState, air: registry.blocksByName.air.defaultState }
  lightTables.set(target.version, table)
  return table
}

export const MERGE_MAX_XZ = 48
export const MERGE_MAX_Y = 64
export const MERGE_MAX_GROWTH = 1.5

const overlaps = (a, b) => a.x0 <= b.x1 && b.x0 <= a.x1 && a.y0 <= b.y1 && b.y0 <= a.y1 && a.z0 <= b.z1 && b.z0 <= a.z1
const volume = b => (b.x1 - b.x0 + 1) * (b.y1 - b.y0 + 1) * (b.z1 - b.z0 + 1)
// yc0: the low end of the box without the downward sky extension (which does not count towards the height cap)
const unionBounds = (a, b, top) => {
  const y1 = Math.max(a.y1, b.y1)
  return {
    x0: Math.min(a.x0, b.x0), x1: Math.max(a.x1, b.x1), y0: Math.min(a.y0, b.y0), yc0: Math.min(a.yc0 ?? a.y0, b.yc0 ?? b.y0), y1,
    z0: Math.min(a.z0, b.z0), z1: Math.max(a.z1, b.z1), virtualTop: y1 === top - 1
  }
}
const mergeable = (a, b, top) => {
  if (!overlaps(a, b)) return false
  const u = unionBounds(a, b, top)
  if (u.x1 - u.x0 + 1 > MERGE_MAX_XZ || u.z1 - u.z0 + 1 > MERGE_MAX_XZ || u.y1 - u.yc0 + 1 > MERGE_MAX_Y) return false
  return volume(u) <= MERGE_MAX_GROWTH * (volume(a) + volume(b))
}

// Merges overlapping boxes while the union stays within the caps (a fill of hundreds of blocks would otherwise chain
// into one huge box). Boxes that overlap but stay apart are still exact when run one after another: a change only
// alters light within 15 cells of itself, inside its own box's interior, and the box shell (16 away) is never touched.
// Block states are read live, so every change is already in them when any box runs, and each change's box recomputes
// its whole neighbourhood from the final states. A cell left stale near another box's shell is inside some other
// change's box and is fixed when that one runs.
// Each box joins the first kept box it can merge with, repeated until nothing merges. The input boxes are never mutated.
export function mergeOverlapping (boxes, top) {
  const pass = list => {
    const out = []
    for (const box of list) {
      const at = out.findIndex(kept => mergeable(kept, box, top))
      if (at < 0) out.push({ ...box, changes: [...box.changes] })
      else {
        const kept = out[at]
        Object.assign(kept, unionBounds(kept, box, top))
        for (const change of box.changes) kept.changes.push(change)
      }
    }
    return out
  }
  let out = pass(boxes)
  for (let again = pass(out); again.length < out.length; again = pass(out)) out = again
  return out
}

// One writer per body. `attach(bot)` hooks a bot (call again for each new bot after a reconnect), `detach()` writes the
// offline pose. `onEvent(event)` receives view.stats and view.error. BODY_VIEW=0 turns it all off.
export function createView ({ stateDir, agent, world, onEvent = () => {}, now = Date.now, enabled = process.env.BODY_VIEW !== '0', poseHz = poseHzFromEnv(), relightBudgetMs = RELIGHT_BUDGET_MS }) {
  if (!enabled) return noView
  let bot = null
  let unhook = () => {}
  let timers = []
  let stats = zeroStats()
  let lastPoseKey = null
  let lastPoseAt = 0
  let lastBodyKey = null
  let lastPose = null
  let lastHudKey = null
  let lastErrorAt = -Infinity
  let mcVersion = null
  const pending = new Set()
  const overlays = new Map()
  let changes = new Map()
  const inflight = new Set()
  const running = new Set()

  const track = promise => {
    running.add(promise)
    promise.finally(() => running.delete(promise))
    return promise
  }

  const reportError = err => {
    const t = now()
    if (t - lastErrorAt < ERROR_EVERY_MS) return
    lastErrorAt = t
    onEvent({ source: 'body', kind: 'view.error', level: 'warn', error: String(err?.message ?? err) })
  }
  const safely = fn => (...args) => {
    try { return fn(...args) } catch (err) { reportError(err) }
  }
  const timed = fn => {
    const start = performance.now()
    try { return fn() } finally { stats.ms += performance.now() - start }
  }

  const markColumn = (cx, cz) => pending.add(`${cx},${cz}`)
  // the body's own dig, place, jumpPlace or useOn: the column is dumped by the next flush (every FLUSH_MS), whatever the server's block update does
  const markCell = (x, y, z) => markColumn(Math.floor(x / 16), Math.floor(z / 16))
  const onLoad = safely(point => {
    const cx = Math.floor(point.x / 16)
    const cz = Math.floor(point.z / 16)
    overlays.delete(`${cx},${cz}`)
    markColumn(cx, cz)
  })
  const onUnload = safely(point => overlays.delete(`${Math.floor(point.x / 16)},${Math.floor(point.z / 16)}`))
  const onUpdate = safely((oldBlock, newBlock) => {
    const p = (newBlock ?? oldBlock)?.position
    if (!p) return
    markColumn(Math.floor(p.x / 16), Math.floor(p.z / 16))
    if (oldBlock?.stateId !== newBlock?.stateId) changes.set(`${p.x},${p.y},${p.z}`, { x: p.x, y: p.y, z: p.z })
  })

  // Relight around the queued block changes, writing the differences into the overlay. Returns the keys of columns
  // whose changes were carried to a later flush (they are not written yet).
  const relight = target => {
    const blocked = new Set()
    if (!changes.size) return blocked
    const start = performance.now()
    const queued = [...changes.values()]
    changes = new Map()
    try {
      relightChanges(target, queued, start, blocked)
    } catch (err) { reportError(err) }
    const spent = performance.now() - start
    stats.relightMs += spent
    stats.relightMaxMs = Math.max(stats.relightMaxMs, spent)
    return blocked
  }

  const relightChanges = (target, queued, start, blocked) => {
    const table = tableFor(target)
    const infosByKey = new Map()
    // a loaded column with its section caches (states and light decoded lazily, once per flush), or null
    const columnAt = (cx, cz) => {
      const key = `${cx},${cz}`
      if (infosByKey.has(key)) return infosByKey.get(key)
      const column = target.world.getColumn(cx, cz)
      const info = column ? { key, column, states: [], light: [] } : null
      infosByKey.set(key, info)
      return info
    }
    const statesOf = (info, s) => info.states[s] ??= columnStateSection(info.column, s)
    const lightOf = (info, s) => info.light[s] ??= columnLightSection(info.column, s)
    const stateAt = (x, y, z) => {
      const info = columnAt(x >> 4, z >> 4)
      if (!info) return null
      const rel = y - info.column.minY
      return statesOf(info, rel >> 4)[(rel & 15) << 8 | (z & 15) << 4 | (x & 15)]
    }
    const first = queued.map(p => columnAt(p.x >> 4, p.z >> 4)).find(Boolean)?.column
    if (!first) return
    const minY = first.minY
    const top = minY + first.worldHeight
    const boxFor = p => {
      let low = p.y
      for (let y = p.y - 1; y >= minY; y--) {
        const state = stateAt(p.x, y, p.z)
        if (state === null || table.filter[state] !== 0) break
        low = y
      }
      const y1 = Math.min(top - 1, p.y + RELIGHT_REACH)
      return {
        x0: p.x - RELIGHT_REACH, x1: p.x + RELIGHT_REACH, z0: p.z - RELIGHT_REACH, z1: p.z + RELIGHT_REACH,
        y0: Math.max(minY, low - RELIGHT_REACH), yc0: Math.max(minY, p.y - RELIGHT_REACH), y1, virtualTop: y1 === top - 1, changes: [p]
      }
    }
    const boxes = mergeOverlapping(queued.filter(p => columnAt(p.x >> 4, p.z >> 4) && p.y >= minY && p.y < top).map(boxFor), top)

    const runBox = box => {
      const sx = box.x1 - box.x0 + 1
      const sz = box.z1 - box.z0 + 1
      const real = box.y1 - box.y0 + 1
      const sy = real + (box.virtualTop ? 1 : 0)
      const cells = sx * sy * sz
      const states = new Uint16Array(cells).fill(table.stone)
      const sky = new Uint8Array(cells)
      const block = new Uint8Array(cells)
      const sLo = (box.y0 - minY) >> 4
      const sHi = (box.y1 - minY) >> 4
      const columns = []
      for (let z = 0; z < sz; z++) {
        for (let x = 0; x < sx; x++) {
          const wx = box.x0 + x
          const wz = box.z0 + z
          const info = columnAt(wx >> 4, wz >> 4)
          if (info) columns.push({ x, z, info, local: (wz & 15) << 4 | (wx & 15) })
        }
      }
      // visit every loaded (x, z) with each section's slice of the box's y range: fn(column, s, yFrom, yTo) in box y
      const eachSlice = fn => {
        for (const col of columns) {
          for (let s = sLo; s <= sHi; s++) {
            const yFrom = Math.max(0, minY + s * 16 - box.y0)
            const yTo = Math.min(real - 1, minY + s * 16 + 15 - box.y0)
            fn(col, s, yFrom, yTo)
          }
        }
      }
      const t0 = performance.now()
      eachSlice(({ x, z, info, local }, s, yFrom, yTo) => {
        const arr = statesOf(info, s)
        const base = minY + s * 16 - box.y0
        for (let y = yFrom; y <= yTo; y++) states[(y * sz + z) * sx + x] = arr[(y - base) << 8 | local]
      })
      if (box.virtualTop) {
        for (let z = 0; z < sz; z++) {
          for (let x = 0; x < sx; x++) {
            const i = (real * sz + z) * sx + x
            states[i] = table.air
            sky[i] = 15
          }
        }
      }
      const t1 = performance.now()
      eachSlice(({ x, z, info, local }, s, yFrom, yTo) => {
        const o = overlays.get(info.key)?.get(s) ?? lightOf(info, s)
        const base = minY + s * 16 - box.y0
        for (let y = yFrom; y <= yTo; y++) {
          const i = (y * sz + z) * sx + x
          const j = (y - base) << 8 | local
          sky[i] = o.sky[j]
          block[i] = o.block[j]
        }
      })
      const t2 = performance.now()
      const out = relightBox({ table, states, sky, block, size: [sx, sy, sz] })
      const t3 = performance.now()
      const touched = new Set()
      eachSlice(({ x, z, info, local }, s, yFrom, yTo) => {
        if (x === 0 || x === sx - 1 || z === 0 || z === sz - 1) return
        const base = minY + s * 16 - box.y0
        let o = null
        for (let y = Math.max(1, yFrom); y <= Math.min(sy - 2, yTo); y++) {
          const i = (y * sz + z) * sx + x
          if (out.sky[i] === sky[i] && out.block[i] === block[i]) continue
          if (!o) {
            let sections = overlays.get(info.key)
            if (!sections) overlays.set(info.key, sections = new Map())
            o = sections.get(s)
            if (!o) {
              const from = lightOf(info, s)
              sections.set(s, o = { sky: from.sky.slice(), block: from.block.slice() })
            }
          }
          const j = (y - base) << 8 | local
          o.sky[j] = out.sky[i]
          o.block[j] = out.block[i]
          touched.add(info.key)
        }
      })
      for (const key of touched) pending.add(key)
      const t4 = performance.now()
      stats.relightStatesMs += t1 - t0
      stats.relightLightMs += t2 - t1
      stats.relightFloodMs += t3 - t2
      stats.relightWriteMs += t4 - t3
      stats.relightBoxes++
      stats.relightCells += (sx - 2) * (sy - 2) * (sz - 2)
    }

    boxes.forEach((box, n) => {
      if (n > 0 && performance.now() - start >= relightBudgetMs) {
        for (const p of box.changes) {
          changes.set(`${p.x},${p.y},${p.z}`, p)
          blocked.add(`${p.x >> 4},${p.z >> 4}`)
          stats.relightCarried++
        }
        return
      }
      runBox(box)
    })
  }

  const writeColumn = async (key, raw) => {
    const [cx, cz] = key.split(',').map(Number)
    const data = await deflate(raw, { level: 1 })
    await writeAtomic(columnFile(stateDir, world, cx, cz), data)
    stats.columns++
    stats.bytes += data.length
  }

  const flushColumns = () => {
    if (!bot) return Promise.resolve()
    const target = bot
    const writes = []
    const blocked = relight(target)
    for (const key of [...pending]) {
      if (writes.length >= MAX_COLUMNS_PER_FLUSH) break
      if (inflight.has(key) || blocked.has(key)) continue
      pending.delete(key)
      const [cx, cz] = key.split(',').map(Number)
      const raw = (() => {
        try {
          return timed(() => {
            const column = target.world.getColumn(cx, cz)
            return column ? encodeColumn({ column, x: cx, z: cz, t: now(), body: agent, mcVersion: target.version, overlay: overlays.get(key) }) : null
          })
        } catch (err) { reportError(err); return null }
      })()
      if (!raw) continue
      inflight.add(key)
      writes.push(writeColumn(key, raw).catch(reportError).finally(() => inflight.delete(key)))
    }
    return track(Promise.all(writes))
  }

  const writePose = coalescedWriter(poseFile(stateDir, world, agent), err => reportError(err))
  const writeHud = coalescedWriter(hudFile(stateDir, world, agent), err => reportError(err))

  // main-thread time here (snapshot, change key, stringify) is counted as poseMs, apart from the column ms
  const tickPose = () => {
    if (!bot) return Promise.resolve()
    const start = performance.now()
    const json = (() => {
      try {
        const t = now()
        const pose = poseSnapshot(bot, { world, now: t })
        const key = poseKey(pose)
        const body = bodyKey(pose)
        const sinceWrite = t - lastPoseAt
        if (key === lastPoseKey && sinceWrite < POSE_REFRESH_MS) return null
        if (body === lastBodyKey && sinceWrite < POSE_SURROUND_MS) return null
        const moved = lastPose && (lastPose.dimension !== pose.dimension || Math.hypot(pose.pos.x - lastPose.pos.x, pose.pos.y - lastPose.pos.y, pose.pos.z - lastPose.pos.z) > POSE_JUMP)
        if (body !== lastBodyKey && sinceWrite < POSE_BODY_MS && !moved && lastPoseKey !== null) return null
        lastPoseKey = key
        lastBodyKey = body
        lastPoseAt = t
        lastPose = pose
        return poseJson(pose)
      } catch (err) { reportError(err); return null }
    })()
    stats.poseMs += performance.now() - start
    if (!json) return Promise.resolve()
    stats.poses++
    stats.poseBytes += Buffer.byteLength(json)
    return track(writePose(json))
  }

  const tickHud = () => {
    if (!bot) return Promise.resolve()
    const hud = (() => {
      try { return timed(() => hudSnapshot(bot, now())) } catch (err) { reportError(err); return null }
    })()
    if (!hud) return Promise.resolve()
    const key = hudKey(hud)
    if (key === lastHudKey) return Promise.resolve()
    lastHudKey = key
    stats.huds++
    return track(writeHud(JSON.stringify(hud)))
  }

  const takeStats = () => {
    const round3 = n => Math.round(n * 1000) / 1000
    const out = { ...stats }
    for (const key of ['ms', 'poseMs', 'relightMs', 'relightMaxMs', 'relightTableMs', 'relightStatesMs', 'relightLightMs', 'relightFloodMs', 'relightWriteMs']) out[key] = round3(stats[key])
    stats = zeroStats()
    return out
  }

  const onPhysics = () => { tickPose().catch(reportError) }

  // mineflayer applies update_light without emitting a bot event, so listen on the raw client
  const onLight = safely(packet => markColumn(packet.chunkX, packet.chunkZ))

  const hook = target => {
    const client = target._client
    const onEnd = () => { if (bot === target) detach() }
    target.on('chunkColumnLoad', onLoad)
    target.on('chunkColumnUnload', onUnload)
    target.on('blockUpdate', onUpdate)
    target.on('end', onEnd)
    target.on('kicked', onEnd)
    if (client) client.on('update_light', onLight)
    if (poseHz === 0) target.on('physicsTick', onPhysics)
    return () => {
      target.removeListener('physicsTick', onPhysics)
      target.removeListener('chunkColumnLoad', onLoad)
      target.removeListener('chunkColumnUnload', onUnload)
      target.removeListener('blockUpdate', onUpdate)
      target.removeListener('end', onEnd)
      target.removeListener('kicked', onEnd)
      if (client) client.removeListener('update_light', onLight)
    }
  }

  // nothing while offline; counters are left to accumulate and go out with the first report after the next attach
  const emitStats = () => {
    if (!bot) return
    onEvent({ source: 'body', kind: 'view.stats', level: 'info', ...takeStats() })
  }

  const startTimers = () => {
    if (timers.length) return
    const every = (ms, fn) => {
      const timer = setInterval(() => { Promise.resolve().then(fn).catch(reportError) }, ms)
      timer.unref()
      return timer
    }
    timers = [
      every(FLUSH_MS, flushColumns),
      // tick mode: physicsTick stops while the column under the body is unloaded or physics is off, so the refresh needs its own timer
      every(poseHz > 0 ? 1000 / poseHz : POSE_REFRESH_MS, tickPose),
      every(HUD_MS, tickHud),
      every(STATS_MS, emitStats)
    ]
  }

  // the server's biome registry (chunk data's biome ids index it), only rewritten when it changed
  const writeBiomes = async target => {
    const list = target.registry?.biomesArray ?? Object.values(target.registry?.biomes ?? {})
    if (!list.length) return
    const biomes = list
      .map(b => ({ id: b.id, name: String(b.name).replace(/^minecraft:/, '') }))
      .sort((a, b) => a.id - b.id)
    const content = JSON.stringify({ v: VIEW_VERSION, mcVersion: target.version, biomes })
    const file = biomesFile(stateDir, world)
    const existing = await fs.promises.readFile(file, 'utf8').catch(() => null)
    if (existing === content) return
    await writeAtomic(file, content)
  }

  const attach = target => {
    unhook()
    overlays.clear()
    changes = new Map()
    bot = target
    mcVersion = target.version
    lastPoseKey = null
    lastBodyKey = null
    lastHudKey = null
    lastPose = null
    unhook = hook(target)
    // columns that arrived before attach (the spawn column) never fire chunkColumnLoad for us
    const loaded = target.world?.getColumns?.() ?? []
    loaded.forEach(({ chunkX, chunkZ }) => markColumn(Number(chunkX), Number(chunkZ)))
    track(writeBiomes(target)).catch(reportError)
    const tableStart = performance.now()
    try { tableFor(target) } catch (err) { reportError(err) }
    stats.relightTableMs += performance.now() - tableStart
    startTimers()
  }

  const detach = () => {
    if (!bot) return Promise.resolve()
    unhook()
    unhook = () => {}
    bot = null
    return track(writePose(JSON.stringify(offlinePose({ world, now: now(), mcVersion, last: lastPose }))))
  }

  const stop = () => {
    timers.forEach(clearInterval)
    timers = []
    unhook()
    unhook = () => {}
    bot = null
  }

  const idle = async () => {
    while (running.size) await Promise.all([...running])
  }

  // the relit light of world section s of column (cx, cz), {sky, block} one byte per cell, or undefined (engine.perception reads light through it)
  const lightOverlay = (cx, cz, s) => overlays.get(`${cx},${cz}`)?.get(s)
  return { attach, detach, stop, markCell, flushColumns, tickPose, tickHud, idle, pendingCount: () => pending.size, stats: takeStats, lightOverlay }
}
