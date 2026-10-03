// View dump: what the body knows about the world, written to disk for an external renderer. Format: docs/view-format.md.
// The body pays no rendering cost here: it copies mineflayer's packed chunk data, deflates off the main thread and writes
// files. Nothing in this module walks blocks.
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { promisify } from 'node:util'

const deflate = promisify(zlib.deflate)

export const VIEW_VERSION = 1
export const FLUSH_MS = 500
export const MAX_COLUMNS_PER_FLUSH = 8
export const POSE_HZ = 10
export const POSE_REFRESH_MS = 2000
export const HUD_MS = 1000
export const STATS_MS = 60000
export const ERROR_EVERY_MS = 60000
export const ENTITY_RANGE = 48
export const LIGHT_SECTION_BYTES = 2048

const round2 = n => Math.round(n * 100) / 100
const xyz = v => ({ x: v.x, y: v.y, z: v.z })

// ---- files ----

export const columnFile = (stateDir, world, cx, cz) => path.join(stateDir, 'worlds', world, 'chunks', `${cx}.${cz}.bin`)
export const poseFile = (stateDir, agent) => path.join(stateDir, 'agents', agent, 'view', 'pose.json')
export const hudFile = (stateDir, agent) => path.join(stateDir, 'agents', agent, 'view', 'hud.json')

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

// ---- chunk column encoding ----

// the light part: sky buffers then block buffers, each LIGHT_SECTION_BYTES; the meta says how to restore them
const encodeLight = column => {
  if (!column.dumpLight) return { buffer: Buffer.alloc(0), meta: {} }
  const light = column.dumpLight()
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

// the uncompressed file content: uint32le header length, JSON header, then the parts
export function encodeColumn ({ column, x, z, t, body, mcVersion }) {
  const sections = column.dump()
  const biomes = column.dumpBiomes?.() ?? Buffer.alloc(0)
  const light = encodeLight(column)
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

const entityView = e => ({
  id: e.id,
  type: e.type ?? null,
  name: e.name ?? null,
  kind: e.kind ?? null,
  ...(e.username ? { username: e.username } : {}),
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
    entities: Object.values(bot.entities ?? {}).filter(e => e !== self && e.position && near(e.position, p)).map(entityView),
    timeOfDay: bot.time?.timeOfDay ?? null,
    rain: bot.rainState ?? 0
  }
}

// a string that changes when the pose does: time ignored, numbers rounded to two decimals
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

const noView = Object.freeze({
  attach: () => {}, detach: async () => {}, stop: () => {}, flushColumns: async () => {}, tickPose: async () => {}, tickHud: async () => {},
  idle: async () => {}, pendingCount: () => 0, stats: () => ({ columns: 0, bytes: 0, ms: 0, poses: 0, poseMs: 0, poseBytes: 0, huds: 0 })
})

// BODY_VIEW_POSE_HZ: pose writes per second at most; 0 = uncapped (every physics tick); default POSE_HZ
export function poseHzFromEnv (env = process.env) {
  const hz = Number(env.BODY_VIEW_POSE_HZ ?? POSE_HZ)
  return Number.isFinite(hz) && hz >= 0 ? hz : POSE_HZ
}

const zeroStats = () => ({ columns: 0, bytes: 0, ms: 0, poses: 0, poseMs: 0, poseBytes: 0, huds: 0 })

// One writer per body. `attach(bot)` hooks a bot (call again for each new bot after a reconnect), `detach()` writes the
// offline pose. `onEvent(event)` receives view.stats and view.error. BODY_VIEW=0 turns it all off.
export function createView ({ stateDir, agent, world, onEvent = () => {}, now = Date.now, enabled = process.env.BODY_VIEW !== '0', poseHz = poseHzFromEnv() }) {
  if (!enabled) return noView
  let bot = null
  let unhook = () => {}
  let timers = []
  let stats = zeroStats()
  let lastPoseKey = null
  let lastPoseAt = 0
  let lastPose = null
  let lastHudKey = null
  let lastErrorAt = -Infinity
  let mcVersion = null
  const pending = new Set()
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
  const onLoad = safely(point => markColumn(Math.floor(point.x / 16), Math.floor(point.z / 16)))
  const onUpdate = safely((oldBlock, newBlock) => {
    const p = (newBlock ?? oldBlock)?.position
    if (p) markColumn(Math.floor(p.x / 16), Math.floor(p.z / 16))
  })

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
    for (const key of [...pending]) {
      if (writes.length >= MAX_COLUMNS_PER_FLUSH) break
      if (inflight.has(key)) continue
      pending.delete(key)
      const [cx, cz] = key.split(',').map(Number)
      const raw = (() => {
        try {
          return timed(() => {
            const column = target.world.getColumn(cx, cz)
            return column ? encodeColumn({ column, x: cx, z: cz, t: now(), body: agent, mcVersion: target.version }) : null
          })
        } catch (err) { reportError(err); return null }
      })()
      if (!raw) continue
      inflight.add(key)
      writes.push(writeColumn(key, raw).catch(reportError).finally(() => inflight.delete(key)))
    }
    return track(Promise.all(writes))
  }

  const writePose = coalescedWriter(poseFile(stateDir, agent), err => reportError(err))
  const writeHud = coalescedWriter(hudFile(stateDir, agent), err => reportError(err))

  // main-thread time here (snapshot, change key, stringify) is counted as poseMs, apart from the column ms
  const tickPose = () => {
    if (!bot) return Promise.resolve()
    const start = performance.now()
    const json = (() => {
      try {
        const t = now()
        const pose = poseSnapshot(bot, { world, now: t })
        const key = poseKey(pose)
        if (key === lastPoseKey && t - lastPoseAt < POSE_REFRESH_MS) return null
        lastPoseKey = key
        lastPoseAt = t
        lastPose = pose
        return JSON.stringify(pose)
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
    const out = { ...stats, ms: Math.round(stats.ms * 1000) / 1000, poseMs: Math.round(stats.poseMs * 1000) / 1000 }
    stats = zeroStats()
    return out
  }

  const onPhysics = () => { tickPose().catch(reportError) }

  const hook = target => {
    const onEnd = () => { if (bot === target) detach() }
    target.on('chunkColumnLoad', onLoad)
    target.on('blockUpdate', onUpdate)
    target.on('end', onEnd)
    target.on('kicked', onEnd)
    if (poseHz === 0) target.on('physicsTick', onPhysics)
    return () => {
      target.removeListener('physicsTick', onPhysics)
      target.removeListener('chunkColumnLoad', onLoad)
      target.removeListener('blockUpdate', onUpdate)
      target.removeListener('end', onEnd)
      target.removeListener('kicked', onEnd)
    }
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
      ...(poseHz > 0 ? [every(1000 / poseHz, tickPose)] : []),
      every(HUD_MS, tickHud),
      every(STATS_MS, () => onEvent({ source: 'body', kind: 'view.stats', level: 'info', ...takeStats() }))
    ]
  }

  const attach = target => {
    unhook()
    bot = target
    mcVersion = target.version
    lastPoseKey = null
    lastHudKey = null
    lastPose = null
    unhook = hook(target)
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

  return { attach, detach, stop, flushColumns, tickPose, tickHud, idle, pendingCount: () => pending.size, stats: takeStats }
}
