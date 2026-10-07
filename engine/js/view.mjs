// Why JavaScript: binary/graphics; column encoding, light overlay and atomic writers for the view dumps.
// View dump: what the body knows about the world, written to disk for an external renderer. Format: docs/view-format.md.
// The body pays no rendering cost here: it copies mineflayer's packed chunk data, deflates off the main thread and writes
// files. The one cell walk is the bounded local relight of the light overlay (light.mjs).
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { createHash } from 'node:crypto'
import { promisify } from 'node:util'
import { relightBox } from './light.mjs'
import { bodyDir, worldsDir } from './bodies.mjs'
import { RELIGHT_BUDGET_MS, RELIGHT_MAX_POINTS, RELIGHT_REACH, columnLightSection, columnStateSection, hasSkyLight, tableFor, mergeOverlapping } from './view-light.mjs'
import { VIEW_VERSION, encodeColumn } from './view-column.mjs'
import { poseJson, poseSnapshot, bodyKey, poseKey, offlinePose, hudSnapshot, hudKey } from './view-pose.mjs'
export * from './view-light.mjs'
export * from './view-column.mjs'
export * from './view-pose.mjs'

const deflate = promisify(zlib.deflate)

export const FLUSH_MS = 500
export const MAX_COLUMNS_PER_FLUSH = 8
export const POSE_HZ = 0
export const POSE_REFRESH_MS = 2000
// a pose whose only change is the surroundings (mobs, time, rain) is written at most this often; the body's own move is capped by POSE_BODY_MS
export const POSE_SURROUND_MS = 500
// the body's own small moves are written at most this often (the viewer interpolates); kept under 2 physics ticks (100 ms) less ~20 ms of tick jitter, so a write lands on every 2nd tick; a jump past POSE_JUMP blocks or a dimension change goes out at once
export const POSE_BODY_MS = 80
export const POSE_JUMP = 2.5
export const HUD_MS = 1000
export const STATS_MS = 60000
export const ERROR_EVERY_MS = 60000

// ---- files ----

// the overworld keeps 'chunks'; another dimension has its own folder (chunks-the_nether), so its columns never overwrite the overworld's.
// The name comes from the server: ':' becomes '_', anything but [a-z0-9_] gets no folder (null) and nothing is written.
const chunksFolder = dimension => {
  const name = (dimension ?? 'overworld').replace(/^minecraft:/, '').replace(/:/g, '_')
  if (name === 'overworld') return 'chunks'
  return /^[a-z0-9_]+$/.test(name) ? `chunks-${name}` : null
}
export const columnFile = (stateDir, world, cx, cz, dimension) => {
  const folder = chunksFolder(dimension)
  return folder && path.join(worldsDir(stateDir), world, folder, `${cx}.${cz}.bin`)
}
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
  await fs.promises.rename(tmp, file).catch(async err => {
    await fs.promises.rm(tmp, { force: true })
    throw err
  })
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


export function createView ({ stateDir, agent, world, onEvent = () => {}, now = Date.now, enabled = process.env.BODY_VIEW !== '0', poseHz = poseHzFromEnv(), relightBudgetMs = RELIGHT_BUDGET_MS, relightMaxPoints = RELIGHT_MAX_POINTS }) {
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
    const all = [...changes.values()]
    const queued = all.slice(0, relightMaxPoints)
    changes = new Map()
    // a huge backlog is not boxed in one go (boxFor and the merge cost grow with it): the rest waits for the next flush
    for (const p of all.slice(relightMaxPoints)) {
      changes.set(`${p.x},${p.y},${p.z}`, p)
      blocked.add(`${p.x >> 4},${p.z >> 4}`)
      stats.relightCarried++
    }
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
    const hasSky = hasSkyLight(target.game?.dimension)
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
    const lightOf = (info, s) => info.light[s] ??= columnLightSection(info.column, s, hasSky)
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
            sky[i] = hasSky ? 15 : 0
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

  // content hash (header excluded: it holds the timestamp) of the last column file written, so a reloaded unchanged column is not rewritten
  const written = new Map()
  const contentHash = raw => createHash('sha1').update(raw.subarray(4 + raw.readUInt32LE(0))).digest('hex')

  const writeColumn = async (key, raw, hash, dimension) => {
    const [cx, cz] = key.split(',').map(Number)
    const file = columnFile(stateDir, world, cx, cz, dimension)
    if (!file) return
    const data = await deflate(raw, { level: 1 })
    await writeAtomic(file, data)
    written.set(key, hash)
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
      const hash = contentHash(raw)
      if (written.get(key) === hash) continue
      inflight.add(key)
      writes.push(writeColumn(key, raw, hash, target.game?.dimension).catch(reportError).finally(() => inflight.delete(key)))
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
