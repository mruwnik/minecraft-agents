// Why JavaScript: worker/binary; schedules the block-scan worker over chunk column files.
import { worldsDir } from '../../engine/js/bodies.mjs'
// Counts, per column file, the blocks the view draws wrong (the flagged names of block-issues.mjs) and where one was first
// seen. The scan runs in a worker thread (block-scan-worker.mjs) so a server mounted in the dashboard never blocks.
import fs from 'node:fs'
import path from 'node:path'
import { Worker } from 'node:worker_threads'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile } from './columns.mjs'
import { columnFormat } from './web-format.mjs'
import { decodeSections } from './web/decode.mjs'
import { textureBytes } from './materials.mjs'
import { loadModels } from './block-models.mjs'
import { classifyStatic } from './block-issues.mjs'
import { findClientJar } from './jar-read.mjs'

export { findClientJar }

// the static records for a version, and everything a scan needs to count them
export function classifyReal ({ version, textureDir, jarPath, table = null }) {
  const built = table ?? textureBytes(version, textureDir, { jarPath }).table
  const materialOf = new Uint16Array(Buffer.from(built.materialOf, 'base64').buffer.slice(0))
  const registry = prismarineRegistry(version)
  const models = jarPath ? loadModels(jarPath) : null
  const records = classifyStatic({ registry, materials: built.materials, materialOf, models })
  return { records, registry, stateCount: built.stateCount }
}

// Lookup over state ids: 0 for a block that is fine, else 1 + the index of its name in `names`
export const flaggedLookup = ({ records, registry, stateCount }) => {
  const names = [...new Set(records.map(r => r.name).filter(n => registry.blocksByName[n]))]
  const lookup = new Uint16Array(stateCount)
  names.forEach((name, i) => {
    const { minStateId, maxStateId } = registry.blocksByName[name]
    lookup.fill(i + 1, minStateId, maxStateId + 1)
  })
  // ids no block owns: the table has nothing for them either
  const known = new Uint8Array(stateCount)
  Object.values(registry.blocksByName).forEach(({ minStateId, maxStateId }) => known.fill(1, minStateId, maxStateId + 1))
  return { names, lookup, known, stateCount }
}

const POSITIONS_KEPT = 5

// One column file's counts: { agent, counts: Map(name -> { count, first: { x, y, z } }), unknown: Map(id -> { count, firsts }) }.
// A state id outside the table's range, or in no block, is unknown (the dump stores ids: it is prismarine's section palette).
// Throws on a file that is not a column.
export function scanColumn (fileBytes, { format, flagged }) {
  const { header, parts } = decodeColumnFile(fileBytes)
  const { ids } = decodeSections(parts.sections, { ...format, numSections: header.worldHeight >> 4 })
  const { names, lookup, known, stateCount } = flagged
  const counts = new Map()
  const unknown = new Map()
  const at = i => ({ x: header.x * 16 + (i & 15), y: header.minY + (i >> 12) * 16 + ((i >> 8) & 15), z: header.z * 16 + ((i >> 4) & 15) })
  const note = (map, key, i, keep) => {
    const held = map.get(key)
    if (!held) return void map.set(key, { count: 1, first: at(i), firsts: [at(i)] })
    held.count++
    if (held.firsts.length < keep) held.firsts.push(at(i))
  }
  for (let i = 0; i < ids.length; i++) {
    const id = ids[i]
    if (id >= stateCount || !known[id]) note(unknown, id, i, POSITIONS_KEPT)
    else if (lookup[id] !== 0) note(counts, names[lookup[id] - 1], i, 1)
  }
  return { agent: header.body ?? null, mcVersion: header.mcVersion ?? null, counts, unknown }
}

export const versionFormat = columnFormat

// consecutive ids merge into one range
const rangesOf = sorted => sorted.reduce((ranges, entry) => {
  const last = ranges[ranges.length - 1]
  if (last && entry.id === last.hi + 1) {
    last.hi = entry.id
    last.entries.push(entry)
    return ranges
  }
  return [...ranges, { lo: entry.id, hi: entry.id, entries: [entry] }]
}, [])

// The keys of `known` (a Map) that are not in `names`: one Set per call, so a sweep over n files stays linear
export const missingFrom = (known, names) => {
  const present = new Set(names)
  return [...known.keys()].filter(name => !present.has(name))
}

// Per-column contributions, so a rewritten column replaces its counts: contributions Map(file -> { agent, counts, unknown }).
// seen() is Map(name -> { count, first: { world, x, y, z, agent }, firsts? }); unknown ids are 'state:<lo>-<hi>' per contiguous range
export function createTotals (world) {
  const contributions = new Map()
  const located = (position, agent) => ({ world, ...position, agent })
  const unknownIds = () => {
    const merged = new Map()
    for (const { agent, unknown } of contributions.values()) {
      for (const [id, { count, firsts }] of unknown) {
        const held = merged.get(id) ?? { id, count: 0, firsts: [] }
        held.count += count
        held.firsts.push(...firsts.map(p => located(p, agent)))
        merged.set(id, held)
      }
    }
    return [...merged.values()].sort((a, b) => a.id - b.id)
  }
  return {
    set: (file, contribution) => contributions.set(file, contribution),
    remove: file => contributions.delete(file),
    files: () => [...contributions.keys()],
    size: () => contributions.size,
    seen: () => {
      const seen = new Map()
      for (const { agent, counts } of contributions.values()) {
        for (const [name, { count, first }] of counts) {
          const held = seen.get(name)
          if (held) held.count += count
          else seen.set(name, { count, first: located(first, agent) })
        }
      }
      for (const { lo, hi, entries } of rangesOf(unknownIds())) {
        const firsts = entries.flatMap(e => e.firsts).slice(0, POSITIONS_KEPT)
        seen.set(`state:${lo}-${hi}`, { count: entries.reduce((n, e) => n + e.count, 0), first: firsts[0], firsts })
      }
      return seen
    }
  }
}

// ---------------------------------------------------------------- the main-thread side

// track(world) starts a scan the first time a world is named; latest(world) resolves with its newest records once the first
// scan is done (null for a world without a chunks directory). The worker also writes worlds/<w>/view-block-issues.json.
export function createBlockScanner ({ stateDir, textureDir, jar, sweepMs = 5000, writeMs = 5000 }) {
  let worker = null
  const worlds = new Map() // world -> { payload, waiters }
  const start = () => {
    if (worker) return worker
    worker = new Worker(new URL('./block-scan-worker.mjs', import.meta.url), { workerData: { stateDir, textureDir, jar: jar === undefined ? findClientJar() : jar, sweepMs, writeMs } })
    worker.unref()
    worker.on('message', message => {
      const entry = worlds.get(message.world)
      if (!entry) return
      entry.payload = message.payload
      entry.waiters.splice(0).forEach(resolve => resolve(message.payload))
    })
    worker.on('error', error => console.error(`[block-scan] ${error.stack ?? error}`))
    return worker
  }
  const track = world => {
    if (worlds.has(world)) return
    if (!fs.existsSync(path.join(worldsDir(stateDir), world, 'chunks'))) return
    worlds.set(world, { payload: null, waiters: [] })
    start().postMessage({ type: 'track', world })
  }
  const latest = world => {
    track(world)
    const entry = worlds.get(world)
    if (!entry) return Promise.resolve(null)
    return entry.payload ? Promise.resolve(entry.payload) : new Promise(resolve => entry.waiters.push(resolve))
  }
  const peek = world => worlds.get(world)?.payload ?? null
  const stop = () => {
    worker?.terminate()
    worker = null
    worlds.forEach(entry => entry.waiters.splice(0).forEach(resolve => resolve(null)))
  }
  return { track, latest, peek, stop }
}
