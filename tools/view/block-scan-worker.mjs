// The scan thread of block-scan.mjs. Per tracked world: an initial scan of the chunk files, then incremental rescans of the
// columns whose mtime changed (a directory watch, with a slow sweep as the fallback). It posts the merged records to the
// main thread after every change and writes state/worlds/<world>/view-block-issues.json (atomically, rarely, on change).
import fs from 'node:fs'
import path from 'node:path'
import { parentPort, workerData } from 'node:worker_threads'
import { classifyReal, flaggedLookup, scanColumn, createTotals, versionFormat } from './block-scan.mjs'
import { decodeColumnFile } from './columns.mjs'
import { mergeSeen } from './block-issues.mjs'

const { stateDir, textureDir, jar, sweepMs, writeMs } = workerData
const FILE_VERSION = 1
const COLUMN_FILE = /^-?\d+\.-?\d+\.bin$/
const STAT_BATCH = 64
const WATCH_DEBOUNCE_MS = 200

const statics = new Map() // mc version -> { records, flagged, format }
const staticFor = version => {
  if (statics.has(version)) return statics.get(version)
  const real = classifyReal({ version, textureDir, jarPath: jar })
  const built = { records: real.records, flagged: flaggedLookup(real), format: versionFormat(version) }
  statics.set(version, built)
  return built
}

const readDir = async dir => (await fs.promises.readdir(dir).catch(() => [])).filter(name => COLUMN_FILE.test(name))
const mtimeOf = async file => (await fs.promises.stat(file).catch(() => null))?.mtimeMs ?? null
const readOrNull = file => fs.promises.readFile(file).catch(() => null)

const writeAtomic = async (file, text) => {
  const tmp = `${file}.${process.pid}.tmp`
  await fs.promises.writeFile(tmp, text)
  await fs.promises.rename(tmp, file)
}

const trackWorld = async world => {
  const dir = path.join(stateDir, 'worlds', world, 'chunks')
  const out = path.join(stateDir, 'worlds', world, 'view-block-issues.json')
  const totals = createTotals(world)
  const mtimes = new Map()
  const dirty = new Set()
  let version = null
  let built = null
  let scan = { columns: 0, ms: 0 }
  let lastText = null
  let lastWrite = 0
  let writeTimer = null

  const publish = () => {
    const records = built ? mergeSeen(built.records, totals.seen()) : []
    const payload = { version: FILE_VERSION, mcVersion: version, jar: jar ? path.basename(jar) : null, updatedAt: new Date().toISOString(), scan, records }
    parentPort.postMessage({ world, payload })
    if (built) scheduleWrite(payload)
  }
  const scheduleWrite = payload => {
    const text = JSON.stringify({ ...payload, updatedAt: null })
    if (text === lastText) return
    clearTimeout(writeTimer)
    const wait = Math.max(0, lastWrite + writeMs - Date.now())
    writeTimer = setTimeout(async () => {
      lastText = text
      lastWrite = Date.now()
      await writeAtomic(out, JSON.stringify(payload, null, 1)).catch(error => console.error(`[block-scan] ${out}: ${error.message}`))
    }, wait)
    writeTimer.unref()
  }

  const rescan = async name => {
    const file = path.join(dir, name)
    const mtime = await mtimeOf(file)
    if (mtime === null) {
      mtimes.delete(name)
      return totals.remove(name)
    }
    mtimes.set(name, mtime)
    const bytes = await readOrNull(file)
    if (!bytes) return false
    const contribution = (() => {
      try {
        return scanColumn(bytes, built)
      } catch {
        return null // half-written or foreign file: the next sweep or watch event retries
      }
    })()
    if (!contribution) return totals.remove(name)
    totals.set(name, contribution)
    return true
  }

  // the version the world's columns were dumped with, from the first file that reads
  const detectVersion = async names => {
    for (const name of names) {
      const bytes = await readOrNull(path.join(dir, name))
      if (!bytes) continue
      const found = (() => {
        try {
          return decodeColumnFile(bytes).header.mcVersion
        } catch {
          return null
        }
      })()
      if (found) return found
    }
    return null
  }

  const settle = async names => {
    const changed = []
    for (let i = 0; i < names.length; i += STAT_BATCH) {
      const batch = names.slice(i, i + STAT_BATCH)
      const stats = await Promise.all(batch.map(name => mtimeOf(path.join(dir, name))))
      batch.forEach((name, j) => mtimes.get(name) !== stats[j] && changed.push(name))
    }
    return changed
  }

  const sweep = async () => {
    const names = await readDir(dir)
    const gone = [...mtimes.keys()].filter(name => !names.includes(name))
    if (!built) {
      version = await detectVersion(names)
      if (version) built = staticFor(version)
    }
    if (!built) return false
    const changed = [...new Set([...await settle(names), ...dirty, ...gone])]
    dirty.clear()
    for (const name of changed) await rescan(name)
    return changed.length > 0
  }

  const started = performance.now()
  await sweep()
  scan = { columns: totals.size(), ms: Math.round(performance.now() - started) }
  publish()

  let running = Promise.resolve()
  const again = () => (running = running.then(async () => (await sweep()) && publish()).catch(error => console.error(`[block-scan] ${world}: ${error.message}`)))
  setInterval(again, sweepMs).unref()
  try {
    const watcher = fs.watch(dir, (_event, name) => {
      if (!COLUMN_FILE.test(name ?? '')) return
      dirty.add(name)
      setTimeout(again, WATCH_DEBOUNCE_MS).unref()
    })
    watcher.on('error', () => watcher.close())
    watcher.unref()
  } catch {
    // the sweep covers it
  }
}

const tracked = new Set()
parentPort.on('message', message => {
  if (message.type !== 'track' || tracked.has(message.world)) return
  tracked.add(message.world)
  trackWorld(message.world).catch(error => console.error(`[block-scan] ${message.world}: ${error.stack}`))
})
