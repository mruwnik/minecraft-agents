#!/usr/bin/env node
// Why JavaScript: reads the recorded chunk columns (binary) through engine/js/view-column.mjs and prismarine-chunk.
// Recounts the drowned meet rate in water (jobs.lib.cost.value ::drowned-meet-per-s) from the trial bodies' event logs.
// Exposure: the seconds between two position samples of one body run (at most --gap-s apart) with its feet in water,
// counted to the first sample's class. Meets: the first hostile decision about a drowned of each respond job, counted
// to the class of the body's cell. Classes split what vanilla's drowned spawn rule splits: river or ocean biome versus
// other water, the feet cell at block light 0 with sky light 7 or less / block light 0 / lit, and whether its column holds
// a dark water cell over water (deep: a drowned can spawn there).
// Light and biome come from the recorded chunks (their light now, not at the sample's time). prior: the two classes
// the setting keeps (a cell a drowned can spawn in, any other water), each (k + 1) / n a second.
// usage: node tools/drowned-prior.mjs [--agents DIR] [--chunks DIR] [--skip REGEX] [--gap-s S] [--since-ms MS]
import fs from 'node:fs'
import path from 'node:path'
import zlib from 'node:zlib'
import { createRequire } from 'node:module'
import { decodeColumnFile, restoreColumn } from '../engine/js/view-column.mjs'
import { MC_VERSION } from '../engine/js/path/snapshot.mjs'

const require = createRequire(import.meta.url)
const registry = require('prismarine-registry')(MC_VERSION)
const ChunkColumn = require('prismarine-chunk')(registry)
const root = path.join(import.meta.dirname, '..')

const arg = (name, dflt) => {
  const i = process.argv.indexOf(name)
  return i < 0 ? dflt : process.argv[i + 1]
}
const agents = arg('--agents', path.join(root, 'worlds', 'test', 'agents'))
const chunks = arg('--chunks', path.join(agents, '..', 'chunks'))
const skip = new RegExp(arg('--skip', '^(Claude)?Probe'))
const gapMs = Number(arg('--gap-s', 10)) * 1000
const sinceMs = Number(arg('--since-ms', 0))
const biomeNames = Object.fromEntries(JSON.parse(fs.readFileSync(path.join(chunks, '..', 'biomes.json'))).biomes.map(b => [b.id, b.name]))

const columns = new Map()
function column (x, z) {
  const key = `${Math.floor(x / 16)}.${Math.floor(z / 16)}`
  if (!columns.has(key)) {
    const file = path.join(chunks, `${key}.bin`)
    let col = null
    if (fs.existsSync(file)) {
      const d = decodeColumnFile(fs.readFileSync(file))
      col = restoreColumn(new ChunkColumn({ minY: d.header.minY, worldHeight: d.header.worldHeight }), d)
    }
    columns.set(key, col)
  }
  return columns.get(key)
}

const water = (col, p) => registry.blocksByStateId[col.getBlockStateId(p)]?.name === 'water'

// whether the water column from feet cell p down holds a cell at block light 0 over water (jobs.lib.look/water-fn's class 2)
function spawnable (col, p) {
  for (let y = p.y; water(col, { ...p, y }); y--) {
    if (!water(col, { ...p, y: y - 1 })) return false
    if (col.getBlockLight({ ...p, y }) === 0) return true
  }
  return false
}

// the class of the feet cell of a position, nil when it is not water or its chunk was never recorded
function classOf ({ x, y, z }) {
  const col = column(x, z)
  if (!col) return null
  const p = { x: ((Math.floor(x) % 16) + 16) % 16, y: Math.floor(y), z: ((Math.floor(z) % 16) + 16) % 16 }
  if (!water(col, p)) return null
  const biome = biomeNames[col.getBiome(p)] ?? ''
  const where = /river|ocean/.test(biome) ? 'river-ocean' : 'other'
  const light = col.getBlockLight(p) > 0 ? 'lit' : col.getSkyLight(p) <= 7 ? 'dark-by-day' : 'dark-at-night'
  return `${where}/${light}/${spawnable(col, p) ? 'deep' : 'shallow'}`
}

const num = (re, line) => { const m = re.exec(line); return m ? Number(m[1]) : null }
const POS = /:pos \{:x (-?[\d.E-]+), :y (-?[\d.E-]+), :z (-?[\d.E-]+)\}/

function records (dir) {
  const names = fs.readdirSync(dir).filter(f => /^events\.edn(\.debug)?(\.\d+)?(\.gz)?$/.test(f))
  const have = new Set(names)
  return names.filter(f => !(f.endsWith('.gz') && have.has(f.slice(0, -3)))).flatMap(f => {
    const raw = fs.readFileSync(path.join(dir, f))
    const text = (f.endsWith('.gz') ? zlib.gunzipSync(raw) : raw).toString('utf8')
    return text.split('\n').flatMap(line => {
      const t = num(/:time-ms (\d+)/, line)
      const gen = /:generation-id "([^"]*)"/.exec(line)?.[1]
      const m = POS.exec(line)
      if (t == null || !gen || !m || t < sinceMs) return []
      const meet = line.includes(':kind :hostile.decided') && line.includes(':mob "drowned"')
        ? /:job-id "([^"/]*)/.exec(line)?.[1] : null
      return [{ t, gen, meet, pos: { x: Number(m[1]), y: Number(m[2]), z: Number(m[3]) } }]
    })
  }).sort((a, b) => a.t - b.t)
}

const seconds = {}
const meets = {}
for (const body of fs.readdirSync(agents)) {
  const dir = path.join(agents, body, 'engine')
  if (skip.test(body) || !fs.existsSync(dir)) continue
  const met = new Set()
  let prev = null
  for (const r of records(dir)) {
    const cls = classOf(r.pos)
    if (prev && prev.gen === r.gen && prev.cls && r.t - prev.t <= gapMs) seconds[prev.cls] = (seconds[prev.cls] ?? 0) + (r.t - prev.t) / 1000
    if (r.meet && cls && !met.has(`${r.gen}/${r.meet}`)) {
      met.add(`${r.gen}/${r.meet}`)
      meets[cls] = (meets[cls] ?? 0) + 1
    }
    prev = { ...r, cls }
  }
}
const rows = Object.keys({ ...seconds, ...meets }).sort().map(cls => ({ class: cls, k: meets[cls] ?? 0, n: Math.round(seconds[cls] ?? 0) }))
// the setting's two classes: water a drowned can spawn in (spawnable: deep) and any other water
const deep = cls => /\/deep$/.test(cls)
const pooled = pick => rows.filter(pick).reduce((a, r) => ({ k: a.k + r.k, n: a.n + r.n }), { k: 0, n: 0 })
const prior = Object.fromEntries([['deep', pooled(r => deep(r.class))], ['other', pooled(r => !deep(r.class))]]
  .map(([c, { k, n }]) => [c, { k, n, perS: n > 0 ? (k + 1) / n : null }]))
console.log(JSON.stringify({ gapS: gapMs / 1000, sinceMs, prior, rows }, null, 1))
