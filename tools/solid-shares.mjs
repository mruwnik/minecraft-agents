// Why JavaScript: binary data; decodes recorded chunk columns (prismarine-chunk).
// Writes engine/src/jobs/lib/cost/solid_shares.json, the prior jobs.lib.cost.base-rates/solid-prior reads: per section
// height (y, its lowest block) a Beta {a b} fitted by moments to the share of solid among the blocks sight meets (the open
// blocks and the solid ones beside them: walls) in every 3x3x3-section window centred there, over the columns the bodies have recorded (worlds/<world>/chunks: the game's terrain as the server sent
// it, not what a body saw), less the fixture plot grid and its lanes (built courses, not terrain). Solid is the engine's rule
// (jobs.lib.walk.rock/open-ids), read from the planner's block table in the compile cache, so run any engine build or test
// first. Rerun when the world or the game version changes:
//   node tools/solid-shares.mjs [chunks-dir]
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile } from '../engine/js/view-column.mjs'
import { MC_VERSION } from '../engine/js/path/snapshot.mjs'
import { decode } from '../engine/js/table-cache.mjs'

const ROOT = path.join(import.meta.dirname, '..')
const OUT = path.join(ROOT, 'engine/src/jobs/lib/cost/solid_shares.json')
const SECTION = 4096

// engine.path.blocks SOLID and NARROW: the kinds rock/open-ids counts solid (doors, ladders, gates and fluids are open)
const SOLID_KINDS = [1, 6]

// 1 by state id for a block the engine counts solid, by the planner block table's kind array
export const solidStates = kind => Uint8Array.from(kind, k => SOLID_KINDS.includes(k) ? 1 : 0)

// The newest planner block table in the compile cache (js/table-cache.mjs), or an error naming what to do
const engineTable = () => {
  const dir = path.join(process.env.MC_COMPILE_CACHE_DIR || path.join(ROOT, 'state', 'compile-cache'), 'tables')
  const files = fs.existsSync(dir) ? fs.readdirSync(dir).filter(f => /^state-table-\w+\.bin$/.test(f)) : []
  if (!files.length) throw new Error(`no planner block table in ${dir}: run an engine build or test first`)
  const newest = files.map(f => path.join(dir, f)).sort((p, q) => fs.statSync(q).mtimeMs - fs.statSync(p).mtimeMs)[0]
  return decode(fs.readFileSync(newest))
}

// world-test.fixture default-grid x0 z0 less 64, as world-test.water-lags/reserved?: the plot grid and its lanes
export const reserved = (cx, cz) => 16 * cx >= 20000 - 64 && 16 * cz >= 20000 - 64

// {a b} of a Beta with mean m and variance v of the share; a share known exactly (v 0) is worth one whole window of cells
export const betaOf = (m, v, cells) => {
  const strength = v > 0 ? Math.min(cells, Math.max(1e-3, m * (1 - m) / v - 1)) : cells
  return { a: m * strength, b: (1 - m) * strength }
}

// [{y a b n}] by section from columns: Map "cx,cz" -> {wall open} Int32Arrays of counts per section, lowest first, minY the
// lowest block; a window with no open block (sight meets nothing there) is no sample
export const fitWindows = (columns, minY) => {
  const acc = new Map()
  for (const [key, { wall, open }] of columns) {
    const [cx, cz] = key.split(',').map(Number)
    const around = []
    for (let dx = -1; dx <= 1; dx++) for (let dz = -1; dz <= 1; dz++) around.push(columns.get(`${cx + dx},${cz + dz}`))
    if (around.some(c => !c)) continue
    for (let sy = 0; sy < wall.length; sy++) {
      let w = 0
      let o = 0
      for (let d = -1; d <= 1; d++) {
        if (sy + d < 0 || sy + d >= wall.length) continue
        around.forEach(c => { w += c.wall[sy + d]; o += c.open[sy + d] })
      }
      if (o === 0) continue
      const share = w / (w + o)
      const e = acc.get(sy) ?? { n: 0, sum: 0, sq: 0, cells: 0 }
      e.n++; e.sum += share; e.sq += share * share; e.cells += w + o
      acc.set(sy, e)
    }
  }
  return [...acc].sort((p, q) => p[0] - q[0]).map(([sy, { n, sum, sq, cells }]) => {
    const m = sum / n
    const { a, b } = betaOf(m, Math.max(0, sq / n - m * m), cells / n)
    return { y: minY + 16 * sy, a: Number(a.toPrecision(4)), b: Number(b.toPrecision(4)), n }
  })
}

// {wall open} Int32Arrays per section of a column from solid (1 by block, section by section, lowest first): its open
// blocks, and its solid ones with an open block beside them (the six faces, those across the column's sides not counted)
export const columnCounts = solid => {
  const n = solid.length / SECTION
  const wall = new Int32Array(n)
  const open = new Int32Array(n)
  const last = solid.length - 256
  for (let j = 0; j < solid.length; j++) {
    const sy = (j / SECTION) | 0
    if (!solid[j]) { open[sy]++; continue }
    const x = j & 15
    const z = (j >> 4) & 15
    if ((x > 0 && !solid[j - 1]) || (x < 15 && !solid[j + 1]) || (z > 0 && !solid[j - 16]) || (z < 15 && !solid[j + 16]) ||
        (j >= 256 && !solid[j - 256]) || (j < last && !solid[j + 256])) wall[sy]++
  }
  return { wall, open }
}

const main = dir => {
  const ChunkColumn = prismarineChunk(prismarineRegistry(MC_VERSION))
  const solid = solidStates(engineTable().kind)
  const columns = new Map()
  let minY = null
  for (const name of fs.readdirSync(dir)) {
    const m = /^(-?\d+)\.(-?\d+)\.bin$/.exec(name)
    if (!m || reserved(Number(m[1]), Number(m[2]))) continue
    let decoded
    try { decoded = decodeColumnFile(fs.readFileSync(path.join(dir, name))) } catch { continue } // a file cut short by a write
    const column = new ChunkColumn({ minY: decoded.header.minY, worldHeight: decoded.header.worldHeight })
    column.load(decoded.sections)
    minY ??= decoded.header.minY
    const flags = new Uint8Array(column.sections.length * SECTION)
    column.sections.forEach((s, sy) => { for (let i = 0; i < SECTION; i++) flags[sy * SECTION + i] = solid[s.data.get(i)] })
    columns.set(`${m[1]},${m[2]}`, columnCounts(flags))
  }
  const sections = fitWindows(columns, minY)
  fs.writeFileSync(OUT, `{"version": "${MC_VERSION}", "columns": ${columns.size}, "sections": [\n${sections.map(e => ' ' + JSON.stringify(e)).join(',\n')}]}\n`)
  console.log(`${columns.size} columns, ${sections.length} section heights -> ${path.relative(ROOT, OUT)}`)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main(process.argv[2] ?? path.join(ROOT, 'worlds/test/chunks'))
