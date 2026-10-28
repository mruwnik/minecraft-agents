// Why JavaScript: binary data; decodes recorded chunk columns (prismarine-chunk).
// Writes engine/src/jobs/lib/cost/solid_shares.json, the prior jobs.lib.cost.base-rates/solid-prior reads: per section
// height (y, its lowest block) a Beta {a b} fitted by moments to the share of solid among the blocks sight meets (the open
// blocks and the solid ones beside them: walls) in every 3x3x3-section window centred there, over the columns the bodies have recorded (worlds/<world>/chunks: the game's terrain as the server sent
// it, not what a body saw), less the fixture plot grid and its lanes (built courses, not terrain); and as reach, by
// source and target section, the slope of a window's share on the share at another height of the same column (shrunk
// toward 0 by its sampling variance, in [0, 1]) and as reach-n the windows it was fitted over. Solid is the engine's rule
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
export const engineTable = () => {
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

// Each window centred on a column whose 9 columns are all recorded, as [{share cells}] by section: its share wall /
// (wall + open) and its wall + open, share NaN where empty? says the window is no sample
const windowShares = function* (columns, empty) {
  for (const [key, { wall }] of columns) {
    const [cx, cz] = key.split(',').map(Number)
    const around = []
    for (let dx = -1; dx <= 1; dx++) for (let dz = -1; dz <= 1; dz++) around.push(columns.get(`${cx + dx},${cz + dz}`))
    if (around.some(c => !c)) continue
    yield Array.from(wall, (_, sy) => {
      let w = 0
      let o = 0
      for (let d = -1; d <= 1; d++) {
        if (sy + d < 0 || sy + d >= wall.length) continue
        around.forEach(c => { w += c.wall[sy + d]; o += c.open[sy + d] })
      }
      return { share: empty(w, o) ? NaN : w / (w + o), cells: w + o }
    })
  }
}

// [{y a b n}] by section from columns: Map "cx,cz" -> {wall open} Int32Arrays of counts per section, lowest first, minY the
// lowest block, the share fitted wall / (wall + open); a window empty? says is no sample (default: no open block, sight
// meets nothing there)
export const fitWindows = (columns, minY, empty = (w, o) => o === 0) => {
  const acc = new Map()
  for (const windows of windowShares(columns, empty)) {
    windows.forEach(({ share, cells }, sy) => {
      if (Number.isNaN(share)) return
      const e = acc.get(sy) ?? { n: 0, sum: 0, sq: 0, cells: 0 }
      e.n++; e.sum += share; e.sq += share * share; e.cells += cells
      acc.set(sy, e)
    })
  }
  return [...acc].sort((p, q) => p[0] - q[0]).map(([sy, { n, sum, sq, cells }]) => {
    const m = sum / n
    const { a, b } = betaOf(m, Math.max(0, sq / n - m * m), cells / n)
    return { y: minY + 16 * sy, a: Number(a.toPrecision(4)), b: Number(b.toPrecision(4)), n }
  })
}

// Under a Normal(0, 1) prior on the slope, the residual variance taken from the fit, the posterior mean of a share's
// slope on another's from a pair's sums, cut to [0, 1] (a seen share cannot say more about another height than about its
// own, nor less than nothing): a source whose share barely varies carries no evidence, so its slope stays near 0
export const shrunkSlope = ({ n, x, y, xy, xx, yy }) => {
  if (n < 3) return 0
  const sxx = Math.max(0, xx - x * x / n)
  const sxy = xy - x * y / n
  const syy = Math.max(0, yy - y * y / n)
  const residual = (sxx > 0 ? Math.max(0, syy - sxy * sxy / sxx) : syy) / (n - 2)
  return sxx + residual > 0 ? Math.min(1, Math.max(0, sxy / (sxx + residual))) : 0
}

// {slope n}, [[by target section] by source section]: shrunkSlope of a window's share on the share of the window centred
// on the same column at the source section, over the n columns where both are samples (1 on the diagonal): how far a
// share seen at one height moves the share expected at another
export const fitReach = (columns, empty = (w, o) => o === 0) => {
  let acc = null
  for (const windows of windowShares(columns, empty)) {
    const n = windows.length
    acc ??= Array.from({ length: n * n }, () => ({ n: 0, x: 0, y: 0, xy: 0, xx: 0, yy: 0 }))
    for (let s = 0; s < n; s++) {
      const x = windows[s].share
      if (Number.isNaN(x)) continue
      for (let t = 0; t < n; t++) {
        const y = windows[t].share
        if (Number.isNaN(y)) continue
        const e = acc[s * n + t]
        e.n++; e.x += x; e.y += y; e.xy += x * y; e.xx += x * x; e.yy += y * y
      }
    }
  }
  if (!acc) return { slope: [], n: [] }
  const n = Math.sqrt(acc.length)
  const rows = f => Array.from({ length: n }, (_, s) => Array.from({ length: n }, (_, t) => f(acc[s * n + t], s === t)))
  return { slope: rows((e, diagonal) => diagonal ? 1 : Number(shrunkSlope(e).toPrecision(3))), n: rows(e => e.n) }
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
  const reach = fitReach(columns)
  const rows = xs => xs.map(e => ' ' + JSON.stringify(e)).join(',\n')
  fs.writeFileSync(OUT, `{"version": "${MC_VERSION}", "columns": ${columns.size}, "sections": [\n${rows(sections)}],\n"reach": [\n${rows(reach.slope)}],\n"reach-n": [\n${rows(reach.n)}]}\n`)
  console.log(`${columns.size} columns, ${sections.length} section heights -> ${path.relative(ROOT, OUT)}`)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main(process.argv[2] ?? path.join(ROOT, 'worlds/test/chunks'))
