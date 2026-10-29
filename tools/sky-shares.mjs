// Why JavaScript: binary data; decodes recorded chunk columns and their light (prismarine-chunk).
// Writes engine/src/jobs/lib/cost/sky_shares.json, the prior jobs.lib.cost.base-rates/sky-share reads: per section height
// (y, its lowest block) and sky light level L 1..15, a Beta fitted by moments to the share of the open blocks sky light L
// or more reaches in every 3x3x3-section window centred there ({y a b}, a and b by L - 1), over the columns the bodies have recorded (worlds/<world>/chunks:
// the game's terrain as the server sent it, not what a body saw), less the fixture plot grid and its lanes (built courses,
// not terrain). Open is the engine's rule (jobs.lib.walk.rock/open-ids), read from the planner's block table in the compile
// cache, so run any engine build or test first. Rerun when the world or the game version changes:
//   node tools/sky-shares.mjs [chunks-dir]
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile, restoreColumn } from '../engine/js/view-column.mjs'
import { MC_VERSION } from '../engine/js/path/snapshot.mjs'
import { decode } from '../engine/js/table-cache.mjs'

const ROOT = path.join(import.meta.dirname, '..')
const OUT = path.join(ROOT, 'engine/src/jobs/lib/cost/sky_shares.json')
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


// [{wall open}] by sky light level 1..15 of Int32Arrays per section of a column for fitWindows: wall its open blocks at that
// sky light or more, open its other open blocks; solid 1 by block and sky its sky light by block, lowest section first
export const columnSky = (solid, sky) => {
  const n = solid.length / SECTION
  const hist = new Int32Array(n * 16)
  for (let j = 0; j < solid.length; j++) if (!solid[j]) hist[((j / SECTION) | 0) * 16 + sky[j]]++
  return Array.from({ length: 15 }, (_, l) => {
    const wall = new Int32Array(n)
    const open = new Int32Array(n)
    for (let sy = 0; sy < n; sy++) for (let v = 0; v < 16; v++) (v > l ? wall : open)[sy] += hist[sy * 16 + v]
    return { wall, open }
  })
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
    if (!decoded.light.meta.sectionBytes) continue // recorded without light
    const column = restoreColumn(new ChunkColumn({ minY: decoded.header.minY, worldHeight: decoded.header.worldHeight }), decoded)
    minY ??= decoded.header.minY
    const flags = new Uint8Array(column.sections.length * SECTION)
    const sky = new Uint8Array(column.sections.length * SECTION)
    // light sections start one under the lowest block section; the server sends none over the highest it lights, all open sky
    const top = column.skyLightSections.findLastIndex(l => l)
    column.sections.forEach((s, sy) => {
      const light = column.skyLightSections[sy + 1]
      for (let i = 0; i < SECTION; i++) {
        flags[sy * SECTION + i] = solid[s.data.get(i)]
        sky[sy * SECTION + i] = light ? light.get(i) : sy + 1 > top ? 15 : 0
      }
    })
    columns.set(`${m[1]},${m[2]}`, columnSky(flags, sky))
  }
  const levels = Array.from({ length: 15 }, (_, l) =>
    fitWindows(new Map([...columns].map(([k, c]) => [k, c[l]])), minY, (w, o) => w + o === 0))
  const sections = levels[0].map(({ y, n }, i) => ({ y, a: levels.map(f => f[i].a), b: levels.map(f => f[i].b), n }))
  fs.writeFileSync(OUT, `{"version": "${MC_VERSION}", "columns": ${columns.size}, "sections": [\n${sections.map(e => ' ' + JSON.stringify(e)).join(',\n')}]}\n`)
  console.log(`${columns.size} columns, ${sections.length} section heights -> ${path.relative(ROOT, OUT)}`)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main(process.argv[2] ?? path.join(ROOT, 'worlds/test/chunks'))
