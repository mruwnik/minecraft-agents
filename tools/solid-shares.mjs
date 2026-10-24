// Why JavaScript: binary data; decodes recorded chunk columns (prismarine-chunk).
// Writes engine/src/jobs/lib/cost/solid_shares.json, the prior jobs.lib.cost.base-rates/solid-prior reads: per section
// height (y, its lowest block) a Beta {a b} fitted by moments to the share of solid blocks in every 3x3x3-section window
// centred there, over the columns the bodies have recorded (worlds/<world>/chunks: the game's terrain as the server sent
// it, not what a body saw). Rerun when the world or the game version changes:
//   node tools/solid-shares.mjs [chunks-dir]
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile } from '../engine/js/view-column.mjs'
import { MC_VERSION } from '../engine/js/path/snapshot.mjs'

const ROOT = path.join(import.meta.dirname, '..')
const OUT = path.join(ROOT, 'engine/src/jobs/lib/cost/solid_shares.json')
const SECTION = 4096

// 1 by state id for a block a body cannot walk through (a full collision box, not a fluid)
export const solidStates = registry => {
  const flags = new Uint8Array(registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0) + 1)
  registry.blocksArray.forEach(b => {
    if (b.boundingBox === 'block' && !/water|lava/.test(b.name)) flags.fill(1, b.minStateId, b.maxStateId + 1)
  })
  return flags
}

// {a b} of a Beta with mean m and variance v of the share; a share known exactly (v 0) is worth one whole window of cells
export const betaOf = (m, v, cells) => {
  const strength = v > 0 ? Math.min(cells, Math.max(1e-3, m * (1 - m) / v - 1)) : cells
  return { a: m * strength, b: (1 - m) * strength }
}

// [{y a b n}] by section from columns: Map "cx,cz" -> Int32Array of solid counts per section, lowest first, minY the lowest block
export const fitWindows = (columns, minY) => {
  const acc = new Map()
  for (const [key, counts] of columns) {
    const [cx, cz] = key.split(',').map(Number)
    const around = []
    for (let dx = -1; dx <= 1; dx++) for (let dz = -1; dz <= 1; dz++) around.push(columns.get(`${cx + dx},${cz + dz}`))
    if (around.some(c => !c)) continue
    for (let sy = 0; sy < counts.length; sy++) {
      let solid = 0
      let cells = 0
      for (let d = -1; d <= 1; d++) {
        if (sy + d < 0 || sy + d >= counts.length) continue
        around.forEach(c => { solid += c[sy + d] })
        cells += 9 * SECTION
      }
      const share = solid / cells
      const e = acc.get(sy) ?? { n: 0, sum: 0, sq: 0, cells }
      e.n++; e.sum += share; e.sq += share * share
      acc.set(sy, e)
    }
  }
  return [...acc].sort((p, q) => p[0] - q[0]).map(([sy, { n, sum, sq, cells }]) => {
    const m = sum / n
    const { a, b } = betaOf(m, Math.max(0, sq / n - m * m), cells)
    return { y: minY + 16 * sy, a: Number(a.toPrecision(4)), b: Number(b.toPrecision(4)), n }
  })
}

const main = dir => {
  const registry = prismarineRegistry(MC_VERSION)
  const ChunkColumn = prismarineChunk(registry)
  const solid = solidStates(registry)
  const columns = new Map()
  let minY = null
  for (const name of fs.readdirSync(dir)) {
    const m = /^(-?\d+)\.(-?\d+)\.bin$/.exec(name)
    if (!m) continue
    let decoded
    try { decoded = decodeColumnFile(fs.readFileSync(path.join(dir, name))) } catch { continue } // a file cut short by a write
    const column = new ChunkColumn({ minY: decoded.header.minY, worldHeight: decoded.header.worldHeight })
    column.load(decoded.sections)
    minY ??= decoded.header.minY
    columns.set(`${m[1]},${m[2]}`, Int32Array.from(column.sections, s => {
      let k = 0
      for (let i = 0; i < SECTION; i++) k += solid[s.data.get(i)]
      return k
    }))
  }
  const sections = fitWindows(columns, minY)
  fs.writeFileSync(OUT, `{"version": "${MC_VERSION}", "columns": ${columns.size}, "sections": [\n${sections.map(e => ' ' + JSON.stringify(e)).join(',\n')}]}\n`)
  console.log(`${columns.size} columns, ${sections.length} section heights -> ${path.relative(ROOT, OUT)}`)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main(process.argv[2] ?? path.join(ROOT, 'worlds/test/chunks'))
