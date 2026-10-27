// Why JavaScript: binary data; decodes recorded chunk columns and their light (prismarine-chunk).
// Writes engine/src/jobs/lib/cost/sky_shares.json, the prior jobs.lib.cost.base-rates/sky-share reads: per section height
// (y, its lowest block) and sky light level L 1..15, a Beta fitted by moments to the share of the open blocks sky light L
// or more reaches in every 3x3x3-section window centred there ({y a b}, a and b by L - 1), over the columns the bodies have recorded, less the fixture plot grid
// and its lanes, as tools/solid-shares.mjs (open is the engine's rule there). Rerun when the world or the game version
// changes:
//   node tools/sky-shares.mjs [chunks-dir]
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import { decodeColumnFile, restoreColumn } from '../engine/js/view-column.mjs'
import { MC_VERSION } from '../engine/js/path/snapshot.mjs'
import { engineTable, fitWindows, reserved, solidStates } from './solid-shares.mjs'

const ROOT = path.join(import.meta.dirname, '..')
const OUT = path.join(ROOT, 'engine/src/jobs/lib/cost/sky_shares.json')
const SECTION = 4096

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
