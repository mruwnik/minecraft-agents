#!/usr/bin/env node
// Why JavaScript: thin entry point; the counting tool is cljs (dashboard/src/world_test/event_counts.cljs), run from the world-test bundle.
// openChunks and seen are the other parts: they read the recorded chunk columns and the seen memory files (binary) through
// engine/js/view-column.mjs, engine/js/seen-file.mjs and prismarine-chunk.
// Recounts the measured combat priors from the trial bodies' event logs (`node tools/event-counts.mjs` prints the usage).
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

const root = process.env.EVENT_COUNTS_ROOT ?? path.join(import.meta.dirname, '..')
const bundle = path.join(root, 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
const newest = (dir) => fs.readdirSync(dir, { recursive: true, withFileTypes: true })
  .filter((e) => e.isFile() && e.name.endsWith('.cljs'))
  .reduce((t, e) => Math.max(t, fs.statSync(path.join(e.parentPath, e.name)).mtimeMs), 0)
const sources = Math.max(newest(path.join(root, 'dashboard', 'src', 'world_test')), fs.statSync(path.join(root, 'dashboard', 'shadow-cljs.edn')).mtimeMs)
if (fs.statSync(bundle).mtimeMs < sources) {
  console.error(`the world-test bundle is older than its sources (${bundle}): it would count with old code; rebuild it with: tools/compile dashboard world-test`)
  process.exit(2)
}

// dir -> (x, y, z) -> {name, id, t, block, sky, biome} of a cell (t: when the column was recorded, ms) of the chunk columns recorded in dir, null for a column never recorded
async function chunkReader () {
  const require = createRequire(import.meta.url)
  const { decodeColumnFile, restoreColumn } = await import('../engine/js/view-column.mjs')
  const { MC_VERSION } = await import('../engine/js/path/snapshot.mjs')
  const registry = require('prismarine-registry')(MC_VERSION)
  const ChunkColumn = require('prismarine-chunk')(registry)
  return (dir) => {
    const biomes = Object.fromEntries(JSON.parse(fs.readFileSync(path.join(dir, '..', 'biomes.json'))).biomes.map(b => [b.id, b.name]))
    const columns = new Map()
    const column = (cx, cz) => {
      const key = `${cx}.${cz}`
      if (!columns.has(key)) {
        const file = path.join(dir, `${key}.bin`)
        const d = fs.existsSync(file) ? decodeColumnFile(fs.readFileSync(file)) : null
        columns.set(key, d && { t: d.header.t, col: restoreColumn(new ChunkColumn({ minY: d.header.minY, worldHeight: d.header.worldHeight }), d) })
      }
      return columns.get(key)
    }
    return (x, y, z) => {
      const c = column(Math.floor(x / 16), Math.floor(z / 16))
      if (!c) return null
      const { col, t } = c
      const p = { x: ((x % 16) + 16) % 16, y, z: ((z % 16) + 16) % 16 }
      const id = col.getBlockStateId(p)
      return { name: registry.blocksByStateId[id]?.name, id, t, block: col.getBlockLight(p), sky: col.getSkyLight(p), biome: biomes[col.getBiome(p)] }
    }
  }
}

async function seenReader () {
  const require = createRequire(import.meta.url)
  const { loadSeen } = await import('../engine/js/seen-file.mjs')
  const { MC_VERSION } = await import('../engine/js/path/snapshot.mjs')
  const registry = require('prismarine-registry')(MC_VERSION)
  return { load: loadSeen, stateName: (id) => registry.blocksByStateId[id]?.name }
}

const openChunks = process.argv[2] === 'spot-returns' ? null : await chunkReader()
const seen = process.argv[2] === 'resight' ? await seenReader() : null
process.env.WORLD_TEST_REPO = fs.realpathSync(process.env.WORLD_TEST_REPO ?? path.join(import.meta.dirname, '..'))
process.exitCode = await createRequire(import.meta.url)(bundle).eventCounts(process.argv.slice(2), openChunks, seen)
