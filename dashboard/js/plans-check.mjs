// Smoke: compare every plan in state/worlds/claude/places.json with the dumped columns (read-only).
import fs from 'node:fs'
import path from 'node:path'
import { comparePlan, createWorldBlocks, listPlans } from './plans.mjs'

const stateDir = path.join(import.meta.dirname, '../../state')
const world = process.argv[2] ?? 'claude'
const places = JSON.parse(fs.readFileSync(path.join(stateDir, 'worlds', world, 'places.json'), 'utf8'))
const { blockAt, close } = createWorldBlocks({ stateDir, world })
const names = new Set(listPlans(places).map(p => p.name))
for (const place of places.filter(p => names.has(p.name))) {
  const started = performance.now()
  const r = comparePlan({ place, blockAt })
  const ms = (performance.now() - started).toFixed(0)
  console.log(`${r.name} total=${r.total} match=${r.match} missing=${r.missing} wrong=${r.wrong} unknown=${r.unknown} ${r.percent}% ${ms}ms`)
}
close()
