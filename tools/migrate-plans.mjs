#!/usr/bin/env node
// Dry-run by default. Root/driver stops old writers, reviews the digest/report,
// then applies with that exact digest. New body writers share the exclusive lock.
import fs from 'node:fs'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { fileURLToPath } from 'node:url'
import { migrateSavedPlans } from '../src/structure/migrate.mjs'
import { withMapLock, atomicMapWrite } from '../src/map-store.mjs'
const hash = text => createHash('sha256').update(text).digest('hex')

// --file= names a map directly (a backup, a test fixture); otherwise --world <name> (or --world=<name>) resolves
// the map that world's bodies actually write, state/worlds/<name>/places.json
export function resolveMapFile (args, root) {
  const fileArg = args.find(a => a.startsWith('--file='))
  if (fileArg) return { file: path.resolve(fileArg.slice('--file='.length)) }
  const worldArg = args.find(a => a === '--world' || a.startsWith('--world='))
  const world = worldArg === '--world' ? args[args.indexOf('--world') + 1] : worldArg?.slice('--world='.length)
  if (!world) return { error: '--world <name> is required, or --file=<path> to point at a map directly' }
  return { file: path.join(root, 'state', 'worlds', world, 'places.json') }
}
export function migrateMapFile (file, { apply = false, expected } = {}) {
  const run = () => {
    const original = fs.readFileSync(file, 'utf8')
    const digest = hash(original)
    if (apply && (!expected || expected !== digest)) throw new Error('shared map changed or --expect digest missing; run dry-run again before applying')
    const result = migrateSavedPlans(JSON.parse(original))
    const count = result.reports.filter(r => r.migrated).length
    const report = { file, sha256: digest, totalRecords: result.places.length, migrated: count, plans: result.reports, applied: false }
    if (!apply || !count) return report
    const backup = `${file}.before-3d-${digest.slice(0, 12)}.json`
    fs.writeFileSync(backup, original, { flag: 'wx', mode: 0o600 })
    // Detect an out-of-date/noncooperating writer even after conversion/backup.
    if (fs.readFileSync(file, 'utf8') !== original) throw new Error('shared map changed during migration; no replacement written')
    atomicMapWrite(file, result.places)
    return { ...report, applied: true, backup }
  }
  return apply ? withMapLock(file, run) : run()
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2)
  const resolved = resolveMapFile(args, path.join(import.meta.dirname, '..'))
  if (resolved.error) {
    console.error(resolved.error)
    process.exitCode = 1
  } else {
    try { console.log(JSON.stringify(migrateMapFile(resolved.file, { apply: args.includes('--apply'), expected: args.find(a => a.startsWith('--expect='))?.slice(9) }), null, 2)) } catch (error) { console.error(error.message); process.exitCode = 1 }
  }
}
