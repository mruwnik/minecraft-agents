// Shared durable build snapshots. Map notes reference these files, not private agent state.
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createHash, randomUUID } from 'node:crypto'
import { canonicalBlueprint } from './schema.mjs'
export const BLUEPRINT_STATE_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../../state/blueprint-builds')
export function manifestId (place, at) { return createHash('sha256').update(JSON.stringify([place, at])).digest('hex').slice(0, 32) }
export function writeBlueprintManifest (manifest, dir = BLUEPRINT_STATE_DIR) {
  if (!/^[a-f0-9]{32}$/.test(manifest.id ?? '')) throw new Error('invalid blueprint manifest ID')
  fs.mkdirSync(dir, { recursive: true })
  const file = path.join(dir, `${manifest.id}.json`), temp = `${file}.${randomUUID()}.tmp`
  const bytes = canonicalBlueprint({ ...manifest, manifestVersion: 2 })
  try { fs.writeFileSync(temp, bytes, { flag: 'wx', mode: 0o600 }); fs.renameSync(temp, file) } finally { if (fs.existsSync(temp)) fs.unlinkSync(temp) }
  return `bp2:${manifest.id}`
}
export function readBlueprintManifest (note, dir = BLUEPRINT_STATE_DIR) {
  const id = /^bp2:([a-f0-9]{32})$/.exec(String(note))?.[1]
  if (!id) return null
  const file = path.join(dir, `${id}.json`)
  if (!fs.existsSync(file)) throw new Error(`blueprint saved manifest ${id} is missing; do not rebuild from an edited source`)
  const manifest = JSON.parse(fs.readFileSync(file, 'utf8'))
  if (manifest.manifestVersion !== 2 || manifest.id !== id) throw new Error('blueprint manifest identity/version is invalid')
  return manifest
}
