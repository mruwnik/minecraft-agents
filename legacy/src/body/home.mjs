// Archived body paths: ROOT contains archived src/ and library/; DATA_ROOT contains shared state/ and textures/.
// and this body's own HOME, whose config.json names it.
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { readConfig } from '../config.mjs'

const DIR = path.dirname(fileURLToPath(import.meta.url))
// The archive root owns code and the repository root owns live data.
export const ROOT = path.resolve(DIR, '..', '..')
export const DATA_ROOT = path.resolve(ROOT, '..')
fs.mkdirSync(path.join(DATA_ROOT, 'state'), { recursive: true })
// `node src/bot.mjs <home>` runs the body whose config.json, events.jsonl and snapshots/ live in <home> (default: the
// working directory), while textures/ stays here, shared by every body. No config.json, no body:
// a default name here once logged a stray `node src/bot.mjs` in as another agent and kicked that agent's real body.
export const HOME = path.resolve(process.argv[2] ?? process.cwd())
export const cfg = readConfig(HOME)
// the shared files of the world (server) this body plays in: state/worlds/<world>, named by config.json
export const WORLD_DIR = cfg.worldDir
