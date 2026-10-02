// The first thing tools/start-body runs: node tools/start-gate.mjs <agentDir> [--now]
// Exits 0 silently when the body may start. Exits 1 with one line on stderr when the agent's config names no
// usable world (readWorld's message), checked before anything else so a missing world refuses even with --now;
// otherwise exits 1 when the clock says night and nobody said --now (START_NOW=1 in the environment says the same,
// for a start wrapper that does not pass its arguments on).
import fs from 'node:fs'
import path from 'node:path'
import { readWorld } from '../src/config.mjs'
import { startRefusal } from '../src/restart.mjs'

const [agentDir, ...flags] = process.argv.slice(2)
let world
try {
  world = readWorld(agentDir)
} catch (error) {
  console.error(error.message)
  process.exit(1)
}
const clockFile = path.join(world.dir, 'clock.json')
const parse = text => { try { return JSON.parse(text) } catch { return null } }
const clock = fs.existsSync(clockFile) ? parse(fs.readFileSync(clockFile, 'utf8')) : null
const refusal = startRefusal(clock, Date.now(), flags.includes('--now') || process.env.START_NOW === '1')
if (refusal) { console.error(refusal); process.exit(1) }
