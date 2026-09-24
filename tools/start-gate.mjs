// The first thing tools/start-body runs: node tools/start-gate.mjs <clock.json> [--now]
// Exits 0 silently when the body may start; prints the refusal and exits 1 when the clock says night and nobody said --now
// (START_NOW=1 in the environment says the same, for a start wrapper that does not pass its arguments on).
import fs from 'node:fs'
import { startRefusal } from '../src/restart.mjs'

const [clockFile, ...flags] = process.argv.slice(2)
const parse = text => { try { return JSON.parse(text) } catch { return null } }
const clock = clockFile && fs.existsSync(clockFile) ? parse(fs.readFileSync(clockFile, 'utf8')) : null
const refusal = startRefusal(clock, Date.now(), flags.includes('--now') || process.env.START_NOW === '1')
if (refusal) { console.error(refusal); process.exit(1) }
