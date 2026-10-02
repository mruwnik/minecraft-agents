// What went wrong across every agent since a time: died, body_down, kicked, routine_stopped and stuck out of every
// state/agents/*/events.jsonl, newest last, one line each. `./mc incidents [since=<minutes|ISO>]` (the default is the
// last hour), or `node tools/incidents.mjs [since]` on its own. Imports nothing but src/cli.mjs (#148).
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { incidentLines, sinceTime, INCIDENT_TYPES } from '../src/cli.mjs'

const readLogs = agentsDir => fs.readdirSync(agentsDir, { withFileTypes: true })
  .filter(e => e.isDirectory() && fs.existsSync(path.join(agentsDir, e.name, 'events.jsonl')))
  .map(e => ({ agent: e.name, text: fs.readFileSync(path.join(agentsDir, e.name, 'events.jsonl'), 'utf8') }))

export function incidentsReport (agentsDir, since, now = Date.now()) {
  const logs = fs.existsSync(agentsDir) ? readLogs(agentsDir) : []
  const lines = incidentLines(logs, since, now)
  if (lines.length) return lines.join('\n')
  return `no incidents since ${new Date(sinceTime(since, now)).toISOString().replace(/\.\d{3}Z$/, 'Z')} (${INCIDENT_TYPES.join(', ')} across ${logs.length} agents)`
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  const [given] = process.argv.slice(2)
  const since = given === undefined ? undefined : (Number.isNaN(Number(given)) ? given : Number(given))
  console.log(incidentsReport(path.join(import.meta.dirname, '..', 'state', 'agents'), since))
}
