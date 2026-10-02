// Claude's Minecraft body, the process: it fills the action tables (src/body/actions/), adds a composite for each
// library/ file, serves the HTTP control API (src/body/api.mjs) and connects (src/body/connection.mjs). Decisions
// arrive over that API, and everything notable that happens is appended to events.jsonl so the planning side can
// follow along.
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { RENAMED, PRIMITIVES, compositeError, stackTop } from './lib.mjs'
import { HOME, cfg } from './body/home.mjs'
import { profileFile } from './auth.mjs'
import { emit, sayError } from './body/events.mjs'
import { LIBRARY_DIR, libraryFiles, compositeName, composites, runComposite } from './body/runner.mjs'
import { task } from './body/state.mjs'
import { loginNeeded, connect } from './body/connection.mjs'
import { boatRuntime, boatTravelRuntime, travelRuntime, ridingRuntime, villagerRuntime } from './body/runtimes.mjs'
import { scheduler } from './body/jobs.mjs'
import { watchesQuick } from './body/watches.mjs'
import { controlApi } from './body/api.mjs'
import { long, quick } from './body/actions/tables.mjs'
import { senseLong, senseQuick } from './body/actions/sense.mjs'
import { mapQuick } from './body/actions/map.mjs'
import { moveLong, moveQuick } from './body/actions/move.mjs'
import { blockLong } from './body/actions/block.mjs'
import { itemLong, itemQuick } from './body/actions/item.mjs'
import { creatureLong, creatureQuick } from './body/actions/creature.mjs'
import { selfLong, selfQuick } from './body/actions/self.mjs'
import { controlLong, controlQuick } from './body/actions/control.mjs'

Object.assign(long, boatRuntime.long, boatTravelRuntime.long, travelRuntime.long, ridingRuntime.long, villagerRuntime.long, senseLong, moveLong, blockLong, itemLong, creatureLong, selfLong, controlLong)
Object.assign(quick, boatRuntime.quick, boatTravelRuntime.quick, travelRuntime.quick, ridingRuntime.quick, villagerRuntime.quick, watchesQuick, senseQuick, mapQuick, moveQuick, itemQuick, creatureQuick, selfQuick, controlQuick)

for (const file of libraryFiles()) {
  const name = compositeName(file)
  const mod = await import(pathToFileURL(path.join(LIBRARY_DIR, file)).href).then(m => m.default, e => ({ loadError: e.message }))
  const problem = mod?.loadError
    ? `library/${file}: ${mod.loadError}`
    : (long[name] || quick[name]) ? `library/${file}: ${name} is already a primitive, rename the file` : compositeError(name, mod)
  if (problem) { console.log(`[library] ${problem}`); emit('error', { message: problem }); continue }
  // instant: it only reads (or writes the map), so it goes in the quick table and never cancels a task that is running
  const table = mod.instant ? quick : long
  table[name] = args => runComposite(name, mod, args, (type, detail) => {
    if (task?.jobId) scheduler?.report(task.jobId, type, detail)
  })
  composites.set(name, mod)
  console.log(`[library] ${name}: ${mod.doc}`)
}

// an action nobody can look up may as well not exist: say so at start rather than let ./mc help quietly skip it
const undocumented = [...Object.keys(long), ...Object.keys(quick)].filter(name => !PRIMITIVES[name] && !composites.has(name))
if (undocumented.length) console.log(`[help] no catalogue line for: ${undocumented.join(' ')} (add one to PRIMITIVES in lib.mjs)`)
// a renamed action must point at one that exists, or the error sends the driver after a ghost
const served = name => Boolean(long[name] || quick[name])
const ghosts = Object.entries(RENAMED).filter(([was, now]) => !served(now) || served(was))
if (ghosts.length) console.log(`[help] RENAMED is stale: ${ghosts.map(([was, now]) => `${was} -> ${now}`).join(' ')} (lib.mjs)`)

http.createServer(controlApi).listen(cfg.apiPort, '127.0.0.1', () => console.log(`control API on http://127.0.0.1:${cfg.apiPort}`))

process.on('uncaughtException', e => sayError(`uncaught: ${e.message}`, { at: stackTop(e.stack) }))
process.on('unhandledRejection', e => sayError(`unhandled: ${e?.message ?? e}`))

if (cfg.auth === 'microsoft' && !fs.existsSync(profileFile(HOME))) loginNeeded()
connect()
