// Launcher for `npm start`: a small Node supervisor, plain JS because it only spawns processes (the dashboard itself is
// ClojureScript). Builds, runs out/server.cjs with inherited stdio, and on a restart request from the server (IPC message
// {type:"restart"}, sent by POST /api/restart) builds FIRST while the old server keeps running; only a good build replaces it.
// Ctrl-C stops both; a server exit nobody asked for ends the launcher with the server's exit code.
import { spawn } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import {
  initial, onRestartRequest, onBuildDone, onServerExit, onQuit, buildSteps, buildOutcome, parseMemAvailableMb, enoughMemory, minMemoryMb,
} from './js/launcher.mjs'

const dir = dirname(fileURLToPath(import.meta.url))
const killAfterMs = 5000
const say = (text) => console.log(`[launcher] ${text}`)

let state = initial
let server = null
let build = null

const availableMb = () => {
  try { return parseMemAvailableMb(readFileSync('/proc/meminfo', 'utf8')) } catch { return null }
}

// Detached, so the whole group (flock, npx, the shadow-cljs JVM) can be killed with one signal.
const killBuild = () => {
  if (!build) return
  try { process.kill(-build.pid, 'SIGTERM') } catch { /* already gone */ }
}

const runStep = (step) => new Promise((resolve) => {
  say(`building (shadow-cljs compile ${step})`)
  build = spawn('flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'compile', step], { cwd: dir, stdio: 'inherit', detached: true })
  build.on('error', (e) => { say(`build could not start: ${e.message}`); build = null; resolve(1) })
  build.on('exit', (code) => { build = null; resolve(code === null ? 1 : code) })
})

// Resolves true on a good build. The compile JVM is shared machine-wide, hence flock. Steps run in buildSteps order and
// stop at the first failure, so a failed ui build leaves out/server.cjs as it was.
const runBuild = async () => {
  const mb = availableMb()
  if (!enoughMemory(mb)) {
    say(`refusing to build: ${mb} MB available, ${minMemoryMb} MB needed`)
    return false
  }
  const codes = []
  for (const step of buildSteps) {
    codes.push(await runStep(step))
    if (state.quitting || codes[codes.length - 1] !== 0) break
  }
  const { ok, failed } = buildOutcome(codes)
  if (!ok) say(`build failed at the ${failed} step`)
  return ok
}

const startServer = () => {
  server = spawn('node', ['--max-old-space-size=256', '--max-semi-space-size=4', 'out/server.cjs'],
    { cwd: dir, stdio: ['inherit', 'inherit', 'inherit', 'ipc'] })
  server.on('message', (m) => { if (m && m.type === 'restart') request() })
  server.on('exit', (code, signal) => {
    server = null
    run(onServerExit(state, code, signal))
  })
}

const stopServer = (why) => {
  const s = server
  if (!s) return
  state = { ...state, stopping: why }
  s.kill('SIGTERM')
  setTimeout(() => { if (server === s) s.kill('SIGKILL') }, killAfterMs).unref()
}

const run = ({ state: next, actions }) => {
  state = next
  for (const action of actions) perform(action)
}

const perform = (action) => {
  if (action === 'build') runBuild().then((ok) => run(onBuildDone(state, ok)))
  else if (action === 'swap') { say('build ok, replacing the server'); if (server) stopServer('swap'); else startServer() }
  else if (action === 'report-failure') say('build failed: the old server keeps running')
  else if (action === 'start') startServer()
  else if (action === 'kill-build') killBuild()
  else if (action === 'stop-server') stopServer('quit')
  else if (action.startsWith('exit:')) {
    killBuild()
    process.exit(Number(action.slice(5)))
  }
}

const request = () => {
  say('restart requested')
  run(onRestartRequest(state))
}

const quit = () => run(onQuit(state, server !== null))
process.on('SIGINT', quit)
process.on('SIGTERM', quit)

// First start: a failed build or too little memory ends the launcher (there is no old server to keep).
state = { ...state, phase: 'building' }
runBuild().then((ok) => {
  if (!ok) { say('first build failed'); process.exit(1) }
  if (state.quitting) return
  state = { ...state, phase: 'idle' }
  startServer()
})
