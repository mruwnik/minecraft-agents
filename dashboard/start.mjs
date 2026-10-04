// Launcher for `npm start`: a small Node supervisor, plain JS because it only spawns processes (the dashboard itself is
// ClojureScript). Builds, runs out/server.cjs with inherited stdio, and on a restart request from the server (IPC message
// {type:"restart"}, sent by POST /api/restart) builds FIRST while the old server keeps running; only a good build replaces it.
// Ctrl-C stops both; a server exit nobody asked for ends the launcher with the server's exit code.
import { spawn } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import {
  initial, onRestartRequest, onBuildDone, onServerExit, parseMemAvailableMb, enoughMemory, minMemoryMb,
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

// Resolves true on a good build. The compile JVM is shared machine-wide, hence flock.
const runBuild = () => new Promise((resolve) => {
  const mb = availableMb()
  if (!enoughMemory(mb)) {
    say(`refusing to build: ${mb} MB available, ${minMemoryMb} MB needed`)
    return resolve(false)
  }
  say('building (shadow-cljs compile server ui)')
  build = spawn('flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'compile', 'server', 'ui'], { cwd: dir, stdio: 'inherit' })
  build.on('error', (e) => { say(`build could not start: ${e.message}`); build = null; resolve(false) })
  build.on('exit', (code) => { build = null; resolve(code === 0) })
})

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
  state = { ...state, stopping: why }
  const s = server
  s.kill('SIGTERM')
  setTimeout(() => { if (server === s) s.kill('SIGKILL') }, killAfterMs).unref()
}

const run = ({ state: next, actions }) => {
  state = next
  for (const action of actions) perform(action)
}

const perform = (action) => {
  if (action === 'build') runBuild().then((ok) => run(onBuildDone(state, ok)))
  else if (action === 'swap') { say('build ok, replacing the server'); stopServer('swap') }
  else if (action === 'report-failure') say('build failed: the old server keeps running')
  else if (action === 'start') startServer()
  else if (action.startsWith('exit:')) {
    if (build) build.kill('SIGTERM')
    process.exit(Number(action.slice(5)))
  }
}

const request = () => {
  say('restart requested')
  run(onRestartRequest(state))
}

const quit = () => {
  if (build) build.kill('SIGTERM')
  if (!server) process.exit(0)
  stopServer('quit')
}
process.on('SIGINT', quit)
process.on('SIGTERM', quit)

// First start: a failed build or too little memory ends the launcher (there is no old server to keep).
state = { ...state, phase: 'building' }
runBuild().then((ok) => {
  if (!ok) { say('first build failed'); process.exit(1) }
  state = { ...state, phase: 'idle' }
  startServer()
})
