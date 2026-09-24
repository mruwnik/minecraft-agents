// The one thing `./start` does before anything else: refuse a SECOND body for this agent (#145).
//   node tools/body-lock.mjs check <agentDir>   exit 3 and say why, or exit 0
//   node tools/body-lock.mjs write <agentDir> <pid>
// Two bodies under one name trade a login every ten seconds ("logged in from another location") and the only way an
// agent has ever found out of that is killing PIDs - which takes down every OTHER agent's body, because they all
// share a command line. The decision itself is bodyRefusal in lib.mjs, where it is tested.
import fs from 'node:fs'
import net from 'node:net'
import path from 'node:path'
import { bodyRefusal } from '../src/lib.mjs'

const [what, dir, pidArg] = process.argv.slice(2)
const pidFile = path.join(dir ?? '.', 'body.pid')

if (what === 'write') {
  fs.writeFileSync(pidFile, `${pidArg}\n`)
  process.exit(0)
}

const readPid = () => {
  const pid = Number(fs.existsSync(pidFile) ? fs.readFileSync(pidFile, 'utf8').trim() : NaN)
  return Number.isInteger(pid) && pid > 0 ? pid : null
}

// /proc is how a pid is told from a pid REUSED by something else; a kernel without it leaves cmdline null, and then
// only the port speaks. `process.kill(pid, 0)` alone would keep an agent locked out for ever over a recycled number.
const cmdlineOf = pid => {
  if (!pid) return null
  try {
    return fs.readFileSync(`/proc/${pid}/cmdline`, 'utf8').replace(/\0/g, ' ').trim()
  } catch {
    return null
  }
}

const listens = port => new Promise(resolve => {
  const socket = net.connect({ host: '127.0.0.1', port })
  const done = answer => { socket.destroy(); resolve(answer) }
  socket.setTimeout(1000)
  socket.once('connect', () => done(true))
  socket.once('timeout', () => done(false))
  socket.once('error', () => done(false))
})

const configFile = path.join(dir ?? '.', 'config.json')
const { apiPort = 3777 } = fs.existsSync(configFile) ? JSON.parse(fs.readFileSync(configFile, 'utf8')) : {}
const pid = readPid()
const refusal = bodyRefusal({ pid, cmdline: cmdlineOf(pid), listening: await listens(apiPort), port: apiPort })
if (!refusal) process.exit(0)
console.error(`REFUSED: ${refusal}`)
process.exit(3)
