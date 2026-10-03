// Quick raw RCON passthrough for the owner's live tests. Not for agents by default; use tools/rcon-test.mjs for the allow-listed tool.
//   node tools/rcon-raw.mjs <command...>      e.g. node tools/rcon-raw.mjs list
// Exit codes: 0 response, 1 connection/auth failure, 2 no command. Password: ~/.config/minecraft-claude/rcon-password.
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { encodePacket, decodePacket } from './rcon.mjs'

const AUTH = 3
const COMMAND = 2
const HOST = '127.0.0.1'
const PORT = 25575
const PASSWORD_FILE = path.join(os.homedir(), '.config/minecraft-claude/rcon-password')

// argv -> one console command string; strips a single leading slash; throws on empty
export function buildRawCommand (argv) {
  const command = argv.join(' ').trim().replace(/^\//, '').trim()
  if (!command) throw new Error('usage: node tools/rcon-raw.mjs <command...>')
  return command
}

function exchange (socket, id, type, body) {
  return new Promise((resolve, reject) => {
    let pending = Buffer.alloc(0)
    const onData = chunk => {
      pending = Buffer.concat([pending, chunk])
      const packet = decodePacket(pending)
      if (!packet) return
      socket.off('data', onData)
      resolve(packet)
    }
    socket.on('data', onData)
    socket.once('error', reject)
    socket.write(encodePacket(id, type, body))
  })
}

async function send (command, password) {
  const socket = net.connect({ host: HOST, port: PORT })
  await new Promise((resolve, reject) => socket.once('connect', resolve).once('error', reject))
  const auth = await exchange(socket, 1, AUTH, password)
  if (auth.id === -1) throw new Error('RCON refused the password')
  const reply = await exchange(socket, 2, COMMAND, command)
  socket.end()
  return reply.body
}

async function main (argv) {
  let command
  try { command = buildRawCommand(argv) } catch (e) { console.error(e.message); return 2 }
  if (!fs.existsSync(PASSWORD_FILE)) { console.error('rcon password file missing'); return 1 }
  try {
    console.log(await send(command, fs.readFileSync(PASSWORD_FILE, 'utf8').trim()))
    return 0
  } catch (e) { console.error(`rcon-raw failed: ${e.message}`); return 1 }
}

if (import.meta.filename === process.argv[1]) process.exitCode = await main(process.argv.slice(2))
