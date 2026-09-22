// A deliberately narrow RCON client: the only command it can send is `whitelist add <valid player name>`.
//   node tools/rcon.mjs Aviendha
// Needs enable-rcon=true in server.properties and the password in ~/.config/minecraft-claude/rcon-password (chmod 600).
// This is a guard rail against accidents, not a security boundary: anything running as the same OS user could read
// the password file and speak RCON itself.
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'

const AUTH = 3
const COMMAND = 2

export function encodePacket (id, type, body) {
  const text = Buffer.from(body, 'utf8')
  const buf = Buffer.alloc(14 + text.length)
  buf.writeInt32LE(10 + text.length, 0)
  buf.writeInt32LE(id, 4)
  buf.writeInt32LE(type, 8)
  text.copy(buf, 12)
  return buf
}

// {id, type, body, size (bytes consumed)} or null when `buf` doesn't hold a whole packet yet
export function decodePacket (buf) {
  if (buf.length < 4) return null
  const size = buf.readInt32LE(0) + 4
  if (buf.length < size) return null
  return { id: buf.readInt32LE(4), type: buf.readInt32LE(8), body: buf.toString('utf8', 12, size - 2), size }
}

export function whitelistCommand (name) {
  if (!/^[A-Za-z0-9_]{3,16}$/.test(name)) throw new Error(`not a valid player name: ${JSON.stringify(name)}`)
  return `whitelist add ${name}`
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

async function main (name) {
  const command = whitelistCommand(name)
  const props = fs.readFileSync(path.join(import.meta.dirname, '..', '..', 'server.properties'), 'utf8')
  const port = Number(/^rcon\.port=(\d+)/m.exec(props)?.[1] ?? 25575)
  const password = fs.readFileSync(path.join(os.homedir(), '.config/minecraft-claude/rcon-password'), 'utf8').trim()
  const socket = net.connect({ host: '127.0.0.1', port })
  await new Promise((resolve, reject) => socket.once('connect', resolve).once('error', reject))
  const auth = await exchange(socket, 1, AUTH, password)
  if (auth.id === -1) throw new Error('RCON refused the password')
  const reply = await exchange(socket, 2, COMMAND, command)
  socket.end()
  console.log(reply.body)
}

if (import.meta.filename === process.argv[1]) main(process.argv[2] ?? '').catch(e => { console.error(`whitelist add failed: ${e.message}`); process.exit(1) })
