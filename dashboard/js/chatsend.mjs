// Send a chat line to everyone on the owner's behalf. Engine bodies see it as a mineflayer `chat` event because
// mineflayer parses system messages shaped like `<name> text` (LEGACY_VANILLA_CHAT_REGEX in lib/plugins/chat.js).
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { encodePacket, decodePacket } from '../../tools/rcon.mjs'

const AUTH = 3
const COMMAND = 2
const HOST = '127.0.0.1'
const PORT = 25575
const PASSWORD_FILE = path.join(os.homedir(), '.config', 'minecraft-claude', 'rcon-password')
const MAX_TEXT = 256
const NAME = /^[A-Za-z0-9_]{1,16}$/

export function validTarget (target) {
  return target === '@a' || (typeof target === 'string' && NAME.test(target))
}

const cleanText = text => String(text ?? '')
  .replace(/§./gu, '')
  .replace(/[\r\n\t]+/g, ' ')
  .replace(/[\u0000-\u001f\u007f]/g, '')
  .trim()
  .slice(0, MAX_TEXT)

export function tellrawCommand ({ target = '@a', from = 'Dan', text }) {
  if (!validTarget(target)) throw new Error(`invalid target: ${JSON.stringify(target)}`)
  if (!NAME.test(from ?? '')) throw new Error(`invalid from: ${JSON.stringify(from)}`)
  const clean = cleanText(text)
  if (!clean) throw new Error('empty message')
  return `tellraw ${target} ${JSON.stringify({ text: `<${from}> ${clean}` })}`
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

export async function rconRun (command) {
  const secret = fs.readFileSync(PASSWORD_FILE, 'utf8').trim()
  const socket = net.connect({ host: HOST, port: PORT })
  await new Promise((resolve, reject) => socket.once('connect', resolve).once('error', reject))
  try {
    const auth = await exchange(socket, 1, AUTH, secret)
    if (auth.id === -1) throw new Error('RCON refused the password')
    return (await exchange(socket, 2, COMMAND, command)).body
  } finally {
    socket.end()
  }
}

export async function sendChat ({ text, target, from, run = rconRun }) {
  return run(tellrawCommand({ text, target, from }))
}
