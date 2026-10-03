// The POST /api/chat/send body in, a {status, json} out. The runner is injected (rconRun in production).
import { tellrawCommand, validTarget, rconRun } from './chatsend.mjs'

export const MAX_BODY_BYTES = 2048
const SENDER = 'Dan'

const reply = (status, json) => ({ status, json })
const bad = error => reply(400, { error })

function parse (text) {
  try { return JSON.parse(text) } catch { return undefined }
}

function validate (input) {
  if (input === undefined) return { error: 'body must be JSON' }
  if (input === null || typeof input !== 'object' || Array.isArray(input)) return { error: 'body must be a JSON object' }
  const { text, target = '@a' } = input
  if (typeof text !== 'string' || !text.trim()) return { error: 'text must be a non-empty string' }
  if (!validTarget(target)) return { error: 'target must be @a or a player name' }
  return { text, target }
}

// bodyText is null when the body was over the limit
export async function chatSendResponse (bodyText, { run = rconRun } = {}) {
  if (bodyText === null) return reply(413, { error: `body exceeds ${MAX_BODY_BYTES} bytes` })
  const { error, text, target } = validate(parse(bodyText))
  if (error) return bad(error)
  let command
  try { command = tellrawCommand({ text, target, from: SENDER }) } catch (e) { return bad(e.message) }
  try {
    await run(command)
  } catch (e) {
    return reply(502, { error: `RCON failed: ${e.message}` })
  }
  return reply(200, { ok: true, command })
}

export function dryRunner (log = console.log) {
  return async command => { log(`chat send (dry run): ${command}`); return 'dry' }
}
