// Chat and whisper: send one line, then listen briefly for
// the server's refusal (a system line only this body sees; an unsigned /tell looks sent otherwise).

export const CHAT_MAX = 256
export const PLAYER_NAME = /^[A-Za-z0-9_]{3,16}$/

// control characters become spaces, the section sign (formatting code) is dropped; never lets a newline or a leading slash through
export const cleanMessage = text => String(text ?? '').replace(/[\x00-\x1f\x7f]/g, ' ').replace(/§/g, '').trim()

const REFUSALS = [/^Command had invalid signature/, /^No player was found/, /^Unknown or incomplete command/, /^An unexpected error occurred trying to execute that command/, /^That player cannot be found/i]

export const chatRefusal = text => REFUSALS.some(r => r.test(String(text ?? ''))) ? String(text) : null

const isSystem = position => position === 'system' || position === 'game_info'
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

// wait `ms`, checking every `stepMs` that the call is still wanted (ctx.alive throws when cut)
const listen = async (ctx, ms, stepMs) => {
  for (let left = ms; left > 0; left -= stepMs) {
    ctx.alive()
    await sleep(Math.min(stepMs, left))
  }
  ctx.alive()
}

// last guard at the send: a public part that starts with a slash would be a command, so it is never sent
export const sendPublic = (bot, part) => {
  if (part.startsWith('/')) throw new Error('refusing to send a chat line that starts with a slash')
  bot.chat(part)
}

export async function say (bot, ctx, a, { timeScale = 1, listenMs = 1000 } = {}) {
  const message = cleanMessage(a.message)
  const to = a.to
  if (message.startsWith('/')) return { status: 'cannot', reason: 'command' }
  if (to && !PLAYER_NAME.test(to)) return { status: 'cannot', reason: 'bad-name' }
  if (to && !Object.hasOwn(bot.players ?? {}, to)) return { status: 'gone', to }
  const refusals = []
  const onMessage = (text, position) => {
    if (!isSystem(position)) return
    const refused = chatRefusal(text)
    if (refused) refusals.push(refused)
  }
  const unsubscribe = () => bot.removeListener('messagestr', onMessage)
  bot.on('messagestr', onMessage)
  ctx.onAbort(unsubscribe)
  try {
    to ? bot.whisper(to, message) : sendPublic(bot, message)
    await listen(ctx, listenMs * timeScale, 50 * timeScale)
  } finally {
    unsubscribe()
  }
  if (refusals.length) return { status: 'failed', reason: refusals[0].slice(0, 200), parts: 1 }
  return to ? { status: 'sent', parts: 1, to } : { status: 'sent', parts: 1 }
}
