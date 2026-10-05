// Chat and whisper: send one line, then listen briefly for the server's refusal (a system line only this body sees;
// an unsigned /tell looks sent otherwise).
// The rules (cleaning, empty, slash, player name, length budget) live in engine.chat/validate, which gate! and direct!
// run before any send. This file keeps the Mineflayer calls, the refusal parsing and assertSendable.

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

// Last line of defence, not cleaning: a line that is not a string, starts with a slash (a command) or holds a
// control character (a second line) is never sent.
export const assertSendable = message => {
  if (typeof message !== 'string' || message.startsWith('/') || /[\x00-\x1f\x7f]/.test(message)) {
    throw new Error('refusing to send a chat line that is not a string, starts with a slash or holds a control character')
  }
}

export async function say (bot, ctx, a, { timeScale = 1, listenMs = 1000 } = {}) {
  const message = a.message
  const to = a.to
  assertSendable(message)
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
    to ? bot.whisper(to, message) : bot.chat(message)
    await listen(ctx, listenMs * timeScale, 50 * timeScale)
  } finally {
    unsubscribe()
  }
  if (refusals.length) return { status: 'failed', reason: refusals[0].slice(0, 200), parts: 1 }
  return to ? { status: 'sent', parts: 1, to } : { status: 'sent', parts: 1 }
}
