// Chat and whisper for the say job: split long text to the server's line limit, send, then listen briefly for
// the server's refusal (a system line only this body sees; an unsigned /tell looks sent otherwise).

export const CHAT_MAX = 256

const SENTENCE_ENDS = ['. ', '! ', '? ', '; ']
const REFUSALS = [/^Command had invalid signature/, /^No player was found/, /^Unknown or incomplete command/, /^An unexpected error occurred trying to execute that command/, /^That player cannot be found/i]

export const chatRefusal = text => REFUSALS.some(r => r.test(String(text ?? ''))) ? String(text) : null

// how many characters of `text` make the next piece under `budget`
const cutAt = (text, budget) => {
  if (text.length <= budget) return text.length
  const head = text.slice(0, budget + 1)
  const sentence = Math.max(...SENTENCE_ENDS.map(m => head.lastIndexOf(m)))
  if (sentence > 0) return sentence + 1
  const space = head.lastIndexOf(' ')
  return space > 0 ? space : budget
}

export const splitSay = (text, budget) => {
  const out = []
  for (let rest = String(text ?? '').trim(); rest.length; rest = rest.slice(cutAt(rest, budget)).trim()) out.push(rest.slice(0, cutAt(rest, budget)).trim())
  return out
}

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

export async function say (bot, ctx, a, { timeScale = 1, listenMs = 1000 } = {}) {
  const message = String(a.message).trim()
  const to = a.to
  if (to && !bot.players?.[to]) return { status: 'gone', to }
  const parts = splitSay(message, to ? CHAT_MAX - `/tell ${to} `.length : CHAT_MAX)
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
    for (const part of parts) to ? bot.whisper(to, part) : bot.chat(part)
    await listen(ctx, listenMs * timeScale, 50 * timeScale)
  } finally {
    unsubscribe()
  }
  if (refusals.length) return { status: 'failed', reason: refusals[0].slice(0, 200), parts: parts.length }
  return to ? { status: 'sent', parts: parts.length, to } : { status: 'sent', parts: parts.length }
}
