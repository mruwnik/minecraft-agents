// Chat and whisper for the say job: split long text to the server's line limit, send, then listen briefly for
// the server's refusal (a system line only this body sees; an unsigned /tell looks sent otherwise).

export const CHAT_MAX = 256
export const PLAYER_NAME = /^[A-Za-z0-9_]{3,16}$/

// control characters become spaces, the section sign (formatting code) is dropped; never lets a newline or a leading slash through
export const cleanMessage = text => String(text ?? '').replace(/[\x00-\x1f\x7f]/g, ' ').replace(/§/g, '').trim()

// Vanilla kicks for spam above 200 points (20 per line or command, 1 off per tick): 10 lines in a burst, or over 1 line/s sustained.
// We stay far below: a gap between lines and a cap per window.
export const createLimiter = ({ gapMs = 1000, max = 5, windowMs = 30000, now = Date.now } = {}) => {
  let times = []
  const recent = () => { times = times.filter(t => now() - t < windowMs); return times }
  return {
    max,
    wouldAllow: n => recent().length + n <= max,
    // ms until n more lines fit the window
    retryMs: n => {
      const kept = recent()
      const overflow = kept.length + n - max
      return overflow <= 0 ? 0 : kept[overflow - 1] + windowMs - now()
    },
    // ms until the gap since the last line has passed
    gapLeftMs: () => times.length ? Math.max(0, times[times.length - 1] + gapMs - now()) : 0,
    record: () => { times.push(now()) }
  }
}

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

const waitGap = async (ctx, limiter, timeScale) => {
  for (let left = limiter.gapLeftMs(); left > 0; left = limiter.gapLeftMs()) {
    ctx.alive()
    await sleep(Math.min(50 * timeScale, left))
  }
}

// last guard at the send: a public part that starts with a slash would be a command, so it is never sent
export const sendPublic = (bot, part) => {
  if (part.startsWith('/')) throw new Error('refusing to send a chat line that starts with a slash')
  bot.chat(part)
}

const sendParts = async (bot, ctx, to, parts, limiter, timeScale) => {
  for (const part of parts) {
    await waitGap(ctx, limiter, timeScale)
    limiter.record()
    to ? bot.whisper(to, part) : sendPublic(bot, part)
  }
}

export async function say (bot, ctx, a, { timeScale = 1, listenMs = 1000, limiter = createLimiter() } = {}) {
  const message = cleanMessage(a.message)
  const to = a.to
  if (message.startsWith('/')) return { status: 'cannot', reason: 'command' }
  if (to && !PLAYER_NAME.test(to)) return { status: 'cannot', reason: 'bad-name' }
  if (to && !Object.hasOwn(bot.players ?? {}, to)) return { status: 'gone', to }
  const parts = splitSay(message, to ? CHAT_MAX - `/tell ${to} `.length : CHAT_MAX)
  if (parts.some(part => part.startsWith('/'))) return { status: 'cannot', reason: 'command' }
  if (parts.length > limiter.max) return { status: 'cannot', reason: 'too-long', parts: parts.length }
  if (!limiter.wouldAllow(parts.length)) return { status: 'blocked', reason: 'rate', retryMs: limiter.retryMs(parts.length) }
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
    await sendParts(bot, ctx, to, parts, limiter, timeScale)
    await listen(ctx, listenMs * timeScale, 50 * timeScale)
  } finally {
    unsubscribe()
  }
  if (refusals.length) return { status: 'failed', reason: refusals[0].slice(0, 200), parts: parts.length }
  return to ? { status: 'sent', parts: parts.length, to } : { status: 'sent', parts: parts.length }
}
