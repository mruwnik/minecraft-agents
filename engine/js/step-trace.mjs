// Why JavaScript: hooks Mineflayer's physicsTick and the bot's world-acting methods, the one place every tick and act passes.
// Opt-in step trace (MC_STEP_TRACE=<file>): one line per physics tick (pos, vel, look, ground, pressed controls) and one
// per world act (dig, place, use, attack, equip, look), in a bounded ring that is rewritten to the file twice a second,
// so a failed world case keeps the last stretch of what the body did. Off: nothing is installed.
import { writeFileSync } from 'node:fs'

export const MAX_LINES = 6000
const FLUSH_MS = 500
const ACTS = ['dig', 'placeBlock', 'activateBlock', 'activateEntity', 'attack', 'equip', 'look', 'lookAt', 'toss', 'consume']

const f = n => Math.round(n * 100) / 100
const vecText = v => v ? `${f(v.x)},${f(v.y)},${f(v.z)}` : '-'
const argText = a => a?.position ? `${a.name ?? a.type ?? ''}@${vecText(a.position)}` : typeof a === 'object' ? JSON.stringify(a)?.slice(0, 60) : String(a)

export function createRing (max = MAX_LINES) {
  const lines = []
  return { push: line => { lines.push(line); if (lines.length > max) lines.splice(0, lines.length - max) }, text: () => lines.join('\n') + '\n' }
}

// returns the function that stops the trace; undefined file = off, nothing installed
export function traceSteps (bot, file, now = Date.now) {
  if (!file) return () => {}
  const ring = createRing()
  const t0 = now()
  let tick = 0
  const stamp = () => `${tick} ${now() - t0}`
  const onTick = () => {
    tick++
    const e = bot.entity
    const on = Object.entries(bot.controlState ?? {}).filter(([, v]) => v).map(([k]) => k).join('+')
    ring.push(`${stamp()} T ${vecText(e.position)} v ${vecText(e.velocity)} y${f(e.yaw)} p${f(e.pitch)} ${e.onGround ? 'G' : '-'} ${on || '-'}`)
  }
  bot.on('physicsTick', onTick)
  const originals = ACTS.filter(m => typeof bot[m] === 'function').map(m => [m, bot[m]])
  for (const [m, orig] of originals) {
    bot[m] = function (...args) {
      ring.push(`${stamp()} A ${m} ${args.map(argText).join(' ')}`)
      return orig.apply(this, args)
    }
  }
  const flush = () => { try { writeFileSync(file, ring.text()) } catch {} }
  const timer = setInterval(flush, FLUSH_MS)
  timer.unref?.()
  return () => {
    clearInterval(timer)
    bot.off('physicsTick', onTick)
    for (const [m, orig] of originals) bot[m] = orig
    flush()
  }
}
