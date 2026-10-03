// Leaving a bed. mineflayer's bot.wake() writes entity_action with a numeric actionId that this protocol maps to
// stop_sprinting, so the server never lets the body up; the packet is sent by name instead.
export const leaveBed = bot => bot._client.write('entity_action', { entityId: bot.entity.id, actionId: 'leave_bed', jumpBoost: 0 })

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))

// Resolves true once the body is out of bed (at once when it already is), false when it is still in bed after timeoutMs.
export async function ensureAwake (bot, { timeoutMs = 1000, pollMs = 50 } = {}) {
  if (!bot.isSleeping) return true
  leaveBed(bot)
  const deadline = Date.now() + timeoutMs
  while (bot.isSleeping) {
    if (Date.now() >= deadline) return false
    await sleepMs(pollMs)
  }
  return true
}
