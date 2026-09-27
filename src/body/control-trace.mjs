// Observational traces for bounded raw-control diagnostics. No steering here.
export function controlTrace (bot, { now = Date.now, duration = 1000 } = {}) {
  const started = now()
  const samples = []
  const spacing = Math.max(50, duration / 19)
  let last = -Infinity
  const sample = () => {
    const ms = now() - started
    if (samples.length >= 20 || ms - last < spacing) return
    last = ms
    const e = bot.entity
    samples.push({ ms, position: e.position.toArray(), velocity: e.velocity.toArray(), yaw: e.yaw,
      onGround: e.onGround, inWater: e.isInWater,
      keys: ['forward', 'back', 'left', 'right', 'jump', 'sprint', 'sneak'].filter(k => bot.getControlState(k)) })
  }
  sample()
  bot.on('physicsTick', sample)
  return () => {
    bot.removeListener('physicsTick', sample)
    return { elapsedMs: now() - started, samples }
  }
}
