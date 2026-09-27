import { createRequire } from 'node:module'
const require = createRequire(import.meta.url)
const { simplify } = require('prismarine-nbt')
const resource = name => typeof name === 'string' && !name.includes(':') ? `minecraft:${name}` : name
const bigint = value => typeof value === 'bigint' ? value : Array.isArray(value) ? BigInt.asIntN(64, BigInt(value[0]) << 32n | BigInt(value[1] >>> 0)) : BigInt(value)

// 26.1 clocks advance on server ticks, not player physics. In particular,
// boarding disables Mineflayer physicsTick but must not freeze night guards.
// Keep private packet anchors: Mineflayer mutates its own clock objects on each
// physicsTick, so reading those totals would double-count walking time.
export function installWorldClock (bot, { now = () => performance.now(), setInterval: setTimer = setInterval, clearInterval: clearTimer = clearInterval } = {}) {
  if (!bot.time) { bot.loadPlugin(injected => installWorldClock(injected, { now, setInterval: setTimer, clearInterval: clearTimer })); return }
  const models = new Map(), names = new Map(), defaults = new Map()
  let modern = false, lastAge = null, serverRate = 20, frozen = false, ended = false
  let epoch = now(), projected = 0, pendingSteps = 0
  const sinceEpoch = () => ended ? 0 : Math.max(0, now() - epoch) * serverRate / 1000
  const prediction = () => projected + (frozen ? Math.min(pendingSteps, sinceEpoch()) : sinceEpoch())
  const value = (model, ticks = prediction()) => {
    const part = model.partial + ticks * model.rate, whole = Math.floor(part)
    return { total: model.total + BigInt(whole), partial: part - whole }
  }
  const advance = ticks => {
    for (const model of models.values()) Object.assign(model, value(model, ticks))
  }
  const holdProjection = () => {
    const ticks = frozen ? Math.min(pendingSteps, sinceEpoch()) : sinceEpoch()
    projected += ticks
    if (frozen) pendingSteps -= ticks
    epoch = now()
  }
  const current = () => {
    const dimension = resource(bot.game?.dimension)
    if (!dimension) return null
    const wanted = defaults.has(dimension) ? defaults.get(dimension) : dimension
    for (const [id, model] of models) {
      const name = names.get(id) ?? resource(bot.registry?.dimensionsById?.[id]?.name)
      if (name === wanted) return model
    }
    return null
  }
  const snapshot = () => {
    const model = modern && current()
    if (!model) return null
    const total = value(model).total, timeOfDay = Number((total % 24000n + 24000n) % 24000n)
    const day = Math.floor(Number(total) / 24000)
    const bigAge = lastAge + BigInt(Math.floor(prediction()))
    return { bigTime: total, time: Number(total), timeOfDay, day, isDay: timeOfDay < 13000, moonPhase: (day % 8 + 8) % 8, doDaylightCycle: model.rate !== 0, bigAge, age: Number(bigAge) }
  }
  const fields = ['bigTime', 'time', 'timeOfDay', 'day', 'isDay', 'moonPhase', 'doDaylightCycle', 'bigAge', 'age']
  for (const field of fields) {
    let fallback = bot.time[field]
    Object.defineProperty(bot.time, field, { enumerable: true, configurable: true, get: () => snapshot()?.[field] ?? fallback, set: next => { fallback = next } })
  }
  const registry = packet => {
    if (packet.id === 'minecraft:world_clock') {
      names.clear()
      for (const [id, entry] of packet.entries.entries()) names.set(id, resource(entry.key))
    } else if (packet.id === 'minecraft:dimension_type') {
      defaults.clear()
      for (const entry of packet.entries) {
        const data = entry.value ? simplify(entry.value) : null
        if (data && Object.hasOwn(data, 'default_clock')) defaults.set(resource(entry.key), resource(data.default_clock))
      }
    }
  }
  const update = packet => {
    if (!Array.isArray(packet.clockUpdates)) { modern = false; return }
    modern = true
    const age = bigint(packet.age)
    // Server age packets correct wall-clock prediction under lag/freeze. Use
    // age since the previous authoritative anchor, never extrapolated totals.
    if (lastAge !== null) {
      const delta = age - lastAge
      if (delta >= 0n && delta <= BigInt(Number.MAX_SAFE_INTEGER)) {
        advance(Number(delta))
        if (frozen) pendingSteps = Math.max(0, pendingSteps - Number(delta))
      }
    }
    lastAge = age
    projected = 0; epoch = now()
    for (const update of packet.clockUpdates) {
      if (!Number.isFinite(update.rate) || !Number.isFinite(update.partialTick)) continue
      models.set(update.id, { total: bigint(update.totalTicks), partial: update.partialTick, rate: update.rate })
    }
  }
  const ticking = packet => {
    holdProjection()
    if (Number.isFinite(packet.tick_rate) && packet.tick_rate > 0) serverRate = packet.tick_rate
    frozen = Boolean(packet.is_frozen)
    if (!frozen) pendingSteps = 0
  }
  const step = packet => {
    if (!frozen || !Number.isInteger(packet.tick_steps) || packet.tick_steps < 0) return
    // A step command schedules ticks; advance them at the server tick rate,
    // bounded by the scheduled count, until authoritative age packets correct it.
    holdProjection()
    pendingSteps = packet.tick_steps
  }
  bot._client.on('registry_data', registry)
  // Native time listeners emit their event while processing the packet. Apply
  // its absolute reset before that event reaches dusk/dawn consumers.
  bot._client.prependListener('update_time', update)
  bot._client.on('set_ticking_state', ticking)
  bot._client.on('step_tick', step)
  const timer = setTimer(() => { if (modern && current() && !ended) bot.emit('time') }, 1000)
  timer?.unref?.()
  bot.once('end', () => {
    holdProjection(); ended = true
    clearTimer(timer)
    bot._client.removeListener('registry_data', registry)
    bot._client.removeListener('update_time', update)
    bot._client.removeListener('set_ticking_state', ticking)
    bot._client.removeListener('step_tick', step)
  })
}
