// Why JavaScript: replaces Mineflayer's physics plugin (a Mineflayer boundary); the logic is a handful of text patches to its source.
// Physics that follows the server's tick rate. Mineflayer's own plugin ticks every 50 ms for ever; with /tick rate R the
// server runs R ticks a second, so the body must too (set_ticking_state: rate and frozen; step_tick: run n ticks).
// Instead of a copy of the 500-line plugin this loads mineflayer's own physics.js, patches the few lines that hold the
// 50 ms (each patch must match, else the bot runs stock physics and bot.tickRateStock names why), and runs it per bot. At 20 TPS
// nothing differs from stock. Pass as `plugins.physics` to createBot (loader.js takes a function as a replacement).
import fs from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import { performance } from 'node:perf_hooks'

const require = createRequire(import.meta.url)
const STOCK_PATH = require.resolve('mineflayer/lib/plugins/physics.js')
const VANILLA_RATE = 20

// [stock text, replacement]: the interval becomes a variable, the accumulator step follows it, a frozen clock ticks
// nothing, and the hooks reach the plugin's closure. Yaw and pitch steps stay per tick (PHYSICS_TIMESTEP, 0.05).
const PATCHES = [
  ['const PHYSICS_INTERVAL_MS = 50', 'let PHYSICS_INTERVAL_MS = 50'],
  ['while (timeAccumulator >= PHYSICS_TIMESTEP) {', 'while (timeAccumulator >= PHYSICS_INTERVAL_MS / 1000) {'],
  ['timeAccumulator -= PHYSICS_TIMESTEP', 'timeAccumulator -= PHYSICS_INTERVAL_MS / 1000'],
  ['lastPhysicsFrameTime = now\n', 'lastPhysicsFrameTime = now\n    if (hooks.frozen) { timeAccumulator = 0; return }\n'],
  ["  bot.on('end', cleanup)\n}", `  hooks.setIntervalMs = ms => {
    PHYSICS_INTERVAL_MS = ms
    if (doPhysicsTimer === null) return
    clearInterval(doPhysicsTimer)
    doPhysicsTimer = setInterval(doPhysics, ms)
  }
  hooks.step = n => { for (let i = 0; i < n; i++) tickPhysics(performance.now()) }
  bot.on('end', cleanup)
}`]
]

function patchedSource (stockSource) {
  return PATCHES.reduce((src, [from, to]) => {
    if (!src.includes(from)) throw new Error(`mineflayer physics.js changed: patch target not found: ${from.trim()}`)
    return src.replace(from, to)
  }, stockSource)
}

// Stock plugin (patched) as a function of (bot, options); its relative requires resolve against mineflayer, its
// perf_hooks clock is ours and `hooks` is the object the patches reach into.
function load (hooks, clock, source) {
  const stockRequire = createRequire(STOCK_PATH)
  const localRequire = id => id === 'perf_hooks' ? { performance: clock } : stockRequire(id)
  const module = { exports: {} }
  const wrapper = new Function('exports', 'require', 'module', '__filename', '__dirname', 'hooks', patchedSource(source) + '\n;return module.exports') // eslint-disable-line no-new-func
  return wrapper(module.exports, localRequire, module, STOCK_PATH, path.dirname(STOCK_PATH), hooks)
}

// The plugin for createBot. `now` is the clock the physics accumulator reads, `source` the stock plugin text (tests
// inject them). A patch that does not match runs stock physics instead: onStock gets the reason, bot.tickRateStock too.
export function tickRatePhysics ({ now = () => performance.now(), source = fs.readFileSync(STOCK_PATH, 'utf8'), onStock = () => {} } = {}) {
  return (bot, options) => {
    const hooks = { frozen: false }
    let plugin
    try {
      plugin = load(hooks, { now }, source)
    } catch (err) {
      bot.tickRateStock = err.message
      onStock(err.message)
      require(STOCK_PATH)(bot, options)
      return
    }
    plugin(bot, options)
    let rate = VANILLA_RATE
    const clock = {
      rate: () => rate,
      intervalMs: () => 1000 / rate,
      frozen: () => hooks.frozen,
      digMs: ms => ms * (VANILLA_RATE / rate) // bot.digTime counts 50 ms a tick
    }
    bot.physicsClock = clock
    const stockDigTime = bot.digTime
    if (typeof stockDigTime === 'function') bot.digTime = (...args) => clock.digMs(stockDigTime(...args))
    bot._client.on('set_ticking_state', packet => {
      hooks.frozen = Boolean(packet.is_frozen)
      if (packet.tick_rate > 0 && packet.tick_rate !== rate) {
        rate = packet.tick_rate
        hooks.setIntervalMs(1000 / rate)
      }
    })
    bot._client.on('step_tick', packet => {
      if (hooks.frozen) hooks.step(packet.tick_steps)
    })
  }
}
