// Why JavaScript: creates the Mineflayer client and loads its pathfinder plugin, a library boundary with no logic.
// Makes the mineflayer bot for a body: creates the client, loads the pathfinder, resolves once spawned. engine.main
// reads config.json and world.json itself and hands the connection settings to createPrimitives. Nothing here runs on
// import: no connection is made until connectBot is called.
import mineflayer from 'mineflayer'
import pf from 'mineflayer-pathfinder'
import { SafeMovements } from './movements.mjs'
import { fixDigMaterials } from './dig-materials.mjs'
import { MC_VERSION } from './path/snapshot.mjs'
import { tickRatePhysics } from './tick-rate-physics.mjs'
import { installLazyChunks, bufferLightArrays } from './lazy-chunk.mjs'
import { installFastBlockUpdatesOnInject } from './block-updates.mjs'

const { pathfinder } = pf

// same defaults as src/config.mjs: the newest protocol mineflayer speaks, offline auth
// viewDistance: chunks loaded around the body (mineflayer's own default 'far' is 12; ~80-100 KB heap per column)
export const DEFAULTS = { version: MC_VERSION, auth: 'offline', viewDistance: 8 }
export const SPAWN_TIMEOUT_MS = 60000
const DIFFICULTIES = ['peaceful', 'easy', 'normal', 'hard']

// The mineflayer createBot options for a body's connection settings. followTickRate (default on) swaps in
// physics that follows /tick rate, see tick-rate-physics.mjs.
export function botOptions ({ host, port, username, auth = DEFAULTS.auth, version = DEFAULTS.version, viewDistance = DEFAULTS.viewDistance, followTickRate = true }) {
  if (version !== MC_VERSION) throw new Error(`minecraft version ${version} is not supported: the planner block table is built for ${MC_VERSION}`)
  const options = { host, port, username, auth, version, viewDistance, validateChannelProtocol: false }
  return followTickRate ? { ...options, plugins: { physics: tickRatePhysics() } } : options
}

// Since 1.21.3 the server clears its "had a position this tick" flag only on a serverbound tick_end, and a 26.3 server
// kicks the second position packet without one (invalid_player_movement, ~1 s after spawn). Mineflayer never sends it.
export function installTickEnd (bot) {
  if (!bot.registry?.protocol?.play?.toServer?.types?.packet_tick_end) return
  bot.on('physicsTick', () => bot._client.write('tick_end', {}))
}

// A 26.1 server sends the world clocks only on join and on a change (/time, a night skip); its time packet every 20 ticks
// carries the world age alone, and mineflayer runs the clocks on by the body's own physics ticks. Under a lagging server
// the body's day ran thousands of ticks ahead (14985 against the server's 9283: the server refused the bed). So each clock follows
// the server's age since the clock's last update; the physics ticks only fill the second between packets.
export function installServerClock (bot) {
  const bases = new Map()
  bot.on('time', () => {
    const time = bot.time
    const age = Number(time?.age)
    if (!time?.clocks || !Number.isFinite(age)) return
    for (const [name, clock] of Object.entries(time.clocks)) {
      const base = bases.get(name)
      if (base?.clock !== clock) {
        bases.set(name, { clock, age, ticks: Number(clock.totalTicks) + (clock.partialTick ?? 0) })
        continue
      }
      const ticks = base.ticks + (age - base.age) * (clock.rate ?? 1)
      clock.totalTicks = Math.floor(ticks)
      clock.partialTick = ticks - clock.totalTicks
    }
    const clock = time.clocks[bot.game?.dimension]
    if (!clock) return
    time.time = clock.totalTicks
    time.bigTime = BigInt(clock.totalTicks)
    time.timeOfDay = time.time % 24000
    time.day = Math.floor(time.time / 24000)
    time.isDay = time.timeOfDay < 13000
    time.moonPhase = time.day % 8
  })
}

// Mineflayer leaves promises nobody awaits (openVillager's trade-list handler rejects when the window never opens), and
// node exits on one: log it instead, a stray rejection must not take the body down.
let guarded = null
export function guardRejections (proc = process, log = console.error) {
  if (guarded === proc) return
  guarded = proc
  proc.on('unhandledRejection', reason => log('unhandled rejection (ignored):', reason?.stack ?? reason))
}

// Resolves with the bot once it has spawned and the pathfinder has movements that never dig or build and
// steer clear of hazards (see movements.mjs): a primitive that walks must not quietly break blocks or wade into
// lava. Rejects when the server refuses, drops the connection or stays silent.
// params.gameClock (game-clock.mjs) is attached to the client before anything can arrive, so the join packet's rate is caught.
// `create` replaces mineflayer.createBot (tests).
export function connectBot (params, { timeoutMs = SPAWN_TIMEOUT_MS, create = mineflayer.createBot } = {}) {
  return new Promise((resolve, reject) => {
    guardRejections()
    installLazyChunks()
    bufferLightArrays(params.version ?? DEFAULTS.version)
    const bot = create(botOptions(params))
    if (bot._client?.removeAllListeners) installFastBlockUpdatesOnInject(bot)
    params.gameClock?.attach(bot._client)
    installTickEnd(bot)
    installServerClock(bot)
    bot.loadPlugin(pathfinder)
    // Mineflayer does not keep the damage_type registry; primitives names a damage packet's type from it.
    bot._client.on('registry_data', packet => {
      if (packet.id === 'minecraft:damage_type') bot.damageTypeNames = packet.entries.map(e => e.key.replace(/^minecraft:/, ''))
    })
    // Mineflayer keeps no simulation distance; the login packet precedes spawn, so catch it here, on the bot's own client.
    bot._client.on('login', packet => { bot.simulationDistance = packet.simulationDistance ?? bot.simulationDistance })
    // Mineflayer's bot.spawnPoint reads (0,0,0) until the spawn_position packet: mark when the game has sent it.
    bot._client.on('spawn_position', () => { bot.spawnPointKnown = true })
    bot._client.on('simulation_distance', packet => { bot.simulationDistance = packet.distance ?? bot.simulationDistance })
    // 26.1's difficulty packet carries the name, which mineflayer's bot.game.difficulty looks up as an index (undefined).
    bot._client.on('difficulty', ({ difficulty: d }) => { bot.difficulty = typeof d === 'string' ? d : DIFFICULTIES[d] ?? null })
    const timer = setTimeout(() => fail(new Error(`no spawn within ${timeoutMs} ms`)), timeoutMs)
    const onError = err => fail(err)
    const onKicked = reason => fail(new Error(`kicked before spawn: ${JSON.stringify(reason)}`))
    const onEnd = reason => fail(new Error(`connection ended before spawn: ${reason}`))
    // A player killed while its client was gone logs in dead; mineflayer reads the death and respawns before 'spawn'.
    const onDeath = () => { bot.diedAtJoin = { pos: bot.entity?.position?.clone() } }
    const settle = () => {
      clearTimeout(timer)
      bot.removeListener('death', onDeath)
      bot.removeListener('error', onError)
      bot.removeListener('kicked', onKicked)
      bot.removeListener('end', onEnd)
    }
    const fail = err => {
      settle()
      bot.on('error', () => {}) // the ended socket may still report a late error (write EPIPE); nobody is left to listen
      bot.end()
      reject(err)
    }
    bot.on('error', onError)
    bot.on('kicked', onKicked)
    bot.on('end', onEnd)
    bot.once('death', onDeath)
    bot.once('spawn', () => {
      settle()
      fixDigMaterials(bot.registry)
      bot.pathfinder.setMovements(new SafeMovements(bot))
      resolve(bot)
    })
  })
}
