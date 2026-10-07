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

const { pathfinder } = pf

// same defaults as src/config.mjs: the newest protocol mineflayer speaks, offline auth
// viewDistance: chunks loaded around the body (mineflayer's own default 'far' is 12; ~80-100 KB heap per column)
export const DEFAULTS = { version: MC_VERSION, auth: 'offline', viewDistance: 8 }
export const SPAWN_TIMEOUT_MS = 60000

// The mineflayer createBot options for a body's connection settings. followTickRate (default on) swaps in
// physics that follows /tick rate, see tick-rate-physics.mjs.
export function botOptions ({ host, port, username, auth = DEFAULTS.auth, version = DEFAULTS.version, viewDistance = DEFAULTS.viewDistance, followTickRate = true }) {
  if (version !== MC_VERSION) throw new Error(`minecraft version ${version} is not supported: the planner block table is built for ${MC_VERSION}`)
  const options = { host, port, username, auth, version, viewDistance }
  return followTickRate ? { ...options, plugins: { physics: tickRatePhysics() } } : options
}

// Resolves with the bot once it has spawned and the pathfinder has movements that never dig or build and
// steer clear of hazards (see movements.mjs): a primitive that walks must not quietly break blocks or wade into
// lava. Rejects when the server refuses, drops the connection or stays silent.
// params.gameClock (game-clock.mjs) is attached to the client before anything can arrive, so the join packet's rate is caught.
// `create` replaces mineflayer.createBot (tests).
export function connectBot (params, { timeoutMs = SPAWN_TIMEOUT_MS, create = mineflayer.createBot } = {}) {
  return new Promise((resolve, reject) => {
    const bot = create(botOptions(params))
    params.gameClock?.attach(bot._client)
    bot.loadPlugin(pathfinder)
    // Mineflayer does not keep the damage_type registry; primitives names a damage packet's type from it.
    bot._client.on('registry_data', packet => {
      if (packet.id === 'minecraft:damage_type') bot.damageTypeNames = packet.entries.map(e => e.key.replace(/^minecraft:/, ''))
    })
    const timer = setTimeout(() => fail(new Error(`no spawn within ${timeoutMs} ms`)), timeoutMs)
    const onError = err => fail(err)
    const onKicked = reason => fail(new Error(`kicked before spawn: ${JSON.stringify(reason)}`))
    const onEnd = reason => fail(new Error(`connection ended before spawn: ${reason}`))
    const settle = () => {
      clearTimeout(timer)
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
    bot.once('spawn', () => {
      settle()
      fixDigMaterials(bot.registry)
      bot.pathfinder.setMovements(new SafeMovements(bot))
      resolve(bot)
    })
  })
}
