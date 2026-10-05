// Makes the mineflayer bot for a body: creates the client, loads the pathfinder, resolves once spawned. engine.main
// reads config.json and world.json itself and hands the connection settings to createPrimitives. Nothing here runs on
// import: no connection is made until connectBot is called.
import mineflayer from 'mineflayer'
import pf from 'mineflayer-pathfinder'
import { SafeMovements } from './movements.mjs'

const { pathfinder } = pf

// same defaults as src/config.mjs: the newest protocol mineflayer speaks, offline auth
export const DEFAULTS = { version: '26.1', auth: 'offline' }
export const SPAWN_TIMEOUT_MS = 60000

// Resolves with the bot once it has spawned and the pathfinder has movements that never dig or build and
// steer clear of hazards (see movements.mjs): a primitive that walks must not quietly break blocks or wade into
// lava. Rejects when the server refuses, drops the connection or stays silent.
export function connectBot ({ host, port, username, auth = DEFAULTS.auth, version = DEFAULTS.version }, { timeoutMs = SPAWN_TIMEOUT_MS } = {}) {
  return new Promise((resolve, reject) => {
    const bot = mineflayer.createBot({ host, port, username, auth, version })
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
      bot.end()
      reject(err)
    }
    bot.on('error', onError)
    bot.on('kicked', onKicked)
    bot.on('end', onEnd)
    bot.once('spawn', () => {
      settle()
      bot.pathfinder.setMovements(new SafeMovements(bot))
      resolve(bot)
    })
  })
}
