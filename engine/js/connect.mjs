// Makes the mineflayer bot for an agent: reads state/agents/<name>/config.json and the world it names, creates the
// client, loads the pathfinder, resolves once spawned. Nothing here runs on import: no connection is made until
// connectBot or connectAgent is called.
import fs from 'node:fs'
import path from 'node:path'
import mineflayer from 'mineflayer'
import pf from 'mineflayer-pathfinder'

const { pathfinder, Movements } = pf

// same defaults as src/config.mjs: the newest protocol mineflayer speaks, offline auth
export const DEFAULTS = { version: '26.1', auth: 'offline' }
export const AUTH_MODES = ['offline', 'microsoft']
export const SPAWN_TIMEOUT_MS = 60000

const readJson = file => JSON.parse(fs.readFileSync(file, 'utf8'))

// the merged connection settings for an agent: defaults, then its config.json, then the world's {host, port}
export function readAgentConfig ({ stateDir, agent }) {
  const file = path.join(stateDir, 'agents', agent, 'config.json')
  if (!fs.existsSync(file)) throw new Error(`no ${file}: the agent has no config`)
  const config = readJson(file)
  if (!config.world) throw new Error(`${file} names no world: add "world": "<name>", a folder under state/worlds/`)
  const worldFile = path.join(stateDir, 'worlds', config.world, 'world.json')
  if (!fs.existsSync(worldFile)) throw new Error(`${file} names world "${config.world}", but there is no ${worldFile}`)
  const cfg = { ...DEFAULTS, ...config, ...readJson(worldFile) }
  if (!cfg.username) throw new Error(`${file} names no username`)
  if (!AUTH_MODES.includes(cfg.auth)) throw new Error(`${file}: auth "${cfg.auth}" is not one of ${AUTH_MODES.join(', ')}`)
  return cfg
}

// Resolves with the bot once it has spawned and the pathfinder has movements that never dig or build: a primitive
// that walks must not quietly break blocks. Rejects when the server refuses, drops the connection or stays silent.
export function connectBot ({ host, port, username, auth = DEFAULTS.auth, version = DEFAULTS.version }, { timeoutMs = SPAWN_TIMEOUT_MS } = {}) {
  return new Promise((resolve, reject) => {
    const bot = mineflayer.createBot({ host, port, username, auth, version })
    bot.loadPlugin(pathfinder)
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
      const movements = new Movements(bot)
      movements.canDig = false
      movements.allow1by1towers = false
      movements.scafoldingBlocks = [] // sic: the pathfinder's own spelling
      bot.pathfinder.setMovements(movements)
      resolve(bot)
    })
  })
}

// the whole thing from an agent name: { bot, disconnect }
export async function connectAgent ({ stateDir, agent }) {
  const bot = await connectBot(readAgentConfig({ stateDir, agent }))
  return { bot, disconnect: () => bot.quit() }
}
