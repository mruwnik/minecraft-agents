// A body's identity: the config.json in its home folder (state/agents/<Name>), written by tools/new-agent.mjs, and the
// world.json of the world it names (state/worlds/<world>). Pure apart from reading files, so the refusals below can be
// tested without a server.
import fs from 'node:fs'
import path from 'node:path'

// no username here on purpose: a body without a config.json used to log in as a default name and kick the
// real body of that name off the server (duplicate_login)
export const DEFAULTS = {
  version: '26.1', // newest protocol mineflayer speaks; ViaBackwards bridges to the newer server
  apiPort: 3777,
  auth: 'offline'
}

export const AUTH_MODES = ['offline', 'microsoft']

export const configFile = home => path.join(home, 'config.json')

export const missingConfig = home =>
  `no ${configFile(home)}: a body runs from its agent folder (cd state/agents/<Name> && ./start), ` +
  'or `node src/bot.mjs <that folder>`; `node tools/new-agent.mjs <Name> --world <world>` makes a new one'

const readJson = file => JSON.parse(fs.readFileSync(file, 'utf8'))

export const readWorld = home => {
  const file = configFile(home)
  if (!fs.existsSync(file)) throw new Error(missingConfig(home))
  const { world: name } = readJson(file)
  if (!name) throw new Error(`${file} names no world: add "world": "<name>", a folder under state/worlds/ (e.g. "world": "main")`)
  const worlds = path.resolve(home, '..', '..', 'worlds')
  const dir = path.join(worlds, name)
  const worldFile = path.join(dir, 'world.json')
  if (!fs.existsSync(worldFile)) {
    const known = fs.existsSync(worlds) ? fs.readdirSync(worlds, { withFileTypes: true }).filter(e => e.isDirectory()).map(e => e.name).sort() : []
    throw new Error(`${file} names world "${name}", but there is no ${worldFile}; worlds there are: ${known.join(', ') || 'none'}`)
  }
  return { name, dir, server: readJson(worldFile) }
}

export const readConfig = home => {
  const file = configFile(home)
  const world = readWorld(home)
  const cfg = { ...DEFAULTS, ...readJson(file), ...world.server, worldDir: world.dir }
  if (!cfg.username) throw new Error(`${file} names no username: ${JSON.stringify(cfg)}`)
  if (!AUTH_MODES.includes(cfg.auth)) throw new Error(`${file}: auth "${cfg.auth}" is not one of ${AUTH_MODES.join(', ')}`)
  return cfg
}
