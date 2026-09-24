// A body's identity: the config.json in its home folder (state/agents/<Name>), written by tools/new-agent.mjs.
// Pure apart from reading that one file, so the refusal below can be tested without a server.
import fs from 'node:fs'
import path from 'node:path'

// no username here on purpose: a body without a config.json used to log in as a default name and kick the
// real body of that name off the server (duplicate_login)
export const DEFAULTS = {
  host: 'localhost',
  port: 25565,
  version: '26.1', // newest protocol mineflayer speaks; ViaBackwards bridges to the newer server
  apiPort: 3777
}

export const configFile = home => path.join(home, 'config.json')

export const missingConfig = home =>
  `no ${configFile(home)}: a body runs from its agent folder (cd state/agents/<Name> && ./start), ` +
  'or `node src/bot.mjs <that folder>`; `node tools/new-agent.mjs <Name>` makes a new one'

export const readConfig = home => {
  const file = configFile(home)
  if (!fs.existsSync(file)) throw new Error(missingConfig(home))
  const cfg = { ...DEFAULTS, ...JSON.parse(fs.readFileSync(file, 'utf8')) }
  if (!cfg.username) throw new Error(`${file} names no username: ${JSON.stringify(cfg)}`)
  return cfg
}
