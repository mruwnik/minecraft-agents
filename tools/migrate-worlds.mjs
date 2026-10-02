#!/usr/bin/env node
// One-shot, idempotent move from the old flat state/ layout to state/worlds/<world>/: team-lead runs this once on
// the real state once every agent's config names its world (src/config.mjs's readWorld).
//   node tools/migrate-worlds.mjs <world-name> [--state <dir>]
import fs from 'node:fs'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { parseArgs as parseNodeArgs } from 'node:util'
import { fileURLToPath } from 'node:url'

const DIR = import.meta.dirname
const ROOT = path.join(DIR, '..')
const WORLD_FILES = ['places.json', 'zones.json', 'gates.log', 'clock.json', 'WORLD.md']

// the world name is a positional argument and --state can come before or after it
export const parseArgs = argv => {
  const { values, positionals } = parseNodeArgs({ args: argv, options: { state: { type: 'string' } }, allowPositionals: true })
  const state = values.state ? path.resolve(values.state) : path.join(ROOT, 'state')
  return { world: positionals[0], state }
}

const agentDirs = state => {
  const agentsDir = path.join(state, 'agents')
  if (!fs.existsSync(agentsDir)) return []
  return fs.readdirSync(agentsDir).sort()
    .map(name => path.join(agentsDir, name))
    .filter(dir => fs.existsSync(path.join(dir, 'config.json')))
}

// body-lock is the one place that already knows a live pid from a dead one and a listening port from a silent one
// (#145); re-deriving that here would just be a second, divergent copy of the same refusal. Any exit but 0 must
// refuse: a crash (an unparsable config.json, say) is not proof the body is down, only that this could not tell.
const runningAgents = state =>
  agentDirs(state)
    .map(dir => ({ dir, status: spawnSync(process.execPath, [path.join(ROOT, 'tools', 'body-lock.mjs'), 'check', dir]).status }))
    .filter(({ status }) => status !== 0)
    .map(({ dir, status }) => ({ name: path.basename(dir), status }))

const readConfig = dir => JSON.parse(fs.readFileSync(path.join(dir, 'config.json'), 'utf8'))
const writeConfig = (dir, cfg) => fs.writeFileSync(path.join(dir, 'config.json'), JSON.stringify(cfg, null, 1) + '\n')

const moveWorldFiles = (state, worldDir, world, changes) => {
  for (const name of WORLD_FILES) {
    const from = path.join(state, name)
    const to = path.join(worldDir, name)
    if (!fs.existsSync(from) || fs.existsSync(to)) continue
    fs.renameSync(from, to)
    changes.push(`moved ${name} to state/worlds/${world}/`)
  }
}

// every config.json with a server still in it, one row per distinct host:port: more than one means the configs
// disagree on which server this world is, which the caller must settle before anything is seeded or rewritten
const distinctServers = state => {
  const byKey = new Map()
  for (const cfg of agentDirs(state).map(readConfig)) {
    if (!cfg.host || cfg.port == null) continue
    byKey.set(`${cfg.host}:${cfg.port}`, { host: cfg.host, port: cfg.port, version: cfg.version })
  }
  return [...byKey.values()]
}

const writeWorldJson = (worldDir, world, seed, changes) => {
  const worldFile = path.join(worldDir, 'world.json')
  if (fs.existsSync(worldFile)) return
  const { host, port, version } = seed
  fs.writeFileSync(worldFile, JSON.stringify(version ? { host, port, version } : { host, port }, null, 1) + '\n')
  changes.push(`wrote state/worlds/${world}/world.json`)
}

const rewriteConfigs = (state, world, changes) => {
  for (const dir of agentDirs(state)) {
    const cfg = readConfig(dir)
    const name = path.basename(dir)
    if (cfg.world && cfg.world !== world) {
      changes.push(`left state/agents/${name}/config.json alone: names world "${cfg.world}"`)
      continue
    }
    if (cfg.world === world && !('host' in cfg) && !('port' in cfg) && !('version' in cfg)) continue
    const { host, port, version, ...rest } = cfg
    writeConfig(dir, { ...rest, world })
    changes.push(`rewrote state/agents/${name}/config.json: world=${world}`)
  }
}

const rewriteBriefings = (state, world, changes) => {
  for (const dir of agentDirs(state)) {
    const file = path.join(dir, 'BRIEFING.md')
    if (!fs.existsSync(file)) continue
    const text = fs.readFileSync(file, 'utf8')
    if (!text.includes('../../WORLD.md')) continue
    fs.writeFileSync(file, text.split('../../WORLD.md').join(`../../worlds/${world}/WORLD.md`))
    changes.push(`rewrote state/agents/${path.basename(dir)}/BRIEFING.md`)
  }
}

function main () {
  const { world, state } = parseArgs(process.argv.slice(2))
  if (!world) {
    console.error('usage: node tools/migrate-worlds.mjs <world-name> [--state <dir>]')
    process.exit(2)
  }

  const running = runningAgents(state)
  if (running.length) {
    const describe = ({ name, status }) => `${name} (${status === 3 ? 'running' : `could not check (exit ${status})`})`
    console.error(`REFUSED: body running for ${running.map(describe).join(', ')}`)
    process.exit(3)
  }

  const worldDir = path.join(state, 'worlds', world)
  const conflicting = WORLD_FILES.filter(name => fs.existsSync(path.join(state, name)) && fs.existsSync(path.join(worldDir, name)))
  if (conflicting.length) {
    console.error(`REFUSED: ${conflicting.join(', ')} exist at both state/ and state/worlds/${world}/`)
    process.exit(2)
  }

  // every write below assumes world.json either exists already or can be seeded: both refusals below must land
  // before moveWorldFiles touches anything, or a refusal leaves the shared files moved with no world.json in sight
  const servers = distinctServers(state)
  if (servers.length > 1) {
    console.error(`REFUSED: configs disagree on server: ${servers.map(s => `${s.host}:${s.port}`).join(', ')}`)
    process.exit(2)
  }
  const worldFile = path.join(worldDir, 'world.json')
  const seed = fs.existsSync(worldFile) ? null : servers[0] ?? null
  if (!fs.existsSync(worldFile) && !seed) {
    console.error(`REFUSED: no ${worldFile} and no agent config.json has host/port to seed it`)
    process.exit(2)
  }

  const changes = []
  const worldDirExisted = fs.existsSync(worldDir)
  fs.mkdirSync(worldDir, { recursive: true })
  if (!worldDirExisted) changes.push(`created state/worlds/${world}/`)

  moveWorldFiles(state, worldDir, world, changes)
  writeWorldJson(worldDir, world, seed, changes)
  rewriteConfigs(state, world, changes)
  rewriteBriefings(state, world, changes)

  if (!changes.length) console.log('nothing to do')
  else changes.forEach(line => console.log(line))
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) main()
