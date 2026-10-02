#!/usr/bin/env node
// One-shot, idempotent move from the old flat state/ layout to state/worlds/<world>/: team-lead runs this once on
// the real state once every agent's config names its world (src/config.mjs's readWorld).
//   node tools/migrate-worlds.mjs <world-name> [--state <dir>]
import fs from 'node:fs'
import path from 'node:path'
import { spawnSync } from 'node:child_process'

const DIR = import.meta.dirname
const ROOT = path.join(DIR, '..')
const WORLD_FILES = ['places.json', 'zones.json', 'gates.log', 'clock.json', 'WORLD.md']

const parseArgs = argv => {
  const stateIdx = argv.indexOf('--state')
  const state = stateIdx === -1 ? path.join(ROOT, 'state') : path.resolve(argv[stateIdx + 1])
  const [world] = argv.filter((_, i) => i !== stateIdx && i !== stateIdx + 1)
  return { world, state }
}

const agentDirs = state => {
  const agentsDir = path.join(state, 'agents')
  if (!fs.existsSync(agentsDir)) return []
  return fs.readdirSync(agentsDir).sort()
    .map(name => path.join(agentsDir, name))
    .filter(dir => fs.existsSync(path.join(dir, 'config.json')))
}

// body-lock is the one place that already knows a live pid from a dead one and a listening port from a silent one
// (#145); re-deriving that here would just be a second, divergent copy of the same refusal.
const runningAgents = state =>
  agentDirs(state)
    .filter(dir => spawnSync('node', [path.join(ROOT, 'tools', 'body-lock.mjs'), 'check', dir]).status === 3)
    .map(dir => path.basename(dir))

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

const writeWorldJson = (state, worldDir, world, changes) => {
  const worldFile = path.join(worldDir, 'world.json')
  if (fs.existsSync(worldFile)) return
  const server = agentDirs(state).map(readConfig).find(cfg => cfg.host && cfg.port)
  if (!server) {
    console.error(`REFUSED: no ${worldFile} and no agent config.json has host/port to seed it`)
    process.exit(2)
  }
  fs.writeFileSync(worldFile, JSON.stringify({ host: server.host, port: server.port }, null, 1) + '\n')
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
    if (cfg.world === world && !('host' in cfg) && !('port' in cfg)) continue
    const { host, port, ...rest } = cfg
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

const { world, state } = parseArgs(process.argv.slice(2))
if (!world) {
  console.error('usage: node tools/migrate-worlds.mjs <world-name> [--state <dir>]')
  process.exit(2)
}

const running = runningAgents(state)
if (running.length) {
  console.error(`REFUSED: body running for ${running.join(', ')}`)
  process.exit(3)
}

const worldDir = path.join(state, 'worlds', world)
const conflicting = WORLD_FILES.filter(name => fs.existsSync(path.join(state, name)) && fs.existsSync(path.join(worldDir, name)))
if (conflicting.length) {
  console.error(`REFUSED: ${conflicting.join(', ')} exist at both state/ and state/worlds/${world}/`)
  process.exit(2)
}

const changes = []
const worldDirExisted = fs.existsSync(worldDir)
fs.mkdirSync(worldDir, { recursive: true })
if (!worldDirExisted) changes.push(`created state/worlds/${world}/`)

moveWorldFiles(state, worldDir, world, changes)
writeWorldJson(state, worldDir, world, changes)
rewriteConfigs(state, world, changes)
rewriteBriefings(state, world, changes)

if (!changes.length) console.log('nothing to do')
else changes.forEach(line => console.log(line))
