// Create a new agent: a folder under state/agents/ with its own name, config, log, journal and tool wrappers.
//   node tools/new-agent.mjs                      draw a name from Dan's generator (~/.claude/hooks/choose_name.py)
//   node tools/new-agent.mjs Lightsong            use this name
//   node tools/new-agent.mjs [Name] --harness codex   the program that will run the agent: one of the notes files in harness/ (default claude-code)
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { minecraftName, parseChosenName, nextPort, newAgentArgs } from '../src/lib.mjs'

const DIR = import.meta.dirname
const ROOT = path.join(DIR, '..')
const AGENTS = path.join(ROOT, 'state', 'agents')
const NAME_SCRIPT = path.join(os.homedir(), '.claude/hooks/choose_name.py')
const HARNESSES = fs.readdirSync(path.join(ROOT, 'harness')).filter(f => f.endsWith('.md') && f !== 'README.md').map(f => f.slice(0, -3)).sort()

const wanted = newAgentArgs(process.argv.slice(2), HARNESSES)
if (wanted.error) { console.error(wanted.error); process.exit(2) }

const existing = fs.existsSync(AGENTS) ? fs.readdirSync(AGENTS).filter(n => fs.existsSync(path.join(AGENTS, n, 'config.json'))) : []
const configs = existing.map(n => JSON.parse(fs.readFileSync(path.join(AGENTS, n, 'config.json'), 'utf8')))
const taken = new Set(existing.map(n => n.toLowerCase()))

const draw = () => parseChosenName(execFileSync('python3', [NAME_SCRIPT], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'inherit'] }))
const usable = c => { const name = minecraftName(c.name); return name && !taken.has(name.toLowerCase()) ? { ...c, username: name } : null }

function pickCharacter (wanted) {
  if (wanted) {
    const c = usable({ name: wanted, source: 'chosen by hand', note: '' })
    if (!c) throw new Error(`"${wanted}" is taken or not a valid Minecraft name (3-16 of A-Z a-z 0-9 _)`)
    return c
  }
  for (let i = 0; i < 30; i++) {
    const c = usable(draw())
    if (c) return c
  }
  throw new Error('no usable name after 30 draws')
}

const character = pickCharacter(wanted.name)
const home = path.join(AGENTS, character.username)
const apiPort = nextPort(configs.map(c => c.apiPort ?? 3777))
const script = (name, body) => fs.writeFileSync(path.join(home, name), `#!/bin/bash\n${body}\n`, { mode: 0o755 })

fs.mkdirSync(path.join(home, 'snapshots'), { recursive: true })
fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify({ username: character.username, apiPort, harness: wanted.harness, character: { name: character.name, source: character.source, note: character.note } }, null, 1) + '\n')
script('mc', '# drive this agent\'s body: ./mc <action> key=value ...\nMC_HOME="$(dirname "$(readlink -f "$0")")" exec node "$(dirname "$(readlink -f "$0")")/../../../tools/mc.mjs" "$@"')
script('start', '# start this agent\'s body (run it in the background); output goes to bot.log\nexec "$(dirname "$(readlink -f "$0")")/../../../tools/start-body" "$(dirname "$(readlink -f "$0")")"')
fs.writeFileSync(path.join(home, 'journal.md'), `# ${character.username}'s journal\n\nNewest entry last. Keep entries short: what you did, what you learned, what you promised, where things are.\n`)
fs.writeFileSync(path.join(home, 'BRIEFING.md'), `# You are ${character.username}

Your name comes from ${character.source}${character.note ? ` (${character.note})` : ''}. In this Minecraft world it is your player name: a body of your own,
on a survival server shared with people and other agents like you (\`../../WORLD.md\` says who). Play, build, help out, have fun.

Everything that is yours lives in this folder, and you work from it:

- \`./start\` starts your body (run it in the background). \`./mc <action> key=value\` drives it.
- \`events.jsonl\` is what happens to you (chat, damage, deaths, nightfall). Follow it; answer when people talk to you.
- \`snapshots/\` is where \`./mc look\` puts what you see.
- \`journal.md\` is your memory between sessions. Read it first. Write to it before you stop.

First call after \`./start\`: \`./mc state\`. If it says \`time=night\` and you have no bed and no sword, don't explore:
get to shelter and sleep, or stop your body (\`./mc quit\`) and block on \`./mc dawn\` with a command timeout of about
10 minutes: it returns when it is day. \`../../WORLD.md\` says where a new agent finds a bed, a sword and food.

Read, in this order:

1. \`../../../harness/${wanted.harness}.md\`: how your harness waits and delegates.
2. \`../../../AGENT_GUIDE.md\`: the full toolset, the house rules, and how to avoid wasting tokens.
3. \`../../WORLD.md\`: this server, its people, shared places and customs.
4. \`journal.md\`: what you did last time.
`)

console.log(`created state/agents/${character.username}  (${character.source}${character.note ? ', ' + character.note : ''})  api port ${apiPort}  harness ${wanted.harness}`)
// whitelist through the narrow RCON tool; if that isn't set up or the server is down, fall back to asking Dan
try {
  console.log(execFileSync('node', [path.join(DIR, 'rcon.mjs'), character.username], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim())
} catch {
  console.log(`NOT WHITELISTED. Dan: run this on the server console so it can join ->  whitelist add ${character.username}`)
}
