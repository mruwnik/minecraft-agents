// Picking and starting another agent's body: names, ports, launch args, and the kill list a body must never touch.

// A Minecraft username is 3-16 of [A-Za-z0-9_]; squeeze a character's name into that, or null if it can't be done.
export function minecraftName (raw) {
  const squeezed = raw.normalize('NFD').replace(/[̀-ͯ]/g, '').replace(/[\s'’.\-]/g, '')
  return /^[A-Za-z0-9_]{3,16}$/.test(squeezed) ? squeezed : null
}

// Output of ~/.claude/hooks/choose_name.py: "✨ Name", "   Source: X", optionally "   a note".
export function parseChosenName (output) {
  const [first = '', ...rest] = output.split('\n').map(l => l.trim()).filter(Boolean)
  const source = rest.find(l => l.startsWith('Source:'))?.slice('Source:'.length).trim() ?? ''
  return { name: first.replace(/^\W+/u, '').trim(), source, note: rest.find(l => !l.startsWith('Source:')) ?? '' }
}

// A second ./start for the same agent (#145): Chani's resumed agent found its body down, ran ./start more than once,
// and two of her bodies traded one login every 10 s until she killed PIDs off `ps aux | grep node.*bot.mjs`. Every
// body has the IDENTICAL command line - the agent is only in the cwd - so that killed Perrin's, Mariel's and mine
// too. The launcher asks this before it does anything, log rotation included: a second start used to wipe the
// running body's bot.log, which is the evidence of whatever went wrong.
// A pid can be reused by something else entirely, so a pid file alone is not proof: the process must still look like
// a body. The control port answering is the other half - a body started before pid files existed leaves none.
// the launcher wrapper counts too: it holds the pid file while check-code, patch-deps and textures run before the body
const bodyProcess = cmdline => /bot\.mjs|start-body/.test(String(cmdline ?? ''))
const NEVER_KILL = 'Never kill a process: every body on this machine has the identical command line, so `pkill -f bot.mjs` or a PID off `ps` takes down other agents\' bodies too (#145). `./mc quit` is the only way down'
export function bodyRefusal ({ pid, cmdline, listening, port }) {
  if (pid && bodyProcess(cmdline)) return `your body is already up (pid ${pid}): ./mc state. ${NEVER_KILL}, and a body that will not answer is a message to your lead, not something to kill`
  if (listening) return `something already answers on port ${port}, this agent's control port, though no body of mine wrote a pid file: run ./mc state. If it answers, that IS your body and it is up. ${NEVER_KILL}`
  return null
}

const FIRST_API_PORT = 3777
export const nextPort = used => {
  const taken = new Set(used)
  for (let port = FIRST_API_PORT; ; port++) if (!taken.has(port)) return port
}

// new-agent.mjs's command line: an optional name and `--harness <name>` (one of the notes files in harness/, default claude-code).
// Returns { name, harness } (name null when it should be drawn) or { error }.
const DEFAULT_HARNESS = 'claude-code'
export function newAgentArgs (argv, harnessesAvailable) {
  const known = harnessesAvailable.join(', ')
  let name = null
  let harness = DEFAULT_HARNESS
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i]
    if (arg === '--harness' || arg.startsWith('--harness=')) {
      const value = arg === '--harness' ? argv[++i] : arg.slice('--harness='.length)
      if (!value) return { error: `--harness needs a name: one of ${known}` }
      harness = value
      continue
    }
    if (arg.startsWith('-')) return { error: `unknown option ${arg} (only --harness <name> is understood)` }
    if (name) return { error: `one name only: got "${name}" and "${arg}"` }
    name = arg
  }
  if (!harnessesAvailable.includes(harness)) return { error: `unknown harness "${harness}": the ones with notes in harness/ are ${known}` }
  return { name, harness }
}
