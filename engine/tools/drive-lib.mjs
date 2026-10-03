import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'

const CONTROLS = ['forward', 'back', 'left', 'right', 'jump', 'sneak', 'sprint']

export const defaultStateDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..', 'state')

export const usage = `usage: drive.mjs <agent> <op> [args] [--who claude] [--state <dir>]
  take --why "<text>" | hold <control>[,<control>...] <ms> | look <yaw> <pitch>
  turn <dyaw> [dpitch] | jump | stop | ping | state | release [--force]
  controls: ${CONTROLS.join(' ')}`

export const socketPathFor = ({ state, agent }) => path.join(state, 'agents', agent, 'engine', 'control.sock')

const options = {
  who: { type: 'string', default: 'claude' },
  state: { type: 'string', default: defaultStateDir },
  why: { type: 'string', default: '' },
  force: { type: 'boolean', default: false }
}

// parseArgs would read "-10" as a short option, so hide negative numbers from it
const NEG = /^-\d+(\.\d+)?$/
const hide = (t) => (NEG.test(t) ? `\0${t}` : t)
const unhide = (t) => (t.startsWith('\0') ? t.slice(1) : t)

const num = (s) => (s !== undefined && s.trim() !== '' && Number.isFinite(Number(s)) ? Number(s) : null)
const post = (body) => ({ method: 'POST', path: '/drive', body })

const builders = {
  take: ({ who, why }) => post({ op: 'take', who, why }),
  stop: ({ who }) => post({ op: 'stop', who }),
  ping: ({ who }) => post({ op: 'ping', who }),
  release: ({ who, force }) => post({ op: 'release', who, ...(force ? { force: true } : {}) }),
  state: () => ({ method: 'GET', path: '/drive', body: null }),
  jump: ({ who }) => post({ op: 'set', who, controls: { jump: true }, ms: 300 }),
  hold: ({ who, args }) => {
    const [names, msText] = args
    if (names === undefined || msText === undefined) return { error: 'hold needs <control>[,<control>...] <ms>' }
    const list = names.split(',')
    const bad = list.find(c => !CONTROLS.includes(c))
    if (bad !== undefined) return { error: `unknown control ${bad}` }
    const ms = num(msText)
    if (ms === null || !Number.isInteger(ms)) return { error: 'ms must be an integer' }
    return post({ op: 'set', who, controls: Object.fromEntries(list.map(c => [c, true])), ms })
  },
  look: ({ who, args }) => {
    const [yaw, pitch] = args.map(num)
    if (yaw === null || yaw === undefined || pitch === null || pitch === undefined) return { error: 'look needs <yaw> <pitch> (numbers)' }
    return post({ op: 'set', who, look: { yaw, pitch } })
  },
  turn: ({ who, args }) => {
    const dyaw = num(args[0])
    const dpitch = args[1] === undefined ? 0 : num(args[1])
    if (dyaw === null || dpitch === null) return { error: 'turn needs <dyaw> [dpitch] (numbers)' }
    return post({ op: 'set', who, look: { dyaw, dpitch } })
  }
}

export function requestFor (argv) {
  let parsed
  try {
    parsed = parseArgs({ args: argv.map(hide), options, allowPositionals: true, allowNegative: false })
  } catch (e) {
    return { error: e.message }
  }
  const [agent, op, ...args] = parsed.positionals.map(unhide)
  if (agent === undefined || op === undefined) return { error: 'need <agent> and <op>' }
  if (!Object.hasOwn(builders, op)) return { error: `unknown op ${op}` }
  const req = builders[op]({ ...parsed.values, args })
  if (req.error) return req
  return { agent, state: parsed.values.state, ...req }
}

export const exitCodeFor = ({ status, json }) => (status >= 200 && status < 300 && json?.ok === true ? 0 : 1)
