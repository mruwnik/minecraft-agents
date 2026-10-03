// The live tester's courses as fixture snapshots: its server commands (setblock / fill) are replayed into fixture entries
// on top of the lane as the tester prepares it, so unit tests and live runs share their terrain.
import { fixtureSnapshot } from './fixture.mjs'
import { COURSES } from './courses-data.mjs'

const BLOCK = /^(?:minecraft:)?([a-z0-9_]+)(?:\[([^\]]*)\])?$/
// commands that change entities or the player, not terrain
const NOT_TERRAIN = new Set(['summon', 'kill', 'say', 'tp', 'effect'])

// names the live scripts use that this version's registry renamed
const RENAMED = { chain: 'iron_chain' }

const propValue = v => v === 'true' ? true : v === 'false' ? false : /^-?\d+$/.test(v) ? Number(v) : v

export function parseBlock (text) {
  const match = BLOCK.exec(text)
  if (!match) throw new Error(`cannot parse block '${text}'`)
  const [, written, props] = match
  const name = RENAMED[written] ?? written
  if (props === undefined || props === '') return [name]
  return [name, Object.fromEntries(props.split(',').map(p => p.split('=')).map(([k, v]) => [k, propValue(v)]))]
}

const coords = (tokens, what) => {
  if (tokens.some(t => t.startsWith('~') || t.startsWith('^'))) throw new Error(`${what}: relative coordinates are not supported`)
  const n = tokens.map(Number)
  if (n.some(Number.isNaN)) throw new Error(`${what}: bad coordinates ${tokens.join(' ')}`)
  return n
}

// one command -> fixture fill entries (a setblock is a one-cell fill, so order between the two is kept)
const entryFor = command => {
  const [verb, ...args] = command.trim().split(/\s+/)
  if (NOT_TERRAIN.has(verb)) return []
  if (verb === 'setblock') {
    const [x, y, z] = coords(args.slice(0, 3), 'setblock')
    const mode = args[4]
    if (mode !== undefined && mode !== 'replace' && mode !== 'destroy') throw new Error(`setblock mode '${mode}' is not supported: ${command}`)
    return [[x, y, z, x, y, z, ...parseBlock(args[3])]]
  }
  if (verb === 'fill') {
    const [x0, y0, z0, x1, y1, z1] = coords(args.slice(0, 6), 'fill')
    const mode = args[7]
    // replace without a filter replaces everything: the same as a plain fill
    if (mode === 'replace' && args.length > 8) throw new Error(`fill replace with a filter is not supported: ${command}`)
    if (mode !== undefined && mode !== 'replace' && mode !== 'destroy') throw new Error(`fill mode '${mode}' is not supported: ${command}`)
    return [[Math.min(x0, x1), Math.min(y0, y1), Math.min(z0, z1), Math.max(x0, x1), Math.max(y0, y1), Math.max(z0, z1), ...parseBlock(args[6])]]
  }
  throw new Error(`unsupported command '${verb}': ${command}`)
}

// entries: fixture `fill` entries so far; returns them plus the commands' entries, in order
export const applyCommands = (entries, cmds) => [...entries, ...cmds.flatMap(entryFor)]

const fill = (x0, y0, z0, x1, y1, z1, name) => [x0, y0, z0, x1, y1, z1, name]

// the lanes as the live scripts clear and floor them: stone floor at y160, glass walls; everything else is air
const LANES = {
  tricky: () => [
    fill(2853, 160, 3208, 2907, 160, 3224, 'stone'),
    fill(2853, 161, 3208, 2907, 170, 3208, 'glass'), fill(2853, 161, 3224, 2907, 170, 3224, 'glass'),
    fill(2853, 161, 3209, 2853, 170, 3223, 'glass'), fill(2907, 161, 3209, 2907, 170, 3223, 'glass')
  ],
  bamboo: ({ walls }) => [
    fill(2853, 160, 3208, 2907, 160, 3224, 'stone'),
    ...(walls === false ? [] : [fill(2853, 161, 3208, 2907, 167, 3208, 'glass'), fill(2853, 161, 3224, 2907, 167, 3224, 'glass')]),
    fill(2853, 161, 3209, 2853, 167, 3223, 'glass'), fill(2907, 161, 3209, 2907, 167, 3223, 'glass')
  ],
  hazards: () => [
    fill(2854, 160, 3206, 2906, 160, 3226, 'stone'),
    fill(2853, 161, 3213, 2907, 163, 3213, 'glass'), fill(2853, 161, 3219, 2907, 163, 3219, 'glass')
  ]
}

export const courseNames = () => Object.keys(COURSES)

// a lane of the given family (see LANES) with the commands replayed into it: for ad-hoc variations of a course
export const laneSnapshot = (family, cmds, options = {}) => fixtureSnapshot({ fill: applyCommands(LANES[family](options), cmds) })

// { snapshot, from, goal }: the course's terrain, the start cell (with the exact position as px, pz) and the go-to goal
// (a 'near' goal of range 1, the job's default)
export function courseSnapshot (name) {
  const course = COURSES[name]
  if (!course) throw new Error(`unknown course ${name}`)
  const { start, to } = course
  return {
    snapshot: laneSnapshot(course.family, course.cmds, course),
    from: { x: Math.floor(start.x), y: start.y, z: Math.floor(start.z), px: start.x, pz: start.z },
    goal: { kind: 'near', x: Math.floor(to.x), y: to.y, z: Math.floor(to.z), range: 1 }
  }
}
