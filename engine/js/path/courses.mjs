// The live tester's courses as fixture snapshots: its server commands (setblock / fill) are replayed into fixture entries
// on top of the lane as the tester prepares it, so unit tests and live runs share their terrain.
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { fixtureSnapshot, stateId } from './fixture.mjs'
import { UNLOADED } from './snapshot.mjs'
import { defaultStateTable } from './blocks.mjs'
import { COURSES } from './courses-data.mjs'

const MC_VERSION = '26.1'
const registry = prismarineRegistry(MC_VERSION)
const Block = prismarineBlock(registry)

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

// ---- what the server does after the commands: attached blocks whose support is gone drop (their neighbour update), so a
// replay that only copies the commands would keep ladders, vines and torches the live world no longer has ----

const OFFSETS = { east: [1, 0, 0], west: [-1, 0, 0], south: [0, 0, 1], north: [0, 0, -1], up: [0, 1, 0], down: [0, -1, 0] }
const OPPOSITE = { east: 'west', west: 'east', south: 'north', north: 'south', up: 'down', down: 'up' }
const FACES = ['north', 'east', 'south', 'west', 'up']
const AIR = stateId('air')

const infoCache = new Map()
const infoOf = id => {
  let info = infoCache.get(id)
  if (info === undefined) {
    const block = Block.fromStateId(id, 0)
    infoCache.set(id, info = { name: block.name, props: block.getProperties() })
  }
  return info
}

// the box [x0, y0, z0, x1, y1, z1] the commands touched, one cell wider on every side
const touched = entries => entries.reduce(
  (b, [x0, y0, z0, x1, y1, z1]) => [Math.min(b[0], x0 - 1), Math.min(b[1], y0 - 1), Math.min(b[2], z0 - 1), Math.max(b[3], x1 + 1), Math.max(b[4], y1 + 1), Math.max(b[5], z1 + 1)],
  [Infinity, Infinity, Infinity, -Infinity, -Infinity, -Infinity]
)

// a full cube a face can be attached to (the planner's collision table: a whole block, no gaps)
const sturdyAt = (snapshot, table, x, y, z) => {
  const id = snapshot.stateAt(x, y, z)
  return id !== UNLOADED && table.top[id] >= 16 && table.base[id] === 0 && !table.partial[id]
}

// what keeps the block at (x, y, z) in place: a vine keeps the faces that still have a block (or a vine above with the face),
// the rest need their one support. Returns the state id to leave there, or null when the block is not an attached one.
const settled = (snapshot, table, x, y, z, id) => {
  const { name, props } = infoOf(id)
  const at = (dir) => { const [dx, dy, dz] = OFFSETS[dir]; return [x + dx, y + dy, z + dz] }
  const sturdy = dir => sturdyAt(snapshot, table, ...at(dir))
  const keep = ok => ok ? id : AIR
  if (name === 'ladder' || /wall_torch$/.test(name)) return keep(sturdy(OPPOSITE[props.facing]))
  if (/(^|_)torch$/.test(name)) return keep(sturdy('down'))
  if (name === 'lever' || /_button$/.test(name)) return keep(sturdy(props.face === 'floor' ? 'down' : props.face === 'ceiling' ? 'up' : OPPOSITE[props.facing]))
  if (name === 'cocoa') {
    const logId = snapshot.stateAt(...at(props.facing))
    return keep(logId !== UNLOADED && infoOf(logId).name === 'jungle_log')
  }
  if (name !== 'vine') return null
  const aboveId = snapshot.stateAt(...at('up'))
  const above = aboveId === UNLOADED ? { name: '', props: {} } : infoOf(aboveId)
  const faces = Object.fromEntries(FACES.map(f => [f, props[f] === true && (sturdy(f) || above.name === 'vine' && above.props[f] === true)]))
  return FACES.some(f => faces[f]) ? stateId('vine', faces) : AIR
}

// repeat until nothing more drops: a vine may have been hanging from one that just went
export function dropUnsupported (snapshot, [x0, y0, z0, x1, y1, z1]) {
  const table = defaultStateTable()
  for (let changed = true; changed;) {
    changed = false
    for (let y = y1; y >= y0; y--) {
      for (let z = z0; z <= z1; z++) {
        for (let x = x0; x <= x1; x++) {
          const id = snapshot.stateAt(x, y, z)
          if (id === UNLOADED || id === AIR) continue
          const now = settled(snapshot, table, x, y, z, id)
          if (now === null || now === id) continue
          snapshot.setState(x, y, z, now)
          changed = true
        }
      }
    }
  }
}

// ---- water, as the server settles it after the commands (vanilla rules, simplified): a source falls into air below it as falling
// water (level 8), a flowing cell or a source resting on something spreads to its four sides one level weaker (a falling cell
// starts at level 1) as far as level 7, and a source over soul sand or magma becomes a bubble column up through the sources
// above. The horizontal spread has no pull toward nearby holes (a source over a hole spreads to all four sides); air is the only
// block water replaces; the box bounds it. ----

const FALLING = 8
const MAX_FLOW = 7
const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]
const waterName = id => id !== UNLOADED && infoOf(id).name === 'water'
const waterLevel = id => Number(infoOf(id).props.level)

export function flowWater (snapshot, [x0, y0, z0, x1, y1, z1]) {
  const key = (x, y, z) => `${x},${y},${z}`
  const inBox = (x, y, z) => x >= x0 && x <= x1 && y >= y0 && y <= y1 && z >= z0 && z <= z1
  const levels = new Map()
  const queue = []
  for (let y = y0; y <= y1; y++) {
    for (let z = z0; z <= z1; z++) {
      for (let x = x0; x <= x1; x++) {
        const id = snapshot.stateAt(x, y, z)
        if (!waterName(id)) continue
        levels.set(key(x, y, z), waterLevel(id))
        queue.push([x, y, z])
      }
    }
  }
  const put = (x, y, z, level) => {
    levels.set(key(x, y, z), level)
    snapshot.setState(x, y, z, stateId('water', { level }))
    queue.push([x, y, z])
  }
  // a cell water can enter at `level`: air, or flowing water that is weaker (a higher number); never a source or falling water
  const takes = (x, y, z, level) => {
    if (!inBox(x, y, z)) return false
    if (snapshot.stateAt(x, y, z) === AIR) return true
    const have = levels.get(key(x, y, z))
    return have !== undefined && have !== 0 && have !== FALLING && (level === FALLING || have > level)
  }
  for (let head = 0; head < queue.length; head++) {
    const [x, y, z] = queue[head]
    const level = levels.get(key(x, y, z))
    // over air or water the cell falls into it; a flowing cell over such a hole only falls, a source spreads as well
    const below = snapshot.stateAt(x, y - 1, z)
    const hole = below === AIR || waterName(below)
    if (takes(x, y - 1, z, FALLING)) put(x, y - 1, z, FALLING)
    if (level !== 0 && hole) continue
    const next = (level === FALLING ? 0 : level) + 1
    if (next > MAX_FLOW) continue
    for (const [dx, dz] of SIDES) if (takes(x + dx, y, z + dz, next)) put(x + dx, y, z + dz, next)
  }
}

// bubble columns: soul sand lifts (drag=false), magma drags (drag=true), through the water sources above it
export function bubbleColumns (snapshot, [x0, y0, z0, x1, y1, z1]) {
  for (let z = z0; z <= z1; z++) {
    for (let x = x0; x <= x1; x++) {
      for (let y = y0; y <= y1; y++) {
        const id = snapshot.stateAt(x, y, z)
        if (id === UNLOADED) continue
        const { name } = infoOf(id)
        if (name !== 'soul_sand' && name !== 'magma_block') continue
        const drag = name === 'magma_block'
        for (let k = y + 1; k <= y1; k++) {
          const above = snapshot.stateAt(x, k, z)
          if (!waterName(above) || waterLevel(above) !== 0) break
          snapshot.setState(x, k, z, stateId('bubble_column', { drag }))
        }
      }
    }
  }
}

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
export const laneSnapshot = (family, cmds, options = {}) => {
  const added = cmds.flatMap(entryFor)
  const snapshot = fixtureSnapshot({ fill: [...LANES[family](options), ...added] })
  if (added.length === 0) return snapshot
  const box = touched(added)
  dropUnsupported(snapshot, box)
  flowWater(snapshot, [box[0] - MAX_FLOW, box[1], box[2] - MAX_FLOW, box[3] + MAX_FLOW, box[4], box[5] + MAX_FLOW])
  bubbleColumns(snapshot, box)
  return snapshot
}

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
