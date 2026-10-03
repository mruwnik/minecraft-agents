// Tiny hand-built worlds for planner tests: a few named blocks in an otherwise empty (air) column set.
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { createSnapshot, UNLOADED } from './snapshot.mjs'
import { defaultStateTable } from './blocks.mjs'

const MC_VERSION = '26.1'
const registry = prismarineRegistry(MC_VERSION)
const Block = prismarineBlock(registry)

// unspecified properties keep the block's default state values (Block.fromProperties would zero them)
export function stateId (name, props = {}) {
  const block = registry.blocksByName[name]
  if (!block) throw new Error(`unknown block ${name}`)
  const defaults = Block.fromStateId(block.defaultState, 0).getProperties()
  return Block.fromProperties(name, { ...defaults, ...props }, 0).stateId
}

const fillBox = (snapshot, [x0, y0, z0, x1, y1, z1, name, props]) => {
  const id = stateId(name, props)
  for (let y = y0; y <= y1; y++) for (let z = z0; z <= z1; z++) for (let x = x0; x <= x1; x++) snapshot.setState(x, y, z, id)
}

const range = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => from + i)
const boxColumns = ([x0, , z0, x1, , z1]) => range(x0 >> 4, x1 >> 4).flatMap(cx => range(z0 >> 4, z1 >> 4).map(cz => [cx, cz]))
const touchedColumns = (blocks, fills) => new Set([
  ...blocks.map(([x, , z]) => [x >> 4, z >> 4]),
  ...fills.flatMap(boxColumns)
].map(([cx, cz]) => `${cx},${cz}`))

// ---- connections: the server joins fences, walls and panes to their neighbours when they are placed; a bare fill would
// leave posts with gaps a body slips through, so the fixture joins them too ----

const FENCE = 1
const NETHER_FENCE = 2 // does not join wooden fences
const WALL = 3
const PANE = 4 // glass panes and bars
const GATE = 5
const familyOfName = name => {
  if (name === 'nether_brick_fence') return NETHER_FENCE
  if (/(^|_)fence$/.test(name)) return FENCE
  if (/_wall$/.test(name)) return WALL
  if (/_pane$|(^|_)bars$/.test(name)) return PANE
  if (/_fence_gate$/.test(name)) return GATE
  return 0
}
const SIDES = [['east', 1, 0], ['west', -1, 0], ['south', 0, 1], ['north', 0, -1]]

let families
const familyTable = () => {
  if (families) return families
  families = new Uint16Array(registry.blocksArray.reduce((m, b) => Math.max(m, b.maxStateId), 0) + 1)
  registry.blocksArray.forEach(b => families.fill(familyOfName(b.name), b.minStateId, b.maxStateId + 1))
  return families
}

// does a post of `family` join the block `id` on its side (dx, dz)? Its own family, a gate across the line, a full block.
const joins = (family, id, dx, dz, table) => {
  if (id === UNLOADED) return false
  const other = familyTable()[id]
  if (other === family) return true
  if (other === GATE) return (family === FENCE || family === WALL) && (dx !== 0) === /^(north|south)$/.test(Block.fromStateId(id, 0).getProperties().facing)
  if (other !== 0) return false
  return table.top[id] >= 16 && table.base[id] === 0 && !table.partial[id]
}

const connect = (snapshot, [x0, y0, z0, x1, y1, z1]) => {
  const families = familyTable()
  const table = defaultStateTable()
  for (let y = y0 - 1; y <= y1 + 1; y++) {
    for (let z = z0 - 1; z <= z1 + 1; z++) {
      for (let x = x0 - 1; x <= x1 + 1; x++) {
        const id = snapshot.stateAt(x, y, z)
        const family = id === UNLOADED ? 0 : families[id]
        if (family === 0 || family === GATE) continue
        const block = Block.fromStateId(id, 0)
        const linked = SIDES.map(([, dx, dz]) => joins(family, snapshot.stateAt(x + dx, y, z + dz), dx, dz, table))
        const [e, w, sth, n] = linked
        const sides = family === WALL
          ? { ...Object.fromEntries(SIDES.map(([side], k) => [side, linked[k] ? 'low' : 'none'])), up: !(n && sth && !e && !w || e && w && !n && !sth) }
          : Object.fromEntries(SIDES.map(([side], k) => [side, linked[k]]))
        snapshot.setState(x, y, z, stateId(block.name, { ...block.getProperties(), ...sides }))
      }
    }
  }
}

const cellBox = ([x, y, z]) => [x, y, z, x, y, z]

export function fixtureSnapshot ({ blocks = [], fill = [] } = {}) {
  const snapshot = createSnapshot({})
  const sectionCount = snapshot.height >> 4
  for (const key of touchedColumns(blocks, fill)) {
    const [cx, cz] = key.split(',').map(Number)
    for (let sy = 0; sy < sectionCount; sy++) snapshot.setSection(cx, sy, cz, new Uint16Array(4096))
  }
  fill.forEach(box => fillBox(snapshot, box))
  blocks.forEach(([x, y, z, name, props]) => snapshot.setState(x, y, z, stateId(name, props)))
  const placed = [...fill.map(box => [box, box[6]]), ...blocks.map(b => [cellBox(b), b[3]])]
  if (placed.some(([, name]) => familyOfName(name) !== 0)) placed.forEach(([box]) => connect(snapshot, box))
  return snapshot
}

const nameAndProps = entry => Array.isArray(entry) ? entry : [entry]

// ascii layers (bottom first) of z rows of x chars -> blocks; '.' is air, ' ' is skipped
export function layers (rows, legend, origin) {
  return rows.flatMap((layer, dy) => layer.flatMap((row, dz) => [...row].flatMap((ch, dx) => {
    if (ch === ' ') return []
    const [name, props] = ch === '.' ? ['air'] : nameAndProps(legend[ch])
    const at = [origin.x + dx, origin.y + dy, origin.z + dz, name]
    return [props ? [...at, props] : at]
  })))
}
