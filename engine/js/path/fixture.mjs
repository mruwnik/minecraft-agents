// Tiny hand-built worlds for planner tests: a few named blocks in an otherwise empty (air) column set.
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { createSnapshot } from './snapshot.mjs'

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

export function fixtureSnapshot ({ blocks = [], fill = [] } = {}) {
  const snapshot = createSnapshot({})
  const sectionCount = snapshot.height >> 4
  for (const key of touchedColumns(blocks, fill)) {
    const [cx, cz] = key.split(',').map(Number)
    for (let sy = 0; sy < sectionCount; sy++) snapshot.setSection(cx, sy, cz, new Uint16Array(4096))
  }
  fill.forEach(box => fillBox(snapshot, box))
  blocks.forEach(([x, y, z, name, props]) => snapshot.setState(x, y, z, stateId(name, props)))
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
