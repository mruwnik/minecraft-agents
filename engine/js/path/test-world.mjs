// Tiny air-and-stone worlds for the bench pins (the planner tests use engine.path.fixture in cljs; no fence/wall joins here). Why JavaScript: it feeds the golden bench pins (bench.golden.mjs).
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { createSnapshot } from './snapshot.mjs'

const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)

// unspecified properties keep the block's default state values (Block.fromProperties would zero them)
export function stateId (name, props = {}) {
  const block = registry.blocksByName[name]
  if (!block) throw new Error(`unknown block ${name}`)
  const defaults = Block.fromStateId(block.defaultState, 0).getProperties()
  return Block.fromProperties(name, { ...defaults, ...props }, 0).stateId
}

const range = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => from + i)
const columnsOf = ([x0, , z0, x1, , z1]) => range(x0 >> 4, x1 >> 4).flatMap(cx => range(z0 >> 4, z1 >> 4).map(cz => `${cx},${cz}`))

// blocks: [x, y, z, name, props?]; fill: [x0, y0, z0, x1, y1, z1, name, props?]; the touched columns are otherwise air
export function fixtureSnapshot ({ blocks = [], fill = [] } = {}) {
  const snapshot = createSnapshot({})
  const sectionCount = snapshot.height >> 4
  const columns = new Set([...blocks.flatMap(([x, y, z]) => columnsOf([x, y, z, x, y, z])), ...fill.flatMap(columnsOf)])
  for (const key of columns) {
    const [cx, cz] = key.split(',').map(Number)
    for (let sy = 0; sy < sectionCount; sy++) snapshot.setSection(cx, sy, cz, new Uint16Array(4096))
  }
  for (const [x0, y0, z0, x1, y1, z1, name, props] of fill) {
    const id = stateId(name, props)
    for (const y of range(y0, y1)) for (const z of range(z0, z1)) for (const x of range(x0, x1)) snapshot.setState(x, y, z, id)
  }
  blocks.forEach(([x, y, z, name, props]) => snapshot.setState(x, y, z, stateId(name, props)))
  return snapshot
}
