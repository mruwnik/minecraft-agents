// Supply optional growth from the configured compost system only. Follow actual
// hopper connections, never a radius search or an arbitrary adjacent container.
import { dropGoal } from '../drop.mjs'
import { cellOf } from './field.mjs'
import { loadedAround, noStanding } from '../navigation/walk.mjs'
import { farmApi, recoverFarm } from './attention.mjs'

const CONTAINERS = new Set(['chest', 'trapped_chest', 'barrel', 'hopper'])
const container = name => CONTAINERS.has(name) || /shulker_box$/.test(name ?? '')
const DIRECTIONS = { down: [0, -1, 0], north: [0, 0, -1], south: [0, 0, 1], west: [-1, 0, 0], east: [1, 0, 0] }
const key = p => `${p.x},${p.y},${p.z}`
const offset = (p, [dx, dy, dz]) => ({ x: p.x + dx, y: p.y + dy, z: p.z + dz })
const MAX_LINKS = 12

export async function provisionBoneMeal (api, target, wanted) {
  const before = api.inv().bone_meal ?? 0
  const result = { got: 0, problems: [] }
  if (!target || wanted <= before) return result
  api = farmApi(api)
  const short = () => Math.max(0, wanted - (api.inv().bone_meal ?? 0))
  const recover = recoverFarm(e => { result.problems.push(e.message); return null })
  const takeDrops = async at => {
    const here = api.pos()
    const range = Math.min(64, Math.ceil(Math.hypot(here.x - at.x, here.y - at.y, here.z - at.z)) + 3)
    const drops = (api.drops?.(range) ?? []).filter(d => d.item === 'bone_meal' && !d.deep && !d.outsidePen && Math.hypot(d.x - at.x, d.y - at.y, d.z - at.z) <= 3).slice(0, 8)
    const cellAt = (x, y, z) => cellOf(api.block(x, y, z))
    for (const drop of drops) {
      if (!short()) break
      const goal = dropGoal(drop, cellAt)
      if (loadedAround(cellAt, goal, goal.range) && noStanding(cellAt, goal, goal.range)) continue
      const reached = await api.act('goto', goal).then(() => true, recover)
      if (reached) await api.pause(0.5)
    }
  }
  const visited = new Set()
  let at = { x: target.x, y: target.y, z: target.z }
  for (let n = 0; at && n < MAX_LINKS && short(); n++) {
    if (visited.has(key(at))) break
    visited.add(key(at))
    let block = api.block(at.x, at.y, at.z)
    if (!block && n === 0) {
      await api.act('goto', { ...at, range: 3 }).catch(recover)
      block = api.block(at.x, at.y, at.z)
    }
    if (!block) { result.problems.push(`configured compost connection at ${key(at)} is not loaded`); break }
    if (container(block.name)) {
      const contents = await api.act('chest_contents', at).catch(recover)
      const count = Math.min(short(), contents?.items?.bone_meal ?? 0)
      if (count > 0) await api.act('withdraw', { ...at, items: { bone_meal: count } }).catch(recover)
    } else if (block.name === 'composter') {
      await takeDrops(at)
      if (short() && Number(api.block(at.x, at.y, at.z)?.properties?.level) === 8) {
        // Empty-handed even if an output hopper drains the ready level while
        // walking up: never feed a held seed before harvest.
        const used = await api.act('use', { ...at, empty_hand: true }).then(() => true, recover)
        if (used) await takeDrops(at)
      }
    } else {
      result.problems.push(`configured compost connection at ${key(at)} is ${block.name}, not a composter or connected container`)
      break
    }
    if (block.name === 'hopper') {
      const direction = DIRECTIONS[block.properties?.facing]
      at = direction ? offset(at, direction) : null
    } else if (block.name === 'composter' || n === 0) {
      // A container supplies only the hopper directly below it. A reached
      // output chest is terminal, so this cannot wander through a warehouse.
      const below = offset(at, DIRECTIONS.down)
      at = api.block(below.x, below.y, below.z)?.name === 'hopper' ? below : null
    } else at = null
  }
  if (at && short() && visited.size === MAX_LINKS) result.problems.push(`compost connection exceeded ${MAX_LINKS} blocks: configure a nearer output container`)
  result.got = Math.max(0, (api.inv().bone_meal ?? 0) - before)
  return result
}
