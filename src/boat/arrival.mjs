import { villagerDockPlan } from '../lib.mjs'
import { cellKey, buildingMaterial, safeFullBlock } from '../enclosure/blocks.mjs'

export function arrivalPlan (house, a) {
  if (!house.entry) throw new Error('dock arrival needs entryX= and entryZ= on the house east wall')
  if (![a.dockX, a.dockY, a.dockZ, a.riverX, a.riverZ].every(Number.isInteger)) throw new Error('dockX/Y/Z and riverX/Z must be integer coordinates')
  if (a.riverX !== 1 || a.riverZ !== 0) throw new Error('this adjacent-house arrival requires a dock opening east toward the river')
  const dock = villagerDockPlan({ x: a.dockX, y: a.dockY, z: a.dockZ }, { x: 1, z: 0 })
  const rear = { x: a.dockX - 2, y: a.dockY + 1, z: house.entry.z }
  if (rear.x !== house.entry.x + 1 || Math.abs(rear.z - a.dockZ) > 1 || house.y < rear.y || house.y > rear.y + 1) throw new Error('house entry must adjoin the dry west dock rear with at most one upward foot step')
  const opening = [0, 1, 2].map(n => ({ ...rear, y: rear.y + n }))
  const roof = { ...rear, y: rear.y + 3 }
  const sides = [-1, 1].flatMap(dz => [0, 1, 2, 3].map(n => ({ ...rear, z: rear.z + dz, y: rear.y + n })))
  return { dock, rear, opening, roof, sides, landing: { x: a.dockX + 1, y: a.dockY - 0.5, z: house.entry.z }, dockArgs: { x: a.dockX, y: a.dockY, z: a.dockZ, riverX: 1, riverZ: 0 } }
}
export function arrivalPreflight (arrival, blockAt) {
  const needed = []
  for (const p of [arrival.roof, ...arrival.sides]) {
    const b = blockAt(p.x, p.y, p.z)
    if (b?.solid && buildingMaterial(b.name)) continue
    if (!['air', 'cave_air', 'void_air'].includes(b?.name)) throw new Error(`arrival boundary ${cellKey(p)} contains ${b?.name ?? 'unloaded'}`)
    needed.push(p)
  }
  for (const p of arrival.opening) {
    const b = blockAt(p.x, p.y, p.z)
    if (!(b?.solid && buildingMaterial(b.name)) && !['air', 'cave_air', 'void_air'].includes(b?.name)) throw new Error(`rear passage ${cellKey(p)} contains ${b?.name ?? 'unloaded'}`)
  }
  const floor = { ...arrival.rear, y: arrival.rear.y - 1 }
  if (!safeFullBlock(blockAt(floor.x, floor.y, floor.z))) throw new Error(`rear passage lacks dry support at ${cellKey(floor)}`)
  return needed
}
