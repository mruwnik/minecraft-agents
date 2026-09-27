import { PLAN_LEGEND, planSpec, planCropMatches } from '../lib/plan.mjs'

export const FARM_FRUITS = ['melon', 'pumpkin']
const SIDES = [[1, 0, 'west'], [-1, 0, 'east'], [0, 1, 'north'], [0, -1, 'south']]

// Fruit occupies a neighboring cell, not the stem's planted bed. Attribute it
// to a compatible stem in this plan, keeping both the stem and neighbors' fruit.
export function farmFruitAt (position, cells, blockAt) {
  const fruit = blockAt(position.x, position.y, position.z)?.name
  if (!FARM_FRUITS.includes(fruit)) return null
  if (cells && !cells.some(c => c.x === position.x && c.z === position.z && c.y + 1 === position.y)) return null
  for (const [dx, dz, facing] of SIDES) {
    const x = position.x + dx
    const z = position.z + dz
    const stem = blockAt(x, position.y, z)
    if (!stem) continue
    const attached = stem.name === `attached_${fruit}_stem` && stem.properties?.facing === facing
    const mature = stem.name === `${fruit}_stem` && Number(stem.properties?.age) === 7
    if (!attached && !mature) continue
    if (cells && !cells.some(c => c.x === x && c.z === z && c.y + 1 === position.y && planCropMatches(planSpec(c), stem.name))) continue
    return fruit
  }
  return null
}
