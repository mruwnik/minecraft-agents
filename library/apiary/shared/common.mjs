import { BEE_FLOWERS, placeTarget } from '../../../src/lib.mjs'
import { hiveState, fireState, CAMPFIRES } from './hive.mjs'
export { carpetCarried } from './hive.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const vec = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })

export async function apiarySnapshot (api, a, action) {
  const aim = placeTarget(api.places(), a, action)
  if (aim.error) throw new Error(aim.error)
  const at = aim.at
  const range = a.range ?? 16
  await api.act('goto', { ...at, range: 2 })
  await api.checkpoint()

  const positions = []
  for (const block of ['beehive', 'bee_nest']) {
    const found = await api.act('find_blocks', { block, maxDistance: range, count: 32 })
    positions.push(...(found.positions ?? []).map(vec))
  }
  const unique = [...new Map(positions.map(p => [key(p), p])).values()]
  const hives = unique.map(p => hiveState({ ...p, block: api.block(p.x, p.y, p.z), blockAt: api.block })).filter(Boolean)
  const seen = await api.act('animals', { mob: 'bee', within: range })
  const bees = seen.found ?? []
  // every campfire in range, lit or not, with whether a carpet guards it: a bare fire burns bees whether or not it smokes a hive
  const firePositions = []
  for (const block of CAMPFIRES) {
    const found = await api.act('find_blocks', { block, maxDistance: range, count: 32 })
    firePositions.push(...(found.positions ?? []).map(vec))
  }
  const fires = [...new Map(firePositions.map(p => [key(p), p])).values()].map(p => fireState({ ...p, block: api.block(p.x, p.y, p.z), blockAt: api.block }))

  // Flowers are blocks, not entities. Reading loaded cells is much cheaper than one find_blocks action per flower.
  const flowers = new Set(BEE_FLOWERS)
  let flowerCount = 0
  for (let x = at.x - range; x <= at.x + range; x++) {
    for (let z = at.z - range; z <= at.z + range; z++) {
      for (let y = at.y - 3; y <= at.y + 3; y++) {
        if (flowers.has(api.block(x, y, z)?.name)) flowerCount++
      }
    }
  }
  return { at, range, hives, bees, fires, flowers: flowerCount }
}

export const hiveLine = hive => `${hive.name}@${hive.x},${hive.y},${hive.z}:honey=${hive.honey}${hive.smoked ? '' : ',NO-SMOKE'}${hive.open ? ',OPEN-FIRE' : ''}${hive.raised ? ',RAISED-FIRE' : ''}${hive.entranceClear ? '' : ',BLOCKED'}`

export const apiaryFires = async (api, a, action) => (await apiarySnapshot(api, a, action)).fires
