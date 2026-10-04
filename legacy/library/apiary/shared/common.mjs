import { BEE_FLOWERS, placeTarget } from '../../../src/lib.mjs'
import { hiveState, fireState, CAMPFIRES } from './hive.mjs'
export { carpetCarried } from './hive.mjs'

// how far up and down from the place the scan reads: a hive stands a few blocks over its fire, and an apiary on a
// slope puts its columns a few blocks apart in height
export const SCAN_HEIGHT = 6
const HIVES = new Set(['beehive', 'bee_nest'])
const FIRES = new Set(CAMPFIRES)
const FLOWERS = new Set(BEE_FLOWERS)

const dist = (p, q) => Math.hypot(p.x - q.x, p.y - q.y, p.z - q.z)
const parseAt = at => { const [x, y, z] = String(at).split(',').map(Number); return { x, y, z } }

// Every hive, fire and flower within range= of the place, read cell by cell. Not find_blocks: that searches from
// wherever the body stands, and walks chunk sections in an octahedron that skips the diagonal section, so a body two
// blocks off the mark across a chunk line counted one fire in four (mariel-apiary, 2026-09-26 21:00Z) while the
// hives, read from their own cells, were all smoked. The place is the anchor and the radius is the one word.
export function apiaryCells (at, range, blockAt) {
  const hives = []
  const fires = []
  let flowers = 0
  for (let x = at.x - range; x <= at.x + range; x++) {
    for (let z = at.z - range; z <= at.z + range; z++) {
      for (let y = at.y - SCAN_HEIGHT; y <= at.y + SCAN_HEIGHT; y++) {
        const name = blockAt(x, y, z)?.name
        if (!name) continue
        if (HIVES.has(name)) hives.push({ x, y, z })
        else if (FIRES.has(name)) fires.push({ x, y, z })
        else if (FLOWERS.has(name)) flowers++
      }
    }
  }
  return { hives, fires, flowers }
}

export async function apiarySnapshot (api, a, action) {
  const aim = placeTarget(api.places(), a, action)
  if (aim.error) throw new Error(aim.error)
  const at = aim.at
  const range = a.range ?? 16
  await api.act('goto', { ...at, range: 2 })
  await api.checkpoint()

  const cells = apiaryCells(at, range, api.block)
  const hives = cells.hives.map(p => hiveState({ ...p, block: api.block(p.x, p.y, p.z), blockAt: api.block })).filter(Boolean)
  // every campfire in range, lit or not, with whether a carpet guards it: a bare fire burns bees whether or not it smokes a hive
  const fires = cells.fires.map(p => fireState({ ...p, block: api.block(p.x, p.y, p.z), blockAt: api.block }))
  // bees are counted from the body, so ask far enough to cover the whole apiary from here and keep the ones near the place
  const me = api.pos()
  const seen = await api.act('animals', { mob: 'bee', within: Math.ceil(range + dist(me, at)) })
  const bees = (seen.found ?? []).filter(b => !b.at || dist(parseAt(b.at), at) <= range)
  return { at, range, hives, bees, fires, flowers: cells.flowers }
}

export const hiveLine = hive => `${hive.name}@${hive.x},${hive.y},${hive.z}:honey=${hive.honey}${hive.smoked ? '' : ',NO-SMOKE'}${hive.open ? ',OPEN-FIRE' : ''}${hive.raised ? ',RAISED-FIRE' : ''}${hive.entranceClear ? '' : ',BLOCKED'}`

export const apiaryFires = async (api, a, action) => (await apiarySnapshot(api, a, action)).fires
