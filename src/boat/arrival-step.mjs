import { breedFullBlock, breedKey } from '../villager/breed.mjs'
import { boatPassengerProfile } from './passenger.mjs'

const wet = name => ['water', 'bubble_column', 'seagrass', 'tall_seagrass', 'kelp', 'kelp_plant'].includes(name)
const air = name => ['air', 'cave_air', 'void_air'].includes(name)
const point = row => {
  const [x, y, z] = String(row.exact ?? row.at).split(',').map(Number)
  if (![x, y, z].every(Number.isFinite)) throw new Error('cannot verify temporary-step occupancy from an unobserved entity position')
  return { x, y, z }
}
const overlaps = (p, q, radius = 0.3, height = 1.95) => q.x + radius > p.x && q.x - radius < p.x + 1 && q.z + radius > p.z && q.z - radius < p.z + 1 && q.y + height > p.y && q.y < p.y + 1

export function arrivalStepInfo (api, arrival, palette) {
  const cell = { ...arrival.dock.cell, y: arrival.dock.cell.y - 1 }
  const name = `${api.me().toLowerCase()}-villager-step-${cell.x}-${cell.y}-${cell.z}`
  const marker = api.places().find(p => p.name === name)
  let record = null
  if (marker) {
    if (String(marker.by ?? '').toLowerCase() !== api.me().toLowerCase()) throw new Error('temporary-step marker belongs to another player')
    try {
      const [item, uuid, phase] = JSON.parse(marker.note)
      record = { cell: breedKey(marker), item, uuid, phase: phase === 'p' ? 'placed' : phase === 'i' ? 'intent' : null }
    } catch { throw new Error('temporary-step marker is not a recognized placement record') }
    if (record.cell !== breedKey(cell) || !palette.includes(record.item) || !['intent', 'placed'].includes(record.phase) || !record.uuid) throw new Error('temporary-step marker does not match this arrival site and palette')
  }
  const b = api.block(cell.x, cell.y, cell.z)
  if (!b) throw new Error(`temporary-step cell is unloaded at ${breedKey(cell)}`)
  if (record?.phase === 'placed' && b.name === record.item) return { cell, name, record, owned: true, needed: false, item: record.item }
  if (record && !wet(b.name) && !air(b.name)) throw new Error('recorded temporary step changed or its ownership was not confirmed; do not dig it automatically')
  if (!record && breedFullBlock(b)) return { cell, name, record: null, owned: false, needed: false }
  if (!wet(b.name) && !air(b.name)) throw new Error(`temporary step contains ${b.name}, not clear water or air`)
  if (!breedFullBlock(api.block(cell.x, cell.y - 1, cell.z)) || !air(api.block(cell.x, cell.y + 1, cell.z)?.name) || !air(api.block(cell.x, cell.y + 2, cell.z)?.name)) throw new Error('temporary arrival step needs full support and two clear cells above')
  const dry = { x: arrival.rear.x + 1, y: arrival.rear.y, z: arrival.rear.z }
  if (!breedFullBlock(api.block(dry.x, dry.y - 1, dry.z)) || !air(api.block(dry.x, dry.y, dry.z)?.name) || !air(api.block(dry.x, dry.y + 1, dry.z)?.name)) throw new Error('temporary step does not join a supported dry rear landing')
  const item = record?.item ?? palette.find(item => (api.inv()[item] ?? 0) > 0)
  return { cell, name, record, owned: false, needed: true, item }
}

async function clearOccupancy (api, info) {
  const rows = (await api.act('entity', { name: '*', uuid: true, count: 1000 })).found ?? []
  if (rows.length >= 1000) throw new Error('temporary-step occupancy census reached its limit')
  if (rows.some(e => overlaps(info.cell, point(e), (e.width ?? 1.375) / 2, e.height ?? 2.75)) || overlaps(info.cell, api.pos(), 0.3, 1.8)) throw new Error('temporary-step cell is occupied by an entity or this bot; wait for a clear placement stance')
  return rows
}

export async function placeArrivalStep (api, info, uuid) {
  if (!info.needed) return info
  if (!info.item || (api.inv()[info.item] ?? 0) < 1) throw new Error('reserve one carried full building block for the temporary arrival step')
  const rows = await clearOccupancy(api, info)
  if (!rows.some(e => e.uuid === uuid && e.vehicleId === null && boatPassengerProfile(e).ok)) throw new Error('temporary step requires the exact supported adult observed on foot after release')
  const boats = (await api.act('boat_state', {})).boats ?? []
  if (boats.some(b => overlaps(info.cell, point(b), 0.6875, 0.5625))) throw new Error('a boat still occupies the temporary-step cell')
  const record = { cell: breedKey(info.cell), item: info.item, uuid, phase: 'intent' }
  await api.act('mark', { name: info.name, kind: 'work', ...info.cell, note: JSON.stringify([record.item, record.uuid, record.phase === 'placed' ? 'p' : 'i']) })
  const current = api.block(info.cell.x, info.cell.y, info.cell.z)
  if (!wet(current?.name) && !air(current?.name)) throw new Error('temporary-step cell changed before placement')
  await clearOccupancy(api, info)
  await api.act('place', { ...info.cell, item: info.item })
  if (api.block(info.cell.x, info.cell.y, info.cell.z)?.name !== info.item) throw new Error('temporary step placement was not observed; its intent marker remains for inspection')
  record.phase = 'placed'
  await api.act('mark', { name: info.name, note: JSON.stringify([record.item, record.uuid, record.phase === 'placed' ? 'p' : 'i']) })
  api.report({ temporaryStep: breedKey(info.cell), stepItem: info.item, stepCleanup: 'after resident is secure, before the next boat' })
  return { ...info, record, owned: true, needed: false }
}

export async function clearArrivalStep (api, info, mainUuids) {
  if (!info.record) return false
  if (!mainUuids.includes(info.record.uuid)) throw new Error('keep the recorded temporary step until its exact villager is secure in the main room')
  const b = api.block(info.cell.x, info.cell.y, info.cell.z)
  if (b?.name === info.record.item && info.record.phase === 'placed') {
    await clearOccupancy(api, info)
    await api.act('dig', info.cell)
    if (!wet(api.block(info.cell.x, info.cell.y, info.cell.z)?.name) && !air(api.block(info.cell.x, info.cell.y, info.cell.z)?.name)) throw new Error('temporary-step removal was not observed; retain its ownership marker')
  } else if (!wet(b?.name) && !air(b?.name)) throw new Error('temporary-step block changed; refuse to remove an unrelated block')
  await api.act('unmark', { name: info.name })
  api.report({ stepCleanup: 'removed', temporaryStep: breedKey(info.cell) })
  return true
}
