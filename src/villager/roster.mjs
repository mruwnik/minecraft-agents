// Shared, UUID-keyed observations of villagers. Missing villagers are never
// removed: an entity query only describes what one body could currently see.
import fs from 'node:fs'
import path from 'node:path'
import { withMapLock, atomicMapWrite } from '../map-store.mjs'
import { entityUuid, villagerObservation } from './observation.mjs'

export const VILLAGER_ROSTER_VERSION = 1

export const emptyVillagerRoster = () => ({ version: VILLAGER_ROSTER_VERSION, villagers: {} })

const validTime = value => typeof value === 'string' && Number.isFinite(Date.parse(value))
const plain = value => value === undefined ? undefined : JSON.parse(JSON.stringify(value))
const ageName = baby => baby === true ? 'baby' : baby === false ? 'adult' : 'unknown'
const newerOrEqual = (incoming, prior) => !validTime(prior) || Date.parse(incoming) >= Date.parse(prior)

function checkedRoster (value) {
  if (!value || value.version !== VILLAGER_ROSTER_VERSION || !value.villagers || Array.isArray(value.villagers) || typeof value.villagers !== 'object') {
    throw new Error(`villager roster needs version=${VILLAGER_ROSTER_VERSION} and a villagers object`)
  }
  return value
}

export function readVillagerRoster (file) {
  if (!fs.existsSync(file)) return emptyVillagerRoster()
  return checkedRoster(JSON.parse(fs.readFileSync(file, 'utf8')))
}

// Merge only observations that have exact identity and a comparable timestamp.
// A later sparse/unknown sample may say age=unknown, but cannot erase purchase
// evidence, explicit place associations, or earlier offer snapshots.
export function mergeVillagerObservation (roster, input) {
  checkedRoster(roster)
  if (!entityUuid(input?.uuid)) throw new Error('villager roster update needs an exact UUID')
  if (!validTime(input.at)) throw new Error('villager roster update needs an ISO timestamp')
  const prior = roster.villagers[input.uuid] ?? { uuid: input.uuid, firstSeenAt: input.at, purchases: [] }
  const next = { ...prior, uuid: input.uuid, firstSeenAt: prior.firstSeenAt ?? input.at }

  if (newerOrEqual(input.at, prior.lastSeenAt)) {
    next.lastSeenAt = input.at
    next.lastSeenBy = String(input.by ?? 'unknown')
    if (input.position && [input.position.x, input.position.y, input.position.z].every(Number.isFinite)) {
      next.lastPosition = { x: input.position.x, y: input.position.y, z: input.position.z }
    }
    if (typeof input.profession === 'string' && input.profession && input.profession !== 'unknown') next.profession = input.profession
    else next.profession ??= 'unknown'
    if (Number.isFinite(input.level)) next.level = input.level
    if (input.baby === true || input.baby === false) next.age = ageName(input.baby)
    else next.age ??= 'unknown'
  }

  if (Array.isArray(input.offers) && newerOrEqual(input.at, prior.offers?.observedAt)) {
    next.offers = {
      observedAt: input.at,
      observedBy: String(input.by ?? 'unknown'),
      profession: typeof input.profession === 'string' ? input.profession : 'unknown',
      level: Number.isFinite(input.level) ? input.level : null,
      items: plain(input.offers)
    }
  }

  if (input.place && typeof input.place.name === 'string' && input.place.name && newerOrEqual(input.at, prior.place?.recordedAt)) {
    next.place = { name: input.place.name, recordedAt: input.at, recordedBy: String(input.by ?? 'unknown'), evidence: 'explicit place= argument' }
  }

  // This is a verified block observation at the supplied trades/roll anchor,
  // not proof that the villager's POI claim points to it.
  if (input.workstation && Number.isInteger(input.workstation.x) && Number.isInteger(input.workstation.y) && Number.isInteger(input.workstation.z) && typeof input.workstation.block === 'string' && newerOrEqual(input.at, prior.workstationObservation?.observedAt)) {
    next.workstationObservation = {
      x: input.workstation.x,
      y: input.workstation.y,
      z: input.workstation.z,
      block: input.workstation.block,
      observedAt: input.at,
      observedBy: String(input.by ?? 'unknown'),
      evidence: 'matching workstation block observed at query coordinates; POI claim unverified'
    }
  }

  if (input.purchase && typeof input.purchase === 'object') {
    const purchase = { ...plain(input.purchase), at: input.at, by: String(input.by ?? 'unknown') }
    const id = `${purchase.at}|${purchase.offer ?? ''}|${purchase.bought ?? ''}`
    const purchases = (prior.purchases ?? []).filter(p => `${p.at}|${p.offer ?? ''}|${p.bought ?? ''}` !== id)
    next.purchases = [...purchases, purchase].sort((a, b) => Date.parse(a.at) - Date.parse(b.at)).slice(-50)
    if (newerOrEqual(input.at, prior.lockEvidence?.at)) {
      next.lockEvidence = {
        at: input.at,
        by: String(input.by ?? 'unknown'),
        profession: typeof input.profession === 'string' ? input.profession : 'unknown',
        level: Number.isFinite(input.level) ? input.level : null,
        basis: 'inventory-confirmed villager trade; profession is trade-locked'
      }
    }
  }

  return { ...roster, villagers: { ...roster.villagers, [input.uuid]: next } }
}

export function saveVillagerObservation (file, input) {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  return withMapLock(file, () => {
    const roster = mergeVillagerObservation(readVillagerRoster(file), input)
    atomicMapWrite(file, roster)
    return roster.villagers[input.uuid]
  })
}

export function saveVillagerObservations (file, inputs) {
  if (!inputs.length) return readVillagerRoster(file)
  fs.mkdirSync(path.dirname(file), { recursive: true })
  return withMapLock(file, () => {
    let roster = readVillagerRoster(file)
    for (const input of inputs) roster = mergeVillagerObservation(roster, input)
    atomicMapWrite(file, roster)
    return roster
  })
}

// Mineflayer emits entityMoved for rotations as well as position changes. Keep
// only the latest useful sample per UUID and write snapshots in small batches.
// entityGone is intentionally not observed: a despawn only says this client no
// longer has the entity loaded, not that the villager died.
export function makeVillagerRosterObserver ({ file, by, now = () => new Date(), flushMs = 10000, refreshMs = 30000 }) {
  const pending = new Map()
  const signatures = new Map()
  let bot = null
  let timer = null
  let busy = false

  const capture = entity => {
    if (entity?.name !== 'villager' || !entityUuid(entity.uuid) || !entity.position) return false
    const observation = villagerObservation(entity)
    const professionKnown = Boolean(observation.metadata?.[19] ?? observation.metadata?.[18])
    const profession = professionKnown ? observation.profession : 'unknown'
    const level = professionKnown ? observation.level : undefined
    const position = { x: observation.position.x, y: observation.position.y, z: observation.position.z }
    const cell = [Math.floor(position.x), Math.floor(position.y), Math.floor(position.z)]
    const signature = JSON.stringify([cell, profession, level, observation.baby])
    const at = now()
    const prior = signatures.get(entity.uuid)
    if (prior?.signature === signature && at.getTime() - prior.at < refreshMs) return false
    signatures.set(entity.uuid, { signature, at: at.getTime() })
    pending.set(entity.uuid, {
      uuid: entity.uuid,
      at: at.toISOString(),
      by,
      position,
      profession,
      level,
      baby: observation.baby
    })
    return true
  }

  const flush = () => {
    if (busy || pending.size === 0) return 0
    busy = true
    const batch = [...pending.values()]
    try {
      const saved = saveVillagerObservations(file, batch)
      for (const item of batch) if (pending.get(item.uuid)?.at === item.at) pending.delete(item.uuid)
      return Object.keys(saved.villagers).length
    } finally { busy = false }
  }

  const attach = client => {
    if (bot === client) return false
    detach()
    bot = client
    client.on('entitySpawn', sample)
    client.on('entityMoved', sample)
    client.on('entityUpdate', sample)
    for (const entity of Object.values(client.entities ?? {})) sample(entity)
    // The periodic pass is over Mineflayer's in-memory loaded entities only;
    // it refreshes a stationary villager without scanning chunks or the world.
    timer = setInterval(() => {
      sweepLoaded()
      try { flush() } catch {}
    }, flushMs)
    timer.unref?.()
    client.once('end', detach)
    return true
  }

  function detach () {
    if (timer) clearInterval(timer)
    timer = null
    if (!bot) return
    const client = bot
    bot = null
    client.off('entitySpawn', sample)
    client.off('entityMoved', sample)
    client.off('entityUpdate', sample)
    client.off('end', detach)
    try { flush() } catch {}
  }
  // The listeners need stable function identity for clean reconnect detach.
  const sample = entity => { try { capture(entity) } catch {} }
  const sweepLoaded = () => {
    for (const entity of Object.values(bot?.entities ?? {})) sample(entity)
  }

  return { attach, detach, capture, sweepLoaded, flush, pendingCount: () => pending.size }
}
