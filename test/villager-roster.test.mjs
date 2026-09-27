import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { makeVillagerRosterObserver, readVillagerRoster, mergeVillagerObservation } from '../src/villager/roster.mjs'

const uuid = '95299feb-5cfb-428a-9ac1-72de2d36dc12'
const villager = (x, metadata = { 16: false, 19: { profession: 'farmer', level: 2 } }) => ({
  id: 4, uuid, name: 'villager', isValid: true, position: { x, y: 65, z: -40 }, metadata
})
const tempRoster = () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'villager-roster-'))
  return { file: path.join(dir, 'state', 'villagers.json'), cleanup: () => fs.rmSync(dir, { recursive: true, force: true }) }
}

test('passive observer batches villager spawn/move/update, deduplicates rotations, and ignores entityGone as death', () => {
  const tmp = tempRoster(), bot = new EventEmitter()
  bot.entities = { 4: villager(1.1) }
  let tick = 1000
  const observer = makeVillagerRosterObserver({ file: tmp.file, by: 'Probe', now: () => new Date(tick), refreshMs: 30000 })
  try {
    observer.attach(bot)
    assert.equal(observer.pendingCount(), 1) // existing visible entities are sampled on attach
    bot.emit('entityMoved', villager(1.8)) // same block, same visible metadata
    bot.emit('entityUpdate', villager(1.8))
    assert.equal(observer.pendingCount(), 1)
    assert.equal(observer.flush(), 1)
    const seen = readVillagerRoster(tmp.file).villagers[uuid]
    assert.equal(seen.age, 'adult')
    assert.equal(seen.profession, 'farmer')
    assert.deepEqual(seen.lastPosition, { x: 1.1, y: 65, z: -40 })
    assert.equal('dead' in seen, false)

    tick += 1000
    bot.emit('entityGone', villager(1.8))
    assert.equal(observer.pendingCount(), 0)
    assert.equal(readVillagerRoster(tmp.file).villagers[uuid].lastSeenAt, new Date(1000).toISOString())

    tick += 1000
    bot.emit('entityMoved', villager(2.1)) // a new block cell updates last seen
    assert.equal(observer.pendingCount(), 1)
    observer.flush()
    assert.equal(readVillagerRoster(tmp.file).villagers[uuid].lastPosition.x, 2.1)

    tick += 31000
    bot.entities[4].position.x = 4.1 // timer sweep refreshes loaded stationary entities without world scans
    observer.sweepLoaded()
    assert.equal(observer.pendingCount(), 1)
    observer.flush()
    assert.equal(readVillagerRoster(tmp.file).villagers[uuid].lastPosition.x, 4.1)
  } finally { observer.detach(); tmp.cleanup() }
})

test('only exact villager UUIDs are persisted; unknown age and older events cannot erase newer facts', () => {
  const tmp = tempRoster(), bot = new EventEmitter()
  bot.entities = {
    4: villager(1),
    5: { ...villager(5), uuid: undefined },
    6: { ...villager(6), name: 'cow', uuid: '11111111-1111-4111-8111-111111111111' }
  }
  const observer = makeVillagerRosterObserver({ file: tmp.file, by: 'Probe' })
  try {
    observer.attach(bot)
    observer.flush()
    let roster = readVillagerRoster(tmp.file)
    assert.deepEqual(Object.keys(roster.villagers), [uuid])
    roster = mergeVillagerObservation(roster, { uuid, by: 'OldProbe', at: '2026-01-01T00:00:00.000Z', position: { x: -1, y: 64, z: 0 }, profession: 'unknown', baby: undefined })
    assert.deepEqual(roster.villagers[uuid].lastPosition, { x: 1, y: 65, z: -40 })
    assert.equal(roster.villagers[uuid].profession, 'farmer')
    assert.equal(roster.villagers[uuid].age, 'adult')
    assert.equal('dead' in roster.villagers[uuid], false)
  } finally { observer.detach(); tmp.cleanup() }
})

test('connection end flushes pending sightings and removes listeners before a replacement body attaches', () => {
  const tmp = tempRoster(), first = new EventEmitter(), second = new EventEmitter()
  first.entities = {}; second.entities = {}
  let tick = 1000
  const observer = makeVillagerRosterObserver({ file: tmp.file, by: 'Probe', now: () => new Date(tick++) })
  try {
    observer.attach(first)
    first.emit('entitySpawn', villager(3))
    assert.equal(observer.pendingCount(), 1)
    first.emit('end')
    assert.equal(observer.pendingCount(), 0)
    assert.equal(first.listenerCount('entityMoved'), 0)
    assert.equal(readVillagerRoster(tmp.file).villagers[uuid].lastPosition.x, 3)

    observer.attach(second)
    const next = { ...villager(9), uuid: '11111111-1111-4111-8111-111111111111' }
    second.emit('entitySpawn', next)
    observer.flush()
    assert.deepEqual(Object.keys(readVillagerRoster(tmp.file).villagers).sort(), [uuid, next.uuid].sort())
    assert.equal(first.listenerCount('entityMoved'), 0)
  } finally { observer.detach(); tmp.cleanup() }
})

test('fresh trade observations supersede offers, explicit places are labeled as user intent, and purchases require confirmation input', () => {
  const roster = { version: 1, villagers: {} }
  const first = mergeVillagerObservation(roster, {
    uuid, by: 'Probe', at: '2026-09-27T10:00:00.000Z', profession: 'librarian', level: 2, baby: false,
    offers: [{ outputItem: { name: 'enchanted_book', enchants: [{ name: 'efficiency', lvl: 3 }] }, inputItem1: { name: 'emerald', count: 12 }, nbTradeUses: 0, maximumNbTradeUses: 12 }],
    place: { name: 'west-village' }
  })
  const second = mergeVillagerObservation(first, {
    uuid, by: 'Probe', at: '2026-09-27T10:01:00.000Z', profession: 'librarian', level: 2, baby: false,
    purchase: { offer: 1, bought: 'enchanted_book', boughtCount: 1, paid: { emerald: 12 }, usedBefore: 0 }
  })
  const record = second.villagers[uuid]
  assert.equal(record.offers.items[0].outputItem.enchants[0].name, 'efficiency')
  assert.deepEqual(record.place, { name: 'west-village', recordedAt: '2026-09-27T10:00:00.000Z', recordedBy: 'Probe', evidence: 'explicit place= argument' })
  assert.equal(record.purchases[0].boughtCount, 1)
  assert.match(record.lockEvidence.basis, /inventory-confirmed/)
  assert.equal(record.workstationObservation, undefined)
})
test('an observed remove-and-replace invalidates a claim even when the final workstation looks unchanged',async()=>{
 const {WORKSTATION_CLAIM_BASIS}=await import('../src/villager/population.mjs'),{saveVillagerObservation}=await import('../src/villager/roster.mjs')
 const tmp=tempRoster(),bot=new EventEmitter();bot.entities={}
 const at=new Date(1000).toISOString(),cell={x:1,y:65,z:2}
 saveVillagerObservation(tmp.file,{uuid,at,by:'Probe',workstationClaim:{...cell,block:'lectern',profession:'librarian',observedAt:at,basis:WORKSTATION_CLAIM_BASIS}})
 const observer=makeVillagerRosterObserver({file:tmp.file,by:'Probe',now:()=>new Date(2000)})
 try{observer.attach(bot);bot.emit('blockUpdate',{name:'lectern',position:cell},{name:'air',position:cell});bot.emit('blockUpdate',{name:'air',position:cell},{name:'lectern',position:cell});observer.flush();const r=readVillagerRoster(tmp.file).villagers[uuid];assert.equal(r.workstationClaim.invalidatedAt,new Date(2000).toISOString());assert.equal(r.lastSeenAt,at);observer.detach();assert.equal(bot.listenerCount('blockUpdate'),0)}finally{observer.detach();tmp.cleanup()}
})
