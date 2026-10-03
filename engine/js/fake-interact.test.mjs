import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const cow = { id: 1, name: 'cow', kind: 'passive', pos: at(1, 64, 0) }
const wheat = [{ name: 'wheat', count: 3 }]

const cases = [
  { label: 'gone', ent: [], args: { id: 9 }, status: 'gone' },
  { label: 'no-item', ent: [cow], args: { id: 1, item: 'wheat' }, status: 'no-item' },
  { label: 'out of reach', ent: [{ ...cow, pos: at(5, 64, 0) }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'out-of-reach', wheat: 3 },
  { label: 'feed adult', held: 'wheat', ent: [cow], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'used', consumed: 1, love: true, wheat: 2, field: ['inLove', true] },
  { label: 'feed in love', held: 'wheat', ent: [{ ...cow, inLove: true }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'no-effect', wheat: 3 },
  { label: 'cooldown', held: 'wheat', ent: [{ ...cow, cooldown: true }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'no-effect', wheat: 3 },
  { label: 'baby', held: 'wheat', ent: [{ ...cow, baby: true }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'used', consumed: 1, love: false, wheat: 2 },
  { label: 'wrong food', held: 'carrot', ent: [cow], inv: [{ name: 'carrot', count: 1 }], args: { id: 1, item: 'carrot' }, status: 'no-effect', carrot: 1 },
  { label: 'shear sheep', held: 'shears', ent: [{ ...cow, name: 'sheep', sheared: false }], inv: [{ name: 'shears', count: 1 }], args: { id: 1, item: 'shears' }, status: 'used', worn: 1, changed: { sheared: [false, true] }, field: ['sheared', true], spawned: 'white_wool' },
  { label: 'shear sheared', held: 'shears', ent: [{ ...cow, name: 'sheep', sheared: true }], inv: [{ name: 'shears', count: 1 }], args: { id: 1, item: 'shears' }, status: 'no-effect' },
  { label: 'lead', held: 'lead', ent: [cow], inv: [{ name: 'lead', count: 1 }], args: { id: 1, item: 'lead' }, status: 'used', consumed: 1, leash: 'attached', field: ['leashed', true] },
  { label: 'empty hand unleashes', ent: [{ ...cow, leashed: true }], args: { id: 1 }, status: 'used', leash: 'detached', held: null, field: ['leashed', false], spawned: 'lead' },
  { label: 'refused villager', ent: [{ ...cow, name: 'villager' }], args: { id: 1 }, status: 'cannot', reason: 'opens-window' },
  { label: 'refused horse before no-item', ent: [{ ...cow, name: 'horse' }], args: { id: 1, item: 'saddle' }, status: 'cannot', reason: 'mounts' },
  { label: 'refused boat', ent: [{ ...cow, name: 'oak_boat' }], args: { id: 1 }, status: 'cannot', reason: 'mounts' },
  { label: 'mounts', held: 'wheat', ent: [{ ...cow, mounts: true }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'failed', reason: 'mounted', wheat: 3 },
  { label: 'opens', held: 'wheat', ent: [{ ...cow, opens: true }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'failed', reason: 'opened-window', wheat: 3 },
  { label: 'accepts false', held: 'wheat', ent: [{ ...cow, accepts: false }], inv: wheat, args: { id: 1, item: 'wheat' }, status: 'no-effect', wheat: 3 }
]

for (const c of cases) {
  test(`interact: ${c.label}`, async () => {
    const p = createFake({ entities: c.ent, inventory: c.inv ?? [] })
    p.setOwner('t')
    const r = await p.interact('t', c.args)
    const count = (n) => p.world.state.inventory.find(i => i.name === n)?.count
    assert.equal(r.status, c.status)
    assert.equal(r.reason, c.reason)
    assert.equal(r.consumed, c.consumed ?? 0)
    assert.equal(r.worn, c.worn ?? 0)
    assert.equal(r.love, c.love ?? false)
    assert.equal(r.leash, c.leash ?? null)
    assert.deepEqual(r.changed, c.changed ?? {})
    assert.equal(count('wheat'), c.wheat)
    assert.equal(count('carrot'), c.carrot)
    assert.deepEqual(c.field ? p.world.state.entities.find(e => e.id === 1)[c.field[0]] : null, c.field ? c.field[1] : null)
    assert.equal(p.world.state.entities.filter(e => e.kind === 'item' && e.item.name === (c.spawned ?? '')).length, c.spawned ? 1 : 0)
    assert.equal(p.world.state.self.held, c.held ?? null)
  })
}

test('interact: a held call rejects with cut when the owner changes', async () => {
  const p = createFake({ entities: [cow] })
  p.setOwner('t')
  p.world.hold('interact')
  const pending = p.interact('t', { id: 1 })
  p.setOwner('u')
  await assert.rejects(pending, { code: 'cut' })
})

test('self raining and thundering default to false', () => {
  const s = createFake().self()
  assert.deepEqual([s.raining, s.thundering], [false, false])
})

test('spec raining shows in self', () => {
  assert.equal(createFake({ raining: true }).self().raining, true)
})

test('world.setRaining sets rain and thunder', () => {
  const p = createFake()
  p.world.setRaining(true, true)
  assert.deepEqual([p.self().raining, p.self().thundering], [true, true])
})

test('entities passes baby and uuid through', () => {
  const [e] = createFake({ entities: [{ ...cow, baby: true, uuid: 'u-1' }] }).entities()
  assert.deepEqual([e.baby, e.uuid], [true, 'u-1'])
})
