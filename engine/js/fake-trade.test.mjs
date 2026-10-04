import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createFake } from './fake.mjs'

const at = (x, y, z) => ({ x, y, z })
const offer = (cost, gives, extra = {}) => ({ cost, gives, ...extra })
const emeraldForWheat = offer([{ item: 'wheat', count: 20 }], { item: 'emerald', count: 1 })
const pair = offer([{ item: 'emerald', count: 5 }, { item: 'book', count: 1 }], { item: 'enchanted_book', count: 1 }, { maxUses: 4, uses: 1 })
const farmer = { id: 1, name: 'villager', kind: 'passive', uuid: 'v-1', pos: at(1, 64, 0), profession: 'farmer', level: 2, offers: [emeraldForWheat, pair] }

const rig = (ent, inventory = []) => {
  const p = createFake({ entities: ent, inventory })
  p.setOwner('t')
  return p
}
const count = (p, name) => p.world.state.inventory.filter(i => i.name === name).reduce((n, i) => n + i.count, 0)
const read = { villager: 'v-1', op: 'offers' }
const buy = (offerIdx, times) => ({ villager: 'v-1', op: 'buy', offer: offerIdx, ...(times && { times }) })

const refusals = [
  { label: 'gone', ent: [], args: read, want: { status: 'gone' } },
  { label: 'not a villager', ent: [{ ...farmer, name: 'cow', offers: undefined }], args: read, want: { status: 'cannot', reason: 'not-villager', name: 'cow' } },
  { label: 'too far', ent: [{ ...farmer, pos: at(9, 64, 0) }], args: read, want: { status: 'out-of-reach', reason: 'too-far' } },
  { label: 'unemployed by default', ent: [{ ...farmer, profession: undefined, offers: [] }], args: read, want: { status: 'cannot', reason: 'no-offers', why: 'unemployed', profession: 'unemployed', level: 2 } },
  { label: 'nitwit', ent: [{ ...farmer, profession: 'nitwit', offers: [] }], args: read, want: { status: 'cannot', reason: 'no-offers', why: 'nitwit', profession: 'nitwit', level: 2 } },
  { label: 'baby', ent: [{ ...farmer, baby: true }], args: read, want: { status: 'cannot', reason: 'no-offers', why: 'baby', profession: 'farmer', level: 2 } },
  { label: 'busy', ent: [{ ...farmer, busy: true }], args: read, want: { status: 'failed', reason: 'window-did-not-open' } },
  { label: 'busy on a buy', ent: [{ ...farmer, busy: true }], args: buy(0), want: { status: 'failed', reason: 'window-did-not-open' } },
  { label: 'zero offers', ent: [{ ...farmer, offers: [] }], args: read, want: { status: 'cannot', reason: 'no-offers', why: 'none', profession: 'farmer', level: 2 } },
  { label: 'no offers field', ent: [{ ...farmer, offers: undefined }], args: read, want: { status: 'cannot', reason: 'no-offers', why: 'none', profession: 'farmer', level: 2 } }
]

for (const c of refusals) {
  test(`trade: ${c.label}`, async () => {
    const r = await rig(c.ent).trade('t', c.args)
    assert.deepEqual(Object.fromEntries(Object.keys(c.want).map(k => [k, r[k]])), c.want)
  })
}

test('trade: too-far reports the distance', async () => {
  const r = await rig([{ ...farmer, pos: at(9, 64, 0) }]).trade('t', read)
  assert.equal(r.distance, 9)
})

test('trade: offers read lists both cost stacks, uses, left, disabled, profession and level', async () => {
  const sold = offer([{ item: 'wheat', count: 1 }], { item: 'bread', count: 2 }, { maxUses: 3, uses: 3 })
  const r = await rig([{ ...farmer, offers: [emeraldForWheat, pair, sold] }]).trade('t', read)
  assert.deepEqual(r, {
    status: 'ok',
    uuid: 'v-1',
    profession: 'farmer',
    level: 2,
    offers: [
      { index: 0, cost: [{ item: 'wheat', count: 20 }], gives: { item: 'emerald', count: 1 }, uses: 0, maxUses: 12, left: 12, disabled: false },
      { index: 1, cost: [{ item: 'emerald', count: 5 }, { item: 'book', count: 1 }], gives: { item: 'enchanted_book', count: 1 }, uses: 1, maxUses: 4, left: 3, disabled: false },
      { index: 2, cost: [{ item: 'wheat', count: 1 }], gives: { item: 'bread', count: 2 }, uses: 3, maxUses: 3, left: 0, disabled: true }
    ]
  })
})

test('trade: level defaults to 1', async () => {
  const r = await rig([{ ...farmer, level: undefined }]).trade('t', read)
  assert.equal(r.level, 1)
})

test('trade: entities() does not report offers or busy', () => {
  const [e] = rig([{ ...farmer, busy: true }]).entities()
  assert.deepEqual([e.offers, e.busy, e.uuid, e.profession], [undefined, undefined, 'v-1', 'farmer'])
})

test('trade: bad args reject with bad-args', async () => {
  const p = rig([farmer])
  const bad = [{}, { villager: '', op: 'offers' }, { villager: 'v-1', op: 'sell' }, { villager: 'v-1', op: 'buy' }, { villager: 'v-1', op: 'buy', offer: -1 }, { villager: 'v-1', op: 'buy', offer: 0, times: 0 }, { villager: 'v-1', op: 'buy', offer: 0, times: 1.5 }]
  for (const a of bad) await assert.rejects(p.trade('t', a), { code: 'bad-args' })
})

test('trade: a held call rejects with cut when the owner changes', async () => {
  const p = rig([farmer])
  p.world.hold('trade')
  const pending = p.trade('t', read)
  p.setOwner('u')
  await assert.rejects(pending, { code: 'cut' })
})

const wheat = n => [{ name: 'wheat', count: n }]
const fillers = n => Array.from({ length: n }, (_, i) => ({ name: `item_${i}`, count: 1 }))
const cheap = (extra = {}) => offer([{ item: 'wheat', count: 1 }], { item: 'emerald', count: 32 }, extra)

const buys = [
  { label: 'no such offer', ent: [farmer], args: buy(2), want: { status: 'cannot', reason: 'no-such-offer', offers: 2 } },
  { label: 'sold out', ent: [{ ...farmer, offers: [offer(emeraldForWheat.cost, emeraldForWheat.gives, { maxUses: 2, uses: 2 })] }], inv: wheat(40), args: buy(0), want: { status: 'cannot', reason: 'sold-out' } },
  { label: 'sold out before payment', ent: [{ ...farmer, offers: [offer(emeraldForWheat.cost, emeraldForWheat.gives, { maxUses: 2, uses: 2 })] }], args: buy(0), want: { status: 'cannot', reason: 'sold-out' } },
  { label: 'payment short', ent: [farmer], inv: wheat(5), args: buy(0), want: { status: 'no-item', short: { wheat: 15 } } },
  { label: 'second stack short', ent: [farmer], inv: [{ name: 'emerald', count: 5 }], args: buy(1), want: { status: 'no-item', short: { book: 1 } } },
  { label: 'both stacks short', ent: [farmer], inv: [{ name: 'emerald', count: 2 }], args: buy(1), want: { status: 'no-item', short: { emerald: 3, book: 1 } } },
  { label: 'full inventory', ent: [farmer], inv: [...wheat(30), ...fillers(35)], args: buy(0), want: { status: 'full' } },
  { label: 'normal buy', ent: [farmer], inv: wheat(45), args: buy(0), want: { status: 'bought', times: 1, requested: 1, gained: { emerald: 1 }, paid: { wheat: 20 }, stopped: null }, have: { wheat: 25, emerald: 1 }, uses: [0, 1] },
  { label: 'two cost stacks', ent: [farmer], inv: [{ name: 'emerald', count: 12 }, { name: 'book', count: 3 }], args: buy(1, 2), want: { status: 'bought', times: 2, requested: 2, gained: { enchanted_book: 2 }, paid: { emerald: 10, book: 2 }, stopped: null }, have: { emerald: 2, book: 1, enchanted_book: 2 }, uses: [1, 3] },
  { label: 'clamped by uses', ent: [{ ...farmer, offers: [{ ...emeraldForWheat, maxUses: 2 }] }], inv: [...wheat(64), ...wheat(64), ...wheat(64)], args: buy(0, 5), want: { status: 'bought', times: 2, requested: 5, gained: { emerald: 2 }, paid: { wheat: 40 }, stopped: 'sold-out' }, have: { wheat: 152, emerald: 2 }, uses: [0, 2] },
  { label: 'clamped by payment', ent: [farmer], inv: wheat(45), args: buy(0, 5), want: { status: 'bought', times: 2, requested: 5, gained: { emerald: 2 }, paid: { wheat: 40 }, stopped: 'payment' }, have: { wheat: 5, emerald: 2 }, uses: [0, 2] },
  { label: 'clamped by room', ent: [{ ...farmer, offers: [cheap()] }], inv: [...wheat(64), ...fillers(34)], args: buy(0, 5), want: { status: 'bought', times: 2, requested: 5, gained: { emerald: 64 }, paid: { wheat: 2 }, stopped: 'room' }, have: { wheat: 62, emerald: 64 }, uses: [0, 2] },
  { label: 'room in a partial stack', ent: [{ ...farmer, offers: [cheap()] }], inv: [...wheat(64), { name: 'emerald', count: 60 }, ...fillers(33)], args: buy(0, 5), want: { status: 'bought', times: 2, requested: 5, gained: { emerald: 64 }, paid: { wheat: 2 }, stopped: 'room' }, have: { wheat: 62, emerald: 124 }, uses: [0, 2] },
  { label: 'sold-out wins over payment and room', ent: [{ ...farmer, offers: [cheap({ maxUses: 1 })] }], inv: wheat(1), args: buy(0, 3), want: { status: 'bought', times: 1, requested: 3, gained: { emerald: 32 }, paid: { wheat: 1 }, stopped: 'sold-out' }, have: { emerald: 32 }, uses: [0, 1] }
]

for (const c of buys) {
  test(`trade buy: ${c.label}`, async () => {
    const p = rig(c.ent, c.inv ?? [])
    const r = await p.trade('t', c.args)
    assert.deepEqual(Object.fromEntries(Object.keys(c.want).map(k => [k, r[k]])), c.want)
    const have = Object.fromEntries(Object.keys(c.have ?? {}).map(k => [k, count(p, k)]))
    assert.deepEqual(have, c.have ?? {})
    const after = (await p.trade('t', read)).offers?.[c.args.offer]?.uses
    assert.deepEqual([c.ent[0].offers[c.args.offer]?.uses ?? 0, after].slice(0, c.uses ? 2 : 0), c.uses ?? [])
  })
}

test('trade buy: a further buy after the clamp says sold-out, and the read shows it', async () => {
  const p = rig([{ ...farmer, offers: [{ ...emeraldForWheat, maxUses: 2 }] }], wheat(100))
  await p.trade('t', buy(0, 5))
  const again = await p.trade('t', buy(0))
  const [o] = (await p.trade('t', read)).offers
  assert.deepEqual([again, o.left, o.disabled, count(p, 'emerald')], [{ status: 'cannot', reason: 'sold-out' }, 0, true, 2])
})

test('trade buy: the stacks of the inventory never pass 64 and 36 slots', async () => {
  const p = rig([{ ...farmer, offers: [cheap()] }], [...wheat(64), ...fillers(34)])
  await p.trade('t', buy(0, 5))
  const stacks = p.world.state.inventory
  assert.deepEqual([stacks.length <= 36, Math.max(...stacks.map(i => i.count)) <= 64], [true, true])
})
