// The fake's `trade` primitive: a villager's offers, and buying from them. Mirrors js/trade.mjs.
const REACH = 3.5
const SLOTS = 36
const STACK = 64

const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)

const badArgs = message => Object.assign(new Error(message), { code: 'bad-args', badArgs: true })
const isCount = n => Number.isInteger(n) && n >= 1

function check (a) {
  if (typeof a?.villager !== 'string' || a.villager === '') throw badArgs('trade needs villager, a uuid string')
  if (!['offers', 'buy'].includes(a.op)) throw badArgs('trade op must be offers or buy')
  if (a.op === 'buy' && !(Number.isInteger(a.offer) && a.offer >= 0)) throw badArgs('trade buy needs offer, an integer >= 0')
  if (a.times !== undefined && !isCount(a.times)) throw badArgs('trade times must be an integer >= 1')
}

const row = (o, index) => {
  const uses = o.uses ?? 0
  const maxUses = o.maxUses ?? 12
  const left = Math.max(0, maxUses - uses)
  return { index, cost: o.cost.map(c => ({ item: c.item, count: c.count })), gives: { ...o.gives }, uses, maxUses, left, disabled: left === 0 }
}

const carried = (s, name) => s.inventory.filter(i => i.name === name).reduce((n, i) => n + i.count, 0)

// How many of the result fit: free slots plus the unfilled part of the stacks already carried
const fits = (s, name, count) => {
  const partial = s.inventory.filter(i => i.name === name).reduce((room, i) => room + Math.max(0, STACK - i.count), 0)
  return Math.floor(((SLOTS - s.inventory.length) * STACK + partial) / count)
}

const take = (s, name, count) => {
  let owed = count
  for (const stack of s.inventory.filter(i => i.name === name)) {
    const n = Math.min(owed, stack.count)
    stack.count -= n
    owed -= n
  }
  s.inventory = s.inventory.filter(i => i.count > 0)
}

const give = (s, name, count) => {
  let rest = count
  for (const stack of s.inventory.filter(i => i.name === name && i.count < STACK)) {
    const n = Math.min(rest, STACK - stack.count)
    stack.count += n
    rest -= n
  }
  for (; rest > 0; rest -= Math.min(rest, STACK)) s.inventory.push({ name, count: Math.min(rest, STACK) })
}

function buy (s, e, rows, a) {
  const r = rows[a.offer]
  if (!r) return { status: 'cannot', reason: 'no-such-offer', offers: rows.length }
  if (r.disabled) return { status: 'cannot', reason: 'sold-out' }
  const short = Object.fromEntries(r.cost.map(c => [c.item, Math.max(0, c.count - carried(s, c.item))]).filter(([, n]) => n > 0))
  if (Object.keys(short).length > 0) return { status: 'no-item', short }
  const room = fits(s, r.gives.item, r.gives.count)
  if (room === 0) return { status: 'full' }
  const times = a.times ?? 1
  const payable = Math.min(...r.cost.map(c => Math.floor(carried(s, c.item) / c.count)))
  const limits = [['sold-out', r.left], ['payment', payable], ['room', room]]
  const n = Math.min(times, ...limits.map(([, v]) => v))
  const paid = Object.fromEntries(r.cost.map(c => [c.item, c.count * n]))
  for (const [item, count] of Object.entries(paid)) take(s, item, count)
  give(s, r.gives.item, r.gives.count * n)
  e.offers[a.offer].uses = r.uses + n
  const stopped = n >= times ? null : limits.find(([, v]) => v === n)[0]
  return { status: 'bought', times: n, requested: times, gained: { [r.gives.item]: r.gives.count * n }, paid, stopped }
}

export const fakeTrade = s => async (token, a) => {
  check(a)
  const e = s.entities.find(x => x.uuid === a.villager)
  if (!e) return { status: 'gone' }
  if (e.name !== 'villager') return { status: 'cannot', reason: 'not-villager', name: e.name }
  const distance = dist(s.self.pos, e.pos)
  if (distance > REACH) return { status: 'out-of-reach', reason: 'too-far', distance }
  const profession = e.profession ?? 'unemployed'
  const level = e.level ?? 1
  const why = e.baby ? 'baby' : ['unemployed', 'nitwit'].includes(profession) ? profession : null
  if (why) return { status: 'cannot', reason: 'no-offers', why, profession, level }
  if (e.busy) return { status: 'failed', reason: 'window-did-not-open' }
  const rows = (e.offers ?? []).map(row)
  if (rows.length === 0) return { status: 'cannot', reason: 'no-offers', why: 'none', profession, level }
  if (a.op === 'offers') return { status: 'ok', uuid: a.villager, profession, level, offers: rows }
  return buy(s, e, rows, a)
}
