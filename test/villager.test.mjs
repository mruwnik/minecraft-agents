import { test } from 'node:test'
import assert from 'node:assert/strict'
import { parseWant, bookOffer, rollVerdict, rollRefusal, tradeLine, offerCost, cheapestLockOffer, matchesVillagerOutput, JOB_BLOCK_PROFESSION, villagerPenPlan } from '../src/lib.mjs'
import roll from '../library/villager/roll.mjs'

const book = (enchant, level, emeralds = 20, index = 1) => ({ index, inputItem1: { name: 'emerald', count: emeralds }, inputItem2: { name: 'book', count: 1 }, outputItem: { name: 'enchanted_book', count: 1, enchants: [{ name: enchant, lvl: level }] }, nbTradeUses: 0, maximumNbTradeUses: 12 })

function reachableStandingCells (plan, cell, radius = 8) {
  const solids = new Set()
  const key = (x, y, z) => `${x},${y},${z}`
  const planned = [...plan.walls, ...plan.guard, ...plan.service, cell]
  for (const p of planned) solids.add(key(p.x, p.y, p.z))

  // Model ordinary flat ground, plus a one-block-high exterior platform around the plan.
  // The raised ring tests whether a villager could step onto the enclosure and bypass it.
  const route = [...plan.interior, plan.gate, plan.outside, plan.serviceStand]
  const plannedColumns = new Set([...planned, ...route].map(p => `${p.x},${p.z}`))
  for (let x = cell.x - radius; x <= cell.x + radius; x++) {
    for (let z = cell.z - radius; z <= cell.z + radius; z++) {
      solids.add(key(x, cell.y - 1, z))
      if (Math.max(Math.abs(x - cell.x), Math.abs(z - cell.z)) >= 3 && !plannedColumns.has(`${x},${z}`)) solids.add(key(x, cell.y, z))
    }
  }

  const walkable = (x, y, z) => !solids.has(key(x, y, z)) && !solids.has(key(x, y + 1, z)) && solids.has(key(x, y - 1, z))
  const pending = []
  for (let x = cell.x - radius; x <= cell.x + radius; x++) {
    for (let z = cell.z - radius; z <= cell.z + radius; z++) {
      if (Math.abs(x - cell.x) !== radius && Math.abs(z - cell.z) !== radius) continue
      for (const y of [cell.y, cell.y + 1]) if (walkable(x, y, z)) pending.push({ x, y, z })
    }
  }
  const reached = new Set(pending.map(p => key(p.x, p.y, p.z)))
  while (pending.length) {
    const here = pending.shift()
    for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      for (let dy = -3; dy <= 1; dy++) {
        const next = { x: here.x + dx, y: here.y + dy, z: here.z + dz }
        if (Math.abs(next.x - cell.x) > radius || Math.abs(next.z - cell.z) > radius || !walkable(next.x, next.y, next.z)) continue
        const id = key(next.x, next.y, next.z)
        if (reached.has(id)) continue
        reached.add(id)
        pending.push(next)
      }
    }
  }
  return [...reached].map(v => {
    const [x, y, z] = v.split(',').map(Number)
    return { x, y, z }
  })
}

test('want grammar accepts exact, minimum and any level, and suggests unknown names', () => {
  assert.deepEqual(parseWant('sharpness:5, efficiency:4+,mending'), [
    { enchant: 'sharpness', level: 5, atLeast: false },
    { enchant: 'efficiency', level: 4, atLeast: true },
    { enchant: 'mending', level: null, atLeast: false }
  ])
  assert.match(parseWant('mendng'), /mending/)
  assert.match(parseWant('sharpness:0'), /invalid/)
})

test('book offers read stored enchantments and both price inputs', () => {
  const offer = book('minecraft:sharpness', 5, 24)
  assert.deepEqual(bookOffer(offer, 2), { enchant: 'sharpness', level: 5, price: 24, index: 2 })
  assert.deepEqual(offerCost(offer), { emerald: 24, book: 1 })
  assert.match(tradeLine(offer, 2), /24 emerald \+ 1 book -> 1 enchanted_book sharpness 5/)
  assert.equal(bookOffer({ ...offer, outputItem: { name: 'bookshelf' } }), null)
})

test('roll selects the cheapest matching book and reports a close miss', () => {
  const offers = [book('sharpness', 4, 12), book('sharpness', 5, 33), book('sharpness', 5, 25)]
  const wants = parseWant('sharpness:5')
  assert.deepEqual(rollVerdict(offers, wants, 30).found, { enchant: 'sharpness', level: 5, price: 25, index: 3 })
  assert.equal(rollVerdict(offers.slice(0, 2), wants, 20).best.level, 5)
})

test('locking buys affordable paper before spending emeralds or a book', () => {
  const enchanted = book('mending', 1, 24, 1)
  const paper = { index: 2, inputItem1: { name: 'paper', count: 24 }, outputItem: { name: 'emerald', count: 1 } }
  assert.equal(cheapestLockOffer([enchanted, paper], { emerald: 64, book: 1, paper: 24 }).index, 2)
  assert.equal(cheapestLockOffer([enchanted, paper], { emerald: 64, book: 1 }).index, 1)
})

test('workstation map and desired-output matcher keep the lock purchase separate', () => {
  assert.equal(JOB_BLOCK_PROFESSION.composter, 'farmer')
  assert.equal(JOB_BLOCK_PROFESSION.fletching_table, 'fletcher')
  const arrow = { index: 1, inputItem1: { name: 'stick', count: 32 }, outputItem: { name: 'arrow', count: 16 }, nbTradeUses: 0, maximumNbTradeUses: 16 }
  const paper = { index: 2, inputItem1: { name: 'paper', count: 1 }, outputItem: { name: 'emerald', count: 1 }, nbTradeUses: 0, maximumNbTradeUses: 16 }
  assert.equal(matchesVillagerOutput(arrow, { output: 'minecraft:arrow' }), true)
  assert.equal(matchesVillagerOutput(paper, { output: 'arrow' }), false)
  const enchanted = { outputItem: { name: 'diamond_sword', enchants: [{ name: 'minecraft:sharpness', lvl: 3 }] } }
  assert.equal(matchesVillagerOutput(enchanted, { output: 'diamond_sword', enchant: 'sharpness', level: 3 }), true)
  assert.equal(matchesVillagerOutput(enchanted, { output: 'diamond_sword', enchant: 'sharpness', level: 4 }), false)
  assert.equal(cheapestLockOffer([arrow, paper], { stick: 32, paper: 1 }).index, 2)
  assert.equal(cheapestLockOffer([{ ...paper, nbTradeUses: 4, maximumNbTradeUses: 4 }, arrow], { stick: 32, paper: 1 }).index, 1)
})

test('pen geometry blocks every reachable exterior cell within lectern claim range, including raised routes', () => {
  const cell = { x: 10, y: 64, z: 10 }
  const pen = villagerPenPlan(cell, { x: 8, y: 64, z: 10 })
  assert.deepEqual(pen.interior, [{ x: 9, y: 64, z: 10 }, { x: 8, y: 64, z: 10 }])
  assert.deepEqual(pen.gate, { x: 7, y: 64, z: 10 })
  assert.deepEqual(pen.serviceStand, { x: 13, y: 64, z: 10 })
  assert.equal(pen.walls.length, 26)
  assert.equal(pen.guard.length, 30)
  assert.equal(pen.service.length, 4)

  for (const [dx, dz] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
    const oriented = villagerPenPlan(cell, { x: cell.x + dx * 2.5, z: cell.z + dz * 2.5 })
    const lecternCenter = { x: cell.x + 0.5, y: cell.y + 0.5, z: cell.z + 0.5 }
    const claimable = p => Math.hypot(p.x + 0.5 - lecternCenter.x, p.y - lecternCenter.y, p.z + 0.5 - lecternCenter.z) < 2
    const reachable = reachableStandingCells(oriented, cell)
    assert.ok(oriented.interior.some(p => reachable.some(r => r.x === p.x && r.y === p.y && r.z === p.z)), 'the open doorway still reaches the intended interior')
    assert.deepEqual(reachable.filter(p => claimable(p) && !oriented.interior.some(i => i.x === p.x && i.y === p.y && i.z === p.z)), [], 'no exterior standing cell in claim range is reachable')
  }
})

test('preconditions require one adult, a job block and money before rolling', () => {
  const base = { villagers: 1, profession: 'unemployed', adult: true, day: true, carried: { lectern: 1, emerald: 64, book: 1 }, buy: true }
  assert.equal(rollRefusal(base), null)
  assert.match(rollRefusal({ ...base, villagers: 2 }), /2 villagers/)
  assert.equal(rollRefusal({ ...base, villagers: 2, allowCrowd: true }), null)
  assert.match(rollRefusal({ ...base, carried: { lectern: 1 } }), /64 emerald/)
  assert.equal(rollRefusal({ ...base, placed: true, carried: { emerald: 64, book: 1 } }), null)
})

test('rerolling refuses without a carried spare lectern before changing the world', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const villager = { id: 7, exact: '2,64,2', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  let changes = 0
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => ({}),
    block: () => ({ name: 'lectern' }), report: () => {}, note: () => {},
    act: async name => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      changes++
      throw new Error(`unexpected world change: ${name}`)
    }
  }
  await assert.rejects(roll.run(api, { want: 'mending', ...cell, tries: 2, pen: false }), /rerolling needs 1 carried lectern/)
  assert.equal(changes, 0)
})

test('roller restores the lectern over three rounds and locks with paper', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const villager = { id: 7, exact: '2,64,2', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  const inv = { lectern: 2, paper: 64 }
  const offered = [
    [book('sharpness', 1, 12)],
    [book('unbreaking', 2, 15)],
    [book('mending', 1, 18), { index: 2, inputItem1: { name: 'paper', count: 24 }, outputItem: { name: 'emerald', count: 1 }, nbTradeUses: 0, maximumNbTradeUses: 16 }]
  ]
  let placed = false; let round = 0; let locked = false; let bought = null
  const notes = []; const reports = []
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: () => placed ? { name: 'lectern' } : { name: 'air' },
    report: r => reports.push(r), note: n => notes.push(n), checkpoint: async () => {},
    until: async pred => { assert.equal(await pred(), true) },
    act: async (name, a) => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'place') { placed = true; inv.lectern--; return {} }
      if (name === 'dig') { placed = false; inv.lectern++; round++; return {} }
      if (name === 'trades') return { profession: placed || locked ? 'librarian' : 'unemployed', offers: offered[round] }
      if (name === 'trade') { bought = a.offer; locked = true; inv.paper -= 24; inv.emerald = 1; return {} }
      throw new Error(name)
    }
  }
  const result = await roll.run(api, { want: 'mending', ...cell, buy: true, tries: 4, pen: false })
  assert.equal(result.rounds, 3)
  assert.equal(result.found, 'mending 1')
  assert.equal(result.boughtOffer, 2)
  assert.equal(result.locked, true)
  assert.equal(bought, 2)
  assert.equal(placed, true)
  assert.equal(notes.length, 3)
  assert.equal(reports.at(-1).rounds, 3)
})

test('generic workstation roll tracks UUID across changing entity IDs and separately locks the cheapest offer', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const uuid = '0c432c3c-1111-4111-8111-111111111111'
  const raw = Array(21).fill(null); raw[19] = { villagerProfession: 0 }
  let query = 0; let lastEntityId = null; let tradeArgs = null; let tradeTimeEntityId = null
  const villager = () => ({ id: 40 + query++, uuid, exact: '2,64,2', metadata: JSON.stringify(raw) })
  const arrow = { index: 1, inputItem1: { name: 'stick', count: 32 }, outputItem: { name: 'arrow', count: 16 }, nbTradeUses: 0, maximumNbTradeUses: 16 }
  const paper = { index: 2, inputItem1: { name: 'paper', count: 1 }, outputItem: { name: 'emerald', count: 1 }, nbTradeUses: 0, maximumNbTradeUses: 16 }
  const inv = { paper: 1, stick: 32 }
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: () => ({ name: 'fletching_table' }), report: () => {}, note: () => {}, checkpoint: async () => {},
    until: async pred => assert.equal(await pred(), true),
    act: async (name, args) => {
      if (name === 'goto' || name === 'find_blocks') return name === 'find_blocks' ? { positions: [] } : {}
      if (name === 'entity') { const found = villager(); lastEntityId = found.id; return { found: [found] } }
      if (name === 'trades') { assert.equal(args.id, lastEntityId); return { profession: 'fletcher', offers: [arrow, paper] } }
      if (name === 'trade') { tradeArgs = args; tradeTimeEntityId = lastEntityId; return {} }
      throw new Error(name)
    }
  }
  const result = await roll.run(api, { ...cell, block: 'fletching_table', output: 'arrow', uuid, buy: true, tries: 1, pen: false })
  assert.equal(result.uuid, uuid)
  assert.equal(result.profession, 'fletcher')
  assert.equal(result.found, 'arrow')
  assert.equal(result.locked, true)
  assert.equal(result.boughtOffer, 2)
  assert.equal(tradeArgs.offer, 2)
  assert.equal(tradeArgs.id, tradeTimeEntityId)
})

test('roller stops with the lectern placed when the last spare has been used', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const villager = { id: 7, exact: '2,64,2', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  const inv = { lectern: 1 }
  let placed = true; let digs = 0
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: () => ({ name: placed ? 'lectern' : 'air' }), report: () => {}, note: () => {}, checkpoint: async () => {},
    until: async pred => assert.equal(await pred(), true),
    act: async (name, args) => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'trades') return { profession: placed ? 'librarian' : 'unemployed', offers: [book('unbreaking', 1, 12)] }
      if (name === 'dig') { placed = false; digs++; return {} }
      if (name === 'place') { placed = true; inv.lectern--; return {} }
      throw new Error(name)
    }
  }
  const result = await roll.run(api, { want: 'mending', ...cell, tries: 4, pen: false })
  assert.match(result.stopped, /out of spare lectern/)
  assert.equal(digs, 1)
  assert.equal(inv.lectern, 0)
  assert.equal(placed, true)
})

test('roller builds an entrance, lures one villager, and closes the pen before reading offers', async () => {
  const cell = { x: 10, y: 64, z: 10 }
  const pen = villagerPenPlan(cell, { x: 12, y: 64, z: 10 })
  const blocks = new Map()
  const key = p => `${p.x},${p.y},${p.z}`
  const inv = { lectern: 1, cobblestone: 63, paper: 64 }
  let lured = false; let traded = false
  const entity = () => ({ id: 4, exact: lured ? '11.5,64,10.5' : '12.5,64,10.5', metadata: JSON.stringify(Array(21).fill(null)) })
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: (x, y, z) => y === 63 ? { name: 'grass_block', solid: true } : { name: blocks.get(key({ x, y, z })) ?? 'air', solid: false },
    report: () => {}, note: () => {}, checkpoint: async () => {}, until: async pred => assert.equal(await pred(), true),
    act: async (name, a) => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [entity()] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'place') {
        for (const p of a.blocks ?? [a]) { blocks.set(key(p), p.item); inv[p.item]-- }
        if (blocks.get(key(cell)) === 'lectern') lured = true
        return {}
      }
      if (name === 'trades') {
        assert.equal(blocks.get(key({ x: 9, y: 64, z: 10 })), 'cobblestone')
        assert.equal(blocks.get(key({ x: 9, y: 65, z: 10 })), undefined)
        assert.equal(blocks.get(key({ x: 9, y: 66, z: 10 })), 'cobblestone')
        assert.equal(blocks.get(key({ x: 10, y: 66, z: 10 })), 'cobblestone')
        assert.equal(blocks.get(key(pen.gate)), 'cobblestone')
        assert.equal(blocks.get(key({ ...pen.gate, y: 65 })), 'cobblestone')
        assert.equal(blocks.get(key({ ...pen.gate, y: 66 })), 'cobblestone')
        return { profession: 'librarian', offers: [book('mending', 1, 20), { index: 2, inputItem1: { name: 'paper', count: 24 }, outputItem: { name: 'emerald', count: 1 } }] }
      }
      if (name === 'trade') { traded = true; return {} }
      throw new Error(name)
    }
  }
  const result = await roll.run(api, { want: 'mending', ...cell, buy: true, tries: 1 })
  assert.equal(result.locked, true)
  assert.equal(result.boughtOffer, 2)
  assert.equal(traded, true)
  assert.equal(result.locked, true)
  assert.equal(blocks.get(key({ ...pen.gate, y: 65 })), 'cobblestone')
})

function serviceRecoveryApi ({ interruptAtNook = false } = {}) {
  const cell = { x: 10, y: 64, z: 10 }
  const pen = villagerPenPlan(cell, { x: 12.5, y: 64, z: 10.5 })
  const blocks = new Map()
  const key = p => `${p.x},${p.y},${p.z}`
  const inv = { lectern: 2, cobblestone: 65 }
  const events = []
  let placed = false; let lured = false; let missedRounds = 0
  const entity = () => ({ id: 9, exact: `${pen.center.x + 0.5},64,${pen.center.z + 0.5}`, metadata: JSON.stringify(Array(21).fill(null)) })
  const move = p => p && p.x === pen.service[0].x && p.y === pen.service[0].y && p.z === pen.service[0].z
  const worldAction = async (name, a) => {
    events.push([name, a])
    if (name === 'goto') {
      if (move(a) && interruptAtNook) throw new Error('interrupted while entering service nook')
      return {}
    }
    if (name === 'entity') return { found: [entity()] }
    if (name === 'find_blocks') return { positions: [] }
    if (name === 'place') {
      for (const p of a.blocks ?? [a]) {
        blocks.set(key(p), p.item)
        inv[p.item] = (inv[p.item] ?? 0) - 1
        if (p.x === cell.x && p.y === cell.y && p.z === cell.z && p.item === 'lectern') { placed = true; lured = true }
      }
      return {}
    }
    if (name === 'dig') {
      blocks.delete(key(a))
      if (a.x === cell.x && a.y === cell.y && a.z === cell.z) { placed = false; missedRounds++ }
      else inv.cobblestone++
      return {}
    }
    if (name === 'trades') return {
      profession: placed ? 'librarian' : 'unemployed',
      offers: [book(missedRounds === 0 ? 'unbreaking' : 'mending', 1, 20)]
    }
    throw new Error(`unexpected action ${name}`)
  }
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: (x, y, z) => y === 63 ? { name: 'grass_block', solid: true } : { name: blocks.get(key({ x, y, z })) ?? 'air', solid: false },
    report: () => {}, note: () => {}, checkpoint: async () => {}, act: worldAction,
    until: async (predicate, options = {}) => {
      events.push(['until', options.what])
      if (/lectern drop stayed beyond pickup reach/.test(options.what ?? '')) {
        assert.ok(events.some(([name, a]) => name === 'goto' && move(a)), 'the bot enters the inner service nook before pickup')
        if (!interruptAtNook) inv.lectern++
      }
      assert.equal(await predicate(), true)
    },
    cleanupAct: async (name, a) => worldAction(name, a)
  }
  return { api, pen, cell, blocks, inv, events, getPlaced: () => placed, getLured: () => lured }
}

function penCaptureApi ({ target = { x: -11, y: 64, z: 0.5 }, id = 94, uuid = null, rival = false, prebuilt = false, blockedGate = false } = {}) {
  const cell = { x: 0, y: 64, z: 0 }
  const pen = villagerPenPlan(cell, target)
  const blocks = new Map()
  const key = p => `${p.x},${p.y},${p.z}`
  const inv = { lectern: 1, cobblestone: prebuilt ? 2 : 63 }
  const events = []
  let lured = false
  if (prebuilt) {
    for (const p of [...pen.walls, ...pen.guard, ...pen.service, { ...pen.gate, y: pen.gate.y + 2 }]) blocks.set(key(p), 'cobblestone')
    if (blockedGate) {
      blocks.set(key(pen.gate), 'cobblestone')
      blocks.set(key({ ...pen.gate, y: pen.gate.y + 1 }), 'cobblestone')
    }
  }
  const metadata = JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } }))
  const entity = () => ({ id, ...(uuid ? { uuid } : {}), exact: lured ? `${pen.center.x + 0.5},64,${pen.center.z + 0.5}` : `${target.x},${target.y},${target.z}`, metadata })
  const entities = () => rival ? [{ id: id - 1, uuid: 'rival-uuid', exact: `${pen.center.x + 0.5},64,${pen.center.z + 0.5}`, metadata }, entity()] : [entity()]
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => inv,
    block: (x, y, z) => y === 63 ? { name: 'grass_block', solid: true } : { name: blocks.get(key({ x, y, z })) ?? 'air', solid: false },
    report: () => {}, note: () => {}, checkpoint: async () => {},
    until: async predicate => assert.equal(await predicate(), true),
    act: async (name, a) => {
      events.push([name, a])
      if (name === 'goto') return {}
      if (name === 'entity') return { found: entities() }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'place') {
        for (const p of a.blocks ?? [a]) {
          blocks.set(key(p), p.item)
          inv[p.item] = (inv[p.item] ?? 0) - 1
          if (p.x === cell.x && p.y === cell.y && p.z === cell.z && p.item === 'lectern') lured = true
        }
        return {}
      }
      if (name === 'trades') return { profession: 'librarian', offers: [book('mending', 1, 20)] }
      throw new Error(`unexpected action ${name}`)
    }
  }
  return { api, cell, pen, blocks, inv, events }
}

test('UUID-targeted pen capture ignores a different fresh villager already inside', async () => {
  const uuid = 'f25ef3e5-37cc-4da0-a192-34493cfa0e22'
  const h = penCaptureApi({ uuid, rival: true })
  const result = await roll.run(h.api, { want: 'mending', ...h.cell, uuid, tries: 1 })
  assert.equal(result.uuid, uuid)
  assert.equal(h.events.some(([name, a]) => name === 'place' && a.x === h.pen.gate.x && a.y === h.pen.gate.y), true)
})

test('roller opens the service nook to recover the dropped lectern, then reseals it', async () => {
  const h = serviceRecoveryApi()
  const result = await roll.run(h.api, { want: 'mending', ...h.cell, tries: 2 })
  assert.equal(result.found, 'mending 1')
  assert.equal(h.getLured(), true)
  assert.equal(h.getPlaced(), true)
  assert.equal(h.inv.lectern, 1)
  for (const p of h.pen.service.filter(p => p.y === h.cell.y)) assert.equal(h.blocks.get(`${p.x},${p.y},${p.z}`), 'cobblestone')
  const names = h.events.map(([name]) => name)
  const cellDig = names.indexOf('dig')
  const nook = names.findIndex((name, i) => name === 'goto' && h.events[i][1].x === h.pen.service[0].x && h.events[i][1].y === h.cell.y)
  const recoveryWait = names.findIndex((name, i) => name === 'until' && /lectern drop stayed/.test(h.events[i][1] ?? ''))
  const serviceExit = names.findIndex((name, i) => name === 'goto' && h.events[i][1].x === h.pen.serviceStand.x && h.events[i][1].y === h.cell.y && i > nook)
  assert.ok(cellDig < nook && nook < recoveryWait && recoveryWait < serviceExit)
})

test('interruption in the service nook uses cleanup actions to seal the route and replace the lectern', async () => {
  const h = serviceRecoveryApi({ interruptAtNook: true })
  await assert.rejects(roll.run(h.api, { want: 'mending', ...h.cell, tries: 2 }), /interrupted while entering service nook/)
  assert.equal(h.getPlaced(), true)
  assert.equal(h.inv.lectern, 0)
  for (const p of h.pen.service.filter(p => p.y === h.cell.y)) assert.equal(h.blocks.get(`${p.x},${p.y},${p.z}`), 'cobblestone')
  const cleanup = h.events.filter(([name]) => name === 'goto' || name === 'place').slice(-4)
  assert.deepEqual(cleanup.map(([name]) => name), ['goto', 'place', 'place', 'place'])
  assert.equal(h.events.at(-1)[1].item, 'lectern')
})

test('an explicit ID can be captured from 11 blocks away and the valid closest footprint is selected', async () => {
  const h = penCaptureApi()
  const result = await roll.run(h.api, { want: 'mending', ...h.cell, id: 94, tries: 1 })
  assert.equal(result.found, 'mending 1')
  const directions = [{ x: 1, z: 0 }, { x: -1, z: 0 }, { x: 0, z: 1 }, { x: 0, z: -1 }]
  const closest = directions
    .map(d => villagerPenPlan(h.cell, { x: h.cell.x + d.x, z: h.cell.z + d.z }))
    .sort((a, b) => Math.hypot(a.outside.x - (-11), a.outside.z - 0.5) - Math.hypot(b.outside.x - (-11), b.outside.z - 0.5))[0]
  const gotos = h.events.filter(([name]) => name === 'goto').map(([, a]) => a)
  assert.ok(gotos.some(p => p.x === closest.serviceStand.x && p.z === closest.serviceStand.z))
  assert.ok(gotos.some(p => p.x === closest.outside.x && p.z === closest.outside.z))
})

test('automatic selection and pen=false still require a villager within eight blocks', async () => {
  const target = { id: 94, exact: '-10.5,64,0.5', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  const makeApi = () => ({
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => ({ lectern: 2 }),
    block: () => ({ name: 'air' }), act: async name => name === 'goto' ? {} : name === 'entity' ? { found: [target] } : { positions: [] }
  })
  await assert.rejects(roll.run(makeApi(), { want: 'mending', x: 0, y: 64, z: 0, tries: 1 }), /no adult unemployed villager near/)
  await assert.rejects(roll.run(makeApi(), { want: 'mending', x: 0, y: 64, z: 0, id: 94, tries: 1, pen: false }), /villager 94 is not near/)
})

test('an existing pen may keep its top gate lintel open, but a blocked lower entry without a villager refuses', async () => {
  const open = penCaptureApi({ target: { x: -5.5, y: 64, z: 0.5 }, prebuilt: true })
  assert.equal(open.blocks.get(`${open.pen.gate.x},${open.pen.gate.y + 2},${open.pen.gate.z}`), 'cobblestone')
  assert.equal(open.blocks.get(`${open.pen.gate.x},${open.pen.gate.y},${open.pen.gate.z}`), undefined)
  const result = await roll.run(open.api, { want: 'mending', ...open.cell, id: 94, tries: 1 })
  assert.equal(result.found, 'mending 1')

  const blocked = penCaptureApi({ target: { x: -5.5, y: 64, z: 0.5 }, prebuilt: true, blockedGate: true })
  let changed = false
  const act = blocked.api.act
  blocked.api.act = async (name, args) => {
    if (name === 'place' || name === 'dig') changed = true
    return act(name, args)
  }
  await assert.rejects(roll.run(blocked.api, { want: 'mending', ...blocked.cell, id: 94, tries: 1 }), /pen entry is already blocked with no villager inside/)
  assert.equal(changed, false)
})

test('roller refuses an unsafe pen route before placing any blocks', async () => {
  const cell = { x: 10, y: 64, z: 10 }
  const villager = { id: 4, exact: '12.5,64,10.5', metadata: JSON.stringify(Array(21).fill(null)) }
  let placements = 0
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => ({ lectern: 2, cobblestone: 20 }),
    block: (x, y, z) => y === 63 ? { name: 'grass_block', solid: !(x === 8 && z === 9) } : { name: 'air', solid: false },
    report: () => {}, note: () => {}, checkpoint: async () => {}, until: async () => { throw new Error('should not wait') },
    act: async name => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'place') { placements++; return {} }
      throw new Error(name)
    }
  }
  await assert.rejects(roll.run(api, { want: 'mending', ...cell }), /pen needs flat solid ground under 8,64,9/)
  assert.equal(placements, 0)
})

test('buy=true stops when the matching book exists but no trade can be paid for', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const villager = { id: 7, exact: '2,64,2', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }),
    inv: () => ({ paper: 64 }), block: () => ({ name: 'lectern' }),
    report: () => {}, note: () => {}, checkpoint: async () => {}, until: async pred => assert.equal(await pred(), true),
    act: async name => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'trades') return {
        profession: 'librarian',
        offers: [book('mending', 1, 20), { index: 2, inputItem1: { name: 'coal', count: 4 }, outputItem: { name: 'emerald', count: 1 } }]
      }
      throw new Error(name)
    }
  }
  await assert.rejects(roll.run(api, { want: 'mending', ...cell, buy: true, pen: false, tries: 1 }), /no offer is affordable to lock the villager/)
  assert.equal(api.block(...Object.values(cell)).name, 'lectern')
})

test('roller restores a dug lectern when an interrupted reroll fails to release the profession', async () => {
  const cell = { x: 1, y: 64, z: 2 }
  const villager = { id: 7, exact: '2,64,2', metadata: JSON.stringify(Object.assign(Array(21).fill(null), { 19: { villagerProfession: 0 } })) }
  let placed = true; let cleanedUp = false
  const api = {
    places: () => [], zones: () => [], me: () => 'Probe', clock: () => ({ day: true }), inv: () => ({ lectern: 1 }),
    block: () => ({ name: placed ? 'lectern' : 'air' }), report: () => {}, note: () => {}, checkpoint: async () => {},
    until: async pred => { if (!(await pred())) throw new Error('profession did not change') },
    act: async name => {
      if (name === 'goto') return {}
      if (name === 'entity') return { found: [villager] }
      if (name === 'find_blocks') return { positions: [] }
      if (name === 'trades') return { profession: 'librarian', offers: [book('unbreaking', 1, 12)] }
      if (name === 'dig') { placed = false; return {} }
      if (name === 'place') { placed = true; return {} }
      throw new Error(name)
    },
    cleanupAct: async name => { assert.equal(name, 'place'); cleanedUp = true; placed = true; return {} }
  }
  await assert.rejects(roll.run(api, { want: 'mending', ...cell, tries: 2, pen: false }), /profession did not change/)
  assert.equal(cleanedUp, true)
  assert.equal(placed, true)
})

test('UUID rolling in a crowd accepts only proven locked rivals and never builds a pen', async () => {
  const uuid='12345678-1234-4234-8234-123456789abc', rivalUuid='12345678-1234-4234-8234-123456789abd'
  const cell={x:0,y:64,z:0};let locked=true;const changes=[]
  const row=(id,uuid,profession)=>({id,uuid,baby:false,exact:'1,64,1',metadata:JSON.stringify({16:false,19:{profession}})})
  const api={places:()=>[],zones:()=>[],me:()=> 'Probe',clock:()=>({day:true}),inv:()=>({lectern:2,emerald:64,book:1}),block:()=>({name:'lectern'}),report:()=>{},note:()=>{},checkpoint:async()=>{},until:async f=>assert.ok(await f()),
    act:async(name,args)=>{
      if(name==='goto')return{}
      if(name==='entity')return{found:[row(1,uuid,'librarian'),row(2,rivalUuid,'farmer')]}
      if(name==='find_blocks')return{positions:[]}
      if(name==='trades')return args.uuid===rivalUuid?{profession:'farmer',level:1,offers:[{nbTradeUses:locked?1:0}]}:{profession:'librarian',level:1,offers:[book('mending',1,12)]}
      changes.push(name);throw Error(name)
    }}
  const args={...cell,uuid,pen:false,want:'mending',tries:1}
  assert.equal((await roll.run(api,args)).found,'mending 1')
  assert.deepEqual(changes,[])
  locked=false
  await assert.rejects(roll.run(api,args),/not proven trade-locked/)
  assert.deepEqual(changes,[])
})

test('a fresh traded-target guard prevents removing its station even when offer use counters reset',async()=>{
  const cell={x:0,y:64,z:0};let reads=0,digs=0
  const row={id:1,exact:'1,64,1',metadata:JSON.stringify({16:false,19:{profession:'librarian'}})}
  const api={places:()=>[],zones:()=>[],me:()=> 'Probe',clock:()=>({day:true}),inv:()=>({lectern:2}),block:()=>({name:'lectern'}),report:()=>{},note:()=>{},until:async f=>assert.ok(await f()),
    act:async n=>{if(n==='goto')return{};if(n==='entity')return{found:[row]};if(n==='find_blocks')return{positions:[]};if(n==='trades')return{profession:'librarian',level:++reads>=4?2:1,offers:[book('unbreaking',1,12)]};if(n==='dig'){digs++;throw Error('unsafe dig')}throw Error(n)}}
  await assert.rejects(roll.run(api,{...cell,id:1,pen:false,want:'mending',tries:2}),/trade-locked/)
  assert.equal(digs,0)
})
