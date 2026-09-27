import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { provisionBoneMeal } from '../src/farm/bone-meal.mjs'
import { planCells, parsePlan } from '../src/lib/plan.mjs'
import { seedDrop } from '../src/lib/farm.mjs'
import maintain from '../library/farm/maintain.mjs'
import { fakeApi } from './helpers.mjs'

const at = { x: 4, y: 64, z: 0 }
const key = p => `${p.x},${p.y},${p.z}`
const make = ({ world = {}, items = {}, bins = {}, directions = {}, drops = [], answers = {} } = {}) => {
  const made = fakeApi({ world, items, drops, answers: {
    chest_contents: p => ({ items: bins[key(p)] ?? {} }),
    withdraw: p => { items.bone_meal = (items.bone_meal ?? 0) + p.items.bone_meal; bins[key(p)].bone_meal -= p.items.bone_meal },
    ...answers
  } })
  const read = made.api.block
  made.api.block = (x, y, z) => {
    const b = read(x, y, z)
    if (b && directions[`${x},${y},${z}`]) b.properties.facing = directions[`${x},${y},${z}`]
    return b
  }
  return { ...made, world, items }
}

test('ready plain composter yields bone meal without seed feeding; only its bone meal drops are approached', async () => {
  const world = { [key(at)]: 'composter#8' }
  const items = { wheat_seeds: 20 }
  const drops = [{ id: 1, item: 'wheat', x: 5, y: 64, z: 0 }, { id: 2, item: 'bone_meal', x: 30, y: 64, z: 0 }]
  const made = make({ world, items, drops, answers: {
    use: p => { assert.equal(p.empty_hand, true); world[key(at)] = 'composter#0'; drops.push({ id: 3, item: 'bone_meal', x: 4, y: 65, z: 0 }) },
    goto: p => { assert.equal(key(p), '4,65,0'); items.bone_meal = 1; drops.pop() }
  } })
  assert.deepEqual(await provisionBoneMeal(made.api, at, 1), { got: 1, problems: [] })
  assert.equal(items.wheat_seeds, 20)
  assert.deepEqual(made.calls, ['use x=4 y=64 z=0 empty_hand', 'goto x=4 y=65 z=0 range=1'])
})

test('already-ejected meal beside an empty composter is collected without activating it', async () => {
  const items = {}
  const made = make({ world: { [key(at)]: 'composter#0' }, items, drops: [{ id: 1, item: 'bone_meal', x: 4, y: 65, z: 0 }], answers: { goto: () => { items.bone_meal = 1 } } })
  assert.equal((await provisionBoneMeal(made.api, at, 1)).got, 1)
  assert.ok(!made.calls.some(c => c.startsWith('use ')))
})

test('a configured input chest traces its hopper, composter, output hopper and facing chest only', async () => {
  const world = { '4,66,0': 'chest', '4,65,0': 'hopper', '4,64,0': 'composter#0', '4,63,0': 'hopper', '5,63,0': 'chest', '3,63,0': 'chest' }
  const items = { bone_meal: 1 }
  const made = make({ world, items, directions: { '4,65,0': 'down', '4,63,0': 'east' }, bins: { '4,63,0': { bone_meal: 1 }, '5,63,0': { bone_meal: 64 }, '3,63,0': { bone_meal: 64 } } })
  assert.equal((await provisionBoneMeal(made.api, { x: 4, y: 66, z: 0 }, 4)).got, 3)
  assert.deepEqual(made.calls.filter(c => c.startsWith('withdraw ')), ['withdraw x=4 y=63 z=0 items(bone_meal)', 'withdraw x=5 y=63 z=0 items(bone_meal:2)'])
  assert.ok(!made.calls.some(c => c.includes('x=3')))
  assert.ok(!made.calls.some(c => /^(use|equip|find_blocks|collect) /.test(c)))
  assert.equal(seedDrop({ name: 'hopper' }), 'deposit', 'configured input hopper accepts end-of-pass compost surplus')
})

test('a named remote composter is approached once to load its output connection', async () => {
  const world = {}, items = {}
  const made = make({ world, items, bins: { '4,63,0': { bone_meal: 2 } }, answers: { goto: () => { world[key(at)] = 'composter#0'; world['4,63,0'] = 'hopper' } } })
  assert.equal((await provisionBoneMeal(made.api, at, 1)).got, 1)
  assert.equal(made.calls[0], 'goto x=4 y=64 z=0 range=3')
})

for (const [target, wanted, items] of [[null, 2, {}], [at, 0, {}], [at, 2, { bone_meal: 2 }]]) {
  test(`unneeded or disabled supply performs no actions (${target === null ? 'disabled' : wanted})`, async () => {
    const made = make({ items })
    assert.equal((await provisionBoneMeal(made.api, target, wanted)).got, 0)
    assert.deepEqual(made.calls, [])
  })
}

test('claimed successful withdrawals do not count as fetched without inventory gain', async () => {
  const made = make({ world: { [key(at)]: 'chest' }, bins: { [key(at)]: { bone_meal: 5 } }, answers: { withdraw: { took: 5 } } })
  assert.equal((await provisionBoneMeal(made.api, at, 3)).got, 0)
})

for (const error of [new Error('cancelled'), new TypeError('unknown container schema'), new Error('protected zone neighbour')]) {
  test(`supply preserves hard error: ${error.message}`, async () => {
    const made = make({ world: { [key(at)]: 'hopper' }, answers: { chest_contents: error } })
    await assert.rejects(provisionBoneMeal(made.api, at, 3), e => e === error)
    assert.equal(made.calls.length, 1)
  })
}

const maintenance = ({ bone_meal = true, compost, mature = false, carried = 0, unavailable = false } = {}) => {
  const field = { name: 'field', kind: 'farm', plan: 'wK', x: 0, y: 63, z: 0 }
  field.parsed = parsePlan(field.plan); field.cells = planCells(field)
  const world = { '0,63,0': 'farmland', '0,64,0': `wheat#${mature ? 7 : 0}`, '1,64,0': 'composter#0', '1,63,0': 'hopper' }
  const items = { stone_hoe: 2, bone_meal: carried }
  const made = make({ world, items, bins: { '1,63,0': { bone_meal: 64 } }, answers: {
    'farm.harvest': { harvested: {}, replanted: 0 },
    ...(unavailable ? { chest_contents: new Error('too far to reach') } : {}),
    fertilize: () => { items.bone_meal--; world['0,64,0'] = 'wheat#7' }
  } })
  made.api.plan = () => field
  return { ...made, run: () => maintain.run(made.api, { place: field.name, bone_meal, ...(compost !== undefined ? { compost } : {}) }) }
}

test('maintain provisions only needed meal from planned K before fertilizing and harvesting', async () => {
  const made = maintenance()
  const result = await made.run()
  assert.equal(result.bone_meal_fetched, 1)
  assert.equal(result.bone_meal_used, 1)
  const withdrawal = made.calls.indexOf('withdraw x=1 y=63 z=0 items(bone_meal)')
  assert.ok(withdrawal >= 0 && withdrawal < made.calls.indexOf('fertilize 0,64,0'))
  assert.ok(made.calls.indexOf('fertilize 0,64,0') < made.calls.findIndex(c => c.startsWith('farm.harvest ')))
})

for (const opts of [{ bone_meal: false }, { mature: true }, { carried: 1 }, { compost: false }]) {
  test(`maintain does not source unnecessary or disabled meal ${JSON.stringify(opts)}`, async () => {
    const made = maintenance(opts)
    await made.run()
    assert.ok(!made.calls.some(c => /^(chest_contents|withdraw|use) /.test(c)))
  })
}

test('inaccessible configured output produces attention while independent harvest continues', async () => {
  const made = maintenance({ unavailable: true })
  const result = await made.run()
  assert.match(result.bone_meal_attention, /too far to reach.*short of bone meal/)
  assert.ok(made.calls.some(c => c.startsWith('farm.harvest ')))
  assert.ok(made.events.some(e => e.type === 'farm_attention' && e.reasons.bone_meal_attention))
})

// Execute the actual use primitive with a fake body; the empty hand guarantee
// must hold after movement and cancellation, not merely in the caller's args.
const source = fs.readFileSync(new URL('../src/bot.mjs', import.meta.url), 'utf8')
const body = source.slice(source.indexOf('  async use (a) {') + '  async use (a) {'.length, source.indexOf('\n  async toggle')).replace(/},\s*$/, '')
const useWith = (bot, walk, alive) => new Function('bot', 'goNear', 'cancelGuard', 'vecOf', 'compact', 'findItem', `return async a => {${body}}`)(bot, walk, () => alive, x => x, () => '', () => ({}))
for (const cancelled of [false, true]) {
  test(`empty-handed use cannot feed held seed after walking${cancelled ? ' or cancellation' : ''}`, async () => {
    let cancel = false
    const calls = []
    const bot = { heldItem: { name: 'wheat_seeds' }, blockAt: () => ({ name: 'composter', getProperties: () => ({ level: 0 }) }),
      unequip: async () => { calls.push('unequip'); bot.heldItem = null },
      activateBlock: async () => { assert.equal(bot.heldItem, null); calls.push('use') }, waitForTicks: async () => {} }
    const use = useWith(bot, async () => { calls.push('walk'); cancel = cancelled }, () => { if (cancel) throw new Error('cancelled') })
    if (cancelled) await assert.rejects(use({ ...at, empty_hand: true }), /cancelled/)
    else await use({ ...at, empty_hand: true })
    assert.deepEqual(calls, cancelled ? ['walk'] : ['walk', 'unequip', 'use'])
  })
}

for (const fail of ['unequip fails', 'cancel after unequip']) {
  test(`empty-handed use never activates when ${fail}`, async () => {
    let cancelled = false, activated = false
    const bot = { blockAt: () => ({ name: 'composter' }), unequip: async () => {
      if (fail === 'unequip fails') throw new Error('inventory full')
      cancelled = true
    }, activateBlock: async () => { activated = true } }
    const use = useWith(bot, async () => {}, () => { if (cancelled) throw new Error('cancelled') })
    await assert.rejects(use({ ...at, empty_hand: true }), /inventory full|cancelled/)
    assert.equal(activated, false)
  })
}

test('a long connected hopper line stops at the bounded limit without searching neighbouring containers', async () => {
  const world = {}, directions = {}
  for (let x = 0; x < 14; x++) { world[`${x},64,0`] = 'hopper'; directions[`${x},64,0`] = 'east' }
  const made = make({ world, directions })
  const result = await provisionBoneMeal(made.api, { x: 0, y: 64, z: 0 }, 1)
  assert.equal(made.calls.length, 12)
  assert.match(result.problems[0], /exceeded 12 blocks/)
})

test('explicit compost target overrides a stocked planned K output, even when the chosen source is empty', async () => {
  const made = maintenance({ compost: '4,64,0' })
  made.world['4,64,0'] = 'chest'
  const result = await made.run()
  assert.equal(result.bone_meal_fetched, 0)
  assert.match(result.bone_meal_attention, /short of bone meal/)
  assert.ok(made.calls.includes('chest_contents 4,64,0'))
  assert.ok(!made.calls.some(c => c.startsWith('chest_contents 1,')))
})
