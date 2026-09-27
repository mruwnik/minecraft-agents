// The kit a routine day needs, decided from what is carried, what the plot's chest holds and what can be crafted
// (card 6cf481c0): a stone hoe broke mid-routine with no spare, the side craft superseded the routine, and on
// autopilot nobody is there to craft. Pure decisions here; library/kit.mjs acts on them.
import test from 'node:test'
import assert from 'node:assert/strict'
import { kindOf, carriedOfKind, kitPlan, kitLine, toolsLost, toolList, NEVER_EAT } from '../src/inventory/kit.mjs'
import { BANNED_FOOD } from '../src/lib/food.mjs'
import kit from '../library/kit.mjs'
import { fakeApi } from './helpers.mjs'

const isFood = name => ['bread', 'cooked_beef', 'apple', 'carrot', 'rotten_flesh'].includes(name)
const CHEST = '12,63,-80'
const plan = (over = {}) => kitPlan({ tools: ['stone_hoe'], spare: 1, food: 12, carried: { bread: 12 }, chest: {}, chestAt: CHEST, isFood, ...over })

test('kit and auto-eat share the same ordinary-food blacklist', () => {
  assert.equal(NEVER_EAT, BANNED_FOOD)
  assert.deepEqual(NEVER_EAT, ['rotten_flesh', 'spider_eye', 'poisonous_potato', 'pufferfish', 'chicken'])
  assert.equal(Object.isFrozen(NEVER_EAT), true)
})

for (const [name, kind] of [['stone_hoe', 'hoe'], ['iron_axe', 'axe'], ['netherite_pickaxe', 'pickaxe'], ['shears', 'shears'], ['wooden_sword', 'sword'], ['cobblestone', null], ['stick', null], ['golden_carrot', null]]) {
  test(`kindOf: ${name} is ${kind}`, () => assert.equal(kindOf(name), kind))
}

test('carriedOfKind: any tier of the kind counts, nothing else does', () => {
  assert.deepEqual([carriedOfKind({ stone_hoe: 1, iron_hoe: 1, stone_axe: 2, cobblestone: 9 }, 'hoe'), carriedOfKind({ stone_axe: 2 }, 'hoe'), carriedOfKind({ shears: 2 }, 'shears')], [2, 0, 2])
})

// ---------------------------------------------------------------- tools: carried, then the chest, then the crafting grid
for (const [name, over, expected] of [
  ['two hoes carried: nothing to do', { carried: { stone_hoe: 2, bread: 12 } }, { take: {}, craft: [], short: [] }],
  ['a hoe of any tier carried counts, and the chest tops up with any tier', { carried: { iron_hoe: 1, bread: 12 }, chest: { wooden_hoe: 3 } }, { take: { wooden_hoe: 1 }, craft: [], short: [] }],
  ['a spare crafted from what is carried', { carried: { stone_hoe: 1, cobblestone: 8, stick: 4, bread: 12 } }, { take: {}, craft: [{ item: 'stone_hoe', count: 1 }], short: [] }],
  ['the materials come out of the chest', { carried: { bread: 12 }, chest: { cobblestone: 4, stick: 4 } }, { take: { cobblestone: 4, stick: 4 }, craft: [{ item: 'stone_hoe', count: 2 }], short: [] }],
  ['no sticks: planks make them (2 planks make 4 sticks)', { carried: { bread: 12 }, chest: { cobblestone: 4, oak_planks: 5 } }, { take: { cobblestone: 4, oak_planks: 2 }, craft: [{ item: 'stick', count: 4 }, { item: 'stone_hoe', count: 2 }], short: [] }],
  ['no planks: a log makes them (1 log makes 4 planks)', { carried: { bread: 12 }, chest: { cobbled_deepslate: 4, birch_log: 2 } }, { take: { cobbled_deepslate: 4, birch_log: 1 }, craft: [{ item: 'birch_planks', count: 4 }, { item: 'stick', count: 4 }, { item: 'stone_hoe', count: 2 }], short: [] }],
  ['sticks partly carried: only the rest is made', { carried: { stone_hoe: 1, stick: 1, cobblestone: 2, bread: 12 }, chest: { oak_planks: 2 } }, { take: { oak_planks: 2 }, craft: [{ item: 'stick', count: 4 }, { item: 'stone_hoe', count: 1 }], short: [] }],
  ['no stone anywhere: short, and the line says what and where', { carried: { bread: 12 }, chest: { stick: 8 } }, { take: {}, craft: [], short: [`stone_hoe: 0 carried, 2 wanted: no cobblestone (or cobbled_deepslate, blackstone) carried or in the chest at ${CHEST}`] }],
  ['no chest at all: short says so', { carried: { bread: 12 }, chest: null }, { take: {}, craft: [], short: ['stone_hoe: 0 carried, 2 wanted: no cobblestone (or cobbled_deepslate, blackstone) carried, and no chest'] }],
  ['stone for one of two: one is made and the line says one is still short', { carried: { bread: 12, stick: 8 }, chest: { cobblestone: 3 } }, { take: { cobblestone: 2 }, craft: [{ item: 'stone_hoe', count: 1 }], short: [`stone_hoe: 0 carried, 2 wanted, 1 made: no cobblestone (or cobbled_deepslate, blackstone) carried or in the chest at ${CHEST}`] }],
  ['shears are two iron ingots each', { tools: ['shears'], carried: { bread: 12 }, chest: { iron_ingot: 5 } }, { take: { iron_ingot: 4 }, craft: [{ item: 'shears', count: 2 }], short: [] }],
  ['an iron hoe takes ingots and sticks', { tools: ['iron_hoe'], carried: { bread: 12, stick: 4 }, chest: { iron_ingot: 4 } }, { take: { iron_ingot: 4 }, craft: [{ item: 'iron_hoe', count: 2 }], short: [] }],
  ['a wooden axe takes planks as its head', { tools: ['wooden_axe'], spare: 0, carried: { bread: 12 }, chest: { spruce_planks: 6 } }, { take: { spruce_planks: 5 }, craft: [{ item: 'stick', count: 4 }, { item: 'wooden_axe', count: 1 }], short: [] }],
  ['two tools, the second short', { tools: ['stone_hoe', 'stone_axe'], carried: { stone_hoe: 2, bread: 12 }, chest: { cobblestone: 2, stick: 2 } }, { take: {}, craft: [], short: [`stone_axe: 0 carried, 2 wanted: no cobblestone (or cobbled_deepslate, blackstone) carried or in the chest at ${CHEST}`] }],
  ['a tool no recipe is known for is short with a word to craft it by hand', { tools: ['fishing_rod'], carried: { bread: 12 } }, { take: {}, craft: [], short: ['fishing_rod: 0 carried, 2 wanted: no recipe known here, craft it by hand'] }],
  ['spare=0 wants one', { spare: 0, carried: { stone_hoe: 1, bread: 12 } }, { take: {}, craft: [], short: [] }]
]) {
  test(`kitPlan tools: ${name}`, () => {
    const { take, craft, short } = plan(over)
    assert.deepEqual({ take, craft, short }, expected)
  })
}

// ---------------------------------------------------------------- food: carried, then the chest, then bread from wheat
for (const [name, over, expected] of [
  ['enough carried', { carried: { bread: 12 } }, { take: {}, craft: [], short: [] }],
  ['topped up from the chest, the biggest stack first, never the never-eat list', { carried: { bread: 4 }, chest: { rotten_flesh: 20, apple: 3, cooked_beef: 10 } }, { take: { cooked_beef: 8 }, craft: [], short: [] }],
  ['two stacks when one is not enough', { carried: {}, chest: { apple: 3, carrot: 5, wheat: 1 } }, { take: { carrot: 5, apple: 3 }, craft: [], short: ['food: 0 carried, 12 wanted, 8 taken: the chest at 12,63,-80 has no more'] }],
  ['wheat becomes bread (3 wheat a loaf)', { carried: { bread: 4 }, chest: { wheat: 30 } }, { take: { wheat: 24 }, craft: [{ item: 'bread', count: 8 }], short: [] }],
  ['carried wheat is baked before the chest is opened', { carried: { bread: 10, wheat: 7 }, chest: {} }, { take: {}, craft: [{ item: 'bread', count: 2 }], short: [] }],
  ['nothing anywhere', { carried: {}, chest: {} }, { take: {}, craft: [], short: ['food: 0 carried, 12 wanted: the chest at 12,63,-80 has none'] }],
  ['no chest', { carried: { bread: 2 }, chest: null }, { take: {}, craft: [], short: ['food: 2 carried, 12 wanted, and no chest'] }],
  ['food=0 asks for nothing', { food: 0, carried: {} }, { take: {}, craft: [], short: [] }]
]) {
  test(`kitPlan food: ${name}`, () => {
    const { take, craft, short } = plan({ tools: [], ...over })
    assert.deepEqual({ take, craft, short }, expected)
  })
}

test('kitPlan: tools and food are taken in one list', () => {
  const { take, craft } = plan({ carried: { stone_hoe: 1, bread: 2 }, chest: { stone_hoe: 1, bread: 20 } })
  assert.deepEqual([take, craft], [{ stone_hoe: 1, bread: 10 }, []])
})

// ---------------------------------------------------------------- the line the day reads
for (const [name, tools, items, expected] of [
  ['each kind by count, and the food', ['stone_hoe'], { stone_hoe: 1, iron_hoe: 1, bread: 7, apple: 2 }, 'hoe:2 food:9'],
  ['a kind with none says 0', ['stone_hoe', 'shears'], { shears: 2, bread: 12 }, 'hoe:0 shears:2 food:12'],
  ['no tools asked for: food alone', [], { bread: 3 }, 'food:3']
]) {
  test(`kitLine: ${name}`, () => assert.equal(kitLine(tools, items, isFood), expected))
}

// a step that ends with fewer of a kind than it began with lost a tool to wear: that is what the routine re-kits for
for (const [name, before, after, tools, expected] of [
  ['the hoe broke', { stone_hoe: 1, bread: 3 }, { bread: 3 }, ['stone_hoe'], ['hoe']],
  ['a spare took over: still one fewer', { stone_hoe: 2 }, { stone_hoe: 1 }, ['stone_hoe'], ['hoe']],
  ['nothing lost', { stone_hoe: 1 }, { stone_hoe: 1, wheat: 20 }, ['stone_hoe'], []],
  ['only the kinds the kit lists count', { stone_axe: 1, shears: 1 }, {}, ['shears'], ['shears']]
]) {
  test(`toolsLost: ${name}`, () => assert.deepEqual(toolsLost(before, after, tools), expected))
}

for (const [name, given, expected] of [['a comma list', 'stone_hoe, shears', ['stone_hoe', 'shears']], ['a list already', ['shears'], ['shears']], ['nothing', undefined, []]]) {
  test(`toolList: ${name}`, () => assert.deepEqual(toolList(given), expected))
}

// ---------------------------------------------------------------- the composite, against a fake body and chest
const FIELD = { name: 'field', by: 'Tester', kind: 'farm', plan: 'C', cells: [{ ch: 'C', x: 12, y: 62, z: -80 }] }
const OTHERS = { name: 'theirs', by: 'Somebody', kind: 'farm', note: 'keep out' }
// withdraw and craft change what is carried, as the real ones do, so the kit line is read off the result
const kitApi = (items, chest) => {
  const made = fakeApi({
    places: [FIELD, OTHERS], place: FIELD, items,
    answers: {
      chest_contents: { items: chest },
      withdraw: ({ items: wanted }) => { for (const [n, c] of Object.entries(wanted)) { items[n] = (items[n] ?? 0) + c; chest[n] -= c } return {} },
      craft: ({ item, count }) => { items[item] = (items[item] ?? 0) + count; return {} }
    }
  })
  return made
}

test('kit: a hoe short by one is made from the chest, and the line is read off what is carried after', async () => {
  const { api, calls } = kitApi({ stone_hoe: 1, bread: 12 }, { cobblestone: 8, stick: 8 })
  const out = await kit.run(api, { tools: 'stone_hoe', place: 'field' })
  assert.deepEqual([out, calls], [
    { kit: 'hoe:2 food:12', took: 'cobblestone:2 stick:2', crafted: 'stone_hoe:1', chest: '12,63,-80' },
    ['chest_contents 12,63,-80', 'withdraw items(cobblestone:2 stick:2) x=12 y=63 z=-80', 'craft item=stone_hoe count=1']
  ])
})

test('kit: rations come out of the chest too, and what is short is said', async () => {
  const { api } = kitApi({ stone_hoe: 2, bread: 3 }, { bread: 4 })
  const out = await kit.run(api, { tools: ['stone_hoe'], place: 'field', food: 12 })
  assert.deepEqual([out.kit, out.kit_short], ['hoe:2 food:7', 'food: 3 carried, 12 wanted, 4 taken: the chest at 12,63,-80 has no more'])
})

test('kit: no place and no chest= is the pockets and the grid alone', async () => {
  const { api, calls } = kitApi({ cobblestone: 4, stick: 4, bread: 12 }, {})
  const out = await kit.run(api, { tools: 'stone_hoe' })
  assert.deepEqual([out.kit, out.kit_short, out.chest, calls], ['hoe:2 food:12', undefined, undefined, ['craft item=stone_hoe count=2']])
})

test('kit: chest=x,y,z names the chest without a plan', async () => {
  const { api, calls } = kitApi({ bread: 12 }, { stone_hoe: 2 })
  const out = await kit.run(api, { tools: 'stone_hoe', chest: '3,64,5' })
  assert.deepEqual([out.kit, calls[0]], ['hoe:2 food:12', 'chest_contents 3,64,5'])
})

test('kit: somebody else’s place is refused before its chest is opened', async () => {
  const { api, calls } = kitApi({}, {})
  await assert.rejects(kit.run(api, { tools: 'stone_hoe', place: 'theirs' }), /Somebody/)
  assert.deepEqual(calls, [])
})

test('kit: a withdraw that fails is said, and the line still tells the truth', async () => {
  const made = kitApi({ bread: 12 }, { stone_hoe: 2 })
  made.api.act = async (name, args) => { made.calls.push(name); if (name === 'withdraw') throw new Error('the chest is empty now'); return { items: { stone_hoe: 2 } } }
  const out = await kit.run(made.api, { tools: 'stone_hoe', place: 'field' })
  assert.deepEqual([out.kit, out.kit_short], ['hoe:0 food:12', 'withdraw: the chest is empty now'])
})
