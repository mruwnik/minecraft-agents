import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { fakeApi } from './helpers.mjs'
import apiaryInspect from '../library/apiary/inspect.mjs'
import apiaryBreed from '../library/apiary/breed.mjs'
import apiaryHarvest from '../library/apiary/harvest.mjs'
import apiaryMaintain from '../library/apiary/maintain.mjs'
import apiaryGuard from '../library/apiary/guard.mjs'
import { hiveState, fireState, carpetCarried, apiaryCensus } from '../library/apiary/shared/hive.mjs'
import { BEE_FLOWERS, BREEDING_FOOD, CREATURE_FOOD, creatureFood, apiaryGoods, routineSteps } from '../src/lib.mjs'

// ---------------------------------------------------------------- bees are livestock, but never a ground flock
test('bee food is available to the feed primitive without making bees a flock animal', () => {
  assert.equal(BREEDING_FOOD.bee, undefined)
  assert.deepEqual([CREATURE_FOOD.bee, creatureFood('bee', ['bread', 'poppy'])], [BEE_FLOWERS, 'poppy'])
})

const blockAt = world => (x, y, z) => world[`${x},${y},${z}`] ?? null
const block = (name, properties = {}, solid = true) => ({ name, properties, solid })
const air = () => block('air', {}, false)
// the standard column: fire one block underground at y, ground on all four sides of it, a carpet on it, one air block,
// then the hive (Dan, 09-23: open fires burn the bees; the fire goes at least one block underground)
const ground = (x, y, z) => Object.fromEntries([[x - 1, z], [x + 1, z], [x, z - 1], [x, z + 1]].map(([gx, gz]) => [`${gx},${y},${gz}`, block('dirt')]))
const column = (extra = {}) => ({
  '10,65,10': block('beehive', { honey_level: 5, facing: 'south' }),
  '10,65,11': air(),
  '10,64,10': air(),
  '10,63,10': block('white_carpet'),
  '10,62,10': block('campfire', { lit: true }),
  ...ground(10, 62, 10),
  ...ground(10, 61, 10),
  ...extra
})
const hive = world => hiveState({ x: 10, y: 65, z: 10, block: world['10,65,10'], blockAt: blockAt(world) })

test('hiveState: the standard column is smoked, guarded and safe', () => {
  assert.deepEqual(hive(column()), {
    x: 10, y: 65, z: 10, name: 'beehive', honey: 5, ripe: true, facing: 'south', entranceClear: true,
    smoked: true, guarded: true, open: false, raised: false, campfire: { x: 10, y: 62, z: 10 }
  })
})

test('hiveState: a lit fire with a side in the open is raised, even under a carpet', () => {
  assert.equal(hive(column({ '9,62,10': air() })).raised, true)
})

// a carpet counts as a block with a collision box to the client, which is not the same thing as stopping the smoke:
// the game lets smoke through any block that sits DIRECTLY on the fire, and stops it at one with a gap beneath.
// guarded and open describe the fire that smokes the hive; a hive with no smoke has neither
for (const [name, world, smoked, guarded] of [
  ['a fire straight under the hive smokes it and the hive itself covers it (a wild nest on its fire)', column({ '10,63,10': air(), '10,62,10': air(), '10,64,10': block('campfire', { lit: true }) }), true, true],
  ['a fire with a gap under the hive and nothing on it is open', column({ '10,63,10': air() }), true, false],
  ['a carpet with a gap beneath it stops the smoke, and the fire under the gap is bare', column({ '10,63,10': air(), '10,64,10': block('white_carpet') }), false, false],
  ['a full block directly on the fire lets the smoke through and covers the fire', column({ '10,63,10': block('stone') }), true, true],
  ['a full block with a gap beneath it stops the smoke', column({ '10,64,10': block('stone') }), false, false],
  ['a soul campfire is a fire', column({ '10,62,10': block('soul_campfire', { lit: true }) }), true, true],
  ['an unlit campfire is no smoke at all', column({ '10,62,10': block('campfire', { lit: false }) }), false, false],
  ['smoke reaches five cells, not six', { ...column({ '10,63,10': air(), '10,62,10': air() }), '10,60,10': block('campfire', { lit: true }), '10,61,10': block('white_carpet') }, true, true]
]) {
  test(`hiveState: ${name}`, () => {
    const seen = hive(world)
    assert.deepEqual([seen.smoked, seen.guarded, seen.open], [smoked, guarded, smoked && !guarded])
  })
}

test('hiveState: a blocked entrance is named', () => {
  const seen = hive(column({ '10,65,11': block('oak_log') }))
  assert.deepEqual([seen.entranceClear, seen.smoked], [false, true])
})

test('hiveState: an unseen entrance is not assumed safe', () => {
  const seen = hiveState({ x: 10, y: 65, z: 10, block: block('beehive', { honey_level: 5, facing: 'south' }), blockAt: () => null })
  assert.equal(seen.entranceClear, false)
})

test('hiveState: something that is not a hive is nothing', () => {
  assert.equal(hiveState({ x: 1, y: 2, z: 3, block: block('stone'), blockAt: () => null }), null)
})

for (const [name, world, expected] of [
  ['a lit fire with a carpet on it is guarded', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('red_carpet') }, { lit: true, guarded: true, open: false, sunk: false }],
  ['a lit fire with nothing on it is open', { '5,60,5': block('campfire', { lit: true }), '5,61,5': air() }, { lit: true, guarded: false, open: true, sunk: false }],
  ['an unseen cell over the fire is not a guard', { '5,60,5': block('campfire', { lit: true }) }, { lit: true, guarded: false, open: true, sunk: false }],
  ['a hive sitting straight on the fire covers it: no cell to carpet, nothing to land in', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('bee_nest', { honey_level: 0, facing: 'north' }) }, { lit: true, guarded: true, open: false, sunk: false }],
  ['a moss carpet is a plant and no guard', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('moss_carpet') }, { lit: true, guarded: false, open: true, sunk: false }],
  ['an unlit fire harms nobody', { '5,60,5': block('campfire', { lit: false }), '5,61,5': air() }, { lit: false, guarded: false, open: false, sunk: false }],
  ['ground on all four sides means the fire is underground', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('red_carpet'), ...ground(5, 60, 5) }, { lit: true, guarded: true, open: false, sunk: true }],
  ['one open side and it is not', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('red_carpet'), ...ground(5, 60, 5), '4,60,5': air() }, { lit: true, guarded: true, open: false, sunk: false }],
  ['water beside it is not ground', { '5,60,5': block('campfire', { lit: true }), '5,61,5': block('red_carpet'), ...ground(5, 60, 5), '4,60,5': block('water', {}, false) }, { lit: true, guarded: true, open: false, sunk: false }]
]) {
  test(`fireState: ${name}`, () => assert.deepEqual(fireState({ x: 5, y: 60, z: 5, block: world['5,60,5'], blockAt: blockAt(world) }), { x: 5, y: 60, z: 5, ...expected }))
}

test('carpetCarried: any carpet colour will do, but moss is a plant', () => {
  assert.deepEqual([carpetCarried({ shears: 1, red_carpet: 2 }), carpetCarried({ moss_carpet: 3 }), carpetCarried({})], ['red_carpet', null, null])
})

// ---------------------------------------------------------------- the composites, driven against a fake body
const APIARY = { name: 'orchard-apiary', kind: 'apiary', x: 10, y: 64, z: 10 }
const apiaryWorld = () => column({ '12,64,10': block('dandelion', {}, false) })
const fires = world => Object.entries(world).filter(([, b]) => b.name.endsWith('campfire')).map(([k]) => { const [x, y, z] = k.split(',').map(Number); return { x, y, z } })
const apiaryAnswers = world => ({
  find_blocks: ({ block: name }) => ({ positions: name === 'beehive' ? [{ x: 10, y: 65, z: 10 }] : name === 'campfire' ? fires(world) : [] }),
  animals: { found: [{ mob: 'bee', id: 21, grown: true, dist: 3 }, { mob: 'bee', id: 22, grown: true, dist: 4 }] },
  use: () => { world['10,65,10'].properties.honey_level = 0; return {} },
  place: ({ x, y, z, item }) => { world[`${x},${y},${z}`] = block(item); return { placed: 1 } },
  dig: ({ x, y, z }) => { world[`${x},${y},${z}`] = air(); return { dug: 1 } },
  collect: { picked: 3 },
  feed: { fed: 1, with: 'dandelion' }
})
const makeApiary = (items = { shears: 1, dandelion: 4 }, world = apiaryWorld()) => {
  const made = fakeApi({ places: [APIARY], items, answers: apiaryAnswers(world) })
  made.api.block = blockAt(world)
  return { ...made, world }
}
const openWorld = () => ({ ...apiaryWorld(), '10,63,10': air() })
const raisedWorld = () => ({ ...apiaryWorld(), '9,62,10': air() })

test('apiary.inspect: a fire with a side in the open is counted as raised and named in the details', async () => {
  const { api } = makeApiary(undefined, raisedWorld())
  const out = await apiaryInspect.run(api, { place: 'orchard-apiary', range: 4 })
  assert.deepEqual([out.raisedFires, out.openFires, out.details], [1, 0, 'beehive@10,65,10:honey=5,RAISED-FIRE'])
})

test('apiary.inspect: reports honey, smoke, open fires, flowers and only visible bees', async () => {
  const { api } = makeApiary()
  const out = await apiaryInspect.run(api, { place: 'orchard-apiary', range: 4 })
  assert.deepEqual([out.hives, out.ripe, out.noSmoke, out.blocked, out.openFires, out.beesVisible, out.grownVisible, out.flowers], [1, 1, 0, 0, 0, 2, 2, 1])
})

test('apiary.inspect: an open fire is counted and named in the details', async () => {
  const { api } = makeApiary(undefined, openWorld())
  // fires that smoke nothing count too: bees fly through them all the same
  const out = await apiaryInspect.run(api, { place: 'orchard-apiary', range: 4 })
  assert.deepEqual([out.openFires, out.details], [1, 'beehive@10,65,10:honey=5,OPEN-FIRE'])
})

// item 18 (Mariel, BUGS.md 09-23 01:12Z): inspect answered `hives=4 ripe=1 unsafe=1 blocked=0`, Mariel looked at the
// ripe hive, found no NO-SMOKE tag on it and concluded the count was wrong. It was not: the unsmoked hive was one of
// the other three, and nothing in the line said so. A count you cannot check against the line below it is a count you
// cannot act on. So every count is made from the same per-hive facts `hiveLine` prints, every one that is not zero
// says WHERE in the same coordinates, and the one that means "no campfire under it" is called what the tag is called.
const hiveFacts = (x, over) => ({ x, y: 65, z: 10, ripe: over.ripe ?? false, smoked: over.smoked ?? true, entranceClear: over.entranceClear ?? true })
for (const [name, hives, fires, expected] of [
  ['nothing at all', [], [], { hives: 0, ripe: 0, noSmoke: 0, blocked: 0, openFires: 0, raisedFires: 0 }],
  ['one good hive says nothing but zeroes', [hiveFacts(10, {})], [],
    { hives: 1, ripe: 0, noSmoke: 0, blocked: 0, openFires: 0, raisedFires: 0 }],
  ['the unsmoked hive is named, and it is not the ripe one (Mariel\'s four)',
    [hiveFacts(10, { ripe: true }), hiveFacts(12, { smoked: false }), hiveFacts(14, {}), hiveFacts(16, {})], [],
    { hives: 4, ripe: 1, ripeAt: '10,65,10', noSmoke: 1, noSmokeAt: '12,65,10', blocked: 0, openFires: 0, raisedFires: 0 }],
  ['two of a kind are both named, in the order the hives came',
    [hiveFacts(10, { blocked: true, entranceClear: false }), hiveFacts(12, { entranceClear: false })], [],
    { hives: 2, ripe: 0, noSmoke: 0, blocked: 2, blockedAt: '10,65,10 12,65,10', openFires: 0, raisedFires: 0 }],
  ['a fire no hive sits over is counted and named, so it cannot be mistaken for a hive flag',
    [hiveFacts(10, {})], [{ x: 30, y: 62, z: 10, lit: true, open: true, sunk: true }],
    { hives: 1, ripe: 0, noSmoke: 0, blocked: 0, openFires: 1, openFiresAt: '30,62,10', raisedFires: 0 }],
  ['a lit fire with a side in the open is raised whether or not it is covered',
    [], [{ x: 30, y: 62, z: 10, lit: true, open: false, sunk: false }],
    { hives: 0, ripe: 0, noSmoke: 0, blocked: 0, openFires: 0, raisedFires: 1, raisedFiresAt: '30,62,10' }]
]) {
  test(`apiaryCensus: ${name}`, () => assert.deepEqual(apiaryCensus(hives, fires), expected))
}

test('apiary.inspect: the count and the details name the same hive', async () => {
  const { api } = makeApiary(undefined, { ...apiaryWorld(), '10,62,10': air() })
  const out = await apiaryInspect.run(api, { place: 'orchard-apiary', range: 4 })
  assert.deepEqual([out.noSmoke, out.noSmokeAt, out.unsafe, out.details],
    [1, '10,65,10', undefined, 'beehive@10,65,10:honey=5,NO-SMOKE'])
})

test('apiary.harvest: uses shears only through verified smoke, checks the level and collects comb', async () => {
  const { api, calls } = makeApiary()
  const out = await apiaryHarvest.run(api, { place: 'orchard-apiary', mode: 'comb', range: 4 })
  assert.deepEqual([out.harvested, calls.filter(c => c.startsWith('use') || c.startsWith('collect'))],
    [1, ['use x=10 y=65 z=10 item=shears', 'collect']])
})

test('apiary.harvest: an unsmoked ripe hive is refused before it is touched', async () => {
  const made = makeApiary(undefined, { ...apiaryWorld(), '10,62,10': air() })
  await assert.rejects(apiaryHarvest.run(made.api, { place: 'orchard-apiary', mode: 'comb', range: 4 }), /no lit campfire/)
  assert.deepEqual(made.calls.filter(c => c.startsWith('use')), [])
})

test('apiary.harvest: a hive over an open fire is refused until the fire is carpeted', async () => {
  const made = makeApiary(undefined, openWorld())
  await assert.rejects(apiaryHarvest.run(made.api, { place: 'orchard-apiary', mode: 'comb', range: 4 }), /open fire at 10,62,10: put a carpet on it/)
  assert.deepEqual(made.calls.filter(c => c.startsWith('use')), [])
})

test('apiary.guard: carpets every open lit fire it can, with any carpet carried', async () => {
  const world = { ...openWorld(), '14,62,10': block('campfire', { lit: true }), '14,63,10': block('blue_carpet'), '16,62,10': block('campfire', { lit: false }), '16,63,10': air() }
  const { api, calls } = makeApiary({ red_carpet: 2 }, world)
  const out = await apiaryGuard.run(api, { place: 'orchard-apiary', range: 8 })
  assert.deepEqual([out, calls.filter(c => c.startsWith('place'))], [
    { fires: 2, raised: 1, sunk: 0, open: 1, carpeted: 1, left: 0, with: 'red_carpet', craft: 'a campfire (3 sticks, 1 coal or charcoal, 3 logs) to sink 1 raised fire' },
    ['place x=10 y=63 z=10 item=red_carpet']
  ])
})

test('apiary.guard: a raised fire is moved one block underground when a campfire is carried, then carpeted', async () => {
  const world = raisedWorld()
  const { api, calls } = makeApiary({ campfire: 1, red_carpet: 2 }, world)
  const out = await apiaryGuard.run(api, { place: 'orchard-apiary', range: 8 })
  assert.deepEqual([out, calls.filter(c => c.startsWith('place') || c.startsWith('dig'))], [
    { fires: 1, raised: 1, sunk: 1, open: 1, carpeted: 1, left: 0, with: 'red_carpet' },
    ['dig 10,63,10', 'dig 10,62,10', 'dig 10,61,10', 'place x=10 y=61 z=10 item=campfire', 'place x=10 y=62 z=10 item=red_carpet']
  ])
})

test('apiary.guard: a raised fire with no campfire carried is carpeted where it is and reported', async () => {
  const world = { ...raisedWorld(), '10,63,10': air() }
  const { api, calls } = makeApiary({ red_carpet: 2 }, world)
  const out = await apiaryGuard.run(api, { place: 'orchard-apiary', range: 8 })
  assert.deepEqual([out.sunk, out.raised, out.carpeted, calls.filter(c => c.startsWith('dig'))], [0, 1, 1, []])
})

test('apiary.guard: nothing to do needs no carpet', async () => {
  const { api, calls } = makeApiary({ shears: 1 })
  const out = await apiaryGuard.run(api, { place: 'orchard-apiary', range: 8 })
  assert.deepEqual([out, calls.filter(c => c.startsWith('place'))], [{ fires: 1, raised: 0, sunk: 0, open: 0, carpeted: 0, left: 0 }, []])
})

test('apiary.guard: an open fire and no carpet is a refusal that says what to bring', async () => {
  const { api } = makeApiary({ shears: 1 }, openWorld())
  await assert.rejects(apiaryGuard.run(api, { place: 'orchard-apiary', range: 8 }), /1 open fire.*carpet/)
})

test('apiary.guard: a carpet that does not take is left, not counted', async () => {
  const world = openWorld()
  const made = makeApiary({ red_carpet: 2 }, world)
  made.api.act = async (name, args = {}) => { made.calls.push(name); return name === 'find_blocks' ? apiaryAnswers(world).find_blocks(args) : {} }
  const out = await apiaryGuard.run(made.api, { place: 'orchard-apiary', range: 8 })
  assert.deepEqual([out.carpeted, out.left], [0, 1])
})

test('apiary.breed: feeds two visible grown bees flowers without invoking flock tools', async () => {
  const { api, calls } = makeApiary()
  const out = await apiaryBreed.run(api, { place: 'orchard-apiary', range: 4 })
  assert.deepEqual([out.fed, calls.filter(c => c.startsWith('feed')), calls.some(c => c.startsWith('flock.'))],
    [2, ['feed mob=bee id=21', 'feed mob=bee id=22'], false])
})

const maintainApi = (inspect, items = { shears: 1, dandelion: 4 }) => fakeApi({
  items, places: [APIARY],
  answers: {
    'apiary.inspect': { hives: 1, ripe: 1, ripeAt: '10,65,10', noSmoke: 0, blocked: 0, openFires: 0, raisedFires: 0, beesVisible: 2, grownVisible: 2, flowers: 8, details: 'beehive@10,65,10:honey=5', ...inspect },
    'apiary.guard': { fires: 1, raised: 1, sunk: 1, open: 1, carpeted: 1, left: 0 },
    'apiary.harvest': { harvested: 1, unsafe: 0, blocked: 0 },
    'apiary.breed': { fed: 2 }
  }
})

test('apiary.maintain: the round says what the inspect said, coordinates and all, and never the details line', async () => {
  const { api } = maintainApi({})
  const out = await apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 })
  assert.deepEqual([out.ripe, out.ripeAt, out.noSmoke, out.details, out.grownVisible], [1, '10,65,10', 0, undefined, undefined])
})

test('apiary.maintain: fires the guard moved take the guard\'s counts, and drop the census coordinates for them', async () => {
  const { api } = maintainApi({ openFires: 1, openFiresAt: '10,62,10', raisedFires: 1, raisedFiresAt: '10,62,10' }, { shears: 1, dandelion: 4, campfire: 1, white_carpet: 3 })
  const out = await apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 })
  assert.deepEqual([out.openFires, out.openFiresAt, out.raisedFires, out.raisedFiresAt], [0, undefined, 0, undefined])
})

test('apiary.maintain: inspects, harvests, then breeds a small visible colony', async () => {
  const { api, calls } = maintainApi({})
  const out = await apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 })
  assert.deepEqual([out.harvested, out.bred, calls], [1, 1, [
    'apiary.inspect place=orchard-apiary',
    'apiary.harvest place=orchard-apiary',
    'apiary.breed place=orchard-apiary count=2'
  ]])
})

test('apiary.maintain: an open fire is carpeted before anything else', async () => {
  const { api, calls } = maintainApi({ openFires: 1 }, { shears: 1, dandelion: 4, white_carpet: 3 })
  const out = await apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 })
  assert.deepEqual([out.carpeted, calls.slice(0, 2)], [1, ['apiary.inspect place=orchard-apiary', 'apiary.guard place=orchard-apiary']])
})

test('apiary.maintain: a raised fire is sunk before anything else', async () => {
  const { api, calls } = maintainApi({ raisedFires: 1 }, { shears: 1, dandelion: 4, campfire: 1, white_carpet: 3 })
  const out = await apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 })
  assert.deepEqual([out.sunk, out.raisedFires, calls.slice(0, 2)], [1, 0, ['apiary.inspect place=orchard-apiary', 'apiary.guard place=orchard-apiary']])
})

test('apiary.maintain: an open fire with no carpet to hand stops the round with the reason', async () => {
  const { api, calls } = maintainApi({ openFires: 1 })
  await assert.rejects(apiaryMaintain.run(api, { place: 'orchard-apiary', size: 6 }), /open fire.*carpet/)
  assert.deepEqual(calls.filter(c => c.startsWith('apiary.harvest')), [])
})

test('apiaryGoods: only honey products are sent to an output chest', () => {
  assert.deepEqual(apiaryGoods({ honeycomb: 6, honey_bottle: 2, glass_bottle: 4, dandelion: 8 }), { honeycomb: 6, honey_bottle: 2 })
})

test('the rancher and beekeeper routines are valid role step lists', () => {
  for (const file of ['roles/rancher/cattle.json', 'roles/rancher/sheep.json', 'roles/rancher/pigs.json', 'roles/rancher/chickens.json', 'roles/beekeeper/apiary.json']) {
    const steps = JSON.parse(fs.readFileSync(new URL(`../${file}`, import.meta.url), 'utf8'))
    assert.equal(routineSteps({ steps, place: 'test-place' }).error, undefined, file)
  }
})
