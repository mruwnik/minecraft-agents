import test from 'node:test'
import assert from 'node:assert/strict'
import { waterShortfall } from '../src/build/plan.mjs'
import { fillOutcome } from '../src/lib/place.mjs'
import { planCells, parsePlan } from '../src/lib/plan.mjs'
import maintain from '../library/farm/maintain.mjs'
import build from '../library/farm/build.mjs'
import { fakeApi } from './helpers.mjs'

const source = { x: 65, y: 67, z: -126 }
const key = p => `${p.x},${p.y},${p.z}`
const worldForWell = () => {
  const world = {}
  for (let x = 61; x <= 70; x++) for (let z = -130; z <= -122; z++) for (let y = 66; y <= 71; y++) {
    world[`${x},${y},${z}`] = y <= (x >= 68 ? 66 : 67) ? 'grass_block' : 'air'
  }
  for (const x of [65, 66]) for (const z of [-127, -126]) world[`${x},67,${z}`] = 'water'
  return world
}

test('refill walks onto a close elevated dry shore instead of filling from below the bank', async () => {
  const world = worldForWell()
  const items = { bucket: 1 }
  let position = { x: 69, y: 67, z: -126 }
  const made = fakeApi({ world, items, answers: {
    find_blocks: { positions: [source] },
    goto: p => { assert.equal(p.range, 0); assert.ok(p.y >= 68); position = p },
    fill: p => {
      assert.deepEqual(p, source)
      assert.ok(Math.hypot(position.x - p.x, position.z - p.z) <= 2)
      assert.equal(world[key(position)], 'air')
      assert.notEqual(world[key({ ...position, y: position.y - 1 })], 'water')
      items.bucket--; items.water_bucket = 1
    }
  } })
  made.api.pos = () => position
  assert.equal(await waterShortfall(made.api), null)
  assert.ok(made.calls[1].startsWith('goto '))
  assert.equal(made.calls[2], 'fill 65,67,-126')
})

test('a source without a loaded dry shore is reported without an unsafe fill', async () => {
  const made = fakeApi({ world: { [key(source)]: 'water' }, items: { bucket: 1 }, answers: { find_blocks: { positions: [source] } } })
  assert.match(await waterShortfall(made.api), /no reachable dry shore.*65,67,-126/)
  assert.equal(made.calls.length, 1)
})

for (const failure of [new Error('cancelled'), new TypeError('bucket state not defined'), new Error('unexpected fill parser state')]) {
  test(`refill preserves hard failure: ${failure.message}`, async () => {
    const made = fakeApi({ world: worldForWell(), items: { bucket: 1 }, answers: { find_blocks: { positions: [source] }, fill: failure } })
    await assert.rejects(waterShortfall(made.api), e => e === failure)
    assert.equal(made.calls.filter(c => c.startsWith('fill ')).length, 1)
  })
}

for (const action of [maintain, build]) {
  test(`${action === maintain ? 'maintain' : 'build'} reports an empty bucket once and still plants independent beds`, async () => {
    const plan = { name: 'field', kind: 'farm', plan: 'w~.~.~', x: 0, y: 63, z: 0 }
    plan.parsed = parsePlan(plan.plan); plan.cells = planCells(plan)
    const world = {}
    for (let x = -3; x <= 12; x++) for (let z = -3; z <= 3; z++) for (let y = 62; y <= 66; y++) world[`${x},${y},${z}`] = y <= 63 ? 'dirt' : 'air'
    world['0,63,0'] = 'farmland'
    world['9,63,0'] = 'water'
    const items = { water_bucket: 1, wheat_seeds: 1, stone_hoe: 2, oak_slab: 3 }
    const made = fakeApi({ place: plan, items, world, answers: {
      'farm.harvest': { harvested: {}, replanted: 0 },
      find_blocks: { positions: [{ x: 9, y: 63, z: 0 }] },
      fill: new Error(fillOutcome('bucket').error),
      dig: p => { world[key(p)] = 'air' },
      pour: p => { world[key({ ...p, y: p.y + 1 })] = 'water'; items.water_bucket--; items.bucket = 1 },
      place: p => { world[key(p)] = p.item === 'wheat_seeds' ? 'wheat#0' : p.item === 'oak_slab' ? 'oak_slab#top~' : p.item; items[p.item]-- }
    } })
    const summary = await action.run(made.api, { place: plan.name, partial: true, compost: false })
    assert.equal(world['0,64,0'], 'wheat#0')
    assert.equal(summary.poured, 1)
    assert.equal(made.calls.filter(c => c.startsWith('fill ')).length, 1, 'failed refills are not retried for each remaining channel')
    assert.ok(made.events.some(e => e.type === 'farm_attention' && /could not fill the bucket from shore/.test(JSON.stringify(e.reasons))))
  })
}
