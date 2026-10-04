// Finding the composter a farm.compost call feeds: the plan's own K cell when it has one, an explicit x=/y=/z=
// always winning outright, and - the gap this file was written to close - a nearby composter picked up automatically
// when the plan has none, instead of refusing outright and stranding a plot that was never given its own composter.
import test from 'node:test'
import assert from 'node:assert/strict'
import { fakeApi } from './helpers.mjs'
import farmCompost from '../library/farm/compost.mjs'

const NO_COMPOSTER_PLACE = { cells: [{ x: 5, y: 60, z: 200, ch: 'w' }] }
const COMPOSTER_PLACE = { cells: [{ x: 5, y: 60, z: 200, ch: 'w' }, { x: 8, y: 60, z: 202, ch: 'K' }] }

test('farm.compost: a plan with a K cell uses it, no search needed', async () => {
  const { api, calls } = fakeApi({ place: COMPOSTER_PLACE })
  const result = await farmCompost.run(api, { place: 'test-field' })
  assert.equal(result.at, '8,61,202')
  assert.ok(!calls.some(c => c.startsWith('find_blocks')), `should not have searched: ${calls}`)
})

test('farm.compost: a plan with no K cell falls back to the nearest real composter instead of refusing', async () => {
  const { api } = fakeApi({
    place: NO_COMPOSTER_PLACE,
    answers: { find_blocks: () => ({ positions: [{ x: 24, y: 64, z: -86 }] }) }
  })
  const result = await farmCompost.run(api, { place: 'jizo-cane' })
  assert.equal(result.at, '24,64,-86')
})

test('farm.compost: no K cell and nothing nearby asks for attention without pretending to compost', async () => {
  const { api, calls, events } = fakeApi({ place: NO_COMPOSTER_PLACE, answers: { find_blocks: () => ({ positions: [] }) } })
  const result = await farmCompost.run(api, { place: 'jizo-cane' })
  assert.match(result.attention, /no composter within 32 blocks/)
  assert.equal(events[0].type, 'farm_attention')
  assert.equal(result.fed, undefined)
  assert.ok(!calls.some(c => c.startsWith('use ')))
})

test('farm.compost: explicit x=/y=/z= always wins, even over a plan with its own K cell', async () => {
  const { api, calls } = fakeApi({ place: COMPOSTER_PLACE })
  const result = await farmCompost.run(api, { place: 'test-field', x: 1, y: 2, z: 3 })
  assert.equal(result.at, '1,2,3')
  assert.ok(!calls.some(c => c.startsWith('find_blocks')))
})

// Below: fed= must reflect what the inventory actually lost, not how many 'use' calls resolved without
// throwing. A right-click can resolve clean on a server that rejected the interaction (range, wrong hand,
// a composter that did not register the click) and the loop must not report a feed that never happened.
test('farm.compost: fed counts seeds that actually left the inventory as the composter fills', async () => {
  const items = { wheat_seeds: 10 }
  const world = { '8,61,202': 'composter#0' }
  const { api } = fakeApi({
    place: COMPOSTER_PLACE, world, items,
    answers: {
      use: () => {
        const level = Number(world['8,61,202'].split('#')[1])
        if (level === 8) { world['8,61,202'] = 'composter#0'; return {} } // taking the bone meal out
        items.wheat_seeds--
        world['8,61,202'] = `composter#${level + 1}`
        return {}
      }
    }
  })
  const result = await farmCompost.run(api, { place: 'test-field', items: 'wheat_seeds', keep: 0 })
  assert.equal(result.fed, 'wheat_seeds:10')
  assert.equal(items.wheat_seeds, 0, 'every reported feed actually left the inventory')
  assert.equal(result.boneMeal, 1)
})

test('farm.compost: a composter that accepts the click but takes nothing is not reported as fed', async () => {
  const items = { wheat_seeds: 5 }
  const world = { '8,61,202': 'composter#0' }
  const { api } = fakeApi({
    place: COMPOSTER_PLACE, world, items,
    answers: { use: () => ({}) } // resolves clean, as a rejected server-side use still would; nothing is consumed
  })
  const result = await farmCompost.run(api, { place: 'test-field', items: 'wheat_seeds', keep: 0 })
  assert.equal(items.wheat_seeds, 5, 'no seed left the inventory')
  assert.equal(result.fed, undefined, 'a use that consumed nothing must not be claimed as a feed')
  assert.match(result.attention, /did not leave|did not take/)
})
