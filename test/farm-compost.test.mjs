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
