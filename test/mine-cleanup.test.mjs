import test from 'node:test'
import assert from 'node:assert/strict'
import mine from '../library/mine/get.mjs'
import { canPlaceFromHere } from '../src/lib/place.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import { fakeApi } from './helpers.mjs'

const key = p => `${p.x},${p.y},${p.z}`
const scenario = ({ dig, goto, place, holes = [{ x: 0, y: 62, z: 0 }, { x: 0, y: 63, z: 0 }, { x: 1, y: 63, z: 0 }], dirt = 10, depth = 60 } = {}) => {
  const world = {}
  for (let x = -4; x <= 4; x++) for (let z = -4; z <= 4; z++) for (let y = 62; y <= 66; y++) world[`${x},${y},${z}`] = y < 64 ? 'dirt' : 'air'
  world['0,60,0'] = 'stone'
  const state = { pos: { x: 0.5, y: 64, z: 0.5 } }
  const items = { dirt, stone_pickaxe: 1 }
  const made = fakeApi({ world, items, answers: {
    find_blocks: { positions: [{ x: 0, y: 60, z: 0 }] },
    zones: { zones: [] },
    // A successful planner result never means the miner has moved.
    path_to: { status: 'success', nodes: 10 },
    dig: p => {
      if (dig) return dig(p)
      world[key(p)] = 'air'
      for (const hole of holes) world[key(hole)] = 'air'
      state.pos = { x: 0.5, y: depth, z: 0.5 }
    },
    goto: p => {
      if (goto) return goto(p, state)
      assert.equal(p.range, 0)
      state.pos = { x: p.x + 0.5, y: p.y, z: p.z + 0.5 }
    },
    place: p => {
      assert.equal(p.blocks, undefined, 'never give place a walking batch')
      assert.ok(canPlaceFromHere(state.pos, p), 'placement must already be in no-walk reach')
      assert.ok(p.y < state.pos.y, 'never fill beside feet or head')
      if (place) return place(p, state, world)
      world[key(p)] = p.item
      items[p.item]--
      return { placed: 1 }
    }
  } })
  made.api.pos = () => ({ ...state.pos })
  return { ...made, state, world }
}

test('mine cleanup actually escapes before filling, even when the path preview would report success', async () => {
  const { api, calls, state } = scenario()
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.equal(result.got, 1)
  assert.match(result.climbedOut, /verified on safe ground/)
  assert.equal(state.pos.y, 64)
  assert.ok(calls.findIndex(c => c.startsWith('goto ')) < calls.findIndex(c => c.startsWith('place ')))
  assert.equal(result.mended, '3 of 3 blocks of the ground you broke open at the start put back')
  assert.equal(result.cleanup_left, undefined)
})

test('a goto success without actual arrival never permits filling around the miner', async () => {
  const { api, calls } = scenario({ goto: () => ({}) })
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.match(result.pit, /no ground was filled/)
  assert.equal(calls.filter(c => c.startsWith('goto ')).length, 2)
  assert.ok(!calls.some(c => c.startsWith('place ')))
})

test('standing two blocks below home is not a successful escape', async () => {
  const { api, calls, state } = scenario({ depth: 62 })
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.equal(state.pos.y, 64)
  assert.match(calls.find(c => c.startsWith('goto ')), /y=64 .*range=0/)
  assert.ok(result.climbedOut)
})

test('failed walking gets one dig escape attempt, and failed escape leaves all holes open', async () => {
  const { api, calls, world } = scenario({ goto: () => { throw new Error('no walkable path') } })
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.match(result.pit, /could not get back out/)
  assert.equal(calls.filter(c => c.startsWith('goto ')).length, 2)
  assert.match(calls.at(-1), / dig$/)
  assert.equal(world['0,63,0'], 'air')
  assert.ok(!calls.some(c => c.startsWith('place ')))
})

test('cleanup leaves distant holes reported instead of making placement walk back into the excavation', async () => {
  const { api, calls, world } = scenario({ holes: [{ x: 0, y: 63, z: 0 }, { x: 2, y: 62, z: 2 }] })
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.match(result.mended, /^1 of 2/)
  assert.match(result.cleanup_left, /2,62,2/)
  assert.equal(world['2,62,2'], 'air')
  assert.equal(calls.filter(c => c.startsWith('goto ')).length, 1)
})

test('missing filler and silently failed placements are unfinished cleanup, never claimed repairs', async () => {
  for (const options of [{ dirt: 0 }, { place: () => ({ placed: 1 }) }]) {
    const { api } = scenario(options)
    const result = await mine.run(api, { block: 'stone', count: 1 })
    assert.match(result.mended, /^0 of 3/)
    assert.match(result.cleanup_left, /3 ground cells remain open/)
  }
})

test('movement away from verified surface during a placement stops further filling', async () => {
  const { api, calls } = scenario({ place: (p, state, world) => { world[key(p)] = p.item; state.pos.y = 60 } })
  const result = await mine.run(api, { block: 'stone', count: 1 })
  assert.equal(calls.filter(c => c.startsWith('place ')).length, 1)
  assert.match(result.pit, /further filling stopped/)
  assert.match(result.cleanup_left, /2 ground cells remain open/)
})

for (const stage of ['dig', 'goto', 'place']) {
  test(`cancellation during ${stage} propagates without any later mining or cleanup action`, async () => {
    const { api, calls } = scenario({ [stage]: () => { throw new Error('cancelled') } })
    await assert.rejects(mine.run(api, { block: 'stone', count: 1 }), /cancelled/)
    assert.ok(calls.at(-1).startsWith(`${stage} `))
    assert.equal(calls.filter(c => c.startsWith(`${stage} `)).length, 1)
  })
}

test('a nested safety hand-back during escape is not mistaken for arrival', async () => {
  const { api, calls } = scenario({ goto: () => ({ stopped: 'health 4' }) })
  await assert.rejects(mine.run(api, { block: 'stone', count: 1 }), e => e instanceof CompositeHandBack && e.reason === 'health 4')
  assert.ok(!calls.some(c => c.startsWith('place ')))
  assert.equal(calls.filter(c => c.startsWith('goto ')).length, 1)
})
