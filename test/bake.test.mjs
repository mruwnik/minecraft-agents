// bake: the store's wheat becomes bread at the table beside it, the bread goes back, and keep= loaves stay carried.
// The fake store and pockets move items the way withdraw, craft and deposit would.
import test from 'node:test'
import assert from 'node:assert/strict'
import bake from '../library/bake.mjs'
import { fakeApi } from './helpers.mjs'

const STORE = { x: 1, y: 64, z: 0 }
const TABLE = { x: 3, y: 64, z: 0 }
const kitchen = ({ chest, carried = {}, table = TABLE }) => {
  const items = { ...carried }
  const move = (from, to, wanted) => Object.entries(wanted).forEach(([name, n]) => { from[name] -= n; to[name] = (to[name] ?? 0) + n })
  return fakeApi({
    items,
    answers: {
      find_blocks: { positions: table ? [table] : [] },
      chest_contents: () => ({ items: { ...chest } }),
      withdraw: ({ items: wanted }) => move(chest, items, wanted),
      deposit: ({ items: wanted }) => move(items, chest, wanted),
      craft: ({ count }) => { items.wheat -= 3 * count; items.bread = (items.bread ?? 0) + count }
    }
  })
}
const acts = calls => calls.filter(c => !c.startsWith('note'))

for (const [name, chest, carried, keep, expected, result] of [
  ['the store’s wheat is baked and the bread goes back, keep= loaves carried', { wheat: 30, bread: 100 }, {}, 4,
    ['find_blocks block=crafting_table maxDistance=32 count=1 x=1 y=64 z=0', 'chest_contents 1,64,0', 'withdraw items(wheat:30) x=1 y=64 z=0', 'goto x=3 y=64 z=0 range=2', 'craft item=bread count=10', 'deposit items(bread:6) x=1 y=64 z=0'],
    { baked: 10, deposited: 6, store_bread: 106, bread_carried: 4 }],
  ['three stacks of wheat a round, the rest the next round', { wheat: 200 }, {}, 0,
    ['find_blocks block=crafting_table maxDistance=32 count=1 x=1 y=64 z=0', 'chest_contents 1,64,0',
      'withdraw items(wheat:192) x=1 y=64 z=0', 'goto x=3 y=64 z=0 range=2', 'craft item=bread count=64', 'deposit items(bread:64) x=1 y=64 z=0',
      'withdraw items(wheat:6) x=1 y=64 z=0', 'goto x=3 y=64 z=0 range=2', 'craft item=bread count=2', 'deposit items(bread:2) x=1 y=64 z=0'],
    { baked: 66, deposited: 66, store_bread: 66, bread_carried: 0 }],
  ['carried wheat is baked too', { bread: 0 }, { wheat: 7 }, 0,
    ['find_blocks block=crafting_table maxDistance=32 count=1 x=1 y=64 z=0', 'chest_contents 1,64,0', 'goto x=3 y=64 z=0 range=2', 'craft item=bread count=2', 'deposit items(bread:2) x=1 y=64 z=0'],
    { baked: 2, deposited: 2, store_bread: 2, bread_carried: 0 }],
  ['under three wheat stays in the store, and short loaves come out of it', { wheat: 2, bread: 50 }, { bread: 10 }, 16,
    ['find_blocks block=crafting_table maxDistance=32 count=1 x=1 y=64 z=0', 'chest_contents 1,64,0', 'withdraw items(bread:6) x=1 y=64 z=0'],
    { baked: 0, deposited: 0, store_bread: 44, bread_carried: 16 }]
]) {
  test(`bake: ${name}`, async () => {
    const { api, calls } = kitchen({ chest, carried })
    const got = await bake.run(api, { store: '1,64,0', keep })
    assert.deepEqual(acts(calls), expected)
    assert.deepEqual(got, { ...result, store: '1,64,0' })
  })
}

test('bake: keep= defaults to 16 loaves', async () => {
  const { api } = kitchen({ chest: { wheat: 60 } })
  assert.equal((await bake.run(api, { store: '1,64,0' })).bread_carried, 16)
})

test('bake: no crafting table within 32 blocks of the store is reported, and nothing is taken out', async () => {
  const { api, calls } = kitchen({ chest: { wheat: 30 }, table: null })
  const got = await bake.run(api, { store: '1,64,0' })
  assert.deepEqual([acts(calls), got], [
    ['find_blocks block=crafting_table maxDistance=32 count=1 x=1 y=64 z=0'],
    { attention: 'no crafting table within 32 blocks of the store at 1,64,0: place one beside it', store: '1,64,0' }])
})

test('bake: a store that is not x,y,z is refused', async () => {
  const { api } = kitchen({ chest: {} })
  await assert.rejects(bake.run(api, { store: 'barn' }), /store=barn is not x,y,z/)
})
