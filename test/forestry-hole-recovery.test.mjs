import test from 'node:test'
import assert from 'node:assert/strict'
import { latestOwnClosedDigHole, recoverOwnDigHole } from '../src/survival/recover-hole.mjs'

const event = JSON.stringify({ seq: 728, type: 'holed_up', way: 'dig', open: false, at: '-112,64,-194' })
const at = { x: -112, y: 64, z: -194 }
const key = p => `${p.x},${p.y},${p.z}`

function holeFixture () {
  const blocks = new Map([[key({ ...at, y: 63 }), 'stone'], [key({ ...at, y: 66 }), 'dirt']])
  const items = { dirt: 2 }, calls = []
  let feet = { x: -111.5, y: 64, z: -193.5 }
  const block = (x, y, z) => {
    const name = blocks.get(`${x},${y},${z}`) ?? 'air'
    return { name, solid: !['air', 'cave_air'].includes(name) }
  }
  const api = {
    me: () => 'Treebeard', pos: () => feet, inv: () => items,
    clock: () => ({ night: false }), block,
    act: async (name, a) => {
      calls.push({ name, ...a })
      if (name === 'dig') {
        assert.deepEqual([a.x, a.y, a.z], [-112, 66, -194], 'only the recorded cap can be dug')
        assert.equal(a.force, true, 'the health override is confined to this authorized cap')
        assert.ok(['dirt', 'grass_block'].includes(block(a.x, a.y, a.z).name))
        blocks.delete(key(a)); items.dirt++
      } else if (name === 'pillar_up') {
        assert.equal(a.item, 'dirt'); assert.equal(a.steps, 1)
        const p = { x: Math.floor(feet.x), y: feet.y, z: Math.floor(feet.z) }
        assert.equal(block(p.x, p.y, p.z).name, 'air')
        assert.ok(items.dirt > 0)
        items.dirt--; blocks.set(key(p), 'dirt')
        feet = { ...feet, y: feet.y + 1 }
      } else if (name === 'collect') throw Error('fixture cap dirt should be picked up directly')
    }
  }
  return { api, blocks, calls, items, setFeet: p => { feet = p } }
}

test('own closed hole event is invalidated by a later death or newer open hole', () => {
  const read = lines => latestOwnClosedDigHole(lines.join('\n'), 'Treebeard')
  assert.deepEqual(read([event])?.at, at)
  assert.equal(read([event, JSON.stringify({ type: 'died' })]), null)
  assert.equal(read([event, JSON.stringify({ type: 'holed_up', way: 'dig', open: true, at: '-112,64,-194' })]), null)
})

test('maintenance restores only the three recorded shaft cells and reaches the surface', async () => {
  const f = holeFixture(), record = latestOwnClosedDigHole(event, 'Treebeard')
  const result = await recoverOwnDigHole(f.api, record)
  assert.equal(result.recovered, true)
  assert.deepEqual(f.calls.map(c => c.name), ['dig', 'pillar_up', 'pillar_up', 'pillar_up'])
  for (let y = 64; y <= 66; y++) assert.equal(f.api.block(-112, y, -194).name, 'dirt')
  assert.equal(f.api.pos().y, 67)
  assert.equal(f.items.dirt, 0)
  assert.equal(f.api.block(-112, 64, -193).name, 'air', 'unproven side-wall contents are untouched')
})

test('a grass block grown over the recorded dirt cap is still opened as that cap', async () => {
  const f = holeFixture(), record = latestOwnClosedDigHole(event, 'Treebeard')
  f.blocks.set('-112,66,-194', 'grass_block')
  const result = await recoverOwnDigHole(f.api, record)
  assert.equal(result.recovered, true)
  assert.deepEqual(f.calls.map(c => c.name), ['dig', 'pillar_up', 'pillar_up', 'pillar_up'])
  assert.equal(f.items.dirt, 0)
})

test('interrupted ascent resumes from matching shaft cells without opening unrelated blocks', async () => {
  const f = holeFixture(), record = latestOwnClosedDigHole(event, 'Treebeard')
  const original = f.api.act
  f.api.act = async (name, args) => {
    const value = await original(name, args)
    if (name === 'pillar_up') throw Error('cancelled after one placed support')
    return value
  }
  await assert.rejects(recoverOwnDigHole(f.api, record), /cancelled/)
  assert.equal(f.api.pos().y, 65)
  f.api.act = original
  const result = await recoverOwnDigHole(f.api, record)
  assert.equal(result.recovered, true)
  assert.equal(f.calls.filter(c => c.name === 'dig').length, 1)
  for (let y = 64; y <= 66; y++) assert.equal(f.api.block(-112, y, -194).name, 'dirt')
})

test('wrong body, owner, cap material, or insufficient dirt cannot open a hole', async () => {
  const record = latestOwnClosedDigHole(event, 'Treebeard')
  for (const mode of ['wrong-owner', 'wrong-body', 'changed-cap', 'short-dirt']) {
    const f = holeFixture()
    if (mode === 'wrong-owner') f.api.me = () => 'SomeoneElse'
    if (mode === 'wrong-body') f.setFeet({ x: -110.5, y: 64, z: -193.5 })
    if (mode === 'changed-cap') f.blocks.set('-112,66,-194', 'stone')
    if (mode === 'short-dirt') f.items.dirt = 1
    const result = await recoverOwnDigHole(f.api, record)
    assert.notEqual(result.recovered, true, mode)
    assert.equal(f.calls.length, 0, mode)
  }
})
