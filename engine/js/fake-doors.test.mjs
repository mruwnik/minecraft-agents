import { test } from 'node:test'
import assert from 'node:assert/strict'
import { isOpen, flipOpen, pathProps, climbsThrough } from './fake-doors.mjs'

const world = (blocks, states = {}) => ({ blocks: new Map(Object.entries(blocks)), states: new Map(Object.entries(states)) })
const at = (x, y, z) => ({ x, y, z })

test('only an openable block with open true is open', () => {
  const s = world({ '0,64,0': 'oak_fence_gate', '1,64,0': 'oak_fence_gate', '2,64,0': 'stone', '3,64,0': 'iron_door' }, { '0,64,0': { open: true }, '1,64,0': { open: false }, '2,64,0': { open: true }, '3,64,0': { open: true } })
  assert.deepEqual(['0,64,0', '1,64,0', '2,64,0', '3,64,0', '4,64,0'].map(k => isOpen(s, k)), [true, false, false, true, false])
})

test('flipping a gate toggles it', () => {
  const s = world({ '0,64,0': 'oak_fence_gate' })
  flipOpen(s, at(0, 64, 0))
  assert.equal(s.states.get('0,64,0').open, true)
  flipOpen(s, at(0, 64, 0))
  assert.equal(s.states.get('0,64,0').open, false)
})

test('flipping either half of a door moves both', () => {
  const door = () => world({ '0,64,0': 'oak_door', '0,65,0': 'oak_door' }, { '0,64,0': { half: 'lower', open: false }, '0,65,0': { half: 'upper', open: false } })
  const lower = door()
  flipOpen(lower, at(0, 64, 0))
  const upper = door()
  flipOpen(upper, at(0, 65, 0))
  assert.deepEqual([lower, upper].map(s => [s.states.get('0,64,0').open, s.states.get('0,65,0').open]), [[true, true], [true, true]])
})

test('the planner properties of a door are its open, half and facing, those of other blocks none', () => {
  const s = world({ '0,64,0': 'oak_door', '1,64,0': 'stone' }, { '0,64,0': { half: 'lower', open: true, facing: 'east', locked: true }, '1,64,0': { open: true } })
  assert.deepEqual(pathProps(s, '0,64,0'), { open: true, half: 'lower', facing: 'east' })
  assert.deepEqual(pathProps(s, '1,64,0'), {})
})

test('the planner properties of a button are its face, facing and powered', () => {
  const s = world({ '0,64,0': 'stone_button' }, { '0,64,0': { face: 'wall', facing: 'west', powered: false, other: 1 } })
  assert.deepEqual(pathProps(s, '0,64,0'), { facing: 'west', face: 'wall', powered: false })
})

test('the planner properties of a ladder are its facing (the trapdoor over it is judged against it)', () => {
  const s = world({ '0,64,0': 'ladder' }, { '0,64,0': { facing: 'south', other: 1 } })
  assert.deepEqual(pathProps(s, '0,64,0'), { facing: 'south' })
})

test('an open trapdoor over a ladder is climbed through, a shut one or one over stone is not', () => {
  const s = world({ '0,64,0': 'ladder', '0,65,0': 'oak_trapdoor', '3,64,0': 'stone', '3,65,0': 'oak_trapdoor', '6,64,0': 'ladder', '6,65,0': 'oak_trapdoor' },
    { '0,65,0': { open: true }, '3,65,0': { open: true }, '6,65,0': { open: false } })
  assert.deepEqual([[0, 65, 0], [3, 65, 0], [6, 65, 0]].map(c => climbsThrough(s, ...c)), [true, false, false])
})
