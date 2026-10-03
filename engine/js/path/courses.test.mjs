import { test } from 'node:test'
import assert from 'node:assert/strict'
import prismarineRegistry from 'prismarine-registry'
import prismarineBlock from 'prismarine-block'
import { fixtureSnapshot } from './fixture.mjs'
import { applyCommands, courseSnapshot, courseNames } from './courses.mjs'
import { COURSES } from './courses-data.mjs'

const registry = prismarineRegistry('26.1')
const Block = prismarineBlock(registry)
const blockAt = (snap, x, y, z) => {
  const block = Block.fromStateId(snap.stateAt(x, y, z), 0)
  return { name: block.name, ...block.getProperties() }
}
const replay = cmds => fixtureSnapshot({ fill: applyCommands([[0, 0, 0, 15, 8, 15, 'stone']], cmds) })

const parsed = [
  ['setblock with a namespace and props', ['setblock 3 4 5 minecraft:cocoa[age=2,facing=north]'], [3, 4, 5], { name: 'cocoa', age: '2', facing: 'north' }],
  ['setblock with no props', ['setblock 3 4 5 oak_planks'], [3, 4, 5], { name: 'oak_planks' }],
  ['boolean props', ['setblock 3 4 5 oak_door[facing=east,half=lower,open=true]'], [3, 4, 5], { name: 'oak_door', open: true, half: 'lower', facing: 'east' }],
  ['plain fill, corners in any order', ['fill 5 6 7 2 4 3 dirt'], [3, 5, 5], { name: 'dirt' }],
  ['fill replace with no filter replaces everything', ['fill 1 1 1 2 2 2 glass replace'], [2, 2, 2], { name: 'glass' }],
  ['a later command wins', ['fill 1 1 1 4 4 4 glass', 'setblock 2 2 2 air'], [2, 2, 2], { name: 'air' }],
  ['a later fill wins over an earlier setblock', ['setblock 2 2 2 glass', 'fill 1 1 1 4 4 4 air'], [2, 2, 2], { name: 'air' }],
  ['non-terrain commands are skipped', ['summon minecart 1.5 2 3.5', 'kill @e[type=item]', 'setblock 1 1 1 glass'], [1, 1, 1], { name: 'glass' }]
]
for (const [what, cmds, [x, y, z], expected] of parsed) {
  test(`applyCommands: ${what}`, () => {
    const found = blockAt(replay(cmds), x, y, z)
    assert.deepEqual(Object.fromEntries(Object.entries(expected).map(([k]) => [k, found[k]])), expected)
  })
}

const refused = [
  ['fill 1 1 1 2 2 2 air keep', /keep/],
  ['fill 1 1 1 4 4 4 glass hollow', /hollow/],
  ['fill 1 1 1 4 4 4 glass outline', /outline/],
  ['fill 1 1 1 4 4 4 glass replace stone', /replace/],
  ['setblock 1 1 1 glass keep', /keep/],
  ['fill ~ 1 1 2 2 2 air', /relative/],
  ['clone 1 1 1 2 2 2 5 5 5', /clone/]
]
for (const [cmd, why] of refused) {
  test(`applyCommands refuses: ${cmd}`, () => {
    assert.throws(() => applyCommands([], [cmd]), why)
  })
}

test('applyCommands keeps the entries it is given and does not mutate them', () => {
  const before = [[0, 0, 0, 1, 1, 1, 'stone']]
  const after = applyCommands(before, ['setblock 1 1 1 glass'])
  assert.equal(before.length, 1)
  assert.equal(after.length, 2)
})

const cells = [
  ['cocoa-a2-both-feethead', [2880, 161, 3216], { name: 'cocoa', age: '2', facing: 'north' }],
  ['cocoa-a2-both-feethead', [2880, 162, 3217], { name: 'cocoa', age: '2', facing: 'south' }],
  ['cocoa-a2-both-feethead', [2880, 161, 3215], { name: 'jungle_log' }],
  ['cocoa-a2-both-feethead', [2880, 163, 3218], { name: 'jungle_log' }],
  ['cocoa-a2-both-feethead', [2880, 164, 3218], { name: 'air' }],
  ['cocoa-a2-both-feethead', [2880, 163, 3216], { name: 'air' }],
  ['cocoa-a0-both-feet', [2880, 162, 3216], { name: 'air' }],
  ['cocoa-a0-both-feet', [2880, 161, 3217], { name: 'cocoa', age: '0', facing: 'south' }],
  ['cocoa-a2-one-feethead', [2880, 161, 3217], { name: 'air' }],
  ['cocoa-open-a2-both-feethead', [2880, 161, 3214], { name: 'air' }],
  ['cocoa-farm-across', [2874, 162, 3209], { name: 'jungle_log' }],
  ['cocoa-farm-across', [2873, 162, 3208], { name: 'glass' }],
  ['cocoa-farm-across', [2875, 162, 3209], { name: 'cocoa', facing: 'west' }],
  ['checker-we', [2875, 161, 3209], { name: 'bamboo' }],
  ['checker-we', [2875, 165, 3209], { name: 'bamboo' }],
  ['checker-we', [2875, 166, 3209], { name: 'air' }],
  ['checker-we', [2876, 161, 3209], { name: 'air' }],
  ['checker-we', [2875, 160, 3209], { name: 'dirt' }],
  ['checker-we', [2870, 160, 3209], { name: 'stone' }],
  ['full-walled', [2889, 165, 3223], { name: 'bamboo' }],
  ['full-walled', [2857, 161, 3224], { name: 'glass' }],
  ['full-open', [2857, 161, 3224], { name: 'air' }],
  ['full-open', [2853, 161, 3216], { name: 'glass' }],
  ['door-open', [2880, 161, 3216], { name: 'oak_door', open: true, half: 'lower' }],
  ['door-open', [2880, 163, 3216], { name: 'stone' }],
  ['tunnel-stairs', [2879, 161, 3216], { name: 'stone_stairs', facing: 'east' }],
  ['tunnel-stairs', [2875, 161, 3216], { name: 'air' }],
  ['trap-ceil-top', [2878, 162, 3216], { name: 'oak_trapdoor', half: 'top', open: false }],
  ['fence-diag', [2880, 161, 3216], { name: 'oak_fence' }],
  ['fence-diag', [2881, 161, 3217], { name: 'oak_fence' }],
  ['fence-diag', [2881, 161, 3216], { name: 'air' }],
  ['stream3', [2879, 159, 3216], { name: 'water' }],
  ['stream3', [2879, 160, 3216], { name: 'air' }],
  ['stream3', [2879, 160, 3213], { name: 'stone' }],
  ['stream3', [2879, 161, 3213], { name: 'glass' }],
  ['stream3', [2870, 160, 3207], { name: 'stone' }]
]
for (const [course, [x, y, z], expected] of cells) {
  test(`course ${course} at ${x} ${y} ${z} is ${expected.name}`, () => {
    const found = blockAt(courseSnapshot(course).snapshot, x, y, z)
    assert.deepEqual(Object.fromEntries(Object.entries(expected).map(([k]) => [k, found[k]])), expected)
  })
}

const queries = [
  ['cocoa-a2-both-feethead', { x: 2857, y: 161, z: 3217 }, { kind: 'near', x: 2902, y: 161, z: 3216, range: 1 }],
  ['cocoa-a2-both-feethead-diag', { x: 2870, y: 161, z: 3210 }, { kind: 'near', x: 2890, y: 161, z: 3223, range: 1 }],
  ['checker-we', { x: 2857, y: 161, z: 3216 }, { kind: 'near', x: 2902, y: 161, z: 3216, range: 1 }],
  ['lad-shaft20-up', { x: 2857, y: 161, z: 3216 }, { kind: 'near', x: 2885, y: 181, z: 3216, range: 1 }],
  ['lake20-high', { x: 2857, y: 161, z: 3216 }, { kind: 'near', x: 2902, y: 162, z: 3216, range: 1 }]
]
for (const [course, from, goal] of queries) {
  test(`course ${course} start and goal cells`, () => {
    const c = courseSnapshot(course)
    assert.deepEqual({ ...c.from, px: undefined, pz: undefined }, { ...from, px: undefined, pz: undefined })
    assert.deepEqual(c.goal, goal)
  })
}

test('the start keeps its exact position for the planner', () => {
  const { from } = courseSnapshot('cocoa-a2-both-feethead')
  assert.deepEqual([from.px, from.pz], [2857.5, 3217])
})

test('every course builds, and every course name is known', () => {
  const names = courseNames()
  assert.deepEqual(names, Object.keys(COURSES))
  assert.ok(names.length > 100)
  names.forEach(name => assert.ok(courseSnapshot(name).snapshot.stateAt(2860, 160, 3216) !== undefined))
})

test('an unknown course is an error', () => {
  assert.throws(() => courseSnapshot('no-such-course'), /unknown course/)
})
