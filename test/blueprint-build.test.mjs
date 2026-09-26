// The walking half of the blueprint format (src/blueprint-build.mjs) over the fake body: what it walks to, what it
// places, what it refuses before touching a block, and what it says when it stops
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fakeApi } from './helpers.mjs'
import { buildBlueprint, checkBlueprint, listText, showText, paramsOf, supplyOf, BLUEPRINT_DIR, readBlueprint, blueprintFiles } from '../src/blueprint-build.mjs'
import { blueprintHash, buildNote, flatGround } from '../src/blueprint.mjs'

const HUT = fs.readFileSync(path.join(BLUEPRINT_DIR, 'starter-hut.md'), 'utf8')
const BOX = `---
name: box
title: Box
description: a closed stone box with a plank lid
tags: storage
front: south
foundation: flat
---

\`\`\`legend
S  cobblestone
B  stone_bricks
P  oak_planks
\`\`\`

## y0

\`\`\`layer
SSS
S.S
SSS
\`\`\`

## y1

\`\`\`layer
BBB
B.B
BBB
\`\`\`

## y2

\`\`\`layer
PPP
PPP
PPP
\`\`\`
`
const FILES = { 'starter-hut': HUT, box: BOX }
const read = name => {
  if (!FILES[name]) throw new Error(`no blueprint called ${name}`)
  return { name, text: FILES[name], hash: blueprintHash(FILES[name]) }
}
const io = { read }
const AT = { x: 100, y: 65, z: -20 }
const key = (x, y, z) => `${x},${y},${z}`

// a world over flat grass at y=64 with the named cells set, in the fake body's string form ('name' or 'name#tag')
const worldOf = (cells = {}) => new Proxy({ ...cells }, {
  get: (t, k) => k in t ? t[k] : (typeof k === 'string' && /^-?\d+,-?\d+,-?\d+$/.test(k) ? flatGround(64)(...k.split(',').map(Number)).name : undefined)
})
const OPPOSITE = { north: 'south', south: 'north', east: 'west', west: 'east' }
const STEP = { north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0] }
// what the server does when a block is placed the way `place` places it: the block appears with the state its item and
// the look direction give it, a door grows its upper half, a bed its head, a chest faces the player, a log placed on a
// floor stands upright. States live beside the world and are read back through api.block
const placing = (world, states, items) => args => {
  const at = key(args.x, args.y, args.z)
  const item = args.item
  world[at] = item
  items[item] = (items[item] ?? 0) - 1
  const state = { ...(args.half ? { half: args.half, type: args.half } : {}) }
  if (args.facing) state.facing = /^(chest|furnace|barrel)$/.test(item) ? OPPOSITE[args.facing] : args.facing
  if (/_log$/.test(item)) state.axis = 'y'
  if (/_door$/.test(item)) {
    state.half = 'lower'
    world[key(args.x, args.y + 1, args.z)] = item
    states[key(args.x, args.y + 1, args.z)] = { ...state, half: 'upper' }
  }
  if (/_bed$/.test(item)) {
    state.part = 'foot'
    const [dx, dy, dz] = STEP[state.facing]
    world[key(args.x + dx, args.y + dy, args.z + dz)] = item
    states[key(args.x + dx, args.y + dy, args.z + dz)] = { ...state, part: 'head' }
  }
  states[at] = state
  return {}
}
const digging = world => args => { world[key(args.x, args.y, args.z)] = 'air'; return {} }

// a body over a world whose blocks answer with the states the placing fake wrote
const body = ({ world, states = {}, items = {}, answers = {}, ...rest }) => {
  const made = fakeApi({ world, items, answers: { place: placing(world, states, items), dig: digging(world), ...answers }, ...rest })
  const block = made.api.block
  made.api.block = (x, y, z) => {
    const raw = block(x, y, z)
    return raw && { ...raw, properties: { ...raw.properties, ...(states[key(x, y, z)] ?? {}) } }
  }
  return made
}

const hutPlace = { name: 'hut', kind: 'build', x: AT.x, y: AT.y, z: AT.z, by: 'Tester', note: buildNote({ blueprint: 'starter-hut', facing: 'south', params: {}, hash: blueprintHash(HUT) }) }
// 60 of the hut's full blocks already standing: the dug floor, the roof and ten of the y2 wall
const halfBuilt = () => {
  const cells = {}
  for (let dx = 0; dx < 5; dx++) for (let dz = 0; dz < 5; dz++) cells[key(AT.x + dx, AT.y - 1, AT.z + dz)] = dx === 0 || dx === 4 || dz === 0 || dz === 4 ? 'cobblestone' : 'oak_planks'
  for (let dx = 0; dx < 5; dx++) for (let dz = 0; dz < 5; dz++) cells[key(AT.x + dx, AT.y + 3, AT.z + dz)] = 'oak_planks'
  for (const dz of [1, 2, 3]) { cells[key(AT.x, AT.y + 2, AT.z + dz)] = 'cobblestone'; cells[key(AT.x + 4, AT.y + 2, AT.z + dz)] = 'cobblestone' }
  for (const dx of [1, 2]) { cells[key(AT.x + dx, AT.y + 2, AT.z)] = 'cobblestone'; cells[key(AT.x + dx, AT.y + 2, AT.z + 4)] = 'cobblestone' }
  return cells
}
const hutKit = () => ({ cobblestone: 64, oak_planks: 64, oak_log: 16, glass_pane: 4, oak_door: 1, white_bed: 1, chest: 1, crafting_table: 1, oak_stairs: 1, torch: 4 })

test('an obstacle without clear= refuses before any act call', async () => {
  const world = worldOf({ [key(AT.x + 1, AT.y, AT.z)]: 'oak_planks' })
  const { api, calls } = body({ world, items: hutKit() })
  await assert.rejects(buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io), { message: `oak_planks at ${AT.x + 1},${AT.y},${AT.z} is in the way of S: dig it, or run again with clear=true` })
  assert.deepEqual(calls, [])
})

test('a container in the way is never dug, even with clear=true', async () => {
  const world = worldOf({ [key(AT.x + 1, AT.y, AT.z)]: 'chest' })
  const { api, calls } = body({ world, items: hutKit() })
  await assert.rejects(buildBlueprint(api, { name: 'starter-hut', place: 'hut', clear: true, ...AT }, io), { message: `chest at ${AT.x + 1},${AT.y},${AT.z} is in the way of S and a container is never dug: move it, or move the anchor` })
  assert.deepEqual(calls, [])
})

test('a new build is marked kind=build with the blueprint in its note before the first block', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: hutKit() })
  await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.equal(calls[0], `mark name=hut kind=build x=100 y=65 z=-20 note=bp=starter-hut f=south h=${blueprintHash(HUT)}`)
})

test('a half-built hut resumes from its mark, skips 60 blocks and finishes as a shelter', async () => {
  const world = worldOf(halfBuilt())
  const { api, calls, report } = body({ world, items: hutKit(), places: [hutPlace] })
  const r = await buildBlueprint(api, { place: 'hut' }, io)
  assert.equal(r.skipped, 60)
  assert.equal(r.built, 42)
  assert.equal(r.dug, undefined)
  assert.equal(r.missing, undefined)
  assert.equal(calls.filter(c => c.startsWith('mark ')).length, 1)
  assert.match(calls.at(-1), /^mark name=hut kind=shelter /)
  assert.equal(report.built, 42)
})

test('a finished build answers already= and touches nothing', async () => {
  const world = worldOf(halfBuilt())
  const first = body({ world, items: hutKit(), places: [hutPlace] })
  await buildBlueprint(first.api, { place: 'hut' }, io)
  const { api, calls } = body({ world, states: {}, items: hutKit(), places: [{ ...hutPlace, kind: 'shelter' }] })
  // the second body reads the same world through the first one's states
  api.block = first.api.block
  const r = await buildBlueprint(api, { place: 'hut' }, io)
  assert.equal(r.already, 'everything starter-hut asks for stands at 100,65,-20')
  assert.deepEqual(calls, [])
})

test('the y-1 floor is dug out of the turf before it is laid', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: hutKit() })
  const r = await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.equal(r.dug, 25)
  assert.equal(calls.filter(c => c.startsWith('dig ')).length, 25)
  assert.equal(calls.indexOf(`dig x=100 y=64 z=-20`) < calls.indexOf('place item=cobblestone x=100 y=64 z=-20'), true)
})

test('grass left in the bed head cell is dug before the bed goes in, and the bed is built', async () => {
  const head = key(AT.x + 2, AT.y, AT.z + 2)
  const world = worldOf({ [head]: 'grass_block' })
  const { api, calls } = body({ world, items: hutKit() })
  const r = await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.equal(r.stuck, undefined)
  const dug = calls.indexOf(`dig ${head}`)
  assert.equal(dug >= 0 && dug < calls.findIndex(c => c.startsWith('place item=white_bed ')), true)
})

// a place the game refuses over a bed or door names the other half's cell and what still stands there, so the driver
// can dig it rather than run the same build again into the same refusal
test('a bed that does not take names what stands in its head cell', async () => {
  const head = key(AT.x + 2, AT.y, AT.z + 2)
  const world = worldOf()
  const refuse = () => { world[head] = 'grass_block'; throw new Error('placing white_bed at (101, 65, -18) did not take with the requested facing and parts') }
  const { api } = body({ world, items: hutKit(), answers: { place: args => args.item === 'white_bed' ? refuse() : placing(world, {}, {})(args) } })
  const r = await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.equal(r.stuck, `place 101,65,-18: placing white_bed at (101, 65, -18) did not take with the requested facing and parts: grass_block stands in its other half at 102,65,-18: dig it, then run blueprint.build place=hut again`)
})

test('an obstacle is dug with clear=true and counted', async () => {
  const world = worldOf({ [key(AT.x + 1, AT.y, AT.z)]: 'oak_planks' })
  const { api } = body({ world, items: hutKit() })
  const r = await buildBlueprint(api, { name: 'starter-hut', place: 'hut', clear: true, ...AT }, io)
  assert.equal(r.obstacles, `1 (oak_planks ${AT.x + 1},${AT.y},${AT.z})`)
})

test('a supply chest short of stage 2 stops with the exact sentence', async () => {
  const world = worldOf()
  const items = {}
  const chest = { cobblestone: 8 }
  const withdraw = args => {
    const short = []
    for (const [item, n] of Object.entries(args.items)) {
      const got = Math.min(n, chest[item] ?? 0)
      items[item] = (items[item] ?? 0) + got
      chest[item] = (chest[item] ?? 0) - got
      if (got < n) short.push(`${item}:${got}/${n}`)
    }
    return short.length ? new Error(`chest has less than asked (have/wanted): ${short.join(' ')}; took what there was`) : {}
  }
  const { api, calls } = body({ world, items, freeSlots: 3, answers: { withdraw } })
  const r = await buildBlueprint(api, { name: 'box', place: 'box', supply: '98,65,-14', ...AT }, io)
  assert.equal(r.stopped, 'stage 2 of 3 needs stone_bricks:8 more: put it in the supply chest at 98,65,-14 and run blueprint.build place=box again')
  assert.equal(r.built, 8)
  assert.equal(r.stage, '2/3')
  assert.equal(calls.filter(c => c.startsWith('withdraw ')).length, 2)
  assert.equal(calls[1], 'goto x=98 y=65 z=-14 range=2')
})

test('without a supply chest a short first stage is refused before a block moves', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: { cobblestone: 3 }, freeSlots: 3 })
  await assert.rejects(buildBlueprint(api, { name: 'box', place: 'box', ...AT }, io), { message: 'stage 1 of 3 needs cobblestone:5 more: fetch it, or put it in a chest and pass supply=x,y,z, then run blueprint.build place=box again' })
  assert.equal(calls.filter(c => !c.startsWith('mark ')).length, 0)
})

test('partial=true builds what the stage can and then stops with the same sentence', async () => {
  const world = worldOf()
  const { api } = body({ world, items: { cobblestone: 3 }, freeSlots: 3 })
  const r = await buildBlueprint(api, { name: 'box', place: 'box', partial: true, ...AT }, io)
  assert.equal(r.built, 3)
  assert.equal(r.missing, 'cobblestone:5')
  assert.equal(r.stopped, 'stage 1 of 3 needs cobblestone:5 more: fetch it, or put it in a chest and pass supply=x,y,z, then run blueprint.build place=box again')
})

test('the lid of a box without a door is laid from outside its walls', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: { cobblestone: 64, stone_bricks: 64, oak_planks: 64 } })
  await buildBlueprint(api, { name: 'box', place: 'box', ...AT }, io)
  // the walk before each plank of the lid
  const stands = calls.filter((c, i) => c.startsWith('goto ') && calls[i + 1]?.startsWith('place item=oak_planks')).map(c => Object.fromEntries(c.split(' ').slice(1).map(kv => kv.split('=')).map(([k, v]) => [k, Number(v)])))
  const inside = stands.filter(s => s.x >= AT.x && s.x <= AT.x + 2 && s.z >= AT.z && s.z <= AT.z + 2)
  assert.deepEqual(inside, [])
  assert.equal(stands.length, 9)
})

test('a build stops at dusk with night= before the next job', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: { cobblestone: 64, stone_bricks: 64, oak_planks: 64 } })
  let ticks = 0
  api.clock = () => ({ time: 1000, day: ticks < 6, night: ticks++ >= 6, elapsedDays: 0 })
  const r = await buildBlueprint(api, { name: 'box', place: 'box', ...AT }, io)
  assert.equal(r.night, 'stopped at dusk: run blueprint.build place=box again at dawn')
  assert.equal(r.built < 24, true)
  assert.equal(calls.filter(c => c.startsWith('place ')).length, r.built)
})

test('the blueprint file changing after the mark is reported, not followed', async () => {
  const world = worldOf()
  const stale = { ...hutPlace, note: buildNote({ blueprint: 'starter-hut', facing: 'south', params: {}, hash: 'deadbeef' }) }
  const { api, calls } = body({ world, items: hutKit(), places: [stale] })
  await assert.rejects(buildBlueprint(api, { place: 'hut' }, io), { message: /^the blueprint changed since this build started: hut was marked from starter-hut h=deadbeef and the file is now h=[0-9a-f]{8}; run blueprint.build name=starter-hut x=100 y=65 z=-20 place=hut to go on with the new version$/ })
  assert.deepEqual(calls, [])
})

// marks made before the hash covered only what a build depends on carry the whole file's hash: those of the library's
// own versions still resume (hollis-hut and pacer-hut hold h=09b5894a, the starter-hut of 09-26)
test('a mark carrying the old whole-file hash of the same hut resumes, and a stranger hash is still refused', async () => {
  const world = worldOf(halfBuilt())
  const legacy = { ...hutPlace, note: buildNote({ blueprint: 'starter-hut', facing: 'south', params: {}, hash: '09b5894a' }) }
  const { api } = body({ world, items: hutKit(), places: [legacy] })
  const r = await buildBlueprint(api, { place: 'hut' }, io)
  assert.equal(r.skipped, 60)
  const older = { ...hutPlace, note: buildNote({ blueprint: 'starter-hut', facing: 'south', params: {}, hash: 'c0596f57' }) }
  const second = body({ world: worldOf(halfBuilt()), items: hutKit(), places: [older] })
  await assert.rejects(buildBlueprint(second.api, { place: 'hut' }, io), { message: /^the blueprint changed since this build started: hut was marked from starter-hut h=c0596f57/ })
})

test('name= and the anchor restated over the stale mark start afresh and mark it again', async () => {
  const world = worldOf(halfBuilt())
  const stale = { ...hutPlace, note: buildNote({ blueprint: 'starter-hut', facing: 'south', params: {}, hash: 'deadbeef' }) }
  const { api, calls } = body({ world, items: hutKit(), places: [stale] })
  const r = await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.equal(r.built, 42)
  assert.equal(calls[0], `mark name=hut kind=build x=100 y=65 z=-20 note=bp=starter-hut f=south h=${blueprintHash(HUT)}`)
})

test("somebody else's place is refused by name", async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: hutKit(), places: [{ ...hutPlace, by: 'Steve' }] })
  await assert.rejects(buildBlueprint(api, { place: 'hut' }, io), { message: /^hut is Steve's ground and the note on it does not invite work/ })
  assert.deepEqual(calls, [])
})

test("somebody else's build goes on when its note invites work, and its kind is left to them", async () => {
  const world = worldOf(halfBuilt())
  const { api, calls } = body({ world, items: hutKit(), places: [{ ...hutPlace, by: 'Steve', note: `${hutPlace.note} anyone welcome` }] })
  const r = await buildBlueprint(api, { place: 'hut' }, io)
  assert.equal(r.built, 42)
  assert.deepEqual(calls.filter(c => c.startsWith('mark ')), [])
})

test("somebody else's plain place is not renamed into a build", async () => {
  const { api, calls } = body({ world: worldOf(), items: hutKit(), places: [{ name: 'hut', kind: 'farm', by: 'Steve', x: 1, y: 2, z: 3, note: 'anyone welcome' }] })
  await assert.rejects(buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io), { message: /^hut is on the shared map as Steve's/ })
  assert.deepEqual(calls, [])
})

test("the hut's floor torch is clicked onto the floor under it, never a wall beside it", async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: hutKit() })
  await buildBlueprint(api, { name: 'starter-hut', place: 'hut', ...AT }, io)
  assert.deepEqual(calls.filter(c => c.startsWith('place item=torch ')), [`place item=torch x=${AT.x + 3} y=${AT.y} z=${AT.z + 3} against=down`])
})

// a beam: a log on its side between two plank posts is clicked onto whichever post stands along its axis, after them
const BEAM = `---
name: beam
title: Beam
description: a log beam between two posts
tags: decorative
front: south
foundation: flat
---

\`\`\`legend
L  oak_log[axis=x]
P  oak_planks
\`\`\`

## y0

\`\`\`layer
PLP
\`\`\`
`
const beamIo = { read: name => ({ name, text: BEAM, hash: blueprintHash(BEAM) }) }
test('a log on its side is clicked onto the block beside it along its axis, placed after it', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: { oak_log: 4, oak_planks: 4 } })
  await buildBlueprint(api, { name: 'beam', place: 'beam', ...AT }, beamIo)
  const log = calls.findIndex(c => c.startsWith('place item=oak_log '))
  assert.deepEqual([calls[log], log > calls.findLastIndex(c => c.startsWith('place item=oak_planks '))], [`place item=oak_log x=${AT.x + 1} y=${AT.y} z=${AT.z} against=east`, true])
})

test('a log on its side with nothing along its axis is named, not placed the wrong way', async () => {
  const world = worldOf()
  const { api, calls } = body({ world, items: { oak_log: 4 } })
  const r = await buildBlueprint(api, { name: 'beam', place: 'beam', partial: true, ...AT }, beamIo)
  assert.deepEqual([calls.filter(c => c.startsWith('place item=oak_log ')), r.stuck], [[], `place ${AT.x + 1},${AT.y},${AT.z}: oak_log on its side (axis=x) is clicked onto a block beside it along x, and neither side holds one yet: run blueprint.build place=beam again once one stands`])
})

// the shaft's ladder hangs on the wall north of it: place is told which neighbour to click (against=), and the ladder
// takes its facing from that face (Hollis, 09-26: the watchtower was refused on every facing)
test('the watchtower builds, its ladder placed against the wall behind it', async () => {
  const world = worldOf()
  const tower = fs.readFileSync(path.join(BLUEPRINT_DIR, 'watchtower.md'), 'utf8')
  const files = { watchtower: tower }
  const kit = { cobblestone: 128, jack_o_lantern: 1, ladder: 16, oak_door: 1, oak_planks: 32, oak_trapdoor: 1, oak_fence: 16, torch: 4, dirt: 64 }
  const { api, calls } = body({ world, items: kit })
  await buildBlueprint(api, { name: 'watchtower', place: 'tower', partial: true, ...AT }, { read: name => ({ name, text: files[name], hash: blueprintHash(files[name]) }) })
  assert.equal(calls.includes(`place item=ladder x=${AT.x + 2} y=${AT.y} z=${AT.z + 2} against=north`), true)
  assert.deepEqual(calls.filter(c => c.startsWith('place item=ladder ')).filter(c => !c.endsWith(' against=north')), [])
})

test('a new build needs a place name, a blueprint and an anchor', async () => {
  const { api } = body({ world: worldOf(), items: hutKit() })
  await assert.rejects(buildBlueprint(api, { name: 'starter-hut', ...AT }, io), { message: 'blueprint.build needs place=<a name for this build on the shared map>' })
  await assert.rejects(buildBlueprint(api, { place: 'hut' }, io), { message: 'blueprint.build needs name= x= y= z= (a blueprint at its anchor), or place=<a build already marked>' })
  await assert.rejects(buildBlueprint(api, { name: 'starter-hut', place: 'hut', x: 1, y: 2 }, io), { message: 'blueprint.build needs z=: the anchor is the north-west corner of the y0 layer' })
})

test('check prints the stage table, the site and no refusal over clear ground', async () => {
  const { api, calls } = body({ world: worldOf(), items: { cobblestone: 8 }, freeSlots: 3 })
  const r = await checkBlueprint(api, { name: 'box', ...AT }, io)
  assert.deepEqual(calls, [])
  assert.deepEqual(r.text.split('\n'), [
    'box at 100,65,-20 facing=south 3x3, y0..y2, 25 items',
    'stage 1/3 y0 carry=cobblestone:8 have=all',
    'stage 2/3 y1 carry=stone_bricks:8 short=stone_bricks:8',
    'stage 3/3 y2 carry=oak_planks:9 short=oak_planks:9',
    'obstacles=0 foundation=ok clearance=ok overlaps=0 unloaded=0',
    'warning: the room at y0 1..1,1..1 has 1 cell at light 0: add a light source',
    'ok: build would start'
  ])
})

test('check names what build would refuse over', async () => {
  const world = worldOf({ [key(AT.x + 1, AT.y, AT.z)]: 'oak_planks', [key(AT.x, AT.y + 3, AT.z)]: 'oak_leaves' })
  const { api } = body({ world, items: { cobblestone: 64, stone_bricks: 64, oak_planks: 64 } })
  const r = await checkBlueprint(api, { name: 'box', ...AT }, io)
  assert.equal(r.text.split('\n').find(l => l.startsWith('obstacles=')), `obstacles=1 (oak_planks ${AT.x + 1},${AT.y},${AT.z}) foundation=ok clearance=ok overlaps=0 unloaded=0`)
  assert.equal(r.text.split('\n').at(-1), `refusal: oak_planks at ${AT.x + 1},${AT.y},${AT.z} is in the way of S: dig it, or run again with clear=true`)
})

test('check over a marked build reads the anchor off the map', async () => {
  const { api } = body({ world: worldOf(halfBuilt()), items: hutKit(), places: [hutPlace] })
  const r = await checkBlueprint(api, { place: 'hut' }, io)
  assert.equal(r.text.split('\n')[0], 'starter-hut at 100,65,-20 facing=south 5x5, y-1..y3, 42 items (place hut, 60 stand)')
})

const PARAM_ROWS = [
  [{ name: 'starter-hut', place: 'hut', x: 1, y: 2, z: 3, wood: 'spruce', clear: true }, { wood: 'spruce' }],
  [{ place: 'hut', supply: '1,2,3', partial: true }, {}],
  [{ name: 'x', x: 1, y: 2, z: 3, bed: 'red', stone: 'stone_bricks', timeout: 5 }, { bed: 'red', stone: 'stone_bricks' }]
]
for (const [given, want] of PARAM_ROWS) {
  test(`paramsOf ${JSON.stringify(given)} -> ${JSON.stringify(want)}`, () => {
    assert.deepEqual(paramsOf(given), want)
  })
}

const SUPPLY_ROWS = [
  ['98,65,-14', [], { x: 98, y: 65, z: -14, label: '98,65,-14' }],
  ['depot', [{ name: 'depot', x: 1, y: 2, z: 3 }], { x: 1, y: 2, z: 3, label: '1,2,3 (depot)' }],
  [undefined, [], null]
]
for (const [supply, places, want] of SUPPLY_ROWS) {
  test(`supplyOf ${supply} -> ${JSON.stringify(want)}`, () => {
    const { api } = fakeApi({ places })
    assert.deepEqual(supplyOf(api, supply), want)
  })
}

test('supplyOf refuses a name nobody marked', () => {
  const { api } = fakeApi({})
  assert.throws(() => supplyOf(api, 'depot'), { message: 'supply=depot is neither x,y,z nor a marked place: ./mc places lists them' })
})

test('the library reads every blueprint file by name and refuses one that is not there', () => {
  assert.deepEqual(blueprintFiles(), ['starter-hut', 'watchtower', 'wheat-field'])
  assert.equal(readBlueprint('starter-hut').hash, blueprintHash(HUT))
  assert.throws(() => readBlueprint('castle'), { message: 'no blueprint called castle: blueprint.list shows starter-hut, watchtower, wheat-field' })
})

const LIBRARY = [{ name: 'starter-hut', text: HUT }, { name: 'box', text: BOX }, { name: 'broken', text: 'front: south' }]
const LIST_ROWS = [
  [{}, ['starter-hut 5x5x5 tags=shelter,storage items=102 Starter hut', 'box 3x3x3 tags=storage items=25 Box', 'broken: does not parse (the file must start with a --- front matter block)']],
  [{ tag: 'shelter' }, ['starter-hut 5x5x5 tags=shelter,storage items=102 Starter hut']],
  [{ q: 'lid' }, ['box 3x3x3 tags=storage items=25 Box']],
  [{ tag: 'farm' }, ['no blueprint matches tag=farm: starter-hut, box, broken']]
]
for (const [a, want] of LIST_ROWS) {
  test(`blueprint.list ${JSON.stringify(a)}`, () => {
    assert.deepEqual(listText(a, LIBRARY).split('\n'), want)
  })
}

test('show renders the metadata, the bill, the stages for what is carried and the turned layers', () => {
  const { api } = fakeApi({ items: { cobblestone: 8, stone_bricks: 8 }, freeSlots: 27 })
  assert.deepEqual(showText(api, { name: 'box', facing: 'east', layer: 2 }, read).split('\n'), [
    'box: Box (3x3x3, y0..y2, front=south facing=east, foundation=flat, clearance=0)',
    'a closed stone box with a plank lid',
    'tags=storage',
    'doors=0 beds=0 containers=0 workstations=0 lights=0 enclosed=2 lit=false spawnSafe=false',
    'y0: cobblestone:8',
    'y1: stone_bricks:8',
    'y2: oak_planks:9',
    'total: cobblestone:8 stone_bricks:8 oak_planks:9',
    'stage 1/1 y0..y2 carry=cobblestone:8 stone_bricks:8 oak_planks:9 short=oak_planks:9',
    'warning: the room at y0 1..1,1..1 has 1 cell at light 0: add a light source',
    'y2',
    'PPP',
    'PPP',
    'PPP'
  ])
})

test('show refuses a parameter the blueprint does not declare, by its own list', () => {
  const { api } = fakeApi({})
  assert.throws(() => showText(api, { name: 'starter-hut', metal: 'iron' }, read), { message: 'metal= is not a parameter of starter-hut (it has wood, stone, bed)' })
})
