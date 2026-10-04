// The air a body reads on dry-looking ground (card 962beec2: oxygen 18 -> 5 standing still at 119,72,-66, walks from
// there frozen with a valid path). The air number is server-sent entity metadata, so a drain on dry land means the
// server sees the head somewhere else than the client does: this is the evidence line for that, and the honest reading
// of a body that has no air number yet. Pure: nothing here touches a body.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { airSample, freshAir, serverPosNote, patchOwnBreath } from '../src/survival/airlog.mjs'

const here = { x: 119.52, y: 72, z: -65.5 }
const reading = { oxygen: 20, health: 20, client: here, server: null, now: 100000, head: 'air', inWater: false }

for (const [name, memory, sample, expected] of [
  ['no air number yet (mineflayer leaves it undefined before the first metadata): nothing to say', freshAir, { ...reading, oxygen: undefined }, null],
  ['the first number, still settling: said, with settling', freshAir, { ...reading, oxygen: 18 },
    { oxygen: 18, prev: null, health: 20, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: true }],
  ['the first number is a full bar: settled at once', freshAir, reading,
    { oxygen: 20, prev: null, health: 20, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: false }],
  ['the same number again: silence', { oxygen: 18, health: 20, settled: false }, { ...reading, oxygen: 18 }, null],
  ['a drop after a full bar: a drain, with the previous number', { oxygen: 20, health: 20, settled: true }, { ...reading, oxygen: 17 },
    { oxygen: 17, prev: 20, health: 20, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: false }],
  ['a drop before any full bar, but after damage: no longer settling', { oxygen: 12, health: 20, settled: false }, { ...reading, oxygen: 10, health: 18 },
    { oxygen: 10, prev: 12, health: 18, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: false }],
  ['a drop while still settling, no damage: still settling', { oxygen: 12, health: 20, settled: false }, { ...reading, oxygen: 10 },
    { oxygen: 10, prev: 12, health: 20, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: true }],
  ['the server position and its age ride along, with the head block and the wet flag', { oxygen: 20, health: 20, settled: true },
    { ...reading, oxygen: 5, server: { x: 111.5, y: 62.9, z: -70.2, at: 40000 }, head: 'water', inWater: true },
    { oxygen: 5, prev: 20, health: 20, client: '119.52,72,-65.5', server: '111.5,62.9,-70.2', serverAgeMs: 60000, head: 'water', inWater: true, settling: false }],
  ['a recovery is a change too', { oxygen: 5, health: 20, settled: true }, { ...reading, oxygen: 12 },
    { oxygen: 12, prev: 5, health: 20, client: '119.52,72,-65.5', server: null, serverAgeMs: null, head: 'air', inWater: false, settling: false }]
]) {
  test(`airSample event: ${name}`, () => assert.deepEqual(airSample({ memory, ...sample }).event, expected))
}

for (const [name, memory, sample, expected] of [
  ['a fresh body keeps nothing from an undefined reading', freshAir, { ...reading, oxygen: undefined }, { oxygen: undefined, health: 20, settled: false }],
  ['a full bar settles the memory for good', freshAir, reading, { oxygen: 20, health: 20, settled: true }],
  ['a settled memory stays settled through a drain', { oxygen: 20, health: 20, settled: true }, { ...reading, oxygen: 3 }, { oxygen: 3, health: 20, settled: true }],
  ['damage settles it', { oxygen: 12, health: 20, settled: false }, { ...reading, oxygen: 12, health: 19 }, { oxygen: 12, health: 19, settled: true }],
  ['healing is not damage', { oxygen: 12, health: 18, settled: false }, { ...reading, oxygen: 12, health: 20 }, { oxygen: 12, health: 20, settled: false }]
]) {
  test(`airSample memory: ${name}`, () => assert.deepEqual(airSample({ memory, ...sample }).memory, expected))
}

// the state line: the server's last position, only when it disagrees with the client by more than a block AND the packet is
// under five seconds old. After a `position` packet the client walks on from where the server put it, so an old packet's distance
// is only how far the body has walked since (Perrin: serverPos=30,66,108 age=404s while standing at 111,70,-136); the spawn
// packet is the only one most bodies ever get, and a bare age clause showed it for ever. A minute was still too long: Jizo's
// spawn packet showed as serverPos=7.5,63,-88.5 age=49s after two walks (09-26 23:18Z, one forcedMove in the log). A real
// rubber band re-sends every tick the body pushes against it, so it is never more than a second old
for (const [name, server, expected] of [
  ['never corrected: nothing', null, null],
  ['the server agrees and spoke lately: nothing', { x: 119.6, y: 72, z: -65.4, at: 90000 }, null],
  ['the server put the body more than a block away, 2 s ago', { x: 121.5, y: 72, z: -65.5, at: 98000 }, { serverPos: '121.5,72,-65.5', age: '2s' }],
  ['five seconds old and off: still shown', { x: 121.5, y: 72, z: -65.5, at: 95000 }, { serverPos: '121.5,72,-65.5', age: '5s' }],
  ['six seconds old and off (the body walked on since): nothing', { x: 121.5, y: 72, z: -65.5, at: 94000 }, null],
  ['the spawn packet 49 s after, the body walked off it: nothing', { x: 107.5, y: 63, z: -88.5, at: 51000 }, null],
  ['a level lower counts too', { x: 119.5, y: 70.5, z: -65.5, at: 99000 }, { serverPos: '119.5,70.5,-65.5', age: '1s' }],
  ['a minute old and off: nothing', { x: 121.5, y: 72, z: -65.5, at: 40000 }, null],
  ['the last packet is old but agrees: nothing', { x: 119.5, y: 72, z: -65.5, at: 30000 }, null],
  ['two days old and off (the spawn packet of a long-running body): nothing', { x: 121.5, y: 72, z: -65.5, at: -160333731 }, null]
]) {
  test(`serverPosNote: ${name}`, () => assert.deepEqual(serverPosNote({ client: here, server, now: 100000 }), expected))
}

// mineflayer 4.39.0 lib/plugins/entities.js: the air_supply of EVERY entity's metadata packet lands in bot.oxygenLevel
// (breath.js used to check the entity id before it moved in here). Standing beside a pond a body reads a glow squid's
// air (8) and a swimmer's (20) by turns: Sancho at -145,60,-195, 41 packets in a minute, none its own
const BREATH_ANY = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null) {\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
const BREATH_OWN = "      // Breathing (formerly in breath.js)\n      if (metas.air_supply != null && entity === bot.entity) { // patched by bot/patch-deps.mjs: my own air, not every swimmer's\n        bot.oxygenLevel = Math.round(metas.air_supply / 15)\n"
for (const [name, source, expected] of [
  ['the upstream source gets the entity check', `a\n${BREATH_ANY}b`, { status: 'patched', source: `a\n${BREATH_OWN}b` }],
  ['patched already: left alone', `a\n${BREATH_OWN}b`, { status: 'already', source: `a\n${BREATH_OWN}b` }],
  ['a new upstream version without that code: say so, change nothing', 'something else', { status: 'anchor missing', source: 'something else' }]
]) {
  test(`patchOwnBreath: ${name}`, () => assert.deepEqual(patchOwnBreath(source), expected))
}
