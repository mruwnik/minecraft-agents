import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { stalkShape, groveExit, steer } from '../src/navigation/bamboo.mjs'

const require = createRequire(import.meta.url)
const VERSION = '1.21.5'
const registry = require('minecraft-data')(VERSION)
const Block = require('prismarine-block')(registry)
const { Physics, PlayerState } = require('prismarine-physics')
const { Vec3 } = require('vec3')

const close = (a, b) => Math.abs(a - b) < 1e-9

// minecraft-data's single fixed bamboo shape is the offset at the origin, where the server's seed is 0: an
// independent check that the formula (not just a transcription of minecraft-data) lands on the known block-local box
test('stalkShape: at the origin matches minecraft-data\'s fixed shape', () => {
  assert.deepEqual(stalkShape(0, 0), [0.15625, 0, 0.15625, 0.34375, 1, 0.34375])
})

for (const [name, x, z, expected] of [
  ['8,-43', 8, -43, [0.18958333333333333, 0, 0.45625, 0.3770833333333333, 1, 0.64375]],
  ['9,-43', 9, -43, [0.4895833333333333, 0, 0.18958333333333333, 0.6770833333333333, 1, 0.3770833333333333]]
]) {
  test(`stalkShape: ${name} pins to the formula's own offset`, () => {
    const shape = stalkShape(x, z)
    assert.ok(shape.every((v, i) => close(v, expected[i])), `${shape} !~ ${expected}`)
  })
}

// the 2026-10-01 incident: the server put a 0.6-wide body here, inside what looked (to the client's one fixed shape)
// like a dense grove; under the server's real per-cell offsets the body's box clears every stalk around it
test('stalkShape: the incident spot overlaps none of the 9 stalks around it', () => {
  const half = 0.3
  const bodyX = [8.692235292704533 - half, 8.692235292704533 + half]
  const bodyZ = [-42.5 - half, -42.5 + half]
  const overlaps = (x, z) => {
    const [minX, , minZ, maxX, , maxZ] = stalkShape(x, z)
    return minX + x < bodyX[1] && maxX + x > bodyX[0] && minZ + z < bodyZ[1] && maxZ + z > bodyZ[0]
  }
  const cells = [7, 8, 9].flatMap(x => [-44, -43, -42].map(z => [x, z]))
  assert.equal(cells.filter(([x, z]) => overlaps(x, z)).length, 0)
})

// the incident's grove: dense bamboo over x 5..11, z -46..-40 on a stone floor
const inGrove = (x, z) => x >= 5 && x <= 11 && z >= -46 && z <= -40
const groveWorld = { getBlock (raw) {
  const p = raw.floored()
  const name = p.y < 0 ? 'stone' : p.y < 4 && inGrove(p.x, p.z) ? 'bamboo' : 'air'
  const block = Block.fromProperties(name, { age: 0, leaves: 'none', stage: 0 }, 0)
  block.position = p
  if (name === 'bamboo') block.shapes = [stalkShape(p.x, p.z)]
  return block
} }
const physics = Physics(registry, groveWorld)
const controls = sneak => ({ forward: true, back: false, left: false, right: false, jump: false, sprint: false, sneak })

// the bot's own drive: face the waypoint, forward, sneak near it, at most 40 ticks a waypoint
function walk (start, waypoints) {
  const bot = {
    version: VERSION,
    entity: { position: new Vec3(start.x, 0, start.z), velocity: new Vec3(0, 0, 0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const reached = waypoint => {
    for (let t = 0; t < 40; t++) {
      const { yaw, sneak, arrived } = steer(bot.entity.position, waypoint)
      if (arrived) return true
      bot.entity.yaw = yaw
      const state = new PlayerState(bot, controls(sneak))
      physics.simulatePlayer(state, groveWorld)
      state.apply(bot)
    }
    return steer(bot.entity.position, waypoint).arrived
  }
  waypoints.every(reached)
  return bot.entity.position
}
const boxClearOfGrove = ({ x, z }) => [x - 0.3, x + 0.3].every(bx => [z - 0.3, z + 0.3].every(bz => !inGrove(Math.floor(bx), Math.floor(bz))))

// spots deep in the grove whose 0.6 box clears every stalk (the server would accept a body there), the incident's first
const starts = [
  { x: 8.692235292704533, z: -42.5 },
  { x: 9.05, z: -43.95 },
  { x: 7.05, z: -41.95 },
  { x: 8.05, z: -43.95 }
]
for (const start of starts) {
  test(`groveExit: a body at ${start.x},${start.z} walks itself out of the grove`, () => {
    const waypoints = groveExit({ from: start, bambooAt: inGrove, openAt: () => true })
    assert.ok(boxClearOfGrove(walk(start, waypoints)))
  })
}

test('groveExit: a pocket of bamboo sealed in by stone has no way out', () => {
  const pocket = (x, z) => Math.abs(x) <= 1 && Math.abs(z) <= 1
  assert.equal(groveExit({ from: { x: 0.5, z: 0.5 }, bambooAt: pocket, openAt: pocket }), null)
})
