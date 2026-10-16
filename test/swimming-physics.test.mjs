// Why JavaScript: tests a JS patch to prismarine-physics in node_modules (Mineflayer boundary).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import Module, { createRequire } from 'node:module'
import { patchSwimming, unpatchSwimming } from '../tools/dependency-patches/swimming.mjs'

const require = createRequire(import.meta.url)
const physicsFile = require.resolve('prismarine-physics')
// as installed: tools/patch-deps.mjs may already have patched it
const source = unpatchSwimming(fs.readFileSync(physicsFile, 'utf8'))
const VERSION = '26.1'
const registry = require('minecraft-data')(VERSION)
const Block = require('prismarine-block')(registry)
const { Vec3 } = require('vec3')

function physicsExports (text) {
  const mod = new Module(physicsFile)
  mod.filename = physicsFile
  mod.paths = Module._nodeModulePaths(physicsFile.slice(0, physicsFile.lastIndexOf('/')))
  mod._compile(text, physicsFile)
  return mod.exports
}
const baseline = physicsExports(source)
const patched = physicsExports(patchSwimming(source).source)

// water up to y 99, air above
const sea = {
  getBlock (raw) {
    const p = raw.floored()
    const block = p.y < 100 ? Block.fromProperties('water', { level: 0 }, 0) : Block.fromProperties('air', {}, 0)
    block.position = p
    return block
  }
}

// [horizontal, vertical] blocks a second over the last 100 of 200 ticks, holding forward from feet y
function swim (engine, { sprint = true, pitch = 0, y = 50 } = {}) {
  const bot = {
    version: VERSION,
    entity: { position: new Vec3(0.5, y, 0.5), velocity: new Vec3(0, 0, 0), onGround: false, effects: {}, attributes: {}, yaw: 0, pitch },
    inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const control = { forward: true, back: false, left: false, right: false, jump: false, sprint, sneak: false }
  const state = new engine.PlayerState(bot, control)
  const physics = engine.Physics(registry, sea)
  let from = null
  for (let i = 0; i < 200; i++) {
    if (i === 100) from = state.pos.clone()
    physics.simulatePlayer(state, sea)
  }
  return [Math.hypot(state.pos.x - from.x, state.pos.z - from.z) / 5, (state.pos.y - from.y) / 5]
}

const near = (actual, expected, tol = 0.05) => assert.ok(Math.abs(actual - expected) < tol, `${actual}, expected ${expected}`)

test('the swimming patch is idempotent, undone by unpatchSwimming, and leaves an unknown source alone', () => {
  const result = patchSwimming(source)
  assert.equal(result.status, 'patched')
  assert.equal(patchSwimming(result.source).status, 'already')
  assert.equal(unpatchSwimming(result.source), source)
  const unknown = 'function unrelated () {}'
  assert.deepEqual(patchSwimming(unknown), { status: 'anchor missing', source: unknown })
})

test('unpatched, a sprint under water swims no faster than a paddle', () => {
  near(swim(baseline)[0], swim(baseline, { sprint: false })[0])
})

// vanilla LivingEntity water travel: sprinting, the water inertia is 0.9 (not 0.8) and gravity is off, so the forward
// acceleration 0.02 x 0.98 settles at 3.92 b/s; a paddle (no sprint) at 1.96
test('patched, a sprint under water swims level at 3.92 blocks a second', () => {
  const [h, v] = swim(patched)
  near(h, 3.92)
  near(v, 0, 0.01)
  near(swim(patched, { sprint: false })[0], 1.96)
})

// vanilla Player.travel: swimming, vy moves toward the look's y by 0.085 a tick looking down (0.06 up) before the move,
// then the water's 0.8: about 6.34 b/s straight down, 4.84 up
test('patched, the look pitch steers a swim down and up', () => {
  near(swim(patched, { pitch: -Math.PI / 2 })[1], -6.34, 0.1)
  near(swim(patched, { pitch: Math.PI / 2, y: -100 })[1], 4.84, 0.1)
})

// a sprint starts a swim only with the eyes (1.62 up) under: with them out, the first tick sinks as a paddle does
test('patched, a sprint with the eyes out of the water is no swim', () => {
  const tick = y => {
    const bot = {
      version: VERSION,
      entity: { position: new Vec3(0.5, y, 0.5), velocity: new Vec3(0, 0, 0), onGround: false, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
      inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
    }
    const state = new patched.PlayerState(bot, { forward: true, back: false, left: false, right: false, jump: false, sprint: true, sneak: false })
    patched.Physics(registry, sea).simulatePlayer(state, sea)
    return state.vel.y
  }
  assert.equal(tick(50), 0)
  assert.ok(tick(98.9) < 0)
})
