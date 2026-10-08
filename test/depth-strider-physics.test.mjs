// Why JavaScript: tests a JS patch to prismarine-physics in node_modules (Mineflayer boundary).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import Module, { createRequire } from 'node:module'
import { patchDepthStrider, BOOTS_NBT, STRIDER_ACCEL, BOOTS_ENCHANTS, STRIDER_ACCEL_WALK } from '../tools/dependency-patches/depth-strider.mjs'

const require = createRequire(import.meta.url)
const physicsFile = require.resolve('prismarine-physics')
// as installed: tools/patch-deps.mjs may already have patched it
const source = fs.readFileSync(physicsFile, 'utf8').replace(BOOTS_ENCHANTS, BOOTS_NBT).replace(STRIDER_ACCEL_WALK, STRIDER_ACCEL)
const VERSION = '26.1'
const registry = require('minecraft-data')(VERSION)
const Block = require('prismarine-block')(registry)
const Item = require('prismarine-item')(VERSION)
const { Vec3 } = require('vec3')

function physicsExports (text) {
  const mod = new Module(physicsFile)
  mod.filename = physicsFile
  mod.paths = Module._nodeModulePaths(physicsFile.slice(0, physicsFile.lastIndexOf('/')))
  mod._compile(text, physicsFile)
  return mod.exports
}
const baseline = physicsExports(source)
const patched = physicsExports(patchDepthStrider(source).source)

// water everywhere: the body swims off the ground the whole time
const sea = {
  getBlock (raw) {
    const block = Block.fromProperties('water', { level: 0 }, 0)
    block.position = raw.floored()
    return block
  }
}

// boots as a 26.1 server sends them: enchantments in the item's components, no nbt
function boots (level) {
  const item = new Item(registry.itemsByName.diamond_boots.id, 1)
  item.componentMap = new Map([['enchantments', { type: 'enchantments', data: [{ name: 'depth_strider', level }] }]])
  return item
}

// blocks a second swum horizontally, holding forward, over the last 100 of 200 ticks
function speed (engine, level) {
  const bot = {
    version: VERSION,
    entity: { position: new Vec3(0.5, 10, 0.5), velocity: new Vec3(0, 0, 0), onGround: false, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: Object.assign([], { 8: level ? boots(level) : null }) }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const control = { forward: true, back: false, left: false, right: false, jump: false, sprint: false, sneak: false }
  const state = new engine.PlayerState(bot, control)
  const physics = engine.Physics(registry, sea)
  let from = null
  for (let i = 0; i < 200; i++) {
    if (i === 100) from = state.pos.clone()
    physics.simulatePlayer(state, sea)
  }
  return Math.hypot(state.pos.x - from.x, state.pos.z - from.z) / 5
}

test('the depth strider patch is idempotent and leaves an unknown source alone', () => {
  const result = patchDepthStrider(source)
  assert.equal(result.status, 'patched')
  assert.equal(patchDepthStrider(result.source).status, 'already')
  const unknown = 'function unrelated () {}'
  assert.deepEqual(patchDepthStrider(unknown), { status: 'anchor missing', source: unknown })
})

test('unpatched, 26.1 boots (components, no nbt) never speed the swim', () => {
  assert.ok(Math.abs(speed(baseline, 3) - speed(baseline, 0)) < 0.01)
})

// vanilla LivingEntity water travel: inertia 0.8 and acceleration 0.02 move toward 0.546 and the walking speed 0.1 by
// level/3, halved off the ground: 1.96 b/s bare, about 2.70, 3.21 and 3.60 b/s at levels 1-3 (jobs.lib.cost.planner/swim-s)
const expected = [[0, 1.96], [1, 2.70], [2, 3.21], [3, 3.60], [5, 3.60]]
for (const [level, bps] of expected) {
  test(`patched, depth strider ${level} swims ${bps} blocks a second`, () => {
    assert.ok(Math.abs(speed(patched, level) - bps) < 0.05, `${speed(patched, level)}`)
  })
}
