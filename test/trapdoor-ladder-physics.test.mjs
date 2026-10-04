import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import Module, { createRequire } from 'node:module'
import { patchClimbableTrapdoor, TRAPDOOR_FEATURE, TRAPDOOR_ALWAYS } from '../tools/dependency-patches/trapdoor-ladder.mjs'

const require = createRequire(import.meta.url)
const physicsFile = require.resolve('prismarine-physics')
// as installed: tools/patch-deps.mjs may already have patched it
const source = fs.readFileSync(physicsFile, 'utf8').replace(TRAPDOOR_ALWAYS, TRAPDOOR_FEATURE)
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
const patched = physicsExports(patchClimbableTrapdoor(source).source)

// a ladder (facing south, on the stone north of it) at y 0..1, a trapdoor at y 2 over it, stone at z -1
const hatch = (trapdoor, ladder = 'south') => ({
  getBlock (raw) {
    const p = raw.floored()
    const [name, properties] = p.x !== 0 || p.y < 0 ? ['stone', {}]
      : p.z === -1 && p.y < 2 ? ['stone', {}]
        : p.z === 0 && p.y < 2 ? ['ladder', { facing: ladder, waterlogged: false }]
          : p.z === 0 && p.y === 2 ? ['oak_trapdoor', { half: 'bottom', powered: false, waterlogged: false, ...trapdoor }]
            : ['air', {}]
    const block = Block.fromProperties(name, properties, 0)
    block.position = p
    return block
  }
})

// the highest the feet get in 100 ticks of holding jump at the ladder's foot
function highest (engine, world, pressed = {}) {
  const bot = {
    version: VERSION,
    entity: { position: new Vec3(0.5, 0, 0.5), velocity: new Vec3(0, 0, 0), onGround: true, effects: {}, attributes: {}, yaw: 0, pitch: 0 },
    inventory: { slots: [] }, jumpTicks: 0, jumpQueued: false, fireworkRocketDuration: 0
  }
  const control = { forward: false, back: false, left: false, right: false, jump: true, sprint: false, sneak: false, ...pressed }
  const state = new engine.PlayerState(bot, control)
  const physics = engine.Physics(registry, world)
  let top = 0
  for (let i = 0; i < 100; i++) {
    physics.simulatePlayer(state, world)
    top = Math.max(top, state.pos.y)
  }
  return top
}

test('the trapdoor patch is idempotent and leaves an unknown source alone', () => {
  const result = patchClimbableTrapdoor(source)
  assert.equal(result.status, 'patched')
  assert.equal(patchClimbableTrapdoor(result.source).status, 'already')
  const unknown = 'function unrelated () {}'
  assert.deepEqual(patchClimbableTrapdoor(unknown), { status: 'anchor missing', source: unknown })
})

// vanilla (LivingEntity.trapdoorUsableAsLadder): an open trapdoor over a ladder of its own facing is climbed; the feature
// list prismarine-physics ships stops at 1.20, so on 26.1 the body stopped with its feet at the trapdoor's floor (y 2).
// climbing ends with the feet a little over the last climbable cell
// [what, ladder facing, trapdoor, highest feet unpatched, patched]
const climbs = [
  ['an open trapdoor facing like the ladder: climbed', 'south', { open: true, facing: 'south' }, 2, 3],
  ['an open trapdoor facing away from the ladder: not climbed', 'south', { open: true, facing: 'north' }, 2, 2],
  ['a shut trapdoor: a ceiling (head at y 2)', 'south', { open: false, facing: 'south' }, 0.19, 0.19],
  ['east ladder, east trapdoor: climbed', 'east', { open: true, facing: 'east' }, 2, 3],
  ['east ladder, west trapdoor: not climbed', 'east', { open: true, facing: 'west' }, 2, 2],
  ['west ladder, west trapdoor: climbed', 'west', { open: true, facing: 'west' }, 2, 3],
  ['west ladder, east trapdoor: not climbed', 'west', { open: true, facing: 'east' }, 2, 2]
]
for (const [what, ladder, trapdoor, before, after] of climbs) {
  test(`physics at a hatch, ${what}`, () => {
    const world = hatch(trapdoor, ladder)
    const tops = [highest(baseline, world), highest(patched, world)]
    assert.ok(tops[0] >= before && tops[0] < before + 0.4 && tops[1] >= after && tops[1] < after + 0.4, `${tops}`)
  })
}

// over a ladder of another facing the trapdoor's panel leaves the ladder's top edge free: a body pressed to the ladder's
// wall (yaw 0 faces north) lands on that edge and jumps off it, its feet a jump over the trapdoor's floor
test('physics at a hatch facing away from the ladder: pressed to the wall, the body stands on the ladder top and jumps', () => {
  const world = hatch({ open: true, facing: 'north' })
  assert.ok(highest(patched, world, { forward: true }) > 3.1)
  assert.ok(highest(baseline, world, { forward: true }) > 3.1)
})
