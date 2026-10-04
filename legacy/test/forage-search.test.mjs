import test from 'node:test'
import assert from 'node:assert/strict'
import search from '../library/forage/search.mjs'
import { searchWaypoints, searchSurface, connectedFrontiers, outwardCandidates } from '../src/forage.mjs'
import { CompositeHandBack } from '../src/composite.mjs'
import { createRequire } from 'node:module'
import { performance } from 'node:perf_hooks'

test('connected ground cannot step down through an overhang at the old head height', () => {
  const world = (x, y, z) => {
    const solid = z !== 0 || x < 0 || x > 1 || (x === 0 ? y < 64 : y < 63 || y >= 65)
    return { name: solid ? 'stone' : 'air', solid }
  }
  assert.equal(connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 1, z: 0 }]).goals.length, 0)
})

const ground = (x, y, z) => ({ name: y === 63 ? 'stone' : 'air', properties: {}, solid: y === 63 })
function fake (respond) {
  let position = { x: 0, y: 64, z: 0 }
  const calls = []
  return { calls, pos: () => position, block: ground, checkpoint: async () => {}, report: () => {}, async act (name, args) {
    calls.push({ name, args })
    const result = await respond?.(name, args, calls)
    if (name === 'goto') position = { x: args.x, y: args.y, z: args.z }
    return result ?? (name === 'find_blocks' ? { positions: [] } : { found: [] })
  } }
}

test('patterns are bounded, distinct, deterministic, and respect the circular radius', () => {
  for (const pattern of ['spiral', 'sweep']) {
    const points = searchWaypoints({ x: 0, z: 0 }, { pattern, radius: 35, spacing: 8, steps: 128 })
    assert.ok(points.length > 12)
    assert.equal(new Set(points.map(p => `${p.x},${p.z}`)).size, points.length)
    assert.ok(points.every(p => Math.hypot(p.x, p.z) <= 35))
    assert.deepEqual(points.slice(0, 3), searchWaypoints({ x: 0, z: 0 }, { pattern, radius: 35, spacing: 8, steps: 3 }))
  }
})
test('surface changes elevation and refuses hazards, unloaded ground, and obstructed headroom', () => {
  assert.deepEqual(searchSurface(ground, 1, 2, 64), { x: 1, y: 64, z: 2 })
  assert.equal(searchSurface(() => null, 0, 0, 64), null)
  assert.equal(searchSurface((x, y) => ({ name: y === 63 ? 'lava' : 'air', properties: {}, solid: true }), 0, 0, 64), null)
  assert.equal(searchSurface((x, y) => ({ name: y === 64 ? 'short_grass' : y <= 63 || y >= 65 ? 'stone' : 'air', properties: {}, solid: y <= 63 || y >= 65 }), 0, 0, 64), null)
})
test('local discovery avoids walking and uses actual block coordinates', async () => {
  const api = fake(() => ({ positions: [{ x: 1, y: 64, z: 1 }] }))
  const result = await search.run(api, { block: 'rose_bush' })
  assert.equal(result.complete, true)
  assert.equal(result.visited.length, 1)
  assert.equal(api.calls.length, 1)
})
test('search deduplicates observations, walks at surface height without digging, and stops at budget', async () => {
  const api = fake(name => name === 'find_blocks' ? { positions: [{ x: 1, y: 64, z: 1 }] } : undefined)
  const result = await search.run(api, { block: 'bamboo', count: 2, steps: 2 })
  assert.equal(result.found.length, 1)
  assert.equal(result.complete, false)
  assert.equal(result.visited.length, 3)
  assert.ok(api.calls.filter(c => c.name === 'goto').every(c => c.args.y === 64 && c.args.dig === false))
})
test('animal pairs retain ids and never invoke attack or leading commands', async () => {
  const api = fake(name => name === 'animals' ? { found: [{ id: 4, at: '1,64,1' }, { id: 5, at: '1,64,1' }] } : undefined)
  const result = await search.run(api, { mob: 'sheep', count: 2 })
  assert.equal(result.complete, true)
  assert.equal(result.found.length, 2)
  assert.deepEqual(api.calls.map(c => c.name), ['animals'])
})
test('navigation failures are reported but cancellation and hard handbacks escape', async () => {
  const api = fake(name => { if (name === 'goto') throw new Error('no path') })
  const result = await search.run(api, { mob: 'sheep', pattern: 'spiral', steps: 2 })
  assert.equal(result.failures.length, 2)
  assert.equal(result.visited.length, 1)
  for (const error of [new Error('cancelled: no path'), new CompositeHandBack('health'), new TypeError('no path')]) {
    const halted = fake(name => { if (name === 'goto') throw error })
    await assert.rejects(search.run(halted, { mob: 'sheep', steps: 2 }), e => e === error)
  }
  const halted = fake(name => name === 'goto' ? { stopped: 'spoken to' } : undefined)
  await assert.rejects(search.run(halted, { mob: 'sheep' }), CompositeHandBack)
})
test('invalid arguments fail before any action and zero steps means a local scan', async () => {
  const api = fake()
  for (const args of [{}, { block: 'bamboo', mob: 'sheep' }, { mob: 'sheep', steps: Infinity }, { mob: 'sheep', range: 65 }]) await assert.rejects(search.run(api, args))
  assert.equal(api.calls.length, 0)
  await search.run(api, { mob: 'sheep', steps: 0 })
  assert.equal(api.calls.length, 1)
})

test('surface never descends beneath a lake, blocked roof, or unloaded upper column', () => {
  const lake = (x, y) => ({ name: y === 63 ? 'water' : y === 60 ? 'stone' : 'air', properties: {}, solid: y === 60 })
  assert.equal(searchSurface(lake, 0, 0, 64), null)
  const roof = (x, y) => ({ name: y >= 80 || y === 60 ? 'stone' : 'air', properties: {}, solid: y >= 80 || y === 60 })
  assert.equal(searchSurface(roof, 0, 0, 64), null)
  const unknown = (x, y) => y > 63 ? null : ground(x, y, 0)
  assert.equal(searchSurface(unknown, 0, 0, 64), null)
})

test('outward expeditions travel hundreds of blocks using short loaded legs and resume radius', async () => {
  const api = fake()
  const result = await search.run(api, { block: 'bamboo', heading: 'east', radius: 1024, steps: 40 })
  assert.equal(result.distance, 640)
  assert.equal(result.visited.length, 41)
  const walks = api.calls.filter(c => c.name === 'goto')
  for (let i = 0; i < walks.length; i++) assert.equal(walks[i].args.x - (walks[i - 1]?.args.x ?? 0), 16)
  assert.match(result.resume, /heading=east origin=0,64,0 radius=1024/)
  const resumed = await search.run(api, { block: 'bamboo', heading: 'east', radius: 1024, origin: '0,64,0', steps: 4 })
  assert.equal(resumed.distance, 704)
})
test('real timeout advisory is recoverable and tries different nearby goals', async () => {
  const diagnostic = 'forage.search/goto: the search ran out of time (5 s) before it found a way, which is not the same as there being none. The usual cause is a dead end close to the goal (a fenced alley beside a pen gate) that the search keeps trying first, and from inside that dead end even path_to finds nothing. Step back 10-20 blocks the way you came; protected zones must be respected.'
  let first = true
  const api = fake(name => { if (name === 'goto' && first) { first = false; throw new Error(diagnostic) } })
  const acknowledged = []
  api.recoverNavigationFailure = args => { acknowledged.push(args); return true }
  const result = await search.run(api, { mob: 'sheep', steps: 2 })
  assert.equal(acknowledged.length, 1)
  assert.equal(result.failures.length, 1)
  assert.equal(result.visited.length, 3)
  const walks = api.calls.filter(c => c.name === 'goto')
  assert.notDeepEqual(walks[0].args, walks[1].args)
  const refusal = fake(name => { if (name === 'goto') throw new Error('protected zone: no path') })
  await assert.rejects(search.run(refusal, { mob: 'sheep' }), /protected zone/)
  const denied = fake(name => { if (name === 'goto') throw new Error(diagnostic) })
  denied.recoverNavigationFailure = () => false
  await assert.rejects(search.run(denied, { mob: 'sheep' }), /search ran out of time/)
})
test('passable grass headroom is safe and an inaccessible frontier terminates with useful progress', async () => {
  const grassy = (x, y) => ({ name: y === 63 ? 'stone' : y === 64 ? 'short_grass' : 'air', properties: {}, solid: y === 63 })
  assert.deepEqual(searchSurface(grassy, 0, 0, 64), { x: 0, y: 64, z: 0 })
  const api = fake()
  api.block = () => null
  const reports = []
  api.report = report => reports.push(report)
  const result = await search.run(api, { block: 'bamboo' })
  assert.equal(result.reason, 'eight rounds without a reachable new waypoint')
  assert.equal(result.visited.length, 1)
  assert.equal(api.calls.length, 1)
  assert.equal(new Set(result.failures.map(f => `${f.x},${f.z}`)).size, result.failures.length)
  assert.ok(reports.some(r => r.search.includes('found 0/1')))
  assert.ok(reports.at(-1).lastFailure.includes('safe footing'))
})

test('a failed walk that partially moves still recovers toward another safe loaded goal', async () => {
  let position = { x: 0, y: 64, z: 0 }
  let failed = false
  const goals = []
  const api = { pos: () => position, block: ground, checkpoint: async () => {}, report: () => {}, recoverNavigationFailure: () => true, async act (name, args) {
    if (name !== 'goto') return { positions: [] }
    goals.push(args)
    if (!failed) { failed = true; position = { x: 0, y: 64, z: -5 }; throw new Error('forage.search/goto: the search ran out of time (5 s). A dead end or protected zone may be nearby.') }
    position = { x: args.x, y: args.y, z: args.z }
    return {}
  } }
  const result = await search.run(api, { block: 'bamboo', steps: 2 })
  assert.equal(result.failures.length, 1)
  assert.equal(result.visited.length, 3)
  assert.notDeepEqual(goals[0], goals[1])
  assert.ok(Math.hypot(goals[1].x, goals[1].z + 5) < 16)
  assert.ok(goals.every(g => g.y === 64 && g.dig === false))
})


const terrain = heights => (x, y, z) => {
  const top = heights(x, z)
  if (top === null) return null
  return { name: y <= top ? 'stone' : 'air', solid: y <= top, properties: {} }
}
test('connected frontier selects ground under canopy and excludes roof/cliff spikes', () => {
  const at = { x: 0, y: 64, z: 0 }
  const candidates = [{ x: 12, z: 0 }, { x: 12, z: 4 }, { x: 12, z: -4 }]
  const world = (x, y, z) => {
    if (x >= 8 && y === 74 && z === 0) return { name: 'oak_leaves', solid: true }
    if (x >= 8 && z >= 3 && y >= 64 && y <= 73) return { name: 'stone', solid: true }
    if (x >= 8 && z <= -3 && y === 74) return { name: 'stone', solid: true }
    return terrain(() => 63)(x, y, z)
  }
  const result = connectedFrontiers(world, at, candidates)
  assert.equal(result.goals.find(g => g.z === 0).y, 64)
  assert.equal(result.goals.some(g => g.z === 4), false)
  assert.equal(result.goals.find(g => g.z === -4).y, 64)
  assert.ok(result.goals.every(g => g.y !== 74 && g.y !== 75))
})
test('connected frontier climbs a tall continuous hill instead of rejecting altitude alone', () => {
  const world = terrain(x => 63 + Math.max(0, Math.min(12, x)))
  const result = connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 12, z: 0 }])
  assert.equal(result.goals[0].y, 76)
  assert.equal(result.goals[0].steps, 12)
})
test('lake blocks connection to cave floor and an unloaded frontier never gets guessed', () => {
  const world = (x, y, z) => {
    if (x >= 4) return { name: y >= 64 ? 'water' : y >= 60 ? 'air' : 'stone', solid: y < 60 }
    return terrain(() => 63)(x, y, z)
  }
  assert.equal(connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0)
  assert.equal(connectedFrontiers(terrain(x => x >= 4 ? null : 63), { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0)
})
test('frontier reads each cell once per plan, respects node bound, and starts on fractional support', () => {
  const reads = new Map()
  const world = (x, y, z) => {
    const key = `${x},${y},${z}`
    reads.set(key, (reads.get(key) ?? 0) + 1)
    return { name: y <= 63 ? 'farmland' : y === 64 ? 'short_grass' : 'air', solid: y <= 63 }
  }
  const result = connectedFrontiers(world, { x: 0.5, y: 63.9375, z: 0.5 }, [{ x: 12, z: 0 }])
  assert.equal(result.goals[0].y, 64)
  assert.equal(result.reads, reads.size)
  assert.ok([...reads.values()].every(n => n === 1))
  const capped = connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 16, z: 0 }], { maxNodes: 10 })
  assert.equal(capped.expanded, 10)
  assert.equal(capped.capped, true)
})
test('outward search avoids synthetic high-goal timeouts while preserving ground progress', async () => {
  const api = fake()
  api.block = (x, y, z) => y === 75 ? { name: 'oak_leaves', solid: true } : terrain(() => 63)(x, y, z)
  const result = await search.run(api, { block: 'bamboo', steps: 4 })
  assert.equal(result.distance, 64)
  assert.equal(result.failures.length, 0)
  assert.ok(api.calls.filter(c => c.name === 'goto').every(c => c.args.y === 64))
  const old = outwardCandidates({ x: 0, y: 64, z: 0 }, 'north', 16).map(p => searchSurface(api.block, p.x, p.z, 64))
  assert.ok(old.every(p => p.y === 76))
})

test('slow frontier scans retain valid plans; read budgets identify incomplete work', () => {
  let time = 0
  const result = connectedFrontiers(terrain(() => 63), { x: 0, y: 64, z: 0 }, [{ x: 16, z: 0 }], { now: () => { time += 1500; return time } })
  assert.equal(result.capped, false)
  assert.equal(result.limited, null)
  assert.equal(result.goals.length, 1)
  assert.ok(result.ms > 1000)
  const limited = connectedFrontiers(terrain(() => 63), { x: 0, y: 64, z: 0 }, [{ x: 16, z: 0 }], { maxReads: 20 })
  assert.equal(limited.capped, true)
  assert.equal(limited.limited, 'read budget')
  assert.equal(limited.reads, 20)
})

test('slow resource scans report one performance bug per run and retain observations', async () => {
  const realNow = Date.now
  let now = 0
  Date.now = () => now
  try {
    const events = []
    const reports = []
    const api = fake(name => {
      if (name === 'find_blocks') { now += 1500; return { positions: [] } }
    })
    api.emit = (event, detail) => events.push({ event, detail })
    api.report = report => reports.push(report)
    const result = await search.run(api, { block: 'bamboo', steps: 2 })
    assert.equal(result.visited.length, 3)
    assert.equal(events.length, 1)
    assert.equal(events[0].event, 'performance_bug')
    assert.equal(events[0].detail.resultRetained, true)
    assert.ok(reports.some(report => report.resource_scan_ms === 1500))
    assert.ok(reports.some(report => report.performance_bug?.includes('result retained')))
  } finally { Date.now = realNow }
})

test('outward search yields the radius checkpoint rather than circling its edge', async () => {
  const api = fake()
  api.pos = () => ({ x: 506, y: 64, z: 0 })
  const result = await search.run(api, { block: 'bamboo', heading: 'east', radius: 512, origin: '0,64,0', steps: 64 })
  assert.equal(result.reason, 'distance budget exhausted')
  assert.equal(result.distance, 506)
  assert.equal(result.visited.length, 1)
  assert.equal(api.calls.some(c => c.name === 'goto'), false)
  assert.match(result.resume, /heading=east.*radius=512/)
})
test('outward radius checkpoint still allows obstacle detours away from the boundary', async () => {
  let first = true
  const api = fake(name => { if (name === 'goto' && first) { first = false; throw new Error('no path') } })
  const result = await search.run(api, { mob: 'sheep', heading: 'east', radius: 512, steps: 2 })
  assert.equal(result.visited.length, 3)
  assert.equal(result.failures.length, 1)
  assert.equal(result.reason, 'step budget exhausted')
})
test('foraging delegates timing diagnostics to the shared reporter without duplicate emits', async () => {
  const operations = []
  const events = []
  const api = fake()
  api.performance = (operation, elapsedMs, details) => operations.push({ operation, elapsedMs, details })
  api.emit = event => events.push(event)
  await search.run(api, { block: 'bamboo', steps: 1 })
  assert.equal(operations.filter(o => o.operation === 'find_blocks').length, 2)
  assert.equal(operations.filter(o => o.operation === 'forage.search.frontier').length, 1)
  assert.equal(events.length, 0)
  assert.ok(operations.every(o => o.elapsedMs >= 0))
})

test('an existing leaf canopy permits connected safe descent but never another leaf goal', () => {
  const world = (x, y, z) => {
    const top = x < 4 ? 67 - Math.max(0, x) : 63
    return { name: y <= top ? x < 4 && y === top ? 'birch_leaves' : 'stone' : 'air', solid: y <= top }
  }
  const result = connectedFrontiers(world, { x: 0, y: 68, z: 0 }, [{ x: 2, z: 0 }, { x: 8, z: 0 }])
  assert.equal(result.escapingCanopy, true)
  assert.equal(result.goals.some(g => g.x === 2), false)
  assert.equal(result.goals.find(g => g.x === 8).y, 64)
  assert.equal(result.originReason, null)
})
test('isolated canopy reports missing safe descent and never jumps a cliff', () => {
  const world = (x, y, z) => ({ name: y === 74 && Math.abs(x) <= 2 && Math.abs(z) <= 2 ? 'birch_leaves' : y <= 63 ? 'stone' : 'air', solid: y <= 63 || y === 74 && Math.abs(x) <= 2 && Math.abs(z) <= 2 })
  const result = connectedFrontiers(world, { x: 0, y: 75, z: 0 }, [{ x: 8, z: 0 }])
  assert.equal(result.goals.length, 0)
  assert.match(result.originReason, /on leaves.*no connected safe descent/)
})
test('frontier follows existing open gate and door corridors while respecting closed passages', () => {
  for (const name of ['oak_fence_gate', 'oak_door']) {
    const world = open => (x, y, z) => {
      if (y < 64) return { name: 'stone', solid: true }
      if (Math.abs(z) > 0 && y <= 66) return { name: 'stone', solid: true }
      if (x === 3 && y === 64) return { name, solid: true, properties: { open } }
      if (x === 3 && y === 65) return { name: name.endsWith('_door') ? name : 'air', solid: name.endsWith('_door'), properties: { open } }
      return { name: 'air', solid: false }
    }
    assert.equal(connectedFrontiers(world(true), { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 1)
    assert.equal(connectedFrontiers(world(false), { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }], { openDoors: false }).goals.length, 0)
  }
})

const require = createRequire(import.meta.url)
const terrainRegistry = require('minecraft-data')('1.21.5')
const TerrainBlock = require('prismarine-block')(terrainRegistry)
function registeredCell (name, properties = {}) {
  const block = TerrainBlock.fromProperties(name, properties, 0)
  return { name: block.name, solid: block.boundingBox === 'block', shapes: block.shapes, properties: block.getProperties() }
}
const airCell = registeredCell('air'), stoneCell = registeredCell('stone')
const decoratedGround = decoration => (x, y, z) => y < 64 ? stoneCell : y === 64 ? decoration : airCell

test('registry collision shapes generically permit decorations at the origin and throughout a frontier', () => {
  const decorations = ['wildflowers', 'leaf_litter', 'pink_petals', 'torch', 'wall_torch', 'redstone_torch', 'rail', 'powered_rail', 'redstone_wire', 'lever', 'stone_button', 'oak_sapling', 'brown_mushroom', 'flowering_azalea', 'short_grass', 'snow']
  for (const name of decorations) {
    const decoration = registeredCell(name, { waterlogged: false })
    // Some decorations (e.g. azalea) have real collision and are not flowers
    // to pass through. Classify from the state shape, not the pretty name.
    if (decoration.shapes.length) continue
    const world = decoratedGround(decoration)
    const plan = connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }])
    assert.equal(plan.goals.length, 1, name)
    assert.equal(plan.originReason, null, name)
    assert.equal(searchSurface(world, 8, 0, 64).y, 64, name)
  }
  const unknown = decoratedGround({ name: 'future_harmless_decoration', solid: true, shapes: [], properties: {} })
  assert.equal(connectedFrontiers(unknown, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 1, 'actual empty collision overrides the coarse solid summary and does not depend on known names')
})
test('collision-free hazards, liquids, waterlogged decorations and crops never become forage corridors', () => {
  for (const name of ['water', 'lava', 'bubble_column', 'powder_snow', 'fire', 'soul_fire', 'sweet_berry_bush', 'wither_rose', 'cobweb', 'wheat', 'carrots', 'nether_portal']) {
    const world = decoratedGround(registeredCell(name))
    assert.equal(connectedFrontiers(world, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0, name)
    assert.equal(searchSurface(world, 8, 0, 64), null, name)
  }
  assert.equal(connectedFrontiers(decoratedGround(registeredCell('rail', { waterlogged: true })), { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0)
})
test('registry shapes distinguish foot carpets, obstructed headroom, slabs and farmland heights', () => {
  const carpet = registeredCell('red_carpet')
  assert.equal(connectedFrontiers(decoratedGround(carpet), { x: 0, y: 64.0625, z: 0 }, [{ x: 8, z: 0 }]).goals[0].y, 64)
  const ceiling = (x, y, z) => y < 64 ? stoneCell : y === 65 ? carpet : airCell
  assert.equal(connectedFrontiers(ceiling, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0, 'a thin collision at head height still blocks the body')
  for (const [name, properties, height] of [['oak_slab', { type: 'bottom', waterlogged: false }, 64.5], ['farmland', {}, 64.9375]]) {
    const support = registeredCell(name, properties)
    const world = (x, y) => y < 64 ? stoneCell : y === 64 ? support : airCell
    assert.equal(connectedFrontiers(world, { x: 0, y: height, z: 0 }, [{ x: 8, z: 0 }]).goals[0].y, 65, name)
  }
  const slab = registeredCell('oak_slab', { type: 'bottom', waterlogged: false })
  const tallStep = (x, y, z) => z !== 0 || x < 0 || x > 1 ? stoneCell : x === 0 ? y < 64 ? stoneCell : y === 64 ? slab : airCell : y <= 65 ? stoneCell : airCell
  assert.equal(connectedFrontiers(tallStep, { x: 0, y: 64.5, z: 0 }, [{ x: 1, z: 0 }]).goals.length, 0, 'one planner-cell rise from a bottom slab is physically too high')
})
test('leaf-litter expedition progresses and shape-aware scans stay in milliseconds', async t => {
  const api = fake()
  api.block = decoratedGround(registeredCell('leaf_litter'))
  const result = await search.run(api, { block: 'bamboo', steps: 3 })
  assert.equal(result.distance, 48)
  assert.equal(result.failures.length, 0)
  const origin = { x: 0, y: 64, z: 0 }, candidates = outwardCandidates(origin, 'north', 16)
  for (let i = 0; i < 2; i++) connectedFrontiers(api.block, origin, candidates)
  const samples = []
  for (let i = 0; i < 5; i++) {
    const began = performance.now()
    const plan = connectedFrontiers(api.block, origin, candidates)
    samples.push(performance.now() - began)
    assert.equal(plan.goals.length, 9)
  }
  samples.sort((a, b) => a - b)
  t.diagnostic(`collision-aware litter frontier median=${samples[2].toFixed(2)}ms max=${samples[4].toFixed(2)}ms (2 warmups, 5 runs)`)
})
test('registry snow layers support actual fractional feet despite an empty bounding-box summary', () => {
  for (let layers = 1; layers <= 8; layers++) {
    const snow = registeredCell('snow', { layers })
    assert.equal(snow.solid, false, 'registry summary alone loses the supporting collision')
    const world = decoratedGround(snow)
    const plan = connectedFrontiers(world, { x: 0, y: 64 + (layers - 1) / 8, z: 0 }, [{ x: 8, z: 0 }])
    assert.equal(plan.goals.length, 1, `snow layers=${layers}`)
    assert.equal(plan.goals[0].y, layers === 1 ? 64 : 65)
    assert.equal(searchSurface(world, 8, 0, 64).y, layers === 1 ? 64 : 65)
  }
  const thick = registeredCell('snow', { layers: 8 })
  const blocked = (x, y) => y < 64 ? stoneCell : y === 65 ? thick : airCell
  assert.equal(connectedFrontiers(blocked, { x: 0, y: 64, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0)
})
test('mixed decorative patches and clear islands form one connected ground route', () => {
  const decorations = ['wildflowers', 'leaf_litter', 'pink_petals'].map(name => registeredCell(name))
  const world = (x, y, z) => y < 64 ? stoneCell : y === 64 && Math.abs(x) % 4 !== 0 ? decorations[Math.abs(x + z) % decorations.length] : airCell
  for (const x of [0, 1, 2, 3]) {
    const plan = connectedFrontiers(world, { x, y: 64, z: 0 }, [{ x: x + 16, z: 0 }])
    assert.equal(plan.goals.length, 1, `starting in patch/island ${x}`)
    assert.equal(plan.goals[0].steps, 16)
  }
})
test('a canopy escape can descend two or three clear blocks but never an unsafe cliff or farmland landing', () => {
  for (const drop of [2, 3, 4]) {
    const world = (x, y, z) => x === 0 && z === 0 && y === 74 ? { name: 'spruce_leaves', solid: true, shapes: [[0, 0, 0, 1, 1, 1]] }
      : y < 75 - drop ? stoneCell : airCell
    const plan = connectedFrontiers(world, { x: 0, y: 75, z: 0 }, [{ x: 8, z: 0 }])
    assert.equal(plan.goals.length, drop <= 3 ? 1 : 0, `drop=${drop}`)
    if (plan.goals.length) assert.equal(plan.goals[0].height, 75 - drop)
  }
  const farm = registeredCell('farmland')
  const farmWorld = (x, y, z) => x === 0 && z === 0 && y === 74 ? { name: 'spruce_leaves', solid: true, shapes: [[0, 0, 0, 1, 1, 1]] }
    : y === 72 ? farm : y < 72 ? stoneCell : airCell
  assert.equal(connectedFrontiers(farmWorld, { x: 0, y: 75, z: 0 }, [{ x: 8, z: 0 }]).goals.length, 0)
})
test('a short drop requires the entire falling column to clear the head, not just a safe landing', () => {
  const world = (x, y, z) => z !== 0 || x < 0 || x > 1 ? stoneCell : x === 0 ? y < 75 ? stoneCell : airCell : y < 72 || y === 76 ? stoneCell : airCell
  assert.equal(connectedFrontiers(world, { x: 0, y: 75, z: 0 }, [{ x: 1, z: 0 }]).goals.length, 0)
})
