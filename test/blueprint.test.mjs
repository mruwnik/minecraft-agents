import { canonicalFixture } from './plan-fixture.mjs'
// The blueprint format (docs/superpowers/specs/2026-09-26-blueprint-format-design.md): a Markdown file is parsed into
// layers of tokens, resolved with material parameters, turned by facing=, costed, staged and judged against a world.
// Everything here is pure; the walking half is in test/blueprint-build.test.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import {
  parseBlueprint, resolve, turnsFor, rotate, blueprintCells, bill, stackSlots, counts, enclosure, lint, placement,
  jobsFor, orderJobs, stages, siteCheck as rawSiteCheck, farmPlanToBlueprint, blueprintHash, buildNote, parseNote, renderLayer, stageLine,
  matchesCell, flatGround, REGISTRY
} from '../src/blueprint/format.mjs'
import { familyName, familyRefusal } from '../src/build/materials.mjs'

const LIBRARY = path.join(import.meta.dirname, 'fixtures', 'blueprints')
const read = name => fs.readFileSync(path.join(LIBRARY, `${name}.txt`), 'utf8')
const hut = () => parseBlueprint(read('starter-hut'))
const tower = () => parseBlueprint(read('watchtower'))
const field = () => parseBlueprint(read('wheat-field'))

// a small blueprint from its parts: the front matter a test does not care about is filled in
const text = ({ front = 'south', tags = 'decorative', foundation = 'flat', legend, layers, extra = '' }) =>
  `---\nname: t\ntitle: T\ndescription: d\ntags: ${tags}\nfront: ${front}\nfoundation: ${foundation}\n${extra}---\n\n# T\n\n\`\`\`legend\n${legend}\n\`\`\`\n\n` +
  layers.map(([y, grid]) => `## y${y}\n\n\`\`\`layer\n${grid}\n\`\`\``).join('\n\n') + '\n'
const parse = parts => parseBlueprint(text(parts))

// a fake world the way a prismarine Block answers: state through getProperties(), never a bare .properties field (a
// fake with .properties once let a guard pass ten tests and refuse nothing in the field)
const fakeBlock = (name, state = {}) => ({ name, getProperties: () => state, solid: REGISTRY.blocksByName[name]?.boundingBox === 'block' })
const worldOf = (cells, base = flatGround(64)) => (x, y, z) => {
  const raw = cells[`${x},${y},${z}`]
  if (raw === null) return null
  return raw === undefined ? base(x, y, z) : raw
}

// ---------------------------------------------------------------- parse

test('parseBlueprint: the examples parse to their sizes', () => {
  const rows = [
    ['starter-hut', { width: 5, depth: 5, layers: 5, tokens: 13, minY: -1, maxY: 3 }],
    ['watchtower', { width: 5, depth: 5, layers: 14, tokens: 10, minY: -1, maxY: 12 }],
    ['wheat-field', { width: 11, depth: 7, layers: 3, tokens: 8, minY: -1, maxY: 1 }]
  ]
  for (const [name, want] of rows) {
    const bp = parseBlueprint(read(name))
    assert.deepEqual(bp.errors, [], name)
    assert.deepEqual({ width: bp.width, depth: bp.depth, layers: bp.layers.length, tokens: Object.keys(bp.legend).length, minY: bp.layers[0].y, maxY: bp.layers.at(-1).y }, want, name)
  }
})

test('parseBlueprint: front matter', () => {
  const bp = hut()
  assert.equal(bp.name, 'starter-hut')
  assert.deepEqual(bp.tags, ['shelter', 'storage'])
  assert.equal(bp.front, 'south')
  assert.equal(bp.clearance, 1)
  assert.deepEqual(bp.params, { wood: 'oak', stone: 'cobblestone', bed: 'white' })
})

test('parseBlueprint: a y2..y9 heading is eight equal layers', () => {
  const bp = tower()
  const ys = bp.layers.map(l => l.y)
  assert.deepEqual(ys, [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
  assert.deepEqual(bp.layers.filter(l => l.y >= 2 && l.y <= 9).map(l => l.grid.join('/')), Array(8).fill('_____/_SSS_/_SHS_/_SSS_/_____'))
})

test('parseBlueprint: the legend', () => {
  const bp = hut()
  assert.deepEqual(bp.legend.D, { token: 'D', alts: [{ name: '{wood:door}', states: { facing: 'north', half: 'lower', hinge: 'left', open: 'false' } }], tags: ['entrance'] })
  assert.deepEqual(bp.legend['.'], { token: '.', alts: [{ name: 'air', states: {} }], tags: [] })
  const f = field()
  assert.deepEqual(f.legend['.'], { token: '.', alts: [{ name: 'air', states: {} }], tags: ['lane'] })
  assert.deepEqual(f.legend['='].tags, ['ground', 'cover', 'lane'])
})

test('parseBlueprint: alternatives keep their order and @solid', () => {
  const bp = parse({ legend: 'r  dirt|grass_block|@solid  #ground', layers: [[0, 'r']] })
  assert.deepEqual(bp.errors, [])
  assert.deepEqual(bp.legend.r.alts.map(a => a.name), ['dirt', 'grass_block', '@solid'])
})

test('parseBlueprint: errors', () => {
  const rows = [
    ['unknown token', parse({ legend: 'S  cobblestone', layers: [[0, 'SS\nSQ']] }), 'y0 row 1 col 1: Q is not in the legend'],
    ['ragged row', parse({ legend: 'S  cobblestone', layers: [[0, 'SSSSS\nSSSS\nSSSSS']] }), 'y0 row 1 is 4 wide, the others 5'],
    ['layer size mismatch', parse({ legend: 'S  cobblestone', layers: [[0, 'SSSSS\nSSSSS\nSSSSS\nSSSSS\nSSSSS'], [2, 'SSSSS\nSSSSS\nSSSSS\nSSSSS']] }), 'y2 is 5x4 but the blueprint is 5x5'],
    ['layer given twice', parseBlueprint(text({ legend: 'S  cobblestone', layers: [[0, 'S']] }) + '\n## y2..y4\n\n```layer\nS\n```\n\n## y3\n\n```layer\nS\n```\n'), 'y3 is given twice (y2..y4 and y3)'],
    ['unknown state', parse({ legend: 'D  oak_door[facing=north,hinges=left]', layers: [[0, 'D']] }), 'D: oak_door has no state hinges (did you mean hinge?)'],
    ['unknown value', parse({ legend: 'D  oak_door[facing=up]', layers: [[0, 'D']] }), 'D: oak_door facing has no value up (north, south, west, east)'],
    ['unknown block', parse({ legend: 'Q  quartzite', layers: [[0, 'Q']] }), 'Q: quartzite is not a block this body knows'],
    ['server-only block', parse({ legend: 'B  sulfur_bricks', layers: [[0, 'B']] }), 'B: this body cannot place sulfur_bricks (server-only block)'],
    ['unknown front-matter key', parseBlueprint(text({ legend: 'S  cobblestone', layers: [[0, 'S']], extra: 'foudation: flat\n' }), REGISTRY), 'front matter: foudation is not a key (did you mean foundation?)'],
    ['missing required key', parseBlueprint(text({ legend: 'S  cobblestone', layers: [[0, 'S']] }).replace('front: south\n', '')), 'front matter: front is required'],
    ['bad tag', parse({ legend: 'S  cobblestone', tags: 'castle', layers: [[0, 'S']] }), 'front matter: castle is not a tag (shelter, storage, farm, pen, lookout, workshop, decorative, bridge)'],
    ['bad front', parse({ legend: 'S  cobblestone', front: 'up', layers: [[0, 'S']] }), 'front matter: front must be north, south, east or west'],
    ['multi-character token', parse({ legend: 'SS  cobblestone', layers: [[0, 'S']] }), 'legend: "SS  cobblestone" is not "<token> <block>[states] #tags" with a one-character token'],
    ['underscore redefined', parse({ legend: '_  cobblestone', layers: [[0, '_']] }), 'legend: _ is reserved (do not care) and cannot be redefined'],
    ['bad default parameter', parse({ legend: 'S  {stone:wall}', layers: [[0, 'S']], extra: 'params: stone=stone\n' }), 'S: stone has no wall: use cobblestone, mossy_cobblestone, stone_bricks, mossy_stone_bricks or another that has one for stone='],
    ['unknown parameter', parse({ legend: 'S  {metal:block}', layers: [[0, 'S']] }), 'S: {metal:block} names a parameter the front matter does not declare (params: metal=...)'],
    ['too tall', parse({ legend: 'S  cobblestone', layers: [[48, 'S']] }), 'y48 is outside y-4..y47'],
    ['no layers', parseBlueprint(text({ legend: 'S  cobblestone', layers: [] })), 'no ## y<n> layer found']
  ]
  for (const [label, bp, want] of rows) assert.ok(bp.errors.includes(want), `${label}: ${JSON.stringify(bp.errors)}`)
})

test('parseBlueprint: the y1 grid of a footprint 64 wide with 16385 cells is refused', () => {
  const grid = Array(64).fill('S'.repeat(64)).join('\n')
  const bp = parse({ legend: 'S  cobblestone', layers: [[0, grid], [1, grid], [2, grid], [3, grid], [4, Array(63).fill('_'.repeat(64)).concat(['S' + '_'.repeat(63)]).join('\n')]] })
  assert.ok(bp.errors.includes('16385 cells are not _; the most a blueprint may have is 16384'), JSON.stringify(bp.errors))
})

// ---------------------------------------------------------------- materials

test('familyName: the irregular ones', () => {
  const rows = [
    ['wood', 'crimson', 'log', 'crimson_stem'],
    ['wood', 'bamboo', 'log', 'bamboo_block'],
    ['wood', 'oak', 'stripped_log', 'stripped_oak_log'],
    ['wood', 'warped', 'stripped_log', 'stripped_warped_stem'],
    ['wood', 'cherry', 'fence_gate', 'cherry_fence_gate'],
    ['stone', 'stone_bricks', 'stairs', 'stone_brick_stairs'],
    ['stone', 'bricks', 'stairs', 'brick_stairs'],
    ['stone', 'cobblestone', 'block', 'cobblestone'],
    ['stone', 'quartz_block', 'slab', 'quartz_slab'],
    ['stone', 'deepslate_bricks', 'wall', 'deepslate_brick_wall']
  ]
  for (const [family, value, role, want] of rows) assert.equal(familyName(family, value, role), want, `${family}:${value}:${role}`)
})

test('familyRefusal', () => {
  const rows = [
    ['stone', 'stone', 'wall', 'stone has no wall: use cobblestone, mossy_cobblestone, stone_bricks, mossy_stone_bricks or another that has one for stone='],
    ['stone', 'cobblestone', 'wall', null],
    ['wood', 'oak', 'door', null],
    ['wood', 'oak', 'roof', 'wood has no role roof: use planks, log, stripped_log, slab, stairs, fence, fence_gate, door, trapdoor, button, pressure_plate, sign'],
    ['metal', 'iron', 'block', 'metal is not a material family: use wood or stone']
  ]
  for (const [family, value, role, want] of rows) assert.equal(familyRefusal(family, value, role, REGISTRY), want, `${family}:${value}:${role}`)
})

test('resolve: templates', () => {
  const rows = [
    [{ wood: 'crimson' }, 'L', 'crimson_stem'],
    [{}, 'L', 'oak_log'],
    [{ stone: 'bricks' }, '^', 'oak_stairs'],
    [{ wood: 'spruce' }, '^', 'spruce_stairs'],
    [{ bed: 'red' }, 'F', 'red_bed'],
    [{ stone: 'stone_bricks' }, 'S', 'stone_bricks']
  ]
  for (const [params, token, want] of rows) assert.equal(resolve(hut(), params).legend[token].alts[0].name, want, `${JSON.stringify(params)} ${token}`)
})

test('resolve: refusals', () => {
  const rows = [
    [{ stone: 'stone' }, 'S', /^S: stone has no wall: use cobblestone/],
    [{ metal: 'iron' }, 'S', /^metal= is not a parameter of t \(it has stone\)$/],
    [{ stone: 'marble' }, 'S', /^S: marble has no wall: use cobblestone/]
  ]
  const bp = parse({ legend: 'S  {stone:wall}', layers: [[0, 'S']], extra: 'params: stone=cobblestone\n' })
  for (const [params, , want] of rows) assert.throws(() => resolve(bp, params), { message: want }, JSON.stringify(params))
})

// ---------------------------------------------------------------- rotation

test('turnsFor: facing minus front, clockwise', () => {
  const rows = [['south', 'south', 0], ['south', 'west', 1], ['south', 'north', 2], ['south', 'east', 3], ['east', 'south', 1], ['north', 'west', 3], ['south', undefined, 0]]
  for (const [front, facing, want] of rows) assert.equal(turnsFor(front, facing), want, `${front} -> ${facing}`)
})

test('rotate: a 5x3 grid one turn is 3x5 with cell (0,0) at (2,0)', () => {
  const bp = parse({ legend: 'a  cobblestone\nb  stone\nc  dirt', layers: [[0, 'abbbb\ncbbbb\nbbbbb']] })
  const turned = rotate(bp, 1)
  assert.deepEqual([turned.width, turned.depth], [3, 5])
  assert.deepEqual(turned.layers[0].grid, ['bca', 'bbb', 'bbb', 'bbb', 'bbb'])
  assert.equal(turned.front, 'west')
  assert.equal(turned.turns, 1)
})

test('rotate: four turns is the identity', () => {
  assert.deepEqual(rotate(hut(), 4).layers, hut().layers)
  assert.deepEqual(rotate(hut(), 4).legend, hut().legend)
})

test('rotate: every directional state', () => {
  const rows = [
    ['oak_stairs[facing=north]', 1, { facing: 'east' }],
    ['oak_stairs[facing=west]', 1, { facing: 'north' }],
    ['oak_stairs[facing=north,half=top,shape=inner_left]', 1, { facing: 'east', half: 'top', shape: 'inner_left' }],
    ['oak_log[axis=x]', 1, { axis: 'z' }],
    ['oak_log[axis=z]', 1, { axis: 'x' }],
    ['oak_log[axis=y]', 1, { axis: 'y' }],
    ['oak_sign[rotation=15]', 1, { rotation: '3' }],
    ['oak_sign[rotation=0]', 2, { rotation: '8' }],
    ['rail[shape=north_south]', 1, { shape: 'east_west' }],
    ['rail[shape=ascending_north]', 1, { shape: 'ascending_east' }],
    ['rail[shape=ascending_south]', 2, { shape: 'ascending_north' }],
    ['rail[shape=south_east]', 1, { shape: 'south_west' }],
    ['rail[shape=north_east]', 1, { shape: 'south_east' }],
    ['oak_door[facing=north,hinge=left,half=lower]', 1, { facing: 'east', hinge: 'left', half: 'lower' }],
    ['white_bed[facing=east,part=foot]', 1, { facing: 'south', part: 'foot' }],
    ['oak_slab[type=top]', 1, { type: 'top' }],
    ['oak_fence[north=true]', 1, { north: 'true' }],
    ['wall_torch[facing=south]', 3, { facing: 'east' }]
  ]
  for (const [spec, turns, want] of rows) {
    const bp = parse({ legend: `X  ${spec}`, layers: [[0, 'X']] })
    assert.deepEqual(bp.errors, [], spec)
    assert.deepEqual(rotate(bp, turns).legend.X.alts[0].states, want, `${spec} x${turns}`)
  }
})

test('blueprintCells: the world cell of a rotated blueprint', () => {
  // the hut's door is at column 2 of the last row (dz 4) facing south; turned once it faces west, on the west side
  const door = bp => blueprintCells(bp).find(c => c.token === 'D')
  assert.deepEqual([door(hut()).dx, door(hut()).dy, door(hut()).dz], [2, 0, 4])
  const turned = rotate(hut(), 1)
  assert.deepEqual([door(turned).dx, door(turned).dy, door(turned).dz], [0, 0, 2])
  assert.equal(turned.legend.D.alts[0].states.facing, 'east')
})

// ---------------------------------------------------------------- bill and metadata

test('bill: the examples', () => {
  const rows = [
    ['starter-hut', { cobblestone: 48, oak_planks: 34, oak_log: 12, glass_pane: 2, oak_door: 1, white_bed: 1, chest: 1, crafting_table: 1, oak_stairs: 1, torch: 1 }, []],
    ['wheat-field', { dirt: 32, wheat_seeds: 36, oak_slab: 9, water_bucket: 1, oak_fence: 31, oak_fence_gate: 1, torch: 4 }, ['hoe']],
    ['watchtower', { cobblestone: 86, jack_o_lantern: 1, ladder: 10, oak_door: 1, oak_planks: 24, oak_trapdoor: 1, oak_fence: 16, torch: 4 }, []]
  ]
  for (const [name, total, tools] of rows) {
    const b = bill(resolve(parseBlueprint(read(name))))
    assert.deepEqual(b.total, total, name)
    assert.deepEqual(b.tools, tools, name)
  }
})

test('bill: per layer, and a door drawn in both halves is one door', () => {
  const b = bill(resolve(hut()))
  assert.deepEqual(b.layers.find(l => l.y === -1).items, { cobblestone: 16, oak_planks: 9 })
  assert.deepEqual(b.layers.find(l => l.y === 3).items, { oak_planks: 25 })
  assert.equal(b.layers.find(l => l.y === 1).items.oak_door, undefined)
  assert.equal(b.layers.find(l => l.y === 0).items.oak_door, 1)
})

test('an unsupported floating platform is unreachable, not promised a pillar that cannot supply its first clicked face', () => {
  const platform = resolve(parse({ legend: 'S  cobblestone', layers: [[6, 'SSS\nSSS\nSSS']] }))
  assert.equal(bill(platform).scaffold, undefined)
  assert.equal(orderJobs(jobsFor(platform, AT, flatGround(64)), platform, AT, flatGround(64)).unreachable.length, 9)
  assert.ok(bill(resolve(tower())).scaffold.dirt > 0, 'the ladder alone does not expose every upper support face')
  assert.ok(bill(resolve(hut())).scaffold.dirt > 0, 'blocked outer roof clicks need explicit support access')
})

test('stackSlots: beds, signs and buckets do not stack to 64', () => {
  const rows = [
    [{ cobblestone: 64 }, 1],
    [{ cobblestone: 65 }, 2],
    [{ white_bed: 2 }, 2],
    [{ oak_sign: 17 }, 2],
    [{ water_bucket: 1, cobblestone: 10, oak_planks: 130 }, 5],
    [{}, 0]
  ]
  for (const [items, want] of rows) assert.equal(stackSlots(items), want, JSON.stringify(items))
})

test('counts', () => {
  assert.deepEqual(counts(resolve(hut())), { doors: 1, beds: 1, containers: 1, workstations: 1, lights: 1 })
  assert.deepEqual(counts(resolve(tower())), { doors: 1, beds: 0, containers: 0, workstations: 0, lights: 5 })
})

test('enclosure: the hut is enclosed and lit, the hut without its torch is not', () => {
  const lit = enclosure(resolve(hut()))
  assert.equal(lit.enclosed, 21)
  assert.equal(lit.lit, true)
  assert.equal(lit.spawnSafe, true)
  const dark = parseBlueprint(read('starter-hut').replace('ST.iS', 'ST..S'))
  const unlit = enclosure(resolve(dark))
  assert.equal(unlit.enclosed, 22)
  assert.equal(unlit.lit, false)
  assert.equal(unlit.dark.length, 5)
  assert.equal(enclosure(resolve(field())).enclosed, 0)
})

test('lint: the examples', () => {
  assert.deepEqual(lint(resolve(hut())), { errors: [], warnings: [] })
  assert.deepEqual(lint(resolve(field())), { errors: [], warnings: [] })
  assert.deepEqual(lint(resolve(tower())), { errors: [], warnings: [] })
})

test('lint: errors', () => {
  const rows = [
    ['bed foot without its head', parse({ legend: 'F  white_bed[facing=east,part=foot]\nS  cobblestone', layers: [[0, 'FS']] }), 'the bed foot at y0 0,0 faces east but 1,0 is not its head'],
    ['door upper without lower', parse({ legend: 'd  oak_door[facing=north,half=upper]\nS  cobblestone', layers: [[0, 'S'], [1, 'd']] }), 'the door upper half at y1 0,0 has no lower half under it'],
    ['door lower without upper', parse({ legend: 'D  oak_door[facing=north,half=lower]\nS  cobblestone', layers: [[0, 'D'], [1, 'S']] }), 'the door at y0 0,0 has no upper half over it: y1 0,0 is S (cobblestone)'],
    ['door with no floor', parse({ legend: 'D  oak_door[facing=north,half=lower]\nd  oak_door[facing=north,half=upper]\nS  cobblestone', layers: [[-1, '_S'], [0, 'DS'], [1, 'dS']] }), 'the door at y0 0,0 stands on nothing: y-1 0,0 is _'],
    ['torch over air', parse({ legend: 'i  torch\nS  cobblestone', layers: [[0, '.S'], [1, 'iS']] }), 'the torch at y1 0,0 needs a block at 0,0 below it; that cell is . (air)'],
    ['wall torch off its wall', parse({ legend: 't  wall_torch[facing=south]\nS  cobblestone', layers: [[0, '..\ntS']] }), 'the wall_torch at y0 0,1 needs a block at 0,0 north of it; that cell is . (air)'],
    ['crop without farmland', parse({ legend: 'w  wheat[age=0]\nr  dirt', layers: [[-1, 'r'], [0, 'w']] }), 'the wheat at y0 0,0 needs farmland under it; y-1 0,0 is r (dirt)'],
    ['farmland with no water', parse({ legend: 'f  farmland\nw  wheat[age=0]', layers: [[-1, 'fffff'], [0, 'wwwww']] }), '5 cells are farmland with no water within 4 blocks (0,0 1,0 2,0 3,0 and 1 more): move the channel or shorten the row'],
    ['unlit shelter', parseBlueprint(read('starter-hut').replace('ST.iS', 'ST..S')), 'the room at y0 1..3,1..3 has 5 cells at light 0: add a light source (a shelter must be spawn-safe)'],
    ['a block no pillar outside the footprint can reach', parse({ legend: 'S  cobblestone', layers: [[6, Array(5).fill('_'.repeat(11)).concat(['_____S_____'], Array(5).fill('_'.repeat(11))).join('\n')]] }), 'y6 5,5 has no cell to stand on within reach, even with a scaffold']
  ]
  for (const [label, bp, want] of rows) {
    const errors = lint(resolve(bp)).errors
    assert.ok(errors.includes(want), `${label}: ${JSON.stringify(errors)}`)
  }
})

test('lint: warnings', () => {
  const rows = [
    ['a torch whose support is _', parse({ legend: 'i  torch\nS  cobblestone', layers: [[0, '_S'], [1, 'iS']] }), 'the torch at y1 0,0 stands on 0,0 below it, which is _: whatever is there must hold it'],
    ['unlit room without the shelter tag', parseBlueprint(read('starter-hut').replace('ST.iS', 'ST..S').replace('tags: shelter, storage', 'tags: storage')), 'the room at y0 1..3,1..3 has 5 cells at light 0: add a light source'],
    ['a crop with no lane', parse({ legend: 'f  farmland\nw  wheat[age=0]', layers: [[-1, Array(11).fill('f'.repeat(11)).join('\n')], [0, Array(11).fill('w'.repeat(11)).join('\n')]] }), '9 crop cells have nothing to stand on within 4 of them (4,4 5,4 6,4 4,5 and 5 more): lay a . path or a covered channel through the rows, eight rows apart at most, or every job there answers nowhere to stand (a walk crosses the rows where it must, but a job never stands in one)']
  ]
  for (const [label, bp, want] of rows) {
    const out = lint(resolve(bp))
    assert.ok(out.warnings.includes(want), `${label}: ${JSON.stringify(out)}`)
  }
})

// a torch clicked onto the side of a wall beside it comes out as a wall torch (Pacer's hut, 09-26): a floor light, a
// standing sign and a floor lever are clicked onto the block under them (against=down), a sneak-click when that is a
// crafting table, so a torch over one is no warning and no refusal
test('lint: a torch over a crafting table is no gap', () => {
  const out = lint(resolve(parse({ legend: 'i  torch\nT  crafting_table\nS  cobblestone', layers: [[0, 'TS'], [1, 'iS']] })))
  assert.deepEqual(out.warnings.filter(w => /torch/.test(w)), [])
})

// a log on its side takes its axis from the face it is clicked onto: whichever neighbour along the axis stands when it
// is placed (build picks), so it is no gap and lint says nothing
test('lint: a log on its side is no gap', () => {
  const out = lint(resolve(parse({ legend: 'L  oak_log[axis=x]\nP  oak_planks', layers: [[0, 'PLP']] })))
  assert.deepEqual(out.warnings.filter(w => /oak_log/.test(w)), [])
})

test('placement: what to hand place for a state', () => {
  const rows = [
    [{ name: 'oak_stairs', states: { facing: 'east' } }, { facing: 'east' }],
    [{ name: 'oak_stairs', states: { facing: 'east', half: 'top' } }, { facing: 'east', half: 'top' }],
    [{ name: 'oak_door', states: { facing: 'north', half: 'lower' } }, { facing: 'north' }],
    [{ name: 'white_bed', states: { facing: 'east', part: 'foot' } }, { facing: 'east' }],
    [{ name: 'oak_fence_gate', states: { facing: 'east' } }, { facing: 'east' }],
    [{ name: 'chest', states: { facing: 'south' } }, { facing: 'north' }],
    [{ name: 'furnace', states: { facing: 'west' } }, { facing: 'east' }],
    [{ name: 'oak_trapdoor', states: { facing: 'south', half: 'top' } }, { facing: 'south', half: 'top' }],
    [{ name: 'oak_slab', states: { type: 'top' } }, { half: 'top' }],
    [{ name: 'oak_slab', states: { type: 'bottom' } }, { half: 'bottom' }],
    [{ name: 'cobblestone', states: {} }, {}],
    [{ name: 'wall_torch', states: { facing: 'south' } }, { against: { dx: 0, dy: 0, dz: -1 } }],
    [{ name: 'ladder', states: { facing: 'east' } }, { against: { dx: -1, dy: 0, dz: 0 } }],
    [{ name: 'oak_log', states: { axis: 'x' } }, { along: 'x' }],
    [{ name: 'oak_log', states: { axis: 'z' } }, { along: 'z' }],
    [{ name: 'oak_log', states: { axis: 'y' } }, {}],
    [{ name: 'torch', states: {} }, { against: { dx: 0, dy: -1, dz: 0 } }],
    [{ name: 'soul_torch', states: {} }, { against: { dx: 0, dy: -1, dz: 0 } }],
    [{ name: 'lantern', states: {} }, { against: { dx: 0, dy: -1, dz: 0 } }],
    [{ name: 'oak_sign', states: { rotation: '8' } }, { against: { dx: 0, dy: -1, dz: 0 } }],
    [{ name: 'lever', states: { face: 'floor', facing: 'north' } }, { against: { dx: 0, dy: -1, dz: 0 } }],
    [{ name: 'lever', states: { face: 'ceiling', facing: 'north' } }, { against: { dx: 0, dy: 1, dz: 0 } }]
  ]
  for (const [alt, want] of rows) assert.deepEqual(placement(alt), want, `${alt.name} ${JSON.stringify(alt.states)}`)
})

// ---------------------------------------------------------------- matching a world cell

test('matchesCell: block and written states, never derived ones', () => {
  const rows = [
    ['same block', fakeBlock('cobblestone'), [{ name: 'cobblestone', states: {} }], true],
    ['other block', fakeBlock('stone'), [{ name: 'cobblestone', states: {} }], false],
    ['second alternative', fakeBlock('grass_block'), [{ name: 'dirt', states: {} }, { name: 'grass_block', states: {} }], true],
    ['@solid takes any full block', fakeBlock('stone'), [{ name: 'dirt', states: {} }, { name: '@solid', states: {} }], true],
    ['@solid refuses a slab', fakeBlock('oak_slab', { type: 'bottom' }), [{ name: '@solid', states: {} }], false],
    ['state matches', fakeBlock('oak_stairs', { facing: 'east', half: 'bottom', shape: 'inner_left' }), [{ name: 'oak_stairs', states: { facing: 'east', half: 'bottom' } }], true],
    ['state differs', fakeBlock('oak_stairs', { facing: 'west', half: 'bottom' }), [{ name: 'oak_stairs', states: { facing: 'east' } }], false],
    ['a written stair shape is not compared', fakeBlock('oak_stairs', { facing: 'east', shape: 'straight' }), [{ name: 'oak_stairs', states: { facing: 'east', shape: 'outer_left' } }], true],
    ['chest type is not compared', fakeBlock('chest', { facing: 'south', type: 'left' }), [{ name: 'chest', states: { facing: 'south', type: 'single' } }], true],
    ['a door that stands open is still the door', fakeBlock('oak_door', { facing: 'north', half: 'lower', open: 'true', hinge: 'right' }), [{ name: 'oak_door', states: { facing: 'north', half: 'lower', hinge: 'left', open: 'false' } }], true],
    ['a grown crop is the crop', fakeBlock('wheat', { age: '7' }), [{ name: 'wheat', states: { age: '0' } }], true],
    ['a waterlogged slab is compared for its water', fakeBlock('oak_slab', { type: 'top', waterlogged: 'false' }), [{ name: 'oak_slab', states: { type: 'top', waterlogged: 'true' } }], false],
    ['air matches cave air', fakeBlock('cave_air'), [{ name: 'air', states: {} }], true],
    ['air does not match grass', fakeBlock('short_grass'), [{ name: 'air', states: {} }], false],
    ['a plain api.block answer with .properties still compares', { name: 'oak_stairs', properties: { facing: 'east' }, solid: true }, [{ name: 'oak_stairs', states: { facing: 'east' } }], true],
    ['unloaded never matches', null, [{ name: 'air', states: {} }], false]
  ]
  for (const [label, block, alts, want] of rows) assert.equal(matchesCell(block, alts), want, label)
})

// ---------------------------------------------------------------- jobs, order, stages

const AT = { x: 100, y: 65, z: -20 }

test('jobsFor: a flat site wants every cell of the hut but the air ones already clear', () => {
  const bp = resolve(hut())
  const jobs = jobsFor(bp, AT, flatGround(64))
  // 25 cells at y-1 (dig the turf, then place), 16 wall cells at y0/y1/y2 each plus the furniture, 25 roof
  assert.equal(jobs.filter(j => j.do === 'dig').length, 25)
  assert.equal(jobs.filter(j => j.do === 'place').length, 25 + 20 + 16 + 16 + 25)
  assert.equal(jobs.filter(j => j.item === 'oak_door').length, 1)
  assert.equal(jobs.filter(j => j.item === 'white_bed').length, 1)
  const door = jobs.find(j => j.item === 'oak_door')
  assert.deepEqual([door.x, door.y, door.z, door.facing], [102, 65, -16, 'north'])
})

test('jobsFor: a half-built hut asks only for what is missing', () => {
  const bp = resolve(hut())
  const whole = Object.fromEntries(jobsFor(bp, AT, flatGround(64)).filter(j => j.do === 'place').map(j => [`${j.x},${j.y},${j.z}`, fakeBlock(j.block.name, j.block.states)]))
  const half = Object.fromEntries(Object.entries(whole).filter(([, b]) => b.name !== 'oak_planks'))
  const jobs = jobsFor(bp, AT, worldOf({ ...half, ...Object.fromEntries(Object.keys(whole).filter(k => !half[k]).map(k => [k, fakeBlock('air')])) }))
  assert.deepEqual(new Set(jobs.map(j => j.item)), new Set(['oak_planks']))
  assert.equal(jobs.length, 34)
  assert.equal(jobsFor(bp, AT, worldOf(whole)).length, 0)
})

test('jobsFor: a . cell holding grass is a dig, a wall cell holding planks is an obstacle', () => {
  const bp = resolve(hut())
  const world = worldOf({ '102,65,-19': fakeBlock('short_grass'), '101,66,-20': fakeBlock('oak_planks') })
  const jobs = jobsFor(bp, AT, world)
  assert.deepEqual(jobs.find(j => j.x === 102 && j.y === 65 && j.z === -19 && j.do === 'dig')?.natural, true)
  const wall = jobs.find(j => j.x === 101 && j.y === 66 && j.z === -20 && j.do === 'dig')
  assert.deepEqual({ natural: wall.natural, was: wall.was }, { natural: false, was: 'oak_planks' })
})

// the bed's head and the door's upper half are placed by the game with the first half, so terrain left in them makes the
// placement fail: their cells are dug before the first half, in its layer (Hollis's hut, 09-26: grass in the head cell)
test('jobsFor and orderJobs: terrain in the other half of a bed or a door is dug before the first half is placed', () => {
  const rows = [
    ['bed head', '102,65,-18', 'grass_block', 'white_bed'],
    ['door upper', '102,66,-16', 'oak_leaves', 'oak_door']
  ]
  const bp = resolve(hut())
  for (const [label, cell, was, item] of rows) {
    const world = worldOf({ [cell]: fakeBlock(was) })
    const dig = jobsFor(bp, AT, world).find(j => `${j.x},${j.y},${j.z}` === cell)
    assert.deepEqual({ do: dig?.do, was: dig?.was, natural: dig?.natural }, { do: 'dig', was, natural: true }, label)
    const { jobs } = orderJobs(jobsFor(bp, AT, world), bp, AT, world)
    const dug = jobs.findIndex(j => `${j.x},${j.y},${j.z}` === cell && j.do === 'dig')
    const placed = jobs.findIndex(j => j.item === item)
    assert.ok(dug >= 0 && dug < placed, `${label}: dug at ${dug}, placed at ${placed}`)
    const staged = stages(jobs, 99).flatMap(s => s.jobs)
    assert.ok(staged.findIndex(j => j.do === 'dig' && `${j.x},${j.y},${j.z}` === cell) < staged.findIndex(j => j.item === item), `${label}: staged in order`)
  }
})

test('orderJobs: layers bottom-up, digs first, walls out from the ground, furniture after the floor, torch after the table', () => {
  const bp = resolve(hut())
  const { jobs } = orderJobs(jobsFor(bp, AT, flatGround(64)), bp, AT, flatGround(64))
  const ys = jobs.map(j => j.y)
  assert.deepEqual(ys, [...ys].sort((a, b) => a - b))
  const first = jobs.filter(j => j.y === 64)
  assert.ok(first.slice(0, 25).every(j => j.do === 'dig'))
  const at = (item) => jobs.findIndex(j => j.item === item)
  assert.ok(at('crafting_table') < at('torch'))
  assert.ok(at('white_bed') > jobs.findIndex(j => j.y === 64 && j.item === 'oak_planks'))
  assert.ok(jobs.every(j => j.stand), 'every job has a cell to stand in')
})

test('orderJobs: the last wall cell that would wall the body in is done from outside', () => {
  // a 3x3 ring of stone two high with no door: the body must end outside
  const bp = resolve(parse({ legend: 'S  cobblestone', layers: [[0, 'SSS\nS.S\nSSS'], [1, 'SSS\nS.S\nSSS'], [2, 'SSS\nSSS\nSSS']] }))
  const { jobs } = orderJobs(jobsFor(bp, AT, flatGround(64)), bp, AT, flatGround(64))
  const inside = s => s.x === 101 && s.z === -19
  assert.ok(jobs.filter(j => j.y === 67).every(j => !inside(j.stand)), 'the roof is laid from outside once the walls are up')
})

test('orderJobs: the watchtower platform grows outward from the shaft', () => {
  const bp = resolve(tower())
  const { jobs } = orderJobs(jobsFor(bp, AT, flatGround(64)), bp, AT, flatGround(64))
  const platform = jobs.filter(j => j.y === 75 && j.item === 'oak_planks')
  const ring = platform.slice(-16)
  assert.ok(platform.slice(0, 8).every(j => j.x >= 101 && j.x <= 103 && j.z >= -19 && j.z <= -17), 'the shaft top first')
  assert.ok(ring.every(j => j.x === 100 || j.x === 104 || j.z === -20 || j.z === -16), 'the overhang last')
})

test('stages: cut on layer boundaries by the carry', () => {
  const bp = resolve(tower())
  const { jobs } = orderJobs(jobsFor(bp, AT, flatGround(64)), bp, AT, flatGround(64))
  const one = stages(jobs, 27)
  assert.equal(one.length, 1)
  const many = stages(jobs, 3)
  assert.ok(many.length > 1)
  for (const stage of many.slice(0, -1)) assert.notEqual(stage.to, many[many.indexOf(stage) + 1].from, 'a stage ends on a whole layer')
  assert.ok(many.every(s => stackSlots(s.bill) <= 3))
  assert.deepEqual(many.map(s => s.n), many.map((s, i) => i + 1))
})

test('stages: a layer bigger than the carry is split inside its own order', () => {
  const bp = resolve(parse({ legend: 'S  cobblestone', layers: [[0, Array(3).fill('S'.repeat(64)).join('\n')]] }))
  const { jobs } = orderJobs(jobsFor(bp, AT, flatGround(64)), bp, AT, flatGround(64))
  const cut = stages(jobs, 1)
  assert.deepEqual(cut.map(s => s.jobs.length), [64, 64, 64])
  assert.deepEqual(cut.map(s => [s.from, s.to]), [[0, 0], [0, 0], [0, 0]])
})

test('stageLine: the table check prints', () => {
  const rows = [
    [{ n: 1, of: 3, from: -1, to: 1, bill: { cobblestone: 64, oak_planks: 40 } }, { cobblestone: 64, oak_planks: 40 }, 'stage 1/3 y-1..y1 carry=cobblestone:64 oak_planks:40 have=all'],
    [{ n: 2, of: 3, from: 67, to: 67, bill: { cobblestone: 120 } }, { cobblestone: 64 }, 'stage 2/3 y67 carry=cobblestone:120 short=cobblestone:56']
  ]
  for (const [stage, have, want] of rows) assert.equal(stageLine(stage, have), want)
})

// ---------------------------------------------------------------- the site

const NOBODY = { zones: [], places: [], me: 'Tester' }

test('siteCheck: clear flat ground is clear', () => {
  assert.deepEqual(siteCheck(resolve(hut()), AT, flatGround(64), NOBODY), { obstacles: [], foundation: [], clearance: [], unloaded: [], overlaps: [], refusal: null })
})

test('siteCheck: findings and the refusal sentence', () => {
  const rows = [
    ['planks in a wall cell', { '101,66,-20': fakeBlock('oak_planks') }, NOBODY, s => s.obstacles.map(o => `${o.name} ${o.x},${o.y},${o.z}`), ['oak_planks 101,66,-20'], 'oak_planks at 101,66,-20 is in the way of S: dig it, or run again with clear=true'],
    ['a chest in a wall cell is never dug', { '101,66,-20': fakeBlock('chest', { facing: 'south' }) }, NOBODY, s => s.obstacles.map(o => o.keep), [true], 'chest at 101,66,-20 is in the way of S and a container is never dug: move it, or move the anchor'],
    ['grass under a dirt|@solid cell is fine', { '100,64,-20': fakeBlock('grass_block') }, NOBODY, s => s.obstacles, [], null],
    ['water under a support', { '103,63,-17': fakeBlock('water', { level: '0' }) }, NOBODY, s => s.foundation.map(f => `${f.name} ${f.x},${f.y},${f.z}`), ['water 103,63,-17'], 'the foundation at 103,63,-17 is water: fill it, choose foundation=any, or move the anchor'],
    ['a log in the clearance', { '102,69,-18': fakeBlock('oak_log') }, NOBODY, s => s.clearance.map(f => `${f.name} ${f.x},${f.y},${f.z}`), ['oak_log 102,69,-18'], 'oak_log at 102,69,-18 is inside the clearance over the roof: dig it, or move the anchor'],
    ['a chunk not loaded', { '104,65,-16': null }, NOBODY, s => s.unloaded.map(f => `${f.x},${f.y},${f.z}`), ['104,65,-16'], 'the chunk under 104,65,-16 is not loaded: stand within 60 blocks of the site and check again'],
    ['inside a zone', {}, { ...NOBODY, zones: [{ name: 'river-pen', x1: 90, y1: 60, z1: -30, x2: 101, y2: 70, z2: -10 }] }, s => s.overlaps, ['river-pen (a protected zone)'], "starter-hut at 100,65,-20 overlaps river-pen (a protected zone): pick another anchor"],
    ["over another agent's place", {}, { ...NOBODY, places: [{ name: 'chani-pen', kind: 'pen', by: 'Chani', x: 104, y: 64, z: -16, plan: '###\n#.#\n###' }] }, s => s.overlaps, ["chani-pen (Chani's)"], "starter-hut at 100,65,-20 overlaps chani-pen (Chani's): pick another anchor"],
    ['my own place does not overlap itself', {}, { ...NOBODY, places: [{ name: 'my-hut', kind: 'build', by: 'Tester', x: 100, y: 65, z: -20, note: 'bp=starter-hut f=south h=00000000' }] }, s => s.overlaps, [], null],
    ["another agent's point place inside the footprint", {}, { ...NOBODY, places: [{ name: 'spot', kind: 'place', by: 'Perrin', x: 102, y: 65, z: -18 }] }, s => s.overlaps, ["spot (Perrin's)"], "starter-hut at 100,65,-20 overlaps spot (Perrin's): pick another anchor"]
  ]
  for (const [label, cells, who, pick, want, refusal] of rows) {
    const out = siteCheck(resolve(hut()), AT, worldOf(cells), who)
    assert.deepEqual(pick(out), want, label)
    assert.equal(out.refusal, refusal, label)
  }
})

test('siteCheck: clear=true keeps containers and beds out of the digs', () => {
  const out = siteCheck(resolve(hut()), AT, worldOf({ '101,66,-20': fakeBlock('oak_planks'), '102,66,-20': fakeBlock('white_bed', { part: 'foot', facing: 'east' }) }), { ...NOBODY, clear: true })
  assert.deepEqual(out.obstacles.map(o => [o.name, o.keep]), [['oak_planks', false], ['white_bed', true]])
  assert.equal(out.refusal, 'white_bed at 102,66,-20 is in the way of S and a bed is never dug: move it, or move the anchor')
})

test('siteCheck: foundation=any fills a hole instead of refusing', () => {
  const bp = resolve(parseBlueprint(read('starter-hut').replace('foundation: flat', 'foundation: any')))
  const out = siteCheck(bp, AT, worldOf({ '100,63,-20': fakeBlock('air') }), NOBODY)
  assert.deepEqual(out.foundation.map(f => f.fill), ['dirt'])
  assert.equal(out.refusal, null)
})

// ---------------------------------------------------------------- the place note and the file hash

test('buildNote and parseNote round-trip inside 80 characters', () => {
  const rows = [
    [{ blueprint: 'starter-hut', facing: 'south', params: {}, hash: 'abcdef01' }, 'bp=starter-hut f=south h=abcdef01'],
    [{ blueprint: 'starter-hut', facing: 'west', params: { wood: 'spruce', bed: 'red' }, hash: 'abcdef01' }, 'bp=starter-hut f=west h=abcdef01 wood=spruce bed=red']
  ]
  for (const [fields, want] of rows) {
    assert.equal(buildNote(fields), want)
    assert.deepEqual(parseNote(want), fields)
    assert.ok(want.length <= 80)
  }
  assert.equal(parseNote('a farm, anyone welcome'), null)
})

test('blueprintHash is eight hex characters of what a build depends on', () => {
  assert.match(blueprintHash(read('starter-hut')), /^[0-9a-f]{8}$/)
  assert.notEqual(blueprintHash(read('starter-hut')), blueprintHash(read('watchtower')))
})

// a mark carries the hash, and a build resumes only while it matches: fixing a sentence in the prose must not strand
// every build marked from the file (hollis-hut and pacer-hut, 09-27), while a change to what gets built must
const HUT = read('starter-hut')
for (const [label, edit, same] of [
  ['the title', t => t.replace('title: Starter hut', 'title: A hut'), true],
  ['the description', t => t.replace('description: A 5x5', 'description: A small 5x5'), true],
  ['the notes', t => t.replace('notes: The floor', 'notes: Its floor'), true],
  ['the difficulty and the author', t => t.replace('difficulty: easy', 'difficulty: medium').replace('by: blueprint', 'by: someone'), true],
  ['the prose between the blocks', t => t.replace('Written for this library, not transcribed.', 'Written here.'), true],
  ['a comment and the heading in the prose', t => t.replace('# Starter hut', '# The starter hut\n\n<!-- checked 09-27 -->'), true],
  ['a layer grid', t => t.replace('ST.iS', 'STi.S'), false],
  ['a legend block', t => t.replace('T  crafting_table', 'T  barrel'), false],
  ['a legend state', t => t.replace('^  {wood:stairs}[facing=west,half=bottom]', '^  {wood:stairs}[facing=east,half=bottom]'), false],
  ['a legend tag', t => t.replace('#entrance\nd', '#door\nd'), false],
  ['a parameter default', t => t.replace('wood=oak', 'wood=spruce'), false],
  ['the front', t => t.replace('front: south', 'front: north'), false],
  ['the foundation', t => t.replace('foundation: flat', 'foundation: any'), false],
  ['the clearance', t => t.replace('clearance: 1', 'clearance: 2'), false],
  ['the tags (the kind a finished build is marked)', t => t.replace('tags: shelter, storage', 'tags: shelter'), false]
]) {
  test(`blueprintHash: an edit to ${label} ${same ? 'keeps' : 'changes'} it`, () => {
    const edited = edit(HUT)
    assert.notEqual(edited, HUT, 'the edit applies')
    assert.equal(blueprintHash(edited) === blueprintHash(HUT), same)
  })
}

test('renderLayer prints the grid after rotation with its y', () => {
  assert.equal(renderLayer(resolve(hut()), 0), 'y0\nLSSSL\nS^.CS\nSFH.S\nST.iS\nLSDSL')
  assert.equal(renderLayer(rotate(resolve(hut()), 1), 0).split('\n')[3], 'D.H.S')
})

// ---------------------------------------------------------------- farms as blueprints

test('farmPlanToBlueprint: the old legend as three layers', () => {
  const bp = farmPlanToBlueprint('w~w')
  assert.deepEqual(bp.errors, [])
  assert.deepEqual(bp.layers.map(l => l.y), [-1, 0, 1])
  const cover = blueprintCells(bp).find(c => c.dy === -1 && c.dx === 1)
  assert.deepEqual(cover.spec.alts[0], { name: 'oak_slab', states: { type: 'top', waterlogged: 'true' } })
  assert.deepEqual(cover.spec.tags, ['ground', 'cover', 'lane'])
  const lane = blueprintCells(bp).find(c => c.dy === 0 && c.dx === 1)
  assert.deepEqual(lane.spec.alts[0].name, 'air')
  const wheat = blueprintCells(bp).find(c => c.dy === 0 && c.dx === 0)
  assert.deepEqual([wheat.spec.alts[0].name, wheat.spec.tags], ['wheat', ['crop']])
  const ground = blueprintCells(bp).find(c => c.dy === -1 && c.dx === 0)
  assert.deepEqual(ground.spec.alts[0].name, 'farmland')
})

test('farmPlanToBlueprint: a torch post is a fence at y0 and a torch at y1', () => {
  const bp = farmPlanToBlueprint('T#G')
  const at = (dx, dy) => blueprintCells(bp).find(c => c.dx === dx && c.dy === dy)?.spec.alts[0].name
  assert.deepEqual([at(0, -1), at(0, 0), at(0, 1), at(1, 0), at(1, 1), at(2, 0)], ['dirt', 'oak_fence', 'torch', 'oak_fence', undefined, 'oak_fence_gate'])
})

function siteCheck (bp, at, world, context) { return rawSiteCheck(bp, at, world, { ...context, places: context.places.map(canonicalFixture) }) }
