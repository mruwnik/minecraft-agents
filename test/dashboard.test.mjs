import { legacyBlueprintDetail as blueprintDetail } from './helpers/blueprint-legacy.mjs'
import { canonicalFixture } from './plan-fixture.mjs'
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { parseAgents, snapshotFile, streamFrames, inventoryIcon, route, mergeChat, parseEventLines, chatLimit, actionLog, parseScan, scanBoxes, nearestBody, groupWorlds, findPlace, unsureWater as rawUnsureWater, parseWorldList, resolveWorld, scopeSnapshot, scopeChatSources, agentInWorld } from '../tools/dashboard/lib.mjs'
import { mergeBodies, humanSightings, mapPoints, fitPoints, isEmptyWorld, worldBounds, fitView, project, zoneRect, fitLabels, onCanvas, planRects as rawPlanRects, cellColour, cellLabel, hitPlan, hitVillagePlace, planDiff as rawPlanDiff, cellExpectation, worldColour, worldLabel } from '../tools/dashboard/map.mjs'
import { villageViews, attachVillageStatus } from '../tools/dashboard/villages.mjs'
import { blueprintRow, layerCells, hoverText, legendRows, billRows, lintLines, blockColour, altColour, familyOf } from '../tools/dashboard/blueprint.mjs'
import { parseBlueprint, resolve, bill, lint } from '../src/blueprint/format.mjs'

const planRects = places => rawPlanRects(places.map(canonicalFixture))
const planDiff = (place, world) => rawPlanDiff(canonicalFixture(place), world)
const unsureWater = (place, world) => rawUnsureWater(canonicalFixture(place), world)

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')

// ---------------------------------------------------------------- reading the agent folders
const config = (username, apiPort, extra = {}) => JSON.stringify({ username, apiPort, harness: 'claude-code', ...extra })

test('parseAgents: one entry per readable config, sorted by name; the world is the folder\'s, not the config\'s', () => {
  assert.deepEqual(parseAgents([
    { name: 'Mariel', world: 'test', text: config('Mariel', 3790) },
    { name: 'Claude', world: 'main', text: config('Claude', 3777, { world: 'elsewhere', character: { name: 'Claude', source: 'chosen by hand' } }) }
  ]), [
    { name: 'Claude', username: 'Claude', apiPort: 3777, harness: 'claude-code', character: 'Claude (chosen by hand)', world: 'main' },
    { name: 'Mariel', username: 'Mariel', apiPort: 3790, harness: 'claude-code', character: null, world: 'test' }
  ])
})

const broken = [
  ['not JSON at all', { name: 'Ruin', text: '{oops' }],
  ['no apiPort', { name: 'Ruin', text: JSON.stringify({ username: 'Ruin' }) }],
  ['apiPort that is not a number', { name: 'Ruin', text: config('Ruin', 'three thousand') }],
  ['an empty file', { name: 'Ruin', text: '' }]
]
broken.forEach(([why, entry]) => test(`parseAgents: skips a folder with ${why}`, () => {
  assert.deepEqual(parseAgents([entry, { name: 'Claude', text: config('Claude', 3777) }]).map(a => a.name), ['Claude'])
}))

test('parseAgents: a config without username falls back to the folder name', () => {
  assert.deepEqual(parseAgents([{ name: 'Chani', text: JSON.stringify({ apiPort: 3788 }) }])[0].username, 'Chani')
})

// ---------------------------------------------------------------- merging the polls
const agents = [
  { name: 'Claude', username: 'Claude', apiPort: 3777, harness: 'claude-code', character: null, world: 'main' },
  { name: 'Perrin', username: 'Perrin', apiPort: 3789, harness: 'claude-code', character: null, world: 'main' }
]
const claudeState = { hp: 20, food: 15, pos: { x: 60.5, y: 65, z: -151.6 }, doing: 'goto 2s', players: {} }

test('mergeBodies: a body that answered is up and carries its state', () => {
  assert.deepEqual(mergeBodies(agents, { 'main/Claude': { ok: true, state: claudeState, at: 1000 } })[0], {
    ...agents[0], up: true, error: null, state: claudeState, at: 1000
  })
})

const missing = [
  ['never polled', undefined, 'not polled yet'],
  ['refused the connection', { ok: false, error: 'connect ECONNREFUSED 127.0.0.1:3789', at: 900 }, 'connect ECONNREFUSED 127.0.0.1:3789']
]
missing.forEach(([why, poll, error]) => test(`mergeBodies: a body that ${why} is down, not an error`, () => {
  const row = mergeBodies(agents, { 'main/Perrin': poll })[1]
  assert.deepEqual([row.name, row.up, row.error, row.state], ['Perrin', false, error, null])
}))

test('mergeBodies: a body that answered ok:false is down with the body its own reason', () => {
  const poll = { ok: false, error: 'bot is not connected to the server (retrying every 10s)', at: 5 }
  assert.equal(mergeBodies(agents, { 'main/Claude': poll })[0].up, false)
})

test('mergeBodies: the same name in two worlds is two bodies, each with its own poll', () => {
  const twins = [{ name: 'Claude', world: 'main' }, { name: 'Claude', world: 'test' }]
  assert.deepEqual(mergeBodies(twins, { 'test/Claude': { ok: true, state: claudeState, at: 1 } }).map(b => b.up), [false, true])
})

test('mergeBodies: every agent gets a row, in the order given', () => {
  assert.deepEqual(mergeBodies(agents, {}).map(b => b.name), ['Claude', 'Perrin'])
})

// ---------------------------------------------------------------- where the humans are
// a body's `state` lists every player it knows of, agents included: a human is any of them whose name is no agent's
const seer = (name, players, at) => ({ name, up: true, at, state: { ...claudeState, players } })
const agentNames = ['Claude', 'Chani']

test('humanSightings: one entry per human, at the freshest sighting', () => {
  assert.deepEqual(humanSightings([
    seer('Claude', { Steve: { x: 1, y: 65, z: 2 }, Chani: { x: 9, y: 65, z: 9 } }, 1000),
    seer('Chani', { Steve: { x: 3, y: 66, z: 4 }, Alex: { x: 5, y: 66, z: 6 } }, 2000)
  ], agentNames), [
    { name: 'Alex', x: 5, y: 66, z: 6, seenBy: 'Chani', at: 2000 },
    { name: 'Steve', x: 3, y: 66, z: 4, seenBy: 'Chani', at: 2000 }
  ])
})

test('humanSightings: an agent seen by another agent is not a human', () => {
  assert.deepEqual(humanSightings([seer('Claude', { Chani: { x: 1, y: 65, z: 2 } }, 1000)], agentNames), [])
})

const blind = [
  ['nobody lists anyone', [seer('Claude', {}, 1000)]],
  ['the only human is out of sight', [seer('Claude', { Steve: 'out of sight' }, 1000)]],
  ['the only body that sees one is down', [{ name: 'Claude', up: false, at: 1, state: null }]],
  ['there are no bodies', []]
]
blind.forEach(([why, bodies]) => test(`humanSightings: nobody when ${why}`, () => {
  assert.deepEqual(humanSightings(bodies, agentNames), [])
}))

// ---------------------------------------------------------------- the map
const places = [{ name: 'hut', x: 116, y: 69, z: -141 }]
const zones = [{ name: 'pen', x1: 100, y1: 60, z1: -150, x2: 110, y2: 70, z2: -140 }]

test('mapPoints: every body, place, zone corner and human is a point to fit', () => {
  assert.deepEqual(mapPoints([seer('Claude', {}, 1)], places, zones, [{ name: 'Steve', x: 0, y: 64, z: 0 }]), [
    { x: 60.5, z: -151.6 }, { x: 116, z: -141 }, { x: 100, z: -150 }, { x: 110, z: -140 }, { x: 0, z: 0 }
  ])
})

test('mapPoints: a body that is down contributes nothing, and there may be no humans', () => {
  assert.deepEqual(mapPoints([{ name: 'Perrin', up: false, state: null }], [], [], []), [])
})

test('worldBounds: the box around the points, padded', () => {
  assert.deepEqual(worldBounds([{ x: 0, z: 0 }, { x: 10, z: -4 }], 5), { minX: -5, maxX: 15, minZ: -9, maxZ: 5 })
})

test('worldBounds: no points is no box', () => {
  assert.equal(worldBounds([], 5), null)
})

test('fitView: uniform scale, the shorter side centred', () => {
  assert.deepEqual(fitView({ minX: 0, maxX: 100, minZ: 0, maxZ: 50 }, 200, 200), { scale: 2, originX: 0, originZ: -25 })
})

test('fitView: a single point still gives a usable scale', () => {
  const view = fitView({ minX: 5, maxX: 5, minZ: 5, maxZ: 5 }, 100, 100)
  assert.equal(Number.isFinite(view.scale) && view.scale > 0, true)
})

test('fitView: no bounds is no view', () => {
  assert.equal(fitView(null, 200, 200), null)
})

const corners = [
  ['the north-west corner', 0, 0, { px: 0, py: 50 }],
  ['the south-east corner', 100, 50, { px: 200, py: 150 }],
  ['east is right and south is down', 50, 25, { px: 100, py: 100 }]
]
corners.forEach(([what, x, z, expected]) => test(`project: ${what}`, () => {
  assert.deepEqual(project(fitView({ minX: 0, maxX: 100, minZ: 0, maxZ: 50 }, 200, 200), x, z), expected)
}))

test('zoneRect: a zone given back to front still has a positive width', () => {
  const view = fitView({ minX: 0, maxX: 100, minZ: 0, maxZ: 100 }, 100, 100)
  assert.deepEqual(zoneRect(view, { x1: 40, z1: 60, x2: 20, z2: 10 }), { px: 20, py: 10, w: 20, h: 50 })
})

// ---------------------------------------------------------------- labels that would sit on top of each other
const box = (text, px, py, w = 10, h = 10) => ({ text, px, py, w, h })

test('fitLabels: labels that clear each other are all kept', () => {
  assert.deepEqual(fitLabels([box('a', 0, 0), box('b', 20, 0), box('c', 0, 20)]).map(l => l.text), ['a', 'b', 'c'])
})

test('fitLabels: the label offered first wins the space', () => {
  assert.deepEqual(fitLabels([box('body', 0, 0), box('place', 5, 5), box('far', 40, 40)]).map(l => l.text), ['body', 'far'])
})

test('fitLabels: touching edges do not count as overlapping', () => {
  assert.deepEqual(fitLabels([box('a', 0, 0), box('b', 10, 0)]).map(l => l.text), ['a', 'b'])
})

test('fitLabels: nothing to place is nothing kept', () => {
  assert.deepEqual(fitLabels([]), [])
})

const offCanvas = [
  ['left of it', { px: -40, py: 50 }],
  ['right of it', { px: 340, py: 50 }],
  ['above it', { px: 50, py: -40 }],
  ['below it', { px: 50, py: 240 }]
]
offCanvas.forEach(([where, at]) => test(`onCanvas: a mark ${where} is not drawn`, () => {
  assert.equal(onCanvas(at, 300, 200, 20), false)
}))

test('onCanvas: a mark just inside the margin is drawn', () => {
  assert.equal(onCanvas({ px: -19, py: 219 }, 300, 200, 20), true)
})

// ---------------------------------------------------------------- serving files and routes
const home = '/srv/bots/state/worlds/main/agents/Claude'

test('snapshotFile: a look answer resolves under the body home', () => {
  assert.equal(snapshotFile(home, 'snapshots/look-009.png'), path.join(home, 'snapshots/look-009.png'))
})

const escapes = [
  ['climbs out of the home', '../Chani/snapshots/look-001.png'],
  ['is an absolute path elsewhere', '/etc/passwd'],
  ['is outside the snapshots folder', 'config.json'],
  ['is missing', undefined]
]
escapes.forEach(([why, file]) => test(`snapshotFile: refuses a path that ${why}`, () => {
  assert.equal(snapshotFile(home, file), null)
}))

// the inventory screen's icon for an item: its own picture, else the block it places, as the game draws it
const RED = [200, 0, 0, 255], GREEN = [0, 200, 0, 255], BLUE = [0, 0, 200, 255]
const texture = rgba => ({ width: 16, height: 16, rgba: Uint8Array.from({ length: 16 * 16 * 4 }, (_, i) => rgba[i % 4]) })
const textures = Object.fromEntries([
  ['item/iron_pickaxe', RED], ['item/compass_00', RED], ['item/crossbow_standby', RED], ['poppy', GREEN], ['oak_log_top', RED], ['oak_log', BLUE],
  ['oak_planks', BLUE], ['furnace_top', RED], ['furnace_front', GREEN], ['furnace_side', BLUE]
].map(([name, rgba]) => [name, texture(rgba)]))
const iconPixel = (icon, x, y) => [icon.width, [...icon.rgba.subarray((y * icon.width + x) * 4, (y * icon.width + x) * 4 + 4)]]
for (const [name, item, x, y, expected] of [
  ['an item with a picture of its own is drawn as it', 'iron_pickaxe', 8, 8, [16, RED]],
  ['an item whose picture turns (a compass) shows its first frame', 'compass', 8, 8, [16, RED]],
  ['a crossbow shows its unloaded picture', 'crossbow', 8, 8, [16, RED]],
  ['a block you walk through (a flower) is flat, like its item', 'poppy', 8, 8, [16, GREEN]],
  ['a solid block is a cube: its top on top', 'oak_log', 16, 4, [32, RED]],
  ['a solid block is a cube: its side on the right', 'oak_log', 26, 18, [32, [0, 0, 120, 255]]],
  ['a block with a front shows it on the left, as the game does', 'furnace', 6, 18, [32, [0, 160, 0, 255]]],
  ['a block drawn from another\'s picture (a chest)', 'chest', 26, 18, [32, [0, 0, 120, 255]]]
]) {
  test(`inventoryIcon: ${name}`, () => assert.deepEqual(iconPixel(inventoryIcon(item, n => textures[n] ?? null), x, y), expected))
}
test('inventoryIcon: nothing to draw it from is no icon (the page writes its initials)', () =>
  assert.equal(inventoryIcon('shield', n => textures[n] ?? null), null))

const routes = [
  ['/', { kind: 'page' }],
  ['/index.html', { kind: 'page' }],
  ['/api/state', { kind: 'state' }],
  ['/api/villagers', { kind: 'villagersApi' }],
  ['/villagers', { kind: 'villagers' }],
  ['/api/villages', { kind: 'villagesApi' }],
  ['/villages?place=market', { kind: 'villages' }],
  ['/api/state?since=3', { kind: 'state' }],
  ['/api/chat', { kind: 'chat' }],
  ['/api/chat?limit=50', { kind: 'chat' }],
  ['/api/chat/', { kind: 'unknown' }],
  ['/api/world?place=jizo-cane', { kind: 'world' }],
  ['/api/world/jizo-cane', { kind: 'unknown' }],
  ['/map.mjs', { kind: 'script' }],
  ['/src/lib.mjs', { kind: 'srclib', name: 'lib.mjs' }],
  // lib.mjs is not the only browser-safe file under src/ that the map module graph can end up importing (it
  // currently re-exports everything from cli.mjs) - any plain .mjs filename directly under src/ is servable,
  // so a new transitive import never again leaves the page silently failing to load its module script.
  ['/src/cli.mjs', { kind: 'srclib', name: 'cli.mjs' }],
  ['/src/bot.mjs', { kind: 'srclib', name: 'bot.mjs' }],
  ['/src/../lib.mjs', { kind: 'unknown' }],
  ['/src/sub/lib.mjs', { kind: 'unknown' }],
  ['/src/lib.txt', { kind: 'unknown' }],
  // lib.mjs's split: the browser also needs the modules it re-exports, one lib/ segment deep
  ['/src/lib/plan.mjs', { kind: 'srclib', name: 'lib/plan.mjs' }],
  ['/src/navigation/walk.mjs', { kind: 'srclib', name: 'navigation/walk.mjs' }],
  ['/src/build/materials.mjs', { kind: 'srclib', name: 'build/materials.mjs' }],
  ['/src/body/runner.mjs', { kind: 'unknown' }],
  // the blueprint library: its page, its module, the list and one blueprint by its kebab-case file name
  ['/blueprints', { kind: 'blueprints' }],
  ['/blueprint.mjs', { kind: 'bpscript' }],
  ['/api/blueprints', { kind: 'bplist' }],
  ['/api/blueprint/starter-hut', { kind: 'blueprint', name: 'starter-hut' }],
  ['/api/blueprint/Starter_Hut', { kind: 'unknown' }],
  ['/api/blueprint/', { kind: 'unknown' }],
  ['/api/blueprint/../etc', { kind: 'unknown' }],
  ['/api/look/Chani', { kind: 'look', name: 'Chani' }],
  ['/api/look/Chani?fresh=1', { kind: 'look', name: 'Chani' }],
  ['/api/look/Chani/live', { kind: 'live', name: 'Chani' }],
  ['/api/look/', { kind: 'unknown' }],
  ['/api/look/../../etc/passwd', { kind: 'unknown' }],
  ['/api/screen/Chani', { kind: 'screen', name: 'Chani' }],
  ['/api/screen/', { kind: 'unknown' }],
  ['/api/screen/../../etc/passwd', { kind: 'unknown' }],
  ['/api/actions/Chani', { kind: 'actions', name: 'Chani' }],
  ['/api/whisper/Chani', { kind: 'whisper', name: 'Chani' }],
  ['/api/actions/', { kind: 'unknown' }],
  ['/api/actions/../../etc/passwd', { kind: 'unknown' }],
  ['/api/icon/oak_log', { kind: 'icon', name: 'oak_log' }],
  ['/api/icon/Oak_Log', { kind: 'unknown' }],
  ['/api/icon/../textures/dirt', { kind: 'unknown' }],
  ['/nope', { kind: 'unknown' }]
]
routes.forEach(([url, expected]) => test(`route: ${url}`, () => {
  assert.deepEqual(route(url), expected)
}))

// a fake clock: each frame takes `drawMs`, and the stream closes after `frames` have been sent
const streamed = async ({ drawMs, frames, error }) => {
  let now = 0
  const sent = []
  const waits = []
  await streamFrames({
    frame: async () => { now += drawMs; return error ? { error } : { png: `frame${sent.length}` } },
    send: f => sent.push(f),
    open: () => sent.length < frames,
    wait: async ms => { waits.push(ms); now += ms },
    now: () => now,
    minMs: 100,
    retryMs: 1000
  })
  return { sent, waits }
}
const streamCases = [
  ['a quick body is held to one frame per 100 ms', { drawMs: 30, frames: 3 }, { sent: ['frame0', 'frame1', 'frame2'], waits: [70, 70, 70] }],
  ['a slow body is asked again the moment a frame arrives', { drawMs: 250, frames: 2 }, { sent: ['frame0', 'frame1'], waits: [0, 0] }],
  ['a body that cannot draw is asked again a second later', { drawMs: 10, frames: 2, error: 'no answer' }, { sent: ['no answer', 'no answer'], waits: [990, 990] }]
]
streamCases.forEach(([name, given, expected]) => test(`live look: ${name}`, async () => {
  const { sent, waits } = await streamed(given)
  assert.deepEqual({ sent: sent.map(f => f.png ?? f.error), waits }, expected)
}))

test('village views: saved plan intent and only fresh saved inspection evidence become current counts', () => {
  const source = JSON.parse(fs.readFileSync(path.join(ROOT, 'examples', 'village-five.blueprint.json'), 'utf8'))
  source.population = { target: 5, roles: [{ id: 'east-composter', count: 1, profession: 'farmer', workstation: [3, 0, 3], trade: { output: 'enchanted_book', enchant: 'efficiency', level: 3, atLeast: true } }] }
  const place = { name: 'market', kind: 'shelter', x: 10, y: 65, z: -5, note: 'bp2:abc' }
  const manifest = { place: 'market', at: { x: 10, y: 65, z: -5 }, facing: 'south', source: { ...source, population: undefined } }
  const observedAt = new Date(100000).toISOString(), uuid = '11111111-1111-4111-8111-111111111111'
  const inspection = { version: 1, place: 'market', at: manifest.at, source, population: source.population, observedAt, report: { satisfied: true, populationStatus: 'satisfied', population: 6, surplus: 1, assigned: [uuid], roles: [{ id: 'east-composter', status: 'satisfied', uuids: [uuid] }], workspaces: [{ id: 'east-composter', at: { x: 13, y: 65, z: -2 }, block: 'composter', status: 'satisfied', uuids: [uuid], associations: [{ uuid, status: 'verified', reason: 'exact claim observed', source: 'villager.roll', basis: 'workstation event and confirmed profession' }] }], unknown: [], requiredBeds: 6, structure: { complete: true, usableBeds: 10, missing: [], unknown: [] } } }
  const roster = { villagers: { [uuid]: { uuid, profession: 'farmer', age: 'adult', offers: { observedAt, items: [] } } } }
  const fresh = villageViews({ places: [place], manifests: [manifest], inspections: [inspection], roster, now: 110000 })[0]
  assert.equal(fresh.state, 'satisfied'); assert.equal(fresh.observed, 6); assert.equal(fresh.population.target, 5)
  assert.deepEqual(fresh.bounds, { x: 10, y: 65, z: -5, width: 10, depth: 10, height: 4 }); assert.equal(fresh.members[0].uuid, uuid)
  assert.equal(fresh.roles[0].observed, 1); assert.equal(fresh.members[0].sightingFresh, false)
  assert.deepEqual(fresh.roles[0].trade, { output: 'enchanted_book', enchant: 'efficiency', level: 3, atLeast: true })
  assert.deepEqual(fresh.workspaces.map(w => [w.id,w.profession,w.at,w.block,w.status,w.uuids]), [['east-composter','farmer',{x:13,y:65,z:-2},'composter','satisfied',[uuid]]])
  const stale = villageViews({ places: [place], manifests: [manifest], inspections: [inspection], roster, now: 500000 })[0]
  assert.equal(stale.state, 'stale'); assert.equal(stale.observed, null); assert.equal(stale.lastObservedPopulation, 6)
  assert.equal(stale.roles[0].observed, null); assert.equal(stale.roles[0].lastObserved, 1)
  assert.equal(stale.workspaces[0].status, 'unknown'); assert.equal(stale.workspaces[0].lastStatus, 'satisfied'); assert.deepEqual(stale.workspaces[0].uuids, [])
})

test('village details show trade constraints and workspace evidence instead of an unverified-claim blanket', () => {
  const html = fs.readFileSync(path.join(ROOT, 'tools', 'dashboard', 'villages.html'), 'utf8')
  assert.match(html, /desired offer: \$\{tradeLabel\(r\.trade\)\}/)
  assert.match(html, /trade\.atLeast\?'at least '/)
  assert.match(html, /workspace \$\{w\.status\}/)
  assert.doesNotMatch(html, /villager claim not verified/)
})

test('workspace view keeps missing or unproven station bindings unknown instead of inferring from a global role match', () => {
  const source = JSON.parse(fs.readFileSync(path.join(ROOT, 'examples', 'village-five.blueprint.json'), 'utf8'))
  source.population = { target: 1, roles: [{ id: 'farm', profession: 'farmer' }], workspaces: [{ id: 'farmer-east', at: [3,0,3], profession: 'farmer' }] }
  const inspection = { place: 'ws', at: {x:0,y:64,z:0}, source, population: source.population, observedAt: new Date(100000).toISOString(), report: { satisfied: false, population: 1, roles: [{id:'farm',status:'satisfied',uuids:['u']}], workspaces: [] } }
  const [view] = villageViews({ places: [{name:'ws',kind:'village',x:0,y:64,z:0}], inspections: [inspection], now: 100001 })
  assert.equal(view.roles[0].status, 'satisfied')
  assert.equal(view.workspaces[0].status, 'unknown')
  assert.equal(view.workspaces[0].at, null)
  assert.deepEqual(view.workspaces[0].uuids, [])
})

test('village views: explicit villages remain visible without a population plan; unrelated blueprints are not villages', () => {
  const places = [
    { name: 'old-village', kind: 'village', x: 1, y: 64, z: 2 },
    { name: 'plain-hut', kind: 'shelter', x: 4, y: 64, z: 5 }
  ]
  const views = villageViews({ places, now: 1000 })
  assert.equal(views.length, 1); assert.equal(views[0].name, 'old-village'); assert.equal(views[0].state, 'unplanned'); assert.equal(views[0].bounds, null)
})

test('village inspection without a surviving place keeps its old map anchor, and map hits prefer nearby village markers', () => {
  const inspection = { version: 1, place: 'lost-village', at: { x: -8, y: 64, z: 3 }, source: null, population: null, observedAt: new Date(1000).toISOString(), report: {} }
  const [view] = villageViews({ inspections: [inspection], now: 2000 })
  assert.deepEqual([view.name, view.x, view.z, view.state], ['lost-village', -8, 3, 'unplanned'])
  assert.deepEqual(attachVillageStatus([], [view]).map(p => [p.name,p.x,p.z,p.village.state]), [['lost-village',-8,3,'unplanned']])
  const places = [{ name: 'ordinary', x: 0, z: 0 }, { name: 'market', x: 10, z: 10, village: view }, { name: 'near-market', x: 10.2, z: 10.1, village: view }]
  assert.equal(hitVillagePlace(places, 10.18, 10.09, 1), 'near-market')
  assert.equal(hitVillagePlace(places, 0, 0, 1), null)
  assert.deepEqual(rawPlanRects([{ name: 'market', x: -8, z: 3, village: { bounds: { x: -8, z: 3, width: 10, depth: 8 } } }]), [{ name: 'market', x: -8, z: 3, w: 10, h: 8 }])
  assert.deepEqual(rawPlanRects([{ name: 'market', x: -8, z: 3, structure: { title: 'old geometry', layers: [{ y: 0, grid: ['###'] }] }, village: { bounds: { x: -8, z: 3, width: 10, depth: 8 } } }]), [{ name: 'market', x: -8, z: 3, w: 10, h: 8 }])
})

// The browser links the page's whole module graph before running any of it, so one module the server cannot serve
// (404 JSON under a .mjs URL: "corrupted" modules in the console) or one `node:` import anywhere below map.mjs leaves
// the page static, with no map and no polling. Walk the graph the way the browser resolves it and check both.
const moduleImports = text => [...text.matchAll(/^(?:import|export)\b[^\n]*?\bfrom\s+['"]([^'"]+)['"]|^import\s+['"]([^'"]+)['"]/gm)].map(m => m[1] ?? m[2])
const moduleFile = url => {
  const r = route(url)
  const files = { script: path.join(ROOT, 'tools', 'dashboard', 'map.mjs'), bpscript: path.join(ROOT, 'tools', 'dashboard', 'blueprint.mjs'), srclib: path.join(ROOT, 'src', r.name ?? '') }
  return files[r.kind] ?? null
}
const browserGraph = entry => {
  const graph = {}
  const queue = [entry]
  while (queue.length) {
    const url = queue.shift()
    if (graph[url]) continue
    const file = moduleFile(url)
    const specs = file ? moduleImports(fs.readFileSync(file, 'utf8')) : []
    graph[url] = { served: file !== null, imports: specs }
    specs.filter(s => s.startsWith('.')).forEach(s => queue.push(new URL(s, `http://dashboard${url}`).pathname))
  }
  return graph
}

;['/map.mjs', '/blueprint.mjs'].forEach(entry => test(`the page module graph: every module below ${entry} is served, and none imports a node builtin or a package`, () => {
  const graph = browserGraph(entry)
  assert.deepEqual(Object.entries(graph).filter(([, m]) => !m.served).map(([url]) => url), [], 'modules the route table cannot serve')
  assert.deepEqual(Object.entries(graph).flatMap(([url, m]) => m.imports.filter(s => !s.startsWith('.')).map(s => `${url} imports ${s}`)), [], 'imports a browser cannot resolve')
}))

// ---------------------------------------------------------------- farm footprints and the plan popup
const wheatField = { name: 'chani-wheat-field', kind: 'farm', x: 110, y: 71, z: -70, by: 'Chani', note: 'anyone welcome',
  plan: '###########\n#.........#\n#.wwwwwww.#\n#.wwwwwww.#\n#.~~~~~~~.#\n#.wwwwwww.#\n#.wwwwwww.#\n#.........#\n#.C.K.T.t.#\n#####G#####' }
const carrotPatch = { name: 'chani-carrot-patch', kind: 'farm', x: 114, y: 71, z: -77, by: 'Chani', note: '',
  plan: '#######\n#.....#\n#ccccc#\n#~~~~~#\n#ccccc#\n#.....#\n###G###' }
const noPlan = { name: 'jizo-hut', kind: 'hut', x: 40, y: 65, z: 12 }
const brokenPlan = { name: 'too-big', kind: 'farm', x: 0, y: 65, z: 0, plan: '' }

test('planRects: a footprint per place with a plan, sized from its rows', () => {
  assert.deepEqual(planRects([wheatField, carrotPatch, noPlan]), [
    { name: 'chani-wheat-field', x: 110, z: -70, w: 11, h: 10 },
    { name: 'chani-carrot-patch', x: 114, z: -77, w: 7, h: 7 }
  ])
})

test('planRects: a place with no plan contributes no rect', () => {
  assert.deepEqual(planRects([noPlan]), [])
})

test('planRects: a plan that fails to parse contributes no rect', () => {
  assert.deepEqual(planRects([brokenPlan]), [])
})

const cells = [
  ['w', 'wheat', '#d9b25f'],
  ['c', 'carrots', '#e08b3d'],
  ['p', 'potatoes', '#c9a15f'],
  ['b', 'beetroots', '#b3435f'],
  ['~', 'water', '#4a90d9'],
  ['.', 'path', '#6b6558'],
  ['#', 'fence', '#8b7355'],
  ['G', 'gate', '#a8895f'],
  ['T', 'torch', '#e0a030'],
  ['C', 'chest', '#a0754a'],
  ['K', 'composter', '#7a5c3a']
]
cells.forEach(([ch, what, colour]) => test(`cellColour: ${what} (${ch})`, () => {
  assert.equal(cellColour(ch), colour)
}))

test('cellColour: an unknown character is grey', () => {
  assert.equal(cellColour('?'), '#555f6e')
})

test('cellColour: two different crops get different colours', () => {
  assert.notEqual(cellColour('w'), cellColour('c'))
})

const labels = [
  ['w', 'wheat'],
  ['c', 'carrots'],
  ['p', 'potatoes'],
  ['b', 'beetroots'],
  ['s', 'sugar cane'],
  ['m', 'melon'],
  ['k', 'pumpkin'],
  ['B', 'bamboo'],
  ['~', 'water'],
  ['.', 'path'],
  ['#', 'fence'],
  ['G', 'gate'],
  ['T', 'torch'],
  ['C', 'chest'],
  ['K', 'composter'],
  ['F', 'flower'],
  ['t', 'sapling'],
  ['A', 'crafting table']
]
labels.forEach(([ch, name]) => test(`cellLabel: ${ch} is ${name}`, () => {
  assert.equal(cellLabel(ch), name)
}))

test('cellLabel: an unknown character is labelled unknown', () => {
  assert.equal(cellLabel('?'), 'unknown')
})

test('cellLabel: every colour key has a matching label, and vice versa', () => {
  const withLabel = labels.map(([ch]) => ch).sort()
  const withColour = ['w', 'c', 'p', 'b', 's', 'm', 'k', 'B', '~', '.', '#', 'G', 'T', 'C', 'K', 'F', 't', 'A'].sort()
  assert.deepEqual(withLabel, withColour)
})

const rects = planRects([wheatField, carrotPatch])

const hits = [
  ['inside the wheat field', 112, -68, 'chani-wheat-field'],
  ['inside the carrot patch', 116, -73, 'chani-carrot-patch'],
  ['on the north-west anchor cell', 114, -77, 'chani-carrot-patch'],
  ['one block outside the south-east edge', 121, -70, null],
  ['nowhere near either footprint', 0, 0, null]
]
hits.forEach(([why, x, z, expected]) => test(`hitPlan: ${why}`, () => {
  assert.equal(hitPlan(rects, x, z), expected)
}))

// ---------------------------------------------------------------- the chat log
const said = (t, from, message, type = 'chat') => ({ seq: 1, t, type, from, message })
const heard = (agent, ...lines) => ({ agent, lines })
const hello = said('2026-09-24T16:36:49.053Z', 'Jizo', 'hello all')
const reply = said('2026-09-24T16:36:52.100Z', 'Steve', 'hi Jizo')

test('mergeChat: a chat heard by three bodies is one line', () => {
  assert.deepEqual(mergeChat([heard('Chani', hello), heard('Perrin', hello), heard('Mariel', hello)], 200), [
    { t: '2026-09-24T16:36:49.053Z', from: 'Jizo', to: null, kind: 'chat', message: 'hello all' }
  ])
})

// each body stamps a chat with its own clock, so one line lands a few ms apart in different files
test('mergeChat: the same chat stamped 3 ms apart by two bodies is one line, at the earliest stamp', () => {
  const late = said('2026-09-24T16:36:49.056Z', 'Jizo', 'hello all')
  assert.deepEqual(mergeChat([heard('Chani', late), heard('Perrin', hello)], 200).map(m => m.t), ['2026-09-24T16:36:49.053Z'])
})

test('mergeChat: the same words said again a minute later are two lines', () => {
  const again = said('2026-09-24T16:37:49.053Z', 'Jizo', 'hello all')
  assert.deepEqual(mergeChat([heard('Chani', hello, again)], 200).map(m => m.t), [hello.t, again.t])
})

test('mergeChat: a whisper is addressed to the body whose file holds it', () => {
  const psst = said('2026-09-24T16:12:47.948Z', 'Jizo', 'done', 'whisper')
  assert.deepEqual(mergeChat([heard('Pacer', psst)], 200), [
    { t: '2026-09-24T16:12:47.948Z', from: 'Jizo', to: 'Pacer', kind: 'whisper', message: 'done' }
  ])
})

test('mergeChat: the same words whispered to two bodies are two lines', () => {
  const psst = said('2026-09-24T16:12:47.948Z', 'Jizo', 'done', 'whisper')
  assert.deepEqual(mergeChat([heard('Pacer', psst), heard('Chani', psst)], 200).map(m => m.to), ['Chani', 'Pacer'])
})

test('mergeChat: lines are ordered by time across files, whatever order the files came in', () => {
  assert.deepEqual(mergeChat([heard('Chani', reply), heard('Perrin', hello, reply)], 200).map(m => m.from), ['Jizo', 'Steve'])
})

test('mergeChat: only the last `limit` lines survive, and those are the newest', () => {
  const many = [0, 1, 2, 3, 4].map(i => said(`2026-09-24T16:00:0${i}.000Z`, 'Jizo', `line ${i}`))
  assert.deepEqual(mergeChat([heard('Chani', ...many)], 2).map(m => m.message), ['line 3', 'line 4'])
})

const noise = [
  ['an event that is not talk', { seq: 2, t: '2026-09-24T16:00:00.000Z', type: 'hurt', hp: 3 }],
  ['a line with no time', { seq: 2, type: 'chat', from: 'Jizo', message: 'when?' }],
  ['a line with no sender', { seq: 2, t: '2026-09-24T16:00:00.000Z', type: 'chat', message: 'who?' }],
  ['a line that is not an object', 'garbage'],
  ['a null line', null]
]
noise.forEach(([what, line]) => test(`mergeChat: skips ${what}`, () => {
  assert.deepEqual(mergeChat([heard('Chani', line, hello)], 200).map(m => m.message), ['hello all'])
}))

test('mergeChat: nothing heard is an empty log', () => {
  assert.deepEqual(mergeChat([heard('Chani'), heard('Perrin')], 200), [])
})

test('parseEventLines: one object per well-formed line, a torn or malformed line skipped', () => {
  const text = '{"seq":1,"t":"x","type":"chat","from":"Jizo","message":"a"}\n{oops\n\n{"seq":2,"t":"y","type":"chat","from":"Jizo","message":"b"}\n'
  assert.deepEqual(parseEventLines(text).map(e => e.message), ['a', 'b'])
})

test('parseEventLines: the first line of a tail is dropped when the tail starts mid-line', () => {
  const text = 'm":"cut"}\n{"seq":2,"t":"y","type":"chat","from":"Jizo","message":"b"}\n'
  assert.deepEqual(parseEventLines(text, true).map(e => e.message), ['b'])
})

const limits = [
  ['the default when nothing is asked', null, 200],
  ['what is asked', '50', 50],
  ['capped at 1000', '5000', 1000],
  ['the default for nonsense', 'lots', 200],
  ['the default for zero', '0', 200],
  ['the default for a negative', '-3', 200]
]
limits.forEach(([what, raw, expected]) => test(`chatLimit: ${what}`, () => {
  assert.equal(chatLimit(raw), expected)
}))

// ---------------------------------------------------------------- the action log
const evt = (type, extra = {}) => ({ t: '2026-10-01T15:21:12.600Z', type, ...extra })
const mapped = [
  ['job_started with args', evt('job_started', { name: 'goto', args: { x: 1, y: 2, home: { x: 1, z: 2 } } }), 'goto x=1 y=2 home={"x":1,"z":2}', false],
  ['job_completed with seconds and gains', evt('job_completed', { name: 'sleep', result: { seconds: 5, gained: { beef: 8 } } }), 'sleep done in 5s +beef×8', false],
  ['job_completed bare', evt('job_completed', { name: 'goto', result: { seconds: 0, gained: {} } }), 'goto done', false],
  ['job_failed', evt('job_failed', { name: 'sleep', error: 'no bed within 32 blocks' }), 'sleep: no bed within 32 blocks', true],
  // abandon() in src/job-scheduler.mjs sends `reason`, not `error`
  ['job_interrupted with reason', evt('job_interrupted', { name: 'farm.get_seeds', reason: 'body restarted while this job owned the controls' }), 'farm.get_seeds: body restarted while this job owned the controls', true],
  ['died with pos', evt('died', { pos: { x: 80.5, y: 66, z: -69.5 }, cause: 'slain by Zombie' }), 'slain by Zombie at 80.5,66,-69.5', true],
  ['respawned falls back to doing when there is no note', evt('respawned', { doing: 'sleep' }), 'sleep', false],
  ['chat', evt('chat', { from: 'Jizo', message: 'hello all' }), 'Jizo: hello all', false],
  ['tool_broke, a plain pass-through field', evt('tool_broke', { item: 'wooden_hoe' }), 'wooden_hoe', false]
]
mapped.forEach(([what, event, gist, bad]) => test(`actionLog: ${what}`, () => {
  assert.deepEqual(actionLog([event], 200), [{ t: event.t, type: event.type, gist, bad }])
}))

const dropped = [
  ['job_queued (always followed by job_started)', evt('job_queued', { name: 'goto' })],
  ['a type with no mapping', evt('hurt', { health: 17 })]
]
dropped.forEach(([what, event]) => test(`actionLog: drops ${what}`, () => {
  assert.deepEqual(actionLog([event], 200), [])
}))

test('actionLog: an entry whose t is not a parseable time is dropped', () => {
  assert.deepEqual(actionLog([evt('tool_broke', { t: 'not a time', item: 'axe' })], 200), [])
})

const missingField = [
  ['job_failed', 'job_failed'],
  ['job_cancelled', 'job_cancelled'],
  ['died', 'died'],
  ['kicked', 'kicked'],
  ['chat', 'chat']
]
missingField.forEach(([what, type]) => test(`actionLog: ${what} missing the field its gist reads still gives a string with no "undefined"`, () => {
  const [entry] = actionLog([evt(type)], 200)
  assert.doesNotMatch(entry.gist, /undefined/)
}))

test('actionLog: the whole gist is capped at ~120 chars, the exact prefix plus an ellipsis', () => {
  const long = { x: 'y'.repeat(200) }
  const [entry] = actionLog([evt('job_started', { name: 'goto', args: long })], 200)
  const full = `goto x=${long.x}`
  assert.equal(entry.gist, `${full.slice(0, 119)}…`)
})

test('actionLog: newest `limit` entries survive, oldest first', () => {
  const many = [0, 1, 2, 3, 4].map(i => evt('tool_broke', { t: `2026-10-01T15:00:0${i}.000Z`, item: `item${i}` }))
  assert.deepEqual(actionLog(many, 2).map(e => e.gist), ['item3', 'item4'])
})

test('actionLog: the limit defaults to 200', () => {
  const base = Date.parse('2026-10-01T15:00:00.000Z')
  const many = Array.from({ length: 201 }, (_, i) => evt('tool_broke', { t: new Date(base + i * 1000).toISOString(), item: `item${i}` }))
  assert.equal(actionLog(many).length, 200)
})

// ---------------------------------------------------------------- reading a body's scan
const scanText = [
  'x -55..-53 across (ruler: last digit of x), z down',
  '     543',
  'y=63',
  '-140 .s.',
  '-139 ..u',
  'y=62 all air',
  's=sugar_cane u=unloaded'
].join('\n')

test('parseScan: one cell per character, air for a dot, the legend giving every other name', () => {
  const cells = parseScan(scanText)
  assert.equal(cells.length, 12)
  assert.deepEqual(cells.filter(c => c.name !== 'air'), [
    { x: -54, y: 63, z: -140, name: 'sugar_cane' },
    { x: -53, y: 63, z: -139, name: 'unloaded' }
  ])
})

test('parseScan: a layer that is all air is every cell of it, as air', () => {
  assert.deepEqual(parseScan(scanText).filter(c => c.y === 62).map(c => `${c.x},${c.z}`), ['-55,-140', '-54,-140', '-53,-140', '-55,-139', '-54,-139', '-53,-139'])
})

test('parseScan: a legend symbol that is a digit or punctuation still resolves', () => {
  const text = 'x 0..1 across (ruler: last digit of x), z down\n  01\ny=5\n0 #%\ny=4 all air\n#=oak_fence %=oak_fence_gate'
  assert.deepEqual(parseScan(text).filter(c => c.y === 5).map(c => c.name), ['oak_fence', 'oak_fence_gate'])
})

test('parseScan: text that is not a scan is no cells', () => {
  assert.deepEqual(parseScan('unknown action scan'), [])
})

// the footprint of a plan, two levels deep (the ground and what stands on it), cut into boxes a scan accepts
test('scanBoxes: a footprint under the cap is one box over both levels', () => {
  assert.deepEqual(scanBoxes({ x: 10, y: 62, z: 20, w: 2, h: 2 }, 1500), [{ x1: 10, y1: 62, z1: 20, x2: 11, y2: 63, z2: 21 }])
})

test('scanBoxes: a wide plan is cut into bands of rows that each fit the cap', () => {
  const boxes = scanBoxes({ x: -55, y: 62, z: -140, w: 27, h: 34 }, 1500)
  assert.deepEqual(boxes.map(b => [b.z1, b.z2]), [[-140, -114], [-113, -107]])
  assert.equal(boxes.every(b => (b.x2 - b.x1 + 1) * 2 * (b.z2 - b.z1 + 1) <= 1500), true)
})

const bodies = [
  { name: 'Claude', up: true, state: { pos: { x: -120, y: 65, z: -138 } } },
  { name: 'Jizo', up: true, state: { pos: { x: 13, y: 62, z: -74 } } },
  { name: 'Miles', up: false, state: null }
]

test('nearestBody: the body that is up and closest on the ground', () => {
  assert.equal(nearestBody(bodies, -55, -140).name, 'Claude')
})

test('nearestBody: a body that is down is never picked', () => {
  assert.equal(nearestBody([bodies[2]], -55, -140), null)
})

// ---------------------------------------------------------------- one map per world
const worldBody = (name, world, players = {}) => ({ name, world, up: true, at: 1000, state: { pos: { x: 0, y: 64, z: 0 }, players } })
const twoWorlds = [
  { name: 'main', places: [{ name: 'hut', x: 1, y: 64, z: 1 }], zones: [{ name: 'pen' }] },
  { name: 'test', places: [{ name: 'farm', x: 2, y: 64, z: 2 }], zones: [] }
]
const everyBody = [
  worldBody('Claude', 'main', { Steve: { x: 5, y: 64, z: 5 } }),
  worldBody('Chani', 'test', { Alex: { x: 7, y: 64, z: 7 } }),
  worldBody('Miles', null, { Herobrine: { x: 9, y: 64, z: 9 } }),
  worldBody('Rand', 'nether-server', { Logain: { x: 11, y: 64, z: 11 } })
]

test('groupWorlds: each world gets its own bodies, the humans they see, and its own places and zones', () => {
  const grouped = groupWorlds(twoWorlds, everyBody, ['Claude', 'Chani', 'Miles', 'Rand'])
  assert.deepEqual(grouped, [
    { ...twoWorlds[0], bodies: [everyBody[0]], humans: [{ name: 'Steve', x: 5, y: 64, z: 5, seenBy: 'Claude', at: 1000 }] },
    { ...twoWorlds[1], bodies: [everyBody[1]], humans: [{ name: 'Alex', x: 7, y: 64, z: 7, seenBy: 'Chani', at: 1000 }] }
  ])
  // a body naming a world nobody made (Rand's 'nether-server') lands on no map at all, not just off the other two
  assert.ok(grouped.every(world => !world.bodies.includes(everyBody[3])))
})

const lookups = [
  ['a place in the first world', 'hut', 'main', ['Claude']],
  ['a place in a later world', 'farm', 'test', ['Chani']]
]
lookups.forEach(([why, name, world, bodyNames]) => test(`findPlace: ${why} comes with the world it was found in`, () => {
  const found = findPlace(groupWorlds(twoWorlds, everyBody, []), name)
  assert.deepEqual([found.place.name, found.world.name, found.world.bodies.map(b => b.name)], [name, world, bodyNames])
}))

test('findPlace: a name two worlds share is the first world\'s place', () => {
  const shared = [{ ...twoWorlds[0], places: [{ name: 'hut', x: 1 }] }, { ...twoWorlds[1], places: [{ name: 'hut', x: 2 }] }]
  assert.equal(findPlace(shared, 'hut').place.x, 1)
})

test('findPlace: a name no world holds is null', () => {
  assert.equal(findPlace(twoWorlds, 'nowhere'), null)
})

// scan names a waterlogged slab as a plain slab: those are the cells the server asks block_at about, one by one
test('unsureWater: only the ~ cells whose ground scan did not settle', () => {
  const plan = { x: 0, y: 62, z: 0, plan: '~~~~' }
  const world = [
    { x: 0, y: 62, z: 0, name: 'water' }, { x: 1, y: 62, z: 0, name: 'oak_slab' },
    { x: 2, y: 62, z: 0, name: 'air' }, { x: 3, y: 62, z: 0, name: 'unloaded' }
  ]
  assert.deepEqual(unsureWater(plan, world), [{ x: 1, y: 62, z: 0 }])
})

// ---------------------------------------------------------------- the plan against the world
const at = (x, y, z, name, extra = {}) => ({ x, y, z, name, ...extra })
const tiny = { name: 'tiny', x: 10, y: 62, z: 20, plan: 's~\n.#' }
const tinyWorld = [
  at(10, 62, 20, 'sand'), at(10, 63, 20, 'sugar_cane'),
  at(11, 62, 20, 'oak_slab', { waterlogged: true }), at(11, 63, 20, 'air'),
  at(10, 62, 21, 'grass_block'), at(10, 63, 21, 'short_grass'),
  at(11, 62, 21, 'dirt'), at(11, 63, 21, 'spruce_fence')
]

test('planDiff: a built plan has no differing cell', () => {
  const d = planDiff(tiny, tinyWorld)
  assert.deepEqual([d.total, d.differ, d.unseen], [4, 0, 0])
  assert.deepEqual(d.cells.map(c => c.ok), [true, true, true, true])
})

test('planDiff: each cell carries the plan, what should be there and what is', () => {
  assert.deepEqual(planDiff(tiny, tinyWorld).cells.find(c => c.ch === 's'), {
    dx: 0, dz: 0, x: 10, y: 63, z: 20, ch: 's', spec: { kind: 'crop', crop: 'sugar_cane', seed: 'sugar_cane', ground: 'sand', literal: false }, expected: 'sugar cane on sand', ground: 'sand', top: 'sugar_cane', ok: true, seen: true
  })
})

const judged = [
  ['a crop of any age on its block', 'w', 'farmland', 'wheat', true],
  ['a crop cell still bare is not the crop', 'w', 'farmland', 'air', false],
  ['a water cell holding water', '~', 'water', 'air', true],
  ['a water cell covered by a waterlogged slab', '~', { name: 'oak_slab', waterlogged: true }, 'air', true],
  ['a dry slab is not water', '~', { name: 'oak_slab', waterlogged: false }, 'air', false],
  ['a path on any solid ground', '.', 'stone', 'air', true],
  ['a path under a flower is still open', '.', 'grass_block', 'dandelion', true],
  ['a path with a fence on it is blocked', '.', 'dirt', 'oak_fence', false],
  ['a path over a hole', '.', 'air', 'air', false],
  ['a fence of any wood', '#', 'dirt', 'spruce_fence', true],
  ['a missing fence', '#', 'dirt', 'air', false],
  ['a gate', 'G', 'dirt', 'oak_fence_gate', true],
  ['a torch post is a fence', 'T', 'dirt', 'oak_fence', true],
  ['a torch itself', 'T', 'dirt', 'torch', true],
  ['a chest', 'C', 'dirt', 'chest', true],
  ['a composter', 'K', 'dirt', 'composter', true],
  ['a crafting table', 'A', 'dirt', 'crafting_table', true],
  ['a flower of any kind', 'F', 'grass_block', 'poppy', true],
  ['a sapling', 't', 'dirt', 'oak_sapling', true],
  ['a sapling that grew', 't', 'dirt', 'oak_log', true],
  ['a cane cell that grew a tree instead', 's', 'sand', 'oak_log', false]
]
judged.forEach(([what, ch, ground, top, ok]) => test(`planDiff: ${what} is ${ok ? 'right' : 'wrong'}`, () => {
  const g = typeof ground === 'string' ? { name: ground } : ground
  const world = [at(0, 62, 0, g.name, g.waterlogged === undefined ? {} : { waterlogged: g.waterlogged }), at(0, 63, 0, top)]
  assert.equal(planDiff({ x: 0, y: 62, z: 0, plan: ch }, world).cells[0].ok, ok)
}))

test('planDiff: a cell no body has loaded is unseen, neither right nor wrong', () => {
  const d = planDiff({ x: 0, y: 62, z: 0, plan: 'w' }, [at(0, 62, 0, 'unloaded'), at(0, 63, 0, 'unloaded')])
  assert.deepEqual([d.cells[0].ok, d.cells[0].seen, d.differ, d.unseen], [null, false, 0, 1])
})

test('planDiff: a cell the scan never covered counts as unseen too', () => {
  const d = planDiff({ x: 0, y: 62, z: 0, plan: 'w' }, [])
  assert.deepEqual([d.cells[0].seen, d.unseen], [false, 1])
})

test('planDiff: a slab whose waterlogging nobody checked is unsure, not wrong', () => {
  const d = planDiff({ x: 0, y: 62, z: 0, plan: '~' }, [at(0, 62, 0, 'oak_slab'), at(0, 63, 0, 'air')])
  assert.deepEqual([d.cells[0].ok, d.cells[0].seen, d.differ, d.unsure], [null, true, 0, 1])
})

test('planDiff: differ counts the wrong cells only', () => {
  const world = [at(10, 62, 20, 'sand'), at(10, 63, 20, 'air'), at(11, 62, 20, 'water'), at(11, 63, 20, 'air'),
    at(10, 62, 21, 'dirt'), at(10, 63, 21, 'air'), at(11, 62, 21, 'dirt'), at(11, 63, 21, 'air')]
  assert.deepEqual(planDiff(tiny, world).cells.filter(c => !c.ok).map(c => c.ch), ['s', '#'])
})

const expectations = [
  ['w', 'wheat on farmland'],
  ['s', 'sugar cane on sand'],
  ['~', 'water, or a waterlogged cover'],
  ['.', 'open, on solid ground'],
  ['#', 'a fence'],
  ['G', 'a fence gate'],
  ['T', 'a fence post with a torch'],
  ['C', 'a chest'],
  ['?', 'not in the legend']
]
expectations.forEach(([ch, text]) => test(`cellExpectation: ${ch} is ${text}`, () => {
  assert.equal(cellExpectation(ch), text)
}))

test('worldLabel: what stands on the cell, or the ground when nothing does', () => {
  assert.deepEqual([worldLabel({ ground: 'sand', top: 'sugar_cane' }), worldLabel({ ground: 'dirt', top: 'air' }), worldLabel({ ground: 'unloaded', top: 'unloaded' })],
    ['sugar_cane on sand', 'dirt', 'not loaded'])
})

test('worldColour: a known block has its colour, an unknown one a stable colour of its own', () => {
  assert.equal(worldColour('water'), '#4a90d9')
  assert.equal(worldColour('unloaded'), '#2a2f38')
  assert.equal(worldColour('mystery_block'), worldColour('mystery_block'))
  assert.notEqual(worldColour('mystery_block'), worldColour('other_block'))
})

// ---------------------------------------------------------------- the blueprint library page
// The server hands the page one detail per file: the parsed, resolved blueprint (plain data), its bill, what lint
// says and the marked places built from it. Everything the page draws from that is pure and tested here.
const alt = (name, states = {}) => ({ name, states })
const tinyBp = {
  name: 'tiny-hut', title: 'Tiny hut', description: 'd', tags: ['shelter', 'storage'], front: 'south', foundation: 'flat', clearance: 1, params: { wood: 'oak' }, width: 3, depth: 2,
  legend: {
    '.': { token: '.', alts: [alt('air')], tags: [] },
    P: { token: 'P', alts: [alt('oak_planks')], tags: [] },
    L: { token: 'L', alts: [alt('oak_log', { axis: 'y' })], tags: [] },
    i: { token: 'i', alts: [alt('torch')], tags: [] },
    S: { token: 'S', alts: [alt('@solid')], tags: [] }
  },
  layers: [{ y: -1, grid: ['SSS', 'S_S'] }, { y: 0, grid: ['LPL', '.i.'] }]
}
const tinyBill = { total: { oak_planks: 1, oak_log: 2, torch: 1 }, layers: [{ y: -1, items: {} }, { y: 0, items: { oak_planks: 1, oak_log: 2, torch: 1 } }], tools: [] }
const clean = { errors: [], warnings: [] }
const tinyDetail = { name: 'tiny-hut', hash: 'abcd1234', bp: tinyBp, bill: tinyBill, lint: clean, errors: [], builds: [] }
const unparsed = { name: 'broken', hash: '00000000', bp: null, bill: null, lint: null, errors: ['front matter: name is required', 'no ## y<n> layer found'], builds: [] }

test('blueprintRow: one list row per file - name, kind from the tags, footprint, layers, blocks from the bill', () => {
  assert.deepEqual(blueprintRow(tinyDetail), { name: 'tiny-hut', title: 'Tiny hut', kind: 'shelter, storage', footprint: '3x2x2', layers: 2, blocks: 4, status: 'ok', builds: 0 })
})

test('blueprintRow: a file that does not parse still gets a row, saying so', () => {
  assert.deepEqual(blueprintRow(unparsed), { name: 'broken', title: '', kind: '', footprint: '', layers: 0, blocks: 0, status: 'does not parse', builds: 0 })
})

const statuses = [
  ['clean lint', { lint: clean }, 'ok'],
  ['one warning', { lint: { errors: [], warnings: ['w'] } }, '1 warning'],
  ['two warnings', { lint: { errors: [], warnings: ['w', 'w2'] } }, '2 warnings'],
  ['a lint error, whatever the warnings', { lint: { errors: ['e'], warnings: ['w'] } }, 'build refuses'],
  ['two builds standing', { builds: [{ place: 'a' }, { place: 'b' }] }, 'ok']
]
statuses.forEach(([why, patch, status]) => test(`blueprintRow status: ${why}`, () => {
  assert.equal(blueprintRow({ ...tinyDetail, ...patch }).status, status)
}))

test('blueprintRow: counts the places built from it', () => {
  assert.equal(blueprintRow({ ...tinyDetail, builds: [{ place: 'a' }, { place: 'b' }] }).builds, 2)
})

test('layerCells: every cell of the layer that is part of the blueprint, with its block, colour and label; _ is left out', () => {
  assert.deepEqual(layerCells(tinyBp, -1).map(c => `${c.dx},${c.dz} ${c.token} ${c.label}`), ['0,0 S any solid block', '1,0 S any solid block', '2,0 S any solid block', '0,1 S any solid block', '2,1 S any solid block'])
  assert.deepEqual(layerCells(tinyBp, 0).map(c => `${c.dx},${c.dz} ${c.token} ${c.label} ${c.air}`), ['0,0 L oak_log[axis=y] false', '1,0 P oak_planks false', '2,0 L oak_log[axis=y] false', '0,1 . air true', '1,1 i torch false', '2,1 . air true'])
})

test('layerCells: a cell carries the colour of its block, air none, and y of its layer', () => {
  const cells = layerCells(tinyBp, 0)
  assert.deepEqual(cells.map(c => c.colour), [blockColour('oak_log'), blockColour('oak_planks'), blockColour('oak_log'), null, blockColour('torch'), null])
  assert.deepEqual([...new Set(cells.map(c => c.y))], [0])
})

test('layerCells: a waterlogged block is tinted toward water, so a covered channel reads as one', () => {
  const bp = { ...tinyBp, legend: { ...tinyBp.legend, '=': { token: '=', alts: [alt('oak_slab', { type: 'top', waterlogged: 'true' })], tags: [] }, s: { token: 's', alts: [alt('oak_slab', { type: 'top' })], tags: [] } }, layers: [{ y: 0, grid: ['=s'] }] }
  const [wet, dry] = layerCells(bp, 0)
  assert.equal(dry.colour, blockColour('oak_slab'))
  assert.equal(wet.colour, altColour(alt('oak_slab', { type: 'top', waterlogged: 'true' })))
  assert.notEqual(wet.colour, dry.colour)
  assert.equal(legendRows(bp)[0].colour, wet.colour)
})

test('altColour: water tints, air stays nothing', () => {
  assert.equal(altColour(alt('oak_slab', { waterlogged: 'true' })), '#658bb0')
  assert.equal(altColour(alt('oak_slab')), blockColour('oak_slab'))
  assert.equal(altColour(alt('air', { waterlogged: 'true' })), null)
})

test('layerCells: a y the blueprint has no layer for is empty', () => {
  assert.deepEqual(layerCells(tinyBp, 7), [])
})

const hovers = [
  ['a block with states', layerCells(tinyBp, 0)[0], 'x+0 y0 z+0 · oak_log[axis=y] (L)'],
  ['a plain block', layerCells(tinyBp, 0)[4], 'x+1 y0 z+1 · torch (i)'],
  ['air', layerCells(tinyBp, 0)[3], 'x+0 y0 z+1 · air (.)'],
  ['any solid', layerCells(tinyBp, -1)[4], 'x+2 y-1 z+1 · any solid block (S)']
]
hovers.forEach(([what, cell, text]) => test(`hoverText: ${what}`, () => {
  assert.equal(hoverText(cell), text)
}))

test('legendRows: the tokens the layers use, in the order they are first met from the lowest layer up, with a count each; air and _ are not listed', () => {
  assert.deepEqual(legendRows(tinyBp).map(r => `${r.token} ${r.label} ${r.count} ${r.colour}`), [
    `S any solid block 5 ${blockColour('@solid')}`,
    `L oak_log[axis=y] 2 ${blockColour('oak_log')}`,
    `P oak_planks 1 ${blockColour('oak_planks')}`,
    `i torch 1 ${blockColour('torch')}`
  ])
})

test('legendRows: a token the legend defines but no layer uses is not listed', () => {
  const bp = { ...tinyBp, legend: { ...tinyBp.legend, X: { token: 'X', alts: [alt('chest')], tags: [] } } }
  assert.deepEqual(legendRows(bp).map(r => r.token), ['S', 'L', 'P', 'i'])
})

test('billRows: the items most needed first, ties by name, each with its share of the largest for a bar', () => {
  assert.deepEqual(billRows({ torch: 1, oak_log: 2, oak_planks: 1, cobblestone: 8 }), [
    { item: 'cobblestone', count: 8, share: 1 },
    { item: 'oak_log', count: 2, share: 0.25 },
    { item: 'oak_planks', count: 1, share: 0.125 },
    { item: 'torch', count: 1, share: 0.125 }
  ])
})

test('billRows: nothing to fetch is an empty list', () => {
  assert.deepEqual(billRows({}), [])
})

const lints = [
  ['a file that does not parse: every parser error, first', unparsed, [{ level: 'parse', text: 'front matter: name is required' }, { level: 'parse', text: 'no ## y<n> layer found' }]],
  ['clean', tinyDetail, [{ level: 'ok', text: 'lint has nothing to say: build accepts it' }]],
  ['lint errors (build refuses) before warnings', { ...tinyDetail, lint: { errors: ['the door at y0 1,1 stands on nothing'], warnings: ['H: ladder needs place against='] } }, [{ level: 'error', text: 'the door at y0 1,1 stands on nothing' }, { level: 'warning', text: 'H: ladder needs place against=' }]],
  ['warnings alone', { ...tinyDetail, lint: { errors: [], warnings: ['w'] } }, [{ level: 'warning', text: 'w' }]]
]
lints.forEach(([what, detail, expected]) => test(`lintLines: ${what}`, () => {
  assert.deepEqual(lintLines(detail), expected)
}))

// the palette: one hue per material, shaded by role, so a hut's oak planks, logs, stairs and fence read as one family
const families = [
  ['oak_planks', { family: 'wood', value: 'oak', role: 'planks' }],
  ['oak_fence_gate', { family: 'wood', value: 'oak', role: 'fence_gate' }],
  ['stripped_spruce_log', { family: 'wood', value: 'spruce', role: 'stripped_log' }],
  ['crimson_stem', { family: 'wood', value: 'crimson', role: 'log' }],
  ['dark_oak_door', { family: 'wood', value: 'dark_oak', role: 'door' }],
  ['stone_brick_slab', { family: 'stone', value: 'stone_bricks', role: 'slab' }],
  ['cobblestone', { family: 'stone', value: 'cobblestone', role: 'block' }],
  ['polished_blackstone_brick_wall', { family: 'stone', value: 'polished_blackstone_bricks', role: 'wall' }],
  ['white_bed', { family: 'dye', value: 'white', role: 'bed' }],
  ['light_blue_wool', { family: 'dye', value: 'light_blue', role: 'wool' }],
  ['torch', null],
  ['@solid', null]
]
families.forEach(([name, expected]) => test(`familyOf: ${name}`, () => {
  assert.deepEqual(familyOf(name), expected)
}))

const colours = [
  ['oak_planks', '#b08a55'],
  ['cobblestone', '#8a8a8a'],
  ['white_bed', '#e9ecec'],
  ['torch', '#e0a030'],
  ['@solid', '#4b5462'],
  ['air', null],
  ['farmland', worldColour('farmland')]
]
colours.forEach(([name, colour]) => test(`blockColour: ${name}`, () => {
  assert.equal(blockColour(name), colour)
}))

test('blockColour: the roles of one wood are shades of the same hue, darker for logs and lighter for stripped ones', () => {
  const lightness = hex => parseInt(hex.slice(1, 3), 16) + parseInt(hex.slice(3, 5), 16) + parseInt(hex.slice(5, 7), 16)
  const [log, planks, stripped] = ['oak_log', 'oak_planks', 'stripped_oak_log'].map(blockColour).map(lightness)
  assert.ok(log < planks && planks < stripped, `${log} < ${planks} < ${stripped}`)
  assert.notEqual(blockColour('oak_fence'), blockColour('spruce_fence'))
})

test('blockColour: a block nobody listed still gets a stable muted colour of its own', () => {
  assert.equal(blockColour('sponge'), blockColour('sponge'))
  assert.match(blockColour('sponge'), /^hsl\(/)
})

// the node side: one file read, parsed, resolved with its defaults, costed and linted, plus the places built from it
const LIBRARY = path.join(ROOT, 'blueprints')
const hutFile = { name: 'starter-hut', text: fs.readFileSync(path.join(ROOT, 'test', 'fixtures', 'blueprints', 'starter-hut.txt'), 'utf8'), hash: 'f00dcafe' }
const hutPlaces = [
  { name: 'a-hut', kind: 'shelter', x: 10, y: 64, z: -5, by: 'Someone', note: 'bp=starter-hut f=east h=f00dcafe wood=spruce' },
  { name: 'an-old-hut', kind: 'shelter', x: 1, y: 2, z: 3, by: 'Nobody', note: 'bp=starter-hut f=south h=01234567' },
  { name: 'a-tower', kind: 'build', x: 0, y: 0, z: 0, note: 'bp=watchtower f=south h=f00dcafe' },
  { name: 'a-field', kind: 'farm', x: 0, y: 0, z: 0, note: 'anyone welcome' }
]

test('blueprintDetail: the library file parsed and resolved, with its bill, lint and the places that carry its build note', () => {
  const d = blueprintDetail(hutFile, hutPlaces)
  assert.deepEqual({ name: d.name, hash: d.hash, errors: d.errors, title: d.bp.title, width: d.bp.width, layers: d.bp.layers.length, blocks: Object.values(d.bill.total).reduce((a, b) => a + b, 0), lint: d.lint, door: d.bp.legend.D.alts[0].name },
    { name: 'starter-hut', hash: 'f00dcafe', errors: [], title: 'Starter hut', width: 5, layers: 5, blocks: 102, lint: clean, door: 'oak_door' })
  assert.deepEqual(d.builds, [
    { place: 'a-hut', kind: 'shelter', x: 10, y: 64, z: -5, by: 'Someone', facing: 'east', params: { wood: 'spruce' }, current: true },
    { place: 'an-old-hut', kind: 'shelter', x: 1, y: 2, z: 3, by: 'Nobody', facing: 'south', params: {}, current: false }
  ])
})

test('blueprintDetail: a file that does not parse carries the parser errors and no blueprint', () => {
  const d = blueprintDetail({ name: 'nope', text: 'not a blueprint', hash: '0' }, [])
  assert.deepEqual({ name: d.name, bp: d.bp, bill: d.bill, lint: d.lint, first: d.errors[0], builds: d.builds }, { name: 'nope', bp: null, bill: null, lint: null, first: 'the file must start with a --- front matter block', builds: [] })
})

test('blueprintRow over the real library: the hut is 5x5x5 in five layers of 102 items', () => {
  const bp = resolve(parseBlueprint(hutFile.text))
  assert.deepEqual(blueprintRow({ name: 'starter-hut', bp, bill: bill(bp), lint: lint(bp), errors: [], builds: [] }), { name: 'starter-hut', title: 'Starter hut', kind: 'shelter, storage', footprint: '5x5x5', layers: 5, blocks: 102, status: 'ok', builds: 0 })
})

// ---------------------------------------------------------------- the world selector
test('parseWorldList: sorted by name, host and port from world.json, unreadable JSON still listed', () => {
  assert.deepEqual(parseWorldList([
    { name: 'test', text: '{"host":"h2","port":25566}' },
    { name: 'claude', text: '{"host":"localhost","port":25565}' },
    { name: 'broken', text: '{nope' },
    { name: 'empty', text: '' }
  ]), [
    { name: 'broken', host: null, port: null },
    { name: 'claude', host: 'localhost', port: 25565 },
    { name: 'empty', host: null, port: null },
    { name: 'test', host: 'h2', port: 25566 }
  ])
})

const resolutions = [
  ['null picks the first', ['claude', 'test'], null, 'claude'],
  ['empty string picks the first', ['claude', 'test'], '', 'claude'],
  ['no worlds and nothing asked is null', [], null, null],
  ['an exact name', ['claude', 'test'], 'test', 'test'],
  ['a case difference is invalid', ['claude'], 'Claude', undefined],
  ['a traversal is invalid', ['claude'], '../x', undefined],
  ['a sibling directory is invalid', ['claude'], '../agents', undefined],
  ['a slash is invalid', ['claude'], 'a/b', undefined],
  ['a nested traversal is invalid', ['claude'], 'claude/../claude', undefined],
  ['a name when no worlds exist is invalid', [], 'claude', undefined],
  ['an absolute path is invalid', ['claude'], '/etc/passwd', undefined]
]
resolutions.forEach(([why, names, requested, expected]) => test(`resolveWorld: ${why}`, () => {
  assert.equal(resolveWorld(names, requested), expected)
}))

test('scopeSnapshot: only the chosen world, its bodies and the names of those bodies', () => {
  const snapshot = {
    at: 5,
    agents: ['Claude', 'Claude2', 'Chani', 'Chani2'],
    bodies: [{ name: 'Claude', username: 'Claude2', world: 'main' }, { name: 'Chani', username: 'Chani2', world: 'test' }],
    worlds: [{ name: 'main', bodies: [] }, { name: 'test', bodies: [] }],
    villageError: null
  }
  assert.deepEqual(scopeSnapshot(snapshot, 'test'), {
    at: 5,
    agents: ['Chani', 'Chani2'],
    bodies: [snapshot.bodies[1]],
    worlds: [snapshot.worlds[1]],
    villageError: null
  })
})

const chatSources = [{ agent: 'Claude', lines: [1] }, { agent: 'Chani', lines: [2] }, { agent: 'Orphan', lines: [3] }]
const chatScopes = [
  ['agents of the world', 'main', ['Claude']],
  ['another world', 'test', ['Chani']],
  ['a world nobody plays in', 'nether', []]
]
chatScopes.forEach(([why, world, expected]) => test(`scopeChatSources: ${why}`, () => {
  const agents = [{ name: 'Claude', world: 'main' }, { name: 'Chani', world: 'test' }]
  assert.deepEqual(scopeChatSources(chatSources, agents, world).map(s => s.agent), expected)
}))

const membership = [
  ['no world asked is no agent: the world is always given', 'Claude', null, false],
  ['the agent\'s world', 'Claude', 'main', true],
  ['another world', 'Claude', 'test', false],
  ['an unknown agent', 'Nobody', 'main', false],
  ['an agent with no world', 'Free', 'main', false]
]
membership.forEach(([why, name, world, expected]) => test(`agentInWorld: ${why}`, () => {
  const agents = [{ name: 'Claude', world: 'main' }, { name: 'Free', world: null }]
  assert.equal(agentInWorld(agents, name, world), expected)
}))

// ---------------------------------------------------------------- what the map fits

const upBody = { name: 'A', up: true, state: { pos: { x: 5, y: 64, z: 6 } } }
const downBody = { name: 'B', up: false, state: null }
const hut = { name: 'hut', x: 10, z: 20 }
const zone = { name: 'z', x1: 1, z1: 2, x2: 3, z2: 4 }
const human = { name: 'Steve', x: 7, y: 64, z: 8 }

const fitCases = [
  ['fits bodies, places, zones and humans together', { bodies: [upBody], places: [hut], zones: [zone], humans: [human] }, [{ x: 5, z: 6 }, { x: 10, z: 20 }, { x: 1, z: 2 }, { x: 3, z: 4 }, { x: 7, z: 8 }]],
  ['with none up it still fits places and zones', { bodies: [downBody], places: [hut], zones: [zone], humans: [] }, [{ x: 10, z: 20 }, { x: 1, z: 2 }, { x: 3, z: 4 }]],
  ['an empty world has no points', { bodies: [], places: [], zones: [], humans: [] }, []]
]
for (const [name, world, points] of fitCases) {
  test(`fitPoints: ${name}`, () => assert.deepEqual(fitPoints(world), { points, pad: 24 }))
}

const emptyCases = [
  ['nothing at all', { bodies: [], places: [], zones: [], humans: [] }, true],
  ['only a body that is down', { bodies: [downBody], places: [], zones: [], humans: [] }, true],
  ['a body up', { bodies: [upBody], places: [], zones: [], humans: [] }, false],
  ['a place', { bodies: [], places: [hut], zones: [], humans: [] }, false],
  ['a zone', { bodies: [], places: [], zones: [zone], humans: [] }, false]
]
for (const [name, world, expected] of emptyCases) {
  test(`isEmptyWorld: ${name}`, () => assert.equal(isEmptyWorld(world), expected))
}

// ---------------------------------------------------------------- inline script syntax
import { mkdtempSync, writeFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { tmpdir } from 'node:os'

const htmlFiles = [
  path.join(ROOT, 'tools/dashboard/index.html'),
  path.join(ROOT, 'tools/dashboard/blueprints.html'),
  path.join(ROOT, 'tools/dashboard/villages.html'),
  path.join(ROOT, 'tools/dashboard/villagers.html')
]

test('dashboard inline module scripts pass node --check', async t => {
  for (const htmlFile of htmlFiles) {
    await t.test(`${path.basename(htmlFile)}`, () => {
      const content = fs.readFileSync(htmlFile, 'utf-8')

      // Extract inline <script type="module"> content
      const scriptRegex = /<script[^>]*type="module"[^>]*>([\s\S]*?)<\/script>/g
      const scripts = []
      let match
      while ((match = scriptRegex.exec(content)) !== null) {
        scripts.push(match[1])
      }

      if (scripts.length === 0) {
        return // Skip if no module scripts found
      }

      // Create a temporary directory and write the script
      const tmpDir = mkdtempSync(path.join(tmpdir(), 'dashboard-'))
      const scriptFile = path.join(tmpDir, 'test-script.mjs')
      const scriptContent = scripts.join('\n\n')

      writeFileSync(scriptFile, scriptContent, 'utf-8')

      // Run node --check on the script
      const result = spawnSync('node', ['--check', scriptFile])

      assert.equal(
        result.status,
        0,
        `node --check failed:\nstdout: ${result.stdout?.toString()}\nstderr: ${result.stderr?.toString()}`
      )
    })
  }
})
