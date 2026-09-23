import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { parseAgents, mergeBodies, danSighting, mapPoints, worldBounds, fitView, project, zoneRect, snapshotFile, route } from '../tools/dashboard/lib.mjs'

// ---------------------------------------------------------------- reading the agent folders
const config = (username, apiPort, extra = {}) => JSON.stringify({ username, apiPort, harness: 'claude-code', ...extra })

test('parseAgents: one entry per readable config, sorted by name', () => {
  assert.deepEqual(parseAgents([
    { name: 'Mariel', text: config('Mariel', 3790) },
    { name: 'Claude', text: config('Claude', 3777, { character: { name: 'Claude', source: 'chosen by hand' } }) }
  ]), [
    { name: 'Claude', username: 'Claude', apiPort: 3777, harness: 'claude-code', character: 'Claude (chosen by hand)' },
    { name: 'Mariel', username: 'Mariel', apiPort: 3790, harness: 'claude-code', character: null }
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
  { name: 'Claude', username: 'Claude', apiPort: 3777, harness: 'claude-code', character: null },
  { name: 'Perrin', username: 'Perrin', apiPort: 3789, harness: 'claude-code', character: null }
]
const claudeState = { hp: 20, food: 15, pos: { x: 60.5, y: 65, z: -151.6 }, doing: 'goto 2s', players: {} }

test('mergeBodies: a body that answered is up and carries its state', () => {
  assert.deepEqual(mergeBodies(agents, { Claude: { ok: true, state: claudeState, at: 1000 } })[0], {
    ...agents[0], up: true, error: null, state: claudeState, at: 1000
  })
})

const missing = [
  ['never polled', undefined, 'not polled yet'],
  ['refused the connection', { ok: false, error: 'connect ECONNREFUSED 127.0.0.1:3789', at: 900 }, 'connect ECONNREFUSED 127.0.0.1:3789']
]
missing.forEach(([why, poll, error]) => test(`mergeBodies: a body that ${why} is down, not an error`, () => {
  const row = mergeBodies(agents, { Perrin: poll })[1]
  assert.deepEqual([row.name, row.up, row.error, row.state], ['Perrin', false, error, null])
}))

test('mergeBodies: a body that answered ok:false is down with the body its own reason', () => {
  const poll = { ok: false, error: 'bot is not connected to the server (retrying every 10s)', at: 5 }
  assert.equal(mergeBodies(agents, { Claude: poll })[0].up, false)
})

test('mergeBodies: every agent gets a row, in the order given', () => {
  assert.deepEqual(mergeBodies(agents, {}).map(b => b.name), ['Claude', 'Perrin'])
})

// ---------------------------------------------------------------- where Dan is
const seer = (name, players, at) => ({ name, up: true, at, state: { ...claudeState, players } })

test('danSighting: the freshest body that can see him wins', () => {
  assert.deepEqual(danSighting([
    seer('Claude', { mruwnik: { x: 1, y: 65, z: 2 } }, 1000),
    seer('Chani', { mruwnik: { x: 3, y: 66, z: 4 } }, 2000)
  ], 'mruwnik'), { x: 3, y: 66, z: 4, seenBy: 'Chani', at: 2000 })
})

const blind = [
  ['nobody lists him', [seer('Claude', { Chani: { x: 1, y: 65, z: 2 } }, 1000)]],
  ['he is out of sight', [seer('Claude', { mruwnik: 'out of sight' }, 1000)]],
  ['the only body that sees him is down', [{ name: 'Claude', up: false, at: 1, state: null }]],
  ['there are no bodies', []]
]
blind.forEach(([why, bodies]) => test(`danSighting: null when ${why}`, () => {
  assert.equal(danSighting(bodies, 'mruwnik'), null)
}))

// ---------------------------------------------------------------- the map
const places = [{ name: 'hut', x: 116, y: 69, z: -141 }]
const zones = [{ name: 'pen', x1: 100, y1: 60, z1: -150, x2: 110, y2: 70, z2: -140 }]

test('mapPoints: every body, place and zone corner is a point to fit', () => {
  assert.deepEqual(mapPoints([seer('Claude', {}, 1)], places, zones, { x: 0, y: 64, z: 0 }), [
    { x: 60.5, z: -151.6 }, { x: 116, z: -141 }, { x: 100, z: -150 }, { x: 110, z: -140 }, { x: 0, z: 0 }
  ])
})

test('mapPoints: a body that is down contributes nothing, and Dan may be missing', () => {
  assert.deepEqual(mapPoints([{ name: 'Perrin', up: false, state: null }], [], [], null), [])
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

// ---------------------------------------------------------------- serving files and routes
const home = '/home/dan/minecraft/claude/bot/state/agents/Claude'

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

const routes = [
  ['/', { kind: 'page' }],
  ['/index.html', { kind: 'page' }],
  ['/api/state', { kind: 'state' }],
  ['/api/state?since=3', { kind: 'state' }],
  ['/api/look/Chani', { kind: 'look', name: 'Chani' }],
  ['/api/look/Chani?fresh=1', { kind: 'look', name: 'Chani' }],
  ['/api/look/', { kind: 'unknown' }],
  ['/api/look/../../etc/passwd', { kind: 'unknown' }],
  ['/nope', { kind: 'unknown' }]
]
routes.forEach(([url, expected]) => test(`route: ${url}`, () => {
  assert.deepEqual(route(url), expected)
}))
