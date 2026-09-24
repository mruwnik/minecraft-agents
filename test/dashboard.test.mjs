import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { parseAgents, snapshotFile, route, mergeChat, parseEventLines, chatLimit } from '../tools/dashboard/lib.mjs'
import { mergeBodies, danSighting, mapPoints, worldBounds, fitView, project, zoneRect, fitLabels, onCanvas, planRects, cellColour, cellLabel, hitPlan } from '../tools/dashboard/map.mjs'

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
  ['/api/chat', { kind: 'chat' }],
  ['/api/chat?limit=50', { kind: 'chat' }],
  ['/api/chat/', { kind: 'unknown' }],
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
  ['/api/look/Chani', { kind: 'look', name: 'Chani' }],
  ['/api/look/Chani?fresh=1', { kind: 'look', name: 'Chani' }],
  ['/api/look/', { kind: 'unknown' }],
  ['/api/look/../../etc/passwd', { kind: 'unknown' }],
  ['/nope', { kind: 'unknown' }]
]
routes.forEach(([url, expected]) => test(`route: ${url}`, () => {
  assert.deepEqual(route(url), expected)
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
const reply = said('2026-09-24T16:36:52.100Z', 'mruwnik', 'hi Jizo')

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
  assert.deepEqual(mergeChat([heard('Chani', reply), heard('Perrin', hello, reply)], 200).map(m => m.from), ['Jizo', 'mruwnik'])
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
