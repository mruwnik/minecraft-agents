// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import http from 'node:http'
import path from 'node:path'
import prismarineRegistry from 'prismarine-registry'
import { saveSeen } from '../../js/seen-file.mjs'
import { execute } from '../../tools/plan-tools-lib.mjs'
import { readEDN, keyword } from './edn.mjs'
import { revision } from '../../tools/world-data.mjs'

function fixture () {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'agent-plan-tools-'))
  const state = path.join(root, 'state'), repo = path.join(root, 'repo')
  fs.mkdirSync(path.join(state, 'worlds', 'fixture'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'blueprints'), { recursive: true })
  fs.writeFileSync(path.join(state, 'worlds', 'fixture', 'world.json'), '{}\n')
  return { root, state, repo, close: () => fs.rmSync(root, { recursive: true, force: true }) }
}

async function invoke (kind, fx, args) {
  let output = ''
  const code = await execute(kind, ['--world', 'fixture', '--state', fx.state, '--repo', fx.repo, ...args], value => { output += value })
  return { code, value: readEDN(output.trim()), output }
}

const planText = id => `{:id "${id}" :parts [{:id "base" :cells [[0 64 0] [1 64 0] [2 64 0]] :want "stone"}]}`
const blueprintText = id => `{:id "${id}" :front :north :key {"S" "stone"} :layers [ ["S"] ]}`

test('a plan written by the tools names its maker in :metadata :by; an edit by another keeps the maker', async () => {
  const fx = fixture()
  try {
    const file = path.join(fx.state, 'worlds', 'fixture', 'plans', 'home.edn')
    assert.equal((await invoke('plan', fx, ['add', 'home', '--edn', planText('home'), '--by', 'Maker'])).code, 0)
    assert.match(fs.readFileSync(file, 'utf8'), /:metadata \{:by "Maker"\}/)
    const shown = await invoke('plan', fx, ['show', 'home'])
    assert.equal((await invoke('plan', fx, ['edit', 'home', '--edn', planText('home'), '--by', 'Other', '--revision', shown.value.revision])).code, 0)
    assert.match(fs.readFileSync(file, 'utf8'), /:metadata \{:by "Maker"\}/)
  } finally { fx.close() }
})

test('plan parser rejects missing world, bad commands, and unsupported options as EDN', async () => {
  const fx = fixture()
  try {
    const missing = await execute('plan', ['list'], text => { assert.match(text, /:invalid-world/) })
    assert.equal(missing, 2)
    const bad = await invoke('plan', fx, ['list', '--geometry'])
    assert.equal(bad.code, 2)
    assert.equal(bad.value.ok, false)
    assert.deepEqual(bad.value.reason, keyword('bad-option'))
  } finally { fx.close() }
})

test('plans use create-only, revision-checked mutations; a retired plan is a deleted plan', async () => {
  const fx = fixture()
  try {
    const added = await invoke('plan', fx, ['add', 'home', '--edn', planText('home'), '--by', 'tester'])
    assert.equal(added.code, 0)
    assert.deepEqual(added.value.op, keyword('add'))
    const shown = await invoke('plan', fx, ['show', 'home'])
    assert.deepEqual(shown.value.scope, keyword('world'))
    assert.equal(shown.value.revision, revision(fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'home.edn'), 'utf8')))
    const conflict = await invoke('plan', fx, ['edit', 'home', '--edn', planText('home'), '--by', 'tester', '--revision', '0'.repeat(24)])
    assert.deepEqual(conflict.value.reason, keyword('revision-conflict'))
    const dry = await invoke('plan', fx, ['remove', 'home', '--by', 'tester', '--revision', shown.value.revision, '--dry-run'])
    assert.equal(dry.value.preview, true)
    assert.equal((await invoke('plan', fx, ['show', 'home'])).code, 0)
    assert.equal((await invoke('plan', fx, ['remove', 'home', '--by', 'tester', '--revision', shown.value.revision])).code, 0)
    assert.equal((await invoke('plan', fx, ['show', 'home'])).code, 1)
  } finally { fx.close() }
})

test('the status command and list --status are gone and say where to go instead', async () => {
  const fx = fixture()
  try {
    const status = await invoke('plan', fx, ['status', 'home', '--status', 'active', '--by', 'tester', '--revision', '0'.repeat(24)])
    assert.equal(status.code, 2)
    assert.deepEqual(status.value.reason, keyword('usage'))
    assert.match(status.value.message, /every submitted plan is active/)
    assert.match(status.value.message, /remove/)
    const listed = await invoke('plan', fx, ['list', '--status', 'active'])
    assert.equal(listed.code, 2)
    assert.deepEqual(listed.value.reason, keyword('bad-option'))
    const withStatus = await invoke('plan', fx, ['validate', 'home', '--edn', '{:id "home" :status :active :parts [{:id "base" :cells [[0 64 0]] :want "stone"}]}'])
    assert.match(withStatus.output, /unknown key :status/)
  } finally { fx.close() }
})

test('blueprint tools validate inline, report global scope, and save with CAS', async () => {
  const fx = fixture()
  try {
    const saved = await invoke('blueprints', fx, ['save', 'stone', '--edn', blueprintText('stone'), '--by', 'tester'])
    assert.equal(saved.code, 0)
    assert.deepEqual(saved.value.scope, keyword('global'))
    const shown = await invoke('blueprints', fx, ['show', 'stone'])
    assert.deepEqual(shown.value.scope, keyword('global'))
    assert.equal(shown.value.width, 1)
    const raw = await invoke('blueprints', fx, ['show', 'stone', '--raw'])
    assert.match(raw.output, /:key \{"S" "stone"\}/)
    const rawList = await invoke('blueprints', fx, ['list', '--raw', '--limit', '1'])
    assert.equal(rawList.value.total, 1)
    assert.match(rawList.output, /:key \{"S" "stone"\}/)
    const stored = fs.readFileSync(path.join(fx.repo, 'blueprints', 'stone.edn'), 'utf8')
    assert.match(stored, /:key \{"S" "stone"\}/)
    const plan = '{:id "uses-stone" :parts [{:id "bp" :blueprint "stone" :at [0 64 0]}]}'
    const planCheck = await invoke('plan', fx, ['validate', 'uses-stone', '--edn', plan])
    assert.equal(planCheck.value.ok, true)
    const valid = await invoke('blueprints', fx, ['validate', 'stone', '--edn', blueprintText('stone')])
    assert.equal(valid.code, 0)
    const conflict = await invoke('blueprints', fx, ['save', 'stone', '--edn', blueprintText('stone'), '--by', 'tester', '--revision', '0'.repeat(24)])
    assert.deepEqual(conflict.value.reason, keyword('revision-conflict'))
  } finally { fx.close() }
})

test('blueprint remove needs the revision, deletes the file and drops it from list', async () => {
  const fx = fixture()
  try {
    await invoke('blueprints', fx, ['save', 'stone', '--edn', blueprintText('stone'), '--by', 'tester'])
    const file = path.join(fx.repo, 'blueprints', 'stone.edn')
    const rev = revision(fs.readFileSync(file, 'utf8'))
    const noRevision = await invoke('blueprints', fx, ['remove', 'stone', '--by', 'tester'])
    assert.deepEqual(noRevision.value.reason, keyword('revision-required'))
    const stale = await invoke('blueprints', fx, ['remove', 'stone', '--by', 'tester', '--revision', '0'.repeat(24)])
    assert.deepEqual(stale.value.reason, keyword('revision-conflict'))
    assert.ok(fs.existsSync(file))
    const dry = await invoke('blueprints', fx, ['remove', 'stone', '--by', 'tester', '--revision', rev, '--dry-run'])
    assert.equal(dry.code, 0)
    assert.ok(fs.existsSync(file))
    const removed = await invoke('blueprints', fx, ['remove', 'stone', '--by', 'tester', '--revision', rev])
    assert.equal(removed.code, 0)
    assert.ok(!fs.existsSync(file))
    assert.equal((await invoke('blueprints', fx, ['list'])).value.total, 0)
    const missing = await invoke('blueprints', fx, ['remove', 'stone', '--by', 'tester', '--revision', rev])
    assert.deepEqual(missing.value.reason, keyword('not-found'))
  } finally { fx.close() }
})

// A body's seen-block memory (engine/js/seen-file.mjs) holding the given [x, y, z, blockName, dimension = overworld] cells at one seen time.
async function writeSeen (fx, body, cells, seenAt) {
  const version = '1.20.4'
  const registry = prismarineRegistry(version)
  const dir = path.join(fx.state, 'worlds', 'fixture', 'agents', body, 'engine')
  fs.mkdirSync(dir, { recursive: true })
  const sections = new Map()
  for (const [x, y, z, name, dim = 'minecraft:overworld'] of cells) {
    const key = [dim, x >> 4, y >> 4, z >> 4].join()
    if (!sections.has(key)) sections.set(key, { dim, cx: x >> 4, sy: y >> 4, cz: z >> 4, seen: seenAt, base: seenAt, ids: new Uint16Array(4096), times: new Uint16Array(4096) })
    sections.get(key).ids[((y & 15) << 8) | ((z & 15) << 4) | (x & 15)] = registry.blocksByName[name].defaultState + 1
  }
  await saveSeen(path.join(dir, 'seen.bin'), { version, sections: [...sections.values()] })
}

test('check scores only cells the body has seen: a never-seen cell is unseen, a seen wrong block is wrong', async () => {
  const fx = fixture()
  try {
    await invoke('plan', fx, ['add', 'house', '--edn', planText('house'), '--by', 'tester'])
    await writeSeen(fx, 'Probe', [[0, 64, 0, 'stone'], [1, 64, 0, 'dirt']], Date.now() - 120000)
    const checked = await invoke('plan', fx, ['check', 'house', '--body', 'Probe'])
    assert.equal(checked.code, 0)
    assert.deepEqual(checked.value.evidence, keyword('seen-memory'))
    assert.equal(checked.value.score.counts.match, 1)
    assert.equal(checked.value.score.counts.wrong, 1)
    assert.equal(checked.value.score.counts.unknown, 1)
    assert.equal(checked.value.score.checked.unseen, 1)
    assert.equal(checked.value.score.checked.seen, 2)
    assert.ok(checked.value.score.checked['oldest-age-ms'] >= 120000)
  } finally { fx.close() }
})

test('check puts a :surface plan on the seen ground as the build does: the fence cell lands on the ground, a plain plan stays at its y', async () => {
  const fx = fixture()
  try {
    const plan = (id, extra) => `{:id "${id}"${extra} :parts [{:id "fence" :cells [[0 66 0]] :want "oak_fence"}]}`
    await invoke('plan', fx, ['add', 'slope', '--edn', plan('slope', ' :surface true'), '--by', 'tester'])
    await invoke('plan', fx, ['add', 'flat', '--edn', plan('flat', ''), '--by', 'tester'])
    const column = [[0, 60, 0, 'stone'], [0, 61, 0, 'stone'], [0, 62, 0, 'stone'], [0, 63, 0, 'oak_fence']]
    for (let y = 64; y <= 72; y++) column.push([0, y, 0, 'air'])
    await writeSeen(fx, 'Probe', column, Date.now() - 1000)
    const slope = await invoke('plan', fx, ['check', 'slope', '--body', 'Probe'])
    assert.equal(slope.value.score.counts.match, 1)
    const flat = await invoke('plan', fx, ['check', 'flat', '--body', 'Probe'])
    assert.equal(flat.value.score.counts.match ?? 0, 0)
  } finally { fx.close() }
})

test('check asks the running body to flush its seen memory first and says so; without one it states the data is the file', async () => {
  const fx = fixture()
  try {
    await invoke('plan', fx, ['add', 'house', '--edn', planText('house'), '--by', 'tester'])
    await writeSeen(fx, 'Probe', [[0, 64, 0, 'stone']], Date.now() - 120000)
    const offline = await invoke('plan', fx, ['check', 'house', '--body', 'Probe'])
    assert.equal(offline.value['live-loaded?'], false)
    assert.match(offline.value['seen-note'], /seen\.bin/)
    const flushed = []
    const server = http.createServer((req, res) => {
      flushed.push(`${req.method} ${req.url}`)
      res.writeHead(200, { 'content-type': 'application/edn' })
      res.end('{:ok true}')
    })
    const socket = path.join(fx.state, 'worlds', 'fixture', 'agents', 'Probe', 'engine', 'events.sock')
    await new Promise(resolve => server.listen(socket, resolve))
    try {
      const live = await invoke('plan', fx, ['check', 'house', '--body', 'Probe'])
      assert.deepEqual(flushed, ['POST /seen/flush'])
      assert.equal(live.value['live-loaded?'], true)
      assert.equal(live.value['seen-note'], undefined)
    } finally { await new Promise(resolve => server.close(resolve)) }
  } finally { fx.close() }
})

test('check judges a plan against the memory of its own :dim, the overworld when it has none', async () => {
  const fx = fixture()
  try {
    const nether = '{:id "hell" :dim "minecraft:the_nether" :parts [{:id "base" :cells [[0 64 0]] :want "stone"}]}'
    const over = '{:id "home" :parts [{:id "base" :cells [[0 64 0]] :want "stone"}]}'
    await invoke('plan', fx, ['add', 'hell', '--edn', nether, '--by', 'tester'])
    await invoke('plan', fx, ['add', 'home', '--edn', over, '--by', 'tester'])
    await writeSeen(fx, 'Probe', [[0, 64, 0, 'stone'], [0, 64, 0, 'dirt', 'minecraft:the_nether']], Date.now() - 1000)
    const hell = await invoke('plan', fx, ['check', 'hell', '--body', 'Probe'])
    assert.equal(hell.value.score.counts.wrong, 1)
    const home = await invoke('plan', fx, ['check', 'home', '--body', 'Probe'])
    assert.equal(home.value.score.counts.match, 1)
  } finally { fx.close() }
})

test('add stamps :dim from the adding body\'s current dimension; a non-body or an explicit :dim is left alone', async () => {
  const fx = fixture()
  try {
    const view = path.join(fx.state, 'worlds', 'fixture', 'agents', 'Probe', 'view')
    fs.mkdirSync(view, { recursive: true })
    fs.writeFileSync(path.join(view, 'pose.json'), JSON.stringify({ world: 'fixture', status: 'online', dimension: 'minecraft:the_nether', t: Date.now() }))
    const file = id => fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', id + '.edn'), 'utf8')
    await invoke('plan', fx, ['add', 'a', '--edn', planText('a'), '--by', 'Probe'])
    assert.match(file('a'), /:dim "minecraft:the_nether"/)
    await invoke('plan', fx, ['add', 'b', '--edn', planText('b'), '--by', 'tester'])
    assert.doesNotMatch(file('b'), /:dim/)
    await invoke('plan', fx, ['add', 'c', '--edn', planText('c').replace('{:id "c"', '{:id "c" :dim "minecraft:overworld"'), '--by', 'Probe'])
    assert.match(file('c'), /:dim "minecraft:overworld"/)
  } finally { fx.close() }
})

test('check without a seen memory file reports every cell unseen; check needs --body', async () => {
  const fx = fixture()
  try {
    await invoke('plan', fx, ['add', 'house', '--edn', planText('house'), '--by', 'tester'])
    const missing = await invoke('plan', fx, ['check', 'house'])
    assert.equal(missing.code, 2)
    assert.deepEqual(missing.value.reason, keyword('body-required'))
    const checked = await invoke('plan', fx, ['check', 'house', '--body', 'Nobody', '--inventory', '{:stone 1}'])
    assert.equal(checked.code, 0)
    assert.deepEqual(checked.value.evidence, keyword('seen-memory'))
    assert.equal(checked.value['live-loaded?'], false)
    assert.equal(checked.value.score.counts.unknown, 3)
    assert.equal(checked.value.score.checked.unseen, 3)
    assert.deepEqual(checked.value.score.materials.availability, keyword('provided-name-counts'))
    assert.equal(checked.value.score.materials.required.stone, 3)
    assert.equal(checked.value.score.materials['unknown-cells'], 3)
  } finally { fx.close() }
})

test('check pages part and score-element details with actionable offsets', async () => {
  const fx = fixture()
  try {
    const parts = Array.from({ length: 12 }, (_, i) => `{:id "p${i}" :cells [[${i} 64 0]] :want "stone"}`).join(' ')
    await invoke('plan', fx, ['add', 'paged', '--edn', `{:id "paged" :parts [${parts}]}`, '--by', 'tester'])
    const checked = await invoke('plan', fx, ['check', 'paged', '--body', 'Probe', '--limit', '3', '--offset', '3'])
    assert.equal(checked.value.parts.total, 12)
    assert.equal(checked.value.parts.items.length, 3)
    assert.equal(checked.value.parts['next-offset'], 6)
    assert.equal(checked.value.score['elements-total'], 12)
    assert.equal(checked.value.score.elements.length, 3)
    assert.equal(checked.value.score['elements-next-offset'], 6)
  } finally { fx.close() }
})

test('compiled CLJS preserves trailing EDN comments and rejects additional forms', async () => {
  const fx = fixture()
  try {
    const source = blueprintText('commented') + ' ; source comment'
    const saved = await invoke('blueprints', fx, ['save', 'commented', '--edn', source, '--by', 'tester', '--raw'])
    assert.equal(saved.code, 0, saved.output)
    assert.match(saved.output, /:key \{"S" "stone"\}/)
    assert.equal(fs.readFileSync(path.join(fx.repo, 'blueprints', 'commented.edn'), 'utf8').trim(), source)
    const shown = await invoke('blueprints', fx, ['show', 'commented', '--raw'])
    assert.equal(shown.value.document.id, 'commented')
    for (const invalid of [source + '\n{}', blueprintText('bad') + ') {} (']) {
      const result = await invoke('blueprints', fx, ['validate', 'bad', '--edn', invalid])
      assert.equal(result.code, 2)
      assert.deepEqual(result.value.reason, keyword('bad-edn'))
    }
  } finally { fx.close() }
})

test('a door with a planned wall beside it validates (the release bundle reads block shapes from minecraft-data by name)', async () => {
  const fx = fixture()
  try {
    const edn = '{:id "x" :parts [{:id "d" :cells [[61 69 10]] :want "oak_door"} {:id "w" :cells [[60 69 10]] :want "cobblestone"}]}'
    const result = await invoke('plan', fx, ['validate', 'x', '--edn', edn])
    assert.equal(result.code, 0, result.output)
  } finally { fx.close() }
})

test('native plan checks report overlapping saved claims', async () => {
  const fx = fixture()
  try {
    assert.equal((await invoke('plan', fx, ['add', 'claimed', '--edn', planText('claimed'), '--by', 'tester'])).code, 0)
    fs.writeFileSync(path.join(fx.state, 'worlds', 'fixture', 'claims.edn'),
      `[{:id "extension" :owner "another-agent" :min [0 64 0] :max [2 64 0] :status :active :until ${Date.now() + 60000}}]`)
    const checked = await invoke('plan', fx, ['check', 'claimed', '--body', 'Probe'])
    assert.equal(checked.code, 0, checked.output)
    assert.equal(checked.value.claims.total, 1)
    assert.equal(checked.value.claims.items[0].id, 'extension')
    assert.equal(checked.value.claims.items[0].count, 3)
  } finally { fx.close() }
})

test('validate judges an inline plan against the saved zones and claims like check does', async () => {
  const fx = fixture()
  try {
    const validate = () => invoke('plan', fx, ['validate', 'home', '--edn', planText('home')])
    assert.deepEqual((await validate()).value.zones.state, keyword('unknown'))
    fs.writeFileSync(path.join(fx.state, 'worlds', 'fixture', 'zones.edn'),
      '[{:name "wall" :owner "Other" :min [0 64 0] :max [1 64 0]}]\n')
    const zones = (await validate()).value.zones
    assert.deepEqual(zones.state, keyword('saved'))
    assert.equal(zones.overlaps.total, 1)
  } finally { fx.close() }
})

const SHAFT = ['mineshaft', 'pit', '--at', '0,64,0', '--heading', 'east', '--to-y', '62', '--by', 'Digger']

test('mineshaft writes a :mineshaft plan with one :stair part that passes validation; show and list name its depth and heading', async () => {
  const fx = fixture()
  try {
    const made = await invoke('plan', fx, SHAFT)
    assert.equal(made.code, 0, made.output)
    const file = fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'pit.edn'), 'utf8')
    assert.match(file, /:kind :mineshaft/)
    assert.match(file, /:stair \{:from \[0 64 0\] :heading :east :to-y 62\}/)
    assert.match(file, /:metadata \{:by "Digger"\}/)
    const shown = await invoke('plan', fx, ['show', 'pit'])
    assert.deepEqual(shown.value.validation.items, [])
    assert.deepEqual(shown.value.shaft, { heading: keyword('east'), from: [0, 64, 0], 'to-y': 62, depth: 2, steps: 2, bottom: [2, 62, 0] })
    const listed = await invoke('plan', fx, ['list'])
    assert.deepEqual(listed.value.items.map(i => [i.id, i.kind, i.shaft.depth]), [['pit', keyword('mineshaft'), 2]])
  } finally { fx.close() }
})

test('mineshaft refuses a bad heading, a to-y at or above the top, bad coordinates and an existing id', async () => {
  const fx = fixture()
  try {
    const bad = (args, reason) => invoke('plan', fx, ['mineshaft', 'pit', '--by', 'Digger', ...args]).then(r => {
      assert.equal(r.code, 2, r.output)
      assert.deepEqual(r.value.reason, keyword(reason))
    })
    await bad(['--at', '0,64,0', '--heading', 'up', '--to-y', '62'], 'bad-heading')
    await bad(['--at', '0,64,0', '--heading', 'east', '--to-y', '64'], 'bad-to-y')
    await bad(['--at', '0,64,0', '--heading', 'east', '--to-y', '70'], 'bad-to-y')
    await bad(['--at', '0,64', '--heading', 'east', '--to-y', '62'], 'bad-at')
    await bad(['--heading', 'east', '--to-y', '62'], 'bad-at')
    assert.equal((await invoke('plan', fx, SHAFT)).code, 0)
    const again = await invoke('plan', fx, SHAFT)
    assert.equal(again.code, 2)
    assert.deepEqual(again.value.reason, keyword('already-exists'))
    assert.match(again.output, /made by .* on \d{4}-\d\d-\d\d/)
  } finally { fx.close() }
})

test('check of a mineshaft counts its steps dug, undug and unseen from what the body has seen', async () => {
  const fx = fixture()
  try {
    await invoke('plan', fx, SHAFT)
    await writeSeen(fx, 'Probe', [[1, 65, 0, 'air'], [1, 64, 0, 'air'], [1, 63, 0, 'air'], [2, 64, 0, 'stone']], Date.now() - 1000)
    const checked = await invoke('plan', fx, ['check', 'pit', '--body', 'Probe'])
    assert.equal(checked.code, 0, checked.output)
    assert.deepEqual(checked.value.shaft, { steps: 2, dug: 1, undug: 1, unseen: 0, heading: keyword('east'), depth: 2 })
  } finally { fx.close() }
})

// Flat ground (stone to y 63, air 64..70) over x 0..40, z 0..10, with the given [x0 x1 top] hills and [x0 x1 floor] chasms.
function groundCells ({ hills = [], chasms = [] } = {}) {
  const cells = []
  for (let x = 0; x <= 40; x++) {
    for (let z = 0; z <= 10; z++) {
      const hill = hills.find(([a, b]) => x >= a && x <= b)
      const chasm = chasms.find(([a, b]) => x >= a && x <= b)
      const top = hill ? hill[2] : chasm ? chasm[2] : 63
      for (let y = 58; y <= 70; y++) cells.push([x, y, z, y <= top ? 'stone' : 'air'])
    }
  }
  return cells
}

const ROUTE = ['route', 'line', '--body', 'Probe', '--by', 'Router', '--from=2,64,5', '--to=38,64,5']

test('route writes the rail plan between two cells over the seen ground, with materials and the next step', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells(), Date.now() - 1000)
    const made = await invoke('plan', fx, ROUTE)
    assert.equal(made.code, 0, made.output)
    assert.ok(made.value.route.materials.items['powered_rail'] > 0)
    assert.match(made.value.route.next, /rail-line/)
    const shown = await invoke('plan', fx, ['show', 'line'])
    assert.deepEqual(shown.value.validation.items, [])
    const text = fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'line.edn'), 'utf8')
    assert.match(text, /"rail"/)
    assert.doesNotMatch(text, /:spans/)
  } finally { fx.close() }
})

test('route runs over rails already laid on the way and over ground cover at rail height: neither is ground', async () => {
  const fx = fixture()
  try {
    const cells = groundCells().map(([x, y, z, name]) => [x, y, z, y === 64 && z === 5 && x > 2 && x < 38 ? 'powered_rail' : y === 64 && z !== 5 ? 'short_grass' : name])
    await writeSeen(fx, 'Probe', cells, Date.now() - 1000)
    const made = await invoke('plan', fx, ROUTE)
    assert.equal(made.code, 0, made.output)
    assert.deepEqual(made.value.route.waypoints, [[2, 64, 5], [38, 64, 5]])
  } finally { fx.close() }
})

test('route tunnels through a hill by default and writes it as the plan\'s :spans; --tunnel 0 forbids it', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells({ hills: [[18, 26, 69]], chasms: [] }), Date.now() - 1000)
    const closed = await invoke('plan', fx, [...ROUTE, '--tunnel', '0'])
    assert.equal(closed.code, 2)
    assert.deepEqual(closed.value.reason, keyword('no-route'))
    const tunnelled = await invoke('plan', fx, ROUTE)
    assert.equal(tunnelled.code, 0, tunnelled.output)
    assert.deepEqual(tunnelled.value.route.spans.map(s => s.kind), [keyword('tunnel')])
    assert.match(fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'line.edn'), 'utf8'), /:spans \[\{:kind :tunnel/)
  } finally { fx.close() }
})

test('route tunnels from ends in solid or unseen rock, priced as the blocks dug, but not through water', async () => {
  const fx = fixture()
  try {
    const solid = groundCells().map(([x, y, z]) => [x, y, z, 'stone'])
    await writeSeen(fx, 'Probe', solid, Date.now() - 1000)
    const inRock = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(inRock.code, 0, inRock.output)
    assert.deepEqual(inRock.value.route.spans.map(s => s.kind), [keyword('tunnel')])
    await writeSeen(fx, 'Probe', solid.filter(([x]) => x >= 12 && x <= 28), Date.now() - 1000)
    const unseenEnds = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(unseenEnds.code, 0, unseenEnds.output)
    await writeSeen(fx, 'Probe', solid.map(([x, y, z]) => [x, y, z, x >= 18 && x <= 24 ? 'water' : 'stone']), Date.now() - 1000)
    assert.equal((await invoke('plan', fx, [...ROUTE, '--dry-run', '--bridge', '0'])).code, 2)
  } finally { fx.close() }
})

test('route bridges a seen drop of 2 or more and a lake by default; --bridge 0 forbids it', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells({ chasms: [[18, 24, 59]] }), Date.now() - 1000)
    assert.deepEqual((await invoke('plan', fx, [...ROUTE, '--bridge', '0'])).value.reason, keyword('no-route'))
    const bridged = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(bridged.code, 0, bridged.output)
    assert.deepEqual(bridged.value.route.spans.map(s => s.kind), [keyword('bridge')])
    const lake = groundCells().map(([x, y, z, name]) => (x >= 18 && x <= 24 && y >= 61 && y <= 63 ? [x, y, z, 'water'] : [x, y, z, name]))
    await writeSeen(fx, 'Probe', lake, Date.now() - 1000)
    const over = await invoke('plan', fx, ['route', 'lake', ...ROUTE.slice(2), '--dry-run'])
    assert.equal(over.code, 0, over.output)
    assert.deepEqual(over.value.route.spans.map(s => s.kind), [keyword('bridge')])
  } finally { fx.close() }
})

test('route bridges a chasm seen open below the column window, and not one only partly seen', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells({ chasms: [[18, 24, 57]] }), Date.now() - 1000)
    const bridged = await invoke('plan', fx, ROUTE)
    assert.equal(bridged.code, 0, bridged.output)
    assert.deepEqual(bridged.value.route.spans.map(s => s.kind), [keyword('bridge')])
    const unseen = groundCells({ chasms: [[18, 24, 57]] }).filter(([x, y]) => !(x >= 18 && x <= 24 && y <= 61))
    await writeSeen(fx, 'Probe', unseen, Date.now() - 1000)
    assert.deepEqual((await invoke('plan', fx, [...ROUTE, '--dry-run'])).value.reason, keyword('no-route'))
  } finally { fx.close() }
})

test('route cuts a bump of one block and fills a dip of one at rail height rather than climbing them', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells({ hills: [[12, 14, 64]], chasms: [[24, 26, 62]] }), Date.now() - 1000)
    const made = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(made.code, 0, made.output)
    assert.deepEqual(made.value.route.waypoints, [[2, 64, 5], [38, 64, 5]])
  } finally { fx.close() }
})

test('route treats a zone as no ground, the maker\'s own too, unless another owner\'s allows :place', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells(), Date.now() - 1000)
    const zones = path.join(fx.state, 'worlds', 'fixture', 'zones.edn')
    const wall = (owner, allow) => `[{:name "wall" :owner "${owner}" :min [10 60 0] :max [14 70 10]${allow}}]\n`
    fs.writeFileSync(zones, wall('Other', ''))
    assert.deepEqual((await invoke('plan', fx, ROUTE)).value.reason, keyword('no-route'))
    fs.writeFileSync(zones, wall('Router', ''))
    assert.deepEqual((await invoke('plan', fx, ROUTE)).value.reason, keyword('no-route'))
    fs.writeFileSync(zones, wall('Other', ' :allow #{:place :dig}'))
    assert.equal((await invoke('plan', fx, ROUTE)).code, 0)
  } finally { fx.close() }
})

// 0f59c1af: an end inside the maker's own zone lost its launch run to the zone's refusal
test('route may use the launch run of an end inside the maker\'s own zone, and no other zone', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells(), Date.now() - 1000)
    const zones = path.join(fx.state, 'worlds', 'fixture', 'zones.edn')
    const around = owner => `[{:name "home" :owner "${owner}" :min [0 60 0] :max [9 70 10]}]\n`
    fs.writeFileSync(zones, around('Other'))
    assert.deepEqual((await invoke('plan', fx, [...ROUTE, '--dry-run'])).value.reason, keyword('no-route'))
    fs.writeFileSync(zones, around('Router'))
    const made = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(made.code, 0, made.output)
    assert.deepEqual(made.value.route.waypoints, [[2, 64, 5], [38, 64, 5]])
  } finally { fx.close() }
})

// e321de28: the line ran through the maker's own house, its blocks priced as plain digs
test('route goes round the cells another plan wants, the maker\'s own plan too', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', groundCells(), Date.now() - 1000)
    const house = '{:id "house" :parts [{:id "wall" :cells [[20 64 4] [20 64 5] [20 64 6] [20 65 4] [20 65 5] [20 65 6]] :want "oak_planks"}]}'
    assert.equal((await invoke('plan', fx, ['add', 'house', '--edn', house, '--by', 'Router'])).code, 0)
    const made = await invoke('plan', fx, [...ROUTE, '--dry-run'])
    assert.equal(made.code, 0, made.output)
    assert.notDeepEqual(made.value.route.waypoints, [[2, 64, 5], [38, 64, 5]])
  } finally { fx.close() }
})

// b2eefec2: a start with no straight launch run got the generic "no rail line fits" hint
test('route with no launch run free at its start names that end and the straight cells it lacks', async () => {
  const fx = fixture()
  try {
    const walled = [-3, -2, -1, 11, 12, 13].flatMap(z => groundCells({ hills: [[0, 40, 69]] }).filter(c => c[2] === 0).map(([x, y, , name]) => [x, y, z, name]))
    await writeSeen(fx, 'Probe', [...groundCells({ hills: [[0, 1, 69], [4, 30, 69]] }), ...walled], Date.now() - 1000)
    const refused = await invoke('plan', fx, [...ROUTE, '--tunnel', '0', '--dry-run'])
    assert.deepEqual(refused.value.reason, keyword('no-route'))
    assert.match(refused.value.message, /--from \[2 64 5\] has no heading with 7 straight cells/)
    assert.doesNotMatch(refused.value.message, /--to /)
  } finally { fx.close() }
})

test('route refuses bad cells, a missing body or by, and an unseen world with no tunnel allowed; --dry-run writes nothing', async () => {
  const fx = fixture()
  try {
    const bad = args => invoke('plan', fx, ['route', 'line', '--body', 'Probe', '--by', 'Router', ...args])
    assert.deepEqual((await bad(['--from=1,2', '--to=5,64,5'])).value.reason, keyword('bad-from'))
    assert.deepEqual((await bad(['--from=1,64,5', '--to=1,64,5'])).value.reason, keyword('bad-to'))
    assert.deepEqual((await bad(['--from=1,64,5', '--to=9,64,5', '--tunnel=-1'])).value.reason, keyword('bad-span'))
    assert.deepEqual((await bad(['--from=1,64,5', '--to=9,64,5', '--nodes', '0'])).value.reason, keyword('bad-nodes'))
    assert.deepEqual((await invoke('plan', fx, ['route', 'line', '--by', 'Router', '--from=1,64,5', '--to=9,64,5'])).value.reason, keyword('body-required'))
    assert.deepEqual((await invoke('plan', fx, ['route', 'line', '--body', 'Probe', '--from=1,64,5', '--to=9,64,5'])).value.reason, keyword('actor-required'))
    assert.deepEqual((await bad(['--from=2,64,5', '--to=38,64,5', '--tunnel', '0', '--dry-run'])).value.reason, keyword('no-route'))
    await writeSeen(fx, 'Probe', groundCells(), Date.now() - 1000)
    const spent = await invoke('plan', fx, [...ROUTE, '--nodes', '4'])
    assert.deepEqual(spent.value.reason, keyword('no-route'))
    assert.match(spent.value.message, /ran out of its 4 nodes/)
    const dry = await invoke('plan', fx, [...ROUTE, '--dry-run', '--tunnel', '99'])
    assert.equal(dry.code, 0, dry.output)
    assert.ok(!fs.existsSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'line.edn')))
  } finally { fx.close() }
})

const ADOPT = ['adopt', 'farm', '--body', 'Probe', '--by', 'Maker', '--box=0,63,0:5,64,0']
const farmCells = [[0, 63, 0, 'dirt'], [1, 63, 0, 'farmland'], [1, 64, 0, 'wheat'], [2, 63, 0, 'water'], [2, 64, 0, 'air'],
  [3, 63, 0, 'stone'], [3, 64, 0, 'oak_fence'], [4, 64, 0, 'oak_fence_gate']]

test('adopt records every seen block of the box as it is, crops as crop wants; air and the unseen are out, --any areas are whatever', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', farmCells, Date.now() - 1000)
    fs.mkdirSync(path.join(fx.state, 'worlds', 'fixture'), { recursive: true })
    fs.writeFileSync(path.join(fx.state, 'worlds', 'fixture', 'zones.edn'), '[{:name "z" :owner "Maker" :min [0 60 0] :max [3 70 0]}]\n')
    const dry = await invoke('plan', fx, [...ADOPT, '--dry-run'])
    assert.equal(dry.code, 0, dry.output)
    assert.ok(!fs.existsSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'farm.edn')))
    const made = await invoke('plan', fx, ADOPT)
    assert.equal(made.code, 0, made.output)
    assert.deepEqual(made.value.adopted.parts, { dirt: 1, farmland: 1, wheat: 1, water: 1, stone: 1, fence: 1, gate: 1 })
    assert.equal(made.value.adopted.unseen, 4)
    assert.equal(made.value.adopted['outside-author-zones'], 1)
    const text = fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'farm.edn'), 'utf8')
    assert.match(text, /:want \{:crop "wheat"\}/)
    assert.match(text, /:want \{:facing "north" :block "oak_fence_gate"\}/)
    assert.match(text, /:metadata \{:by "Maker"\}/)
    assert.doesNotMatch(text, /:open/)
    const any = await invoke('plan', fx, ['adopt', 'wild', ...ADOPT.slice(2), '--any=0,63,0:1,64,0', '--any=3,63,0:3,63,0', '--dry-run'])
    assert.deepEqual(any.value.adopted.parts, { water: 1, fence: 1, gate: 1, any: 0, 'any-2': 0 })
  } finally { fx.close() }
})

test('adopt --pen and --herd add the pen part animals.maintain reads, and refuse one without the other', async () => {
  const fx = fixture()
  try {
    await writeSeen(fx, 'Probe', farmCells, Date.now() - 1000)
    const pen = await invoke('plan', fx, [...ADOPT, '--pen=0,64,0:5,64,0', '--herd', 'cow:2'])
    assert.equal(pen.code, 0, pen.output)
    const text = fs.readFileSync(path.join(fx.state, 'worlds', 'fixture', 'plans', 'farm.edn'), 'utf8')
    assert.match(text, /:id "pen"/)
    assert.match(text, /:herd \{:mob "cow" :target 2\}/)
    const half = await invoke('plan', fx, [...ADOPT, '--pen=0,64,0:5,64,0', '--dry-run'])
    assert.deepEqual(half.value.reason, keyword('bad-pen'))
  } finally { fx.close() }
})

test('adopt refuses a bad box, a missing body and a box where nothing was seen', async () => {
  const fx = fixture()
  try {
    const bad = args => invoke('plan', fx, ['adopt', 'farm', '--by', 'Maker', ...args])
    assert.deepEqual((await bad(['--body', 'Probe', '--box=0,63:5,64,0'])).value.reason, keyword('bad-box'))
    assert.deepEqual((await bad(['--box=0,63,0:5,64,0'])).value.reason, keyword('body-required'))
    assert.deepEqual((await bad(['--body', 'Probe', '--box=0,63,0:5,64,0'])).value.reason, keyword('nothing-seen'))
  } finally { fx.close() }
})

test('blueprints save --from-plan cuts a part into a blueprint that places the same cells', async () => {
  const fx = fixture()
  try {
    const plan = '{:id "src" :parts [{:id "wall" :box [[3 64 5] [5 65 5]] :want "stone"}]}'
    assert.equal((await invoke('plan', fx, ['add', 'src', '--edn', plan, '--by', 'tester'])).code, 0)
    const saved = await invoke('blueprints', fx, ['save', 'cut', '--from-plan', 'src/wall', '--by', 'tester'])
    assert.equal(saved.code, 0)
    const placed = '{:id "dst" :parts [{:id "p" :blueprint "cut" :at [3 64 5]}]}'
    assert.equal((await invoke('plan', fx, ['add', 'dst', '--edn', placed, '--by', 'tester'])).code, 0)
    const geo = async id => (await invoke('plan', fx, ['show', id, '--geometry'])).value.geometry
    assert.deepEqual(await geo('dst'), await geo('src'))
    const missing = await invoke('blueprints', fx, ['save', 'x', '--from-plan', 'src/nope', '--by', 'tester'])
    assert.equal(missing.code, 2)
  } finally { fx.close() }
})
