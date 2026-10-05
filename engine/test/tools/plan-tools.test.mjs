// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createRequire } from 'node:module'
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
const require = createRequire(import.meta.url)

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

test('check marks absent column data unknown and includes bounded offline evidence', async () => {
  const fx = fixture()
  try {
    await invoke('plan', fx, ['add', 'house', '--edn', planText('house'), '--by', 'tester'])
    const checked = await invoke('plan', fx, ['check', 'house', '--inventory', '{:stone 1}'])
    assert.equal(checked.code, 0)
    assert.deepEqual(checked.value.evidence, keyword('saved-column-dumps'))
    assert.equal(checked.value['live-loaded?'], false)
    assert.equal(checked.value.score.counts.unknown, 3)
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
    const checked = await invoke('plan', fx, ['check', 'paged', '--limit', '3', '--offset', '3'])
    assert.equal(checked.value.parts.total, 12)
    assert.equal(checked.value.parts.items.length, 3)
    assert.equal(checked.value.parts['next-offset'], 6)
    assert.equal(checked.value.score['elements-total'], 12)
    assert.equal(checked.value.score.elements.length, 3)
    assert.equal(checked.value.score['elements-next-offset'], 6)
  } finally { fx.close() }
})

test('score bridge preserves coordinate, material and block-property string keys', async () => {
  const bridge = require('../../../dashboard/out/agent-tools.cjs')
  const prepared = bridge.prepare(planText('score'), 'score', '[]', '[]', null, '[]')
  assert.equal(prepared.ok, true)
  const blocks = {
    '0,64,0': { name: 'stone', state: {} },
    '1,64,0': { name: 'dirt', state: {} },
    '2,64,0': { name: 'air', state: {} }
  }
  const withInventory = bridge.score(prepared['expansion-edn'], JSON.stringify(blocks), JSON.stringify({ '0,0': 1 }), JSON.stringify({ stone: 1 }), true, 0, 10)
  assert.equal(withInventory.counts.match, 1)
  assert.equal(withInventory.counts.wrong, 1)
  assert.equal(withInventory.counts.missing, 1)
  assert.equal(withInventory.materials.required.stone, 3)
  assert.equal(withInventory.materials.remaining.stone.shortage, 1)
  const unknown = bridge.score(prepared['expansion-edn'], JSON.stringify({}), JSON.stringify({}), '{}', false, 0, 10)
  assert.equal(unknown.counts.unknown, 3)
  assert.equal(unknown.materials.availability, 'unknown')
})

test('candidate checks report conflicts with every stored plan', () => {
  const bridge = require('../../../dashboard/out/agent-tools.cjs')
  const candidate = '{:id "candidate" :parts [{:id "a" :cells [[4 64 9]] :want "dirt"}]}'
  const active = '{:id "active" :parts [{:id "b" :cells [[4 64 9]] :want "stone"}]}'
  const prepared = bridge.prepare(candidate, 'candidate', '[]', JSON.stringify([{ id: 'active', text: active }]), null, '[]')
  assert.equal(prepared.ok, true)
  assert.equal(prepared.conflicts.length, 1)
  assert.equal(prepared.conflicts[0].with, 'active')
  assert.equal(prepared.conflicts[0].count, 1)
})

test('the aggregate cell budget counts every stored plan', () => {
  const bridge = require('../../../dashboard/out/agent-tools.cjs')
  const big = id => `{:id "${id}" :parts [{:id "all" :cells [${Array.from({ length: 60000 }, (_, x) => `[${x} 64 0]`).join(' ')}] :want "stone"}]}`
  const candidate = '{:id "candidate" :parts [{:id "a" :cells [[0 70 0]] :want "dirt"}]}'
  const prepared = bridge.prepare(candidate, 'candidate', '[]', JSON.stringify([{ id: 'one', text: big('one') }, { id: 'two', text: big('two') }]), null, '[]')
  assert.equal(prepared.ok, false)
  assert.match(prepared.errors.join(' '), /exceeds 100000/)
})

test('bridge rejects oversized plan geometry before expanding cells', () => {
  const bridge = require('../../../dashboard/out/agent-tools.cjs')
  const cells = Array.from({ length: 100001 }, (_, x) => `[${x} 64 0]`).join(' ')
  const plan = `{:id "large" :parts [{:id "all" :cells [${cells}] :want "stone"}]}`
  const result = bridge.prepare(plan, 'large', '[]', '[]', null, '[]')
  assert.equal(result.ok, false)
  assert.match(result.errors.join(' '), /aggregate plan conflict index exceeds 100000/)
  assert.equal(result.cells.length, 0)
})

test('bridge estimates wide shallow blueprint placements by width before expansion', () => {
  const bridge = require('../../../dashboard/out/agent-tools.cjs')
  const row = 'S'.repeat(100001)
  const blueprint = `{:id "wide" :front :north :key {"S" "stone"} :layers [["${row}"]]}`
  const plan = '{:id "wide-plan" :parts [{:id "p" :blueprint "wide" :at [0 64 0]}]}'
  const result = bridge.prepare(plan, 'wide-plan', JSON.stringify([{ id: 'wide', text: blueprint }]), '[]', null, '[]')
  assert.equal(result.ok, false)
  assert.match(result.errors.join(' '), /exceeds 100000/)
  assert.equal(result.cells.length, 0)
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

test('native plan checks report overlapping saved claims', async () => {
  const fx = fixture()
  try {
    assert.equal((await invoke('plan', fx, ['add', 'claimed', '--edn', planText('claimed'), '--by', 'tester'])).code, 0)
    fs.writeFileSync(path.join(fx.state, 'worlds', 'fixture', 'claims.edn'),
      `[{:id "extension" :owner "another-agent" :min [0 64 0] :max [2 64 0] :status :active :until ${Date.now() + 60000}}]`)
    const checked = await invoke('plan', fx, ['check', 'claimed'])
    assert.equal(checked.code, 0, checked.output)
    assert.equal(checked.value.claims.total, 1)
    assert.equal(checked.value.claims.items[0].id, 'extension')
    assert.equal(checked.value.claims.items[0].count, 3)
  } finally { fx.close() }
})
