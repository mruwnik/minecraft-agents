// Why JavaScript: node:test of the node entry point tools/event-counts.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'

const tool = path.join(import.meta.dirname, 'event-counts.mjs')

function tree(bundleAge, sourceAge) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'event-counts-'))
  const src = path.join(root, 'dashboard', 'src', 'world_test')
  fs.mkdirSync(src, { recursive: true })
  fs.mkdirSync(path.join(root, 'dashboard', 'out'))
  const file = (p, ageS) => {
    fs.writeFileSync(p, 'x')
    const t = new Date(Date.now() - ageS * 1000)
    fs.utimesSync(p, t, t)
  }
  file(path.join(src, 'event_counts.cljs'), sourceAge)
  file(path.join(root, 'dashboard', 'shadow-cljs.edn'), 1000)
  file(path.join(root, 'dashboard', 'out', 'world-test.cjs'), bundleAge)
  return root
}

function run(root) {
  const r = spawnSync('node', [tool, 'fold'], { env: { ...process.env, EVENT_COUNTS_ROOT: root }, encoding: 'utf8' })
  fs.rmSync(root, { recursive: true })
  return r
}

test('a bundle older than its sources is refused, not run', () => {
  const r = run(tree(100, 10))
  assert.equal(r.status, 2)
  assert.match(r.stderr, /older than its sources/)
})

test('a bundle newer than its sources is run', () => {
  assert.doesNotMatch(run(tree(10, 100)).stderr, /older than its sources/)
})
