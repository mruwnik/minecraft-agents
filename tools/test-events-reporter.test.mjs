// Why JavaScript: node --test file for tools/test-events-reporter.mjs (a node:test reporter).
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import test, { describe } from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'

const reporter = path.resolve(import.meta.dirname, 'test-events-reporter.mjs')

// run_tests (node-test runner) injects --test-reporter flags via NODE_OPTIONS; the child must not inherit them.
const withoutRunnerEnv = ({ NODE_OPTIONS, NODE_TEST_CONTEXT, ...rest }) => rest

const run = (env, lines = true) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'test-events-reporter-'))
  const file = path.join(dir, 'sample.test.mjs')
  fs.writeFileSync(file, `import test, { describe } from 'node:test'
import assert from 'node:assert/strict'
test('adds', () => assert.equal(1 + 1, 2))
test('breaks', () => assert.equal(1, 2))
test('later', { skip: true }, () => {})
describe('outer', () => { test('inner', () => {}) })
`)
  try {
    const r = spawnSync(process.execPath, ['--test', `--test-reporter=${reporter}`, file], { encoding: 'utf8', env: { ...withoutRunnerEnv(process.env), ...env } })
    if (!lines) return r.stdout
    return r.stdout.split('\n').filter((l) => l.startsWith('@@test ')).map((l) => JSON.parse(l.slice(7)))
  } finally { fs.rmSync(dir, { recursive: true }) }
}

test('TEST_EVENTS=1: one result line per test with its outcome', () => {
  const results = run({ TEST_EVENTS: '1' }).filter((e) => e.event === 'result')
  assert.deepEqual(results.map((e) => [e.name, e.outcome]), [['adds', 'passed'], ['breaks', 'failed'], ['later', 'skipped'], ['outer/inner', 'passed']])
})

test('TEST_EVENTS=1: a failing result carries the failure message', () => {
  const failed = run({ TEST_EVENTS: '1' }).find((e) => e.outcome === 'failed')
  assert.match(failed.message, /1 !== 2|Expected values/)
})

test('without TEST_EVENTS: no @@test lines', () => {
  assert.deepEqual(run({ TEST_EVENTS: '' }), [])
})

test('without TEST_EVENTS: piped output stays node\'s default non-TTY format (tap)', () => {
  assert.match(run({ TEST_EVENTS: '' }, false), /^TAP version 13/m)
})
