// Why JavaScript: tests the Node loader-boundary module compile-cache.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const module = fileURLToPath(new URL('./compile-cache.mjs', import.meta.url))
const run = (env, code) => spawnSync(process.execPath, ['--input-type=module', '-e', code], { encoding: 'utf8', env: { ...process.env, ...env } })

test('default cache dir is the repo-ignored state/compile-cache', () => {
  const out = run({}, `import { cacheDir } from ${JSON.stringify(module)}; console.log(cacheDir)`)
  assert.equal(path.basename(path.dirname(out.stdout.trim())), 'state')
  assert.equal(path.basename(out.stdout.trim()), 'compile-cache')
})

test('importing it creates the cache dir', () => {
  const dir = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'cc-test-')), 'cache')
  try {
    const out = run({ MC_COMPILE_CACHE_DIR: dir }, `import ${JSON.stringify(module)}; import 'node:assert'`)
    assert.equal(out.status, 0, out.stderr)
    assert.ok(fs.existsSync(dir))
  } finally {
    fs.rmSync(path.dirname(dir), { recursive: true, force: true })
  }
})

test('MC_COMPILE_CACHE=off leaves the dir uncreated', () => {
  const dir = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'cc-test-')), 'cache')
  try {
    run({ MC_COMPILE_CACHE_DIR: dir, MC_COMPILE_CACHE: 'off' }, `import ${JSON.stringify(module)}`)
    assert.ok(!fs.existsSync(dir))
  } finally {
    fs.rmSync(path.dirname(dir), { recursive: true, force: true })
  }
})
