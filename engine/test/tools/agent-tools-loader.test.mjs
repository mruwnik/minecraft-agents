import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import {
  buildRequiredEdn,
  buildRequiredMessage,
  missingExports,
} from '../../tools/agent-tools-bundle-check.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const toolsDir = path.join(here, '..', '..', 'tools')
const loadToolsRe = /loadTools\(\[([^\]]*)\]\)/
const launchers = () => fs.readdirSync(toolsDir)
  .filter(f => f.endsWith('.mjs'))
  .map(f => ({ file: f, source: fs.readFileSync(path.join(toolsDir, f), 'utf8') }))
  .filter(({ source }) => loadToolsRe.test(source))
const declared = source => [...loadToolsRe.exec(source)[1].matchAll(/'([A-Za-z]+)'/g)].map(m => m[1]).sort()
const called = source => [...new Set([...source.matchAll(/\btools\.([a-z]+[A-Z][A-Za-z]*|storage)\b/g)].map(m => m[1]))].sort()
const shadowExports = () => {
  const edn = fs.readFileSync(path.join(here, '..', '..', '..', 'dashboard', 'shadow-cljs.edn'), 'utf8')
  return new Set([...edn.matchAll(/^\s*(?::exports \{)?:([A-Za-z]+) agent-tools\./gm)].map(m => m[1]))
}

test('stale bundle report names the missing exports and the rebuild command', () => {
  const bundle = { sayMain: () => {} }
  assert.deepEqual(missingExports(bundle, ['driveMain', 'sayMain', 'worldMain']), ['driveMain', 'worldMain'])
  assert.match(buildRequiredMessage(['driveMain', 'worldMain']), /stale/)
  assert.match(buildRequiredMessage(['driveMain', 'worldMain']), /npm run build-agent-tools/)
  assert.equal(
    buildRequiredEdn(['driveMain', 'worldMain']),
    '{:ok false :reason :build-required :missing-exports ["driveMain" "worldMain"] :message "The compiled agent-tools bundle is stale; missing exports: driveMain, worldMain. Rebuild it once with: cd dashboard && npm run build-agent-tools"}',
  )
})

test('every launcher declares exactly the bundle exports it calls, each a configured shadow export', () => {
  const found = launchers()
  assert.ok(found.length >= 10)
  const configured = shadowExports()
  for (const { source } of found) {
    assert.deepEqual(declared(source), called(source))
    assert.deepEqual(declared(source).filter(name => !configured.has(name)), [])
  }
})

test('every launcher importing the loader declares its exports', () => {
  const undeclared = fs.readdirSync(toolsDir)
    .filter(f => f.endsWith('.mjs') && f !== 'agent-tools-loader.mjs')
    .filter(f => /from '\.\/agent-tools-loader\.mjs'/.test(fs.readFileSync(path.join(toolsDir, f), 'utf8')))
    .filter(f => !launchers().some(l => l.file === f))
  assert.deepEqual(undeclared, [])
})

const importWith = (bundleSource, launcher) => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scratch-bundle-'))
  try {
    const bundle = path.join(dir, 'agent-tools.cjs')
    fs.writeFileSync(bundle, bundleSource)
    return spawnSync(process.execPath, ['--input-type=module', '-e',
      `const m = await import(${JSON.stringify(path.join(toolsDir, launcher))}); process.stdout.write(String(m.usage))`],
    { env: { ...process.env, AGENT_TOOLS_BUNDLE: bundle }, encoding: 'utf8' })
  } finally {
    fs.rmSync(dir, { recursive: true, force: true })
  }
}

const sayOnlyBundle = 'module.exports = { sayUsage: "say-usage", sayRequestFor () {}, sayMain () {} }'

test('a bundle missing only the snapshot exports still loads the say launcher', () => {
  const result = importWith(sayOnlyBundle, 'say.mjs')
  assert.equal(result.status, 0, result.stderr)
  assert.equal(result.stdout, 'say-usage')
})

test('the launcher whose export is missing reports a stale bundle naming only its own exports', () => {
  const result = importWith(sayOnlyBundle, 'snapshot.mjs')
  assert.equal(result.status, 2)
  assert.match(result.stderr, /stale; missing exports: snapshotMain, snapshotUsage\./)
  assert.match(result.stdout, /:reason :build-required :missing-exports \["snapshotMain" "snapshotUsage"\]/)
})
