import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import {
  buildRequiredEdn,
  buildRequiredMessage,
  launcherExports,
  missingExports,
} from '../../tools/agent-tools-bundle-check.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const toolsDir = path.join(here, '..', '..', 'tools')
const launcherCalls = () => [...new Set(fs.readdirSync(toolsDir)
  .filter(f => f.endsWith('.mjs'))
  .flatMap(f => [...fs.readFileSync(path.join(toolsDir, f), 'utf8').matchAll(/\btools\.([a-z]+[A-Z][A-Za-z]*|storage)\b/g)].map(m => m[1])))].sort()
const shadowExports = () => {
  const edn = fs.readFileSync(path.join(here, '..', '..', '..', 'dashboard', 'shadow-cljs.edn'), 'utf8')
  return new Set([...edn.matchAll(/^\s*(?::exports \{)?:([A-Za-z]+) agent-tools\./gm)].map(m => m[1]))
}

test('compiled launcher bundle check identifies stale drive and world exports', () => {
  const bundle = Object.fromEntries(launcherExports.map(name => [name, () => {}]))
  delete bundle.driveMain
  delete bundle.worldMain

  assert.deepEqual(missingExports(bundle), ['driveMain', 'worldMain'])
  assert.match(buildRequiredMessage(['driveMain', 'worldMain']), /stale/)
  assert.match(buildRequiredMessage(['driveMain', 'worldMain']), /npm run build-agent-tools/)
  assert.equal(
    buildRequiredEdn(['driveMain', 'worldMain']),
    '{:ok false :reason :build-required :missing-exports ["driveMain" "worldMain"] :message "The compiled agent-tools bundle is stale; missing exports: driveMain, worldMain. Rebuild it once with: cd dashboard && npm run build-agent-tools"}',
  )
})

test('complete compiled launcher bundle passes without spawning a build', () => {
  const bundle = Object.fromEntries(launcherExports.map(name => [name, () => {}]))
  assert.deepEqual(missingExports(bundle), [])
})

test('the checked list names every bundle export the launchers call, and each is a configured export', () => {
  assert.deepEqual(launcherCalls().filter(name => !launcherExports.includes(name)), [])
  const configured = shadowExports()
  assert.deepEqual(launcherExports.filter(name => !configured.has(name)), [])
})
