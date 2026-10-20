// Why JavaScript: tests the Node loader-boundary module lazy-requires.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const here = path.dirname(fileURLToPath(import.meta.url))

function run (script) {
  const source = `import './lazy-requires.mjs'\nimport { createRequire } from 'node:module'\nconst require = createRequire(import.meta.url)\n${script}`
  const r = spawnSync(process.execPath, ['--input-type=module', '-e', source], { cwd: here, encoding: 'utf8' })
  assert.equal(r.status, 0, r.stderr)
  return JSON.parse(r.stdout)
}

test('an unspecified ProtoDef has no schema validator, an explicit one keeps it', () => {
  const out = run(`
    const { ProtoDef } = require('protodef')
    console.log(JSON.stringify({ implicit: new ProtoDef().validator, explicit: new ProtoDef(true).validator !== null }))`)
  assert.deepEqual(out, { implicit: null, explicit: true })
})

test('the Microsoft login module is not loaded until a login is started', () => {
  const out = run(`
    const names = Object.keys(require('minecraft-protocol/src/client/microsoftAuth.js'))
    console.log(JSON.stringify({ names, loaded: Object.keys(require.cache).some(f => f.includes('prismarine-auth')) }))`)
  assert.deepEqual(out, { names: ['authenticate', 'realmAuthenticate'], loaded: false })
})
