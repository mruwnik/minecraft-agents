// Why JavaScript: tests the compiled agent-tools bundle (a JS artifact) through its exports.
import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'

const bundle = createRequire(import.meta.url)(fileURLToPath(new URL('../../../dashboard/out/agent-tools.cjs', import.meta.url)))

// The release bundle renames .-prop reads of minecraft-data objects. phantom and slime are hostile only by
// their "category" field, so a renamed read makes them non-hostile.
test('the bundle reads minecraft-data entity fields by name', () => {
  assert.equal(bundle.snapshotEntityKind('1.21.4', 'phantom'), 'hostile')
  assert.equal(bundle.snapshotEntityKind('1.21.4', 'slime'), 'hostile')
  assert.equal(bundle.snapshotEntityKind('1.21.4', 'zombie'), 'hostile')
  assert.equal(bundle.snapshotEntityKind('1.21.4', 'cow'), null)
})
