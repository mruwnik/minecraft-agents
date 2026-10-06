// Why JavaScript: runs tools/commit-mine.test.sh inside the `tools` suite (node --test); no logic here.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

test('commit-mine.test.sh passes', () => {
  const r = spawnSync('bash', [join(dirname(fileURLToPath(import.meta.url)), 'commit-mine.test.sh')], { encoding: 'utf8' })
  assert.equal(r.status, 0, (r.stdout.match(/^FAIL.*$/gm) || []).join('\n') + r.stderr)
})
