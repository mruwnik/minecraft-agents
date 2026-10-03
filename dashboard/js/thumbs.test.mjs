import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createRenderer, workerLimits } from './thumbs.mjs'

// a state dir whose bodies A, B, C and Nobody all have a pose file without a position (a body that never fully started)
const emptyState = () => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'thumbs-'))
  for (const name of ['A', 'B', 'C', 'Nobody']) {
    fs.mkdirSync(path.join(stateDir, 'agents', name, 'view'), { recursive: true })
    fs.writeFileSync(path.join(stateDir, 'agents', name, 'view', 'pose.json'), '{}')
  }
  return stateDir
}

test('render gives null for a pose without a position', async () => {
  const r = createRenderer({ stateDir: emptyState() })
  const result = await r.render('Nobody')
  r.close()
  assert.equal(result, null)
})

test('a recycled renderer starts a fresh worker on the next render', async () => {
  const r = createRenderer({ stateDir: emptyState() })
  assert.equal(await r.render('Nobody'), null)
  r.recycle()
  assert.equal(await r.render('Nobody'), null)
  r.close()
})

test('concurrent renders each get their own answer', async () => {
  const r = createRenderer({ stateDir: emptyState() })
  const results = await Promise.all(['A', 'B', 'C'].map(name => r.render(name)))
  r.close()
  assert.deepEqual(results, [null, null, null])
})

test('render rejects when there is no pose file', async () => {
  const r = createRenderer({ stateDir: emptyState() })
  await assert.rejects(r.render('Ghost'), /no pose\.json for Ghost/)
  r.close()
})

test('workerLimits caps the old generation', () => {
  assert.equal(workerLimits.maxOldGenerationSizeMb, 160)
})
