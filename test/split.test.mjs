// The module split is a pure move: every moved name is exported from its new home and still reachable through the barrel
// it used to live in, as the same binding.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import * as lib from '../src/lib.mjs'

const MOVED = [
  ['../src/lib/jobs.mjs', lib, ['tillWarning', 'farmJobs', 'jobCall', 'jobsBill', 'shortLine', 'penProbes', 'penInside', 'openingJobs', 'penOpenRefusal', 'billShortfall', 'groundJobs']],
  ['../src/lib/help.mjs', lib, ['RENAMED', 'renamedTo', 'renamedList', 'didYouMean', 'SECTIONS', 'argsUsage', 'docText', 'helpText', 'PRIMITIVES']]
]

for (const [path, barrel, names] of MOVED) {
  test(`${path} exports what moved into it, and the barrel re-exports the same bindings`, async () => {
    const mod = await import(path)
    assert.deepEqual(names.filter(n => !(n in mod)), [])
    assert.deepEqual(names.filter(n => barrel[n] !== mod[n]), [])
  })
}
