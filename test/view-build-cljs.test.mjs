// When the viewer's cljs build is rebuilt before the tests and by the dashboard launcher.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { needsBuild, sources } from '../tools/view/build-cljs.mjs'

for (const [name, outputMtime, sourceMtimes, expected] of [
  ['no output', null, [1, 2], true],
  ['every source older', 10, [1, 2], false],
  ['a source as old as the output', 10, [10], false],
  ['one source newer', 10, [1, 11], true],
  ['no sources', 10, [], false]
]) {
  test(`needsBuild: ${name}`, () => assert.equal(needsBuild(outputMtime, sourceMtimes), expected))
}

test('sources cover all of the dashboard src (any namespace the viewer requires) and the build config', () => {
  assert.ok(sources.some(s => s.endsWith(path.join('dashboard', 'src'))))
  assert.ok(sources.some(s => s.endsWith('shadow-cljs.edn')))
})
