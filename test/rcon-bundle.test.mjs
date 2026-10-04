import test from 'node:test'
import assert from 'node:assert/strict'
import { loadRconTools, BUILD_HINT } from '../tools/rcon-bundle.mjs'

test('an unbuilt bundle gives a clear error naming the build command', () => {
  assert.throws(() => loadRconTools('/nonexistent/rcon-tools.cjs'), err => err.message.includes('not built') && err.message.includes(BUILD_HINT))
})
