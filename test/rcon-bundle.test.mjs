import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import test from 'node:test'
import assert from 'node:assert/strict'
import { loadRconTools, BUILD_HINT } from '../tools/rcon-bundle.mjs'

test('an unbuilt bundle gives a clear error naming the build command', () => {
  assert.throws(() => loadRconTools('/nonexistent/rcon-tools.cjs'), err => err.message.includes('not built') && err.message.includes(BUILD_HINT))
})

const touch = (file, seconds) => fs.utimesSync(file, seconds, seconds)

function fixture () {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'rcon-bundle-'))
  const bundle = path.join(dir, 'rcon-tools.cjs')
  const source = path.join(dir, 'rcon.cljs')
  fs.writeFileSync(bundle, 'module.exports = {}')
  fs.writeFileSync(source, '(ns x)')
  return { dir, bundle, source }
}

test('a bundle older than an RCON source file is refused with the rebuild command', () => {
  const { dir, bundle, source } = fixture()
  touch(bundle, 1000)
  touch(source, 2000)
  assert.throws(() => loadRconTools(bundle, dir), err => err.message.includes('stale') && err.message.includes(BUILD_HINT))
})

test('a bundle newer than every RCON source file loads', () => {
  const { dir, bundle, source } = fixture()
  touch(source, 1000)
  touch(bundle, 2000)
  assert.deepEqual(loadRconTools(bundle, dir), {})
})

test('only rcon*.cljs sources count towards staleness', () => {
  const { dir, bundle, source } = fixture()
  touch(source, 500)
  const other = path.join(dir, 'server.cljs')
  fs.writeFileSync(other, '(ns y)')
  touch(bundle, 1000)
  touch(other, 2000)
  assert.deepEqual(loadRconTools(bundle, dir), {})
})
