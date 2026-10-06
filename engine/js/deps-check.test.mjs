// Why JavaScript: tests deps-check.mjs, which stays JS: inspects patch markers in JS node_modules sources (Mineflayer dependencies); must run before any cljs is built.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { join } from 'node:path'
import { PATCHES, REQUIRED, missingPatches, missingRequired } from './deps-check.mjs'

const allPresent = path => PATCHES.filter(([p]) => p === path).map(([, marker]) => marker).join('\n')

test('all markers present -> nothing missing', () => {
  assert.deepEqual(missingPatches(allPresent), [])
})

test('a missing marker names its title', () => {
  const read = path => path === PATCHES[3][0] ? allPresent(path).replace(PATCHES[3][1], '') : allPresent(path)
  assert.deepEqual(missingPatches(read), [PATCHES[3][2]])
})

test('a missing file names every title in it', () => {
  const path = 'node_modules/mineflayer-pathfinder/index.js'
  const read = p => p === path ? null : allPresent(p)
  assert.deepEqual(missingPatches(read), ['centred gate waypoints', 'gate fix', 'scaffold descent driver', 'immutable search nodes', 'terrain waypoints', 'terrain start', 'terrain stops'])
})

test('the real repo files are patched', () => {
  const root = join(import.meta.dirname, '..', '..')
  const read = p => { try { return readFileSync(join(root, p), 'utf8') } catch { return null } }
  assert.deepEqual(missingPatches(read), [])
})

test('only a required patch is named by missingRequired', () => {
  const without = title => path => PATCHES.filter(([p, , t]) => p === path && t !== title).map(([, marker]) => marker).join('\n')
  assert.deepEqual(missingRequired(without('gate fix')), [])
  assert.deepEqual(missingRequired(without('trapdoor over a ladder')), ['trapdoor over a ladder'])
})

test('every required patch is a known title', () => {
  assert.deepEqual(REQUIRED.filter(t => !PATCHES.some(([, , title]) => title === t)), [])
})

test('every patch tools/patch-deps.mjs applies has a marker here', () => {
  const root = join(import.meta.dirname, '..', '..')
  const applied = readFileSync(join(root, 'tools', 'patch-deps.mjs'), 'utf8').match(/^ {2}\['node_modules\//gm)
  assert.equal(PATCHES.length, applied.length)
})
