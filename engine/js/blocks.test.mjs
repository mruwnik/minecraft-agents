// Why JavaScript: node --test file for engine/js/blocks.mjs (Mineflayer-side predicates).
import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { isInteractable } from './blocks.mjs'

const source = readFileSync(new URL('../src/jobs/lib/interactable_blocks.txt', import.meta.url), 'utf8').trim()
const table = new RegExp(source)

test('isInteractable follows the shared table in jobs/lib/interactable_blocks.txt', () => {
  for (const name of ['chest', 'oak_door', 'iron_trapdoor', 'red_bed', 'stone_button', 'shulker_box', 'blue_shulker_box',
    'lever', 'stone', 'oak_planks', 'oak_log', 'air', 'oak_fence', 'torch']) {
    assert.equal(isInteractable(name), table.test(name), name)
  }
})
