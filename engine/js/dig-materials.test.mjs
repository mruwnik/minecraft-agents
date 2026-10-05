// Why JavaScript: tests dig-materials.mjs, a patch of Mineflayer's minecraft-data registry (the Mineflayer boundary).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import registryFor from 'prismarine-registry'
import blockFor from 'prismarine-block'
import { fixDigMaterials } from './dig-materials.mjs'

const digMs = (registry, blockName, toolName) => {
  const Block = blockFor(registry)
  const block = Block.fromStateId(registry.blocksByName[blockName].defaultState, 0)
  return block.digTime(toolName ? registry.itemsByName[toolName].id : null, false, false, false, [], {})
}

test('obsidian, ores and copper dig at the speed of the tool, not at hand speed', () => {
  const registry = registryFor('1.21.8')
  fixDigMaterials(registry)
  assert.equal(Math.round(digMs(registry, 'obsidian', 'diamond_pickaxe') / 100), 94)
  assert.equal(digMs(registry, 'iron_ore', 'iron_pickaxe'), 750)
  assert.equal(digMs(registry, 'copper_block', 'stone_pickaxe'), 1150)
})

test('a block no tool harvests keeps the slow hand time', () => {
  const registry = registryFor('1.21.8')
  fixDigMaterials(registry)
  assert.equal(digMs(registry, 'obsidian', null), 250000)
})

test('fixing twice changes nothing more', () => {
  const registry = registryFor('1.21.8')
  fixDigMaterials(registry)
  fixDigMaterials(registry)
  assert.equal(Math.round(digMs(registry, 'obsidian', 'diamond_pickaxe') / 100), 94)
})
