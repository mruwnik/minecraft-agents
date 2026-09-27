import { test } from 'node:test'
import assert from 'node:assert/strict'
import { compatibleInventoryStacks, inventoryCompactPair } from '../src/inventory/compact.mjs'

function stack (slot, count, overrides = {}) {
  return {
    name: 'bread', slot, count, stackSize: 64, type: 297, metadata: 0,
    nbt: null, componentMap: [], removedComponents: [], ...overrides
  }
}

test('inventory compaction preserves 68 fragmented bread and terminates at 64 plus 4', () => {
  const items = [stack(9, 13), stack(10, 31), ...Array.from({ length: 24 }, (_, i) => stack(11 + i, 1))]
  assert.equal(items.reduce((sum, item) => sum + item.count, 0), 68)
  let moves = 0
  while (true) {
    const pair = inventoryCompactPair(items, 'bread')
    if (!pair) break
    assert.ok(pair.moved > 0)
    const source = items.find(item => item.slot === pair.source)
    const destination = items.find(item => item.slot === pair.destination)
    assert.ok(source && destination)
    assert.ok(compatibleInventoryStacks(source, destination))
    source.count -= pair.moved
    destination.count += pair.moved
    if (source.count === 0) items.splice(items.indexOf(source), 1)
    moves++
    assert.ok(moves <= 26, 'the plan must make progress and terminate within the initial stack count')
  }
  assert.equal(moves, 25)
  assert.deepEqual(items.map(item => item.count).sort((a, b) => b - a), [64, 4])
  assert.equal(items.reduce((sum, item) => sum + item.count, 0), 68)
})

test('inventory compaction keeps stacks separate when NBT, metadata, or modern components differ', () => {
  const base = stack(1, 4)
  const differentNbt = stack(2, 3, { nbt: { display: { Name: 'custom bread' } } })
  const differentComponents = stack(3, 2, { componentMap: [{ type: 'custom_name', value: 'custom bread' }] })
  const differentRemoved = stack(4, 1, { removedComponents: ['food'] })
  const differentMetadata = stack(5, 1, { metadata: 1 })
  for (const other of [differentNbt, differentComponents, differentRemoved, differentMetadata]) {
    assert.equal(compatibleInventoryStacks(base, other), false)
    assert.equal(inventoryCompactPair([base, other], 'bread'), null)
  }
})

test('inventory compaction ignores full and unstackable items and stops at 64 plus 4', () => {
  const full = stack(1, 64)
  const remainder = stack(2, 4)
  const unstackable = stack(3, 1, { stackSize: 1 })
  assert.equal(inventoryCompactPair([full, remainder, unstackable], 'bread'), null)
  assert.equal(inventoryCompactPair([unstackable], 'bread'), null)
  assert.equal(full.count + remainder.count + unstackable.count, 69)
})
