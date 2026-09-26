// A primitive with a required argument left out used to run against undefined and answer with it ("unknown block
// name: undefined" for find_blocks name=oak_log, card c9 keeper): the request handler now names the argument first,
// and takes the other spelling a driver reaches for (find_blocks name= is block=).
import test from 'node:test'
import assert from 'node:assert/strict'
import { neededArgs, NEEDS } from '../src/needs.mjs'

for (const [name, action, given, expected] of [
  ['find_blocks with block=', 'find_blocks', { block: 'oak_log', count: 3 }, { args: { block: 'oak_log', count: 3 }, error: null }],
  ['find_blocks name= is taken as block=', 'find_blocks', { name: 'oak_log' }, { args: { block: 'oak_log' }, error: null }],
  ['find_blocks with both: block= wins, name= dropped', 'find_blocks', { block: 'stone', name: 'oak_log' }, { args: { block: 'stone' }, error: null }],
  ['find_blocks with neither', 'find_blocks', { count: 3 }, { args: { count: 3 }, error: 'find_blocks needs block= (name= is taken as block= too)' }],
  ['craft with no item', 'craft', {}, { args: {}, error: 'craft needs item=' }],
  ['craft with item', 'craft', { item: 'stone_hoe' }, { args: { item: 'stone_hoe' }, error: null }],
  ['give needs both, the first missing is named', 'give', { item: 'bread' }, { args: { item: 'bread' }, error: 'give needs player=' }],
  ['give with neither names both', 'give', {}, { args: {}, error: 'give needs player= item=' }],
  ['a point is one argument', 'fill', { x: 1 }, { args: { x: 1 }, error: 'fill needs x= y= z=' }],
  ['a point given', 'fill', { x: 1, y: 2, z: 3 }, { args: { x: 1, y: 2, z: 3 }, error: null }],
  ['an action with no needs passes through', 'goto', { x: 1 }, { args: { x: 1 }, error: null }],
  ['an unknown action passes through', 'nope', { a: 1 }, { args: { a: 1 }, error: null }],
  ['null counts as missing', 'craft', { item: null }, { args: { item: null }, error: 'craft needs item=' }],
  ['zero and empty string count as given', 'trade', { offer: 0 }, { args: { offer: 0 }, error: null }]
]) {
  test(`neededArgs: ${name}`, () => {
    assert.deepEqual(neededArgs(action, given), expected)
  })
}

test('neededArgs: the given object is not changed', () => {
  const given = { name: 'oak_log' }
  neededArgs('find_blocks', given)
  assert.deepEqual(given, { name: 'oak_log' })
})

test('NEEDS: every alias names a key of its own action', () => {
  const stray = Object.entries(NEEDS).flatMap(([action, needs]) =>
    Object.entries(needs).flatMap(([key, aliases]) => aliases.filter(alias => alias in needs).map(alias => `${action} ${key}<-${alias}`)))
  assert.deepEqual(stray, [])
})
