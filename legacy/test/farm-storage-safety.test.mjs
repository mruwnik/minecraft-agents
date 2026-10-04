import test from 'node:test'
import assert from 'node:assert/strict'
import { storeSurplus } from '../src/storage.mjs'
import { assertFarmRecoverable } from '../src/farm/attention.mjs'

for (const stage of ['lookup', 'deposit']) {
  for (const error of [new Error('cancelled'), new TypeError('broken storage adapter')]) {
    test(`farm storage preserves ${error.message} during ${stage}`, async () => {
      const calls = []
      const api = {
        inv: () => ({ wheat: 12 }),
        block: () => ({ name: 'chest' }),
        act: async action => { calls.push(action); throw error }
      }
      const target = stage === 'lookup' ? { kind: 'place', name: 'barn', x: 0, y: 64, z: 0 } : { kind: 'cell', x: 0, y: 64, z: 0 }
      await assert.rejects(storeSurplus(api, { target, surplus: { wheat: 12 }, checkError: assertFarmRecoverable }), caught => caught === error)
      assert.deepEqual(calls, [stage === 'lookup' ? 'goto' : 'deposit'])
    })
  }
}

test('farm storage still reports a full chest without changing destinations', async () => {
  const calls = []
  const api = {
    inv: () => ({ wheat: 12 }),
    block: () => ({ name: 'chest' }),
    act: async (action, args) => { calls.push({ action, args }); throw new Error('the CHEST is full') }
  }
  const result = await storeSurplus(api, { target: { kind: 'cell', x: 0, y: 64, z: 0 }, surplus: { wheat: 12 }, checkError: assertFarmRecoverable })
  assert.equal(result.storage_full, 'wheat:12 carried')
  assert.equal(calls.length, 1)
  assert.equal(calls[0].action, 'deposit')
})
