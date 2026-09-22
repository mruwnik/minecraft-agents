import test from 'node:test'
import assert from 'node:assert/strict'
import { encodePacket, decodePacket, whitelistCommand } from '../tools/rcon.mjs'

test('encodePacket: length, id, type, body and two terminating zeros, little endian', () => {
  assert.deepEqual([...encodePacket(7, 2, 'hi')], [12, 0, 0, 0, 7, 0, 0, 0, 2, 0, 0, 0, 104, 105, 0, 0])
})

test('decodePacket: round trip, and tells how many bytes it used', () => {
  const buf = Buffer.concat([encodePacket(3, 0, 'Added Aviendha to the whitelist'), Buffer.from([9, 9])])
  assert.deepEqual(decodePacket(buf), { id: 3, type: 0, body: 'Added Aviendha to the whitelist', size: buf.length - 2 })
})

test('decodePacket: incomplete data gives null', () => {
  assert.equal(decodePacket(encodePacket(1, 2, 'list').subarray(0, 9)), null)
})

const badNames = ['', 'ab', 'x y', 'Avi\nstop', 'Avi;op Claude', 'a'.repeat(17), 'remove mruwnik', '../x']
for (const name of badNames) {
  test(`whitelistCommand refuses ${JSON.stringify(name)}`, () => assert.throws(() => whitelistCommand(name)))
}

test('whitelistCommand: the only command this tool can ever send', () => {
  assert.equal(whitelistCommand('Aviendha'), 'whitelist add Aviendha')
})
