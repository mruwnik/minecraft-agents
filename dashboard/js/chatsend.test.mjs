import test from 'node:test'
import assert from 'node:assert/strict'
import { tellrawCommand, validTarget, sendChat } from './chatsend.mjs'

const payload = command => JSON.parse(command.slice(command.indexOf(' ', 'tellraw '.length) + 1))

const textCases = [
  ['plain', 'hi there', '<Dan> hi there'],
  ['quotes', 'say "hi"', '<Dan> say "hi"'],
  ['backslash', 'a\\b\\', '<Dan> a\\b\\'],
  ['unicode', 'こんにちは ✓', '<Dan> こんにちは ✓'],
  ['newline', 'a\nb\r\nc', '<Dan> a b c'],
  ['section codes', '§cred§r text', '<Dan> red text'],
  ['trim', '  spaced  ', '<Dan> spaced'],
  ['limit', 'x'.repeat(300), `<Dan> ${'x'.repeat(256)}`]
]

for (const [name, text, expected] of textCases) {
  test(`tellrawCommand ${name}`, () => {
    const command = tellrawCommand({ text })
    assert.ok(command.startsWith('tellraw @a '))
    assert.deepEqual(payload(command), { text: expected })
  })
}

test('tellrawCommand target and from', () => {
  const command = tellrawCommand({ target: 'Aviendha', from: 'Bob', text: 'yo' })
  assert.ok(command.startsWith('tellraw Aviendha '))
  assert.deepEqual(payload(command), { text: '<Bob> yo' })
})

for (const text of ['', '   ', '\n', '§c', undefined]) {
  test(`tellrawCommand rejects empty ${JSON.stringify(text)}`, () => {
    assert.throws(() => tellrawCommand({ text }), /empty/)
  })
}

for (const [target, ok] of [['@a', true], ['Dan', true], ['a_b9', true], ['@e', false], ['', false], ['a b', false], ['x'.repeat(17), false], ['@a[x=1]', false], [undefined, false]]) {
  test(`validTarget ${JSON.stringify(target)}`, () => assert.equal(validTarget(target), ok))
}

test('tellrawCommand rejects bad target or from', () => {
  assert.throws(() => tellrawCommand({ target: '@e', text: 'x' }), /target/)
  assert.throws(() => tellrawCommand({ from: 'a b', text: 'x' }), /from/)
})

test('sendChat runs the command and returns the reply', async () => {
  const seen = []
  const reply = await sendChat({ text: 'hello', run: async c => { seen.push(c); return 'ok' } })
  assert.deepEqual(seen, ['tellraw @a {"text":"<Dan> hello"}'])
  assert.equal(reply, 'ok')
})

test('sendChat does not run on invalid input', async () => {
  const seen = []
  const run = async c => { seen.push(c) }
  await assert.rejects(sendChat({ text: '', run }))
  await assert.rejects(sendChat({ text: 'x', target: 'no way', run }))
  assert.deepEqual(seen, [])
})
