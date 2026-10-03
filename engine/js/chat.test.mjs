import { test } from 'node:test'
import assert from 'node:assert/strict'
import { stubBot } from './stub-bot.mjs'
import { chatRefusal, say, cleanMessage } from './chat.mjs'

const OPTS = () => ({ timeScale: 0.01 })
const ctx = () => ({ alive: () => {}, onAbort: () => {} })

const chatBot = ({ players = { Steve: {} }, refuse = null, position = 'system' } = {}) => {
  const bot = stubBot()
  bot.sent = []
  bot.players = players
  const send = (kind, args) => {
    bot.sent.push({ kind, args })
    if (refuse) bot.emit('messagestr', refuse, position)
  }
  bot.chat = (...args) => send('chat', args)
  bot.whisper = (...args) => send('whisper', args)
  return bot
}

test('chatRefusal cases', () => {
  const cases = [
    ['Command had invalid signature x', 'Command had invalid signature x'],
    ['No player was found', 'No player was found'],
    ['Unknown or incomplete command, see below', 'Unknown or incomplete command, see below'],
    ['An unexpected error occurred trying to execute that command', 'An unexpected error occurred trying to execute that command'],
    ['That player cannot be found', 'That player cannot be found'],
    ['that player cannot be found', 'that player cannot be found'],
    ['hello No player was found', null],
    ['fine', null]
  ]
  for (const [text, want] of cases) assert.equal(chatRefusal(text), want, text)
})

test('say to all, one part', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: '  hi all ' }, OPTS()), { status: 'sent', parts: 1 })
  assert.deepEqual(bot.sent, [{ kind: 'chat', args: ['hi all'] }])
})

test('whisper to an online player', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Steve' }, OPTS()), { status: 'sent', parts: 1, to: 'Steve' })
  assert.deepEqual(bot.sent, [{ kind: 'whisper', args: ['Steve', 'psst'] }])
})

test('whisper to an offline player is gone and sends nothing', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Alex' }, OPTS()), { status: 'gone', to: 'Alex' })
  assert.deepEqual(bot.sent, [])
})

test('a refusal line gives failed', async () => {
  for (const position of ['system', 'game_info']) {
    const bot = chatBot({ refuse: 'No player was found', position })
    const r = await say(bot, ctx(), { message: 'hi', to: 'Steve' }, OPTS())
    assert.deepEqual(r, { status: 'failed', reason: 'No player was found', parts: 1 }, position)
  }
})

test('a chat-position line matching the pattern is ignored', async () => {
  const bot = chatBot({ refuse: 'No player was found', position: 'chat' })
  assert.equal((await say(bot, ctx(), { message: 'hi' }, OPTS())).status, 'sent')
})

test('the listener is removed afterwards', async () => {
  const bot = chatBot({ refuse: 'No player was found' })
  const before = bot.listenerCount('messagestr')
  await say(bot, ctx(), { message: 'hi' }, OPTS())
  assert.equal(bot.listenerCount('messagestr'), before)
})

test('a cut rejects and removes the listener', async () => {
  const bot = chatBot()
  const before = bot.listenerCount('messagestr')
  const cut = { alive: () => { throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(say(bot, cut, { message: 'hi' }, OPTS()), /cut/)
  assert.equal(bot.listenerCount('messagestr'), before)
})

test('cleanMessage strips control characters and the section sign', () => {
  const cases = [['a\nb', 'a b'], ['  hi\x00there\x7f ', 'hi there'], ['§cred', 'cred'], ['\n\n', ''], ['/say x', '/say x']]
  for (const [text, want] of cases) assert.equal(cleanMessage(text), want, JSON.stringify(text))
})

test('a message that starts with a slash is a command and is never sent', async () => {
  for (const [message, to] of [['/op me', undefined], ['/op me', 'Steve'], ['\n/stop', undefined], ['§/stop', 'Steve']]) {
    const bot = chatBot()
    assert.deepEqual(await say(bot, ctx(), { message, to }, OPTS()), { status: 'cannot', reason: 'command' }, message)
    assert.deepEqual(bot.sent, [])
  }
})

test('newlines in the message are sent as spaces', async () => {
  const bot = chatBot()
  await say(bot, ctx(), { message: 'a\n/b' }, OPTS())
  assert.deepEqual(bot.sent, [{ kind: 'chat', args: ['a /b'] }])
})

test('say refuses bad whisper targets and prototype names', async () => {
  const cases = [
    ['constructor', { status: 'gone', to: 'constructor' }],
    ['toString', { status: 'gone', to: 'toString' }],
    ['__proto__', { status: 'gone', to: '__proto__' }],
    ['@a', { status: 'cannot', reason: 'bad-name' }],
    ['Name extra', { status: 'cannot', reason: 'bad-name' }],
    ['ab', { status: 'cannot', reason: 'bad-name' }]
  ]
  for (const [to, expected] of cases) {
    const bot = chatBot()
    assert.deepEqual(await say(bot, ctx(), { message: 'hi', to }, OPTS()), expected, to)
    assert.deepEqual(bot.sent, [], to)
  }
})

