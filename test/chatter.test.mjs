// Chattiness: messageWeight (how much a line asks for an answer), hears (allow/deny/threshold), the skipped-count
// line, and the haiku grader's cache. The rules-based half is canonically in src/cli.mjs (tools/mc.mjs and cli.mjs
// itself may import none of our other files, #148); this module re-exports that same function, so these tests also
// stand in for chatter.mjs's public API.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import {
  messageWeight, hears, skippedLine, isGreeting, isQuestion, isDanger, namesMe,
  cacheKey, readCache, appendCache, haikuGrade, haikuPrompt, gradeWeight
} from '../src/chatter.mjs'

// ---------------------------------------------------------------- the little text classifiers
const greetingRows = [
  ['a bare greeting', 'morning', true],
  ['two words the spec names together', 'good morning', true],
  ['thanks with punctuation', 'thanks!', true],
  ['an ack', 'ok', true],
  ['case and trailing punctuation do not matter', 'OK.', true],
  ['not a greeting', 'where are you', false],
  ['a greeting word used mid-sentence is not a bare greeting', 'thanks for the wheat, could you spare more', false]
]
for (const [name, message, expected] of greetingRows) {
  test(`isGreeting: ${name}`, () => assert.equal(isGreeting(message), expected))
}

const questionRows = [
  ['ends with a question mark', 'are you home?', true],
  ['starts with a question word', 'where is the chest', true],
  ['anyone works too', 'anyone seen my sword', true],
  ['a statement', 'I found the chest', false]
]
for (const [name, message, expected] of questionRows) {
  test(`isQuestion: ${name}`, () => assert.equal(isQuestion(message), expected))
}

const dangerRows = [
  ['help, bare', 'help', true],
  ['a creeper mention', 'creeper behind you!', true],
  ['stuck', 'I am stuck in a hole', true],
  ['a word that only contains a danger word is not a match', 'unstuck the cart', false],
  ['nothing dangerous', 'nice house', false]
]
for (const [name, message, expected] of dangerRows) {
  test(`isDanger: ${name}`, () => assert.equal(isDanger(message), expected))
}

const namesMeRows = [
  ['names me, whole word', 'Jizo, are you there', 'Jizo', true],
  ['a different name', 'Chani, are you there', 'Jizo', false],
  ['my name inside another word does not count', 'Jizodile is a fun name', 'Jizo', false]
]
for (const [name, message, me, expected] of namesMeRows) {
  test(`namesMe: ${name}`, () => assert.equal(namesMe(message, me), expected))
}

// ---------------------------------------------------------------- messageWeight
const weightRows = [
  ['a whisper to me tops out at 1 regardless of content', { from: 'Steve', message: 'hi', to: 'Jizo', me: 'Jizo' }, 1],
  ['a greeting in open chat is the quiet floor', { from: 'Chani', message: 'good morning', me: 'Jizo' }, 0.1],
  ['a plain remark is the default', { from: 'Chani', message: 'the wheat is ripe', me: 'Jizo' }, 0.3],
  ['a question raises it', { from: 'Chani', message: 'where is the chest?', me: 'Jizo' }, 0.6],
  ['a danger word raises it further', { from: 'Chani', message: 'creeper!', me: 'Jizo' }, 0.7],
  ['naming me raises it more', { from: 'Chani', message: 'Jizo, catch', me: 'Jizo' }, 0.9],
  ['a question that also names me takes the higher of the two', { from: 'Chani', message: 'Jizo, are you there?', me: 'Jizo' }, 0.9],
  ['a human sender is at least 0.8 even with plain text', { from: 'Steve', message: 'the wheat is ripe', me: 'Jizo', humans: ['Steve'] }, 0.8],
  ['a human sender does not get pulled DOWN by a greeting', { from: 'Steve', message: 'morning', me: 'Jizo', humans: ['Steve'] }, 0.8],
  ['a human who also names me keeps the higher weight', { from: 'Steve', message: 'Jizo, help', me: 'Jizo', humans: ['Steve'] }, 0.9],
  ['an agent sender (not in humans) gets no human bonus', { from: 'Chani', message: 'the wheat is ripe', me: 'Jizo', humans: ['Steve'] }, 0.3]
]
for (const [name, input, expected] of weightRows) {
  test(`messageWeight: ${name}`, () => assert.equal(messageWeight(input), expected))
}

// ---------------------------------------------------------------- hears: allow/deny/threshold
const hearsRows = [
  ['chattiness 1 hears everything, even a greeting', { type: 'chat', from: 'Chani', message: 'morning' }, { chattiness: 1 }, []],
  ['chattiness 0 hears nothing but a whisper', { type: 'chat', from: 'Chani', message: 'creeper!' }, { chattiness: 0 }, []],
  ['chattiness 0 still hears a whisper', { type: 'whisper', from: 'Chani', message: 'hi' }, { chattiness: 0 }, []],
  ['chattiness 0.5 hears a question', { type: 'chat', from: 'Chani', message: 'where are you?' }, { chattiness: 0.5 }, []],
  ['chattiness 0.5 does not hear a greeting', { type: 'chat', from: 'Chani', message: 'good morning' }, { chattiness: 0.5 }, []],
  ['chattiness 0.5 hears a plain human', { type: 'chat', from: 'Steve', message: 'the wheat is ripe' }, { chattiness: 0.5 }, ['Steve']],
  ['deny wins over everything, even a whisper', { type: 'whisper', from: 'Chani', message: 'help' }, { chattiness: 1, deny: ['Chani'] }, []],
  ['a non-empty allow list is the only door in', { type: 'chat', from: 'Chani', message: 'creeper!' }, { chattiness: 1, allow: ['Steve'] }, []],
  ['allow admits the named sender', { type: 'chat', from: 'Steve', message: 'hi' }, { chattiness: 1, allow: ['Steve'] }, []]
]
const hearsExpected = [true, false, true, true, false, true, false, false, true]
hearsRows.forEach(([name, event, chat, humans], i) =>
  test(`hears: ${name}`, () => assert.equal(hears({ event, chat, me: 'Jizo', humans }), hearsExpected[i])))

// ---------------------------------------------------------------- skippedLine
test('skippedLine: names the count, the chattiness and how to read the rest', () =>
  assert.equal(skippedLine(3, 0.5), 'skipped 3 chat lines below your chattiness (0.5): ./mc events type=chat n=3'))
test('skippedLine: one line is singular', () =>
  assert.equal(skippedLine(1, 0.4), 'skipped 1 chat line below your chattiness (0.4): ./mc events type=chat n=1'))

// ---------------------------------------------------------------- the haiku cache: same key twice, one grade
const tmpCache = () => path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'chatweights-')), 'chat-weights.jsonl')

test('cacheKey: from, t and message make the key, so two agents reading the same line agree', () =>
  assert.equal(cacheKey({ from: 'Chani', t: '2026-09-26T18:00:00Z', message: 'hi' }), 'Chani\u00002026-09-26T18:00:00Z\u0000hi'))

test('readCache: no file yet is an empty map', () => assert.equal(readCache(path.join(os.tmpdir(), 'never-written.jsonl')).size, 0))

test('appendCache then readCache: the weight comes back under its key', () => {
  const file = tmpCache()
  appendCache('a-key', 0.7, file)
  assert.equal(readCache(file).get('a-key'), 0.7)
})

test('haikuGrade: a stubbed run that answers a bare number is read as the weight', () =>
  assert.equal(haikuGrade('prompt', { run: () => '0.8\n' }), 0.8))
test('haikuGrade: a reply that is not a number falls back to null', () =>
  assert.equal(haikuGrade('prompt', { run: () => 'sure!' }), null))
test('haikuGrade: a run that throws (error or timeout) falls back to null', () =>
  assert.equal(haikuGrade('prompt', { run: () => { throw new Error('timed out') } }), null))

test('haikuPrompt: three lines naming who said what, asking for a bare number', () =>
  assert.equal(haikuPrompt({ from: 'Chani', message: 'where are you?' }, 'Jizo'),
    'On a scale from 0 to 1, how much does this message ask Jizo to answer?\nChani: "where are you?"\nReply with only the number, nothing else.'))

// ---------------------------------------------------------------- gradeWeight: dispatches rules vs haiku, and caches
test('gradeWeight: grader "rules" (or missing) never shells out, uses messageWeight', () => {
  const event = { type: 'chat', from: 'Chani', message: 'where are you?', t: '2026-09-26T18:00:00Z' }
  assert.equal(gradeWeight({ event, chat: {}, me: 'Jizo' }), 0.6)
})

test('gradeWeight: grader "haiku" uses the stubbed run and caches the result', () => {
  const file = tmpCache()
  const event = { type: 'chat', from: 'Chani', message: 'where are you?', t: '2026-09-26T18:00:00Z' }
  const calls = []
  const run = (...args) => { calls.push(args); return '0.9' }
  const first = gradeWeight({ event, chat: { grader: 'haiku' }, me: 'Jizo', run, cacheFile: file })
  const second = gradeWeight({ event, chat: { grader: 'haiku' }, me: 'Jizo', run, cacheFile: file })
  assert.equal(first, 0.9)
  assert.equal(second, 0.9)
  assert.equal(calls.length, 1, 'the second grade of the same line must come from the cache, not a second shell-out')
})

test('gradeWeight: grader "haiku" falls back to the rules weight when the model gives no usable number', () => {
  const file = tmpCache()
  const event = { type: 'chat', from: 'Chani', message: 'where are you?', t: '2026-09-26T18:00:00Z' }
  assert.equal(gradeWeight({ event, chat: { grader: 'haiku' }, me: 'Jizo', run: () => { throw new Error('down') }, cacheFile: file }), 0.6)
})
