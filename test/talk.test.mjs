// Talking to one person: a chat that opens with someone's name goes to everyone, a whisper only to them (and it wakes
// their wait). Today's tally was 38 chats from agents against 5 whispers (720224ff), so `chat` reads its own text.
import test from 'node:test'
import assert from 'node:assert/strict'
import { addressedTo, whisperHint, offlineWhisper, splitSay, sayLimit } from '../src/talk.mjs'

const online = ['Chani', 'Perrin', 'Steve']

const openings = [
  ['a name and a colon', 'Chani: of course, use the bed at 109,72,-63', 'Chani'],
  ['a name and a comma', 'Perrin, the cows are yours', 'Perrin'],
  ['an at-sign', '@Steve heading over now', 'Steve'],
  ['the name in another case', 'chani: done', 'Chani'],
  ['leading spaces', '  Chani: done', 'Chani'],
  ['a greeting to everyone', 'hello all, cane is ready', null],
  ['all with a colon', 'all: the bed by the hut is free', null],
  ['everyone with a comma', 'Everyone, sleep now please', null],
  ['a name nobody online has', 'Miles: are you there?', null],
  ['a name at the end after a comma', 'the bed is yours, Chani', 'Chani'],
  ['a greeting with the name at the end', 'morning, Chani', 'Chani'],
  ['a trailing name with punctuation', 'thanks, Perrin!', 'Perrin'],
  ['a trailing name in another case', 'on my way, perrin', 'Perrin'],
  ['a trailing name without a comma is just a word', 'morning Chani!', null],
  ['a name mentioned mid-sentence', 'I walked the sheep home with Chani today', null],
  ['a trailing crowd word', 'good night, everyone', null],
  ['a trailing name nobody online has', 'see you, Miles', null],
  ['a name followed by other words', 'Chani has the carrots', null],
  ['a name that is only a prefix of a longer word', 'Chanice: hi', null],
  ['an empty message', '', null],
  ['no players online', 'Chani: done', null, []]
]
for (const [what, text, expected, players = online] of openings) {
  test(`addressedTo: ${what}`, () => { assert.equal(addressedTo(text, players), expected) })
}

test('whisperHint: names the player and the whisper to use, in words that fit anyone', () => {
  assert.equal(whisperHint('Chani'), 'this read as a message to Chani: whisper player=Chani next time, only they see it and it wakes their wait')
})

test('offlineWhisper: null when the player is online', () => {
  assert.equal(offlineWhisper('Chani', online), null)
})

test('offlineWhisper: an error naming who is online when they are not', () => {
  assert.equal(offlineWhisper('Miles', online), 'Miles is not online, so a whisper would go nowhere. Online now: Chani, Perrin, Steve')
})

test('offlineWhisper: with nobody online, says so', () => {
  assert.equal(offlineWhisper('Miles', []), 'Miles is not online, so a whisper would go nowhere. Nobody else is online')
})

test('offlineWhisper: the name is matched in any case, and the error says the spelling that is online', () => {
  assert.equal(offlineWhisper('chani', online), null)
})

// the server takes 256 characters per line and a whisper spends some on its "/tell <name> " header; a long message
// used to be cut off silently at a fixed length (the human, 17:50Z: "whisper seems to have a length limit"), so a
// long text now goes out in numbered pieces, each cut at a sentence or word end
const long = 'The cows are penned at last, all four of them, and the gate is shut. I left two leads in the farm chest as promised; the third one snapped on the fence post. Tomorrow I will till the row beside the pond, unless it rains, and then I will plant the carrots you asked for.'
const splits = [
  ['a short text is one piece, untouched', 'hello all', 50, ['hello all']],
  ['a text exactly at the limit is one piece', 'x'.repeat(50), 50, ['x'.repeat(50)]],
  ['a long text is cut at sentence ends and every piece is numbered', long, 120, [
    '(1/3) The cows are penned at last, all four of them, and the gate is shut.',
    '(2/3) I left two leads in the farm chest as promised; the third one snapped on the fence post.',
    '(3/3) Tomorrow I will till the row beside the pond, unless it rains, and then I will plant the carrots you asked for.'
  ]],
  ['without a sentence end in reach, the cut falls on a word end', 'one two three four five six seven eight nine ten', 20, [
    '(1/4) one two three', '(2/4) four five six', '(3/4) seven eight', '(4/4) nine ten'
  ]],
  ['a single word longer than the limit is cut hard', 'abcdefghijklmnopqrstuvwxyz', 16, ['(1/3) abcdefghij', '(2/3) klmnopqrst', '(3/3) uvwxyz']],
  ['blank and undefined texts are nothing to say', '   ', 50, []]
]
for (const [name, text, max, expected] of splits) {
  test(`splitSay: ${name}`, () => assert.deepEqual(splitSay(text, max), expected))
}

test('splitSay: every piece fits the limit, with its number', () => {
  const pieces = splitSay(long, 60)
  assert.deepEqual(pieces.map(p => p.length <= 60), pieces.map(() => true))
  assert.equal(pieces.map(p => p.replace(/^\(\d+\/\d+\) /, '')).join(' '), long)
})

test('sayLimit: chat has the whole line, a whisper pays for its header', () => {
  assert.equal(sayLimit(), 256)
  assert.equal(sayLimit('Chani'), 256 - '/tell Chani '.length)
})
