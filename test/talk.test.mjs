// Talking to one person: a chat that opens with someone's name goes to everyone, a whisper only to them (and it wakes
// their wait). Today's tally was 38 chats from agents against 5 whispers (720224ff), so `chat` reads its own text.
import test from 'node:test'
import assert from 'node:assert/strict'
import { addressedTo, whisperHint, offlineWhisper } from '../src/talk.mjs'

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
  ['a name that is mentioned but does not open the message', 'the bed is yours, Chani', null],
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
