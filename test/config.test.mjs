// A body's identity comes from the config.json in its home folder. Without one, bot.mjs used to fall back to the
// username Claude on port 3777, so a `node src/bot.mjs` typed in the repo root logged in as Claude and kicked the
// real Claude body (duplicate_login, 2026-09-24 17:57Z). Now a missing config refuses to start and says where to run.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { readConfig, missingConfig, DEFAULTS } from '../src/config.mjs'

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), 'config-'))

test('a home without config.json refuses to start and names the folder', () => {
  const home = tmp()
  assert.throws(() => readConfig(home), { message: missingConfig(home) })
})

test('the refusal tells an agent where to run and how to make a body', () => {
  const text = missingConfig('/somewhere')
  assert.match(text, /\/somewhere\/config\.json/)
  assert.match(text, /state\/agents\/<Name>/)
  assert.match(text, /tools\/new-agent\.mjs/)
})

const merges = [
  ['only a username', { username: 'Steve' }, { ...DEFAULTS, username: 'Steve' }],
  ['a username and a port', { username: 'Steve', apiPort: 3790 }, { ...DEFAULTS, username: 'Steve', apiPort: 3790 }],
  ['extra keys pass through', { username: 'Steve', harness: 'claude-code' }, { ...DEFAULTS, username: 'Steve', harness: 'claude-code' }]
]
for (const [name, written, expected] of merges) {
  test(`config.json with ${name} fills in the defaults`, () => {
    const home = tmp()
    fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify(written))
    assert.deepEqual(readConfig(home), expected)
  })
}

test('a config.json without a username is refused too', () => {
  const home = tmp()
  fs.writeFileSync(path.join(home, 'config.json'), '{"apiPort": 3790}')
  assert.throws(() => readConfig(home), /username/)
})

test('the defaults carry no username', () => {
  assert.equal('username' in DEFAULTS, false)
})
