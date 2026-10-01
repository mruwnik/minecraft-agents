// The one-time Microsoft sign-in (tools/login.mjs) and the body (src/bot.mjs) share these decisions. They never talk
// to Microsoft, so they are tested here; the live sign-in is checked by hand on the server.
import test from 'node:test'
import assert from 'node:assert/strict'
import prismarineAuth from 'prismarine-auth'
import { authDir, profileFile, profileMismatch, loginAdvice, AUTHFLOW_OPTIONS } from '../src/auth.mjs'

const { Titles } = prismarineAuth

test('the token cache and the sign-in marker live under the agent folder', () => {
  assert.equal(authDir('/agents/Zed'), '/agents/Zed/auth')
  assert.equal(profileFile('/agents/Zed'), '/agents/Zed/auth/profile.json')
})

// minecraft-protocol builds its Authflow with exactly these (src/client/microsoftAuth.js validateOptions); the login
// tool must match or the body cannot read the cache the tool wrote
test('the login tool signs in the way minecraft-protocol does', () => {
  assert.deepEqual(AUTHFLOW_OPTIONS, { authTitle: Titles.MinecraftNintendoSwitch, deviceType: 'Nintendo', flow: 'live' })
})

const matches = [
  ['the same name', { name: 'AmethystFan7865' }, 'AmethystFan7865'],
  ['the same name in another case', { name: 'amethystfan7865' }, 'AmethystFan7865']
]
for (const [what, profile, username] of matches) {
  test(`a profile with ${what} is this agent`, () => {
    assert.equal(profileMismatch(profile, username), null)
  })
}

const mismatches = [
  ['another name', { name: 'mruwnik' }, /signed in as mruwnik.*AmethystFan7865/],
  ['no profile', null, /no profile.*AmethystFan7865/],
  ['a profile without a name', {}, /no profile.*AmethystFan7865/]
]
for (const [what, profile, message] of mismatches) {
  test(`a profile with ${what} is refused`, () => {
    assert.match(profileMismatch(profile, 'AmethystFan7865'), message)
  })
}

test('the advice names the login tool and this agent', () => {
  assert.match(loginAdvice('/x/state/agents/AmethystFan7865'), /node tools\/login\.mjs AmethystFan7865/)
})
