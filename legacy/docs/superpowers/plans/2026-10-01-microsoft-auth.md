# Microsoft Login Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let one body log in to an online-mode Paper server with a real Microsoft account, signed in once by a human through a device code.

**Architecture:** `config.json` gains `auth: "offline" | "microsoft"`. A new `tools/login.mjs` does the one-time device-code sign-in with prismarine-auth and caches tokens in the agent's `auth/` folder; `src/bot.mjs` passes the auth mode and that folder to mineflayer and refuses to start a microsoft body that has not been signed in. Pure decisions live in `src/auth.mjs` so they are testable without Microsoft.

**Tech Stack:** Node 22 ESM (`.mjs`), `node --test`, mineflayer 4.39 over minecraft-protocol, prismarine-auth 3.1 (already in `node_modules`, a dependency of minecraft-protocol).

Spec: `docs/superpowers/specs/2026-10-01-microsoft-auth-design.md`.

## Global Constraints

- Tests: plain `test(...)` functions with `node:test` and `node:assert/strict`, no test classes, no `if` in tests. Run one file with `node --test test/<file>.test.mjs`, all with `npm test`.
- Imports at the top of every file. Prefer early return. Comments only say WHY.
- `cfg.username` is the account's profile name (gamertag). Never introduce a second name field for the player.
- The token cache lives at `state/agents/<Name>/auth/`, created with mode `0o700`. `state/` is already gitignored.
- The Authflow arguments in `tools/login.mjs` must equal the ones minecraft-protocol uses (`node_modules/minecraft-protocol/src/client/microsoftAuth.js`): `authTitle: Titles.MinecraftNintendoSwitch`, `deviceType: 'Nintendo'`, `flow: 'live'`, username `cfg.username`, cache dir the `auth/` folder. Otherwise the body cannot read the cache the tool wrote.
- Server for the first body: host `91.222.26.35`, port `30115`, account `AmethystFan7865`, character Breq.
- Commit after each task. Commit messages end with `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.

---

### Task 1: `auth` in config.json

**Files:**
- Modify: `src/config.mjs`
- Test: `test/config.test.mjs`

**Interfaces:**
- Produces: `readConfig(home)` returns `cfg.auth` as `'offline'` (default) or `'microsoft'`; any other value throws. `DEFAULTS.auth === 'offline'`.

- [ ] **Step 1: Write the failing tests**

Append to `test/config.test.mjs`:

```js
test('auth defaults to offline', () => {
  const home = tmp()
  fs.writeFileSync(path.join(home, 'config.json'), '{"username": "Steve"}')
  assert.equal(readConfig(home).auth, 'offline')
})

test('auth may be microsoft', () => {
  const home = tmp()
  fs.writeFileSync(path.join(home, 'config.json'), '{"username": "Steve", "auth": "microsoft"}')
  assert.equal(readConfig(home).auth, 'microsoft')
})

test('any other auth is refused and the two choices are named', () => {
  const home = tmp()
  fs.writeFileSync(path.join(home, 'config.json'), '{"username": "Steve", "auth": "mojang"}')
  assert.throws(() => readConfig(home), /auth "mojang".*offline.*microsoft/)
})
```

- [ ] **Step 2: Run the file to verify the new tests fail**

Run: `node --test test/config.test.mjs`
Expected: the first passes only by accident of `DEFAULTS` lacking `auth` (it fails: `undefined !== 'offline'`), the third fails with "Missing expected exception".

- [ ] **Step 3: Implement**

In `src/config.mjs`, change `DEFAULTS` and `readConfig`:

```js
export const DEFAULTS = {
  host: 'localhost',
  port: 25565,
  version: '26.1', // newest protocol mineflayer speaks; ViaBackwards bridges to the newer server
  apiPort: 3777,
  auth: 'offline'
}

export const AUTH_MODES = ['offline', 'microsoft']

export const readConfig = home => {
  const file = configFile(home)
  if (!fs.existsSync(file)) throw new Error(missingConfig(home))
  const cfg = { ...DEFAULTS, ...JSON.parse(fs.readFileSync(file, 'utf8')) }
  if (!cfg.username) throw new Error(`${file} names no username: ${JSON.stringify(cfg)}`)
  if (!AUTH_MODES.includes(cfg.auth)) throw new Error(`${file}: auth "${cfg.auth}" is not one of ${AUTH_MODES.join(', ')}`)
  return cfg
}
```

- [ ] **Step 4: Run the file to verify it passes**

Run: `node --test test/config.test.mjs`
Expected: all pass. The existing `merges` tests still pass because `DEFAULTS` now carries `auth` on both sides of the comparison.

- [ ] **Step 5: Commit**

```bash
git add src/config.mjs test/config.test.mjs
git commit -m "Add auth mode to a body's config

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 2: `src/auth.mjs`, the pure decisions

**Files:**
- Create: `src/auth.mjs`
- Test: `test/auth.test.mjs`

**Interfaces:**
- Produces:
  - `authDir(home: string): string` → `<home>/auth`
  - `profileFile(home: string): string` → `<home>/auth/profile.json`
  - `AUTHFLOW_OPTIONS` → `{ authTitle: Titles.MinecraftNintendoSwitch, deviceType: 'Nintendo', flow: 'live' }`
  - `profileMismatch(profile: {name?: string} | null | undefined, username: string): string | null` → a message, or null when the signed-in profile is this agent
  - `loginAdvice(home: string): string` → the sentence a refusal shows, naming `node tools/login.mjs <Name>` where `<Name>` is `path.basename(home)`

- [ ] **Step 1: Write the failing tests**

Create `test/auth.test.mjs`:

```js
// The one-time Microsoft sign-in (tools/login.mjs) and the body (src/bot.mjs) share these decisions. They never talk
// to Microsoft, so they are tested here; the live sign-in is checked by hand on the server.
import test from 'node:test'
import assert from 'node:assert/strict'
import { Titles } from 'prismarine-auth'
import { authDir, profileFile, profileMismatch, loginAdvice, AUTHFLOW_OPTIONS } from '../src/auth.mjs'

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
```

- [ ] **Step 2: Run to verify it fails**

Run: `node --test test/auth.test.mjs`
Expected: fails at import, `Cannot find module '.../src/auth.mjs'`.

- [ ] **Step 3: Implement**

Create `src/auth.mjs`:

```js
// A body on an online-mode server logs in with a real Microsoft account. The human signs in once (tools/login.mjs),
// the tokens are cached under the agent folder, and the body reads that cache. The decisions both sides share are here.
import path from 'node:path'
import { Titles } from 'prismarine-auth'

export const authDir = home => path.join(home, 'auth')
export const profileFile = home => path.join(authDir(home), 'profile.json')

// minecraft-protocol's own Authflow arguments (src/client/microsoftAuth.js): the cache is keyed by them
export const AUTHFLOW_OPTIONS = { authTitle: Titles.MinecraftNintendoSwitch, deviceType: 'Nintendo', flow: 'live' }

// the folder is named after the player and Mojang decides the player name: a sign-in under the wrong folder would
// run a body whose config, API port and clock entries belong to someone else
export const profileMismatch = (profile, username) => {
  if (!profile?.name) return `Microsoft returned no profile for ${username}: does that account own Minecraft Java?`
  if (profile.name.toLowerCase() === username.toLowerCase()) return null
  return `signed in as ${profile.name}, but this folder is ${username}: sign in with the account that owns ${username}`
}

export const loginAdvice = home => `no Microsoft sign-in for ${path.basename(home)} yet: run \`node tools/login.mjs ${path.basename(home)}\` once (from the repo root) and ./start again`
```

- [ ] **Step 4: Run to verify it passes**

Run: `node --test test/auth.test.mjs`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add src/auth.mjs test/auth.test.mjs
git commit -m "Add the shared decisions of a Microsoft sign-in

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 3: `tools/login.mjs`, the one-time sign-in

**Files:**
- Create: `tools/login.mjs`

**Interfaces:**
- Consumes: `readConfig` from `src/config.mjs`; `authDir`, `profileFile`, `profileMismatch`, `AUTHFLOW_OPTIONS` from `src/auth.mjs`.
- Produces: on success, `state/agents/<Name>/auth/profile.json` containing `{ "name", "id", "at" }` plus prismarine-auth's own cache files in the same folder.

No automated test: the tool's only logic beyond the tested helpers is calling Microsoft.

- [ ] **Step 1: Write the tool**

Create `tools/login.mjs`:

```js
// The one-time Microsoft sign-in for a body on an online-mode server:
//   node tools/login.mjs AmethystFan7865
// Prints a URL and a code; sign in there with the account that owns that player. Tokens land in
// state/agents/<Name>/auth/ and refresh themselves from then on; this is run again only when the body reports
// login_needed (a refresh token expires after months of disuse) or to switch the account.
import fs from 'node:fs'
import path from 'node:path'
import { Authflow } from 'prismarine-auth'
import { readConfig } from '../src/config.mjs'
import { authDir, profileFile, profileMismatch, AUTHFLOW_OPTIONS } from '../src/auth.mjs'

const name = process.argv[2]
if (!name) { console.error('usage: node tools/login.mjs <Name>'); process.exit(2) }
const home = path.join(import.meta.dirname, '..', 'state', 'agents', name)
const cfg = readConfig(home)
if (cfg.auth !== 'microsoft') { console.error(`${name}'s config.json says auth "${cfg.auth}": nothing to sign in to`); process.exit(2) }

const dir = authDir(home)
fs.mkdirSync(dir, { recursive: true, mode: 0o700 })
const showCode = data => console.log(`\nOpen ${data.verification_uri} and enter the code ${data.user_code}\n(signing in as the account that owns ${cfg.username})\n`)
const flow = new Authflow(cfg.username, dir, AUTHFLOW_OPTIONS, showCode)
const { profile } = await flow.getMinecraftJavaToken({ fetchProfile: true })

const mismatch = profileMismatch(profile, cfg.username)
if (mismatch) {
  fs.rmSync(dir, { recursive: true, force: true })
  console.error(mismatch)
  process.exit(1)
}
fs.writeFileSync(profileFile(home), JSON.stringify({ name: profile.name, id: profile.id, at: new Date().toISOString() }, null, 1) + '\n')
console.log(`signed in: ${profile.name} (${profile.id}); tokens cached in ${dir}`)
```

- [ ] **Step 2: Check it parses and refuses an offline body**

Run: `node tools/login.mjs Breq` (Breq's config says no auth, so it defaults to offline)
Expected: `Breq's config.json says auth "offline": nothing to sign in to`, exit code 2.

Run: `node tools/login.mjs`
Expected: the usage line, exit code 2.

- [ ] **Step 3: Commit**

```bash
git add tools/login.mjs
git commit -m "Add the one-time Microsoft sign-in tool

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 4: the body logs in with the configured auth

**Files:**
- Modify: `src/bot.mjs` (the imports near line 64, `connect()` near line 259, and the entry point at the end, `connect()` near line 3654)

**Interfaces:**
- Consumes: `cfg.auth` (Task 1); `authDir`, `profileFile`, `loginAdvice` (Task 2); `emit` from `./body/events.mjs` (already imported); `HOME` from `./body/home.mjs` (already imported).

No automated test: `src/bot.mjs` connects to a server on import. The refusal text it uses is tested in Task 2.

- [ ] **Step 1: Import the helpers**

After the line `import { ROOT, HOME, cfg } from './body/home.mjs'` add:

```js
import { authDir, profileFile, loginAdvice } from './auth.mjs'
```

- [ ] **Step 2: Pass the auth mode and cache folder to mineflayer**

In `connect()`, replace

```js
  bot = mineflayer.createBot({
    host: cfg.host, port: cfg.port, username: cfg.username, version: cfg.version, auth: 'offline'
  })
```

with

```js
  bot = mineflayer.createBot({
    host: cfg.host, port: cfg.port, username: cfg.username, version: cfg.version, auth: cfg.auth,
    ...(cfg.auth === 'microsoft' && { profilesFolder: authDir(HOME), onMsaCode: loginNeeded })
  })
```

and add, just above `function connect ()`:

```js
// a device code asked for at runtime means the cached refresh token is gone (months of disuse): a background body
// cannot show it to anyone, and the reconnect loop would ask for a new one every ten seconds, so say so and stop.
// exit 6: tools/start-body writes the body_down line for any non-zero exit
const loginNeeded = () => {
  emit('login_needed', { advice: loginAdvice(HOME) })
  process.exit(6)
}
```

- [ ] **Step 3: Refuse to start a microsoft body nobody has signed in**

At the end of the file, replace the final `connect()` with:

```js
if (cfg.auth === 'microsoft' && !fs.existsSync(profileFile(HOME))) loginNeeded()
connect()
```

- [ ] **Step 4: Check the code parses and the offline path is unchanged**

Run: `node tools/check-code.mjs . 1`
Expected: exit 0, no output (that is the parse check `./start` runs).

Run: `cd state/agents/Breq && node ../../../src/bot.mjs . & sleep 8; kill %1; tail -3 events.jsonl; cd ../../..`
Expected: with no server at `SERVER_HOST`, the log shows a connection error, not a login_needed line (Breq is offline). Delete `state/agents/Breq/events.jsonl` afterwards so the folder stays as made.

- [ ] **Step 5: Commit**

```bash
git add src/bot.mjs
git commit -m "Log a body in with the auth mode its config names

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 5: the AmethystFan7865 folder and the README

**Files:**
- Create: `state/agents/AmethystFan7865/` (gitignored: not committed)
- Delete: `state/agents/{Breq,Kvothe,Hoid,Sazed,Shevek,Essun,Tehanu,Vimes,Lyra,Genly}` (gitignored)
- Modify: `README.md:3-6`

- [ ] **Step 1: Make the folder from the tool, then rewrite its config**

```bash
rm -rf state/agents/{Breq,Kvothe,Hoid,Sazed,Shevek,Essun,Tehanu,Vimes,Lyra,Genly}
node tools/new-agent.mjs AmethystFan7865
```

Then overwrite `state/agents/AmethystFan7865/config.json` with:

```json
{
 "username": "AmethystFan7865",
 "host": "91.222.26.35",
 "port": 30115,
 "auth": "microsoft",
 "apiPort": 3777,
 "harness": "claude-code",
 "character": {
  "name": "Breq",
  "source": "Ancillary Justice (Ann Leckie)",
  "note": "a ship's AI reduced to one human body"
 },
 "chat": {
  "chattiness": 0.5,
  "allow": [],
  "deny": [],
  "grader": "rules"
 }
}
```

- [ ] **Step 2: Fix the briefing's name line**

In `state/agents/AmethystFan7865/BRIEFING.md`, the first line is `# You are AmethystFan7865`; keep it. Replace the paragraph that starts `Your name comes from chosen by hand.` so it begins:

```
Your player name is AmethystFan7865 and that is what others call you. You are Breq, from Ancillary Justice by Ann
Leckie: a ship's AI reduced to one human body. In this Minecraft world the player name is your body: a body of your own,
```

leaving the rest of that paragraph (`on a survival server shared with people...`) as it is.

- [ ] **Step 3: Check the body refuses without a sign-in**

```bash
cd state/agents/AmethystFan7865 && node ../../../src/bot.mjs . ; echo "exit $?"; tail -1 events.jsonl; cd ../../..
```

Expected: `exit 6` and a `login_needed` event whose advice says `node tools/login.mjs AmethystFan7865`. Then `rm state/agents/AmethystFan7865/events.jsonl`.

- [ ] **Step 4: README**

Replace README lines 3-6 (the paragraph starting `` `src/bot.mjs` joins the server in `..` ``) with:

```
`src/bot.mjs` joins the server named in the `config.json` of the agent folder it runs from (`state/agents/<Name>/`,
made by `node tools/new-agent.mjs <Name>`): `host`, `port` (default localhost:25565) and `auth`. `auth: "offline"`
(the default) is for an offline-mode server, where whitelisting the name is all it takes. `auth: "microsoft"` is for
an online-mode server: the name is a real account's profile name, and a human signs in once with
`node tools/login.mjs <Name>` (a device code in the browser); the body then refreshes its own tokens, and reports
`login_needed` if that ever stops working. Without a config.json the body refuses to start, so a stray run can never
log in under another agent's name.
Mineflayer speaks protocol 26.1; a server newer than that needs ViaVersion + ViaBackwards to bridge it.
```

- [ ] **Step 5: Run everything and commit**

Run: `npm test`
Expected: all pass.

```bash
git add README.md
git commit -m "Describe offline and Microsoft login in the README

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

## Done when

- `npm test` passes.
- `node tools/login.mjs AmethystFan7865` is left for the human: it needs a browser sign-in. Report that it is the next step and what it will print.
- A body started before that sign-in exits 6 with a `login_needed` event.
