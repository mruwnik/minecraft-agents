# Dashboard Popup: watch by default, action log in the popup, no pano, whisper input — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Selecting a body starts watching it; the action log lives only in the agent popup; the pano option is gone; the popup has an input that whispers to the agent as an in-game whisper would.

**Architecture:** All UI is `tools/dashboard/index.html` (inline module script, tested by slicing the script from `const select = name =>` up to the chat-log banner in `test/dashboard-look.test.mjs`). The server `tools/dashboard.mjs` forwards to each body's HTTP API (`ask(port, action, args, ms)`); routes are parsed by `route()` in `tools/dashboard/lib.mjs` (tested in `test/dashboard.test.mjs`). A whisper is delivered by a new body quick action `hear` that emits the same `whisper` event `bot.on('whisper')` emits, so the body keeps owning `events.jsonl` and its seq numbers.

**Tech Stack:** Node ESM, node:test, vanilla browser JS, mineflayer body (`src/bot.mjs`).

## Global Constraints

- Work only in the worktree `/Users/dan/code/minecraft-agents/scratchpad/dashboard-popup` on branch `dashboard-popup`.
- Run tests from the worktree root: `node --test test/<file>.test.mjs`.
- TDD: failing test first. Extend existing tests rather than adding files. No conditionals in test bodies. Delete tests that only covered pano.
- Comments explain why, never what. Code is a cost: delete rather than hide.
- Same data source for the log (`/api/actions/<Name>`), same 2 s poll, newest at bottom, auto-scroll unless scrolled up.
- The whisper event is exactly `{ type: 'whisper', from, message }` as `bot.on('whisper')` emits it (`emit('whisper', { from, message })`), with `from: 'dashboard'`.
- Enter sends, input clears, the sent line shows in the action log. Empty input does nothing. No auth.
- Never write under `state/`. Never start or stop any process. Never call a real body's API with `hear`/whisper.
- Never `--no-verify`, never bare `git stash`. Commits end with `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.
- Styling: match the existing palette (CSS variables in `:root`); nothing beyond that.
- `test/gate-waypoints.test.mjs` and `test/attribute-protocol.test.mjs` fail in worktrees (node_modules by relative path); ignore them.

---

### Task 1: Remove the pano option from the dashboard

The panoramic renderer stays: `./mc look pano=true` (src/vision/eyes.mjs, src/lib/help.mjs, AGENT_GUIDE.md, README's `./mc look` section) uses it. Only the dashboard's checkbox and its plumbing go.

**Files:**
- Modify: `tools/dashboard/index.html` (checkbox at ~L196, `#lookCard.pano` CSS ~L75 and the comment above it ~L72, `startWatch` ~L831, `openLook` ~L926-928)
- Modify: `tools/dashboard.mjs` (`serveLook` ~L251, `LIVE_SIZE`/`streamLook` ~L283-290)
- Modify: `test/dashboard-look.test.mjs` (delete the two pano tests ~L170 and ~L197)
- Modify: `test/dashboard.test.mjs` (~L269 route case `/api/look/Chani/live?pano=1`)
- Modify: `README.md` (~L152: drop "with `?pano=1`")

- [ ] **Step 1: Delete the pano tests.** In `test/dashboard-look.test.mjs` delete `test('look popup: a panorama streams as a panorama', ...)` and `test('inline card: honours the pano checkbox the same way the popup does', ...)`. In `test/dashboard.test.mjs` delete the route case `['/api/look/Chani/live?pano=1', { kind: 'live', name: 'Chani' }],` (the route ignores the query anyway; the plain live case stays).

- [ ] **Step 2: Change the existing stream-URL expectations to no query string.** Every `'/api/look/<Name>/live?'` in `test/dashboard-look.test.mjs` becomes `'/api/look/<Name>/live'`. Run `node --test test/dashboard-look.test.mjs`; expect those assertions to FAIL (the page still appends `?`).

- [ ] **Step 3: Remove the UI.** In `index.html`:
  - delete `<label class="muted"><input type="checkbox" id="pano"> pano</label>`
  - delete `#lookCard.pano { width: min(96vw, 864px); }` and drop "a wider panorama picture," from the comment above `#lookCard`
  - in `startWatch`: delete the `params` line and use `watchStream = new EventSource(`/api/look/${encodeURIComponent(selected)}/live`)`
  - in `openLook`: delete `el('lookCard').className = ...` and the `params` line; use `lookStream = new EventSource(`/api/look/${encodeURIComponent(selected)}/live`)`

- [ ] **Step 4: Remove the server plumbing.** In `tools/dashboard.mjs`:

```js
const args = { file: LOOK_FILE, ...(query.get('dir') ? { dir: query.get('dir') } : {}) }
```
in `serveLook`, and in the live stream:
```js
const LIVE_SIZE = { width: 320, height: 180 }
let liveStreams = 0
const streamLook = async (req, res, name) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const file = `dashboard-live-${++liveStreams}.png`
  const args = { file, marks: true, ...LIVE_SIZE }
```
and the caller becomes `streamLook(req, res, r.name)`.

- [ ] **Step 5: README.** In the dashboard paragraph change "`/api/look/<Name>` (a PNG, with `?pano=1`; what the body saw..." to "`/api/look/<Name>` (a PNG; what the body saw...". Leave the `./mc look pano=true` lines alone.

- [ ] **Step 6: Verify.** `grep -n pano tools/ test/dashboard*.mjs` prints nothing. `node --test test/dashboard-look.test.mjs test/dashboard.test.mjs` all pass. `node tools/check-code.mjs` clean.

- [ ] **Step 7: Commit** `dashboard: remove the pano option (the panoramic renderer stays for ./mc look pano=true)`.

---

### Task 2: Watch on select; the action log moves into the popup

**Files:**
- Modify: `tools/dashboard/index.html`
- Modify: `test/dashboard-look.test.mjs`
- Modify: `README.md` (dashboard paragraph ~L144-146)

**Interfaces:**
- Produces: popup markup `#lookCard > #lookMain` (all of the popup's current children) + `#lookSide` (`<h2>actions</h2>`, `#actionsLog`). Task 3 appends its input to the end of `#lookSide`.
- `renderActions()` and `refreshActions()` keep their names; `renderActions` no longer touches any visibility or `#actionsName`.

Behaviour:
1. `select(name)` turns watching on (`setWatching(true)`) for a body and off for `null`; the old `if (watching) startWatch()` goes. Pause still works; selecting again (same or other body) resumes.
2. The left `#actions` panel is deleted: markup, its CSS (`#actions`, `#actions[hidden]`, `#actions h2`, and `#actions` in the shared `h2` selector), `#actionsName`. `#actionsLog` lives in the popup's side column; the popup's own `hidden` is the only gate. `setWatching` no longer calls `renderActions()`.
3. Polling is unchanged: every 2 s for the selected body, cleared/rendered on select, stale answers dropped.

- [ ] **Step 1: Rewrite the tests first.** In `test/dashboard-look.test.mjs`:
  - Replace `'inline card: switching the selected body while paused does not start a stream'` with:
```js
test('inline card: selecting a body starts watching it, even after a pause', () => {
  const { el, streams, select } = page()
  select('Bob')
  assert.deepEqual([streams.length, streams[0].url, el('relook').textContent], [1, '/api/look/Bob/live', 'pause'])
  el('relook').listeners.click()
  assert.deepEqual([streams[0].closed, el('relook').textContent], [true, 'watch'])
  select('Bob')
  assert.deepEqual([streams.length, streams[1].url, streams[1].closed, el('relook').textContent], [2, '/api/look/Bob/live', false, 'pause'])
})

test('inline card: deselecting stops watching', () => {
  const { el, streams, select } = page()
  select('Bob')
  select(null)
  assert.deepEqual([streams[0].closed, el('relook').textContent], [true, 'watch'])
})
```
  - In `'inline card: switching the selected body while watching keeps watching, on the new body'` the first `select('Bob')` now closes the stream the button opened and opens Bob's; keep the assertion shape, expected `[2, true, '/api/look/Bob/live', false]`.
  - Delete the four visibility tests: `'actions panel: hidden while nothing is selected, even if a poll fires'`, `'actions panel: selecting a body alone does not show it - only watching its view does'`, `'actions panel: deselecting hides it even while watching'`, and replace `'actions panel: watching shows it and renders the body\'s log; pausing hides it again'` with:
```js
test('actions log: renders the selected body\'s log, whether or not the small picture is paused', async () => {
  const FIXED_NOW = Date.parse('2026-10-01T15:21:24.000Z')
  const { el, select, run } = page(undefined, { clock: FIXED_NOW })
  select('Chani')
  el('relook').listeners.click()
  run('Chani', { entries: [entry({ t: '2026-10-01T15:21:12.000Z' })] })
  await settle()
  assert.deepEqual(el('actionsLog').children.map(c => ({ cls: c.className, text: c.textContent, title: c.title })), [
    { cls: '', text: '-12s job_started goto x=1', title: 'goto x=1' }
  ])
})
```
  - In `'actions panel: switching bodies clears the previous log immediately, before the new one answers'` delete the `actionsName` assertion.
  - Rename the remaining `'actions panel: ...'` titles to `'actions log: ...'`; update the header comment of the file (the panel is now inside the popup).
  - Add a markup test, so the log cannot drift back out of the popup:
```js
test('actions log: lives inside the popup and nowhere else', () => {
  const popup = html.slice(html.indexOf('<div id="lookOverlay"'), html.indexOf('<div id="planOverlay"'))
  assert.deepEqual([popup.includes('id="actionsLog"'), html.split('id="actionsLog"').length - 1, html.includes('id="actions"')], [true, 1, false])
})
```
  Run `node --test test/dashboard-look.test.mjs`; expect the new select/markup tests to FAIL.

- [ ] **Step 2: Markup.** Delete the `<div id="actions" hidden>...</div>` block from `#mapview`. Wrap the popup's current children in `<div id="lookMain">` and add the side column:
```html
<div id="lookOverlay" hidden>
  <div id="lookCard">
    <div id="lookMain">
      ...the current planHead, lookFrame, lookHud, lookWindowMeta, lookWindow, lookInventory, unchanged...
    </div>
    <div id="lookSide">
      <h2>actions</h2>
      <div id="actionsLog"></div>
    </div>
  </div>
</div>
```

- [ ] **Step 3: CSS.** Delete the `#actions`, `#actions[hidden]`, `#actions h2` rules and `#actions h2, ` from the shared `h2` rule (make it `#chatbar h2, section h2, #lookSide h2`). Keep `#actionsLog div { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }`. Replace the `#lookCard` rule (keep its comment, minus panorama) with:
```css
  #lookCard { display: flex; gap: 12px; width: min(96vw, 980px); max-height: 94vh; }
  #lookMain { flex: 1 1 640px; min-width: 0; display: flex; flex-direction: column; gap: 8px; overflow-y: auto; }
  #lookSide { flex: 0 0 320px; min-height: 0; display: flex; flex-direction: column; gap: 6px; background: var(--panel); border: 1px solid var(--line); border-radius: 6px; padding: 8px 10px; }
  #lookSide h2 { margin: 0; }
  #actionsLog { flex: 1; min-height: 0; overflow-y: auto; }
```

- [ ] **Step 4: Script.**
```js
const select = name => {
  selected = name
  ...unchanged clearing lines, minus nothing...
  actionsEntries = []
  renderActions()
  renderList()
  draw()
  setWatching(Boolean(name))
  refreshActions()
}
```
`renderActions` drops its first two lines (`el('actions').hidden = ...`, `el('actionsName').textContent = ...`) and starts `if (!selected) return`. `setWatching` drops `renderActions()`. Update the comment above `watching` ("started by "watch", stopped by "pause"") to say selecting a body starts it.

- [ ] **Step 5: README.** Replace "A translucent panel over the left of the map shows that body's recent actions - job starts/completions/failures, death, respawn, holing up, chat - newest at the bottom, aging in place." with: "Selecting a body starts watching it (pause stops the small picture). The popup shows that body's recent actions beside the picture - job starts/completions/failures, death, respawn, holing up, chat - newest at the bottom, aging in place."

- [ ] **Step 6: Verify.** `node --test test/dashboard-look.test.mjs test/dashboard.test.mjs` pass; `node tools/check-code.mjs` clean; `grep -n "actionsName\|id=\"actions\"" tools/dashboard/index.html` prints nothing.

- [ ] **Step 7: Commit** `dashboard: selecting a body watches it; the action log lives in the agent popup`.

---

### Task 3: Whisper to the agent from the popup

**Files:**
- Modify: `src/talk.mjs` (new `heardWhisper`), `test/talk.test.mjs`
- Modify: `src/bot.mjs` (new quick action `hear`, next to `whisper` ~L3070; import `heardWhisper` in the existing `./talk.mjs` import at L37)
- Modify: `src/job-policy.mjs` (add `'hear'` to `CONCURRENT_READ_ACTIONS`: otherwise it queues behind a long job like goto and the whisper arrives minutes late)
- Modify: `src/lib/help.mjs` (entry for `hear`, section `self`)
- Modify: `tools/dashboard/lib.mjs` (route `/api/whisper/<Name>`), `test/dashboard.test.mjs` (route case)
- Modify: `tools/dashboard.mjs` (POST handler)
- Modify: `tools/dashboard/index.html` (input in `#lookSide`), `test/dashboard-look.test.mjs`
- Modify: `README.md` (dashboard paragraph + API list), `AGENT_GUIDE.md` (whisper row ~L128)

**Interfaces:**
- Consumes: `#lookSide` and `#actionsLog`, `refreshActions()`, `selected` from Task 2.
- Produces: body action `hear {from, message}` → emits `whisper {from, message}`, answers `{}`; dashboard `POST /api/whisper/<Name>` body `{"message": "..."}` → `200 {ok:true}` | `4xx/503 {error}`.

- [ ] **Step 1: Failing test for `heardWhisper`** in `test/talk.test.mjs` (add `heardWhisper` to its import):
```js
test('heardWhisper: shaped as bot.on(\'whisper\') records one, text trimmed', () => {
  assert.deepEqual(heardWhisper({ from: 'dashboard', message: '  come home  ' }), { from: 'dashboard', message: 'come home' })
})
for (const [what, args, error] of [
  ['no sender', { message: 'hi' }, /from=/],
  ['no text', { from: 'dashboard', message: '   ' }, /message=/],
  ['a non-string text', { from: 'dashboard', message: { a: 1 } }, /message=/]
]) test(`heardWhisper: ${what} is refused`, () => {
  assert.throws(() => heardWhisper(args), error)
})
```
Run `node --test test/talk.test.mjs`: FAIL (not exported).

- [ ] **Step 2: Implement** in `src/talk.mjs`:
```js
// a whisper that did not come through the game (a line typed into the dashboard's popup), shaped exactly as
// bot.on('whisper') records one, so the driver cannot tell the two apart
export const heardWhisper = ({ from, message }) => {
  const text = typeof message === 'string' ? message.trim() : ''
  if (!from) throw new Error('hear needs from=: who is whispering')
  if (!text) throw new Error('nothing said: the text goes in message=')
  return { from: String(from), message: text }
}
```
Run: PASS.

- [ ] **Step 3: Body action.** In `src/bot.mjs` `quick`, after `whisper (a) {...}`:
```js
  // the dashboard's: a line typed into an agent's popup, recorded as the whisper it stands for (bot.on('whisper') above)
  hear (a) {
    const said = heardWhisper(a)
    lastDriven = Date.now()
    emit('whisper', said)
    return {}
  },
```
Add `'hear'` to `CONCURRENT_READ_ACTIONS` in `src/job-policy.mjs`. Add to `src/lib/help.mjs` near `whisper`:
```js
  hear: { section: 'self', args: 'from= message=', doc: "the dashboard's: a whisper typed outside the game, recorded as if whispered in it" },
```
Run `node --test test/talk.test.mjs test/job-policy*.test.mjs test/help*.test.mjs` (whichever exist; `ls test | grep -i "help\|policy"`) and any test that checks every quick action has a help entry (`grep -ln "PRIMITIVES" test`). All pass.

- [ ] **Step 4: Route, test first.** In `test/dashboard.test.mjs` add to the route cases next to `['/api/actions/Chani', ...]`:
```js
  ['/api/whisper/Chani', { kind: 'whisper', name: 'Chani' }],
```
FAIL; then in `tools/dashboard/lib.mjs` add `const WHISPER = /^\/api\/whisper\/([A-Za-z0-9_]{1,32})$/` beside `ACTIONS`, and in `route()` after the actions match:
```js
  const whisper = WHISPER.exec(pathname)
  if (whisper) return { kind: 'whisper', name: whisper[1] }
```
PASS.

- [ ] **Step 5: Server handler** in `tools/dashboard.mjs`, after `serveActions`:
```js
// a line typed into the popup goes to the body as the whisper it stands for: the body's `hear` appends it to its own
// events.jsonl (it owns that file and its seq numbers), so the driver reads it exactly as one whispered in game
const WHISPER_FROM = 'dashboard'
const serveWhisper = async (req, res, name) => {
  if (req.method !== 'POST') return sendJson(res, 405, { error: 'POST {"message": "..."} to whisper' })
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  let body = ''
  for await (const chunk of req) body += chunk
  const r = await ask(agent.apiPort, 'hear', { from: WHISPER_FROM, message: parseJson(body)?.message }, 5000)
  if (!r.ok) return sendJson(res, 503, { error: r.error ?? r.answer?.error ?? 'the body did not answer' })
  return sendJson(res, 200, { ok: true })
}
```
In the request listener, beside the `look`/`live` lines: `if (r.kind === 'whisper') return serveWhisper(req, res, r.name).catch(e => sendJson(res, 500, { error: e.message }))`. Add `POST /api/whisper/<Name>` to the `unknown` handler's list and to the header comment of the file.

- [ ] **Step 6: Popup input, tests first.** In `test/dashboard-look.test.mjs`'s `page()` harness: give every stub node `value: ''`; make `fetch` take `(url, options)` and record `posts.push({ url, options })` and return `Promise.resolve(postAnswer)` for urls starting `/api/whisper/` (where `postAnswer` is a `page()` option defaulting to `{ ok: true, json: async () => ({ ok: true }) }`); return `posts` from `page()`. Then:
```js
const typeAndEnter = (el, text) => { el('whisper').value = text; el('whisper').listeners.keydown({ key: 'Enter' }) }

test('whisper: Enter posts the trimmed text to the selected body, clears the input, then refreshes the log', async () => {
  const { el, posts, pending, select } = page()
  select('Chani')
  typeAndEnter(el, '  come home  ')
  assert.deepEqual(posts.map(p => [p.url, p.options.method, JSON.parse(p.options.body)]), [['/api/whisper/Chani', 'POST', { message: 'come home' }]])
  assert.equal(el('whisper').value, '')
  await settle()
  assert.deepEqual(pending.map(p => p.url), ['/api/actions/Chani', '/api/actions/Chani'])
})

for (const [what, text] of [['empty', ''], ['blank', '   ']]) test(`whisper: ${what} input sends nothing`, () => {
  const { el, posts, select } = page()
  select('Chani')
  typeAndEnter(el, text)
  assert.deepEqual(posts, [])
})

test('whisper: other keys send nothing', () => {
  const { el, posts, select } = page()
  select('Chani')
  el('whisper').value = 'hi'
  el('whisper').listeners.keydown({ key: 'a' })
  assert.deepEqual([posts, el('whisper').value], [[], 'hi'])
})

test('whisper: a refused send says why and gives the text back', async () => {
  const { el, select } = page(undefined, { postAnswer: { ok: false, json: async () => ({ error: 'the body did not answer' }) } })
  select('Chani')
  typeAndEnter(el, 'come home')
  await settle()
  assert.deepEqual([el('whisperErr').textContent, el('whisper').value], ['the body did not answer', 'come home'])
})
```
(`page(screen, { clock, postAnswer })`.) Run: FAIL.

- [ ] **Step 7: Implement the input.** Markup, at the end of `#lookSide`:
```html
      <input id="whisper" type="text" placeholder="whisper to the agent, Enter sends" spellcheck="false" autocomplete="off">
      <div id="whisperErr" class="err"></div>
```
CSS (beside `#chatfilter`'s rules):
```css
  #whisper { font: inherit; color: var(--ink); background: var(--bg); border: 1px solid var(--line); border-radius: 4px; padding: 4px 8px; }
  #whisper:focus { outline: none; border-color: var(--zone); }
  #whisperErr:empty { display: none; }
```
Script, inside the test slice (after `refreshActions`):
```js
// what is typed under the log reaches the driver as a whisper from "dashboard" (tools/dashboard.mjs), and comes back
// into the log above as one; a refused send keeps the text, so nothing typed is lost
el('whisper').addEventListener('keydown', e => {
  if (e.key !== 'Enter') return
  const message = el('whisper').value.trim()
  if (!message || !selected) return
  el('whisper').value = ''
  el('whisperErr').textContent = ''
  const failed = error => { el('whisperErr').textContent = error; el('whisper').value ||= message }
  fetch(`/api/whisper/${encodeURIComponent(selected)}`, { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ message }) })
    .then(r => r.ok ? refreshActions() : r.json().then(a => failed(a.error ?? 'not sent'), () => failed('not sent')))
    .catch(e => failed(e.message))
})
```
Run `node --test test/dashboard-look.test.mjs test/dashboard.test.mjs test/talk.test.mjs`: PASS.

- [ ] **Step 8: Docs.** README dashboard paragraph: after the action-log sentence add "Under the log, a line typed and sent with Enter reaches the agent as a whisper from `dashboard`, the same `whisper` event an in-game whisper makes." and add `POST /api/whisper/<Name>` (`{"message": "..."}`) to the API list. AGENT_GUIDE.md whisper row (~L128): append "A whisper `from=dashboard` is a human typing in the dashboard: there is no player to whisper back to, so answer it with `chat`."

- [ ] **Step 9: Verify.** `node --test test/dashboard-look.test.mjs test/dashboard.test.mjs test/talk.test.mjs` and the help/policy tests pass; `node tools/check-code.mjs` clean. Do not call any real body's API.

- [ ] **Step 10: Commit** `dashboard: whisper to an agent from its popup (body action hear emits the in-game whisper event)`.
