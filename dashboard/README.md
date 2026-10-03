# Dashboard (ClojureScript)

Replacement for `tools/dashboard.mjs`, for ENGINE bodies (agent folders with `engine/events.jsonl`).

    npm install
    npm test          # shadow-cljs compile test && node out/test.cjs
    npm run build     # compiles :server (out/server.cjs) and :ui (out/public/js)
    npm start         # build, then PORT=3701 node out/server.cjs   (127.0.0.1 only)

`DASHBOARD_ROOT` overrides the repo root (default: two levels above `out/`).

## Endpoints (JSON, `cache-control: no-store`)

`/api/worlds`, `/api/state?world=`, `/api/chat?world=&limit=`, `/api/villages?world=`, `/api/villagers`,
`/api/plans?world=` (plans with a completion summary, cached 10 s), `/api/plan/<name>?world=` (full comparison with layers and bill),
`/api/blueprints`, `/api/blueprint/<name>`, POST `/api/blueprint-preview`, `/api/world` (501).

- `/api/thumb/<body>.png`: the body's latest view as a PNG (rendered by `js/thumbs.mjs` from `pose.json`; `x-pose-mtime`
  header; 404 when the body has no view). `/api/thumbs/stats`: the thumbnailer's counters.
- `/api/events/<body>?limit=`: the tail of the body's `events.jsonl`, filtered to what the popup lists (default 300, at most 2000).
- `/api/item-icon/<item>.png`: an item's picture from the repo's `textures/`.
- `/api/jobs`: `{at, jobs: [{id, category, name, file, ns-doc, doc, args, backoff, running, reflex}]}`, one per file of
  `engine/src/jobs/**/*.cljs`. The files are read at request time (cached per file by mtime) with `cljs.tools.reader`,
  leniently like `engine/src/engine/registry.clj`: `id` is `jobs.<dir>.<name>`, `doc` the `(def doc ...)` string, `args`
  the `(def args ...)` map printed as EDN (one entry per line), `backoff` whether the namespace defines `backoff`.
  `running` and `reflex` are the bodies whose job list or reflex register mentions the job. A file that does not read
  has `error` instead.
- POST `/api/chat/send`, body `{text, target?}` (JSON, at most 2 KB; `target` is `@a` by default, or a player name):
  sends `tellraw <target> {"text":"<Dan> <text>"}` over RCON, so engine bodies hear it as a chat event. The sender is
  always `Dan`. Replies `{ok, command}`, 400 `{error}` for bad input, 413 over the limit, 502 `{error}` when RCON fails.
  `DASHBOARD_CHAT_DRY=1` makes the server log the command and not send it. Logic in `js/chatroute.mjs`, RCON in `js/chatsend.mjs`.
- The live 3D view is mounted on this origin by `js/viewmount.mjs` (the handler of `tools/view/serve.mjs`, never listening):
  `/view`, `/agents`, `/pose/<body>`, `/hud/<body>`, `/drive/<body>` (POST takeover controls, loopback only), `/web/`, `/columns/`,
  `/blocks/`, `/textures/`.

Pages `/` (bodies: a card per body with a thumbnail, click for the popup with live view and takeover), `/map` (places, zones,
plans and live bodies; the default view fits places and plans, "home" returns to it, "fit all" and "fit bodies" refit; a body
off screen gets an arrow on the edge, click it to pan there), `/plans` (completion against the dumped chunks), `/villages`,
`/villagers`, `/blueprints`, `/jobs` (every job by category, with filter) serve `public/index.html`; static files come from
`public/` and `out/public/js/` (at `/js/`). The chat panel on the right of every page has a composer: Enter sends as above.
`?world=` is validated against the `state/worlds/*/world.json` listing (unknown -> 400 `{error, worlds}`; absent -> first world).

## Differs from the old dashboard

- No body is ever contacted: engine bodies are read from `events.jsonl` (incremental tail) and `engine.edn`
  (jobs, reflexes). Folders without `engine/events.jsonl` are listed as down, "not an engine body (unsupported)".
- No look/screen/actions/whisper/icon endpoints, and `/api/world` is 501.
- Chat comes from engine events (`source chat`, kind `said`/`whisper`); `t` is epoch millis.
- Villages, villagers and blueprints reuse the old JS modules (read-only, loaded with `require`).
