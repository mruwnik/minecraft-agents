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
  header; 404 when the body has no view). `/api/thumbs/stats`: the thumbnailer's counters. The render worker has heap caps (`resourceLimits`, 160 MB old generation) and is replaced
  once it holds more than 400 loaded columns (the view's column cache never evicts). On SIGTERM/SIGINT the server closes the
  thumbnailer and the view mount (`close()`, ends the block-issues scan worker) and exits. A body card whose view is older
  than 10 s shows an "N s old" / "N min old" mark when the body is online (`trouble/thumb-age-mark`).
- `/api/events/<body>?limit=`: the tail of the body's `events.jsonl`, filtered to what the popup lists (default 300, at most 2000).
- `/api/item-icon/<item>.png`: an item's picture from the repo's `textures/`.
- `/api/jobs`: `{at, jobs: [{kind, id, category, name, file, ns-doc, doc, args, backoff, running, reflex}]}`: every job
  namespace of `engine/src/jobs/**/*.cljs` (`kind` job, `id` `jobs.<dir>.<name>`) and every trigger of
  `engine/src/engine/triggers/*.cljs` (`kind` trigger, category `triggers`, `id` `engine.triggers.<name>`). The list is
  **fixed at dashboard build time**: the macro `dashboard.jobs-registry/compile-entries` (src/dashboard/jobs_registry.clj)
  calls `engine.registry` (`jobs-dir`, `job-files`, `expected-ns`, `read-forms`, the engine's own lenient reader; `../engine/src`
  is on `:source-paths`, only its `.clj` is loaded) and emits a literal vector, so a new or changed job needs `npm run build`.
  `doc` is the `(def doc ...)` string, `args` the `(def args ...)` map printed as EDN (one entry per line), `backoff` whether
  the namespace defines `backoff`; a file the reader rejects has `error` instead. `running` and `reflex` are the bodies whose
  job list or reflex register mentions the job, computed per request.
- POST `/api/chat/send`, body `{text}` (JSON, at most 4 KB): sends the fixed command `tellraw @a {"text":"<Dan> <text>"}`
  over RCON, so engine bodies hear it as a chat event. There is no target: a `target` (or any other) field gets 400.
  The text component is built with `JSON.stringify`; control characters, newlines and section codes are stripped, the text
  is capped at 256 chars and must not be empty. The sender is env `DASHBOARD_CHAT_AS` (default `Dan`, must match
  `^[A-Za-z0-9_]{1,16}$`, checked at startup). Rate limit 1 per second and 5 per 30 s (429). Replies `{ok, command}`, 400 `{error}`,
  502 `{error}` when RCON fails. `DASHBOARD_CHAT_DRY=1` logs the command instead of sending. Code: `dashboard.chat-send`
  (pure), `dashboard.rcon` (socket, password read in-process).
- Guard (`dashboard.guard`) on every state-changing route (POST `/api/chat/send`, POST `/api/blueprint-preview`), same rules as
  `tools/view/drive-proxy.mjs` (whose check is not exported, so it is reimplemented): Host must be `127.0.0.1`, `localhost` or `[::1]`
  with the server's port; Origin, when present, must equal `http://<Host>` (a foreign Origin or `null` is 403); Content-Type
  must be `application/json` (415); other methods get 405; body limit 4 KB for chat (413), 2 MiB for blueprint-preview.
  `/drive/<body>` goes through drive-proxy and keeps its own check.
- The live 3D view is mounted on this origin by `js/viewmount.mjs` (the handler of `tools/view/serve.mjs`, never listening):
  `/view`, `/agents`, `/pose/<body>`, `/hud/<body>`, `/drive/<body>` (POST takeover controls, loopback only), `/web/`, `/columns/`,
  `/blocks/`, `/textures/`.

Pages `/` (bodies: a card per body with a thumbnail, click for the popup with live view and takeover), `/map` (places, zones,
plans and live bodies; the default view fits places and plans, "home" returns to it, "fit all" and "fit bodies" refit; a body
off screen gets an arrow on the edge, click it to pan there), `/plans` (completion against the dumped chunks), `/villages`,
`/villagers`, `/blueprints`, `/jobs` (every job and trigger by category, with filter) serve `public/index.html`; static files come from
`public/` and `out/public/js/` (at `/js/`). The chat panel on the right of every page has a composer: Enter sends as above.
`?world=` is validated against the `state/worlds/*/world.json` listing (unknown -> 400 `{error, worlds}`; absent -> first world).

## Differs from the old dashboard

- No body is ever contacted: engine bodies are read from `events.jsonl` (incremental tail) and `engine.edn`
  (jobs, reflexes). Folders without `engine/events.jsonl` are listed as down, "not an engine body (unsupported)".
- No look/screen/actions/whisper/icon endpoints, and `/api/world` is 501.
- Chat comes from engine events (`source chat`, kind `said`/`whisper`); `t` is epoch millis.
- Villages, villagers and blueprints reuse the old JS modules (read-only, loaded with `require`).
