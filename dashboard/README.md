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
Pages `/`, `/villagers`, `/villages`, `/blueprints` serve `public/index.html`; static files come from `public/`
and `out/public/js/` (at `/js/`). `?world=` is validated against the `state/worlds/*/world.json` listing
(unknown -> 400 `{error, worlds}`; absent -> first world).

## Differs from the old dashboard

- No body is ever contacted: engine bodies are read from `events.jsonl` (incremental tail) and `engine.edn`
  (jobs, reflexes). Folders without `engine/events.jsonl` are listed as down, "not an engine body (unsupported)".
- No look/screen/actions/whisper/icon endpoints, and `/api/world` is 501.
- Chat comes from engine events (`source chat`, kind `said`/`whisper`); `t` is epoch millis.
- Villages, villagers and blueprints reuse the old JS modules (read-only, loaded with `require`).
