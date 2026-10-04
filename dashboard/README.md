# Dashboard (ClojureScript)

Replacement for `tools/dashboard.mjs`, for ENGINE bodies (agent folders with `engine/events.sock` or legacy event files).

    npm install
    npm test          # shadow-cljs compile test && node out/test.cjs
    npm run build     # compiles :server (out/server.cjs) and :ui (out/public/js)
    npm start         # launcher (start.mjs): build, then run out/server.cjs on PORT (default 3701, 127.0.0.1 only); stays in the foreground
    npm run restart   # ask the running launcher to rebuild and restart the server (POST /api/restart)
    node --test js/launcher.test.mjs   # the launcher's pure decisions

### Start and restart

`npm start` runs `start.mjs`, a small Node supervisor (plain JS: it only spawns processes). It builds with
`flock /tmp/mc-compile.lock npx shadow-cljs compile server ui`, then runs `node --max-old-space-size=256 --max-semi-space-size=4
out/server.cjs` with inherited stdio, so the server's output stays in your terminal. Start it once; you do not restart it by hand.

To pick up new code, run `npm --prefix dashboard run restart` (or `POST /api/restart`, accepted from loopback peers only,
403 otherwise, 202 at once). The server tells the launcher over its IPC channel, and the launcher:

- refuses when `MemAvailable` in `/proc/meminfo` is under 3500 MB (message printed, old server kept);
- builds FIRST while the old server keeps running; requests that arrive during a build coalesce into one more build;
- on a **failed build**: prints the compiler output, keeps the old server running, and is done;
- on a good build: sends SIGTERM to the old server, waits for it to exit (SIGKILL after 5 s), starts the new one.

Ctrl-C stops the launcher and the server. A server exit nobody asked for ends the launcher with the server's exit code
(no restart loops). An open page reloads itself when the `build-id` in `/api/state` changes (`dashboard.ui.buildid`).
If the server runs without the launcher, `/api/restart` answers 409.

The compile JVM is capped (`:jvm-opts ["-Xmx1G"]` in `shadow-cljs.edn`) because one compile must fit beside the game server on a 31 GB machine.

`DASHBOARD_ROOT` overrides the repo root (default: two levels above `out/`).

## Endpoints

`/api/state?world=`, the plan endpoints, and the blueprint, village, villager and entity endpoints use EDN (`application/edn`). Other read endpoints below return JSON unless noted; all use `cache-control: no-store`.

`/api/worlds`, `/api/chat?world=&limit=`, `/api/villages?world=`, `/api/villagers?world=&dimension=`, `/api/entities?world=&dimension=`,
`/api/plans?world=` (every plan file of the world with its totals, cached 10 s), `/api/plan/<id>?world=` (full comparison: elements, per-layer grids, errors),
`/api/blueprints`, `/api/blueprint/<name>`, POST `/api/blueprint-preview`, `/api/world` (501).

- `/api/thumb/<world>/<body>.png`: the fallback still of a body's latest view (software-rendered from `pose.json`, `x-pose-mtime`
  header; 404 when the body has no view), used only when the browser has no WebGL2 or the page has `?nogl=1`.
  `dashboard.thumbs` (cljs) decides: cached by pose mtime, a newer pose is re-rendered only once the cached still is 2 s old, one render
  at a time, the render worker replaced once it holds more than 400 columns. `js/thumbs.mjs` renders one still on request in a
  worker thread (heap caps, `resourceLimits` 160 MB old generation). `/api/thumbs/stats`: `{bodies, renders, last-ms, mean-ms, queue}`.
  On SIGTERM/SIGINT the server closes the thumbnailer and the view mount (`close()`, ends the block-issues scan worker) and exits. A body card whose view is older
  than 10 s shows an "N s old" / "N min old" mark when the body is online (`trouble/thumb-age-mark`).
- Body cards (hub mode): online bodies get a live textured scene from the view hub (`ui/livecards.cljs`); an offline body shows the server's still
  (`/api/thumb`, greyed by CSS) with no scene. Debug flags on the page URL: `?fps=1` labels live cards with their fps, `?nogl=1` forces the server stills,
  `?allive=1` gives every card with a view a live scene, offline ones too (the hub holds at most 12 scenes).
- `/api/events/<world>/<body>?limit=&stream-id=&after=`: EDN page of canonical events (default tail 300, maximum 1000), the current outstanding attention map, and a cursor. `after` is exclusive. On `:gap? true`, the dashboard refreshes the snapshot and replaces its retained log tail before resuming. Legacy bodies without `events.edn` or an event socket may use their old `events.jsonl` as historical best-effort input.
- `POST /api/whisper/<world>/<body>` `{"text": ...}`: a private message to one body, sent through RCON as `tellraw <body>` of the vanilla whisper line from the chat sender (the body records it as a `whisper` event; the chat panel reads it from `events.edn`). The target must match `[A-Za-z0-9_]{3,16}` (400), be an engine body (404) and be up (409); the text is cleaned and cut like chat; the rate limit is shared with `POST /api/chat/send`. The body popup has the input.
- `POST /api/attention/<world>/<body>/resolve`: EDN request `{:request-id "..." :reason :handled}` to acknowledge an outstanding request. This only marks that request handled; it does not retry or restart its job.
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
- POST `/api/chat/send`, body `{text}` (JSON, at most 4 KB): sends the fixed command `tellraw @a {"text":"<dashboard> <text>"}`
  over RCON, so engine bodies hear it as a chat event. There is no target: a `target` (or any other) field gets 400.
  The text component is built with `JSON.stringify`; control characters, newlines and section codes are stripped, the text
  is capped at 256 chars and must not be empty. The sender is env `DASHBOARD_CHAT_AS` (default `dashboard`, must match
  `^[A-Za-z0-9_]{1,16}$`, checked at startup). Rate limit 1 per second and 5 per 30 s (429). Replies `{ok, command}`, 400 `{error}`,
  502 `{error}` when RCON fails. `DASHBOARD_CHAT_DRY=1` logs the command instead of sending. Code: `dashboard.chat-send`
  (pure), `dashboard.rcon` (socket, password read in-process).
- Guard (`dashboard.guard`) on every state-changing route, including POST `/api/attention/<world>/<body>/resolve`, applies the loopback
  Host/Origin restrictions used by `tools/view/drive-proxy.mjs`; attention resolution requires `application/edn`. Existing
  mutation routes use JSON.
  `/drive/<world>/<body>` goes through drive-proxy and keeps its own check.
- The live 3D view is mounted on this origin by `js/viewmount.mjs` (the handler of `tools/view/serve.mjs`, never listening):
  `/view?agent=<world>/<body>`, `/agents`, `/pose/<world>/<body>`, `/hud/<world>/<body>`, `/drive/<world>/<body>` (POST takeover controls, loopback only), `/web/`, `/columns/`,
  `/blocks/`, `/textures/`.

Pages `/` (bodies: a card per body with a thumbnail, click for the popup with live view and takeover), `/map` (places, zones,
plans (outlined, coloured by completion; zoomed in, their elements too; a click opens the plan) and live bodies; the default view fits places and plans, "home" returns to it, "fit all" and "fit bodies" refit; a body
off screen gets an arrow on the edge, click it to pan there), `/plans` (plan files compared with the dumped chunks, see "Plans"), `/villages`,
`/villagers`, `/blueprints`, `/jobs` (every job and trigger by category, with filter) serve `public/index.html`; static files come from
`public/` and `out/public/js/` (at `/js/`). The chat panel on the right of every page has a composer: Enter sends as above.
`?world=` is validated against the `state/worlds/*/world.json` listing (unknown -> 400 `{error, worlds}`; absent -> first world).

## Differs from the old dashboard

- Engine bodies with the canonical event service are read from their local `events.sock` for snapshots, paginated event replay
  and attention resolution; the socket is separate from the viewer control socket. Old bodies can still be read from
  `events.jsonl` as best-effort history. `engine.edn` remains a legacy fallback for jobs and reflexes. Folders without recognized
  engine data are listed as down, "not an engine body (unsupported)".
- No look/screen/actions/whisper/icon endpoints, and `/api/world` is 501.
- Chat comes from engine events (`:source :chat`, kind `:said`/`:whisper`); canonical `:time-ms` is epoch millis.
- Blueprints read the canonical `blueprints/<id>.edn` library and validate drafts using the engine's native schema. The editor previews and downloads EDN; preview does not place or change a build. POST `/api/blueprint-preview` requires `application/edn` and accepts `{:source "..." :stock "..."}`; stock is an optional EDN map of block names to counts.
- Villages read native world plans with `:kind :village`. A plan's optional finite `:at [x y z]` is a map anchor; empty `:parts` remains incomplete geometry. Inspection metadata is historical evidence. See [village plan migration](docs/village-plan-migration.md) for the compiled, guarded migration CLI.
- Entity observations come from each engine's `GET /entities` on `engine/control.sock`. Requests run asynchronously with a 1-second deadline, 4 MiB response limit, and at most four concurrent requests. Original observation times expire after two minutes; polling and unavailable bodies never renew them. The dashboard merges by world and UUID before filtering dimension, and caps cached sources, records, and output. `/api/villagers` filters that transient entity layer. No persistent villager roster is read or written.
- The map draws all observed moving entity types on canvas, with distinct player and villager colors, bounded labels, and hover identity/age details. Agent players use their body markers. Older bodies without `/entities` show a restart message.

## Plans

A plan records the desired state of a piece of the world; the format is the one of `docs/design.md`, "Plans and blueprints". The
dashboard compares it with the chunk columns the bodies dumped (`state/worlds/<world>/chunks/<cx>.<cz>.bin`, `cx = floor(x/16)`).
One EDN file per plan, `state/worlds/<world>/plans/<id>.edn`, and one per blueprint, `blueprints/<id>.edn` at the repo root (next to
legacy `.blueprint.json` files, which the dashboard ignores); the file name is the id. `plan.shape` (`../engine/src/plan/shape.cljc`, plain data, no
IO, meant to be required by the engine's jobs too) is the only namespace that knows the format: checking (`plan-errors`,
`blueprint-errors`), expansion into cells (`expand`, the later part winning a shared cell; blueprints placed and turned with `place`)
and plan minus world (`judge`, `plan-minus-world`, `assignment-answers`). `dashboard.plan` reads the files, `dashboard.plan-compare`
counts answers and lays out grids (pure), `dashboard.plan-api` builds the two endpoints, `js/worldblocks.mjs` is the glue that reads
block names out of the dumped columns with the view's decoders.

- Each part is listed as an element on the Plans page (kind = `box`, `outline`, `cells` or `blueprint`), with its own counts.
- Cell answers: `match`, `missing` (air where something is wanted), `wrong` (another block), `extra` (a block where `:clear` is
  wanted), `unknown` (the column was never dumped, or the want names block state: the columns give names only). Percent = match / all
  cells (unknown included). A part may have at most 200000 cells.
- Conflicts: two `:active` plans that want different things of the same cell (`plan.conflicts`, `../engine/src/plan/conflicts.cljc`: the
  cells are indexed once, never plan against plan) are shown where plans are shown. `/api/plans` carries `conflicts` (one entry per pair
  of plans: `plans`, `count`, `same` = cells both want identically, `box`, `cells` capped at 3000 with `shown`) and each plan item its own
  `conflicts` (`with`, `count`, `box`); `/api/plan/<id>` flags the cells `x` in its layers. Two wants agree when equal (a bare block name is
  the block with no state) or when an `:any` shares a choice with the other. Same-want overlap is not reported; `proposed` and `retired`
  plans conflict with nothing. The Plans page marks a plan "conflicts with <plan> in N cells" (click opens the other plan) and outlines the
  cells in the grid; the map draws the cells in pink inside a dashed box with a label, and clicking the label centres the map on the box.
- An invalid file (bad EDN, id not equal to the file name, a bad part, want or blueprint) is not listed; its problems, naming the part,
  are returned in `errors` of `/api/plans` and shown on the Plans page. A part placing a missing blueprint, or an assignment naming no
  part or spot, is an error of that plan's detail.

## Terrain rendering benchmark

`tools/terrain-web-bench.mjs` measures the dashboard map in an already-open Chromium tab. It samples equal idle, warm pan/zoom,
and zoomed-out overview-pan phases, recording animation-frame intervals, long tasks, browser main-thread task/script time, canvas
draw/resize counts and tile requests. It can save a screenshot for each phase. Use the same Chromium build, viewport, saved world,
phase duration and warmed map data when comparing revisions; a cold `/api/tiles/<world>` scan is a separate startup cost.

From the repository root, build and start the dashboard using saved local data (do not start a body to run this benchmark):

```sh
cd dashboard
npm run build
DASHBOARD_ROOT=/path/to/minecraft-agents PORT=3701 node --max-old-space-size=256 --max-semi-space-size=4 out/server.cjs
```

In another terminal, open the map in headless Chromium, then run the harness from the repository root:

```sh
profile=$(mktemp -d /tmp/terrain-chrome-XXXXXX)
/usr/bin/chromium --headless=new --no-sandbox --disable-dev-shm-usage \
  --remote-debugging-port=9223 --user-data-dir="$profile" --window-size=1440,1000 \
  'http://127.0.0.1:3701/map?world=claude'
node tools/terrain-web-bench.mjs --cdp http://127.0.0.1:9223/json \
  --out /tmp/terrain-perf.json --screenshots /tmp/terrain-perf-shots
```

For close pan/zoom, center the page on a body whose coordinates overlap dumped terrain and ask the harness to zoom in before sampling:

```sh
node tools/terrain-web-bench.mjs --cdp http://127.0.0.1:9223/json \
  --page '/map?world=claude' --start-zoom-in 1 --phase-ms 8000 \
  --out /tmp/terrain-close.json --screenshots /tmp/terrain-close-shots
```

For example, `/map?world=claude&show=ClaudeProbe` centers the saved ClaudeProbe position in this dataset. The harness waits for a
map-pane `drawImage` before it samples a prepared zoom; if the chosen position has no image terrain, it stops instead of reporting
an invalid close-terrain result. This is a basic image-rendering check, not proof that every tile has loaded or that the final viewport contains terrain. Compare tile requests and screenshots, and choose a body and zoom that keep image tiles in view throughout the pan/zoom phase.

The default run takes about 50 seconds. The harness sends pointer and wheel input to the map only; it does not call body-control or
chat routes. Its browser task/script counters are useful for same-machine comparisons, not GPU timings: headless Chromium may use
software rendering, and the canvas call timers do not include all raster/compositor work. Compare screenshots as well as numbers
to catch tile gaps or changes in terrain shading.

### Measured result

Two paired runs used the same saved world, headless browser, viewport and 8-second phases. Browser main-thread task/script deltas
are milliseconds; ranges below cover the two runs:

| Phase | Baseline task / script | Path2D task / script |
|---|---:|---:|
| Warm pan/zoom | 1351–1535 / 923–1010 | 1440–1504 / 1005–1083 |
| Overview pan | 1464–1768 / 1188–1519 | 815–823 / 453–470 |

Across the paired overview samples, task time fell by 44–54% and script time by 62–69%. Warm pan/zoom ranges overlap, so this
change has no demonstrated pan speedup. Idle results are excluded because tile image completions differed between runs (2 versus
26 redraw-triggering arrivals); frame p95 stayed around 16.7–16.8 ms, so these measurements do not establish an FPS gain. The
overview optimization caches one `Path2D` of world-coordinate chunk coverage and fills it through the viewport transform,
instead of rebuilding and filling a visible rectangle for every column on each redraw.

A further close-pan experiment tested a spatial grid for label collision checks. One browser comparison showed only a small 2–4% CPU reduction, with slightly higher overview script time and unchanged 16.7 ms frame p95. The paired repeat did not complete, so the candidate was discarded; this pass changes the benchmark only and establishes no additional rendering speedup.
