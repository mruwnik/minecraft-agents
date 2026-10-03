# Dashboard (ClojureScript)

Replacement for `tools/dashboard.mjs`, for ENGINE bodies (agent folders with `engine/events.sock` or legacy event files).

    npm install
    npm test          # shadow-cljs compile test && node out/test.cjs
    npm run build     # compiles :server (out/server.cjs) and :ui (out/public/js)
    npm start         # build, then PORT=3701 node --max-old-space-size=256 --max-semi-space-size=4 out/server.cjs   (127.0.0.1 only)

`DASHBOARD_ROOT` overrides the repo root (default: two levels above `out/`).

## Endpoints

`/api/state?world=` is EDN (`application/edn`) because it carries engine summaries and outstanding attention requests. Other read endpoints below return JSON unless noted; all use `cache-control: no-store`.

`/api/worlds`, `/api/chat?world=&limit=`, `/api/villages?world=`, `/api/villagers`,
`/api/plans?world=` (every plan file of the world with its totals, cached 10 s), `/api/plan/<id>?world=` (full comparison: elements, per-layer grids, errors),
`/api/blueprints`, `/api/blueprint/<name>`, POST `/api/blueprint-preview`, `/api/world` (501).

- `/api/thumb/<body>.png`: the fallback still of a body's latest view (software-rendered from `pose.json`, `x-pose-mtime`
  header; 404 when the body has no view), used only when the browser has no WebGL2 or the page has `?nogl=1`.
  `dashboard.thumbs` (cljs) decides: cached by pose mtime, a newer pose is re-rendered only once the cached still is 2 s old, one render
  at a time, the render worker replaced once it holds more than 400 columns. `js/thumbs.mjs` renders one still on request in a
  worker thread (heap caps, `resourceLimits` 160 MB old generation). `/api/thumbs/stats`: `{bodies, renders, last-ms, mean-ms, queue}`.
  On SIGTERM/SIGINT the server closes the thumbnailer and the view mount (`close()`, ends the block-issues scan worker) and exits. A body card whose view is older
  than 10 s shows an "N s old" / "N min old" mark when the body is online (`trouble/thumb-age-mark`).
- Body cards (hub mode): online bodies get a live textured scene from the view hub (`ui/livecards.cljs`); an offline body gets ONE frame at its
  last pose (`ui/stills.cljs`: one scene at a time is opened, snapshotted into a 2D canvas once the columns are loaded, and closed; re-taken only when
  `poseMtimeMs` changes). Debug flags on the page URL: `?fps=1` labels live cards with their fps, `?nogl=1` forces the server stills,
  `?allive=1` gives every card with a view a live scene, offline ones too (the hub holds at most 12 scenes).
- `/api/events/<body>?limit=&stream-id=&after=`: EDN page of canonical events (default tail 300, maximum 1000), the current outstanding attention map, and a cursor. `after` is exclusive. On `:gap? true`, the dashboard refreshes the snapshot and replaces its retained log tail before resuming. Legacy bodies without `events.edn` or an event socket may use their old `events.jsonl` as historical best-effort input.
- `POST /api/attention/<body>/resolve`: EDN request `{:request-id "..." :reason :handled}` to acknowledge an outstanding request. This only marks that request handled; it does not retry or restart its job.
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
- Guard (`dashboard.guard`) on every state-changing route, including POST `/api/attention/<body>/resolve`, applies the loopback
  Host/Origin restrictions used by `tools/view/drive-proxy.mjs`; attention resolution requires `application/edn`. Existing
  mutation routes use JSON.
  `/drive/<body>` goes through drive-proxy and keeps its own check.
- The live 3D view is mounted on this origin by `js/viewmount.mjs` (the handler of `tools/view/serve.mjs`, never listening):
  `/view`, `/agents`, `/pose/<body>`, `/hud/<body>`, `/drive/<body>` (POST takeover controls, loopback only), `/web/`, `/columns/`,
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
- Villages, villagers and blueprints reuse the old JS modules (read-only, loaded with `require`).

## Plans (draft)

A plan says what should stand in a region of the world; the dashboard compares it with the chunk columns the bodies dumped
(`state/worlds/<world>/chunks/<cx>.<cz>.bin`, `cx = floor(x/16)`). One EDN file per plan, `state/worlds/<world>/plans/<id>.edn`
(the file name is the id). **This format is a draft, input for an overhaul**; `plan.shape` (`src/plan/shape.cljc`, plain data, no IO, meant to be
required by the engine's jobs too) is the only namespace that knows it: validation, child resolution, expansion into cells and the
cell predicates. `dashboard.plan` reads the files, `dashboard.plan-compare` counts statuses and lays out grids (pure), `dashboard.plan-api` builds the two endpoints, `js/worldblocks.mjs` is the glue that
reads blocks out of the dumped columns with the view's decoders.

```clojure
{:id "jizo-farm" :name "Jizo's farm" :owner "Jizo" :kind :farm :status :active   ; :proposed (default) :active :done :abandoned
 :note "free text"
 :region {:min [100 70 -80] :max [120 72 -60]}                                       ; inclusive block coordinates, required
 :elements
 [{:id "plot-a" :kind :plot :region {:min [101 71 -79] :max [109 71 -71]} :content {:crop "wheat"}}
  {:id "fence" :kind :border :region {:min [100 71 -80] :max [120 71 -60]} :content {:block "oak_fence"}}
  {:id "well" :kind :structure :at [115 70 -65] :rotation 90 :content {:blueprint "well-small"}}
  {:id "melons" :kind :plan :ref "jizo-melons"}]}
```

- `:content` is exactly one of `{:crop c}` (the crop block in any growth stage; `melon`/`pumpkin`/`*_stem` also accept the stem and the
  attached stem; the cell below a farmland crop (wheat, carrots, potatoes, beetroots, melon/pumpkin stems, torchflower, pitcher_crop)
  is judged too: it wants `farmland`, and dirt/grass/coarse dirt/path/air there is `missing`; farmland where a crop is wanted is `missing`, not
  `wrong`; block states such as growth age and orientation are not available from the column decoder and are not judged), `{:block b}` (exact block), `{:palette [b ...]}` (any of them),
  `{:blueprint name}` (a `:structure` element only), `{:air true}` (keep clear).
- A region element covers every cell of its `:region`; `:kind :border` only its outer ring (per y layer). Other kinds (`:plot`, `:path`,
  `:feature`, `:area`, ...) are free labels. `:structure` takes the cells of a blueprint of the library (`/api/blueprints`): offset
  (0, 0, 0) is the anchor corner at `:at` (the layer `y 0`), turned clockwise seen from above by `:rotation` 0/90/180/270; blueprint air
  cells mean "keep clear", `@solid` any non-liquid block. An unknown blueprint is an error on the element.
- `:kind :plan` nests another plan by `:ref`; its cells roll up as one element (weighted by its cell count). A cycle or unknown ref is an
  error on the element. The child may lie outside the parent's `:region`.
- Cell status: `match`, `missing` (air where something is wanted), `wrong` (another block), `extra` (a block where air is wanted),
  `unknown` (the column was never dumped). Percent = match / all cells (unknown included). A cell wanted by two elements counts in both;
  the grid shows the later element's. An element may have at most 200000 cells.
- An invalid file (bad EDN, id not equal to the file name, bad region or content, duplicate element ids) is not listed as a plan; its
  problems are returned in `errors` of `/api/plans` and shown on the Plans page.
