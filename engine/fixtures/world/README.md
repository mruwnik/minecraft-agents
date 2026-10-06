# World fixtures

Live test cases as data. Each case gets a fresh 32x32 plot of a reserved grid on the test server (x/z 20000..20640,
floor at y 149, body level y 150), is built, run with one probe body and judged from the body's event log and a few
RCON checks. Run them with `node tools/world-test.mjs` (runner: `dashboard/src/world_test/`, build
`tools/compile dashboard world-test`; unit tests `tools/compile dashboard world-test-unit && node dashboard/out/world-test-unit.cjs`). Before starting the body it runs `tools/compile engine body` when `engine/out/body.cjs` is older than a file under `engine/src` or `engine/js`. A setup or block reply saying a position is not loaded rebuilds the plot (up to 4 tries), then the case ends as a setup error.

```
node tools/world-test.mjs                                  # every file here, once
node tools/world-test.mjs engine/fixtures/world/herd-pen.edn --repeat 2
node tools/world-test.mjs --tag hostile --match glass --results /path/results.edn
node tools/world-test.mjs --tag night --allow-time --time-log <shared time log>
node tools/world-test.mjs --list
```

Options: `--body` (default `ProbeFixture`, created and whitelisted when missing), `--world` (default `claude`),
`--repeat N` (runs each case N times, each run on its own plot), `--first-plot I` (plot index to start at),
`--allow-time` (a case whose `:time` is not the server's may `time set 14000`/`1000`; every set is appended to
`--time-log` as local ISO with offset, naming `--card ID`; without it such a case is skipped), `--results FILE` (all results as EDN). With `TEST_EVENTS=1` it also prints live-tests `@@test` lines (plan, phase, one result per case and run, progress per fixture). Each run leases its plot (a file per plot index in `<tmpdir>/world-test-plot-leases/`, created exclusively, released after the run, a dead PID's lease reclaimed): runners started together never share a plot, and `--first-plot` is only where the search starts. Exit code 0 when every run
passed. The runner refuses to start when another player is within 500 blocks of the grid's centre or the body already
runs. The body is stopped (SIGTERM to its own child) and started again (`--fresh`, and its own `engine/memory.edn` and `seen.bin` deleted, since `--fresh` only drops `engine.edn`; the plot it was last left on is cleared first) before every case, so no case sees another's memory (`:slept`, `:futile`, ...); `:keep-memory true` opts out. It is stopped at the end.

Per run: forceload the plot, kill every non-player entity in it, clear it to air up to `:plot :height`, lay the
floor; build `:blocks`; write `:plans`; put the body at its start (survival, cleared, healed, fed, inventory,
effects, spawn point); kill hostiles within 32 blocks; put the `:register`; wait `:settle-s`; kill hostiles again; note
t0; run `:act` (its `:await` steps, like `:expect`, count from just before the register); poll the log until every
`:expect` is decided (or `:limit-s`); run the `:after` checks; then `jobs.mjs cancel-all`, kill the plot's
entities, clear the plot again (so the next restarted body does not start beside this case's hut or bed), put the body on it, clear the body, delete the plans, remove the forceload.

## Format

A file is one case map, or `{:defaults {...} :cases [{...} ...]}`. A case over the defaults: `:body` and `:plot` merge
key by key; `:blocks :act :expect :after :plans` append; the rest replace. The case id is `<file stem>/<:name>`.

Positions in `:blocks`, `:body`, `:act` summons and `:after` are plot-relative `[x y z]` (origin = the plot's
north-west corner at body level; the floor is y -1; the middle is `[16.5 0 16.5]`). Inside job specs, plans and event
patterns write `#at [x y z]` (absolute `[x y z]`) or `#xyz [x y z]` (absolute `{:x :y :z}`).

| key | meaning | default |
|---|---|---|
| `:name` | case name (string) | required |
| `:tags` | keywords for `--tag` | none |
| `:time` | `:day`, `:night`, `:night-exclusive` or `:any`; day/night cases hold a time lock shared by phase (same phase together, the other waits; `:night-exclusive` (cases that sleep: a night skip ends every other night case) holds it alone; `tools/time-set.mjs` takes it too), `:any` takes none | `:day` |
| `:register` | the body's scenario register (triggers), as in `engine/scenarios/*.edn`; put on the body (triggers put) only after the plot, time and body are in place | `[]` |
| `:memory` | `[{:kind :food-source :data {:pos #xyz [x y z] ...}} ...]` (optional `:policy {:cap :ttl}`): entries written into the body's `engine/memory.edn` before it starts (so before the register is put), `:t` = now; appends to the defaults; not with `:keep-memory`. Memory holds what the body has seen: also build the matching block in `:blocks` | `[]` |
| `:keep-memory` | `true`: the body's `engine/memory.edn` is not deleted before this case, and when a case follows another in the same register group the body is not restarted (it goes on with the memory it has); for cases that test memory across runs | `false` (every case starts a restarted body with its `memory.edn` deleted) |
| `:mobs` | `:keep`: the runner does not kill hostile mobs within 32 blocks of the plot before `:act` (each run, `--repeat` too); for danger cases that rely on mobs already there | absent (hostiles near the plot are killed) |
| `:plot` | `{:height 2..31 :floor "block"}`; `:length` (x, 16..1024) and `:width` (z, 16..64) make a large plot, leased from 16 lanes south of the grid (z 20704 + 96 j, never overlapping it; setup clears the lane's full 64-wide floor first); keep each `:blocks` fill under 32768 blocks | `{:height 16 :floor "stone"}`, 32x32 |
| `:blocks` | `[:fill a b "block" (:hollow/:outline/...)]`, `[:set p "block[state]"]` | `[]` |
| `:plans` | plan maps (`:id`, `:parts`); written as `worlds/<world>/plans/test-<body>-<id>.edn`, deleted after; a job-spec string `"$plan:<id>"` becomes that plan's id | `[]` |
| `:body` | `{:at p :inventory [["item" n]] :effects [["effect" s amp]] :spawnpoint p :settle-s 3}` | at the middle |
| `:act` | steps, in order (below) | `[]` |
| `:expect` | event expectations (below) | `[]` |
| `:after` | RCON checks after the expectations are decided (below) | `[]` |
| `:limit-s` | the longest a run may watch the log | 120 |

Act steps: `[:summon "type" p "{NBT}"]` (the runner adds `Tags:["wt"]` and `PersistenceRequired`), `[:job (spec) [:now]]`
(submitted with `engine/tools/jobs.mjs`; its id joins the run's jobs), `[:wait-s n]`, `[:await pattern s]` (wait for an
event since t0, error after s), `[:kill-body]`, `[:time-set ticks]` (needs `--allow-time`), `[:rcon "text"]` (with
`$BODY $X $Y $Z $BOX` = the body, the plot origin and the plot box; every `@e` selector must be a plot box, e.g. `kill @e[type=zombie,$BOX]`).

Expectations: `{:event pattern :within-s n}` passes when a matching event is logged within n s of t0;
`{:no-event pattern :for-s n}` passes when none is logged for n s, or, with `:until pattern`, until the first event
matching that (if it comes within n s). `:of-job true` only counts events of the run's submitted jobs (their chain
starts with one). Patterns are partial: a map matches a map holding at least its keys (recursively), a set any of its
members, `[:> n] [:>= n] [:< n] [:<= n]` numbers, `[:near p r]` a position within r, `[:contains "s"]` a substring,
`[:not p]`, `[:any]` anything present; a vector of patterns matches a sequence of that length; else equality.

After checks: `[:block p "block[state]"]` and `[:not-block p "block"]` (`execute if block`), `[:body-near p r]` and `[:body-far p r]` (body within / at least r blocks from p),
`[:item "name" n-or-[:>= n]]` (count in the body's inventory), `[:entities "selector args" [a b] n-or-[:>= n]]`
(entities in the box a..b, e.g. `"type=cow,tag=wt"`).

What does not fit: natural terrain (forests, caves, far walks, unloaded land) stays in the go-to soak; cases needing
two bodies or another player; cases judged by looking (the viewer).
