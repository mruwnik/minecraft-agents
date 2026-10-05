# World fixtures

Live test cases as data. Each case gets a fresh 32x32 plot of a reserved grid on the test server (x/z 20000..20640,
floor at y 149, body level y 150), is built, run with one probe body and judged from the body's event log and a few
RCON checks. Run them with `node tools/world-test.mjs` (runner: `dashboard/src/world_test/`, build
`tools/compile dashboard world-test`; unit tests `tools/compile dashboard world-test-unit && node dashboard/out/world-test-unit.cjs`).

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
`--time-log` as local ISO with offset, naming `--card ID`; without it such a case is skipped), `--results FILE` (all results as EDN). Each run leases its plot (a file per plot index in `<tmpdir>/world-test-plot-leases/`, created exclusively, released after the run, a dead PID's lease reclaimed): runners started together never share a plot, and `--first-plot` is only where the search starts. Exit code 0 when every run
passed. The runner refuses to start when another player is within 500 blocks of the grid's centre or the body already
runs. The body is started (`--fresh`) once per distinct `:register` and stopped (SIGTERM to its own child) at the end.

Per run: forceload the plot, kill every non-player entity in it, clear it to air up to `:plot :height`, lay the
floor; build `:blocks`; write `:plans`; put the body at its start (survival, cleared, healed, fed, inventory,
effects, spawn point); wait `:settle-s`; note the event log's end (t0); run `:act`; poll the log until every
`:expect` is decided (or `:limit-s`); run the `:after` checks; then `jobs.mjs cancel-all`, kill the plot's
entities, clear the body, delete the plans, remove the forceload.

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
| `:time` | `:day`, `:night` or `:any` | `:day` |
| `:register` | the body's scenario register (triggers), as in `engine/scenarios/*.edn` | `[]` |
| `:plot` | `{:height 2..31 :floor "block"}` | `{:height 16 :floor "stone"}` |
| `:blocks` | `[:fill a b "block" (:hollow/:outline/...)]`, `[:set p "block[state]"]` | `[]` |
| `:plans` | plan maps (`:id`, `:parts`); written as `worlds/<world>/plans/test-<body>-<id>.edn`, deleted after | `[]` |
| `:body` | `{:at p :inventory [["item" n]] :effects [["effect" s amp]] :spawnpoint p :settle-s 3}` | at the middle |
| `:act` | steps, in order (below) | `[]` |
| `:expect` | event expectations (below) | `[]` |
| `:after` | RCON checks after the expectations are decided (below) | `[]` |
| `:limit-s` | the longest a run may watch the log | 120 |

Act steps: `[:summon "type" p "{NBT}"]` (the runner adds `Tags:["wt"]` and `PersistenceRequired`), `[:job (spec) [:now]]`
(submitted with `engine/tools/jobs.mjs`; its id joins the run's jobs), `[:wait-s n]`, `[:await pattern s]` (wait for an
event since t0, error after s), `[:kill-body]`, `[:time-set ticks]` (needs `--allow-time`), `[:rcon "text"]` (with
`$BODY $X $Y $Z` = the body and the plot origin).

Expectations: `{:event pattern :within-s n}` passes when a matching event is logged within n s of t0;
`{:no-event pattern :for-s n}` passes when none is logged for n s, or, with `:until pattern`, until the first event
matching that (if it comes within n s). `:of-job true` only counts events of the run's submitted jobs (their chain
starts with one). Patterns are partial: a map matches a map holding at least its keys (recursively), a set any of its
members, `[:> n] [:>= n] [:< n] [:<= n]` numbers, `[:near p r]` a position within r, `[:contains "s"]` a substring,
`[:not p]`, `[:any]` anything present; a vector of patterns matches a sequence of that length; else equality.

After checks: `[:block p "block[state]"]` and `[:not-block p "block"]` (`execute if block`), `[:body-near p r]`,
`[:item "name" n-or-[:>= n]]` (count in the body's inventory), `[:entities "selector args" [a b] n-or-[:>= n]]`
(entities in the box a..b, e.g. `"type=cow,tag=wt"`).

What does not fit: natural terrain (forests, caves, far walks, unloaded land) stays in the go-to soak; cases needing
two bodies or another player; cases judged by looking (the viewer).
