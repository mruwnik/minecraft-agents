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

Options: `--body` (default `ProbeFixture`, created and whitelisted when missing), `--world` (default `test`),
`--repeat N` (runs each case N times, each run on its own plot), `--first-plot I` (plot index to start at),
`--allow-time` (a case whose `:time` is not the server's may `time set 14000`/`1000`; every set is appended to
`--time-log` as local ISO with offset, naming `--card ID`; without it such a case is skipped), `--results FILE` (all results as EDN), `--changed-since-pass` (skips a fixture file that fully passed at a revision recorded in `.claude/world-test-passes.edn` when no file of the jobs/triggers it names, transitively, changed since; any engine, js, trigger-default or runner change, or its own file, runs it; every full pass records), `--retry-failed N` (reruns each failed case up to N times after the batch on the same body; a pass on a retry is `:flaky`, counted apart, never `:pass`, exit 0, the first failure kept under `:first-failure`). With `TEST_EVENTS=1` it also prints live-tests `@@test` lines (plan, phase, one result per case and run, progress per fixture). Each run leases its plot (a file per plot index in `/tmp/world-test-plot-leases/` (one set for the machine, whatever TMPDIR), created exclusively, released after the run, a dead PID's lease reclaimed): runners started together never share a plot, and `--first-plot` is only where the search starts. Exit code 0 when every run
passed. The runner refuses to start when another player is within 500 blocks of the grid's centre or the body already
runs. The body is stopped (SIGTERM to its own child) and started again (`--fresh`, and its own `engine/memory.edn` and `seen.bin` deleted, since `--fresh` only drops `engine.edn`; the plot it was last left on is cleared first) before every case, so no case sees another's memory (`:slept`, `:futile`, ...); `:keep-memory true` opts out. Between cases it waits as a spectator 100 blocks above its plot, out of sight, so it never remembers a plot from before its blocks were built. It is stopped at the end.

Per run: set the body's goal (`goal.edn`, shown on the dashboard) to `world-test <id>: <expectations> (run N, retry M)`, cleared when the run ends; forceload the plot, kill every non-player entity in it, clear it to air up to `:plot :height`, lay the
floor; build `:blocks`; write `:plans`; put the body at its start (survival, cleared, healed, fed, inventory,
effects, spawn point); kill hostiles in the plot (at night also natural ones within 24 blocks of it, then every 5 s
until the case ends); put the `:register`; wait `:settle-s`; kill hostiles again; note
t0; run `:act` (its `:await` steps, like `:expect`, count from just before the register); poll the log until every
`:expect` is decided (or `:limit-s`); run the `:after` checks; then `jobs.mjs cancel-all`, kill the plot's
entities, clear the plot again (so the next restarted body does not start beside this case's hut or bed), put the body on it, clear the body, delete the plans, remove the forceload.

## Format

A file is one case map, or `{:defaults {...} :cases [{...} ...]}`. A case over the defaults: `:body` and `:plot` merge
key by key; `:blocks :act :expect :after :plans` append; the rest replace. The case id is `<file stem>/<:name>`.

Positions in `:blocks`, `:body`, `:act` summons and `:after` are plot-relative `[x y z]` (origin = the plot's
north-west corner at body level; the floor is y -1; the middle is `[16.5 0 16.5]`). Inside job specs, plans and event
patterns write `#at [x y z]` (absolute `[x y z]`) or `#xyz [x y z]` (absolute `{:x :y :z}`). A two-element `#at [x z]` / `#xyz [x z]` is a column (`[X Z]` / `{:x :z}`) for go-to's column goals. `#seg {:axis :x :y 0 :line 16 :from 6 :to 26}` is a strip segment with plot-relative y, line and from/to (x or z along the axis), made absolute with its `:id`.

| key | meaning | default |
|---|---|---|
| `:name` | case name (string) | required |
| `:tags` | keywords for `--tag` | none |
| `:time` | `:day`, `:night`, `:night-exclusive` or `:any`; day/night cases hold a time lock shared by phase (same phase together, the other waits; `:night-exclusive` (cases that sleep: a night skip ends every other night case) holds it alone, or only with the cases of its `--night-batch` (the pool's `--night-batches`: cases that only need a night share one rolling night after one `time set 14000` (a freed body starts the next case while its recorded duration still fits; a batch case reads the clock and re-sets the night when it is not night, and keeps the batch's start as its queue place, so a later day waiter cannot slip in between cases); `:sleep-skip true` (needs the vanilla skip) cases run in their own batch; a case with one `:time-set` step joins a barrier batch with the cases that start alike and set the same time (all meet at the step, one sets it), any other `:time-set` case runs alone; failed cases rerun batched the same way; a `:day` case with a `:time-set` to 11500 or later is exclusive but starts at day (its batch sets 1000), a `:night` case with any `:time-set` is exclusive starting at night; a batch of sleepers sets gamerule `players_sleeping_percentage` 101 (old value recorded first in `~/.local/state/minecraft-claude/world-test-sleep-rule.edn`; restored at batch end, pool exit or signal, or by the next pool run / `tools/time-set.mjs` after a crash; all logged; an unreadable read sets nothing; a read of 101 counts as 100); `--list --night-classes` prints each case's class); with `--allow-time` every case after the first re-sets the time (logged) when it left the phase window (day: >= 11000; night: outside 13000-22000); `tools/time-set.mjs` takes it too), `:any` joins the current phase (with `--allow-time` it takes the day lock like an untimed case), except that a `:weather` `:rain`/`:thunder` case runs alone and leaves the time of day as it is; `--phase day\|night\|night-exclusive` runs one time class only (case level; `:any` and untimed are day) | `:day` |
| `:weather` | `:rain`, `:thunder` or `:clear`: the case runs alone (server-wide weather), under `--allow-time` the runner sets it before and `clear` after, logged in `--time-log`; skipped without `--allow-time` | clear |
| `:register` | the body's scenario register (triggers), as in `engine/scenarios/*.edn`; put on the body (triggers put) only after the plot, time and body are in place | `[]` |
| `:time-why` | one of `:sleep :bees :villager-schedule :sunlight-burn :dawn-decision :weather`: why the case needs the real clock. A case that sets `:time` other than `:any` needs it, else the `fixtures` check fails ("use light": a dark or lit plot with `:time :any`); cases still without are listed in `engine/fixtures/time-allow-list.edn`, which only shrinks (the check fails for a listed case that stopped offending) | absent |
| `:memory` | `[{:kind :food-source :data {:pos #xyz [x y z] ...}} ...]` (optional `:policy {:cap :ttl}`, `:age-s n` backdates the entry by n seconds): entries written into the body's `engine/memory.edn` before it starts (so before the register is put), `:t` = now; appends to the defaults; not with `:keep-memory`. Night cases also get a `:log-out` `unsupported` entry (other bodies sleep in the same night; the night job would log out for them), unless the case mentions log-out or keeps memory. Memory holds what the body has seen: also build the matching block in `:blocks` | `[]` |
| `:zones` `:places` | `[{:name :min [x y z] :max [x y z] :owner "Other" (`"$body"`: the probe body) :allow #{..}}]` / `[{:name :kind :pos [x y z] :note}]`, plot-relative (no `#at`): merged into `worlds/<world>/zones.edn` / `places.json` before the register is put (zone named `wt-<body>-<name>`, marker `:by wt-<body>`; marker names are shared by every shard: write `$tag` (the body's lowercase tag) into them, also in job specs), the runner waits (up to 4 s) for the body's `world.reloaded` event, and removed after the case on every exit path (this body's leftovers also at start); edits take the agent tools' `<file>.lock` | `[]` |
| `:settings` | `{:ns/key value}`: written as the body's `settings.edn` before it starts (removed after), e.g. `{:engine.perception/save-ms 1000}` so seen memory is saved every second; keys must be namespaced; not with `:keep-memory` | none |
| `:keep-memory` | `true`: the body's `engine/memory.edn` is not deleted before this case, and when a case follows another in the same register group the body is not restarted (it goes on with the memory it has); for cases that test memory across runs | `false` (every case starts a restarted body with its `memory.edn` deleted) |
| `:mobs` | `:keep`: the runner does not kill hostile mobs in or (at night) near the plot (each run, `--repeat` too); for danger cases that rely on mobs already there | absent (hostiles near the plot are killed) |
| `:plot` | `{:height 2..31 :floor "block"}`; `:length` (x, 16..1024) and `:width` (z, 16..64) make a large plot, leased from 16 lanes south of the grid (z 20704 + 96 j, never overlapping it; setup clears the lane's full 64-wide floor first); `:isolate true` first clears the 32-block gap round the plot to air up to y 31 (for cases that must not see a neighbour's leftovers); keep each `:blocks` fill under 32768 blocks | `{:height 16 :floor "stone"}`, 32x32 |
| `:blocks` | `[:fill a b "block" (:hollow/:outline/...; a one-layer :outline is a ring, not a slab)]`, `[:set p "block[state]"]` | `[]` |
| `:plans` | plan maps (`:id`, `:parts`); written as `worlds/<world>/plans/test-<body>-<id>.edn`, the runner waits for the body's `world.reloaded` of each file; deleted after; `"$body"` anywhere in a plan or job spec becomes the running body (plan `:metadata :by`); a job-spec string `"$plan:<id>"` becomes that plan's id | `[]` |
| `:body` | `{:at p :yaw deg (0 south, 180 north) :pitch deg (0 level, positive down) :inventory [["item" n]] :effects [["effect" s amp]] :spawnpoint p :settle-s 3}` | at the middle |
| `:act` | steps, in order (below) | `[]` |
| `:expect` | event expectations (below) | `[]` |
| `:after` | RCON checks after the expectations are decided (below) | `[]` |
| `:limit-s` | the longest a run may watch the log | 120 |
| `:idle-grace-s` | a job case whose jobs have all ended is judged this many seconds later (early end); positive number, a file default applies to its cases | 3 |

Act steps:
- `[:summon "type" p "{NBT}"]` (the runner adds `Tags:["wt"]` and `PersistenceRequired`), `[:rcon "text"]` (with `$BODY $X $Y $Z $BOX` = the body, the plot origin and the plot box; every `@e` selector must be a plot box, e.g. `kill @e[type=zombie,$BOX]`).
- A summoned entity's `UUID` must be unique across all fixture cases: the pool runs cases on neighbouring plots at once, and a second summon with a taken UUID is dropped (the job then finds another plot's mob).
- A case that summons a hostile mob (`:act` `[:summon ...]`) gets a mob plot: every third plot of every third row (rows 0-8, 64 blocks apart, beyond sight), its setup clears 32 blocks of air round it so no mob walks over or spawns near; plain cases use every third plot of every third row from row 10 (28 plots, also 64 blocks apart: neighbours never see each other's chests, torches or mobs). The body turns survival only once it stands at its start; a setup `hurt` by suffocation fails the case as a setup error.
- `[:job (spec) [:now]]`: submitted with `engine/tools/jobs.mjs`; its id joins the run's jobs. A string `"$entity-id:<type>"` in the spec becomes the numeric id of the nearest mob of that type the body sees (entities.mjs, polled 10 s).
- `[:await pattern s]`: wait for an event since t0, error after s (`:of-job true` in the pattern: only jobs submitted by earlier steps count; `:after-previous true`: only events after the one the previous `:await` found, to wait for a repeat). `[:until check s]`: poll a `:memory` / `:file` check or a `:cli` step every 0.5 s until it passes, error after s. `[:until-seen {:entity "cow" :min 2} s]` / `{:block "wheat"}`: `:until` over the body's entities / seen blocks, true once one is seen. `[:until-rcon cmd regex s]`: sends cmd every 0.5 s until the reply matches the regex (e.g. `"data get entity $BODY foodLevel"`), error with the last reply after s. `[:until-effect-gone "hunger" s]`: polls the body's active_effects until the effect has shown and then ended (error if never seen: a bare negative `:until-rcon` pattern returns on an empty reply). `[:until-pos {:x-min 20 :y-max 3} s]`: polls the body's Pos until it lies inside the bounds (offsets from the plot origin; keys :x-min :x-max :y-min :y-max :z-min :z-max). Prefer these to `[:wait-s n]`.
- `[:rcon-until "cmd" {:until pattern :every-s n :limit-s n}]`: sends cmd every n s until an event matching pattern is logged (or limit-s passes, no error); for shoves that must outlast a job.
- `[:kill-body]`, `[:time-set ticks]` (needs `--allow-time`), `[:restart-body]` (stops the body, starts it keeping memory, position, the queue and the register).
- `[:cli "tool" [args] pattern?]`: runs `engine/tools/<tool>.mjs` with `$body $world $job $event-job` (last awaited event's job) `$plan:<id>` `$tag` filled in, in the pattern too (an unfilled id fails the step); a map argument is printed as EDN, a position vector spread; the printed EDN must match the pattern, else exit 0.
- `[:http op arg pattern?]`: `:submit spec [flags]` (its id becomes `$job` and joins the run's jobs), `:cancel id`, `:cancel-all` through jobs.mjs (flags `[:next]`, `[:now]`, `[:exclusive]`, ...).
- `:memory` pattern: the body's `engine/memory.edn` is `{:entries {kind [{:t :wt :data {...}}]} :policies {...}}`, so match `{:entries {:deaths [{:data {:pos [:any]}}]}}`.

Expectations: `{:event pattern :within-s n}` passes when a matching event is logged within n s of t0;
`{:no-event pattern :for-s n}` passes when none is logged for n s, or, with `:until pattern`, until the first event
matching that (if it comes within n s). `{:count-event pattern :for-s n :at-least a :at-most b}` passes when the number of matching events in the n s from t0 (or `:from-s`) lies in range (above `:at-most` fails at once; `:at-least` counts as a positive anchor). `:of-job true` only counts events of the run's submitted jobs (their chain
starts with one); on a `:no-event` it scopes the `:until` event too. Patterns are partial: a map matches a map holding at least its keys (recursively), a set any of its
members, `[:> n] [:>= n] [:< n] [:<= n]` numbers, `[:near p r]` a position within r, `[:contains "s"]` a substring, `[:has p]` a list with one element matching p,
`[:not p]`, `[:any]` anything present; a vector of patterns matches a sequence of that length; else equality.

After checks: `[:block p "block[state]"]` and `[:not-block p "block"]` (`execute if block`), `[:body-near p r]` and `[:body-far p r]` (body within / at least r blocks from p),
`[:item "name" n-or-[:>= n]]` (count in the body's inventory), `[:memory pattern]` and `[:file "path" pattern]` (the body's `engine/memory.edn` / a file under its directory, read as EDN, must match the pattern), `[:entities "selector args" [a b] n-or-[:>= n]]`
(entities in the box a..b, e.g. `"type=cow,tag=wt"`).

What does not fit: natural terrain (forests, caves, far walks, unloaded land) stays in the go-to soak; cases needing
two bodies or another player; cases judged by looking (the viewer).
