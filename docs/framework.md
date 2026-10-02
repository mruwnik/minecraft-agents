# One task framework for the body: actions, goals, roles

Design, 2026-10-02. Owner's request: "This needs to be cleaned up. Can you get someone to design a coherent framework
for all of this?" This document maps what exists, weighs three designs, recommends one, and lays out the migration.
Nothing here is implemented.

## 1. What exists today

Five mechanisms share the job of "make the body do something for a while", each with its own syntax, loop and
stop rules.

**Actions.** Primitives are the `long` and `quick` tables (src/body/actions/tables.mjs), filled from one module per
help section under src/body/actions/ (`goto` at src/body/actions/move.mjs:97, `sleep` at src/body/actions/self.mjs:16)
and from the runtimes (src/body/runtimes.mjs). Composites are one module per verb under `library/`, registered at
src/bot.mjs:33-47 and run by `runComposite` (src/body/runner.mjs:278-288) through the api built in `makeApi`
(src/body/runner.mjs:72-276). Every `api.act` counts failures per action name (:88-112); `api.checkpoint`
(:162-215) sleeps the night when a bed is near and otherwise hands the body back for the reasons in
`handBackReason` (src/lib/composite.mjs:42-53): spoken to, health, food, failed twice, inventory full, night with
no bed, `days`, `count`, `until` minutes. Farm composites classify their own failures with regexes
(src/farm/attention.mjs:7-17) and emit `farm_attention` (:40-54). Some composites carry their own day loop:
`farm.maintain days=` waits for dusk and dawn itself (library/farm/maintain.mjs:418-428), `flock.maintain days=`
likewise, `blueprint.build` stops at dusk. Each `./mc` call is one job: the durable shelf (src/job-shelf.mjs)
serialises them, holds the queue on any failure (src/job-scheduler.mjs:105,118) and on every restart
(src/job-shelf.mjs:22-39), and a foreground action supersedes the running task (AGENT_GUIDE.md:364).

**Flows.** `run steps=` (src/body/actions/control.mjs:15-48) accepts either a JSON list, run as a plain sequence
(src/flow.mjs:117, :385), or an EDN program of `seq`/`action`/`when`/`any` with read-conditions over a fixed
observation list (src/flow.mjs:19, :23-70, :278-383). No loop, no retry, abort on first failure, 3600 s cap
(src/flow.mjs:8). The EDN half pulls in the `edn-data` dependency and 300 lines of tests; until this morning nothing
in `library/`, `roles/` or the guide used it, and now one caller does (the routine's `until=`, below). The JSON
sequence is what drivers use ("one call, not five", AGENT_GUIDE.md:33).

**Routines.** `library/routine.mjs` is itself a composite: a flat JSON step list run once per game day
(:150-178), sleeping at a bed within 32 blocks or walking to its own bed within `bed_range` (:99-117), re-running a
`kit` step when a tool wears out (:59-66), writing `routine_day` and `routine_stopped` events whose advice text
lives in src/routine.mjs:60-117. `until` is two things: a number is the runner's minutes
(src/lib/composite.mjs:51), EDN text is a goal read after each day's steps (library/routine.mjs:33, :140-151, merged
as 6dc85b4 this morning). `days=0` has to be deleted from the shared args so the runner's own day rule does not end it
(:38-39). The stuck watch reads routine-specific progress fields (src/navigation/stuck.mjs:4, :115-135).

**Roles.** `roles/<role>/ROLE.md` is prose for the driver; `roles/<role>/*.json` are routine step lists with
`$place`, `$store`, `$places` and `vars=` substitution (src/lib/composite.mjs:88-102). The farmer's whole day is one
step (roles/farmer/homestead.json). Nothing binds a role to a body; the driver chooses.

**Reflexes.** Flee and fight (src/body/reflexes.mjs), the bedtime reflex that sleeps an idle body after nightfall
(src/body/bedtime.mjs:59-93, rule in src/cli.mjs:162), the stuck watch (src/body/stuck-watch.mjs), the respawn plan
(src/body/connection.mjs:499-516), hunger and health stops inside composites via `handBackReason`. Watches
(src/body/watches.mjs:13-44) fire `watch_hit` every 5 s without a driver. `./mc wait` (tools/mc.mjs:45-102) blocks on
events.jsonl and wakes for the types in `wakeWorthy` (src/cli.mjs:84-88).

**In flight.** `routine-goal` merged as 6dc85b4 while this was written: the routine's `until='<EDN>'` goal, a
`chest_count` read that walks to the chest (src/body/actions/sense.mjs:202), `bed=x,y,z`, `library/bake.mjs` and
roles/farmer/bread.json, documented at AGENT_GUIDE.md:215. `scratchpad/queue-resume` had no diff when read; the
brief describes it as holding the queue only on death, item loss or a job failing twice, auto-restoring after
reconnect, and capping `jobs`/`events` output. Both are mechanics the framework absorbs (sections 4 and 5).

**Measured this session:** 52% of driver calls were polling, 1554 of 2252 jobs were primitives, the queue was held
362 times. The driver is the loop, and every loop iteration costs tokens.

## 2. Three candidate designs

**A. Goals as code only.** Every long-running goal is a JS composite under `library/goal/` that loops until its own
condition; routines, flows and role JSON are deleted; the driver picks a composite and its arguments. This is the
smallest engine (none) and the most testable (fakeApi, test/helpers.mjs:5). Its cost is that every new goal, and every
variation ("the same, but also bake", "the same, but come home under 20 bread") is a code change by the owner, so the
driver cannot compose a one-off from existing verbs, and the "one call, not five" sequence still needs a data form.
It also leaves each composite re-implementing days, beds, re-kit and digests, which is the duplication we have now.

**B. One data unit, the goal, over code actions.** Actions stay JS. One JSON document, the goal, says what must
become true and which actions to run each round until it is; one engine runs it, owns nights, retries, digests and
persistence; routines, EDN flows and role step lists are absorbed into it. The driver starts a goal in one line and is
woken only when it ends or cannot progress. Cost: a new engine (about the size of today's routine plus flow condition
code, both of which it replaces) and one more vocabulary to learn, kept to a handful of fields.

**C. Extend the routine.** Build on the merged `routine-goal`: `until=<EDN>`, `chest_count`, more steps, more
roles. Least work now. But the routine remains a composite nested inside the composite runner, so days live in three places
(runner, routine, `farm.maintain`), nights in two, failure policy in four, and the EDN surface grows for one caller.
It does not survive a restart: the shelf marks the job interrupted and holds the queue (src/job-shelf.mjs:27-39), and
a driver has to notice and restart it. The polling and holds that cost 52% of calls stay.

**Recommendation: B.** It is the only one of the three that removes mechanisms rather than adding one: flows, routines
and role JSON become one thing; `days=`, `until=` and the day loops leave the composites; the queue stops holding on
goal failures because the goal's steps are not jobs. A rejected B-variant kept the EDN condition language. The goal
needs six facts and three comparators; a Lisp with `read` paths and the `edn-data` dependency is more surface than
that, so conditions become small JSON objects (section 4) and the EDN parser is deleted.

## 3. The model in one breath

An **action** is one verb the body does now, written in JS (a primitive in `src/body/actions/`, a composite in
`library/`); it returns one result line or fails with a reason, and it does one round of its work, never a day loop.
A **goal** is a JSON document saying what must become true (`until`) and which actions to run each round until it is;
while live it is `state/agents/<name>/goal.json`, and templates ship in `roles/<role>/<name>.goal.json`.
A **role** is a folder `roles/<role>/` holding `ROLE.md` for the driver and the goal templates it ships; `./mc goal
start name=farmer/bread` binds one to this body, and that binding is the `role` field of the live goal.
A **reflex** is what the body does unasked (eat, flee, fight, sleep when idle, dig out, hole up); the goal engine is
written on the assumption that reflexes interrupt it, and resumes after them.

Where each lives: actions in `src/body/actions/` and `library/`; the engine in `src/goal/`; the live goal in
`state/agents/<name>/goal.json`; templates in `roles/`; reflexes in `src/body/reflexes.mjs`, `src/body/bedtime.mjs`
and `src/survival/`.

## 4. The goal document

```json
{
  "name": "bread",
  "role": "farmer/bread",
  "cadence": "daily",
  "vars": { "farm": "mruwnik-farm", "store": "114,71,-107", "bed": "mruwnik-bed" },
  "until": { "fact": "chest", "at": "$store", "item": "bread", "gte": 576 },
  "bed": "$bed",
  "home": "$farm",
  "guards": [ { "when": { "fact": "carried", "item": "bread", "lt": 4 }, "do": "kit" } ],
  "steps": [
    { "action": "kit", "food": 12, "chest": "$store" },
    { "action": "farm.maintain", "place": "$farm", "deposit": "$store", "reserve_for": "$farm" },
    { "action": "bake", "store": "$store", "keep": 16 }
  ]
}
```

**Fields.** `until` is the stop condition, read after every round; when it reads true the goal is done. `cadence` is
`daily` (one round per game day: run the steps, wait for dusk, sleep, next round at dawn) or `continuous` (the next
round starts when this one ends). `steps` run in order; a step is an action name plus its arguments with `$var`
substitution (the routine's `$place`/`$store` rule, src/lib/composite.mjs:78-79, generalised to any var and to
`$var.field` paths). `bed` is a mark name or `x,y,z`, or `carry` (place the carried bed at nightfall), or absent
(nearest bed within 32, else the body's own `kind=bed` mark within 200, else carry, else hole up). `home` is where a
`home` guard walks to. `guards` are conditions read before every step; `do` is `home` (abort the round, walk home,
start the next round), `kit` (run the goal's kit step now, then continue), `pause` (stop and wake the driver).

**Step fields.** `when` (a condition; a false one skips the step and the round digest says so), `foreach` (`{"var":
[values]}` expands the step once per value, values may be objects reached as `$var.field`), `save` (the step's result
is kept as `$<name>` for later steps of the same round), `once` (round one only), `on_fail` (`continue`, the default;
`retry` once more after a re-kit; `stop` ends the goal and wakes the driver). A `kit` step is special as it is in the
routine today: the tools it lists are watched through the round and a step that wears one out gets the kit again and
one more try (library/routine.mjs:54-61 moves to the engine unchanged).

**Conditions.** `{"fact": <name>, ...args, "<op>": value}` with ops `eq`, `ne`, `lt`, `lte`, `gt`, `gte`; a
`{"var": "search.found", "gte": 1}` form compares a saved result; `{"all": [...]}`, `{"any": [...]}`, `{"not": ...}`.
A fact that cannot be read (chest unreachable, chunk unloaded) is unknown: `all` with an unknown is unknown, the
round digest says what could not be read, and three rounds of the same unknown escalate. This is
`evaluateFlowCondition`'s three-valued rule (src/flow.mjs:243-252) rewritten over objects instead of tagged arrays.

| fact | arguments | answers | reads |
|---|---|---|---|
| `carried` | `item` | count in the pockets | `api.inv()` |
| `chest` | `at` (x,y,z or storage mark), `item` | count in that chest; walks there when out of reach | `chest_contents` (src/body/actions/sense.mjs:26), as `chest_count` (:202) does |
| `have` | `item`, `at` | carried plus chest | both of the above |
| `farm` | `place` | `bare`, `ripe`, `planted` counts over the plan's crop cells | plan cells against `api.block` (the counts `farm.maintain` already derives) |
| `build` | `place` | `missing` block count | `blueprint.check` |
| `place` | `kind` or `name`, `by` | `exists`, `count` | `api.places()` |
| `near` | `mob` or `block`, `within` | count | the watch counter (src/body/watches.mjs:19-32), already pure over the entity list |
| `time` | | `day` (boolean), `tick`, `round` | `api.clock()` |
| `vitals` | | `hp`, `food` | `state` |

Facts are the only way a goal reads the world. They are functions over the composite api in `src/goal/facts.mjs`, so
they are tested with the same fakeApi as composites.

## 5. The engine and the supervisor

The engine is one composite, `goal.run`, in `library/goal/run.mjs`, kept internal (not in `./mc help`'s catalogue)
and driven by the steering verbs below. One round: for each step, read the guards, read `when`, substitute vars, call
`api.act`, record the outcome into goal.json, `api.checkpoint()`. After the round: read `until`; done, or
`cadence` decides whether to wait for dusk. Nights are the routine's night rule (library/routine.mjs:99-117) with the
walk-back leg deleted: the next round's steps walk to their own places, as the routine's own note already admits
(:95). The engine never loops a step itself: an action that wants retries (`mine.get` rounds, `forage.search` legs,
`farm.maintain`'s dig walk) keeps them, because they need the world state only the action has.

The supervisor is a 10-second timer in `src/body/bedtime.mjs`, beside the bedtime reflex (:59). It reads goal.json and, when
the status is `running`, no job is active, the queue is empty and not held, submits a `goal.run` job. That one rule
resumes the goal after a body restart, a reconnect (the scheduler's `abandon`, src/job-scheduler.mjs:207-219), a death
and respawn, a driver's foreground command, and a hold the driver cleared. The job shelf's restart hold
(src/job-shelf.mjs:27-39) exists so physical work is never replayed; a goal round is not physical work to replay, every
step rereads the world, so a hold whose interrupted owner was `goal.run` is released by the supervisor. Ordinary jobs
keep the `queue-resume` policy.

While a goal is live, a foreground `./mc <action>` interrupts the goal at once through the scheduler's existing
`interrupt` (src/job-scheduler.mjs:153-167), runs, and the supervisor resubmits the goal, which resumes its round at
the step after the last completed one. This replaces "any foreground action supersedes the routine" (AGENT_GUIDE.md:364)
and the `stop`-then-restart dance the guide recommends (:210). `./mc stop` pauses the goal and takes the body back.

## 6. Failure policy

| class | examples | engine does | escalates when |
|---|---|---|---|
| transient navigation | no path, search ran out of time, no first move (src/composite.mjs:19-25) | the step is skipped this round and counted as failed; a composite that owns a dig walk already retried | the same step fails 3 rounds running (`goal_stalled`) |
| resource shortage | no seed, no hoe, kit_short, storage_full, chest_missing, untillable, no water (src/farm/attention.mjs:8-17) | the action skips and reports as now; the round digest carries it; a `kit` guard refills food | the same shortage 3 rounds running, or `until` cannot move for 3 rounds |
| reflex took the body | fleeing, fighting, surfacing, digging out, holed up (`interrupted: fleeing from zombie`) | wait for the reflex to clear (`isReady`, src/body/jobs.mjs:178), re-run the same step, up to 3 times in a round | 3 interruptions of one step in one round |
| night | no bed within 32 | bed walk within range, else carried bed, else hole up and wait for dawn | a walk, a placement and a hole all fail (`goal_paused: night`) |
| hunger | food at the floor with something edible | the eat reflex eats; the engine only waits | nothing edible carried and no `kit` step can fix it (`goal_paused: food`) |
| disconnect, restart | kicked, body restart, duplicate login yield | nothing: the supervisor resubmits when the body is back | never |
| hard | death, kit lost on death, somebody's ground, a protected zone, spoken to by a human, TypeError, goal.json unreadable | the goal is paused with the reason and advice | always, at once (`goal_paused`) |
| rule | `on_fail: stop` on a step, a `pause` guard | the goal is paused | always |

The classifier is one pure module, `src/goal/policy.mjs`, built from the three regex sets that exist today
(src/composite.mjs:19-25, src/farm/attention.mjs:7-17, src/body/runner.mjs:88-112) so that farm composites and the
engine stop disagreeing about what is recoverable. `handBackReason` keeps spoken, health, food, failed twice, inventory
full and night; it loses `days`, `count` stays for `hunt`/`mine.get`, `until` minutes go.

## 7. Persistence

`state/agents/<name>/goal.json` holds the document plus: `status` (`running`, `paused`, `done`), `started`, `round`,
`step` (index of the step in flight), `outcomes` (the last round's result line per step), `failed` (`[{round, step,
why}]`, the stuck watch's `failedSteps` renamed), `unknown` (facts that could not be read, per round), `paused_why`,
`digest` (the last one-line digest). It is written with the shelf's tmp-and-rename (src/job-shelf.mjs:41-47) after
every step and every round. A driver rotation changes nothing: the new driver runs `./mc goal` and gets the digest.
Nothing of the goal lives in the job shelf except the current `goal.run` job's id. `events.jsonl` keeps the history
(`goal_started`, `goal_round`, `goal_paused`, `goal_resumed`, `goal_stalled`, `goal_done`); goal.json is the state.

## 8. Steering

`./mc goal` is one quick action whose bare word is the verb (the CLI already parses a bare word as `topic`,
src/cli.mjs:195-202), so nothing new is needed in `tools/mc.mjs`:

```
./mc goal                                       # the digest, one line
./mc goal start name=farmer/bread vars='{"farm":"mruwnik-farm","store":"114,71,-107"}'
./mc goal start until='{"fact":"carried","item":"oak_log","gte":64}' steps='[{"action":"tree.harvest",...}]'
./mc goal pause | resume | stop                 # stop ends it and writes goal_stopped
./mc goal set until='...' | vars='...'          # edit the live goal; takes effect next step
./mc goal log last=5                            # the last round digests
./mc goal dry name=farmer/bread vars=...        # the expanded steps, nothing runs
```

The digest: `goal bread round 4 until=chest bread 410/576 last=farm.maintain ok harvested=38 | bake ok baked=12
next=dusk status=running`. `state` (src/body/actions/sense.mjs:67-102) gains a `goal:` field with the same line, so the dashboard
shows it beside `doing=` with no new API. The dashboard popup's whisper line and in-game whispers accept the verbs
`goal`, `goal pause`, `goal resume`, `goal stop` and are answered by the body, not the driver, when the sender is the
owner or dashboard; any other whisper wakes the driver as today.

Wake-ups: `goal_done`, `goal_paused` and `goal_stalled` are wake-worthy (src/cli.mjs:84-88); `goal_round` is written
with `notify: false`, so a driver on `./mc wait` sleeps through an ordinary day. Each wake event carries one `advice`
line in the style of src/routine.mjs:59-71. The lead's watcher spawns a driver on `goal_paused`, `goal_stalled`,
`died`, `body_down` and `stuck`, replacing `routine_stopped` in harness/claude-code.md:17 and AGENT_GUIDE.md:73.

What the driver LLM is still for: turning the owner's one line into a goal (choose a template, fill vars, or write
`until`+`steps`), answering people, deciding on an escalation (fetch iron, build a chest, change the goal), and
planning across goals (bread first, then villagers). It is never the loop.

## 9. Worked examples

**(a) Bank nine stacks of bread** is the document in section 4. Day by day: kit takes 12 food from the store;
`farm.maintain` harvests, replants and deposits; `bake` turns the store's wheat into bread and puts it back, keeping
16 loaves; the engine reads the store's bread count, sleeps at `mruwnik-bed`, and stops at 576 with `goal_done
read=chest bread 576/576`. A hoe that breaks mid-sweep triggers the re-kit and one more sweep. A full chest is
`storage_full` in the digest for one day and `goal_stalled` on the third.

**(b) Find villagers within 800 blocks and mark the village.**

```json
{ "name": "find-village", "role": "forager/village", "cadence": "continuous",
  "vars": { "home": "mruwnik-farm", "store": "114,71,-107" },
  "until": { "fact": "place", "kind": "village", "by": "me", "exists": true },
  "bed": "carry", "home": "$home",
  "guards": [ { "when": { "fact": "carried", "item": "bread", "lt": 20 }, "do": "home" } ],
  "steps": [
    { "action": "kit", "food": 40, "chest": "$store" },
    { "foreach": { "heading": ["north", "east", "south", "west"] }, "save": "search",
      "action": "forage.search", "mob": "villager", "heading": "$heading", "radius": 800, "steps": 64, "minutes": 30,
      "origin": "$home" },
    { "when": { "var": "search.found", "gte": 1 }, "action": "mark", "name": "village", "kind": "village",
      "note": "villagers seen at $search.at" }
  ] }
```

Each round is four spokes from home; `forage.search` keeps its own legs and route retries. At nightfall between legs
the engine places the carried bed, sleeps, picks it up at dawn (the reflex's pickup rule, src/body/bedtime.mjs:65-68). When
bread falls under 20 the guard aborts the round, walks home, and the next round kits 40 bread again; if the store cannot
supply 40, `kit_short` is in the digest and three such rounds pause the goal. A `mark` by this body with `kind=village`
ends it. `forage.search` has to report `at=` for its first sighting; today it reports `found=` as a count (library/
forage/search.mjs:58), so the position goes into the result as one more field.

**(c) Build the fenced annex from a plan.**

```json
{ "name": "annex", "cadence": "continuous",
  "vars": { "place": "mruwnik-annex", "supply": "114,71,-107" },
  "until": { "fact": "build", "place": "$place", "missing": 0 },
  "steps": [
    { "once": true, "action": "blueprint.build", "name": "hut-annex", "x": 120, "y": 71, "z": -110, "place": "$place",
      "supply": "$supply", "partial": true },
    { "once": true, "action": "protect", "name": "$place", "place": "$place" },
    { "action": "kit", "tools": "stone_axe,stone_shovel", "food": 12, "chest": "$supply" },
    { "save": "build", "action": "blueprint.build", "place": "$place", "supply": "$supply", "partial": true },
    { "foreach": { "need": "$build.missing" }, "when": { "var": "need.count", "gt": 0 },
      "action": "mine.get", "block": "$need.block", "count": "$need.count" },
    { "foreach": { "need": "$build.craft" }, "action": "craft", "item": "$need.item", "count": "$need.count" }
  ] }
```

Round one lays the plan down and protects it (`protect` needs a `place=` form that reads the footprint from the
blueprint mark; today it takes a box). Every later round builds what it can, and `blueprint.build`'s `missing=`
becomes two lists: raw blocks to mine and items to craft from what is carried. `foreach` over a saved result's list is
what makes this a data goal rather than a new composite. The fence is part of the blueprint. Night stops
`blueprint.build` at dusk today (library/blueprint/build.mjs:6); under the engine it hands back at its checkpoint, the
engine sleeps, and the next round resumes from the world, which is what `partial=true` already does.

**(d) Plant beetroot, carrot and potato once seeds exist.**

```json
{ "name": "root-crops", "cadence": "daily",
  "vars": { "farm": "mruwnik-farm", "store": "114,71,-107" },
  "until": { "fact": "farm", "place": "$farm", "bare": 0 },
  "steps": [
    { "foreach": { "seed": [ { "crop": "beetroot", "item": "beetroot_seeds" }, { "crop": "carrot", "item": "carrot" },
                             { "crop": "potato", "item": "potato" } ] },
      "when": { "fact": "have", "item": "$seed.item", "at": "$store", "lt": 8 },
      "action": "farm.get_seeds", "crop": "$seed.crop", "count": 16, "place": "$farm" },
    { "action": "farm.maintain", "place": "$farm", "deposit": "$store" }
  ] }
```

"Once seeds exist" is the `when` on the seed step: with eight or more of a seed in hand or in the store the step is
skipped, and `farm.maintain` sows generic beds from what is carried (roles/farmer/ROLE.md, mixed beds). The goal ends
when the plan has no bare crop cell. A crop whose seed cannot be found (no wild carrots within range) is
`farm.get_seeds`'s "two rounds that bring nothing back" in the digest, and `goal_stalled` after three days.

## 10. Migration

Each phase leaves the live body working after merge and is a branch of its own.

**Phase 1, the vocabulary (about +350, −250 lines).** New pure modules under `src/goal/`: `document.mjs` (parse,
validate, `$var` substitution, `foreach` expansion, `once`), `condition.mjs` (section 4's ops over facts and vars,
three-valued), `facts.mjs`, `policy.mjs` (the three regex sets joined), `digest.mjs` (the one line, round events,
advice). Tests first, all through fakeApi and plain data. `./mc goal start|dry` write and expand goal.json; `./mc
goal` reads it; nothing moves the body yet. Deleted: the EDN half of `src/flow.mjs` and the `edn-data` dependency;
`run steps=` keeps the JSON sequence as 15 lines in `src/body/actions/control.mjs`; test/flow.test.mjs shrinks to the sequence tests.
Of the merged `routine-goal`: `bake` and `bed=x,y,z` stay, `chest_count` becomes the `chest` fact, and the routine's
`until=` reads the new JSON condition (`untilGoal` in src/routine.mjs calls `src/goal/condition.mjs`), so the EDN
parser can go in this phase and AGENT_GUIDE.md:215 changes its one example.

**Phase 2, the engine (about +600 lines).** `library/goal/run.mjs`, the supervisor timer, `goal` verbs `pause`,
`resume`, `stop`, `set`, `log`, the `goal_*` events and their wake rules, the `state` field, interrupt-and-resume. The
routine's night rule and re-kit move into the engine; `library/routine.mjs` keeps working beside it. The bread goal goes
live on the engine and runs for two days before phase 3.

**Phase 3, the deletions (about −700, +150 lines).** `library/routine.mjs`, `src/routine.mjs`, `routineSteps` and
its tests, `days=`/`until=` from every composite and from `handBackReason`, the day loops in `farm.maintain` and
`flock.maintain`, `blueprint.build`'s dusk stop, the routine fields of the stuck watch (now goal fields),
`roles/*/*.json` renamed to `*.goal.json` in the new form. AGENT_GUIDE.md and harness/claude-code.md say "goal" where
they say "routine"; `tools/incidents.mjs` reads `goal_paused` and `goal_stalled`.

**Phase 4, steering surface (about +150 lines).** Whisper verbs, the dashboard popup's pause and resume buttons
through `/api/whisper`, the driver brief (.claude/agents/body-driver.md) rewritten around `./mc goal`, and a
re-measurement of polling calls, primitive jobs and holds against this session's numbers.

## 11. Open questions for the owner

1. A foreground `./mc <action>` while a goal runs: interrupt and resume (recommended here), or keep today's rule
   that it supersedes the running task and the goal stays paused until `goal resume`?
2. Conditions as JSON objects, deleting the EDN surface and the `edn-data` dependency, against the routine's
   just-merged EDN `until=`: is dropping that investment acceptable?
3. One live goal per body (recommended; `goal start` replaces the live one and says so), or a stack where an
   expedition can interrupt the bread goal and hand back to it?
4. Who may steer by whisper without waking the driver: the owner's names in config.json and the dashboard only, or
   any player?
5. Sleeping out: must a goal say `bed: carry` to place beds away from home (recommended, since it leaves beds and
   marks across the map), or is it the default when no bed is in range?
6. When a `home` guard fires and the store cannot refill (no bread at home either), the goal pauses and the driver
   decides. Should a `farmer/food` goal template exist so the driver's answer is one line?
7. The round digest goes to events.jsonl and goal.json. Should it also be appended to the agent's journal.md, so a
   rotated driver reads the week in one place?
