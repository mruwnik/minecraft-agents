# MVP live trial

First run of the engine body against the real server, 2026-10-03, branch `engine-mvp`.

## Environment

- **Agent:** ClaudeProbe, world `claude`. No config names an auth mode, so the default (offline) applied. Chosen because its config calls it a temporary test body.
- **Server:** 127.0.0.1:25565, a Paper server behind the owner's ViaProxy, reachable by a TCP connect. Nothing was started or restarted.
- **Build:** `npm install` clean. `npm test` before the trial: 55 cljs tests (170 assertions) and 110 node tests, all green. After the fixes: 57 cljs tests (177 assertions), 110 node tests, all green.
- **Scenario:** `scenarios/woodcutter.edn`: register hostile-near, health-low and night-and-bed-known; queue harvest-wood (oak, radius 16) then deposit.
- **Runs:** three full body runs (about 45 s, 90 s and 3.5 min), each from a fresh state directory after the first. No run reached the 15 minute limit, because the behaviour was identical and idle after about two seconds.

## What the body did

1. Connected and spawned at (-79, 70, -40). Registered the three reflexes, queued harvest-wood (j1) and deposit (j2).
2. Deposit was marked blocked at once with a warn, because no chest is known. Correct.
3. Harvest-wood walked a few blocks to (-77, 69, -38). The nearest oak is at (-74, 75, -31) and birch at (-79, 74, -44), about 5 blocks above the bot on a ledge. A separate probe showed `moveTo` returning `blocked` (no path, as the pathfinder is not allowed to dig or build). `dig` returns `unreachable`.
4. After three failed rounds fell-tree emitted `tree_blocked` and gave up with done. The collect child found nothing and finished. The plant child then found a replant debt at the unfelled tree but no sapling and returned not-ready.
5. From then on harvest-wood stepped one round every 0.25 s, each one yielding not-ready, for as long as the body ran (about 4 rounds per second, two events each, roughly 90 KB of events.jsonl per minute). The queue never completed and deposit never ran.

## Triggers

None fired. No hostile came within 8 blocks, health stayed full, there was no known bed or chest, and the trial happened in a short window. The cut path (a trigger interrupting a running round) was therefore not exercised live. It is covered only by the stub tests.

## State files left

Under `state/agents/ClaudeProbe/engine/` (gitignored): `engine.edn` (list, register, cursor), `body.json` (restart and disconnect records), `common.json` (one replant debt for the unfelled oak), `jobs/j1.json` (fell and collect marked done), and `events.jsonl`. Restart recovery was exercised once by accident: the second run resumed the persisted list and register without a `started` event, and emitted `restored` instead.

## Warn and error events

| event | reading |
|---|---|
| warn blocked, deposit | No chest in common memory. Expected. |
| warn tree_blocked, fell | The tree cannot be reached on foot. Real world condition, correctly reported. |
| warn failed, j1 (run 2 only, fixed) | A filtered collect round threw on an item entity with no resolvable stack and the whole job was dropped. |

No error-level events. stderr held only JVM warnings once the first bug was fixed.

## Bugs found

Fixed, with tests, in commits 5021b39 and 085e02a:

- **Deprecation spam.** `entities()` read `entity.objectType`, which logs a stack trace on every access. It produced 216,000 stderr lines in 45 seconds. Now uses `displayName`.
- **Partial walk fell through to dig.** `dig-up!` treated a `partial` move as arrival, dug out of reach and counted a failure. Now it returns and the round continues.
- **Null item crash.** `collect-drops` with a filter dereferenced a null `item`. `droppedItem` returned null live for an item entity, which probably means `getDroppedItem` did not resolve it (about 60% confident the cause is metadata not yet parsed). The job now treats an unknown stack as not wanted. The primitive itself still returns null, which deserves a look.
- `plant-sapling-ready?` now tolerates `blockAt` returning null for an unloaded chunk (defensive, no failing test could be built because the scheduler already guards preconditions).

Not fixed, too big for a small patch:

- **Stalled not-ready busy loop.** A parent whose child returns not-ready forever is stepped every tick. There is no backoff, no wake condition and no ceiling, so the event file grows without bound and the body does no useful work. The design has a job yield "wake me when X". Here the harvest composite returns the child's bare not-ready with no wake condition.
- **Stale replant debt.** The debt is committed when a tree is chosen, before anything is felled. A tree that is never felled leaves a debt that blocks the plant step and, through it, the whole composite.
- **No reachability check in tree choice.** `find-tree` picks by distance and leaf proximity, not by whether a path exists. A ledge tree wins over a reachable one. Nothing falls back to the next candidate after `tree_blocked`.
- **Queue starvation.** Because j1 never finishes or sleeps, j2 and anything after it never get a turn, though they are on a round-robin list.

## Verdict

The plumbing matches `docs/design.md`: connection, token ownership, primitives, event envelope, scenario loading, persistence and restore, precondition blocking and bounded failure counting all behaved as described against a real server. The scheduling story did not hold in the one case that came up: a job with nothing to do spun instead of yielding, and starved the list. Triggers and cuts remain untested live. Confidence that the design is sound as written: about 70%. Confidence that this MVP is correct in the live cut and reflex paths: about 35%, because they were never exercised.

A better next trial would use a world position with a reachable tree, a known chest and bed, and a spawned hostile, and would add wake conditions to not-ready returns first.

## Second run

2026-10-03, branch `engine-mvp`, same agent (ClaudeProbe), world `claude`, server probed first (TCP connect up, no body running). Engine state under `state/agents/ClaudeProbe/engine/` was deleted before each run. Logs are in the scratchpad `trial2/` directory. Build before the run: 75 cljs tests (240 assertions), 121 node tests, all green.

### Fixes made (commits 46a4ac7, b0369bb, 7f6583b, 2aea754, fd885cb, 2dc50f4)

- **Spin, root cause.** `settle-listed!` booked a not-ready round with whatever wake the round returned, usually none, so the job stayed ready. With deposit blocked by its precondition, round-robin had only one ready job and picked it on every 250 ms tick. `step-child` also returned a bare status, so a composite lost a child's wake. Now a not-ready round with no wake gets a persisted not-before time (default 5 s, engine option `:min-recheck-ms`) that readiness checks, and `step-child` passes the child's wake up.
- **Tree choice.** `fell-tree` records a tree as unreachable in job memory when the walk is blocked or a dig says unreachable, and picks the next candidate. It warns and finishes only when none is left. The replant debt is recorded only once the first log is dug.
- **Dropped item.** `droppedItem` now handles every slot shape (prismarine item, network slot with or without `present`, missing count, unknown id gives name `unknown`). I could not read a live item entity this time, so the exact live shape is unconfirmed (about 55% that this covers the observed null).
- **New:** job `look-around`, trigger `every-interval` (cooldown persistence, timestamp in body memory, so it survives restart), scenario `woodcutter-cuts.edn`. The trigger `:when` now receives the entry's args as a third argument. Later also a job `pace` and scenario `pace-cuts.edn`, see below.

### What the body did

Three runs.

1. **woodcutter-cuts.edn, first run (about 100 s).** Look-around fired at once (seq 8). Harvest rounds now ran at 5 s spacing (seq 11, 14, 16) instead of four per second, and deposit was blocked once (seq 13, no chest known). After three dig failures `tree_blocked` (seq 17) and harvest completed (seq 18). The every-interval reflex fired again at 45 s intervals (seq 19, 22) but nothing was running to cut. That was the pre-fix `fell-tree`, which gave up on the first unreachable tree.
2. **woodcutter-cuts.edn, after the dig-unreachable fix (about 100 s).** The body went through the oak candidates one round at a time (eight rounds, about 25 s), marking each unreachable, then warned `no reachable tree` (seq 27) and completed (seq 28). No spin, the round rate was sane. A read-only probe then showed the cause: the body stands in a closed cell (stone walls and an obsidian ceiling), the pathfinder is not allowed to dig, and `moveTo` is blocked toward every tree, so no tree in this world position is reachable by any logic. The harvest cannot run here at all.
3. **pace-cuts.edn (about 200 s).** Because harvest cannot run in the cell, I added a harmless long job `pace` (walk between two points inside the cell, three laps per round) and a scenario that puts `every-interval 20` first. This is not the harvest cut the brief asked for, but it exercises the same engine path with a real long round.

### Triggers fired

In the pace run, `every-interval` fired 11 times, at 20 s intervals until a hostile appeared (seq 6, 14, 19, 24, 29, 34, 39, 44, 49, then 59 and 83). `hostile-near` fired 5 times from about 179 s (seq 54, 64, 69, 74, 79), so a real hostile came within 8 blocks of the cell at dusk. Each retreat ended `completed_not_cleared`, and the 5 s cooldown re-fired it. `health-low` did not fire.

### Cut and resume evidence

- Fired seq 14 interrupting j1 round 3, cut seq 15 with `cause` 14, reflex round started seq 16, ended seq 17 (`cleared`), then j1 round 4 started seq 18. That is the resume.
- The same sequence repeated: fire 19, cut 20, resume 23 and so on for fire 24 and 29 and 34 and 39 and 44 and 49.
- The hostile-near cuts follow the same shape (fire 54, cut 55, retreat round 56, ended 57, j1 round 12 at seq 58).
- Rounds 1 and 2 of pace finished (seq 10, 12 yielded). After the cut in round 3 each resumed round hung in `moveTo` until the next cut, because one pace point sits against the cell wall and the walk times out after 20 s. So the resumed round started every time but made little progress. This is a flaw of the trial job, not of the engine.
- Round count over 202 s was 33 rounds in total, including reflex rounds.

### Deposit

Deposit was never runnable: it is blocked by its precondition because no chest is known (seq 13 in the woodcutter runs). It was not queued in the pace run. I did not verify the "deposit gets its turn" requirement live. Starvation is covered by the unit test (two ready jobs, one backing off).

### Bugs remaining

- **Shutdown drops the running job.** On SIGTERM the primitives are closed, the in-flight round throws the cut error, and `settle-listed!` treats `:cut` as a failure: warn `failed` (seq 87) and the job and its memory are deleted. A restart therefore loses the interrupted job. A cut that comes from closing the body should keep the job. Not fixed.
- **Harvest is unprovable in this cell.** The test body is walled in. The scenario should be re-run from an open position.
- **`droppedItem` live shape unconfirmed**, see above.
- Mid-round `moveTo` with a wall-adjacent target burns its whole timeout. Not an engine bug.

### Updated verdict

The scheduling fix held live: a not-ready job is stepped at the re-check interval and the list moves on. Reflex cuts and resumes work against a real server for both a clock reflex and a world reflex (confidence about 80%). The design is sound as written (about 75%). The MVP is correct for the harvest path under live conditions: still unknown (about 40%), because no tree was ever reachable. The next trial needs an open spot with trees, a known chest and a restart in the middle, and the shutdown-drop bug should be fixed first.
