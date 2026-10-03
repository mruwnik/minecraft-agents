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
