# Body engine (MVP)

The engine from `docs/design.md`, scoped by `docs/mvp-brief.md`. ClojureScript
for the engine, jobs and triggers; JavaScript for the mineflayer layer
("primitives").

## Toolchain

shadow-cljs 3.5.4 (ClojureScript 1.12.145), because a JVM is present. Async
code uses `^:async` functions with `await`; promesa is not used. `await` works
inside `let`, `loop`/`recur`, `cond` and `try` in an `^:async` fn, but not
inside a nested `fn`, which is a separate (non-async) function. An anonymous
async fn needs a name: `(fn ^:async round [ctx] ...)` compiles,
`^:async (fn [ctx] ...)` and `(fn ^:async [ctx] ...)` do not. Prefer
`(defn ^:async foo-round [ctx] ...)`.

```
cd engine
npm install
npm test                                   # cljs tests, then node --test js/**/*.test.mjs
npm run test:cljs                          # cljs only
npm run test:js                            # JS only
npm run body -- --agent <name> --scenario <file.edn>
```

Builds (`shadow-cljs.edn`): `:test` is a `:node-test` build to
`out/test.cjs`, picking up every namespace ending in `-test`; `:body` is a
`:node-script` build to `out/body.cjs` with `engine.main/main`.

Layout:

- `js/primitives.mjs` the real mineflayer layer; `js/connect.mjs` makes the bot; `js/stub-bot.mjs` is a bare stub bot for the primitive tests.
- `js/fake.mjs` a scriptable fake world with the same interface, for tests.
- `src/engine/` `core` (list, register, scheduler, act wrapper,
  call-child), `memory` (the body store), `events`, `ctx` (helpers checks and
  rounds call), `catalog` (every job and trigger by name), `scenario`, `main`.
- `src/engine/jobs/` job definitions; `engine.jobs.samples` holds small ones
  (`:go-to`, `:wait-for-day`, `:eat`, `:look-around`, `:pace`).
  `src/engine/triggers.cljs` holds the triggers. Register new jobs and
  triggers in `engine.catalog`.
- `scenarios/sample.edn` a scenario using the samples.
- `test/engine/` cljs tests. Test helpers live in `engine.test-util`.

The cljs side loads JS modules at runtime with `js/require` (Node 24 can
`require` an ES module without top-level await). From a compiled file in
`out/`, `(js/require "../js/fake.mjs")` resolves to `engine/js/fake.mjs`.

## Primitives

The JS layer exports one factory per module:

```js
// js/primitives.mjs
export async function createPrimitives ({ host, port, username, auth }) // resolves once spawned
// js/fake.mjs
export function createFake (worldSpec)
```

Both return a `primitives` object. The cljs side calls its methods through
interop, for example `(.moveTo p token #js {:pos #js {:x 1 :y 64 :z 2}})`. JS
values are never converted wholesale; read fields with `.-field` or `aget`.

### Conventions

- **Positions** are plain `{x, y, z}` objects. Block positions are integers.
- **Item and block names** are mineflayer registry names without the
  namespace: `oak_log`, `bread`.
- **Every acting method** is `async name(token, args)` and resolves to a plain
  object with a `status` string. A domain failure (blocked path, no item) is a
  status, never a rejection.
- **Rejections** happen only for a cut (`err.code === 'cut'`) and for
  programming errors (bad args, `err.code === 'bad-args'`). The engine treats
  any other rejection as a job failure.
- **Time bounds** below are hard. A method that reaches its bound stops what it
  was doing and resolves with `status: 'timeout'` (or `partial` for `moveTo`).
  Nothing in a primitive runs for minutes; long waits are declining checks.
- **Sensing methods** are synchronous, take no token, and never wait for a
  turn. Triggers and checks call them every tick, so they must stay
  cheap: scans are bounded by radius and `max`, and run on the JS side.

### Ownership token

```js
primitives.setOwner(token)   // sync; token is a string or null
```

The engine calls `setOwner` before each round with a fresh token and passes
that token to the round; jobs reach the primitives only through `ctx/act`. A call whose token is not the current owner rejects
at once with `code: 'cut'`. When `setOwner` changes the owner, every in-flight
call made with the old token stops what it was doing within one game tick
(pathfinder goal cleared, `stopDigging`, window closed, controls released)
and rejects with `code: 'cut'`. `null` means nobody may act.

`isOwner(token)` (sync) reports whether a token is current.

### Sensing (sync, no token)

| method | args | returns |
|---|---|---|
| `self()` | none | `{username, pos, health, food, foodSaturation, oxygen, onFire, inWater, inLava, isSleeping, experience, dimension, timeOfDay, isDay, held, inventory}`; see below |
| `entities(opts)` | `{radius = 16, kind?, names?, max = 32}`; `kind` is one of `hostile`, `passive`, `player`, `item`, `other` | `[{id, name, kind, pos, distance, item?, username?, sleeping?, creeper?}]` sorted by distance; see below |
| `blocks(opts)` | `{radius = 16, names?, match?, max = 64}`; `names` is an array of block names, `match` a JS predicate on the block name; with neither, every non-air block | `[{name, pos, distance}]` sorted by distance |
| `blockAt(pos)` | `{x, y, z}` | `{name, pos}`, or `null` when the chunk is not loaded |

`self()` fields:

- `health` 0..20 and `food` 0..20; `foodSaturation` is the hidden saturation (0..20).
- `oxygen` 0..20 bubbles (`bot.oxygenLevel`; 20 until the server reports air).
- `onFire`: bit `0x01` of the entity's shared-flags metadata byte. mineflayer 4.39 has no `onFire` field, so this
  reads `bot.entity.metadata[i]` where `i` is the index of `shared_flags` in the registry's `metadataKeys` for the
  entity (0 if the registry does not know it). The server only sends the byte when it changes, so it is accurate
  after the first update and false before it.
- `inWater`, `inLava`: `bot.entity.isInWater` / `isInLava`, which the physics plugin sets every tick; if physics
  has not run yet, whether the block at the feet is `water` / `lava`.
- `isSleeping`: `bot.isSleeping`.
- `experience`: `{level, points, progress}` (`progress` 0..1 through the current level; zeros before the server sends any).
- `dimension`: `bot.game.dimension` as the protocol version spells it (for example `overworld` or `minecraft:overworld`).
- `timeOfDay` is 0..23999, `isDay` is `timeOfDay < 12542 || timeOfDay > 23460`, `held` is an item name or null,
  `inventory` is `[{name, count, slot}]`.

`entities()` extras: `item` is `{name, count}` for dropped items. Players also carry `username` and `sleeping`
(the pose metadata field equals 2, found through the registry's `metadataKeys`, else index 6; the server sends it only
when it changes). Hostile classification is `entity.type === 'hostile'` (the registry's type) or a `kind` naming
hostile mobs; creepers additionally carry `creeper: true` (their `name` is `creeper` too).

Remembered places (a known bed, a known chest) are not primitives. They are
`:bed` and `:chest` entries in body memory; see `engine.memory/place` below.

### Acting (async, token first)

| method | args | statuses | bound | on cut |
|---|---|---|---|---|
| `moveTo(token, a)` | `{pos, range = 1, timeoutS = 20, maxDistance = 64}` | `arrived`, `partial` (bound or `maxDistance` reached, closer than before), `blocked` (no path, or no progress) | `timeoutS`, at most 60 | goal cleared, controls released |
| `dig(token, a)` | `{pos}` | `dug`, `missing` (air), `unreachable` (more than 4.5 away), `cannot` (unbreakable) | 10 s | `stopDigging` |
| `place(token, a)` | `{pos, item}` | `placed`, `occupied`, `no-item`, `no-support`, `unreachable` | 5 s | nothing placed after the cut |
| `collect(token, a)` | `{id, timeoutS = 10}` | `collected`, `gone`, `unreachable`, `timeout` | `timeoutS`, at most 20 | as `moveTo` |
| `inspectContainer(token, a)` | `{pos}` | `ok`, `missing`, `unreachable` | 5 s | window closed |
| `transfer(token, a)` | `{pos, direction, item, count}`; `direction` is `deposit` or `withdraw` | `ok` (`moved` may be less than `count`), `missing`, `unreachable`, `no-item`, `full` | 5 s | window closed |
| `equip(token, a)` | `{item, dest = 'hand'}`; `dest` is `hand`, `off-hand`, `head`, `torso`, `legs`, `feet` | `equipped`, `no-item` | 2 s | none needed |
| `eat(token, a)` | `{item?}`; without `item`, the best food carried | `ate`, `no-food`, `full` | 5 s | `deactivateItem` |
| `attack(token, a)` | `{id}` | `hit`, `killed`, `gone`, `out-of-reach` | 1 s (one swing) | none needed |
| `sleep(token, a)` | `{pos}` (a bed) | `sleeping`, `not-night`, `occupied`, `monsters-near`, `missing`, `unreachable` | 5 s | wake if asleep |
| `look(token, a)` | `{pos}` or `{yaw, pitch}` | `ok` | 1 s | none needed |
| `offline(token, a)` | `{ms = 300000}`, at most 600000 | `ok` (`ms` is the wait used), `cut`, `closed`, `unsupported` | `ms` plus the reconnect | see below |

Extra fields on the result:

- `moveTo`: `pos` (where the body ended), `distance` (to the target).
- `dig`: `block` (name dug), `drops` (`[{id, name, count, pos}]`, the item
  entities that appeared within 2 blocks during up to 1 s after the break).
- `place`: `block` (name placed).
- `collect`: `gained` (`[{name, count}]`).
- `inspectContainer`: `items` (`[{name, count, slot}]`).
- `transfer`: `moved` (count).
- `eat`: `item`, `food` (after eating).
- `attack`: `health` (target's, when known).
- `offline`: `ms`, the wait actually used (clamped to 0..600000, rounded down).

`offline` quits the bot, emits the body event `offline`, waits `ms` (default 5 minutes, hard maximum 10 minutes),
reconnects with the same connection params (up to 3 tries, 5 s apart), rebinds the library bot so every other
primitive and every listener works on the new bot, emits `online`, and resolves `{status: 'ok', ms}`. It does not
use the cut-on-token-change rule of the other primitives, because the body must never stay offline: a cut ends the
wait early, the body reconnects, and the call resolves `{status: 'cut'}`. `close()` during the wait or the reconnect
cancels it and resolves `{status: 'closed'}` (a bot the reconnect already produced is quit). If every reconnect try
fails it emits `disconnected` and rejects with the last error. Only `createPrimitives`, which owns the connection
params, supports it; `createPrimitivesFromBot` resolves `{status: 'unsupported'}` without touching the bot. A stale
token rejects with `cut` on entry and bad `ms` (not a number, negative) with `bad-args`.

Reach for `dig`, `place`, `inspectContainer`, `transfer`, `sleep` and
`attack` is the caller's job: walk there first with `moveTo` (`range` 2 to 3).
The primitives do not walk.

### Body events

```js
primitives.onBodyEvent(listener)   // returns an unsubscribe function
```

`listener` receives plain objects `{kind, ...}` for momentary events:
`hurt` (`health`, `food`, `cause?`), `died` (`pos`, `inventory`), `respawned` (`pos`, `dimension`), `chat`
(`from`, `message`), `woke`, `spawned`, `disconnected` (`reason`), `offline` (`ms`), `online` (`pos`). `died` is
emitted at the moment health reaches 0: `pos` is where the body died and `inventory` (`[{name, count, slot}]`) what
it carried, before the server clears it. `respawned` is emitted at the first `spawned` after the library's respawn
signal, so `pos` is the new position (and `dimension` the new dimension; a portal also counts as a respawn). `offline`
and `online` are the two ends of the `offline` primitive; after `online` the `bot` underneath is a new one. The engine
turns each into an entry of that kind in body memory (see Memory).

### Lifecycle

`close()` disconnects (the fake does nothing).

### The fake

`createFake(spec)` in `js/fake.mjs` returns the primitives plus a `world`
handle for tests. It has the same sensing fields as above (`spec.self` may set `oxygen`, `onFire`, `inWater`,
`inLava`, `isSleeping`, `foodSaturation`, `experience`, `dimension`; player entities default to `sleeping: false` and
`username` equal to `name`). Its `offline` flips `world.state.offline`, emits `offline` and `online`, and waits
`ms * spec.offlineScale` (default 0.001, so 5 minutes is 0.3 s) before resolving the same results. The handle:

```js
const p = createFake({
  self: { pos: { x: 0, y: 64, z: 0 }, health: 20, food: 20, username: 'Fake' },
  time: 1000,                                   // timeOfDay
  blocks: { '3,64,0': 'oak_log' },              // "x,y,z" -> name; anything else is air
  entities: [{ id: 7, name: 'zombie', kind: 'hostile', pos: { x: 5, y: 64, z: 0 }, health: 20 }],
  inventory: [{ name: 'bread', count: 2 }],
  containers: { '1,64,1': [{ name: 'cobblestone', count: 10 }] },
  drops: { stone: 'cobblestone' },              // block -> dropped item; default: the block itself
  unreachable: ['9,64,9'],                      // moveTo / collect targets reported as blocked
})
p.world.state            // the mutable world (self, time, blocks, entities, inventory, containers)
p.world.calls            // [{ name, token, args }] for every acting call, in order
p.world.hold('moveTo')   // the next moveTo call waits; returns release(result?)
p.world.override('dig', async (token, args, defaultImpl) => ({ status: 'cannot' }))
p.world.emit({ kind: 'hurt', health: 6 })   // delivered to onBodyEvent listeners
p.world.setTime(13000)
```

Fake semantics: `moveTo` jumps to the target if within `maxDistance`, else
moves `maxDistance` toward it and returns `partial`. `dig` removes the block
and adds an item entity at its cell. `collect` moves the item entity into the
inventory. `attack` takes 5 health per swing. `sleep` succeeds at night on a
cell whose block name ends in `_bed`, and sets the time to 0. `eat` raises
`food` by 5 and consumes one item. A held call rejects with `cut` when the
owner changes, exactly as the real layer must.

## Jobs

A job definition is a map of a name, a check and a round:

```clojure
{:name  :go-to                    ; keyword, unique in the catalog
 :check (fn [ctx] bool)           ; required; (constantly true) is fine
 :round go-to-round}              ; (defn ^:async go-to-round [ctx] ...) => :done | :continue
```

- **The check** says whether the job can usefully run now. It reads sensing
  and memory through the ctx, returns a boolean, and is cheap and side-effect
  free: its ctx has no token, so `act`, `update-mem!` and `remember!` throw.
  Checks are asked every tick; a declined job costs nothing. A check that
  throws declines (with a `system.error` warn). The engine refuses a job
  definition without a `:check`.
- **The round** returns `:done` (the job leaves the list and its memory is
  deleted) or `:continue`. Anything else, or a throw, drops the job with a
  `job.failed` warn. A cut is never a failure: the job stays listed with its
  memory.
- Long waits are not loops: a job waiting for daylight returns `:continue`
  and declines in its check until the sun is up (`:wait-for-day`).

### ctx

The single argument of a check and a round. Use the helpers in `engine.ctx`.

| helper | what |
|---|---|
| `(:args ctx)`, `(:id ctx)`, `(:primitives ctx)` | args, instance id (`"j4"`, or `"j4/fell"` for a child), the primitives for sensing |
| `(ctx/mem ctx)` | this job's memory map (its sub-map, for a child) |
| `(ctx/update-mem! ctx f & args)` | apply `f` to it, in RAM; saved by the next `act` or at round end. Throws `cut` if the round was cut |
| `(ctx/view ctx)`, `(ctx/now ctx)` | a body-memory view `{:data :now}`, the engine clock |
| `(ctx/latest ctx kind)`, `entries`, `since`, `count-in` | time-filtered body-memory reads (see Memory) |
| `(ctx/remember! ctx kind data policy?)` | append an entry to body memory |
| `(ctx/forget-where! ctx kind pred)`, `(ctx/forget-until! ctx kind t)` | drop entries whose data matches, or written at or before `t` |
| `(await (ctx/act ctx :moveTo #js {...}))` | call an acting primitive through the act wrapper |
| `(await (ctx/call-child ctx slot def args))` | one round of a child job (below) |
| `(ctx/check-child ctx slot def args)` | the child's check against its sub-map, for a parent's check |
| `(ctx/submit! ctx job-name args opts)` | put a peer job at the end of the list; returns its id |
| `(ctx/emit! ctx kind level fields)` | an event with `:source :job` |

**act.** Every acting primitive call goes through `act`. It checks the
ownership token (a stale token rejects with `cut` before the primitive is
called), saves memory, emits `action.started` (debug), calls the primitive,
saves memory again and emits `action.done` (debug) with the status. There is
no commit: a job updates its memory map and calls `act`, so a cut loses at
most the work since the last save. Write a debt or an intent with
`update-mem!` before the `act` it protects.

**call-child.** `(ctx/call-child ctx slot def args)` takes a slot keyword, a
job definition (the map, not a catalog name) and args. The child's memory is
the parent's `[:children slot]` sub-map, created as `{:args args :children {}}`
when missing. The engine runs the child's check against it (false resolves
to `:declined`, no round run), else one child round with the parent's token,
resolving to `:done` or `:continue`. The same slot resumes the same child
(its memory is kept after `:done`, for the parent to read); a new slot is a
fresh child. Children can call children, recursion included, with no depth
cap. A cut anywhere ends the whole chain's round. Cancel and done take the
subtree, since it lives inside the parent's memory. `submit!` is delegation:
a peer on the list, not a child.

## Memory

One EDN store per body, `memory.edn` under `state/agents/<name>/engine/`,
read and written with `cljs.reader` and `pr-str`, so keywords survive. It is
`{:entries {kind [entry]} :policies {kind policy}}`:

- An **entry** is `{:t wall-clock-ms :wt world-time :data ...}`, newest
  last. `:wt` is the body's `timeOfDay` when written. No provenance field.
- **Kinds** are an open vocabulary keyed by what the observation is about:
  `:hurt`, `:died`, `:chat`, `:restart`, `:bed`, `:chest`, `:looked`,
  `:forestry/replant`, `:job/j7`.
- **Policy** `{:cap n :ttl ms}` is passed with a write and stored beside the
  kind. A new kind written without one gets the default, cap 50 and ttl one
  hour; a later write without one keeps the kind's policy. Forever is the
  keyword `:forever`; an omitted ttl is an error, not forever. Forever kinds
  keep their cap. The cap drops the oldest.
- **Sweep**, on boot, every `:sweep-ms` (default 60 s) of ticks, and before
  every save: drops expired entries, kinds with no entries left, and any
  `:job/<id>` kind whose instance is not live (listed, or the running reflex
  job).
- **Saves** happen around every `act`, at every round end and whenever the
  engine writes (body events, `:restart`, listing, done, cancel).
- **Reads** (`engine.memory`) take a view `{:data :now}` and are
  time-filtered, so an expired entry never reaches a check or trigger:
  `entries`, `latest`, `since` (written at or after `t`), `count-in` (within
  the last `ms`), `policy`, and `place` (`(mem/place view :bed)` is the
  `:pos` of the latest `:bed` entry; places use `mem/place-policy`, cap 1,
  forever).
- **Body events** become entries of their kind (`:hurt` with
  `{:health :food}`, `:died`, `:chat`, ...). Every start appends `:restart`.
  The handling job clears them with `forget-until!` up to a timestamp, so a
  hit during the job is kept.
- **Job memory**: a listed (or running reflex) instance owns kind `:job/<id>`,
  cap 1, `:forever`, created when listed and deleted on done, cancel or drop.
  Its single entry's data is `{:args ... :children {slot child-map}}` plus
  the job's own keys (phase, debts, intents); children nest the same way.

## Triggers and the register

A trigger definition:

```clojure
{:name        :hostile-near
 :when        (fn [world view args] bool) ; view is a memory view; args are the entry's
 :job         :retreat                    ; default job (a catalog name)
 :args        {:radius 8}                 ; defaults, merged under the entry's :args
 :persistence :cooldown                   ; :retry | :cooldown | :stop
 :cooldown-s  5}
```

The register is an ordered vector of entries `{:id :trigger :job :args
:persistence :cooldown-s :builtin?}`; the id defaults to the trigger name and
`:args` is the trigger's defaults merged with the scenario's. The same args
go to the reflex job. The engine evaluates the effective order every tick and
fires the first entry whose `:when` holds and which is not muted or cooling
down.

- A firing reflex cuts a running listed job. It cuts a running reflex job
  only if it sits above that reflex. A reflex whose own job is running does
  not fire again.
- The cut listed job stays on the list and is the next to run (if its check
  passes) once no reflex holds the body. A cut reflex job is dropped; it
  fires again from the world if its condition still holds.
- A reflex job has no check (a trigger always fires); it keeps the body while
  its rounds return `:continue`. `:done` or a throw ends it. If the condition
  still holds, persistence decides: `:retry` fires again next tick,
  `:cooldown` waits `:cooldown-s`, `:stop` waits until the condition has been
  false once.
- Agent-only edits (functions in `engine.core`): `register-reflex!`,
  `remove-reflex!` (refused for built-ins), `mute!` (with TTL), `move!`
  (`{:above id}` or `{:below id}`, with TTL), `clear-change!`. Each property
  (mute, position) is at its default or under exactly one change; a new change
  replaces the old one; on expiry it reverts to the default and emits
  `reflex.reverted`. A move whose anchor is gone puts the reflex at the bottom.
  Jobs cannot reach these.

## The scheduler

`tick!` (every 250 ms from `start!`):

1. Expire register changes; sweep memory if `:sweep-ms` has passed.
2. Evaluate the register; a firing entry that preempts the holder cuts by
   rotating the ownership token and runs a round of its job.
3. Otherwise, if a round is in flight, nothing. A reflex job between rounds
   gets its next round.
4. Otherwise the list: a holding job runs if its check passes, else the body
   idles (holding keeps the body); else the cut job, if its check passes;
   else round-robin from the job after the last one run, to the next job
   whose check passes. Whatever a round returns, the list moves on. If every
   check declines, nothing runs until the next tick.

`submit!` and `cancel!` (agent) edit the list; `do-now!` (agent) cuts the
running listed job and submits at the front with `:hold? true`.

The list, register and changes are written to `engine.edn` on every change;
memory as above. On boot both are reloaded, reflex instances are dropped, the
in-flight job resumes first, and a `:restart` entry is appended.

**Shutdown** (`core/shutdown!`, called by `main` on SIGINT/SIGTERM before the
primitives close) rotates the token so the in-flight round's outcome is never
booked; the job stays on the persisted list with its memory. A primitive
rejecting with `cut` while the token is still current is also a cut, not a
failure.

**No progress.** After each round of a holding listed job, the engine
compares its act-call count and its memory with before the round. After
`:stall-rounds` (default 20) rounds with neither, it emits one `job.stalled`
warn with `:rounds`; any progress resets the count. Nothing is capped.

## Events

JSON lines, one per event, on stdout and appended to
`state/agents/<name>/engine/events.jsonl`. Envelope (from `docs/events.md`):

| field | what |
|---|---|
| `seq` | monotone per body, continues across restarts |
| `t` | wall time, ms since epoch |
| `body` | username |
| `source` | `job`, `reflex`, `action`, `body`, `world`, `system` |
| `kind` | for example `round_started`; the pair `source.kind` names the event |
| `level` | `debug`, `info`, `warn`, `error` |
| `job` | instance id owning the body, or null |
| `chain` | ids from the top-level job down to `job`, for children |
| `reflex` | reflex id when inside a reflex job |
| `round` | the job's round number |
| `pos` | integer cell of the body |
| `cause` | optional seq of the event that caused this one |
| `text` | optional one-line prose |

Plus kind-specific fields. Kinds the engine emits: `job.queued`,
`job.round_started`, `job.yielded` (a `:continue`), `job.cut`,
`job.completed`, `job.failed`, `job.cancelled`, `job.stalled` (warn),
`job.memory_written` (debug, `memory` is the kind), `action.started` and
`action.done` (debug), `reflex.fired`, `reflex.ended` (`how`: `cleared`,
`completed_not_cleared`, `dropped`), `reflex.changed`, `reflex.reverted`,
`body.<kind>` for body events, `system.started`, `system.restored`,
`system.stopping`.

## Scenarios

EDN, read with `cljs.reader`:

```clojure
{:register [{:trigger :health-low}                                  ; trigger defaults
            {:trigger :hostile-near :args {:radius 12}}             ; args merged over the defaults
            {:trigger :health-low :id :health-low-2 :job :eat
             :persistence :retry}]                                  ; overrides
 :queue    [{:job :go-to :args {:pos {:x 10 :y 64 :z 0}}}
            {:job :wait-for-day :hold? false}]}
```

`npm run body -- --agent <name> --scenario <file>` loads
`state/agents/<name>/config.json` (`username`, `world`), the world's
`state/worlds/<world>/world.json` (`host`, `port`), and `js/primitives.mjs`.
It refuses to start, with a message, when `js/primitives.mjs` does not exist.
If `state/agents/<name>/engine/engine.edn` exists the saved list and register
are restored and the scenario is ignored; pass `--fresh` to discard saved
engine state and start from the scenario (memory is kept; job kinds of the
discarded list are swept). The scenario is validated against the catalog
before connecting. `--state-dir <dir>` overrides the repo's `state/`.

`test/engine/scenarios_test.cljs` runs `woodcutter.edn` and `pace-cuts.edn`
end to end against the fake primitives.

## Job library

Jobs live in `engine.jobs.forestry`, `engine.jobs.storage`,
`engine.jobs.survival` and `engine.jobs.samples`, helpers in
`engine.jobs.util`. All are registered in `engine.catalog`. Every round
re-reads the world and does a bounded piece. Positions in memory are
`{:x :y :z}` maps. Failed rounds are counted in job memory as `:failures`;
after three the job emits a warn and ends.

| job | args | check | job memory | body memory |
|---|---|---|---|---|
| `:go-to` | `{:pos :range 1}` | always | `:blocked` count | none |
| `:wait-for-day` | none | it is day | none | none |
| `:eat` | `{:item?}` | always | none | none |
| `:look-around` | none | always | none | writes `:looked` (cap 1, forever) |
| `:pace` | `{:a pos :b pos :laps 3 :rounds 8 :range 1}` | always | `:rounds-run` | none |
| `:fell-tree` | `{:species nil :radius 16}` | a column is chosen, or every candidate was unreachable, or a tree (log column with leaves near its top) is in radius | `:column {:x :z}`, `:species`, `:base`, `:partials`, `:unreachable` | writes one `:forestry/replant` `{:pos base :species}` when the base log is dug |
| `:collect-drops` | `{:radius 16 :filter [names] or nil}` | always | `:skipped` ids of unreachable items | none |
| `:plant-sapling` | `{:at pos or nil :species nil}` | nothing to plant, or a matching sapling is carried and the spot holds no log | none | plants at the oldest `:forestry/replant` debt and forgets it |
| `:harvest-wood` | `{:species nil :radius 16 :filter nil}` | the current phase's child check | `:phase`, children in slots `:fell`, `:collect`, `:plant` | as its children |
| `:deposit` | `{:chest pos or nil :items [names] or nil}` | a chest is known (args or `:chest`) | none | reads `:chest` |
| `:retreat` | `{:radius 8 :step 8}` | always | `:moves` count | none |
| `:sleep` | none | a `:bed` is known and it is night | none | reads `:bed` |

- `:fell-tree` digs up to two logs per round of the chosen column, lowest
  first, and is done when the column has no logs. It walks with `moveTo`
  directly (range 3). A tree whose walk is blocked, partial three times in a
  row, or whose logs cannot be dug, is remembered as unreachable and the next
  candidate is chosen; with none left it warns `tree_blocked` and finishes.
- `:collect-drops` calls `collect` once per round for the nearest matching
  item entity (the primitive walks itself). Done when none match in radius.
- `:plant-sapling` without `:at` plants at the first debt (of `:species` when
  given), walks within 3, equips, places. `occupied` counts as planted.
- `:harvest-wood` is phase-driven: each round calls the current phase's child
  once (`:fell-tree`, then `:collect-drops` with the species' log, sapling,
  stick and apple, then `:plant-sapling`) and advances the phase when the
  child is done. Its check is the current child's check, so it declines
  (rather than spinning) while no tree is in sight or no sapling is carried.
- `:deposit` puts away every carried stack except tools and armour (suffixes
  `_pickaxe _axe _shovel _hoe _sword _helmet _chestplate _leggings _boots`,
  plus shears, bow, crossbow, fishing_rod, flint_and_steel, shield, trident),
  one stack per round. Saplings included: on a shared list with
  `:harvest-wood` it can take the sapling the replant needs. Warn kind
  `chest_unusable`.
- `:retreat` walks `:step` blocks directly away from the nearest hostile per
  round, at most five walks, then gives up with a `retreat_gave_up` warn.
- `:sleep` walks within 2 of the known bed and calls `sleep`. `sleeping` and
  `not-night` are done; a taken bed or nearby monster retries three times; a
  missing bed warns `bed_missing` and ends.

Triggers (`engine.triggers`; `:when` receives the world, a memory view and
the register entry's `:args`):

| trigger | holds when | job | persistence |
|---|---|---|---|
| `:health-low` | health at most `:health` (default 8) | `:eat` | cooldown 30 s |
| `:hostile-near` | a hostile within `:radius` (default 8); the same `:radius` goes to the job | `:retreat` | cooldown 5 s |
| `:night-and-bed-known` | not day and a `:bed` entry exists | `:sleep` | cooldown 60 s |
| `:inventory-nearly-full` | `:stacks` (default 30) or more carried stacks and a `:chest` entry exists | `:deposit` | cooldown 60 s |
| `:every-interval` | no `:looked` entry, or the latest is at least `:seconds` (default 60) old | `:look-around` | cooldown 0 |

`:every-interval` is a wall-clock reflex: `:look-around` looks at a point
three blocks ahead and writes `:looked`, which survives a restart and makes
the trigger stop holding. With no entry it fires at once.
`scenarios/woodcutter-cuts.edn` is the woodcutter with it first in the
register; `scenarios/pace-cuts.edn` puts it above a long `:pace` job.

`:inventory-nearly-full` counts stacks, since `self().inventory` has no slot
total; the real inventory has 36 main slots, so 30 is a threshold, not a
measurement.

## Not built (hooks only)

Claims, no-touch regions, flapping counters, the per-body no-progress
detector, progress events, the HTTP API, multi-body, world memory, soft
pathfinding weights, a reflex pointing at a listed instance (a register entry
may carry `:instance` later).

## Primitives: implementation notes

`js/primitives.mjs` exports `createPrimitives({host, port, username, auth, version?})` as the contract says, plus
`createPrimitivesFromBot(bot, {timeScale = 1})` over an already spawned bot (the tests use it with
`js/stub-bot.mjs`; `timeScale` shrinks every time bound). `js/connect.mjs` exports `readAgentConfig({stateDir,
agent})` (defaults, then `state/agents/<name>/config.json`, then the world's `{host, port}`; defaults copied from
`src/config.mjs`), `connectBot(cfg)` and `connectAgent({stateDir, agent})` (`{bot, disconnect}`). Nothing connects on
import. `mineflayer`, `mineflayer-pathfinder` and `vec3` resolve from the repo root `node_modules`; the engine's own
`package.json` does not list them yet.

Every acting call goes through one wrapper. It checks the token on entry, registers the call, and races the work
against the time bound and against `setOwner`. A cut or a timeout runs the cleanups the work registered, ends the call,
and makes the work's own `alive()` check throw, so nothing it does later reaches the bot. A cut rejects with an error
that has `code: 'cut'` (and `cut: true`); bad args reject with `code: 'bad-args'`.

| primitive | what it does to the bot |
|---|---|
| `moveTo` | `pathfinder.goto(GoalNear)`. Beyond `maxDistance` it walks a `GoalNearXZ` point that far along the straight line, so the result is `partial`. Cleanup: `setGoal(null)`, `clearControlStates`. After the bound or a pathfinder failure: `partial` if at least 1 block closer, else `blocked`. Movements are built by `connect.mjs` with digging, towers and scaffolding off |
| `dig` | checks `blockAt`, reach (eye to cell center, 4.5) and `diggable`, then `bot.dig(block, true)`. Cleanup `stopDigging`. Then polls up to 1 s for item entities within 2 blocks of the cell |
| `place` | picks a solid neighbour as the reference (below first), `equip` to hand, `placeBlock`. Liquids count as replaceable |
| `collect` | `goto` next to the item entity, then waits until the entity is gone. `collected` carries the inventory diff; an entity that vanished with no gain is `gone` |
| `inspectContainer`, `transfer` | `openContainer`, read or `deposit`/`withdraw` on the window, `closeWindow` in a finally and on abort. A thrown error mentioning full, room or space is `full` |
| `equip` | `bot.equip(item, dest)` |
| `eat` | best food by `foodPoints` (or the named item), `equip` then `bot.consume()`. Cleanup `deactivateItem`. On timeout it reports `ate` if `food` rose |
| `attack` | one `bot.attack`, then about 100 ms to read the target's health; `killed` when the entity is gone or its health is 0 |
| `sleep` | bed name check, reach, night (`isDay` formula above), hostile within 8 blocks, then `bot.sleep`. Cleanup `wake` |
| `look` | `lookAt` or `look` with force |

Sensing reads `bot.entities`, `bot.findBlocks` and `bot.blockAt` directly and never waits. Body events come from the
bot's `health` (a drop is `hurt`), `death`, `respawn` (remembered, then reported as `respawned` at the next `spawn`),
`chat`, `wake`, `spawn`, `end` and `kicked` events. The listeners are bound per bot, and `offline` unbinds the old bot
before it quits so its `end` does not report a `disconnected`.

Known gaps:

- No tool selection before `dig`; the bot digs with whatever is in hand.
- `moveTo` has no no-progress detector: a stuck walk ends at the time bound as `blocked` or `partial`.
- The `hurt` event has no `cause`. `isDay` ignores thunderstorms for `sleep`.
- `eat` relies on `bot.consume()`; on a server that never sends the finish status it can only time out (the `food`
  rise check softens this). Not exercised against a real server.
- Everything is tested against a stub bot only: reach numbers, window handling and the pathfinder are unverified live.

### Deviations

- `createPrimitives` accepts an optional `version` (default as in `src/config.mjs`).
- Timeouts resolve with a status as the contract says, not by rejecting; only cuts and bad args reject.
