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
  rounds call), `expr` (job expressions), `composite` (combinators as jobs),
  `registry` (the compile-time job registry, `.clj` macro plus `.cljs`),
  `build_hooks.clj`, `triggers` (every trigger; `triggers/all`), `scenario`,
  `main`.
- `src/jobs/` the jobs, one namespace each (`jobs.survival.eat`,
  `jobs.forestry.fell-tree`, ...). Nothing registers them: the build finds
  them. Helpers they share live outside this tree (`engine.jobs.util`,
  `engine.jobs.forestry`).
- `scenarios/*.edn` scenarios.
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
  programming errors (bad args, `err.code === 'bad-args'`). A mineflayer
  rejection inside an acting call (a refused `placeBlock`, an aborted `dig`,
  a failed `equip`) resolves `{status: 'failed', reason}` with the library's
  message, at most 200 characters. The only other throw is `offline` when
  every reconnect try fails.
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
| `blocks(opts)` | `{radius = 16, names?, match?, max = 64}`; `names` is an array of block names, `match` a JS predicate on the block name; with neither, every non-air block | `[{name, pos, age?, distance}]` sorted by distance |
| `blockAt(pos)` | `{x, y, z}` | `{name, pos, age?}`, or `null` when the chunk is not loaded |

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

`blocks()` and `blockAt()` carry `age` (a number) when the block has an `age` state: the crop growth stage of wheat,
carrots and potatoes (ripe at 7), beetroots (ripe at 3) and sweet berry bushes (berries from 2). Other blocks have no `age`.

Sensing has no line of sight: `entities()` and `blocks()` see through walls, so a hostile behind a wall is "near".

Remembered places (a known bed, a known chest) are not primitives. They are
`:bed` and `:chest` entries in body memory; see `engine.memory/place` below.

### Acting (async, token first)

| method | args | statuses | bound | on cut |
|---|---|---|---|---|
| `moveTo(token, a)` | `{pos, range = 1, timeoutS = 20, maxDistance = 64}` | `arrived` (the goal is satisfied where the body stands, not merely a resolved walk), `partial` (bound or `maxDistance` reached, closer than before), `blocked` (no path, or no progress; `reason: 'noPath'` when the pathfinder gave up with no path and the body is not there) | `timeoutS`, at most 60 | goal cleared, controls released |
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
| `swim(token, a)` | `{ms = 3000}`, at most 10000 | `surfaced` (head out of water), `timeout` | `ms`, at most 10 s | jump released |
| `offline(token, a)` | `{ms = 300000}`, at most 600000 | `ok` (`ms` is the wait used), `cut`, `closed`, `unsupported` | `ms` plus the reconnect | see below |

Extra fields on the result:

- `moveTo`: `pos` (where the body ended), `distance` (to the target), `reason` (`'noPath'`, only on `blocked`).
- `swim`: `oxygen` (`{before, after}`, the air level when the call started and ended).
- `dig`: `block` (name dug), `drops` (`[{id, name, count, pos}]`, the item
  entities that appeared within 2 blocks during up to 1 s after the break).
- `place`: `block` (name placed). Buckets (`bucket`, `water_bucket`, `lava_bucket`) are used, not placed: `pos` is
  the cell that receives the liquid (it must be air, else `occupied`, and have a solid neighbour, else `no-support`),
  or for an empty `bucket` the liquid cell to scoop (`missing` when it holds none). The body equips the bucket, looks
  at the supporting block (or the liquid), calls `activateItem`, and waits up to 1.5 s for the cell to change:
  `placed` (`block` is `water`, `lava` or `bucket`), else `{status: 'failed', reason: 'unchanged'}`.
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

An unplanned disconnect (the bot's `end` or `kicked`) emits `disconnected` and marks the body down; an `error` on the
bot or its client is emitted as the body event `error` and never thrown, so it cannot crash the process. While the
body is down, the next acting call (`moveTo`, `dig`, `place`, `collect`, `inspectContainer`, `transfer`, `equip`,
`eat`, `attack`, `sleep`, `look`, `swim`) first makes the same reconnect `offline` uses (3 tries, 5 s apart; calls
arriving meanwhile share it), emits `online` and runs on the new bot. If every try fails it emits `reconnect-failed`
(an error-level engine event) and resolves `{status: 'disconnected'}`; the next acting call tries again. Sensing
reads keep answering from the dead bot. Without a connection to remake (`createPrimitivesFromBot`) a down body
resolves `disconnected` at once. A stale token still rejects with `cut` first.

`swim` holds the jump control until the block at the head is no longer water, polling every 50 ms, because the
pathfinder has no swim-up move and `moveTo` cannot surface a submerged body. It releases jump on every exit:
surfaced, the bound, a cut, an error. A body whose head is already out returns `surfaced` at once without pressing
anything. Bad `ms` (not a number, zero or negative) rejects with `bad-args`.

Reach for `dig`, `place`, `inspectContainer`, `transfer`, `sleep` and
`attack` is the caller's job: walk there first with `moveTo` (`range` 2 to 3).
The primitives do not walk.

### Body events

```js
primitives.onBodyEvent(listener)   // returns an unsubscribe function
```

`listener` receives plain objects `{kind, ...}` for momentary events:
`hurt` (`health`, `food`, `cause?`), `died` (`pos`, `inventory`, `experience`), `respawned` (`pos`, `dimension`), `chat`
(`from`, `message`), `woke`, `spawned`, `disconnected` (`reason`), `error` (`reason`), `reconnect-failed` (`reason`), `offline` (`ms`), `online` (`pos`). `died` is
emitted at the moment health reaches 0: `pos` is where the body died and `inventory` (`[{name, count, slot}]`) what
it carried, before the server clears it; `experience` is `{level, points}` at death. mineflayer emits `death` from
the health packet and only overwrites `bot.experience` on a later `experience` packet, so the values are the pre-death
ones. `respawned` is emitted at the first `spawned` after the library's respawn
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
  noPath: ['8,64,8'],                           // moveTo targets blocked with reason 'noPath', body unmoved
  swimFails: false,                             // true: swim times out and moves nothing
  ages: { '5,64,0': 7 },                        // "x,y,z" -> crop age, reported as `age`
})
p.world.state            // the mutable world (self, time, blocks, entities, inventory, containers)
p.world.calls            // [{ name, token, args }] for every acting call, in order
p.world.hold('moveTo')   // the next moveTo call waits; returns release(result?)
p.world.override('dig', async (token, args, defaultImpl) => ({ status: 'cannot' }))
p.world.emit({ kind: 'hurt', health: 6 })   // delivered to onBodyEvent listeners
p.world.setTime(13000)
p.world.die()            // emits died (pos, inventory, experience), drops the inventory as items, zeroes experience
```

Fake semantics: `swim` lifts the body to the top water cell of its column and refills oxygen to 20. `moveTo` jumps to the target if within `maxDistance`, else
moves `maxDistance` toward it and returns `partial`. `dig` removes the block
and adds an item entity at its cell. `collect` moves the item entity into the
inventory. `attack` takes 5 health per swing. `sleep` succeeds at night on a
cell whose block name ends in `_bed`, and sets the time to 0. `eat` raises
`food` by 5 and consumes one item. A held call rejects with `cut` when the
owner changes, exactly as the real layer must. Where the body stands decides
`inLava` and `inWater` after every `moveTo` and `place`, and water puts out
fire. Placing a `water_bucket` pours water into the cell and leaves an empty
`bucket`. An entity killed by `attack` spawns its `drops` (`[{name, count}]`)
as items. A `creeper` entity carries `creeper: true` unless the spec says
otherwise. `dig` clears a cell's crop age.

## Jobs

A job is a namespace under `src/jobs/` exporting `check` and `round`,
optionally `doc` and `args`. Directories nest freely; the namespace follows
the path (`src/jobs/forestry/fell_tree.cljs` is `jobs.forestry.fell-tree`).

```clojure
(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]))

(def doc "Eat the best food carried, once.")                 ; optional

(def args                                                    ; optional
  {:item {:doc "the food to eat; the best carried when nil" :default nil}})

(defn check [_c] true)                                       ; ctx -> boolean

(defn ^:async round [c]                                      ; ctx -> :done | :continue
  (await (ctx/act c :eat #js {}))
  :done)
```

There is no protocol and no catalog to edit: adding the file adds the job.

**The registry.** `engine.registry/jobs` is `{ns-symbol {:check :round :doc
:args}}`, built at compile time:

1. The build hook `engine.build-hooks/add-job-namespaces` (in both builds'
   `:build-hooks`) runs at the `:compile-prepare` stage of every compile. It
   lists every `.cljs`/`.cljc` file under the `jobs` directory on the
   classpath, reads each with the Clojure reader, and fails the compile with
   a message naming the namespace when one does not define `check` and
   `round`, or when the file's `ns` does not match its path. It puts every
   job namespace at the front of the `:main` module's entries, so each is
   compiled and loaded although nothing requires it, and gives
   `engine.registry` those namespaces as `:extra-requires`, so parallel
   compilation analyses them first (shadow's own test runner does the same).
2. The macro `engine.registry/job-registry` (in `registry.clj`) scans the
   same files and emits the map with a var reference per export.
   `engine.registry` is `^:dev/always`, so the macro re-runs on every
   compile.

Limits: the scan reads the source text, so `check` and `round` must be
top-level `def`/`defn` forms (not defined by another macro). A job namespace
must not require `engine.registry` (it would wait on itself). Under `shadow
watch`, adding a job file does not by itself trigger a compile; the next
compile, from any source change, picks it up. Release (`:advanced`) builds
are untested.

- **The check** says whether the job can usefully run now. It reads sensing
  and memory through the ctx, returns a boolean, and is cheap and side-effect
  free: its ctx has no token, so `act`, `update-mem!` and `remember!` throw.
  Checks are asked every tick; a declined job costs nothing. A check that
  throws declines (with a `system.error` warn). The engine refuses a job
  without a `check` and a `round`.
- **The round** returns `:done` (the job leaves the list and its memory is
  deleted) or `:continue`. Anything else, or a throw, is a failure: a
  listed job stays on the list with its memory, marked failed (`:failed {id
  {:error :t}}` in the engine state, persisted in `engine.edn`), with a
  `job.failed` warn. The scheduler skips a failed job, even a holding one,
  until `retry!` clears the mark; `cancel!` removes it as usual. A reflex job
  that fails is dropped (one chance) with the same warn. A cut is never a
  failure: the job stays listed with its memory.
- Long waits are not loops: a job waiting for daylight returns `:continue`
  and declines in its check until the sun is up (`jobs.time.wait-for-day`).
- **`args`** maps each arg key to `{:doc :default}`. The engine merges the
  spec's args over the defaults, so a round can read `(:args ctx)` without
  its own defaults. Undeclared keys are passed through.

## Job expressions

Wherever a job is named (scenario queue entries, register entries,
`submit!`, `do-now!`, `ctx/submit!`) it is given as a job spec: an EDN
list, read with the EDN reader and interpreted, never evaluated.

| form | meaning |
|---|---|
| `(jobs.survival.eat)`, `(jobs.survival.eat {:item "bread"})` | a leaf: the job namespace, args optional, merged over its `args` defaults |
| `(seq e1 e2 ...)` | run the children in order, one child call per round; check = the check of the next unfinished child; done when the last is done |
| `(any e1 e2 ...)` | each round, call the first child whose check passes; check = any child's check; done when the child that ran is done |
| `(repeat e)` | when the child is done, start it again fresh; never done; declines when the child declines |
| `(hold e)` | like `e`, but the list entry holds the body; only around a whole spec, and not in a register entry |

Nothing else is valid: a symbol that is neither a combinator nor a job in
the registry, a missing or extra argument, args that are not a map, or a
nested `hold` is refused at load (scenario validation, `submit!`,
`register-reflex!`) with a message naming the problem and the whole spec.

```clojure
(jobs.forestry.harvest-wood {:species "oak"})
(hold (seq (jobs.movement.go-to {:pos {:x 10 :y 64 :z 0}})
           (jobs.time.wait-for-day)))
(repeat (any (jobs.forestry.harvest-wood) (jobs.storage.deposit)))
```

The engine parses a spec into a node (`engine.expr`; plain EDN, persisted in
`engine.edn`) and runs combinators as generic jobs (`engine.composite`)
through `call-child`. Child slots are positional, `:c0`, `:c1`, ..., so a
spec's memory nests like any parent's: `(seq (repeat (any (a))) (b))`
keeps `a` at `[:children :c0 :children :c0 :children :c0]`. `seq` keeps its
position as `:at`, `repeat` counts finished runs as `:runs` and drops the
child's memory to start it fresh. Events name a job by its label:
`jobs.forestry.fell-tree`, or `(seq jobs.a (repeat jobs.b))`. A restored
instance whose job namespace no longer exists is dropped with a
`job.failed` warn.


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
| `(await (ctx/call-child ctx slot job args))` | one round of a child job (below); `job` is a job symbol or a definition map |
| `(ctx/check-child ctx slot job args)` | the child's check against its sub-map, for a parent's check |
| `(ctx/result! ctx data)` | hand `data` to the parent; only the round that ends `:done` hands it over |
| `(ctx/child-result ctx slot)` | the data the child in `slot` handed over in the round it finished, during that round of the parent; else nil |
| `(ctx/submit! ctx spec opts)` | put a job spec at the end of the list as a peer; returns its id |
| `(ctx/emit! ctx kind level fields)` | an event with `:source :job` |

**act.** Every acting primitive call goes through `act`. It checks the
ownership token (a stale token rejects with `cut` before the primitive is
called), saves memory, emits `action.started` (debug), calls the primitive,
saves memory again and emits `action.done` (debug) with the status. There is
no commit: a job updates its memory map and calls `act`, so a cut loses at
most the work since the last save. Write a debt or an intent with
`update-mem!` before the `act` it protects. After every `moveTo`, `act`
writes a `:moved` body-memory entry `{:from :to :status :target}` (cap 20,
ten minutes), which the stuck trigger reads.

**call-child.** `(ctx/call-child ctx slot job args)` takes a slot keyword,
a job (a job namespace symbol such as `'jobs.forestry.fell-tree`, whose
`args` defaults are merged under `args`, or a definition map `{:check
:round}`) and args. The child's memory is
the parent's `[:children slot]` sub-map, created as `{:args args :children {}}`
when missing. The engine runs the child's check against it (false resolves
to `:declined`, no round run), else one child round with the parent's token,
resolving to `:done` or `:continue`. The same slot resumes the same child
while it returns `:continue` or `:declined`; only when it returns `:done` is
its sub-map cleared, so the next call in that slot starts fresh (counters such
as go-to's `:blocked` do not leak into a later walk). A declined child keeps
its memory, so debts it wrote (a replant owed, say) survive until a later call
in that slot runs it again. A new slot is a fresh
child. The combinators get this for free: `seq` and `any` children start
fresh when called again, and `repeat` starts its child fresh each run.

**Child results.** The outcome of a child is still only `:done`, `:continue`
or `:declined`. To say more, a child calls `(ctx/result! ctx data)` in the
round in which it finishes; the parent reads it with `(ctx/child-result ctx
slot)` after the `call-child` that returned `:done`. A result from a round
that returned `:continue` is discarded, and every result is gone when the
parent's round ends; a check cannot hand one over. `jobs.movement.go-to`
hands over `{:arrived true}` or `{:arrived false :reason :unreachable}`;
`jobs.forestry.collect-drops` hands over `{:collected n}`.

Children can call children, recursion included, with no depth cap. A cut anywhere ends the whole chain's round. Cancel and done take the
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
  `:moved`, `:forestry/replant`, `:job/j7`. The survival jobs write `:breathe`,
  `:extinguish`, `:hazard`, `:hostile`, `:fed`, `:hungry`, `:slept`,
  `:shelter`, `:log-out`, `:stuck` and `:recovered`, and read `:home` and
  `:food-source`, which nothing writes yet (an agent or a later job will).
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
 :job         '(jobs.survival.retreat)    ; default job spec
 :args        {:radius 8}                 ; the trigger's own, merged under the entry's :args
 :persistence :cooldown                   ; :retry | :cooldown | :stop
 :cooldown-s  5}
```

The register is an ordered vector of entries `{:id :trigger :job :args
:persistence :cooldown-s :builtin?}`; the id defaults to the trigger name and
`:args` is the trigger's defaults merged with the scenario's; they are the
trigger's only. `:job` is a job spec without `hold`; the reflex job's args
are in it. The engine evaluates the effective order every tick and
fires the first entry whose `:when` holds and which is not muted or cooling
down.

- A firing reflex cuts a running listed job. It cuts a running reflex job
  only if it sits above that reflex. A reflex whose own job is running does
  not fire again.
- The cut listed job stays on the list and is the next to run (if its check
  passes) once no reflex holds the body. A cut reflex job is dropped; it
  fires again from the world if its condition still holds.
- A reflex job gets one chance. Its check is never asked: the trigger is its
  check. The engine starts its first round in the tick the trigger fires and
  gives it the next round in every later tick, whatever the job's own `check`
  would say, while its rounds return `:continue`. When a round returns `:done`
  or `:declined`, or throws, the job is removed from the list and the body is
  free; a `:declined` also emits an info event `reflex.declined` naming the
  reflex and the job. The job never lingers: if the trigger still holds, it
  fires the job again as a new instance, subject to persistence. So a job
  meant for a register entry must cope in its round with a world in which its
  check would decline: return `:done` (or `:declined`) at the top of the round
  when there is nothing to do.
- Combinators follow the rule: `any` returns `:declined` when no child's
  check passes in its round, `seq` returns `:declined` when the next
  unfinished child declines (its check is that child's check), and `repeat`
  returns `:declined` when its child declines, so none of them holds the
  body. A listed combinator is unaffected, because its check already keeps it
  from running when it would decline; a round returning `:declined` just
  yields like `:continue`.
- Persistence, when the trigger still holds as the job ends: `:retry` fires
  again next tick, `:cooldown` waits `:cooldown-s` **counted from the job
  ending** (not from firing), and `:stop` waits until the condition has been
  false once. If the trigger no longer holds at the end, there is no wait. A
  register entry's own `:persistence` and `:cooldown-s` override the
  trigger's.

  The survival scenario sets, explicitly, `:persistence :cooldown` and:

  | trigger | cooldown (s) | why |
  |---|---|---|
  | suffocating | 2 | back on the body almost at once |
  | burning | 2 | same |
  | health-low | 10 | |
  | hostile-near | 5 | |
  | hungry | 90 | a body with no food does not retry every tick |
  | night-unsafe | 10 | keep trying through the night |
  | stuck | 60 | must outlast the 60 s window of the moves that fired it |
  | died | 30 | |
- Agent-only edits (functions in `engine.core`): `register-reflex!`,
  `remove-reflex!` (refused for built-ins), `mute!` (with TTL), `move!`
  (`{:above id}` or `{:below id}`, with TTL), `clear-change!`. Each property
  (mute, position) is at its default or under exactly one change; a new change
  replaces the old one; on expiry it reverts to the default and emits
  `reflex.reverted`. A move whose anchor is gone puts the reflex at the bottom.
  Jobs cannot reach these.

### Debugging triggers

To see that a trigger fires without running the real job, register it against
`jobs.debug.notify`:

```clojure
{:trigger :burning :job (jobs.debug.notify {:text "burning fired"})}
```

Each run emits an info event of kind `job.notify` (the text plus health, food,
oxygen, on-fire, position, time of day and nearby hostiles) and writes a
`:notify` entry `{:text ...}` to body memory (cap 20, ttl 10 minutes). With
`:chat? true` it also sends the text to game chat, once a primitive `chat`
exists; none does yet, so today the flag does nothing.

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

`submit!` and `cancel!` (agent) edit the list; `retry!` (agent, `(core/retry!
eng id)`, true when `id` was marked failed) clears a failed mark and emits
`job.retried`; `do-now!` (agent) cuts the
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
`job.completed`, `job.failed`, `job.retried`, `job.cancelled`, `job.stalled` (warn),
`job.memory_written` (debug, `memory` is the kind), `action.started` and
`action.done` (debug), `reflex.fired`, `reflex.ended` (`how`: `cleared`,
`completed_not_cleared`, `dropped`), `reflex.changed`, `reflex.reverted`,
`body.<kind>` for body events, `system.started`, `system.restored`,
`system.stopping`.

Save measurement (no optimisation, just numbers): every write of `memory.edn`
or `engine.edn` emits `memory.saved` (debug) with `file` (`memory.edn` or
`engine.edn`), `bytes` written and `ms` the write took. Once a minute
(`:stats-ms`, checked in `tick!`) `memory.save-stats` (info) sums the window:
`count`, `bytes`, `ms` (total) and `max-ms`, then the window restarts.

## Scenarios

EDN, read with `cljs.reader`:

```clojure
{:register [{:trigger :health-low}                                  ; trigger defaults
            {:trigger :hostile-near :args {:radius 12}              ; trigger args
             :job (jobs.survival.retreat {:radius 12 :step 10})}    ; job spec
            {:trigger :health-low :id :health-low-2
             :job (jobs.survival.eat {:item "bread"})
             :persistence :retry}]                                  ; overrides
 :queue    [(jobs.movement.go-to {:pos {:x 10 :y 64 :z 0}})         ; job specs
            (hold (jobs.time.wait-for-day))]}
```

`npm run body -- --agent <name> --scenario <file>` loads
`state/agents/<name>/config.json` (`username`, `world`), the world's
`state/worlds/<world>/world.json` (`host`, `port`), and `js/primitives.mjs`.
It refuses to start, with a message, when `js/primitives.mjs` does not exist.
If `state/agents/<name>/engine/engine.edn` exists the saved list and register
are restored and the scenario is ignored; pass `--fresh` to discard saved
engine state and start from the scenario (memory is kept; job kinds of the
discarded list are swept). The scenario is validated against the job
registry and the triggers before connecting. `--state-dir <dir>` overrides the repo's `state/`.

`test/engine/scenarios_test.cljs` runs `woodcutter.edn`, `pace-cuts.edn`
and `survival.edn` end to end against the fake primitives. `survival.edn`
registers every survival trigger in the order of the Triggers table and
queues `(repeat (jobs.movement.look-around))`, so the body looks around
between reflexes; its test drops the health, then places a zombie, then kills
the body, and checks that recover, respond-to-hostile and recover-drops fire
in turn and that the body goes back to looking around.

## Job library

Jobs live under `src/jobs/`, helpers in `engine.jobs.util` and
`engine.jobs.forestry`. Each declares its args with defaults. Every round
re-reads the world and does a bounded piece. Positions in memory are
`{:x :y :z}` maps. Failed rounds are counted in job memory as `:failures`;
after three the job emits a warn and ends.

| job | args | check | job memory | body memory |
|---|---|---|---|---|
| `jobs.movement.go-to` | `{:pos :range 1}` | always | `:blocked` count | none; hands over `{:arrived bool :reason?}` |
| `jobs.time.wait-for-day` | none | it is day | none | none |
| `jobs.survival.eat` | `{:item nil :until 18 :allow-bad false}` | food below `:until` and something edible carried | none | writes `:fed` (cap 20, 6 h) |
| `jobs.movement.look-around` | none | always | none | writes `:looked` (cap 1, forever) |
| `jobs.movement.pace` | `{:a pos :b pos :laps 3 :rounds 8 :range 1}` | always | `:rounds-run` | none |
| `jobs.forestry.fell-tree` | `{:species nil :radius 16}` | a column is chosen, or every candidate was unreachable, or a tree (log column with leaves near its top) is in radius | `:column {:x :z}`, `:species`, `:base`, `:partials`, `:unreachable` | writes one `:forestry/replant` `{:pos base :species}` when the base log is dug |
| `jobs.forestry.collect-drops` | `{:radius 16 :filter [names] or nil}` | always | `:skipped` ids of unreachable items, `:collected` count | none; hands over `{:collected n}` |
| `jobs.forestry.plant-sapling` | `{:at pos or nil :species nil}` | nothing to plant, or a matching sapling is carried and the spot holds no log | none | plants at the oldest `:forestry/replant` debt and forgets it |
| `jobs.forestry.harvest-wood` | `{:species nil :radius 16 :filter nil}` | the current phase's child check | `:phase`, children in slots `:fell`, `:collect`, `:plant` | as its children |
| `jobs.storage.deposit` | `{:chest pos or nil :items [names] or nil}` | a chest is known (args or `:chest`) | none | reads `:chest` |
| `jobs.survival.retreat` | `{:radius 8 :step 6 :cooldown-ms 5000}` | always | `:last-seen` | reads `:bed`, `:home`, `:hazard` |
| `jobs.survival.sleep` | `{:bed-radius}` | night and a `:bed` within `:bed-radius` | child `:go` | reads `:bed`; writes `:slept`, retracts a missing `:bed` |
| `jobs.survival.breathe` | `{:min-oxygen 12 :radius 2 :reach 10}` | drowning (swims up, or walks sideways to a column with air) or enclosed (the suffocating condition) | `:noted` | writes `:breathe` (cap 20, 1 h) |
| `jobs.survival.extinguish` | `{:water-radius 6 :step 4 :scan-radius 8}` | on fire or in lava | none | writes `:extinguish` (cap 20, 1 h), `:hazard` for lava seen (cap 50, 6 h) |
| `jobs.survival.recover` | `{:health 7 :healed 16 :sight 16}` | health below `:health`, or below `:healed` with a `:hurt` in the last 5 min, or a spell under way | `:spell-started`, children `:flee`, `:safety`, `:eat` | writes one `:hurt` per spell; reads `:bed`, `:home` |
| `jobs.survival.respond-to-hostile` | `{:radius 8 :fight-health 12 :min-health 8 :max-fight 2 :weapons ["_sword" "_axe"]}` | a hostile within `:radius` | `:decision`, `:logged`, child `:fight` or `:flee` | writes one `:hostile` per encounter (cap 50, 1 h) |
| `jobs.survival.fight-back` | `{:range 4 :min-health 8 :weapons ["_sword" "_axe"] :attack-gap-ms 600}` | health at least `:min-health` and a hostile within `:range` | `:last-attack` | none |
| `jobs.survival.get-food` | `{:food 6 :food-when-hurt 14 :source-radius 64 :hunt-radius 24 :farm-radius 6 :take 16 :attack-gap-ms 600 :ask-cooldown-ms 600000}` | hungry (as the hungry trigger), or a meal under way | `:eating`, `:dead-source`, `:last-swing`, `:skipped-animals`, `:skipped-blocks`, children `:eat`, `:goto`, `:collect` | reads `:food-source`; writes `:hungry` when nothing is found |
| `jobs.survival.shelter` | `{:roof-height 4 :bed-radius :urgent-bed-radius 128 :max-days-awake 3 :offline-allowed true :offline-ms 300000 :player-radius 128}` | the night-unsafe condition, or a built `:shelter` here whose roof is still over the body | `:needs-bed-noted`, children `:sleep`, `:log-out`, `:dig-in`, `:wait` | writes `:shelter` (built, reopened); reads `:slept` |
| `jobs.survival.dig-in` | `{:roof-height 4 :blocks [building blocks] :max-places 4}` | night and no roof within `:roof-height` | `:mode` and its targets | writes `:shelter` (cap 10, 1 day) |
| `jobs.survival.log-out` | `{:bed-radius :offline-allowed true :offline-ms 300000 :player-radius 128}` | night, no usable bed, allowed, not unsupported before, another player sleeping | none | writes `:log-out` (cap 10, 1 day) |
| `jobs.survival.recover-drops` | `{:margin 0 :danger-radius 8 :collect-radius 6}` | a `:died` with no newer `:recovered` | `:decided`, `:phase`, children `:go`, `:collect` | reads `:died`; writes `:recovered` `{:decision :collected/:skip/:abandoned ...}` (cap 10, 1 day) |
| `jobs.maintenance.unstick` | `{:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000 :max-attempts 4}` | stuck (as the stuck trigger), or an attempt under way | `:attempts` | reads `:moved`; writes `:stuck` (cap 10, 1 h) when it gives up; equips the best pickaxe before digging |

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
  once by symbol (`jobs.forestry.fell-tree`, then `collect-drops` with the
  species' log, sapling, stick and apple, then `plant-sapling`) and advances the phase when the
  child is done. Its check is the current child's check, so it declines
  (rather than spinning) while no tree is in sight or no sapling is carried.
- `:deposit` puts away every carried stack except tools and armour (suffixes
  `_pickaxe _axe _shovel _hoe _sword _helmet _chestplate _leggings _boots`,
  plus shears, bow, crossbow, fishing_rod, flint_and_steel, shield, trident),
  one stack per round. Saplings included: on a shared list with
  `:harvest-wood` it can take the sapling the replant needs. Warn kind
  `chest_unusable`.
- `:retreat` walks `:step` blocks away from the nearest hostile per round,
  leaning towards the latest `:bed` or `:home` when that is not through the
  hostile, and turning up to 90 degrees to keep clear of `:hazard` cells. Done
  once no hostile has been within `:radius` for `:cooldown-ms`. No way out
  counts as a failed round (`retreat_blocked`).
- `:sleep` walks within 2 of the known bed (go-to as a child) and calls
  `sleep`. `sleeping` and `not-night` are done; a go-to that hands over
  `{:arrived false}`, a taken bed or a nearby monster is a failed round
  (`bed_unreachable`, `bed_unusable`); a missing bed warns `bed_missing`,
  retracts the `:bed` and ends.
- The survival jobs' docstrings (`(:doc (registry/jobs 'jobs.survival.x))`)
  give the full rules; in short: `:breathe` swims up to air or digs the head
  cell free, one move per round; `:extinguish` pours a carried water bucket at
  its feet, else walks into water or to the safest dry cell nearby;
  `:recover` flees, walks to a bed or home, eats and waits for health to reach
  `:healed`; `:respond-to-hostile` fights (`fight-back`) when healthy, armed,
  not facing a creeper and outnumbered by at most `:max-fight`, else retreats;
  `:get-food` climbs a ladder of eat, known source, hunt or harvest, then
  gives up with a `food.none` warn; `:shelter` tries sleep, then log-out,
  then dig-in, and reopens the shelter at day; `:recover-drops` weighs the
  drops' value (`engine.value`) against the trip and goes back for them or
  skips; `:unstick` escalates from stepping back to digging to pillaring.

Triggers (`engine.triggers`; `:when` receives the world, a memory view and
the register entry's `:args`):

Listed in the order a survival register puts them (most urgent first, as
`scenarios/survival.edn` does); `engine.triggers/all` lists them the same way.

| trigger | holds when | job | persistence |
|---|---|---|---|
| `:suffocating` | in water with oxygen below `:min-oxygen` (default 12) and the head not in air, or the head cell holds a suffocating block | `(jobs.survival.breathe)` | retry |
| `:burning` | on fire or in lava | `(jobs.survival.extinguish)` | retry |
| `:health-low` | health below `:health` (default 7) | `(jobs.survival.recover)` | cooldown 30 s |
| `:hostile-near` | a hostile mob within `:radius` (default 8), walls included; set the job's own `:radius` in `:job` | `(jobs.survival.respond-to-hostile)` | cooldown 5 s |
| `:hungry` | food below `:food` (default 6), or below `:food-when-hurt` (default 14) while health is below 20 | `(jobs.survival.get-food)` | cooldown 60 s |
| `:night-unsafe` | night, awake, and nothing solid within `:roof-height` (default 4) above | `(jobs.survival.shelter)` | cooldown 10 s |
| `:night-and-bed-known` | an alias of `:night-unsafe` under its old name, kept for the older scenarios; register one or the other | `(jobs.survival.shelter)` | cooldown 10 s |
| `:stuck` | the last `:n` (4) `:moved` entries, all within `:window-ms` (60 s) and none older than the latest `:stuck`, are bad moves (not arrived or partial, or under `:min-move` 1.5 blocks), and the latest `:stuck` is over `:quiet-ms` (5 min) old | `(jobs.maintenance.unstick)` | cooldown 60 s |
| `:died` | a `:died` entry younger than five minutes with no newer `:recovered` | `(jobs.survival.recover-drops)` | cooldown 0 |
| `:inventory-nearly-full` | `:stacks` (default 30) or more carried stacks and a `:chest` entry exists | `(jobs.storage.deposit)` | cooldown 60 s |
| `:every-interval` | no `:looked` entry, or the latest is at least `:seconds` (default 60) old | `(jobs.movement.look-around)` | cooldown 0 |

`:every-interval` is a wall-clock reflex: `look-around` looks at a point
three blocks ahead and writes `:looked`, which survives a restart and makes
the trigger stop holding. With no entry it fires at once.
`scenarios/woodcutter-cuts.edn` is the woodcutter with it first in the
register; `scenarios/pace-cuts.edn` puts it above a long `pace` job.

`:inventory-nearly-full` counts stacks, since `self().inventory` has no slot
total; the real inventory has 36 main slots, so 30 is a threshold, not a
measurement.

### Live-unverified assumptions

The survival jobs pass against the fake only. What they assume about the real
server and mineflayer, none of it checked live:

- `extinguish` pours a water bucket with `place` at the body's own feet cell. Falsified live (2026-10-03): `place` with a water bucket on air rejects with "Server refused to place water_bucket ... the block is still air", and with a fire block in the feet cell it returns `occupied`; a bucket needs a use-item primitive. Since then `place` uses buckets through `activateItem` (see the `place` result above); not yet re-checked live.
- Verified live: `unstick`'s pillar attempt cannot work. The server refuses
  `place` into the body's own cell ("the block is still air") because the body
  occupies it; without a jump primitive there is no way round that, and `place`
  throws instead of returning a status, which fails the job.
- `breathe`, `extinguish` and `unstick` use `moveTo` with range 0 to step
  into a cell, water included.
- `dig-in`'s roof placement may fail with no supporting neighbour
  (`no-support`); it then counts a failed round, and three end the job with
  `dig_in_failed`.
- `onFire` and the sleeping pose are read from metadata indices found through
  the registry's `metadataKeys` (see Sensing); the fallback indices are guesses.
- Sensing has no line of sight, so hostiles behind walls count for
  `:hostile-near` and `respond-to-hostile`.
- Wheat is not food raw; `get-food` harvests it but cannot eat it (no
  crafting yet).

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
| `moveTo` | `pathfinder.goto(GoalNear)`. Beyond `maxDistance` it walks a `GoalNearXZ` point that far along the straight line, so the result is `partial`. Cleanup: `setGoal(null)`, `clearControlStates`. After the bound or a pathfinder failure: `partial` if at least 1 block closer, else `blocked`. A resolved `goto` counts as `arrived` only if `goal.isEnd` holds for the floored body position: goto.js (patched) also resolves on a `noPath` update with an empty path, which is `blocked` with `reason: 'noPath'`. Movements are built by `connect.mjs` with digging, towers and scaffolding off |
| `swim` | `setControlState('jump', true)` while `blockAt(eye cell)` is water, `false` on every exit |
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
