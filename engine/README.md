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
- `src/engine/` `core` (list, register, scheduler), `memory`, `events`,
  `ctx` (helpers job rounds call), `conditions` (data conditions for yields),
  `catalog` (every job and trigger by name), `scenario`, `main`.
- `src/engine/jobs/` job definitions; `engine.jobs.samples` holds three tiny
  ones (`:go-to`, `:wait-for-day`, `:eat`) that prove the contract.
  `src/engine/triggers.cljs` triggers (`:health-low`). Register new jobs and
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
  Nothing in a primitive runs for minutes; long waits are yields.
- **Sensing methods** are synchronous, take no token, and never wait for a
  turn. Triggers and preconditions call them every tick, so they must stay
  cheap: scans are bounded by radius and `max`, and run on the JS side.

### Ownership token

```js
primitives.setOwner(token)   // sync; token is a string or null
```

The engine calls `setOwner` before each round with a fresh token and passes
that token to the round. A call whose token is not the current owner rejects
at once with `code: 'cut'`. When `setOwner` changes the owner, every in-flight
call made with the old token stops what it was doing within one game tick
(pathfinder goal cleared, `stopDigging`, window closed, controls released)
and rejects with `code: 'cut'`. `null` means nobody may act.

`isOwner(token)` (sync) reports whether a token is current.

### Sensing (sync, no token)

| method | args | returns |
|---|---|---|
| `self()` | none | `{username, pos, health, food, timeOfDay, isDay, held, inventory}` where `timeOfDay` is 0..23999, `isDay` is `timeOfDay < 12542 \|\| timeOfDay > 23460`, `held` is an item name or null, `inventory` is `[{name, count, slot}]` |
| `entities(opts)` | `{radius = 16, kind?, names?, max = 32}`; `kind` is one of `hostile`, `passive`, `player`, `item`, `other` | `[{id, name, kind, pos, distance, item?}]` sorted by distance; `item` is `{name, count}` for dropped items |
| `blocks(opts)` | `{radius = 16, names?, match?, max = 64}`; `names` is an array of block names, `match` a JS predicate on the block name; with neither, every non-air block | `[{name, pos, distance}]` sorted by distance |
| `blockAt(pos)` | `{x, y, z}` | `{name, pos}`, or `null` when the chunk is not loaded |

Remembered places (a known bed, a known chest) are not primitives. They live
in common memory; see `engine.memory/places` below.

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

Reach for `dig`, `place`, `inspectContainer`, `transfer`, `sleep` and
`attack` is the caller's job: walk there first with `moveTo` (`range` 2 to 3).
The primitives do not walk.

### Body events

```js
primitives.onBodyEvent(listener)   // returns an unsubscribe function
```

`listener` receives plain objects `{kind, ...}` for momentary events:
`hurt` (`health`, `food`, `cause?`), `died` (`pos`), `respawned`, `chat`
(`from`, `message`), `woke`, `spawned`, `disconnected` (`reason`). The engine
turns each into a record in body memory (see Memory).

### Lifecycle

`close()` disconnects (the fake does nothing).

### The fake

`createFake(spec)` in `js/fake.mjs` returns the primitives plus a `world`
handle for tests:

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

A job definition is a map:

```clojure
{:name         :go-to                         ; keyword, unique in the catalog
 :precondition (fn [world memory args] ...)   ; optional; true | :not-yet | false
 :round        go-to-round}                   ; a (defn ^:async go-to-round [ctx] ...); results below
```

- `world` is the primitives object (sensing methods only, by convention).
- `memory` is `{:common {...} :body {...} :job {...}}`, the job's own memory
  under `:job`.
- Precondition results: `true` runs the round. `:not-yet` skips it silently
  (waiting, as designed). `false` skips it and emits one `job.blocked` warn
  until it next holds: use it for "this cannot happen without someone else
  acting".

Round results:

| result | meaning |
|---|---|
| `:done` | the job is finished; it leaves the list, its memory is deleted |
| `:continue` | more to do; ready again whenever its precondition holds |
| `:not-ready` | could do nothing this round; recorded as no progress. Without a wake the job is not stepped again for a minimum re-check interval (engine option `:min-recheck-ms`, default 5000 ms) |
| `{:status :continue :wake [:day]}` | a yield with a per-round precondition (a data condition, see `engine.conditions`); it overrides the definition's precondition until the job next runs |

A round may also throw; the job is then dropped with a `job.failed` warn.
A reflex job keeps the body between its rounds while it returns `:continue`;
`:done`, `:not-ready` or a throw ends it.

### ctx

The round's single argument. Use the helpers in `engine.ctx` rather than the
keys directly.

| key | helper | what |
|---|---|---|
| `:primitives` | | the primitives object |
| `:token` | | this round's ownership token; pass it to every acting call |
| `:args` | | the instance's args (EDN map) |
| `:id` | | instance id, for example `"j4"` or `"j4/walk"` for a child |
| `:memory` | `(ctx/mem ctx)`, `(ctx/mem ctx :body)`, `(ctx/mem ctx :common)` | read a scope now |
| | `(ctx/commit! ctx m-or-f)`, `(ctx/commit! ctx :body m-or-f)` | replace the scope with `m`, or with `(f current)`; written to disk at once; throws `cut` if this round was cut |
| `:step-child` | `(await (ctx/step-child ctx slot job-name args))` | run one round of a child; returns `:done`, `:continue` or `:not-ready`, or `{:status :not-ready :wake w}` when the child yielded a wake condition (return it as your own result to pass the wake up) |
| `:submit` | `(ctx/submit! ctx job-name args {:hold? false})` | put a new job at the end of the list; returns its id |
| `:emit` | `(ctx/emit! ctx kind level fields)` | an event with `:source :job` |
| | `(ctx/act ctx :moveTo #js {...})` | call a primitive with the token: `(.moveTo p token args)` |

Children: a child's memory lives inside its parent's, under the slot name, and
its id is `<parent-id>/<slot>`. Stepping the same slot again resumes it; once
it returned `:done`, stepping that slot again returns `:done` without running
it, so a fresh child needs a fresh slot. A child's precondition is checked
before its round; when it does not hold, `step-child` returns `:not-ready`.
Children share the parent's token and round. The parent's `:done` deletes the
children's memory with its own.

Instance ids are deterministic: top-level instances are `j1`, `j2`, ... from
a persisted counter, in the order they are created (scenario queue first,
then reflex firings and submits as they happen).

### Memory

Three scopes, each a JSON object on disk, keys read back as keywords. Store
strings, numbers, booleans, vectors and maps; keywords come back as strings.

| scope | file under `state/agents/<name>/engine/` |
|---|---|
| common | `common.json` (per agent for now; shared across bodies once multi-body exists) |
| body | `body.json` |
| job | `jobs/<id>.json`, children inside their parent's file |

Body memory has `:records`, a vector of momentary events the engine recorded:
`{:kind "hurt" :t <ms> ...fields}` for each body event, plus `{:kind
"restart" :t ...}` at every start. Triggers read records; the job that handles
one removes it with `(ctx/commit! ctx :body f)`. Helpers in `engine.memory`:
`records` (of a kind), `drop-records`, `places` (`(places memory :bed)` reads
`[:common :places :bed]`, a vector of `{:pos {...}}`).

## Triggers and the register

A trigger definition:

```clojure
{:name        :health-low
 :when        (fn [world memory args] bool) ; memory is {:common :body :now ms}; args are the entry's
 :job         :eat                        ; default job (a catalog name)
 :args        {}
 :persistence :cooldown                   ; :retry | :cooldown | :stop
 :cooldown-s  30}
```

The register is an ordered vector of entries `{:id :trigger :job :args
:persistence :cooldown-s :builtin?}`; the id defaults to the trigger name.
The engine evaluates the effective order every tick and fires the first
entry whose `:when` holds and which is not muted or cooling down.

- A firing reflex cuts a running listed job. It cuts a running reflex job
  only if it sits above that reflex. A reflex whose own job is running does
  not fire again.
- The cut listed job stays on the list and is the next to run once no reflex
  holds the body. A cut reflex job is dropped; it fires again from the world
  if its condition still holds.
- A reflex job runs rounds until it returns `:done` or `:not-ready`, then
  ends. If its condition still holds, persistence decides: `:retry` fires
  again next tick, `:cooldown` waits `:cooldown-s`, `:stop` waits until the
  condition has been false once.
- Agent-only edits (functions in `engine.core`): `register-reflex!`,
  `remove-reflex!` (refused for built-ins), `mute!` (with TTL), `move!`
  (`{:above id}` or `{:below id}`, with TTL), `clear-change!`. Each property
  (mute, position) is at its default or under exactly one change; a new change
  replaces the old one; on expiry it reverts to the default and emits
  `reflex.reverted`. A move whose anchor is gone puts the reflex at the bottom.
  Jobs cannot reach these.

## The list

Round-robin: after a round, the next job whose precondition holds, after the
last one that ran, wrapping. A holding job (`:hold? true`) is always chosen
while it is on the list, without checking preconditions. A cut job is next
when the body is free again. `submit!` and `cancel!` (agent) edit the list;
`do-now!` (agent) is cut + submit at the front with `:hold? true`.

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
`job.round_started`, `job.yielded`, `job.cut`, `job.completed`,
`job.failed`, `job.blocked`, `job.cancelled`, `reflex.fired`,
`reflex.ended` (`how`: `cleared`, `completed_not_cleared`, `dropped`),
`reflex.changed`, `reflex.reverted`, `body.<kind>` for body events,
`system.started`, `system.restored`.

## Scenarios

EDN, read with `cljs.reader`:

```clojure
{:register [{:trigger :health-low}                                  ; trigger defaults
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
engine state and start from the scenario. The scenario is validated against
the catalog before connecting. `--state-dir <dir>` overrides the
repo's `state/`.

## Job library

Jobs live in `engine.jobs.forestry`, `engine.jobs.storage` and
`engine.jobs.survival`, helpers in `engine.jobs.util`. All are registered in
`engine.catalog`. Every round re-reads the world and does a bounded piece.
Positions in memory are `{:x :y :z}` maps. Failed rounds are counted in job
memory as `:failures`; after three the job emits a warn and ends.

| job | args | precondition | job memory | commits to common |
|---|---|---|---|---|
| `:fell-tree` | `{:species nil :radius 16}` | `:not-yet` until a tree (log column with leaves near its top) is in radius, or the column is already chosen | `:column {:x :z}`, `:species`, `:base` pos | `[:debts :replant]` gets `{:pos base :species}` once, when the tree is chosen |
| `:collect-drops` | `{:radius 16 :filter [names] or nil}` | none | `:skipped` ids of unreachable items | none |
| `:pace` | `{:a pos :b pos :laps 3 :rounds 8 :range 1}` | none | `:rounds-run` | none |
| `:plant-sapling` | `{:at pos or nil :species nil}` | `:not-yet` without a matching sapling carried, or while the target still holds a log | none | removes the planted debt from `[:debts :replant]` |
| `:deposit` | `{:chest pos or nil :items [names] or nil}` | `false` while no chest is known | none | none |
| `:harvest-wood` | `{:species nil :radius 16 :filter nil}` | none | children under slots `:fell`, `:collect`, `:plant` | as its children |
| `:retreat` | `{:radius 8 :step 8}` | none | `:moves` count | none |
| `:sleep` | none | `false` without a known bed, `:not-yet` by day | none | none |

- `:fell-tree` digs up to two logs per round of the chosen column, lowest
  first, and is done when the column has no logs. It walks with `moveTo`
  directly (range 3) rather than a `:go-to` child, since one walk and one dig
  are a single round.
- `:collect-drops` calls `collect` once per round for the nearest matching
  item entity (the primitive walks itself). Done when none match in radius.
- `:plant-sapling` without `:at` plants at the first debt (of `:species` when
  given), walks within 3, equips, places. `occupied` counts as planted.
  With no debt and no `:at` it is done at once.
- `:deposit` takes the chest from `:chest`, else the first `[:places :chest]`
  in common memory. Without `:items` it deposits everything except tools and
  armour (suffixes `_pickaxe _axe _shovel _hoe _sword _helmet _chestplate
  _leggings _boots`, plus shears, bow, crossbow, fishing_rod, flint_and_steel,
  shield, trident), one stack per round. Warn kind `chest_unusable`.
- `:harvest-wood` steps `:fell-tree`, `:collect-drops` (filter defaults to the
  species' log, sapling, stick and apple) and `:plant-sapling` in order and
  returns the first child result that is not `:done`. It waits (`:not-ready`)
  while no sapling is carried, so a tree with no sapling drop stalls it on the
  list rather than failing.
- `:retreat` walks `:step` blocks directly away from the nearest hostile per
  round, at most five walks, then gives up with a `retreat_gave_up` warn.
- `:sleep` walks within 2 of the first `[:places :bed]` and calls `sleep`.
  `sleeping` and `not-night` are done; a taken bed or nearby monster retries
  three times; a missing bed warns `bed_missing` and ends.

Triggers (`engine.triggers`; `:when` receives the register entry's `:args` as
its third argument, and `:now`, the engine clock in ms, in its memory):

| trigger | holds when | job | persistence |
|---|---|---|---|
| `:health-low` | health at most 8 | `:eat` | cooldown 30 s |
| `:hostile-near` | a hostile within 8 blocks; the radius is fixed in the trigger, the job's `:radius` arg is overridable | `:retreat` `{:radius 8}` | cooldown 5 s |
| `:night-and-bed-known` | not day and a `[:places :bed]` is known | `:sleep` | cooldown 60 s |
| `:inventory-nearly-full` | 30 or more carried stacks and a `[:places :chest]` is known | `:deposit` | cooldown 60 s |
| `:every-interval` | no `[:body :every-interval-last]` record, or it is at least `:seconds` (args, default 60) old | `:look-around` | cooldown 0 |

`:every-interval` is a wall-clock reflex: the `:look-around` job looks at a
point three blocks ahead and commits the engine time as `:every-interval-last`
to body memory (so it survives a restart), which makes the trigger stop
holding. With no record it fires at once, so the first look comes at engine
start. Register it with `{:trigger :every-interval :args {:seconds 45}}`;
`scenarios/woodcutter-cuts.edn` is the woodcutter with it first in the
register.

`:inventory-nearly-full` counts stacks, since `self().inventory` has no slot
total; the real inventory has 36 main slots, so 30 is a threshold, not a
measurement. `scenarios/woodcutter.edn` uses the first three triggers plus
`[:harvest-wood :deposit]`.

## Not built (hooks only)

Claims, no-touch regions, flapping counters, no-progress detectors, the HTTP
API, multi-body, soft pathfinding weights, a reflex pointing at a listed
instance (a register entry may carry `:instance` later).

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
bot's `health` (a drop is `hurt`), `death`, `respawn`, `chat`, `wake`, `spawn`, `end` and `kicked` events.

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
