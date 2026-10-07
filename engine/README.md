# Body engine (MVP)

The engine that runs one Minecraft body. ClojureScript owns behaviour and state (jobs, triggers, scheduler,
memory, planner). JavaScript is the mineflayer layer ("primitives") and a few thin adapters (sockets, rendering).

How the parts fit:

- A **body** is one mineflayer bot plus one engine process (`engine.main`).
- **Primitives** are the only way to sense or act on the bot. Sensing is sync and cheap. Acting is async and needs an
  ownership token.
- A **job** is a goal-achiever: a namespace with a `check` (can it usefully run now?) and a `round` (do one bounded piece).
  Jobs compose through job expressions (`seq`, `any`, `repeat`, `until`, `hold`) and call child jobs.
- A **trigger** is an exception handler: when its condition holds, the register fires a one-off **reflex job**, which cuts
  whatever is running. Jobs are goals; triggers are reflexes. Never mix the two.
- The **scheduler** ticks every 250 ms, picks the next round, and cuts by rotating the ownership token.
- **Memory** is one EDN store per body. **Events** are an EDN log with a local HTTP API. Agents use small CLI tools on top.
- **Zones and claims** are social rules that jobs consult. The engine never enforces them.
- **Sensing like a player**: tools and jobs only report what a normal player could see or hear (no x-ray, no entities
  through walls).

## Toolchain and commands

shadow-cljs 3.5.4 (ClojureScript 1.12.145). Async code uses `^:async` functions with `await`; promesa is not used.
`await` works inside `let`, `loop`/`recur`, `cond` and `try` of an `^:async` fn, but not inside a nested `fn`. An anonymous
async fn needs a name: `(fn ^:async round [ctx] ...)` compiles, `^:async (fn ...)` does not. Prefer `(defn ^:async foo-round [ctx] ...)`.

```
cd engine
npm install
npm test                      # cljs tests, builds the agent-tools bundle, then node --test js/**/*.test.mjs
npm run test:cljs             # cljs only (4 GB node heap: --max-old-space-size=4096)
npm run test:js               # JS only (test/tools needs the bundle: run test:agent-tools or npm test on a fresh clone)
npm run test:agent-tools      # builds the agent-tools bundle, then runs test/tools
npm run body -- --agent <name> --world <world> [--scenario <file.edn>]   # node --max-old-space-size=128 --max-semi-space-size=8: V8 flags only work on argv (RSS 335 -> ~255 MB); any other launcher must pass them
```

Run one namespace with `node --max-old-space-size=4096 out/test.cjs --test=engine.<ns>-test`.

Builds (in `dashboard/shadow-cljs.edn`, which holds the engine and dashboard builds; engine outputs go to `engine/out/`): `:test` (`:node-test`, the `engine|jobs|plan` namespaces ending in `-test`, to `out/test.cjs`) and `:body`
(`:node-script`, `out/body.cjs`, `engine.main/main`). The compile JVM is capped at 1 GB.

**Compiling: `tools/compile <engine|dashboard> <build>... [--priority] [--release]`** (repo root). Every cljs compile goes
through it. It queues on `/tmp/mc-compile.lock` (never wrap it in `flock`) and compiles against the checkout's one long-lived
`shadow-cljs server` (both project words use it), which it starts when missing. `--priority` is for the owner's UI rebuilds, agents never use it.
`--release` runs `shadow-cljs release`. `MC_COMPILE_LOG=1` prints lock wait and compile time. `tools/compile engine --stop`
stops the server.

Agent command-line tools run ahead-of-time compiled JavaScript directly in Node, with no compiler or JVM per call.

Layout:

- `js/primitives.mjs` the real mineflayer layer (lifecycle, reconnect, events; its acting and sensing live in `js/prim-*.mjs`); `js/compile-cache.mjs` (first import of primitives.mjs) turns on node's compile cache in `state/compile-cache` (`MC_COMPILE_CACHE=off` disables); `js/connect.mjs` makes the bot (`js/tick-rate-physics.mjs` swaps in physics that follows `/tick rate`, `set_ticking_state` and `step_tick`; stock at 20 TPS; on by default, `:engine.game/follow-tick-rate false` turns it off; a patch that no longer matches runs stock physics and logs `tick-rate.stock`); `js/stub-bot.mjs` is a bare stub for primitive
  tests; `js/view.mjs` writes the view dump for the renderer (skips a reloaded column whose content is unchanged; `BODY_VIEW=0` disables; format in `docs/view-format.md`).
  Other `js/*.mjs` files are helpers per primitive (furnace, enchant, trade, vehicle, leash, light, sight, ...).
- `src/engine/` the engine core only: `core` (API and lifecycle; parts in `core.*`: list edits, register, scheduler, round with act wrapper and call-child, settling, backoff, restart), `memory`, `events`,
  `ctx`, `expr`, `composite`, `registry` (compile-time registries), `triggers` (the trigger registry), `hooks` (job code
  the engine calls), `condition`, `scenario`, `takeover`/`lease`, `perception` (+ `perception.light|store|rays|mobs|persist`), `main`; `engine.path.*` is the planner.
  Perception sees dark cells within `:near` (4) blocks, within `:near-torch` (7) with a torch in either hand.
  One cell: `sensedAt`/`perception/sensed` answers felt, visible (one-ray glance, view cone), remembered (`ageMs`; fluids, doors, gates, trapdoors, fire, and non-solid cells beside a remembered fluid, older than 10 s read unknown) or `{unknown}`; `feel`, `glance`; the wrapped `look` glances where it turned. `blockAt` stays raw.
  A still body in an unchanged world looks again every `:still-ms` (30 s); unchanged means no block or chunk update within 50 blocks, same daylight, and same torch in hand.
- `src/jobs/` the jobs, one namespace each (`jobs.survival.eat`); the build finds them. Helpers: `jobs.lib.*` (shared,
  including the walker `jobs.lib.walk`/`near`/`pass`), `jobs.<area>.*` (one area's). `jobs/hooks.edn` names the hooks.
  Results: `jobs.lib.result` (`stop!` gives up with reason/text/`:cause`, `cause-of` nests a child's stop, `finish!` hands success data).
  `jobs.lib.child/run!` calls a child that still answers `:continue` per step until it ends (50 ms timer between calls; `:continue` after a call cap; a slot run with other args starts afresh).
- `src/triggers/` the triggers, plain fns by area (`triggers.survival.hungry`); `defaults.edn` is the default set.
- `scenarios/*.edn` scenarios (`survival.edn`, `woodcutter.edn`, ...; `scratch/` is git-ignored for hand-run probe starts; `idle.edn` is the idle template).
- `test/engine/` cljs tests (helpers in `engine.test-util`); `test/engine/fake.cljs` is the scriptable fake world.
- `tools/*.mjs` agent CLI tools (below). `fixtures/world/` holds EDN world fixtures run by `../tools/world-test.mjs`.

The cljs side loads JS modules with `js/require`; from `out/`, `(js/require "../js/sight.mjs")` is `engine/js/sight.mjs`.

## Primitives

`createPrimitives({host, port, username, auth})` (resolves once spawned and the column under the body is loaded, up to
10 s) and `createPrimitivesFromBot(bot, {timeScale})` (for tests) return a `primitives` object. The cljs side calls it
through interop, for example `(.moveTo p token #js {:pos #js {:x 1 :y 64 :z 2}})`. JS values are read with `.-field` or `aget`.

### Conventions

- Positions are `{x, y, z}`; block positions are integers. Item and block names are registry names without namespace.
- Every acting method is `async name(token, args)` and resolves to an object with a `status` string. A domain failure is a
  status, never a rejection. A mineflayer error inside an acting call resolves `{status: 'failed', reason}`.
- Rejections happen only for a cut (`err.code === 'cut'`) and for bad args (`'bad-args'`).
- Time bounds are hard: a method that reaches its bound stops and resolves `timeout` (or `partial` for `moveTo`). A `timeout` after the stacks changed carries `inventoryChange` `{item: delta}`: `trade` and `enchant` read it, the others re-read the inventory.
- Sensing methods are synchronous, take no token, and are cheap (scans are bounded by radius and `max`).
- Primitives do not walk. Reach for `dig`, `place`, `transfer`, `sleep`, `attack` etc. is the caller's job (`moveTo` first).

### Ownership token

`setOwner(token)` is called by the engine before each round with a fresh token. Jobs reach primitives only through `ctx/act`.
A call with a stale token rejects at once with `cut`. When the owner changes, every in-flight call of the old token stops
within one game tick (goal cleared, `stopDigging`, window closed, controls released) and rejects with `cut`. `null` means
nobody may act. `isOwner(token)` reports whether a token is current.

### Sensing (sync, no token)

| method | returns |
|---|---|
| `self()` | `{username, pos, health, food, foodSaturation, oxygen, onFire, inWater, inLava, onGround, chunkLoaded, settling, isSleeping, vehicle, effects, experience, dimension, timeOfDay, isDay, raining, thundering, players, held, equipment, inventory}` |
| `worldAge()` | the world age in game ticks (`null` offline); a cheap read of what `self()` carries, used for memory `:age` |
| `entities({radius=16, kind?, names?, ids?, max=32})` | `[{id, name, kind, pos, distance, visible?, ...}]` by distance; `kind` is `hostile`, `passive`, `player`, `item` or `other`; passive mobs and villagers are listed only with a clear line of sight; hostiles, items and players are listed regardless and carry `visible` (a player counts as `sleeping` only in sight) |
| `blockAt(pos)` | `{name, pos, age?, properties?}`, or `null` when the chunk is not loaded |

Notes:

- `health` and `food` are 0..20. `equipment` is `{head, torso, legs, feet, offHand, mainHand}`, each `null` or
  `{name, count, durability?, enchants?: [{name, level}]}`. `inventory` is main and hotbar only: `[{name, count, slot}]`. `effects` are
  `[{name, amplifier, duration}]` with snake_case names. `vehicle` is `{id, uuid, name}` or `null` (`js/vehicle.mjs` repairs
  mineflayer's stale `bot.vehicle` and moves riders with their mount). `engine.senses` derives `isDay` (`timeOfDay < 12542 || > 23460`), `raining` (rain level above 0.2) and `thundering` (above 0.9 while raining) from the raw `timeOfDay`, `rainState`, `thunderState`, and flips `weather-changed` from the raw `weather-levels` event.
- `chunkLoaded` false means the column under the body is not loaded (physics then emits no tick). `settling` is true while
  the body is connected but its senses are not yet trustworthy.
- Entities: items carry `item {name, count}`; players `username`, `sleeping` (only in sight); mobs `uuid`, `baby`, villagers `profession` (`unemployed` when none), sheep `sheared`; leashed
  mobs `leashed`, `leashedToMe`, `leashHolder`; riders `passengers` and `vehicle`. Creepers carry `creeper: true`. Every
  hostile carries `visible` (`engine.senses` over the raw world: a raycast from the eye to the entity's middle; glass, fences, gates, iron bars, water, fire and the like do not block, an
  unloaded cell never blocks). Raw entity lists go through `js/live-entities.mjs` (drops bare, never-spawned entities, picked-up drops and mobs that died).
- Blocks carry `age` for crops (wheat/carrots/potatoes ripe at 7, beetroots 3, sweet berries from 2) and, with
  `properties`, every block state (integers as numbers, booleans and enum names as they are).
- **Hostile trigger rule.** `:hostile-near` holds only for a real danger within its radius (`jobs.lib.danger/danger?`): a
  melee mob the body knows of that has a walkable way to the body, or a ranged mob (skeleton and the like) with a line of
  fire (in a tunnel: solid overhead and both sides along an axis, it must be seen, not only heard). A mob walled in or fenced in (fences, walls and shut gates are 1.5 high, never stepped onto), across a deep trench, or with the body sealed in is no danger. "Knows of" comes from perception's
  mob memory (`engine.perception`): heard within 16 blocks (not a silent creeper), or seen (clear line, within 48, in the
  view cone or heard, and lit; in the dark only within 4). A creeper with a lit fuse hisses (`fusing`) and is heard. A heard
  mob is judged at its rough spot (direction, band: near 4, far 16, derived from `obs/near-band`; `danger/mob-pos`, `mob-distance`), never its exact place. `retreat` and `respond-to-hostile` use the
  same danger rules.
  A hostile that cannot hurt the body firing the response is a trigger bug, not a job bug.

### Acting (async, token first)

Statuses below are the common ones; `reason` and extra fields are in `js/primitives.mjs` and the per-primitive modules.

| method | args | statuses |
|---|---|---|
| `moveTo` | `{pos, range=1, timeoutS=20, maxDistance=64}` (numbers, range from 0, the others above 0, else `bad-args`) | `arrived`, `partial`, `blocked` (reason `noPath`, `planTimeout`, `stalled`, `timeout`), `mounted`. Uses the mineflayer pathfinder and treats doors as walls; jobs walk with `go-to` instead |
| `dig` | `{pos}` | `dug` (`drops`), `missing`, `unreachable` (over 4.5), `cannot` |
| `place` | `{pos, item, click?}` | `placed`, `occupied`, `no-item`, `no-support`, `unreachable`, `failed` (server refused: reason names face, body spot and entities near the cell); buckets pour/scoop at `pos` |
| `jumpPlace` | `{item, count=1}` (max 8) | `done`, `partial`, `failed` (`no-item`, `no-support`, `no-headroom`, `not-raised`) |
| `collect` | `{id, timeoutS=10}` | `collected`, `gone`, `unreachable`, `timeout` |
| `inspectContainer` | `{pos}` | `ok` (`items`, `size`, `free` slots), `missing`, `unreachable` |
| `transfer` | `{pos, direction: deposit/withdraw, item, count}` | `ok` (`moved`), `missing`, `unreachable`, `no-item`, `full` |
| `equip` / `unequip` | `{item, dest='hand'}` / `{}` | `equipped`, `no-item` / `ok`, `empty`, `full` |
| `toss` | `{item, count?, slot?, watchS?}` | `tossed` (`watchS` up to 5 (a thrown item is untakeable for 2 s) waits and adds `takenBy` {collector uuid: count}), `no-item` |
| `craft` | `{item, count=1, table?}`; one recipe per call, never walks; only the handed `table` counts (the job passes a seen one) | `crafted`, `partial`, `no-item`, `out-of-reach`, `unreachable`, `full`, `cannot` (`jobs.items.shortfall` turns shortages into `short` and `alternatives`) |
| `furnace` | `{pos, op: read/load/take, input?, fuel?, output?}` | `ok`, `missing`, `unreachable`, `cannot`, `no-item`, `busy`, `rejected`, `full`; never waits for cooking |
| `enchant` | `{pos, op: offers/enchant, item, choice?, levelCost?}` | `ok`, `enchanted`, `cannot`, `no-item`, `no-lapis`, `no-levels`, `full`, `failed` |
| `chat` | `{message, to?}` validated by `engine.chat/validate` | `sent`, `gone`, `cannot` (`bad-name`, `empty`, `command`, `too-long`), `blocked` (rate), `failed` |
| `eat` | `{item}` | `ate`, `no-food`, `full` |
| `attack` | `{id}` (one swing) | `hit`, `killed`, `gone`, `out-of-reach` |
| `interact` | `{id, item?}` use item on entity | `used`, `no-effect`, `gone`, `out-of-reach`, `no-item`, `full`, `cannot`, `failed` |
| `trade` | `{villager uuid, op: offers/buy, offer?, times?}` | `ok`, `bought`, `gone`, `out-of-reach`, `no-item`, `full`, `cannot`, `failed`; `bought` ends `stopped: window-closed` if the window closed mid-run |
| `sleep` | `{pos}` bed | `sleeping`, `not-night`, `occupied`, `monsters-near`, `missing`, `unreachable` |
| `look` / `wait` | `{pos}` or `{yaw, pitch}` / `{ms}` (max 10000) | `ok` |
| `swim` | `{ms=3000, toward?}` | `surfaced`, `landed`, `timeout` |
| `paddle` | `{ticks, turn?: 'left'/'right', forward?}` | one stroke of the boat the body is in (the boat is simulated client side, sent as `vehicle_move`; `ticks` at most 40): `ok`, `blocked` (the next forward tick leaves the water), `not-mounted`, `not-a-boat`; result `{pos, yaw, ticks, turnDeg, speed}` (the model: degrees and blocks a tick); ticks 0 reads the pose |
| `mount` / `dismount` | `{id}` / `{yaw?, pitch?}` | `mounted`, `already-mounted`, `gone`, `not-mountable`, `occupied`, `out-of-reach`, `hand-full`, `timeout` / `dismounted`, `not-mounted`, `timeout` |
| `useOn` | `{pos, item?, face='up'}` | `used`, `unchanged`, `missing`, `no-item`, `no-room`, `unreachable` (`too-far`, or `no-line`: no point of the block is in view past other blocks' shapes; its own other half never blocks), `cannot` (beds, containers, hazards) |
| `offline` | `{ms=300000}` (max 600000) | `ok`, `cut`, `closed`, `unsupported`, `offline` |

Bounds are 1 to 60 s per call (most 2 to 10 s). `jumpPlace` pillars up: sneak to cell centre, jump, place under the feet,
stop at the first failure. `swim` without `toward` holds jump until the head is out of water; with `toward` it climbs out
onto a rim. Every bound bot has collision half-width 0.31 (0.3 left the body flush against block faces and rejected every move).
Acting while asleep first leaves the bed.

**Offline is body state.** `offline` quits the bot, emits `offline`, waits `ms`, reconnects with the same params (3 tries),
rebinds the library bot, emits `online`. While offline: `self()` is `{status: 'offline'}`, `entities` returns `[]`,
`blockAt` returns `null`, `isOffline()` is true, the register and list are paused (no trigger evaluated, no round
started), and acting calls resolve `{status: 'offline'}`. A cut ends the wait early but the body reconnects first. `close()`
cancels it. Only `createPrimitives` supports it. The engine records why in its `:away` atom (`engine.core/away`); a
log-out (`jobs.survival.log-out`) gives `:why`, otherwise it is `:connection-lost`. `lastKnown()` is the position, health, food, inventory and equipment `self()` read just
before the body left (null online); offline status and inventory show it marked `:last-known`.

An unplanned disconnect (kick, socket end, restart) emits `disconnected` and reconnects by itself: one try, then after
each failure a `reconnect-failed` event and a wait of 1 s doubling up to 60 s, until `online` or `close()`. A bot `error`
is emitted as a body event, never thrown. The engine turns an act's `offline` into a cut; the job stays listed.

The pathfinder goal never outlives a walk, and death and respawn clear it. The pathfinder's movements (`js/movements.mjs`)
never dig or build; hazards (powder snow, cobweb, magma, campfires) add cost, and fire and lava are avoided outright.

### Body events

`primitives.onBodyEvent(listener)` delivers `{kind, ...}` objects: `hurt` (health, food, amount, cause, attacker), `died`
(pos, inventory, experience, cause), `respawned`, `chat`, `picked-up`, `woke`, `player-joined`/`player-left`, `weather-changed` (raining, thundering), `spawned`,
`disconnected`, `error`, `reconnect-failed`, `world-not-loaded`, `physics-stalled`, `offline`, `online`, `sleep-status`
(the action bar's sleep count: `sleeping`, `needed`, or `skipping`), `spawn-set` (pos; the "Respawn point set" line) and `spawn-reset` (the bed is gone). The engine
turns each into a body-memory entry of that kind. `hurt` events are merged into one per second in the event log. A death
drops every listed job (cancelled `:by :death`) and every reflex job; register entries stay, so a trigger that still
holds (`died` starts recover-drops) runs its job again.

### The fake

`(engine.fake/create spec)` in `test/engine/fake.cljs` returns the same primitives plus a `world` handle:
`world.state` (atom of cljs data), `world.calls` (every acting call), `world.hold('moveTo')`, `world.override(name, fn)`,
`world.emit(event)`, `world.setTime(t)`, `world.die()`. The spec sets `self`, `time`, `blocks` (`"x,y,z" -> name`),
`entities`, `inventory`, `containers`, `drops`, `unreachable`, `noPath`, `ages`, `states`, `furnaces`, `enchantTables`,
`recipes`, `raining`, `players`, and more; the namespace docstring and `engine.fake.*` (placing, animals, trade, unequip,
use-on, furnace, enchant) have the details. The fake has the same sensing fields and statuses as the real layer, a
built-in recipe table, and instant, deterministic behaviour (`moveTo` jumps to the target, `offline` waits
`ms * spec.offlineScale`).

`engine.job-fuzz-test` runs every registered job on seeded random args and worlds and fails on a throwing check or round, a plain-false decline, a stop without reason or a sync runaway; carded defects sit in its `known` map. `FUZZ_JOB`, `FUZZ_SEED`, `FUZZ_CASES`, `FUZZ_TRACE`, `FUZZ_EXTREME`, `FUZZ_WRONG_TYPES` replay or widen a run.

## Jobs

A job is a namespace under `src/jobs/` exporting `check` and `round`, and optionally `doc` and `args`. The
namespace follows the path (`src/jobs/forestry/fell_tree.cljs` is `jobs.forestry.fell-tree`).

```clojure
(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]))

(def doc "Eat the best food carried.")                       ; optional
(def args {:item {:doc "the food to eat" :default nil}})     ; optional
(defn check [_c] true)                                       ; ctx -> boolean
(defn ^:async round [c]                                      ; ctx -> :done | :continue | :declined
  (await (ctx/act c :eat #js {}))
  :done)
```

An arg may declare `:type` (`:keyword :int :number :bool :string :item :pos`, or `:enum` with `:values`; numbers take `:min`/`:max`); a value that does not fit is refused at submit, naming job, arg, type and value. `:pos` also normalises `[x y z]` to `{:x :y :z}`. No `:type` = unchecked; nil = unset. List and map args (`jobs.lib.args`: names, targets, counts, box) are checked by the job's `check`, which waits `:bad-args` with a `:why`.

There is no catalog to edit: adding the file adds the job. `engine.registry/jobs` is `{ns-symbol {:check :round :doc :args}}`,
built at compile time. The build hook `engine.build-hooks/add-job-namespaces` lists every file under `jobs`
that defines both `check` and `round` (others are helpers), fails the compile when such a file's `ns` does not match its
path, and the macro `engine.registry/job-registry` emits the map. `check` and `round` must be top-level `def`/`defn` forms, and a job must not
require `engine.registry`.

- **check**: can the job usefully run now? Cheap and side-effect free: its ctx has no token, so `act`, `update-mem!` and
  `remember!` throw. Asked every tick. A check that throws declines with a `system.error` warn.
- **round**: returns `:done` (the job leaves the list, its memory is deleted), `:continue`, or `:declined` (not now; see
  Triggers). Anything else, or a throw, is a failure: the job stays listed, marked failed, with a `job.failed` warn, and
  the scheduler skips it until `retry!` clears the mark. A reflex job that fails is dropped. A cut is never a failure.
- A round works at its goal for as long as it can do useful work; `:continue` means yield (nothing useful to do now,
  e.g. waiting for crops or daylight), not "next step". Holding still on purpose is declared with `ctx/hold-still!`.
- **args**: `{key {:doc :default}}`; the engine merges the spec's args over the defaults. Undeclared keys are refused.

### Job expressions

Wherever a job is named (scenarios, register entries, `submit!`, `do-now!`, the CLI) it is an EDN list, parsed by
`engine.expr` and never evaluated:

| form | meaning |
|---|---|
| `(jobs.survival.eat {:item "bread"})` | a leaf: the job namespace with optional args |
| `(seq e1 e2 ...)` | run children in order, one child round per round |
| `(any e1 e2 ...)` | each round, call the first child whose check passes |
| `(repeat e)` | when the child is done, start it fresh; never done |
| `(repeat n e)` | the same, done after `n` runs |
| `(until g e)` | repeat `e`; done at the top of a round once `g`'s check passes (`g` never runs), e.g. `(until (jobs.time.wait-for-dusk) e)`; a daily routine is `(repeat (seq (jobs.time.wait-for-day) ... (jobs.time.wait-for-dusk)))` |
| `(hold e)` | like `e`, but the list entry holds the body; around a whole spec only, not in a register entry |

An unknown symbol, a wrong arity, an undeclared arg, a bad position arg (`[x y z]` or `{:x :y :z}`) or a nested `hold`
is refused at load with a message naming the spec. Specs are limited to 256 nodes and depth 24. Memory nests by child
slot (`:c0`, `:c1`, ...). Events name a job by its label (`jobs.forestry.fell-tree`, `(seq jobs.a (repeat jobs.b))`).

### ctx

The one argument of a check and a round. Helpers are in `engine.ctx`:

| helper | what |
|---|---|
| `(:args ctx)`, `(:id ctx)`, `(:primitives ctx)` | args, instance id (`"j4"`, `"j4/fell"` for a child), primitives for sensing |
| `(ctx/mem ctx)`, `(ctx/update-mem! ctx f & args)` | this job's memory; update in RAM, saved by the next `act` or at round end |
| `(ctx/view ctx)`, `latest`, `entries`, `since`, `count-in` | body-memory reads (time-filtered) |
| `(ctx/remember! ctx kind data policy?)`, `forget-where!`, `forget-until!` | body-memory writes |
| `(await (ctx/act ctx :moveTo #js {...}))` | call an acting primitive |
| `(await (ctx/call-child ctx slot job args))` | run one round of a child (`:done`, `:continue`, `:declined`) |
| `(ctx/check-child ctx slot job args)` | the child's check, for a parent's check |
| `(ctx/wait ctx reason)` | in a check: false, noting why the job waits |
| `(ctx/result! ctx data)`, `(ctx/child-result ctx slot)` | hand data to the parent in the round that ends `:done` |
| `(ctx/submit! ctx spec opts)` | put a peer job at the end of the list; returns its id |
| `(ctx/emit! ctx kind level fields)` | an event, `:level` kept in the log (the dashboard hides `:debug` unless ticked; dropped once the round is cut); `:warn`/`:error` without `:attention` stores `:notice`. Warn only for a give-up |
| `(ctx/hold-still! ctx reason)` | the round holds the body still on purpose until it ends (`nil` clears); `act :wait` with `:why` does the same while it waits |
| `(ctx/alive? ctx)` | false once the round is cut: a search loop with no act checks it and stops |
| `(ctx/warn-once! ctx key kind fields)` | a warn once per job per process |

**act.** Every acting call goes through `act`: it checks the token, saves memory, emits `action.started`/`action.done`
(debug), calls the primitive, saves again. There is no commit: a cut loses at most the work since the last save, so write a
debt or intent with `update-mem!` before the `act` it protects. After every `moveTo` it writes a `:moved` body-memory entry.

**call-child.** The child's memory is the parent's `[:children slot]` sub-map. The same slot resumes the same child while
it returns `:continue` or `:declined`; on `:done` the sub-map is cleared so the next call starts fresh. A cut anywhere ends
the whole chain's round; cancel and done take the subtree. `submit!` is delegation (a peer on the list), not a child.
Each call emits debug `job.child_started` and `job.child_ended` (`:slot :chain :status :reason`; a stopped result is `:stopped`).

**World knowledge.** `jobs.lib.world-files` reads the plans (`worlds/<world>/plans/<id>.edn`), blueprints
(`blueprints/<id>.edn`), zones, claims and the shared markers (`places.json`, written by the agent tools only) read-only (`jobs.lib.world/marker`, `find-markers` {:text :kind :near :limit}, 50 at most, default 10; coordinates as given, no dimension), re-stat-ed at most every 3 s; a file that turns invalid keeps its last
good copy and warns once. Jobs read it through `jobs.lib.world` (`plan`, `zones`, `claims`, `markers`, `marker`, `footprints`). The engine
calls job code only through `engine.hooks`, named in `src/jobs/hooks.edn` (world open and blank).
`engine.notes` is what bodies saw (`worlds/<world>/notes/<body>.edn`, each body writes only its own file;
`notes/notes`, `notes/note!`). `engine.chat` holds the chat limits (at least 1 s between lines, at most 5 in 30 s, per body).

## Settings

Tuning numbers live in one global map, read by anyone. A namespace under `jobs/` or `triggers/` declares its keys in a top-level `(def settings {::wait-ms {:default 5000 :doc "..." :type :int :min 0}})` (the job arg spec shape; the key's namespace is the declaring one, `:engine.<area>/<name>` in `engine.settings`). `engine.registry/settings` gathers them at compile time, helper files included. The engine's own keys (`:engine.perception/`, `lease`, `events`, `event-api`, `backoff`, `hurt`, `entities`) sit in the `settings` of their namespaces under `engine/`; `engine.settings-registry/settings` gathers them at compile time and `engine.main` adds its own (the vanilla facts hit range and the sleep monster box are plain constants in `engine.senses`); `(perception/defaults)` and `(backoff/defaults)` read them at use.

- Read `(settings/get settings ::wait-ms)` with the namespace's own map (a typo throws); never inside a top-level `def`.
- Layers, low to high: the code `:default`, `worlds/<world>/settings.edn` (`:world`), `worlds/<world>/agents/<Name>/settings.edn` (`:body`). Files are EDN maps `{key value}`, read at body start (`settings/load!` again reloads).
- A bad value, unknown key or unreadable file keeps the lower layer and emits one `settings.bad` `:info` event naming the key and file.
- `(settings/resolved)` gives `{key {:value :layer :doc}}`. One body per process: `load!` throws for a second one. Tests wrap in `settings/with-settings {key value} f`.
- Rates: the **game rate** (`(settings/game-rate)`, `frozen?`, `rate-source`) is the server's `/tick rate`, read from `set_ticking_state` by `js/game-clock.mjs`, attached in `connectBot` before spawn so the join packet counts, and independent of the physics shim; with no packet it is 20, source `:assumed`, and one `game-rate.assumed` `:info` is emitted. Game ticks (item despawn 6000, smelting, the night) become ms with `ticks->ms` / `ms->ticks`, re-read at each use. The **physics step** is `(settings/physics-ms p)`: the shim's interval, else 50. Wall-clock timers (engine tick, JS time scale) are never converted. Memory entries and views carry `:age`, the world age in game ticks (still while the tick is frozen); drop timers count it.

## Memory

One EDN store per body: `worlds/<world>/agents/<name>/engine/memory.edn`, `{:entries {kind [entry]} :policies {kind policy}}`.

- An **entry** is `{:t wall-clock-ms :wt world-time :age world-ticks :data ...}` (`:age` when known), newest last. **Kinds** are an open vocabulary: `:hurt`, `:died`,
  `:chat`, `:restart`, `:bed`, `:chest`, `:home`, `:looked`, `:moved`, `:picked-up`, `:fed`, `:slept`, `:shelter`, `:threat`,
  `:tidy`, `:scaffold`, `:forestry/replant`, `:job/j7`, and so on. Body events become entries of their kind; every start
  appends `:restart`.
- **Policy** `{:cap n :ttl ms}` is stored beside the kind. A new kind gets cap 50 and ttl one hour unless a write gives
  one; `:forever` is a keyword; the cap drops the oldest.
- **Sweep** on boot, every `:sweep-ms` (60 s) and before every save: drops expired entries and `:job/<id>` kinds whose
  instance is not live. Saves happen around every `act`, at every round end and whenever the engine writes.
- **Reads** (`engine.memory`) take a view `{:data :now}` and are time-filtered: `entries`, `latest`, `since`, `count-in`,
  `place`.
- **Job memory**: a listed (or running reflex) instance owns kind `:job/<id>`, cap 1, forever, deleted on done, cancel or
  drop. Its data is `{:args ... :children {slot child-map}}` plus the job's own keys.
- **Named places** (`jobs.lib.places`): a place is a kind named after it (`:bed`, `:chest`, `:home`, `:food-source`, or a name
  of 1 to 32 lowercase letters, digits, dashes) with one `{:pos {:x :y :z}}` entry, cap 1, forever. `jobs.memory.set-place`
  records, moves or verifies one; `jobs.memory.forget-place` removes one. `jobs.survival.sleep` and `jobs.storage.deposit`
  record the bed or chest they used when none is recorded; they never overwrite a live different one.
- **Deaths**: engine core adds one `:deaths` entry `{:pos :dimension :cause :death-t}` per `:died` event, in any mode (cap 10, forever).
  Jobs read it with `(ctx/entries c :deaths)`; a death the body was offline for is not listed.

## Triggers and the register

A trigger is a plain fn `(fn [world view args plans live] bool)` (view is a memory view, args the entry's). The default
trigger set `src/triggers/defaults.edn` names each by id; `engine.triggers/all` is built from it at compile time, so a new
trigger is a fn plus one line:

```clojure
{:id :hostile-near :when triggers.survival.hostile-near/hostile-near   ; the fn
 :job (jobs.survival.respond-to-hostile)                             ; default job spec
 :args triggers.survival.hostile-near/defaults                         ; a map or a var, merged under the entry's :args
 :persistence :retry}                                                ; :retry | :cooldown (with :cooldown-s) | :stop
```

The **register** is an ordered vector of entries `{:id :trigger :job :args :persistence :cooldown-s :backoff :builtin?}`.
Each tick the engine fires the first entry whose `:when` holds and which is not muted or cooling down.

- A firing reflex cuts a running listed job (which stays listed and runs again once no reflex holds the body). It cuts a
  running reflex job only if it sits above that one. A reflex whose job is running does not fire again.
- A reflex job runs one round. Its check is never asked: the trigger is its check. A `:continue` counts as `:declined`
  (warn `reflex.continued`, once per reflex an hour). On `:done`, `:declined` (also an info `reflex.declined`) or a throw it is removed. If the trigger
  still holds it fires again as a new instance, subject to persistence. So a reflex job must return `:done` or `:declined`
  at the top of its round when there is nothing to do. Combinators follow this: `any`, `seq` and `repeat` return `:declined`
  when the relevant child declines.
- A reflex job cut by a higher reflex is dropped with its job memory. What it must not lose lives in body memory
  (recover-drops' `:recover-trip`, recover's `:hurt`, the shelter's `:shelter`).
- **Persistence**, when the trigger still holds as the job ends: `:retry` fires again next tick; `:cooldown` waits
  `:cooldown-s` counted from the job end; `:stop` waits until the condition has been false once. An entry's own values
  override the trigger's.
- **Settling**: after a login, reconnect, respawn or teleport (a forced move over 16 blocks) no trigger is evaluated until
  `settleMs` (1 s) after the column under the body loads. A reflex that ends meanwhile is judged on the first ready tick.
- Agent-only edits (`engine.core`): `register-reflex!`, `remove-reflex!` (not built-ins), `mute!` (with TTL), `move!`
  (`{:above id}`/`{:below id}`), `clear-change!`. A change expires back to the default and emits `reflex.reverted`.

Built-in triggers, in the order of `triggers/defaults.edn` (the default register's priority, most urgent first). A scenario with no `:register` key registers all of them in that order; `:register []` registers none; a list registers exactly that list:

| trigger | holds when | job | cooldown |
|---|---|---|---|
| `:suffocating` | drowning (in water, oxygen below `:min-oxygen` 12, head not in air) or head in a suffocating block | `breathe` | 0 |
| `:burning` | on fire or in lava, without `fire_resistance`, or an unscooped `:extinguish-pour` in memory | `extinguish` | 0 |
| `:wedged` | a full block fills the cell of the feet and the eye cell is not solid (sand fallen on the body; both solid is `:suffocating`); quiet while a recent `:unwedge-blocked` entry names the cell | `survival.unwedge` | 0 |
| `:hostile-near` | a real danger (see Sensing) within `:radius` 8, ranged within `:ranged-radius` 16; `:visible-only false` counts heard mobs | `respond-to-hostile` | none (retry) |
| `:hungry` | food below `:food` 6 plus one per missing hp (at most 18: below 18 nothing heals); or hurt, below 18 and common food carried; or health below `:health` 7 and food carried (eats to 20) | `get-food` | 90 s |
| `:night` | night, awake, and a bed to use or carried, someone asleep, unroofed, or shut in its shelter; by day shut in its shelter or a bed it put down outside its zone still stands | `survival.night` | 10 s |
| `:door-left` | a door a walk opened and meant to shut was last seen open 10 s after (never read through a wall) | `maintenance.shut-doors` | 5 s |
| `:died` | a `:died` under the despawn window (6000 game ticks) old with a newer `:respawned` and no `:recovered` | `recover-drops` | 30 s |
| `:inventory-nearly-full` | at most `:free` 2 of 36 main and hotbar slots empty | `storage.make-room` | 120 s |
| `:scaffold-left` | the scaffold ledger holds blocks whose job is gone | `access.cleanup` | stop |
| `:tidy-pending` | body safe and on the ground, a `:tidy` entry pending; a cell a run tried (the entry's `:tried` stamp) waits 2 min or until the body moves 8 blocks | `survival.restore-broken` | 10 s |
| `:pen-gate` | a planned fence gate within 8 was last seen open, body more than 2 away; no `:opened`/`:gate-held`/`:gate-gave-up` entry for it | `animals.shut-gate` (waits `:open-s`, 4 s, before shutting; a gate it left for an animal or an interrupted walk is paused 30 s) | 5 s |
| `:mounted` | the body rides something and no live job holds a vehicle | `movement.leave-vehicle` | cooldown 30 s |

The dangers (`:suffocating`, `:burning`) have no cooldown and no backoff. Needs rest with a reason the agent
sees (`:hungry` after `food.none`). There is no timer trigger: periodic work is a job.
To see a trigger fire without its real job, register it against `jobs.debug.notify`.

### Conditions

An ad hoc `:when` can be a condition: an EDN list read by `engine.condition` against a fixed table, never evaluated.

```clojure
(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
(held-for 5 (hostile-near 10))
(not (known? (place :home)))
(or (not (known? (since :bred-cows))) (> (since :bred-cows) 1200))
```

- Operators: `and or not < > <= >= =`, `(held-for seconds cond)`, `(known? x)`.
- Facts: numbers `(health) (food) (inventory "item") (free-slots) (distance-to pos) (blocks-near "name" r) (seen blocks only)
  (since :kind)`; a position `(place :kind)`; booleans `(daytime) (in-water) (hostile-near r, a real danger) (burning) (suffocating)
  (night-unsafe) (wearing "item")`. `(since :kind)` is the seconds since body memory last recorded an unexpired
  entry of that kind (unknown when none).
- A fact can be unknown (offline, no such place). Unknown propagates; `and`/`or` are three-valued; a condition holds only
  when definitely true; `known?` is how to ask about absence.
- No variables, arithmetic or functions. What the vocabulary cannot say becomes a new fact in the table `defaults.edn` names (`:facts`).
- `compile` validates once at registration; a bad form is refused as data `{:ok false :reason :at :message :allowed}`.

## The scheduler

`tick!` runs every 250 ms:

1. Expire register changes; sweep memory when due.
2. Evaluate the register. A firing entry that preempts the holder cuts by rotating the token and runs a round of its job.
3. Otherwise, if a round is in flight, nothing.
4. Otherwise the list: a holding job runs if its check passes, else the body idles; else the cut job if its check passes;
   else round-robin from the job after the last one run, to the next whose check passes. If every check declines, nothing
   runs until the next tick.

**Waiting is never silent.** A check says why with `(ctx/wait ctx reason)` (a keyword or a map with `:reason`). The
scheduler emits one `job.waiting` when a check first declines and again only when the reason changes; it shows as
`:waiting` in `jobs show`/`list` and `observe`. A parent whose round returns a child's `:declined` keeps the child's reason;
`jobs.lib.declined` parks such a parent until the child's check passes.

**List edits** (agent): `submit!` (appends; opts `:hold?`, `:next?`, `:now?`, `:by`), `cancel!`, `retry!`
(clears a failed mark), `do-now!` (cuts the running listed job, never a reflex, lists the new job directly before it as a
holder; the cut job continues afterwards with its memory). `:next?` lists the job directly after the current one so it gets
the very next round, without cutting anything.

**Persistence.** The list, register and changes are written to `engine.edn` on every change. On boot they are reloaded,
reflex instances are dropped, the in-flight job resumes first, and a `:restart` entry is appended. A register entry whose
saved args hold keys its job no longer declares is kept with those keys removed and raises a required attention request;
an entry whose trigger or job is gone is dropped with a `system.dropped` warn. `core/shutdown!` (SIGINT/SIGTERM) rotates
the token so the in-flight round is never booked; the job stays listed.

**Doing nothing.**
- `job.idle` (warn, once per spell): a round holds the body with no act in flight, no declared hold, and its last act (or
  start) more than `:idle-s` (10) ago. An act or clearing a hold ends the spell.
- `job.holding` (info): a declared hold (`ctx/hold-still!`, `act :wait` with `:why`); `jobs show` gives
  `:holding {:reason :since}`.
- `job.fruitless`: a required attention request when every act of a listed job failed in 3 rounds in a row (a round that
  ends the job does not count; a round with no act neither counts nor resets); resolved by a round with progress. It only flags.
- `job.round_started` and `job.yielded` are debug.

**Backoff.** Register entries only: a reflex whose runs keep failing would fire as fast as the tick, so its entry waits.
Listed jobs are never backed off.

- `act!` classifies each act result. Failure statuses are `blocked failed unreachable cannot timeout gone out-of-reach
  no-item no-support no-headroom occupied full disconnected unsupported not-night monsters-near no-effect unchanged
  no-room missing`; anything else is progress. `look`, `wait`, `equip` and `steer` are neutral. A walk round
  (`jobs.lib.near`) counts as one `:walk` act: `arrived` is progress, `partial` and long detours are neutral, a blocked
  walk is a failure.
- A **fruitless run** made no progress and every act failed, or it declined or stopped. Cut runs and throws do not count.
  The first progress act resets everything.
- Schedule `{:after 3 :first-s 1 :max-s 30}`: after `:after` fruitless runs the entry does not fire for `:first-s`
  seconds, doubling per further fruitless run up to `:max-s`.
- Config, most specific wins: engine `:backoff` < the trigger's `:backoff` in `triggers/defaults.edn` < the entry's.
  `false` turns it off (`:suffocating`, `:burning`, `:hostile-near`, `:night`).
- State is the non-persisted `:backoffs` atom, cleared after every pause (offline, settling, manual control).
- Events: warn `reflex.backoff`, repeated at most every `:backoff-alert-ms` (300000); info `reflex.recovered`.

## Events and the local API

The engine writes one EDN map per line to stdout and `worlds/<world>/agents/<name>/engine/events.edn`. The contract is in
`docs/event-stream.md`. Fields: `:seq`, `:generation-id`, `:time-ms`, `:source`, `:kind`, `:context` (job id, child chain,
round, reflex id, action-call id), `:data`, `:message`, `:attention` (omitted/`:none`, `:notice`, `:required`),
`:request-id`. Required attention requests stay in saved state until explicitly resolved. The log keeps 64 MiB across
`events.edn` and `.1` to `.3` (setting `:engine.events/max-bytes`, min 1024); a cursor is `{:stream-id :seq}` and a
rotated-away cursor gets an explicit gap.

A job that ends with a result of `:status :stopped` gets a `:stopped` warn event; its text is the result's `:text` (clipped to 200 chars), else `stopped: <reason>`.

HTTP over `worlds/<world>/agents/<name>/engine/events.sock` (mode 0600; all bodies and responses are EDN):

| request | behaviour |
|---|---|
| `GET /snapshot` | coherent engine state, outstanding requests, cursor |
| `GET /events?stream-id=&after=&limit=` | an event page after a cursor, with gap indication |
| `POST /attention/resolve` | `{:request-id :reason :handled}`; does not retry or cancel its job |
| `POST /chat` | one public line or whisper, sharing the chat limits |
| `GET /status?limit=` | compact body/job/attention projection (includes `:died` while a death is under 6000 game ticks of world age old; supplied by recover-drops through the status extras main passes to event-api/create) |
| `GET /inventory` | carried stacks and worn equipment (with a short enchants list), read-only |
| `GET /job?id=` | one job's parsed spec, args, state, linked requests |
| `GET /catalog?kind=jobs\|triggers&prefix=` / `kind=job\|trigger&name=` | names page / one description |
| `GET /triggers[?id=]` | the register with live mute/move/cooldown/backoff state (`?id=` adds `:explain`) |
| `POST /triggers` | one register edit |
| `GET/POST /jobs` | list, `:submit`, `:interrupt`, `:cancel` (also a running reflex job: ends `reflex.ended :cancelled`, its trigger may fire again), `:cancel-all`, `:retry` (used by `tools/jobs.mjs`) |

Register edits (`POST /triggers`, applied between ticks; a refusal is `{:ok false :reason :at :message}` and changes
nothing; `:by` names who asks, `:generation-id` is checked when given):

| `:op` | request |
|---|---|
| `:put` | `{:op :put :id :hungry-12 :trigger :hungry :args {:food 12}}` or ad hoc `{:op :put :id :bread-low :when (< (inventory "bread") 8) :job (jobs.survival.eat)}`; also `:persistence :cooldown-s :backoff :ttl-s`. Creates or replaces a custom entry (built-ins refused). Ad hoc entries default to `:persistence :stop` |
| `:remove` | `{:op :remove :id ...}` (built-ins refused; the round in flight finishes) |
| `:mute` | `{:op :mute :id ... :ttl-s 60}` |
| `:move` | `{:op :move :id ... :above :hostile-near :ttl-s 60}` (or `:below`) |
| `:clear` | `{:op :clear :id ... :property :mute}` (or `:position`) |
| `:upgrade`, `:decline` | `{:op :upgrade :ids [..]}` adds the default triggers the last restart offered (all when no `:ids`; ids the register has are skipped); `:decline` marks them seen, never offered again; reply `{:ok true :added/:declined [ids] :offered [left]}` |

`POST /jobs {:op :cancel-all ...}` cancels every listed job (running, queued, held, failed) through the single-cancel path,
one `job.cancelled` event each; the register is untouched, so reflexes keep running.

## Manual takeover

For rescuing a stuck body by hand. The body listens on `worlds/<world>/agents/<name>/engine/control.sock` (mode 0600).
Three control modes exist: normal scheduling, `do-now!` (an urgent holder), and manual takeover. `take` cuts the current
holder and mutes all triggers and the loop until release or lease expiry: only the driver's slot job runs (the job the
driver submits with `:by` = the lease's `who`; a new one replaces it). It gets one call and leaves the slot whatever it returns: `:done`/`:stopped` as returned, an error ends it `job.failed` with attention (unlisted, not parked), a cut ends it `:stopped` reason `:cut`, `:continue` or `:declined` end it `:stopped` reason `:yielded` (or the child's wait reason) plus the wait text, a failing check ends it at once `:stopped` with the check's reason. Then the body idles. Jobs from others queue and
wait, `:next?`/`interrupt` from them are refused `:manual-control`, release cancels the slot job, and a slot round in
flight keeps the lease alive (nothing else does); `/drive set` answers 409 `job-running` while a slot job exists; `/drive stop` cancels the slot job (running or just submitted), then stops. A cut listed job (not the slot job) stays listed; a cut
reflex job is dropped. Who drives is decided by a lease, first come (`take` is refused `held-by <who>` otherwise). The
lease is not saved; restart or going offline ends it.

- `POST /drive` body `{op, who, ...}`: `take` (`why`, optional `idleS` 1..3600), `set` (`controls`, `look`, `ms`), `stop`,
  `ping`, `release` (`force` reclaims). `GET /drive` reads the state without resetting the silence clock.
  Refusals: `offline`, `settling`, `held-by <who>`, `not-taken`, `not-driver` (`detail {:holder :idle-left-s}`), `bad-args`.
- Dead-man: untimed controls are released after 1 s without an op. The takeover ends after 15 s of silence by default
  (`--drive-idle-s`, or `idleS` on `take`); `ping` keeps the lease. Timed holds last at most 10 s.
- `world.mjs submit <move-to|dig|place|use-on|interact|wear|equip|mount|dismount>` submits the job of that action (`go-to`, `blocks.dig`, `blocks.place`,
  `blocks.use-on`, `items.interact`, `items.wear`, `items.equip`, `movement.mount`, `movement.leave-vehicle`) with `:by` = `--who`: under manual control it is the slot job.
- Rules live in `engine.lease` (pure); `engine.takeover` applies them; `engine/js/control.mjs` is a stateless socket adapter.
- Events: `system.takeover_started`, `system.takeover_ended` (reason `released`, `forced`, `idle`, `offline`, `shutdown`),
  `system.drive_deadman`.

```
node engine/tools/drive.mjs ProbeDrive --world claude take --who claude --why "stuck in a pit" [--idle-s 30]
node engine/tools/drive.mjs ProbeDrive --world claude look 270 0 --who claude      # yaw 0 south, 90 west, 270 east
node engine/tools/drive.mjs ProbeDrive --world claude hold forward,jump 2000 --who claude
node engine/tools/drive.mjs ProbeDrive --world claude state | stop | release [--force]
node engine/tools/world.mjs ProbeDrive --world claude submit move-to -5 64 -7 --who claude [--wait --timeout 60s]
```

Exit codes: 0 ok, 1 refused, 2 no running body or bad usage. The view page can drive too (`docs/view-format.md`).

## Running a body: scenarios

A scenario is EDN read with `cljs.reader`:

```clojure
{:register [{:trigger :hungry}
            {:trigger :hostile-near :args {:radius 12}
             :job (jobs.survival.retreat {:radius 12 :step 10})}]
 :queue    [(jobs.movement.go-to {:pos {:x 10 :y 64 :z 0}})
            (hold (jobs.time.wait-for-day))]}
```

`npm run body -- --agent <name> --world <world> --scenario <file>` loads `worlds/<world>/agents/<name>/config.json`
(`username`, optional `viewDistance` chunks, default 8) and `worlds/<world>/world.json` (`host`, `port`). It refuses to start when `--world` is missing, and with exit
code 3 when that body is already running (it binds `engine/body.sock` first). If `engine/engine.edn` exists the saved list
and register are restored as they are (a manual slot job is ended, `stopped` reason `restart`); `--fresh` discards saved engine state (memory is kept). The scenario is validated
before connecting; on a restart an unknown trigger in it is skipped with a warn and an attention request, and scenario triggers the
body never had are offered in one attention request: `triggers upgrade [ids]` adds them (`--upgrade` at start adds all), `triggers
decline [ids]` never offers them again; ids not in the offer come back under `:ignored` with a reason. Other flags: `--worlds <dir>`, `--drive-idle-s <s>`.

`survival.edn` lists the survival triggers and queues `(repeat (jobs.movement.look-around))`.
`woodcutter-cuts.edn` registers an ad hoc `:look-timer` condition to cut a long job on a schedule.
`test/engine/scenarios_test.cljs` runs `woodcutter` and `survival` end to end against the fake.

## Zones and claims

Zones (`worlds/<world>/zones.edn`) and claims (`claims.edn`) are a social rule that jobs consult. The engine never enforces
them: `act!` and the primitives check neither, and a job may ignore them. The helper is `jobs.lib.access` (`may?` for
`:dig :place :sow :harvest :take :put`) over the pure `jobs.lib.access.zones/verdict`. First match wins:

1. no zone list read: `:no-zones`;
2. the cell is in another active plan's footprint (the plan the job builds is left out): `:footprint`;
3. a zone holds the cell, its `:owner` is not the body (case-insensitive; `"unknown"` is foreign) and its `:allow` lacks
   the action (`:allow` is what others may do): `:zone`. The owner may always act;
4. an active, unexpired claim of another owner holds the cell: `:claim`;
5. else ok (`:own-zone`, `:own-claim`, `:open`).

- Every job that digs, places or takes accepts `:ignore-zones? true` (default false). By default it skips a refused target
  with one warn naming the zone, claim or plan; a cell two plans share is such a target, and `:ignore-zones? true` acts on it anyway.
- `withdraw`, `deposit`, `smelt` and jobs built on them give up `:refused` on a foreign container.
- Survival jobs (`breathe`, `extinguish`, `dig-in`, the crop dig of `get-food`) prefer a permitted option and break
  another's block only as a last resort (warn `<job>.trespass-last-resort`). They never take from a foreign container. A
  missing zone list never blocks a survival job.
- A plan's footprint is the body's own when its `:metadata :by` equals the body's username (case-insensitive).
- A body's own plan never blocks its explicit work: `blocks.dig`, `blocks.place`, `access.stair` and `access.pillar` act on
  its cells. `gather.mine` and `forestry.fell-tree` spare own-plan cells unless `:spare-own-builds false` (default true).
- **Tidying** (`jobs.lib.tidy`): a dig or place that breaks another's block is noted as a `:tidy` memory entry.
  `jobs.survival.restore-broken` puts the cells back when the body is safe; the `:tidy-pending` trigger starts it.

## Job library

Each job declares its args with defaults and its full rules in `doc`: read it with
`node engine/tools/observe.mjs <body> --world <w> catalog job <name>`. Failed rounds are usually counted in job memory as
`:failures`; after a few the job warns and ends. Ends are: `:done` (normal), a `:short`/`:gave-up` style hand-over result
(`ctx/result!`), or a warn. Jobs that need to be rethought for a world state decline (`ctx/wait`) rather than spin.

**Movement and time**

| job | what it does |
|---|---|
| `movement.go-to` `{:pos :place :range 1 :doors :shut :escalate true :dangers true :dark true :tolls nil :zone-tolls false :drop-cost 1 :costs nil :landing nil :gait nil :min-health nil :max-damage nil :hp-seconds 10 :food nil :fall-margin 1 :leg-s nil :retry true :look-round true :warn true :goal-danger :wait}` | Walks to a cell (`:place` = a named place in memory, else the shared marker of that name (1 to 100 characters, exact then any case), instead of `:pos`; refused `:unknown-place`, `:bad-name`, `:bad-tolls`, `:bad-drop-cost`, `:bad-costs`, `:bad-landing`, `:bad-gait`). One call is one whole attempt: it plans and walks (`jobs.lib.walk`, search slices of about 100 ms with a 50 ms timer between, walks of at most 60 s) until it arrives or gives up; `:continue` only while an escalation or put-back child, or healing, waits. Each call re-checks the world; a cut ends it at once. Hands over `{:arrived true}` or `{:status :stopped :arrived false :reason :unreachable :why ... :text <reason in words>}` (the job ends `:stopped`; planner reason, `:stuck`, `:off-plan`, `:no-progress`, `:moved-while-searching`, ...; also `:at` (feet cell) and `:near` (blocks to the goal)). Walks toward unloaded land to the loaded edge. Sprints a diagonal jump past a corner block as high as the landing; at food 6 or less (no sprint) it refuses gap jumps 2-3 cells wide (`:gap-sprint`) and other moves (`:abilities`). On a vine or ladder above a walk step it holds still until it has dropped to the step (forward would climb). A hostile first sensed within 12 blocks of the way still ahead (`:dangers` on) makes the walk plan again, once per mob. Three walks without getting closer give up; so does a call sent back by three cuts with no 2-block gain between, each restart more than 3 blocks farther from the goal than where the cut found the body (a reflex sent it back; in-place cuts neither count nor break it; the walk's approach is kept in process, not job memory; the count clears when the closest approach improves 2 blocks, or for a new goal; settings `jobs.movement.go-to/send-back-margin`, `cut-progress`, `max-cut-restarts`; `:why :cut-again`, `:nearest-hostile` a guess at the cause); a walk stuck or off its plan at a cell is a `walker-fault` warn and the next plans go round that cell when there is a way. `:escalate`: a body shut in, with no door or gate it can use beside it (iron doors with no button, lever or plate in reach, and every door under `:doors :never`, are walls; a body standing in the free part of a shut door's cell, the panel on its far side, counts as shut in), makes a way (pillar, stair, clear-path, or a walk to the nearest wall; a method that fails is followed by the next; a stair refused on one heading tries the others), digging only natural terrain and blocks it put back itself, outside others' zones (a way out only a zone refuses ends `:escalation {:step :none :why :zone}`); every dug cell goes into the `:tidy` ledger at once, and it puts them back once it got on past them (never one it stands on or needs, nor one more than 2 below it out of reach: no walk back down into its own stair), restore-broken the rest. `:doors` is `:shut` (open, pass, shut again), `:leave-open` or `:never`; an iron door is opened by pressing its button, pulling its lever (left on, the door stays open; costs 6 s in the plan, so a way round is preferred) or standing on its plate, and read back for 600 ms, else a wall. `:goal-danger` for a goal inside the full-cost radius of a known mob the body would flee: `:wait` (default) walks on with an info `goal-in-danger`, `:end` gives up at once (`:why :goal-dangerous`, `:mob`, `:mob-pos`, `:mob-distance`). `:dangers false` plans past known dangers; `:dark false` plans dark cells like lit ones (default: a dark cell, seen dark or unseen at night, costs twice a lit one); `:tolls [{:x :y :z :factor}]` (`jobs.lib.cost/farm-tolls`, `zone-tolls`) makes those cells cost factor times their seconds more, crossed only when the way round is longer; `:drop-cost n` scales the cost of a drop (0 free), `false` takes no drop of 2 or 3; `:costs {:swim-h 1 ...}` sets the planner's seconds for a move (names in `jobs.lib.cost.planner`, others keep their defaults; a downward bubble column is a forced descent to its bottom (`:bubble-down` a block), left only there, never brushed by a diagonal, a head out of water, in a column or at the surface, gets back 4 s of air a second, a dive drains 1/(Respiration+1) s a second and a turtle helmet adds 10 free seconds a dive (read from the body's helmet, no arg; effects are not counted); a plan made with the head under water starts from the air the oxygen says is used, with no turtle grace and at least the 3 s margin to reach air (to a goal with its head in air, no limit: each second past the supply is priced as 2 hp); `:swim-h` and `:exit` under a block's walk, 0.23 s, are `:bad-costs`); `:landing {"hay_block" 0.2}` sets the share of a fall's damage taken landing on a block (negative: a bounce, no damage but settle time, only onto a pad in view with bouncing blocks or a wall all round and not under `:gait :sneak`, else the full fall; defaults hay_block 0.2, honey_block 0.2, slime_block -1; entries override); `:gait` is `:auto` (the walker sprints long straight runs and gap jumps; at food 6 or less the plan prices every step as a walk), `:walk` (never sprints: no gap jump of 2-3 cells) or `:sneak` (about 1.3 blocks a second, never sprints; sneak held on level steps so it never walks off an edge, let go for a planned drop, 1-cell gap jump, climb, water or scaffolding; a climb down from scaffolding holds sneak under every gait); nil: the body's `:walk-settings` `:gait`, else `:auto`; a walk may spend a damage budget (`jobs.lib.cost.health`: health over `:min-health` 12 less 1, at most `:max-damage`; 0 while burning or poisoned; kept under the hungry line) on drops over 3 blocks (up to 16, feather falling and protection cut the damage) and plants that hurt, each hp priced `:hp-seconds` (default 10, `:bad-hp-seconds`; a known danger is priced the same: its hp a second times the seconds near it); the food it counts on (budget, sprint, healing) is `:food` (0-20, default the body's own food each step); falls over the plan by more than `:fall-margin` emit an info `damage-mismatch`; `:min-health` (1-20, `:bad-min-health`) and `:max-damage` (>= 0, `:bad-max-damage`) set it. Absent, the body's memory `:walk-settings` entry (`{:min-health :max-damage :gait}`, written with `memory.remember`, `:ttl-s :forever`, `:cap 1`) sets them, a value out of range ignored with a `walk-settings.bad` info; the arg wins. When it is why no way is found the body waits to heal (info `go-to.waiting-health`; eats a carried food first when hungry), else drops over it down to 1 hp (info `go-to.over-budget`); a given `:min-health` or `:max-damage` is never crossed: `:why :needs-health`; `:leg-s n` walks one leg of at most n s and ends `{:arrived false :leg true}` for a chaser to re-aim; `:retry false` gives up at once on a walk that got no nearer (no 3 tries); `:look-round false` skips the arrival look; `:warn false` makes a give-up or refusal an info event; `:zone-tolls true` also tolls other bodies' zone cells near each walk (none under `:ignore-zones?`); `:ignore-zones? true` lifts the zone and claim refusals of the escalation |
| `movement.path-preview` `{:pos :place :range 1 :doors :shut :dangers true :dark true :tolls nil :drop-cost 1 :costs nil :landing nil :gait nil :min-health nil :max-damage nil :hp-seconds 10 :food 20 :one-way :open}` | Dry run of a go-to: plans with go-to's policy, costs, dangers and tolls, walks nowhere. Hands over `{:status :completed :found true :length :seconds :summary :steps :moves :doors :waypoints}`, or `{:status :stopped :found false :reason ... :near}` (`:reason :partial` with `:partial` {the same up to where the plan ends}; refusals too carry `:found false`: `:bad-pos`, `:bad-name`, `:unknown-place`, `:bad-tolls`, `:bad-drop-cost`, `:bad-costs`, `:bad-landing`, `:bad-gait`, `:bad-min-health`, `:bad-max-damage`, `:bad-hp-seconds`, `:unsupported`) |
| `movement.look-around` `{:every-ms 2000}` | Faces a random point; writes `:looked` |
| `movement.follow` `{:player :range :radius}` | Keeps within range of a player: walks (go-to legs) in one round until in range, then yields; give-up counts are per round |
| `movement.linger-near` `{:pos :range 3 :wait-s}` | Stays within range of a cell for `:wait-s` s spent in range (declared hold `:lingering`; walks back with a go-to child when out of range); hands over `{:lingered true}`, or stopped `:unreachable` after three failed walks back |
| `movement.mount` `{:id or :name}` | Waits (:no-vehicle) until the vehicle is in sight, then walks to a boat, raft, minecart or rideable mob and gets on; by `:name` prefers an unoccupied one; boats and minecarts need no free hand; the body stays aboard only in manual mode (else the `:mounted` trigger steps off after the job ends); stopped `:gone`, `:not-mountable`, `:occupied`, `:hand-full`, `:timeout`, `:unreachable`, `:aboard-other`, `:bad-args` |
| `movement.leave-vehicle` | Gets off a boat, minecart or mount (run by `:mounted`; one run, retries inside, stopped when still aboard, fired again after the cooldown) |
| `movement.boat-launch` `{:action :launch/:recover :pos :item :board :id :max-s}` | Launch: puts a carried boat (else fetched) on shore-side water in sight (`jobs.blocks.place`) and boards it (`:board`); waits `:no-water`; stopped `:aboard`, `:need`, `:refused`, `:no-support`, `:unreachable`, `:occupied`, `:no-boat`, `:board-failed`, `:failed`, `:bad-args`. Recover: gets off, walks up, hits the boat until it breaks (up to `:max-s`), collects the item; waits `:no-boat`; stopped `:gone`, `:unreachable`, `:leave-failed`, `:not-broken`, `:not-collected` |
| `movement.boat-drive` `{:pos :range :max-strokes :max-s}` | Steers the boat the body is in to the water cell `:pos` with `paddle` strokes (turn toward it, then forward); done within `:range`, info `boat.driven`; waits `:unseen` (check, and a round when the cell turns unsensed); stopped `:bad-args`, `:not-aboard`, `:not-a-boat`, `:not-water`, `:blocked` (land in the way), `:timeout`, `:failed` |
| `movement.boat-land` `{:pos :radius :max-spots :recover}` | Brings the boat the body is in to the nearest seen shore (a stand cell with water beside it at the block's level; `:pos` names the land cell), via `boat-drive`, then `leave-vehicle` facing the land; `:recover` then takes the boat back (`boat-launch :recover`); info `boat.landed`; waits `:unseen` for a given unsensed `:pos`; stopped `:bad-args`, `:not-aboard`, `:no-shore`, `:blocked`, `:drive-failed`, `:not-landed`, `:recover-failed` |
| `movement.cross-water` `{:pos :goal :radius :min-cross :item :recover}` | Crosses water by boat: `boat-launch` (a boat not carried is fetched; skipped when already aboard), then `boat-land` with `:pos` the far shore and `:recover` (default true: the boat comes along). The far shore is `:pos` (must be a seen shore) else a seen shore within `:radius`, at least `:min-cross` away and across water from the body: nearest `:goal` if given, else the farthest; on foot after a restart it is `:done` only on the far shore (else `:not-landed`); waits `:unseen` for a given unsensed `:pos`; stopped `:bad-args`, `:no-far-shore`, `:not-water`, `:no-boat` (launch found none), `:boat-not-seen`, the launch's and landing's stops (with `:cause`). A job for callers/travel to choose; go-to never boats |
| `time.wait-for-day`, `time.wait-for-dusk` | Done once it is day / evening; waits `:day-not-come` / `:dusk-not-come` |

**Survival (reflex and need jobs)**

| job | what it does |
|---|---|
| `survival.eat` `{:item :until 18 :max-bites}` | Eats the best carried food (points, then saturation); harmful food only with `:allow-bad`; result `{:ate n}`, `:reason :eat-failed` when a bite fails |
| `survival.get-food` | One round: eat carried food; while hungry take the next way (bake, known source, drops, hunt (looks around once first), wild crops; withdraw/attack/dig children) and eat again. Fed: `{:food n}`; nothing left: stopped `:no-food` with warn `food.none` (names animals passed over: baby, tried, refused, and the mobs seen or heard in range when none qualified); inside its ask cooldown it declines |
| `survival.breathe` | One run: look up the own column, swim up at once when no block is seen over it (kelp, seagrass and upward bubble columns count as water, a downward one is never swum into or crossed by a swim or side step, nor its corner cut; a go-to leg plans it as a ride to its bottom and goes round it when it can; a swim that rose above the best height reached since the last surfacing is progress, so a deep column takes several); only a seen block there sends looks to the near columns, then (go-to air seen (never through stone) within `:air-radius`, else dig up through a natural, non-falling cap, a cell a pass; stopped `:no_air` with `:air-radius :cap`) or dig the head free, then to land: shore swim, go-to land within `:search-radius`, a placed step block (carried pillar block, ledger purpose `:breathe-step`), outward swim legs; stopped `:no_land_in_range` (field `:step`) only when all are spent, `:no_blocks` when only a step cell was left and none carried (no hold). Never trespass except as a last resort; `:ignore-zones?` lifts it |
| `survival.extinguish` | One run until no longer burning: look round (fire at the feet and lava beside are seen), pour water (scooped back after) or reach water, cover lava beside the feet, step off hazards, else hold still (`:burning-wait`); stopped `:stuck` or `:still-burning`. Never trespass except as a last resort; `:ignore-zones?` lifts it |
| `survival.respond-to-hostile` | Fights (`fight-back`) when the odds are fair, else `retreat` (only ranged mobs near, body roofed and walled on 3 sides, building blocks and a stone-digging tool carried: `block-arrow-gap` first, once per call), deciding afresh each call; one round until no danger is near; `:quiet-s` 30 goes to the retreat's hide hold (stopped when the retreat stops, or `:no_response` after 3 calls in a row that change neither body cell, health, dangers nor the fight's hits) |
| `survival.retreat` | One whole flight per round: flees away from its chasers (nearer ones weigh more), leaning to a bed or home within `:home-range` (64), after looking away and to either side where it has not seen the ground (still unseen reads as rock), until none chases (gone, beyond its follow range, out of line for `:lost-s` 4, no way), or all stay beyond `:radius` with no gain for `:no-gain-steps` steps (`:keeps-off`), with no time limit: `:no-gain-steps` 20 steps without a new best gap take the cornered options; stopped `:cannot_escape` only when every option failed in two sweeps in a row with no gain between. Cornered (rechecked each step) it fights, seals itself in, pillars, digs down (dropping only onto floors it has seen: a zigzag over a side column, an open one or a rock one dug first) or digs a side pocket (dropped when the dig shows fluid or a cave; lava is filled first, `:on-lava :stop` leaves it), then hides (a declared hold) while a hostile within its follow range could reach the refuge and `:quiet-s` 30 after the last one. Writes a `:threat` entry per mob fled (5 min; a place only when seen, else direction and band); the third from one mob warns `hostile.chased`. Helpers: `retreat-walk`, `retreat-flight`, `retreat-refuge` |
| `survival.fight-back` | Equips the best weapon and hits the nearest seen hostile within `:range`, none over `:leash` 20 from where it started; a heard-only one is never walked to or hit (the body turns toward it and the job declines). One call fights until none is left (swings paced by the weapon's gap inside the call); it yields when health fell during the call, so the parent decides again |
| `survival.night` | The night in one round, choosing again from the world each pass: sleep (seen, known or carried bed), else log out while anyone else sleeps, else roofed or buried ends, else a close roofed `:home` (`:roofed-places`, `:walk-radius` 32) over a lit route (block light `:lit-light`, seen cells only) is walked to (go-to `:place`; a failed walk digs in), else dig-in; a failed site is remembered until morning and it walks to another within 16 blocks (at most 4), then cuts a niche into a hillside (`survival.dig-niche`, `:fetch false`), else flees (`:flee-radius` 64, 0 never; go-to, at most 3 walks) to the known bed or `:roofed-places` place go-to reaches soonest (`jobs.lib.targets/nearest!`: danger and darkness costed; lit route not needed), a seen cave or overhang cell (about 16 blocks; walled in on arrival with `dig-in :enclose` when the carried blocks cover the open sides) or seen natural ground the carried tools dig (`shelter.fled`, `shelter.flee_failed`), else holds exposed and tries again after moving or 11 min (twice). Muting `:night` does not end a running night job; cancel it too. By day leaves its pit and picks up a bed it put down outside its zone; ends done `{:night how :fled n}` or stopped (`:exposed`, `:no-way-out`). Opt out: mute `:night` |
| `survival.dig-niche` | Cut a 2-deep 1x2 niche from a standing cell beside a solid face within 16 (standing cells it has seen only), plug the opening from inside; ends `{:pos :door}` or stopped (`:no-site`, `:no-tool` (stone without a pickaxe; `:fetch`, default true, gets one first), `:refused` (every face would dig or plug another's zone, claim or plan; `:ignore-zones?` lifts it), `:unreachable`, `:dig-failed`, `:no-blocks`, `:place-failed` (a mob in the door cell is waited out, `dig-in-cells/mob-wait-ticks` 200, yielding `:continue`), `:no-progress`) |
| `survival.block-arrow-gap` | Stop a ranged mob's arrows at a gap in the body's cover without moving: fills the seen open cell within `:reach` (3) that cuts most of the line from the mob's eye to the body (nearest first; needs a solid neighbour; an open door is shut), until no ranged mob within `:radius` (16) has a line of fire. Declines `:no-ranged-danger`; with no block carried it fetches one (`:fetch`, default true); ends `{:placed [cells]}` or stopped (`:no-blocks`, `:refused` (zone, claim or plan; `:ignore-zones?` lifts it), `:no-gap`, `:place-failed`, `:no-progress`) |
| `survival.sleep`, `survival.dig-in`, `survival.log-out` | Walk to a known bed and sleep; roof the body in (one call: walls, a plug or a pit; ends `{:pos :mode :roof}` or stopped with the site's reason; `:ignore-zones?`, `:enclose` (walls only, for a body under an overhang) and `:on-lava` (`:seal` fills lava a dig lays open with a carried block, `:stop` gives up) on dig-in; a place refused for a mob in the cell is retried `survival.dig-in-cells` `mob-wait-ticks` 200, yielding `:continue`, then stops naming the mob; the retreat's seal waits the same way); leave the server for a stint and wait for the sleep count. Helpers: `dig-in-cells` (the cells and placing), `dig-in-leave` (`leave!`, `climb!`) |
| `survival.recover-drops` | After death, one run is the whole trip within the despawn window (6000 game ticks of world age): holds `:respawning` then `:settling`, weighs the drops' value against the trip's danger (`jobs.lib.cost`), walks, collects. Never `:continue`; ends `:declined` with a `recover-drops.declined` event whose `:reason` is `:danger` (mob and place) or `:unreachable` |
| `survival.restore-broken` | Puts back what a job broke in another's zone, and the holes go-to's escalation dug; one run over every cell |
| `survival.unwedge` | Steps out of a full block at the feet cell, else digs it, all in one run (stopped + warn `unwedge.blocked` for bedrock or three failed digs); `:ignore-zones?` lifts the zone check |
| `maintenance.shut-doors` | Shut every door a walk left open in one run (stopped `:left` with the reasons when any stays open) |
| `combat.attack` `{:targets :radius :absent :done}` | Kills named targets (ids, usernames, mob types) in one call; ends `:cleared`, `:gave-up`, `:lost`, `:timeout` or `:absent`. `:absent :wait` makes a standing guard |
| `combat.hunt`, `animals.cull` | Kill adults of a mob kind, never the last `:keep`, and collect drops, all in one call (yields only while a child waits); `animals.cull` skips animals in another's zone (`:ignore-zones?`) |

**Gathering, items and storage**

| job | what it does |
|---|---|
| `gather.mine` `{:block :count :radius ...}` | One call is the whole attempt (stair, strip tunnel, ore, mend, walk home; `:continue` only while a child or walk waits on the world). Mines like a player: strip-tunnels and digs ore it has seen; never targets unseen ore; a block under its feet (floor below unseen) is dug from a side cell, never from above; every cell it judges (tunnel, ground to mend, torch) is read as seen, after a look round, never through walls. Stone-type block with none in sight: stairs down through soil first (`:descend-limit` 12), else ends `:no-stone-found`. Picks the cheapest suited tool, fetching a pickaxe unless `:fetch false`. Torches the tunnel every `:torch-interval` (10) steps and a dig 4+ off the line, crafting more from coal and sticks |
| `gather.get-seeds` | One call runs to the count. Carries `:count` more of a planting material from known sources |
| `forestry.fell-tree`, `collect-drops`, `plant-sapling`, `harvest-wood` | One call is the whole attempt (`:continue` only while a child waits on the world). Fell a column of a tree the body has seen (digs the hazards of `:accept`, default all three; writes a `:forestry/replant` debt; a log out of reach of the ground is felled from a pillar that cleanup takes back, `:pillar?` default true; fetches the pillar height in dirt or cobblestone unless `:fetch` false, then waits `:need`), collect drops within the radius of where it began, harvest-wood's around the felled tree's base (never chased further; a felling that brings in no item ends `:stopped :nothing-collected`), plant a sapling, and the three in turn; harvest-wood passes `:fetch` to plant-sapling, repeats on the next tree while fewer than `:count` logs are carried (`:stopped :no-tree` with `:got`, `:replant-owed` when none is left), and plants only debts within its radius and warns `harvest-wood.debts-owed` for the rest; near debts left unplanted warn `harvest-wood.replant-owed` (`:count :pos :reason`) and the result is `{:replant-owed n}` (still `:completed`: the wood is the goal); plant-sapling fetches a missing sapling from a seen chest or by breaking seen leaves (`:fetch`, default true; `:no-sapling` wait when none can be got); a debt cell not loaded is never planted |
| `forestry.maintain`, `forestry.prepare` | One call works through every step (`prepare`'s `:continue` while a dammed flow recedes). Keep and prepare the tree cells of a forest plan (maintain fells tall trees from a pillar unless `:pillar? false`, which leaves those over `:max-logs`); `prepare` keeps its rule and field reading in the helper files `prepare_rule`, `prepare_field` |
| `storage.deposit`, `withdraw`, `kit` | Each call walks (one go-to) and moves every stack or name; a stop carries `:status :stopped` and the count (`:moved`, `:short`). Put away everything except tools, armour and the body's 3-day food reserve (`jobs.lib.cost/food-reserve`; `:keep`; gives up when nothing moves); take named items (explicit `:items` ignore the reserve); take a tool and food kit (food up to the reserve). `withdraw` and `kit` record what a chest holds in `:fetch/stock` |
| `items.obtain`, `items.get-tool`, `items.fetch-limits` | One call is the whole attempt (`:continue` only while a child waits on the world); get an item (or any of several) from carried stock, seen chests that allow `:take`, or a craft chain planned from recipes over what is carried (logs to planks, sticks, a table put down, the tool; `jobs.items.recipes`; a seen table the craft cannot reach is dropped from the plan and a carried or new one is put down, else it stops `:table-unreachable`) or `:gather` (a sapling by breaking the seen leaves of its tree with `gather.get-seeds`; what the chain lacks: logs by `forestry.harvest-wood`, cobblestone (or cobbled_deepslate, blackstone: whatever the recipes accept and is seen) and coal by `gather.mine`, seen blocks only); get a tool that harvests a block; set the body's fetch limits (`:fetch/limits`) |
| `storage.make-room` | The `:inventory-nearly-full` job. One run deposits by value, swaps for worthier items, else tosses junk until `:free` slots are free, then steps away; stopped `:short` or `:nothing-to-go` when no more may go |
| `items.craft`, `smelt`, `enchant`, `wear`, `equip`, `bake`, `give` | Craft (one call: walks to a table (puts a carried or fetched one down when none is seen), crafts the full count, fetches an ingredient that runs out, all unless `:fetch false`; a short craft is stopped), smelt in a furnace, enchant, put armour on, hold an item in a hand, bake bread, give items to a player |
| `village.trade` `{:villager :buy :count}` | Buys from a villager in one call (go-to legs, offers, buys); a give-up ends stopped with the reason |
| `village.feed` `{:villager :item :count :radius :fetch}` | Tosses food to one villager (food fetched unless `:fetch false`) and counts only what that villager's uuid collected (toss `:watchS` receipt); the rest is collected back (never a drop lying before the toss); a toss cut before its receipt is settled on restart back at its spot (food eaten meanwhile is not counted; none taken counts as refused); stopped `gone`, `no-food`, `unreachable`, `not-taken`, `litter` |
| `village.roll` `{:villager :pos :item :profession :want :trade :max-price :tries :claim-s :radius :fetch}` | Re-rolls an untraded villager's (default: the nearest adult) offers until one gives `:want`: reads the offers, breaks the workstation (`blocks.dig`), places it again (`blocks.place`) and waits for the villager to claim it; the state is re-read every step, so a restart finds a missing workstation and places it first; offers do not name a book's enchantment; stopped `gone`, `baby`, `nitwit`, `locked`, `occupied`, `still-employed`, `no-claim`, `no-match`, `unreachable`, `window`, `dig-failed`, `place-failed` |
| `village.maintain` `{:plan :target :roles :radius :ignore-zones?}` | One pass over a village: repairs the plan (`build.from-plan`), breeds up to `:target` (`village.breed`), rolls a villager to each role `{:profession :pos :trade}` no adult holds (`village.roll`, `:pos` the workstation cell; none: skipped `:no-pos` and the pass ends stopped `incomplete`); import is skipped `:no-transport`; a stopped, declined (its reason is in the report) or failed child ends the pass stopped `incomplete` |
| `village.breed` `{:target :radius :wait-s :fetch}` | Grows the villagers in sight to `:target`: feeds the two nearest untried adults (village.feed child: 3 bread, else 12 carrot/potato/beetroot; fetched unless `:fetch false`), lingers `:wait-s` near them, recounts; a rise in baby villagers in sight is a birth; done at the target (already met = 0 births), else stopped `no-pair`, `no-births` (3 fruitless attempts or no untried pair), `no-food` |

**Farming, animals, apiary, building**

| job | what it does |
|---|---|
| `farm.till` (a hoe is fetched unless `:fetch false`), `plant` (a missing seed is fetched unless `:fetch false`, then waits `:need`; `tend` passes false; a box with no bare cell seen is looked around once before it waits `:nothing-to-do`), `harvest`, `fertilize` (crops, or open grass with `:grass`; bone meal is fetched unless `:fetch false`), `compost`, `tidy`, `find-spot`, `tend` | Field work; each job is one call, the whole attempt (`:continue` only while a walk, fetch or sweep waits on the world) (`find-spot` and `till` numeric args have maxima, refused at submit; `till` bad cells args wait `:bad-args`); `tend` keeps one field in order: crops stored, a 64-seed backup per seed type kept, the rest composted, crops stored down to the 3-day food reserve (60 hunger points, golden apples excluded); a harvested cell is always replanted, but new sowing and tilling of carrots and potatoes only above the reserve; plan mode, decisions and keep lists are in the helper files `tend_plan`, `tend_decide`, `tend_stock` |
| `animals.breed`, `leash`, `unleash`, `lead-to`, `herd`, `shear`, `pen-check`, `shut-gate` (shuts every open planned gate in one run; stopped `:left` when any stays open), `tend` | Animal care, each one call = the whole run; `tend` keeps one pen in order; `breed`, `shear`, `leash` fetch the food, shears or lead they lack (`:fetch`, default true); `cull`, `shear`, `breed`, `leash`, `unleash` (so `lead-to`, `herd`) skip animals in another's zone or claim (`:ignore-zones?`); `herd` refuses a gate in another's zone; `lead-to` shuts the gates its legs left open when it ends; `herd` keeps its pen and gate, trip and run state in the helper files `herd_pen`, `herd_trip`, `herd_run` |
| `apiary.guard`, `harvest`, `maintain` | Keep campfire columns, take honey, keep an apiary in order; fires and hives come from what the body has seen (harvest looks around once when it knows none); harvest shears from the side the body stands on so the comb lies in view; one call works every fire or hive (:continue only while a walk or pick-up waits), place and click intents are saved before the act |
| `build.from-plan`, `pen`, `rail-line`, `clear-box`, `path` | Build what a plan wants (missing blocks and a pickaxe for a block to dig are fetched unless `:fetch false`); build a pen's fence or a rail line; dig out a box; shovel grass or dirt cells of a box or square into path (a shovel is fetched unless `:fetch false`); each works to the end in one call (`:continue` only while a walk or fetch waits); plan, pen and rail cells are read as seen (never seen = unknown; pen looks around before its final check) |
| `blocks.dig`, `blocks.place` | One block at `:pos`, one call = the whole attempt (fetch, one go-to walk, act, collect own drops); dig holds the best suited tool; `:on-fluid :fail` ends a dig beside water at once (default `:wait`). Out of reach after two walks, or a zone/hazard that appears mid-call, declines (the check waits); a missing tool/item after the fetch declines; fluid, bedrock, a refused primitive, an occupied cell end stopped; seeds, saplings, carpets and torches (empty collision shape in minecraft-data) may go where the body stands; place takes `:click` (jobs.lib.placement) and `:clear false` (place into plants without digging) and reports the placed `:block`; both read what the body senses: a cell it has not seen reads as stone for the rules (a dig goes ahead, then looks at what it laid open), an unseen face is no support, and the head turns to an unseen target once, and tool and room are judged after that look |
| `access.pillar`, `bridge`, `stair`, `clear-path`, `tunnel`, `leave-tunnel`, `toggle`, `cleanup` | Ways through the world: pillar up (rides out up to 8 knockbacks, after a good or failed place: lands, walks back to its start point (centred within 0.2), retries), bridge a 1-wide row of `:length` blocks along `:heading` (place ahead against the block stood on, go-to onto it; ledger purpose `:bridge`; missing blocks are fetched (`:fetch` false waits `:too-few-blocks`); a shove walks back to the last cell, 3 tries, else `:off-line`; an unsensed cell ahead after a look ends `:unseen`; an already-solid row ends `:done :built 0`; hands over `{:placed :built}`), dig a stair (reads what the body senses, an unseen cell taken for stone and dug to see; exposed lava is sealed with a building block, `:on-lava :stop` stops, a failed seal, or lava flowing back in after a reseal, steps back `:lava-unsealed`; it waits a lava flow delay and looks again before stepping beside unseen cells, and stops `:floor-unseen` at a floor still unseen once the cut is open and looked at once more; stops `:undercuts-way` at a still-solid floor of an earlier stair of this body and dimension, memory `:stair-way`; tunnel plans round it), dig through a wall (an unseen cell taken for stone, looked at before its dig; a far side it cannot see is guessed, looked at once dug to, and planned on from), cut a tunnel to a buried block and leave it, set a door or lever state, take back scaffold blocks (never the last way back to firm ground from a bridge row: it walks to the ground first, 3 tries). Pillar, bridge, stair, clear-path, tunnel, leave-tunnel and toggle do their whole run in one call (`:continue` only while a child waits on the world); stair and clear-path `:note` writes each dug cell to the tidy ledger as it is dug (go-to's holes) |
| `jobs.lib.access.approach/plan` (helper, no job) | Pure: target cells + feet + `:reach` + the rules input -> `{:stand [cells]}` (every target within reach, nearest in 3D first), else `{:pillar {:base :height :stand}}` (ground base outside zones and footprints, least height, walk plus 2 per block), else `{:reason :not-loaded\|:no-base\|:zone\|:no-stand}`. Radius 3, widened to 5 |
| `explore.search`, `explore.look` | Search for a block or entity by a pattern (uses notes); a one-shot read of the nearby blocks and entities the body has seen (`look.observed`, `:scope :seen`); the search reads stand cells as seen; a column in a loaded chunk but not seen is walked toward in steps of up to 12 blocks and looked at again, an unloaded one is deferred |
| `memory.set-place`, `forget-place`, `remember` | Write named places or entries of your own kind |
| `debug.notify`, `debug.access-check`, `debug.walk-plan` | Test helpers |

**Seeing** (`jobs.lib.look`): jobs find blocks and entities through `seen-blocks` (memory of what the body saw; `:live?` drops changed cells, `:live-within-ms` only for recent ones; ask for `:names`, `:match` or `:all?`), `seen-block` (`{:unknown true}` if never seen), `seen-entities` (players, known hostiles, visible others), `seen-items` (item entities not hidden behind a wall; jobs never count a drop they cannot see) and `find-seen!` (one look around when nothing is seen), never a scan through walls (`jobs.lib.combat/hostiles` and `sensed` list hostiles as known: seen or heard). A job that chooses work from what is seen looks around first (`survey!` per run or cell, `survey-until!` for entities, `wait-unless-surveyed` in its check): plant, harvest, tend, fertilize, smelt, fell-tree, forest and apiary jobs, craft, bake, enchant, compost, get-seeds, get-food, obtain, find-spot (no water seen), tidy, cross-water, boat-launch, cleanup (unseen cells), night (before it holds exposed).

**Fetching what a job lacks** (`jobs.lib.fetch`): `blocks.dig`, `blocks.place`, `access.stair`, `access.tunnel`, `access.leave-tunnel` and `items.equip` take `:fetch`
(default true; `false` waits or stops with the reason). Go-to's escalation children, clear-path's digs, the stairs and places
of leave-tunnel, tunnel, dig-in and breathe, obtain's table place passes `:fetch false`, its gather mines `:fetch {:what #{:tool} :how #{:chest :craft}}` (no mine inside a mine's fetch); `gather.mine`'s stair passes true. `true` allows every kind (`:tool :item`) and source (`:chest :craft :gather`); a
set narrows the kinds; a map gives limits `{:what :how :depth :minutes :fail-minutes}` (built-in, the job's default, the
body's defaults from `items.fetch-limits`, then the call's arg; later wins). A fetchable wait (`:no-tool`, `:need`) then runs
`items.get-tool` or `items.obtain` as child `:fetch` (info `fetch.started`, `fetch.done`). A failed fetch (also a child that ends done while the same wait remains: `:not-solved`) writes
`:fetch/failed` (warn `fetch.failed`) and the job waits for `:fail-minutes` before trying again. Only the job given `:fetch`
fetches; its children and go-to never do. Sources are carried items, seen chests, crafting and gathering (a sapling by breaking seen leaves). A fetch child that waits on the world makes the job yield `:continue` at once (the fetch resumes next round).

Helpers shared by jobs (not jobs): `jobs.lib.watch` (`watch/watch!` between acts: when the place is dark or a hostile
was known recently, the body turns to look behind it so a creeper from behind is noticed (a heard mob gives only a direction and band); used by mine, fell-tree,
from-plan, attack, fight-back, herd), `jobs.lib.cost` (pure cost calculators),
`jobs.lib.escape/choose`, `jobs.lib.tools/equip-for!` (cheapest carried tool that harvests the block; reflex digs use
the fastest), `jobs.lib.tools/needs-tool-to-clear?` (what stair, tunnel cleanup and from-plan ask: no harvesting tool carried and nothing carried or bare hands breaks the block within `:jobs.lib.tools/quick-clear-ms` (1.5 s); mine asks `can-harvest?`), `jobs.lib.declined`, `jobs.lib.danger` (danger checks), `jobs.lib.reach` (walk and arrow searches), `jobs.lib.reach.proofs`, `jobs.lib.tidy`, `jobs.lib.tidy-rules` (the never-dug blocks and the access verdict of a cell, shared by farm.tidy and the prepare jobs), `jobs.lib.trees` (tree and replant-debt helpers), `jobs.lib.blocks` (also building-blocks, drop-item, pick), `jobs.lib.step-off` (a go-to hop to the nearest standable non-hazard cell (no campfire, no foreign zone via zone-ok) off a cell the body stands in; it first looks at floors it has not seen), `jobs.lib.hunting` (the drops table and the attack, collect and search steps of `combat.hunt` and `animals.cull`).

## Path planner

`src/engine/path/planner_tuned.cljs` is the only planner (the `Search` fields and its methods by part in `src/engine/path/planner/`): an A* over a snapshot of section state ids (walking, jumps, drops,
gap jumps, climbing, water, doors), with costs in seconds plus risk. Its inputs: `engine.path.blocks` (the block state table, typed arrays by state id), `engine.path.space` (free body positions in a cell; both load `js/offsets.mjs` from the engine root, `engine.path.offsets`; `jobs.lib.walk.world/path-world` adds both to the primitives' snapshot); the snapshot stays `js/path/snapshot.mjs`. Tests are `test/engine/planner_*_test.cljs`;
the recorded pins (`planner_{bench,options,goals,courses}_golden.cljs`, `js/path/bench.golden.mjs`) are opt-in: `tools/test-engine --golden` or `npm run test:golden`, run when planner code changes (`tools/test-engine <ns>` of a -golden namespace exits 2: it is not in the :test bundle). `planner_bench_golden.cljs` replays recorded queries against `test/planner-bench.json` (the frozen-world queries need
`PLANNER_BENCH_DIR` (skipped with a warning where there is no world); after an intended planner change run
`npm run record:planner-bench`). Benchmarks are under `bench-lang/` (compile `planner-bench`, `goto-bench`, `search-bench`).

- `jobs.lib.walk` plans and walks one round (`walk-to!`, `follow!`, `walk!`); its pieces: `jobs.lib.walk.plan` (planning), `jobs.lib.walk.search` (budgeted search a round), `jobs.lib.walk.watch` (look-ahead), `jobs.lib.walk.world` (pathWorld and its costs). `engine.path.executor` steers a plan
  tick by tick (a drop onto slime waits on the pad until the bounce settles, at most 5 s; leaving the pad, its 3x3 when all slime, is off the plan). `jobs.lib.near` (`walk-round!` for go-to, `go-near!` a walk to something visible as a go-to child that escalates when shut in, with `:leg-s`, `:look-round` and go-to `:one-way :closed` (default there: no drop it cannot climb back, no frontier walk): the forestry, farm, smelt, get-seeds, mine, build, apiary and animals jobs, and restore-broken; build, apiary and animals pass `:escalate false`, so they never dig through what they build, guard or fence) opens and
  re-shuts doors via `jobs.lib.pass`; go-to's `:shut-also` shuts doors in or next to another owner's zone. The farm jobs' walks (till, plant, harvest, fertilize, compost, tidy) pass `:tolls` from `jobs.lib.toll-cells/walk-tolls`: the crops and farmland the body has seen near the walk and other owners' zone cells, crossed only when the way round is much longer; the zone cells over 8000 are cut to the ones nearest the straight line (a `toll-cells.capped` warn). Zone-respecting jobs (every job with an `:ignore-zones?` arg that walks, e.g. mine, fell-tree, deposit, kit, bake, smelt, cleanup, from-plan, apiary, animals, rail) pass `:zone-tolls true` to `near/go-near!` or their go-to child. `jobs.lib.targets/nearest!` finds the soonest-reachable of many targets in one
  bounded, resumable search (used by `fell-tree` and `mine`).
- A search is bounded per round (about 100 ms) and resumable; a search that needs more rounds walks toward where it has
  got to, or waits. A start closed in the loaded world, with the goal unloaded, ends `start-enclosed` (not `goal-unloaded`); a goal proved walled in or cut off while the start's own land is walled in too (a small forward drain, no cliff) ends `start-enclosed`. An enclosed goal is found by a small backward flood before any walking (`goal-enclosed`; a late flood that ends on a region with a cliff edge says `goal-cut-off`, an island or deck across a drop, which go-to treats alike; `options.preFlood`, default 256 cells), and go-to keeps its flood between searches toward one goal (`goalFloodMemo`).
- A partial plan ends at the node nearest the goal that the body can come back from (one-way drops, gap jumps and ways into a
  downward bubble column above its bottom are not taken). `go-near!` never leads the body off a ledge it cannot climb back.
- A search that runs out of loaded land names a frontier (a loaded edge) at most `options.frontierReach` (256) farther from
  the goal than the start.
- The walker watches the way ahead and replans when the world under the plan changed, when a mob blocks a leg (stuck, or standing 1 s in a 1-wide way ahead), when the hp planned for drops ahead is over what it may spend now (`:health`), and every 4 s
  for a partial plan. A fall that costs over 1 hp more than planned is a `damage-mismatch` warn.
- Risks are priced rather than banned: gap jumps over pits, corner slides over lava or fire, time near known dangers
  (`options.dangers`: the walks of `jobs.lib.near` pass the sensed real dangers and remembered `:threat` spots of
  `jobs.lib.threats`, dear when the hostile reflex would flee the mob, cheap when it would fight; `go-near!`
  `:dangers false` for a walk up to the mob fought). Darkness is priced too (`options.dark`: a dark cell costs twice its seconds more; `jobs.lib.look/dark-fn`: seen and dark, or unseen at night). The caller's own cells are priced by `options.tolls` (a Map of cell key to factor; the planner knows no zones or crops). Certain damage (falls over 3 blocks, hurting plants) sums per path: `options.damageBudget` (hp) refuses a move over it (`damageRefused`), `damageWeight` prices an hp, `fallFactor`/`landing` shape a fall; `cost.damage` reports it. Farmland is never fallen onto: no drop or gap jump lands on it, and no diagonal
  passes a pit floored with it. The planner
  takes `options.limits` for what the walker can do.

## Agent command-line tools

All take `<body> --world <world>`, print EDN, read through the local sockets, and never start a body or take a lease unless
noted. Most run a prebuilt bundle: build it once with `cd dashboard && npm run build-agent-tools`. The loader warns on stderr when a source is newer than the bundle: rebuild with `tools/compile dashboard agent-tools`.

```
node engine/tools/observe.mjs Bob --world claude                       # compact status (--raw: full snapshot, --verbose)
node engine/tools/observe.mjs Bob --world claude --wait [--timeout 30s] [--watch j17]
node engine/tools/observe.mjs Bob --world claude job j17 | result j17
node engine/tools/observe.mjs Bob --world claude catalog jobs jobs.farm. --limit 10 | catalog job <name> | catalog trigger <name>
node engine/tools/observe.mjs Bob --world claude inventory [--slots] | equipment
node engine/tools/entities.mjs Bob --world claude [--type zombie --radius 32 --player Alex --limit 20]
node engine/tools/jobs.mjs Bob --world claude list | show j17 | cancel j17 | cancel-all | retry j17 | resolve <attention> --reason handled
node engine/tools/jobs.mjs Bob --world claude submit '(jobs.movement.go-to {:pos {:x 10 :y 64 :z 20}})' [--next|--now|--hold] [--wait --timeout 5m]
node engine/tools/triggers.mjs Bob --world claude list | show hungry | upgrade [ids] | decline [ids] | add hungry --trigger hungry | mute hungry --for 10m | unmute | move X --before Y | reset X --property position | remove X
node engine/tools/triggers.mjs Bob --world claude put near-home --when '(< (inventory "bread") 8)' --job '(jobs.movement.look-around)' --persistence cooldown --cooldown-s 30
node engine/tools/say.mjs Bob --world claude [--to Steve] "message"       # chat, shares the engine's rate limits
node engine/tools/snapshot.mjs Bob --world claude [--yaw 90 --pitch -20 --look-at X,Y,Z]   # PNG of its view: draws its seen entities, prints :heard and :light
node engine/tools/time.mjs --world claude clock | dawn --timeout 1200   # world time from a fresh observer pose
```

- `observe --wait` blocks until addressed chat, new attention, a watched job or action finishing, an engine restart or the
  timeout (default 60 s). Each named observer (`--observer`) keeps its cursor in `worlds/<world>/observers/<body>/`; delivery
  is at least once. `--chatter none|addressed|all`, `--danger` and `--disconnect` choose what wakes it. Only the first `reconnect-failed` of an
  outage wakes it (later tries are summarised). Status of an offline body adds `:back-in-s`. A `died` wake or summary item carries `:cancelled-jobs [{:id :name}]` (jobs the death cancelled, seen in the same call).
- `jobs.mjs show jID` of a job the scheduler no longer holds answers from the event history, as `observe job` does.
- `jobs.mjs submit` appends to the list; `--next` lists after the current job without cutting; `--now` cuts the current
  listed job and runs the new one as a holder (a running reflex is not cut); `--hold` keeps the body while the check
  declines. `--wait` blocks until the job ends (or a wake) and prints the result and a bounded summary. Mutations carry a
  request id (`--request-id` to retry safely); the engine keeps the latest 128 ids and a duplicate returns the original
  result. A manual lease rejects `--now` with `:manual-control`.
- `triggers.mjs` uses `GET/POST /triggers`; built-ins cannot be replaced or removed, only muted or moved. `put`/`add`
  replace a custom entry and reset its condition, latch, cooldown and backoff state.
- `entities.mjs` reads the body's perception cache (`:sense` is `:seen`, `:heard` or `:remembered`; a `:heard` row has no `:pos`, only `:direction` (8 compass points)
  and `:band` (`:near` within 8, else `:far`); rows older than two minutes expire). Engine reflexes do not read it.
- `drive.mjs` and `world.mjs` are the manual takeover tools above.

### Plan, blueprint and map tools

`plans.mjs` (one world's plans), `blueprints.mjs` (global blueprints), `map.mjs` (markers, zones, claims) and
`world-changes.mjs` (a change feed) work directly on world files and need no body or dashboard. They require `--world`,
return EDN, cap pages at 10 records (100 with `--limit`, `--offset` pages on), and take `--raw`, `--dry-run`.

```
node engine/tools/plans.mjs --world claude list | find home | show home --raw
node engine/tools/plans.mjs --world claude validate home --edn '{:id "home" :parts []}'
node engine/tools/plans.mjs --world claude check home --body Wren --inventory '{:stone 24}'     # material needs and conflicts vs what that body has seen (its seen memory, in the plan's `:dim`, default overworld; `add` by a body stamps its current dimension); never-seen cells count unknown
node engine/tools/plans.mjs --world claude add|edit home --edn '...' --by builder [--revision <digest>]
node engine/tools/blueprints.mjs --world claude find hut | show starter-hut | validate hut --edn '...' | save hut --edn '...' --by builder [--revision <digest>] | remove hut --by builder --revision <digest>
node engine/tools/map.mjs --world claude find --type marker --text farm
node engine/tools/map.mjs --world claude add zone garden --by Alice --data '{:min [10 63 20] :max [20 70 30] :allow #{:harvest}}'
node engine/tools/map.mjs --world claude add claim extension --by Alice --for 30m --data '{:min [21 63 20] :max [30 70 30]}'
node engine/tools/map.mjs --world claude renew|release|remove claim extension --by Alice --if-revision REV
node engine/tools/world-changes.mjs --world claude --wait --type claim --observer planner --timeout 30s
```

- Mutations are atomic compare-and-swap writes under a shared lock. `add` creates only; `edit` and `remove` need the
  digest/revision shown by a read, so stale intent is refused. Mutations require `--by`.
- Plans have no status: every submitted plan is active and takes part in conflicts and footprints; `remove` deletes it.
- Claims are authored social intentions (default 30 minutes, 100 ms to 7 days). An active claim overlapping another owner's
  is refused with the conflicting ids. Only the owner may renew, release, edit or remove one. Claims do not authorize
  anything; jobs consult them as above.
- Markers keep the `places.json` format; zones use the strict `zones.edn` schema.
- `world-changes` keeps a named cursor outside engine state and returns grouped changes (latest 2048; `:cursor-gap` after loss).
