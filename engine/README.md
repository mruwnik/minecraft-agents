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
- `js/view.mjs` the view dump (chunk columns, pose, hud files for an external renderer; `BODY_VIEW=0` disables), format in `docs/view-format.md`.
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
export async function createPrimitives ({ host, port, username, auth }) // resolves once spawned and the column under the body is loaded (waits up to 10 s)
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
| `self()` | none | `{username, pos, health, food, foodSaturation, oxygen, onFire, inWater, inLava, onGround, chunkLoaded, settling, isSleeping, effects, experience, dimension, timeOfDay, isDay, held, inventory}`; see below |
| `entities(opts)` | `{radius = 16, kind?, names?, max = 32}`; `kind` is one of `hostile`, `passive`, `player`, `item`, `other` | `[{id, name, kind, pos, distance, visible?, item?, username?, sleeping?, creeper?}]` sorted by distance; see below |
| `blocks(opts)` | `{radius = 16, names?, match?, max = 64, properties = false}`; `names` is an array of block names, `match` a JS predicate on the block name; with neither, every non-air block | `[{name, pos, age?, properties?, distance}]` sorted by distance |
| `blockAt(pos)` | `{x, y, z}` | `{name, pos, age?, properties?}`, or `null` when the chunk is not loaded |

`self()` fields:

- `health` 0..20 and `food` 0..20; `foodSaturation` is the hidden saturation (0..20).
- `oxygen` 0..20 bubbles (`bot.oxygenLevel`; 20 until the server reports air).
- `chunkLoaded`: false when the column under the body is not loaded (`bot.blockAt` of the body's position is null); mineflayer's physics then emits no tick and the body hangs frozen.
- `settling`: true while the body is connected but its senses are not trustworthy yet (`isSettling()`): see Settling below.
- `effects`: the active status effects, `[{name, amplifier, duration}]` (`[]` when none), from `bot.entity.effects`
  with the name looked up in `bot.registry.effects` and normalised to snake_case without namespace (the registry
  says `FireResistance`; `self()` says `fire_resistance`). The burning trigger ignores a body with `fire_resistance`.
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
- `raining` and `thundering`: the world's weather as the vanilla client judges it (rain level above 0.2, thunder level above 0.9 while raining; mineflayer's `isRaining` is not used, it misses changes on 26.1), not whether rain falls on this body (a dry biome or a roof does not change it).

`entities()` extras: `item` is `{name, count}` for dropped items. Players also carry `username` and `sleeping`
(the pose metadata field equals 2, found through the registry's `metadataKeys`, else index 6; the server sends it only
when it changes). Hostile classification is `entity.type === 'hostile'` (the registry's type) or a `kind` naming
hostile mobs; creepers additionally carry `creeper: true` (their `name` is `creeper` too). Mobs carry `uuid`, `baby`
(registry metadataKeys index `baby`, 16 on 26.1; absent counts as adult, the server only sends true) and sheep `sheared`
(`wool` byte bit 0x10, index 18), verified against `data get entity` (Age, Sheared, UUID). Villager profession is not
exposed: villager_data carries a numeric profession id, which needs a hand table.

`blocks()` and `blockAt()` carry `age` (a number) when the block has an `age` state: the crop growth stage of wheat,
carrots and potatoes (ripe at 7), beetroots (ripe at 3) and sweet berry bushes (berries from 2). Other blocks have no `age`.
`blockAt()` (and `blocks()` with `properties: true`) also carry `properties`, every block state (`level`, `honey_level`, `lit`, `open`, `facing`, `half`, `age`, `moisture`, ...), left out when the block has none. mineflayer's `getProperties()` gives integer states as strings on 26.1 (`{level: "8"}`, verified live); they are turned into numbers, booleans and enum names pass as they are. `blocks()` leaves them out by default: live, a 64-block scan took 3.75 ms with them against 2.60 ms without.

Line of sight: every hostile entity carries `visible`, true when a block raycast from the body's eye to the middle of the
entity crosses no sight-blocking block. A block blocks sight when its bounding box is a full cube, except glass, water,
fire, grass, snow, vines, ladders, torches and lava; an unloaded cell never blocks, so a gap in the map cannot hide a
threat. The walk is bounded by the segment, so its length is at most the scan radius, and it is computed for hostiles
only (other kinds have no `visible` field). `entities()` and `blocks()` themselves still list things behind walls with
their distances; only `visible` tells them apart. The fake computes `visible` the same way over its cells (a spec
entity may force it with `visible: true|false`).

The `:hostile-near` trigger uses it: it holds only for a visible hostile within `:radius` (trigger arg `:visible-only`,
default true; false restores the old behaviour). The trigger is wrong, not the job, when a hostile behind a wall fires
the response. `engine.jobs.combat/hostiles` takes an optional third argument `{:sight :only|:prefer}`: `:only` keeps the
visible ones, `:prefer` lists them first (each group nearest first); two arguments ignore sight as before.

Remembered places (a known bed, a known chest) are not primitives. They are
`:bed` and `:chest` entries in body memory; see `engine.memory/place` below.

### Acting (async, token first)

| method | args | statuses | bound | on cut |
|---|---|---|---|---|
| `moveTo(token, a)` | `{pos, range = 1, timeoutS = 20, maxDistance = 64}` | `arrived` (the goal is satisfied where the body stands, not merely a resolved walk), `partial` (bound or `maxDistance` reached, closer than before), `blocked` (no path or no progress); a non-arrived result may carry `reason`: `'noPath'` (the planner found no way), `'planTimeout'` (planning took too long), `'stalled'` (the body got less than 1 block from where it was for 8 s, so a walk stuck flush against a ledge ends in seconds instead of sitting out `timeoutS`), `'timeout'` (`timeoutS` ran out); a target farther than `maxDistance` is walked in hops, each ending within 4 blocks (XZ) of the point `maxDistance` along the line, on a cell open to the sky (sky light >= 12), so a far walk stays on the surface rather than following caves | `timeoutS`, at most 60 | goal cleared, controls released |
| `dig(token, a)` | `{pos}` | `dug`, `missing` (air), `unreachable` (more than 4.5 away), `cannot` (unbreakable) | 10 s | `stopDigging` |
| `place(token, a)` | `{pos, item}` | `placed`, `occupied`, `no-item`, `no-support`, `unreachable` | 5 s | nothing placed after the cut |
| `jumpPlace(token, a)` | `{item, count = 1}`, count at most 8 | `done`, `partial` (some placed), `failed` (none); `placed` (blocks placed) and `reason`: `no-item`, `no-support`, `no-headroom`, `not-raised`, `place-failed: ...`, `timeout` | 2 s per block | jump released |
| `collect(token, a)` | `{id, timeoutS = 10}` | `collected`, `gone`, `unreachable`, `timeout` | `timeoutS`, at most 20 | as `moveTo` |
| `inspectContainer(token, a)` | `{pos}` | `ok`, `missing`, `unreachable` | 5 s | window closed |
| `transfer(token, a)` | `{pos, direction, item, count}`; `direction` is `deposit` or `withdraw` | `ok` (`moved` is measured from a reopened container and may be less than `count`, even 0), `missing`, `unreachable`, `no-item`, `full` | 5 s | window closed after its slot updates settle |
| `equip(token, a)` | `{item, dest = 'hand'}`; `dest` is `hand`, `off-hand`, `head`, `torso`, `legs`, `feet` | `equipped`, `no-item` | 2 s | none needed |
| `toss(token, a)` | `{item, count?, slot?}`; with `slot` it throws exactly that slot's whole stack (`count` ignored; `no-item` when the slot is empty or holds another item), without it that many of the item (all carried stacks together; default and maximum: everything carried) in the direction the body looks, it does not look anywhere itself | `tossed` (`count` thrown), `no-item` (`count: 0`) | 2 s | none needed (nothing to undo) |
| `craft(token, a)` | `{item, count = 1, table?}`; `count` is items wanted, rounded up to whole batches (`made` may exceed it: 1 log, `count` 1 `oak_planks` makes 4); one recipe per call, never chains (planks then sticks is the caller's business), never walks; `table` is a `{x,y,z}` crafting table, else any within 4.5 blocks is used; the result stacks are merged afterwards | `crafted` (`made`, `used`), `partial`, `no-item` (`short`, `alternatives`), `out-of-reach` (`reason`: `too-far`, `table`: `{x,y,z}` of the nearest known table, within 32), `unreachable` (`reason`: `no-table` (none within 32), `not-a-table`), `full`, `cannot` (`reason`: `unknown-item`, `no-recipe`), `failed`, `timeout` | `min(60, 4 + 6 * count)` s | closes an open window |
| `chat(token, a)` | `{message, to?}`; control characters (newlines too) become spaces and `§` is dropped; `to` must be a player name (3-16 letters, digits, `_`), else bad-args, as is a message empty after stripping; one line only (over 256 characters, or 256 less `/tell <to> ` for a whisper, is bad-args; `engine.chat/say!` splits); with `to` it whispers. No limiter here: the engine's `act!` enforces at least 1 s between lines and at most 5 lines in any 30 s (per body, across jobs) | `sent` (`parts`), `gone` (`to` is not online), `failed` (`reason` = the server's refusal line), `cannot` (`command`: the message starts with `/`, nothing sent; `too-long` from `engine.chat`: more than 5 lines), `blocked` (`rate`, `retryMs`: given by `act!`, not the primitive: the line does not fit the 30 s window now, nothing sent) | 3 s | none needed |
| `eat(token, a)` | `{item?}`; without `item`, the best food carried | `ate`, `no-food`, `full` | 5 s | `deactivateItem` |
| `attack(token, a)` | `{id}` | `hit`, `killed`, `gone`, `out-of-reach` | 1 s (one swing) | none needed |
| `interact(token, a)` | `{id, item?}`; uses the item (empty hand when omitted or null) on an entity | `used`, `no-effect`, `gone`, `out-of-reach`, `no-item`, `full` (empty hand asked, no free slot: nothing done), `cannot` (`reason`: `mounts`, `opens-window`; refused kind), `failed` (`reason`: `mounted`, `opened-window`; the guard dismounted / closed the window) | 2 s | closes the window, dismounts |
| `unequip(token, a)` | `{}`; empties the main hand (selects an empty hotbar slot, else moves the stack into a free slot) | `ok` (`item`), `empty` (nothing held), `full` (no free slot: nothing done, never tossed; mineflayer's unequip would toss) | 2 s | none |
| `sleep(token, a)` | `{pos}` (a bed) | `sleeping`, `not-night`, `occupied`, `monsters-near`, `missing`, `unreachable` | 5 s | wake if asleep |
| `look(token, a)` | `{pos}` or `{yaw, pitch}`; resolves after the next physics tick (the rotation has been sent to the server), at most ~50 ms later, bounded by 100 ms; | `ok` | 1 s | none needed |
| `wait(token, a)` | `{ms}`, clamped to 0..10000 | `ok` | `ms` (scaled by `timeScale`) | none needed; a cut rejects at once |
| `swim(token, a)` | `{ms = 3000, toward?}`, at most 10000; `toward` is `{x, y, z}` | `surfaced` (head out of water), `landed` (with `toward`), `timeout` | `ms`, at most 10 s | jump and forward released |
| `useOn(token, a)` | `{pos, item?, face = 'up'}`; `face` is `up`, `down`, `north`, `south`, `east`, `west`; without `item` it uses an empty hand (an empty hotbar slot, else the held stack moved to a free slot; never tossed) | `used` (the block's name or properties, or the carried count of `item`, changed within 1 s), `unchanged`, `missing` (air or not loaded), `no-item`, `no-room` (empty hand asked, inventory full), `unreachable` (more than 4.5 from the eye; `reason: 'too-far'`, `distance`), `cannot` (`reason`: `bed` for beds and respawn anchors, `container` for blocks that open a window, `hazard` for flint_and_steel, fire_charge and lava_bucket, `use-place` for a block item on anything but a composter, `window` when a window opened anyway: it is closed) | 5 s | none needed |
| `offline(token, a)` | `{ms = 300000}`, at most 600000 | `ok` (`ms` is the wait used), `cut`, `closed`, `unsupported` | `ms` plus the reconnect | see below |

Acting while asleep first leaves the bed (`leave_bed` sent by name, since mineflayer's `wake()` sends a wrong id on this protocol), bounded at 1 s (scaled by `timeScale`); a cut during `sleep` leaves the bed too.

Extra fields on the result:

- `moveTo`: `pos` (where the body ended), `distance` (to the target), `reason` (one of `'noPath'`, `'planTimeout'`, `'stalled'`, `'timeout'`, on `partial` or `blocked`).
- `swim`: `oxygen` (`{before, after}`, the air level when the call started and ended).
- `dig`: `block` (name dug), `drops` (`[{id, name, count, pos}]`, the item
  entities that appeared within 2 blocks during up to 1 s after the break).
- `place`: `block` (name placed). A cell holding fire, soul fire, grass (`short_grass`, `tall_grass`, `grass`) or a snow layer counts as free,
  because the game replaces those; any other block is `occupied` (water and lava are replaced for block items as before). Buckets (`bucket`, `water_bucket`, `lava_bucket`) are used, not placed: `pos` is
  the cell that receives the liquid (it must be air or a replaceable block, else `occupied`, and have a solid
  neighbour, else `no-support`),
  or for an empty `bucket` the liquid cell to scoop (`missing` when it holds none). The body equips the bucket, looks
  at the supporting block (or the liquid), calls `activateItem`, and waits up to 2 s for the cell to change or the inventory to show the filled (scoop) or emptied (pour) bucket, whichever comes first (the block update can arrive late):
  `placed` (`block` is `water`, `lava` or `bucket`), else `{status: 'failed', reason: 'unchanged'}`.
- `collect`: `gained` (`[{name, count}]`).
- `inspectContainer`: `items` (`[{name, count, slot}]`).
- `transfer`: `moved` (count).
- `toss`: `count` (thrown; 0 on `no-item`).
- `craft`: `made`, `used` (`{name: n}` consumed), `short` (`{name: n}` still missing, from the recipe closest to done; ties go to the more common base item), `alternatives` (`{name: [cousin names]}`, only when other recipes use different ingredients in its place, such as `{cobblestone: ['cobbled_deepslate', 'blackstone']}`).
- `chat`: `parts` (lines sent); `blocked` also has `retryMs`.
- `useOn`: `before` and `after` (`{name, properties}` of the cell), `consumed` (how many of `item` left the inventory; 0 for an empty hand). Not on `missing`. A full composter ejects its bone meal as an item entity; the caller collects it.
- `interact`: `consumed` (item count used up; may lag the love event, so it can read 0 on a feed that took), `worn` (durability used on the held item), `love` (entity_status 18 on that id, which mineflayer does not map), `leash` (`attached`/`detached`/null, from the raw attach_entity packet; 26.1 sends holder 0 on unleash), `changed` (`{field: [before, after]}` of baby/sheared). `item` omitted or null empties the hand first. Use is one use_entity interact packet (mineflayer useOn), verified accepted on 26.1: feed adult (love), feed baby (consumed, no love), feed in-love cow (no-effect), shear (sheared, worn), lead and unleash.
- `eat`: `item`, `food` (after eating).
- `attack`: `health` (target's, when known; never live, mineflayer does not track other entities' health), `hurt` (whether the server reported the target damaged after the swing).
- `offline`: `ms`, the wait actually used (clamped to 0..600000, rounded down).

Offline is body state. `offline` quits the bot, emits the body event `offline`, waits `ms` (default 5 minutes, hard
maximum 10 minutes), reconnects with the same connection params (up to 3 tries, 5 s apart), rebinds the library bot so
every other primitive and every listener works on the new bot (after it spawned and, up to 10 s, its world loaded), emits `online`, and resolves `{status: 'ok', ms}`. From
the moment it quits until the fresh bot is adopted the body is offline, and `isOffline()` is true:

- Sensing says so instead of returning stale values: `self()` returns `{status: 'offline'}`, `entities()` and `blocks()`
  return `[]`, `blockAt()` returns `null`. `isOffline()` is the one way to tell an offline body from an empty world. `isSettling()` (sync) is the third state: connected, not yet trustworthy; offline is not settling.
- The engine pauses the register and the list: `tick` evaluates no trigger and starts no round, so a reflex cannot cut
  the wait and act on the quit bot, and no trigger reads frozen sensing. The scheduler just waits for the reconnect.
- A cut (a token change, `cancel`, a reflex that somehow fired) does not end the offline state. It ends the wait early,
  the body reconnects first, and only then does the call resolve `{status: 'cut'}` and the engine resume ticking. An
  acting primitive called by the new owner meanwhile waits for the reconnect, then acts on the new bot; a stale token
  still rejects with `cut`. The body is never left offline.
- `close()` during the wait or the reconnect cancels it and resolves `{status: 'closed'}` (a bot the reconnect already
  produced is quit). The engine's shutdown does this, so a SIGTERM during a log-out quits at once instead of
  reconnecting a body that is about to leave anyway.
- If every reconnect try fails it emits `disconnected` and rejects with the last error; the body is then marked down,
  and the next acting call tries the reconnect again.

Only `createPrimitives`, which owns the connection params, supports it; `createPrimitivesFromBot` resolves
`{status: 'unsupported'}` without touching the bot. A stale token rejects with `cut` on entry and bad `ms` (not a
number, negative) with `bad-args`. The fake behaves the same: `isOffline()`, the offline sensing above, and a cut ends
its wait early with the body back (online event) before the call resolves `cut`.

The pathfinder goal never outlives a walk: `moveTo` and `collect` clear it (and the control states) on every way out, and the
body's `death` and `respawn` events clear it too, so the body does not walk back to an old goal after a respawn. A reconnected
bot starts with no goal.

The pathfinder's movements (`js/movements.mjs`) never dig or build. Powder snow, cobweb, sweet berry bush and wither rose in the body's cells, and magma, campfire and soul campfire underfoot, add a cost rather than a ban (30, 40, 20, 20 and 20, 40, 40: a detour of up to roughly that many blocks is preferred, crossing stays possible when it is the only way). Fire, soul fire and lava are avoided outright, a parkour jump never passes over lava or fire (no way round means `noPath`), and a drop into water is bound by the usual 4-block drop limit.

An unplanned disconnect (the bot's `end` or `kicked`) emits `disconnected` and marks the body down; an `error` on the
bot or its client is emitted as the body event `error` and never thrown, so it cannot crash the process. While the
body is down, the next acting call (`moveTo`, `dig`, `place`, `collect`, `inspectContainer`, `transfer`, `equip`,
`eat`, `attack`, `sleep`, `look`, `swim`) first makes the same reconnect `offline` uses (3 tries, 5 s apart, then up to 10 s for the world; calls
arriving meanwhile share it), emits `online` and runs on the new bot. If every try fails it emits `reconnect-failed`
(an error-level engine event) and resolves `{status: 'disconnected'}`; the next acting call tries again. Sensing
reads keep answering from the dead bot. Without a connection to remake (`createPrimitivesFromBot`) a down body
resolves `disconnected` at once. A stale token still rejects with `cut` first.

`jumpPlace` pillars the body up out of a pit: for each of `count` repetitions it first sneaks to the centre of its cell (within 0.1 and until it has stopped, up to 1 s; this gets the body off a wall, where the jump never happens, and a failure to centre is not fatal: a body that cannot jump ends `not-raised`), looks straight down, jumps, and once the feet clear
the cell it stood in places `item` there against the block below that cell, releases jump and waits to stand one block
higher. It checks before each jump that something solid is under the feet (`no-support`) and that the cell two above the
feet is not solid (`no-headroom`), and stops at the first failure, reporting `placed`, the blocks it did raise. `not-raised`
means the body never left the ground, or did not end a block higher. The result is `done` when all `count` were placed,
`partial` when some were, `failed` when none. Cut-aware like `swim`: jump is released on every exit. Not yet verified live
(unit tests on the stub bot and the fake only).

With `toward`, `swim` looks at the target and holds jump and forward until the feet stand on a dry cell over a full block, or (dry) are within 1.5 blocks of the target's cell centre, and resolves `landed`; it is for climbing out of water onto a rim one or two blocks up, which the pathfinder cannot path to. Both controls are released on every exit. Every bound bot has collision half-width 0.31 instead of mineflayer's 0.3: verified live on 26.1, a body whose box touches a block face exactly (flush against a rim wall or a step, or beside a block it walks past) has every move rejected and is set back about 20 times a second, so the water-exit impulse never lands (2 of 2 walks past one block stuck 25 s at 0.3; 3 of 3 took under a second at 0.31). The fake lands the body on the target when it is within 6 blocks and standable, else times out.

`swim` without `toward` holds the jump control until the block at the head is no longer water, polling every 50 ms, because the
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
(`from`, `message`) (lines from the body itself are not reported), `picked-up` (`item`, `count`: the body picked up an item entity), `woke`, `spawned`, `disconnected` (`reason`), `error` (`reason`), `reconnect-failed` (`reason`), `world-not-loaded` (`ms`, warn level: the column under the body did not load within 10 s after spawn or a reconnect; the body is used anyway), `physics-stalled` (`pos`, `ms`, warn level: no physics tick for 2 s because the column under the body is not loaded; the walk goal is cleared and controls released), `offline` (`ms`), `online` (`pos`). `died` is
emitted at the moment health reaches 0: `pos` is where the body died and `inventory` (`[{name, count, slot}]`) what
it carried. An instant death (`/kill`, void, damage) has the server clear the slots before the event is read, so when the live inventory is empty `inventory` is the last snapshot of the living body (taken on each health event above 0 and about once a second of physics ticks); `experience` is `{level, points}` at death. mineflayer emits `death` from
the health packet and only overwrites `bot.experience` on a later `experience` packet, so the values are the pre-death
ones. `respawned` is emitted at the first `spawned` after the library's respawn
signal, so `pos` is the new position (and `dimension` the new dimension; a portal also counts as a respawn). `offline`
and `online` are the two ends of the `offline` primitive; after `online` the `bot` underneath is a new one. The engine
turns each into an entry of that kind in body memory (see Memory). `picked-up` is emitted at level `debug` and its `:picked-up {:item :count}`
entries are what make-room reads as "newer" (a stack picked up recently is tossed last).

### Lifecycle

`close()` disconnects (the fake does nothing).

### The fake

`createFake(spec)` in `js/fake.mjs` returns the primitives plus a `world`
handle for tests. It has the same sensing fields as above (`spec.self` may set `oxygen`, `effects` (default `[]`), `onFire`, `inWater`,
`inLava`, `isSleeping`, `foodSaturation`, `experience`, `dimension`; player entities default to `sleeping: false` and
`username` equal to `name`). Its `offline` flips `world.state.offline` (what `isOffline()` reads), emits `offline` and `online`, and waits
`ms * spec.offlineScale` (default 0.001, so 5 minutes is 0.3 s) before resolving the same results; a cut ends the wait early. The handle:

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
  states: { '6,64,0': { level: 3 } },           // "x,y,z" -> block states, reported as `properties` (with `age`)
})
p.world.state            // the mutable world (self, time, blocks, entities, inventory, containers)
p.world.calls            // [{ name, token, args }] for every acting call, in order
p.world.hold('moveTo')   // the next moveTo call waits; returns release(result?)
p.world.override('dig', async (token, args, defaultImpl) => ({ status: 'cannot' }))
p.world.emit({ kind: 'hurt', health: 6 })   // delivered to onBodyEvent listeners
p.world.setTime(13000)
p.world.die()            // emits died (pos, inventory, experience), drops the inventory as items, zeroes experience
```

Fake semantics: `jumpPlace` raises the body one block per placement and consumes the item, with the same stop reasons as the real one (`no-item`, `no-support`, `no-headroom`); `swim` lifts the body to the top water cell of its column and refills oxygen to 20. `moveTo` jumps to the target if within `maxDistance`, else
moves `maxDistance` toward it and returns `partial`. `dig` removes the block
and adds an item entity at its cell. `collect` moves the item entity into the
inventory and emits one `picked-up` per gained item. `place` of `wheat_seeds`, `carrot`, `potato` or `beetroot_seeds` needs `farmland` below (else `failed`, nothing consumed), consumes the item and sets the crop block at age 0, resolving `{status: 'placed', block: item}`. A `drops` value may be an array of item names, one item entity each. `toss` takes the items from the inventory and adds one item entity 3 blocks along +x of the body. `craft` (`out-of-reach` with `table` when a table is within 32 blocks, else `no-table`) uses a built-in recipe table (bread, oak_planks, stick, crafting_table, torch, wooden_pickaxe, stone_pickaxe; extend with `spec.recipes` `{name: {count, needs, table?}}`) and has the real statuses; an unknown item is `no-recipe`. `chat` appends `{message, to}` to `world.state.chat` and resolves `sent` (`gone` when `to` names no player entity, nothing recorded). The failure statuses of `craft` and `chat` were chosen from the backoff failure list, so backoff needs no change. `interact` (`js/fake-interact.mjs`): breeding food on a ready adult consumes 1 and sets `inLove`; `inLove` or `cooldown` gives no-effect; a baby consumes without love; shears on an unsheared sheep give sheared plus a white_wool item, worn 1; lead and empty-hand unleash work; an entity spec may set `accepts: false`, `mounts` (failed `mounted`) or `opens` (failed `opened-window`); refused kinds are `cannot`. Spec fields `raining`/`thundering` set the weather, `world.setRaining(on, thunder)` changes it. `unequip` (`js/fake-unequip.mjs`): `empty` when nothing is held, `full` at 36 stacks, else clears the hand and resolves `ok` with `item`. `attack` takes 5 health per swing and reports `hurt: true`; an entity with `invulnerable: true` takes none (`hit`, health unchanged, `hurt: false`). `useOn` (`js/fake-use-on.mjs`): a hoe tills dirt, grass_block or dirt_path into farmland (not from below, air above); bone meal adds 2 to a crop's age up to ripe (consumed; ripe is `unchanged`) and is consumed on a sapling or grass_block; a compostable item raises a composter's `level` by 1 every time (7 jumps to 8), and at 8 any hand empties it to 0 and drops a `bone_meal` item entity above it; anything else is `unchanged`. `sleep` succeeds at night on a
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
optionally `doc`, `args` and `backoff`. Directories nest freely; the namespace follows
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
:args :backoff}}`, built at compile time:

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
- **`backoff`** (optional) is the job's own backoff config, `{:after :first-s
  :max-s}` or `false`; see Backoff. It applies to a job whose spec is that
  leaf, not to a combinator around it.
  A reflex job that ends `:backoff` is dropped with its memory (the next
  firing starts fresh); a listed job keeps its memory. So a job with its own
  give-up must conclude within `:after` (default 3) fruitless rounds in a row,
  or set its own `backoff` (a larger `:after`, or `false`). The jobs with
  `backoff` false are `jobs.survival.sleep`, `jobs.survival.shelter` and
  `jobs.maintenance.unstick`, each with its reason in its def's docstring; a
  test pins that set.

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

**engine.chat.** Limits as data, enforced in `act!` for every `:chat` act (`gate!`); `say!` splits and spaces lines.

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
  `:moved`, `:picked-up`, `:forestry/replant`, `:job/j7`. The survival jobs write `:breathe`,
  `:extinguish`, `:hazard`, `:hostile`, `:fed`, `:hungry`, `:slept`,
  `:shelter`, `:log-out`, `:stuck`, `:recovered` and `:chest-unusable` (make-room: a chest that failed it), and read `:home` and
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
:persistence :cooldown-s :backoff :builtin?}`; the id defaults to the trigger name and
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
- Settling: right after a login, a reconnect, a respawn or a teleport (a forced
  move over 16 blocks) the body is connected but its senses are not trustworthy
  (entities arrive after the chunks; there is no signal for "all sent"). It ends
  `settleMs` (default 1 s, scaled by `timeScale`) after the column under the
  body is loaded; an unloaded column always counts as settling. While settling
  `tick!` evaluates no trigger, so nothing fires and no `:stop` latch clears
  (rounds already running are untouched). A reflex job that ends meanwhile is
  not judged: its end is kept and judged on the first ready tick, with the
  cooldown counted from the job end; the `reflex.ended` event then carries
  `:deferred-ms`. Without this a log-out, which ends with a reconnect before
  the sleeper is sensed, looked cleared and fired again at once.

  The survival scenario sets, explicitly, `:persistence :cooldown` and:

  | trigger | cooldown (s) | why |
  |---|---|---|
  | suffocating | 2 | back on the body almost at once |
  | burning | 2 | same |
  | health-low | 10 | |
  | hostile-near | 5 | |
  | hungry | 90 | a body with no food does not retry every tick |
  | night-unsafe | 10 | keep trying through the night |
  | player-sleeping-nearby | 30 | the 30 s cooldown spaces log-outs (a log-out ends at the reconnect, while the body is settling, so it is judged on the first ready tick with the cooldown counted from the job end) |
  | stuck | 60 | must outlast the 60 s window the newest move is measured in |
  | died | 30 | |
  | inventory-nearly-full | 120 | a body with nothing it may toss does not retry every tick |
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
`submit!` opts: `:hold?`, `:backoff` (a config map or `false`, over any
`(backoff cfg e)` wrapper), `:front?`, `:by`.

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

**Backoff.** A job whose rounds keep failing at once would spin: every
tick a round, each act answering `blocked`. The engine backs such a job off;
it never removes it, and it alerts.

- *Act results.* `act!` classifies every act result of a round (the acts of
  children count for the top-level job). Failures are the statuses `blocked
  failed unreachable cannot timeout gone out-of-reach no-item no-support
  no-headroom occupied full disconnected unsupported not-night monsters-near
  no-effect unchanged no-room missing`;
  any other status (`arrived partial dug placed ok hit ...`) is progress. An
  act that throws counts as neither. *Neutral acts* neither count nor
  reset: `look`, `wait` and `equip` (a job that looks and then gets a blocked
  `moveTo` each round still backs off), and a `moveTo` with a failure status
  (`timeout`, `blocked`...) that moved the body at least 1 block (straight
  line, from where the act started to where it ended). A round of only
  neutral acts is not fruitless and does not reset.
- *Fruitless round.* It ran at least one act and every act failed. A round
  with no act, a cut round and a round that threw neither count nor reset,
  and neither does a round that ends `:declined` (the job saying "not now"
  on purpose), listed job or reflex.
  The first progress act resets the count and the delay at once, mid-round.
- *Schedule.* `{:after 3 :first-s 1 :max-s 30}`: after `:after` fruitless
  rounds in a row the job gets no round for `:first-s` seconds; each further
  fruitless round (after the wait) doubles the delay, up to `:max-s`.
- *Listed jobs* are counted per instance id, so a `(repeat ...)` keeps its
  count across child restarts. A job in backoff is passed over in
  `choose-listed` exactly as if its check declined (a holder in backoff
  leaves the body idle) and stays listed.
- *Reflexes* are counted per reflex id across firings. On reaching backoff a
  job whose round continued ends with `reflex.ended` outcome `backoff`; one
  whose round ended it (`done`, failed, cut) keeps that outcome.
  Either way the reflex cannot fire until the delay ends.
- *State* is the engine's `:backoffs` atom `{key {:fruitless :last :delay-ms
  :until :since :alerted}}` (key: instance id, or the reflex id keyword). It
  is not persisted: a restart starts with none, and every count and delay is
  cleared on the first tick after the engine leaves a pause (offline,
  settling or manual control; `reset-backoff!`), since what failed before
  says nothing about the body afterwards.
- *Config*, most specific wins: engine default (`create` opt `:backoff`, else
  the defaults above) < the job namespace's `backoff` var (leaf specs) <
  the register entry's `:backoff` or the listed job's (`submit!` opt
  `:backoff`, or a top-level `(backoff cfg e)` wrapper, which nests with
  `hold` in either order). A map is merged over the level below; `false`
  turns backoff off.
- *Events.* warn `job.backoff` (`reflex.backoff` for a reflex) when it starts:
  `act`, `status`, `reason` of the last failing act, `delay-ms`, `fruitless`,
  `passes` (scheduler passes that skipped it), `since`, `text`; again at most
  every `:backoff-alert-ms` (default 300000) while it lasts. Info
  `job.recovered` (`reflex.recovered`) when a progress act ends it.

## Events

The engine writes one EDN map per line to stdout and
`state/agents/<name>/engine/events.edn`. The canonical contract is in
[`docs/event-stream.md`](../docs/event-stream.md). Event HTTP responses and
requests also use `application/edn`; there is no JSON serialization boundary
for engine events. Existing JSONL logs remain historical files and are not
appended to or mixed with the EDN stream.

| field | meaning |
|---|---|
| `:seq` | increasing stream sequence, retained across restarts and `--fresh` |
| `:generation-id` | engine-state identity; retained on restore, replaced by `--fresh` |
| `:time-ms` | wall-clock milliseconds since epoch |
| `:source`, `:kind` | event producer area and event kind |
| `:context` | applicable job ID, child chain, round, reflex ID, action-call ID and causal sequence |
| `:data` | structured event-specific fields, including position when available |
| `:message` | optional display text, not a machine-readable reason |
| `:attention` | omitted/`:none`, `:notice`, or `:required` |
| `:request-id` | stable identity for a required request and its resolution |

Body identity belongs to the subscription, not every event. There is no
schema-version or severity field. Existing internal emitters are normalized
at the appender boundary; consumers use structured kinds, outcomes and attention.

Sources cover job and reflex lifecycle, primitive action starts/outcomes,
body events, system lifecycle/manual takeover, memory writes and save statistics.
Primitive starts and outcomes share an action-call ID. Routine events do not
notify an agent. Notices can be batched. Required requests remain in saved
engine state until explicitly resolved, independently of retained history;
reading one does not resolve it or pause the whole engine.

The appender retains **64 MiB total** by default across active and rotated
segments (`events.edn.1` through `.3`). `events.edn.meta.edn` preserves stream
identity and reserved sequence numbers. A cursor is `{:stream-id ... :seq ...}`.
Rotation can make a cursor too old, and a crash can leave reserved sequence
gaps; readers receive an explicit gap and reconcile current state. Incomplete
trailing records are repaired on startup; malformed complete records are reported.
The cap is a byte budget, not a guaranteed duration of history.

Configure `engine.events.maxBytes` in the existing agent runtime configuration,
or override it for one launch with `--events-max-bytes <bytes>`; this is not a
job-scenario setting. The minimum accepted cap is 1024 bytes. Records larger
than the cap are rejected explicitly, never silently truncated. An outstanding
required request remains in saved state even if its notification cannot be logged.

### Local event API

The engine exposes HTTP over `state/agents/<name>/engine/events.sock`, a local
Unix socket with mode 0600. It is separate from manual driving's `control.sock`.
All responses are EDN, including errors; mutation bodies must be EDN too.

| request | behavior |
|---|---|
| `GET /snapshot` | coherent engine state, outstanding requests, body metadata and cursor |
| `GET /events?stream-id=<id>&after=<seq>&limit=<n>` | bounded event page after a cursor, with oldest/latest sequence and explicit gap indication |
| `POST /attention/resolve` | `{:request-id "..." :reason :handled}` resolves a request idempotently; it does not retry or cancel its job |
| `GET /status?limit=<n>` | compact body/job/attention projection; `limit` is 1..32 (default 4) |
| `GET /job?id=<id>&limit=<n>` | one listed or reflex job's bounded parsed spec, effective args, state and linked outstanding requests |
| `GET /catalog?kind=jobs&prefix=jobs.farm.&limit=20&offset=0` | bounded page of exact job names (names only) |
| `GET /catalog?kind=triggers&prefix=health&limit=20&offset=0` | bounded page of exact trigger names (names only) |
| `GET /catalog?kind=job&name=jobs.<namespace>.<name>` | one job's description and argument defaults |
| `GET /catalog?kind=trigger&name=<name>` | one trigger's default job, args, persistence and cooldown |

For example, read a snapshot without taking control of the body:

```sh
curl --unix-socket state/agents/Bob/engine/events.sock http://localhost/snapshot
```

On connection/reconnection or a history gap, reconcile outstanding requests
from `/snapshot`, then read events after that snapshot's cursor. This is an
observational stream, not an event-sourced database. State and memory snapshots
remain authoritative. The ClojureScript dashboard uses this API and offers a
read-only historical fallback for offline engines and old logs.

For a compact terminal/agent read, `node engine/tools/observe.mjs <agent>`
prints the status projection as EDN. It reads the same private event socket and
does not contact or disturb Mineflayer. Use `job <id>` or `catalog job|trigger
<name>` only when the summary needs detail; `--state <dir>` selects another
state root and `--limit <n>` bounds the listed queue rows.

```sh
node engine/tools/observe.mjs Bob
node engine/tools/observe.mjs Bob --raw
node engine/tools/observe.mjs Bob job j17
node engine/tools/observe.mjs Bob catalog jobs jobs.farm. --limit 10
node engine/tools/observe.mjs Bob catalog job jobs.forestry.harvest-wood
node engine/tools/observe.mjs Bob catalog trigger hostile-near
```

The default status contains body identity, generation and event cursor,
scheduled/manual/offline/settling mode, position, health/food, current job,
up to four queue rows, failed IDs and up to four outstanding required
requests. Counts and `:more?` flags show when rows were bounded. `--raw` opts in
to the full EDN snapshot, including complete scheduler state. The default omits
raw memory and event history; inspect one job or request details through the
existing snapshot/event routes when needed. Job detail bounds the parsed
spec/args projection and linked requests. Catalog pages return names only with
an offset for continuation; exact-name detail is separate, so discovery never
sends the whole registry's documentation. Every CLI read has a 3-second total
deadline, caps responses at 256 KiB, and returns a structured EDN error for
unavailable sockets, timeouts or oversized responses instead of waiting
indefinitely.

If `observe.mjs` returns `:observe-unavailable`, that body is running an
older engine that has `/snapshot` but not the compact `/status`, `/job` and
`/catalog` routes. Its error includes a raw-snapshot fallback and the selected
state directory; pass the same `--state` value with `--raw` to read it. To
enable the projection, stop that body normally and start it from
the current engine build; from the repository's `engine/` directory:

```sh
npm run body -- --agent Bob
```

That script compiles `out/body.cjs` from the current ClojureScript source
before starting the body. Do not start a second process for an agent that is
already running.

## Manual takeover

For rescuing a stuck body by hand. Movement only: no dig, place or use yet. The body listens on a unix socket,
`state/agents/<name>/engine/control.sock` (mode 0600, HTTP + JSON), created at start and removed at shutdown. If it
cannot listen (for example a path over 100 bytes) the body emits `system.control_unavailable` (error) and runs without it.

The control socket is the body's first outside input channel, and it is deliberately minimal: only the `/drive` routes.
There are no list or register edits through it; whether to add any is a separate design decision. It relates to
`docs/design.md`'s assumption of a single input source ("There is a single input source. Which agent gets to call what is
the agents' problem, not the engine's.") like this: while the driver lease is held it is that single source for movement,
and who drives is decided by whoever holds the lease, first come (`take` is refused with `held-by <who>` otherwise).

Ops (`POST /drive`, body `{op, who, ...}`; `GET /drive` returns the state): `take` (`why`, optional `idleS`, a number 1..3600: this takeover's idle limit instead of the default), `set`, `stop`, `ping`,
`release` (`force` reclaims another driver's hold). A refusal is `{ok:false, reason}` with reason `offline`, `settling`,
`held-by <who>`, `not-taken`, `not-driver` or `bad-args`. `set` fields:

| field | what |
|---|---|
| `controls` | `{forward, back, left, right, jump, sneak, sprint}` booleans |
| `look` | `{yaw, pitch}` absolute or `{dyaw, dpitch}` relative, in Minecraft F3 degrees: yaw 0 south (+z), 90 west, 180 north, 270 east; pitch -90 up to 90 down |
| `ms` | 1..10000, hold the controls for that long, then release them |

Engine semantics: `take` cuts the holder like a reflex does. A listed job resumes after release; a reflex job is dropped.
While manual the scheduler is paused through the same gate as offline and settling: no trigger is evaluated, no `:stop`
latch clears, and reflex ends are deferred. Nothing about it is written to `engine.edn`. A restart ends it, and so does
going offline.

Dead-man: untimed controls are released after 1 s without any op from the driver (warn `system.drive_deadman`). The
takeover itself ends after 15 s of silence by default (`--drive-idle-s`, or `idleS` on `take`), reason `idle`. Timed
holds end on their own. Every reply's `manual` (and `GET /drive`) carries `idleMs` (the lease's idle limit), `expiresAt`
(epoch ms, last op plus `idleMs`) and `idleLeftS` (seconds left, one decimal). `GET` is read-only: it does not count as an
op and does not reset the silence clock. An agent driving step by step must keep its commands under the idle limit apart,
or take with a longer `--idle-s` (a `ping` keeps the lease alive without moving).
The timers run in the body, so a dead CLI, view server or browser tab cannot leave it walking.

Where the rules live: `engine.lease` holds them, pure; `engine.takeover` applies them to the engine and is ticked by the engine loop (also while paused); `engine/js/control.mjs` is a stateless socket adapter. There is one heartbeat clock: any op from the holder (a `ping` included) keeps the lease, and 15 s of silence ends it; `engine.lease/beat-ops` decides which ops count.

Events: `system.takeover_started` `{who why}`; `system.takeover_ended` `{who reason held-ms}` with reason `released`,
`forced`, `idle`, `offline` or `shutdown`; `system.drive_deadman`.

CLI (`--state <dir>` is the state directory holding `agents/`, default the repo's `state/`; `--who` defaults to `claude`):

```
node engine/tools/drive.mjs ProbeDrive take --who claude --why "stuck in a pit"
node engine/tools/drive.mjs ProbeDrive look 270 0 --who claude        # face east
node engine/tools/drive.mjs ProbeDrive hold forward,jump 2000 --who claude
node engine/tools/drive.mjs ProbeDrive turn 90 --who claude
node engine/tools/drive.mjs ProbeDrive jump --who claude
node engine/tools/drive.mjs ProbeDrive stop --who claude
node engine/tools/drive.mjs ProbeDrive state
node engine/tools/drive.mjs ProbeDrive release --who claude           # --force reclaims another driver's hold
```

Exit codes: 0 ok, 1 refused, 2 no running body or bad usage. The view page can drive too; see `docs/view-format.md`.

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
(once per `:every-ms`, default 2000) between reflexes; its test drops the health, then places a zombie, then kills
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
| `jobs.movement.go-to` | `{:pos :range 1}` | always | `:blocked` count (consecutive walks without a new best distance, more than 1 below `:best`, the nearest any walk ended; a new best resets it), `:best` | none; hands over `{:arrived bool :reason?}`, also emitted as a `:result` info event |
| `jobs.time.wait-for-day` | none | it is day | none | none |
| `jobs.survival.eat` | `{:item nil :until 18 :allow-bad false}` | food below `:until` and something edible carried | none | writes `:fed` (cap 20, 6 h) |
| `jobs.movement.look-around` | `{:every-ms 2000}` | always | none | writes `:looked` (cap 1, forever); looks in a random direction |
| `jobs.movement.pace` | `{:a pos :b pos :laps 3 :rounds 8 :range 1}` | always | `:rounds-run` | none; a leg that does not arrive warns `:leg-unfinished` and ends it |
| `jobs.forestry.fell-tree` | `{:species nil :radius 16}` | a column is chosen, or every candidate was unreachable, or a tree (log column with leaves near its top) is in radius | `:column {:x :z}`, `:species`, `:base`, `:partials`, `:unreachable` | writes one `:forestry/replant` `{:pos base :species}` when the base log is dug |
| `jobs.forestry.collect-drops` | `{:radius 16 :filter [names] or nil}` | always | `:skipped` ids of unreachable items, `:collected` count | none; hands over `{:collected n}` |
| `jobs.forestry.plant-sapling` | `{:at pos or nil :species nil :bone-meal 0}` | nothing to plant, or a matching sapling is carried and the spot holds no log | none | plants at the oldest `:forestry/replant` debt and forgets it; with `:bone-meal n` it then uses up to n bone meal on the sapling, one per round (memory `:meal {:pos :left}`), stopping when it is no longer a sapling or none is carried |
| `jobs.forestry.harvest-wood` | `{:species nil :radius 16 :filter nil}` | the current phase's child check | `:phase`, children in slots `:fell`, `:collect`, `:plant` | as its children |
| `jobs.storage.deposit` | `{:chest pos or nil :items [names] or nil :keep {}}` | a chest is known (args or `:chest`) | `:failures` | reads `:chest`; hands over `{:gave-up false}` when nothing is left, `{:gave-up true :reason status}` when failures used it up (`"unreachable"` for a blocked walk) |
| `jobs.farm.till` | `{:from pos :to pos}` or `{:center pos :radius r}` (at most 256 cells) | nothing pending, or a `_hoe` is carried | `:tilled` (set of cells), `:tries`, `:skipped {pos reason}` | none; digs ground cover first, skips `:not-tillable`, `:covered`, `:unreachable`, `:refused`, `:cover-stuck`, `:gone` (info `till.skipped`); hands over `{:tilled n :skipped {...}}` with info `till.done`. Dry farmland turns back to dirt within a minute or so on this server; water is the caller's concern |
| `jobs.farm.fertilize` | `{:at pos or nil :center pos or nil :radius 8 :max 16}` | bone meal carried, nothing left to do, or some already used | `:used`, `:refused` (set) | none; bone meal on unripe wheat, carrots, potatoes, beetroots (the crop at `:at`, else within `:radius` of `:center` or the body), nearest first; hands over `{:used n}` with info `fertilize.done`; ends when the crops are ripe, `:max` is used or the bone meal runs out |
| `jobs.farm.compost` | `{:at pos or nil :radius 16 :items [names] or nil :keep {} :times 1}` | always | `:composter`, `:fed {name n}`, `:taken`, `:strikes`, `:waits` | none; feeds the composter (`:items`, else every compostable carried except seeds and own food, minus `:keep`), waits out level 7, empties level 8 with an empty hand and collects the bone meal; done after `:times` bone meal (info `compost.done`), or `:nothing-to-feed`, `:no-composter` (warn), `:gave-up` after 3 failures in a row (warn); hands over `{:fed :bone-meal :level :reason?}` |
| `jobs.apiary.guard` | `{:box {:from :to} or nil :center pos or nil :radius 16 :max 12}` | a lit campfire in the area that needs a sink or a carpet; false when every one is safe, so cheap under `repeat` | `:center`, `:started`, `:sunk`, `:carpeted`, `:skipped {pos reason}`, `:strikes`, `:sinking {:fire :kind :carpet}` (a cut resumes it) | none; makes lit campfires safe for bees and bodies, nearest first, at most `:max` actions: a raised fire (a side open) over walled ground is dug out with the ground under it and a carried campfire placed one lower, then a fire with nothing on it gets a non-moss carpet; never stands in a fire's cell; hands over `{:sunk :carpeted :reason :left :skipped :fires}`, `:reason` one of `:guarded :limit :safe :no-fire :no-carpet :no-campfire :unreachable :on-fire :occupied :cannot :place-failed :gave-up`; info `apiary.guard-done`, warn `apiary.guard-gave-up` unless `:guarded`/`:limit` |
| `jobs.build.clear-box` | `{:from pos :to pos :keep [names]}` (at most 400 cells) | always | `:dug`, `:tries`, `:skipped {pos reason}` | none; digs top layer first, nearest first, equipping the best carried shovel, axe or pickaxe (`engine.jobs.tools`); keeps beds, containers, fluids and `:keep` names; walks to unloaded cells first; skips `:cannot`, `:unreachable`, `:refused` (info `clear-box.skipped`); hands over `{:dug :skipped :kept :fluids}` with info `clear-box.done`. Drops are not collected |
| `jobs.farm.find-spot` | `{:w 5 :h 5 :range 24 :center nil :depth 12 :limit 3 :walk false}` | always | `:scan {:next-x :found}` while scanning, then `:spots` | none; scores every w x h patch within `:range` of `:center` (else the body): level% + 25 x share of cells hydrated (water within 4 in x and z, at the cell's y or one above) + 15 if no block above the scan window − min(40, height deviation) − min(30, distance/4); water, lava, ice and magma surfaces never count. The scan reads at most 4096 blocks per round plus one patch row (about 44 rounds for the defaults); then info `find-spot.found` (or warn `find-spot.none`) and, with `:walk`, a walk to the best north-west corner; hands over `{:spot pos :spots [...] :walked bool :reason?}` |
| `jobs.storage.make-room` | `{:free 4 :chest-range 32 :keep-food 16 :keep-blocks 64 :toss-below 1 :swap-radius 8 :away 4 :max-rounds 40}` | fewer than `:free` slots free | `:rounds`, `:tossed-at`, `:toss-dir`, `:walked`, `:acted`, `:swap-id` (with `:swap-item`, `:swap-worth`), child `:deposit` | reads `:chest`, `:chest-unusable`, `:picked-up`; writes `:chest-unusable` `{:pos :reason}` (cap 5, 10 min); emits info `make-room.tossed`, `.swapped`, `.done`, `.declined`, warn `make-room.stalled`, `make-room.toss-failed` |
| `jobs.storage.withdraw` | `{:chest pos or nil :items {name count}}` | a chest is known (args or `:chest`) | `:failures` | reads `:chest`; carries at least count of each name, one name per round, re-deriving what is short from the inventory; hands over `{:gave-up false :short {name n}}` (short is what the chest lacked) or `{:gave-up true :reason status :short {name n}}` (`"unreachable"` for a blocked walk, `"nothing-moved"`); info `withdraw.short`, warn `withdraw.gave-up` |
| `jobs.storage.kit` | `{:tools [kind] (default ["hoe"]) :spare n (1) :food n (12) :chest pos or nil}` | a chest is known (args or `:chest`) | `:failures`, child `:take` | reads `:chest`; carries 1 + spare of each tool kind (a name equal to the kind or ending `_kind`, any tier, best first) and `:food` food items, taking the plan from the inspected chest through `jobs.storage.withdraw`, re-derived every round; hands over `{:gave-up false :short {kind n}}` (short is what the chest lacked, keyed by kind string or `:food`) or `{:gave-up true :reason r :short {kind n}}` (`"unreachable"`, the inspect status, or the withdraw's reason); info `kit.short`, warn `kit.gave-up` |
| `jobs.farm.harvest` | `{:radius 12 :center pos or nil :replant true :crops [crop block names] or nil :give-up 4 :reach 4.2}` | a replant debt or collect sweep is owed, a ripe wanted crop is within `:radius` of the centre while fewer than `:give-up` were unreachable, or the job has begun (so the finishing round runs) | `:center` (the body's position at the first round when nil), `:replant` (debt `[{:pos :seed}]`, written before each dig), `:skipped`, `:unreachable`, `:collect`, `:cut`, `:replanted`, `:bare`, `:warned`, child `:collect` | cuts ripe wheat, carrots, potatoes and beetroots (walking only when none is within `:reach`), seeds each cut cell again from what is carried (collecting the drops first), gives up cutting after `:give-up` unreachable crops; hands over `{:cut n :replanted n :bare [cells] :gave-up bool}`; info `harvest.done`, warn `harvest.gave-up`, warn `harvest.bare` |
| `jobs.survival.retreat` | `{:radius 8 :ranged-radius 16 :clear-radius 40 :eat-gap 12 :step 6 :cooldown-ms 5000 :weapons}` | always | `:last-seen` | reads `:bed`, `:home`, `:hazard` |
| `jobs.survival.sleep` | `{:bed-radius}` | night, a `:bed` within `:bed-radius`, and no unexpired `:bed-unreachable` at that pos | child `:go` | reads `:bed`, `:bed-unreachable`; writes `:slept`, retracts a missing `:bed` (not when its chunk is unloaded: retried, warns `bed_unloaded`), writes `:bed-unreachable {:pos}` (cap 5, 10 min) when the bed stays unreachable after three tries |
| `jobs.survival.breathe` | `{:min-oxygen 12 :radius 2 :reach 10 :shore-radius 6}` | drowning (swims up, or walks sideways to a column with air, then swims toward the nearest land within `:shore-radius`), enclosed (the suffocating condition: a sideways step first, else dig), or surfaced and still in water | `:noted`, `:surfaced`, `:side-tried`, `:failures` | writes `:breathe` (cap 20, 1 h) |
| `jobs.survival.extinguish` | `{:water-radius 6 :step 4 :scan-radius 8}` | on fire or in lava, without fire resistance; stands still (info `:extinguish_wait`, done) when on fire with no bucket use, no water in `:water-radius` and no hazard within 1.5 blocks; after pouring a carried water bucket it remembers `:poured` and, once the fire is out, scoops the water back with `bucket` (info `:scoop_failed` if not placed; gives up waiting after 8 rounds) | none | writes `:extinguish` (cap 20, 1 h), `:hazard` for lava seen (cap 50, 6 h) |
| `jobs.survival.recover` | `{:health 7 :healed 16 :sight 16}` | health below `:health`, or below `:healed` with a `:hurt` in the last 5 min, or a spell under way | `:spell-started`, children `:flee`, `:safety`, `:eat` | writes one `:hurt` per spell; reads `:bed`, `:home` |
| `jobs.survival.respond-to-hostile` | `{:radius 8 :fight-health 12 :min-health 8 :max-fight 2 :weapons ["_sword" "_axe"]}` | a hostile within `:radius` | `:decision`, `:logged`, child `:fight` or `:flee` | writes one `:hostile` per encounter (cap 50, 1 h) |
| `jobs.survival.fight-back` | `{:range 4 :min-health 8 :weapons ["_sword" "_axe"] :attack-gap-ms 600}` | health at least `:min-health` and a hostile within `:range` | `:last-attack` | none |
| `jobs.combat.attack` | `{:targets [] :radius 16 :weapons :attack-gap-ms nil :lost-s 5 :timeout-s 120 :no-damage-hits 4 :max-hits 40 :walk-timeout-s 5 :absent :done}` | a listed target within `:radius`, or started, or `:absent` is `:done` | `:started :last-seen :last-attack :seen :hits :quiet :health :fails :given-up :killed :killed-players` | none; hands over `{:reason :killed :given-up}`; emits info `attack.done`, warns `attack.gave-up`, `attack.timeout` |
| `jobs.survival.get-food` | `{:food 6 :food-when-hurt 14 :source-radius 64 :hunt-radius 24 :farm-radius 6 :take 16 :attack-gap-ms 600 :ask-cooldown-ms 600000}` | hungry (as the hungry trigger), or a meal under way | `:eating`, `:dead-source`, `:last-swing`, `:skipped-animals`, `:skipped-blocks`, children `:eat`, `:goto`, `:collect` | reads `:food-source` (forgets one found empty or unreachable); writes `:hungry` when nothing is found; during `:ask-cooldown-ms` after that it still eats and harvests/hunts what is in sight (no wheat) but skips the remembered sources it already knew when it gave up (one learned since is still tried first) and returns `:declined` when nothing is in sight |
| `jobs.survival.shelter` | `{:roof-height 4 :bed-radius :urgent-bed-radius 128 :max-days-awake 3}` | the night-unsafe condition; a round ends `:done` when asleep, roofed within `:roof-height` or not night, and `:declined` when no child could do anything | `:sleep-failed`, children `:sleep`, `:dig-in` | reads `:slept`; writes `:needs-bed` (cap 1, 1 day; the once-a-day `needs_bed` warn flag); dig-in writes `:shelter`, which nothing reads) |
| `jobs.survival.dig-in` | `{:roof-height 4 :blocks [building blocks] :max-places 4}` | night and no roof within `:roof-height` | `:mode` (and `:roof`, `:target-y` in dig mode), `:placed` | writes `:shelter` (cap 10, 1 day) `{:pos :roof :state :built}` from the current feet and the cells it placed, plus `:door` in walls mode (history only), and `:dig-in-futile` `{:pos}` (cap 5, 10 min) when a dig yields nothing to roof the pit with; its check then declines while no block is carried and one lies within 8 blocks. Walls mode recomputes its cells from the current feet each round; dig mode rechooses if the body leaves its column and stops (`dig_in_failed`) when a dig yields nothing to roof the pit with |
| `jobs.survival.log-out` | `{:bed-radius :offline-allowed true :offline-ms 20000 :player-radius 128}` | night, no usable bed, allowed, not unsupported before, another player sleeping (fired by the `:player-sleeping-nearby` reflex; the next firing logs out again if the night is not over) | none | writes `:log-out` (cap 10, 1 day) |
| `jobs.survival.recover-drops` | `{:margin 0 :danger-radius 8 :collect-radius 6}` | a `:died` with no newer `:recovered` | `:death-t` (the death it is about; a different death resets the rest), `:decided`, `:phase`, children `:go`, `:collect` | reads `:died`, `:respawned` (waits 2 s after a respawn before estimating); writes `:recovered` `{:decision :collected/:skip/:abandoned ...}` (cap 10, 1 day); emits info `:recover-drops.decided` with the decision and a `:text` |
| `jobs.maintenance.unstick` | `{:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000 :max-attempts 6}` | stuck (as the stuck trigger), or an attempt under way | `:attempts :rounds :best-y` | reads `:moved`; each attempt ends with a `moveTo` toward the stored goal (range 1, `:maxDistance` 3), then an uncapped retry (`:timeoutS` 6) if the body did not move over `:min-move` (displacement, not status, decides); waits for the body to land (`onGround`, up to 1 s); attempt 1 steps back unless in a pit; in a pit it pillars with `jumpPlace` (depth-many blocks of the largest placeable stack) whenever a block is carried and the cell above the head is open (else `pillar: no headroom`), else digs a door (one-block wall: front at feet and head height) or a stair step (headroom plus the next step's two cells, then `moveTo` onto it; never under sand/gravel or next to water/lava; a dig status but dug/missing ends the attempt); a round after which the feet are higher than ever yet in the spell (`:best-y`) is progress and does not count as an attempt, so climbing a deep pit one stair step per round is not cut short; only counted attempts reach `:max-attempts`, and a cap of `:max-attempts` + 8 rounds per spell bounds the rest; a stair `moveTo` records no reason when it arrived or the feet rose, else `stair: moveTo <status>`; writes `:stuck` (cap 10, 1 h) when it gives up, the `unstick.failed` warn carrying `:attempts` (counted), `:rounds` (used), `:reasons` (each distinct reason once, with a count like `(x6)` when repeated) and a `:text` naming them; equips the best pickaxe before digging |
| (culling) | `(jobs.combat.hunt {:mob "cow" :keep 4 :count 8})` | the hunt's own check | the hunt's own | the hunt's own; culling is not a job of its own, it is this hunt expression: it kills adults of the kind down to `:keep` within `:radius`, at most `:count` per run (the per-run cap), never calves |
| `jobs.animals.tend` | `{:mob "cow" :box nil :target 4 :chest nil :keep {}}` | a `:box` is given and some step would run (breed, cull, shear, collect or deposit), or the job has started; otherwise it declines (cheap under `repeat`) | `:todo` (steps still to run, from `:breed :cull :shear :collect :deposit`), `:report` `{step summary}`, `:call-args` (the step under way, not re-decided), children `:breed`, `:cull`, `:shear`, `:collect`, `:deposit` (one child round per job round; skipped steps are booked `{:skipped reason}`); known limit: the pen is the `:box` only, nothing checks that an animal inside it is inside the fence, and breed, shear and collect-drops look around the body (a radius covering the box), so same-kind animals or items just outside the box can be fed, sheared or picked up | breed (2 animals, with `:target` counting babies), cull (`:keep` the larger of 2 and `:target` minus babies), shear (sheep with shears), collect-drops (only the item names lying in the box), deposit (`:chest`, produce only, `:keep` left carried; tools and breeding food stay); hands over `{:mob :target :adults :babies :steps}` (live census in the box); emits info `tend.done`; ends `:done` even when every step was skipped |

| `jobs.apiary.harvest` | `{:with :either :box {:from :to} or nil :center pos or nil :radius 12 :max 8 :walk-timeout-s 8}` | always | `:center`, `:harvested`, `:tool`, `:skipped {pos reason}`, `:collected`, `:strikes`, `:phase` | none; takes the honey of ripe hives (`honey_level` 5) with shears (then collects the honeycomb) or a glass bottle, nearest first, only when smoked by vanilla's rule (lit campfire up to 5 under it); an unsmoked hive is declined without a click (`:not-smoked`), one over an open lit fire too (`:open-fire`); hands over `{:harvested :reason :with :declined :skipped :collected}`, `:reason` one of `:harvested :limit :no-tool :no-hive :not-ripe :not-smoked :open-fire :unreachable :gave-up`; info `apiary.done`, warn `apiary.gave-up` unless `:harvested`/`:limit` |
| `jobs.apiary.maintain` | `{:box {:from :to} or nil :center nil :radius 12 :with :either :target nil :chest nil :keep {}}` | some step would run: a lit fire a carried item can sink or carpet, a ripe smoked hive with its tool carried and no unsafe fire, bees below `:target` by day without rain with 2 adults and a flower carried, or produce above `:keep` with a `:chest`; false when none, so cheap under `repeat`; always once started | `:todo` (steps left of `:guard :harvest :breed :deposit`), `:report` `{step summary}`, `:call-args` (the step under way), `:center`, children `:guard`, `:harvest`, `:breed`, `:deposit` (one child round per job round; each step runs at most once per pass) | one convergent pass in a fixed order over `jobs.apiary.guard`, `jobs.apiary.harvest`, `jobs.animals.breed` (2 bees, `:mob "bee"`, `:target` counts adults and babies in the area) and `jobs.storage.deposit` (honeycomb and honey bottles only; tools, bottles, carpet, campfires and flowers stay carried); harvest is held back (`:unsafe-fire`) while a ripe hive stands over a fire that still lacks a sink or a carpet; a step that does not apply is booked `{:skipped reason}`, a declined child `:declined`, a child that throws `:failed` with `:error`; hands over `{:target :bees :steps}`; info `maintain.done`; ends `:done` also when every step was skipped after the first |
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
  `chest_unusable`. `:keep` (`{item-name count}`) leaves at least that many of
  a name carried: with `:items` the names are taken in the order given (first
| `jobs.animals.cull` | `{:mob "cow" :keep 2 :centre nil :radius 16 :box nil :count nil :collect-radius 8 :drops nil :weapons :max-skips 3}` | more than `:keep` adults of `:mob` inside the bound (`:box`, else `:radius` around `:centre`, else around the body; babies never count), or started | `:started :target :collecting :killed :skipped :skips :misses`, children `:attack`, `:collect` | none; attack and collect-drops children as hunt; candidates are not-skipped adults, ones whose `hittable` is not false first then nearest; hands over `{:killed :remaining :babies :reason :skipped}` with reason `:keep`, `:count`, `:unreachable`, `:gave-up` or `:none`; emits info `cull.done`, warns `cull.gave-up` |
  stack of the first name whose carried total is over its keep; without it,
  the first stack in inventory order) and the stack is moved, cut to `total - keep` when it is bigger. The job hands its
  parent `{:gave-up false}` when nothing was left to put away and `{:gave-up
  true :reason r}` when the failed attempts ended it (`r` the transfer status
  `full`, `missing`, `unreachable`, ... or `"unreachable"` for a walk that
  could not get there).
- `:make-room` is the `:inventory-nearly-full` reflex job. Every round first
  asks whether anything is left to do, and ends `:done` when at least `:free`
  slots are free. Protection: tools, weapons and armour (deposit's `tool?`)
  and the three buckets are never put away or thrown. Food (`jobs.survival.eat/edible`)
  is never thrown and is put away only above `:keep-food` (best food-points
  first); building blocks (`jobs.survival.dig-in/building-blocks`, in that
  order) are put away or thrown only above `:keep-blocks`. Steps: (1) a
  `:chest` within `:chest-range` of the body with no `:chest-unusable` entry
  for its position takes the names above their keep, least worth keeping first (names
  without a floor before food and building blocks, then the cheapest by
  `engine.value/item-worth`, then the name picked up longest ago): deposit with
  `:keep` and `:items` in that order; a
  chest that gives up is remembered unusable for 10 minutes and the job goes
  on without it); (2) with no slot free, the nearest item within
  `:swap-radius` whose `engine.value/item-worth` is above that of the first
  throwable stack: that stack is thrown away from the item, then `collect`
  fetches the item (`make-room.swapped`); (3) otherwise the first stack of
  the toss order is thrown (`look` then `toss`): worth below `:toss-below`,
  cheapest first, then the name picked up longest ago (the `:picked-up` body
  entries), then the smaller count; a stack is only thrown whole and only when
  its name keeps its floor (food, block and tool floors above) afterwards. The
  direction is the first of +x, -x, +z, -z whose two cells ahead at eye level
  are not solid (+x when none is; for a swap, the cardinals most opposite the
  item first). After throwing, once `:free` slots are free (or nothing more may
  be thrown) the body walks `:away` blocks back from where it threw, so it does
  not pick the stack up again. With nothing it may throw it declines
  (`make-room.declined`, reason `nothing-to-toss`), and after `:max-rounds`
  rounds it declines with a `make-room.stalled` warn. Three failed tosses end
  it with a `make-room.toss-failed` warn.
- `:attack` kills the entities `:targets` names (ids, player usernames, mob types) within `:radius`: nearest first,
  best weapon, one swing per `:attack-gap-ms` (nil: the held weapon's cooldown, `combat/attack-gap-ms`). A target is
  given up on (warn `attack.gave-up`, reason `:unreachable`, `:no-damage` or `:too-many-hits`) after three blocked
  walks or out-of-reach swings, `:no-damage-hits` swings that did no damage, or `:max-hits` hits. A kill is booked
  only when `attack` reports `killed` (a target that merely vanishes is not); a killed target is not attacked again,
  even respawned. Done with
  `:cleared` (nothing in `:radius` for `:lost-s`, waiting in 1 s steps, and every target ever seen was killed),
  `:gave-up` (the same, each killed or given up on, at least one given up on; also when every target present is
  given up on), `:lost` (the same, but one was neither, e.g. it left or vanished), `:timeout`, or
  `:absent` (no target, after a 2 s grace for the world's entities to arrive). `:absent :done` (default) starts the job anyway and ends it:
  `(jobs.combat.attack {:targets [123 "zombie"]})` is a one-shot order. `:absent :wait` makes the check decline until
  a target is present: `(repeat (jobs.combat.attack {:targets "zombie" :absent :wait}))` is a standing guard.
  Creepers get no special handling: they are attacked only when listed, and the body does not back off. It does not
  guard health: the survival register cuts it and it resumes.
- `:retreat` walks `:step` blocks away from the nearest hostile per round,
  leaning towards the latest `:bed` or `:home` when that is not through the
  hostile, and turning up to 120 degrees to keep clear of `:hazard` cells and
  of walls (feet and head cells along the way must be passable). A hostile
  within `:radius` (ranged ones within `:ranged-radius`) starts the flight; it
  goes on while one is within `:clear-radius`, and is done once none has been
  for `:cooldown-ms`. Cornered (no open direction, or the walk is blocked) it
  runs `fight-back` with `:min-health 0` when a weapon is carried; unarmed,
  no way out counts as a failed round (`retreat_blocked`).
  Once per flight, with at least `:eat-gap` blocks to the hostile, it eats
  (up to 20 food) so health regenerates on the run. `:respond-to-hostile`
  keeps a fight going below `:min-health` while the target is nearly dead by
  the hits `fight-back` landed (`combat/nearly-dead?`).
- `:sleep` walks within 2 of the known bed (go-to as a child) and calls
  `sleep`, and remembers a bed it gave up on as `:bed-unreachable` for ten minutes. `sleeping` and `not-night` are done; a go-to that hands over
  `{:arrived false}`, a taken bed or a nearby monster is a failed round
  (`bed_unreachable`, `bed_unusable`); a missing bed warns `bed_missing`,
  retracts the `:bed` and ends.
- The survival jobs' docstrings (`(:doc (registry/jobs 'jobs.survival.x))`)
  give the full rules; in short: `:breathe` swims up to air then walks to land (info
  `:no_shore_near` when none is in reach), or steps sideways, else digs the head
  cell free, one move per round; `:extinguish` pours a carried water bucket at
  its feet, else walks into water or to the safest dry cell nearby;
  `:recover` flees, walks to a bed or home, eats and waits (a 2 s `wait` per
  round) for health to reach `:healed`, giving up below 18 food with nothing
  to eat; `:respond-to-hostile` fights (`fight-back`) when healthy, armed,
  not facing a creeper and outnumbered by at most `:max-fight`, else retreats;
  `:get-food` climbs a ladder of eat, known source, hunt or harvest, then
  gives up with a `food.none` warn; `:shelter` tries sleep, then dig-in
  (logging out for a sleeping player is the `:player-sleeping-nearby` reflex's own job), ends once the body is roofed (or asleep, or it is day) and declines
  when none of them can act; `:recover-drops` weighs the
  drops' value (`engine.value`) against the trip and goes back for them or
  skips; `:unstick` steps back (not in a pit), pillars out of a pit (`jumpPlace`) when it carries a block, else digs a door or a stair step.

Triggers (`engine.triggers`; `:when` receives the world, a memory view and
the register entry's `:args`):

Listed in the order a survival register puts them (most urgent first, as
`scenarios/survival.edn` does); `engine.triggers/all` lists them the same way.

| trigger | holds when | job | persistence |
|---|---|---|---|
| `:suffocating` | in water with oxygen below `:min-oxygen` (default 12) and the head not in air, or the head cell holds a suffocating block | `(jobs.survival.breathe)` | cooldown 2 s |
| `:burning` | on fire or in lava, and no `fire_resistance` effect | `(jobs.survival.extinguish)` | cooldown 2 s |
| `:hostile-near` | a hostile mob within `:radius` (default 8), or a ranged one (skeleton, stray, bogged, pillager, witch) within `:ranged-radius` (default 16), that the body can see (`:visible-only false` counts hidden ones too); set the job's own `:radius` and `:ranged-radius` in `:job` | `(jobs.survival.respond-to-hostile)` | cooldown 5 s |
| `:health-low` | health below `:health` (default 7) | `(jobs.survival.recover)` | cooldown 10 s |
| `:hungry` | food below `:food` (default 6), or below `:food-when-hurt` (default 14) while health is below 20 | `(jobs.survival.get-food)` | cooldown 90 s |
| `:night-unsafe` | night, awake, and nothing solid within `:roof-height` (default 4) above | `(jobs.survival.shelter)` | cooldown 10 s |
| `:player-sleeping-nearby` | night, another player within `:player-radius` (default 128) asleep, no `:bed` remembered within `:bed-radius` (default 48), `:offline-allowed` not false, the last `:log-out` not `unsupported`; being roofed does not matter | `(jobs.survival.log-out)` | cooldown 30 s |
| `:night-and-bed-known` | an alias of `:night-unsafe` under its old name, kept for the older scenarios; register one or the other | `(jobs.survival.shelter)` | cooldown 10 s |
| `:stuck` | the last `:n` (4) `:moved` entries, none older than the latest `:stuck` and the latest `:restart`, are all bad moves (not arrived or partial, or under `:min-move` 1.5 blocks), the newest of them is under `:window-ms` (60 s) old, and the latest `:stuck` is over `:quiet-ms` (5 min) old | `(jobs.maintenance.unstick)` | cooldown 60 s |
| `:died` | a `:died` entry younger than five minutes with no newer `:recovered` | `(jobs.survival.recover-drops)` | cooldown 30 s |
| `:inventory-nearly-full` | at most `:free` (default 2) of the 36 main and hotbar slots are empty; no chest is needed | `(jobs.storage.make-room)` | cooldown 120 s |
| `:every-interval` | no `:looked` entry, or the latest is at least `:seconds` (default 60) old | `(jobs.movement.look-around)` | cooldown 0 |

`:every-interval` is a wall-clock reflex: `look-around` looks at a point
three blocks ahead and writes `:looked` (which survives a restart and makes
the trigger stop holding), then waits `:every-ms` (default 2000) before its
next round. With no entry it fires at once.
`scenarios/woodcutter-cuts.edn` is the woodcutter with it first in the
register; `scenarios/pace-cuts.edn` puts it above a long `pace` job.

`:inventory-nearly-full` counts free slots as 36 minus the stacks in
`self().inventory`, which lists main and hotbar slots only (armour and the
off-hand are not in it): `engine.jobs.util/free-slots`.

`engine.value/item-worth` (a number: 0, 1, 5 or 25, by tier; a missing `:count`
counts as 1) is the shared item worth: `recover-drops` sums it over a death's
inventory (`inventory-value`) and `make-room` reads it to decide what may be
thrown.

### Live-unverified assumptions

The survival jobs pass against the fake only. What they assume about the real
server and mineflayer, none of it checked live:

- `extinguish` pours a water bucket with `place` at the body's own feet cell. Falsified live (2026-10-03): `place` with a water bucket on air rejects with "Server refused to place water_bucket ... the block is still air", and with a fire block in the feet cell it returned `occupied`; a bucket needs a use-item primitive. Since then `place` uses buckets through `activateItem` and treats fire, grass and snow as free (see the `place` result above); not yet re-checked live.
- Verified live: placing into the body's own cell is refused by the server ("the block is still air"), so `unstick` pillars with `jumpPlace` instead: in a 3-deep 1x1 pit, count 3 places 3 blocks in about 1.9 s and leaves the body at ground level; with no block it returns `failed`/`no-item` at once, under a roof `failed`/`no-headroom`.
- `jobs.combat.attack` judges damage by `attack`'s `hurt` (the server's entityHurt) since `health` is never present live; whether a creative player, an `Invulnerable` mob and PvP-off all report `hurt: false` is unchecked.
- `breathe`, `extinguish` and `unstick` use `moveTo` with range 0 to step
  into a cell, water included.
- `dig-in`'s roof placement may fail with no supporting neighbour
  (`no-support`); it then counts a failed round, and three end the job with
  `dig_in_failed`.
- `onFire` and the sleeping pose are read from metadata indices found through
  the registry's `metadataKeys` (see Sensing); the fallback indices are guesses.
- `:hostile-near` needs a visible hostile, but `respond-to-hostile` still picks its targets with `combat/hostiles`
  without `{:sight ...}`, so once it runs it may choose one behind a wall.
- Wheat is not food raw and there is no crafting yet, so `get-food` skips it.
- `make-room`: a tossed stack lands outside the pickup range of the body, so it
  does not take the stack straight back (the walk-away is the guard);
  `playerCollect` fires for the body's own ground pick-ups (verified live) and
  its dropped item is readable (the `picked-up` event is skipped when it is
  not), but not for `/give` (verified live: 0 `picked-up` events after about
  150 `/give`s). Items that arrived any way but a pick-up (`/give`, withdrawn
  from a chest) have no `:picked-up` entry and count as picked up longest ago
  (recency 0) in the toss and deposit orders.

## Not built (hooks only)

Claims, no-touch regions, flapping counters, the per-body no-progress
detector, progress events, a general job-control API, multi-body, world memory, soft
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
| `moveTo` | `pathfinder.goto(GoalNear)`. Beyond `maxDistance` it walks a `GoalNearXZ` point that far along the straight line, so the result is `partial`. Cleanup: `setGoal(null)`, `clearControlStates`. After the bound or a pathfinder failure: `partial` if at least 1 block closer, else `blocked`. When the pathfinder reports `path_reset` `'stuck'` and its first path node is one block up and horizontally adjacent (a 1-deep hole: the body presses flush against the ledge and the server rejects every jump), the walk steps up at most twice: it centres in the cell, holds jump alone, presses forward once the feet are a block above the start, then re-issues the `goto`. A resolved `goto` counts as `arrived` only if `goal.isEnd` holds for the floored body position: goto.js (patched) also resolves on a `noPath` update with an empty path, which is `blocked` with `reason: 'noPath'`. Movements are built by `connect.mjs` with digging, towers and scaffolding off |
| `swim` | `setControlState('jump', true)` while `blockAt(eye cell)` is water, `false` on every exit |
| `dig` | checks `blockAt`, reach (eye to cell center, 4.5) and `diggable`, then `bot.dig(block, true)`. Cleanup `stopDigging`. Then polls up to 1 s for item entities within 2 blocks of the cell |
| `place` | picks a solid neighbour as the reference (below first), `equip` to hand, `placeBlock`. Liquids count as replaceable |
| `jumpPlace` | per block: needs the item, a full block under the feet and the cell two above the feet not solid; sneaks to the cell centre (within 0.1 and until it has stopped, up to 1 s; a failure to centre is ignored); equips, looks straight down (`look(yaw, -pi/2)`), `setControlState('jump', true)`, polls every 20 ms until the feet are 1.01 above the start cell (clear of it), `_placeBlockWithOptions(block under the start cell, +y, {swingArm: 'right', forceLook: 'ignore'})` into the cell just left (`placeBlock`'s own unforced look turns gradually when the body is slightly off-centre and delays the packet ~1 s, by which time the body has fallen back into the cell and the server refuses: seen live), releases jump, waits up to 1 s for `onGround` and checks the body now stands a block higher. Cleanup: jump released |
| `collect` | `goto` next to the item entity, then waits until the entity is gone. `collected` carries the inventory diff; an entity that vanished with no gain is `gone` |
| `inspectContainer`, `transfer` | `openContainer`, read or `deposit`/`withdraw` on the window, `closeWindow` in a finally and on abort. A thrown error mentioning full, room or space is `full`. `transfer` waits for the window's slot updates to stop (150 ms quiet, 1.5 s cap) before closing, then reopens the container and reads the true count: on the first open after a login mineflayer stamps the click burst with a stale stateId and the server's resyncs stop at an intermediate state, so only a fresh open shows the true slots; `moved` is the change in the container's count between the two opens |
| `equip` | `bot.equip(item, dest)` |
| `toss` | with `slot`: `bot.tossStack(bot.inventory.slots[slot])`. Otherwise sums the carried stacks of the item, `bot.toss(typeId, null, count)` with `count = min(count ?? total, total)`; nothing carried is `no-item` without touching the bot. Throws where the body looks |
| `eat` | best food by `foodPoints` (or the named item), `equip` then `bot.consume()`. Cleanup `deactivateItem`. On timeout it reports `ate` if `food` rose |
| `attack` | one `bot.attack`, then waits up to 300 ms for `entityHurt` on the target or its removal; `killed` when the entity is gone, its health is 0, or an `entityDead` for it arrived, `hurt` when that event came or it was killed |
| `sleep` | bed name check, reach, night (`isDay` formula above), hostile within 8 blocks, then `bot.sleep`. Cleanup `wake` |
| `look` | `lookAt` or `look` with force, then waits for the next `physicsTick` (mineflayer sends the rotation right after emitting it), at most 100 ms |

Sensing reads `bot.entities`, `bot.findBlocks` and `bot.blockAt` directly and never waits. Body events come from the
bot's `health` (a drop is `hurt`), `death`, `respawn` (remembered, then reported as `respawned` at the next `spawn`),
`chat`, `wake`, `spawn`, `end` and `kicked` events, and `playerCollect` (reported as `picked-up` only when the collector is the body's own entity). The listeners are bound per bot, and `offline` unbinds the old bot
before it quits so its `end` does not report a `disconnected`.

Known gaps:

- No tool selection before `dig`; the bot digs with whatever is in hand.
- `moveTo` has no general no-progress detector (only the step-up out of a 1-deep hole): a stuck walk ends at the time bound as `blocked` or `partial`.
- The `hurt` event has no `cause`. `isDay` ignores thunderstorms for `sleep`.
- `eat` relies on `bot.consume()`; on a server that never sends the finish status it can only time out (the `food`
  rise check softens this). Not exercised against a real server.
- Everything is tested against a stub bot only: reach numbers, window handling and the pathfinder are unverified live.

### Deviations

- `createPrimitives` accepts an optional `version` (default as in `src/config.mjs`).
- Timeouts resolve with a status as the contract says, not by rejecting; only cuts and bad args reject.

## Path planner (not used by the engine yet)

`js/path/planner.mjs` is the specification (an A* over a snapshot of section state ids: walking, jumps, drops, gap jumps,
climbing, water, doors; costs in seconds plus risk); `src/engine/path/planner_tuned.cljs` is its port, kept equal to it
by `bench-lang/fixtures-equal.test.mjs`. Build: `npx shadow-cljs compile planner-bench` (tests) and `npx shadow-cljs
release planner-bench-release` (bench). A gap jump or a drop never lands on farmland (vanilla tramples farmland under a
fall of over 0.5 blocks); a jump up one block onto it is allowed (it falls about 0.3 from the top of the arc).

`engine.path.alternatives/plan-alternatives` (`planAlternatives(snapshot, query, options, k = 3)`) returns
`{status, reason, paths, searches, ms}`: up to k paths, best first, each `{steps, cost, summary, total, differs}`
(`total` = seconds + riskWeight × risk, the true cost of the walk; `differs` says what sets it apart, e.g. `on foot
instead of by ladder`). A path is returned only when, against the paths returned before it, its set of kinds (ladder,
water, door; none of them is on foot) is not one of theirs, or fewer than 60% of its cells lie within 2 blocks of their
cells. The first path is `plan`'s own; the others come from the same search re-run with a kind refused, or with the
cells within 1 block of earlier paths costing more (weight 1.5, at most 10× the first search's expansions). One path
back is the normal answer where there is only one way. On the bench set (304 queries, best of 7 interleaved): k=1
p50 1.09 ms, p99 89 ms (as `plan`); k=3 p50 7.0 ms, p99 335 ms, with 2 or 3 paths for 83 queries.

## Migrating old bots

`npx shadow-cljs compile migrate`, then `node out/migrate.cjs [--dry-run] [--state-dir <dir>] Name...` (default state
dir: the repo's `state/`; default names: every folder under `state/agents/` with no `engine/`). `--dry-run` prints one
line per body and writes nothing. Write mode only touches bodies without `engine/`, creates `engine/memory.edn` and
`view/pose.json`, never overwrites, moves or deletes, and refuses a body that looks connected (`engine/control.sock`, a
live `body.pid`, or a listener on `127.0.0.1:<apiPort>`).

Carried: the body's own `bed` and `chest` places (`by` equals its name) from the world's `places.json`, as memory kinds
`:bed` and `:chest`; both are cap 1, so the last one in the file wins and the earlier ones are only reported as dropped.
And an offline pose (`view/pose.json`) at the last position found in `events.jsonl` (`pos` or `position`), at that
event's time. Not carried: jobs, journal, watches, other places, events, chat. A file that fails to parse is skipped
and named; the rest converts.

Every converted body gets an (empty, when it has no bed or chest) `engine/memory.edn`, so the dashboard shows it. The pose carries `dimension` when the event it comes from has one.

Migrated entries have `:t` = the mtime of `places.json` and `:wt` 0. The place policy is `:ttl :forever`, so the first
start's sweep keeps them (tested in `migrate_test.cljs`).
