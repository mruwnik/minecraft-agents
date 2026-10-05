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
npm run body -- --agent <name> --world <world> --scenario <file.edn>
```

Builds (`shadow-cljs.edn`): `:test` is a `:node-test` build to
`out/test.cjs`, picking up every namespace ending in `-test`; `:body` is a
`:node-script` build to `out/body.cjs` with `engine.main/main`.
The compile JVM is capped (`:jvm-opts ["-Xmx1G"]` in `shadow-cljs.edn`) because one compile must fit beside the game server on a 31 GB machine; uncapped it grew past 2 GB.

Layout:

- `js/primitives.mjs` the real mineflayer layer; `js/connect.mjs` makes the bot; `js/stub-bot.mjs` is a bare stub bot for the primitive tests.
- `js/view.mjs` the view dump (chunk columns, pose, hud files for an external renderer; `BODY_VIEW=0` disables), format in `docs/view-format.md`.
- `test/engine/fake.cljs` (`engine.fake`, with `engine.fake.*` for its mechanics) a scriptable fake world with the same interface, for the cljs tests.
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
`out/`, `(js/require "../js/sight.mjs")` resolves to `engine/js/sight.mjs`.

## Primitives

The JS layer exports one factory per module:

```js
// js/primitives.mjs
export async function createPrimitives ({ host, port, username, auth }) // resolves once spawned and the column under the body is loaded (waits up to 10 s)
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
| `self()` | none | `{username, pos, health, food, foodSaturation, oxygen, onFire, inWater, inLava, onGround, chunkLoaded, settling, isSleeping, vehicle, effects, experience, dimension, timeOfDay, isDay, held, equipment, inventory}`; see below |
| `entities(opts)` | `{radius = 16, kind?, names?, max = 32}`; `kind` is one of `hostile`, `passive`, `player`, `item`, `other` | `[{id, name, kind, pos, distance, visible?, item?, username?, sleeping?, creeper?, passengers?, vehicle?}]` sorted by distance; see below |
| `blocks(opts)` | `{radius = 16, names?, match?, max = 64, properties = false}`; `names` is an array of block names, `match` a JS predicate on the block name; with neither, every non-air block | `[{name, pos, age?, properties?, distance}]` sorted by distance |
| `blockAt(pos)` | `{x, y, z}` | `{name, pos, age?, properties?}`, or `null` when the chunk is not loaded |

`self()` fields:

- `health` 0..20 and `food` 0..20; `foodSaturation` is the hidden saturation (0..20).
- `oxygen` 0..20 bubbles (`bot.oxygenLevel`; 20 until the server reports air).
- `equipment`: `{head, torso, legs, feet, offHand, mainHand}`, each `null` or `{name, count, durability?}` (`durability` is what is left, `maxDurability - durabilityUsed`, only for items with a maximum). Armour and off-hand are slots 5 to 8 and 45 of the player window, which `inventory` (main and hotbar) does not list; `mainHand` is `bot.heldItem`. The fake takes `equipment: {head, torso, legs, feet, offHand: {name, count?, durability?}}` in its spec (its `equip` does not move items into it) and the stub bot `worn: [{name, count, slot, durabilityUsed?}]`.
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
- `vehicle`: what the body rides, `{id, uuid, name}`, or `null` on foot (`js/vehicle.mjs`). Mineflayer 4.39 never clears `bot.vehicle` on a dismount, so every bot (and every reconnected one) tracks the full `set_passengers` lists and repairs it from them. The fake takes `self.vehicle` (an entity id) in its spec.
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
A mob on a lead carries `leashed: true`, `leashedToMe` (the holder is this body) and `leashHolder` (the holder's entity id; a fence knot is a `leash_knot` entity), read from the attach_entity packets tracked per bot (`js/leash.mjs`; 26.1 sends holder 0 on release, and the keys are absent when nothing was seen). On 26.1 an empty hand on a `leash_knot` removes it and hands its animals to the body's own lead; a second empty-hand click on the animal drops the lead as an item; a click on a fence post (empty hand or a lead) ties every animal the body leads to a knot there. The fake (`engine.fake.animals`) models this: led animals follow the body's `moveTo` (spec `snaps: true` breaks the lead on the first walk), `useOn` on an `_fence` block ties, `interact` on the knot hands over. Every raw entity list (`entities`, `collect`, hostile checks, the pose snapshot, the observation cache) goes through `js/live-entities.mjs`: an entity no spawn packet described (a bare one Mineflayer makes when a velocity, teleport or metadata packet names an id after its removal) is not listed, and a drop is hidden at once when a `collect` packet takes its whole stack (a partial pickup leaves it; the id is forgotten at `entityGone`).
Any entity but an item carries `passengers` (the ids riding it, the body's own id among them) when someone rides it and `vehicle` (the id it rides) when it rides something, from the same tracked `set_passengers` lists (`js/vehicle.mjs`); absent otherwise. The fake passes `passengers` and `vehicle` from the entity spec.

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

The `:hostile-near` trigger uses it: it never holds while the body is dead (a `:died` with no newer `:respawned`), and otherwise holds only for a real danger within `:radius` (`engine.jobs.reach/danger?`): a
melee mob that is visible and has a walkable way to the body (a bounded search over the blocks as a zombie walks: one
step up, up to three down, doors only when open, water swum; so a mob walled in, across a trench two deep, or with the
body sealed in is no danger), or a ranged mob (skeleton and the like) with a line of fire (visible). Trigger arg
`:visible-only` (default true) false drops a melee mob's sight test; the way to the body still counts. `retreat` and
`respond-to-hostile` count dangers the same way, so a mob that cannot reach the body does not keep a flight alive (BaseMiner
j301 sat 600+ rounds on a walled-off hostile within retreat's 40-block clear radius). The trigger is wrong, not the job,
when a hostile that cannot hurt the body fires the response. `engine.jobs.combat/hostiles` takes an optional third argument `{:sight :only|:prefer}`: `:only` keeps the
visible ones, `:prefer` lists them first (each group nearest first); two arguments ignore sight as before.

Remembered places (a known bed, a known chest) are not primitives. They are
`:bed` and `:chest` entries in body memory; see `engine.memory/place` below.

### Acting (async, token first)

| method | args | statuses | bound | on cut |
|---|---|---|---|---|
| `moveTo(token, a)` | `{pos, range = 1, timeoutS = 20, maxDistance = 64}` | `mounted` while the body rides something (nothing is done: no physics runs aboard; `steer` answers the same), `arrived` (the goal is satisfied where the body stands, not merely a resolved walk), `partial` (bound or `maxDistance` reached, closer than before), `blocked` (no path or no progress); a non-arrived result may carry `reason`: `'noPath'` (the planner found no way), `'planTimeout'` (planning took too long), `'stalled'` (the body got less than 1 block from where it was for 8 s, so a walk stuck flush against a ledge ends in seconds instead of sitting out `timeoutS`), `'timeout'` (`timeoutS` ran out); a target farther than `maxDistance` is walked in hops, each ending within 4 blocks (XZ) of the point `maxDistance` along the line, on a cell open to the sky (sky light >= 12), so a far walk stays on the surface rather than following caves | `timeoutS`, at most 60 | goal cleared, controls released |
| `dig(token, a)` | `{pos}` | `dug` (`drops`: only the items that appeared with this dig within 2 blocks, waited for up to 1 s; an item already lying there is not listed unless its stack grew, and then only the growth is counted), `missing` (air), `unreachable` (more than 4.5 away), `cannot` (unbreakable) | 10 s | `stopDigging` |
| `place(token, a)` | `{pos, item, click?}` | `placed`, `occupied`, `no-item`, `no-support`, `unreachable` | 5 s | nothing placed after the cut; sneak released |
| `jumpPlace(token, a)` | `{item, count = 1}`, count at most 8 | `done`, `partial` (some placed), `failed` (none); `placed` (blocks placed) and `reason`: `no-item`, `no-support`, `no-headroom`, `not-raised`, `place-failed: ...`, `timeout` | 2 s per block | jump released |
| `collect(token, a)` | `{id, timeoutS = 10}` | `collected`, `gone`, `unreachable` (`reason` `out-of-reach` or `not-picked-up`, none when a walk fell short), `timeout` | `timeoutS`, at most 20 | as `moveTo` |
| `inspectContainer(token, a)` | `{pos}` | `ok`, `missing`, `unreachable` | 5 s | window closed |
| `transfer(token, a)` | `{pos, direction, item, count}`; `direction` is `deposit` or `withdraw` | `ok` (`moved` is measured from a reopened container and may be less than `count`, even 0), `missing`, `unreachable`, `no-item`, `full` | 5 s | window closed after its slot updates settle |
| `equip(token, a)` | `{item, dest = 'hand'}`; `dest` is `hand`, `off-hand`, `head`, `torso`, `legs`, `feet` | `equipped`, `no-item` | 2 s | none needed |
| `toss(token, a)` | `{item, count?, slot?}`; with `slot` it throws exactly that slot's whole stack (`count` ignored; `no-item` when the slot is empty or holds another item), without it that many of the item (all carried stacks together; default and maximum: everything carried) in the direction the body looks, it does not look anywhere itself | `tossed` (`count` thrown), `no-item` (`count: 0`) | 2 s | none needed (nothing to undo) |
| `craft(token, a)` | `{item, count = 1, table?}`; `count` is items wanted, rounded up to whole batches (`made` may exceed it: 1 log, `count` 1 `oak_planks` makes 4); one recipe per call, never chains (planks then sticks is the caller's business), never walks; `table` is a `{x,y,z}` crafting table, else any within 4.5 blocks is used; the result stacks are merged afterwards | `crafted` (`made`, `used`), `partial`, `no-item` (`recipes`, `have`; `engine.craft` turns them into `short` and `alternatives`), `out-of-reach` (`reason`: `too-far`, `table`: `{x,y,z}` of the nearest known table, within 32), `unreachable` (`reason`: `no-table` (none within 32), `not-a-table`), `full`, `cannot` (`reason`: `unknown-item`, `no-recipe`), `failed`, `timeout` | `min(60, 4 + 6 * count)` s | closes an open window |
| `furnace(token, a)` | `{pos, op, input?, fuel?, output?}`; one short visit to a furnace, blast furnace or smoker: open, act, settle, close; never waits for the cooking, never walks, decides nothing. `op` `read` answers the state; `load` puts `input` and/or `fuel` (`{item, count?}`) in (input first, then fuel; both checked before anything moves); `take` takes the `output` (unless `output: false`) and, when `true`, the leftover `input` and `fuel` | `ok`, `missing` (no block there), `unreachable` (`reason` `too-far`, `distance`), `cannot` (`reason` `not-a-furnace`), `no-item` (`slot`, `item`: not carried), `busy` (`slot`, `holds`: the slot holds another item), `rejected` (`slot`, `item`, `reason` `not-accepted` or `slot-full`: nothing moved), `full` (take: no room, `taken` may be partial) | 5 s | closes the window |
| `enchant(token, a)` | `{pos, op, item, choice?, levelCost?}`; one short visit to an enchanting table: open, put `item` in (a plain copy: one with no enchantments), then `op` `offers` reads the three offers and gives the item back, `enchant` also puts the lapis in, enchants with offer `choice` (0, 1 or 2; costs `choice + 1` lapis and as many levels) and closes; never walks, decides nothing. `levelCost`, when given, must equal the chosen offer's level cost as read earlier (offers move after every enchant) | `ok` (`offers`), `enchanted`, `missing`, `unreachable` (`reason` `too-far`), `cannot` (`reason` `not-a-table`, `already-enchanted` (every copy carried has enchantments, nothing opened), `not-enchantable` (no offer for the item), `no-such-offer` (that slot offers nothing, `offers`: the three level costs), `offer-changed` (`offers`)), `no-item` (`item`), `no-lapis` (`have`, `need`), `no-levels` (`need`, `have`), `full` (the item did not come back and no pocket is free), `failed` (`reason`: `window-did-not-open`, `offers-did-not-arrive`, `enchant-stalled`, `not-confirmed`, `item-not-returned`, or the call's error; for an enchant `lapisSpent` and `levelsSpent` are measured, a failed call may still have taken the price) | 15 s | closes the window |
| `chat(token, a)` | `{message, to?}`; the message arrives validated and cleaned by `engine.chat/validate`, which `gate!` and `direct!` both run (control characters, newlines too, become spaces, `§` is dropped, trimmed; then `bad-name` unless `to` is a player name of 3-16 letters, digits, `_`; `empty`; `command` when it starts with `/`; `too-long` over 256 characters, or 256 less `/tell <to> ` for a whisper; each is `cannot`, nothing sent; `engine.chat/say!` splits). The primitive does no cleaning or rules of its own, only a last-line assertion: a message that is not a string, starts with `/` or holds a control character fails (`failed` with a "refusing" reason; `chatDirect` throws); with `to` it whispers. No limiter here: the engine's `act!` enforces at least 1 s between lines and at most 5 lines in any 30 s (per body, across jobs) | `sent` (`parts`), `gone` (`to` is not online), `failed` (`reason` = the server's refusal line), `cannot` (`bad-name`, `empty`, `command`, `too-long`: from `engine.chat/validate`, nothing sent; `too-long` also from the limiter: more than 5 lines), `blocked` (`rate`, `retryMs`: given by `act!`, not the primitive: the line does not fit the 30 s window now, nothing sent) | 3 s | none needed |
| `eat(token, a)` | `{item?}`; without `item`, the best food carried | `ate`, `no-food`, `full` | 5 s | `deactivateItem` |
| `attack(token, a)` | `{id}` | `hit`, `killed`, `gone`, `out-of-reach` | 1 s (one swing) | none needed |
| `interact(token, a)` | `{id, item?}`; uses the item (empty hand when omitted or null) on an entity | `used`, `no-effect`, `gone`, `out-of-reach`, `no-item`, `full` (empty hand asked, no free slot: nothing done), `cannot` (`reason`: `mounts`, `opens-window`; refused kind), `failed` (`reason`: `mounted`, `opened-window`; the guard dismounted / closed the window) | 2 s | closes the window, dismounts |
| `trade(token, a)` | `{villager, op, offer?, times?}`; `villager` is the entity uuid; `op` `offers` reads the window, `buy` buys `times` (default 1) of the 0-based `offer` | `ok` (`offers`), `bought` (`times`, `requested`, `gained`, `paid`, `stopped`: null, `sold-out`, `payment` or `room`), `gone`, `out-of-reach` (`too-far`), `no-item` (`short`), `full`, `cannot` (`reason`: `not-villager`, `no-offers` with `why` `unemployed`, `nitwit`, `baby` or `none`; `no-such-offer`; `sold-out`), `failed` (`window-did-not-open`, `not-confirmed`, `trade-stalled`); a buy whose trade call stalls (mineflayer waits for a result slot refill that an adjusted price never sends) is cut after 4 s, the window closed and the inventory measured: `bought` carries `stalled: true` (and `stopped: 'stalled'` when fewer than requested), nothing changed gives `trade-stalled` | 8 s | closes the window |
| `unequip(token, a)` | `{}`; empties the main hand (selects an empty hotbar slot, else moves the stack into a free slot) | `ok` (`item`), `empty` (nothing held), `full` (no free slot: nothing done, never tossed; mineflayer's unequip would toss) | 2 s | none |
| `sleep(token, a)` | `{pos}` (a bed) | `sleeping`, `not-night`, `occupied`, `monsters-near`, `missing`, `unreachable` | 5 s | wake if asleep |
| `look(token, a)` | `{pos}` or `{yaw, pitch}`; resolves after the next physics tick (the rotation has been sent to the server), at most ~50 ms later, bounded by 100 ms; | `ok` | 1 s | none needed |
| `wait(token, a)` | `{ms}`, clamped to 0..10000 | `ok` | `ms` (scaled by `timeScale`) | none needed; a cut rejects at once |
| `swim(token, a)` | `{ms = 3000, toward?}`, at most 10000; `toward` is `{x, y, z}` | `surfaced` (head out of water), `landed` (with `toward`), `timeout` | `ms`, at most 10 s | jump and forward released |
| `mount(token, a)` | `{id}`; gets the body on a boat, raft, minecart or rideable mob (`js/vehicle.mjs`): empties the hand (a held lead or food would leash or feed), looks at it, uses it, waits (wall time: no physics tick runs aboard) for the server to list the body as a passenger | `mounted` (`vehicle {id, uuid, name}`), `already-mounted` (`vehicle`), `gone`, `not-mountable` (not a boat, raft, minecart or rideable mob by name), `occupied` (every seat taken: boats and rafts 2, chest boats 1, camels 2, happy ghasts 4, others 1; a mob in a boat's front seat does not count, vanilla puts a boarding player first), `out-of-reach` (more than 3 from the eye), `hand-full`, `timeout` (not seated within 1 s: an unsaddled pig, a refusal) | 2 s | none needed |
| `dismount(token, a)` | `{yaw?, pitch?}` in Minecraft degrees (0 south, 90 west); a yaw is written as a raw `look` packet first (mounted Mineflayer sends none; the server picks the exit from it), then sneak (26.1 dismounts on sneak; Mineflayer's `dismount()` sends jump) until the body is off the tracked list and a server position arrived | `dismounted` (`pos`, where the server put the body; the landing is the caller's to judge), `not-mounted`, `timeout` (`mounted` true or false) | 2 s | sneak released |
| `useOn(token, a)` | `{pos, item?, face = 'up'}`; `face` is `up`, `down`, `north`, `south`, `east`, `west`; without `item` it uses an empty hand (an empty hotbar slot, else the held stack moved to a free slot; never tossed) | `used` (the block's name or properties, or the carried count of `item`, changed within 1 s), `unchanged`, `missing` (air or not loaded), `no-item`, `no-room` (empty hand asked, inventory full), `unreachable` (more than 4.5 from the eye; `reason: 'too-far'`, `distance`), `cannot` (`reason`: `bed` for beds and respawn anchors, `container` for blocks that open a window, `hazard` for flint_and_steel, fire_charge and lava_bucket, `use-place` for a block item on anything but a composter, `window` when a window opened anyway: it is closed) | 5 s | none needed |
| `offline(token, a)` | `{ms = 300000}`, at most 600000 | `ok` (`ms` is the wait used), `cut`, `closed`, `unsupported` | `ms` plus the reconnect | see below |

Acting while asleep first leaves the bed (`leave_bed` sent by name, since mineflayer's `wake()` sends a wrong id on this protocol), bounded at 1 s (scaled by `timeScale`); a cut during `sleep` leaves the bed too.

Extra fields on the result:

- `moveTo`: `pos` (where the body ended), `distance` (to the target), `reason` (one of `'noPath'`, `'planTimeout'`, `'stalled'`, `'timeout'`, on `partial` or `blocked`).
- `swim`: `oxygen` (`{before, after}`, the air level when the call started and ended).
- `dig`: `block` (name dug), `drops` (`[{id, name, count, pos}]`, the item
  entities that appeared within 2 blocks during up to 1 s after the break).
- `place`: `block` (the item placed) and, for a block item, `placed` (`{name, properties}`, the cell read back after the place: a torch may come back `wall_torch`, a stair with its `facing`/`half`). `click` (`{against, cursor, yaw?, pitch?, sneak?}`, chosen by `engine.placement`) names the neighbour to click (`against`, a cell beside `pos`, else bad-args; air or a fluid there is `no-support`), the point on it (`cursor`, 0..1 on the against block), the look held while clicking (`yaw`/`pitch` in mineflayer radians, yaw 0 north, pi/2 west, pitch -pi/2 down; a missing one keeps the current value; neither: look at the cursor point) and whether to sneak (clicking a usable block, placing a chest that must not join). Without `click` the reference is picked as before. A cell holding fire, soul fire, grass (`short_grass`, `tall_grass`, `grass`) or a snow layer counts as free,
  because the game replaces those; any other block is `occupied` (water and lava are replaced for block items as before). Buckets (`bucket`, `water_bucket`, `lava_bucket`) are used, not placed: `pos` is
  the cell that receives the liquid (it must be air or a replaceable block, else `occupied`, and have a solid
  neighbour, else `no-support`),
  or for an empty `bucket` the liquid cell to scoop (`missing` when it holds none). The body equips the bucket, looks
  at the supporting block (or the liquid), calls `activateItem`, and waits up to 2 s for the cell to change or the inventory to show the filled (scoop) or emptied (pour) bucket, whichever comes first (the block update can arrive late):
  `placed` (`block` is `water`, `lava` or `bucket`), else `{status: 'failed', reason: 'unchanged'}`.
- `collect`: `gained` (`[{name, count}]`, the inventory diff; empty on `gone`, absent when the entity was already gone or not an item at the start) and, on `unreachable`, `reason`.
- `inspectContainer`: `items` (`[{name, count, slot}]`).
- `transfer`: `moved` (count).
- `furnace`: `kind` (the block), `input`, `fuel`, `output` (`{name, count}` or null), `lit`, `burn` (`{left, total}` ticks of the burning fuel) and `cook` (`{done, total}` ticks of the item in the fire), both null only when the server sent no total; `load` adds `moved` (`{input, fuel}`, measured from the carried items in the window: one fuel item that burns at once never shows in the slot), `take` adds `taken` (`[{part, name, count}]`). Measured on 26.1 (`engine/js/furnace.mjs`): the input slot accepts any item (a smoker takes iron and never cooks it; callers must know what a kind smelts), the fuel slot only fuel; the bars arrive as window properties (totals once, at the open, before mineflayer's own listener exists; running values every tick while they change, so a visit settles before it reads them); `bot.inventory` is not updated while a window is open, so carried counts come from the window; a blast furnace and a smoker cook in 100 ticks and burn fuel twice as fast, so an item costs the same fuel everywhere; `lit` stays true after the last item while the fuel burns on.
- `enchant`: `offers` adds `item`, `xpLevel`, `lapis` (carried) and `offers` (`[{index, levelCost, lapisCost, hint}]`; `levelCost` 0 means no offer, `hint` is `{enchant, level}` when the server hints which enchantment the offer holds, else null); `enchanted` adds `item`, `choice`, `enchants` (`[{name, level}]` of the new copy), `lapisSpent`, `levelsSpent`, `xpLevel`, and `stalled: true` when the call hung but the measurement shows the item enchanted. Measured from what is carried and the body's level after the window closed and the inventory went quiet, never from the call returning; a stalled `win.enchant` is cut after 4 s. A book comes out as `enchanted_book`. The offers only appear while an item lies in the table, and the server returns what lies in the table when the window closes. Slot numbers are the window's, not the inventory's (`engine/js/enchant.mjs`).
- `toss`: `count` (thrown; 0 on `no-item`).
- `craft`: `made`, `used` (`{name: n}` consumed), `recipes` (every candidate recipe, `[{name: n}]`) and `have` (`{name: n}` carried) on `no-item` and on a `partial` with reason `no-item`. `engine.craft` chooses from them: `short` (`{name: n}` still missing, from the recipe closest to done; ties go to the more common base item) and `alternatives` (`{name: [cousin names]}`, only when other recipes use different ingredients in its place, such as `{cobblestone: ['cobbled_deepslate', 'blackstone']}`), which `jobs.items.craft` returns.
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
`hurt` (`health`, `food`, `cause?`: as for `died`), `died` (`pos`, `inventory`, `experience`, `cause?`: `lava`, `fire` or `void` when the body stood in it at that moment, else absent), `respawned` (`pos`, `dimension`), `chat`
(`from`, `message`) (lines from the body itself are not reported), `picked-up` (`item`, `count`: the body picked up an item entity), `woke`, `player-joined` and `player-left` (`player`: the username of another player whose join or leave reaches the body, from Mineflayer `playerJoined`/`playerLeft`; the body's own are not reported; entries of those kinds in body memory), `spawned`, `disconnected` (`reason`), `error` (`reason`), `reconnect-failed` (`reason`), `world-not-loaded` (`ms`, warn level: the column under the body did not load within 10 s after spawn or a reconnect; the body is used anyway), `physics-stalled` (`pos`, `ms`, warn level: no physics tick for 2 s because the column under the body is not loaded; the walk goal is cleared and controls released), `offline` (`ms`), `online` (`pos`). `died` is
emitted at the moment health reaches 0: `pos` is where the body died and `inventory` (`[{name, count, slot}]`) what
it carried. An instant death (`/kill`, void, damage) has the server clear the slots before the event is read, so when the live inventory is empty `inventory` is the last snapshot of the living body (taken on each health event above 0 and about once a second of physics ticks); `experience` is `{level, points}` at death. mineflayer emits `death` from
the health packet and only overwrites `bot.experience` on a later `experience` packet, so the values are the pre-death
ones. `respawned` is emitted at the first `spawned` after the library's respawn
signal, so `pos` is the new position (and `dimension` the new dimension; a portal also counts as a respawn). A death drops every job: each listed job (queued, held, cut or running) is cancelled with `:by :death` and each reflex job ends `:dropped`; register entries stay, so a trigger that still holds after the respawn (`died` starts recover-drops) runs its job again. `offline`
and `online` are the two ends of the `offline` primitive; after `online` the `bot` underneath is a new one. The engine
turns each into an entry of that kind in body memory (see Memory). `picked-up` is emitted at level `debug` and its `:picked-up {:item :count}`
entries are what make-room reads as "newer" (a stack picked up recently is tossed last).

### Lifecycle

`close()` disconnects (the fake does nothing).

### The fake

`(engine.fake/create spec)` in `test/engine/fake.cljs` (spec as a cljs map) returns the primitives plus a `world`
handle for tests; `world.state` is an atom of cljs data (shape in the namespace docstring; `fake/state`, `set-block!`, `add-entity!`, `entities` and `swap-self!` read and change it). It has the same sensing fields as above (`spec.self` may set `oxygen`, `effects` (default `[]`), `onFire`, `inWater`,
`inLava`, `isSleeping`, `foodSaturation`, `experience`, `dimension`; player entities default to `sleeping: false` and
`username` equal to `name`). Its `offline` flips `world.state.offline` (what `isOffline()` reads), emits `offline` and `online`, and waits
`ms * spec.offlineScale` (default 0.001, so 5 minutes is 0.3 s) before resolving the same results; a cut ends the wait early. The handle:

```js
const p = fake.create({
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
p.world.state            // atom of the world (self, time, blocks, entities, inventory, containers)
p.world.calls            // [{ name, token, args }] for every acting call, in order
p.world.hold('moveTo')   // the next moveTo call waits; returns release(result?)
p.world.override('dig', async (token, args, defaultImpl) => ({ status: 'cannot' }))
p.world.emit({ kind: 'hurt', health: 6 })   // delivered to onBodyEvent listeners
p.world.setTime(13000)
p.world.die()            // emits died (pos, inventory, experience), drops the inventory as items, zeroes experience
```

Fake semantics: `place` of a block item sets the state the game would (`engine.fake.placing`: stairs, slabs, logs and pillars, gates, doors with their upper half, beds with their head, chests and furnaces facing the placer, trapdoors, floor and wall torches, ladders; any other block gets no state) from the `click`, or without one from the first non-empty neighbour (below first) looked at from the eye; a click on air or a fluid is `no-support`; a door or bed without room (or a door without a floor) is `failed` with the item kept. `jumpPlace` raises the body one block per placement and consumes the item, with the same stop reasons as the real one (`no-item`, `no-support`, `no-headroom`); `swim` lifts the body to the top water cell of its column and refills oxygen to 20. `moveTo` jumps to the target if within `maxDistance`, else
moves `maxDistance` toward it and returns `partial`. `dig` removes the block
and adds an item entity at its cell. `collect` moves the item entity into the
inventory and emits one `picked-up` per gained item. `place` of `wheat_seeds`, `carrot`, `potato` or `beetroot_seeds` needs `farmland` below (else `failed`, nothing consumed), consumes the item and sets the crop block at age 0, resolving `{status: 'placed', block: item}`. A `drops` value may be an array of item names, one item entity each. `toss` takes the items from the inventory and adds one item entity 3 blocks along +x of the body. `craft` (`out-of-reach` with `table` when a table is within 32 blocks, else `no-table`) uses a built-in recipe table (bread, oak_planks, stick, crafting_table, torch, wooden_pickaxe, stone_pickaxe; extend with `spec.recipes` `{name: {count, needs, table?}}`) and has the real statuses; an unknown item is `no-recipe`. `chat` appends `{message, to}` to `world.state.chat` and resolves `sent` (`gone` when `to` names no player entity, nothing recorded). The failure statuses of `craft` and `chat` were chosen from the backoff failure list, so backoff needs no change. `interact` (`engine.fake.animals`): breeding food on a ready adult consumes 1 and sets `inLove`; `inLove` or `cooldown` gives no-effect; a baby consumes without love; shears on an unsheared sheep give sheared plus a white_wool item, worn 1; lead and empty-hand unleash work; an entity spec may set `accepts: false`, `mounts` (failed `mounted`) or `opens` (failed `opened-window`); refused kinds are `cannot`. Spec fields `raining`/`thundering` set the weather, `world.setRaining(on, thunder)` changes it. `trade` (`engine.fake.trade`): a `villager` entity spec may set `profession` (default `unemployed`), `level` (default 1), `baby`, `offers` (`[{cost: [{item, count}], gives: {item, count}, maxUses: 12, uses: 0}]`, `uses` kept up to date after a buy) and `busy` (the window never opens); `entities` never reports `offers` or `busy`. `unequip` (`engine.fake.unequip`): `empty` when nothing is held, `full` at 36 stacks, else clears the hand and resolves `ok` with `item`. `attack` takes 5 health per swing and reports `hurt: true`; an entity with `invulnerable: true` takes none (`hit`, health unchanged, `hurt: false`). `useOn` (`engine.fake.use-on`): a hoe tills dirt, grass_block or dirt_path into farmland (not from below, air above); bone meal adds 2 to a crop's age up to ripe (consumed; ripe is `unchanged`) and is consumed on a sapling or grass_block; a compostable item raises a composter's `level` by 1 every time (7 jumps to 8), and at 8 any hand empties it to 0 and drops a `bone_meal` item entity above it; a door, trapdoor or fence gate (not iron) flips `open` and a lever flips `powered` with an empty hand, a button sets `powered` once (it never falls back), a state `locked: true` swallows the click (`unchanged`; a protected area); anything else is `unchanged`. `sleep` succeeds at night on a
cell whose block name ends in `_bed`, and sets the time to 0. `eat` raises
`food` by 5 and consumes one item. A held call rejects with `cut` when the
owner changes, exactly as the real layer must. Where the body stands decides
`inLava` and `inWater` after every `moveTo` and `place`, and water puts out
fire. Placing a `water_bucket` pours water into the cell and leaves an empty
`bucket`. An entity killed by `attack` spawns its `drops` (`[{name, count}]`)
as items. A `creeper` entity carries `creeper: true` unless the spec says
otherwise. `dig` clears a cell's crop age. `furnace` (`engine.fake.furnace`): furnace, blast furnace and smoker with the real rates (200 ticks an item, 100 for the other two), fuel burn times, one stack per slot, the `lit` block property; nothing cooks until `world.advance(ticks)`; a spec may start a furnace with stacks: `furnaces: {"x,y,z": {input, fuel, output}}` (the cell must be a furnace block). The input slot takes anything, as on the server; what the kind cannot smelt never cooks. `enchant` (`engine.fake.enchant`): a table's offers come from its bookshelves (none: levels 2, 3, 5; fifteen: 10, 20, 30) unless the spec gives them: `enchantTables: {"x,y,z": {shelves, offers: [l, l, l], hints: [[name, level] | null, ...], busy}}`; the body's levels are `self.experience.level`; an enchanted item in `inventory` is `{name, count, enchants: [{name, level}]}`; an enchanted item gets sharpness, efficiency, protection or power by kind and, from level cost 15, unbreaking.

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
the registry, a missing or extra argument, args that are not a map, an args key the job does not declare (`jobs.movement.go-to has no arg :target; its args are :doors, :pos, :range`; a job with no args takes none), or a
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
| `(ctx/emit! ctx kind level fields)` | an event with `:source :job`; the level is not stored, but `:warn` or `:error` without an `:attention` in `fields` stores `:attention :notice`, so a give-up shows in `observe --wait` and the dashboard notice list. An explicit `:attention` in `fields` always wins; `:info`, `:debug` and no level carry none. Use `:warn` only for a give-up or decline, never per cell or per tick |
| `(ctx/plan ctx id)` | the body's world's plan `id`, from memory: nil (no such file), `{:id :broken text}` (never readable), else `{:id :plan :cells :errors}` (`:cells` `[{:pos [x y z] :want :part}]` from `plan.shape/expand`), with `:error` while the file is bad and this is its last good copy |
| `(ctx/warn-once! ctx key kind fields)` | a warn event, only the first time this job gives `key` in this body process; checks may call it |

**World knowledge.** `engine.world` reads the plans of the body's world (`worlds/<world>/plans/<id>.edn`,
the world named in the agent's `config.json`) and the blueprints (`blueprints/<id>.edn` at the repo root), parses them
with `plan.parse` and expands them with `plan.shape` (both under `src/plan/`, shared with the dashboard). Bodies only
read. The files are stat-ed again at most every 3 s, by the first read after that, and only changed files are parsed
again. A file that turns unreadable or invalid keeps its last good copy and is reported once (`:system`
`world.plan-unreadable` / `world.blueprint-unreadable`, `:plan` or `:blueprint`, `:error`, `:kept`).

**Notes.** What bodies saw, written by the bodies into the world's shared knowledge (`engine.notes`; plans stay
read-only). Each body writes only its own file, `worlds/<world>/notes/<body>.edn` (`notes/paths`, the one place
that says so), `{:body :notes [note]}`, whole, to a temp file then renamed. A note is `{:kind :what :pos [x y z] :by :t
:until}` (`:t`, `:until` wall-clock ms): `{:kind :seen :what "oak_log"}` a block seen there, with `:id` (uuid) for an
entity, `{:kind :searched :what [names] :r n}` ground looked over for those names within `r` (XZ). One item per
`[kind what (or id pos)]`, the newest `:t` kept (tie: the lowest `:by`). Reads drop notes past `:until`; a body's file
keeps at most 2000, the oldest dropped. Readers merge every body's file like plans: the folder is stat-ed at most every
3 s, changed files re-read, a broken file keeps its last good copy with one `:system` `world.notes-unreadable` warn
(`:body :error :kept`); the body's own notes are held in memory, so its writes show at once (a broken own file at start:
one warn, start empty, moved to `<body>.edn.broken` at the first write; a failed write: one `world.notes-unwritable`
warn per error, kept in memory, retried by the next write). Jobs: `(notes/notes c)` the merged live notes,
`(notes/note! c [{:kind :what :pos :ttl-ms ...}])` stamps `:by :t :until` and writes. The store hangs off the world
(`(:notes world)`); none means `[]` and no writes.

**engine.chat.** Limits as data, enforced in `act!` for every `:chat` act (`gate!`, which also validates and cleans the message with `validate`, as `direct!` does); `say!` splits and spaces lines.

**engine.craft.** What a craft that ran out of ingredients reports: from the candidate recipes and the counts carried the `craft` primitive hands back, `shortfall` (the recipe closest to done; ties go to the more common base item), `alternatives` and `no-item` (both together).

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
hands over `{:arrived true}` or `{:arrived false :reason :unreachable :why? :kind?}`;
`jobs.forestry.collect-drops` hands over `{:collected n}`.

Children can call children, recursion included, with no depth cap. A cut anywhere ends the whole chain's round. Cancel and done take the
subtree, since it lives inside the parent's memory. `submit!` is delegation:
a peer on the list, not a child.

## Memory

One EDN store per body, `memory.edn` under `worlds/<world>/agents/<name>/engine/`,
read and written with `cljs.reader` and `pr-str`, so keywords survive. It is
`{:entries {kind [entry]} :policies {kind policy}}`:

- An **entry** is `{:t wall-clock-ms :wt world-time :data ...}`, newest
  last. `:wt` is the body's `timeOfDay` when written. No provenance field.
- **Kinds** are an open vocabulary keyed by what the observation is about:
  `:hurt`, `:died`, `:chat`, `:restart`, `:bed`, `:chest`, `:looked`,
  `:moved`, `:picked-up`, `:forestry/replant`, `:job/j7`. The survival jobs write `:breathe`,
  `:extinguish`, `:hazard`, `:hostile`, `:fed`, `:hungry`, `:slept`,
  `:shelter`, `:log-out`, `:stuck`, `:recovered` and `:chest-unusable` (make-room: a chest that failed it) and read `:home` and
  `:food-source`, which an agent sets with `jobs.memory.set-place` (see Named places below). `jobs.access.pillar` writes `:scaffold`, the scaffold ledger (`engine.access.ledger`). `jobs.access.cleanup` drops its entries and writes `:scaffold-held`.
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
- **Named places** (`engine.places`): a place is a kind named after it (`:bed`, `:chest`,
  `:home`, `:food-source`, any name of 1 to 32 lowercase letters, digits and dashes) with one
  entry `{:pos {:x :y :z}}` under `place-policy`; a retracted one is `{:gone true :was pos}`.
  Nothing else has to be hand-edited: `jobs.memory.set-place` records, moves or (with
  `:block`) verifies one, `jobs.memory.forget-place` removes one, both one round, both
  submittable over the control route (`POST /jobs` `{:op :submit :front? true :spec
  (jobs.memory.set-place {:name :home :pos [x y z]})}`); the outcome is in the event stream
  (`place.set`, `place.forgotten`, or a `place.refused` warn with `:reason`), not in the route's
  reply. Names of kinds the engine uses (`:hurt :slept :moved ...`, `engine.places/reserved`)
  and kinds that hold other memory are refused. Self-recording: `jobs.survival.sleep` with a
  `:bed` arg and `jobs.storage.deposit` with a `:chest` arg record the bed slept in / chest
  deposited into when none is recorded or the recorded one is gone, never over a different
  live one (one `place.kept` info event says so); a deposit or withdraw that finds the recorded
  chest missing retracts it (warn `chest_missing`), as sleep does a bed (`bed_missing`).
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
 :when        (fn [world view args plans] bool) ; view is a memory view; args are the entry's; plans is the engine.world
                                              ; (answers from memory, `engine.world/derived` for a cached index), may be ignored
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

### Conditions

An ad hoc trigger's `:when` can be a condition: an EDN list in the look of a
job expression, read and walked by `engine.condition` against a fixed table,
never evaluated. Not wired into the register or any command yet.

```clojure
(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
(held-for 5 (hostile-near 10))
(not (known? (place :home)))
```

- Operators: `and or not` (booleans), `< > <= >=` (two numbers), `=` (two
  numbers, strings, keywords or booleans), `(held-for seconds cond)`: true once
  cond has been definitely true for that many seconds (a literal), and
  `(known? x)`: x is a fact or any call; it is always a definite boolean, true
  when x's value is known and false when it is unknown, and the only form that
  turns unknown into a definite value. Over a boolean it is true exactly when
  that boolean is definite (`(known? (and a ?))` is true only if a is false),
  and over `held-for` it follows the inner condition (a held-for is unknown
  exactly while its condition is). Its sub-form is evaluated every tick like
  every other, so a `held-for` inside still times.
- Facts (`engine.condition.facts/table`, each with argument types, result
  type and cost): `(health) (food) (inventory "item") (free-slots)
  (distance-to pos) (blocks-near "name" r)` are numbers, `(place :kind)` a
  position from memory, `(daytime) (in-water) (hostile-near r) (burning)
  (suffocating) (night-unsafe) (stuck) (wearing "item")` booleans (`wearing`: the
  item is in an armour slot or the off-hand, from `self().equipment`). Cost `:cheap` is read every
  tick; `:scan` (`blocks-near`) is cached per instance for `:refresh-s` (5 s).
- `(since :kind)` is a number: the seconds (wall clock) since body memory last
  recorded an unexpired entry of that kind, unknown when there is none, so
  `(not (known? (since :slept)))` says "never" and `(or (not (known? (since
  :slept))) (> (since :slept) 3600))` says "never, or more than an hour ago".
  The kind is a keyword literal and is open: a kind nothing writes is simply
  unknown. It reads the latest entry of that one kind (at most its `:cap`
  entries are looked at) with the clock and entry `:every-interval` uses, and
  only sees entries within their policy's ttl, so a kind meant to answer "more
  than a day ago" must be written with a ttl longer than that (`:slept` keeps
  seven in-game days). Entries carry `:wt`, the time of day (0 to 24000, which
  wraps), not the world's age, so there is no days-since fact. A bare timer
  ("every N seconds" with nothing recorded) is deliberately not in the
  language: periodic work is a job that declines in its check until its own
  clock says so, and rate limiting is `:cooldown`.
- No variables, arithmetic, `let`, `fn` or functions of one's own: what the
  vocabulary cannot say becomes a new fact, written in ClojureScript with a test.
- A fact can be unknown (body offline, no such place). Unknown propagates
  through comparisons and `not`; `and`/`or` are three-valued (`(and false ?)`
  is false, `(or true ?)` true). A condition holds only when definitely true.
  `known?` is how to ask about absence: "no home yet" is `(not (known? (place
  :home)))` (without it `(not (place :home))` is unknown and never fires),
  and `(and (known? (place :home)) (< (distance-to (place :home)) 16))` says
  the same near-home test with the absent case spelled out. For a clock
  there is `since`: "every N minutes" is `(or (not (known? (since :kind)))
  (>= (since :kind) 60N))` for a kind the job writes.
  A job sequence writes its own kind with `jobs.memory.remember`: `(seq (jobs.animals.breed {...}) (jobs.memory.remember
  {:kind :bred-cows}))` plus a trigger on `(or (not (known? (since :bred-cows))) (> (since :bred-cows) 1200))` breeds again
  20 minutes after the last time. The entry lasts an hour unless `:ttl-s` says longer; once it expires the condition is unknown.
  `jobs.animals.breed` writes such a kind itself, `:bred/<mob>` (e.g. `:bred/cow`), when a run fed at least two animals, so
  `(since :bred/cow)` needs no `remember`; a run that fed nobody leaves nothing.
- `compile` validates once, at registration: an unknown symbol, wrong arity,
  wrong argument type (`known?` also refuses a literal), incomparable `=`, a bad held-for duration, a leading
  quote or a non-boolean top is refused as data `{:ok false :reason :at
  :message :allowed}`, `:at` the offending sub-form and `:allowed` the
  signatures that would do.
- `(when-fn node)` is the register's `:when` with its own held-for timers and
  scan cache (one per registered instance, lost on restart); `(explain node
  env state)` lists every sub-term with its value now.
- Built-ins it can say: health-low `(< (health) 7)`, inventory-nearly-full
  `(<= (free-slots) 2)`, hungry `(or (< (food) 6) (and (< (health) 20) (< (food)
  14)))`, and burning, suffocating, night-unsafe and stuck as one fact each.

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
running listed job and submits first in the list with `:hold? true`.
`submit!` opts: `:hold?`, `:backoff` (a config map or `false`, over any
`(backoff cfg e)` wrapper), `:front?`, `:by`. `:front?` lists the job directly
after the current job, so it gets the very next round; round-robin then goes on
in list order (`[A B C D]`, C current, front X: `[A B C X D]`, rounds X, D, A,
B, C, X...). There is no front other than the current job. Details: the
current job is the one running its round, or the cut one waiting to resume,
or, between rounds, the one that ran last; with none (nothing ran yet, or it
is gone) the job goes where the next scan starts. Two front submits in one
round each go directly after the current job, so the later one runs first.
A front submit never cuts the running round, and a held job keeps the body
until it ends (the front job waits directly behind it). A front job whose
check declines is skipped like any other. Every submit makes a new job (there
is no re-submit of an id), so nothing is moved or deduplicated. The order and
the scan position are in `engine.edn`; after a restart the in-flight job
resumes first, then the front job after it.

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
  reset: `look`, `wait`, `equip` and `steer` (a job that looks and then gets a blocked
  `moveTo` each round still backs off), and a `moveTo` with a failure status
  (`timeout`, `blocked`...) that moved the body at least 1 block (straight
  line, from where the act started to where it ended). A walk round of
  `engine.path.near` (walk-near!, go-to) is booked as one `:walk` act with
  its `:moved` status (`ctx/note-walk!`; its steers are neutral): `arrived`
  is progress; `partial` (ended nearer) is neutral, and so is a walk that got
  no nearer but moved the body at least 8 blocks (a long way round); one that
  moved less, or found no path, is a failure, so a blocked walk backs off. A
  round of only neutral acts is not fruitless and does not reset.
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
`worlds/<world>/agents/<name>/engine/events.edn`. The canonical contract is in
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

The engine exposes HTTP over `worlds/<world>/agents/<name>/engine/events.sock`, a local
Unix socket with mode 0600. It is separate from manual driving's `control.sock`.
All responses are EDN, including errors; mutation bodies must be EDN too.

| request | behavior |
|---|---|
| `GET /snapshot` | coherent engine state, outstanding requests, body metadata and cursor |
| `GET /events?stream-id=<id>&after=<seq>&limit=<n>` | bounded event page after a cursor, with oldest/latest sequence and explicit gap indication |
| `POST /attention/resolve` | `{:request-id "..." :reason :handled}` resolves a request idempotently; it does not retry or cancel its job |
| `POST /chat` | one public line or whisper; uses the body's identity and shared chat limits without acquiring its scheduler lease |
| `GET /status?limit=<n>` | compact body/job/attention projection; `limit` is 1..32 (default 4). `:died` `{:pos :cause? :ago-ms :despawns-in-ms}` is present while the latest death is under five minutes old (the drops lie at `:pos`); `observe status` shows it as `:died {:at :cause :ago-s :pile-at :despawns-in-s}`. The died event does not carry a cause yet, so `:cause` shows only once a primitive records one |
| `GET /inventory` | read-only carried stack and worn equipment snapshot, independent of manual takeover |
| `GET /job?id=<id>&limit=<n>` | one listed or reflex job's bounded parsed spec, effective args, state and linked outstanding requests |
| `GET /catalog?kind=jobs&prefix=jobs.farm.&limit=20&offset=0` | bounded page of exact job names (names only) |
| `GET /catalog?kind=triggers&prefix=health&limit=20&offset=0` | bounded page of exact trigger names (names only) |
| `GET /catalog?kind=job&name=jobs.<namespace>.<name>` | one job's description and argument defaults |
| `GET /catalog?kind=trigger&name=<name>` | one trigger's default job, args, persistence and cooldown |
| `GET /triggers` | the register in its own order, each entry with its live `:muted`, `:moved`, `:cooling-until`, `:stopped?` and `:backing-off`, and `:order`, the ids in firing order now; `?id=<id>` adds `:explain` for that entry |
| `POST /triggers` | one register edit (below): 200 with the result, 409 with a refusal |

For example, read a snapshot without taking control of the body:

```sh
curl --unix-socket worlds/claude/agents/Bob/engine/events.sock http://localhost/snapshot
```

On connection/reconnection or a history gap, reconcile outstanding requests
from `/snapshot`, then read events after that snapshot's cursor. This is an
observational stream, not an event-sourced database. State and memory snapshots
remain authoritative. The ClojureScript dashboard uses this API and offers a
read-only historical fallback for offline engines and old logs.

For a compact terminal/agent read, `node engine/tools/observe.mjs <agent> --world <world>`
prints the status projection as EDN. It reads the same private event socket and
does not contact or disturb Mineflayer. Use `job <id>` or `catalog job|trigger
<name>` only when the summary needs detail; `--world` is required (a name is unique only within a world); `--state <dir>` selects another
state root and `--limit <n>` bounds the listed queue rows. `inventory` returns
aggregate carried counts and non-empty equipment slots in one read; `--slots`
adds each carried stack's slot. `equipment` returns only equipped gear. Both
modes are read-only and need no takeover lease; `--raw` returns the bounded
stack and equipment snapshot.

```sh
node engine/tools/observe.mjs Bob --world claude
node engine/tools/observe.mjs Bob --world claude --raw
node engine/tools/observe.mjs Bob --world claude inventory
node engine/tools/observe.mjs Bob --world claude inventory --slots
node engine/tools/observe.mjs Bob --world claude equipment --raw
node engine/tools/observe.mjs Bob --world claude job j17
node engine/tools/observe.mjs Bob --world claude catalog jobs jobs.farm. --limit 10
node engine/tools/observe.mjs Bob --world claude catalog job jobs.forestry.harvest-wood
node engine/tools/observe.mjs Bob --world claude catalog trigger hostile-near
```

List recently observed entities without asking Mineflayer to scan or changing
the body's lease:

```sh
node engine/tools/entities.mjs Bob --world claude
node engine/tools/entities.mjs Bob --world claude --type zombie --radius 32
node engine/tools/entities.mjs Bob --world claude --player Alex --limit 20
node engine/tools/entities.mjs Bob --world claude --center 100,64,-20 --dimension overworld --raw
```

The command reads the body's in-memory `/entities` cache. By default it centers
a 64-block, 3D radius on the newest unexpired self observation, uses that
observation's dimension, and returns the nearest 10 rows. Players and other
entity types are included; `--type` and exact `--player` filters narrow the
result. `--limit` accepts 1..50 and `--offset` accepts 0..10000; `:more?` and
`:next-offset` continue a bounded page. If the body has not observed its own
position, give `--center X,Y,Z`; a dimension override with a self-centered query
is refused so coordinates from one dimension are never reused in another.

Each observation expires two minutes after the server timestamp supplied by the
body. Rows still cached after disconnect are returned with `:online? false` on
the result and their original `:age-ms`; the command never refreshes their
timestamps. `--raw` returns the selected rows with their observed/expiry times
and cache metadata, still within the same page bound. The CLI does not write
files, start a body, or create a movement lease.

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
npm run body -- --agent Bob --world claude
```

That script compiles `out/body.cjs` from the current ClojureScript source
before starting the body. Do not start a second process for an agent that is
already running.

### Editing the register and the list

A running body takes one edit per request on the same socket; nothing needs a
restart. An edit applies when its request arrives: the handler and `tick!` share
one event loop, so it lands between two ticks, and no edit cuts a round in
flight except a cancel or an interrupt (by token rotation, as a reflex cuts).
Each applied edit emits its event. A refusal is data, `{:ok false :reason r :at
[key ...] :message text}`, and changes nothing. On `/triggers`, `:generation-id`
is optional (checked when given); `:by` names who asks (a short string or
keyword) and is kept on the entry.

| `:op` | request | applies |
|---|---|---|
| `:put` | `{:op :put :id :health-low-12 :trigger :health-low :args {:health 12}}`, or ad hoc `{:op :put :id :bread-low :when (< (inventory "bread") 8) :job (jobs.survival.eat)}`; also `:persistence :cooldown-s :backoff :ttl-s :by` | creates the entry, or replaces the one with that id in place (its mute or move kept; latch, cooldown, backoff and condition state start over); a built-in entry is refused |
| `:remove` | `{:op :remove :id :bread-low}` | `remove-reflex!` (built-ins refused); a round of its job in flight finishes, then the job is dropped (`reflex.ended` `:dropped`) |
| `:mute` | `{:op :mute :id :hostile-near :ttl-s 60}` | `mute!` |
| `:move` | `{:op :move :id :bread-low :above :hostile-near :ttl-s 60}` (or `:below`) | `move!` |
| `:clear` | `{:op :clear :id :bread-low :property :mute}` (or `:position`) | `clear-change!` |

An ad hoc entry gives `:when`, a condition (an EDN list, `engine.condition`),
instead of `:trigger`. It needs `:id` and `:job`, takes no `:args`, and defaults
to `:persistence :stop :cooldown-s 0`: it fires when the condition is definitely
true, and once its job has ended with the condition still true it waits until the
condition has been false. The condition is compiled at the request (a refusal
carries the language's own `:at`, `:message` and `:allowed` under `:condition`);
`engine.edn` keeps the form, boot compiles it again, and an entry that no longer
compiles is dropped with one `system.dropped` warn. Each entry has its own
compiled condition (its own `held-for` timers and scan cache, in memory only: a
restart starts them over). To see why a condition does not fire, `GET
/triggers?id=bread-low` adds `:explain {:id :bread-low :terms [{:form f :value
v :remaining-ms n} ...]}`, every sub-term with its value now, root first. An
active `held-for` term includes `:remaining-ms` (zero once elapsed); a false or
unknown inner condition has no active timer and omits it. The explanation reads
without advancing timers (`:engine.condition.facts/unknown` while offline).

`:ttl-s` on a put removes the entry after that long (`reflex.expired`); without
it the entry stays. An entry put over the socket whose job keeps failing while
it holds backs off like any reflex and also raises one required request
(`:reason :reflex-backoff`), resolved when the backoff ends or the entry goes.
Rounds that throw or decline do not count toward a backoff, so under
`:cooldown` or `:retry` a job failing that way fires again every cooldown. A scenario's register goes through the
same validation and is loaded as puts by `:scenario`; its ids must be unique.

`POST /jobs` (`tools/jobs.mjs`, below) also takes `:front?`, `:hold?`,
`:backoff` and `:by` on `:submit`, and `:by` on `:interrupt`; `:by` is kept on
the instance.

`POST /jobs {:op :cancel-all :request-id r :generation-id g :by who}` clears the whole list: every listed job (the
running one, queued, held (`:hold?`) and parked-failed ones) is cancelled through the same path as a single `:cancel`
(`core/cancel!`: the running round is cut at its cut point, the job's cleanup and memory are dropped, attention
resolved), in list order, one `job.cancelled` event each with `:by`. Reply `{:ok true :cancelled [id ...]}` (an empty
list is not a refusal). The register is not touched: reflex entries, and a reflex job in flight, go on, so a body
still eats and flees after a stop (mute or remove one with `POST /triggers`). It takes no `:id` (`:bad-field`) and
the same dedupe as the other ops: the same request id replays the result with `:duplicate true`, changed fields are
`:request-id-conflict`. A single `:cancel` also records `:by` now.

## Manual takeover

For rescuing a stuck body by hand. The body listens on a unix socket,
`worlds/<world>/agents/<name>/engine/control.sock` (mode 0600), created at start and removed at shutdown. If it
cannot listen (for example a path over 107 bytes) the body emits `system.control_unavailable` (error) and runs without it.

The body has three control modes. In normal scheduling, triggers are evaluated
and eligible jobs take round-robin turns; a listed job does not interrupt
another listed job. Reflexes can cut a listed round according to register
priority. An explicit `do-now!` request cuts a running listed round, queues the
new job at the front as a holder, and records the cut listed job for later
resumption. The do-it-now operation does not cut a running reflex. After the
urgent holder ends, the normal holder/check rules apply, so a prior hold or a
declining resume check can affect which listed job runs next.

Manual takeover is the third mode: `take` cuts the current holder and pauses
all trigger evaluation and job rounds until release or lease expiry. The
browser sends movement to the view server, which proxies `/drive/<name>` to
the body's engine control socket; `engine/js/control.mjs` calls the engine's
takeover adapter, and manual movement reaches Mineflayer through the
ownership-checked primitive layer. The browser never accesses Mineflayer
directly. This is an engine-owned control mode, not a holding job or a reflex
mute. The socket exposes `/drive` for direct movement (JSON) and `/world` for bounded
primitive actions (EDN); it does not edit the list or register. Who drives is decided by the lease, first come (`take` is refused
with `held-by <who>` otherwise).

Ops (`POST /drive`, body `{op, who, ...}`; `GET /drive` returns the state): `take` (`why`, optional `idleS`, a number 1..3600: this takeover's idle limit instead of the default), `set`, `stop`, `ping`,
`release` (`force` reclaims another driver's hold). A refusal is `{ok:false, reason}` with reason `offline`, `settling`,
`held-by <who>`, `not-taken`, `not-driver` or `bad-args`. `set` fields:

| field | what |
|---|---|
| `controls` | `{forward, back, left, right, jump, sneak, sprint}` booleans |
| `look` | `{yaw, pitch}` absolute or `{dyaw, dpitch}` relative, in Minecraft F3 degrees: yaw 0 south (+z), 90 west, 180 north, 270 east; pitch -90 up to 90 down |
| `ms` | 1..10000, hold the controls for that long, then release them |

Engine semantics: `take` cuts the holder. A cut listed job stays on the list
and is recorded for resumption; it runs when the scheduler selects it and its
check passes, subject to existing holding-job and round-robin rules. A cut
reflex job is dropped. While manual, the scheduler uses the same pause gate as
offline and settling: no trigger is evaluated, no `:stop` latch clears, and
reflex ends are deferred until the first ready tick. Pause bookkeeping is
transient; on that first ready tick backoff state is reset and deferred reflex
ends are judged. Manual ownership and lease state are not written to
`engine.edn`; restart ends the takeover, and going offline releases it too.

Dead-man: untimed controls are released after 1 s without any op from the driver (warn `system.drive_deadman`). This
clears held movement controls but keeps the takeover lease. The takeover itself ends after 15 s of silence by default
(`--drive-idle-s`, or `idleS` on `take`, from 1 to 3600 seconds), reason `idle`; `ping` refreshes the lease without
moving. Timed control holds last at most 10 s and end on their own. Every reply's `manual` (and `GET /drive`) carries
`idleMs` (the lease's idle limit), `expiresAt`
(epoch ms, last op plus `idleMs`) and `idleLeftS` (seconds left, one decimal). `GET` is read-only: it does not count as an
op and does not reset the silence clock. An agent driving step by step must keep its commands under the idle limit apart,
or take with a longer `--idle-s` (a `ping` keeps the lease alive without moving).
The timers run in the body, so a dead CLI, view server or browser tab cannot leave it walking.

Where the rules live: `engine.lease` holds them, pure; `engine.takeover` applies them to the engine and is ticked by the engine loop (also while paused); `engine/js/control.mjs` is a stateless socket adapter. There is one heartbeat clock: any op from the holder (a `ping` included) keeps the lease, and 15 s of silence ends it; `engine.lease/beat-ops` decides which ops count.

Events: `system.takeover_started` `{who why}`; `system.takeover_ended` `{who reason held-ms}` with reason `released`,
`forced`, `idle`, `offline` or `shutdown`; `system.drive_deadman`.

CLI (`--worlds <dir>` selects the worlds directory, default the repo's `worlds/`; `--state <dir>` retains the legacy parent layout; `--who` defaults to `claude`):

```
node engine/tools/drive.mjs ProbeDrive --world claude take --who claude --why "stuck in a pit"
node engine/tools/drive.mjs ProbeDrive --world claude look 270 0 --who claude        # face east
node engine/tools/drive.mjs ProbeDrive --world claude hold forward,jump 2000 --who claude
node engine/tools/drive.mjs ProbeDrive --world claude turn 90 --who claude
node engine/tools/drive.mjs ProbeDrive --world claude jump --who claude
node engine/tools/drive.mjs ProbeDrive --world claude stop --who claude
node engine/tools/drive.mjs ProbeDrive --world claude state
node engine/tools/drive.mjs ProbeDrive --world claude release --who claude           # --force reclaims another driver's hold
```

World actions keep the same exclusive lease and use the engine's existing owner-token primitives; they do not create a
second Mineflayer connection. `world.mjs` accepts `move-to`, `dig`, `place`, `use-on`, `interact`, and `inventory`. Mutating
actions have finite primitive deadlines (at most 10 s), require a lease with at least one extra second of idle time, and
run one at a time. Starting one clears held drive controls. While it runs, `/drive set` and `stop` are refused; cancel,
release, lease expiry, or engine shutdown cuts the action. This socket is local access control, not per-`who` authentication:
`who` is the lease label and the Unix socket's filesystem permissions govern access.

Submission returns promptly with an operation ID. Reuse `--request-id` if a response is lost; operation IDs are retained
in memory for the latest 32 actions and are not durable across restart. `status` can read a finished result after release;
`cancel` requires the current lease. Inventory is compact and capped at 40 stacks.

```
node engine/tools/drive.mjs ProbeDrive --world claude take --who claude --why "move to the gate" --idle-s 30
node engine/tools/world.mjs ProbeDrive --world claude submit move-to -5 64 -7 --who claude
node engine/tools/world.mjs ProbeDrive --world claude status <request-id> --who claude
node engine/tools/world.mjs ProbeDrive --world claude cancel <request-id> --who claude
node engine/tools/world.mjs ProbeDrive --world claude inventory --who claude
node engine/tools/drive.mjs ProbeDrive --world claude release --who claude
```

`move-to` accepts `--range`, `--max-distance` (up to 64 blocks), and `--timeout-s` (1..10; for a longer walk submit `jobs.movement.go-to` or chain calls). A move-to within `--max-distance` walks as go-to does (engine.path.near/walk-round!: it opens a shut door, gate or trapdoor, passes and shuts it again; a result that is not `arrived` carries a `:reason` such as `no-path`, `door-stuck at [cells]` or `timeout`); a farther one, or a body without path sensing, runs the pathfinder primitive `moveTo`, which treats every door as a wall. `dig`, `place`, `use-on`, and
`dig` holds the best carried tool for the block first, as jobs do (`engine.takeover/dig-with-tool!`, `engine.jobs.tools/harvest-need`). A block that needs a pickaxe tier no carried tool reaches (stone, cobblestone or any ore by hand or without a pickaxe; iron ore with a wooden pickaxe; gold, diamond, emerald, redstone ore with less than iron; obsidian with less than diamond) is not dug: the result is `{:status "no-tool" :block b :needed "stone_pickaxe" :reason ...}` (`needed` is `pickaxe` for any, else the minimum tier). Reported `drops` are items that appeared with this dig only (snapshot before, growth only).
`interact` use the existing primitive's reach, item and interaction checks. `interact` takes an entity ID, not a name.
`use-on` refuses beds, containers and its existing hazard list. The operation status describes what the primitive returned;
it does not schedule jobs or alter the engine's queue.

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

`npm run body -- --agent <name> --world <world> --scenario <file>` loads
`worlds/<world>/agents/<name>/config.json` (`username`; the world is where the folder is, never a config field),
the world's `worlds/<world>/world.json` (`host`, `port`), and `js/primitives.mjs`
(`engine.bodies` and `js/bodies.mjs` are the one place that builds a body folder path).
It refuses to start, with a message, when `--world` is missing or `js/primitives.mjs` does not exist.
If `worlds/<world>/agents/<name>/engine/engine.edn` exists the saved list and register
are restored and the scenario is ignored; pass `--fresh` to discard saved
engine state and start from the scenario (memory is kept; job kinds of the
discarded list are swept). The scenario is validated against the job
registry and the triggers before connecting. `--worlds <dir>` overrides the repo's `worlds/`; legacy `--state-dir <dir>` selects its parent. `--drive-idle-s <s>` (default 15) ends a manual takeover after that much silence from the driver.

`test/engine/scenarios_test.cljs` runs `woodcutter.edn`, `pace-cuts.edn`
and `survival.edn` end to end against the fake primitives. `survival.edn`
registers every survival trigger in the order of the Triggers table and
queues `(repeat (jobs.movement.look-around))`, so the body looks around
(once per `:every-ms`, default 2000) between reflexes; its test drops the health, then places a zombie, then kills
the body, and checks that recover, respond-to-hostile and recover-drops fire
in turn and that the body goes back to looking around.

## Zones and claims

Zones (`worlds/<world>/zones.edn`) and claims (`claims.edn`) are a social rule that jobs consult, never one the engine
enforces: `act!` and the primitives check neither, and a job may ignore them (the rules of the game allow it). The helper
is `engine.jobs.access` (`may?` for `:dig :place :sow :harvest :take :put`, `choose` and `trespass!` for survival jobs,
`container-refusal` for chests and furnaces) over the pure `engine.access.zones/verdict`. The verdict, first match wins:

1. no zone list read: `:no-zones`;
2. the cell is in another active plan's footprint (the plan the job builds is left out): `:footprint`;
3. a zone holds the cell, its `:owner` is not the body (compared ignoring case; `"unknown"` counts as foreign) and its
   `:allow` lacks the action: `:zone`. `:allow` is what OTHERS may do there (`:dig :place :harvest :take :put`); the
   owner may always act;
4. an active, unexpired claim of another owner holds the cell (a claim has no `:allow`): `:claim`;
5. else ok: `:own-zone`, `:own-claim` or `:open`.

Three decisions are named defaults at the top of `engine.access.zones`, one line each: `deposit-into-foreign-chest?`
(false: `:put` into another's chest is refused), `plan-footprint-beats-zone?` (true: a plan's own builder works over a
foreign zone), `unknown-owner-foreign?` (true).

Every job that digs, places or takes accepts `:ignore-zones? true` (default false: act regardless of zones and claims).
By default a job skips a target the verdict refuses, with one warn naming the zone, claim or plan; `withdraw`, `deposit`,
`smelt` and the jobs built on them give up `:refused {:zones :claims}` on a foreign container. Survival jobs (`breathe`,
`extinguish`, `dig-in`, `maintenance.unstick`, the crop dig of `get-food`) take a permitted option first and break
another's block only as a last resort, with one `<job>.trespass-last-resort` warn; they never take from a foreign
container (`get-food` skips such a chest). A missing zone list never blocks a survival job.

Tidying up (`engine.jobs.tidy`): a dig or place that breaks another's block (a survival last resort in `breathe`, `dig-in` and `maintenance.unstick`, or any act of `mine` and `clear-box` run with `:ignore-zones? true`) is noted as a `:tidy` entry `{:cell :action :was :now :zone/:claim/:plan :tries :job}` in body memory (`:job` is the top-level job that recorded it); `jobs.survival.restore-broken` puts the cells back when the body is safe and reports what it could not. The trigger `:tidy-pending` (`engine.triggers.tidy-pending`, last in `scenarios/survival.edn`) starts it on its own, see Debugging triggers. Not noted: `extinguish`, `get-food` and the other `:ignore-zones?` jobs.

## Job library

Jobs live under `src/jobs/`, helpers in `engine.jobs.util` and
`engine.jobs.forestry`. Each declares its args with defaults. Every round
re-reads the world and does a bounded piece. Positions in memory are
`{:x :y :z}` maps. Failed rounds are counted in job memory as `:failures`;
after three the job emits a warn and ends.

| job | args | check | job memory | body memory |
|---|---|---|---|---|
| `jobs.movement.go-to` | `{:pos :range 1 :doors :shut}` | always | `:blocked` count (consecutive rounds without a new best distance, more than 1 below `:best`, the nearest any round ended; a new best resets it), `:best` | none; plans with the tuned planner and walks with the executor through `engine.path.walk`: a round is one plan and one walk (at most 60 s), planned again in the same round when the way ahead changes, a partial plan is refreshed or a mob blocks a stuck walk (`engine.path.walk/follow!`, at most 12 a round, each a `:replan` info event `{:why :ms :kept :replans :at}`); a partial plan (goal unloaded or far) is walked as far as its steps can be undone, or past a one-way step (a drop of 2 or 3, a gap jump down) when the land past it runs on into unloaded land (never into a loaded pit), and the next round plans on from there; three fruitless rounds give up (warn `unreachable` with `:why`, `:refused-kind`); `:range` is cells from the target's cell (`0` is standing in it, `1` next to it); `:doors` says what to do at shut doors, gates and trapdoors (`engine.path.pass`): `:shut` (default) plans through them, opens one with an empty hand (`engine.access.click`, shared with `jobs.access.toggle`), passes and shuts it again when the walker opened it (one found open is left open); `:leave-open` the same but leaves it open (`jobs.animals.lead-to` passes it); `:never` makes them walls. A gate or door in or next to a zone of another owner is shut whatever `:doors` says; one that cannot be shut (an animal in its cell after about 2 s of waiting, a click that does nothing) stays open with a `door-left-open` warn `{:cell :why}`. Each block the walker opened has an `:opened` memory entry `{:cell :by :t}` until it is shut (a round cut between the open and the shut shuts it at its next round); a block that will not open is a wall for one more plan (a gate, 1.5 high, with the cell over it: the plan does not jump onto it), then the round's result is `:no-path :door-stuck`; iron doors and trapdoors are walls; writes a `:moved` entry `{:from :to :status :target}` per walk (`arrived`, `partial`, `blocked`) that the stuck trigger and `unstick` read; a body without `pathWorld` is refused as `:unsupported`; hands over `{:arrived bool :reason? :why? :kind? :detail?}` (`:reason :unreachable` after three fruitless rounds, with `:why` from the last one: the planner's `:no-path` reason (`:abilities`, `:goal-enclosed`, `:exhausted`, `:door-stuck` with `:cells`, `:one-way` with `:near` and `:one-way`, ...) or `:no-path` when it gave none, `:stuck` (the executor made no progress on a step; `:kind` the step's move, `:detail` its text), `:off-plan`, `:steer-failed` (`:detail` the steer's reason) or `:no-progress` (a walk that ended no nearer); `:kind` also the refused step kind when the executor cannot walk the way there), also emitted as a `:result` info event (`:refused-kind` for `:kind`) |
| `jobs.time.wait-for-day` | none | it is day | none | none |
| `jobs.time.wait-for-dusk` | none | the day's tick is 12000 or later (dusk, then all night until dawn) | none | none; the mirror of `wait-for-day`: declines by day, done at once when started at night |
| `jobs.survival.eat` | `{:item nil :until 18 :allow-bad false}` (golden carrots are the last choice when no :item is given; golden apples are never eaten) | food below `:until` and something edible carried | none | writes `:fed` (cap 20, 6 h) |
| `jobs.movement.look-around` | `{:every-ms 2000}` | always | none | writes `:looked` (cap 1, forever); looks in a random direction |
| `jobs.movement.leave-vehicle` | `{:toward nil :max-tries 2 :radius 2}` | the body rides something | `:tries` | none; done at once on foot. Faces `:toward`, else the nearest dry cell (feet and head air over a solid non-fluid block, y one below to one above) within `:radius` of the vehicle, else no look; then `dismount` with that yaw. Hands over `{:pos :landed}` (`:dry` or `:in-water`, the body's read after landing), info `vehicle.left`; a failed dismount is retried next round, after `:max-tries` warn `vehicle.dismount_failed {:tries :status}` and done, still aboard. Run by the `:mounted` trigger |
| `jobs.movement.pace` | `{:a pos :b pos :laps 3 :rounds 8 :range 1}` | always | `:rounds-run` | none; a leg that does not arrive warns `:leg-unfinished` and ends it |
| `jobs.forestry.fell-tree` | `{:species nil :radius 16 :at pos or nil}` | a column is chosen, or every candidate was unreachable, or a tree (log column with leaves near its top) is in radius (with `:at`: a log stands at that base cell; the column is read cell by cell from there up, wherever the body is, no leaves asked for) | `:column {:x :z}`, `:species`, `:base`, `:partials`, `:unreachable` | writes one `:forestry/replant` `{:pos base :species}` when the base log is dug |
| `jobs.forestry.collect-drops` | `{:radius 16 :filter [names] or nil :visible-only false}` | always | `:skipped` ids of items whose `collect` ended `unreachable` or `timeout`, `:collected` count | none; `:visible-only` skips items whose `visible` (line of sight, as hostiles') is false; `:collected` adds the stack counts of `gained` every round, a round that gave up included; hands over `{:collected n}`, the items that entered the inventory |
| `jobs.forestry.plant-sapling` | `{:at pos or nil :species nil :bone-meal 0}` | nothing to plant, or a matching sapling is carried and the spot holds no log | none | plants at the oldest `:forestry/replant` debt and forgets it; with `:bone-meal n` it then uses up to n bone meal on the sapling, one per round (memory `:meal {:pos :left}`), stopping when it is no longer a sapling or none is carried |
| `jobs.forestry.harvest-wood` | `{:species nil :radius 16 :filter nil}` | the current phase's child check (the `:plant` phase always runs: with no sapling carried it ends at once and the replant stays owed, never queued waiting for one) | `:phase`, children in slots `:fell`, `:collect`, `:plant` | as its children |
| `jobs.forestry.maintain` | `{:plan id :part id :max-logs 6 :accept #{:fluid-adjacent :falling-block} :collect-radius 8}` | the plan can be worked (readable, tree cells in `:part`, a zone list loaded; else one warn `forest.declined` `{:plan :part :reason}` per reason) and the job has begun, or a grown tree of the wanted species stands on a planned cell not left, or a planned cell stands bare (air over soil) with its sapling carried | `:begun`, `:cut {:pos :species}`, `:replant` (`[{:pos :species}]`, written when a tree is begun), `:fails`, `:collect` (species felled), `:felled`, `:planted`, child slots `:fell-x-y-z`, `:collect`; body memory `:forestry/left` (30 min) | each round: collect the drops of a felled tree; go on felling the tree begun (`jobs.forestry.fell-tree {:at}` as a child); plant the planned sapling on a bare planned cell or one this job cut (walking in reach, off its own cell); begin the nearest grown tree (not too tall, `engine.access.rules/may-dig?` ok for every log of its column); else finish with `{:felled :planted :left [cells] :bare [cells]}`. Trees off the planned cells, saplings of the wanted species and anything else on a planned cell are never touched (warn `forest.foreign`, once per cell and block); a bare cell without its sapling is warn `forest.no-sapling` `{:species :cells}` once; a tree left (warn `forest.left` `{:pos :reason :too-tall/:unreachable/:refused/...}`) is skipped for 30 minutes; info `forest.done` |
| `jobs.forestry.prepare` | `{:plan id :part id :accept #{:fluid-adjacent} :headroom {} :collect-radius 8}` | the plan can be worked (readable, tree cells in `:part`, a zone list loaded; else one warn `prepare.declined` `{:plan :part :reason}` per reason) and the job has begun, or a planted cell has a step to take: a stray to dig, water to repair, natural ground to replace with carried dirt, or its sapling carried; while a cell is still receding from a dam the check declines (and a started job's round returns `:declined`); a field where every cell is done, wrong, wet, cramped, soil-less or short is left alone | `:begun`, `:cleared`, `:soiled`, `:planted`, `:dammed`, `:recede` (cell -> time its flow is waited out until, 10 s after a dam), `:holes` (cells whose ground this job dug, filled next), `:fails`, `:collect` (cell whose drops are owed), child slots `:plant`, `:collect`; body memory `:forestry/prepare-skip` (10 min) | gets the planting spots of a forest plan ready, one step per round on the nearest cell: dig the stray in the cell (plants, snow, leaves, stone ...); dig natural ground under it (stone, sand, gravel ...) and place carried dirt; plant the carried sapling (`jobs.forestry.plant-sapling` as a child); collects the drops. Provision first: `(seq (jobs.storage.withdraw ...) (jobs.forestry.prepare {:plan "forest"}))`. Water in a planted cell is repaired (`blockAt` `properties.level`: 0 source, 1-7 flowing, 8+ falling): a source in the cell is filled with carried dirt and the dirt dug as a stray; a flow is traced upstream (at most 16 cells) and its source filled, then the flow is given 10 s to recede (nothing is polled); water that cannot be traced, no dirt carried, or a source the access rules refuse leaves the cell `:wet {:pos :why :source?}` (warn `prepare.wet {:pos :why :water-source :text}`). Never dug, reported: another species' sapling, other logs, lava, containers in the cell (`:wrong`, warn `prepare.wrong`); built ground, air, fluid or a plan cell under it, or natural ground with no dirt carried (`:no-soil {:pos :why}`, warn `prepare.no-soil`); nothing above the cell is ever dug: a block in the growth space (headroom cells from the cell up: oak 7, birch 8, spruce 9, jungle 13, acacia 10, dark_oak 10, pale_oak 10, cherry 9; air, leaves, saplings, plants, snow and vines do not block) leaves the cell alone (`:cramped {:pos :at :block}`, warn `prepare.cramped`). Cells short of saplings: `:short {species missing}` and one warn `prepare.short {:species :missing}` once the rest is done. Every dig and place asks `engine.access.rules` (zones, other active plans' footprints) when the cell is chosen and right before the act; a refusal, an unaccepted hazard or 3 failed tries skip the cell for 10 minutes (`:refused {:pos :reason}`, warn `prepare.refused`); a species outside the table is refused `:unsupported-species`; hands over `{:cleared :soiled :planted :dammed :short :wrong :no-soil :cramped :wet :refused}`; info `prepare.done` |
| `jobs.storage.deposit` | `{:chest pos or nil :items [names] or nil :keep {}}` | a chest is known (args or `:chest`) | `:failures` | reads `:chest`; a `:chest` arg that took items and finished clean is recorded as `:chest` when none is recorded or the recorded one is gone, never over a live different one (info `place.kept`); a `missing` transfer at the recorded chest (loaded cell) retracts it (warn `chest_missing`); hands over `{:gave-up false}` when nothing is left, `{:gave-up true :reason status}` when failures used it up (`"unreachable"` for a blocked walk) |
It also refuses, with exit code 3 and one line naming the world, the body and the pid that answered, when that body is
already running, before it opens or deletes any file in the body folder (`--fresh` included) and before it logs in: the
first thing a start does is bind `engine/body.sock` in the body folder, which a live body holds for its whole life and
answers with its pid. A socket left behind by a crash refuses connections and is replaced, so no crash leaves a body
unstartable (`engine.single`; only two starts racing over the same stale file within milliseconds can both pass).
| `jobs.farm.till` | `{:from pos :to pos}` or `{:center pos :radius r}` (at most 256 cells), `:for-plan id or nil` | nothing pending, or a `_hoe` is carried | `:tilled` (set of cells), `:tries`, `:skipped {pos reason}` | none; digs ground cover first, skips `:not-tillable`, `:covered`, `:unreachable`, `:refused`, `:cover-stuck`, `:gone` (info `till.skipped`); hands over `{:tilled n :skipped {...}}` with info `till.done`. Dry farmland turns back to dirt within a minute or so on this server; water is the caller's concern. With `:for-plan` every cell (and ground cover over it) is asked of `may-dig?` with the zones and the other active plans' footprints when the round looks at it and again before the hoe, a refused cell is skipped `:not-permitted`, and the check declines while no zone list has been read |
| `jobs.farm.fertilize` | `{:at pos or nil :center pos or nil :radius 8 :max 16}` | bone meal carried, nothing left to do, or some already used | `:used`, `:refused` (set) | none; bone meal on unripe wheat, carrots, potatoes, beetroots (the crop at `:at`, else within `:radius` of `:center` or the body), nearest first; hands over `{:used n}` with info `fertilize.done`; ends when the crops are ripe, `:max` is used or the bone meal runs out |
| `jobs.farm.compost` | `{:at pos or nil :radius 16 :items [names] or nil :keep {} :times 1}` | always | `:composter`, `:fed {name n}`, `:taken`, `:strikes`, `:waits` | none; feeds the composter (`:items`, else every compostable carried except seeds and own food, minus `:keep`), waits out level 7, empties level 8 with an empty hand and collects the bone meal; done after `:times` bone meal (info `compost.done`), or `:nothing-to-feed`, `:no-composter` (warn), `:gave-up` after 3 failures in a row (warn); hands over `{:fed :bone-meal :level :reason?}` |
| `jobs.apiary.guard` | `{:box {:from :to} or nil :center pos or nil :radius 16 :max 12}` | a lit campfire in the area that needs a sink or a carpet; false when every one is safe, so cheap under `repeat` | `:center`, `:started`, `:sunk`, `:carpeted`, `:skipped {pos reason}`, `:strikes`, `:sinking {:fire :kind :carpet}` (a cut resumes it) | none; makes lit campfires safe for bees and bodies, nearest first, at most `:max` actions: a raised fire (a side open) over walled ground is dug out with the ground under it and a carried campfire placed one lower, then a fire with nothing on it gets a non-moss carpet; never stands in a fire's cell; hands over `{:sunk :carpeted :reason :left :skipped :fires}`, `:reason` one of `:guarded :limit :safe :no-fire :no-carpet :no-campfire :unreachable :on-fire :occupied :cannot :place-failed :gave-up`; info `apiary.guard-done`, warn `apiary.guard-gave-up` unless `:guarded`/`:limit` |
| `jobs.build.clear-box` | `{:from pos :to pos :keep [names]}` (corners `{:x :y :z}` or `[x y z]`; at most 400 cells) | corners are well formed and the box small enough (else one warn `clear-box.declined` `{:reason :bad-args :text}` naming the shape) | `:dug`, `:tries`, `:skipped {pos reason}` | none; digs top layer first, nearest first, equipping the best carried shovel, axe or pickaxe (`engine.jobs.tools`); keeps beds, containers, fluids and `:keep` names; walks to unloaded cells first; skips `:cannot`, `:unreachable`, `:refused` (info `clear-box.skipped`); hands over `{:dug :skipped :kept :fluids}` with info `clear-box.done`. Drops are not collected |
| `jobs.build.from-plan` | `{:plan id :part id :reach 4.2 :give-up 3 :accept [:fluid-adjacent] :ignore-zones? false :sturdy-ground false}` (`:sturdy-ground`: a sturdy block on a rail line's ground, `plan.rail/ground`, is no `:wrong` block) | the plan is readable and has cells (in `:part`), a zone list has been read, and the job has begun, nothing is missing, or a missing cell's block is carried; else declines with one warn `build.declined` (`:plan :reason`) | `:begun`, `:placed`, `:fails {[x y z] n}`, `:digs {[x y z] n}`, `:given-up {[x y z] reason}`, `:refused {[x y z] why}` | none; every place asks `engine.access.rules/may-place?` (zones, footprints of the other active plans) when the cell is chosen and again right before the place: a cell in a zone not allowing `:place` or in another active plan's footprint is refused for good (listed in `:refused [{:pos :reason :zone}]` or `[{:pos :reason :footprint :plan id}]`, `:reason` being `:zone` or `:footprint` and `:zone` / `:plan` naming the zone or the plan that claims the cell; one warn `build.refused` per reason), a cell with a fluid hazard not in `:accept` is refused as `:hazard` (water beside is accepted by default because placing beside or into water seals and bridges; lava beside is not, the body stands beside the cell); each round re-judges the plan against the world (`plan.shape/plan-minus-world`) and places every `:missing` cell (or one holding a replaceable block such as `leaf_litter`, `fern`, `vine` or water, which the place takes over) in reach, lowest first; a cell holding another wrong block (dirt, leaves, stone ...) whose item is carried is dug first (`:dig?`; asks `engine.access.rules/may-dig?` with the same zones and footprints, so another plan's footprint or a foreign zone refuses it as for a place, listed in `:refused` and `:wrong`; best carried tool, by hand when none is needed, else given up `:no-tool`; a cell that keeps refilling is given up `:refilled`) and then placed; a door's or tall plant's upper half and a bed's head are never targets (the lower or foot cell places the item, a wrong block in the other half is dug first, `:other-half` is no longer reported) (a cell waits while the cell below it is still owed), else walks to a stand cell 1-3 blocks beside or diagonally off the nearest (a stand it could not walk to is not tried again); every cell's state comes from `engine.placement/click` (the neighbour, face, cursor, look and sneak are sent with the place, from wherever the body stands); a `:facing` want it places plainly (a family it does not know) keeps the old rule: placed only while the body looks that way, walked to on the side it faces away from; a cell no click can give now waits, and if still missing at the end is given up with the reason (`:no-support`, `:no-room`, `:opened`, `:double-slab`); a placed block whose reported `placed` state is not the want is listed under `:wrong` as `{:pos :found "name[k=v]" :want :placed true}`, never dug or retried; wrong blocks it may not or cannot dig stay listed; `:clear`, crops and trees are skipped; an unloaded cell is walked toward, never taken as built (given up `:unloaded`); a cell refused or not walkable to `:give-up` times is given up; hands over `{:placed :missing :short {item n} :given-up :wrong :refused}` with info `build.done`, warns `build.short`, `build.gave-up`, `build.wrong`; an event text names the count and the first 8 cells, the whole list is in the event's `:cells` and the result, never clipped. Positions are plan vectors `[x y z]`. Stand cells are at the body's feet height (flat ground) |
| `jobs.build.pen` | `{:plan id :part id :reach 4.2 :give-up 3 :accept [:fluid-adjacent] :max-cells 2000}` | the plan is readable, has cells (in `:part`) of which some want a `*_fence`, `*_fence_gate` or `*_wall`, a zone list has been read (else one warn `pen-build.declined` `:plan :reason`); declines without a warn while nothing is missing and the pen already holds; through the builder's own `build.declined` while no material is carried; once begun it stays true | `:phase` (`:build` / `:check`), `:built` (the builder's result), and the builder's memory in slot `:build` | none of its own: phase 1 runs `jobs.build.from-plan` as a child (args `:plan :part :reach :give-up :accept` pass through; zones, other plans' footprints, materials, cuts and resuming are its rules); phase 2 runs `engine.jobs.pen/check` over the bounding box of the plan's fence, gate and wall cells (what `jobs.animals.pen-check` does with a `:box`); nothing is placed that the plan does not ask for, so a leak left is the plan's hole or a cell the build refused, gave up or lacked material for; ends `:done` with `{:closed? :reason :cells :leaks [{:pos :why}] :gates :built {:placed :missing :short :given-up :wrong :refused}}` (`:reason` nil when closed, else `:leak :unbounded :unloaded :no-start`); closed emits info `pen-build.done`, not closed ONE warn `pen-build.leaky` (`:leaks`, and `:refused :given-up :short` from the build); it ends `:done` even when not closed (read `:closed?`; a `seq` after it runs regardless, and `jobs.animals.herd` checks the pen itself); a restart rebuilds and re-checks nothing that holds (the world is the memory) |
| `jobs.build.rail-line` | `{:plan id :part id :reach 4.2 :give-up 3 :accept [:fluid-adjacent] :all-carried true :fix 1 :dig false}` (`:fix`: how often a rail that settled in the wrong shape is dug and placed again, then given up as `:shape` (0 or false: at once); `:dig` is accepted and does nothing yet: digging is slice 3) | the plan is readable (else `:reason :plan`), its rail cells (wants naming a `*rail` block) form one chain by `plan.rail/line` (else `:not-a-line` with `:why` `:gap`/`:branch`/`:two-chains`/`:loop`/`:no-rails`), a zone list has been read (else `:no-zones`), and no cell still to build is refused by `engine.access.rules` (else `:refused` with the whole list), and no cell wanting a redstone block holds another block (else `:source-blocked` with `:cells`: the builder never digs, so `:power :block` needs a raised bed, or use `:torch`/`:lever`); each once as warn `rail-build.declined` (`:plan :reason`); with `:all-carried`, every item still to place is carried (else ONE warn `rail-build.short` `{item n}`, an `:any` want keyed by its text and counting every choice carried; only cells seen empty are owed, `:up-to` counts the cells nobody has seen as well); declines without a warn while nothing is missing and the line passes the proof; once begun it stays true | `:phase` (`:build` / `:switch` / `:check`), `:building`, `:built` (the builder's result), `:switched` (levers tried), `:from` (the end the build began at), `:fixes` (`{pos n}` digs of wrong rails), `:fails` / `:given-up` / `:refused` as `jobs.build.from-plan`, the toggle's memory in slot `:lever` | corners and slopes: phase `:build` is the head-first builder `engine.jobs.rail` (its own rounds, using `jobs.build.from-plan`'s placing, rules and result functions with a sturdy block on the line's ground no `build.wrong`): the cells go in along the line from the end nearer the body, station by station (bed and power under, then the rail, then the torch or lever beside it), so each rail takes the shape its neighbours give it; the body stands on the rail behind the cell it places; a rail that has settled (every neighbour in the chain is a rail) in the wrong shape is dug (rules asked first) and placed again up to `:fix` times, then given up as `:shape`; with nothing placeable carried and no wrong shape it is skipped; phase `:switch` turns every planned lever that stands off on with `jobs.access.toggle` as a child, once each; phase `:check` runs `plan.rail/judge-line`; any sturdy block where the plan wants bed or buffer fill is kept and is not listed `:wrong`; ends `:done` with `{:ok? :breaks [{:pos :why}] :hazards [{:pos :why :falling-bed}] :built {:placed :missing :short :given-up :wrong :refused}}`, `:why` one of `:gap :shape :unlit :lit-brake :no-bed :blocked :wet :no-buffer :launch-trap :unloaded`; sound: info `rail-build.done` (`:plan :cells :placed`), else ONE warn `rail-build.broken` (`:breaks` at most 12, `:breaks-total`, `:refused :given-up :short`). Plans come from `plan.rail/layout` (waypoints, each leg along x or z, flat or 1 up or down per cell: buffer, two normal rails and a launch group of 3-5 lit powered rails at each end, then one lit powered rail every 30 cells (measured: 34 holds 0.40 blocks per tick, 35 fails) or all powered; a redstone block, torch or lever per run of up to 17 powered rails. A corner is a normal rail with a lit powered rail on each side and the spacing count restarting there; every other climbing cell is a lit powered rail with a source of its own; two clear cells above each climbing cell. Errors as data: `:leg-too-short`, `:slope-into-corner`, `:slope-into-launch`, `:power-side` (the side asked for is a cell of the line), `:touching`, `:valley`, `:reversal`, `:bad-slope`, `:not-straight`) |
| `jobs.explore.search` | `{:target name-or-names :count 1 :max-distance 96 :pattern :spiral :heading :north :spacing 16 :scan-radius 24 :max-legs 32 :timeout-s 600 :use-notes true :seen-ttl-s 259200 :entity-ttl-s 600 :searched-ttl-s 86400 :load-wait-s 30}` | a target is named and it is not waiting for leg columns to load | `:origin :started :legs :scans :found :tried :failed :skipped :farthest :failed-in-row :waiting-since :wait-until`, `:leg` while a walk is under way | none; writes notes (`engine.notes`): `:seen` per target sighted, `:searched` per look. Each step looks round (blocks and entities named `:target` within `:scan-radius`) and ends when `:count` are found, else walks the next leg with `jobs.movement.go-to` (range 2): `:spiral` rings `:spacing` apart round the origin, or `:outward` ahead along `:heading` then the sides; a leg is never tried twice, never beyond `:max-distance` (XZ), skipped when a live `:searched` note of any body listing every target lies within half its radius; a leg column must be dry and have a standing cell within 12 of the feet (read down through leaves) or it is recorded failed (`:wet :no-surface`) and the next tried; an unloaded column is not tried but looked at again later, and when only unloaded ones are left the round ends and the check declines for 2 s, up to `:load-wait-s` (a body just logged in has no chunks round it yet), then it ends `:not-loaded`; a walk go-to gives up on is `:unreachable`, three in a row end it (`:stuck`); with `:use-notes`, live `:seen` notes of a target within `:max-distance` count as found (`:noted true :by`). Hands over `{:reason :found|:not-found :why :found [{:what :pos :id?}] :coverage {:legs :scans :failed :skipped :farthest}}`, `:why` `:distance :legs :time :stuck :not-loaded :bad-args`; info `search.done`, warn `search.not-found`; `backoff {:after 9}` |
| `jobs.explore.look` | `{:radius 16 :block-names nil :entity-names nil :max-blocks 6 :max-entities 2 :properties? false :at nil}` | always | none | none. One round, read-only (never moves, equips, digs or places): the nearest loaded blocks (up to `:max-blocks`, 0..16, optionally only `:block-names`, with state `:properties` when `:properties?`) and entities (up to `:max-entities`, 0..8, optionally only `:entity-names`) within `:radius` (1..32), nearest first, `:more-blocks?`/`:more-entities?` when more matched; `:at [x y z]` inspects one cell with its properties, `:loaded? false` for an unloaded cell (known air is `{:name "air" :loaded? true}`). `:scope :loaded-chunks`: loaded-chunk evidence, not a terrain map. Info `look.observed` with the result (read it back with `observe result jN`), warn `look.refused` on bad args |
| `jobs.farm.find-spot` | `{:w 5 :h 5 :range 24 :center nil :depth 12 :limit 3 :walk false}` | always | `:scan {:next-x :found}` while scanning, then `:spots` | none; scores every w x h patch within `:range` of `:center` (else the body): level% + 25 x share of cells hydrated (water within 4 in x and z, at the cell's y or one above) + 15 if no block above the scan window − min(40, height deviation) − min(30, distance/4); water, lava, ice and magma surfaces never count. The scan reads at most 4096 blocks per round plus one patch row (about 44 rounds for the defaults); then info `find-spot.found` (or warn `find-spot.none`) and, with `:walk`, a walk to the best north-west corner; hands over `{:spot pos :spots [...] :walked bool :reason?}` |
| `jobs.storage.make-room` | `{:free 4 :chest-range 32 :keep-food 16 :keep-blocks 64 :toss-below 1 :swap-radius 8 :away 4 :max-rounds 40}` | fewer than `:free` slots free | `:rounds`, `:tossed-at`, `:toss-dir`, `:walked`, `:acted`, `:swap-id` (with `:swap-item`, `:swap-worth`), child `:deposit` | reads `:chest`, `:chest-unusable`, `:picked-up`; writes `:chest-unusable` `{:pos :reason}` (cap 5, 10 min); emits info `make-room.tossed`, `.swapped`, `.done`, `.declined`, warn `make-room.stalled`, `make-room.toss-failed` |
| `jobs.storage.withdraw` | `{:chest pos or nil :items {name count}}` | a chest is known (args or `:chest`) | `:failures` | reads `:chest`, retracting it when the container at it is `missing` (loaded cell; warn `chest_missing`); carries at least count of each name, one name per round, re-deriving what is short from the inventory; hands over `{:gave-up false :short {name n}}` (short is what the chest lacked) or `{:gave-up true :reason status :short {name n}}` (`"unreachable"` for a blocked walk, `"nothing-moved"`); info `withdraw.short`, warn `withdraw.gave-up` |
| `jobs.storage.kit` | `{:tools [kind] (default ["hoe"]) :spare n (1) :food n (12) :chest pos or nil}` | a chest is known (args or `:chest`) | `:failures`, child `:take` | reads `:chest`; carries 1 + spare of each tool kind (a name equal to the kind or ending `_kind`, any tier, best first) and `:food` food items, taking the plan from the inspected chest through `jobs.storage.withdraw`, re-derived every round; hands over `{:gave-up false :short {kind n}}` (short is what the chest lacked, keyed by kind string or `:food`) or `{:gave-up true :reason r :short {kind n}}` (`"unreachable"`, the inspect status, or the withdraw's reason); info `kit.short`, warn `kit.gave-up` |
| `jobs.items.smelt` | `{:furnace pos :item nil :count nil :fuel nil}` | no load made yet, or the cook is due: the clock is past `:ready-at` (now plus the cook time of the items loaded, 200 ticks each in a furnace, 100 in a blast furnace or smoker, plus 2 s), or past `:unlit-from` (6 s after a load) with the furnace block loaded and not lit (fuel out, or the cook over). Reads memory, the clock and one block, opens no window; the body does other jobs while it cooks | `:owed {:item :count}`, `:target` (the input the furnace must hold, so a cut load is not repeated), `:got`, `:ready-at`, `:unlit-from`, `:failures` | none; result `{:smelted n :wanted n}` (+ `:reason`); gives up (warn `smelt.gave-up` with `:reason`) on `no-furnace`, `not-a-furnace`, `furnace-gone`, `output-gone`, `unreachable`, `no-item`, `nothing-smeltable`, `not-smeltable`, `no-fuel`, `unknown-fuel`, `out-of-fuel`, `furnace-busy`, `furnace-full`, `fuel-busy`, `rejected-fuel`, `inventory-full` |
| `jobs.items.enchant` | `{:item nil :table nil :radius 16 :max-level-cost nil :slot nil :choice "best"}` | `:item` is a string | `:attempt` (`{:slot :level-cost :xp :lapis}`, written before the enchant call), `:failures` | none; walks within 3 of the table (`:table`, else the nearest within `:radius`), reads the offers with the item in the table, chooses (`:slot` 1-3 asks for that offer; else `"best"` takes the dearest offer the body can pay, `"cheapest"` the lowest level cost it can pay; offers over `:max-level-cost` are ignored; an offer needs its slot number in lapis and levels, and levels of at least its level cost) and enchants; result `{:enchanted :item :slot :level-cost :levels-spent :lapis-spent :enchants [{:name :level}] :xp-level :hint}` (`:hint`: the table's hint for the chosen offer, `{:enchant :level}` or nil) measured by the primitive, info `enchant.done`; give-ups `{:enchanted false :reason}`, warn `enchant.gave-up`: `no-table`, `not-a-table`, `no-item`, `already-enchanted`, `not-enchantable`, `no-lapis`, `too-few-levels`, `no-offer`, `no-offer-within-cost`, `inventory-full`, `window` (did not open, 3 times), `window-stalled` / `not-confirmed` (never repeated: the call may have taken the price; `:levels-spent`/`:lapis-spent` say what went), `offer-changed`, `unreachable` (the last three after 3 tries); a restart after the enchant landed finds only enchanted copies and reports `:resumed true` |
| `jobs.farm.harvest` | `{:radius 12 :center pos or nil :replant true :crops [crop block names] or nil :give-up 4 :reach 4.2 :plan id or nil :part id or nil}` | a replant debt or collect sweep is owed, a ripe wanted crop is within `:radius` of the centre (with `:plan`: on a planned cell that wants that crop) while fewer than `:give-up` were unreachable, or the job has begun (so the finishing round runs); with `:plan` never while the plan is missing, unreadable or without crop cells (one warn `harvest.declined` `{:plan :part :reason}` per reason) | `:center` (the body's position at the first round when nil), `:replant` (debt `[{:pos :seed}]`, written before each dig), `:skipped`, `:unreachable`, `:collect`, `:cut`, `:replanted`, `:bare`, `:warned`, child `:collect` | cuts ripe wheat, carrots, potatoes and beetroots (walking only when none is within `:reach`), seeds each cut cell again from what is carried (collecting the drops first), gives up cutting after `:give-up` unreachable crops; with `:plan` (and `:part`) the field is the plan's crop cells, read afresh each round: crops outside it stand, and every planned cell standing bare over farmland is seeded with the planned crop (the plan is the debt); hands over `{:cut n :replanted n :bare [cells] :gave-up bool}`; info `harvest.done`, warn `harvest.gave-up`, warn `harvest.bare` |
| `jobs.farm.tidy` | `{:plan id :part id or nil :accept #{:fluid-adjacent} :reach 4.2 :give-up 3}` | the plan is readable and has cells (in `:part`) and a zone list has been read; otherwise it declines (one warn `tidy.declined` `{:plan :part :reason}` per reason; nil zones never mean no zones) | `:dug`, `:collected`, `:refused` (`[{:pos :block :reason ...}]`), `:fails`, `:collect`, child `:collect` | digs the strays over a plan: anything in a cell the plan wants `:clear`, anything but the planned crop in a crop cell (weeds, saplings, leaves, stone, dirt), and the one cell of air above each crop cell the plan does not name; sweeps top-down, nearest first, then collects the drops (`jobs.forestry.collect-drops`). Never dug: the planned crop, a block the plan wants, water or lava, farmland, a container, workstation, bed, sign, banner, head or any light (listed in `:kept {:pos :block :why}`); a block where the plan wants a different block and a crop of another kind in a crop cell are reported in `:wrong` (warn `tidy.wrong`), never dug, so the plan's own water is never drained. Every dig is asked of `engine.access.rules/may-dig?` with the zones and the footprints of the OTHER active plans when the cell is chosen and again right before the dig; `:footprint`, `:zone` and an undiggable block are refused at once, a hazard not in `:accept` defers the cell until nothing else is left and is then tried `:give-up` times (lava beside is `:lava-adjacent`, not accepted by default); an unwalkable or unreachable cell is refused as `:unreachable` after `:give-up` tries; hands over `{:dug :collected :kept :wrong :refused}`; info `tidy.done` ("nothing to dig" over a tidy field), warn `tidy.refused`, warn `tidy.wrong` |
| `jobs.farm.plant` | `{:box {:min :max} or nil :seed nil :reach 4.2 :plan id or nil :part id or nil}` | a box is given, the run has started, or a bare cell (farmland with air above, ground layer y = `(:y :min)`) not skipped exists and a seed is carried | `:started`, `:planted`, `:skipped` (cells), `:fails`/`:walk-fails` (`[{:pos :n}]`) | sows the bare cells of the box with `:seed`, else the carried seed with the largest stack (wheat_seeds, carrot, potato, beetroot_seeds), walking to the nearest cell when none is within `:reach`; a cell whose place or walk fails 3 times is skipped; hands over `{:planted n :skipped [cells] :reason r}` with `:reason` one of `:done :none :no-seed :gave-up`; info `plant.done`, warn `plant.gave-up`; with `:plan` (and `:part`) the field is the active plan's crop cells (wheat, carrots, potatoes, beetroots): each bare cell is sown with its own crop's seed when carried, every sowing asked of `may-place?` (zones, other active plans' footprints; the body standing in the cell is no reason) when the cell is chosen and right before the place, refused cells stay bare and are listed; result `{:planted :skipped :refused [{:pos :reason}] :short [seeds not carried] :reason}`; declines (one warn `plant.declined` `{:plan :part :reason}` per reason) while the plan is missing, unreadable, without crop cells or no zone list has been read |
| `jobs.farm.tend` | `{:box {:min :max} or nil :plan id or nil :part id or nil :till true :fertilize false :composter nil :chest nil :keep {}}` | a usable box (max y at least min y + 1, at most 2048 cells) is given and some step would run, or the job has started; otherwise it declines | `:todo`, `:report` `{step summary}`, `:call-args`, `:till-tried`, `:tilled`, children `:harvest :till :plant :fertilize :compost :deposit` (one child round per job round; `:till` is one cell per call and is re-decided until it skips) | harvest (ripe crops in the box, replanting), till (one cell at a time while a hoe is carried and more seed than bare farmland is carried), plant (`jobs.farm.plant`), fertilize (only with `:fertilize` and bone meal), compost (surplus seed above the reserve into `:composter`), deposit (farm goods above the reserve into `:chest`); the seed reserve is twice the beds, spread over the carried seeds, or `:keep`, whichever is larger; skipped steps are booked `{:skipped reason}`; known limits: harvest and fertilize work in a sphere around the box centre, so crops just outside the box may be cut or fertilized; a dirt lane in the box is a bed unless `:till` is false; an unreachable ripe crop makes the check pass on every run; hands over `{:steps :field {:crops :bare :untilled}}`; info `farm-tend.done`; with `:plan` (and `:part`, instead of `:box`) the field is the active plan's crop cells and each step works per cell with the crop the plan names there: harvest cuts the ripe planned crop (`:replant false`; the plant step sows), till hoes the ground under a crop cell (the plan's farmland: the cell below, unless the plan names it as something else) when that crop's seed outruns its bare cells, plant sows each bare cell with its own seed; a crop of another kind in a crop cell is never dug, only listed in the `:field` `:wrong` (warn `farm-tend.wrong`); a bare cell whose seed is not carried stays bare with one warn `farm-tend.short-seed` per seed; tilling and sowing are checked with the access rules when chosen and again before the act (in till and plant, called with the plan); declines (one warn `farm-tend.declined` `{:plan :part :reason}` per reason) while the plan is missing, unreadable, without crop cells or no zone list has been read; a started run declines in its next round when the plan stops being workable; untilled ground the rules refuse (foreign zone or claim, another plan's footprint) is left alone with one warn `farm-tend.refused` `{:zones :claims :plans :owners :count}`, and a run with nothing else to do stays queued with that reason on record |
| `jobs.gather.get-seeds` | `{:item "wheat_seeds" :count 8 :radius 16 :sources nil :per-round 4 :chest nil :plan nil :collect-radius 8 :dry-digs 40 :accept #{:falling-block :under-feet}}` | the way for `:item` has something to work on: `:chest`/`:plan` (or a chest-only material: carrot, potato, beetroot_seeds) and a chest is known (else one warn `get-seeds.declined` `{:reason}` `:no-chest`, `:plan-missing`, `:plan-broken`, `:plan-inactive` or `:no-chest-cell`); `:sources` or wheat_seeds and a source block is within `:radius`; sugar_cane/bamboo and a stand with a cuttable cell is (cane or bamboo with no stand of two or more declines `:too-short` and stays queued); any other material with none of these declines `:no-source`; the break ways also decline `:no-zones` while no zone list has been read | `:goal`, `:last-carried`, `:dry`, `:barren`, `:collecting`, `:skipped`, `:refused` (what refused: `{:zone :plan}`), children `:take`, `:collect` | grass (`wheat_seeds`: `short_grass`/`tall_grass`, `:sources` overrides), stalks (`sugar_cane`, `bamboo`: only the second segment of a stand of two or more is cut, so the base grows again and a stand of one is never touched), roots (`carrot`, `potato`, `beetroot_seeds`: taken from the `:chest`, the chest cell of `:plan`, or the known `:chest` place through `jobs.storage.withdraw`, never dug from a field); goal = carried + `:count`, fixed in round one; every dig target is judged by `engine.access.rules` when chosen and again right before the dig (zone not allowing `:dig` or another plan's footprint: skipped for good and remembered; hazard outside `:accept`: skipped); collects only the item afterwards (`jobs.forestry.collect-drops`); ends `:count`, `:short` (chest had less), `:none` (no source left), `:refused` (every remaining source refused: warn `get-seeds.gave-up` with `:zones`/`:plans`), `:dry` (`:dry-digs` digs with no new item) or `:barren` (two batches where nothing was diggable), the last three with that warn; hands over `{:got n :reason r}` (info `get-seeds.done`); melon and pumpkin are not seed sources here (the old rule only cut the fruit); unit-tested, live tests to follow |
| `jobs.survival.retreat` | `{:radius 8 :ranged-radius 16 :clear-radius 40 :eat-gap 12 :step 6 :cooldown-ms 5000 :weapons :blocks :max-places 4 :max-hide-ms 60000}` | always | `:last-seen` | reads `:bed`, `:home`, `:hazard` |
| `jobs.survival.sleep` | `{:bed-radius :bed nil}` | night, a `:bed` within `:bed-radius` (or the `:bed` arg, any distance), and no unexpired `:bed-unreachable` at that pos | child `:go` | reads `:bed`, `:bed-unreachable`; writes `:slept`, retracts a missing `:bed` (not when its chunk is unloaded: retried, warns `bed_unloaded`), writes `:bed-unreachable {:pos}` (cap 5, 10 min) when the bed stays unreachable after three tries; after sleeping in the `:bed` arg records it as `:bed` when none is recorded or the recorded one is gone (block loaded and not a bed), else keeps the recorded one with info `place.kept` |
| `jobs.survival.breathe` | `{:min-oxygen 12 :radius 2 :reach 10 :shore-radius 6 :far-radius 24}` | drowning (swims up, or walks sideways to a column with air, then swims toward the nearest land within `:shore-radius`, else walks with the walk driver to land within `:far-radius`; with no way out it stays afloat, jump held, and warns `:afloat` once instead of ending and sinking), enclosed (the suffocating condition: a sideways step first, else dig), or surfaced and still in water | `:noted`, `:surfaced`, `:side-tried`, `:failures`, `:afloat` | writes `:breathe` (cap 20, 1 h) |
| `jobs.survival.extinguish` | `{:water-radius 6 :step 4 :scan-radius 8}` | on fire or in lava, without fire resistance; stands still (info `:extinguish_wait`, done) when on fire with no bucket use, no water in `:water-radius` and no hazard within 1.5 blocks; after pouring a carried water bucket it remembers `:poured` and, once the fire is out, scoops the water back with `bucket` (waits up to 10 rounds for the poured cell to read as water, the block read can lag the pour; warn `extinguish.scoop_failed` naming the cell if the scoop is not placed; gives up waiting after 8 rounds) | none | writes `:extinguish` (cap 20, 1 h), `:hazard` for lava seen (cap 50, 6 h) |
| `jobs.survival.recover` | `{:health 7 :healed 16 :sight 16}` | health below `:health`, or below `:healed` with a `:hurt` in the last 5 min, or a spell under way | `:spell-started`, children `:flee`, `:safety`, `:eat` | writes one `:hurt` per spell; reads `:bed`, `:home` |
| `jobs.survival.respond-to-hostile` | `{:radius 8 :fight-health 12 :min-health 8 :max-fight 2 :weapons ["_sword" "_axe"]}` | a hostile within `:radius` | `:decision`, `:logged`, child `:fight` or `:flee` | writes one `:hostile` per encounter (cap 50, 1 h) |
| `jobs.survival.fight-back` | `{:range 4 :min-health 8 :weapons ["_sword" "_axe"] :skip [] :attack-gap-ms 600}` | health at least `:min-health` and a hostile within `:range` | `:last-attack`, `:struck`, `:killed` | none |
| `jobs.combat.attack` | `{:targets [] :radius 16 :weapons :attack-gap-ms nil :lost-s 5 :timeout-s 120 :no-damage-hits 4 :max-hits 40 :walk-timeout-s 5 :absent :done}` | a listed target within `:radius`, or started, or `:absent` is `:done` | `:started :last-seen :last-attack :seen :hits :quiet :health :fails :given-up :killed :killed-players` | none; hands over `{:reason :killed :given-up}`; emits info `attack.done`, warns `attack.gave-up`, `attack.timeout` |
| `jobs.survival.get-food` | `{:food 6 :food-when-hurt 14 :source-radius 64 :hunt-radius 24 :farm-radius 6 :take 16 :attack-gap-ms 600 :ask-cooldown-ms 600000}` | hungry (as the hungry trigger, including the top-up of a hurt body carrying common food below 18; the top-up never starts a hunt), or a meal under way | `:eating`, `:dead-source`, `:last-swing`, `:skipped-animals`, `:skipped-blocks`, children `:eat`, `:goto`, `:collect` | reads `:food-source` (forgets one found empty or unreachable); writes `:hungry` when nothing is found; during `:ask-cooldown-ms` after that it still eats and harvests/hunts what is in sight (no wheat) but skips the remembered sources it already knew when it gave up (one learned since is still tried first) and returns `:declined` when nothing is in sight |
| `jobs.survival.shelter` | `{:roof-height 4 :bed-radius :urgent-bed-radius 128 :max-days-awake 3}` | the night-unsafe condition; a round ends `:done` when asleep, roofed within `:roof-height` or not night, and `:declined` when no child could do anything | `:sleep-failed`, children `:sleep`, `:dig-in` | reads `:slept`; writes `:needs-bed` (cap 1, 1 day; the once-a-day `needs_bed` warn flag); dig-in writes `:shelter`, which nothing reads) |
| `jobs.survival.dig-in` | `{:roof-height 4 :blocks [building blocks] :max-places 4}` | night and no roof within `:roof-height` | `:mode` (`:walls`, `:dig`, or `:no-roof-support`: no pit; and `:roof`, `:start`, `:target-y` in dig mode: 2 or 3 deep, see `dig-plan`), `:placed`, `:occupied` (cells given up on, never retried: reported occupied by a block that fills the cell (leaves), counted as sealed; only such cells left ends the job). A cell is already sealed (`sealed?`) only when blockAt says `:fullCube` (stone, leaves, glass) or the block is a fence, wall, pane, iron bars, gate or door; signs, banners, rails, pressure plates, buttons, levers, carpets, cobweb, torches, plants, slabs and stairs are open cells that get dug out once and walled, `:cleared` (cells whose walk-through block (torch, sapling, cobweb; place reported occupied, blockAt not `:fullCube`) was dug out once and placed again; occupied again or a failed dig moves the cell to `:occupied`) | writes `:shelter` (cap 10, 1 day) `{:pos :roof :state :built}` from the current feet and the cells it placed, plus `:door` in walls mode (history only), and `:dig-in-futile` `{:pos}` (cap 5, 10 min) when a dig yields nothing to roof the pit with; its check then declines while no block is carried and one lies within 8 blocks. Walls mode recomputes its cells from the current feet each round; dig mode rechooses if the body leaves its column and stops (`dig_in_failed`) when a dig yields nothing to roof the pit with |
| `jobs.survival.log-out` | `{:bed-radius :offline-allowed true :offline-ms 20000 :player-radius 128}` | night, no usable bed, allowed, not unsupported before, another player sleeping (fired by the `:player-sleeping-nearby` reflex; the next firing logs out again if the night is not over) | none | writes `:log-out` (cap 10, 1 day) |
| `jobs.survival.recover-drops` | `{:margin 0 :danger-radius 8 :collect-radius 10}` | a `:died` with no newer `:recovered` | `:death-t` (the death it is about; a different death resets the rest), `:decided`, `:baseline` (carried counts when decided), `:phase`, children `:go`, `:collect` | reads `:died`, `:respawned` (decides and walks only once a `:respawned` newer than the death exists, then waits 2 s before estimating); collects only the carried item names, only items in line of sight (`collect-drops {:filter :visible-only true}`), within `:collect-radius` 10 because a pile on open ground rolls 6-8 blocks out; writes `:recovered` `{:decision :collected/:skip/:abandoned ...}` (cap 10, 1 day; `:items` is how many of the pile's items entered the inventory since the decision, so pickups on the walk count, and a pile already carried again ends `:collected` without a collect step); emits info `:recover-drops.decided` with the decision and a `:text` |
| `jobs.survival.restore-broken` | `{:min-health 14 :danger-radius 8 :reach 3}` | any `:tidy` entry | `:restored`, `:failed`, `:seen`, child `:go` | reads and forgets `:tidy` (cap 100, 6 h); places a dug block again from the carried item of that name (a body standing in the cell first walks to a clear standable cell within 4 blocks, once per run; with none the cell is skipped as `:occupied` and kept, so a block is never placed into the body), digs a placed one, only when health is at least `:min-health`, no hostile is within `:danger-radius` and the cell still holds what the job left; 3 tries per cell; emits info `:tidy.restored` `{:cells}` only when a cell was put back and, only when a cell is left, warn `:tidy.not-restored` `{:cells [{:cell :was :why}]}` at the end (`:why` is `:unsafe`, `:not-carried`, `:occupied`, `:changed` or `:gave-up`); remembers the cells still waiting as `:tidy-reported` (cap 1, 6 h); started by trigger `:tidy-pending` |
| `jobs.maintenance.unstick` | `{:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000 :max-attempts 6}` | stuck (as the stuck trigger), or an attempt under way | `:attempts :rounds :best-y` | reads `:moved`; each attempt ends with a `moveTo` toward the stored goal (range 1, `:maxDistance` 3), then an uncapped retry (`:timeoutS` 6) if the body did not move over `:min-move` (displacement, not status, decides); waits for the body to land (`onGround`, up to 1 s); attempt 1 steps back unless in a pit; in a pit it pillars with `jumpPlace` (depth-many blocks of the largest placeable stack) whenever a block is carried and the cell above the head is open (else `pillar: no headroom`), else digs a door (one-block wall: front at feet and head height) or a stair step (headroom plus the next step's two cells, then `moveTo` onto it; never under sand/gravel or next to water/lava; a dig status but dug/missing ends the attempt); a round after which the feet are higher than ever yet in the spell (`:best-y`) is progress and does not count as an attempt, so climbing a deep pit one stair step per round is not cut short; only counted attempts reach `:max-attempts`, and a cap of `:max-attempts` + 8 rounds per spell bounds the rest; a stair `moveTo` records no reason when it arrived or the feet rose, else `stair: moveTo <status>`; writes `:stuck` (cap 10, 1 h) when it gives up, the `unstick.failed` warn carrying `:attempts` (counted), `:rounds` (used), `:reasons` (each distinct reason once, with a count like `(x6)` when repeated) and a `:text` naming them; equips the best pickaxe before digging |
| `jobs.maintenance.shut-doors` | `{:radius 16 :reach 3 :tries 3}` | always (ends `:done` when nothing is left) | `:shut`, `:fails {cell n}`, `:left [{:cell :reason}]` | shuts the doors, gates and trapdoors a walk opened and left open: only blocks with an `:opened` entry that the walk meant to shut (`:shut?` not false; a `:leave-open` walk's are left), any age, standing open within `:radius`, nearest first; walks within `:reach` (`walk-near!`, `:doors :never`) when farther than 4, then shuts it as the walker does (`pass/shut-column!`: an animal in its cell is waited out); a block the body stands in waits; one not shut after `:tries` rounds is given up with one `shut-doors.gave-up` warn and its entry dropped; ends with info `shut-doors.done` and `{:shut n :left [...]}`; own backoff `{:after 9}`; the job of the `:door-left` trigger |
| (culling) | `(jobs.combat.hunt {:mob "cow" :keep 4 :count 8})` | the hunt's own check | the hunt's own | the hunt's own; culling is not a job of its own, it is this hunt expression: it kills adults of the kind down to `:keep` within `:radius`, at most `:count` per run (the per-run cap), never calves |
| `jobs.animals.tend` | `{:mob "cow" :box nil :target 4 :chest nil :keep {}}` | a `:box` is given and some step would run (breed, cull, shear, collect or deposit), or the job has started; otherwise it declines (cheap under `repeat`) | `:todo` (steps still to run, from `:breed :cull :shear :collect :deposit`), `:report` `{step summary}`, `:call-args` (the step under way, not re-decided), children `:breed`, `:cull`, `:shear`, `:collect`, `:deposit` (one child round per job round; skipped steps are booked `{:skipped reason}`); known limit: the pen is the `:box` only, nothing checks that an animal inside it is inside the fence, and breed, shear and collect-drops look around the body (a radius covering the box), so same-kind animals or items just outside the box can be fed, sheared or picked up | breed (2 animals, with `:target` counting babies), cull (`:keep` the larger of 2 and `:target` minus babies), shear (sheep with shears), collect-drops (only the item names lying in the box), deposit (`:chest`, produce only, `:keep` left carried; tools and breeding food stay); hands over `{:mob :target :adults :babies :steps}` (live census in the box); emits info `tend.done`; ends `:done` even when every step was skipped |
| `jobs.animals.leash` | `{:mob nil :radius 8 :skip [] :walk-timeout-s 5 :timeout-s 30}` | always | `:started`, `:given-up {key reason}`, `:fails`, `:in-row` | none; puts a lead on the nearest animal of `:mob` not on a lead and not in `:skip` (keys), walking to within 3; counts only when the sensing then shows it `leashedToMe`; hands over `{:reason :animal key :id :given-up}`, `:reason` one of `:leashed :no-lead :none :all-leashed :unreachable :refused :timeout`; info `leash.done`, warn `leash.gave-up` unless `:leashed` | the lead stays in the hand |
| `jobs.animals.unleash` | `{:mob nil :animal nil :radius 8 :walk-timeout-s 5 :timeout-s 30 :collect true}` | always | `:freed`, `:given-up`, `:phase`, `:dropped`, child `:collect` | collect-drops (leads); frees animals on this body's lead (empty-hand click) or tied to a `leash_knot` (click the knot, which hands them to the body's lead, then the animal; a settle poll of 1.5 s follows each click), then waits 0.8 s and picks the leads up; hands over `{:reason :freed :given-up :collected :leads}` (`:collected`: leads gained since the first round, also one picked up the moment it dropped), `:reason` one of `:unleashed :none :unreachable :refused :timeout`; info `unleash.done`, warn `unleash.gave-up` unless `:unleashed` | |
| `jobs.animals.lead-to` | `{:mob nil :pos nil :fence nil :range 2 :radius 8 :gather-radius 3 :gather-tries 3 :watch-radius 64 :timeout-s 180}` | always | `:phase` (`:leash :walk :gather :arrive :release`), `:animal`, `:still-led`, `:ties`, `:pulls`, `:pull-target`, `:gather-seen`, `:settles`, children `:leash`, `:walk`, `:pull`, `:unleash` | leash, go-to, unleash; without `:fence` a `:gather` phase after the walk (a body outwalks a led animal, which trails a lead length behind): while the animal is farther than `:gather-radius` from `:pos`, a go-to child (range 1) walks the body on past the spot so the lead pulls it within `:gather-radius`, waiting for it to settle between pulls (`:gather-seen`, `:settles`), at most `:gather-tries` times, each pull going no farther past the spot than keeps the body within 11 blocks of the animal (the lead breaks past 12 on 26.1, `Leashable.LEASH_TOO_FAR_DIST`; so a cow 10 or more blocks out is never pulled) given up after 20 s (its go-to child dropped) and given up before it starts when the planned walk to its target strays more than 11 blocks from the animal (a wall the animal is jammed at, the detour would break the lead); a failed, given-up or impossible pull or spent pulls is no failure: the animal is let go where it is with a warn `lead-to.gather-short` `{:distance}` (from the spot) and `:gathered false` in the result (`:gathered` is true when the animal was within `:gather-radius`, or tied); with `:fence` (a block named `*_fence`, checked before anything is leashed) the animal is tied by an empty-hand `useOn` on the post and counted only when the sensing shows it held by something else than the body; every walking round and the arrival check that the animal is still on the lead (`:lead-broke` when seen off it, `:lost` when not seen); hands over `{:reason :animal :still-led :gathered :at}`, `:reason` one of `:tied :unleashed :lead-broke :lost :unreachable :tie-failed :timeout :no-fence` or a leash/unleash reason; info `lead-to.done`, warn `lead-to.gave-up` unless tied/unleashed | on `:unreachable`, `:tie-failed` and `:timeout` the animal stays on the lead |
| `jobs.animals.herd` | `{:mob nil :box nil :target 2 :gate nil :radius 24 :timeout-s 180}` | started, or `:mob` and `:box` given and fewer than `:target` adults of `:mob` stand in the box (so a full pen declines) | `:phase` (`:survey :regather :leash :approach :clear-out :line-up :open :step :shut-behind :shut-deepest :shut-back :shut-click :deep :let-go :exit-open :shut-gate :retry-out :give-up-walk :census`), `:gate`, `:inside-cell`, `:outside-cell`, `:wanted`, `:led`, `:brought`, `:given-up`, `:trouble`, `:regathered`, `:releasing`, `:release-tried`, `:ending`, children `:leash`, `:unleash`, `:collect`, `:gate`, `:walk` | leash (one animal, with `:skip`: babies, animals in the pen, given-up ones), go-to (one cell at a time, `:doors :never`), toggle, unleash (by uuid), collect-drops (leads, once after every lead broke); reads the pen with `engine.jobs.pen` over `:box`, picks a gate with pen floor on one side and free floor straight across, needs 4 free cells straight out from it (`:no-gate` `:why :no-approach`) and 5 pen cells straight in (`:too-shallow`), counts adults on the pen cells; no food is used. One animal per round: leash it, walk to 4 cells outside the gate and settle, wait until no adult stands within 2.5 of the cell inside, step out-3..out-1 (the cow lines up on the axis), open the gate, then step through it and 5 cells in, one cell and one settle (rests within 3.6, moved under 0.25) at a time: a led animal the body leaves within 6 blocks walks by its own path and rests 3.3 behind it, one dragged by a long walk jams in the gap. Pinned (beyond 3.6 after 6 s): back one step twice, out to out-4 and again once, then `:jammed` (let go outside, gate shut). The gate is shut from inside once no adult overlaps its cell (never while one does), the body leads the animal deep, unleashes it, walks to the cell inside, opens, walks out and shuts from outside. A `:gate-held` memory entry `{:cell}` is written before each open and dropped after each shut (the `:pen-gate` trigger skips such a gate for `:held-s`). Timeout `:timeout-s` per animal. An animal seen off the lead is `:lead-broke`, unseen `:lost`; nobody led any more: leads within 8 picked up, leashing started again once; a give-up with an animal on the lead unleashes it and shuts an open gate first; hands over `{:reason :inside :target :brought :given-up :gate}`, `:reason` one of `:brought :short :full :no-pen :leaky :no-gate :too-shallow :unreachable :gate-stuck :lost :timeout` or a leash reason; info `herd.done`, one warn `herd.gave-up` unless brought/full | one animal at a time through the gate; body ends outside with the gate shut; unit-tested, live tests to follow |
| `jobs.animals.pen-check` | `{:at [x y z] or nil :box {:min :max} or nil :max-cells 2000}` | `:at` or `:box` is given (declines otherwise) | none | none; read-only, never moves; flood-fills what a cow can walk (`engine.jobs.pen`: fence, wall and closed gate 1.5 high, an open gate or door a way out, a block, slab or carpet stepped onto, a drop over 3 not taken, a diagonal step needs both sides open) from the feet cell `:at`, or from every surface inside `:box` where a step out of it is a leak; hands over and emits (info `pen-check.done`) `{:closed? :reason :cells :leaks [{:pos :why}] :gates [{:pos :open?}]}`, `:reason` nil when closed, else `:leak` (`:why` one of `:open-gate :gap :climb :open :unloaded`, at most 12 listed), `:unbounded` (more than `:max-cells` reached: a pen bigger than the bound, or an open one without a `:box` whose gap shows no wall either side), `:unloaded` or `:no-start`; `:cells` of a leaky pen without a `:box` is what it would enclose with its leaks shut (0 when shutting up to 4 rounds does not close it, or a wall is climbed, e.g. a low wall all round); `engine.jobs.pen/in-pen?` counts an animal's position against the inside cells |
| `jobs.animals.shut-gate` | `{:plan nil :radius 8 :reach 3 :tries 3}` | with `:plan` always (a plan that is missing or unreadable ends with one `shut-gate.declined` warn); without, an open gate of a plan is within `:radius` and not given up lately | `:shut`, `:tries {cell n}`, `:given-up [[cell reason]]`, `:standing` | writes `:gate-gave-up` `{:cell :reason}` (cap 50, 10 min) when a gate is given up; shuts the open planned gates (every one of `:plan` wherever it is, else those within `:radius`), nearest first: walks within `:reach`, clicks with an empty hand, reads the block again (the open reading of `engine.jobs.pen`); a gate the body stands in is left (`:standing-in`); a gate unreachable or not shut after `:tries` rounds is given up with one `shut-gate.gave-up` warn; a gate in no plan is never touched; ends with info `shut-gate.done` and `{:shut n :left [{:cell :reason}]}`; own backoff `{:after 9}` |
| `jobs.debug.walk-plan` | `{:to [x y z] :range 0 :timeout-s 60 :weight 1.2}` | always | none | none; plans with the path planner within the executor's abilities and walks the plan with `steer`; hands over `{:status :arrived\|:refused\|:no-path\|:stuck\|:gave-up\|:failed\|:unsupported :replans}` (`:no-path :reason :abilities :kind`: only a step the executor cannot do leads there; it swims, and walks a partial plan only to its last step out of water), also emitted as `:walk-plan.result` (`:kind` as `:refused-kind`) with `:ms`, `:walked` (blocks of the plans followed), `:walk-ms` (time in `steer`) and, when arrived, `:blocks-per-s` |

| `jobs.apiary.harvest` | `{:with :either :box {:from :to} or nil :center pos or nil :radius 12 :max 8 :walk-timeout-s 8}` | always | `:center`, `:harvested`, `:tool`, `:skipped {pos reason}`, `:collected`, `:strikes`, `:phase` | none; takes the honey of ripe hives (`honey_level` 5) with shears (then collects the honeycomb) or a glass bottle, nearest first, only when smoked by vanilla's rule (lit campfire up to 5 under it); an unsmoked hive is declined without a click (`:not-smoked`), one over an open lit fire too (`:open-fire`); hands over `{:harvested :reason :with :declined :skipped :collected}`, `:reason` one of `:harvested :limit :no-tool :no-hive :not-ripe :not-smoked :open-fire :unreachable :gave-up`; info `apiary.done`, warn `apiary.gave-up` unless `:harvested`/`:limit` |
| `jobs.apiary.maintain` | `{:box {:from :to} or nil :center nil :radius 12 :with :either :target nil :chest nil :keep {}}` | some step would run: a lit fire a carried item can sink or carpet, a ripe smoked hive with its tool carried and no unsafe fire, bees below `:target` by day without rain with 2 adults and a flower carried, or produce above `:keep` with a `:chest`; false when none, so cheap under `repeat`; always once started | `:todo` (steps left of `:guard :harvest :breed :deposit`), `:report` `{step summary}`, `:call-args` (the step under way), `:center`, children `:guard`, `:harvest`, `:breed`, `:deposit` (one child round per job round; each step runs at most once per pass) | one convergent pass in a fixed order over `jobs.apiary.guard`, `jobs.apiary.harvest`, `jobs.animals.breed` (2 bees, `:mob "bee"`, `:target` counts adults and babies in the area) and `jobs.storage.deposit` (honeycomb and honey bottles only; tools, bottles, carpet, campfires and flowers stay carried); harvest is held back (`:unsafe-fire`) while a ripe hive stands over a fire that still lacks a sink or a carpet; a step that does not apply is booked `{:skipped reason}`, a declined child `:declined`, a child that throws `:failed` with `:error`; hands over `{:target :bees :steps}`; info `maintain.done`; ends `:done` also when every step was skipped after the first |
| `jobs.village.trade` | `{:villager uuid :buy item :count 1 :max-price nil}` | `:villager` and `:buy` are strings | `:bought`, `:paid {item n}` (kept across a cut and restart), `:failures` | none; finds the villager within 48, walks within 2, reads its offers every round (prices move) and buys the cheapest open offer that gives `:buy` and whose first cost stack is within `:max-price` (ties: lowest index), `ceil(remaining / items per trade)` trades per round (may overshoot); hands over `{:bought :paid :item}` plus `:reason` one of `"gone"` (also a cow under the uuid) `"unreachable" "not-villager" "no-offers" "window" "no-offer" "sold-out" "price"` (with `:price`, the cheapest seen) `"payment-short" "no-room" "incomplete"` or a primitive failure reason; info `trade.done`, warn `trade.gave-up` |
- `:fell-tree` digs up to two logs per round of the chosen column, lowest
  first, and is done when the column has no logs. It walks with
  `engine.path.near/walk-near!` (range 3). A tree whose walk is blocked, partial three times in a
  row, or whose logs cannot be dug, is remembered as unreachable and the next
  candidate is chosen; with none left it warns `tree_blocked` and finishes.
- `:collect-drops` calls `collect` once per round for the nearest matching
  item entity (the primitive walks itself and judges the pickup reach).
  An item that ended `unreachable` or `timeout` is skipped from then on;
  `:collected` counts the items gained. Done when none match in radius.
- `:plant-sapling` without `:at` plants at the first debt (of `:species` when
  given), walks within 3, equips, places. `occupied` counts as planted.
- `:harvest-wood` is phase-driven: each round calls the current phase's child
  once by symbol (`jobs.forestry.fell-tree`, then `collect-drops` with the
| `jobs.combat.hunt` | `{:mob "cow" :count 1 :radius 24 :keep nil :collect-radius 8 :weapons :drops nil :max-skips 3}` | more than `:keep` adults of `:mob` within `:radius`, or started | `:started :target :collecting :killed :skipped :skips :misses :keep`, children `:attack`, `:collect` | none; kills `:count` adults, collects the kind's drops. The pair rule is the default: `:keep nil` is 2 for an animal kind and 0 for a hostile one (a kind any of whose entities reports kind hostile), so a plain hunt stops while two adults remain in `:radius`; `:keep 0` turns it off, any number wins. Babies are never targets and never counted. Hands over `{:killed :reason :spared :remaining}`, reason `:count`, `:keep`, `:none` or `:gave-up` (`:spared` = kills asked for and withheld by the pair rule); info `hunt.done`, warn `hunt.gave-up` |
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
| `jobs.debug.access-check` | `{:cells [[x y z]] or :from [x y z] :to [x y z] (at most 400) :zones [] :footprints [] :ledger []}` | always | none | none; digs and places nothing; hands over `{:verdicts [{:cell :block :dig :place}]}` (each verdict `{:ok true}`, `{:ok true :hazards [{:reason kw ...}]}` for a dig that is allowed but dangerous (fluid beside, falling block, under the feet), or `{:ok false :reason kw ...}` for an impossible or not permitted one, from `engine.access.rules`, whose docstring gives the input shape, the order of checks and `accepts?`) and emits `:access-check.result` with the dig and place counts; `:zones nil` means no zone list loaded (every verdict then refused `:no-zones` unless a more specific refusal comes first); bad args hand over `{:status :bad-args}` |
| `jobs.memory.set-place` | `{:name :pos nil :block nil}` | always (refusals are events) | none | writes the place kind `:name` (`place-policy`); `:pos` `[x y z]` or `{:x :y :z}` floored, nil = the cell the body stands in; with `:block` (a name, `"bed"` = any `*_bed`) the block must stand at `:pos` or within 1 and its own cell is recorded; one round; hands over `{:ok true :name :pos}` with info `place.set` (`:name :pos :block :was`), or `{:ok false :reason r}` with warn `place.refused` (`:text`, `:reason` one of `:bad-name :reserved-name :not-a-place :bad-pos :bad-block :no-such-block :ambiguous :not-loaded`) |
| `jobs.memory.forget-place` | `{:name}` | always | none | removes the place kind `:name` (a gone one too); hands over `{:ok true :name :was}` with info `place.forgotten`, or `{:ok false :reason r}` with warn `place.refused` (`:bad-name :reserved-name :no-such-place :not-a-place`) |
| `jobs.memory.remember` | `{:kind :data nil :ttl-s nil :cap nil}` | always (refusals are events) | none | writes one entry of the kind `:kind` (an unnamespaced keyword) with `:data` (a map, default `{}`); no `:ttl-s` (seconds) and `:cap` means the kind's own policy, else the engine default (50 entries, 1 hour); one round; hands over `{:ok true :kind}` with info `memory.remembered` (`:memory-kind :entry :policy`), or `{:ok false :reason r}` with warn `memory.refused` (`:text`, `:reason` one of `:bad-kind :reserved-kind :bad-data :bad-ttl :bad-cap`; reserved: every kind the engine or another job writes (`engine.places/owned-kinds`) and the name of a recorded place; namespaced kinds are refused as `:bad-kind`); reads back with `(since :kind)` |
| `jobs.access.pillar` | `{:height 1 :item nil}` (height 1 to 64; `:item` nil: dirt while any is carried, then cobblestone) | always | `:base` (the start cell), `:failures` | the scaffold ledger, kind `:scaffold` (cap 1, forever; `engine.access.ledger`: `{:entries [{:cell :item :before :job :purpose :state}]}`, one entry per cell, written as an `:intent` before each `jumpPlace` (count 1) and confirmed when the cell shows the item; an intent left by a cut or restart is decided from the cell: the item -> `:placed`, anything else -> dropped, unloaded -> kept); one block per round from the body's cell; before each: the rest of the column permitted (zones, other plans' footprints, a zone list loaded: `ctx/zones`, `ctx/footprints`), standing on a solid block, the new feet and head cells clear, a block carried, `may-place?` for the cell; ends with `{:status :done|:gave-up :reason :cells :built :height}` (`:cells` this job's confirmed blocks, lowest first), info `pillar.done` or warn `pillar.gave-up` with `:reason` `:too-few-blocks` (`:short`), `:ceiling` (`:at :block`), `:not-on-solid`, `:zone` (`:zone :at`), `:footprint`, `:no-zones`, `:not-loaded`, `:not-replaceable`, `:off-column`, `:place-failed` (3 failed jumps in a row, `:detail`) or `:bad-args`; never takes blocks back (a cleanup reads `ledger/open-entries`) |
| `jobs.access.cleanup` | `{:job nil :accept #{:fluid-adjacent} :reach 4.5 :give-up 2}` (`:job` nil: entries whose placing job is no longer live and not held; `"jN"`: that instance's and its children's; `:all`: every entry) | a zone list has been read (else one warn `cleanup.declined`) and an entry is offered (selected and loaded) or a run is under way | `:started`, `:removed`, `:dropped`, `:fails`, `:held` (cells given up this run), `:collect`, `:collected`, children `:walk` `:collect` | takes back the scaffold ledger's open entries. Each round settles its entries from their cells (`engine.access.ledger/settle`: the item there -> ours; a `:removing` cell now air -> removed; anything else -> dropped with info `cleanup.dropped` `{:cell :item :found}`, never dug; unloaded -> kept), then one step: dig the highest, then nearest, cell within `:reach` of the eye (entry marked `:removing` before the dig, so a cut or restart is decided from the cell), else walk to within 3 of the nearest (`jobs.debug.walk-plan` child; never digs a way), else finish. The body's own column below the feet only from on top: the block under the feet when the cell below it is a solid floor (`:no-floor-below` otherwise), deeper cells wait (`:under-body`). `may-dig?` (zones, footprints, the ledger's cells) when chosen and right before the dig; refusals and hazards not in `:accept` (lava beside is `:lava-adjacent`) keep the entry open; no plan to walk holds it `:unreachable`, `:give-up` walks out of reach `:out-of-reach`, failed digs `:dig-failed`. Collects the removed items' drops (`jobs.forestry.collect-drops`, filtered); hands over `{:removed [{:cell :item}] :dropped [{:cell :item :found}] :open [{:cell :item :reason ...}] :collected n}`, info `cleanup.done`, warn `cleanup.left`; open cells are held (body memory `:scaffold-held`, cap 1, 10 minutes). Offered by the trigger `:scaffold-left` (`engine.triggers.scaffold-left`, persistence `:stop`, in `triggers/all`, registered by no scenario yet) |
| `jobs.access.toggle` | `{:pos [x y z] or {:x :y :z} :state :open/:closed/:on/:off/:press :reach 3}` | always | none (one click per run) | none; puts one block into a wanted state: `:open`/`:closed` for fence gates, doors and trapdoors (wood, copper), `:on`/`:off` for a lever, `:press` for a button (counts when the click shows it `powered`); reads the block first and a block already there is left alone (`:already`, no click); declines before any walk with warn `toggle.declined` `:reason` `:bad-args`, `:not-loaded`, `:no-block`, `:not-toggleable`, `:needs-redstone` (iron door/trapdoor), `:bad-state` (`:valid`), `:standing-in` (closing a door, gate or trapdoor in whose column the body stands); walks within `:reach` (`jobs.movement.go-to` child with `:doors :never`), clicks ONCE with an empty hand (`useOn` without `:item`) and reads the block again; gives up with one warn `toggle.gave-up` `:unchanged` (nothing moved: protected area, lag, iron-like), `:wrong-way`, `:no-room`, `:unreachable`, `:gone` or `:refused`; zones and footprints do not apply (no block type changes); hands over `{:status :done/:declined/:gave-up :reason :pos :block :wanted :was :now}`, info `toggle.done`; own backoff `{:after 9}`. The pen-gate trigger can shut a planned gate this job opened once the body is more than 2 blocks away for 4 s. |
| `jobs.access.stair` | `{:dir :down/:up :heading :north/:east/:south/:west :steps n or :y feet-y :accept #{}}` | always | `:origin`, `:target`, `:checked`, `:at-step`, `:dug`, `:digging` (intent before each dig), `:tries` | none; cuts a 1-wide stair one step per few rounds (3 cells top first, full headroom), walks into each step with `jobs.debug.walk-plan` and plans the way back to the first cell on a fresh `pathWorld` before cutting the next; every cell judged by `engine.access.rules` (zones from `ctx/zones`, active plans from `ctx/footprints`) when chosen and again before its dig, plus a falling block over the next column; `:accept` (default `#{}`: water let into the cut floods it and the body's cell, which the walker cannot leave; `:water`, `:lava`, `:falling-block`, `:under-feet` must be named); stops (warn `stair.stopped`) on `:no-floor`, `:cave-below`, `:fluid-in-cut`, `:hazard`, `:zone`, `:footprint`, `:not-loaded`, `:no-zones`, `:no-tool`, `:unbreakable` (bedrock and the like; no tool helps), `:inventory-full`, `:refills`, `:no-way-back`, `:step-failed`, `:off-stair`, `:dig-failed`, `:place-failed`, `:bad-args`; a step whose floor is air gets a carried `jobs.survival.dig-in` building block placed there (`may-place?` first; the cell under a placed floor is not judged; never a valuable or golden block; lava or water floors are never bridged), no such block carried stops `:no-floor` with `:filler :none` and a `:why` naming the missing filler; resumes from where the body stands on the stair line; hands over `{:status :done/:stopped :reason :steps :at :dug [{:cell :block}]}` (info `stair.done`, info `stair.step` per step). A top-level job that ends `:done` having handed over a result with `:status :stopped` (any job, stair included) ends with a `job.stopped` event (data: the result without `:dug`, so `:reason`, `:cell`, `:steps`) instead of `job.completed`. |
| `jobs.access.tunnel` | `{:target [x y z] :max-length 24 :accept #{} :keep false}` (`:keep` false: a dead end, torches into the scaffold ledger; true: a tunnel that stays, lit and open) | a zone list is loaded (else one warn `tunnel.declined`) | `:plan` (entry, heading, dir, steps, run, stand, target, `:sites`), `:checked` (line index), `:stair-done`, `:dug`, `:digging`, `:tries`, `:unlit`, `:stop`, `:way-out`; children `:in`, `:stair`, `:walk`, `:out` | the scaffold ledger (`:purpose :tunnel-torch`, `:keep` false only); reaches a stand beside a buried block, the target the next cell ahead at feet height, along one straight line: from a surface entry (top of its column, solid floor, no water) a stair down or up to the target's height (`jobs.access.stair` child, called per segment with `:y`), then a flat 1-wide 2-high run; the four headings and entry distances 1..`:max-length` are judged cell by cell from the loaded blocks before any walk or dig (the stair's step rules, the run's the same, the target's column as a last run step: target, the cell over it, its floor) and the shortest valid line wins (ties: one that leaves the floor under the body uncut, then the entry nearest the body); at the stand the cell over the target is dug (the target is left to the caller; once dug its cell is 2 high, so a drop that landed out of pickup reach can be walked to); none valid stops with the shared reason (`:zone`, `:footprint`, `:hazard`, `:cave-below`, `:no-floor`, `:fluid-in-cut`, `:not-loaded`; `:fluid-in-cut`: a fluid, lava too, stands in a cell to cut; `:hazard`: a fluid beside a cut or a falling block over one that `:accept` does not name), `:too-far`, or `:no-approach` with `:headings`; every cell judged again right before its dig; the way back to the entry planned on a fresh `pathWorld` before each run step, each stair segment and at the stand (`:no-way-back` stays put). Torches on the way in: line cells 0 (entry) .. n (stand), n+1 the target; sites 0, then each the farthest whose standing cell s+1 the torch before still lights (light 14 less the taxicab distance from the farther of the site's head and feet cell), until every cell is lit (a run every 11 cells, a stair every 5 steps); the stair is cut in segments to s+1 so the torch goes in between; from s+1 a wall torch in s's head cell (left wall, then right), else a floor torch in its feet cell, `may-place?` right before, a ledger intent before the place (`:keep` false); the world is the record of what hangs, so a restart hangs nothing twice. No torch carried, a refusal or a failed place: the site is `:unlit` (`:no-torches`, `:zone`, `:footprint`, `:no-support`, `:place-failed`, `:passed`), the tunnel goes on dark, one warn `tunnel.unlit`. Any other stop after entering leaves: `:keep` false through a `jobs.access.leave-tunnel` child (torches back, mouth sealed; its result in `:leave`), `:keep` true by walking to the entry (`:out`); resumes from the body's cell on the line; hands over `{:status :done/:stopped :reason :reached/kw :target :entry :heading :stand :at :dug [{:cell :block}] :inside :keep :line {:entry :heading :dir :steps :run :stand :target} :torches [{:cell :site :block}] :unlit [{:cell :site :reason}]}` (`:unlit` `:cell`: where the torch would have gone; `:inside`: the body is off the entry, on the way in; `:torches`: those standing now) (info `tunnel.plan`, `tunnel.step`, `tunnel.done`; warn `tunnel.stopped`, `tunnel.unlit`). `jobs.gather.mine` (`:buried` on by default; `:buried false`: exposed blocks only, and with only buried blocks in range one warn `mine.declined` `:no-exposed` while the job stays queued) visits buried targets through it: tunnel, dig, collect, then out through `jobs.access.leave-tunnel` (`:spare` the mined item; a kept tunnel: walk up to the entry with `jobs.debug.walk-plan`; a stop ends mine `:trapped`), then home with moveTo; a mouth cell left open (a zone, no block to fill it) is in mine's result as `:open [{:cell :reason}]`. A body with six solid cells over its head (a cave or its own mine) has no surface to tunnel from: `jobs.gather.mine` then digs straight toward a buried block within `:burrow-max` (default 12) instead, along x then z at the body's height (a climbing or descending step per block when the heights differ, `:shaft`/`:too-steep` stops), a 1-wide 2-high way, each cut judged by the rules and refused in or beside a fluid (lava, or water unless `:wet`) or over a missing floor; once the block has an air face it is an ordinary target. Nothing is mended or walked back; a stop skips the target and counts a failure (info `mine.burrow-stopped`). Job memory `:burrow {:target}`. |
| `jobs.access.leave-tunnel` | `{:tunnel nil :spare [] :reach 4.5}` (`:tunnel`: a `jobs.access.tunnel` result with `:line :dug :torches`; `:spare`: items filled with only when nothing else is carried) | a zone list is loaded (else one warn `leave-tunnel.declined`) | `:taken`, `:left`, `:filled`, `:open`, `:collect`; children `:walk`, `:collect` | drops the scaffold ledger's entries of the torches it takes (`:removing` before each dig); leaves a dead-end tunnel, every round read from the world so a cut or restart goes on: the torches of `:torches` still standing, deepest site first: walk to cell site+1 (`jobs.debug.walk-plan`), `may-dig?` (a refusal leaves the torch, never forced), dig, collect torch drops (`jobs.forestry.collect-drops`, radius 3); then walk to the entry; then seal from it: the mouth is the dug cells at or above the entry's floor (entry y − 1) that are open now and touch an open cell the tunnel did not dig (flat ground, stair down: the ground-level cells of the first steps, flush when filled; into a hill: its face), lowest first, then farthest; each: within `:reach` of the eye (`:out-of-reach`), `may-place?` right before (`:zone`, `:footprint`, `:own-body`: never forced), filled with the dug block's drop if carried and not spare, else a carried `jobs.survival.dig-in` building block not spare, else a placeable spare (`:no-blocks`), plain `place` (`:place-failed` when the cell stays open). Hands over `{:status :done/:stopped :reason :sealed/:open/:walk-failed/:bad-args :at :taken [cells] :left [{:cell :site :reason}] :filled [cells] :open [{:cell :reason}]}` (info `leave-tunnel.done`; warn `leave-tunnel.open`, `leave-tunnel.stopped`). A discarded caller's torches are left to `jobs.access.cleanup` (ledger); its open mouth is recorded nowhere. |
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
  of walls (feet and head cells along the way must be passable; the probe
  starts at the body's cell centre and steps one block up or down where a
  walker would, so a stair dug behind the body is a way back; a column with
  no floor, a drop of two or more blocks, is not walked, so a void edge ends
  the probe). A hostile
  within `:radius` (ranged ones within `:ranged-radius`) starts the flight; it
  goes on while one is within `:clear-radius`, and is done once none has been
  for `:cooldown-ms`. Cornered (no open direction, or the walk is blocked) it
  escalates instead of repeating: armed, `fight-back` with `:min-health 0`;
  unarmed with `:blocks` carried, it seals itself in (dig-in's 1x1 cells, at
  most `:max-places` a round, warn `retreat_sealed`; a cell in another's zone
  is a last resort, `retreat.trespass-last-resort`) and waits there until the
  flight is over or `:max-hide-ms` has passed; when it cannot seal (a hostile
  in a cell, nothing to place, a refused placement) it fights with the best
  tool (pickaxe, shovel, hoe) or the fist. Only a fight that cannot reach any
  hostile counts a failed round (`retreat_blocked` after three). A hostile that
  fight killed is remembered (`:dead`) and no longer counts as a threat or a
  target while its corpse stays listed: a won fight neither swings again nor warns.
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
  `:afloat` warn and a held jump when none is in reach), or steps sideways, else digs the head
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
| `:hungry` | food below `:food` (default 6), or below `:food-when-hurt` (default 14) while health is below 20; also a top-up: health below 20, food below 18 (natural regeneration needs 18) and a common food carried (golden apples and golden carrots never count); registered after `:hostile-near`, so a danger wins | `(jobs.survival.get-food)` | cooldown 90 s |
| `:night-unsafe` | night, awake, and nothing solid within `:roof-height` (default 4) above (`engine.jobs.shelter/solid?`: not air, fluids, plants, leaves, nor walk-through signs, banners, rails, plates, buttons, levers, carpets, cobweb) | `(jobs.survival.shelter)` | cooldown 10 s |
| `:player-sleeping-nearby` | night, another player within `:player-radius` (default 128) asleep, no `:bed` remembered within `:bed-radius` (default 48), `:offline-allowed` not false, the last `:log-out` not `unsupported`; being roofed does not matter | `(jobs.survival.log-out)` | cooldown 30 s |
| `:pen-gate` | a gate of a plan (a cell whose want is a fence gate; the index is rebuilt only when a plan changes) within `:radius` (8) stands open, the body is more than `:min-dist` (2) from it, and that has held for `:open-s` (4) seconds (the clock restarts whenever the body is near the gate again, so a job holding a gate open on purpose is not fought); a gate the job gave up on is left for `:quiet-s` (600), a gate with a `:gate-held` entry younger than `:held-s` (30) is left alone (written by `jobs.animals.herd` while it leads animals through) | `(jobs.animals.shut-gate)` | cooldown 5 s |
| `:door-left` | a block a walk opened and meant to shut (an `:opened` entry of `engine.path.pass`, `:shut?` not false) has had its entry for `:open-s` (10) seconds and still stands open, within `:radius` (16), with the body out of its column: the walk was cut or cancelled between the open and the shut, or ended in the doorway (each walk also shuts its own job's leftovers within 4 blocks before and after it walks) | `(jobs.maintenance.shut-doors)` | cooldown 5 s |
| `:night-and-bed-known` | an alias of `:night-unsafe` under its old name, kept for the older scenarios; register one or the other | `(jobs.survival.shelter)` | cooldown 10 s |
| `:stuck` | the last `:n` (4) `:moved` entries, none older than the latest `:stuck` and the latest `:restart`, are all bad moves (not arrived or partial, or under `:min-move` 1.5 blocks), the newest of them is under `:window-ms` (60 s) old, and the latest `:stuck` is over `:quiet-ms` (5 min) old | `(jobs.maintenance.unstick)` | cooldown 60 s |
| `:died` | a `:died` entry younger than five minutes with a newer `:respawned` and no newer `:recovered` | `(jobs.survival.recover-drops)` | cooldown 30 s |
| `:inventory-nearly-full` | at most `:free` (default 2) of the 36 main and hotbar slots are empty; no chest is needed | `(jobs.storage.make-room)` | cooldown 120 s |
| `:every-interval` | no `:looked` entry, or the latest is at least `:seconds` (default 60) old | `(jobs.movement.look-around)` | cooldown 0 |
| `:tidy-pending` | the body is safe (health at least `:min-health` 14, no hostile within `:danger-radius` 8) and a `:tidy` entry with a loaded cell, whose recording job is no longer live (running, queued or paused; so a job still digging is not undone mid-way), is restorable now (cell as the job left it, dug block's item carried, under 3 tries) or not yet reported (cell not in the latest `:tidy-reported`, written by `restore-broken`), so entries it cannot restore are warned once and then left until something changes (item carried, hostile gone, new entries); in the survival scenario last | `(jobs.survival.restore-broken)` | stop |
| `:mounted` | the body rides something (`self().vehicle`) and no live job holds a vehicle (`engine.jobs.vehicle/held?`: a `:vehicle-hold {:job root-id}` entry whose job still has its `:job/<id>` memory; riding jobs call `hold!` before they mount and `release!` once off); in `triggers/all`, registered by no scenario yet | `(jobs.movement.leave-vehicle)` | stop |
| `:player-joined` | the latest `:player-joined` memory entry is at most `:window-s` (default 10) old (`engine.triggers.player-joined`; in `triggers/all`, registered by no scenario yet; `:player-left` has no trigger) | `(jobs.debug.notify {:text "player joined"})` | cooldown 10 s |

`:every-interval` is a wall-clock reflex: `look-around` looks at a point
three blocks ahead and writes `:looked` (which survives a restart and makes
the trigger stop holding), then waits `:every-ms` (default 2000) before its
next round. With no entry it fires at once.
`scenarios/woodcutter-cuts.edn` is the woodcutter with it first in the
register; `scenarios/pace-cuts.edn` puts it above a long `pace` job.

`:inventory-nearly-full` counts free slots as 36 minus the stacks in
`self().inventory`, which lists main and hotbar slots only (armour and the
off-hand are not in it): `engine.jobs.util/free-slots`.

`engine.value/item-worth` (a number: 0, 1, 5 or 25 by tier, or 2 per item for raw
iron, iron and gold ingots, coal and iron blocks; a missing `:count` counts as 1)
is the shared item worth: `recover-drops` sums it over a death's inventory
(`inventory-value`, which first adds the stacks of one name together, so a pile
split over slots counts as one) and `make-room` reads it to decide what may be
thrown.

### Live-unverified assumptions

The survival jobs pass against the fake only. What they assume about the real
server and mineflayer, none of it checked live:

- `extinguish` pours a water bucket with `place` at the body's own feet cell. Falsified live (2026-10-03): `place` with a water bucket on air rejects with "Server refused to place water_bucket ... the block is still air", and with a fire block in the feet cell it returned `occupied`; a bucket needs a use-item primitive. Since then `place` uses buckets through `activateItem` and treats fire, grass and snow as free (see the `place` result above); not yet re-checked live.
- Verified live: placing into the body's own cell is refused by the server ("the block is still air"), so `unstick` pillars with `jumpPlace` instead: in a 3-deep 1x1 pit, count 3 places 3 blocks in about 1.9 s and leaves the body at ground level; with no block it returns `failed`/`no-item` at once, under a roof `failed`/`no-headroom`.
- `jobs.combat.attack` judges damage by `attack`'s `hurt` (the server's entityHurt) since `health` is never present live; whether a creative player, an `Invulnerable` mob and PvP-off all report `hurt: false` is unchecked.
- `breathe`, `extinguish` and `unstick` use `moveTo` with range 0 to step
  into a cell, water included.
- `dig-in` dig mode picks the roof cell so it has a solid side neighbour (`dig-plan`): the start cell and a 2-deep pit
  when a side of it is solid, else the ground-layer cell under it and a 3-deep pit (flat ground has nothing beside the
  start cell; live the roof place gave `no-support` and left the body in an open pit); with neither it does not dig
  (`dig_in_failed`, `:dig-in-futile` reason `:no-roof-support`). A roof place that still fails ends the job with
  `dig_in_failed` after three failed rounds, the body left in the pit (open).
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
detector, progress events, multi-body, world memory, soft
pathfinding weights, a reflex pointing at a listed instance (a register entry
may carry `:instance` later).

## Primitives: implementation notes

`js/primitives.mjs` exports `createPrimitives({host, port, username, auth, version?})` as the contract says, plus
`createPrimitivesFromBot(bot, {timeScale = 1})` over an already spawned bot (the tests use it with
`js/stub-bot.mjs`; `timeScale` shrinks every time bound). `js/connect.mjs` exports `connectBot(cfg)` (defaults copied from
`src/config.mjs`; `engine.main` reads `config.json` and `world.json` itself). Nothing connects on
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
| `place` | picks a solid neighbour as the reference (below first), `equip` to hand, `placeBlock`. Liquids count as replaceable. With `click`: `setControlState('sneak')` if asked, a forced `look` (or `lookAt` the cursor point) through `lookNow`, then `_placeBlockWithOptions(against, face, {forceLook: 'ignore', delta: cursor})`, sneak released in a finally and on abort; then `blockAt` the cell for `placed`. The state rule (face, cursor height, look, sneak) is vanilla's: see `engine.placement`'s doc and `js/placing.mjs` |
| `jumpPlace` | per block: needs the item, a full block under the feet and the cell two above the feet not solid; sneaks to the cell centre (within 0.1 and until it has stopped, up to 1 s; a failure to centre is ignored); equips, looks straight down (`look(yaw, -pi/2)`), `setControlState('jump', true)`, polls every 20 ms until the feet are 1.01 above the start cell (clear of it), `_placeBlockWithOptions(block under the start cell, +y, {swingArm: 'right', forceLook: 'ignore'})` into the cell just left (`placeBlock`'s own unforced look turns gradually when the body is slightly off-centre and delays the packet ~1 s, by which time the body has fallen back into the cell and the server refuses: seen live), releases jump, waits up to 1 s for `onGround` and checks the body now stands a block higher. Cleanup: jump released |
| `collect` | loops while the entity exists, re-reading its position each time. Item within 1.0 horizontally and 1 vertically of the body (the server picks up within about 1.4): waits for the pickup, `unreachable` with `not-picked-up` after 3 s (full inventory). Item further away (it slid, or the last walk stopped short): walks to it again (first to within 1 block, then into its own cell), at most 3 walks, then `unreachable` with `out-of-reach`; a walk that does not arrive while the item remains is `unreachable` without a reason. Entity gone: `collected` with the inventory diff, or `gone` with no gain (also at once for an id that is not an item entity). `timeout` carries `gained` too |
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
- `isDay` ignores thunderstorms for `sleep`.
- `eat` relies on `bot.consume()`; on a server that never sends the finish status it can only time out (the `food`
  rise check softens this). Not exercised against a real server.
- Everything is tested against a stub bot only: reach numbers, window handling and the pathfinder are unverified live.

### Deviations

- `createPrimitives` accepts an optional `version` (default as in `src/config.mjs`).
- Timeouts resolve with a status as the contract says, not by rejecting; only cuts and bad args reject.

## Path planner

`src/engine/path/planner_tuned.cljs` is the only planner (an A* over a snapshot of section state ids: walking, jumps,
drops, gap jumps, climbing, water, doors; costs in seconds plus risk). Its tests are `test/engine/planner_*_test.cljs`
(helpers in `planner_fixture.cljs`); `planner_bench_test.cljs` plans the recorded benchmark queries (the frozen world
queries and the live tester's courses) and checks the answers recorded in `test/planner-bench.json` (the world queries need the frozen bench, gitignored: set
`PLANNER_BENCH_DIR` to it, or `PLANNER_BENCH_SKIP_WORLD=1` on a machine without one, e.g. CI, else that test fails;
after an intended planner change `npm run record:planner-bench` re-records the file and prints which answers changed). Bench: `npx
shadow-cljs compile planner-bench && npx shadow-cljs release planner-bench-release`, then `node bench-lang/bench.mjs`
A gap jump over 3 cells above a pit the body cannot jump out of (the floor 2 or more blocks down) carries 0.5 risk
(`GAP-PIT-RISK`): one that falls short traps the body (live: a crater whose only way out was a corner jump), so a short way round
is taken instead; where the jump is the only way it is still planned.
(dev against :advanced build, see its header). A drop out of or into a tight cell (a doorway on a sill, a ledge beside a
fence) is judged where the body falls: 5/16 past the edge it walked off (`DROP-INSET`, its 0.31 half-width clear of the
ledge), so stepping down out of a door that stands a block above the ground outside is planned. A gap jump or a drop never lands on farmland (vanilla tramples farmland under a
fall of over 0.5 blocks); a jump up one block onto it is allowed (it falls about 0.3 from the top of the arc).

The tuned planner also takes `options.limits {kinds, gap, corner}`: what the walker can do. `kinds` (the
`AVOID-*` bits: climbing, water, doors) are never planned; `gap(x, y, z, h, move, lx, ly, lz, lh)` returning anything but
true refuses that gap jump (takeoff feet cell, stand height in 1/16, the move that reached it; landing cell and height);
`corner(x, y, z, h, lx, ly, lz, lh)` returning anything but true refuses a diagonal jump with one blocked side (a corner slide)
from that takeoff to that landing.
Without it the walker is assumed to do anything. `engine.path.executor/planner-limits` builds it from the executor's `policy`.

A partial plan ends at the node nearest the goal that the body can come back from. A step is one-way when the planner cannot plan
its undoing: a gap jump down, a drop of more than a jump up (1.25 blocks), or a drop with no step-up move back from the lower
cell (a drop of exactly 1 is returnable when that move is legal, by the planner's own rule: it does not look at the headroom over
the upper cell, which no jump's rule does). When the path to the nearest node of all holds one, a second search with
`options.returnable` (one-way steps never planned; no goal flood) finds the partial end, and `result.oneWay` is
`{move, x, y, z, distance, path, open}`: the first one-way step on the way to the nearer node (the move code, the cell it enters),
that node's distance to the goal, the path to it, and `open`: true when that node stands within 2 columns of unloaded land (the
land past the step runs on past what is loaded). `null` when nothing nearer lies behind one, and on every complete plan (which
are never changed). With no returnable node 2 blocks nearer the status is `none` with `oneWay` set. `engine.path.walk` turns
`oneWay` into the walk-plan `:one-way {:kind :at}` stop. `plan-walk` with `:one-way :open` (go-to and `walk-near!`, through
`engine.path.near`) instead walks `oneWay.path` when `open` is true: a far goal past a cliff is walked on to, and a pit whose
cells are all loaded and that gets no nearer is never entered. A search where the path to its nearest node has no one-way step
pays nothing.

The walk driver watches the way ahead while it walks (`engine.path.walk/follow!`, used by `walk-to!` and go-to's round). At
each step the body reaches, and every 5 ticks along a plain straight or diagonal leg, while it stands on the ground (never in the
air, in water, on a ladder, or before a gap, climb or swim step), the cells of the next 10 legs (the columns the body's footprint
crosses, from the floor to two over the feet) are read in a fresh `pathWorld` and compared with the plan's own snapshot; a cell
whose state differs for the planner (any per-state array of the state table or its collision boxes; a crop's age does not) stops
the walk and plans again from the body's cell in the same round (`:changed`, the new plan is walked). A partial plan is planned
again every 4 s, or less often when planning is slow (at most 5 % of the walk) (`:refresh`); the new plan replaces it only when
it is whole or ends at least 2 blocks nearer the goal. A walk stuck with a mob on its leg waits 1 s (at most 3 times) and plans
with the mob's cells as walls (`:mob`). At most 12 such replans per follow; past that the plan is walked unwatched. Cells the plan
opens by hand are not watched. Each replan is announced (go-to: a `:replan` info event `{:why :ms :kept :replans :at}`, `:ms` the
planning time). Measured on the frozen bench (cljs dev build, machine load ~19 on 16 cores): plan p50 2.5 ms, p90 16 ms, p99
203 ms, max 484 ms.

An enclosed goal is found before any walking: for a `near` goal whose column is loaded, `step` first runs the backward goal
flood with a small budget (`options.preFlood`, default 24 cells seen, capped by `goalFlood`; 0 turns it off). If the flood
exhausts without meeting the start and the flooded cells have no cliff edge beside them (a free column with nothing to stand on
within a drop: a goal on an island or above a drop is left to the search, which ends in a partial plan) the answer is
`goal-enclosed`, `none`, no path, `expanded` 0. Otherwise the search goes on as
before and the late flood (after `floodAfter` expansions, `goalFlood` cells) still covers bigger enclosed areas. The early
pass leaves `stats.flooded` and `expanded` alone and reports its size as `stats.preFlooded`. Because it fires first, a
goal sealed off by walls says `goal-enclosed` at once (`preFlood: 0` for the old exhausted search).

A step up by walking (a stairs, a slab, a snow layer of 3 or more, a diagonal step up) lifts the body into the slab above its old
top, so that slab must be free in the cell it leaves: stairs right behind a 2 high doorway are no way in (the head meets the lintel),
behind a 3 high one they are. A ladder start, a tight cell (its mask judges) and a jump (which already checked) are as before.

`engine.path.alternatives/plan-alternatives` (`planAlternatives(snapshot, query, options, k = 3)`) returns
`{status, reason, paths, searches, ms}`: up to k paths, best first, each `{steps, cost, summary, total, differs}`
(`total` = seconds + riskWeight × risk, the true cost of the walk; `differs` says what sets it apart, e.g. `on foot
instead of by ladder`). A path is returned only when, against the paths returned before it, its set of kinds (ladder,
water, door; none of them is on foot) is not one of theirs, or fewer than 60% of its cells lie within 2 blocks of their
cells. The first path is `plan`'s own; the others come from the same search re-run with a kind refused, or with the
cells within 1 block of earlier paths costing more (weight 1.5, at most 10× the first search's expansions). One path
back is the normal answer where there is only one way. On the bench set (304 queries, best of 7 interleaved): k=1
p50 1.09 ms, p99 89 ms (as `plan`); k=3 p50 7.0 ms, p99 335 ms, with 2 or 3 paths for 83 queries.

### Executor

`jobs.debug.walk-plan` (`{:to [x y z] :range 0 :timeout-s 60 :weight 1.2}`) plans from the body's cell with the tuned planner and walks the plan with
the `steer` primitive, deciding every physics tick in `engine.path.executor` (pure). It walks `:start :walk :diagonal :corner :jump :drop
:climb-up :climb-down :jump-climb :gap :swim :swim-up :swim-down :exit` steps (`policy` `:moves`, with the gap rules below: the one place the walker's abilities are stated).
The planner is told them (`executor/planner-limits`: no doors, and every gap jump tested by the executor's own gap refusal), so it
plans round what the executor cannot do. When that search finds no whole path, one without the limits is run: if it finds one,
the result is `{:status :no-path :reason :abilities :kind :at}`, the kind and cell of its first step the executor refuses (no path within
abilities; the body does not move). Otherwise `:reason` is the planner's: `:exhausted` (no `:kind`) means every cell the body can
reach was searched and none is the goal even with every ability, as with a gap of 4 or more cells (wider than any jump), a wall or
a drop too deep; the walk first goes to the reachable cell nearest the goal (live: 6 blocks to the edge of a 4-wide gap). A plan with a step the executor cannot walk is still refused with `{:status :refused :kind :at :reason}`
Either search that finds no whole path and ran into the planner's search box (reason `box`: 64 blocks round start and goal, 48
up and down) is run once more in `walk/wide-box` (256 and 96): a way round can run far past the default box (live: a walled
walkway whose way down lay 200 blocks along).
before the body moves (a backstop). In the `:walk-plan.result` event `:kind` is `:refused-kind` (an event's `:kind`
is its own). `jobs.access.stair`'s way-back check plans the same way (`walk-plan/plan-within`).
Swimming (steps in water cells: `:swim :swim-up :swim-down :exit`, a drop into water, wading in water 1 deep, a start in water) is a
normal step, at night too. In water the body holds jump while its feet are below the step's height + 0.2 (`:swim-float`: at the
surface the head stays out), lets go on a `:swim-down` (it sinks), never sprints (the server's swimming pose is one block high), and
onto a bank holds jump and forward (the water's lift and the push at a flush edge carry it out). A floating body arrives at a final
step in water; sinking below the leg in water is not off the plan (drifting sideways is); a step in water has 6 s
(`:swim-no-progress-ticks`) before `:stuck`. Breath is bounded by the planner (air) and guarded by `jobs.survival.breathe`; the
executor adds nothing. A partial plan is walked only up to its last step out of water, so a walk never ends swimming (a bank too high to
climb out is `:no-path`). Prismarine-physics courses (`swim_physics_test`): flush-bank crossings 6 wide both ways and 20 wide, a 1-deep
wade, a current across, a 3-block drop in, up from the bottom and out, a dive to a goal on the bottom. Not yet run live.
A straight `:jump` up one cell (no corner) from the cell before it, with the body at the wall it climbs (its edge within `:wall-gap` 0.05 of the
step's cell) and its feet below the step's height, holds jump without forward and presses forward once up (live, stair `:dir :up`: forward and
jump pressed on the wall made the client sink 0.02 into it, the server refused the position and put the body back for 3 s; the first step of a
stair out of a cut, `stairs_physics_test`, with a check that no tick of the walk is inside the wall). The fake's `steer` lifts a jump made without forward.
A gap jump (1 to 3 empty cells, landing level or up to one block lower) is jumped from the takeoff edge, by width: over 1 a
walking jump from 0.2 before the edge, over 2 a sprint jump from 0.4 before (one block down: a walking jump from the edge), over 3 a sprint
jump from 0.1 before; the body aims at the landing point
in the air too, which brakes an overshoot, and the landing counts only on the ground. No run-up is needed. Refused: `:gap-up` (landing higher; not
measured), `:gap-low-ceiling` (a block within 3 of the takeoff's feet over the takeoff or gap cells: the planner allows feet+2, where the head hits at
once), `:gap-takeoff` (from a ladder), `:gap-width`. These numbers come from prismarine-physics (the body's own physics) driven by this executor,
then measured on a live body (Paper 26.1): the server accepted every jump (no pull-back), landings matched the simulation, and the sprint jump
over 2 one block down overshot a 1x1 landing 3 of 5 times (fell, re-planned), hence the walking jump there.
A diagonal jump that slides along a corner (one side cell blocked) works only when the corner is no higher than the landing's floor:
the body must slide out of the corner's column while it is in the air, and over a higher corner (a leaf block beside the leaf
block it jumps onto, live) it falls back 0.04 short. Refused: `:corner-jump` (a side cell holds collision at the landing's feet or
head height; `high-corner?`), and the planner is told (`:corner`) so it plans round it. A step of any size ahead that the body is
pressed on (`:collided`) is jumped: prismarine-physics lifts the body 0.6 where it stands before it steps, and refuses when the
lifted body meets a ceiling over the step (live: a 1/16 step under leaves 0.26 above the head). Stairs, slabs, staircases and a
stair roof walk with no special case (`stairs_physics_test`, prismarine-physics). Result statuses: `:arrived`, `:refused`, `:no-path` (`:reason`), `:stuck` (`:why`, also a walk past `:timeout-s`), `:gave-up`
(`:reason :replan-limit`), `:failed`, `:unsupported` (no `pathWorld`); all carry `:replans`. A body that ends off the plan (also a partial plan walked to
its end short of the goal) is planned again, at most 5 times (`executor/policy`: every number the executor uses). `moveTo` does not use it; `jobs.movement.go-to`
plans and walks with it one round at a time, and so does every job that walks with `engine.path.near/walk-near!` (one round: plan, one walk of at most
60 s, a `:moved` entry; `:there`, `:partial` when it ended more than 1 closer, `:blocked`; `{:doors :shut}` by default, herd, shut-gate and
lead-to's fence walk pass `:never`). Live (ProbeNight, 2026-10-04, scenarios `live-ProbeNight-exec-*.edn`): flat 30 blocks 6.95 blocks/s against `moveTo` 7.0; steps, a corner slide, an 8-high ladder up and down all arrived; a manual `take` cuts the walk with every control released.

## Migrating old bots

`npx shadow-cljs compile migrate`, then `node out/migrate.cjs --world <world> [--dry-run] [--worlds <dir>] [--state-dir <legacy-parent>] Name...` (default state
directory: the repo's `worlds/`; default names: every folder under `worlds/<world>/agents/` with no `engine/`). `--dry-run` prints one
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

`observe` now projects a small EDN status by default: rounded `:pos`, health,
food, mode, and any current work or outstanding attention. Empty/nil fields,
body names, cursors and generation UUIDs are omitted. `--verbose` retains the
bounded machine status; `--raw` retains the complete engine snapshot.

```bash
node engine/tools/observe.mjs Bob --world claude --wait
node engine/tools/observe.mjs Bob --world claude --wait --timeout 30s --watch j17
node engine/tools/observe.mjs Bob --world claude --wait --chatter all --from Dan --observer builder
node engine/tools/observe.mjs Bob --world claude --wait --chatter none --danger --disconnect
```

The external tool polls the existing EDN event API locally (250 ms default,
`--poll-ms 50..5000`). The engine is unchanged. A wait defaults to 60 seconds
(`--timeout` accepts milliseconds, seconds or minutes, maximum 60 minutes).
Addressed public chat (whole body name, case insensitive), whispers when emitted
by the running body, new/changed durable attention and explicitly watched job
completion/failure wake immediately. `--chatter none|addressed|all` controls chat;
`--from` narrows chat to a sender. Classification by a model is deferred.
Danger and individual disconnections wake only with their explicit flags;
exhausted reconnection (`:reconnect-failed`) wakes by default. This is a
momentary notification, not a new durable engine attention request.

Routine movement/action progress stays quiet. At timeout the tool returns a
bounded summary of job completions/failures, pickups, health/disconnection
outcomes and notices, or `{:wake :timeout :changed false}`. Summaries contain
counts and at most four examples, with `:more?` when examples are omitted.
The tool does not resolve attention requests or alter jobs/reflexes.

Each named observer stores its EDN cursor and semantic attention signatures
outside the engine state, in `worlds/<world>/observers/<body>/<observer>.edn`. The first
call establishes a baseline at the current event cursor, including outstanding
attention; later calls replay intervening events. Different observers are
independent; simultaneous waits using the same observer return `:observer-busy`.
There are at most 64 observer files per body and 4096 tracked attention requests;
exceeding either limit is explicit. Reads require write access to this tool state
folder. An initial baseline survives cancellation, while consumed cursors advance
only after output is accepted by stdout. Cancellation/crashes can replay already
printed events; delivery is at least once, not exactly once. `SIGINT`/`SIGTERM`
cancel and release the lock, and dead-process locks are reclaimed. Stream gaps
and engine start/restore events return a small `:reset` notice, once for a cursor older
than the restart (the notice carries no summary of the skipped old events; the next call
looks watched jobs up in the history, so a job submitted after the restart is still
reported). No cursor or UUID
appears in normal model-facing output.

The tool can only expose events the body emits. Older body builds without a
whisper listener do not expose incoming whispers; addressed public chat works
through the existing chat event. Restart a body with the current build when
upgrading its event support.

Live validation used the isolated `ObserveWaitTest` body on the test server,
with tool state under `/tmp/observe-wait-live`. Two-second quiet waits returned
31-byte EDN (32 bytes including the newline). Real addressed public chat and whispers woke about 70–80 ms after
injection in these samples; public banter remained quiet under `addressed` and
woke under `all`, while `none` suppressed whispers. A real wheat pickup produced
a timeout summary, and a running look-around job woke its watcher on completion.
The test used the primary checkout's existing engine build and concurrent
whisper-listener changes; this tool change contains no engine modifications.
The temporary body stopped and its added whitelist entry was removed. Durable
attention lifecycle, cancellation/replay, bounds and reset behavior are covered
by simulated API tests; a live failure injection did not create required attention
because the existing wait primitive normalized the malformed argument.

Use `--watch-action <request-id>` to wake when an explicitly tracked manual world
operation emits its action completion event. Both watcher flags accept repeated
options or comma-separated IDs (maximum 32 per kind):

```bash
node engine/tools/world.mjs Bob --world claude submit move-to 10 64 20 --request-id move-home
node engine/tools/observe.mjs Bob --world claude --wait --watch-action move-home
```

On a first-ever observer, explicitly watched job/action IDs are checked against
up to the last 1000 retained events in the current generation. Already completed
work returns immediately; unrelated historical chat stays quiet. If retention
prevents this lookup, the tool returns an explicit `:history-unavailable` reset.
Subsequent waits use the saved cursor. Action completion returns
`{:wake :action-finished :action "move-home" :result {...}}` with compact status,
reason and movement/outcome fields. Unwatched action events stay quiet. Addressed
chat and required attention retain their normal wake behavior while tracking an
action. Classification by a model remains deferred.

Completed job outcomes can be read back without reading engine files:

```bash
node engine/tools/observe.mjs Bob --world claude result j17
node engine/tools/observe.mjs Bob --world claude job j17
```

`result` reads the latest 1000 retained events of the current engine generation in
pages of at most 128 (halved when a page exceeds the response byte cap) under one
three-second deadline. `job` falls back to this read when the scheduler no longer
holds the job. The answer has the terminal `:status` and up to eight domain events
of the job and its children (internal scheduler events left out), such as
`search.done` with found coordinates and coverage; a watched `:job-finished` wake
carries the same `:events` and `:history`. This is recorded event evidence, not
every child's `result!` value; a job that emits no domain events returns its status
alone. `:history :partial`, `:events-truncated?` and `:job-history-unavailable`
make the retention and projection limits explicit. A first-ever watch of a job
found neither in that history nor in the scheduler returns
`{:wake :reset :reason :history-unavailable :jobs [...] :history-window 1000}` at once.
Waiting state and observer cursors stay in the tool.

For a one-shot look at nearby blocks and entities, submit the read-only
`jobs.explore.look` job and read its `look.observed` event back:

```bash
node engine/tools/jobs.mjs Bob --world claude submit '(jobs.explore.look {:radius 12 :block-names ["chest"] :max-blocks 6 :at [10 64 20]})' --front
node engine/tools/observe.mjs Bob --world claude result j17
```

World time can be read without choosing or starting a body:

```bash
node engine/tools/time.mjs --world claude clock
node engine/tools/time.mjs --world claude dawn --timeout 1200
```

`clock` returns compact EDN with `:time-of-day`, `:day?`, `:seen-at`, `:age-ms`
and `:by` from the newest fresh connected Overworld observer's `view/pose.json`.
It reads no legacy shared clock and never predicts time between reports. Reports
older than 90 seconds, disconnected bodies, other dimensions and malformed
reports are ignored; no usable report returns `{:ok false :reason :time-unknown}`.
`dawn` polls those same observations until day, loss of all usable reports, or
the timeout (default 1200 seconds, maximum 3600). Day uses the engine's exact
boundaries: before tick 12542 or after 23460. `--state DIR` selects another state
directory; `--poll-ms` controls dawn polling (default 1000). Both commands run
the ahead-of-time CLJS tools bundle directly in Node; build it once with
A picture of what a body sees, without the dashboard and without touching the body:

```bash
node engine/tools/snapshot.mjs Bob --world claude
node engine/tools/snapshot.mjs Bob --world claude --yaw 90 --pitch -20 --width 960 --height 540
node engine/tools/snapshot.mjs Bob --world claude --look-at 10,64,20
```

It draws the body's view with the software renderer (`tools/view/render.mjs`) from
the body's `view/pose.json` and the world's chunk dumps, writes
`<workspace>/snapshots/snap-<UTC time>.png` (the newest 20 are kept; standalone the
workspace is the body folder, in a generated workspace `bin/snapshot` binds its own
directory) and prints EDN with `:png` (relative to the workspace), `:facing`,
`:crosshair` (block, cell, face and distance of what the view's centre hits),
`:entities` (in the picture, nearest first, left/centre/right) and a one-line
`:text`. `--yaw`/`--pitch` (degrees; 0 north, 90 west, pitch up positive) and
`--look-at X,Y,Z` turn only the picture. A body that is offline or whose pose is
older than 10 s is refused with `:body-offline` (exit 1); bad options give
`:invalid-option` (exit 2). Logic is `agent-tools.snapshot` (cljs); the launcher
imports the renderer only when it draws.

`cd dashboard && npm run build-agent-tools`.

Agent job management uses the existing engine scheduler through an EDN API;
these commands return immediately, while `observe --wait --watch` handles wakeups:

```bash
node engine/tools/jobs.mjs Bob --world claude list
node engine/tools/jobs.mjs Bob --world claude show j17
node engine/tools/jobs.mjs Bob --world claude submit '(jobs.movement.go-to {:pos {:x 10 :y 64 :z 20}})'
node engine/tools/jobs.mjs Bob --world claude submit '(jobs.time.wait-for-day)' --hold
node engine/tools/jobs.mjs Bob --world claude interrupt '(jobs.movement.look-around {:every-ms 2000})'
node engine/tools/jobs.mjs Bob --world claude cancel j17
node engine/tools/jobs.mjs Bob --world claude cancel-all
node engine/tools/jobs.mjs Bob --world claude retry j17
node engine/tools/jobs.mjs Bob --world claude resolve attention-17 --reason handled
node engine/tools/observe.mjs Bob --world claude --wait --watch j17
```

Resolve accepts `handled` or `condition-recovered`; it only marks the durable
attention request and never retries or cancels its job. The command makes one
request. If transport confirmation is unknown, inspect outstanding attention
before deciding whether to send another.

Fast chat uses the body's existing server identity directly, independently of
job scheduling or a manual-control lease:

```bash
node engine/tools/say.mjs Bob --world claude "Hello, everyone!"
node engine/tools/say.mjs Bob --world claude --to Steve "I found the cave entrance."
```

Omitting `--to` sends public chat; supplying it whispers to that online player.
Messages are one line and capped at 256 characters (including the `/tell`
header budget for whispers); slash-prefixed messages and invalid recipients are
refused. Sends share the engine's rate limits with job chat. The command returns
`{:status "blocked" :reason "busy"}` if another chat send is queued, and
`{:status "disconnected"}` while the body is offline; direct messages are never
deferred until reconnect. It does not take over or interrupt the current job or
manual control.

Specs are native EDN lists parsed by `engine.expr`, including existing `seq`,
`any`, `repeat`, `hold` and `backoff` forms; they are never evaluated as code.
The server validates names/shape before changing any job or reserving a command.
Expressions are limited to 256 nodes/depth 24 and the CLI caps input at 12000
bytes. `GET /jobs?limit=8&offset=0` lists at most 32 jobs per page; `show` reuses
`GET /job` and removes bookkeeping metadata. `POST /jobs` handles only submit,
interrupt, cancel, cancel-all and retry. `submit --hold` sets `:hold? true`: the
listed job keeps the body while its check declines; an interrupt already holds
the body. `cancel-all` takes no job ID and cancels every listed job (including
queued, held and failed ones) through the engine's existing cancellation path;
reflexes stay registered. The engine owns scheduling, interruption, native
job/child memory and failure attention; the tool owns transport/request metadata.
Trigger edits are `POST /triggers` (Local event API).

An interrupt cuts a running listed job, queues the new job at the front holding
the body, and preserves the predecessor for resumption. Reflexes retain their
existing priority. If the interrupt fails, existing engine behavior parks it,
creates required attention, and resumes the predecessor. Cancellation removes
the native job's owned child memory; retry clears its failure and attention while
preserving job memory. No failure policy was changed. An exclusive manual lease
permits normal submit to queue but rejects interrupt with `:manual-control`.

Mutation requests carry a generated request ID and engine generation, hidden
from normal successful output. For deliberate retries, use the same
`--request-id <ID>` and exactly the same operation/arguments. The engine saves a
bounded ledger of the latest 128 command IDs with its normal state; it survives
ordinary restart. Duplicate requests return the original job ID/current status
without repeating the mutation; reuse with changed arguments is a conflict.
Before mutation the engine saves a pending reservation. If a crash/save failure
leaves it pending, retry returns `:request-uncertain` and does not execute again;
inspect jobs/attention before deciding another command. This prevents automatic
duplicate execution, rather than claiming an atomic transaction across memory
and engine files.

Job request parsing and EDN construction run in the ahead-of-time CLJS tools
bundle, built once with `cd dashboard && npm run build-agent-tools`; the Node
entry point retains the existing HTTP transport boundary.

The CLI retains original generation metadata for its latest 128 IDs under
`worlds/<world>/agents/<body>/.commands/jobs/` (outside engine/observer state), so reuse after a
fresh engine reset is rejected instead of silently applying to a different
body generation. Reusing an ID outside these bounded retention windows can
execute again; never treat an old ID as an unlimited deduplication guarantee.
Transport uncertainty prints the request ID and asks for a same-ID query/retry;
it never automatically resubmits with a new ID.

Live validation used an isolated `ObserveJobsTest` body: a repeat look-around
job ran, a one-shot interrupt completed and woke observe, and the original job
resumed. Cancellation stopped it. A go-to job without its target produced a
real parked failure/required attention, then retry and cancel cleared it.
Replaying the original submit ID created no additional job; the final queue was
empty. The owned body stopped and its added whitelist entry was removed.
Unit tests additionally verify saved dedupe across restore, pending uncertainty,
ledger bounds, lease protection, and failed-interrupt predecessor resumption.

Trigger management uses the existing `GET/POST /triggers` API. The external
command returns compact EDN immediately; the engine owns condition evaluation,
priority, mute/order expiry, and scheduling. It adds no observer state or engine
changes.

```bash
node engine/tools/triggers.mjs Bob --world claude list --limit 8
node engine/tools/triggers.mjs Bob --world claude show hungry
node engine/tools/triggers.mjs Bob --world claude add hungry --trigger hungry
node engine/tools/triggers.mjs Bob --world claude put near-home --when '(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))' --job '(jobs.movement.look-around)' --persistence cooldown --cooldown-s 30
node engine/tools/triggers.mjs Bob --world claude mute hungry --for 10m
node engine/tools/triggers.mjs Bob --world claude unmute hungry
node engine/tools/triggers.mjs Bob --world claude move near-home --before hungry --for 5m
node engine/tools/triggers.mjs Bob --world claude reset near-home --property position
node engine/tools/triggers.mjs Bob --world claude remove near-home
```

`add` and `put` both create or replace a custom entry. Replacement follows the
engine's upsert semantics: it retains existing mute/order changes but resets
condition, latch, cooldown, and backoff state. Built-in entries cannot be
replaced or removed; they can be muted or moved. Predefined triggers accept an
optional native EDN `--job` and `--args` map. Ad hoc conditions require `--job`
and take its arguments inside that list. Conditions use the engine's allowlisted
EDN interpreter; unknown functions, invalid arity and types return structured
refusals. They never evaluate arbitrary code. Missing facts leave a condition
inactive; `show` includes its current term explanation, including `:unknown`.

Mute prevents future activations and leaves a running reflex job alone. Removal
also lets its current round finish; the engine drops the orphan before another
round. `--for` accepts seconds or units such as `300ms`, `30s`, `10m`, `2h` and
`1d`; omitting it leaves the entry or change without expiry. `unmute` clears the
mute change, and `reset --property position` restores its original position.
The list retains registration order and shows each active entry's effective
`:priority` (1 fires first); muted entries have no firing priority.

Lists default to eight entries, with `--limit` up to 32 and `--offset` for the
next page. Pagination and compact projection happen in the tool: the current
API still sends the complete register, subject to a 256 KiB response cap and a
three-second request deadline. Detailed entries and compiler explanations are
bounded too. Each mutation checks the current engine generation immediately
before sending one request. This API does not deduplicate request IDs; the tool
never automatically retries an uncertain mutation. If confirmation is unknown,
inspect `show` or `list` before issuing it again, since a repeated put resets
entry state.

Live validation used an isolated `TriggerToolTest` body. Giving it bread fired
an inventory condition; mute suppressed future firing without cancelling its
active round, unmute resumed it, and mute expiry resumed it automatically.
Ordering, reset, temporary order expiry, built-in removal protection, missing
home explanations, invalid-condition refusal, and custom removal were verified.
The owned body stopped and its temporary whitelist entry was removed.

## Plan and blueprint tools

`plans.mjs` manages one world's EDN plans and `blueprints.mjs` manages the shared
EDN blueprint library. All four shared-world commands (`map`, `world-changes`,
`plans`, and `blueprints`) are implemented in ClojureScript under
`dashboard/src/agent_tools/`. Build the optimized Node library once with
`cd dashboard && npm run build-agent-tools`; calls do not compile on demand or
start a JVM. For development, `npm run build-agent-tools:dev` creates an
unoptimized build of the same library. Rebuild after changing its CLJS sources.
Each launcher calls `loadTools([...names])` with only the bundle exports it uses, so a stale bundle (or a new tool whose exports are not built yet) fails only that tool with `:build-required` naming its missing exports; `AGENT_TOOLS_BUNDLE` points the loader at a scratch bundle. The `.mjs` command paths remain stable, thin Node launchers; the existing Node
regression tests exercise these public boundaries independently of CLJS. These
tests live in `engine/test/tools/`; run them with
`cd engine && npm run test:agent-tools` (builds the library, then tests it).

The optimized build measured 41 ms median for a fresh Node process to load the
library, 89 ms for map search, and 59 ms for plan detail (five runs each, warm
filesystem cache). A saved-terrain check took 317 ms including its actual work.
These are measurements on the development machine, not latency guarantees.
Both tools require `--world`, return EDN, and cap ordinary pages at ten records
(up to 100 with `--limit`; use `--offset` for the next page). `--raw` includes
the exact stored EDN document and is capped at 64 KiB unless `--large` is
explicitly supplied. Geometry is omitted by default; `--geometry --large`
allows inspection up to the model's 200,000-cell limit.

```bash
node engine/tools/plans.mjs --world claude list
node engine/tools/plans.mjs --world claude find home
node engine/tools/plans.mjs --world claude show home --raw
node engine/tools/plans.mjs --world claude validate home --edn '{:id "home" :parts []}'
node engine/tools/plans.mjs --world claude check home --inventory '{:stone 24 :oak_planks 16}'
node engine/tools/plans.mjs --world claude remove home --by builder --revision <digest>
node engine/tools/plans.mjs --world claude edit home --edn '{:id "home" :parts []}' --by builder --revision <digest>
node engine/tools/blueprints.mjs --world claude find hut
node engine/tools/blueprints.mjs --world claude show starter-hut --raw
node engine/tools/blueprints.mjs --world claude validate hut --edn '{:id "hut" :front :north :key {"S" "stone" "." :clear} :layers [["S."]]}'
node engine/tools/blueprints.mjs --world claude save hut --edn '<blueprint-map>' --by builder
```

Plan mutations are atomic compare-and-swap writes. `add` is create-only;
`edit` and `remove` require the digest shown by `list` or `show`, so
the tool refuses to apply an intent based on stale observed data. Updating an
existing blueprint also requires its shown digest; the first save creates it.
`--dry-run` validates and previews without writing. Blueprint files are global across
worlds, so every result marks `:scope :global`; plans mark `:scope :world`.
Plans have no status: every submitted plan is active and takes part in conflicts
and engine footprints, a draft stays local until it is submitted, and `remove`
deletes a plan (there is no completed or retired state).

`validate` checks EDN structure and blueprint references without saving.
`check` compares a saved plan with previously dumped chunk columns. Its results
are evidence from those saved dumps, not from live loaded chunks; `:checked`
reports the dump coverage and ages. Undumped cells remain `:unknown`. Material
requirements count block names; `--inventory` supplies optional name counts so
the tool can report shortages. It does not infer crafting recipes, conversion
between non-block items and placed blocks, path reachability, or build safety.
The check also reports active-plan conflicts, overlapping zones, and explicit
social-claim overlaps. Social claims are independent annotations; they do not
change engine access rules. Dashboard plan summaries can remain cached for up
to ten seconds after a write.

Shared planning tools work directly with world files; they require no body,
engine, or running dashboard. Every command requires an explicit world. Mutations
also require an explicit author; edits/removals require the current content
revision from a read. Creation requires the ID to be absent.

```bash
node engine/tools/map.mjs --world claude find --type marker --text farm
node engine/tools/map.mjs --world claude find --place spawn --radius 128 --owner Alice
node engine/tools/map.mjs --world claude show marker home --raw
node engine/tools/map.mjs --world claude add marker home --by Alice --data '{:kind "base" :x 10 :y 64 :z 20}' --raw
node engine/tools/map.mjs --world claude edit marker home --by Alice --if-revision REV --data '{:note "shared supplies"}' --dry-run --raw
node engine/tools/map.mjs --world claude add zone garden --by Alice --data '{:min [10 63 20] :max [20 70 30] :allow #{:harvest}}'
node engine/tools/map.mjs --world claude add claim extension --by Alice --for 30m --data '{:min [21 63 20] :max [30 70 30]}'
node engine/tools/map.mjs --world claude renew claim extension --by Alice --if-revision REV --for 1h
node engine/tools/map.mjs --world claude release claim extension --by Alice --if-revision REV
node engine/tools/map.mjs --world claude remove marker home --by Alice --if-revision REV
node engine/tools/world-changes.mjs --world claude --wait --type claim --observer planner --timeout 30s --raw
```

`--revision` is an alias for `--if-revision`. `--preview` is an alias for
`--dry-run`; both validate and report changed fields without changing the object
or recording a mutation. Lists default to 10 and allow up to 100 with pagination.
An explicit center (`--center '[x y z]'`) or named marker enables a default
radius of 128 blocks; without a center the search has no implied body location.
Filters include type, owner, status and text. `--raw` on a read or mutation includes
the complete selected, current, proposed, or removed record, bounded to 64KiB
output; it never widens a query or exposes internal paths. Summary notes stop at
240 characters and set `:note-truncated?` when text was cut. Files are bounded to
8MiB, directories to 1000 plan/blueprint files, and each shared scan/list to
64MiB. Limits fail explicitly.

Markers preserve the existing `places.json` format and ownership policy:
substantive edits/removal belong to their author; another author may update a
note without taking ownership or moving the marker. Existing embedded structures
survive edits; this tool does not edit them. New markers declare `:source`
`"intent"` by default; authors may explicitly record `"observation"`, which is an
assertion they supply rather than a live-world verification. Existing unlabelled
markers read as unknown source. Zones use the canonical strict `zones.edn` schema
and engine permissions. The dashboard projects these to its map; `zones.json`
remains a display fallback only when canonical EDN is absent.

Claims are separate authored social intentions, not the engine's plan-derived
cell protections or zones. They default to 30 minutes, allow 100ms..7 days, and
retain explicit active/released status. An active claim overlapping another
owner's active claim is refused with up to 10 IDs, owners, boxes and expiry,
plus a total and `:more?` flag. The error includes a spatial `find --type claim`
command for remaining claims. Only its owner may renew, release, edit or remove
it. Expired/released claims stay
inspectable but are hidden from default searches and dashboard overlays; use
`--status expired` or `--status released` to find them. They do not authorize
world actions. The dashboard labels live claims distinctly with owner; overlay
records include expiry. Invalid canonical overlay files are reported through
world `:map-errors` instead of silently falling back to divergent JSON.

New tools hold a shared lock across read, revision check, validation and atomic
rename. Content revisions detect external edits between commands. A small
write-ahead journal recovers interrupted cooperating writes; global blueprints
share a global lock/journal across worlds and explicitly report global scope.
Manual file editors and legacy marker writers that lock only the final write
are outside this concurrency guarantee. They can race new tools; no automatic
mutation retries are performed. A recovery conflict preserves the external file
and reports the pending journal for inspection. A crashed stale-lock reclamation
guard requires inspection before removal. Dashboard plan summaries may take up
to 10 seconds to refresh after file changes.

`world-changes` stores a named cursor outside engine state, starts at the current
baseline, and returns grouped changes for marker/zone/claim/plan/global blueprint
objects. An explicit `--cursor EDN` avoids this saved observer state. Filters
suppress unrelated changes while advancing the cursor. A wait returns on a
matching change or its finite timeout; cancellation retains the prior checkpoint.
`--raw` requests the full bounded change records; pages remain limited to 100
result records. Changes include operation, revision, author, source and scope.
External writes are detected on polling and labelled external; intermediate external edits
between scans cannot be reconstructed. The ledger retains the latest 2048 changes
and reports `:cursor-gap` after loss/reset, with a new cursor. Output precedes
checkpointing, so crash ambiguity permits repeated delivery. This is a bounded
change feed, not a complete event-sourced database.
