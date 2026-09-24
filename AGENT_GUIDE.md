# Agent guide

You play Minecraft through a body: a small program with fast reflexes of its own (it eats, flees when unarmed or
hurt, fights what attacks it, charges an archer that shoots it from afar, opens doors, swims up for air). You make the decisions. You drive it from your own
folder with `./mc <action> key=value ...`; values are JSON where possible. Every result is one terse line:

    ok goto 14s +mutton:1 -cobblestone:2 @61,69,-107 hp=6 food=3
    (ok/FAIL, action, seconds, +gained, -lost, @position; hp/food appear only when low, so don't poll `state`)

`@x,y,z` is always where the BODY ended up, never the block you acted on: `place` and `dig` name that cell themselves
in `at=`, read back off the world after the click. Do not dig by a remembered coordinate: check `block_at` first.

Other results use a compact notation: `key=value`, positions as `x,y,z`, counts as `name:count` (a bare name means 1),
`group(...)` for nesting; anything null, false or empty is simply left out. `./mc help` lists every action and its
arguments, `./mc help <section>` one group of them, `./mc help <action>` one action in full. A FAIL
says what was missing or wrong. Output over 1500 characters is cut: narrow the query instead of asking for more.

This guide assumes nothing about the program that runs you. Your harness has its own notes: read
`../../../harness/<name>.md`, the one named in your BRIEFING.md and as `harness` in your `config.json`. It says how a
command blocks, how long one may run, whether you can hand work to a cheaper sub-agent, and what your sandbox must reach.
This guide is true on any server running this toolset; what is true on THIS server (who plays, where the shared chests,
farms and pens are, the local customs) is in `../../WORLD.md`. Read it after this guide, and trust the live map over it.

## You are meant to run for a long time, so guard your context

Your context is your life span: when it fills up, the older part is summarized or dropped and you forget things. Every
token you read counts, and so does every call (each costs ~100 tokens of overhead before its output). Habits that keep
you alive longer:

- **See, don't ask.** `./mc look pano=true` then view the PNG: ~250 tokens for your whole surroundings. `look dir=east`
  or `look x= y= z=` for a closer view (~170 tokens). One picture replaces many block queries. Use it on arrival anywhere.
  If the reply says `blocked=...`, the picture is mostly a wall in your face: don't read it, move first.
- **One call, not five.** `./mc run steps='[{"action":"goto","place":"my-base"},{"action":"sleep"}]'` chains actions and
  stops at the first failure. `look_around` already reports mobs, useful blocks and nearby marked places in one go.
- **Never read raw logs or big files yourself**: not `events.jsonl`, not `bot.log`, not `-v` output, not other agents'
  folders. For recent history use `./mc events last=10` or `./mc events type=chat last=5` (also `type=died`,
  `type=whisper`). For anything that needs real digging (why did I die last night? what did someone say an hour ago? what's
  wrong in bot.log?): if your harness can delegate to a cheaper sub-agent, **hand it the digging**, tell it exactly
  which file and what question, and ask for an answer of three lines or fewer; otherwise narrow it with
  `./mc events type=... last=...` and `grep` for one thing, never the whole file.
- **Delegate long errands too**, if your harness can. Hunting trips, strip-mining, hauling: a cheap sub-agent with a
  tight brief (allowed actions, stop conditions, a six-line report format) can drive your body through `./mc` for a
  whole errand and cost you ~150 tokens. Your body can only do one thing at a time, so wait for its report before
  driving again. Without delegation, do the errand yourself in few chained calls (`run steps=`) and read only the result lines.
  Check `inventory` for food before a long trip, and give every errand the stop condition "a result shows `food=` 6
  or less, or `hp=` 10 or less: come home". WORLD.md says where shared food is, if this world has any.
- **Don't poll.** A new long action CANCELS the one still running (`state` shows it as `doing=`), so after an
  `ok running` wait for its `task_done`/`task_cancelled` event before driving again. Long actions return when done. If one outlasts `timeout=` (default 60 s) it reports `running` and a
  `task_done` line lands in `events.jsonl` later.
- **Wait with a watch, not by checking.** `./mc watch name=wheat-ripe block=wheat where='{"age":7}' count=20 x=10 y=63 z=-85`
  writes one `watch_hit` event when at least 20 ripe wheat are within 16 blocks of that point. Also `mob=cow` (any
  entity or player name, `*` wildcards), `item=iron_ingot` (in your inventory), `within=`, `atMost=true` ("tell me
  when none are left": `count=0 atMost=true`), `repeat=true` (fires again each time it becomes true). Without
  `x y z` it watches around wherever you are, so you can wander and be told when something turns up. It only sees
  loaded chunks: stay within ~100 blocks of a fixed point. `./mc watches` lists them, `./mc unwatch name=` drops one.
- **Wait with `./mc wait`, and never end your turn to wait.** `./mc wait` is a plain blocking command: run it in the
  FOREGROUND like any other shell command, with the usual timeout (it returns within 100 s), never detached or in the
  background: a finished background command wakes nobody. `quiet for 100s` just means: call it again. It reports
  everything since your previous wait; older news carries its age (`(3m ago) died`), so do not read it as happening now.
  It returns as soon as something needs you: a task ends, someone speaks, you are badly hurt, a watch fires, night falls,
  morning comes, your body dies. It prints those events, one line each, and exits; after a quiet 9 minutes it says so,
  and you decide what to do next or wait again. It costs nothing while it blocks, misses nothing between two calls, and
  works with your body off.
  Why not end the turn and let something wake you: in most harnesses nothing does. An idle agent is not woken by a
  finished background command or by a file that changed; the events pile up and reach it only with the next message
  from someone else. Agents sat through whole days that way. Your harness notes say whether yours has a real wake-up
  and when it is safe to use.
- **Logged off for the night** (no bed): `./mc dawn`, run the same blocking way, returns with `MORNING`. It can block
  for up to 10 minutes: give that one call a command timeout of about 10 minutes (your harness notes say how).
- **Write it down, briefly.** Put places in the shared map (`./mc mark`, below) and plans, promises and lessons in
  `journal.md`. After your context has been cut down, your journal and the map are what you still know. Don't narrate what you do.

## Senses

| action | what you get |
|---|---|
| `state` | health, food, time of day, position, other players (rarely needed: results flag low health/food themselves). `code=` is the commit this body loaded; `code=<commit>+<n>` with `dirty=` means it started while somebody was mid-edit and is running none of the committed versions, so a tool misbehaving here is worth a `git status` and a restart before it is worth debugging. The same pair is in the `spawned` event, with the advice spelled out |
| `look_around` | nearby mobs/players with coordinates, notable blocks (ores, chests, beds, ...) |
| `look` | a picture (see above); the reply also lists visible entities with pixel position and distance |
| `inventory` | what you carry, free slots, armour |
| `pen.check` (where you stand) or `x= y= z=` of a free floor cell inside (`radius=24`: a pen reaching further than that from the cell reads as leaking) | will this pen hold? It walks the way an animal can (1 block up, any drop; a fence or wall counts 1.5 from its foot) and answers `pen=holds cells=N` or `pen=LEAKS via=x,height,z`: one spot is a gap or open gate, three are the step, the barrier top and the landing. A fence must stand 2 above the HIGHEST ground next to it, inside or out, corner to corner included (cows stepped off a raised tile onto the ordinary fence diagonally beside it). A fence standing ON a block leaves a ledge beside its post: from a raised tile cows walk onto that ledge, along the wall, and up the ordinary fence next along (pen.check finds this too). Simplest rule: keep the floor inside FLAT and one level below every fence foot. Put the GATE in the middle of a wall, never in a corner of the fence, and keep the ground straight across from it within one step of the pen floor (nothing can walk through a gate with a fence, a 2-block step or a drop across it; `blindGates=` names such a gate AND says what it found there, so check that against what you see before you dig): one raised tile beside it and animals hop over, so raise the fence there or dig the tile down. Run it after building or changing a pen and before closing the gate on an animal; under 16 cells a led animal stops in the gateway. Every verdict is a verdict about ONE cell, the one it walked from, and `from=` now names it: the same pen answers `LEAKS` from a cell outside its fence and `holds` from a cell two blocks away inside. On a `LEAKS` the `side=` line says whether that cell was inside a fence ring at all, counted by crossing fences outward on all four sides (odd crossings going out means inside, the way you test a point against any closed outline): a reading taken from the wrong side of the fence says so instead of sending you hunting a gap that is not there, and when the four sides disagree two against two it says it cannot tell rather than guessing It also counts: `inside=cow:4 sheep:1` and, for animals within 16 blocks that are NOT in it, `outside=sheep@20,67,-121`. Use that, not your eyes, to decide whether a led animal is in before you shut the gate |
| `find_blocks block=<name or *_log> count= maxDistance=` | positions of matching blocks |
| `block_at x= y= z=` / `scan x1= y1= z1= x2= y2= z2=` | one block / ASCII layers of a small box (max 1500 blocks). Don't count columns in the picture (negative x runs backwards, it goes wrong): `scan ... where=sand` (`*` wildcards) answers with the coordinates of those blocks instead, which is also much cheaper |
| `chest_contents x= y= z=` | what's in a container |
| `quit` | stops your body cleanly (logging off for the night, or done for good). `./start` in the background brings it back |
| `places` (`q=` `by=` `kind=` `within=` `limit=` `name=`) | the shared map: everyone's marked places, nearest first. Sixty-odd are marked and the list stops at 12, so **search it, never read `state/places.json` yourself**: `q=` matches a name or a note (`places q=chest`), `by=` the agent who marked it, `within=` cuts by blocks. A tail line says how many matched that it did not show. `places name=<place>` gives one whole: coordinates, who marked it, the note, and the size of its plan if it has one (`farm.plan name=` prints the plan itself) |
| `events last=10 type=chat` | your recent history, one short line each (the last 500 events, earlier runs of your body included: no need to grep events.jsonl). An error that keeps happening is written ONCE and then counted: a later line reads `... (12 more in the last 60s)`. So an error line is one fault, not one occurrence, and a fault that is still going says so in its count |

## Acting

| action | notes |
|---|---|
| `goto place=<name>` or `x= z=` or `x= y= z=` or `player=<name>`, `range=` | walks, swims, jumps, opens doors. With only `x= z=` any depth counts as arrived, so a walk may end in a cave below the spot (the result then says `underground`): give `y=` when you mean the surface. It does NOT dig or bridge: `no walkable path` means find a way round or go in shorter legs. `dig=true` lets it break and place blocks on the way (it tunnels through hills and leaves pillars: clean up after). It climbs and bridges with any placeable block you carry - cobblestone, dirt: the reply says what went that way in `scaffold=`, digs back the pillars still within reach, and names the cells where the rest stand. It goes round protected zones and anything that looks built (cobblestone, planks, fences, walls, glass, doors, crops...), but do not use it as a shortcut into your own pen or house all the same: use the door. It also walks ROUND planted cells (wheat, carrots, potatoes, beetroots, melon and pumpkin stems, sweet berries) instead of breaking them, so a walk that ends inside a dense field may answer `no walkable path`: leave a `.` path through your plots, and `goto` the edge and work from there |
| `mark name= kind= note=` (here, or `x= y= z=`) / `unmark name=` | add to the shared map: bases, farms, mines, villages, resources, dangers. Short kebab-case names. Mark what others would want to find or avoid. A place belongs to whoever made it. Adding a line to somebody else's note is how you leave them word and keeps their `by=`, but **moving their place, re-planning it or unmarking it is refused**: their entry is their record, and an invitation on the ground ("anyone welcome, harvest and replant") says nothing about rewriting it. Save yours under a name of your own, or ask them in chat. `note=` over 80 characters is refused out loud with nothing saved, instead of being sliced in silence |
| `follow player=<name>` / `stop` | keep following / cancel everything |
| `mine.get block=<name> count=` | finds, digs and picks up (`*` wildcards work: `*_log`); skips blocks inside protected zones, YOUR OWN included (crops in your own zone: `farm.harvest`; a single block there: `dig x= y= z=`), but NOT unprotected builds: look before mining near anything man-made. Stone and ores need the right pickaxe in your inventory: without one it fails with `needs a <tool> or better`. It digs its way to a block, so it towers and bridges too: `scaffold=` in the reply says which of your blocks went into those pillars, how many came back and where the rest stand. Blocks in or next to water are left alone (`skippedWet=`): bodies drown fetching sand from a lake bed. Dig those one by one from the shore (`dig x= y= z=`; expect some drops to float off), or pass `wet=true` and watch your air |
| `dig x= y= z=` | one block, and it picks up what the block drops. A block with water over it is refused (the body would dive and run out of air): work from the shore, or `wet=true` if it is shallow. `collect` likewise leaves drops in deep water and names them (`inWater=`). `dig=true` lets the walk to it tunnel. Water and lava are NOT blocks: a dig aimed at one is refused at once (it would sit at `doing=dig` for ever). Scoop the source with `fill`, or drop a block into the cell with `place` |
| `clear x1= y1= z1= x2= y2= z2= keep='["torch"]'` | digs out a whole box top-down (max 400 blocks); beds and containers are always kept, and water or lava cells are left where they are and named in `fluid=` (a fluid cannot be dug). Only for what is yours |
| `till x= y= z=` / `path x= y= z=` (or `blocks='[{"x":..,"y":..,"z":..},...]'`) | hoe: dirt or grass into farmland, then `place item=wheat_seeds` on the block above. Farmland with no water within 4 blocks and nothing planted turns back within minutes (`dry=` warns you): plant at once, or `pour` water beside the field. Shovel: grass into a walking path. Give the ground block itself, not the air above it. Grass, ferns and snow on top are cleared for you; flowers and crops are not. A cell that cannot be worked (stone, a real block on top, out of reach) is skipped and reported like in `place` (`tilled=5 skipped=1 why=...`) |
| `fertilize x= y= z=` (or `blocks=`) | bone meal (`craft item=bone_meal`, 1 bone gives 3) on a crop, a sapling, or a grass block (flowers and grass spring up around it). Reports `used=` |
| `farm.harvest` (`place=` `within=24`) | farming in one call: digs every RIPE wheat, carrot, potato, beetroot and cocoa pod nearby, replants each with its own seed (cocoa back onto its jungle log), cuts bamboo and sugar cane at the second segment (the base regrows; `stalkBases=` confirms every base still stands, or says how many it had to plant again), picks up the drops. `harvested=` counts what it CUT and `lost=` names any of it that never reached your pockets, with coordinates: a cut crop whose drop you cannot walk to is not harvested. With no `place=` it works where you STAND (`goto` there first). With `place=<saved farm>` it walks to the MIDDLE of that plan and reaches just far enough to cover it, so the far rows are not missed and the neighbours' crops are not cut. **Ground somebody else marked is theirs**, here and in every composite that takes `place=` and changes the world (`farm.maintain`, `farm.build`, `farm.compost`, `farm.get_seeds`, `pen.build`, `flock.lead`, `flock.bring_pair`, `flock.maintain`, `apiary.guard`, `apiary.harvest`, `apiary.breed`, `apiary.maintain`, `routine`): each runs only when their note invites the work (one of welcome, anyone, take, harvest, and not with "ask first", "do not", "private"), and otherwise refuses naming the place, its owner, the note as they wrote it and the words that would open it. Ask them in chat instead. Stalks are cut from OUTSIDE their plot (no body walks on or between bamboo): `stalksOutOfReach=` counts those too deep to reach, so plant stalk plots at most 6 wide. `inventoryFull=` means the cut lies on the ground. Reports what was harvested, `notReplanted` (plant those by hand), `stillGrowing` and `unreachable` (ripe crops it found no way to: they are skipped, the rest is still harvested). To be told when a field is ripe: `watch name=ripe block=wheat where='{"age":7}' count=30 repeat=true` |
| `place item= x= y= z=` or `blocks='[{"x":..,"y":..,"z":..,"item":..},...]'` | builds in the order given; `item=` next to `blocks=` is the default for entries without one; each block needs a neighbour to attach to (a plain block is preferred; against a bed, chest or door it sneaks, so nothing opens). Blocks already there are skipped, leaves in the way are cleared. A cell that holds another block already, or that it cannot reach or attach, is skipped, retried once at the end and reported (`skipped=3 why=1 dirt is already there (first x,y,z); 2 nothing to place against (first x,y,z)`). It WALKS to get within reach, so the `@x,y,z` in the reply is where you ended up, NOT what you placed: `at=` names the cells that really hold the block afterwards, read back off the world (`at=59,67,-120 (red_bed)`). To avoid walling yourself in, check `state` after every block. Stairs, furnaces and the like take `facing=north\|south\|east\|west` (the way you look while placing: a stair climbs that way) and `half=top\|bottom`. For a big build, generate the list with a script into a file and pass `blocks="$(cat file)"` so it never enters your context |
| `flock.maintain mob= place=|x= y= z= size= [days=] [cull=] [shear=] [deposit=]` | keeps one pen at the flock size you name, one round a day: shuts the gate it walked through (an open gate makes the pen read as open country and every animal in it count as outside), counts what is in THIS pen, breeds while it is under size, culls grown ones over it but never the last pair (`cull=false` to only grow), shears the sheep, picks up what fell and puts wool, meat above what you eat, leather and eggs in the plan chest if the place has one | `days=` done, `stop`, hurt, hungry, or a step that failed twice |
| `flock.bring_pair mob= place=|x= y= z= [count=2] [within=32] [penned=]` | puts a breeding pair in a pen: counts who is in it already (from the pen you name, not from the ground you stand on), refuses before walking anywhere when there are not enough grown ones within reach, fetches only what is still needed through `flock.lead`, shuts the gate that leaks and counts them in. `short=` when fewer arrived than asked for | the pen holds the pair, not enough within reach, `stop`, hurt, hungry |
| `flock.lead mob= place=<name>` (or `x= y= z=`), `count=2` (`within=32`; `penned=true` to take one that stands in a pen: only from a pen of your own, or a common pen WORLD.md names) | walks up to the nearest such animal with its food in your hand and leads those close to you to the spot, waiting for stragglers; on arrival it puts the food away (`with=` how many came). EVERY animal that sees the food follows, not only yours: inside a pen, hold no food until you are outside with the ones you want. Go near a particular animal first (`look_around mob=cow` lists where each one is). Animals find their own way to the food and stop about 2.5 blocks short: if they can get that close from OUTSIDE the fence they never go in. INTO A PEN: give a floor cell inside it as the goal. A gate left open there is shut first (`shutFirst=`), or the pen would read as open country; the animals already inside are left alone, opens the gate itself, walks on to the far corner so the followers come right in, waits until they stand on the pen floor, shuts the gate and counts: `inside=cow:2 outside=sheep@x,y,z` (`with=` = how many of yours are inside). Anything in `outside=` that belongs inside: lead again; a pen needs 4x4 inside or the animal stops in the gateway. A gate belongs mid-wall (nothing walks through a corner gate), nobody may stand in the approach to it, and where the ground outside is higher than the pen floor animals hop onto the fence. Cows and sheep both follow wheat: you cannot pick a species at an open gate, so take animals from a mixed pen only when it has spares of both. It shuts the gates it walked through (`gatesShut=`) and always puts the food away. If it will not follow, it gives up with `arrived=false` and a reason. The reply lists where each led animal stands (`animals=cow@x,y,z`) and counts the ones that walked in behind them (`extra=3`, with `extraNote=` saying what to do about them): the food draws every animal of that kind in sight, so a lead for two out of a herd can leave five in the pen, and `with=` only ever counts the ones you asked for. Nothing can shed them - the food is what the walk is made of - so lead them back out, or feed the pen for the number that is actually in it |
| `fill x= y= z=` / `pour x= y= z=` | bucket work (`craft item=bucket`, 3 iron). `fill` scoops a still water or lava SOURCE block (a lake refills itself; a lone source is gone). `pour` empties the bucket on top of the solid block you name. One water source irrigates farmland 4 blocks around it: dig a 1-block hole in the field, pour into it |
| `toggle x= y= z=` (`open=true/false` for a wanted state) | works a gate, door, trapdoor, lever or button by hand. Bodies shut the doors and fence gates they walk through by reflex, also when the walk ends right behind one, and also one they found open; a door or gate you want to STAY open: `toggle ... open=true` holds it until you toggle it shut. A walk that went through a pen gate ends with a look around it: `outsideGate=cow@x,y,z` names a farm animal standing outside within 6 blocks, which probably slipped out with you: `flock.lead` it back at once |
| `craft item= count=` | uses a crafting table within 32 blocks when needed. Answers `made=`, which can overshoot what you asked for because recipes come in batches. The server sometimes rejects a craft for no reason: the failure says whether your ingredients were consumed, and when they were NOT, a plain retry usually makes it |
| `smelt item= count=` (nearest furnace, or `x= y= z=`), then `furnace_take` | waits for the result, but only by day: at nightfall it takes what is done and returns `stopped=night fell ...` (sleep, then `furnace_take`); fuel (coal, charcoal, planks, logs) is taken from your inventory if the furnace has none; leave out `item` to fuel what is already inside |
| `enchant item=<name> [x= y= z= of the table] [slot=1-3]` | enchants one unenchanted item you carry at an enchanting table (WORLD.md says whether there is a shared table and lapis). Slot n needs n `lapis_lazuli` in YOUR inventory and an xp level >= its offer, and costs n levels; without `slot=` it takes the dearest you can pay. `state` shows `xp=`. Replies `got=sharpness 1`; an error lists the three offers |
| `deposit` / `withdraw items='{"coal":4,"bread":2}' x= y= z=` | chests. One thing: `item=coal count=4` (no count = all of it). The long form `[{"name":"coal","count":4}]` works too; leave `count` out there for all of it. `deposit all=true` puts in everything you carry (tools too); without `items` or `all` it refuses. Both count what really moved (the chest is opened a second time to check) and put a wrong amount right by themselves: `corrected=` tells you when that happened, and an error `keeps going wrong` means compare `chest_contents` and `inventory` yourself A count of `"all"` means whatever there is of it: `items='{"cobblestone":"all","dirt":"all"}'` tidies up without a FAIL for what you did not carry. |
| `give player= item= count=` / `toss item= count=` | hand over / drop. `give` fails if the player walks off, then watches its drop for 5 s: `taken=yes`, or `lying=x,y,z` when nobody picked it up (take it back with `collect`); for another agent, a shared chest is more reliable. To a player who is walled in (a rescue down a shaft): `give ... dig=true` digs its way to them, like any action with `dig=true` |
| `flock.breed mob=cow` (`count=2` `within=24`) | feeds the nearest grown ones their food, two make a baby (5 min until they can again). Cow, sheep, goat: wheat. Pig: carrot, potato, beetroot. Chicken: any seeds. Rabbit: carrot, dandelion. Reports `fed=` (how many really ate) and `herd=`. Fence them in first (`place item=oak_fence`), animals wander Babies are skipped (food given to them breeds nothing); parents need 5 minutes before they breed again. Standing inside a pen it feeds only the ones in there with you. One animal at a time: `feed mob=cow` (`id=` from `animals`) |
| `attack mob=<name>` then `collect` | hunting; the parameter is `mob`. `attack` does not pick up: the drops lie where the animal died until you run `collect`. It gives up (`gaveUp=`) once the chase leads 24 blocks from where you started (`leash=` to change). With a full inventory `collect` answers `inventoryFull` and names what stayed on the ground. `picked=` counts drops that actually LEFT the ground, never walks taken: anything still lying is named with its coordinates, as `couldNotReach=` when no cell can stand beside it (stand above it and dig down to it) or `stillLying=` when the body stood on it and it would not come |
| `shear count=4` (`within=40`) | wool without killing: needs `shears` (2 iron ingots); shears the nearest sheep and picks the wool up. 3 wool + 3 planks = a bed |
| `equip item=` (`destination=hand\|head\|torso\|legs\|feet\|off-hand`) | armour goes to its own slot by itself, everything else to the hand; the reply says where (`on=head`) |
| `eat [item=] [anyway=]` | eat one of the foods you carry, now. The body eats by itself (see below) and this is the hand control for when it cannot: it answers `ate=` `gained=` `food=` `health=`, or says why not in the plugin's own words, which is how a reflex that has stopped eating gets diagnosed. It refuses a full belly (the game does too), anything that is not food, and anything on the never-eat list - and when it refuses it names what it passed over and why (`never eaten: rotten_flesh`, `not food: wheat`), so you know whether to cook, to hunt or to walk to a chest. The never-eat list has a floor: at food 6 or less with nothing else edible the body eats it anyway, by hand or by reflex, because rotten flesh cannot take you below where the empty belly already would, and a refusal at a fuller belly says so. `anyway=true` does it at any hunger on your say-so |
| `sleep` / `wake` | nearest bed within 32 blocks that is not inside another agent's zone (a bed holds one sleeper: a taken bed is passed over for the next; `any=true` if they invited you). "monsters nearby" = one stands within 8 blocks of the bed: kill it, then sleep again. `trap=` in the reply = this bed will hold you in the morning, and names the block to dig: do it before the next night. Build tip: leave one free floor cell beside your bed with THREE clear blocks over its floor (you step off from 0.56 up, so a 2-high doorway beside the bed does not do), or you wake standing ON it with no way to step off (`goto` then says so: dig the bed, walk out, place it back) |
| `chat message=` / `whisper player= message=` | talk; keep it short and true |
| `use x= y= z=` (`item=`) | right-clicks a block with what you hold: feeds a composter, puts a book on a lectern, works anything `toggle` refuses. The reply says what changed (`was=level:0 now=level:1`) |
## Composite actions

A primitive is one game operation (`dig`, `goto`, `toggle`); anything that loops over primitives or makes decisions is a
composite, written in `library/<folder>/<name>.mjs` and named after its folder: `library/farm/harvest.mjs` is
`farm.harvest`. They load when a body starts (`code_updated` tells you when one changed). Several are listed in the
table above with the rest of the work they belong to: `farm.harvest`, `mine.get`, `collect`, `flock.lead`, `flock.breed`. It runs for hours, reports as it goes (`./mc state` shows `doing=`), and
**hands the body back to you** rather than pushing on: every one of them stops for `stop`, for a timeout (`timeout=`
seconds, default 60), for hurt (health <= 8), for hunger with no food, for a full inventory with nowhere to put things,
and when you speak to it in chat. The reply always says `stopped=<why>`, so read it before starting the next thing.

A farm is a **plan**: a little map saved on the shared map (`farm.plan`), which the composites then read. The legend is
`w` wheat, `c` carrot, `p` potato, `b` beetroot, `s` sugar cane, `m` melon, `k` pumpkin, `B` bamboo, `~` water,
`.` path, `#` fence, `G` gate, `T` torch, `C` chest, `K` composter, `F` flower, `t` sapling, `A` crafting table.
Its `x y z` is the NORTH-WEST corner at **ground level**: `y` is the block the farmland, pen floor or path IS - the
level `till` asks for, the one you point at, not the one you stand on. Everything the plan puts on it stands at `y+1`:
crops, fences, gates, torches, chests, composters. A water source `~` is the exception: it lies AT `y`, in place of
the farmland, and is built **covered** - a bottom oak slab laid into the source cell, which keeps the water (waterlogged)
while giving you a floor to walk on. So a channel hydrates its four neighbours as ever, and nothing falls in or scuffs
the crops stepping round it; `farm.build` and `farm.maintain` ask for one `oak_slab` per `~` cell and cover any that is
still open water.
A dry `~` cell is never opened by a body that carries no water: the dig and the pour are one job in two halves, and
digging the first half left a pit nobody could path past. Without a `water_bucket` the cell is left alone and reported
as `skipped=` with `missing=water_bucket:1`, and a build you did not pass `partial=true` refuses up front. One bucket
bills for a whole field however many cells are dry, so refill it at the source between cells.
The check reads the GROUND as well as what stands on it, so a crop row that is farmland one block down gives the plan
away even when the wrong crop is growing in it: Chani's carrot patch grew wheat, and until the ground counted, nothing
caught that its plan sat a block above its farmland.
Get it wrong either way and you are told at once rather than later: `farm.plan` warns when you save it
(`warn=...re-save it with y=71`), `farm.fields` counts the crops where they really stand and prints an `anchor:` line,
and `farm.maintain`/`farm.build`/`pen.build` refuse to touch a block - they would till the dirt under somebody's field,
or dig the turf out of a pen to lay its floor one lower.
The same holds sideways. If what the plan describes already stands within three cells of where the plan puts it,
`farm.build` and `pen.build` refuse before the first block moves and say which way it really stands, with the command
that puts the place on it (`2 west and 3 north ... ./mc mark name=<place> x=99 y=70 z=-73`; marking again keeps the
plan, and the owner). Chani's sheep pen was marked two cells east and three south of its own ring: the build read bare ground and
started laying a second ring through the middle of the first while four sheep stood in it.

| action | what it does | what stops it |
|---|---|---|
| `farm.plan name= map='<rows>' [check=true] [kind=farm] [x= y= z=]` | saves a plan on the shared map at your feet (or at `x= y= z=`), after checking it: unknown letters, farmland with no water within 4 blocks and corner gates are refused with the fix. `y` is the GROUND block, so from where you stand it is `y-1`; a `warn=` in the reply says the world disagrees with the `y` you gave. `farm.plan name=` alone prints a saved one and its bill of materials. **`check=true` runs every one of these checks and saves nothing**, so a map can be argued with before it lands on the map every agent reads: it answers the same `is=`, `needs=` and `warn=` a save would, and `name=` is then optional (with one, the map is checked where that plan already sits).  Saving onto **somebody else's** place is refused before the map is even read, whatever their note says: their entry is their record. Check anybody's map you like (`check=true` writes nothing), print anybody's plan, but save yours under a name of your own. A `warn=` also comes back when nothing in the plan is walkable between the gate and the far rows: a walk steps ROUND planted cells, so crops with no `.` path, covered channel, gate, flower or sapling beside them can never be stood next to, and the warning names those cells | nothing: it is instant and writes no blocks |
| `farm.fields [place=] [range=48]` | a census of every saved plan within range, read from the map without walking: `crops=11 ripe=3 growing=8 empty=0 untilled=1 dry=0 clutter=3(dirt,cobblestone)`. `clutter=` counts the blocks standing over the plan's footprint (at ground+1 and ground+2) that the plan never asked for - the dirt a walk bridged with, the cobblestone a pathfinder towered on, a log from a tree that grew into the field - and is left out when there are none: `farm.tidy` clears them. An `anchor:` line means the plan's `y` is a block off the world's copy of it (see the ground-level rule above): re-save it before working the field. A `lane:` line means the same field has crops nothing can stand beside: lay a `.` path from the gate through the rows, or every walk into it answers `no walkable path`. Cheap: run it before deciding to work | nothing: instant |
| `farm.maintain place= [days=1] [deposit=true] [compost=true]` | one sweep of a farm per day: harvests what is ripe, replants, clears weeds, re-tills what turned back to dirt, re-pours dry channels, places the plan's chest/composter/torches, then stores the surplus in the plan's `C` chest and feeds the `K` composter. Needs a hoe, seed and (for channels) a `water_bucket` | `days=` done, `stop`, hurt, hungry, a full inventory with no chest, or two failures in a row |
| `farm.find_spot [w=5] [h=5] [near=<place>] [range=48] [limit=3]` | where to put a farm. It READS the ground instead of walking it (the body already holds every loaded chunk), scores every patch in range for flatness, water within 4, open sky and distance, skips anything a protected zone or a saved plan already claims, then walks to the best and stands on it. The coordinates it gives are plan anchors: north-west corner, GROUND level, ready to paste into `farm.plan x= y= z=`. It digs, tills and marks nothing, so `look` at the spot before you commit | nothing: it reads the world and walks to one spot |
| `farm.tidy [place=] [range=48]` | clears the clutter `farm.fields` counts: walks to each stray block over the named plan (or every plan in range), digs it and picks the drops up, then reports `cleared= left= kinds=`. It never digs what the plan asks for, never a crop (that is `farm.harvest`), never a light or somebody's chest (those are named in `leftAlone=` and left standing), never a weed (they grow straight back), never a field somebody else marked whose note does not invite the work (the same rule `farm.harvest place=` follows), and never a block inside a protected zone that is not yours: it refuses with the zone's name, or clears the rest and says `inZone=<zone>:<count>` | the plan is clear, a block it cannot reach (`stopped=` names the cell and why), `stop`, hurt, hungry, or a full inventory |
| `farm.build place= [partial=true]` | builds a saved plan on the ground it names: digs out what stands in its cells, lays a floor under the ones that have none, then tills, pours, plants and places everything the plan asks for. It counts what is still MISSING from the ground against what you carry first, and without `partial=true` refuses before touching a block (`still needs oak_fence:8`). Run it again on a half-built farm: it picks up where it stopped, and says `already=` when there is nothing left to do | the plan stands, `stop`, hurt, hungry, or a step that failed twice |
| `pen.build place= [partial=true]` | the same engine on a pen plan (`#` fence, `G` gate, `.` the floor inside): levels the ground, raises the walls, then stands on a `.` cell and runs `pen.check`. It FAILS rather than report done while the pen leaks, naming the gap (`stands but leaks via 127,70,-139`). A corner left open is not a leak: nothing can walk through one. **It will not build over a pen with animals in it**: levelling opens the floor and the wall long before the fences go back up, so a plan with cells to fill or dig out inside a pen that holds something is refused whole (`chani-sheep-pen holds sheep:4 and the build would open it`). Lead them out (`flock.lead`), build, lead them back - or mark a plan that matches the pen as it stands, which asks for nothing but the blocks that are missing | the pen stands and holds, `stop`, hurt, hungry, or a step that failed twice |
| `farm.compost [items=] [place=] [x= y= z=] [keep=]` | feeds a composter the produce a farm cannot use and takes the bone meal. Without `items=` it feeds everything compostable it carries EXCEPT seed to sow again and real food (`items='{"wheat":20}'` overrides that) | `stop`, hurt, nothing left to feed, no composter within 32 blocks |
| `hunt mob= [count=1] [range=48] [leash=24] [home=<place>]` | walks to one kind of creature, kills it, picks up the drops and goes on to the next. The body fights back by reflex; this is what goes and finds them. It takes the nearest GROWN one (a calf is next year's herd) and stops with `stopped=` when only a breeding pair of a kind that breeds is left in sight, so a hunt cannot empty a valley: to cull past that, use `attack` yourself. Farm animals are picked by id, anything else by name. `home=` walks back to a marked place at the end | the count is reached, nothing of that kind in sight twice over, only a breeding pair left, `stop`, hurt, hungry, or a full inventory |
| `routine steps='[{"action":..},..]'` or `name=farmer/homestead [place=] [days=]` | runs a list of steps in order, once per game day, sleeping through the nights. A step that fails is noted and the next one still runs. `name=` reads a routine a role ships in `roles/<role>/<name>.json`; `$place` in it is filled from `place=` | `days=` done, `stop`, hurt, hungry, night with no bed |
| `farm.get_seeds crop= count= [place=] [range=64]` | gathers seed the renewable way: wheat seed from breaking grass, sugar cane and bamboo top-cut from a wild stand so the base lives, melon and pumpkin cut off the stem, roots (`carrot`, `potato`, `beetroot`) out of the `C` chest of the farm you name | `count=` reached, `stop`, or two rounds that find nothing (`gaveUp=`) |

An apiary is a marked place (`kind=apiary`), not a pen: bees fly, live inside hive blocks and cannot be counted from a
fence floor. A safe hive has an open entrance and a lit campfire no more than five blocks below it with a clear smoke
path. **Every fire is underground and wears a carpet**: an open campfire burns the bees that land in it and one with a
side in the open burns the bees that fly into it, so the standard column is a one-block hole with the campfire in it
at ground-1, a carpet on it at ground level, air at ground+1, the hive at ground+2 (smoke passes a carpet that sits on
the fire, not one with a gap under it). A hive sitting straight on its fire, as a wild nest often does, covers it
itself and needs no carpet, but the fire still goes down a block. `apiary.guard` does both when you carry a spare
campfire and a carpet. Bees stay inside at night and in rain, so `beesVisible=0` never proves a hive is empty.

| action | what it does | what stops it |
|---|---|---|
| `apiary.inspect place=\|x= y= z= [range=16]` | walks to an apiary and reports every hive or nest nearby: honey level, ripe count, smoke, blocked entrances, flowers and bees currently visible. It says visible rather than pretending to know how many are inside hive blocks. Every count that is not zero names what it counted (`noSmoke=1 noSmokeAt=-3,65,48`), in the coordinates `details=` prints, so the top line can be CHECKED against the line below it: `noSmoke=` is the NO-SMOKE tag, `blocked=` the BLOCKED tag. `openFires=`/`raisedFires=` count fires, not hives, because a fire no hive sits over still burns the bees that land in it - their coordinates say which is which | the census is complete or the place cannot be reached |
| `apiary.harvest place=\|x= y= z= [mode=comb]` | harvests honey-level-5 hives with shears (`comb`) or glass bottles (`bottle`), but only after positively verifying smoke, a carpeted fire and a clear entrance. It checks the honey level fell and collects comb drops. The counts in the reply describe what is LEFT when it finishes, read off the hives again after the last cut, in the same words `apiary.inspect` uses: `ripe=0` after a run that emptied the only ripe hive means the job is DONE, not that one is still waiting. What it found on arrival is `wasRipe=`. `apiary.maintain`'s round ends with the same fresh counts | all safe ripe hives are done, equipment is missing, or no ripe hive is safe |
| `apiary.guard place=\|x= y= z= [range=16]` | moves every raised lit campfire in range one block underground (needs a spare campfire carried: 3 sticks, 1 coal, 3 logs), then puts a carpet (any colour you carry; 2 wool make 3) on every one that has nothing on it, and says `raised= sunk= carpeted= left=`; a fire with a hive or full block straight on it is already covered | every fire is underground with something on it, or one is open and you carry no carpet |
| `apiary.breed place=\|x= y= z= [count=2]` | feeds flowers to visible grown bees in dry daylight. Bees use the ordinary low-level `feed`, but never the ground-animal `flock.*` tools | `count=` bees ate, too few are visible, rain/night, or no flower is carried |
| `apiary.maintain place= [size=6] [mode=comb] [breed=true] [deposit=false]` | one beekeeper round: inspect, sink any raised fire (when you carry a campfire) and carpet any open one (it stops if you carry no carpet), safely harvest, then breed when enough grown bees are visible and the colony is below `size`. `deposit=true` uses the nearest chest, so use it only where that chest is unambiguous | one round is done, a ripe hive is unsafe, or a step fails twice |

Roles: `roles/<role>/ROLE.md` is the trade's handbook (what the job needs to know, which composites and marks it uses)
and `roles/<role>/*.json` are the routines it ships. The current roles are `farmer`, `rancher` and `beekeeper`.

## House rules

1. **Never break or take what others built or stored** without asking. Other players' and agents' builds are listed in
   `./mc zones`. Ask in chat before using someone's chest, furnace or bed for more than a moment.
2. **Protect what you build**, right after you start it, including two layers of ground below:
   `./mc protect name=<yours> x1= y1= z1= x2= y2= z2=`. Otherwise a `mine.get` or a `goto dig=true` (yours or another
   agent's) may tunnel through it. After building, `scan` it for stray scaffold blocks.
3. **Nights are skipped by sleeping, and everyone online must sleep.** At `night_fell`, get to a bed and `sleep`. Carry a
   bed or have one at your base (3 wool + 3 planks; WORLD.md says where spares are). If you have no bed, say so
   in chat and log out until morning: stop your body with `./mc quit` (never `pkill`: it hits other things), then run `./mc dawn` as a
   blocking command (with a command timeout of about 10 minutes): it needs no body, waits, and returns `MORNING` when it is day. Start your
   body again then. (`./mc clock` shows the time at once, also without a body. Never reconnect just to look.) One awake player keeps the night going for everyone. Your body helps: at night, with no task running, no command from you for 90 s and a bed within 32 blocks, it goes to bed by itself (event `bedtime`; `bedtime_failed` says why not). `reflexes on=false` switches that off too. It cannot help a body without a bed.
4. **Don't die stupidly.** Keep real food on you: the body eats by itself below 15 food (below 18 while hurt: health only comes back at 18 and up, so carry enough to get there), but never rotten flesh,
   raw chicken or spider eyes UNTIL food 6 or less with nothing else edible, when it eats those too rather than starve holding them; eggs, seeds and wheat are not food at all. It cannot eat on a full belly, so being hurt at food 20 is not fed by food: rest. `eat` does it
   by hand, and an `eat_failed` event says the reflex tried and could not - read it, and say so, because a body that stops eating starves with bread in its pockets. It never eats with a hostile within 6 blocks or during a fight or a
   run: the sword stays in the hand until the mob is dead, and a meal already going when one comes near is dropped. A meal
   that fails is retried after 5, 10, 20 and then every 30 s, not every 3. A meal is about a second and a half of
   holding the food still, and a hand swap or a gate worked in that moment cancels it, so while one is in the air the walk's own tool swaps wait for it: being
   stuck against a gate no longer stops the body eating.
   If a result shows `food:` and you carry only those, hunt or fetch shared food (WORLD.md says where). At food 0 you cannot sprint away
   from anything. Don't mine at low health, don't
   dig straight down, don't wander at night unarmed. Make a sword first, then a pickaxe.
5. **Place doors from outside**, standing in front of the gap, or they end up sideways.
6. **Tell the truth in chat.** Say "done" only after the result line said `ok`. If something went wrong, say that.
7. **Leave the landscape as you found it, or better.** Clean up scaffold blocks, pillars and holes you leave; `look`
   at a build from outside when done and fix what looks wrong. Whatever standard of looks this world expects is in WORLD.md.
8. **Shared pens are breeding stock, not a larder.** Take animals from a common pen only as WORLD.md allows, and always
   leave at least one pair of each species there. Count before you take (`look_around mob=cow` lists each one) and
   `flock.breed` them if low. Never hold wheat inside a pen with a gate open: the whole herd follows you out. Every pass
   through a pen gate can let one slip out (walks avoid pen gates unless the pen is where you are going): read
   `outsideGate=` in the reply, and `flock.lead ... within=60` fetches one that strayed past 32 blocks.
9. If the body misbehaves (a tool bug, not a game problem), or a tool made you do the same manual work twice, APPEND
   it to the shared `../../BUGS.md` (one entry: UTC time, your name, the exact command, its output, what you expected;
   `cat >> ../../BUGS.md`, never rewrite the file). The maintainer (WORLD.md says who) reads it and marks entries fixed; a
   bug that lives only in your journal is never seen. It is the ONE shared file you write to: don't edit the shared
   code in `../..` yourself; other agents depend on it.
10. **Before you call a build, farm or pen done, look at it (`./mc look`) and fix what looks wrong; pretty counts.**

## Known quirks

- A fight or a flee interrupts the running action (`interrupted: fleeing from zombie`): wait a few seconds, then retry.
  An unarmed body flees every hostile within 7 blocks and gets nothing done near mobs: carry a sword or an axe.
- **The flee reflex is on a leash too, and it comes back.** A run goes away from the threat until it is 16 blocks off
  (28 for an archer), then walks back to the cell the run started from, so a body chased once no longer drifts away
  from its work. It digs and bridges while fleeing, because that is the escape. Every change of state is an event with
  the threat, the position and the home cell: `flee_started`, `flee_clear` (far enough, turning back), `flee_returned`
  (home). Two events mean the body has run out of ideas and is handing you the legs: `flee_stuck`, when the run has
  covered no ground for six seconds (it is boxed in: dig straight down and wall the hole behind you, or fight), and
  `flee_held`, when the same mob drove it off again within a minute of the last walk back (the walk back was feeding a
  loop, so this run does not come home). After either, that mob starts no new run for fifteen seconds and the body
  will FIGHT it instead if it can, because standing still is worse. `stop` always clears a flee.
- **The fight reflex is on a leash.** A mob that walks the body more than 8 blocks from where the fight started, or more
  than 3 blocks below it, has the fight broken off: the body stops swinging, walks back to where it began and says
  `interrupted: breaking off a fight with spider that pulled me too far` (event `leashed`). This is what stands between
  you and the cave a spider led one of us into. It will not pick that fight up again for ten seconds, so move away or
  finish it deliberately with `attack mob=`, which has its own `leash=` (24 by default).
  One exception: when a skeleton shoots from across a field the body charges it, and the ground that charge has
  to cross is added to its leash, so the charge lands instead of being broken off a third of the way there.
  A break-off that was a DROP digs and bridges its way back up, and is given fifteen seconds to do it: the body dug
  its way down into the hole, so walking back is not enough. Getting that wrong killed a body once.
- **Endermen and wardens are not fights.** One killed a body at its own door in five seconds. `attack mob=enderman` is
  refused, the reflex never starts that fight, and one that comes within 5 blocks is run from like a creeper. Aiming at
  an enderman's head is what provokes it, so never `look_at` one; `look_around` says `careful=` when one is in sight.
  Snapshots (`look`) are safe: they use a camera, not the body's head.
- `mine.get` retries by itself when the pathfinder gives up on a block; its result says `got=` and, if short, `gaveUp=why`.
  Don't rerun it blindly: move closer or pick another spot.
- `mine.get` digs its own way down to ore and often leaves you in a pit no walk can leave (it says `pit=`): come back up with `goto ... dig=true`.
  Underground, gravel and sand fall: the body won't tunnel under them and digs its head free if buried (`buried` event).
- A `tool_broke` event (`item=stone_axe slot=hand spare=0`) wakes `./mc wait` when a tool, weapon or armour piece wears out.
  With `spare=0` craft a new one at once: a body whose only weapon broke counts as unarmed and flees every fight.
- `ignored=<names>` in a reply means the action never read those parameters: almost always a misspelt name (`item=` for `items=`,
  `block=` for `item=`). What you asked for was NOT done the way you meant: look the action up in the table and run it again.
- A `smelt` that you cut short (by sleeping, or any new task) loses nothing: the furnace cooks on, `furnace_take x= y= z=` fetches the output.
  `furnace_take` answers `stuck=` when a furnace holds input but has no fire: `smelt fuel=coal count=<n> x= y= z=` adds fuel only.
- `mine.get` needs one free inventory slot (`inventory full`): `toss` junk or `deposit` first.
- `craft` can fail with `server kept rejecting the craft (n/m made)` although you have the materials (a race with the
  server): run it again for what is still missing. `count=` is the number of items you want, not the number of crafts.
- A `code_updated` event means the maintainer fixed something in the shared code after your body started: restart it (`./mc quit`, then
  `./start` in the background) when you are safe and between tasks. Nothing is lost: inventory, position and journal stay.
- **Never kill a process.** Every body on this machine runs the same command line (`node .../src/bot.mjs .`: only the working
  directory says whose it is), so `pkill -f bot.mjs`, or a PID off `ps aux | grep node`, takes down other agents' bodies
  with yours. `./mc quit` is the only way down. A body that will not answer is a message to your lead, not something to kill.
- `./start` refuses a second body for you: `REFUSED: your body is already up (pid N): ./mc state`. Two bodies under one name
  trade a login every ten seconds and neither can work. If you get that and `./mc state` does NOT answer, say so to your
  lead rather than reaching for `kill`.
- A `wedged` event (task cancelled, "server keeps resetting my position") means the body is jammed against a block: the
  event and the error name it (`against=stone at x=.. y=.. z=..`): `dig` exactly that block, then retry. Walking harder never
  helps. When it is only leaves (a low branch at head height), the body breaks them itself and walks on (`unwedged`).
  It should be rare: if you see it against plain ground, put the coordinates in `../../BUGS.md`.
- Water: out of air, the body drops its task and swims to the nearest open surface (`surfacing`); with no task it treads water
  instead of sinking. Neither helps under a roof with no way out: never build over deep water, and farm from dry land.
- A `stalled` event (task cancelled, "no movement for 12s") means a walk never got going. `goto` a spot two blocks away, then
  retry, and copy the event's `evidence` into `../../BUGS.md`: it says what the legs were doing.
- Caves render fully lit in `look` pictures; real light levels still matter for mob spawns, so place torches.
- A server newer than the body's protocol works through a bridge; brand-new blocks may then show as flat colours in pictures.

A `died` line is written for every death and says where to go and what to fetch: `died pos=128,61,-124 cause=slain by
Zombie carried=bucket, stone_pickaxe and 50 other blocks`. The position is where the body FELL, not where it woke up,
and the drops lie there for five minutes: read it with `events type=died last=1` and go, armed. The cause is the
server's own words when it gave any, otherwise the last wound or the mobs that were on you.
