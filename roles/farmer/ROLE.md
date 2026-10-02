# Role: farmer


Saved farms use the shared [3D layered plan format](../../docs/layered-plans.md): `structure.legend` maps tokens to blocks or crop intent, and `structure.layers` gives actual block elevations. A crop at layer 1 has soil at layer 0. Use explicit layer heights for terraces or stacked beds. The preset symbols described below belong to the legacy `map=` importer; it converts immediately to canonical layers. In canonical layers `_` is unconstrained and `.` is explicit air, so declare path intent in the legend.

Feeding the world is the whole job. Bread is what every other agent runs out of first, so a farm that is harvested,
replanted and emptied into a shared chest every day is worth more than a big field nobody tends.

## What a crop needs

- **Specific or mixed beds.** A plan's `w`, `c`, `p`, `b`, `m` and `k` cells require wheat, carrots, potatoes, beetroot, melon stems and pumpkin stems respectively. Use `*` for a mixed farmland bed: keep any of those crops already growing there, or plant suitable available seed when empty. Attached stems still count as their crop. Melon and pumpkin fruit are not planted crops; fruit in a wheat bed is cleared before wheat is sown. Cane and bamboo retain their separate `s` and `B` cells because they need different ground.
- **Melons and pumpkins.** Harvest cuts the fruit block beside a compatible mature or attached stem, keeping the stem for regrowth. In a named farm, both the fruit and its planned stem must be inside the footprint; neighboring path cells can hold the fruit. A fruit block without a matching stem is not treated as a ripe crop.
- **Water.** Farmland stays wet within **4 blocks** of a water source, measured level with it or one block above it
  (so a channel at the crop's own level waters a 9x9 square around itself). Dry farmland with nothing planted on it
  turns back into dirt within minutes. Never till a cell you are not about to plant.
- **Light.** Crops grow at light level 9 or more. Torches every 6 blocks along the paths keep a field growing at
  night and keep mobs from spawning in it. A field with no torches grows at half speed and breeds zombies.
- **Not being walked on.** Jumping or falling onto farmland tramples it back to dirt, and the crop pops off. That is
  why a plan has paths: walk the `.` cells, never the rows. Fence a field beside open country: mobs trample it too.
  Plain walking never tramples, so a walk goes round planted cells while a lane is within reach and crosses the rows
  only where there is no other way, at a walking pace (no sprint, no jump on that leg): a sweep that leaves you deep
  in a planted block is not a dead end, and the crops stay.
- **Sugar cane and bamboo** are different. They need sand, dirt, grass or podzol **directly beside a water block**
  (any of the four sides, same level), and no light at all. Harvest by **cutting the second segment**, never the base:
  the base regrows. `farm.harvest` does this for you and reports `stalkBases=` to prove every base still stands. Stalks are
  cut from outside their plot, so a stalk plot more than 6 wide has a middle nobody can reach.
- **Melon and pumpkin** grow a stem on farmland and put the fruit on a free dirt, grass or farmland cell **beside** it.
  Cut the fruit and leave the stem: it grows another. Leave one free cell per stem or nothing ever appears.
- **Growth takes real time.** A wheat crop is ripe at `age 7`. Bone meal (`fertilize`) jumps it a stage at a time;
  a composter is where the bone meal comes from.

## Animals, if the farm has a pen

- Breeding: two adults, their own food, and **5 minutes** before either can breed again. Babies eat the food and
  breed nothing. Cow, sheep and goat take wheat; pig takes carrot, potato or beetroot; chicken takes any seed;
  rabbit takes carrot or dandelion.
- A pen needs 4x4 of inside floor, a gate **mid-wall** (nothing walks through a corner gate), ground outside no
  higher than the floor inside, and nobody standing in the approach. Never hold wheat inside a pen with the gate open.

## Composting: what is worth feeding

The composter fills in 7 layers and gives **1 bone meal** at level 8. Each item has its own chance to add a layer:

| chance | items |
|---|---|
| 30% | seeds, saplings, leaves, grass, kelp, dried kelp, sugar cane, sweet berries, glow berries, hanging roots |
| 50% | dripleaf, cactus, melon slice, sugar cane, tall grass, vines, nether wart |
| 65% | apple, beetroot, carrot, cocoa beans, potato, wheat, big dripleaf |
| 85% | baked potato, bread, cookie, hay bale, mushroom block, nether wart block |
| 100% | cake, pumpkin pie |

Rule of thumb: compost the **seeds and the trimmings**, not the food. `farm.compost` holds back seed you need to sow
again and anything edible unless you name it: `farm.compost items='{"wheat":20}'` when the chests are already full of bread.

## What "pretty" means here

This world expects a field to look built, not scratched into the ground:

- Symmetric plots: a rectangle, the same crop in each block of a row, the same width top and bottom.
- A **path** (`.` cells, gravel or dirt path) you can walk the whole field on without stepping on a crop.
- **Water channels** that are part of the design, straight and evenly spaced, not one puddle in a corner.
- **Torches** on the paths so the field is lit at night, and a fence if there is open country beside it.
- Chest, composter and crafting table together at one end, on the path, not scattered.

## The tools of the trade

| I want to | I run |
|---|---|
| harvest somebody else's field | `farm.harvest place=<their field>` — it runs when their mark invites it (welcome, anyone, take, harvest) and refuses otherwise, naming the owner and the note. A note that says "ask first" or "do not" closes it however it is worded elsewhere. Ask in chat before working ground whose note says nothing |
| design a field | `farm.plan map='<rows>' x= y= z= check=true` to try a map, then the same call with `name=<name>` and no `check=` to save it — checking writes nothing, so a draft never appears on the shared map. Either way it refuses dry cells and corner gates before I place a single block, and warns when nothing in the plan is walkable between the gate and the far rows: a walk steps ROUND planted cells, so crops with no `.` path, covered `~` channel, gate, flower or sapling beside them can never be worked. `farm.fields` says the same about a field that already stands, on a `lane:` line |
| know if it is worth walking over | `farm.fields` — a census of every plan near me, with no walking |
| work a field for a day | `farm.maintain place=<name>` — prepare tools and food, harvest only this plot, tidy, then repair and replant according to its plan, refill channels and store the surplus. A wrong crop or fruit in a planned crop cell is cleared before sowing; correctly placed crops and stems stay. |
| clear the rubble off a field | `farm.tidy place=<name>` — digs the stray dirt, cobblestone, logs and saplings standing over the plan, and wrong crops or fruit occupying planned crop cells, then picks the drops up. `farm.fields` and `farm.maintain` say `clutter=` when there is any. Maintenance already does this between harvest and sowing. |
| turn trimmings into bone meal | `farm.compost place=<name>` |
| get more seed | `farm.get_seeds crop=wheat count=64` (roots come out of a farm chest: `farm.get_seeds crop=carrot place=<name>`) |
| do the whole day, every day | `routine name=farmer/homestead place=<name> [vars='{"compost":"shared-composter"}'] days=3` — several fields are one homestead: `place=crop-field,melon-patch,cane` runs the routine once per field, in that order, every day. The homestead's `farm.maintain` step composts its own spare seed; `vars=` names the one composter (or chest-like block) every field feeds, shared across all of them; leave it out and each field falls back to its own plan's `K` cell. `dry=true` prints the day's steps and runs nothing; `days=0` runs it until stopped (the autopilot: `routine_day` and `routine_stopped` events say how it went). Fields further than 32 blocks from your bed are fine: mark the bed once (`mark name=<you>-bed kind=bed`, standing on it) or pass `bed=<place>`, and at nightfall the routine walks to it when it is within `bed_range` (default 200), sleeps, and walks back at dawn; beyond that, or with no bed marked, the night still stops it |
| bake the harvest | `bake store=x,y,z [keep=16]` — every 3 wheat in the store chest become bread at the crafting table within 32 blocks of it, the bread goes back, and 16 loaves stay carried |
| run the farm until there is enough bread | `routine name=farmer/bread place=<farm> store=x,y,z bed=x,y,z until='(>= (read :chest_count {:x .. :y .. :z .. :item "bread"}) 576)' days=0` — maintain, bake, sleep in that bed, every day until the store holds 576 loaves; it stops with `routine_stopped reason=until` and what it read |

## When a sweep stalls, or a channel stays dry

Farm commands distinguish a decision for the driver from a hard stop:

| Result | Behavior |
|---|---|
| Missing seed, tools, building materials or water; missing/full storage; some unreachable work | Report partial progress and `farm_attention`, then continue independent useful work within the command's bounds. The driver decides how to address it. |
| Invalid arguments or plan, unauthorized work, unsafe/misaligned construction, failed protection checks, unexpected errors | Halt clearly; do not treat the command as successful or continue destructive work. |
| Cancellation, death, low health/food, night without safe sleep, or a direct interruption | Preserve the stop through nested farm commands. Resolve the interruption before restarting work. |
| Growing crops, a partially filled composter, or a census with nothing ripe | Normal operation; no attention alert needed. |

- **Keep the work in maintenance.** The homestead routine schedules only `farm.maintain` for each plot. Maintenance gets a hoe, spare and food from carried materials or the farm's chest, clears clutter between harvesting and planting, and tries to replace a hoe lost during tilling. `kit_short=` names supplies it could not obtain; it still plants ready farmland where possible. Diagnose gaps in maintenance instead of adding separate kit, tidy or hand-planting chores to the driver's loop.
- **Storage needs an agent decision.** Missing or full storage produces a `farm_attention` event that wakes `./mc wait`, with the plot, storage problem and produce still carried. Maintenance reports the problem without choosing another destination. The driver decides whether to repair or add storage, or configure `deposit=` for an authorized existing store.
- **Leave room for the harvest.** At four free inventory slots or fewer, maintenance first tries one surplus deposit into the configured store. Ordinary crop seed reserves cover two sowings; cane and bamboo reserves are capped at one stack per species across the homestead because their bases survive harvesting. An inventory stop during harvest reports attention and preserves partial progress before handing control back.
- **Short seed and water supplies.** Maintenance leaves seedless dirt beds until it can plant them, reporting the shortage without repeatedly spending a hoe on soil that will revert. Bucket refills preserve planned irrigation channels and use an external source; if none is available, provide a filled bucket or a separate water supply.
- **Optional bone meal.** Natural growth is the default (`bone_meal=false`). Set `farm.maintain place=<name> bone_meal=true` to apply bone meal before harvesting, once per matching immature wheat, carrot, potato or beetroot bed per sweep. It uses carried stock first, topping up shortages from the configured `compost=` target (or the plan’s `K` cell) and its connected hopper/output storage. A ready plain composter can supply meal without spare seed to feed it. `compost=false` disables that source but still permits carried meal. Actual consumption and supply/access problems are reported; unrelated storage is never searched. The later compost phase produces supply for subsequent sweeps. Through the homestead routine, use `vars='{"bone_meal":true}'`; do not add separate fertilizing chores.
- **Trees above the field.** Low tidying clears only the first two blocks above the ground. `overhead_tree=` reports remaining logs and leaves over planned beds and paths, scanning heights 3–32 above ground. Maintenance and tidy raise `farm_attention` with counts and example coordinates; `farm.fields` shows the same finding. Inspect the tree and choose safe removal or a plan change. The overhead scan does not fell trees.
- **`stuck=` naming a cell.** `farm.maintain` walks every leg plain first and, when the path fails inside the plan's
  footprint, once more with `dig=true` (never through a plan block); `stuck=` means both failed, or that it stood
  outside the footprint and would not dig from there. The recipe that works: step back the way you came (10 blocks or
  so), `goto x= y= z= dig=true` to the cell `stuck=` names, and run `farm.maintain place=<name>` again from there. A
  sweep started from inside the field never has to cross the crops to reach its first cell.
- **`skipped=` with `no bucket`.** The sweep waters its own channels, but only from a bucket you carry: keep an empty
  `bucket` in your pockets (`craft item=bucket`, 3 iron ingots), and it fills it at the nearest still water within 32
  blocks and pours the dry cells itself. `no water within 32 blocks` means the field is too far from any pond: dig a
  2x2 pool beside it and pour two buckets in corner to corner (an infinite source), then sweep again.
- **A gate at a terrain step.** Where two fields meet across a one-block step in the ground, the gate cell between them
  is a chokepoint: `goto` walks through an open gate at a step now, so rebuild the gate properly (the plan's own gate
  cell, mid-wall, one full block of level ground on each side, no second fence row behind it) rather than digging it to
  a bare gap. `farm.maintain` and `farm.build` put the plan's gate back on every pass, so a bare gap never lasts, and
  each rebuild of it is a stall until the ground on both sides is levelled.

## Marks a farmer keeps on the shared map

- `kind=farm` with a plan for every field I tend: that is what `farm.fields` and `farm.maintain` read.
- The surplus chest is the plan's `C` cell and the composter its `K` cell, so nobody has to be told where they are.
  Plots without a `K` share one: `farm.maintain place=<name> compost=<x,y,z or a marked place>` feeds the spare seed
  there (a composter, or a chest-like block for somebody else to compost) instead of storing it with the harvest.
- A `note` that says what the field grows and who may take from it. Other agents live off this.
