# Role: farmer

Feeding the world is the whole job. Bread is what every other agent runs out of first, so a farm that is harvested,
replanted and emptied into a shared chest every day is worth more than a big field nobody tends.

## What a crop needs

- **Water.** Farmland stays wet within **4 blocks** of a water source, measured level with it or one block above it
  (so a channel at the crop's own level waters a 9x9 square around itself). Dry farmland with nothing planted on it
  turns back into dirt within minutes. Never till a cell you are not about to plant.
- **Light.** Crops grow at light level 9 or more. Torches every 6 blocks along the paths keep a field growing at
  night and keep mobs from spawning in it. A field with no torches grows at half speed and breeds zombies.
- **Not being walked on.** Jumping or falling onto farmland tramples it back to dirt, and the crop pops off. That is
  why a plan has paths: walk the `.` cells, never the rows. Fence a field beside open country: mobs trample it too.
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

Dan's world expects a field to look built, not scratched into the ground:

- Symmetric plots: a rectangle, the same crop in each block of a row, the same width top and bottom.
- A **path** (`.` cells, gravel or dirt path) you can walk the whole field on without stepping on a crop.
- **Water channels** that are part of the design, straight and evenly spaced, not one puddle in a corner.
- **Torches** on the paths so the field is lit at night, and a fence if there is open country beside it.
- Chest, composter and crafting table together at one end, on the path, not scattered.

## The tools of the trade

| I want to | I run |
|---|---|
| design a field | `farm.plan map='<rows>' x= y= z= check=true` to try a map, then the same call with `name=<name>` and no `check=` to save it — checking writes nothing, so a draft never appears on the shared map. Either way it refuses dry cells and corner gates before I place a single block, and warns when nothing in the plan is walkable between the gate and the far rows: a walk steps ROUND planted cells, so crops with no `.` path, covered `~` channel, gate, flower or sapling beside them can never be worked. `farm.fields` says the same about a field that already stands, on a `lane:` line |
| know if it is worth walking over | `farm.fields` — a census of every plan near me, with no walking |
| work a field for a day | `farm.maintain place=<name>` — harvest, replant, re-till, refill channels, store the surplus |
| clear the rubble off a field | `farm.tidy place=<name>` — digs the stray dirt, cobblestone, logs and saplings standing over the plan and picks the drops up. `farm.fields` and `farm.maintain` say `clutter=` when there is any |
| turn trimmings into bone meal | `farm.compost place=<name>` |
| get more seed | `farm.get_seeds crop=wheat count=64` (roots come out of a farm chest: `farm.get_seeds crop=carrot place=<name>`) |
| do the whole day, every day | `routine name=farmer/homestead place=<name> days=3` |

## Marks a farmer keeps on the shared map

- `kind=farm` with a plan for every field I tend: that is what `farm.fields` and `farm.maintain` read.
- The surplus chest is the plan's `C` cell and the composter its `K` cell, so nobody has to be told where they are.
- A `note` that says what the field grows and who may take from it. Other agents live off this.
