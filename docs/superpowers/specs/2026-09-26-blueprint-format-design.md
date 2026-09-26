# Blueprint format and build command

Status: design, nothing implemented. Written for the human's request for "a library of houses/buildings" in the wiki's
format, with a plan, a bill of materials and metadata, a command that builds one at given coordinates or says what is
missing, and an answer for builds larger than one inventory. The same format has to carry farms later: the farm plan's
fixed twelve-letter legend cannot name the blocks a house needs.

Examples that exercise every rule below live in `docs/blueprints-examples/`: `starter-hut.md` (a dug floor, a bed, a
door, a wall torch, material parameters), `watchtower.md` (don't-care cells, a repeated layer range, overhangs, layers out
of reach), `wheat-field.md` (the farm semantics as tags).

## 1. The file

**Markdown, one file per blueprint.** The curated library lives in `blueprints/<name>.md`, in the repo, reviewed like
code. A blueprint is a front-matter block, a fenced `legend` block, and one fenced `layer` block per layer under a
`## y<n>` heading. Everything else in the file is prose for people and is ignored by the parser.

Why Markdown: a layer is a picture, and a fenced grid is the same picture in a diff, on GitHub and in `blueprint.show`;
the file reads like the wiki page it was copied from and needs a parser of about a hundred lines. JSON was considered and
rejected: rows become quoted strings with escaped backslashes, comments are impossible, and nobody transcribes a house
into it by hand. YAML was rejected because it needs a dependency and its grids break on indentation; the front matter
here is flat `key: value` lines, which `split(':', 1)` parses.

### Front matter

| key | required | meaning |
|---|---|---|
| `name` | yes | kebab-case, equal to the file name |
| `title`, `description` | yes | shown by `blueprint.list` and `show` |
| `tags` | yes | comma list from a fixed vocabulary: shelter, storage, farm, pen, lookout, workshop, decorative, bridge |
| `front` | yes | the side of the grid the entrance faces: north, south, east or west (north is the top row) |
| `foundation` | yes | `flat` (every support stands on solid ground), `any` (holes under supports are filled with `fill`), `dug` (layer y-1 replaces the ground) |
| `clearance` | no, 0 | air layers wanted above the top layer: checked, never dug |
| `params` | no | `wood=oak, stone=cobblestone`: parameter names with defaults (see Materials) |
| `source`, `license` | no | the wiki URL a transcription came from, and its licence |
| `difficulty`, `notes`, `by` | no | free text |

Unknown keys are refused at parse time, so a typo (`foudation`) is never silently a default.

### The legend

One line per token: the token, the block in the game's own `/setblock` syntax, then optional `#tags`.

```text
D  {wood:door}[facing=north,half=lower,hinge=left,open=false]   #entrance
r  dirt|grass_block|@solid                                      #ground
```

- **Tokens are single printable characters.** A fixed-width grid is what makes a layer readable and diffable; the wiki's
  sprite grids are one block per cell too. Multi-character tokens separated by spaces were considered and rejected: a
  layer stops being a picture. One block in four facings takes four tokens (`^ > v <`); a blueprint has about ninety
  characters to spend, and the largest village house needs about thirty.
- **States** are the game's property names and values, checked against minecraft-data for the server's version at parse
  time. A state not written is not checked: `{wood:fence_gate}` accepts a gate in any facing. States the game derives
  from neighbours (stair `shape`, fence and pane connections, chest `type`, a door's upper half, a bed's head) may be
  written for the reader but are never placed and never compared.
- **Alternatives** with `|`: the first is what gets placed, any of them counts as already there. `@solid` means any full
  solid block. This is how a farm's `dirt` stops calling the grass block under it an obstacle.
- **Reserved tokens.** `_` is "don't care": the build neither places, digs nor judges that cell, and a grid pads its
  outside with it. `.` is air that must be clear when the build is done. A legend may redefine `.` (the farm migration
  does, for `air #lane`), never `_`. A space is not a cell: trailing spaces vanish in editors.

### Materials

A blueprint names its variable materials as parameters, and one house builds in any wood or stone:

- `{wood}` substitutes the raw value (`{bed}_bed` with `bed=red` is `red_bed`).
- `{wood:planks}` looks the role up in a shared family table in `src/materials.mjs`, because names are irregular:
  `{wood:log}` is `crimson_stem` for crimson and `bamboo_block` for bamboo, `{stone:stairs}` is `stone_brick_stairs` for
  stone_bricks and `brick_stairs` for bricks.
- Families: `wood` (planks, log, stripped_log, slab, stairs, fence, fence_gate, door, trapdoor, button, pressure_plate,
  sign) and `stone` (block, slab, stairs, wall). Colours need no table.
- A role a value lacks is refused when the parameters are resolved: `stone has no wall: use cobblestone, stone_bricks,
  bricks or deepslate_bricks for stone=`.

A separate preset mechanism (`@door` meaning "the door of the current wood") was considered; `{wood:door}` says the same
thing with one mechanism instead of two.

### Layers and coordinates

`## y0` is the layer the body's feet stand in when it stands on the ground: the first layer above the ground block.
`## y-1` is dug into the ground (a sunken floor, farmland, a channel). Headings may name a range, `## y2..y9`, to repeat
one grid; it is the only shorthand. Layers may be listed in any order but may not overlap, and every layer must be
exactly `width x depth`, with no ragged rows. A grid's top row is the blueprint's north, its left column its west, as on
the wiki and in today's farm plans.

The anchor `x y z` given to a command is the **world** north-west corner of the built footprint at layer y0, after
rotation. So `y` is the y the body stands at on the site, and the footprint always covers `x..x+W'-1`, `z..z+D'-1`,
where `W' x D'` is the rotated size. Today's farm `y` is the ground block, one lower: the migration adds one.

### Rotation

`facing=` names the world direction the blueprint's `front` should face. The turn is `facing - front` in quarter turns
clockwise seen from above; the default is no turn. A quarter turn clockwise maps a cell and every directional state:

| what | one quarter turn clockwise |
|---|---|
| cell `(col, row)` in `W x D` | `(D-1-row, col)` in `D x W` |
| `facing` | north to east to south to west to north |
| `axis` | x to z, z to x, y stays |
| `rotation` (signs, banners, 0-15) | plus 4, modulo 16 |
| rail `shape` | north_south to east_west, ascending_north to ascending_east, south_east to south_west to north_west to north_east |
| stair `shape`, door `hinge`, bed `part`, slab `type`, `half` | unchanged: they are relative to the block's own facing |
| fence, wall and pane sides (`north=true`...) | derived, never stored, so never turned |

Mirroring would swap stair shapes and door hinges; it is left out of the first version.

### Size limits

Footprint 64 x 64, as today's plans. Layers y-4 to y47: deep enough for a cellar, tall enough for a tower. At most
16384 cells that are not `_`. A 64-wide footprint spans five chunks, all loaded around a body standing in the middle; the
job list stays a fast in-memory sort. Anything bigger is several blueprints.

## 2. Metadata

Declared metadata is what a person knows (the front matter). Derived metadata is computed and never written in the file,
so it cannot go stale; `blueprint.show` prints it.

| derived | how |
|---|---|
| size, footprint | `W x H x D`, cells that are not `_` or `.` on the lowest layer |
| bill | items per layer and in total; block to item via a table (`wall_torch` is `torch`, `wheat` is `wheat_seeds`, `water` is `water_bucket` counted once, `bamboo_sapling` is `bamboo`), two-part blocks counted once, tools separately (`farmland` needs a hoe) |
| stack slots | the bill in inventory slots, from minecraft-data stack sizes (beds, buckets and signs do not stack to 64) |
| counts | doors, beds, containers, workstations, light sources |
| enclosed | the interior air cells no path reaches from outside the bounding box; doors and gates count as closed walls, `_` as open |
| lit, spawn-safe | block light propagated from every source (torch 14, lantern and jack o'lantern 15, decay 1 per cell); an enclosed cell a mob could stand in with light 0 makes it unlit; spawn-safe is enclosed and lit |
| stages | see section 4, for the default inventory |

**When each is validated.** Parse time (the file alone): grammar, tokens, sizes, front-matter keys, block and state names,
parameter defaults, two-part pairs, support, and the lint of section 5. Resolve time (with the call's parameters and
`facing=`): templates, family roles, rotation. Check time (against the world): obstacles, foundation, clearance, zones,
materials, stages. The `shelter` tag is refused at parse time on a blueprint that is not spawn-safe.

### Farm semantics as tags

A farm's meaning rides on tags, so farms never need a second format. `#crop` (planted with `plant`, harvested by the
sweeps), `#ground` (made with `till` or laid; natural ground counts via alternatives), `#cover` (a slab laid into a
settled source, the waterlogged state marks it), `#lane` (a cell a body stands on to work the rows, what `planLane` floods
through today), `#entrance` (a door or gate cell). Fences and gates are barriers by block name, so `blindGates` and
`penInside` need no tag. Today's legend, as columns of the new format:

| old | y-1 | y0 | y1 |
|---|---|---|---|
| `w c p b m k` | `farmland #ground` | `wheat[age=0]`, `carrots`, `potatoes`, `beetroots`, `melon_stem`, `pumpkin_stem` `#crop` | `_` |
| `s` | `sand #ground` | `sugar_cane #crop` | `_` |
| `B` | `dirt #ground` | `bamboo_sapling #crop` | `_` |
| `~` | `oak_slab[type=top,waterlogged=true] #ground #cover #lane` | `air #lane` | `_` |
| `.` | `dirt\|grass_block #ground` | `air #lane` | `_` |
| `#` `G` | `dirt\|@solid` | `oak_fence`, `oak_fence_gate` | `_` |
| `T` | `dirt\|@solid` | `oak_fence` | `torch` |
| `C K A` | `dirt\|@solid` | `chest`, `composter`, `crafting_table` | `_` |
| `F` `t` | `grass_block` / `dirt` | `dandelion #lane`, `oak_sapling #lane` | `_` |

The one-character farm map converts mechanically into this three-layer blueprint (`farmPlanToBlueprint`); see section 6.

## 3. Commands

All four are composites in `library/blueprint/`, built on existing primitives through `api.act`, because `src/bot.mjs` is
frozen for the module split. The pure part is `src/blueprint.mjs`; the walking part is `src/blueprint-build.mjs`, beside
`src/builder.mjs` and in its style.

| command | does | writes blocks |
|---|---|---|
| `blueprint.list [tag=] [q=]` | name, size, tags, total items, one line each | no |
| `blueprint.show name= [facing=] [<param>=] [layer=]` | metadata, bill per layer and total, stages for the current inventory, lint warnings, and the layers rendered after rotation | no |
| `blueprint.check name= x= y= z= [facing=] [<param>=] [supply=]` or `place=` | everything `build` would refuse over, nothing built: obstacles by block and cell, foundation cells that are not solid, clearance, zones and other agents' places, loaded chunks, missing materials against inventory plus the supply chest, per stage | no |
| `blueprint.build` with the same arguments plus `place=<new name> [clear=true] [partial=true]`, or `place=<existing build>` alone | builds, see below | yes |

**`build` is idempotent and resumable.** Every job is recomputed from the world each time: a cell whose block and written
states already match is skipped, so the world is the only progress record and a second run after a stop, a death or a
restart picks up where the blocks say. On the first call the place is marked at once with `kind=build` and the blueprint
name, rotation, parameters and a hash of the file, so `blueprint.build place=<name>` resumes without restating
coordinates, and so no other agent builds over the site. On success the kind becomes the blueprint's first tag. A
blueprint edited after its place was marked is reported (`the blueprint changed since this build started`), not silently
followed.

**Obstacles.** Natural blocks in a cell (terrain, stone, leaves, plants, snow) are dug without asking, like the farm
builder levels ground. Anything else, any crafted or placed block, is an obstacle: `check` lists it, `build` refuses
before touching a block unless `clear=true`. Containers, beds and anything inside another agent's marked place are never
dug, with or without `clear=`. This follows the village lesson that mining near something man-made needs a look first.

**The reply** is key=value, as the other builders answer: `built=34 skipped=66 stage=2/3 missing=cobblestone:20
obstacles=2 (oak_planks 104,65,-12; chest 105,65,-12)`. `already=` when nothing is left.

**Refusals are full sentences naming the cell and the fix**:
- `the foundation at 103,64,-11 is water: fill it, choose foundation=any, or move the anchor`
- `oak_planks at 104,65,-12 is in the way of the wall: dig it, or run again with clear=true`
- `starter-hut at 100,65,-14 overlaps river-pen (Chani's): pick another anchor`
- `the chunk under 130,65,-30 is not loaded: stand within 60 blocks of the site and check again`

### Order of work

Layers go bottom-up. Inside a layer the classes go in this order:

1. digs: obstacles and `.` cells;
2. full blocks, in support order (a breadth-first walk out from cells that already touch something solid), so every
   click has a face and an overhang grows outward from its wall;
3. partial blocks that stand alone: slabs, stairs, fences, walls, panes;
4. attachables, each after its support: torches, wall torches, ladders, buttons, levers, carpets, rails, pressure
   plates, crops, flowers, saplings, doors, beds;
5. fluids, with the covers that go over them. A waterlogged cover is two jobs, pour then place, the second held back
   until the cell holds a settled source (the builder's existing rule).

Gravity blocks (sand, gravel, concrete powder) count as attachables to the cell below.

Two-part blocks are one job: the door's lower half, the bed's foot, the tall plant's bottom. The job places one item and
then checks both cells, as `place` already does for beds.

### Where the body stands

Each job's standing cell comes from `src/stand.mjs`'s `standingSpots`, run over a predicted world: the real world with
the jobs done so far laid over it. Two rules are added:

- A job may not fill the body's chosen standing cell, and may not leave that cell cut off from the ring of cells
  just outside the footprint. Doors and gates count as passable. A job that would cut the body off is deferred. When
  only such jobs remain, the body walks out to the ring and does them from outside. The door therefore need not be
  last. What must be last is any solid block that closes the only way out.
- A roof is laid from the floor inside while it is in reach: eyes at 1.62 reach 4.5, which covers a roof three layers up.
  A layer with no standing cell in reach needs a scaffold (section 4).

At parse time the same simulation is run over flat, empty ground. A blueprint with a cell that no standing cell can
reach, or that can only be finished by walling the body in, fails lint and names that cell.

### What placing needs that `place` lacks today

Blueprint states are the game's states, and `place` takes the direction the body looks. `placement(state)` translates
one to the other:

| block family | from the state |
|---|---|
| stairs, doors, beds, fence gates | look toward `facing` |
| chests, furnaces, barrels, other front-facing blocks | look away from `facing` |
| wall torches, ladders, wall signs, buttons on walls | click the support at the cell minus `facing` |
| logs and pillars | click a face on `axis` |
| slabs, stairs, trapdoors | `half`/`type` top: click the upper half of a side |

`place` takes `against=north|south|east|west|up|down`, the one neighbour to click (09-27). A wall-hung or wall-faced
block is clicked onto the block behind it, a hanging one onto the block above, and a floor light, standing sign or floor
lever onto the block under it (a sneak-click when that is a crafting table). A block placed without `facing=` is judged
by the cell changing, not by a full block or the item's name, so `torch` becoming `wall_torch` is not a failure. A log on its
side is placed after the rest of its layer's full blocks and clicked onto whichever neighbour along its axis stands
then; with neither, build names it on `stuck=` and places nothing. No token is refused any more. A block that comes out
in another state is reported as `wrong=`. It never retries such a block. The rows in the
table marked for trapdoors and front-facing blocks are confirmed by one live placement each before they are trusted.

`mark` also needs fields for the blueprint name, rotation, parameters and hash. Until then they ride in the note.

## 4. Larger than one inventory

The inventory has 36 slots. The driver's kit stays: tools, food, a bucket. A build counts only the slots that are free
when it starts, minus a margin of two, as its carry. Every item takes `ceil(count / stackSize)` slots.

**Stages.** The remaining jobs, in build order, are cut into stages. Each stage is the longest run of whole layers whose
remaining bill fits the carry. A single layer that does not fit is cut inside its own order. Stages are computed from
what is still missing, so a half-built house has fewer, smaller stages, and resuming needs no stored stage number.

**The supply chest.** `supply=x,y,z` (or the name of a marked storage place) is a container near the site. Before each
stage the body walks to it, withdraws exactly that stage's shortfall with `withdraw`, and goes back. It deposits nothing.
A chest that cannot cover the stage stops the build before the stage starts, with a sentence the driver can act on:

```text
stage 3 of 5 needs cobblestone:80 more: put it in the supply chest at 98,65,-14 and run blueprint.build place=hall again
```

With `partial=true` it builds what the stage can and then stops with the same sentence. `check` prints the stage table up
front, so a driver fetches everything before starting:

```text
stage 1/3 y-1..y1 carry=cobblestone:64 oak_planks:40 oak_log:12 ... have=all
stage 2/3 y2..y4  carry=cobblestone:120 glass_pane:12              short=cobblestone:56
stage 3/3 y5..y7  carry=oak_planks:150 oak_stairs:40                short=oak_stairs:40
```

**Scaffold.** A layer out of reach from every standing cell gets a pillar outside the footprint, built the way `climb`
builds one. It is taken back with `scaffoldTakeBack` once the layers it serves are done. The material is the `scaffold=`
parameter, `dirt` by default. It is budgeted per stage: one pillar per 8 cells of perimeter, as high as the layer minus
two. It shows in the bill as `scaffold=dirt:18 (returned)`. The scaffold is dug again whenever a stage ends, so a stopped
build never leaves a pillar standing unexplained.

**Death, stops and night.** The world is the record, so a dead driver loses at most the stage it carried. The build stops
the way the other builders do: on hurt, hunger, `stop`, or a step that failed twice. The drops lie where the body died
and despawn in five minutes. The report at the stop names that cell (`carried=cobblestone:40 lies at 101,66,-12`), so the
driver runs `collect` there first and then `blueprint.build place=<name>`. A build does not start a stage after dusk and
stops at nightfall with `night=`, including inside the half-built house. A room without its door or torch is not a
shelter.

## 5. Validation

Parse-time errors refuse the file, and warnings are shown by `show` and `check`:

| check | severity | example text |
|---|---|---|
| unknown token | error | `y1 row 3 col 4: Q is not in the legend` |
| ragged row, layer size mismatch | error | `y2 is 5x4 but the blueprint is 5x5` |
| overlapping or missing layers | error | `y3 is given twice (y2..y4 and y3)` |
| unknown block, state or value | error | `D: oak_door has no state hinges (did you mean hinge?)` |
| a block the 26.1 client cannot name | error | `B: this body cannot place pale_moss_carpet (server-only block)` |
| two-part mismatch | error | `the bed foot at y0 1,2 faces east but 2,2 is not its head` |
| door with no floor | error | `the door at y0 2,4 stands on nothing: y-1 2,4 is _` |
| floating attachable | error, or warning when the support is `_` | `the wall torch at y1 2,1 needs a block at 2,0; that cell is . (air)` |
| unreachable or walling-in cell | error | `y10 0,0 has no cell to stand on within reach, even with a scaffold` |
| farmland with no water within 4 | error | today's `planErrors` text, per cell |
| unlit enclosed room | warning; error with tag shelter | `the room at y0 1..3,1..3 has 4 cells at light 0: add a light source` |
| crop with no lane within reach | warning | today's `planLane` text |

## 6. Tests

All pure functions are in `src/blueprint.mjs`. They get their own `test/blueprint.test.mjs`, never `lib.test.mjs`,
whose shared hunks have eaten other agents' commits. The tests are node:test tables with no conditionals. Each table
has some example rows below.

| function | example rows (input -> output) |
|---|---|
| `parseBlueprint(text)` | starter-hut -> 5x5x5, 12 tokens; a `## y2..y9` heading -> eight equal layers; a ragged row -> `y0 row 2 is 4 wide, the others 5` |
| `resolve(bp, params)` | `{wood:log}` crimson -> `crimson_stem`; `{stone:stairs}` bricks -> `brick_stairs`; `{stone:wall}` stone -> the "has no wall" refusal |
| `rotate(bp, turns)` | a 5x3 grid, 1 turn -> 3x5 with cell (0,0) at (2,0); `oak_stairs[facing=north]`, 1 turn -> `facing=east`; rail `ascending_south`, 2 turns -> `ascending_north` |
| `bill(bp)` | starter-hut -> cobblestone:48 oak_planks:34 oak_log:12 glass_pane:2 oak_door:1 white_bed:1 chest:1 crafting_table:1 oak_stairs:1 torch:1; wheat-field -> wheat_seeds:36 water_bucket:1, tools hoe; a door drawn in both halves -> 1 door |
| `order(jobs)` | a wall torch before its wall -> after; bed and floor in one layer -> floor first; an overhang ring -> outward from the shaft |
| `stages(jobs, free)` | watchtower, 27 free -> 1 stage; 5 free -> several stages, each ending on a layer boundary; a layer bigger than the carry -> split inside the layer |
| `siteCheck(bp, at, worldAt)` | clear flat ground -> {}; a grass block under a `dirt\|@solid` cell -> {}; planks in a wall cell -> `obstacles` naming them |
| `placement(state)` | `oak_stairs[facing=east]` -> look east; `chest[facing=south]` -> look north; `wall_torch[facing=south]` -> click the south face of the block to the north |
| `lint(bp)` | watchtower -> no errors; the hut without its torch -> unlit warning; a torch over `.` -> floating error |
| `farmPlanToBlueprint(map)` | `w~w` -> three layers, `~` a waterlogged top slab under an air lane; `T` -> fence at y0, torch at y1 |

The walking half (`blueprint-build.mjs`) is tested with `fakeApi` from `test/helpers.mjs`, as the stand tests are.
Rows: a half-built hut resumes and skips 60 blocks; a supply chest short of stage 2 stops with the exact sentence; a
wall-in job is deferred until the body is outside; an obstacle without `clear=` refuses before any `act` call.

## 7. Farms later

What changes:
- `farm.plan` accepts `blueprint=<name>` beside `map=`.
- A saved map string converts on read with `farmPlanToBlueprint`, so the maps already in the places file keep working
  and nobody's record is rewritten.
- `planCells` returns blueprint cells with the extra layers.
- `farmJobs`, `groundJobs`, `planLane`, `planErrors`, `fieldCensus` and `blindGates` read tags and block names instead
  of `PLAN_LEGEND[ch].kind`.
- `farm.build` and `pen.build` keep `buildFromPlan` and swap its job source.
- The farm `y` becomes the blueprint `y` plus one, inside the adapter only.

What stays:
- the watering and lane rules;
- the pen-leak refusal;
- `fetchWaterBucket`;
- every reply the guide documents.

Out of scope now:
- `farm.maintain` on non-crop cells;
- farm blueprints taller than three layers;
- retiring the old `map=` argument, which waits until no saved plan uses it.

## 8. Open questions for the human

1. **Where do agents' own blueprints go?** My default: the curated library stays in the repo's `blueprints/`, which
   the human commits. Agents save drafts with a later `blueprint.save` into `state/blueprints/`, and a name clash is
   refused.
2. **Transcriptions from the wiki.** Its text is CC BY-NC-SA. My default: every transcription carries `source:` and
   `license:`, and the library is used on this server but not published elsewhere.
3. **How far may `clear=true` go?** My default: it digs placed blocks in the footprint. It never digs containers,
   beds, or anything in another agent's marked place.
4. **One supply container or several?** My default: one container, where a double chest counts as one. A list of
   containers or a storage place can come later.
5. **Should `shelter` require spawn-safe?** My default: yes, and it is refused at parse time. A hut with no light is
   the most likely way a body built its own grave.
