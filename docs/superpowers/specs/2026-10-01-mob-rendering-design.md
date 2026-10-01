# Mob rendering: silhouettes, real colours, outlined on the dashboard

Every entity in a look is one flat-coloured box today, coloured from nine hand-picked names, red for some hostiles and a
hash of the name for the rest. A creeper, a spider and a chicken are three anonymous blocks; which way a cow faces is
invisible. This draws each mob as a few part boxes in its game colours, turned the way it faces, and lets the
dashboard outline and label each one over the picture.

## Base

The branch starts from `screen-popup` (master is its ancestor), not master: the render loop changed there (entities
tested only inside their screen rectangle, the look cache keyed by `lookKey`, the `x-look-seen` header), and this work
rewrites the same lines. It merges after screen-popup.

## Architecture

All drawing stays in `src/vision/renderer.mjs`, which stays pure.

- **Entity input** gains `yaw` (radians, mineflayer's convention: 0 faces north, growing to the left). `eyes.mjs`
  passes `e.yaw`. Width and height fall back to `bot.registry.entitiesByName[name]` before the 0.6 x 1.8 default, so a
  chicken that mineflayer reports without a size is still small.
- **Families.** A table maps a mob name to a family; each family is a list of part boxes in the mob's own frame:
  x across and z forward in units of the mob's width, y up in units of its height. A part is
  `[x1, y1, z1, x2, y2, z2, paint]`, where paint picks the palette entry (0 body, 1 head, 2 limbs).
- **Ray test.** Per entity and pixel the ray is turned into the mob's frame (two dot products each for origin and
  direction, which keeps `t`), tested against the family's enclosing box, and only on a hit against each part with the
  existing `rayBox`. The nearest part wins against the other entities and the terrain exactly as the single box did,
  so occlusion is unchanged. A box's face is then a face of the mob, so `FACE_SHADE` shades front, sides and back.
- **Facing.** The head's front face is painted the palette's face colour, so a mob's face shows which way it looks.
  Quadrupeds and spiders also carry the head forward of the body, so the silhouette itself is lopsided.
- **Screen rectangle.** The enclosing box's four corners are turned by yaw and the world-space box around them goes to
  the existing `screenRect`.

### Families (at most 6 parts each)

| family | parts | mobs |
|---|---|---|
| biped | head, torso, two arms, two legs | player, zombie, husk, drowned, skeleton, stray, bogged, wither_skeleton, villager, wandering_trader, pillager, vindicator, evoker, illusioner, witch, piglin, piglin_brute, zombified_piglin, zombie_villager, enderman, iron_golem, snow_golem, creaking, warden |
| quadruped | body, head forward, four legs | cow, mooshroom, pig, sheep, goat, horse, donkey, mule, skeleton_horse, zombie_horse, llama, trader_llama, camel, wolf, fox, cat, ocelot, polar_bear, panda, hoglin, zoglin, ravager, sniffer, armadillo, turtle |
| creeper | head, body, four short legs | creeper |
| spider | abdomen, head forward, four leg bars across | spider, cave_spider |
| bird | body, head forward, two legs | chicken, parrot |
| blob | the hitbox itself, its front face painted | everything else (slime, ghast, fish, items, orbs, ...) |

A mob not in the table is a biped when it is tall (height at least twice its width) and a blob otherwise. Items are
blobs with no face, so a dropped item stays a small yellow cube.

### Palette rule

Each named mob has `[body, head, limbs, face]` in its game colours (creeper green with a dark face, skeleton bone,
zombie cyan shirt, green head and blue legs, spider black with red eyes, enderman black with purple eyes, cow brown with
white legs, sheep white wool with a beige head, chicken white with an orange beak and legs, player magenta with a skin
head). A mob without its own colours is red when hostile and a hash of its name otherwise; the face is the head darker.
Players stay magenta: the guide tells the driver a magenta figure is a player.

## Structured `seen`

The renderer's `seen` entries gain `kind` and `box: [x1, y1, x2, y2]`, the pixel rectangle the entity's drawn pixels
actually cover (not hidden ones). The look reply's `seen` strings do not change: `cow 12m @px210,140`.

The dashboard asks its looks with `marks=true`, and only then does the reply carry
`marks: [{name, kind, dist, box}]` with `box` in fractions of the picture (0..1, three decimals), so a scaled image
needs no size. The driver never pays for marks it did not ask for. `/api/look/<Name>` sends them as an `x-look-marks`
header; the live stream carries them in each event.

The page draws, over both the small picture and the popup, a thin outline per mark with a label above it
(`cow 12m`), always on: the picture is small and a person reading it wants the names without hunting with a mouse.
Hostiles are outlined red, players magenta, everything else white. The caption under the picture keeps its list.

## Cache key

`lookKey`'s per-entity token adds the yaw to a sixteenth of a turn (`Math.round(yaw * 8 / PI)`), so a mob turning
round draws a new picture and a mob twitching its head does not.

## Cost budget

Each pixel inside a mob's screen rectangle pays one frame change and one enclosing-box test; only pixels on the mob pay
up to six part tests. Measured on a 480x270 frame with eight mobs close up (bench script in the session scratchpad):
before 45 ms with mobs, 42 ms without. The budget is no more than 10% over the before figure with mobs.

## Tests (test/vision.test.mjs, test/dashboard-look.test.mjs)

- A cow seen side on shows the floor between its front and back legs, where the old box was solid.
- A cow seen side on facing east covers a pixel right of its body at head height and not the same distance left;
  facing west the other way round.
- Palette, parametrized: creeper green, skeleton pale, spider dark, an unlisted hostile red, a player magenta.
- `seen` boxes: each holds its centre, and a chicken's is smaller than a cow's at the same distance.
- `lookKey`: an entity that turned a quarter is a new picture, one that turned a degree is not.
- `look` with `marks=true` returns marks in fractions with kind and distance; without it, no marks, and the strings are
  as before; an entity without a size takes the registry's.
- Dashboard: a frame with marks draws one labelled outline per mark at its percentage place; a frame without clears them.
- The pinned view and panorama hashes are updated once, on purpose, when silhouettes land.

## Out of scope

Item models, mob textures, health bars, baby sizes, poses (sitting, sleeping), the body's job system.
