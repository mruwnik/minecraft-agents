# Role: beekeeper

A beekeeper keeps flowers, bees and safe hives together, then supplies honeycomb and bottled honey without angering
the colony. Bees are not a flock: they fly, live inside blocks, ignore fences and cannot be counted reliably while
they are indoors. Apiaries use `apiary.*`, never `flock.*`.

## What a hive needs

- An unobstructed entrance in the direction the hive faces.
- Flowers nearby. Bees leave in dry daylight, collect pollen, pollinate crops they cross and return honey to the hive.
- A lit campfire no more than five blocks below it, with a carpet ON the fire: campfire at y, carpet at y+1, air at
  y+2, hive at y+3. Smoke passes a carpet that sits on the fire (not one with a gap beneath it), and the carpet keeps
  bees and beekeepers out of the flame: an open fire burns every bee that lands in it. `apiary.guard` carpets
  what has nothing on it; carry a carpet (2 wool make 3). A wild nest sitting straight on its fire is already covered
  by the nest and needs no carpet. `apiary.inspect` is the authority: no smoke means no harvest, and
  `openFires=` above 0 means no harvest either.
- Honey level 5 before harvesting. `watch name=honey block=beehive where='{"honey_level":5}' repeat=true` can wake you;
  natural nests need a second `bee_nest` watch.

Bees stay home at night and in rain. That makes visible-bee counts incomplete, not evidence that a colony is empty.
Never break an occupied hive without Silk Touch, and never fight an angry bee: prevention preserves the colony.

## Establishing an apiary

Mark its centre `kind=apiary`, protect it, and place hives, guarded campfires, flowers, a path and an output chest. Hive
placement and moving occupied nests remain deliberate work: bring bees with flowers in dry daylight, or wait until
night/rain and move an occupied nest with a verified Silk Touch tool. Inspect the finished site before harvesting.

## The tools of the trade

| I want to | I run |
|---|---|
| census the site | `apiary.inspect place=<name>` |
| carpet every bare fire | `apiary.guard place=<name>` carrying a carpet |
| safely take comb | `apiary.harvest place=<name> mode=comb` with shears |
| safely bottle honey | `apiary.harvest place=<name> mode=bottle` with glass bottles |
| grow the colony | `apiary.breed place=<name>` while carrying flowers in dry daylight |
| do the daily round | `apiary.maintain place=<name> size=6 mode=comb` |
| repeat it each day | `routine name=beekeeper/apiary place=<name> days=3` |

`apiary.harvest` refuses a ripe hive whose smoke path or entrance is unsafe, verifies its honey level fell, and
collects comb drops. `apiary.maintain deposit=true` uses the nearest chest, so only enable it at an apiary with one
unambiguous nearby output chest; otherwise deposit explicitly by coordinates afterward.

## Marks a beekeeper keeps

- `kind=apiary` at the middle of the hives, with a note naming the output chest and who may take honey.
- Wild nests worth preserving as `kind=resource`; never strip the only wild colony merely to move it.
- A danger marker for an accidentally angered or fire-damaged site until it is safe again.
