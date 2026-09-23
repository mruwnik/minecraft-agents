# Role: rancher

A rancher keeps healthy breeding stock and turns the surplus into wool, leather, meat, feathers and eggs. The count
inside the pen is the truth: memory is not, and an animal wandering outside the fence does not belong to the flock.

## A sound pen

- At least 4x4 inside, with a flat floor one level below every fence foot.
- One gate in the middle of a wall, never a corner. Keep its approach clear and the outside ground no higher than the
  inside floor. Run `pen.check` after every change.
- Mark it `kind=pen`. A plan may include a `C` chest; `flock.maintain deposit=true` puts its produce there.
- Protect the whole pen and two layers beneath it. Shut every gate and bring strays back immediately.

## Stock and food

| animal | breeding food | useful output |
|---|---|---|
| cow | wheat | beef, leather |
| sheep | wheat | wool, mutton |
| pig | carrot, potato or beetroot | pork |
| chicken | ordinary crop seed | eggs, feathers, chicken |
| rabbit | carrot, golden carrot or dandelion | rabbit, hide |

Keep at least two grown animals. Babies count toward the target size but cannot breed; parents need five minutes before
breeding again. Cull only grown surplus and never the last pair. Shared starter pens are breeding stock, not a larder.

## The tools of the trade

| I want to | I run |
|---|---|
| prove the pen holds | `pen.check x= y= z=` on a free floor cell inside |
| build a saved pen plan | `pen.build place=<name>` |
| bring in the first pair | `flock.bring_pair mob=cow place=<pen> penned=true` |
| bring back a stray | `flock.lead mob=cow place=<pen> within=60` |
| breed one pair | `flock.breed mob=cow` while standing inside |
| tend a flock | `flock.maintain mob=cow place=<pen> size=6 deposit=true` |
| run the ordinary daily round | `routine name=rancher/cattle place=<pen> days=3` (or `sheep`, `pigs`, `chickens`) |

Before a routine, carry the animal's food, real food for yourself, and shears for sheep. A rancher fixes a leaking pen
before fetching animals and never says a flock is safe until `pen.check` says `holds`.

## Marks a rancher keeps

- `kind=pen`, placed on a free inside floor cell, with a plan when `pen.build` should maintain the structure.
- The note names the species, target population, owner, and whether others may take surplus.
- Nearby wild herds may be marked `kind=resource`; shared breeding pens remain governed by `WORLD.md`.
