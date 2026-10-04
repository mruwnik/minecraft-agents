# villager.roll: find and lock a librarian trade

The action automates the repetitive part of obtaining enchanted books: choose an adult villager, give it a lectern,
read its first-tier offers, and reroll until one matches `want=`. With `buy=true`, buy the cheapest affordable offer
once; a completed trade locks the villager's profession and offers. One villager can produce one useful first-tier
roll. Run the action again for another villager or another book.

## Game assumptions

- A grown, untraded villager can claim a lectern by day. Breaking that lectern releases its librarian profession
  and permits a fresh roll. A completed trade locks the profession and offers.
- Babies and nitwits cannot take the job. The action stops when night falls because villagers sleep and stop claiming
  workstations.
- A first-tier librarian may not offer an enchanted book. Its book costs emeralds and one book; `maxPrice=` filters
  the emerald cost. Villager trade rebalance settings can restrict book rolls by biome.

These are game rules; the full lure, reroll and purchase loop still needs a successful live run before it is treated
as verified on this server.

## Command

`villager.roll want= x= y= z= [id=] [block=lectern] [pen=true] [penBlock=cobblestone] [maxPrice=64] [tries=40] [buy=false]`

`want=` accepts a comma-separated list of enchantments: `mending` matches any level, `sharpness:5` requires level
5, and `efficiency:4+` accepts level 4 or higher. Unknown enchantments and invalid levels are refused with a useful
message. Matching offers over `maxPrice=` do not count. `tries=` is limited to 1..200 and `maxPrice=` to 1..64.

By default, the action builds a three-block-high cobblestone enclosure with an outer buffer facing the selected
villager. Two interior standing cells lead from the lectern to an entrance three blocks away. A narrow service tunnel
behind the lectern lets the bot reach and break the block while its sill and lintel keep villagers out. The Paper
profession assignment range is less than two blocks from the lectern center, so the outer buffer blocks reachable
exterior standing cells in that radius, even with one-block-high terrain beside it. The action stands at the service
side to place and break the lectern, moves to the far side of the entrance, waits for the adult to enter, and closes
the three-block-high gate before reading offers. It needs up to 63 cobblestone to build, plus two blocks held in
reserve for resealing the service route during a dropped-item recovery.
Without `id=`, selection uses the nearest adult unemployed villager observed within 8 blocks before the lectern is
placed; the same observed entity ID is followed as it moves. With the enclosure enabled, an explicit `id=` may name
an observed villager up to 16 blocks away, allowing the pen to wait for that individual to return. `pen=false` keeps
the 8-block limit even with `id=` and requires the caller to have isolated the villager already. Other nearby
villagers may be present because the enclosure is intended to capture only the selected adult.

With `buy=true`, the action chooses the cheapest affordable current offer, buys it once, re-reads the list, and reports
`locked=true` only when the librarian profession and matching book remain. Offer selection heavily penalizes spending
emeralds or books, so a paper-to-emerald offer is preferred over buying the enchanted book when enough paper is
carried. That trade locks the villager while leaving the wanted book listed for a later purchase. It accepts either
enough emeralds for the price cap plus a book, or 64 paper for that locking trade. If a matching book appears but no
offer can be paid for, it stops without claiming a lock.

## Safety and stopping behavior

Before placing blocks, the action checks the target villager, age and profession, daylight, nearby lecterns, ownership
and protected zones, the enclosure floor and walking cells, obstructions, and the material count. It checks all four
cardinal enclosure orientations and chooses the closest valid entrance facing the selected villager, while retaining
the orientation of an existing enclosure. A prebuilt open pen may have its permanent lintel at gate y+2 while the
two lower gate cells are clear; a blocked lower entry with no selected villager already inside is refused. The target
cell cannot be in somebody else's protected zone, and the enclosure cannot cross one. Each failure names the missing
condition before construction begins.

On a miss, the action returns to the service stand, breaks the lectern, waits for the villager to become unemployed,
replaces the lectern, and records a checkpoint. If the drop stays inside the enclosure, it opens the two service sills
and tries to collect the lectern from the inner nook, then exits and reseals the route. Drop recovery is best effort:
a live manual dig left the lectern beyond pickup range even from the nook. For multiple tries it reserves one lectern
when one already stands, or two when it must place the first lectern. Before another reroll, it stops if no spare
lectern remains and leaves the current job block placed. The action also stops on a matching offer, when
`tries=` is exhausted, at nightfall, or if the villager leaves the work area. Interrupted recovery uses cleanup
actions to reseal the route and restore the lectern; a cleanup failure reports the affected cells.

## Implementation and checks

`library/villager/roll.mjs` implements the composite. `parseWant`, `bookOffer`, `rollVerdict`, `cheapestLockOffer`,
`rollRefusal`, and `villagerPenPlan` in `src/lib.mjs` hold the parsing, offer selection, safety and geometry rules.
The `trades` and `trade` primitives expose and purchase offers. `test/villager.test.mjs` covers offer parsing and
selection, four enclosure orientations, reachability within the strict claim radius on flat and raised exterior
ground, refusal before a risky build, gate closure before reading offers, cheapest affordable locking, service-nook
lectern recovery, interruption cleanup, reserve refusal, and safe stopping when no spare remains.

## Live evidence

The live probe confirmed that `trades` reads the existing offer list. The expanded enclosure captured a preexisting
locked librarian and restored the lectern afterward. In a later run, an east-facing oak enclosure at `-86,66,-166`
captured fresh, untraded villager `46537`. It produced three distinct book rolls in 57 seconds: Projectile Protection
III for 38 emeralds, Respiration I for 14, and Flame I for 17. This confirms fresh capture and rerolling once on the
server.

The initial paper-to-emerald lock changed paper from 128 to 104 and emeralds from 0 to 1; RCON showed villager XP rise
from 0 to 2. The villager remained a librarian and Flame I was still offered. The `trade` primitive initially
reported `bought=0 paid=paper:0` because it read inventory before closing the merchant window. After the fix moved
the inventory check after close, a second live `buy=true` run returned `found=flame 1`, `boughtOffer=2`,
`locked=true`, price 17; paper changed 104 to 80, emeralds 1 to 2, and XP 2 to 4.

With the lectern removed for more than 40 seconds, villager `46537` remained a librarian, had no job-site memory, and
stayed captive. The lectern at `-86,66,-166` was restored and the service sills and gate were closed. The final dig's
lectern drop remained trapped inside; the spare was consumed restoring the lectern, so no further reroll was safe.
The service-nook drop pickup remains best effort, and another spare lectern is needed before continuing.

The earlier overnight probe ended when zombies killed the agent; its equipment drops expired, and no recovery
succeeded. The live agent rebuilt its kit from supplies already in chests and continued the probe.
